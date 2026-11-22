use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Arc, Mutex};

use anyhow::Context;
use hashbrown::{HashMap, HashSet};
use lsp_types as lsp;
use redscript_community_formatter::{FormatSettings, format_document};
use redscript_compiler_api::ast::Span;
use redscript_compiler_api::{
    Diagnostic, LoweredCompilationUnit, QueryTarget, Symbols, TypeId, TypeSchema, ast,
};

use crate::analysis::{AnalysisMode, BaseEnv, CacheBytes, CompilationCache};
use crate::code_actions::{Fix, FixMap, FixRequest, FixScope, generate_fix};
use crate::completions::{
    CompletionResolveData, generate_completions, member_completion_range,
    repair_completion_terminator,
};
use crate::diagnostics::{DiagnosticBatch, DiagnosticTracker, diagnostic_tags, location, range};
use crate::dialect::{self, Dialect};
use crate::query::ExprAt;
use crate::server::{CodeLocation, CompletionPresentation, Document, LanguageServer, LspContext};
use crate::workspace::{
    FileResolution, LoadedWorkspace, Overlay, WorkspaceDir, collect_workspace_sources,
    content_hash, record_source_read_error,
};

#[path = "document_symbols.rs"]
mod document_symbols;

const EMBEDDED_CACHE: &[u8] = include_bytes!("../assets/api.redscripts.zlib");
const REDMOD_SUBSET_DOCS_URL: &str =
    "https://github.com/jac3km4/redscript/blob/ab77364/crates/compiler/redmod/adapter/README.md";
/// Disabled-action reason shared by stale single fixes and fix-all actions.
const STALE_FIX_REASON: &str =
    "This fix is stale because workspace files changed; save to refresh diagnostics";

/// Fails fast when the analysis worker's current request was cancelled.
///
/// Query entry points call this so a `$/cancelRequest` that arrives before the
/// expensive analysis phase aborts the work instead of computing a result the
/// worker would discard anyway.
pub(crate) fn cancelled() -> anyhow::Result<()> {
    if crate::server::is_cancelled() {
        anyhow::bail!("request cancelled");
    }
    Ok(())
}

/// Accumulates compiler diagnostics and refreshed quick fixes across the
/// workspace and standalone analyses performed during one publish pass.
struct CompilerDiagnostics {
    by_file: BTreeMap<PathBuf, Vec<lsp::Diagnostic>>,
    fixes: FixMap,
}

pub struct RedscriptLanguageServer {
    workspaces: HashMap<PathBuf, WorkspaceDir>,
    config_warnings: BTreeSet<String>,
    reported_config_warnings: BTreeSet<String>,
    io_warnings: BTreeSet<String>,
    reported_io_warnings: BTreeSet<String>,
    standalone_paths: HashSet<PathBuf>,
    compilation_cache: CompilationCache,
    cached_fixes: FixMap,
    overlays: HashMap<PathBuf, Overlay>,
    standalone: HashMap<PathBuf, CompilationCache>,
    generation: u64,
    missing_roots: BTreeSet<PathBuf>,
    reported_missing_roots: BTreeSet<PathBuf>,
    warned_extensions: HashSet<PathBuf>,
    diagnostics: DiagnosticTracker,
    completion_cache: Mutex<Option<CachedCompletion>>,
    /// Monotonic id of the completion response currently cached, so a stale
    /// `completionItem/resolve` is rejected instead of returning a wrong doc.
    completion_generation: AtomicU64,
    /// Test-only count of workspace snapshot rebuilds, used to assert that a
    /// standalone edit leaves the workspace snapshot alone.
    #[cfg(test)]
    workspace_rebuilds: usize,
}

struct CachedCompletion {
    path: PathBuf,
    receiver_end: usize,
    source_without_member: String,
    other_overlays: BTreeMap<PathBuf, Arc<str>>,
    response: Arc<lsp::CompletionResponse>,
    /// Documentation deferred out of `response` when the client resolves it
    /// lazily, indexed by each item's `data.id`.
    docs: Vec<String>,
    /// Identifies this cached response's `docs` so a resolve against a newer
    /// response is not answered with the wrong documentation.
    generation: u64,
    /// The response adapted to a client's completion presentation capabilities,
    /// produced on first use. Capabilities are fixed after `initialize`, so this
    /// normally holds one entry that every later request reuses.
    adapted: Option<(CompletionPresentation, Arc<lsp::CompletionResponse>)>,
}

/// Orders a file's edits by position so `TextEdit` ordering is deterministic.
/// The LSP applies edits in array order, and diagnostic-span iteration order
/// (hash-map backed) is otherwise unspecified.
fn sort_text_edits(edits: &mut [lsp::TextEdit]) {
    edits.sort_by(|a, b| {
        (
            a.range.start.line,
            a.range.start.character,
            a.range.end.line,
            a.range.end.character,
            a.new_text.as_str(),
        )
            .cmp(&(
                b.range.start.line,
                b.range.start.character,
                b.range.end.line,
                b.range.end.character,
                b.new_text.as_str(),
            ))
    });
}

/// Whether `cached` matches the current overlays excluding `path`, without
/// allocating a comparison map on cache hits.
fn overlays_match(
    cached: &BTreeMap<PathBuf, Arc<str>>,
    overlays: &HashMap<PathBuf, Overlay>,
    path: &Path,
) -> bool {
    let expected = overlays.len() - usize::from(overlays.contains_key(path));
    expected == cached.len()
        && overlays.iter().all(|(other, overlay)| {
            other == path || cached.get(other).is_some_and(|text| text == &overlay.text)
        })
}

/// Whether a standalone `CompilationCache` was built from exactly `text` at
/// `path`, so its already-forced analysis can be reused as-is.
fn standalone_matches(cache: &CompilationCache, path: &Path, text: &str) -> bool {
    cache.borrow_file_ids().get(path).is_some_and(|&id| {
        cache.with_sources(|sources| sources.get(id).is_some_and(|file| file.source() == text))
    })
}

impl RedscriptLanguageServer {
    pub fn new(
        cache_path: &Path,
        workspace_folders: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> anyhow::Result<Self> {
        let cache_bytes = fs::read(cache_path)?;
        // A user-supplied cache must be validated so `main` can fall back to the
        // embedded cache when it is corrupt.
        Self::from_bytes(cache_bytes.into(), workspace_folders, true)
    }

    pub fn embedded(
        workspace_folders: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> anyhow::Result<Self> {
        let cache_bytes = miniz_oxide::inflate::decompress_to_vec_zlib(EMBEDDED_CACHE)
            .map_err(|err| anyhow::anyhow!("failed to decompress embedded cache: {err:?}"))?;
        // The compiled-in cache is trusted and its zlib stream is already
        // checksummed, so skip the redundant eager decode.
        Self::from_bytes(cache_bytes.into(), workspace_folders, false)
    }

    fn from_bytes(
        cache_bytes: CacheBytes,
        workspace_folders: impl IntoIterator<Item = impl Into<PathBuf>>,
        validate: bool,
    ) -> anyhow::Result<Self> {
        let mut workspaces = HashMap::new();
        let mut config_diagnostics = BTreeMap::new();
        let mut config_warnings = BTreeSet::new();
        for dir in workspace_folders {
            let dir: PathBuf = dir.into();
            let dir = dialect::identity(&dir);
            let loaded = WorkspaceDir::load(&dir, None);
            if let Some(diagnostic) = loaded.diagnostic {
                config_diagnostics.insert(loaded.dotfile_path, diagnostic);
            }
            config_warnings.extend(loaded.warnings);
            workspaces.insert(dir, loaded.workspace);
        }
        let workspace_dirs = workspaces.iter().flat_map(|(_, w)| w.roots.clone());
        let mut missing_roots = BTreeSet::new();
        let mut read_diagnostics = BTreeMap::new();
        let mut io_warnings = BTreeSet::new();
        let sources = collect_workspace_sources(
            workspace_dirs,
            &HashMap::new(),
            |_| false,
            &mut missing_roots,
            &mut read_diagnostics,
            &mut io_warnings,
        );
        let mut diagnostics = DiagnosticTracker::default();
        diagnostics.set_reads(read_diagnostics);
        diagnostics.set_configs(config_diagnostics);
        let base = Arc::new(BaseEnv::new(cache_bytes));
        // Validate a configured cache once, so a corrupt file is reported at
        // startup (and `main` can fall back to the embedded cache) instead of on
        // the first analysis. The trusted embedded cache skips this decode, and
        // a validated cache warms the shared base for the first snapshot.
        if validate {
            base.ensure_decoded()?;
        }
        Ok(Self {
            compilation_cache: CompilationCache::from_sources(base, sources)?,
            workspaces,
            config_warnings,
            reported_config_warnings: BTreeSet::new(),
            io_warnings,
            reported_io_warnings: BTreeSet::new(),
            standalone_paths: HashSet::new(),
            cached_fixes: HashMap::new(),
            overlays: HashMap::new(),
            standalone: HashMap::new(),
            generation: 0,
            missing_roots,
            reported_missing_roots: BTreeSet::new(),
            warned_extensions: HashSet::new(),
            diagnostics,
            completion_cache: Mutex::new(None),
            completion_generation: AtomicU64::new(0),
            #[cfg(test)]
            workspace_rebuilds: 0,
        })
    }

    fn invalidate(&mut self) {
        self.generation = self.generation.wrapping_add(1);
        // Cached fixes are validated per file against the current source
        // contents, so an unrelated edit must not discard fixes for untouched
        // files. `collect_compiler_diagnostics` refreshes each analyzed file's
        // fixes on every save.
    }

    fn make_edit_snapshot(&self) -> anyhow::Result<CompilationCache> {
        let sources = ast::SourceMap::new();
        let mut files: BTreeMap<PathBuf, String> = BTreeMap::new();
        self.compilation_cache.with_sources(|active| {
            for (_, file) in active.files() {
                if self
                    .compilation_cache
                    .borrow_file_ids()
                    .contains_key(file.path())
                {
                    let text = self.overlays.get(file.path()).map_or_else(
                        || file.source().to_owned(),
                        |overlay| overlay.text.to_string(),
                    );
                    files.insert(file.path().to_owned(), text);
                }
            }
        });
        for (path, overlay) in &self.overlays {
            if self.resolve_file(path).as_workspace().is_some() {
                files.insert(path.clone(), overlay.text.to_string());
            }
        }
        for (path, text) in files {
            sources.push_back(path, text);
        }
        CompilationCache::from_sources(self.compilation_cache.borrow_base().clone(), sources)
    }

    fn make_snapshot(
        &self,
        roots: impl IntoIterator<Item = PathBuf>,
        missing_roots: &mut BTreeSet<PathBuf>,
        read_diagnostics: &mut BTreeMap<PathBuf, lsp::Diagnostic>,
        io_warnings: &mut BTreeSet<String>,
    ) -> anyhow::Result<CompilationCache> {
        let sources = collect_workspace_sources(
            roots,
            &self.overlays,
            |path| self.resolve_file(path).as_workspace().is_some(),
            missing_roots,
            read_diagnostics,
            io_warnings,
        );
        CompilationCache::from_sources(self.compilation_cache.borrow_base().clone(), sources)
    }

    fn rebuild(&mut self, ctx: &LspContext) -> anyhow::Result<()> {
        self.rebuild_with_reload(ctx, true)
    }

    fn rebuild_with_reload(&mut self, ctx: &LspContext, reload: bool) -> anyhow::Result<()> {
        self.rebuild_scoped(ctx, reload, true, true)
    }

    /// Whether `path` belongs to a loaded workspace, and so dirties the
    /// workspace snapshot rather than a standalone analysis.
    fn is_workspace_document(&self, path: &Path) -> bool {
        self.resolve_file(path).as_workspace().is_some()
    }

    /// Rebuilds the workspace and/or standalone snapshots, logging progress.
    ///
    /// `workspace`/`standalone` select which partition is recomputed; a document
    /// edit only dirties one of them. A `reload` always dirties both, because it
    /// re-reads the workspace from disk and can move files between partitions.
    fn rebuild_scoped(
        &mut self,
        ctx: &LspContext,
        reload: bool,
        workspace: bool,
        standalone: bool,
    ) -> anyhow::Result<()> {
        let workspace = workspace || reload;
        let standalone = standalone || reload;
        let started = std::time::Instant::now();
        ctx.logger().verbose(|| {
            format!(
                "Rebuild started reload={reload} workspace={workspace} standalone={standalone} overlays={} versions={:?}",
                self.overlays.len(),
                self.overlays
                    .iter()
                    .map(|(path, overlay)| (path, overlay.version))
                    .collect::<Vec<_>>(),
            )
        });
        let result = self.rebuild_inner(ctx, reload, workspace, standalone);
        match &result {
            Ok(()) => ctx.logger().verbose(|| {
                format!(
                    "Rebuild finished generation={} elapsed_ms={:.3}",
                    self.generation,
                    started.elapsed().as_secs_f64() * 1000.0,
                )
            }),
            Err(error) => ctx.logger().error(format!(
                "Rebuild failed generation={} elapsed_ms={:.3}: {error:#}",
                self.generation,
                started.elapsed().as_secs_f64() * 1000.0,
            )),
        }
        result
    }

    fn rebuild_inner(
        &mut self,
        ctx: &LspContext,
        reload: bool,
        workspace_dirty: bool,
        standalone_dirty: bool,
    ) -> anyhow::Result<()> {
        let started = std::time::Instant::now();
        self.invalidate();

        // Read diagnostics are partitioned so only the dirty partition is
        // recomputed; the other keeps its entries.
        let (mut workspace_reads, mut standalone_reads): (
            BTreeMap<PathBuf, lsp::Diagnostic>,
            BTreeMap<PathBuf, lsp::Diagnostic>,
        ) = self
            .diagnostics
            .reads()
            .iter()
            .map(|(path, diag)| (path.clone(), diag.clone()))
            .partition(|(path, _)| self.resolve_file(path).as_workspace().is_some());
        let mut io_warnings = self.io_warnings.clone();
        let mut missing_roots = BTreeSet::new();

        if workspace_dirty {
            #[cfg(test)]
            {
                self.workspace_rebuilds += 1;
            }
            if reload {
                self.completion_cache.lock().unwrap().take();
                workspace_reads.clear();
                io_warnings.clear();
            }
            ctx.logger().verbose(|| {
                if reload {
                    "Rebuild phase=workspace discovery, parsing and analysis".to_owned()
                } else {
                    "Rebuild phase=cached sources, overlays and analysis (no workspace scan)"
                        .to_owned()
                }
            });
            let workspace = if reload {
                self.make_snapshot(
                    self.workspaces
                        .values()
                        .flat_map(|workspace| workspace.roots.clone()),
                    &mut missing_roots,
                    &mut workspace_reads,
                    &mut io_warnings,
                )?
            } else {
                self.make_edit_snapshot()?
            };
            self.compilation_cache = workspace;
            ctx.logger().verbose(|| {
                format!(
                    "Rebuild workspace ready elapsed_ms={:.3}",
                    started.elapsed().as_secs_f64() * 1000.0,
                )
            });
        }

        if standalone_dirty {
            ctx.logger()
                .verbose(|| "Rebuild phase=standalone analysis".to_owned());
            let paths: HashSet<_> = self
                .standalone
                .keys()
                .chain(self.standalone_paths.iter())
                .chain(self.overlays.keys())
                .filter(|path| self.resolve_file(path).as_workspace().is_none())
                .cloned()
                .collect();
            // Take the previous caches so an unchanged standalone file keeps its
            // already-analyzed snapshot instead of re-interning the base-game
            // symbols into a fresh snapshot.
            let mut reusable = std::mem::take(&mut self.standalone);
            let mut standalone = HashMap::with_capacity(paths.len());
            let mut active_standalone_paths = HashSet::new();
            for path in paths {
                let text = self
                    .overlays
                    .get(&path)
                    .map(|overlay| Ok(overlay.text.clone()))
                    .unwrap_or_else(|| fs::read_to_string(&path).map(Arc::from));
                match text {
                    Ok(text) => {
                        active_standalone_paths.insert(path.clone());
                        // A successful read supersedes any prior read error.
                        standalone_reads.remove(&path);
                        if let Some(cache) = reusable.remove(&path)
                            && standalone_matches(&cache, &path, &text)
                        {
                            standalone.insert(path, cache);
                            continue;
                        }
                        let sources = ast::SourceMap::new();
                        sources.push_back(&path, text.to_string());
                        standalone.insert(
                            path,
                            CompilationCache::from_sources(
                                self.compilation_cache.borrow_base().clone(),
                                sources,
                            )?,
                        );
                    }
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                    Err(error) => {
                        active_standalone_paths.insert(path.clone());
                        record_source_read_error(
                            &path,
                            &error,
                            &mut standalone_reads,
                            &mut io_warnings,
                        );
                    }
                }
            }
            // Drop read errors for paths that are no longer active.
            standalone_reads.retain(|path, _| active_standalone_paths.contains(path));
            self.standalone = standalone;
            self.standalone_paths = active_standalone_paths;
        }

        if reload {
            self.missing_roots = missing_roots;
        }
        workspace_reads.extend(standalone_reads);
        self.diagnostics.set_reads(workspace_reads);
        self.io_warnings = io_warnings;
        if reload {
            self.log_missing_roots(ctx);
        } else {
            self.log_io_warnings(ctx);
        }
        Ok(())
    }

    /// Collects and publishes diagnostics synchronously. Kept for unit tests;
    /// production code publishes through the protocol thread instead.
    #[cfg(test)]
    pub fn publish_all_diagnostics(&mut self, ctx: &LspContext) -> anyhow::Result<()> {
        if let Some(batch) = self.collect_diagnostic_batch(ctx, || false)? {
            self.diagnostics.apply_and_flush(batch, ctx)?;
        }
        Ok(())
    }

    /// Computes every diagnostic the server would publish without mutating the
    /// client-visible publication state.
    ///
    /// `should_abort` is consulted between expensive phases so a superseded
    /// computation can stop early and return `Ok(None)`; in that case the
    /// quick-fix cache is still refreshed for whatever was analyzed, but no
    /// batch is produced and nothing is published.
    pub(crate) fn collect_diagnostic_batch(
        &mut self,
        ctx: &LspContext,
        should_abort: impl Fn() -> bool,
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        let abort = || should_abort() || crate::server::is_cancelled();
        if abort() {
            return Ok(None);
        }
        let mut collected = CompilerDiagnostics {
            by_file: BTreeMap::new(),
            fixes: std::mem::take(&mut self.cached_fixes),
        };
        self.check_workspace(|unit, symbols, diagnostics, sources| {
            self.collect_compiler_diagnostics(
                Some(unit),
                diagnostics,
                symbols,
                sources,
                &mut collected,
                ctx,
            )
        })?;
        if abort() {
            self.cached_fixes = collected.fixes;
            return Ok(None);
        }
        for cache in self.standalone.values() {
            cache.analyze(|unit, symbols, diagnostics, sources| {
                self.collect_compiler_diagnostics(
                    Some(unit),
                    diagnostics,
                    symbols,
                    sources,
                    &mut collected,
                    ctx,
                )
            })?;
            if abort() {
                self.cached_fixes = collected.fixes;
                return Ok(None);
            }
        }
        self.cached_fixes = collected.fixes;
        self.prune_cached_fixes();
        let versions = self
            .overlays
            .iter()
            .map(|(path, overlay)| (path.clone(), overlay.version))
            .collect();
        Ok(Some(self.diagnostics.snapshot(collected.by_file, versions)))
    }

    /// Drops cached quick fixes for files that are no longer part of any
    /// analyzed snapshot, so removing a workspace folder or deleting a file
    /// does not leave its fixes addressable.
    fn prune_cached_fixes(&mut self) {
        let workspace = self.compilation_cache.borrow_file_ids();
        let standalone = &self.standalone;
        let overlays = &self.overlays;
        self.cached_fixes.retain(|path, _| {
            workspace.contains_key(path)
                || standalone.contains_key(path)
                || overlays.contains_key(path)
        });
    }

    /// The most specific workspace root containing `path`, together with the
    /// workspace folder that owns it and the loaded workspace.
    fn find_workspace_root(&self, path: &Path) -> Option<(&PathBuf, &PathBuf, &WorkspaceDir)> {
        self.workspaces
            .iter()
            .flat_map(|(owner, workspace)| {
                workspace
                    .roots
                    .iter()
                    .map(move |root| (root, owner, workspace))
            })
            .filter(|(root, _, _)| path.starts_with(root))
            .max_by(|(a, owner_a, _), (b, owner_b, _)| {
                a.components()
                    .count()
                    .cmp(&b.components().count())
                    .then_with(|| owner_b.cmp(owner_a))
            })
    }

    fn resolve_file(&self, path: &Path) -> FileResolution<'_> {
        let path = dialect::identity(path);
        if let Some((root, _, workspace)) = self.find_workspace_root(&path) {
            // An open buffer that the workspace walk would skip is treated as
            // if it had never been discovered, so it cannot be injected into
            // workspace compilation.
            if dialect::is_ignored(&path, root) {
                FileResolution::NonWorkspace
            } else {
                FileResolution::Workspace(workspace)
            }
        } else {
            FileResolution::NonWorkspace
        }
    }

    /// Returns the workspace folder key (the key of `self.workspaces`) that owns
    /// `path`, or `None` when the path is not part of any workspace.
    fn workspace_root_for(&self, path: &Path) -> Option<PathBuf> {
        let path = dialect::identity(path);
        let (root, owner, _) = self.find_workspace_root(&path)?;
        (!dialect::is_ignored(&path, root)).then(|| owner.clone())
    }

    fn hover_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Option<lsp::Hover>> {
        cancelled()?;
        if Dialect::of(loc.doc().path()).is_none() {
            return Ok(None);
        }
        let requested_pos = loc.pos();
        self.expr_at(
            loc,
            |at| {
                let range = at.expr().and_then(|e| {
                    let span = e.span();
                    range(span, at.sources().get(span.file)?)
                });
                let span = at.expr().map(redscript_compiler_api::ir::Expr::span);
                let value = at.display().to_string();
                ctx.logger().verbose(|| {
                    format!(
                        "Hover payload content_bytes={} range={range:?} expression_span={span:?} contains_requested_byte={:?}",
                        value.len(),
                        span.map(|span| span.contains(requested_pos)),
                    )
                });
                if value.is_empty() {
                    return Ok(None);
                }
                Ok(Some(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value,
                    }),
                    range,
                }))
            },
            ctx,
        )
    }

    fn completion_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp::CompletionResponse>> {
        cancelled()?;
        if Dialect::of(loc.doc().path()).is_none() {
            return Ok(Arc::new(lsp::CompletionResponse::Array(vec![])));
        }
        let contents = loc.doc().buffer().contents().to_string();
        let Some(member) = member_completion_range(&contents, loc.pos()) else {
            ctx.logger().verbose(|| {
                format!(
                    "Completion empty path={} byte_offset={}: cursor is not after a member-access dot (only member completion is supported)",
                    loc.doc().path().display(),
                    loc.pos(),
                )
            });
            return Ok(Arc::new(lsp::CompletionResponse::Array(vec![])));
        };
        ctx.logger().verbose(|| {
            format!(
                "Completion receiver path={} byte_offset={} erased_member_range={member:?}",
                loc.doc().path().display(),
                loc.pos(),
            )
        });
        let path = dialect::identity(loc.doc().path());
        let receiver_end = member.start;
        if let Some(cached) = self.completion_cache.lock().unwrap().as_ref()
            && cached.path == path
            && cached.receiver_end == receiver_end
            && cached.source_without_member.len() == contents.len() - member.len()
            && cached
                .source_without_member
                .starts_with(&contents[..receiver_end])
            && cached
                .source_without_member
                .ends_with(&contents[member.end..])
            && overlays_match(&cached.other_overlays, &self.overlays, &path)
        {
            ctx.logger()
                .verbose(|| "Completion analysis=reused member results".to_owned());
            return Ok(cached.response.clone());
        }
        // Only allocate the stripped source and overlay snapshot on a miss.
        let source_without_member = {
            let mut stripped = contents.clone();
            stripped.replace_range(member.clone(), "");
            stripped
        };
        let other_overlays: BTreeMap<_, _> = self
            .overlays
            .iter()
            .filter(|(other, _)| **other != path)
            .map(|(path, overlay)| (path.clone(), overlay.text.clone()))
            .collect();
        let dialect = Dialect::of(loc.doc().path());
        // A client that can resolve documentation lazily gets it omitted from
        // the response and looked up again from the cache on
        // `completionItem/resolve`.
        let resolve = ctx
            .supports_lazy_completion_documentation()
            .then(|| self.completion_generation.fetch_add(1, Ordering::Relaxed));
        let generated = self.expr_at_with(
            loc.with_pos(member.start as u32),
            Some(member),
            Some(contents),
            |at| generate_completions(at, dialect, resolve),
            ctx,
        )?;
        let response = Arc::new(lsp::CompletionResponse::Array(generated.items));
        *self.completion_cache.lock().unwrap() = Some(CachedCompletion {
            path,
            receiver_end,
            source_without_member,
            other_overlays,
            response: response.clone(),
            docs: generated.docs,
            generation: resolve.unwrap_or(0),
            adapted: None,
        });
        Ok(response)
    }

    /// Returns the completion response adapted to the client's presentation
    /// capabilities, reusing the adapted form across prefix edits at the same
    /// receiver. The raw response stays cached so the trait's [`completion`]
    /// remains independent of client capabilities.
    ///
    /// [`completion`]: LanguageServer::completion
    fn completion_for_client(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp::CompletionResponse>> {
        let response = self.completion_at(loc, ctx)?;
        let presentation = ctx.completion_presentation();
        let mut cache = self.completion_cache.lock().unwrap();
        if let Some(cached) = cache.as_mut()
            && Arc::ptr_eq(&cached.response, &response)
            && let Some((key, adapted)) = &cached.adapted
            && *key == presentation
        {
            return Ok(adapted.clone());
        }
        let mut adapted = response.as_ref().clone();
        ctx.adapt_completion(&mut adapted);
        let adapted = Arc::new(adapted);
        if let Some(cached) = cache.as_mut()
            && Arc::ptr_eq(&cached.response, &response)
        {
            cached.adapted = Some((presentation, adapted.clone()));
        }
        Ok(adapted)
    }

    /// Populates the documentation deferred out of a completion item.
    ///
    /// The item's `data` carries the generation of the response it came from and
    /// an index into that response's deferred docs. A resolve against a
    /// superseded generation (the cache moved on) leaves the item untouched
    /// rather than returning the wrong documentation. Payloads this server did
    /// not attach are preserved so a client or proxy relying on them keeps them.
    fn completion_resolve(
        &self,
        mut item: lsp::CompletionItem,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::CompletionItem> {
        let Some(data) = item.data.as_ref() else {
            return Ok(item);
        };
        let Ok(resolve) = <CompletionResolveData as serde::Deserialize>::deserialize(data) else {
            return Ok(item);
        };
        item.data = None;
        let cache = self.completion_cache.lock().unwrap();
        let Some(cached) = cache.as_ref() else {
            return Ok(item);
        };
        if cached.generation != resolve.generation {
            return Ok(item);
        }
        let Some(doc) = cached.docs.get(resolve.id) else {
            return Ok(item);
        };
        item.documentation = Some(lsp::Documentation::MarkupContent(lsp::MarkupContent {
            kind: lsp::MarkupKind::Markdown,
            value: doc.clone(),
        }));
        ctx.adapt_completion_item(&mut item);
        Ok(item)
    }

    fn definition_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
        cancelled()?;
        if Dialect::of(loc.doc().path()).is_none() {
            return Ok(lsp::GotoDefinitionResponse::Array(vec![]));
        }
        self.expr_at(
            loc,
            |at| {
                let locations = at
                    .definition_span()
                    .and_then(|span| Some(vec![location(span, at.sources(), ctx)?]))
                    .unwrap_or_default();
                Ok(locations.into())
            },
            ctx,
        )
    }

    fn workspace_symbols(
        &self,
        query: &str,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::WorkspaceSymbolResponse> {
        cancelled()?;
        self.check_workspace(|_, syms, _, sources| {
            let funcs = syms
                .free_functions()
                .filter_map(|e| Some((*e.name(), e.func().span()?)))
                .filter(|(name, _)| name.as_ref().last().is_some_and(|n| n.contains(query)))
                .filter_map(|(name, span)| {
                    #[allow(deprecated)]
                    Some(lsp::SymbolInformation {
                        name: name.to_string(),
                        kind: lsp::SymbolKind::FUNCTION,
                        tags: None,
                        deprecated: None,
                        location: location(span, sources, ctx)?,
                        container_name: None,
                    })
                });
            let mut results = Vec::new();
            for (id, def) in syms.types() {
                cancelled()?;
                let kind = match def.schema() {
                    TypeSchema::Aggregate(agg) if agg.flags().is_struct() => {
                        lsp::SymbolKind::STRUCT
                    }
                    TypeSchema::Aggregate(_) => lsp::SymbolKind::CLASS,
                    TypeSchema::Enum(_) => lsp::SymbolKind::ENUM,
                    TypeSchema::Primitive => continue,
                };
                let mut push =
                    |name: &str, kind, span: Option<Span>, container: Option<TypeId<'_>>| {
                        if name.contains(query)
                            && let Some(location) =
                                span.and_then(|span| location(span, sources, ctx))
                        {
                            #[allow(deprecated)]
                            results.push(lsp::SymbolInformation {
                                name: name.to_owned(),
                                kind,
                                tags: None,
                                deprecated: None,
                                location,
                                container_name: container.map(|id| id.to_string()),
                            });
                        }
                    };
                push(&id.as_str(), kind, def.span(), None);
                if let TypeSchema::Aggregate(agg) = def.schema() {
                    for method in agg.methods().iter() {
                        push(
                            method.name(),
                            lsp::SymbolKind::METHOD,
                            method.func().span(),
                            Some(id),
                        );
                    }
                    for field in agg.fields().iter() {
                        push(
                            field.name(),
                            lsp::SymbolKind::FIELD,
                            field.field().span(),
                            Some(id),
                        );
                    }
                }
            }
            results.extend(funcs);
            Ok(lsp::WorkspaceSymbolResponse::Flat(results))
        })
    }

    pub(crate) fn document_symbols(
        &self,
        doc: Document<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::DocumentSymbolResponse> {
        cancelled()?;
        Ok(lsp::DocumentSymbolResponse::Nested(
            document_symbols::document_symbols(doc.path(), doc.buffer().contents().to_string()),
        ))
    }

    fn format_document(
        &self,
        doc: Document<'_>,
        tab_size: u16,
        ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp::TextEdit>> {
        cancelled()?;
        let Some(dialect) = Dialect::of(doc.path()) else {
            return Ok(vec![]);
        };
        let resolved = self.resolve_file(doc.path());
        let mut settings = resolved
            .as_workspace()
            .map(|ws| ws.format_settings.clone())
            .unwrap_or(FormatSettings::DEFAULT);
        // Fall back to the indentation the client negotiated when `.redscript`
        // does not explicitly configure `format.indent`.
        if resolved
            .as_workspace()
            .is_none_or(|ws| ws.format_indent.is_none())
        {
            settings.indent = tab_size.max(1);
        }

        let contents = doc.buffer().contents();
        let map = ast::SourceMap::new();
        let id = map.push_back(doc.path(), contents.to_string());
        let file = map.get(id).context("missing source file")?;

        let formatted = match dialect {
            Dialect::Redmod => match redscript_redmod_formatter::format_source(file.source(), id) {
                Ok(formatted) => Some(formatted),
                Err(errors) => {
                    if let Some(first) = errors.first() {
                        ctx.logger().verbose(|| {
                            format!(
                                "Formatting skipped path={}: syntax error ({first})",
                                doc.path().display(),
                            )
                        });
                    }
                    None
                }
            },
            Dialect::Community => {
                let (module, errors) = format_document(file.source(), id, &settings);
                if let Some(first) = errors.first() {
                    ctx.logger().verbose(|| {
                        format!(
                            "Formatting skipped path={}: syntax error ({first})",
                            doc.path().display(),
                        )
                    });
                    None
                } else {
                    module.map(|module| module.to_string())
                }
            }
        };
        if let Some(formatted) = formatted
            && formatted != file.source()
        {
            let end = crate::positions::position(file.source(), file.source().len() as u32)
                .context("invalid document end")?;
            let edit =
                lsp::TextEdit::new(lsp::Range::new(lsp::Position::new(0, 0), end), formatted);
            return Ok(vec![edit]);
        };

        Ok(vec![])
    }

    fn code_action(
        &self,
        path: PathBuf,
        span: Range<u32>,
        context: lsp::CodeActionContext,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeActionResponse> {
        cancelled()?;
        if Dialect::of(&path) != Some(Dialect::Community)
            || context.only.as_ref().is_some_and(|kinds| {
                !kinds
                    .iter()
                    .any(|kind| kind.as_str().is_empty() || kind == &lsp::CodeActionKind::QUICKFIX)
            })
        {
            return Ok(vec![]);
        }
        let path = dialect::identity(&path);
        let scope = match self.workspace_root_for(&path) {
            Some(ws_root) => FixScope::Workspace(ws_root),
            None => FixScope::Standalone(path.clone()),
        };
        let fixes = &self.cached_fixes;

        let mut actions = Vec::new();
        let mut fix_all_kinds = Vec::new();
        let mut fix_hashes = HashMap::new();
        for (&(start, end), slot_fixes) in
            fixes.get(&path).into_iter().flat_map(|fixes| fixes.iter())
        {
            let matches = if span.is_empty() {
                start == span.start || (start < span.start && span.start < end)
            } else if start == end {
                span.contains(&start)
            } else {
                start < span.end && span.start < end
            };
            if !matches {
                continue;
            }
            for fix in slot_fixes {
                if !self.valid_fix_cached(fix, &mut fix_hashes) {
                    continue;
                }
                let matched_diagnostics = fix.matching_diagnostics(&context.diagnostics);
                let fix_one = lsp::CodeAction {
                    title: fix.title(),
                    kind: Some(lsp::CodeActionKind::QUICKFIX),
                    diagnostics: (!matched_diagnostics.is_empty()).then_some(matched_diagnostics),
                    data: Some(serde_json::to_value(FixRequest::FixOne(Box::new(
                        fix.clone(),
                    )))?),
                    is_preferred: Some(true),
                    ..Default::default()
                };

                actions.push(fix_one);
                if fix_all_kinds.contains(&fix.kind) {
                    continue;
                }
                fix_all_kinds.push(fix.kind);
                let fix_all_title = match &scope {
                    FixScope::Workspace(_) => {
                        format!("{} - fix all in the workspace", fix.kind)
                    }
                    FixScope::Standalone(_) => format!("{} - fix all in this file", fix.kind),
                };
                let fix_all = lsp::CodeAction {
                    title: fix_all_title,
                    kind: Some(lsp::CodeActionKind::QUICKFIX),
                    data: Some(serde_json::to_value(FixRequest::FixAll(
                        fix.kind,
                        self.generation,
                        scope.clone(),
                    ))?),
                    ..Default::default()
                };

                actions.push(fix_all);
            }
        }
        actions
            .into_iter()
            .map(|action| {
                let action = if ctx.supports_code_action_resolve_edit() {
                    action
                } else {
                    let mut action = self.code_action_resolve(action, ctx)?;
                    action.data = None;
                    action
                };
                Ok(lsp::CodeActionOrCommand::CodeAction(action))
            })
            .collect()
    }

    fn code_action_resolve(
        &self,
        mut action: lsp::CodeAction,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeAction> {
        cancelled()?;
        let Some(data) = action.data.clone() else {
            return Ok(action);
        };

        let request: FixRequest = serde_json::from_value(data)?;
        match request {
            FixRequest::FixOne(fix) if self.valid_fix(&fix) => {
                action.edit = Some(self.workspace_edit(vec![(fix.uri, vec![fix.edit])], ctx));
            }
            FixRequest::FixAll(kind, _generation, scope) => {
                let mut unique = HashSet::<(lsp::Uri, lsp::Range)>::new();

                #[allow(clippy::mutable_key_type)]
                let mut edits = std::collections::HashMap::<lsp::Uri, Vec<lsp::TextEdit>>::new();
                let fixes = &self.cached_fixes;
                let mut selected = Vec::new();
                match &scope {
                    FixScope::Standalone(target) => {
                        if let Some(slots) = fixes.get(target) {
                            selected.extend(slots.values().flatten());
                        }
                    }
                    FixScope::Workspace(ws_root) => {
                        for (path, slots) in fixes {
                            if self.workspace_root_for(path).as_ref() == Some(ws_root) {
                                selected.extend(slots.values().flatten());
                            }
                        }
                    }
                }
                let mut file_hashes = HashMap::new();
                for fix in selected
                    .into_iter()
                    .filter(|f| f.kind == kind)
                    .filter(|f| self.valid_fix_cached(f, &mut file_hashes))
                {
                    if unique.insert((fix.uri.clone(), fix.edit.range)) {
                        edits
                            .entry(fix.uri.clone())
                            .or_default()
                            .push(fix.edit.clone());
                    }
                }

                if edits.is_empty() {
                    self.disable_action(&mut action, STALE_FIX_REASON, ctx);
                } else {
                    action.edit = Some(self.workspace_edit(edits.into_iter().collect(), ctx));
                }
            }
            other => {
                let reason = match other {
                    FixRequest::FixOne(fix)
                        if crate::server::path_from_uri(&fix.uri)
                            .is_ok_and(|path| Dialect::of(&path) != Some(Dialect::Community)) =>
                    {
                        "Quick fixes are only supported for community REDscript (.reds) files"
                    }
                    _ => STALE_FIX_REASON,
                };
                self.disable_action(&mut action, reason, ctx);
            }
        }
        Ok(action)
    }

    /// Marks an action whose edits are no longer applicable, using the
    /// disabled-action capability the client negotiated when supported.
    fn disable_action(&self, action: &mut lsp::CodeAction, reason: &'static str, ctx: &LspContext) {
        action.edit = None;
        if ctx.supports_code_action_disabled() {
            action.disabled = Some(lsp::CodeActionDisabled {
                reason: reason.into(),
            });
        }
    }

    fn workspace_edit(
        &self,
        edits: Vec<(lsp::Uri, Vec<lsp::TextEdit>)>,
        ctx: &LspContext,
    ) -> lsp::WorkspaceEdit {
        let mut edits = edits;
        edits.sort_by(|(a, _), (b, _)| a.as_str().cmp(b.as_str()));
        for (_, file_edits) in &mut edits {
            sort_text_edits(file_edits);
        }
        if ctx.supports_workspace_edit_document_changes() {
            lsp::WorkspaceEdit {
                document_changes: Some(lsp::DocumentChanges::Edits(
                    edits
                        .into_iter()
                        .map(|(uri, edits)| {
                            let version = crate::server::path_from_uri(&uri)
                                .ok()
                                .and_then(|path| self.overlays.get(&dialect::identity(&path)))
                                .map(|overlay| overlay.version);
                            lsp::TextDocumentEdit {
                                text_document: lsp::OptionalVersionedTextDocumentIdentifier {
                                    uri,
                                    version,
                                },
                                edits: edits.into_iter().map(lsp::OneOf::Left).collect(),
                            }
                        })
                        .collect(),
                )),
                ..Default::default()
            }
        } else {
            #[allow(clippy::mutable_key_type)]
            let changes = edits.into_iter().collect();
            lsp::WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }
        }
    }

    fn valid_fix(&self, fix: &Fix) -> bool {
        let mut hashes = HashMap::new();
        self.valid_fix_cached(fix, &mut hashes)
    }

    /// Like [`valid_fix`], but memoizes the on-disk content hash per path so a
    /// fix-all over many fixes in the same unchanged file reads it once.
    fn valid_fix_cached(&self, fix: &Fix, hashes: &mut HashMap<PathBuf, Option<u64>>) -> bool {
        let Ok(path) = crate::server::path_from_uri(&fix.uri) else {
            return false;
        };
        if Dialect::of(&path) != Some(Dialect::Community) {
            return false;
        }
        let path = dialect::identity(&path);
        if let Some(overlay) = self.overlays.get(&path) {
            return content_hash(overlay.text.as_bytes()) == fix.expected_source_hash;
        }
        let hash = *hashes
            .entry(path.clone())
            .or_insert_with(|| fs::read(&path).ok().map(|bytes| content_hash(&bytes)));
        hash == Some(fix.expected_source_hash)
    }

    pub fn log_missing_roots(&mut self, ctx: &LspContext) {
        let configured: HashSet<_> = self
            .workspaces
            .values()
            .flat_map(|workspace| workspace.roots.iter())
            .collect();
        for root in self.reported_missing_roots.difference(&self.missing_roots) {
            if configured.contains(root) {
                ctx.logger().info(format!(
                    "Source root '{}' is available again.",
                    root.display()
                ));
            }
        }
        drop(configured);
        for root in self.missing_roots.difference(&self.reported_missing_roots) {
            let message = format!(
                "Source root '{}' is missing; treating it as temporarily empty. Restore/create the directory or update source_roots in .redscript, then refresh the workspace.",
                root.display()
            );
            ctx.logger().warn(&message);
            ctx.show_message(lsp::MessageType::WARNING, message);
        }
        self.reported_missing_roots.clone_from(&self.missing_roots);
        self.log_config_warnings(ctx);
        self.log_io_warnings(ctx);
    }

    fn log_io_warnings(&mut self, ctx: &LspContext) {
        for warning in self.io_warnings.difference(&self.reported_io_warnings) {
            ctx.logger().warn(warning);
            ctx.show_message(lsp::MessageType::WARNING, warning);
        }
        self.reported_io_warnings.clone_from(&self.io_warnings);
    }

    fn log_config_warnings(&mut self, ctx: &LspContext) {
        for warning in self
            .config_warnings
            .difference(&self.reported_config_warnings)
        {
            ctx.logger().warn(warning);
            ctx.show_message(lsp::MessageType::WARNING, warning);
        }
        self.reported_config_warnings
            .clone_from(&self.config_warnings);
    }

    fn reload_workspaces(&mut self) {
        self.reload_workspaces_with(&HashMap::new());
    }

    /// Reloads workspace configuration for every loaded folder, reusing the
    /// results in `preloaded` for folders the caller has already loaded.
    fn reload_workspaces_with(&mut self, preloaded: &HashMap<PathBuf, LoadedWorkspace>) {
        dialect::clear_identity_cache();
        let mut workspaces = HashMap::with_capacity(self.workspaces.len());
        let mut config_diagnostics = BTreeMap::new();
        let mut config_warnings = BTreeSet::new();
        for (dir, previous) in &self.workspaces {
            let loaded = match preloaded.get(dir) {
                Some(loaded) => loaded.clone(),
                None => WorkspaceDir::load(dir, Some(previous)),
            };
            if let Some(diagnostic) = loaded.diagnostic {
                config_diagnostics.insert(loaded.dotfile_path, diagnostic);
            }
            config_warnings.extend(loaded.warnings);
            workspaces.insert(dir.clone(), loaded.workspace);
        }
        self.workspaces = workspaces;
        self.diagnostics.set_configs(config_diagnostics);
        self.config_warnings = config_warnings;
    }

    fn check_workspace<A>(
        &self,
        mut cb: impl for<'ctx> FnMut(
            &LoweredCompilationUnit<'ctx>,
            &Symbols<'ctx>,
            &[&Diagnostic<'ctx>],
            &'ctx ast::SourceMap,
        ) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        self.compilation_cache.analyze(&mut cb)
    }

    #[cfg(test)]
    fn publish_diagnostics<'ctx>(
        &mut self,
        diags: &[&Diagnostic<'ctx>],
        symbols: &Symbols<'ctx>,
        sources: &'ctx ast::SourceMap,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        let mut batch = CompilerDiagnostics {
            by_file: BTreeMap::new(),
            fixes: std::mem::take(&mut self.cached_fixes),
        };
        self.collect_compiler_diagnostics(None, diags, symbols, sources, &mut batch, ctx)?;
        self.cached_fixes = batch.fixes;
        self.diagnostics.set_compiler(batch.by_file);
        let overlays = &self.overlays;
        self.diagnostics.flush(ctx, |path| {
            overlays.get(path).map(|overlay| overlay.version)
        })
    }

    /// Converts the compiler diagnostics produced for `sources` into LSP
    /// diagnostics, refreshing the quick-fix cache for every analyzed file,
    /// and accumulates them into `batch` for a later `flush`.
    fn collect_compiler_diagnostics<'ctx>(
        &self,
        unit: Option<&LoweredCompilationUnit<'ctx>>,
        diags: &[&Diagnostic<'ctx>],
        symbols: &Symbols<'ctx>,
        sources: &'ctx ast::SourceMap,
        batch: &mut CompilerDiagnostics,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        let mut file_diags = HashMap::new();
        for diag in diags {
            let file = diag.span().file;
            if sources
                .get(file)
                .is_some_and(|f| Dialect::of(f.path()).is_some())
            {
                file_diags.entry(file).or_insert_with(Vec::new).push(diag);
            }
        }

        for (_, file) in sources.files() {
            batch.fixes.remove(file.path());
        }

        for (file, diags) in file_diags {
            let file = sources.get(file).context("missing source file")?;
            let path = file.path();

            let slots = batch.fixes.entry_ref(path).or_default();
            for diag in &diags {
                if Dialect::of(path) == Some(Dialect::Community)
                    && let Some(fix) = generate_fix(diag, file, unit, symbols, sources, ctx)
                {
                    let span = diag.span();
                    slots.entry((span.start, span.end)).or_default().push(fix);
                }
            }

            let mut diags: Vec<_> = diags
                .into_iter()
                .map(|diag| (diag, diag.to_string()))
                .collect();
            diags.sort_by(|(a, message_a), (b, message_b)| {
                let a_span = a.span();
                let b_span = b.span();
                (a_span.start, a_span.end, a.code(), message_a).cmp(&(
                    b_span.start,
                    b_span.end,
                    b.code(),
                    message_b,
                ))
            });
            let diagnostics = diags
                .into_iter()
                .filter_map(|(diag, message)| {
                    let is_redmod_capability = diag.code() == "REDMOD_CAPABILITY";
                    let message = if is_redmod_capability {
                        format!("unsupported by IDE REDmod analyzer: {message}")
                    } else {
                        message
                    };
                    let code_description = is_redmod_capability
                        .then(|| REDMOD_SUBSET_DOCS_URL.parse().ok())
                        .flatten()
                        .map(|href| lsp::CodeDescription { href });
                    Some(lsp::Diagnostic {
                        range: range(diag.span(), file)?,
                        severity: Some(if diag.is_error() {
                            lsp::DiagnosticSeverity::ERROR
                        } else {
                            lsp::DiagnosticSeverity::WARNING
                        }),
                        code: Some(lsp::NumberOrString::String(diag.code().into())),
                        code_description,
                        source: Some("redscript".into()),
                        message,
                        related_information: {
                            let related: Vec<_> = diag
                                .related_locations()
                                .filter_map(|(span, message)| {
                                    let file = sources.get(span.file)?;
                                    Some(lsp::DiagnosticRelatedInformation {
                                        location: lsp::Location {
                                            uri: ctx.uri(file.path()).ok()?,
                                            range: range(span, file)?,
                                        },
                                        message: message.into(),
                                    })
                                })
                                .collect();
                            (!related.is_empty()).then_some(related)
                        },
                        tags: diagnostic_tags(diag, ctx),
                        ..Default::default()
                    })
                })
                .collect();
            batch.by_file.insert(path.to_owned(), diagnostics);
        }
        Ok(())
    }

    fn expr_at<A>(
        &self,
        loc: CodeLocation<'_>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        self.expr_at_with(loc, None, None, cb, ctx)
    }

    #[cfg(test)]
    fn patched_expr_at<A>(
        &self,
        loc: CodeLocation<'_>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        let pos = loc.pos() as usize;
        let contents = loc.doc().buffer().contents().to_string();
        let end = contents
            .get(pos..)
            .and_then(|suffix| suffix.chars().next())
            .map_or(pos, |ch| pos + ch.len_utf8());
        self.expr_at_with(loc, Some(pos..end), None, cb, ctx)
    }

    fn expr_at_with<A>(
        &self,
        loc: CodeLocation<'_>,
        patch: Option<Range<usize>>,
        source: Option<String>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        anyhow::ensure!(
            Dialect::of(loc.doc().path()).is_some(),
            "unsupported source extension"
        );
        let path = dialect::identity(loc.doc().path());
        let workspace = self.resolve_file(&path).as_workspace().is_some();
        let base = if workspace {
            &self.compilation_cache
        } else {
            self.standalone
                .get(&path)
                .unwrap_or(&self.compilation_cache)
        };
        ctx.logger().verbose(|| {
            format!(
                "Query path={} dialect={:?} byte_offset={} version={:?} workspace={} generation={} source_files={} overlays={} speculative_patch={patch:?}",
                path.display(),
                Dialect::of(&path),
                loc.pos(),
                loc.doc().buffer().version(),
                workspace,
                self.generation,
                base.borrow_file_ids().len(),
                self.overlays.len(),
            )
        });
        if patch.is_none()
            && let Some(&id) = base.borrow_file_ids().get(&path)
            && base.with_sources(|sources| {
                sources
                    .get(id)
                    .is_some_and(|file| file.source() == loc.doc().buffer().contents())
            })
        {
            ctx.logger()
                .verbose(|| "Query analysis=reused active snapshot".to_owned());
            return base.query_at(id, loc.pos(), cb, ctx);
        }
        ctx.logger()
            .verbose(|| "Query analysis=speculative snapshot".to_owned());
        // A query owns its storage; speculative sources never enter the active cache.
        let sources = ast::SourceMap::new();
        if workspace {
            // Every active file path was canonicalized when it entered the
            // cache, so skip the current file by id instead of re-resolving it.
            let skip = base.borrow_file_ids().get(&path).copied();
            base.with_sources(|active| {
                for (id, file) in active.files() {
                    if Some(id) != skip && base.borrow_file_ids().contains_key(file.path()) {
                        sources.push_back(file.path(), file.source());
                    }
                }
            });
        }
        let mut contents = source.unwrap_or_else(|| loc.doc().buffer().contents().to_string());
        if let Some(range) = patch {
            contents.replace_range(range.clone(), &" ".repeat(range.len()));
            if Dialect::of(&path) == Some(Dialect::Redmod)
                && repair_completion_terminator(&mut contents, range)
            {
                ctx.logger().verbose(|| {
                    "Query recovery=inserted speculative REDmod statement terminator".to_owned()
                });
            }
        }

        let id = sources.push_back(&path, contents);
        cancelled()?;
        // `query_at` selects the enclosing function at `loc.pos().saturating_sub(1)`,
        // so the single-function lowering must use the same byte.
        let target = QueryTarget {
            file: id,
            pos: loc.pos().saturating_sub(1),
        };
        let query_cache = CompilationCache::from_sources_with(
            base.borrow_base().clone(),
            sources,
            AnalysisMode::Query(target),
        )?;
        cancelled()?;
        query_cache.query_at(id, loc.pos(), cb, ctx)
    }

    /// Apply an overlay for a single document without triggering a rebuild.
    ///
    /// Returns `Some(reload)` for a supported dialect (`reload` is true when the
    /// buffer was closed and disk contents must be restored), or `None` for a
    /// path whose dialect is unsupported.
    fn apply_document(
        &mut self,
        path: &Path,
        buffer: Option<&crate::buffers::Buffer>,
        ctx: &LspContext,
    ) -> Option<bool> {
        let path = dialect::identity(path);
        if Dialect::of(&path).is_none() {
            if buffer.is_some()
                && let Some(ext) = path.extension().and_then(|ext| ext.to_str())
                && ext.eq_ignore_ascii_case("reds")
                && self.warned_extensions.insert(path.clone())
            {
                let message = format!(
                    "File '{}' uses uppercase extension '.{ext}'; community REDscript requires lowercase '.reds', so this file will not be analyzed.",
                    path.display(),
                );
                ctx.logger().warn(&message);
                ctx.show_message(lsp::MessageType::WARNING, message);
            }
            return None;
        }
        if let Some(buffer) = buffer {
            self.overlays.insert(
                path,
                Overlay {
                    text: Arc::from(buffer.contents().to_string()),
                    version: buffer.version().unwrap_or(0),
                },
            );
        } else {
            self.overlays.remove(&path);
            self.standalone.remove(&path);
            self.standalone_paths.remove(&path);
            // A closed standalone file is no longer analyzed, so its cached
            // quick fixes would otherwise linger forever.
            if self.resolve_file(&path).as_workspace().is_none() {
                self.cached_fixes.remove(&path);
            }
        }
        Some(buffer.is_none())
    }
}

impl LanguageServer for RedscriptLanguageServer {
    fn watch_roots(&self) -> Vec<PathBuf> {
        let mut roots: Vec<_> = self
            .workspaces
            .keys()
            .map(|path| dialect::identity(path))
            .chain(
                self.workspaces
                    .values()
                    .flat_map(|workspace| workspace.roots.clone()),
            )
            .collect();
        roots.sort();
        roots.dedup();
        roots
    }

    fn document_changed(
        &mut self,
        path: PathBuf,
        buffer: Option<&crate::buffers::Buffer>,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        // Closing a workspace document must restore disk contents; closing a
        // standalone document just drops it. Open/change use known sources;
        // filesystem notifications refresh discovery.
        let workspace = self.is_workspace_document(&path);
        if self.apply_document(&path, buffer, ctx).is_some() {
            let reload = buffer.is_none() && workspace;
            self.rebuild_scoped(ctx, reload, workspace, !workspace)?;
        }
        Ok(())
    }

    fn documents_changed(
        &mut self,
        documents: &[(PathBuf, Option<&crate::buffers::Buffer>)],
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        // Apply every overlay first so a batch of edits sharing sources rebuilds
        // exactly once instead of once per changed document, and only rebuild the
        // partitions the batch actually touched.
        let mut workspace_dirty = false;
        let mut standalone_dirty = false;
        let mut reload = false;
        let mut applied = false;
        for (path, buffer) in documents {
            let workspace = self.is_workspace_document(path);
            if self.apply_document(path, *buffer, ctx).is_some() {
                applied = true;
                reload |= buffer.is_none() && workspace;
                workspace_dirty |= workspace;
                standalone_dirty |= !workspace;
            }
        }
        if applied {
            self.rebuild_scoped(ctx, reload, workspace_dirty, standalone_dirty)?;
        }
        Ok(())
    }

    fn files_changed(&mut self, _paths: Vec<PathBuf>, ctx: &LspContext) -> anyhow::Result<()> {
        dialect::clear_identity_cache();
        self.reload_workspaces();
        self.rebuild(ctx)
    }

    fn check(
        &mut self,
        path: PathBuf,
        ctx: &LspContext,
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        self.check_cancellable(path, ctx, &|| false)
    }

    fn check_cancellable(
        &mut self,
        path: PathBuf,
        ctx: &LspContext,
        should_abort: &(dyn Fn() -> bool + Sync),
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        if path.file_name().is_some_and(|name| name == ".redscript") {
            // Reload configuration before the abort check so a superseded check
            // that still runs is cheap; the newer check reuses the reload.
            dialect::clear_identity_cache();
            self.reload_workspaces();
            if should_abort() {
                return Ok(None);
            }
            self.rebuild(ctx)?;
            return self.collect_diagnostic_batch(ctx, should_abort);
        }

        let path = dialect::identity(&path);
        if self.resolve_file(&path).as_workspace().is_none()
            && Dialect::of(&path).is_some()
            && path.is_file()
        {
            self.standalone_paths.insert(path);
        }
        // A newer save already superseded this check, and it will rebuild the
        // snapshot it publishes; skip the filesystem scan here.
        if should_abort() {
            return Ok(None);
        }
        self.rebuild(ctx)?;
        self.collect_diagnostic_batch(ctx, should_abort)
    }

    fn startup_diagnostics(
        &mut self,
        ctx: &LspContext,
        should_abort: &(dyn Fn() -> bool + Sync),
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        self.collect_diagnostic_batch(ctx, should_abort)
    }

    fn change_workspace_folders(
        &mut self,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        dialect::clear_identity_cache();
        let removed: Vec<PathBuf> = removed
            .into_iter()
            .map(|folder| dialect::identity(&folder))
            .collect();
        let added: Vec<PathBuf> = added
            .into_iter()
            .map(|folder| dialect::identity(&folder))
            .collect();
        for folder in removed {
            self.workspaces.remove(&folder);
        }
        // Load each added folder once; the reload below reuses these results and
        // only re-reads folders that were already present.
        let preloaded: HashMap<PathBuf, LoadedWorkspace> = added
            .into_iter()
            .map(|dir| {
                let loaded = WorkspaceDir::load(&dir, self.workspaces.get(&dir));
                (dir, loaded)
            })
            .collect();
        for (dir, loaded) in &preloaded {
            self.workspaces
                .insert(dir.clone(), loaded.workspace.clone());
        }
        self.reload_workspaces_with(&preloaded);
        self.rebuild(ctx)
    }

    fn hover(
        &self,
        loc: CodeLocation<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<Option<lsp_types::Hover>> {
        self.hover_at(loc, _ctx)
    }

    fn completion(
        &self,
        loc: CodeLocation<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp_types::CompletionResponse>> {
        self.completion_at(loc, _ctx)
    }

    fn completion_for_client(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp_types::CompletionResponse>> {
        RedscriptLanguageServer::completion_for_client(self, loc, ctx)
    }

    fn completion_resolve(
        &self,
        item: lsp::CompletionItem,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::CompletionItem> {
        RedscriptLanguageServer::completion_resolve(self, item, ctx)
    }

    fn goto_definition(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::GotoDefinitionResponse> {
        self.definition_at(loc, ctx)
    }

    fn document_symbols(
        &self,
        doc: Document<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::DocumentSymbolResponse> {
        RedscriptLanguageServer::document_symbols(self, doc, ctx)
    }

    fn workspace_symbol(
        &self,
        query: &str,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::WorkspaceSymbolResponse> {
        self.workspace_symbols(query, ctx)
    }

    fn format(
        &self,
        doc: Document<'_>,
        tab_size: u16,
        ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp_types::TextEdit>> {
        self.format_document(doc, tab_size, ctx)
    }

    fn code_action(
        &self,
        path: PathBuf,
        span: Range<u32>,
        context: lsp::CodeActionContext,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::CodeActionResponse> {
        self.code_action(path, span, context, ctx)
    }

    fn code_action_resolve(
        &self,
        action: lsp::CodeAction,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::CodeAction> {
        self.code_action_resolve(action, ctx)
    }
}

#[cfg(test)]
mod tests {
    use redscript_compiler_api::{
        CompileErrorReporter, LowerError, ScriptBundle, SourceMapExt, TypeInterner, ir,
    };

    use super::*;
    use crate::code_actions::FixKind;

    fn tempdir() -> tempfile::TempDir {
        // Windows runners can expose TEMP through an 8.3 alias such as RUNNER~1.
        tempfile::tempdir_in(fs::canonicalize(std::env::temp_dir()).unwrap()).unwrap()
    }

    fn fixture() -> (
        tempfile::TempDir,
        RedscriptLanguageServer,
        LspContext,
        crossbeam_channel::Receiver<lsp_server::Message>,
    ) {
        let temp = tempdir();
        let cache = temp.path().join("cache.bin");
        fs::write(
            &cache,
            ScriptBundle::default().into_writeable().to_bytes().unwrap(),
        )
        .unwrap();
        let server = RedscriptLanguageServer::new(&cache, [temp.path()]).unwrap();
        let (sender, receiver) = crossbeam_channel::unbounded();
        (temp, server, LspContext::new(sender), receiver)
    }

    fn open(
        server: &mut RedscriptLanguageServer,
        ctx: &LspContext,
        path: &Path,
        text: &str,
        version: i32,
    ) -> crate::buffers::Buffers {
        let mut buffers = crate::buffers::Buffers::default();
        let uri = ctx.uri(path).unwrap();
        buffers.open(uri.clone(), text.into(), version);
        server
            .document_changed(path.to_owned(), buffers.get(&uri), ctx)
            .unwrap();
        buffers
    }

    /// Runs a save exactly as the protocol thread does: the server computes a
    /// batch off the loop and the loop commits it, publishing the diff.
    fn check_and_publish(server: &mut RedscriptLanguageServer, path: PathBuf, ctx: &LspContext) {
        if let Some(batch) = server.check(path, ctx).unwrap() {
            server.diagnostics.apply_and_flush(batch, ctx).unwrap();
        }
    }

    #[test]
    fn document_symbols_use_unsaved_buffers_in_workspaces_and_standalone_files() {
        let (temp, mut server, ctx, _) = fixture();
        let external = tempdir();
        for root in [temp.path(), external.path()] {
            for (extension, saved, live, replacement) in [
                (
                    "reds",
                    "public class Saved {}",
                    "public class Live { public let value: Int32; }",
                    "public struct Changed {}",
                ),
                (
                    "SCRIPT",
                    "class Saved {}",
                    "class Live { var value : Int32; }",
                    "struct Changed {}",
                ),
            ] {
                let path = root.join(format!("outline.{extension}"));
                fs::write(&path, saved).unwrap();
                for (version, text, name) in
                    [(1, live, "Live"), (2, replacement, "Changed"), (3, "", "")]
                {
                    let buffers = open(&mut server, &ctx, &path, text, version);
                    let uri = ctx.uri(&path).unwrap();
                    let doc = Document::new(path.clone(), buffers.get(&uri).unwrap());
                    let lsp::DocumentSymbolResponse::Nested(symbols) =
                        server.document_symbols(doc, &ctx).unwrap()
                    else {
                        panic!("expected hierarchical document symbols");
                    };
                    if name.is_empty() {
                        assert!(symbols.is_empty());
                    } else {
                        assert_eq!(symbols.len(), 1);
                        assert_eq!(symbols[0].name, name);
                        if version == 1 {
                            assert_eq!(symbols[0].children.as_ref().unwrap()[0].name, "value");
                        } else {
                            assert_eq!(symbols[0].kind, lsp::SymbolKind::STRUCT);
                        }
                    }
                    assert_eq!(fs::read_to_string(&path).unwrap(), saved);
                }
            }
        }
        let path = temp.path().join("new/nested/unsaved.script");
        let buffers = open(&mut server, &ctx, &path, "class NewFile {}", 1);
        let uri = ctx.uri(&path).unwrap();
        let lsp::DocumentSymbolResponse::Nested(symbols) = server
            .document_symbols(
                Document::new(path.clone(), buffers.get(&uri).unwrap()),
                &ctx,
            )
            .unwrap()
        else {
            panic!("expected hierarchical document symbols");
        };
        assert_eq!(symbols[0].name, "NewFile");
        assert!(!path.exists());
    }

    #[test]
    fn workspace_symbols_include_members_containers_and_type_kinds() {
        let (temp, _, ctx, _) = fixture();
        let community = temp.path().join("symbols.reds");
        fs::write(
            &community,
            "module Symbols\npublic class Base { public let inherited: Int32; }\n\
             public class Owner extends Base {\n\
               public let value: Int32;\n\
               public func Act() {}\n\
               public func Act(value: Int32) {}\n\
             }\npublic struct Record { public let count: Int32; }\n\
             public enum Choice { First = 0, Second = 1 }\n\
             public func Global() {}",
        )
        .unwrap();
        let native = temp.path().join("native.Script");
        fs::write(
            &native,
            "class NativeOwner { var nativeValue : Int32; function NativeAct() {} }\n\
             struct NativeRecord { var count : Int32; }\n\
             enum NativeChoice { First = 0, Second = 1 }",
        )
        .unwrap();
        let server =
            RedscriptLanguageServer::new(&temp.path().join("cache.bin"), [temp.path()]).unwrap();
        let lsp::WorkspaceSymbolResponse::Flat(symbols) =
            server.workspace_symbols("", &ctx).unwrap()
        else {
            panic!("expected flat workspace symbols");
        };
        for (name, kind, container, path) in [
            ("Symbols.Owner", lsp::SymbolKind::CLASS, None, &community),
            ("Symbols.Record", lsp::SymbolKind::STRUCT, None, &community),
            ("Symbols.Choice", lsp::SymbolKind::ENUM, None, &community),
            (
                "Symbols.Global",
                lsp::SymbolKind::FUNCTION,
                None,
                &community,
            ),
            (
                "value",
                lsp::SymbolKind::FIELD,
                Some("Symbols.Owner"),
                &community,
            ),
            (
                "Act",
                lsp::SymbolKind::METHOD,
                Some("Symbols.Owner"),
                &community,
            ),
            ("NativeRecord", lsp::SymbolKind::STRUCT, None, &native),
            ("NativeChoice", lsp::SymbolKind::ENUM, None, &native),
            (
                "nativeValue",
                lsp::SymbolKind::FIELD,
                Some("NativeOwner"),
                &native,
            ),
            (
                "NativeAct",
                lsp::SymbolKind::METHOD,
                Some("NativeOwner"),
                &native,
            ),
        ] {
            assert!(
                symbols.iter().any(|symbol| {
                    symbol.name == name
                        && symbol.kind == kind
                        && symbol.container_name.as_deref() == container
                        && symbol.location.uri == ctx.uri(path).unwrap()
                }),
                "missing symbol {name}: {symbols:?}"
            );
        }
        assert_eq!(
            symbols.iter().filter(|symbol| symbol.name == "Act").count(),
            2
        );
        assert_eq!(
            symbols
                .iter()
                .filter(|symbol| symbol.name == "inherited")
                .count(),
            1
        );
        assert!(symbols.iter().all(|symbol| symbol.name != "Int32"));
        let lsp::WorkspaceSymbolResponse::Flat(filtered) =
            server.workspace_symbols("nativeV", &ctx).unwrap()
        else {
            panic!("expected flat workspace symbols");
        };
        assert_eq!(filtered.len(), 1);
        assert_eq!(filtered[0].name, "nativeValue");
        assert_eq!(filtered[0].container_name.as_deref(), Some("NativeOwner"));
    }

    #[test]
    fn startup_publishes_workspace_diagnostics() {
        let (temp, _server, ctx, receiver) = fixture();
        let path = temp.path().join("broken.reds");
        fs::write(&path, "public class {").unwrap();
        let mut server =
            RedscriptLanguageServer::new(&temp.path().join("cache.bin"), [temp.path()]).unwrap();
        server.log_missing_roots(&ctx);
        server.publish_all_diagnostics(&ctx).unwrap();
        assert!(receiver.try_iter().any(|message| {
            let lsp_server::Message::Notification(notification) = message else {
                return false;
            };
            notification.method == "textDocument/publishDiagnostics"
                && serde_json::from_value::<lsp::PublishDiagnosticsParams>(notification.params)
                    .is_ok_and(|params| {
                        params.uri == ctx.uri(&path).unwrap() && !params.diagnostics.is_empty()
                    })
        }));
    }

    #[test]
    fn collect_diagnostic_batch_returns_a_batch_without_publishing() {
        let (temp, _server, ctx, receiver) = fixture();
        let path = temp.path().join("broken.reds");
        fs::write(&path, "public class {").unwrap();
        let mut server =
            RedscriptLanguageServer::new(&temp.path().join("cache.bin"), [temp.path()]).unwrap();
        server.log_missing_roots(&ctx);

        let batch = server
            .collect_diagnostic_batch(&ctx, || false)
            .unwrap()
            .unwrap();
        assert!(batch.compiler.contains_key(&dialect::identity(&path)));

        // Neither the produced batch nor an aborted collection may publish.
        let published = |receiver: &crossbeam_channel::Receiver<lsp_server::Message>| {
            receiver
                .try_iter()
                .filter(|message| {
                    matches!(
                        message,
                        lsp_server::Message::Notification(notification)
                            if notification.method == "textDocument/publishDiagnostics"
                    )
                })
                .count()
        };
        assert_eq!(published(&receiver), 0);

        assert!(
            server
                .collect_diagnostic_batch(&ctx, || true)
                .unwrap()
                .is_none()
        );
        assert_eq!(published(&receiver), 0);
    }

    #[test]
    fn superseded_checks_skip_the_filesystem_scan() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("saved.reds");
        fs::write(&path, "public class Saved {}").unwrap();
        let generation = server.generation;

        assert!(
            server
                .check_cancellable(path.clone(), &ctx, &|| true)
                .unwrap()
                .is_none()
        );
        assert_eq!(
            server.generation, generation,
            "an aborted check must not rebuild the snapshot"
        );

        assert!(
            server
                .check_cancellable(path, &ctx, &|| false)
                .unwrap()
                .is_some()
        );
        assert_eq!(server.generation, generation.wrapping_add(1));
    }

    #[test]
    fn member_results_are_reused_only_for_prefix_changes() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("completion.reds");
        let dependency = temp.path().join("dependency.reds");
        let mut previous = None;
        for (version, prefix) in [(1, ""), (2, "V"), (3, "Val")] {
            let text = format!(
                "public class Host {{ public func Value() -> Int32 {{ return 1; }} }} \
                 public func Test(host: ref<Host>) {{ host.{prefix}; }}"
            );
            let buffers = open(&mut server, &ctx, &path, &text, version);
            let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
            let loc = CodeLocation::new(
                Document::new(path.clone(), buffer),
                text.rfind("; }").unwrap() as u32,
            );
            let response = server.completion_at(loc, &ctx).unwrap();
            let lsp::CompletionResponse::Array(items) = &*response else {
                panic!("expected completion items");
            };
            assert!(items.iter().any(|item| item.label.starts_with("Value")));
            if let Some(previous) = previous {
                assert!(Arc::ptr_eq(&previous, &response));
            }
            previous = Some(response);
        }
        let _buffers = open(&mut server, &ctx, &dependency, "public class Other {}", 1);
        let text = "public class Host { public func Value() -> Int32 { return 1; } } \
                    public func Test(host: ref<Host>) { host.Val; }";
        let buffers = open(&mut server, &ctx, &path, text, 4);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let loc = CodeLocation::new(
            Document::new(path.clone(), buffer),
            text.rfind("; }").unwrap() as u32,
        );
        let changed = server.completion_at(loc.clone(), &ctx).unwrap();
        assert!(!Arc::ptr_eq(&previous.unwrap(), &changed));
        server.files_changed(vec![dependency], &ctx).unwrap();
        let refreshed = server.completion_at(loc, &ctx).unwrap();
        assert!(!Arc::ptr_eq(&changed, &refreshed));
    }

    #[test]
    fn adapted_completions_are_reused_across_prefix_edits() {
        let (temp, mut server, mut ctx, _receiver) = fixture();
        ctx.set_presentation_capabilities(
            &serde_json::from_value(serde_json::json!({
                "textDocument": {"completion": {"completionItem": {
                    "snippetSupport": true,
                    "labelDetailsSupport": true,
                    "documentationFormat": ["markdown"],
                }}},
            }))
            .unwrap(),
        );
        let path = temp.path().join("completion.reds");
        let text = |prefix: &str| {
            format!(
                "public class Host {{ public func Value() -> Int32 {{ return 1; }} }} \
                 public func Test(host: ref<Host>) {{ host.{prefix}; }}"
            )
        };
        let mut previous = None;
        for (version, prefix) in [(1, ""), (2, "V"), (3, "Val")] {
            let text = text(prefix);
            let buffers = open(&mut server, &ctx, &path, &text, version);
            let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
            let loc = CodeLocation::new(
                Document::new(path.clone(), buffer),
                text.rfind("; }").unwrap() as u32,
            );
            let response = server.completion_for_client(loc, &ctx).unwrap();
            let lsp::CompletionResponse::Array(items) = &*response else {
                panic!("expected completion items");
            };
            let value = items.iter().find(|item| item.label == "Value").unwrap();
            assert!(value.label_details.is_some());
            assert_eq!(
                value.insert_text_format,
                Some(lsp::InsertTextFormat::SNIPPET)
            );
            if let Some(previous) = &previous {
                assert!(Arc::ptr_eq(previous, &response));
            }
            previous = Some(response);
        }

        // A different presentation must adapt the cached raw response afresh
        // rather than reuse the one adapted for the negotiated capabilities.
        let adapted = previous.unwrap();
        ctx.set_presentation_capabilities(&lsp::ClientCapabilities::default());
        let text = text("Val");
        let buffers = open(&mut server, &ctx, &path, &text, 4);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let loc = CodeLocation::new(
            Document::new(path.clone(), buffer),
            text.rfind("; }").unwrap() as u32,
        );
        let plain = server.completion_for_client(loc, &ctx).unwrap();
        assert!(!Arc::ptr_eq(&adapted, &plain));
        let lsp::CompletionResponse::Array(items) = &*plain else {
            panic!("expected completion items");
        };
        let value = items.iter().find(|item| item.label == "Value").unwrap();
        assert!(value.label_details.is_none());
        assert_eq!(
            value.insert_text_format,
            Some(lsp::InsertTextFormat::PLAIN_TEXT)
        );
    }

    #[test]
    fn diagnostics_are_only_published_on_save() {
        for standalone in [false, true] {
            let (temp, mut server, ctx, receiver) = fixture();
            let external = tempdir();
            let directory = if standalone {
                external.path()
            } else {
                temp.path()
            };
            let path = directory.join("test.script");
            fs::write(&path, "class {").unwrap();
            let publications = || {
                receiver
                    .try_iter()
                    .filter_map(|message| {
                        let lsp_server::Message::Notification(notification) = message else {
                            return None;
                        };
                        (notification.method == "textDocument/publishDiagnostics").then(|| {
                            serde_json::from_value::<lsp::PublishDiagnosticsParams>(
                                notification.params,
                            )
                            .unwrap()
                        })
                    })
                    .collect::<Vec<_>>()
            };

            server.files_changed(vec![path.clone()], &ctx).unwrap();
            let _buffers = open(&mut server, &ctx, &path, "class {", 1);
            assert!(publications().is_empty());

            check_and_publish(&mut server, path.clone(), &ctx);
            let saved = publications();
            assert_eq!(saved.len(), 1);
            assert_eq!(saved[0].uri, ctx.uri(&path).unwrap());
            assert_eq!(saved[0].version, Some(1));
            assert!(!saved[0].diagnostics.is_empty());

            let _buffers = open(&mut server, &ctx, &path, "class Fixed {}", 2);
            fs::write(&path, "class Fixed {}").unwrap();
            server.files_changed(vec![path.clone()], &ctx).unwrap();
            assert!(publications().is_empty());
            assert!(server.diagnostics.is_published(&dialect::identity(&path)));

            check_and_publish(&mut server, path.clone(), &ctx);
            let saved = publications();
            assert_eq!(saved.len(), 1);
            assert_eq!(saved[0].version, Some(2));
            assert!(saved[0].diagnostics.is_empty());

            let _buffers = open(&mut server, &ctx, &path, "class {", 3);
            check_and_publish(&mut server, path.clone(), &ctx);
            assert!(!publications()[0].diagnostics.is_empty());
            server.document_changed(path.clone(), None, &ctx).unwrap();
            server
                .change_workspace_folders(vec![], vec![], &ctx)
                .unwrap();
            assert!(publications().is_empty());
            check_and_publish(&mut server, path, &ctx);
            assert!(publications()[0].diagnostics.is_empty());
        }
    }

    #[test]
    fn query_logging_explains_missing_hover_and_non_member_completion() {
        let (temp, mut server, ctx, receiver) = fixture();
        let path = temp.path().join("empty.script");
        let buffers = open(&mut server, &ctx, &path, "", 7);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let loc = CodeLocation::new(Document::new(path, buffer), 0);
        assert!(server.hover_at(loc.clone(), &ctx).unwrap().is_none());
        server.completion_at(loc, &ctx).unwrap();
        let logs: Vec<lsp::LogMessageParams> = receiver
            .try_iter()
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                (notification.method == "window/logMessage")
                    .then(|| serde_json::from_value(notification.params).unwrap())
            })
            .collect();
        assert!(
            logs.iter()
                .any(|log| log.message.contains("Rebuild started"))
        );
        assert!(logs.iter().any(|log| {
            log.message.contains("Rebuild finished generation=")
                && log.message.contains("elapsed_ms=")
        }));
        assert!(logs.iter().any(|log| {
            log.message.contains("Query path=")
                && log.message.contains("version=Some(7)")
                && log.message.contains("workspace=true")
        }));
        assert!(logs.iter().any(|log| {
            log.message.contains("Query result")
                && log.message.contains("ast_match=false")
                && log.message.contains("expression=false")
        }));
        assert!(logs.iter().any(|log| {
            log.message.contains("Hover payload content_bytes=0")
                && log.message.contains("range=None")
                && log.message.contains("contains_requested_byte=None")
        }));
        assert!(logs.iter().any(|log| {
            log.message.contains("Completion empty")
                && log.message.contains("only member completion is supported")
        }));
    }

    #[test]
    fn hover_reuses_analysis_and_edits_replace_it() {
        let (temp, mut server, ctx, receiver) = fixture();
        let path = temp.path().join("hover.reds");
        for (version, typ, value) in [(1, "Int32", "1"), (2, "Bool", "true")] {
            let text = format!(
                "public func Test() -> {typ} {{ let value: {typ} = {value}; return value; }}"
            );
            let buffers = open(&mut server, &ctx, &path, &text, version);
            let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
            let loc = CodeLocation::new(
                Document::new(path.clone(), buffer),
                text.rfind("value").unwrap() as u32 + 1,
            );
            let unit_address = server
                .compilation_cache
                .with(|cache| &cache.analysis.unit as *const _ as usize);
            for _ in 0..2 {
                let hover = server.hover_at(loc.clone(), &ctx).unwrap().unwrap();
                let lsp::HoverContents::Markup(contents) = hover.contents else {
                    panic!("expected markup hover");
                };
                assert!(contents.value.contains(typ), "{}", contents.value);
                assert_eq!(
                    server
                        .compilation_cache
                        .with(|cache| &cache.analysis.unit as *const _ as usize),
                    unit_address,
                );
            }
        }
        let logs: Vec<lsp::LogMessageParams> = receiver
            .try_iter()
            .filter_map(|message| match message {
                lsp_server::Message::Notification(notification)
                    if notification.method == "window/logMessage" =>
                {
                    Some(serde_json::from_value(notification.params).unwrap())
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            logs.iter()
                .filter(|log| log.message == "Query analysis=reused active snapshot")
                .count(),
            4,
        );
        assert!(
            !logs
                .iter()
                .any(|log| log.message == "Query analysis=speculative snapshot")
        );
    }

    #[test]
    fn documents_changed_applies_a_batch_with_one_rebuild() {
        let (temp, mut server, ctx, receiver) = fixture();
        let a = temp.path().join("a.reds");
        let b = temp.path().join("b.reds");
        let mut buffers = crate::buffers::Buffers::default();
        let uri_a = ctx.uri(&a).unwrap();
        let uri_b = ctx.uri(&b).unwrap();
        buffers.open(uri_a.clone(), "public class A {}".into(), 1);
        buffers.open(uri_b.clone(), "public class B {}".into(), 1);
        server
            .documents_changed(
                &[
                    (a.clone(), buffers.get(&uri_a)),
                    (b.clone(), buffers.get(&uri_b)),
                ],
                &ctx,
            )
            .unwrap();
        let rebuilds = receiver
            .try_iter()
            .filter_map(|message| match message {
                lsp_server::Message::Notification(notification)
                    if notification.method == "window/logMessage" =>
                {
                    serde_json::from_value::<lsp::LogMessageParams>(notification.params).ok()
                }
                _ => None,
            })
            .filter(|log| log.message.contains("Rebuild started"))
            .count();
        assert_eq!(rebuilds, 1);
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "A"));
                assert!(symbols.types().any(|(id, _)| id.as_str() == "B"));
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn edits_use_cached_dependencies_until_filesystem_refresh() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let dependency = temp.path().join("dependency.reds");
        fs::write(&dependency, "public class Before {}").unwrap();
        server
            .files_changed(vec![dependency.clone()], &ctx)
            .unwrap();
        fs::write(&dependency, "public class After {}").unwrap();
        let path = temp.path().join("open.reds");
        let _buffers = open(&mut server, &ctx, &path, "public class Open {}", 1);
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Before"));
                assert!(!symbols.types().any(|(id, _)| id.as_str() == "After"));
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Open"));
                Ok(())
            })
            .unwrap();
        server.files_changed(vec![dependency], &ctx).unwrap();
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "After"));
                assert!(!symbols.types().any(|(id, _)| id.as_str() == "Before"));
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Open"));
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn mixed_case_redmod_saved_and_live_snapshots() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let community = temp.path().join("community.reds");
        let mixed = temp.path().join("mixed.Script");
        let upper = temp.path().join("upper.SCRIPT");
        fs::write(&community, "public class Community {}").unwrap();
        fs::write(&mixed, "class MixedDisk {}").unwrap();
        fs::write(&upper, "class UpperDisk {}").unwrap();
        server
            .files_changed(vec![community, mixed.clone(), upper.clone()], &ctx)
            .unwrap();
        server
            .check_workspace(|_, symbols, diags, _| {
                assert!(diags.is_empty(), "{diags:?}");
                for name in ["Community", "MixedDisk", "UpperDisk"] {
                    assert!(symbols.types().any(|(id, _)| id.as_str() == name));
                }
                Ok(())
            })
            .unwrap();
        let _mixed = open(&mut server, &ctx, &mixed, "class MixedLive {}", 1);
        let _upper = open(&mut server, &ctx, &upper, "class UpperLive {}", 1);
        let unsaved = temp.path().join("new.sCrIpT");
        let _unsaved = open(&mut server, &ctx, &unsaved, "class Unsaved {}", 1);
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 4);
        server
            .check_workspace(|_, symbols, diags, _| {
                assert!(diags.is_empty(), "{diags:?}");
                for name in ["Community", "MixedLive", "UpperLive", "Unsaved"] {
                    assert!(symbols.types().any(|(id, _)| id.as_str() == name));
                }
                for name in ["MixedDisk", "UpperDisk"] {
                    assert!(!symbols.types().any(|(id, _)| id.as_str() == name));
                }
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn mixed_live_snapshot_empty_replacement_and_standalone_safety() {
        let (temp, mut server, ctx, receiver) = fixture();
        let community = temp.path().join("a.reds");
        let redmod = temp.path().join("b.script");
        fs::write(&community, "public class Community {}").unwrap();
        fs::write(&redmod, "class Redmod { var value : Int32; }").unwrap();
        server
            .files_changed(vec![community.clone(), redmod.clone()], &ctx)
            .unwrap();
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 2);
        server
            .check_workspace(|_, symbols, diags, _| {
                assert!(diags.is_empty(), "{diags:?}");
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Redmod"));
                Ok(())
            })
            .unwrap();
        let _buffers = open(&mut server, &ctx, &redmod, "", 5);
        server
            .check_workspace(|_, symbols, diags, _| {
                assert!(diags.is_empty(), "{diags:?}");
                assert!(!symbols.types().any(|(id, _)| id.as_str() == "Redmod"));
                Ok(())
            })
            .unwrap();
        server.compilation_cache.with(|cache| {
            let id = cache.file_ids[&dialect::identity(&redmod)];
            assert!(
                cache
                    .analysis
                    .parsed
                    .modules
                    .iter()
                    .any(|(file, _)| *file == id)
            );
        });
        let external = tempdir();
        let standalone = external.path().join("solo.script");
        fs::write(&standalone, "class Standalone {}").unwrap();
        check_and_publish(&mut server, standalone.clone(), &ctx);
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 2);
        assert_eq!(
            server.standalone[&dialect::identity(&standalone)]
                .borrow_file_ids()
                .len(),
            1
        );
        let _buffers = open(&mut server, &ctx, &redmod, "class {", 6);
        check_and_publish(&mut server, redmod.clone(), &ctx);
        let params: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                serde_json::from_value(notification.params).ok()
            })
            .collect();
        assert!(params.iter().any(|params| {
            params.version == Some(6)
                && params.diagnostics.iter().any(|diag| {
                    diag.code == Some(lsp::NumberOrString::String("REDMOD_PARSE".into()))
                })
        }));
    }

    #[test]
    fn queries_use_all_overlays_without_mutating_active_sources() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let target = temp.path().join("target.script");
        let user = temp.path().join("user.script");
        let _target_buffers = open(
            &mut server,
            &ctx,
            &target,
            "class Target { public var value : Int32; }",
            1,
        );
        let text = "class User { function Test(obj : Target) : Int32 { return obj.value; } }";
        let buffers = open(&mut server, &ctx, &user, text, 1);
        let buffer = buffers.get(&ctx.uri(&user).unwrap()).unwrap();
        let loc = CodeLocation::new(
            Document::new(user.clone(), buffer),
            text.find("value").unwrap() as u32 + 1,
        );
        for _ in 0..3 {
            let definition = server.definition_at(loc.clone(), &ctx).unwrap();
            let lsp::GotoDefinitionResponse::Array(locations) = definition else {
                panic!("expected locations");
            };
            assert_eq!(locations.len(), 1);
            assert_eq!(
                dialect::identity(&crate::server::path_from_uri(&locations[0].uri).unwrap()),
                dialect::identity(&target)
            );
            server
                .patched_expr_at(loc.clone(), |_| Ok(()), &ctx)
                .unwrap();
        }
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 2);
        assert_eq!(
            server.compilation_cache.with_sources(ast::SourceMap::len),
            3
        );
        server
            .check_workspace(|_, _, diags, _| {
                assert!(diags.is_empty(), "{diags:?}");
                Ok(())
            })
            .unwrap();
        let zero = CodeLocation::new(Document::new(user, buffer), 0);
        assert!(
            matches!(&*server.completion_at(zero, &ctx).unwrap(), lsp::CompletionResponse::Array(items) if items.is_empty())
        );
    }

    #[test]
    fn implicit_redmod_members_keep_hover_and_definition_targets() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("implicit.script");
        let text = "class Host { var value: Int32; function Value(): Int32 { return 1; } function Read(): Int32 { return value; } function Run(): Int32 { return Value(); } function Nested(): Int32 { if (true) { return value; } return Value(); } function Explicit(): Int32 { if (true) { return this.value; } return this.Value(); } }";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        for (name, heading) in [("value", "### Field"), ("Value", "### Signature")] {
            let declaration = text.find(name).unwrap() as u32;
            for (usage, _) in text.match_indices(name).skip(1) {
                for offset in 1..=name.len() as u32 {
                    let loc = CodeLocation::new(
                        Document::new(path.clone(), buffer),
                        usage as u32 + offset,
                    );
                    let check = |at: ExprAt<'_, '_>| {
                        let span = at.definition_span().unwrap_or_else(|| {
                            panic!(
                                "missing definition for {name} at {usage}+{offset}: {:?}",
                                at.expr()
                            )
                        });
                        assert!(span.contains(declaration));
                        let hover = at.display().to_string();
                        assert!(hover.contains(heading), "{hover}");
                        assert!(hover.contains("Int32"), "{hover}");
                        Ok(())
                    };
                    server.expr_at(loc.clone(), check, &ctx).unwrap();
                    let pos = loc.pos() as usize;
                    server
                        .expr_at_with(loc, Some(pos..pos), None, check, &ctx)
                        .unwrap();
                }
            }
        }
        for (usage, _) in text.match_indices("this") {
            let loc = CodeLocation::new(Document::new(path.clone(), buffer), usage as u32 + 1);
            let check = |at: ExprAt<'_, '_>| {
                assert!(matches!(
                    at.expr(),
                    Some(ir::Expr::Local(ir::Local::This, _))
                ));
                assert!(at.display().to_string().contains("### Type"));
                Ok(())
            };
            server.expr_at(loc.clone(), check, &ctx).unwrap();
            let pos = loc.pos() as usize;
            server
                .expr_at_with(loc, Some(pos..pos), None, check, &ctx)
                .unwrap();
        }
    }

    #[test]
    fn redmod_cast_type_navigation_and_unicode_syntax_diagnostics() {
        let (temp, mut server, ctx, receiver) = fixture();
        let target = temp.path().join("target.script");
        let _buffers = open(&mut server, &ctx, &target, "class Target {}", 1);
        let path = temp.path().join("cast.script");
        let text =
            "class User { function Test(obj : IScriptable) : Target { return (Target)obj; } }";
        let buffers = open(&mut server, &ctx, &path, text, 2);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let pos = text.find("(Target)").unwrap() as u32 + 3;
        let response = server
            .definition_at(
                CodeLocation::new(Document::new(path.clone(), buffer), pos),
                &ctx,
            )
            .unwrap();
        let lsp::GotoDefinitionResponse::Array(locations) = response else {
            panic!("expected definition locations");
        };
        assert_eq!(locations.len(), 1);
        assert_eq!(
            dialect::identity(&crate::server::path_from_uri(&locations[0].uri).unwrap()),
            dialect::identity(&target)
        );
        receiver.try_iter().for_each(drop);
        let text = "// é😀\r\nclass User { function Test() { var value : ; } }";
        let _buffers = open(&mut server, &ctx, &path, text, 3);
        check_and_publish(&mut server, path.clone(), &ctx);
        let params: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                serde_json::from_value(notification.params).ok()
            })
            .collect();
        let params = params
            .iter()
            .find(|params| params.version == Some(3))
            .unwrap();
        assert!(!params.diagnostics.is_empty());
        assert!(
            params
                .diagnostics
                .iter()
                .all(|diag| diag.range.start.line == 1)
        );
    }

    #[test]
    fn mixed_semantics_apply_to_workspace_and_query_snapshots() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let dependency = temp.path().join("dependency.reds");
        let _dependencies = open(
            &mut server,
            &ctx,
            &dependency,
            "public class Object {} public func Shared() -> Int32 { if true { let x: Int32 = 1; return x; } let x: Int32 = 2; return x; }",
            1,
        );
        let path = temp.path().join("arrays.script");
        let text = "function Test(obj: Object) -> Int32 { let values: array<Int32>; values.PushBack(Shared()); if (obj) { return values.Size(); } return 0; }";
        let buffers = open(&mut server, &ctx, &path, text, 2);
        server
            .check_workspace(|unit, _, diagnostics, _| {
                assert!(diagnostics.is_empty(), "{diagnostics:?}");
                assert!(unit.all_functions().any(|function| function.span.file
                    == server.compilation_cache.borrow_file_ids()[&dialect::identity(&path)]));
                Ok(())
            })
            .unwrap();
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let loc = CodeLocation::new(
            Document::new(path.clone(), buffer),
            text.find("Size").unwrap() as u32 + 2,
        );
        server
            .expr_at(
                loc,
                |at| {
                    assert_eq!(
                        at.expr_type().map(|typ| typ.to_string()).as_deref(),
                        Some("Int32")
                    );
                    Ok(())
                },
                &ctx,
            )
            .unwrap();
        let _buffers = open(
            &mut server,
            &ctx,
            &path,
            "function Test(n: Int32) { if (n) {} }",
            3,
        );
        server
            .check_workspace(|_, _, diagnostics, _| {
                assert!(!diagnostics.is_empty());
                assert!(diagnostics.iter().any(|diag| diag.span().file
                    == server.compilation_cache.borrow_file_ids()[&dialect::identity(&path)]));
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn override_metadata_survives_live_and_speculative_snapshots() {
        let (temp, mut server, ctx, receiver) = fixture();
        let base = temp.path().join("base.reds");
        let _base = open(
            &mut server,
            &ctx,
            &base,
            "public class Base { public func F(x: Int32) {} }",
            1,
        );
        let path = temp.path().join("child.script");
        let text = "class Child extends Base { public override function F(x: Int32 ) {} }";
        let buffers = open(&mut server, &ctx, &path, text, 2);
        server
            .check_workspace(|_, _, diagnostics, _| {
                assert!(diagnostics.is_empty(), "{diagnostics:?}");
                Ok(())
            })
            .unwrap();
        server.compilation_cache.with(|cache| {
            let id = cache.file_ids[&dialect::identity(&path)];
            let (_, crate::dialect::ParsedModule::Redmod(parsed)) = cache
                .analysis
                .parsed
                .modules
                .iter()
                .find(|(file, _)| *file == id)
                .unwrap()
            else {
                panic!("expected REDmod metadata");
            };
            assert_eq!(parsed.overrides.len(), 1);
        });
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let loc = CodeLocation::new(
            Document::new(path.clone(), buffer),
            text.find("Int32").unwrap() as u32 + 5,
        );
        server
            .patched_expr_at(
                loc,
                |at| {
                    assert_eq!(at.type_().map(|typ| typ.as_str()).as_deref(), Some("Int32"));
                    Ok(())
                },
                &ctx,
            )
            .unwrap();
        receiver.try_iter().for_each(drop);
        let _buffers = open(
            &mut server,
            &ctx,
            &path,
            "class Child extends Base { public function F(x: Int32) {} }",
            3,
        );
        server
            .check_workspace(|_, _, diagnostics, _| {
                assert!(
                    diagnostics.iter().any(|diag| {
                        diag.to_string()
                            == redscript_redmod_adapter::SemanticError::MissingOverride.to_string()
                    }),
                    "{diagnostics:?}"
                );
                Ok(())
            })
            .unwrap();
        check_and_publish(&mut server, path.clone(), &ctx);
        let publications: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                serde_json::from_value(notification.params).ok()
            })
            .collect();
        assert!(publications.iter().any(|params| {
            params.version == Some(3)
                && params.diagnostics.iter().any(|diag| {
                    diag.message
                        == redscript_redmod_adapter::SemanticError::MissingOverride.to_string()
                })
        }));
        let _buffers = open(
            &mut server,
            &ctx,
            &path,
            "class Child extends Base { public override function F(x: Bool) {} }",
            4,
        );
        server
            .check_workspace(|_, _, diagnostics, _| {
                assert!(
                    diagnostics.iter().any(|diag| {
                        diag.to_string()
                            == redscript_redmod_adapter::SemanticError::OverrideWithoutBase
                                .to_string()
                    }),
                    "{diagnostics:?}"
                );
                Ok(())
            })
            .unwrap();
    }

    fn assert_native_loop_queries(text: &str, calls: &[&str]) {
        let (temp, mut server, ctx, _receiver) = fixture();
        let dependency = temp.path().join("dependency.reds");
        let declarations = "public func Init() -> Int32 { return 0; }\npublic func Condition() -> Bool { return false; }\npublic func Step() -> Int32 { return 1; }\npublic func Body() -> Int32 { return 2; }";
        let _dependency = open(&mut server, &ctx, &dependency, declarations, 1);
        let path = temp.path().join("loops.script");
        fs::write(&path, "function DiskOnly() {}").unwrap();
        server.files_changed(vec![path.clone()], &ctx).unwrap();
        let buffers = open(&mut server, &ctx, &path, text, 2);
        server
            .check_workspace(|unit, _, diagnostics, _| {
                assert!(diagnostics.is_empty(), "{diagnostics:?}");
                let id = server.compilation_cache.borrow_file_ids()[&dialect::identity(&path)];
                let function = unit
                    .all_functions()
                    .find(|function| function.span.file == id)
                    .unwrap();
                // Each native loop needs its metadata-driven first-entry guard.
                assert_eq!(function.locals.len(), 1);
                assert!(function.locals[0].name.is_none());
                for name in calls {
                    let pos = text.find(name).unwrap() as u32;
                    assert!(
                        matches!(function.block.find_at(pos), Some(ir::Expr::Call { .. })),
                        "workspace lookup hid {name}"
                    );
                }
                Ok(())
            })
            .unwrap();
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        for name in calls {
            let pos = text.find(name).unwrap() as u32 + 1;
            let loc = CodeLocation::new(Document::new(path.clone(), buffer), pos);
            let lsp::GotoDefinitionResponse::Array(locations) =
                server.definition_at(loc.clone(), &ctx).unwrap()
            else {
                panic!("expected definition locations for {name}");
            };
            assert_eq!(locations.len(), 1, "missing definition for {name}");
            assert_eq!(locations[0].uri, ctx.uri(&dependency).unwrap());
            assert_eq!(
                locations[0].range.start.line,
                declarations[..declarations.find(&format!("func {name}")).unwrap()]
                    .bytes()
                    .filter(|byte| *byte == b'\n')
                    .count() as u32
            );
            let hover = server.hover_at(loc.clone(), &ctx).unwrap().unwrap();
            let lsp::HoverContents::Markup(contents) = hover.contents else {
                panic!("expected markup hover for {name}");
            };
            let typ = if *name == "Condition" {
                "Bool"
            } else {
                "Int32"
            };
            assert!(
                contents.value.contains(&format!("{name}() -> {typ}")),
                "unexpected hover for {name}: {}",
                contents.value
            );
            let start = text.find(name).unwrap();
            let line = text[..start].bytes().filter(|byte| *byte == b'\n').count() as u32;
            let column = (start - text[..start].rfind('\n').map_or(0, |index| index + 1)) as u32;
            assert_eq!(
                hover.range,
                Some(lsp::Range::new(
                    lsp::Position::new(line, column),
                    lsp::Position::new(line, column + name.len() as u32 + 2),
                ))
            );

            // Model completion's token erasure without publishing the broken buffer.
            let speculative = text.replace("Body()", "Body().unfinished");
            let mut query_buffers = crate::buffers::Buffers::default();
            let uri = ctx.uri(&path).unwrap();
            query_buffers.open(uri.clone(), speculative.clone(), 3);
            let start = speculative.find(".unfinished").unwrap();
            let query_loc = CodeLocation::new(
                Document::new(path.clone(), query_buffers.get(&uri).unwrap()),
                speculative.find(name).unwrap() as u32 + 1,
            );
            server
                .expr_at_with(
                    query_loc,
                    Some(start..start + ".unfinished".len()),
                    None,
                    |at| {
                        assert_eq!(at.func().unwrap().locals.len(), 1);
                        assert!(at.func().unwrap().locals[0].name.is_none());
                        assert!(
                            matches!(at.expr(), Some(ir::Expr::Call { .. })),
                            "speculative lookup hid {name}"
                        );
                        assert_eq!(at.expr_type().unwrap().to_string(), typ);
                        assert_eq!(
                            location(at.definition_span().unwrap(), at.sources(), &ctx).unwrap(),
                            locations[0]
                        );
                        assert!(
                            at.display()
                                .to_string()
                                .contains(&format!("{name}() -> {typ}"))
                        );
                        Ok(())
                    },
                    &ctx,
                )
                .unwrap();
        }
        server.compilation_cache.with_sources(|sources| {
            let id = server.compilation_cache.borrow_file_ids()[&dialect::identity(&path)];
            assert_eq!(sources.get(id).unwrap().source(), text);
        });
        assert_eq!(fs::read_to_string(path).unwrap(), "function DiskOnly() {}");
    }

    #[test]
    fn native_for_navigation_and_hover_survive_live_and_speculative_analysis() {
        assert_native_loop_queries(
            "function Run() {\n    for (Init(); Condition(); Step()) {\n        Body();\n        continue;\n    }\n}",
            &["Init", "Condition", "Step", "Body"],
        );
    }

    #[test]
    fn native_do_while_navigation_and_hover_survive_live_and_speculative_analysis() {
        assert_native_loop_queries(
            "function Run() {\n    do {\n        Body();\n        continue;\n    } while (Condition());\n}",
            &["Body", "Condition"],
        );
    }

    #[test]
    fn failed_redmod_inputs_do_not_block_valid_workspace_modules() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let good = temp.path().join("good.script");
        let _good = open(&mut server, &ctx, &good, "class Good {}", 1);
        let community = temp.path().join("community.reds");
        let _community = open(
            &mut server,
            &ctx,
            &community,
            "public class Community {}",
            1,
        );
        let bad = temp.path().join("bad.script");
        for text in ["class Generic<T> {}", "class {"] {
            let _bad = open(&mut server, &ctx, &bad, text, 2);
            server
                .check_workspace(|_, symbols, diagnostics, sources| {
                    assert!(symbols.types().any(|(id, _)| id.as_str() == "Good"));
                    assert!(symbols.types().any(|(id, _)| id.as_str() == "Community"));
                    assert!(!symbols.types().any(|(id, _)| id.as_str() == "Generic"));
                    let errors: Vec<_> = diagnostics
                        .iter()
                        .filter(|diag| {
                            sources.get(diag.span().file).unwrap().path() == dialect::identity(&bad)
                        })
                        .collect();
                    assert!(!errors.is_empty());
                    assert!(errors.iter().all(|diag| {
                        matches!(diag, Diagnostic::Custom(_, code, _, _) if code.starts_with("REDMOD_"))
                    }));
                    Ok(())
                })
                .unwrap();
        }
    }

    #[test]
    fn nested_unsaved_overlay_survives_save_delete_and_close() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("nested/deeper/new.script");
        let identity = dialect::identity(&path);
        let _buffer = open(&mut server, &ctx, &path, "class Live {}", 1);
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, "class Saved {}").unwrap();
        server.files_changed(vec![path.clone()], &ctx).unwrap();
        fs::remove_dir_all(temp.path().join("nested")).unwrap();
        server.files_changed(vec![path.clone()], &ctx).unwrap();
        assert_eq!(server.overlays.len(), 1);
        assert!(server.overlays.contains_key(&identity));
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 1);
        server
            .check_workspace(|_, symbols, diagnostics, _| {
                assert!(diagnostics.is_empty(), "{diagnostics:?}");
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Live"));
                assert!(!symbols.types().any(|(id, _)| id.as_str() == "Saved"));
                Ok(())
            })
            .unwrap();
        server.document_changed(path, None, &ctx).unwrap();
        assert!(server.overlays.is_empty());
        assert!(server.compilation_cache.borrow_file_ids().is_empty());
    }

    fn missing_external_root_diagnostics(initially_missing: bool) {
        let (temp, _server, ctx, receiver) = fixture();
        let external = tempdir();
        let root = external.path().join("nested/root");
        let removed = root.join("removed.script");
        let healthy = temp.path().join("healthy.script");
        let standalone_dir = tempdir();
        let standalone = standalone_dir.path().join("solo.script");
        fs::write(&healthy, "class {").unwrap();
        fs::write(&standalone, "class {").unwrap();
        if !initially_missing {
            fs::create_dir_all(&root).unwrap();
            fs::write(&removed, "class {").unwrap();
        }
        fs::write(
            temp.path().join(".redscript"),
            format!(
                "source_roots = [{:?}, {:?}]",
                temp.path().to_string_lossy().replace('\\', "/"),
                root.to_string_lossy().replace('\\', "/"),
            ),
        )
        .unwrap();
        // Exercise startup discovery too, before an LspContext is available.
        let mut server =
            RedscriptLanguageServer::new(&temp.path().join("cache.bin"), [temp.path()]).unwrap();
        assert_eq!(server.missing_roots.len(), usize::from(initially_missing));
        server.log_missing_roots(&ctx);
        let startup: Vec<_> = receiver.try_iter().collect();
        assert_eq!(
            startup.iter().any(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return false;
                };
                notification.method == "window/logMessage"
                    && serde_json::from_value::<lsp::LogMessageParams>(notification.params.clone())
                        .is_ok_and(|params| params.typ == lsp::MessageType::WARNING)
            }),
            initially_missing
        );
        assert_eq!(
            startup.iter().any(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return false;
                };
                notification.method == "window/showMessage"
                    && serde_json::from_value::<lsp::ShowMessageParams>(notification.params.clone())
                        .is_ok_and(|params| {
                            params.typ == lsp::MessageType::WARNING
                                && params.message.contains("Source root")
                        })
            }),
            initially_missing
        );
        check_and_publish(&mut server, standalone.clone(), &ctx);
        let initial: Vec<lsp::PublishDiagnosticsParams> = startup
            .into_iter()
            .chain(receiver.try_iter())
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                serde_json::from_value(notification.params).ok()
            })
            .collect();
        for path in [&healthy, &standalone] {
            assert!(initial.iter().any(
                |params| params.uri == ctx.uri(path).unwrap() && !params.diagnostics.is_empty()
            ));
        }
        if !initially_missing {
            assert!(
                initial
                    .iter()
                    .any(|params| params.uri == ctx.uri(&removed).unwrap()
                        && !params.diagnostics.is_empty())
            );
            fs::remove_dir_all(&root).unwrap();
        }
        fs::write(&healthy, "class Healthy {}").unwrap();
        fs::write(&standalone, "class Solo {}").unwrap();
        server
            .files_changed(
                vec![root.clone(), healthy.clone(), standalone.clone()],
                &ctx,
            )
            .unwrap();
        let messages: Vec<_> = receiver.try_iter().collect();
        assert_eq!(
            messages.iter().any(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return false;
                };
                if notification.method != "window/logMessage" {
                    return false;
                }
                let params: lsp::LogMessageParams =
                    serde_json::from_value(notification.params.clone()).unwrap();
                params.typ == lsp::MessageType::WARNING
                    && params
                        .message
                        .contains(&dialect::identity(&root).display().to_string())
                    && params.message.contains("source_roots")
                    && params.message.contains("Restore/create")
            }),
            !initially_missing,
        );
        assert!(!messages.iter().any(|message| {
            matches!(message, lsp_server::Message::Notification(notification)
                if notification.method == "textDocument/publishDiagnostics")
        }));
        check_and_publish(&mut server, healthy.clone(), &ctx);
        let publications: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                serde_json::from_value(notification.params).ok()
            })
            .collect();
        for path in [&healthy, &standalone]
            .into_iter()
            .chain((!initially_missing).then_some(&removed))
        {
            assert!(
                publications
                    .iter()
                    .any(|params| params.uri == ctx.uri(path).unwrap()
                        && params.diagnostics.is_empty()),
                "missing clear for {}",
                path.display()
            );
        }
        assert!(server.resolve_file(&removed).as_workspace().is_some());
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 1);
        assert_eq!(server.standalone.len(), 1);
        // Missing roots must not block diagnostic updates on save either.
        for path in [&healthy, &standalone] {
            let _buffer = open(&mut server, &ctx, path, "class {", 7);
        }
        check_and_publish(&mut server, healthy.clone(), &ctx);
        let publications: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    return None;
                };
                serde_json::from_value(notification.params).ok()
            })
            .collect();
        for path in [&healthy, &standalone] {
            assert!(
                publications
                    .iter()
                    .any(|params| params.uri == ctx.uri(path).unwrap()
                        && params.version == Some(7)
                        && !params.diagnostics.is_empty())
            );
        }

        // Restoring and deleting the root again transitions warnings cleanly.
        receiver.try_iter().for_each(drop);
        fs::create_dir_all(&root).unwrap();
        server.files_changed(vec![root.clone()], &ctx).unwrap();
        let restored: Vec<lsp::LogMessageParams> = receiver
            .try_iter()
            .filter_map(|message| match message {
                lsp_server::Message::Notification(notif) if notif.method == "window/logMessage" => {
                    serde_json::from_value(notif.params).ok()
                }
                _ => None,
            })
            .collect();
        assert!(
            restored
                .iter()
                .all(|log| log.typ != lsp::MessageType::WARNING)
        );
        assert!(
            restored
                .iter()
                .any(|log| log.message.contains("is available again"))
        );

        fs::remove_dir_all(&root).unwrap();
        server.files_changed(vec![root.clone()], &ctx).unwrap();
        let re_deleted: Vec<lsp::LogMessageParams> = receiver
            .try_iter()
            .filter_map(|message| match message {
                lsp_server::Message::Notification(notif) if notif.method == "window/logMessage" => {
                    serde_json::from_value(notif.params).ok()
                }
                _ => None,
            })
            .collect();
        assert_eq!(
            re_deleted
                .iter()
                .filter(|log| log.typ == lsp::MessageType::WARNING)
                .count(),
            1
        );
    }

    #[test]
    fn deleted_external_root_keeps_workspace_and_standalone_diagnostics_updating() {
        missing_external_root_diagnostics(false);
    }

    #[test]
    fn initially_missing_root_keeps_workspace_and_standalone_diagnostics_updating() {
        missing_external_root_diagnostics(true);
    }

    #[test]
    fn external_roots_watch_delete_and_configuration_reload() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let external = tempdir();
        let script = external.path().join("external.script");
        fs::write(&script, "class External {}").unwrap();
        fs::write(
            temp.path().join(".redscript"),
            format!(
                "source_roots = [ {:?} ]",
                external.path().to_string_lossy().replace('\\', "/")
            ),
        )
        .unwrap();
        server.files_changed(vec![], &ctx).unwrap();
        assert!(server.resolve_file(&script).as_workspace().is_some());
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 1);
        let _buffers = open(&mut server, &ctx, &script, "class Unsaved {}", 2);
        fs::remove_file(&script).unwrap();
        server.files_changed(vec![script.clone()], &ctx).unwrap();
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 1);
        server.document_changed(script, None, &ctx).unwrap();
        assert!(server.compilation_cache.borrow_file_ids().is_empty());
        server
            .change_workspace_folders(vec![], vec![temp.path().to_owned()], &ctx)
            .unwrap();
        assert!(server.workspaces.is_empty());
    }

    #[test]
    fn added_workspace_folder_reports_configuration_errors() {
        let (_temp, mut server, ctx, receiver) = fixture();
        let added = tempdir();
        let dotfile = added.path().join(".redscript");
        fs::write(&dotfile, "source_roots = 123\n").unwrap();
        server
            .change_workspace_folders(vec![added.path().to_owned()], vec![], &ctx)
            .unwrap();
        server.publish_all_diagnostics(&ctx).unwrap();
        let dotfile_uri = ctx.uri(&dialect::identity(&dotfile)).unwrap();
        let messages: Vec<_> = receiver.try_iter().collect();
        assert!(messages.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "textDocument/publishDiagnostics"
                    && serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params.clone())
                        .is_ok_and(|p| p.uri == dotfile_uri
                            && p.diagnostics.len() == 1
                            && p.diagnostics[0].code
                                == Some(lsp::NumberOrString::String("INVALID_CONFIG".into()))))
        }));
        assert!(messages.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "window/showMessage"
                    && serde_json::from_value::<lsp::ShowMessageParams>(notif.params.clone())
                        .is_ok_and(|p| p.typ == lsp::MessageType::WARNING
                            && p.message.contains("Invalid configuration")))
        }));
    }

    #[test]
    fn ignored_overlay_is_excluded_from_workspace_compilation() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let text = "class Ignored { var value : Int32; }";
        let ignored = temp.path().join("ignored.script");
        fs::write(&ignored, text).unwrap();
        fs::write(temp.path().join(".ignore"), "ignored.script\n").unwrap();
        dialect::clear_identity_cache();

        // The workspace walk skips the ignored file, and an open buffer for it
        // is treated as standalone instead of being injected into the workspace.
        let _buffers = open(&mut server, &ctx, &ignored, text, 1);
        assert!(server.resolve_file(&ignored).as_workspace().is_none());
        assert!(
            !server
                .compilation_cache
                .borrow_file_ids()
                .contains_key(&ignored)
        );
        assert!(server.standalone.contains_key(&ignored));

        // Removing the ignore rule and refreshing restores workspace membership.
        fs::remove_file(temp.path().join(".ignore")).unwrap();
        server
            .files_changed(vec![temp.path().to_owned()], &ctx)
            .unwrap();
        assert!(server.resolve_file(&ignored).as_workspace().is_some());
        assert!(
            server
                .compilation_cache
                .borrow_file_ids()
                .contains_key(&ignored)
        );
    }

    fn dot_completions(
        server: &mut RedscriptLanguageServer,
        ctx: &LspContext,
        path: &Path,
        text: &str,
        version: i32,
    ) -> Arc<lsp::CompletionResponse> {
        let buffers = open(server, ctx, path, text, version);
        let buffer = buffers.get(&ctx.uri(path).unwrap()).unwrap();
        let pos = text.rfind('.').unwrap() as u32 + 1;
        server
            .completion_at(
                CodeLocation::new(Document::new(path.to_owned(), buffer), pos),
                ctx,
            )
            .unwrap()
    }

    #[test]
    fn member_completion_ranges_preserve_utf8_boundaries() {
        for (source, cursor, expected) in [
            ("values.", 7, Some(6..7)),
            ("values.Size", 8, Some(6..11)),
            ("values.é", 9, Some(6..9)),
            ("values.é", 8, None),
            ("values + S", 10, None),
            ("values. S", 9, None),
            ("values", 6, None),
            ("", 0, None),
            ("values.", 20, None),
        ] {
            assert_eq!(
                member_completion_range(source, cursor),
                expected,
                "{source}"
            );
        }
    }

    #[test]
    fn member_completions_recompute_after_sequential_changes() {
        for (extension, function, receiver) in [
            ("script", "function", "Other"),
            ("reds", "func", "ref<Other>"),
        ] {
            let (temp, mut server, ctx, _receiver) = fixture();
            let path = temp.path().join(format!("members.{extension}"));
            let uri = ctx.uri(&path).unwrap();
            let mut buffers = crate::buffers::Buffers::default();
            buffers.open(uri.clone(), String::new(), 0);
            let mut previous = String::new();
            for (index, prefix) in ["", "S", "Si", "é", "é界", "S"].into_iter().enumerate() {
                let text = format!(
                    "class Other {{ public {function} Size() -> String {{ return \"custom\"; }} }} \
                     {function} Test(values: {receiver}) {{ let text: String = \"😀é\"; values.{prefix}; }}"
                );
                let change = if index == 0 {
                    lsp::TextDocumentContentChangeEvent {
                        range: None,
                        range_length: None,
                        text: text.clone(),
                    }
                } else {
                    let start = previous.rfind('.').unwrap() + 1;
                    let end = previous.rfind(';').unwrap();
                    lsp::TextDocumentContentChangeEvent {
                        range: Some(lsp::Range::new(
                            crate::positions::position(&previous, start as u32).unwrap(),
                            crate::positions::position(&previous, end as u32).unwrap(),
                        )),
                        range_length: None,
                        text: prefix.into(),
                    }
                };
                assert!(buffers.change(&uri, index as i32 + 1, vec![change]));
                let buffer = buffers.get(&uri).unwrap();
                assert_eq!(buffer.contents().to_string(), text);
                previous.clone_from(&text);
                server
                    .document_changed(path.clone(), Some(buffer), &ctx)
                    .unwrap();
                let pos = (text.rfind('.').unwrap() + 1 + prefix.len()) as u32;
                let response = server
                    .completion_at(
                        CodeLocation::new(Document::new(path.clone(), buffer), pos),
                        &ctx,
                    )
                    .unwrap();
                let lsp::CompletionResponse::Array(items) = &*response else {
                    panic!("expected completion items");
                };
                assert_eq!(items.len(), 1, "{extension}: {prefix}");
                assert_eq!(items[0].label, "Size");
                assert_eq!(items[0].detail.as_deref(), Some("() -> String"));
                server.compilation_cache.with_sources(|sources| {
                    assert_eq!(
                        sources
                            .files()
                            .find(|(_, file)| file.path() == dialect::identity(&path))
                            .unwrap()
                            .1
                            .source(),
                        text
                    );
                });
            }
        }
    }

    /// A speculative query lowers only the function enclosing the cursor, so a
    /// completion must still resolve a receiver type declared in another file
    /// even when an unrelated function precedes the queried one.
    #[test]
    fn member_completion_resolves_a_cross_file_receiver_in_a_later_function() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let other = temp.path().join("other.reds");
        fs::write(
            &other,
            "public class Other { public func Size() -> String { return \"x\"; } }",
        )
        .unwrap();
        server.files_changed(vec![other], &ctx).unwrap();
        let path = temp.path().join("members.reds");
        let text = "public func Unrelated() -> Int32 { return 0; }\n\
                    public func Test(values: ref<Other>) { values.Si; }";
        let response = dot_completions(&mut server, &ctx, &path, text, 1);
        let lsp::CompletionResponse::Array(items) = &*response else {
            panic!("expected completion items");
        };
        assert_eq!(items.len(), 1);
        assert_eq!(items[0].label, "Size");
    }

    #[test]
    fn member_completions_specialize_generic_receiver_methods() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("generic_members.reds");
        let text = "public class Box<T> { public let value: T; public func Get() -> T { return this.value; } public func Replace(replacement: T) -> T { return replacement; } } public func Test(box: Box<Int32>) { box.; }";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let pos = (text.rfind("box.").unwrap() + "box.".len()) as u32;
        let response = server
            .completion_at(
                CodeLocation::new(Document::new(path.clone(), buffer), pos),
                &ctx,
            )
            .unwrap();
        let lsp::CompletionResponse::Array(items) = &*response else {
            panic!("expected completion items");
        };
        let get = items
            .iter()
            .find(|item| item.label == "Get")
            .expect("Get completion");
        assert_eq!(
            get.detail.as_deref(),
            Some("() -> Int32"),
            "{:?}",
            get.detail
        );
        let replace = items
            .iter()
            .find(|item| item.label == "Replace")
            .expect("Replace completion");
        assert_eq!(
            replace.detail.as_deref(),
            Some("(replacement: Int32) -> Int32"),
            "{:?}",
            replace.detail
        );
        assert!(
            items
                .iter()
                .all(|item| !item.detail.as_deref().unwrap_or("").contains("-> T")),
            "{:?}",
            items.iter().map(|item| &item.detail).collect::<Vec<_>>()
        );
    }

    #[test]
    fn member_completions_specialize_generic_receiver_fields() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("generic_fields.reds");
        let text = "public class Box<T> { public let value: T; } public class Derived<T> extends Box<array<T>> {} public func Test(box: Box<Int32>, derived: Derived<Int32>) { box.; derived.; }";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        for (receiver, expected) in [("box.", ": Int32"), ("derived.", ": array<Int32>")] {
            let pos = (text.rfind(receiver).unwrap() + receiver.len()) as u32;
            let response = server
                .completion_at(
                    CodeLocation::new(Document::new(path.clone(), buffer), pos),
                    &ctx,
                )
                .unwrap();
            let lsp::CompletionResponse::Array(items) = &*response else {
                panic!("expected completion items");
            };
            let value = items
                .iter()
                .find(|item| item.label == "value")
                .expect("value completion");
            assert_eq!(value.detail.as_deref(), Some(expected), "{receiver}");
        }
    }

    #[test]
    fn member_completions_specialize_non_generic_subclass_of_generic_base() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("generic_base.reds");
        let text = "public class Box<T> { public let value: T; public func Get() -> T { return this.value; } } public class IntBox extends Box<Int32> {} public func Test(box: IntBox) { box.; }";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let pos = (text.rfind("box.").unwrap() + "box.".len()) as u32;
        let response = server
            .completion_at(
                CodeLocation::new(Document::new(path.clone(), buffer), pos),
                &ctx,
            )
            .unwrap();
        let lsp::CompletionResponse::Array(items) = &*response else {
            panic!("expected completion items");
        };
        let value = items
            .iter()
            .find(|item| item.label == "value")
            .expect("value completion");
        assert_eq!(
            value.detail.as_deref(),
            Some(": Int32"),
            "{:?}",
            value.detail
        );
        let get = items
            .iter()
            .find(|item| item.label == "Get")
            .expect("Get completion");
        assert_eq!(
            get.detail.as_deref(),
            Some("() -> Int32"),
            "{:?}",
            get.detail
        );
    }

    #[test]
    fn closing_standalone_file_drops_cached_fixes() {
        let (_temp, mut server, ctx, _receiver) = fixture();
        let external = tempdir();
        let path = dialect::identity(&external.path().join("solo.reds"));
        let text = "public class Test { let value: Int32; }";
        let uri = ctx.uri(&path).unwrap();
        insert_cached_fix(&mut server, path.clone(), uri, text);
        assert!(server.resolve_file(&path).as_workspace().is_none());
        assert!(server.cached_fixes.contains_key(&path));
        assert_eq!(server.apply_document(&path, None, &ctx), Some(true));
        assert!(!server.cached_fixes.contains_key(&path));
    }

    #[test]
    fn rebuild_prunes_cached_fixes_for_removed_files() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let deleted = dialect::identity(&temp.path().join("deleted.reds"));
        fs::write(&deleted, "public class Deleted {}").unwrap();
        server.files_changed(vec![deleted.clone()], &ctx).unwrap();
        assert!(
            server
                .compilation_cache
                .borrow_file_ids()
                .contains_key(&deleted)
        );
        server.cached_fixes.insert(deleted.clone(), BTreeMap::new());

        fs::remove_file(&deleted).unwrap();
        server.files_changed(vec![deleted.clone()], &ctx).unwrap();
        server.collect_diagnostic_batch(&ctx, || false).unwrap();
        assert!(!server.cached_fixes.contains_key(&deleted));
    }

    #[test]
    fn local_and_field_bindings_shadow_type_names_in_hover_and_definition() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("shadow.script");
        let text = "class Value {}\nclass Owner {\n    var Value: Int32;\n    function Read(): Int32 { return Value; }\n    function Test(): Int32 {\n        let Value: Int32 = 1;\n        return Value;\n    }\n}\n";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let line_of =
            |index: usize| text[..index].bytes().filter(|byte| *byte == b'\n').count() as u32;
        let values: Vec<usize> = text
            .match_indices("Value")
            .map(|(index, _)| index)
            .collect();
        assert_eq!(values.len(), 5, "unexpected Value occurrences");
        // Occurrences: class name, field declaration, field usage, local
        // declaration, local usage.
        for (usage, declaration) in [(values[2], values[1]), (values[4], values[3])] {
            let loc = CodeLocation::new(Document::new(path.clone(), buffer), (usage + 1) as u32);
            let hover = server.hover_at(loc.clone(), &ctx).unwrap().unwrap();
            let lsp::HoverContents::Markup(contents) = hover.contents else {
                panic!("expected markup hover");
            };
            assert!(
                !contents.value.contains("### Symbol"),
                "shadowed type leaked into hover: {}",
                contents.value
            );
            let lsp::GotoDefinitionResponse::Array(locations) =
                server.definition_at(loc, &ctx).unwrap()
            else {
                panic!("expected definition locations");
            };
            assert_eq!(locations.len(), 1);
            assert_eq!(
                locations[0].range.start.line,
                line_of(declaration),
                "definition for usage at {usage}"
            );
        }
    }

    #[test]
    fn redmod_member_completion_recovers_missing_terminators() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("incomplete.script");
        for (index, statement) in [
            "values.",
            "values.Si",
            "values.é界",
            "values.;",
            "Use(values.)",
            "Use(values.);",
            "Use(values.) // trailing comment\n",
            "Use(values.) /* nested /* comment */ é */",
            "Use(values. /* comment */)",
            "Use(values. // comment\r)",
            "Use(values. // comment\u{2028})",
            "Use(Identity(values.))",
            "values.child.",
            "items[0].",
            "let result: Other; result = values.",
            "values. // trailing comment\n",
            "let result: Other = values.",
        ]
        .into_iter()
        .enumerate()
        {
            let text = format!(
                "class Other {{ public var child: Other; \
                     public function Size() -> String {{ return \"custom\"; }} }} \
                 function Use(value: Other) {{}} \
                 function Identity(value: Other) -> Other {{ return value; }} \
                 function Test(values: Other) {{ let items: array<Other>; {statement} }}"
            );
            let response = dot_completions(&mut server, &ctx, &path, &text, index as i32 + 1);
            let lsp::CompletionResponse::Array(items) = &*response else {
                panic!("expected completion items");
            };
            assert!(
                items.iter().any(|item| item.label == "Size"),
                "{statement}: {items:?}"
            );
            server.compilation_cache.with_sources(|sources| {
                assert_eq!(
                    sources
                        .files()
                        .find(|(_, file)| file.path() == dialect::identity(&path))
                        .unwrap()
                        .1
                        .source(),
                    text,
                );
            });
        }
    }

    #[test]
    fn completion_terminator_repair_is_local_and_strict() {
        for (statement, expected) in [
            ("values.", true),
            ("values.;", false),
            ("Use(values.)", true),
            ("Use(items[values.])", true),
            ("Use(values.,", false),
            ("values. +", false),
            ("values.; other", false),
            ("other values.", false),
            ("values. other", false),
            ("values.\nlet other: Int32 = 1", false),
            ("values. /* unterminated", false),
            ("new Other().", false),
        ] {
            let mut source = format!("function Test(values: Int32) {{ {statement} }}");
            let dot = source.find('.').unwrap();
            let member = member_completion_range(&source, dot as u32 + 1).unwrap();
            source.replace_range(member.clone(), &" ".repeat(member.len()));
            let original = source.clone();
            assert_eq!(
                repair_completion_terminator(&mut source, member),
                expected,
                "{statement}"
            );
            if !expected {
                assert_eq!(
                    source, original,
                    "failed repairs must not modify the source"
                );
            }
        }
    }

    #[test]
    fn redmod_array_completion_recovers_missing_terminator() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("arrays.script");
        let text = "function Test(values: array<String>) { values.Si }";
        let response = dot_completions(&mut server, &ctx, &path, text, 1);
        let lsp::CompletionResponse::Array(items) = &*response else {
            panic!("expected completion items");
        };
        let last = items.iter().find(|item| item.label == "Last").unwrap();
        assert_eq!(last.detail.as_deref(), Some("() -> String"));
    }

    #[test]
    fn prefixed_array_completions_use_the_current_receiver_type() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("arrays.script");
        let uri = ctx.uri(&path).unwrap();
        let mut buffers = crate::buffers::Buffers::default();
        buffers.open(uri.clone(), String::new(), 0);
        for (index, (typ, prefix, expected)) in [
            ("array<Int32>", "", Some("() -> Int32")),
            ("array<Int32>", "S", Some("() -> Int32")),
            ("array<String>", "Si", Some("() -> String")),
            ("array<String>", "é", Some("() -> String")),
            ("Int32", "S", None),
        ]
        .into_iter()
        .enumerate()
        {
            let text = format!("function Test(values: {typ}) {{ values.{prefix}; }}");
            assert!(buffers.change(
                &uri,
                index as i32 + 1,
                vec![lsp::TextDocumentContentChangeEvent {
                    range: None,
                    range_length: None,
                    text: text.clone(),
                }]
            ));
            let buffer = buffers.get(&uri).unwrap();
            server
                .document_changed(path.clone(), Some(buffer), &ctx)
                .unwrap();
            let pos = (text.rfind('.').unwrap() + 1 + prefix.len()) as u32;
            let response = server
                .completion_at(
                    CodeLocation::new(Document::new(path.clone(), buffer), pos),
                    &ctx,
                )
                .unwrap();
            let lsp::CompletionResponse::Array(items) = &*response else {
                panic!("expected completion items");
            };
            if let Some(expected) = expected {
                let last = items.iter().find(|item| item.label == "Last").unwrap();
                assert_eq!(last.detail.as_deref(), Some(expected));
            } else {
                assert!(items.is_empty(), "stale receiver completions: {items:?}");
            }
        }
    }

    #[test]
    fn redmod_array_dot_completion_uses_shared_descriptors_and_typed_boot_signatures() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("arrays.script");
        for (version, element) in [(1, "Int32"), (2, "String")] {
            let text = format!("function Test(values: array<{element}>) {{ values.; }}");
            let response = dot_completions(&mut server, &ctx, &path, &text, version);
            let lsp::CompletionResponse::Array(items) = &*response else {
                panic!("expected completion items");
            };
            let mut expected: Vec<_> = redscript_redmod_adapter::array_operations()
                .iter()
                .map(|operation| operation.name)
                .collect();
            let mut labels: Vec<_> = items.iter().map(|item| item.label.as_str()).collect();
            expected.sort();
            labels.sort();
            assert_eq!(labels, expected);
            let size = items.iter().find(|item| item.label == "Size").unwrap();
            assert_eq!(size.detail.as_deref(), Some("() -> Int32"));
            assert_eq!(size.insert_text.as_deref(), Some("Size()"));
            let push = items.iter().find(|item| item.label == "PushBack").unwrap();
            assert_eq!(push.detail, Some(format!("(elem: {element}) -> Void")));
            assert_eq!(push.insert_text.as_deref(), Some("PushBack(${1:elem})"));
            assert_eq!(push.kind, Some(lsp::CompletionItemKind::METHOD));
            assert_eq!(
                push.insert_text_format,
                Some(lsp::InsertTextFormat::SNIPPET)
            );
            assert!(push.documentation.is_some());
            let last = items.iter().find(|item| item.label == "Last").unwrap();
            assert_eq!(last.detail, Some(format!("() -> {element}")));
            // Queries and their completion patches never become active snapshots.
            assert_eq!(server.compilation_cache.borrow_file_ids().len(), 1);
            assert_eq!(
                server.compilation_cache.with_sources(ast::SourceMap::len),
                2
            );
        }
    }

    #[test]
    fn completion_resolve_restores_deferred_documentation() {
        let (temp, mut server, mut ctx, _receiver) = fixture();
        ctx.set_presentation_capabilities(
            &serde_json::from_value(serde_json::json!({
                "textDocument": {"completion": {"completionItem": {
                    "snippetSupport": true,
                    "labelDetailsSupport": true,
                    "documentationFormat": ["markdown"],
                    "resolveSupport": {"properties": ["documentation"]},
                }}},
            }))
            .unwrap(),
        );
        assert!(ctx.supports_lazy_completion_documentation());
        let path = temp.path().join("arrays.script");
        let text = "function Test(values: array<String>) { values.; }";
        let response = dot_completions(&mut server, &ctx, &path, text, 1);
        let lsp::CompletionResponse::Array(items) = &*response else {
            panic!("expected completion items");
        };
        let push = items.iter().find(|item| item.label == "PushBack").unwrap();
        assert!(
            push.documentation.is_none(),
            "documentation should be deferred: {push:?}"
        );
        let data = push.data.clone().expect("deferred doc payload");
        assert!(data.get("generation").is_some());
        assert!(data.get("id").is_some());

        let resolved = server.completion_resolve(push.clone(), &ctx).unwrap();
        assert!(resolved.documentation.is_some());
        assert!(resolved.data.is_none(), "resolve must consume the payload");

        // A resolve against a superseded response must not attach stale docs.
        let text = "function Test(values: array<Int32>) { values.; }";
        dot_completions(&mut server, &ctx, &path, text, 2);
        let stale = server.completion_resolve(push.clone(), &ctx).unwrap();
        assert!(stale.documentation.is_none());
    }

    /// Prints end-to-end member-completion latency for a multi-file workspace.
    ///
    /// Run explicitly with
    /// `cargo test --bin redscript-ide -- --ignored completion_latency`.
    #[test]
    #[ignore = "timing benchmark; run explicitly"]
    fn completion_latency_benchmark() {
        use std::time::Instant;

        let (temp, mut server, ctx, _receiver) = fixture();
        // A workspace large enough that parsing and lowering dominate a cold
        // speculative query.
        for index in 0..30 {
            let path = temp.path().join(format!("mod{index}.reds"));
            let text = format!(
                "public class Host{index} {{ public func Value() -> Int32 {{ return {index}; }} }}"
            );
            fs::write(&path, text).unwrap();
        }
        let path = temp.path().join("bench.reds");
        let text = "public class Bench { public func Value() -> Int32 { return 1; } } \
                    public func Test(host: ref<Bench>) { host.Val; }";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        let loc = CodeLocation::new(
            Document::new(path.clone(), buffer),
            text.rfind("; }").unwrap() as u32,
        );

        let mut samples = Vec::new();
        for _ in 0..20 {
            // Force the cold speculative path, as typing `.` does.
            server.completion_cache.lock().unwrap().take();
            let started = Instant::now();
            server.completion_at(loc.clone(), &ctx).unwrap();
            samples.push(started.elapsed().as_secs_f64() * 1000.0);
        }
        samples.sort_by(f64::total_cmp);
        eprintln!(
            "completion latency: n={} p50={:.3}ms p95={:.3}ms min={:.3}ms max={:.3}ms",
            samples.len(),
            samples[samples.len() / 2],
            samples[(samples.len() * 95 / 100).min(samples.len() - 1)],
            samples[0],
            samples[samples.len() - 1],
        );
    }

    #[test]
    fn redmod_array_completion_is_not_offered_in_community_sources() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let community = temp.path().join("community.reds");
        let response = dot_completions(
            &mut server,
            &ctx,
            &community,
            "func Test(values: array<Int32>) { values.; }",
            1,
        );
        assert!(matches!(&*response, lsp::CompletionResponse::Array(items) if items.is_empty()));
    }

    #[test]
    fn redmod_formatting_preserves_native_syntax_trivia_and_utf16_ranges() {
        let (temp, server, ctx, receiver) = fixture();
        let path = temp.path().join("format.script");
        let source = "class Test {\r\n// comment { 😀\r\noverride function Run() {\r\nvar text: String = \"first\n  second\"; // }\r\n/* block\n unmodified */\r\n}\r\n} // é😀";
        let expected = "class Test {\r\n    // comment { 😀\r\n    override function Run() {\r\n        var text: String = \"first\n  second\"; // }\r\n        /* block\n unmodified */\r\n    }\r\n} // é😀";
        let buffer = crate::buffers::Buffer::new(source.into());
        let edits = server
            .format_document(Document::new(path.clone(), &buffer), 2, &ctx)
            .unwrap();
        assert_eq!(edits.len(), 1);
        assert_eq!(edits[0].new_text, expected);
        assert_eq!(
            edits[0].range,
            lsp::Range::new(lsp::Position::new(0, 0), lsp::Position::new(8, 8))
        );
        let mut formatted = crate::buffers::Buffer::new(source.into());
        formatted.update_range(edits[0].range, edits[0].new_text.clone());
        assert_eq!(formatted.contents().to_string(), expected);
        assert!(
            server
                .format_document(Document::new(path.clone(), &formatted), 8, &ctx)
                .unwrap()
                .is_empty()
        );
        receiver.try_iter().for_each(drop);
        let invalid = crate::buffers::Buffer::new("function Broken( {".into());
        assert!(
            server
                .format_document(Document::new(path.clone(), &invalid), 4, &ctx)
                .unwrap()
                .is_empty()
        );
        assert!(receiver.try_iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "window/logMessage"
                    && serde_json::from_value::<lsp::LogMessageParams>(notif.params.clone())
                        .is_ok_and(|p| p.message.contains("Formatting skipped")))
        }));
        assert!(
            server
                .format_document(
                    Document::new(temp.path().join("unknown.txt"), &buffer),
                    4,
                    &ctx
                )
                .unwrap()
                .is_empty()
        );
        // No semantic validity is required for indentation-only formatting.
        assert!(ctx.uri(&path).is_ok());
    }

    #[test]
    fn community_formatting_honors_client_tab_size() {
        let (temp, server, ctx, _receiver) = fixture();
        let path = temp.path().join("format.reds");
        let source = "public class Test {\npublic func Act() {\nreturn;\n}\n}\n";
        let buffer = crate::buffers::Buffer::new(source.into());
        let format = |tab: u16| {
            let edits = server
                .format_document(Document::new(path.clone(), &buffer), tab, &ctx)
                .unwrap();
            assert_eq!(edits.len(), 1, "expected one formatting edit");
            edits[0].new_text.clone()
        };
        assert!(format(2).contains("\n  public func Act"));
        assert!(format(4).contains("\n    public func Act"));
    }

    #[test]
    fn dotfile_format_indent_overrides_client_tab_size() {
        let temp = tempdir();
        let cache = temp.path().join("cache.bin");
        fs::write(
            &cache,
            ScriptBundle::default().into_writeable().to_bytes().unwrap(),
        )
        .unwrap();
        fs::write(temp.path().join(".redscript"), "[format]\nindent = 3\n").unwrap();
        let server = RedscriptLanguageServer::new(&cache, [temp.path()]).unwrap();
        let (sender, _) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        let path = temp.path().join("format.reds");
        let buffer = crate::buffers::Buffer::new(
            "public class Test {\npublic func Act() {\nreturn;\n}\n}\n".into(),
        );
        let edits = server
            .format_document(Document::new(path, &buffer), 8, &ctx)
            .unwrap();
        assert_eq!(edits.len(), 1);
        assert!(edits[0].new_text.contains("\n   public func Act"));
    }

    #[test]
    fn unchanged_formatting_and_fixes_reject_redmod_and_stale_targets() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let path = temp.path().join("test.script");
        let buffers = open(&mut server, &ctx, &path, "class Native {}", 1);
        assert!(
            server
                .format_document(
                    Document::new(path.clone(), buffers.get(&ctx.uri(&path).unwrap()).unwrap()),
                    2,
                    &ctx,
                )
                .unwrap()
                .is_empty()
        );
        let mut fix = Fix {
            kind: FixKind::MakeFieldPublic,
            target_name: None,
            diagnostic_range: None,
            diagnostic_code: None,
            uri: ctx.uri(&path).unwrap(),
            edit: lsp::TextEdit::new(lsp::Range::default(), "public ".into()),
            expected_source_hash: content_hash(b"class Native {}"),
        };
        assert!(!server.valid_fix(&fix));
        let community = temp.path().join("test.reds");
        let _buffers = open(
            &mut server,
            &ctx,
            &community,
            "public class Community {}",
            1,
        );
        fix.uri = ctx.uri(&community).unwrap();
        fix.expected_source_hash = content_hash(b"public class Community {}");
        assert!(server.valid_fix(&fix));
        fix.expected_source_hash = content_hash(b"");
        assert!(!server.valid_fix(&fix));
        let action = lsp::CodeAction {
            title: "stale".into(),
            data: Some(serde_json::to_value(FixRequest::FixOne(Box::new(fix))).unwrap()),
            ..Default::default()
        };
        assert!(
            server
                .code_action_resolve(action, &ctx)
                .unwrap()
                .edit
                .is_none()
        );
        // A community diagnostic can still propose an edit in another dialect.
        let redmod_fix = Fix {
            kind: FixKind::MakeFieldPublic,
            target_name: None,
            diagnostic_range: None,
            diagnostic_code: None,
            uri: ctx.uri(&path).unwrap(),
            edit: lsp::TextEdit::new(lsp::Range::default(), "public ".into()),
            expected_source_hash: content_hash(b"class Native {}"),
        };
        server
            .cached_fixes
            .entry(community)
            .or_default()
            .insert((0, 1), vec![redmod_fix]);
        let action = lsp::CodeAction {
            title: "fix all".into(),
            data: Some(
                serde_json::to_value(FixRequest::FixAll(
                    FixKind::MakeFieldPublic,
                    server.generation,
                    FixScope::Workspace(temp.path().to_path_buf()),
                ))
                .unwrap(),
            ),
            ..Default::default()
        };
        let resolved = server.code_action_resolve(action, &ctx).unwrap();
        // Every candidate fix is stale or from another dialect, so the fix-all
        // resolves to no edit rather than an empty workspace edit.
        assert!(resolved.edit.is_none());
    }

    #[test]
    fn matching_diagnostics_require_identical_codes() {
        let fix = Fix {
            kind: FixKind::MakeFieldPublic,
            target_name: None,
            diagnostic_range: Some(lsp::Range::default()),
            diagnostic_code: Some("INACCESSIBLE_FIELD".into()),
            uri: "file:///C:/scripts/test.reds".parse().unwrap(),
            edit: lsp::TextEdit::new(lsp::Range::default(), String::new()),
            expected_source_hash: content_hash(b""),
        };
        let coded = |code: &str| lsp::Diagnostic {
            code: Some(lsp::NumberOrString::String(code.into())),
            ..Default::default()
        };
        assert_eq!(
            fix.matching_diagnostics(&[coded("INACCESSIBLE_FIELD")])
                .len(),
            1
        );
        assert!(fix.matching_diagnostics(&[coded("OTHER")]).is_empty());
        assert!(
            fix.matching_diagnostics(&[lsp::Diagnostic::default()])
                .is_empty()
        );
    }

    fn server() -> RedscriptLanguageServer {
        RedscriptLanguageServer {
            workspaces: HashMap::new(),
            config_warnings: BTreeSet::new(),
            reported_config_warnings: BTreeSet::new(),
            io_warnings: BTreeSet::new(),
            reported_io_warnings: BTreeSet::new(),
            standalone_paths: HashSet::new(),
            compilation_cache: CompilationCache::default(),
            cached_fixes: HashMap::new(),
            overlays: HashMap::new(),
            standalone: HashMap::new(),
            generation: 0,
            missing_roots: BTreeSet::new(),
            reported_missing_roots: BTreeSet::new(),
            warned_extensions: HashSet::new(),
            diagnostics: DiagnosticTracker::default(),
            completion_cache: Mutex::new(None),
            completion_generation: AtomicU64::new(0),
            workspace_rebuilds: 0,
        }
    }

    #[test]
    fn diagnostics_include_codes_and_supported_tags() {
        let mut server = server();
        let root = PathBuf::from("C:/scripts");
        server.workspaces.insert(
            root.clone(),
            WorkspaceDir {
                roots: vec![root.clone()],
                format_settings: FormatSettings::default(),
                format_indent: None,
            },
        );
        let sources = ast::SourceMap::new();
        let id = sources.push_back(root.join("test.reds"), "let unused = 1;\n");
        let diags = [
            Diagnostic::UnusedLocal(None, Span::new(4, 10, id)),
            Diagnostic::MissingFunctionBody(Span::new(0, 3, id)),
            Diagnostic::LoweringError(LowerError::DeprecatedNameOf(Span::new(11, 14, id))),
            Diagnostic::LoweringError(LowerError::RedundantDynCast(Span::new(14, 15, id))),
        ];
        let symbols = Symbols::with_default_types();
        let (sender, receiver) = crossbeam_channel::unbounded();
        let mut ctx = LspContext::new(sender);
        for supports_tags in [false, true] {
            if supports_tags {
                let capabilities = serde_json::from_value(serde_json::json!({
                    "textDocument": { "publishDiagnostics": { "tagSupport": { "valueSet": [1, 2] } } }
                }))
                .unwrap();
                ctx.set_diagnostic_capabilities(&capabilities);
            }
            server
                .publish_diagnostics(&diags.iter().collect::<Vec<_>>(), &symbols, &sources, &ctx)
                .unwrap();
            let lsp_server::Message::Notification(notification) = receiver.try_recv().unwrap()
            else {
                panic!("expected diagnostics notification");
            };
            let params: lsp::PublishDiagnosticsParams =
                serde_json::from_value(notification.params).unwrap();
            let unused = &params.diagnostics[1];
            assert_eq!(
                unused.code,
                Some(lsp::NumberOrString::String("UNUSED_LOCAL".into()))
            );
            assert_eq!(unused.source.as_deref(), Some("redscript"));
            assert_eq!(unused.severity, Some(lsp::DiagnosticSeverity::WARNING));
            assert_eq!(
                unused.tags,
                supports_tags.then(|| vec![lsp::DiagnosticTag::UNNECESSARY])
            );
            assert_eq!(
                params.diagnostics[0].severity,
                Some(lsp::DiagnosticSeverity::ERROR)
            );
            assert_eq!(params.diagnostics[0].tags, None);
            assert_eq!(
                params.diagnostics[2].tags,
                supports_tags.then(|| vec![lsp::DiagnosticTag::DEPRECATED])
            );
            assert_eq!(
                params.diagnostics[3].tags,
                supports_tags.then(|| vec![lsp::DiagnosticTag::UNNECESSARY])
            );
        }
    }

    #[test]
    fn replacement_conflict_diagnostics_include_related_information() {
        let mut server = server();
        let root = PathBuf::from("C:/scripts");
        let previous_path = root.join("z-first.reds");
        let primary_path = root.join("a-second.reds");
        let sources = ast::SourceMap::new();
        use redscript_compiler_frontend::{Aggregate, FunctionType, Method, MethodFlags, TypeDef};

        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let mut symbols = Symbols::with_default_types();
        let mut aggregate = Aggregate::default();
        aggregate.methods_mut().add(
            "Value",
            Method::new(
                MethodFlags::new().with_is_native(true),
                FunctionType::default(),
                None,
                [],
                None,
            ),
        );
        symbols.add_type(
            interner.intern("NativeTarget"),
            TypeDef::new([], TypeSchema::Aggregate(aggregate.into()), []),
        );
        sources.push_back(
            &previous_path,
            "\n@replaceMethod(NativeTarget)\nfunc Value() {}",
        );
        sources.push_back(
            &primary_path,
            "\n\n@replaceMethod(NativeTarget)\nfunc Value() {}",
        );
        sources.populate_boot_lib();
        let (_, symbols) =
            redscript_compiler_api::infer_from_sources(&sources, symbols, &mut reporter, &interner);
        let diags = reporter.into_reported();
        assert_eq!(diags.len(), 1, "{diags:?}");
        assert!(matches!(
            diags[0],
            Diagnostic::ReplaceMethodConflict(_, _, _, _)
        ));
        let (sender, receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        server
            .publish_diagnostics(&diags.iter().collect::<Vec<_>>(), &symbols, &sources, &ctx)
            .unwrap();

        let lsp_server::Message::Notification(notification) = receiver.try_recv().unwrap() else {
            panic!("expected diagnostics notification");
        };
        assert_eq!(notification.method, "textDocument/publishDiagnostics");
        let params: lsp::PublishDiagnosticsParams =
            serde_json::from_value(notification.params).unwrap();
        assert_eq!(params.uri, ctx.uri(&primary_path).unwrap());
        assert_eq!(params.diagnostics.len(), 1);
        let diagnostic = &params.diagnostics[0];
        assert_eq!(
            diagnostic.code,
            Some(lsp::NumberOrString::String(
                "REPLACE_METHOD_CONFLICT".into()
            ))
        );
        assert_eq!(
            diagnostic.range,
            lsp::Range::new(lsp::Position::new(2, 0), lsp::Position::new(3, 15))
        );
        assert_eq!(
            diagnostic.related_information,
            Some(vec![lsp::DiagnosticRelatedInformation {
                location: lsp::Location {
                    uri: ctx.uri(&previous_path).unwrap(),
                    range: lsp::Range::new(lsp::Position::new(1, 0), lsp::Position::new(2, 15)),
                },
                message: "previous replacement is overridden here".into(),
            }])
        );
        assert!(receiver.try_recv().is_err());
    }

    #[test]
    fn publishing_replaces_diagnostics_without_clearing_first() {
        let mut server = server();
        let root = PathBuf::from("C:/scripts");
        server.workspaces.insert(
            root.clone(),
            WorkspaceDir {
                roots: vec![root.clone()],
                format_settings: FormatSettings::default(),
                format_indent: None,
            },
        );
        let sources = ast::SourceMap::new();
        let id = sources.push_back(root.join("test.reds"), "let unused = 1;\n");
        let diags = [Diagnostic::UnusedLocal(None, Span::new(4, 10, id))];
        let symbols = Symbols::with_default_types();
        let (sender, receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);

        for _ in 0..2 {
            server
                .publish_diagnostics(&diags.iter().collect::<Vec<_>>(), &symbols, &sources, &ctx)
                .unwrap();
            let messages: Vec<_> = receiver.try_iter().collect();
            assert_eq!(messages.len(), 1);
            let lsp_server::Message::Notification(notification) = &messages[0] else {
                panic!("expected diagnostics notification");
            };
            let params: lsp::PublishDiagnosticsParams =
                serde_json::from_value(notification.params.clone()).unwrap();
            assert_eq!(params.diagnostics.len(), 1);
        }
        server
            .publish_diagnostics(&[], &symbols, &sources, &ctx)
            .unwrap();
        let messages: Vec<_> = receiver.try_iter().collect();
        assert_eq!(messages.len(), 1);
        let lsp_server::Message::Notification(notification) = &messages[0] else {
            panic!("expected diagnostics notification");
        };
        let params: lsp::PublishDiagnosticsParams =
            serde_json::from_value(notification.params.clone()).unwrap();
        assert!(params.diagnostics.is_empty());
        server
            .publish_diagnostics(&[], &symbols, &sources, &ctx)
            .unwrap();
        assert!(receiver.try_recv().is_err());
    }

    #[test]
    fn publications_are_sorted_by_path_and_diagnostics_by_span_code_message() {
        let mut server = server();
        let root = PathBuf::from("C:/scripts");
        server.workspaces.insert(
            root.clone(),
            WorkspaceDir {
                roots: vec![root.clone()],
                format_settings: FormatSettings::default(),
                format_indent: None,
            },
        );
        let sources = ast::SourceMap::new();
        let z = sources.push_back(root.join("z.reds"), "test");
        let a = sources.push_back(root.join("a.reds"), "test");
        let mut diags = [
            Diagnostic::UnusedLocal(None, Span::new(0, 1, z)),
            Diagnostic::UnusedLocal(None, Span::new(2, 3, a)),
            Diagnostic::UnknownIntrinsic("z", Span::new(0, 2, a)),
            Diagnostic::UnknownIntrinsic("a", Span::new(0, 2, a)),
            Diagnostic::MissingFunctionBody(Span::new(0, 2, a)),
            Diagnostic::UnusedLocal(None, Span::new(0, 1, a)),
        ];
        let expected = [5, 3, 2, 4, 1].map(|i| diags[i].to_string());
        let symbols = Symbols::with_default_types();
        let (sender, receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);

        for _ in 0..2 {
            server
                .diagnostics
                .force_published([root.join("0.reds"), root.join("zz.reds")]);
            server
                .publish_diagnostics(&diags.iter().collect::<Vec<_>>(), &symbols, &sources, &ctx)
                .unwrap();
            let params = receiver
                .try_iter()
                .map(|message| {
                    let lsp_server::Message::Notification(notification) = message else {
                        panic!("expected diagnostics notification");
                    };
                    serde_json::from_value::<lsp::PublishDiagnosticsParams>(notification.params)
                        .unwrap()
                })
                .collect::<Vec<_>>();
            assert_eq!(
                params.iter().map(|p| p.uri.clone()).collect::<Vec<_>>(),
                ["0.reds", "a.reds", "z.reds", "zz.reds"]
                    .map(|name| ctx.uri(&root.join(name)).unwrap())
            );
            assert!(params[0].diagnostics.is_empty());
            assert!(params[3].diagnostics.is_empty());
            assert_eq!(
                params[1]
                    .diagnostics
                    .iter()
                    .map(|d| d.message.clone())
                    .collect::<Vec<_>>(),
                expected
            );
            diags.reverse();
        }
        server
            .publish_diagnostics(&[], &symbols, &sources, &ctx)
            .unwrap();
        let cleared = receiver
            .try_iter()
            .map(|message| {
                let lsp_server::Message::Notification(notification) = message else {
                    panic!("expected diagnostics notification");
                };
                serde_json::from_value::<lsp::PublishDiagnosticsParams>(notification.params)
                    .unwrap()
            })
            .collect::<Vec<_>>();
        assert_eq!(
            cleared.iter().map(|p| p.uri.clone()).collect::<Vec<_>>(),
            ["a.reds", "z.reds"].map(|name| ctx.uri(&root.join(name)).unwrap())
        );
        assert!(cleared.iter().all(|p| p.diagnostics.is_empty()));
    }

    #[test]
    fn diagnostic_and_fix_ranges_use_utf16() {
        let sources = ast::SourceMap::new();
        let id = sources.push_back("test.reds", "let s = \"😀\"; error");
        let file = sources.get(id).unwrap();
        let start = file.source().find("error").unwrap() as u32;
        let range = range(Span::new(start, start + 5, id), file).unwrap();
        assert_eq!(range.start, lsp::Position::new(0, 14));
        assert_eq!(range.end, lsp::Position::new(0, 19));
    }

    fn code_action_fixture() -> (RedscriptLanguageServer, LspContext, PathBuf) {
        let mut server = server();
        let (sender, _) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        let uri: lsp::Uri = "file:///C:/scripts/actions.reds".parse().unwrap();
        let path = dialect::identity(&crate::server::path_from_uri(&uri).unwrap());
        let text = "public class Test {}";
        server.overlays.insert(
            path.clone(),
            Overlay {
                text: text.into(),
                version: 1,
            },
        );
        let fixes = [(0, 10), (2, 4), (4, 6), (6, 6), (10, 12), (12, 14)]
            .into_iter()
            .map(|(start, end)| {
                (
                    (start, end),
                    vec![Fix {
                        kind: FixKind::MakeFieldPublic,
                        target_name: None,
                        diagnostic_range: None,
                        diagnostic_code: None,
                        uri: uri.clone(),
                        edit: lsp::TextEdit::new(lsp::Range::default(), format!("fix {start}")),
                        expected_source_hash: if start == 12 {
                            content_hash(b"stale")
                        } else {
                            content_hash(text.as_bytes())
                        },
                    }],
                )
            })
            .collect();
        server.cached_fixes.insert(path.clone(), fixes);
        (server, ctx, path)
    }

    fn action_context(only: Option<Vec<lsp::CodeActionKind>>) -> lsp::CodeActionContext {
        lsp::CodeActionContext {
            diagnostics: vec![],
            only,
            trigger_kind: None,
        }
    }

    fn literal_actions(actions: lsp::CodeActionResponse) -> Vec<lsp::CodeAction> {
        actions
            .into_iter()
            .map(|action| {
                let lsp::CodeActionOrCommand::CodeAction(action) = action else {
                    panic!("expected a code action");
                };
                action
            })
            .collect()
    }

    #[test]
    fn code_actions_select_all_overlapping_valid_fixes() {
        let (server, ctx, path) = code_action_fixture();
        for (span, expected) in [
            (0..10, 5), // Exact range, nested fixes and a zero-width diagnostic.
            (0..0, 2),  // Cursor at diagnostic start.
            (2..4, 3),  // Exact nested range, including its containing diagnostic.
            (3..5, 4),  // Partial overlap in both directions.
            (4..4, 3),  // Exclude the diagnostic ending here.
            (6..6, 3),  // Include a zero-width diagnostic at the cursor.
            (6..10, 3),
            (10..10, 2), // Exclude the diagnostic ending here.
            (12..14, 0), // Stale fixes are never offered.
        ] {
            let actions = literal_actions(
                server
                    .code_action(path.clone(), span.clone(), action_context(None), &ctx)
                    .unwrap(),
            );
            assert_eq!(actions.len(), expected, "selection {span:?}");
            assert!(
                actions
                    .iter()
                    .all(|action| action.kind == Some(lsp::CodeActionKind::QUICKFIX))
            );
            if !actions.is_empty() {
                assert_eq!(
                    actions
                        .iter()
                        .filter(|action| action.title.contains("fix all"))
                        .count(),
                    1
                );
            }
        }
        for only in [
            vec![],
            vec![lsp::CodeActionKind::REFACTOR],
            vec![lsp::CodeActionKind::new("quickfix.child")],
        ] {
            assert!(
                server
                    .code_action(path.clone(), 0..10, action_context(Some(only)), &ctx)
                    .unwrap()
                    .is_empty()
            );
        }
        for only in [
            vec![lsp::CodeActionKind::QUICKFIX],
            vec![lsp::CodeActionKind::EMPTY],
            vec![lsp::CodeActionKind::REFACTOR, lsp::CodeActionKind::QUICKFIX],
        ] {
            assert!(
                !server
                    .code_action(path.clone(), 0..10, action_context(Some(only)), &ctx)
                    .unwrap()
                    .is_empty()
            );
        }
        assert!(
            server
                .code_action(
                    path.with_extension("script"),
                    0..10,
                    action_context(None),
                    &ctx
                )
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn code_actions_negotiate_lazy_edits_and_preserve_properties_on_resolve() {
        let (mut server, mut ctx, path) = code_action_fixture();
        for support in [
            serde_json::json!({}),
            serde_json::json!({"dataSupport": true, "resolveSupport": {"properties": ["edit"]}}),
            serde_json::json!({"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}}, "resolveSupport": {"properties": ["edit"]}}),
            serde_json::json!({"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}}, "dataSupport": true, "resolveSupport": {"properties": ["command"]}}),
            serde_json::json!({"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}}, "dataSupport": true, "resolveSupport": {"properties": ["edit"]}}),
            serde_json::json!({"codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}}, "dataSupport": true, "disabledSupport": true, "resolveSupport": {"properties": ["edit"]}}),
        ] {
            let lazy = support.get("codeActionLiteralSupport").is_some()
                && support.get("dataSupport") == Some(&serde_json::json!(true))
                && support.pointer("/resolveSupport/properties/0")
                    == Some(&serde_json::json!("edit"));
            let disabled_supported =
                support.get("disabledSupport") == Some(&serde_json::json!(true));
            let capabilities = serde_json::from_value(
                serde_json::json!({"textDocument": {"codeAction": support}}),
            )
            .unwrap();
            ctx.set_code_action_capabilities(&capabilities);
            let actions = literal_actions(
                server
                    .code_action(path.clone(), 0..0, action_context(None), &ctx)
                    .unwrap(),
            );
            assert_eq!(actions.len(), 2);
            for mut action in actions {
                assert_eq!(action.edit.is_none(), lazy);
                assert_eq!(action.data.is_some(), lazy);
                if !lazy {
                    continue;
                }
                action.diagnostics = Some(vec![lsp::Diagnostic::new_simple(
                    lsp::Range::default(),
                    "diagnostic".into(),
                )]);
                action.command = Some(lsp::Command::new("command".into(), "test".into(), None));
                let mut resolved = server.code_action_resolve(action.clone(), &ctx).unwrap();
                assert!(resolved.edit.take().is_some());
                assert_eq!(resolved, action);
                let mut unchanged = action.clone();
                unchanged.data = None;
                assert_eq!(
                    server.code_action_resolve(unchanged.clone(), &ctx).unwrap(),
                    unchanged
                );

                let is_fix_all = matches!(
                    serde_json::from_value::<FixRequest>(action.data.clone().unwrap()),
                    Ok(FixRequest::FixAll(..))
                );
                if is_fix_all {
                    continue;
                }

                // Simulate the target file changing so the cached fix is stale.
                server.overlays.get_mut(&path).unwrap().text = "changed".into();
                action.edit = Some(lsp::WorkspaceEdit::default());
                let mut stale = server.code_action_resolve(action.clone(), &ctx).unwrap();
                assert!(stale.edit.is_none());
                assert_eq!(stale.disabled.take().is_some(), disabled_supported);
                action.edit = None;
                assert_eq!(stale, action);
                server.overlays.get_mut(&path).unwrap().text = "public class Test {}".into();
            }
        }
    }

    /// LSP clients keep `CodeAction.data` as parsed JSON, where numbers are
    /// IEEE 754 doubles and lose precision above `2^53`. Resolving a fix-one
    /// action after such a round trip must still match the cached source hash.
    #[test]
    fn fix_one_survives_client_json_number_round_trip() {
        let (server, mut ctx, path) = code_action_fixture();
        ctx.set_code_action_capabilities(
            &serde_json::from_value(serde_json::json!({
                "textDocument": {"codeAction": {
                    "codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}},
                    "dataSupport": true,
                    "resolveSupport": {"properties": ["edit"]},
                }},
            }))
            .unwrap(),
        );
        assert!(ctx.supports_code_action_resolve_edit());

        let actions = literal_actions(
            server
                .code_action(path, 0..0, action_context(None), &ctx)
                .unwrap(),
        );
        let mut action = actions
            .into_iter()
            .find(|action| action.title == "Make field public")
            .expect("fix one action");
        let payload = action.data.clone().unwrap();
        assert_eq!(
            payload["FixOne"]["expected_source_hash"]
                .as_str()
                .map(str::len),
            Some(16),
            "source hash must travel as text: {payload}"
        );

        action.data = Some(round_trip_client_numbers(payload));
        let resolved = server.code_action_resolve(action, &ctx).unwrap();
        assert!(resolved.disabled.is_none());
        assert!(resolved.edit.is_some());
    }

    /// Round-trips every JSON number through `f64`, like a client that stores
    /// parsed JSON in JavaScript numbers does.
    fn round_trip_client_numbers(value: serde_json::Value) -> serde_json::Value {
        match value {
            serde_json::Value::Number(number) => {
                let float = number.as_f64().expect("finite JSON number");
                if float.fract() == 0.0 && float >= 0.0 {
                    // JavaScript re-serializes integral doubles without a fraction.
                    serde_json::Value::from(float as u64)
                } else {
                    serde_json::json!(float)
                }
            }
            serde_json::Value::Array(items) => {
                items.into_iter().map(round_trip_client_numbers).collect()
            }
            serde_json::Value::Object(entries) => entries
                .into_iter()
                .map(|(key, value)| (key, round_trip_client_numbers(value)))
                .collect(),
            value => value,
        }
    }

    #[test]
    fn resolving_fix_all_preserves_cached_fixes() {
        let server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let mut ctx = LspContext::new(sender);
        let uri: lsp::Uri = "file:///C:/scripts/test.reds".parse().unwrap();
        let mut server = server;
        server.overlays.insert(
            dialect::identity(&crate::server::path_from_uri(&uri).unwrap()),
            Overlay {
                text: Arc::from(""),
                version: 1,
            },
        );
        let kinds = [FixKind::MakeMethodPublic, FixKind::MakeFieldPublic];
        let fixes = kinds
            .into_iter()
            .enumerate()
            .map(|(i, kind)| {
                let start = i as u32;
                (
                    (start, start + 1),
                    vec![Fix {
                        kind,
                        target_name: None,
                        diagnostic_range: None,
                        diagnostic_code: None,
                        uri: uri.clone(),
                        expected_source_hash: content_hash(b""),
                        edit: lsp::TextEdit::new(
                            lsp::Range::new(
                                lsp::Position::new(0, start),
                                lsp::Position::new(0, start + 1),
                            ),
                            "public".into(),
                        ),
                    }],
                )
            })
            .collect();
        server
            .cached_fixes
            .insert(PathBuf::from("test.reds"), fixes);
        let scope = FixScope::Standalone(PathBuf::from("test.reds"));

        for document_changes in [false, true] {
            ctx.set_code_action_capabilities(
                &serde_json::from_value(serde_json::json!({
                    "workspace": { "workspaceEdit": { "documentChanges": document_changes } }
                }))
                .unwrap(),
            );
            for kind in [kinds[0], kinds[0], kinds[1]] {
                let action = lsp::CodeAction {
                    data: Some(
                        serde_json::to_value(FixRequest::FixAll(
                            kind,
                            server.generation,
                            scope.clone(),
                        ))
                        .unwrap(),
                    ),
                    ..Default::default()
                };
                let resolved = server.code_action_resolve(action, &ctx).unwrap();
                let edit = resolved.edit.unwrap();
                if document_changes {
                    let lsp::DocumentChanges::Edits(edits) = edit.document_changes.unwrap() else {
                        panic!("expected document edits");
                    };
                    assert_eq!(edits.len(), 1);
                    assert_eq!(edits[0].text_document.uri, uri);
                    assert_eq!(edits[0].text_document.version, Some(1));
                    assert_eq!(edits[0].edits.len(), 1);
                } else {
                    #[allow(clippy::mutable_key_type)]
                    let changes = edit.changes.unwrap();
                    assert_eq!(changes.len(), 1);
                    assert_eq!(changes[&uri].len(), 1);
                }
                assert_eq!(
                    server
                        .cached_fixes
                        .values()
                        .map(BTreeMap::len)
                        .sum::<usize>(),
                    2
                );
            }
        }
    }

    fn make_public_fix(uri: lsp::Uri, expected_source: &str) -> Fix {
        Fix {
            kind: FixKind::MakeFieldPublic,
            target_name: None,
            diagnostic_range: None,
            diagnostic_code: None,
            uri,
            edit: lsp::TextEdit::new(lsp::Range::default(), "public ".into()),
            expected_source_hash: content_hash(expected_source.as_bytes()),
        }
    }

    fn insert_cached_fix(
        server: &mut RedscriptLanguageServer,
        path: PathBuf,
        uri: lsp::Uri,
        text: &str,
    ) {
        server.overlays.insert(
            path.clone(),
            Overlay {
                text: text.into(),
                version: 1,
            },
        );
        server.cached_fixes.insert(
            path,
            BTreeMap::from([((0, 1), vec![make_public_fix(uri, text)])]),
        );
    }

    #[test]
    fn fix_all_standalone_scope_excludes_other_files() {
        let mut server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        let text = "public class Test {}";
        let uri_a: lsp::Uri = "file:///C:/scripts/a.reds".parse().unwrap();
        let uri_b: lsp::Uri = "file:///C:/scripts/b.reds".parse().unwrap();
        let path_a = dialect::identity(&crate::server::path_from_uri(&uri_a).unwrap());
        let path_b = dialect::identity(&crate::server::path_from_uri(&uri_b).unwrap());
        insert_cached_fix(&mut server, path_a.clone(), uri_a.clone(), text);
        insert_cached_fix(&mut server, path_b, uri_b.clone(), text);

        let action = lsp::CodeAction {
            data: Some(
                serde_json::to_value(FixRequest::FixAll(
                    FixKind::MakeFieldPublic,
                    0,
                    FixScope::Standalone(path_a),
                ))
                .unwrap(),
            ),
            ..Default::default()
        };
        let resolved = server.code_action_resolve(action, &ctx).unwrap();
        #[allow(clippy::mutable_key_type)]
        let changes = resolved.edit.unwrap().changes.unwrap();
        assert_eq!(changes.len(), 1);
        assert!(changes.contains_key(&uri_a));
        assert!(!changes.contains_key(&uri_b));
    }

    #[test]
    fn fix_all_workspace_scope_excludes_other_workspaces() {
        let mut server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        let root_a = tempdir();
        let root_b = tempdir();
        for root in [root_a.path(), root_b.path()] {
            server.workspaces.insert(
                root.to_path_buf(),
                WorkspaceDir {
                    roots: vec![root.to_path_buf()],
                    format_settings: FormatSettings::default(),
                    format_indent: None,
                },
            );
        }
        let text = "public class Test {}";
        let path_a = dialect::identity(&root_a.path().join("a.reds"));
        let path_b = dialect::identity(&root_b.path().join("b.reds"));
        let uri_a = ctx.uri(&path_a).unwrap();
        let uri_b = ctx.uri(&path_b).unwrap();
        insert_cached_fix(&mut server, path_a.clone(), uri_a.clone(), text);
        insert_cached_fix(&mut server, path_b.clone(), uri_b.clone(), text);
        assert_eq!(
            server.workspace_root_for(&path_a),
            Some(root_a.path().to_path_buf())
        );
        assert_eq!(
            server.workspace_root_for(&path_b),
            Some(root_b.path().to_path_buf())
        );

        let action = lsp::CodeAction {
            data: Some(
                serde_json::to_value(FixRequest::FixAll(
                    FixKind::MakeFieldPublic,
                    0,
                    FixScope::Workspace(root_a.path().to_path_buf()),
                ))
                .unwrap(),
            ),
            ..Default::default()
        };
        let resolved = server.code_action_resolve(action, &ctx).unwrap();
        #[allow(clippy::mutable_key_type)]
        let changes = resolved.edit.unwrap().changes.unwrap();
        assert_eq!(changes.len(), 1);
        assert!(changes.contains_key(&uri_a));
        assert!(!changes.contains_key(&uri_b));
    }

    #[test]
    fn unrelated_edits_preserve_valid_fixes() {
        let mut server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        let text = "public class Test {}";
        let uri_a: lsp::Uri = "file:///C:/scripts/a.reds".parse().unwrap();
        let uri_b: lsp::Uri = "file:///C:/scripts/b.reds".parse().unwrap();
        let path_a = dialect::identity(&crate::server::path_from_uri(&uri_a).unwrap());
        let path_b = dialect::identity(&crate::server::path_from_uri(&uri_b).unwrap());
        insert_cached_fix(&mut server, path_a.clone(), uri_a.clone(), text);
        let fix = make_public_fix(uri_a, text);
        assert!(server.valid_fix(&fix));

        // Simulate editing another file and rebuilding; the cached fix for the
        // untouched file A must survive because validity is content-based.
        server.overlays.insert(
            path_b,
            Overlay {
                text: "public class Other {}".into(),
                version: 1,
            },
        );
        server.invalidate();

        assert!(server.valid_fix(&fix));
        assert!(server.cached_fixes.contains_key(&path_a));
        let action = lsp::CodeAction {
            data: Some(serde_json::to_value(FixRequest::FixOne(Box::new(fix))).unwrap()),
            ..Default::default()
        };
        assert!(
            server
                .code_action_resolve(action, &ctx)
                .unwrap()
                .edit
                .is_some()
        );
    }

    #[test]
    fn workspace_edit_sorts_text_edits_per_file() {
        let server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
        let uri: lsp::Uri = "file:///C:/scripts/a.reds".parse().unwrap();
        let edit = |line: u32, text: &str| {
            lsp::TextEdit::new(
                lsp::Range::new(lsp::Position::new(line, 0), lsp::Position::new(line, 1)),
                text.to_owned(),
            )
        };
        let edits = vec![(uri.clone(), vec![edit(5, "e"), edit(1, "a"), edit(3, "c")])];
        #[allow(clippy::mutable_key_type)]
        let changes = server.workspace_edit(edits, &ctx).changes.unwrap();
        let lines: Vec<_> = changes
            .get(&uri)
            .unwrap()
            .iter()
            .map(|edit| edit.range.start.line)
            .collect();
        assert_eq!(lines, [1, 3, 5]);
    }

    #[test]
    fn fix_all_disables_when_every_fix_is_stale() {
        let mut server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let mut ctx = LspContext::new(sender);
        ctx.set_code_action_capabilities(
            &serde_json::from_value(serde_json::json!({
                "textDocument": {"codeAction": {"disabledSupport": true}}
            }))
            .unwrap(),
        );
        let uri: lsp::Uri = "file:///C:/scripts/a.reds".parse().unwrap();
        let path = dialect::identity(&crate::server::path_from_uri(&uri).unwrap());
        // The cached fix is stale: its expected source no longer matches the
        // current overlay text.
        insert_cached_fix(
            &mut server,
            path.clone(),
            uri.clone(),
            "public class Test {}",
        );
        server.overlays.insert(
            path.clone(),
            Overlay {
                text: "public class Other {}".into(),
                version: 1,
            },
        );

        let action = lsp::CodeAction {
            data: Some(
                serde_json::to_value(FixRequest::FixAll(
                    FixKind::MakeFieldPublic,
                    0,
                    FixScope::Standalone(path),
                ))
                .unwrap(),
            ),
            ..Default::default()
        };
        let resolved = server.code_action_resolve(action, &ctx).unwrap();
        assert!(resolved.edit.is_none());
        assert!(resolved.disabled.is_some());
    }

    #[test]
    fn standalone_edits_leave_the_workspace_snapshot_alone() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let workspace_path = temp.path().join("ws.reds");
        let _buffers = open(&mut server, &ctx, &workspace_path, "public class Ws {}", 1);
        let before = server.workspace_rebuilds;

        // A file outside every workspace root is analyzed standalone.
        let external = tempdir();
        let standalone_path = external.path().join("solo.reds");
        let _buffers = open(
            &mut server,
            &ctx,
            &standalone_path,
            "public class Solo {}",
            1,
        );
        assert!(
            server
                .standalone
                .contains_key(&dialect::identity(&standalone_path))
        );
        assert_eq!(
            server.workspace_rebuilds, before,
            "a standalone edit must not rebuild the workspace snapshot"
        );

        // Editing the workspace file still rebuilds the workspace.
        let _buffers = open(&mut server, &ctx, &workspace_path, "public class Ws2 {}", 2);
        assert_eq!(server.workspace_rebuilds, before + 1);
    }

    #[test]
    fn standalone_caches_are_reused_across_reloads_and_edits() {
        let (_temp, mut server, ctx, _receiver) = fixture();
        let external = tempdir();
        let first = external.path().join("first.reds");
        let second = external.path().join("second.reds");
        let _buffers = open(&mut server, &ctx, &first, "public class First {}", 1);
        let _buffers = open(&mut server, &ctx, &second, "public class Second {}", 1);
        // Force both snapshots so reuse can be observed without re-analysis.
        server.collect_diagnostic_batch(&ctx, || false).unwrap();

        let forced = |server: &RedscriptLanguageServer, path: &Path| {
            server
                .standalone
                .get(&dialect::identity(path))
                .expect("standalone cache present")
                .analysis_is_forced()
        };
        assert!(forced(&server, &first));
        assert!(forced(&server, &second));

        // A reload, as every save triggers, must reuse untouched standalone
        // caches instead of re-interning the base-game symbols.
        server.rebuild_scoped(&ctx, true, true, true).unwrap();
        assert!(
            forced(&server, &first),
            "unchanged standalone cache was discarded on reload"
        );
        assert!(forced(&server, &second));

        // Editing one standalone file rebuilds only that file's cache.
        let _buffers = open(&mut server, &ctx, &first, "public class First2 {}", 2);
        assert!(
            !forced(&server, &first),
            "edited standalone cache must be rebuilt"
        );
        assert!(
            forced(&server, &second),
            "untouched standalone cache was discarded on edit"
        );
    }

    #[test]
    fn embedded_cache_loads_base_game_definitions() {
        let dir = tempdir();
        let file = dir.path().join("test.reds");
        fs::write(
            &file,
            "func Test(obj: ref<IScriptable>) -> Bool { return IsDefined(obj); }",
        )
        .unwrap();

        let server = RedscriptLanguageServer::embedded([dir.path()]).unwrap();
        server
            .check_workspace(|_, symbols, diagnostics, _| {
                assert!(
                    diagnostics.is_empty(),
                    "unexpected diagnostics: {diagnostics:?}"
                );
                assert!(
                    symbols
                        .types()
                        .any(|(name, _)| name.as_str() == "IScriptable")
                );
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn only_configured_caches_are_validated_eagerly() {
        let dir = tempdir();
        // Corrupt bytes are accepted without validation (analysis is deferred)
        // but rejected when eager validation is requested.
        let corrupt: CacheBytes = vec![0, 1, 2, 3].into();
        assert!(RedscriptLanguageServer::from_bytes(corrupt.clone(), [dir.path()], false).is_ok());
        assert!(RedscriptLanguageServer::from_bytes(corrupt, [dir.path()], true).is_err());
    }

    #[test]
    fn unsupported_extensions_return_empty_queries_and_warn_once_for_uppercase_reds() {
        let (temp, mut server, ctx, receiver) = fixture();
        for name in ["notes.txt", "WrongCase.Reds", "UPPER.REDS"] {
            let path = temp.path().join(name);
            let text = "public class Test { public func Act() { this.Act(); } }";
            let buffers = open(&mut server, &ctx, &path, text, 1);
            let _buffers = open(&mut server, &ctx, &path, text, 2);
            let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
            let loc = CodeLocation::new(
                Document::new(path.clone(), buffer),
                text.find("Act();").unwrap() as u32,
            );
            assert!(server.hover_at(loc.clone(), &ctx).unwrap().is_none());
            assert!(matches!(
                &*server.completion_at(loc.clone(), &ctx).unwrap(),
                lsp::CompletionResponse::Array(items) if items.is_empty()
            ));
            assert!(matches!(
                server.definition_at(loc, &ctx).unwrap(),
                lsp::GotoDefinitionResponse::Array(locations) if locations.is_empty()
            ));
        }
        let mut warnings = Vec::new();
        let mut popups = Vec::new();
        for message in receiver.try_iter() {
            let lsp_server::Message::Notification(notification) = message else {
                continue;
            };
            match notification.method.as_str() {
                "window/logMessage" => {
                    let params: lsp::LogMessageParams =
                        serde_json::from_value(notification.params).unwrap();
                    if params.typ == lsp::MessageType::WARNING {
                        warnings.push(params.message);
                    }
                }
                "window/showMessage" => {
                    let params: lsp::ShowMessageParams =
                        serde_json::from_value(notification.params).unwrap();
                    if params.typ == lsp::MessageType::WARNING {
                        popups.push(params.message);
                    }
                }
                _ => {}
            }
        }
        for messages in [&warnings, &popups] {
            assert_eq!(messages.len(), 2, "{messages:?}");
            assert!(messages.iter().any(|msg| msg.contains("WrongCase.Reds")));
            assert!(messages.iter().any(|msg| msg.contains("UPPER.REDS")));
            assert!(messages.iter().all(|msg| msg.contains("lowercase '.reds'")));
        }
    }

    #[test]
    fn invalid_dotfile_publishes_config_diagnostic_and_preserves_workspace_analysis() {
        let (temp, _server, ctx, receiver) = fixture();
        let external = tempdir();
        let external_file = external.path().join("ext.reds");
        fs::write(&external_file, "public class ExternalType {}").unwrap();
        let local_file = temp.path().join("local.reds");
        fs::write(&local_file, "public class LocalType {}").unwrap();
        let dotfile = temp.path().join(".redscript");
        fs::write(&dotfile, "unknown_key = true\n").unwrap();

        // Startup with broken .redscript still succeeds using default roots.
        let mut server =
            RedscriptLanguageServer::new(&temp.path().join("cache.bin"), [temp.path()]).unwrap();
        server.log_missing_roots(&ctx);
        server.publish_all_diagnostics(&ctx).unwrap();
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "LocalType"));
                Ok(())
            })
            .unwrap();
        let dotfile_uri = ctx.uri(&dialect::identity(&dotfile)).unwrap();
        let startup_messages: Vec<_> = receiver.try_iter().collect();
        assert!(startup_messages.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "textDocument/publishDiagnostics"
                    && serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params.clone())
                        .is_ok_and(|p| p.uri == dotfile_uri
                            && p.diagnostics.len() == 1
                            && p.diagnostics[0].code == Some(lsp::NumberOrString::String("INVALID_CONFIG".into()))
                            && p.diagnostics[0].range.end.character > 0))
        }));

        // Fixing .redscript clears INVALID_CONFIG and activates external roots + template warnings.
        fs::write(
            &dotfile,
            format!(
                "source_roots = [{:?}, \"{{REDSCRIPT_IDE_UNSET_TEST_VAR}}/missing\"]\n",
                external.path().to_string_lossy().replace('\\', "/"),
            ),
        )
        .unwrap();
        check_and_publish(&mut server, dotfile.clone(), &ctx);
        let fixed_messages: Vec<_> = receiver.try_iter().collect();
        assert!(fixed_messages.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "textDocument/publishDiagnostics"
                    && serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params.clone())
                        .is_ok_and(|p| p.uri == dotfile_uri && p.diagnostics.is_empty()))
        }));
        assert!(fixed_messages.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "window/logMessage"
                    && serde_json::from_value::<lsp::LogMessageParams>(notif.params.clone())
                        .is_ok_and(|p| p.typ == lsp::MessageType::WARNING
                            && p.message.contains("REDSCRIPT_IDE_UNSET_TEST_VAR")))
        }));
        assert!(fixed_messages.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "window/showMessage"
                    && serde_json::from_value::<lsp::ShowMessageParams>(notif.params.clone())
                        .is_ok_and(|p| p.typ == lsp::MessageType::WARNING
                            && p.message.contains("REDSCRIPT_IDE_UNSET_TEST_VAR")))
        }));
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "ExternalType"));
                Ok(())
            })
            .unwrap();

        // Breaking .redscript mid-session preserves the previous valid roots while reporting the error.
        fs::write(&dotfile, "source_roots = 123\n").unwrap();
        check_and_publish(&mut server, dotfile, &ctx);
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "ExternalType"));
                Ok(())
            })
            .unwrap();
        let broken_again: Vec<_> = receiver.try_iter().collect();
        assert!(broken_again.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "textDocument/publishDiagnostics"
                    && serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params.clone())
                        .is_ok_and(|p| p.uri == dotfile_uri && !p.diagnostics.is_empty()))
        }));
    }

    #[test]
    fn non_utf8_files_and_invalid_roots_are_isolated_with_diagnostics() {
        let (temp, _server, ctx, receiver) = fixture();
        let healthy = temp.path().join("healthy.reds");
        fs::write(&healthy, "public class Healthy {}").unwrap();
        let bad_utf8 = temp.path().join("bad_encoding.reds");
        fs::write(&bad_utf8, b"public class Bad { // \xff\xfe }\n").unwrap();
        fs::write(
            temp.path().join(".redscript"),
            format!(
                "source_roots = [\".\", {:?}]\n",
                healthy
                    .join("not-a-dir")
                    .to_string_lossy()
                    .replace('\\', "/"),
            ),
        )
        .unwrap();

        let mut server =
            RedscriptLanguageServer::new(&temp.path().join("cache.bin"), [temp.path()]).unwrap();
        server.log_missing_roots(&ctx);
        server.publish_all_diagnostics(&ctx).unwrap();
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Healthy"));
                Ok(())
            })
            .unwrap();

        let bad_uri = ctx.uri(&dialect::identity(&bad_utf8)).unwrap();
        let initial: Vec<_> = receiver.try_iter().collect();
        assert!(initial.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "textDocument/publishDiagnostics"
                    && serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params.clone())
                        .is_ok_and(|p| p.uri == bad_uri
                            && p.diagnostics.len() == 1
                            && p.diagnostics[0].code == Some(lsp::NumberOrString::String("INVALID_UTF8".into()))))
        }));
        assert!(initial.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "window/logMessage"
                    && serde_json::from_value::<lsp::LogMessageParams>(notif.params.clone())
                        .is_ok_and(|p| p.typ == lsp::MessageType::WARNING
                            && p.message.contains("Skipping invalid source root")))
        }));
        assert!(initial.iter().any(|msg| {
            matches!(msg, lsp_server::Message::Notification(notif)
                if notif.method == "window/showMessage"
                    && serde_json::from_value::<lsp::ShowMessageParams>(notif.params.clone())
                        .is_ok_and(|p| p.typ == lsp::MessageType::WARNING
                            && p.message.contains("Skipping invalid source root")))
        }));

        // Re-saving as valid UTF-8 with a syntax error replaces INVALID_UTF8 with SYNTAX_ERR cleanly.
        fs::write(&bad_utf8, "public class {").unwrap();
        check_and_publish(&mut server, bad_utf8.clone(), &ctx);
        let bad_pubs: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|msg| match msg {
                lsp_server::Message::Notification(notif)
                    if notif.method == "textDocument/publishDiagnostics" =>
                {
                    serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params)
                        .ok()
                        .filter(|p| p.uri == bad_uri)
                }
                _ => None,
            })
            .collect();
        assert_eq!(bad_pubs.len(), 1, "{bad_pubs:?}");
        assert_eq!(
            bad_pubs[0].diagnostics[0].code,
            Some(lsp::NumberOrString::String("SYNTAX_ERR".into()))
        );

        // Fixing the file clears diagnostics and exposes its symbols.
        fs::write(&bad_utf8, "public class Recovered {}").unwrap();
        check_and_publish(&mut server, bad_utf8, &ctx);
        let cleared: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|msg| match msg {
                lsp_server::Message::Notification(notif)
                    if notif.method == "textDocument/publishDiagnostics" =>
                {
                    serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params)
                        .ok()
                        .filter(|p| p.uri == bad_uri)
                }
                _ => None,
            })
            .collect();
        assert_eq!(cleared.len(), 1);
        assert!(cleared[0].diagnostics.is_empty());
        server
            .check_workspace(|_, symbols, _, _| {
                assert!(symbols.types().any(|(id, _)| id.as_str() == "Recovered"));
                Ok(())
            })
            .unwrap();
    }

    #[test]
    fn redmod_capability_diagnostics_explain_analyzer_scope_and_link_docs() {
        let (temp, mut server, ctx, receiver) = fixture();
        let path = temp.path().join("capability.script");
        let _buffers = open(
            &mut server,
            &ctx,
            &path,
            "class Example { var first, second: Int32 = 1; }",
            1,
        );
        check_and_publish(&mut server, path.clone(), &ctx);
        let uri = ctx.uri(&dialect::identity(&path)).unwrap();
        let published: Vec<lsp::PublishDiagnosticsParams> = receiver
            .try_iter()
            .filter_map(|msg| match msg {
                lsp_server::Message::Notification(notif)
                    if notif.method == "textDocument/publishDiagnostics" =>
                {
                    serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params)
                        .ok()
                        .filter(|p| p.uri == uri)
                }
                _ => None,
            })
            .collect();
        assert_eq!(published.len(), 1);
        let diag = &published[0].diagnostics[0];
        assert_eq!(
            diag.code,
            Some(lsp::NumberOrString::String("REDMOD_CAPABILITY".into()))
        );
        assert!(
            diag.message
                .starts_with("unsupported by IDE REDmod analyzer:"),
            "{}",
            diag.message
        );
        assert_eq!(
            diag.code_description
                .as_ref()
                .map(|desc| desc.href.as_str()),
            Some(REDMOD_SUBSET_DOCS_URL)
        );
    }

    #[test]
    fn quick_fixes_name_targets_link_diagnostics_and_keep_same_span_entries() {
        let (temp, mut server, ctx, receiver) = fixture();
        let owner = temp.path().join("owner.reds");
        let caller = temp.path().join("caller.reds");
        fs::write(
            &owner,
            "module Other\npublic class Vault { let first: Int32; let second: Int32; func Secret() {} }\npublic struct Record { public let x: Int32; }\n",
        )
        .unwrap();
        let caller_source = "import Other.*\nfunc Use(v: ref<Vault>) -> Int32 {\n  v.Secret();\n  let r = new Record(1);\n  v.first;\n  v.second;\n  return r.x;\n}\n";
        fs::write(&caller, caller_source).unwrap();
        check_and_publish(&mut server, caller.clone(), &ctx);

        let caller_uri = ctx.uri(&dialect::identity(&caller)).unwrap();
        let published = receiver
            .try_iter()
            .find_map(|msg| match msg {
                lsp_server::Message::Notification(notif)
                    if notif.method == "textDocument/publishDiagnostics" =>
                {
                    serde_json::from_value::<lsp::PublishDiagnosticsParams>(notif.params)
                        .ok()
                        .filter(|p| p.uri == caller_uri)
                }
                _ => None,
            })
            .unwrap();
        assert_eq!(published.diagnostics.len(), 4, "{published:?}");

        let actions = literal_actions(
            server
                .code_action(
                    caller.clone(),
                    0..caller_source.len() as u32,
                    lsp::CodeActionContext {
                        diagnostics: published.diagnostics.clone(),
                        only: None,
                        trigger_kind: None,
                    },
                    &ctx,
                )
                .unwrap(),
        );
        let titles: Vec<_> = actions.iter().map(|a| a.title.as_str()).collect();
        for expected in [
            "Make method `Secret` public",
            "Fix by using `Record` struct constructor",
            "Make field `first` public",
            "Make field `second` public",
            "Make field public - fix all in the workspace",
        ] {
            assert!(
                titles.contains(&expected),
                "missing {expected} in {titles:?}"
            );
        }
        let first_action = actions
            .iter()
            .find(|a| a.title == "Make field `first` public")
            .unwrap();
        assert_eq!(first_action.diagnostics.as_ref().map(Vec::len), Some(1));
        assert_eq!(
            first_action.diagnostics.as_ref().unwrap()[0].code,
            Some(lsp::NumberOrString::String("INACCESSIBLE_FIELD".into()))
        );

        // Two fixes stored at the exact same span are both returned.
        let caller_fixes = server
            .cached_fixes
            .get_mut(&dialect::identity(&caller))
            .unwrap();
        let sample = caller_fixes.values().next().unwrap()[0].clone();
        let mut second_same_span = sample.clone();
        second_same_span.target_name = Some("otherSameSpan".into());
        caller_fixes.insert((0, 1), vec![sample, second_same_span]);

        let same_span_actions = literal_actions(
            server
                .code_action(caller, 0..1, action_context(None), &ctx)
                .unwrap(),
        );
        assert!(
            same_span_actions
                .iter()
                .any(|a| a.title.contains("otherSameSpan"))
        );
    }

    #[test]
    fn quick_fixes_cover_private_imports_deprecated_nameof_and_base_static_calls() {
        let (temp, mut server, ctx, _receiver) = fixture();
        let owner = temp.path().join("owner.reds");
        let caller = temp.path().join("caller.reds");
        let owner_source = "module Other\nclass Hidden {}\npublic class Base { public static func Create() -> Int32 { return 1; } }\npublic class Child extends Base {}\n";
        let caller_source = "import Other.{Hidden, Base, Child}\nfunc Use() -> Int32 {\n  let _n = NameOf(Child);\n  return Child.Create();\n}\n";
        fs::write(&owner, owner_source).unwrap();
        fs::write(&caller, caller_source).unwrap();
        check_and_publish(&mut server, caller.clone(), &ctx);

        let actions = literal_actions(
            server
                .code_action(
                    caller.clone(),
                    0..caller_source.len() as u32,
                    action_context(None),
                    &ctx,
                )
                .unwrap(),
        );
        let titles: Vec<_> = actions.iter().map(|a| a.title.as_str()).collect();
        for expected in [
            "Make type `Hidden` public",
            "Replace with `NameOf<Child>()`",
            "Qualify static call as `Base.Create`",
        ] {
            assert!(
                titles.contains(&expected),
                "missing {expected} in {titles:?}"
            );
        }

        let owner_uri = ctx.uri(&dialect::identity(&owner)).unwrap();
        let caller_uri = ctx.uri(&dialect::identity(&caller)).unwrap();
        let mut owner_buf = crate::buffers::Buffer::new(owner_source.into());
        let mut caller_buf = crate::buffers::Buffer::new(caller_source.into());

        for title in [
            "Make type `Hidden` public",
            "Qualify static call as `Base.Create`",
            "Replace with `NameOf<Child>()`",
        ] {
            let action = actions.iter().find(|a| a.title == title).unwrap();
            #[allow(clippy::mutable_key_type)]
            let changes = action.edit.as_ref().unwrap().changes.as_ref().unwrap();
            if let Some(edits) = changes.get(&owner_uri) {
                for edit in edits {
                    assert!(owner_buf.update_range(edit.range, edit.new_text.clone()));
                }
            }
            if let Some(edits) = changes.get(&caller_uri) {
                for edit in edits {
                    assert!(caller_buf.update_range(edit.range, edit.new_text.clone()));
                }
            }
        }
        assert!(
            owner_buf
                .contents()
                .to_string()
                .contains("public class Hidden {}")
        );
        let fixed_caller = caller_buf.contents().to_string();
        assert!(fixed_caller.contains("NameOf<Child>()"), "{fixed_caller}");
        assert!(fixed_caller.contains("Base.Create()"), "{fixed_caller}");
    }
}
