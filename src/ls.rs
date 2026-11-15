use std::cell::{LazyCell, RefCell};
use std::collections::{BTreeMap, BTreeSet};
use std::ops::{Deref, Range};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::{fmt, fs};

use anyhow::Context;
use hashbrown::{HashMap, HashSet};
use lsp_types::{self as lsp};
use ouroboros::self_referencing;
use redscript_code_edit::{CodeEdit, TextEdit};
use redscript_community_formatter::{FormatSettings, format_document};
use redscript_compiler_api::ast::Span;
use redscript_compiler_api::pass::{DiagnosticPass, UnusedLocals};
use redscript_compiler_api::{
    CompilationInputs, CompileErrorReporter, Diagnostic, LowerError, LoweredCompilationUnit,
    ScriptBundle, SourceMapExt, Symbols, TypeFlagRegistry, TypeInterner, TypeSchema, ast,
};
use redscript_dotfile::Dotfile;
use serde::{Deserialize, Serialize};

use crate::completions;
use crate::dialect::{self, Dialect, ParsedSources};
use crate::query::{AtContext, ExprAt};
use crate::server::{CodeLocation, Document, LanguageServer, LspContext};

#[path = "document_symbols.rs"]
mod document_symbols;

const DIAGNOSTIC_PASSES: &[&'static (dyn DiagnosticPass + 'static)] = &[&UnusedLocals];
const EMBEDDED_CACHE: &[u8] = include_bytes!("../assets/api.redscripts.zlib");
const REDMOD_SUBSET_DOCS_URL: &str =
    "https://github.com/jac3km4/redscript/blob/v1.0.0/crates/compiler/redmod/adapter/README.md";

#[derive(Clone)]
enum CacheBytes {
    Embedded(&'static [u8]),
    Owned(Rc<[u8]>),
}

impl Deref for CacheBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Embedded(bytes) => bytes,
            Self::Owned(bytes) => bytes,
        }
    }
}

impl From<Vec<u8>> for CacheBytes {
    fn from(bytes: Vec<u8>) -> Self {
        Self::Owned(Rc::from(bytes))
    }
}

type FixMap = HashMap<PathBuf, BTreeMap<(u32, u32), Vec<Fix>>>;

pub struct RedscriptLanguageServer {
    workspaces: HashMap<PathBuf, WorkspaceDir>,
    config_diagnostics: BTreeMap<PathBuf, lsp::Diagnostic>,
    config_warnings: BTreeSet<String>,
    reported_config_warnings: RefCell<BTreeSet<String>>,
    last_published_configs: RefCell<HashSet<PathBuf>>,
    read_diagnostics: RefCell<BTreeMap<PathBuf, lsp::Diagnostic>>,
    io_warnings: RefCell<BTreeSet<String>>,
    reported_io_warnings: RefCell<BTreeSet<String>>,
    last_published_reads: RefCell<HashSet<PathBuf>>,
    standalone_paths: HashSet<PathBuf>,
    compilation_cache: CompilationCache,
    cached_fixes: RefCell<FixMap>,
    last_published_files: RefCell<HashSet<PathBuf>>,
    overlays: HashMap<PathBuf, Overlay>,
    standalone: HashMap<PathBuf, CompilationCache>,
    generation: u64,
    missing_roots: RefCell<BTreeSet<PathBuf>>,
    reported_missing_roots: RefCell<BTreeSet<PathBuf>>,
    warned_extensions: HashSet<PathBuf>,
    completion_cache: RefCell<Option<CachedCompletion>>,
}

struct CachedCompletion {
    path: PathBuf,
    receiver_end: usize,
    source_without_member: String,
    other_overlays: BTreeMap<PathBuf, String>,
    response: Rc<lsp::CompletionResponse>,
}

impl RedscriptLanguageServer {
    pub fn new(
        cache_path: &Path,
        workspace_folders: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> anyhow::Result<Self> {
        let cache_bytes = fs::read(cache_path)?;
        Self::from_bytes(cache_bytes.into(), workspace_folders)
    }

    pub fn embedded(
        workspace_folders: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> anyhow::Result<Self> {
        let cache_bytes = miniz_oxide::inflate::decompress_to_vec_zlib(EMBEDDED_CACHE)
            .map_err(|err| anyhow::anyhow!("failed to decompress embedded cache: {err:?}"))?;
        Self::from_bytes(cache_bytes.into(), workspace_folders)
    }

    fn from_bytes(
        cache_bytes: CacheBytes,
        workspace_folders: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> anyhow::Result<Self> {
        let mut workspaces = HashMap::new();
        let mut config_diagnostics = BTreeMap::new();
        let mut config_warnings = BTreeSet::new();
        for dir in workspace_folders {
            let dir = dir.into();
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
        Ok(Self {
            compilation_cache: CompilationCache::from_sources(
                cache_bytes,
                sources,
                TypeInterner::default(),
            )?,
            workspaces,
            config_diagnostics,
            config_warnings,
            reported_config_warnings: RefCell::new(BTreeSet::new()),
            last_published_configs: RefCell::new(HashSet::new()),
            read_diagnostics: RefCell::new(read_diagnostics),
            io_warnings: RefCell::new(io_warnings),
            reported_io_warnings: RefCell::new(BTreeSet::new()),
            last_published_reads: RefCell::new(HashSet::new()),
            standalone_paths: HashSet::new(),
            cached_fixes: RefCell::new(HashMap::new()),
            last_published_files: RefCell::new(HashSet::new()),
            overlays: HashMap::new(),
            standalone: HashMap::new(),
            generation: 0,
            missing_roots: RefCell::new(missing_roots),
            reported_missing_roots: RefCell::new(BTreeSet::new()),
            warned_extensions: HashSet::new(),
            completion_cache: RefCell::new(None),
        })
    }

    fn invalidate(&mut self) {
        self.generation = self.generation.wrapping_add(1);

        self.cached_fixes.borrow_mut().clear();
    }

    fn make_edit_snapshot(&self) -> anyhow::Result<CompilationCache> {
        let sources = ast::SourceMap::new();
        let mut files = BTreeMap::new();
        self.compilation_cache.with_sources(|active| {
            for (_, file) in active.files() {
                if self
                    .compilation_cache
                    .borrow_file_ids()
                    .contains_key(file.path())
                {
                    let text = self
                        .overlays
                        .get(file.path())
                        .map_or_else(|| file.source().to_owned(), |overlay| overlay.text.clone());
                    files.insert(file.path().to_owned(), text);
                }
            }
        });
        for (path, overlay) in &self.overlays {
            if self.resolve_file(path).as_workspace().is_some() {
                files.insert(path.clone(), overlay.text.clone());
            }
        }
        for (path, text) in files {
            sources.push_back(path, text);
        }
        CompilationCache::from_sources(
            self.compilation_cache.borrow_cache_bytes().clone(),
            sources,
            TypeInterner::default(),
        )
    }

    fn make_snapshot(
        &self,
        roots: impl IntoIterator<Item = PathBuf>,
        read_diagnostics: &mut BTreeMap<PathBuf, lsp::Diagnostic>,
        io_warnings: &mut BTreeSet<String>,
    ) -> anyhow::Result<CompilationCache> {
        let mut missing_roots = BTreeSet::new();
        let sources = collect_workspace_sources(
            roots,
            &self.overlays,
            |path| self.resolve_file(path).as_workspace().is_some(),
            &mut missing_roots,
            read_diagnostics,
            io_warnings,
        );
        *self.missing_roots.borrow_mut() = missing_roots;
        CompilationCache::from_sources(
            self.compilation_cache.borrow_cache_bytes().clone(),
            sources,
            TypeInterner::default(),
        )
    }

    fn rebuild(&mut self, ctx: &LspContext) -> anyhow::Result<()> {
        self.rebuild_with_reload(ctx, true)
    }

    fn rebuild_with_reload(&mut self, ctx: &LspContext, reload: bool) -> anyhow::Result<()> {
        let started = std::time::Instant::now();
        ctx.logger().info(format!(
            "Rebuild started reload={reload} overlays={} versions={:?}",
            self.overlays.len(),
            self.overlays
                .iter()
                .map(|(path, overlay)| (path, overlay.version))
                .collect::<Vec<_>>(),
        ));
        let result = self.rebuild_inner(ctx, reload);
        match &result {
            Ok(()) => ctx.logger().info(format!(
                "Rebuild finished generation={} elapsed_ms={:.3}",
                self.generation,
                started.elapsed().as_secs_f64() * 1000.0,
            )),
            Err(error) => ctx.logger().error(format!(
                "Rebuild failed generation={} elapsed_ms={:.3}: {error:#}",
                self.generation,
                started.elapsed().as_secs_f64() * 1000.0,
            )),
        }
        result
    }

    fn rebuild_inner(&mut self, ctx: &LspContext, reload: bool) -> anyhow::Result<()> {
        let started = std::time::Instant::now();
        self.invalidate();
        let mut read_diagnostics = if reload {
            self.completion_cache.borrow_mut().take();
            BTreeMap::new()
        } else {
            self.read_diagnostics
                .borrow()
                .iter()
                .filter(|(path, _)| {
                    self.resolve_file(path).as_workspace().is_some()
                        && !self.overlays.contains_key(*path)
                })
                .map(|(path, diag)| (path.clone(), diag.clone()))
                .collect()
        };
        let mut io_warnings = if reload {
            BTreeSet::new()
        } else {
            self.io_warnings.borrow().clone()
        };
        let workspace = if reload {
            ctx.logger()
                .info("Rebuild phase=workspace discovery, parsing and analysis");
            self.make_snapshot(
                self.workspaces
                    .values()
                    .flat_map(|workspace| workspace.roots.clone()),
                &mut read_diagnostics,
                &mut io_warnings,
            )?
        } else {
            ctx.logger()
                .info("Rebuild phase=cached sources, overlays and analysis (no workspace scan)");
            self.make_edit_snapshot()?
        };
        ctx.logger().info(format!(
            "Rebuild workspace ready elapsed_ms={:.3}",
            started.elapsed().as_secs_f64() * 1000.0,
        ));
        ctx.logger().info("Rebuild phase=standalone analysis");
        let paths: HashSet<_> = self
            .standalone
            .keys()
            .chain(self.standalone_paths.iter())
            .chain(self.overlays.keys())
            .filter(|path| self.resolve_file(path).as_workspace().is_none())
            .cloned()
            .collect();
        let mut standalone = HashMap::new();
        let mut active_standalone_paths = HashSet::new();
        for path in paths {
            let text = self
                .overlays
                .get(&path)
                .map(|overlay| Ok(overlay.text.clone()))
                .unwrap_or_else(|| fs::read_to_string(&path));
            match text {
                Ok(text) => {
                    active_standalone_paths.insert(path.clone());
                    let sources = ast::SourceMap::new();
                    sources.push_back(&path, text);
                    standalone.insert(
                        path,
                        CompilationCache::from_sources(
                            self.compilation_cache.borrow_cache_bytes().clone(),
                            sources,
                            TypeInterner::default(),
                        )?,
                    );
                }
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => {
                    active_standalone_paths.insert(path.clone());
                    record_source_read_error(
                        &path,
                        &error,
                        &mut read_diagnostics,
                        &mut io_warnings,
                    );
                }
            }
        }
        self.compilation_cache = workspace;
        self.standalone = standalone;
        self.standalone_paths = active_standalone_paths;
        *self.read_diagnostics.borrow_mut() = read_diagnostics;
        *self.io_warnings.borrow_mut() = io_warnings;
        if reload {
            self.log_missing_roots(ctx);
        } else {
            self.log_io_warnings(ctx);
        }
        Ok(())
    }

    pub fn publish_all_diagnostics(&self, ctx: &LspContext) -> anyhow::Result<()> {
        self.check_workspace_and_publish(ctx)?;
        for cache in self.standalone.values() {
            cache.analyze(|unit, symbols, diagnostics, sources| {
                self.publish_diagnostics_with_unit(Some(unit), diagnostics, symbols, sources, ctx)
            })?;
        }
        // Also clear removed folders, deleted files and closed unsaved standalone files.
        let active: HashSet<_> = self
            .compilation_cache
            .borrow_file_ids()
            .keys()
            .chain(self.standalone.keys())
            .cloned()
            .collect();
        let stale: Vec<_> = self
            .last_published_files
            .borrow()
            .difference(&active)
            .filter(|path| !self.read_diagnostics.borrow().contains_key(*path))
            .cloned()
            .collect();
        for path in stale {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(&path)?,
                diagnostics: vec![],
                version: None,
            });
            self.last_published_files.borrow_mut().remove(&path);
        }
        for path in self.read_diagnostics.borrow().keys() {
            self.last_published_files.borrow_mut().remove(path);
        }
        self.publish_read_diagnostics(ctx)?;
        self.publish_config_diagnostics(ctx)?;
        Ok(())
    }

    fn publish_read_diagnostics(&self, ctx: &LspContext) -> anyhow::Result<()> {
        let read_diagnostics = self.read_diagnostics.borrow();
        let mut last_published = self.last_published_reads.borrow_mut();
        for (path, diagnostic) in &*read_diagnostics {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(path)?,
                diagnostics: vec![diagnostic.clone()],
                version: None,
            });
            last_published.insert(path.clone());
        }
        let mut stale: Vec<_> = last_published
            .iter()
            .filter(|path| !read_diagnostics.contains_key(*path))
            .cloned()
            .collect();
        stale.sort();
        let published_files = self.last_published_files.borrow();
        for path in stale {
            if !published_files.contains(&path) {
                ctx.notify::<lsp::notification::PublishDiagnostics>(
                    lsp::PublishDiagnosticsParams {
                        uri: ctx.uri(&path)?,
                        diagnostics: vec![],
                        version: self.overlays.get(&path).map(|overlay| overlay.version),
                    },
                );
            }
            last_published.remove(&path);
        }
        Ok(())
    }

    fn publish_config_diagnostics(&self, ctx: &LspContext) -> anyhow::Result<()> {
        let mut last_published = self.last_published_configs.borrow_mut();
        for (path, diagnostic) in &self.config_diagnostics {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(path)?,
                diagnostics: vec![diagnostic.clone()],
                version: None,
            });
            last_published.insert(path.clone());
        }
        let mut stale: Vec<_> = last_published
            .iter()
            .filter(|path| !self.config_diagnostics.contains_key(*path))
            .cloned()
            .collect();
        stale.sort();
        for path in stale {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(&path)?,
                diagnostics: vec![],
                version: None,
            });
            last_published.remove(&path);
        }
        Ok(())
    }

    fn resolve_file(&self, path: &Path) -> FileResolution<'_> {
        let path = dialect::identity(path);
        if let Some((_, _, workspace)) = self
            .workspaces
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
        {
            FileResolution::Workspace(workspace)
        } else {
            FileResolution::NonWorkspace
        }
    }

    fn hover_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Option<lsp::Hover>> {
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
                ctx.logger().info(format!(
                    "Hover payload content_bytes={} range={range:?} expression_span={span:?} contains_requested_byte={:?}",
                    value.len(),
                    span.map(|span| span.contains(requested_pos)),
                ));
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
    ) -> anyhow::Result<Rc<lsp::CompletionResponse>> {
        if Dialect::of(loc.doc().path()).is_none() {
            return Ok(Rc::new(lsp::CompletionResponse::Array(vec![])));
        }
        let contents = loc.doc().buffer().contents().to_string();
        let Some(member) = member_completion_range(&contents, loc.pos()) else {
            ctx.logger().info(format!(
                "Completion empty path={} byte_offset={}: cursor is not after a member-access dot (only member completion is supported)",
                loc.doc().path().display(),
                loc.pos(),
            ));
            return Ok(Rc::new(lsp::CompletionResponse::Array(vec![])));
        };
        ctx.logger().info(format!(
            "Completion receiver path={} byte_offset={} erased_member_range={member:?}",
            loc.doc().path().display(),
            loc.pos(),
        ));
        let path = dialect::identity(loc.doc().path());
        let mut source_without_member = contents;
        source_without_member.replace_range(member.clone(), "");
        let other_overlays: BTreeMap<_, _> = self
            .overlays
            .iter()
            .filter(|(other, _)| **other != path)
            .map(|(path, overlay)| (path.clone(), overlay.text.clone()))
            .collect();
        if let Some(cached) = self.completion_cache.borrow().as_ref()
            && cached.path == path
            && cached.receiver_end == member.start
            && cached.source_without_member == source_without_member
            && cached.other_overlays == other_overlays
        {
            ctx.logger()
                .info("Completion analysis=reused member results");
            return Ok(cached.response.clone());
        }
        let receiver_end = member.start;
        let dialect = Dialect::of(loc.doc().path());
        let completions = self.expr_at_with(
            loc.with_pos(member.start as u32),
            Some(member),
            |at| generate_completions(at, dialect),
            ctx,
        )?;
        let response = Rc::new(lsp::CompletionResponse::Array(completions));
        *self.completion_cache.borrow_mut() = Some(CachedCompletion {
            path,
            receiver_end,
            source_without_member,
            other_overlays,
            response: response.clone(),
        });
        Ok(response)
    }

    fn definition_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
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
                let kind = match def.schema() {
                    TypeSchema::Aggregate(agg) if agg.flags().is_struct() => {
                        lsp::SymbolKind::STRUCT
                    }
                    TypeSchema::Aggregate(_) => lsp::SymbolKind::CLASS,
                    TypeSchema::Enum(_) => lsp::SymbolKind::ENUM,
                    TypeSchema::Primitive => continue,
                };
                let mut push = |name: &str, kind, span: Option<Span>, container_name| {
                    if name.contains(query)
                        && let Some(location) = span.and_then(|span| location(span, sources, ctx))
                    {
                        #[allow(deprecated)]
                        results.push(lsp::SymbolInformation {
                            name: name.to_owned(),
                            kind,
                            tags: None,
                            deprecated: None,
                            location,
                            container_name,
                        });
                    }
                };
                push(id.as_str(), kind, def.span(), None);
                if let TypeSchema::Aggregate(agg) = def.schema() {
                    for method in agg.methods().iter() {
                        push(
                            method.name(),
                            lsp::SymbolKind::METHOD,
                            method.func().span(),
                            Some(id.to_string()),
                        );
                    }
                    for field in agg.fields().iter() {
                        push(
                            field.name(),
                            lsp::SymbolKind::FIELD,
                            field.field().span(),
                            Some(id.to_string()),
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
        Ok(lsp::DocumentSymbolResponse::Nested(
            document_symbols::document_symbols(doc.path(), doc.buffer().contents().to_string()),
        ))
    }

    fn format_document(
        &self,
        doc: Document<'_>,
        _tab_size: u16,
        ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp::TextEdit>> {
        let Some(dialect) = Dialect::of(doc.path()) else {
            return Ok(vec![]);
        };
        let resolved = self.resolve_file(doc.path());
        let settings = resolved
            .as_workspace()
            .map(|ws| &ws.format_settings)
            .unwrap_or(&FormatSettings::DEFAULT);

        let contents = doc.buffer().contents();
        let map = ast::SourceMap::new();
        let id = map.push_back(doc.path(), contents.to_string());
        let file = map.get(id).context("missing source file")?;

        let formatted = match dialect {
            Dialect::Redmod => match redscript_redmod_formatter::format_source(file.source(), id) {
                Ok(formatted) => Some(formatted),
                Err(errors) => {
                    if let Some(first) = errors.first() {
                        ctx.logger().info(format!(
                            "Formatting skipped path={}: syntax error ({first})",
                            doc.path().display(),
                        ));
                    }
                    None
                }
            },
            Dialect::Community => {
                let (module, errors) = format_document(file.source(), id, settings);
                if let Some(first) = errors.first() {
                    ctx.logger().info(format!(
                        "Formatting skipped path={}: syntax error ({first})",
                        doc.path().display(),
                    ));
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
        let fixes = self.cached_fixes.borrow();

        let mut actions = Vec::new();
        let mut fix_all_kinds = Vec::new();
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
                if !self.valid_fix(fix) {
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
                let fix_all = lsp::CodeAction {
                    title: format!("{} - fix all in the workspace", fix.kind),
                    kind: Some(lsp::CodeActionKind::QUICKFIX),
                    data: Some(serde_json::to_value(FixRequest::FixAll(
                        fix.kind,
                        self.generation,
                    ))?),
                    ..Default::default()
                };

                actions.push(fix_all);
            }
        }
        drop(fixes);
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
        let Some(data) = action.data.clone() else {
            return Ok(action);
        };

        let request: FixRequest = serde_json::from_value(data)?;
        match request {
            FixRequest::FixOne(fix) if self.valid_fix(&fix) => {
                action.edit = Some(self.workspace_edit(vec![(fix.uri, vec![fix.edit])], ctx));
            }
            FixRequest::FixAll(kind, generation) if generation == self.generation => {
                let mut unique = HashSet::<(lsp::Uri, lsp::Range)>::new();

                #[allow(clippy::mutable_key_type)]
                let mut edits = std::collections::HashMap::<lsp::Uri, Vec<lsp::TextEdit>>::new();
                for fix in self
                    .cached_fixes
                    .borrow()
                    .values()
                    .flat_map(|f| f.values())
                    .flatten()
                    .filter(|f| f.kind == kind && self.valid_fix(f))
                {
                    if unique.insert((fix.uri.clone(), fix.edit.range)) {
                        edits
                            .entry(fix.uri.clone())
                            .or_default()
                            .push(fix.edit.clone());
                    }
                }

                action.edit = Some(self.workspace_edit(edits.into_iter().collect(), ctx));
            }
            other => {
                action.edit = None;
                if ctx.supports_code_action_disabled() {
                    let reason = match other {
                        FixRequest::FixOne(fix)
                            if crate::server::path_from_uri(&fix.uri).is_ok_and(|path| {
                                Dialect::of(&path) != Some(Dialect::Community)
                            }) =>
                        {
                            "Quick fixes are only supported for community REDscript (.reds) files"
                        }
                        _ => {
                            "This fix is stale because workspace files changed; save to refresh diagnostics"
                        }
                    };
                    action.disabled = Some(lsp::CodeActionDisabled {
                        reason: reason.into(),
                    });
                }
            }
        }
        Ok(action)
    }

    fn workspace_edit(
        &self,
        edits: Vec<(lsp::Uri, Vec<lsp::TextEdit>)>,
        ctx: &LspContext,
    ) -> lsp::WorkspaceEdit {
        let mut edits = edits;
        edits.sort_by(|(a, _), (b, _)| a.as_str().cmp(b.as_str()));
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
        let Ok(path) = crate::server::path_from_uri(&fix.uri) else {
            return false;
        };
        if Dialect::of(&path) != Some(Dialect::Community) || fix.generation != self.generation {
            return false;
        }
        let path = dialect::identity(&path);
        if let Some(overlay) = self.overlays.get(&path) {
            overlay.text == fix.expected_source
        } else {
            fs::read_to_string(&path).is_ok_and(|text| text == fix.expected_source)
        }
    }

    pub fn log_missing_roots(&self, ctx: &LspContext) {
        let current = self.missing_roots.borrow();
        let mut reported = self.reported_missing_roots.borrow_mut();
        let configured: HashSet<_> = self
            .workspaces
            .values()
            .flat_map(|workspace| workspace.roots.iter())
            .collect();
        for root in reported.difference(&current) {
            if configured.contains(root) {
                ctx.logger().info(format!(
                    "Source root '{}' is available again.",
                    root.display()
                ));
            }
        }
        for root in current.difference(&reported) {
            let message = format!(
                "Source root '{}' is missing; treating it as temporarily empty. Restore/create the directory or update source_roots in .redscript, then refresh the workspace.",
                root.display()
            );
            ctx.logger().warn(&message);
            ctx.show_message(lsp::MessageType::WARNING, message);
        }
        *reported = current.clone();
        drop(reported);
        self.log_config_warnings(ctx);
        self.log_io_warnings(ctx);
    }

    fn log_io_warnings(&self, ctx: &LspContext) {
        let current = self.io_warnings.borrow();
        let mut reported = self.reported_io_warnings.borrow_mut();
        for warning in current.difference(&reported) {
            ctx.logger().warn(warning);
            ctx.show_message(lsp::MessageType::WARNING, warning);
        }
        *reported = current.clone();
    }

    fn log_config_warnings(&self, ctx: &LspContext) {
        let mut reported = self.reported_config_warnings.borrow_mut();
        for warning in self.config_warnings.difference(&reported) {
            ctx.logger().warn(warning);
            ctx.show_message(lsp::MessageType::WARNING, warning);
        }
        *reported = self.config_warnings.clone();
    }

    fn reload_workspaces(&mut self) {
        let mut workspaces = HashMap::with_capacity(self.workspaces.len());
        let mut config_diagnostics = BTreeMap::new();
        let mut config_warnings = BTreeSet::new();
        for (dir, previous) in &self.workspaces {
            let loaded = WorkspaceDir::load(dir, Some(previous));
            if let Some(diagnostic) = loaded.diagnostic {
                config_diagnostics.insert(loaded.dotfile_path, diagnostic);
            }
            config_warnings.extend(loaded.warnings);
            workspaces.insert(dir.clone(), loaded.workspace);
        }
        self.workspaces = workspaces;
        self.config_diagnostics = config_diagnostics;
        self.config_warnings = config_warnings;
    }

    fn check_workspace_and_publish(&self, ctx: &LspContext) -> anyhow::Result<()> {
        self.check_workspace(|unit, symbols, diags, sources| {
            self.publish_diagnostics_with_unit(Some(unit), diags, symbols, sources, ctx)
        })
    }

    fn check_workspace<A>(
        &self,
        cb: impl for<'ctx> Fn(
            &LoweredCompilationUnit<'ctx>,
            &Symbols<'ctx>,
            &[&Diagnostic<'ctx>],
            &'ctx ast::SourceMap,
        ) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        self.compilation_cache.analyze(cb)
    }

    #[cfg(test)]
    fn publish_diagnostics<'ctx>(
        &self,
        diags: &[&Diagnostic<'ctx>],
        symbols: &Symbols<'ctx>,
        sources: &'ctx ast::SourceMap,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        self.publish_diagnostics_with_unit(None, diags, symbols, sources, ctx)
    }

    fn publish_diagnostics_with_unit<'ctx>(
        &self,
        unit: Option<&LoweredCompilationUnit<'ctx>>,
        diags: &[&Diagnostic<'ctx>],
        symbols: &Symbols<'ctx>,
        sources: &'ctx ast::SourceMap,
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

        let mut last_diagnostics = self.last_published_files.borrow_mut();
        let mut fixes = self.cached_fixes.borrow_mut();
        for (_, file) in sources.files() {
            fixes.remove(file.path());
        }

        let mut publications = Vec::new();
        let mut current_files = HashSet::new();
        for (file, diags) in file_diags {
            let file = sources.get(file).context("missing source file")?;
            let path = file.path();

            current_files.insert(path.to_owned());

            let slots = fixes.entry_ref(path).or_default();
            for diag in &diags {
                if Dialect::of(path) == Some(Dialect::Community)
                    && let Some(mut fix) = generate_fix(diag, file, unit, symbols, sources, ctx)
                {
                    fix.generation = self.generation;
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
            publications.push((
                path,
                lsp::PublishDiagnosticsParams {
                    uri: ctx.uri(path)?,
                    diagnostics: diags
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
                        .collect(),
                    version: self.overlays.get(path).map(|overlay| overlay.version),
                },
            ));
        }
        let analyzed_paths: HashSet<_> = sources
            .files()
            .map(|(_, file)| file.path().to_owned())
            .collect();
        let is_workspace = sources
            .files()
            .any(|(_, file)| self.resolve_file(file.path()).as_workspace().is_some());
        let read_diagnostics = self.read_diagnostics.borrow();
        for path in last_diagnostics.difference(&current_files).filter(|path| {
            !read_diagnostics.contains_key(*path)
                && (analyzed_paths.contains(*path)
                    || (is_workspace && self.resolve_file(path).as_workspace().is_some())
                    || (!self.overlays.contains_key(*path) && !path.exists()))
        }) {
            publications.push((
                path.as_path(),
                lsp::PublishDiagnosticsParams {
                    uri: ctx.uri(path)?,
                    diagnostics: vec![],
                    version: self.overlays.get(path).map(|overlay| overlay.version),
                },
            ));
        }
        publications.sort_by_key(|(path, _)| *path);
        for (_, params) in publications {
            ctx.notify::<lsp::notification::PublishDiagnostics>(params);
        }

        last_diagnostics.retain(|path| {
            !analyzed_paths.contains(path)
                && !(is_workspace && self.resolve_file(path).as_workspace().is_some())
                && (path.exists() || self.overlays.contains_key(path))
        });
        last_diagnostics.extend(current_files);
        Ok(())
    }

    fn expr_at<A>(
        &self,
        loc: CodeLocation<'_>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        self.expr_at_with(loc, None, cb, ctx)
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
        self.expr_at_with(loc, Some(pos..end), cb, ctx)
    }

    fn expr_at_with<A>(
        &self,
        loc: CodeLocation<'_>,
        patch: Option<Range<usize>>,
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
        ctx.logger().info(format!(
            "Query path={} dialect={:?} byte_offset={} version={:?} workspace={} generation={} source_files={} overlays={} speculative_patch={patch:?}",
            path.display(),
            Dialect::of(&path),
            loc.pos(),
            loc.doc().buffer().version(),
            workspace,
            self.generation,
            base.borrow_file_ids().len(),
            self.overlays.len(),
        ));
        if patch.is_none()
            && let Some(&id) = base.borrow_file_ids().get(&path)
            && base.with_sources(|sources| {
                sources
                    .get(id)
                    .is_some_and(|file| file.source() == loc.doc().buffer().contents())
            })
        {
            ctx.logger().info("Query analysis=reused active snapshot");
            return base.query_at(id, loc.pos(), cb, ctx);
        }
        ctx.logger().info("Query analysis=speculative snapshot");
        // A query owns its storage; speculative sources never enter the active cache.
        let sources = ast::SourceMap::new();
        base.with_sources(|active| {
            for (_, file) in active.files() {
                if dialect::identity(file.path()) != path
                    && base.borrow_file_ids().contains_key(file.path())
                    && workspace
                {
                    sources.push_back(file.path(), file.source());
                }
            }
        });
        let mut contents = loc.doc().buffer().contents().to_string();
        if let Some(range) = patch {
            contents.replace_range(range.clone(), &" ".repeat(range.len()));
            if Dialect::of(&path) == Some(Dialect::Redmod)
                && repair_completion_terminator(&mut contents, range)
            {
                ctx.logger()
                    .info("Query recovery=inserted speculative REDmod statement terminator");
            }
        }

        let id = sources.push_back(&path, contents);
        let query_cache = CompilationCache::from_sources(
            base.borrow_cache_bytes().clone(),
            sources,
            TypeInterner::default(),
        )?;
        query_cache.query_at(id, loc.pos(), cb, ctx)
    }
}

impl CompilationCache {
    fn query_at<A>(
        &self,
        id: ast::FileId,
        pos: u32,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        let preceding_pos = pos.saturating_sub(1);
        self.with(|cache| {
            let match_ = cache
                .analysis
                .parsed
                .modules
                .iter()
                .find(|(file, _)| *file == id)
                .and_then(|(_, module)| module.module().find_at(preceding_pos));
            let (query_context, typ) = match match_ {
                Some(ast::QueryResult::Type(&ast::Type::Named { name, .. })) => (None, Some(name)),
                Some(ast::QueryResult::Expr(&ast::Expr::Ident(name))) => {
                    (Some(AtContext::Expr), Some(name))
                }
                _ => (None, None),
            };

            let analysis = cache.analysis;
            let unit = &analysis.unit;
            let syms = &analysis.symbols;
            let func = unit
                .all_functions()
                .find(|f| f.span.file == id && f.span.contains(preceding_pos));
            let typ = typ
                .and_then(|t| unit.scopes.get(&id)?.get(t)?.id())
                .or_else(|| cache.interner.get_index(cache.interner.get_index_of(typ?)?));
            ExprAt::new(None, func, typ, syms, cache.sources, query_context).with_expr_at(preceding_pos, |at| {
                ctx.logger().info(format!(
                "Query result lookup_byte_offset={preceding_pos} parsed_module={} ast_match={} function={} expression={} named_type={} receiver_type={} context={query_context:?} parse_diagnostics={} semantic_diagnostics={}",
                analysis.parsed.modules.iter().any(|(file, _)| *file == id),
                match_.is_some(),
                func.is_some(),
                at.expr().is_some(),
                typ.is_some(),
                at.expr_type().is_some(),
                analysis.parsed.diagnostics.len(),
                analysis.diagnostics.len(),
            ));
                cb(at)
            })
        })
    }
}

/// Repair only a missing terminator immediately after the completion expression.
/// The adapter deliberately rejects recovered trees, so reparse the temporary
/// source strictly instead of allowing invalid modules into normal analysis.
fn repair_completion_terminator(source: &mut String, member: Range<usize>) -> bool {
    let sources = ast::SourceMap::new();
    let id = sources.push_back("completion.script", "");
    let insertion = {
        let (_, errors) = redscript_redmod_adapter::parse_file_with_metadata(source, id);
        let [redscript_redmod_adapter::Error::Parse(error)] = errors.as_slice() else {
            return false;
        };
        // The pinned parser exposes this recovery as a message, not a typed kind.
        if error.to_string() != "expected ';' after statement in statement" {
            return false;
        }
        let span = error.span();
        if span.start != span.end {
            return false;
        }
        span.start as usize
    };
    let Some(tail) = source.get(member.end..insertion) else {
        return false;
    };
    // Closing calls/indexes may separate the receiver from its statement end.
    // Do not repair another statement or an unfinished argument/operator.
    let Some(end) = completion_statement_end(tail) else {
        return false;
    };
    let insertion = member.end + end;
    source.insert(insertion, ';');
    let valid = {
        let (module, errors) = redscript_redmod_adapter::parse_file_with_metadata(source, id);
        module.is_some() && errors.is_empty()
    };
    if !valid {
        source.remove(insertion);
    }
    valid
}

// Only traverse closing delimiters and trivia, including nested block comments.
// Return the end of the expression, before trailing comments.
fn completion_statement_end(mut tail: &str) -> Option<usize> {
    let length = tail.len();
    let mut end = 0;
    loop {
        tail = tail.trim_start();
        if tail.is_empty() {
            return Some(end);
        }
        if tail.starts_with([')', ']']) {
            tail = &tail[1..];
            end = length - tail.len();
        } else if tail.starts_with("//") {
            tail = tail
                .find([
                    '\n', '\r', '\u{000B}', '\u{000C}', '\u{0085}', '\u{2028}', '\u{2029}',
                ])
                .map_or("", |newline| &tail[newline..]);
        } else {
            tail = tail.strip_prefix("/*")?;
            let mut depth = 1;
            while depth != 0 {
                if let Some(rest) = tail.strip_prefix("/*") {
                    depth += 1;
                    tail = rest;
                } else if let Some(rest) = tail.strip_prefix("*/") {
                    depth -= 1;
                    tail = rest;
                } else {
                    tail = tail.get(tail.chars().next()?.len_utf8()..)?;
                }
            }
        }
    }
}

/// Locate a contiguous member token at the cursor, including its leading dot.
/// Unicode letters are accepted here even though the parsers currently require
/// ASCII identifiers: erase the unfinished token before parsing the receiver.
fn member_completion_range(source: &str, pos: u32) -> Option<Range<usize>> {
    let pos = pos as usize;
    let prefix = source.get(..pos)?;
    let is_member_char = |ch: char| ch == '_' || ch.is_alphanumeric();
    let (dot, ch) = prefix
        .char_indices()
        .rev()
        .find(|(_, ch)| !is_member_char(*ch))?;
    if ch != '.' || dot == 0 {
        return None;
    }
    let suffix = source.get(pos..)?;
    let end = pos
        + suffix
            .char_indices()
            .find(|(_, ch)| !is_member_char(*ch))
            .map_or(suffix.len(), |(index, _)| index);
    Some(dot..end)
}

fn generate_fix<'ctx>(
    diag: &Diagnostic<'ctx>,
    diag_file: &ast::File,
    unit: Option<&LoweredCompilationUnit<'ctx>>,
    symbols: &Symbols<'ctx>,
    sources: &'ctx ast::SourceMap,
    ctx: &LspContext,
) -> Option<Fix> {
    if let Some(fix) = generate_ide_fix(diag, diag_file, unit, ctx) {
        return Some(fix);
    }
    let code_edit = CodeEdit::from_diagnostic(diag)
        .or_else(|| private_import_code_edit(diag, diag_file, symbols))?;
    let target_name = match &code_edit {
        CodeEdit::FixStructNewConstructor(type_id, _) | CodeEdit::MakeTypePublic(type_id) => {
            Some(type_id.simple_name().to_owned())
        }
        CodeEdit::MakeMethodPublic(method_id) => symbols
            .get_method(*method_id)
            .map(|(name, _)| name.to_owned()),
        CodeEdit::MakeFieldPublic(field_id) => symbols
            .get_field(*field_id)
            .map(|(name, _)| name.to_owned()),
    };
    let edit = TextEdit::from_code_edit(&code_edit, symbols, sources).ok()?;
    let file = sources.get(edit.file)?;
    if Dialect::of(file.path()) != Some(Dialect::Community) {
        return None;
    }
    let edit_range = range(Span::new(edit.start as _, edit.end as _, edit.file), file)?;
    Some(Fix {
        kind: FixKind::from(&code_edit),
        target_name,
        diagnostic_range: range(diag.span(), diag_file),
        diagnostic_code: Some(diag.code().into()),
        uri: ctx.uri(file.path()).ok()?,
        edit: lsp::TextEdit::new(edit_range, edit.content.to_string()),
        expected_source: file.source().to_owned(),
        generation: 0,
    })
}

fn private_import_code_edit<'ctx>(
    diag: &Diagnostic<'ctx>,
    diag_file: &ast::File,
    symbols: &Symbols<'ctx>,
) -> Option<CodeEdit<'ctx>> {
    let Diagnostic::ImportIsPrivate(name, span) = diag else {
        return None;
    };
    let raw = diag_file
        .source()
        .get(span.start as usize..span.end as usize)?
        .trim();
    let rest = raw
        .strip_prefix("import")?
        .trim_start()
        .trim_end_matches(';')
        .trim();
    let compact: String = rest.chars().filter(|ch| !ch.is_whitespace()).collect();
    let qualified = if let Some(prefix) = compact.strip_suffix(".*") {
        format!("{prefix}.{name}")
    } else if let Some((prefix, _)) = compact.split_once(".{") {
        format!("{prefix}.{name}")
    } else if compact == *name || compact.ends_with(&format!(".{name}")) {
        compact
    } else {
        return None;
    };
    let (type_id, _) = symbols
        .types()
        .find(|(id, def)| id.as_str() == qualified && def.span().is_some())?;
    Some(CodeEdit::MakeTypePublic(type_id))
}

fn generate_ide_fix<'ctx>(
    diag: &Diagnostic<'ctx>,
    diag_file: &ast::File,
    unit: Option<&LoweredCompilationUnit<'ctx>>,
    ctx: &LspContext,
) -> Option<Fix> {
    if Dialect::of(diag_file.path()) != Some(Dialect::Community) {
        return None;
    }
    let source = diag_file.source();
    let (kind, target_name, edit_span, replacement) = match diag {
        Diagnostic::LoweringError(LowerError::DeprecatedNameOf(span)) => {
            let slice = source.get(span.start as usize..span.end as usize)?;
            let inner = slice
                .strip_prefix("NameOf")?
                .trim_start()
                .strip_prefix('(')?
                .strip_suffix(')')?
                .trim();
            if !is_ascii_ident(inner) {
                return None;
            }
            (
                FixKind::ReplaceDeprecatedNameOf,
                Some(inner.to_owned()),
                *span,
                format!("NameOf<{inner}>()"),
            )
        }
        Diagnostic::LoweringError(LowerError::CallingBaseStaticMethod {
            type_id,
            name,
            span,
        }) => {
            let base_name = type_id.simple_name();
            let in_scope = if let Some(unit) = unit {
                unit.scopes
                    .get(&span.file)
                    .and_then(|scope| scope.get(base_name))
                    .is_some_and(|type_ref| type_ref.id() == Some(*type_id))
            } else {
                type_id.as_str() == base_name
            };
            if !in_scope {
                return None;
            }
            let slice = source.get(span.start as usize..span.end as usize)?;
            let receiver_len = slice
                .bytes()
                .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
                .count();
            if receiver_len == 0 || !is_ascii_ident(&slice[..receiver_len]) {
                return None;
            }
            let after_dot = slice[receiver_len..]
                .trim_start()
                .strip_prefix('.')?
                .trim_start();
            let after_method = after_dot.strip_prefix(*name)?.trim_start();
            if !after_method.starts_with(['(', '<']) {
                return None;
            }
            let receiver_span = Span::new(
                span.start,
                span.start.checked_add(u32::try_from(receiver_len).ok()?)?,
                span.file,
            );
            (
                FixKind::QualifyBaseStaticMethod,
                Some(format!("{base_name}.{name}")),
                receiver_span,
                base_name.to_owned(),
            )
        }
        _ => return None,
    };
    let edit_range = range(edit_span, diag_file)?;
    Some(Fix {
        kind,
        target_name,
        diagnostic_range: range(diag.span(), diag_file),
        diagnostic_code: Some(diag.code().into()),
        uri: ctx.uri(diag_file.path()).ok()?,
        edit: lsp::TextEdit::new(edit_range, replacement),
        expected_source: source.to_owned(),
        generation: 0,
    })
}

fn is_ascii_ident(text: &str) -> bool {
    let mut chars = text.chars();
    chars
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        && chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

fn generate_completions(
    at: ExprAt<'_, '_>,
    dialect: Option<Dialect>,
) -> Result<Vec<lsp_types::CompletionItem>, anyhow::Error> {
    let mut completions = vec![];

    if let Some(typ) = at.type_()
        && matches!(at.context(), Some(AtContext::Expr))
    {
        match at.symbols()[typ].schema() {
            TypeSchema::Aggregate(_) => {
                let methods = at
                    .symbols()
                    .query_methods(typ)
                    .filter(|m| m.func().flags().is_static())
                    .map(|e| completions::method(e.name(), e.func().type_(), e.func().doc()));
                completions.extend(methods);
            }
            TypeSchema::Enum(enum_) => {
                let variants = enum_
                    .variants()
                    .map(|(name, _)| completions::enum_member(name));
                completions.extend(variants);
            }
            _ => {}
        };
    }

    let typ = at.expr_type();
    if let Some(typ) = typ.as_ref()
        && let Some(typ) = typ.unwrap_ref_or_self().upper_bound()
    {
        if dialect == Some(Dialect::Redmod)
            && typ.id() == redscript_compiler_api::types::predef::ARRAY
            && let [element] = typ.args()
        {
            completions.extend(
                redscript_redmod_adapter::array_operations()
                    .iter()
                    .filter_map(|operation| {
                        completions::array_method(
                            operation.name,
                            operation.intrinsic,
                            element,
                            at.symbols(),
                        )
                    }),
            );
        }

        let methods = at
            .symbols()
            .query_methods(typ.id())
            .filter(|m| !m.func().flags().is_static())
            .map(|e| completions::method(e.name(), e.func().type_(), e.func().doc()));
        completions.extend(methods);

        let fields = at
            .symbols()
            .base_iter_with_self(typ.id())
            .flat_map(|(_, agg)| agg.fields().iter())
            .map(|e| completions::field(e.name(), e.field()));
        completions.extend(fields);
    };

    Ok(completions)
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
        let path = dialect::identity(&path);
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
            return Ok(());
        }
        if let Some(buffer) = buffer {
            self.overlays.insert(
                path.clone(),
                Overlay {
                    text: buffer.contents().to_string(),
                    version: buffer.version().unwrap_or(0),
                },
            );
        } else {
            self.overlays.remove(&path);
            self.standalone.remove(&path);
            self.standalone_paths.remove(&path);
        }
        // Closing must restore disk contents (or remove an unsaved file).
        // Open/change use known sources; filesystem notifications refresh discovery.
        self.rebuild_with_reload(ctx, buffer.is_none())
    }

    fn files_changed(&mut self, _paths: Vec<PathBuf>, ctx: &LspContext) -> anyhow::Result<()> {
        self.reload_workspaces();
        self.rebuild(ctx)
    }

    fn check(&mut self, path: PathBuf, ctx: &LspContext) -> anyhow::Result<()> {
        if path.file_name().is_some_and(|name| name == ".redscript") {
            self.files_changed(vec![path], ctx)?;
            return self.publish_all_diagnostics(ctx);
        }

        let path = dialect::identity(&path);
        if self.resolve_file(&path).as_workspace().is_none()
            && Dialect::of(&path).is_some()
            && path.is_file()
        {
            self.standalone_paths.insert(path);
        }
        self.rebuild(ctx)?;
        self.publish_all_diagnostics(ctx)
    }

    fn change_workspace_folders(
        &mut self,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        for folder in removed {
            self.workspaces.remove(&folder);
        }
        for dir in added {
            let previous = self.workspaces.get(&dir);
            let loaded = WorkspaceDir::load(&dir, previous);
            self.workspaces.insert(dir, loaded.workspace);
        }
        self.reload_workspaces();
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
    ) -> anyhow::Result<Rc<lsp_types::CompletionResponse>> {
        self.completion_at(loc, _ctx)
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

fn diagnostic_tags(diag: &Diagnostic<'_>, ctx: &LspContext) -> Option<Vec<lsp::DiagnosticTag>> {
    let tag = match diag {
        Diagnostic::UnusedLocal(_, _)
        | Diagnostic::UnusedItemQualifiers(_, _)
        | Diagnostic::AddFieldConflict(_)
        | Diagnostic::LoweringError(LowerError::RedundantDynCast(_)) => {
            lsp::DiagnosticTag::UNNECESSARY
        }
        Diagnostic::LoweringError(LowerError::DeprecatedNameOf(_)) => {
            lsp::DiagnosticTag::DEPRECATED
        }
        _ => return None,
    };
    ctx.supports_diagnostic_tag(&tag).then(|| vec![tag])
}

fn location(span: ast::Span, sources: &ast::SourceMap, ctx: &LspContext) -> Option<lsp::Location> {
    let file = sources.get(span.file)?;
    Some(lsp::Location {
        uri: ctx.uri(file.path()).ok()?,
        range: range(span, file)?,
    })
}

fn range(span: ast::Span, file: &ast::File) -> Option<lsp::Range> {
    Some(lsp::Range {
        start: crate::positions::file_position(file, span.start)?,
        end: crate::positions::file_position(file, span.end)?,
    })
}

#[self_referencing]
struct CompilationCache {
    cache_bytes: CacheBytes,
    interner: TypeInterner,
    sources: ast::SourceMap,
    file_ids: HashMap<PathBuf, ast::FileId>,

    #[borrows(cache_bytes, interner, sources)]
    #[not_covariant]
    analysis: DeferredAnalysis<'this>,
}

type DeferredAnalysis<'ctx> =
    LazyCell<AnalyzedSources<'ctx>, Box<dyn FnOnce() -> AnalyzedSources<'ctx> + 'ctx>>;

struct AnalyzedSources<'ctx> {
    parsed: ParsedSources<'ctx>,
    unit: LoweredCompilationUnit<'ctx>,
    symbols: Symbols<'ctx>,
    diagnostics: Vec<Diagnostic<'ctx>>,
}

impl<'ctx> AnalyzedSources<'ctx> {
    fn deferred(
        sources: &'ctx ast::SourceMap,
        symbols: Symbols<'ctx>,
        interner: &'ctx TypeInterner,
    ) -> DeferredAnalysis<'ctx> {
        LazyCell::new(Box::new(move || {
            Self::new(dialect::parse(sources), symbols, interner)
        }))
    }

    fn new(
        parsed: ParsedSources<'ctx>,
        symbols: Symbols<'ctx>,
        interner: &'ctx TypeInterner,
    ) -> Self {
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = parsed.process(symbols, &mut reporter, interner);
        unit.run_diagnostics(DIAGNOSTIC_PASSES, &mut reporter);
        Self {
            parsed,
            unit,
            symbols,
            diagnostics: reporter.into_reported(),
        }
    }
}

impl CompilationCache {
    fn analyze<A>(
        &self,
        cb: impl for<'ctx> Fn(
            &LoweredCompilationUnit<'ctx>,
            &Symbols<'ctx>,
            &[&Diagnostic<'ctx>],
            &'ctx ast::SourceMap,
        ) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        self.with(|cache| {
            let analysis = cache.analysis;
            // Keep syntax errors, but suppress recovery-induced semantic cascades.
            let invalid_files = analysis.parsed.diagnostic_files();
            let diags: Vec<_> = analysis
                .parsed
                .diagnostics
                .iter()
                .chain(
                    analysis
                        .diagnostics
                        .iter()
                        .filter(|diag| !invalid_files.contains(&diag.span().file)),
                )
                .collect();
            cb(&analysis.unit, &analysis.symbols, &diags, cache.sources)
        })
    }

    fn from_sources(
        cache_bytes: CacheBytes,
        sources: ast::SourceMap,
        interner: TypeInterner,
    ) -> anyhow::Result<Self> {
        let file_ids = sources
            .files()
            .map(|(id, file)| (file.path().to_owned(), id))
            .collect();
        sources.populate_boot_lib();

        Self::try_new(
            cache_bytes,
            interner,
            sources,
            file_ids,
            |bytes, interner, sources| {
                let bundle = ScriptBundle::from_bytes(bytes)?;
                let symbols = CompilationInputs::load_without_mapping(
                    &bundle,
                    interner,
                    &TypeFlagRegistry::default(),
                )?;
                Ok(AnalyzedSources::deferred(sources, symbols, interner))
            },
        )
    }
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new(
            CacheBytes::Embedded(&[]),
            TypeInterner::default(),
            ast::SourceMap::default(),
            HashMap::default(),
            |_, interner, _| {
                LazyCell::new(Box::new(move || {
                    AnalyzedSources::new(
                        ParsedSources {
                            modules: vec![],
                            diagnostics: vec![],
                        },
                        Symbols::with_default_types(),
                        interner,
                    )
                }))
            },
        )
    }
}

struct Overlay {
    text: String,
    version: i32,
}

#[derive(Debug, Clone)]
struct WorkspaceDir {
    roots: Vec<PathBuf>,
    format_settings: FormatSettings,
}

struct LoadedWorkspace {
    workspace: WorkspaceDir,
    dotfile_path: PathBuf,
    diagnostic: Option<lsp::Diagnostic>,
    warnings: Vec<String>,
}

impl WorkspaceDir {
    fn from_dotfile(dir: &Path, dotfile: &Dotfile) -> Self {
        let roots = dotfile
            .expanded_source_roots()
            .into_iter()
            .map(|p| dialect::identity(&dir.join(p)))
            .collect();

        let mut settings = FormatSettings::default();
        if let Some(indent) = dotfile.format.indent {
            settings.indent = indent;
        }
        if let Some(max_width) = dotfile.format.max_width {
            settings.max_width = max_width;
        }
        if let Some(max_chain_calls) = dotfile.format.max_chain_calls {
            settings.max_chain_calls = max_chain_calls;
        }
        if let Some(max_chain_fields) = dotfile.format.max_chain_fields {
            settings.max_chain_fields = max_chain_fields;
        }
        if let Some(max_chain_operators) = dotfile.format.max_chain_operators {
            settings.max_chain_operators = max_chain_operators;
        }
        if let Some(max_chain_total) = dotfile.format.max_chain_total {
            settings.max_chain_total = max_chain_total;
        }

        Self {
            roots,
            format_settings: settings,
        }
    }

    fn load(dir: &Path, fallback: Option<&Self>) -> LoadedWorkspace {
        let raw_dotfile = dir.join(".redscript");
        let dotfile_path = dialect::identity(&raw_dotfile);
        let fallback_workspace = || {
            fallback
                .cloned()
                .unwrap_or_else(|| Self::from_dotfile(dir, &Dotfile::default()))
        };

        match fs::read_to_string(&raw_dotfile) {
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => LoadedWorkspace {
                workspace: Self::from_dotfile(dir, &Dotfile::default()),
                dotfile_path,
                diagnostic: None,
                warnings: vec![],
            },
            Err(err) => {
                let message = format!("failed to read .redscript: {err}");
                let warning = format!(
                    "Failed to read '{}': {err}; keeping previous or default workspace configuration.",
                    dotfile_path.display()
                );
                LoadedWorkspace {
                    workspace: fallback_workspace(),
                    dotfile_path,
                    diagnostic: Some(config_diagnostic(lsp::Range::default(), message)),
                    warnings: vec![warning],
                }
            }
            Ok(content) => match toml::from_str::<Dotfile>(&content) {
                Ok(dotfile) => {
                    let warnings =
                        inspect_source_root_templates(&dotfile.source_roots, &dotfile_path);
                    LoadedWorkspace {
                        workspace: Self::from_dotfile(dir, &dotfile),
                        dotfile_path,
                        diagnostic: None,
                        warnings,
                    }
                }
                Err(err) => {
                    let range = err
                        .span()
                        .and_then(|span| {
                            let start = crate::positions::position(
                                &content,
                                u32::try_from(span.start).ok()?,
                            )?;
                            let end = crate::positions::position(
                                &content,
                                u32::try_from(span.end).ok()?,
                            )?;
                            Some(lsp::Range::new(start, end))
                        })
                        .unwrap_or_default();
                    let warning = format!(
                        "Invalid configuration in '{}': {}; keeping previous or default workspace configuration.",
                        dotfile_path.display(),
                        err.message()
                    );
                    LoadedWorkspace {
                        workspace: fallback_workspace(),
                        dotfile_path,
                        diagnostic: Some(config_diagnostic(range, err.message().to_owned())),
                        warnings: vec![warning],
                    }
                }
            },
        }
    }
}

fn collect_workspace_sources(
    roots: impl IntoIterator<Item = PathBuf>,
    overlays: &HashMap<PathBuf, Overlay>,
    is_workspace_file: impl Fn(&Path) -> bool,
    missing_roots: &mut BTreeSet<PathBuf>,
    read_diagnostics: &mut BTreeMap<PathBuf, lsp::Diagnostic>,
    io_warnings: &mut BTreeSet<String>,
) -> ast::SourceMap {
    let sources = ast::SourceMap::new();
    let mut files = BTreeMap::new();
    let discovered = dialect::discover(roots, |issue| match issue {
        dialect::DiscoveryIssue::MissingRoot(root) => {
            missing_roots.insert(root);
        }
        dialect::DiscoveryIssue::InvalidRoot { root, error } => {
            io_warnings.insert(format!(
                "Skipping invalid source root '{}': {error}.",
                root.display()
            ));
        }
        dialect::DiscoveryIssue::WalkError { root, error } => {
            io_warnings.insert(format!(
                "Encountered filesystem error while scanning source root '{}': {error}.",
                root.display()
            ));
        }
    });
    for path in discovered {
        if let Some(overlay) = overlays.get(&path) {
            files.insert(path, overlay.text.clone());
        } else {
            match fs::read_to_string(&path) {
                Ok(text) => {
                    files.insert(path, text);
                }
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => {
                    record_source_read_error(&path, &error, read_diagnostics, io_warnings);
                }
            }
        }
    }
    // Unsaved new files are absent from discovery but still own an active module.
    for (path, overlay) in overlays {
        if is_workspace_file(path) {
            read_diagnostics.remove(path);
            files.insert(path.clone(), overlay.text.clone());
        }
    }
    for (path, text) in files {
        sources.push_back(path, text);
    }
    sources
}

fn record_source_read_error(
    path: &Path,
    error: &std::io::Error,
    read_diagnostics: &mut BTreeMap<PathBuf, lsp::Diagnostic>,
    io_warnings: &mut BTreeSet<String>,
) {
    let (code, message, warning) = if error.kind() == std::io::ErrorKind::InvalidData {
        (
            "INVALID_UTF8",
            "file is not valid UTF-8; re-save it with UTF-8 encoding to enable analysis".to_owned(),
            format!(
                "Skipping '{}': file is not valid UTF-8 ({error}). Re-save it with UTF-8 encoding to enable analysis.",
                path.display()
            ),
        )
    } else {
        (
            "IO_ERROR",
            format!("failed to read source file: {error}"),
            format!(
                "Skipping '{}': failed to read source file ({error}).",
                path.display()
            ),
        )
    };
    io_warnings.insert(warning);
    read_diagnostics.insert(
        path.to_owned(),
        lsp::Diagnostic {
            range: lsp::Range::default(),
            severity: Some(lsp::DiagnosticSeverity::ERROR),
            code: Some(lsp::NumberOrString::String(code.into())),
            source: Some("redscript".into()),
            message,
            ..Default::default()
        },
    );
}

fn config_diagnostic(range: lsp::Range, message: String) -> lsp::Diagnostic {
    lsp::Diagnostic {
        range,
        severity: Some(lsp::DiagnosticSeverity::ERROR),
        code: Some(lsp::NumberOrString::String("INVALID_CONFIG".into())),
        source: Some("redscript".into()),
        message,
        ..Default::default()
    }
}

fn inspect_source_root_templates(roots: &[String], dotfile_path: &Path) -> Vec<String> {
    let mut warnings = Vec::new();
    for template in roots {
        let mut chars = template.char_indices().peekable();
        while let Some((_, ch)) = chars.next() {
            if ch == '\\' {
                chars.next();
                continue;
            }
            if ch == '{' {
                let mut key = String::new();
                let mut closed = false;
                while let Some((_, inner)) = chars.next() {
                    if inner == '\\' {
                        if let Some((_, escaped)) = chars.next() {
                            key.push(escaped);
                        }
                        continue;
                    }
                    if inner == '}' {
                        closed = true;
                        break;
                    }
                    key.push(inner);
                }
                let trimmed = key.trim();
                if !closed || trimmed.is_empty() {
                    warnings.push(format!(
                        "Malformed template '{template}' in '{}' source_roots; treating it literally.",
                        dotfile_path.display()
                    ));
                    break;
                } else if std::env::var_os(trimmed).is_none() {
                    warnings.push(format!(
                        "Environment variable '{trimmed}' referenced in '{}' source_roots is not set; expanding it to an empty string.",
                        dotfile_path.display()
                    ));
                }
            }
        }
    }
    warnings
}

#[derive(Debug)]
enum FileResolution<'a> {
    Workspace(&'a WorkspaceDir),
    NonWorkspace,
}

impl FileResolution<'_> {
    pub fn as_workspace(&self) -> Option<&WorkspaceDir> {
        match self {
            Self::Workspace(ws) => Some(ws),
            Self::NonWorkspace => None,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
enum FixRequest {
    FixOne(Box<Fix>),
    FixAll(FixKind, u64),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Fix {
    kind: FixKind,
    #[serde(default)]
    target_name: Option<String>,
    #[serde(default)]
    diagnostic_range: Option<lsp::Range>,
    #[serde(default)]
    diagnostic_code: Option<String>,
    uri: lsp::Uri,
    edit: lsp::TextEdit,
    expected_source: String,
    generation: u64,
}

impl Fix {
    fn title(&self) -> String {
        let Some(name) = &self.target_name else {
            return self.kind.to_string();
        };
        match self.kind {
            FixKind::FixStructNewConstructor => {
                format!("Fix by using `{name}` struct constructor")
            }
            FixKind::MakeMethodPublic => format!("Make method `{name}` public"),
            FixKind::MakeFieldPublic => format!("Make field `{name}` public"),
            FixKind::MakeTypePublic => format!("Make type `{name}` public"),
            FixKind::ReplaceDeprecatedNameOf => format!("Replace with `NameOf<{name}>()`"),
            FixKind::QualifyBaseStaticMethod => format!("Qualify static call as `{name}`"),
        }
    }

    fn matching_diagnostics(
        &self,
        context_diagnostics: &[lsp::Diagnostic],
    ) -> Vec<lsp::Diagnostic> {
        let Some(diag_range) = self.diagnostic_range else {
            return vec![];
        };
        context_diagnostics
            .iter()
            .filter(|diag| {
                diag.range == diag_range
                    && match (&diag.code, &self.diagnostic_code) {
                        (Some(lsp::NumberOrString::String(actual)), Some(expected)) => {
                            actual == expected
                        }
                        (Some(_), Some(_)) => false,
                        _ => true,
                    }
            })
            .cloned()
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum FixKind {
    FixStructNewConstructor,
    MakeMethodPublic,
    MakeFieldPublic,
    MakeTypePublic,
    ReplaceDeprecatedNameOf,
    QualifyBaseStaticMethod,
}

impl From<&CodeEdit<'_>> for FixKind {
    fn from(edit: &CodeEdit<'_>) -> Self {
        match edit {
            CodeEdit::FixStructNewConstructor(_, _) => Self::FixStructNewConstructor,
            CodeEdit::MakeMethodPublic(_) => Self::MakeMethodPublic,
            CodeEdit::MakeFieldPublic(_) => Self::MakeFieldPublic,
            CodeEdit::MakeTypePublic(_) => Self::MakeTypePublic,
        }
    }
}

impl fmt::Display for FixKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FixStructNewConstructor => write!(f, "Fix by using struct constructor"),
            Self::MakeMethodPublic => write!(f, "Make method public"),
            Self::MakeFieldPublic => write!(f, "Make field public"),
            Self::MakeTypePublic => write!(f, "Make type public"),
            Self::ReplaceDeprecatedNameOf => write!(f, "Replace deprecated NameOf syntax"),
            Self::QualifyBaseStaticMethod => {
                write!(f, "Qualify static method call with declaring type")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use redscript_compiler_api::ir;

    use super::*;

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
        let server =
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
                assert!(Rc::ptr_eq(&previous, &response));
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
        assert!(!Rc::ptr_eq(&previous.unwrap(), &changed));
        server.files_changed(vec![dependency], &ctx).unwrap();
        let refreshed = server.completion_at(loc, &ctx).unwrap();
        assert!(!Rc::ptr_eq(&changed, &refreshed));
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

            server.check(path.clone(), &ctx).unwrap();
            let saved = publications();
            assert_eq!(saved.len(), 1);
            assert_eq!(saved[0].uri, ctx.uri(&path).unwrap());
            assert_eq!(saved[0].version, Some(1));
            assert!(!saved[0].diagnostics.is_empty());

            let _buffers = open(&mut server, &ctx, &path, "class Fixed {}", 2);
            fs::write(&path, "class Fixed {}").unwrap();
            server.files_changed(vec![path.clone()], &ctx).unwrap();
            assert!(publications().is_empty());
            assert!(
                server
                    .last_published_files
                    .borrow()
                    .contains(&dialect::identity(&path))
            );

            server.check(path.clone(), &ctx).unwrap();
            let saved = publications();
            assert_eq!(saved.len(), 1);
            assert_eq!(saved[0].version, Some(2));
            assert!(saved[0].diagnostics.is_empty());

            let _buffers = open(&mut server, &ctx, &path, "class {", 3);
            server.check(path.clone(), &ctx).unwrap();
            assert!(!publications()[0].diagnostics.is_empty());
            server.document_changed(path.clone(), None, &ctx).unwrap();
            server
                .change_workspace_folders(vec![], vec![], &ctx)
                .unwrap();
            assert!(publications().is_empty());
            server.check(path, &ctx).unwrap();
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
        server.check(standalone.clone(), &ctx).unwrap();
        assert_eq!(server.compilation_cache.borrow_file_ids().len(), 2);
        assert_eq!(
            server.standalone[&dialect::identity(&standalone)]
                .borrow_file_ids()
                .len(),
            1
        );
        let _buffers = open(&mut server, &ctx, &redmod, "class {", 6);
        server.check(redmod.clone(), &ctx).unwrap();
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
                        .expr_at_with(loc, Some(pos..pos), check, &ctx)
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
                .expr_at_with(loc, Some(pos..pos), check, &ctx)
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
        server.check(path.clone(), &ctx).unwrap();
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
                    assert_eq!(at.type_().map(|typ| typ.as_str()), Some("Int32"));
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
        server.check(path.clone(), &ctx).unwrap();
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
        assert_eq!(
            server.missing_roots.borrow().len(),
            usize::from(initially_missing)
        );
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
        server.check(standalone.clone(), &ctx).unwrap();
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
        server.check(healthy.clone(), &ctx).unwrap();
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
        server.check(healthy.clone(), &ctx).unwrap();
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

    fn dot_completions(
        server: &mut RedscriptLanguageServer,
        ctx: &LspContext,
        path: &Path,
        text: &str,
        version: i32,
    ) -> Rc<lsp::CompletionResponse> {
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
            expected_source: "class Native {}".into(),
            generation: server.generation,
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
        fix.expected_source = "public class Community {}".into();
        fix.generation = server.generation;
        assert!(server.valid_fix(&fix));
        fix.expected_source.clear();
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
            expected_source: "class Native {}".into(),
            generation: server.generation,
        };
        server
            .cached_fixes
            .borrow_mut()
            .entry(community)
            .or_default()
            .insert((0, 1), vec![redmod_fix]);
        let action = lsp::CodeAction {
            title: "fix all".into(),
            data: Some(
                serde_json::to_value(FixRequest::FixAll(
                    FixKind::MakeFieldPublic,
                    server.generation,
                ))
                .unwrap(),
            ),
            ..Default::default()
        };
        let resolved = server.code_action_resolve(action, &ctx).unwrap();
        assert!(matches!(resolved.edit.unwrap().changes, Some(changes) if changes.is_empty()));
    }

    fn server() -> RedscriptLanguageServer {
        RedscriptLanguageServer {
            workspaces: HashMap::new(),
            config_diagnostics: BTreeMap::new(),
            config_warnings: BTreeSet::new(),
            reported_config_warnings: RefCell::new(BTreeSet::new()),
            last_published_configs: RefCell::new(HashSet::new()),
            read_diagnostics: RefCell::new(BTreeMap::new()),
            io_warnings: RefCell::new(BTreeSet::new()),
            reported_io_warnings: RefCell::new(BTreeSet::new()),
            last_published_reads: RefCell::new(HashSet::new()),
            standalone_paths: HashSet::new(),
            compilation_cache: CompilationCache::default(),
            cached_fixes: RefCell::new(HashMap::new()),
            last_published_files: RefCell::new(HashSet::new()),
            overlays: HashMap::new(),
            standalone: HashMap::new(),
            generation: 0,
            missing_roots: RefCell::new(BTreeSet::new()),
            reported_missing_roots: RefCell::new(BTreeSet::new()),
            warned_extensions: HashSet::new(),
            completion_cache: RefCell::new(None),
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
        let server = server();
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
                .last_published_files
                .borrow_mut()
                .extend([root.join("0.reds"), root.join("zz.reds")]);
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
                        expected_source: text.into(),
                        generation: if start == 12 { 1 } else { 0 },
                    }],
                )
            })
            .collect();
        server.cached_fixes.borrow_mut().insert(path.clone(), fixes);
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

                server.generation += 1;
                action.edit = Some(lsp::WorkspaceEdit::default());
                let mut stale = server.code_action_resolve(action.clone(), &ctx).unwrap();
                assert!(stale.edit.is_none());
                assert_eq!(stale.disabled.take().is_some(), disabled_supported);
                action.edit = None;
                assert_eq!(stale, action);
                server.generation -= 1;
            }
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
                text: String::new(),
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
                        expected_source: String::new(),
                        generation: 0,
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
            .borrow_mut()
            .insert(PathBuf::from("test.reds"), fixes);

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
                        serde_json::to_value(FixRequest::FixAll(kind, server.generation)).unwrap(),
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
                        .borrow()
                        .values()
                        .map(BTreeMap::len)
                        .sum::<usize>(),
                    2
                );
            }
        }
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
        server.check(dotfile.clone(), &ctx).unwrap();
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
        server.check(dotfile, &ctx).unwrap();
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
        server.check(bad_utf8.clone(), &ctx).unwrap();
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
        server.check(bad_utf8, &ctx).unwrap();
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
        server.check(path.clone(), &ctx).unwrap();
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
        server.check(caller.clone(), &ctx).unwrap();

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
        let mut slots = server.cached_fixes.borrow_mut();
        let caller_fixes = slots.get_mut(&dialect::identity(&caller)).unwrap();
        let sample = caller_fixes.values().next().unwrap()[0].clone();
        let mut second_same_span = sample.clone();
        second_same_span.target_name = Some("otherSameSpan".into());
        caller_fixes.insert((0, 1), vec![sample, second_same_span]);
        drop(slots);

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
        server.check(caller.clone(), &ctx).unwrap();

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
