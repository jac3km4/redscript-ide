use std::cell::{LazyCell, RefCell};
use std::collections::{BTreeMap, BTreeSet};
use std::ops::Range;
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
    CompilationInputs, CompileErrorReporter, Diagnostic, LoweredCompilationUnit, ScriptBundle,
    SourceMapExt, Symbols, TypeFlagRegistry, TypeInterner, TypeSchema, ast,
};
use redscript_dotfile::Dotfile;
use serde::{Deserialize, Serialize};

use crate::completions;
use crate::dialect::{self, Dialect, ParsedSources};
use crate::query::{AtContext, ExprAt};
use crate::server::{CodeLocation, Document, LanguageServer, LspContext};

const DIAGNOSTIC_PASSES: &[&'static (dyn DiagnosticPass + 'static)] = &[&UnusedLocals];

type FixMap = HashMap<PathBuf, BTreeMap<(u32, u32), Fix>>;

pub struct RedscriptLanguageServer {
    workspaces: HashMap<PathBuf, WorkspaceDir>,
    compilation_cache: CompilationCache,
    cached_fixes: RefCell<FixMap>,
    last_published_files: RefCell<HashSet<PathBuf>>,
    overlays: HashMap<PathBuf, Overlay>,
    standalone: HashMap<PathBuf, CompilationCache>,
    generation: u64,
    missing_roots: RefCell<BTreeSet<PathBuf>>,
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
        let workspaces = workspace_folders
            .into_iter()
            .map(|dir| {
                let dir = dir.into();
                let workspace = WorkspaceDir::load(&dir)?;
                Ok((dir, workspace))
            })
            .collect::<anyhow::Result<HashMap<_, _>>>()?;
        let workspace_dirs = workspaces.iter().flat_map(|(_, w)| &w.roots);
        let mut missing_roots = BTreeSet::new();
        Ok(Self {
            compilation_cache: CompilationCache::make(
                cache_bytes,
                workspace_dirs,
                TypeInterner::default(),
                |root| {
                    missing_roots.insert(root.to_owned());
                },
            )?,
            workspaces,
            cached_fixes: RefCell::new(HashMap::new()),
            last_published_files: RefCell::new(HashSet::new()),
            overlays: HashMap::new(),
            standalone: HashMap::new(),
            generation: 0,
            missing_roots: RefCell::new(missing_roots),
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
    ) -> anyhow::Result<CompilationCache> {
        let sources = ast::SourceMap::new();
        let mut files: BTreeMap<_, _> = dialect::discover(roots, |root| {
            self.missing_roots.borrow_mut().insert(root.to_owned());
        })?
        .into_iter()
        .map(|path| {
            let text = self
                .overlays
                .get(&path)
                .map(|overlay| Ok(overlay.text.clone()))
                .unwrap_or_else(|| fs::read_to_string(&path));
            text.map(|text| (path, text))
        })
        .collect::<Result<_, _>>()?;
        // Unsaved new files are absent from discovery but still own an active module.
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
        if reload {
            self.completion_cache.borrow_mut().take();
        }
        let workspace = if reload {
            ctx.logger()
                .info("Rebuild phase=workspace discovery, parsing and analysis");
            self.make_snapshot(
                self.workspaces
                    .values()
                    .flat_map(|workspace| workspace.roots.clone()),
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
            .chain(self.overlays.keys())
            .filter(|path| self.resolve_file(path).as_workspace().is_none())
            .cloned()
            .collect();
        let mut standalone = HashMap::new();
        for path in paths {
            let text = self
                .overlays
                .get(&path)
                .map(|overlay| Ok(overlay.text.clone()))
                .unwrap_or_else(|| fs::read_to_string(&path));
            match text {
                Ok(text) => {
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
                Err(error) => return Err(error.into()),
            }
        }
        self.compilation_cache = workspace;
        self.standalone = standalone;
        self.log_missing_roots(ctx);
        Ok(())
    }

    pub fn publish_all_diagnostics(&self, ctx: &LspContext) -> anyhow::Result<()> {
        self.check_workspace_and_publish(ctx)?;
        for cache in self.standalone.values() {
            cache.analyze(|_, symbols, diagnostics, sources| {
                self.publish_diagnostics(diagnostics, symbols, sources, ctx)
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

    fn hover_at(&self, loc: CodeLocation<'_>, ctx: &LspContext) -> anyhow::Result<lsp::Hover> {
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
                Ok(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value,
                    }),
                    range,
                })
            },
            ctx,
        )
    }

    fn completion_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Rc<lsp::CompletionResponse>> {
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
            let types = syms
                .types()
                .filter_map(|(id, def)| Some((id, def.span()?)))
                .filter(|(id, _)| id.as_str().contains(query))
                .filter_map(|(id, span)| {
                    #[allow(deprecated)]
                    Some(lsp::SymbolInformation {
                        name: id.to_string(),
                        kind: lsp::SymbolKind::CLASS,
                        tags: None,
                        deprecated: None,
                        location: location(span, sources, ctx)?,
                        container_name: None,
                    })
                });

            let results = types.chain(funcs).collect();
            Ok(lsp::WorkspaceSymbolResponse::Flat(results))
        })
    }

    fn format_document(
        &self,
        doc: Document<'_>,
        _tab_size: u16,
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
            Dialect::Redmod => redscript_redmod_formatter::format_source(file.source(), id).ok(),
            Dialect::Community => {
                let (module, errors) = format_document(file.source(), id, settings);
                module
                    .filter(|_| errors.is_empty())
                    .map(|module| module.to_string())
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
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeActionResponse> {
        if Dialect::of(&path) != Some(Dialect::Community) {
            return Ok(vec![]);
        }
        let path = dialect::identity(&path);
        let fixes = self.cached_fixes.borrow();

        if let Some(fixes) = fixes.get(&path)
            && let Some((&(start, end), fix)) = fixes.range(..(span.start, span.end)).last()
            && (start..end).contains(&span.start)
            && self.valid_fix(fix)
        {
            let fix_one = lsp::CodeAction {
                title: fix.kind.to_string(),
                kind: Some(lsp::CodeActionKind::REFACTOR_REWRITE),
                data: Some(serde_json::to_value(FixRequest::FixOne(fix.clone()))?),
                is_preferred: Some(true),
                ..Default::default()
            };

            let fix_all = lsp::CodeAction {
                title: format!("{} - fix all in the workspace", fix.kind),
                kind: Some(lsp::CodeActionKind::REFACTOR_REWRITE),
                data: Some(serde_json::to_value(FixRequest::FixAll(
                    fix.kind,
                    self.generation,
                ))?),
                ..Default::default()
            };

            Ok(vec![
                lsp::CodeActionOrCommand::CodeAction(fix_one),
                lsp::CodeActionOrCommand::CodeAction(fix_all),
            ])
        } else {
            Ok(vec![])
        }
    }

    fn code_action_resolve(
        &self,
        action: lsp::CodeAction,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeAction> {
        let Some(data) = action.data else {
            return Ok(action);
        };

        let request: FixRequest = serde_json::from_value(data)?;
        let action = match request {
            FixRequest::FixOne(fix) if self.valid_fix(&fix) => lsp::CodeAction {
                title: fix.kind.to_string(),
                edit: Some(self.workspace_edit(vec![(fix.uri, vec![fix.edit])])),
                ..Default::default()
            },
            FixRequest::FixAll(kind, generation) if generation == self.generation => {
                let mut unique = HashSet::<(lsp::Uri, lsp::Range)>::new();

                #[allow(clippy::mutable_key_type)]
                let mut edits = std::collections::HashMap::<lsp::Uri, Vec<lsp::TextEdit>>::new();
                for (_, fix) in self
                    .cached_fixes
                    .borrow()
                    .values()
                    .flat_map(|f| f.iter())
                    .filter(|(_, f)| f.kind == kind && self.valid_fix(f))
                {
                    if unique.insert((fix.uri.clone(), fix.edit.range)) {
                        edits
                            .entry(fix.uri.clone())
                            .or_default()
                            .push(fix.edit.clone());
                    }
                }

                lsp::CodeAction {
                    title: kind.to_string(),
                    edit: Some(self.workspace_edit(edits.into_iter().collect())),
                    ..Default::default()
                }
            }
            _ => lsp::CodeAction {
                title: action.title,
                disabled: Some(lsp::CodeActionDisabled {
                    reason: "This fix is stale or targets an unsupported dialect".into(),
                }),
                ..Default::default()
            },
        };
        Ok(action)
    }

    fn workspace_edit(&self, edits: Vec<(lsp::Uri, Vec<lsp::TextEdit>)>) -> lsp::WorkspaceEdit {
        let mut edits = edits;
        edits.sort_by(|(a, _), (b, _)| a.as_str().cmp(b.as_str()));
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
        for root in std::mem::take(&mut *self.missing_roots.borrow_mut()) {
            ctx.notify::<lsp::notification::LogMessage>(lsp::LogMessageParams {
                typ: lsp::MessageType::WARNING,
                message: format!(
                    "Source root '{}' is missing; treating it as temporarily empty. Restore/create the directory or update source_roots in .redscript, then refresh the workspace.",
                    root.display()
                ),
            });
        }
    }

    fn check_workspace_and_publish(&self, ctx: &LspContext) -> anyhow::Result<()> {
        self.check_workspace(|_, symbols, diags, sources| {
            self.publish_diagnostics(diags, symbols, sources, ctx)
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

    fn publish_diagnostics<'ctx>(
        &self,
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
                    && let Some(mut fix) = generate_fix(diag, symbols, sources, ctx)
                {
                    fix.generation = self.generation;
                    let span = diag.span();
                    slots.insert((span.start, span.end), fix);
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
                            Some(lsp::Diagnostic {
                                range: range(diag.span(), file)?,
                                severity: Some(if diag.is_error() {
                                    lsp::DiagnosticSeverity::ERROR
                                } else {
                                    lsp::DiagnosticSeverity::WARNING
                                }),
                                code: Some(lsp::NumberOrString::String(diag.code().into())),
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
                                tags: (matches!(diag, Diagnostic::UnusedLocal(_))
                                    && ctx
                                        .supports_diagnostic_tag(lsp::DiagnosticTag::UNNECESSARY))
                                .then(|| vec![lsp::DiagnosticTag::UNNECESSARY]),
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
        for path in last_diagnostics.difference(&current_files).filter(|path| {
            analyzed_paths.contains(*path)
                || (is_workspace && self.resolve_file(path).as_workspace().is_some())
                || (!self.overlays.contains_key(*path) && !path.exists())
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
            let expr = func.and_then(|f| f.block.find_at(preceding_pos));

            let typ = typ
                .and_then(|t| unit.scopes.get(&id)?.get(t)?.id())
                .or_else(|| cache.interner.get_index(cache.interner.get_index_of(typ?)?));
            let at = ExprAt::new(expr, func, typ, syms, cache.sources, query_context);
            ctx.logger().info(format!(
                "Query result lookup_byte_offset={preceding_pos} parsed_module={} ast_match={} function={} expression={} named_type={} receiver_type={} context={query_context:?} parse_diagnostics={} semantic_diagnostics={}",
                analysis.parsed.modules.iter().any(|(file, _)| *file == id),
                match_.is_some(),
                func.is_some(),
                expr.is_some(),
                typ.is_some(),
                at.expr_type().is_some(),
                analysis.parsed.diagnostics.len(),
                analysis.diagnostics.len(),
            ));
            cb(at)
        })
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
    symbols: &Symbols<'ctx>,
    sources: &'ctx ast::SourceMap,
    ctx: &LspContext,
) -> Option<Fix> {
    let code_edit = CodeEdit::from_diagnostic(diag)?;
    let edit = TextEdit::from_code_edit(&code_edit, symbols, sources).ok()?;
    let file = sources.get(edit.file)?;
    if Dialect::of(file.path()) != Some(Dialect::Community) {
        return None;
    }
    let range = range(Span::new(edit.start as _, edit.end as _, edit.file), file)?;
    Some(Fix {
        kind: FixKind::from(&code_edit),
        uri: ctx.uri(file.path()).ok()?,
        edit: lsp::TextEdit::new(range, edit.content.to_string()),
        expected_source: file.source().to_owned(),
        generation: 0,
    })
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
        }
        // Closing must restore disk contents (or remove an unsaved file).
        // Open/change use known sources; filesystem notifications refresh discovery.
        self.rebuild_with_reload(ctx, buffer.is_none())
    }

    fn files_changed(&mut self, _paths: Vec<PathBuf>, ctx: &LspContext) -> anyhow::Result<()> {
        let workspaces = self
            .workspaces
            .keys()
            .map(|path| Ok((path.clone(), WorkspaceDir::load(path)?)))
            .collect::<anyhow::Result<HashMap<_, _>>>()?;
        self.workspaces = workspaces;
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
            let sources = ast::SourceMap::new();
            let text = self
                .overlays
                .get(&path)
                .map(|overlay| Ok(overlay.text.clone()))
                .unwrap_or_else(|| fs::read_to_string(&path))?;
            sources.push_back(&path, text);
            self.standalone.insert(
                path,
                CompilationCache::from_sources(
                    self.compilation_cache.borrow_cache_bytes().clone(),
                    sources,
                    TypeInterner::default(),
                )?,
            );
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
        for dir in added {
            let value = WorkspaceDir::load(&dir)?;
            self.workspaces.insert(dir, value);
        }
        for folder in removed {
            self.workspaces.remove(&folder);
        }
        self.rebuild(ctx)
    }

    fn hover(&self, loc: CodeLocation<'_>, _ctx: &LspContext) -> anyhow::Result<lsp_types::Hover> {
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
        _ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp_types::TextEdit>> {
        self.format_document(doc, tab_size)
    }

    fn code_action(
        &self,
        path: PathBuf,
        span: Range<u32>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::CodeActionResponse> {
        self.code_action(path, span, ctx)
    }

    fn code_action_resolve(
        &self,
        action: lsp::CodeAction,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::CodeAction> {
        self.code_action_resolve(action, ctx)
    }
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
    cache_bytes: Vec<u8>,
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

    pub fn make(
        cache_bytes: Vec<u8>,
        workspace_dirs: impl IntoIterator<Item = impl Into<PathBuf>>,
        interner: TypeInterner,
        missing_root: impl FnMut(&Path),
    ) -> anyhow::Result<Self> {
        let sources = ast::SourceMap::from_files(dialect::discover(workspace_dirs, missing_root)?)?;
        Self::from_sources(cache_bytes, sources, interner)
    }

    fn from_sources(
        cache_bytes: Vec<u8>,
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
            vec![],
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

#[derive(Debug)]
struct WorkspaceDir {
    roots: Vec<PathBuf>,
    format_settings: FormatSettings,
}

impl WorkspaceDir {
    fn load(dir: &Path) -> anyhow::Result<Self> {
        let dotfile = Dotfile::load_or_default(dir)?;
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

        Ok(Self {
            roots,
            format_settings: settings,
        })
    }
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
    FixOne(Fix),
    FixAll(FixKind, u64),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct Fix {
    kind: FixKind,
    uri: lsp::Uri,
    edit: lsp::TextEdit,
    expected_source: String,
    generation: u64,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
enum FixKind {
    FixStructNewConstructor,
    MakeMethodPublic,
    MakeFieldPublic,
    MakeTypePublic,
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
        server.hover_at(loc.clone(), &ctx).unwrap();
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
                let hover = server.hover_at(loc.clone(), &ctx).unwrap();
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
                && params
                    .diagnostics
                    .iter()
                    .any(|diag| diag.code == Some(lsp::NumberOrString::String("OTHER".into())))
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
        let text = "class Host { var value: Int32; function Value(): Int32 { return 1; } function Read(): Int32 { return value; } function Run(): Int32 { return Value(); } }";
        let buffers = open(&mut server, &ctx, &path, text, 1);
        let buffer = buffers.get(&ctx.uri(&path).unwrap()).unwrap();
        for (name, heading) in [("value", "### Field"), ("Value", "### Signature")] {
            let declaration = text.find(name).unwrap() as u32;
            let usage = text.rfind(name).unwrap() as u32;
            for offset in 1..=name.len() as u32 {
                server
                    .expr_at(
                        CodeLocation::new(Document::new(path.clone(), buffer), usage + offset),
                        |at| {
                            let span = at.definition_span().unwrap_or_else(|| {
                                panic!("missing definition for {name} at {offset}: {:?}", at.expr())
                            });
                            assert!(span.contains(declaration));
                            let hover = at.display().to_string();
                            assert!(hover.contains(heading), "{hover}");
                            assert!(hover.contains("Int32"), "{hover}");
                            Ok(())
                        },
                        &ctx,
                    )
                    .unwrap();
            }
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
            let hover = server.hover_at(loc.clone(), &ctx).unwrap();
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
                    assert!(
                        errors
                            .iter()
                            .all(|diag| matches!(diag, Diagnostic::Other(_, _, _)))
                    );
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
        assert!(messages.iter().any(|message| {
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
        }));
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
        let (temp, server, ctx, _receiver) = fixture();
        let path = temp.path().join("format.script");
        let source = "class Test {\r\n// comment { 😀\r\noverride function Run() {\r\nvar text: String = \"first\n  second\"; // }\r\n/* block\n unmodified */\r\n}\r\n} // é😀";
        let expected = "class Test {\r\n    // comment { 😀\r\n    override function Run() {\r\n        var text: String = \"first\n  second\"; // }\r\n        /* block\n unmodified */\r\n    }\r\n} // é😀";
        let buffer = crate::buffers::Buffer::new(source.into());
        let edits = server
            .format_document(Document::new(path.clone(), &buffer), 2)
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
                .format_document(Document::new(path.clone(), &formatted), 8)
                .unwrap()
                .is_empty()
        );
        let invalid = crate::buffers::Buffer::new("function Broken( {".into());
        assert!(
            server
                .format_document(Document::new(path.clone(), &invalid), 4)
                .unwrap()
                .is_empty()
        );
        assert!(
            server
                .format_document(Document::new(temp.path().join("unknown.txt"), &buffer), 4)
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
                    2
                )
                .unwrap()
                .is_empty()
        );
        let mut fix = Fix {
            kind: FixKind::MakeFieldPublic,
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
            data: Some(serde_json::to_value(FixRequest::FixOne(fix)).unwrap()),
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
            .insert((0, 1), redmod_fix);
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
        assert!(
            matches!(resolved.edit.unwrap().document_changes, Some(lsp::DocumentChanges::Edits(edits)) if edits.is_empty())
        );
    }

    fn server() -> RedscriptLanguageServer {
        RedscriptLanguageServer {
            workspaces: HashMap::new(),
            compilation_cache: CompilationCache::default(),
            cached_fixes: RefCell::new(HashMap::new()),
            last_published_files: RefCell::new(HashSet::new()),
            overlays: HashMap::new(),
            standalone: HashMap::new(),
            generation: 0,
            missing_roots: RefCell::new(BTreeSet::new()),
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
            Diagnostic::UnusedLocal(Span::new(4, 10, id)),
            Diagnostic::MissingFunctionBody(Span::new(0, 3, id)),
        ];
        let symbols = Symbols::with_default_types();
        let (sender, receiver) = crossbeam_channel::unbounded();
        let mut ctx = LspContext::new(sender);
        for supports_tags in [false, true] {
            if supports_tags {
                let capabilities = serde_json::from_value(serde_json::json!({
                    "textDocument": { "publishDiagnostics": { "tagSupport": { "valueSet": [1] } } }
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
        let diags = [Diagnostic::UnusedLocal(Span::new(4, 10, id))];
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
            Diagnostic::UnusedLocal(Span::new(0, 1, z)),
            Diagnostic::UnusedLocal(Span::new(2, 3, a)),
            Diagnostic::UnknownIntrinsic("z", Span::new(0, 2, a)),
            Diagnostic::UnknownIntrinsic("a", Span::new(0, 2, a)),
            Diagnostic::MissingFunctionBody(Span::new(0, 2, a)),
            Diagnostic::UnusedLocal(Span::new(0, 1, a)),
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

    #[test]
    fn resolving_fix_all_preserves_cached_fixes() {
        let server = server();
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let ctx = LspContext::new(sender);
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
                    Fix {
                        kind,
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
                    },
                )
            })
            .collect();
        server
            .cached_fixes
            .borrow_mut()
            .insert(PathBuf::from("test.reds"), fixes);

        for kind in [kinds[0], kinds[0], kinds[1]] {
            let action = lsp::CodeAction {
                data: Some(
                    serde_json::to_value(FixRequest::FixAll(kind, server.generation)).unwrap(),
                ),
                ..Default::default()
            };
            let resolved = server.code_action_resolve(action, &ctx).unwrap();
            let lsp::DocumentChanges::Edits(edits) =
                resolved.edit.unwrap().document_changes.unwrap()
            else {
                panic!("expected document edits");
            };
            assert_eq!(edits.len(), 1);
            assert_eq!(edits[0].text_document.uri, uri);
            assert_eq!(edits[0].text_document.version, Some(1));
            assert_eq!(edits[0].edits.len(), 1);
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
