use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::{fs, iter, mem};

use hashbrown::{HashMap, HashSet};
use lsp_types as lsp;
use ouroboros::self_referencing;
use redscript_compiler_api::pass::{DiagnosticPass, UnusedLocals};
use redscript_compiler_api::{
    CompilationInputs, CompileErrorReporter, Diagnostic, Evaluator, LoweredCompilationUnit,
    ScriptBundle, SourceMapExt, Symbols, TypeFlagRegistry, TypeInterner, TypeSchema, ast,
    infer_from_sources, parse_file, parse_files, process_sources,
};
use redscript_dotfile::Dotfile;
use redscript_formatter::{FormatSettings, format_document};

use crate::completions;
use crate::query::{AtContext, ExprAt};
use crate::server::{CodeLocation, Document, LanguageServer, LspContext};

const DIAGNOSTIC_PASSES: &[&'static (dyn DiagnosticPass + 'static)] = &[&UnusedLocals];

pub struct RedscriptLanguageServer {
    workspaces: HashMap<PathBuf, WorkspaceDir>,
    cache: CompilationCache,
    cached_completions: RefCell<Option<CachedCompletions>>,
    last_diagnostics: RefCell<HashSet<PathBuf>>,
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
        Ok(Self {
            cache: CompilationCache::make(cache_bytes, workspace_dirs, TypeInterner::default())?,
            workspaces,
            cached_completions: RefCell::new(None),
            last_diagnostics: RefCell::new(HashSet::new()),
        })
    }

    fn resolve_file(&self, path: &Path) -> FileResolution<'_> {
        if let Some(workspace) =
            iter::successors(Some(path), |p| p.parent()).find_map(|p| self.workspaces.get(p))
        {
            FileResolution::Workspace(workspace)
        } else {
            FileResolution::NonWorkspace(path.to_owned())
        }
    }

    fn hover_at(&self, loc: CodeLocation<'_>, ctx: &LspContext) -> anyhow::Result<lsp::Hover> {
        self.expr_at(
            loc,
            |at| {
                let range = at.expr().and_then(|e| {
                    let span = e.span();
                    range(span, at.sources().get(span.file)?)
                });
                Ok(lsp::Hover {
                    contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                        kind: lsp::MarkupKind::Markdown,
                        value: at.display().to_string(),
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
        let preceding_pos = loc.pos() - 1;

        let byte = loc.doc().buffer().contents().byte(preceding_pos as usize);

        if let Some(cached) = &mut *self.cached_completions.borrow_mut()
            && cached.file == loc.doc().path()
            && loc.pos().saturating_sub(cached.pos) <= 1
            && (byte == b'_' || byte.is_ascii_alphanumeric())
        {
            cached.pos = loc.pos();
            return Ok(cached.completions.clone());
        };

        if byte != b'.' {
            return Ok(Rc::new(lsp::CompletionResponse::Array(vec![])));
        }

        let completions = self.patched_expr_at(
            loc.clone().with_pos(preceding_pos),
            generate_completions,
            ctx,
        )?;

        if !completions.is_empty() {
            let resp = Rc::new(lsp::CompletionResponse::Array(completions));
            *self.cached_completions.borrow_mut() = Some(CachedCompletions::new(
                resp.clone(),
                loc.doc().path().to_owned(),
                loc.pos(),
            ));
            Ok(resp)
        } else {
            Ok(Rc::new(lsp::CompletionResponse::Array(vec![])))
        }
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
        let resolved = self.resolve_file(doc.path());
        let settings = resolved
            .as_workspace()
            .map(|ws| &ws.format_settings)
            .unwrap_or(&FormatSettings::DEFAULT);

        let contents = doc.buffer().contents();
        let map = ast::SourceMap::new();
        let id = map.push_back(doc.path(), contents.to_string());
        let file = map.get(id).unwrap();

        let (module, errors) = format_document(file.source(), id, settings);
        if let Some(module) = module
            && errors.is_empty()
        {
            let last_line = contents.len_lines() - 1;
            let edit = lsp::TextEdit::new(
                lsp::Range::new(
                    lsp::Position::new(0, 0),
                    lsp::Position::new(
                        last_line as u32,
                        contents.chars_at(contents.line_to_char(last_line)).count() as u32,
                    ),
                ),
                module.to_string(),
            );
            return Ok(vec![edit]);
        };

        Ok(vec![])
    }

    pub fn check_workspace_and_publish(&self, ctx: &LspContext) -> anyhow::Result<()> {
        self.check_workspace(|_, _, diags, sources| self.publish_diagnostics(diags, sources, ctx))
    }

    fn check_workspace<A>(
        &self,
        cb: impl Fn(
            &LoweredCompilationUnit<'_>,
            &Symbols<'_>,
            &[Diagnostic<'_>],
            &ast::SourceMap,
        ) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        self.cache.with(|cache| {
            let mut reporter = CompileErrorReporter::default();
            let (unit, syms) = infer_from_sources(
                cache.sources,
                cache.symbols.clone(),
                &mut reporter,
                cache.interner,
            );
            unit.run_diagnostics(DIAGNOSTIC_PASSES, &mut reporter);

            cb(&unit, &syms, &reporter.into_reported(), cache.sources)
        })
    }

    fn publish_diagnostics(
        &self,
        diags: &[Diagnostic<'_>],
        sources: &ast::SourceMap,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        let mut file_diags = HashMap::new();
        for diag in diags {
            let file = diag.span().file;
            if sources
                .get(file)
                .is_some_and(|f| self.resolve_file(f.path()).as_workspace().is_some())
            {
                file_diags.entry(file).or_insert_with(Vec::new).push(diag);
            }
        }

        let mut last_diagnostics = self.last_diagnostics.borrow_mut();

        for path in last_diagnostics.drain() {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(&path)?,
                diagnostics: vec![],
                version: None,
            });
        }

        for (file, diags) in file_diags {
            let file = sources.get(file).unwrap();

            if !last_diagnostics.contains(file.path()) {
                last_diagnostics.insert(file.path().to_owned());
            }

            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(file.path())?,
                diagnostics: diags
                    .into_iter()
                    .filter_map(|diag| {
                        Some(lsp::Diagnostic {
                            range: range(diag.span(), file)?,
                            severity: Some(if diag.is_error() {
                                lsp::DiagnosticSeverity::ERROR
                            } else {
                                lsp::DiagnosticSeverity::WARNING
                            }),
                            message: diag.to_string(),
                            ..Default::default()
                        })
                    })
                    .collect(),
                version: None,
            });
        }
        Ok(())
    }

    fn expr_at<A>(
        &self,
        loc: CodeLocation<'_>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        self.expr_at_with(loc, false, cb, ctx)
    }

    fn patched_expr_at<A>(
        &self,
        loc: CodeLocation<'_>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        self.expr_at_with(loc, true, cb, ctx)
    }

    fn expr_at_with<A>(
        &self,
        loc: CodeLocation<'_>,
        patch: bool,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        _ctx: &LspContext,
    ) -> anyhow::Result<A> {
        self.cache.with(|cache| {
            let mut contents = loc.doc().buffer().contents().to_string();
            if patch {
                let pos = loc.pos() as usize;
                if let Some(c) = contents[pos..].chars().next() {
                    contents.replace_range(pos..pos + c.len_utf8(), &" ".repeat(c.len_utf8()));
                }
            };

            let preceding_pos = loc.pos() - 1;

            let id = cache.sources.push_back(loc.doc().path(), contents);
            let file = cache.sources.get(id).unwrap();

            let previous_id = cache.file_ids.get(loc.doc().path()).copied();

            let mut reporter = CompileErrorReporter::default();
            let module = parse_file(id, file, &mut reporter);

            let match_ = module.as_ref().and_then(|m| m.find_at(preceding_pos));
            let (ctx, typ) = match match_ {
                Some(ast::QueryResult::Type(&ast::Type::Named { name, .. })) => (None, Some(name)),
                Some(ast::QueryResult::Expr(&ast::Expr::Ident(name))) => {
                    (Some(AtContext::Expr), Some(name))
                }
                _ => (None, None),
            };

            let evaluator = Evaluator::from_modules(cache.modules.iter().chain(module.as_ref()));
            let mods = cache
                .modules
                .iter()
                .filter(|m| m.span().map(|s| s.file) != previous_id)
                .cloned()
                .chain(module);
            let (unit, syms) = process_sources(
                mods,
                cache.symbols.clone(),
                evaluator,
                &mut reporter,
                cache.interner,
            );
            let func = unit
                .all_functions()
                .find(|f| f.span.file == id && f.span.contains(preceding_pos));
            let expr = func.and_then(|f| f.block.find_at(preceding_pos));

            let typ = typ
                .and_then(|t| unit.scopes.get(&id)?.get(t)?.id())
                .or_else(|| cache.interner.get_index(cache.interner.get_index_of(typ?)?));
            cb(ExprAt::new(expr, func, typ, &syms, cache.sources, ctx))
        })
    }
}

fn generate_completions(
    at: ExprAt<'_, '_>,
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
    fn check(&mut self, path: PathBuf, ctx: &LspContext) -> anyhow::Result<()> {
        match self.resolve_file(&path) {
            FileResolution::Workspace(_) => {
                let workspace_dirs = self.workspaces.values().flat_map(|w| &w.roots);
                self.cache.remake(workspace_dirs)?;
                self.check_workspace_and_publish(ctx)
            }
            FileResolution::NonWorkspace(path) => {
                self.cache.remake([path])?;
                self.check_workspace_and_publish(ctx)
            }
        }
    }

    fn change_workspace_folders(
        &mut self,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
        _ctx: &LspContext,
    ) -> anyhow::Result<()> {
        for dir in added {
            let value = WorkspaceDir::load(&dir)?;
            self.workspaces.insert(dir, value);
        }
        for folder in removed {
            self.workspaces.remove(&folder);
        }
        Ok(())
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
}

fn location(span: ast::Span, sources: &ast::SourceMap, ctx: &LspContext) -> Option<lsp::Location> {
    let file = sources.get(span.file)?;
    Some(lsp::Location {
        uri: ctx.uri(file.path()).ok()?,
        range: range(span, file)?,
    })
}

fn range(span: ast::Span, file: &ast::File) -> Option<lsp::Range> {
    let start = file.lookup(span.start);
    let end = file.lookup(span.end);
    Some(lsp::Range {
        start: lsp::Position::new(start.line as u32, start.col as u32),
        end: lsp::Position::new(end.line as u32, end.col as u32),
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
    symbols: Symbols<'this>,

    #[borrows(sources)]
    #[not_covariant]
    modules: Vec<ast::SourceModule<'this>>,
}

impl CompilationCache {
    pub fn make(
        cache_bytes: Vec<u8>,
        workspace_dirs: impl IntoIterator<Item = impl Into<PathBuf>>,
        interner: TypeInterner,
    ) -> anyhow::Result<Self> {
        let sources = ast::SourceMap::from_paths_recursively(workspace_dirs)?;

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
            |bytes, interner, _| {
                let bundle = ScriptBundle::from_bytes(bytes)?;
                Ok(CompilationInputs::load_without_mapping(
                    &bundle,
                    interner,
                    &TypeFlagRegistry::default(),
                )?)
            },
            |sources| {
                let mut reporter = CompileErrorReporter::default();
                Ok(parse_files(sources, &mut reporter))
            },
        )
    }

    pub fn remake(
        &mut self,
        dirs: impl IntoIterator<Item = impl Into<PathBuf>>,
    ) -> anyhow::Result<()> {
        let cache = mem::take(self).into_heads();
        *self = CompilationCache::make(cache.cache_bytes, dirs, cache.interner)?;
        Ok(())
    }
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new(
            vec![],
            TypeInterner::default(),
            ast::SourceMap::default(),
            HashMap::default(),
            |_, _, _| Symbols::with_default_types(),
            |_| vec![],
        )
    }
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
            .map(|p| dir.join(p))
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
    NonWorkspace(PathBuf),
}

impl FileResolution<'_> {
    pub fn as_workspace(&self) -> Option<&WorkspaceDir> {
        match self {
            Self::Workspace(ws) => Some(ws),
            Self::NonWorkspace(_) => None,
        }
    }
}

#[derive(Debug)]
struct CachedCompletions {
    completions: Rc<lsp::CompletionResponse>,
    file: PathBuf,
    pos: u32,
}

impl CachedCompletions {
    fn new(completions: Rc<lsp::CompletionResponse>, file: PathBuf, pos: u32) -> Self {
        Self {
            completions,
            file,
            pos,
        }
    }
}
