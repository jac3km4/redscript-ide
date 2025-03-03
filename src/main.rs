use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::{fs, iter};

use anyhow::bail;
use display::{DocDisplay, FunctionTypeDisplay, SnippetDisplay};
use lsp_types as lsp;
use mimalloc::MiMalloc;
use query::{ExprAt, find_expr};
use redscript_compiler_api::types::Type;
use redscript_compiler_api::{
    CompilationInputs, CompileErrorReporter, Diagnostic, LoweredCompilationUnit, ScriptBundle,
    SourceMapExt, Symbols, TypeInterner, ast, infer_from_sources, parse_sources, process_sources,
};
use redscript_formatter::{FormatSettings, format_document};
use serde::Deserialize;
use server::{CodeLocation, Document, LanguageServer, LspContext, LspServer};

mod buffers;
mod display;
mod query;
mod server;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Debug, Deserialize)]
struct InitializationOptions {
    game_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    LspServer::spawn(
        |opts, _ctx| {
            let game_dir = if let Some(opts) = opts
                .init_options
                .map(serde_json::from_value::<InitializationOptions>)
                .transpose()?
            {
                opts.game_dir
            } else {
                bail!("game directory initialization options were not provided");
            };
            let bytes = fs::read(find_cache_file(&game_dir)?)?;
            Ok((opts.workspace_dirs, bytes))
        },
        |(dirs, bytes), ctx| {
            let bundle = ScriptBundle::from_bytes(bytes)?;
            let ls = RedscriptLanguageServer::new(bundle, dirs.iter().cloned().collect());
            ls.check_workspace(ctx)?;
            Ok(Box::new(ls))
        },
    )
}

#[derive(Debug)]
struct RedscriptLanguageServer<'a> {
    bundle: ScriptBundle<'a>,
    workspace_folders: BTreeSet<PathBuf>,
}

impl<'a> RedscriptLanguageServer<'a> {
    fn new(bundle: ScriptBundle<'a>, workspace_folders: BTreeSet<PathBuf>) -> Self {
        Self {
            bundle,
            workspace_folders,
        }
    }

    fn resolve_path(&self, path: &Path) -> anyhow::Result<FileResolution> {
        if iter::successors(Some(path), |p| p.parent()).any(|p| self.workspace_folders.contains(p))
        {
            Ok(FileResolution::Workspace)
        } else {
            Ok(FileResolution::NonWorkspace(path.to_owned()))
        }
    }

    fn hover_at(&self, loc: CodeLocation<'_>) -> anyhow::Result<lsp::Hover> {
        self.expr_at(loc, |at| {
            Ok(lsp::Hover {
                contents: lsp::HoverContents::Markup(lsp::MarkupContent {
                    kind: lsp::MarkupKind::Markdown,
                    value: at.display().to_string(),
                }),
                range: None,
            })
        })
    }

    fn completion_at(&self, loc: CodeLocation<'_>) -> anyhow::Result<lsp::CompletionResponse> {
        let preceding_pos = loc.pos() - 1;
        self.patched_expr_at(loc.with_pos(preceding_pos), |at| {
            let typ = at.expr_type();
            let res = if let Some(typ) = typ.as_ref().and_then(Type::upper_bound) {
                let fields = at
                    .symbols()
                    .query_methods(typ.id())
                    .filter(|m| !m.func().flags().is_static())
                    .map(|e| lsp::CompletionItem {
                        label: (*e.name()).to_string(),
                        label_details: Some(lsp::CompletionItemLabelDetails {
                            detail: Some(FunctionTypeDisplay::new(e.func().type_()).to_string()),
                            description: None,
                        }),
                        kind: Some(lsp::CompletionItemKind::METHOD),
                        documentation: Some(lsp::Documentation::MarkupContent(
                            lsp::MarkupContent {
                                kind: lsp::MarkupKind::Markdown,
                                value: DocDisplay::new(e.func().doc()).to_string(),
                            },
                        )),
                        insert_text: Some(
                            SnippetDisplay::new(e.name(), e.func().type_()).to_string(),
                        ),
                        insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                        ..Default::default()
                    });

                let methods = at
                    .symbols()
                    .base_iter(typ.id())
                    .filter_map(|(_, def)| def.schema().as_aggregate())
                    .flat_map(|agg| agg.fields().iter())
                    .map(|e| lsp::CompletionItem {
                        label: e.name().to_string(),
                        label_details: Some(lsp::CompletionItemLabelDetails {
                            detail: Some(format!(": {}", e.field().type_())),
                            description: None,
                        }),
                        kind: Some(lsp::CompletionItemKind::FIELD),
                        documentation: Some(lsp::Documentation::MarkupContent(
                            lsp::MarkupContent {
                                kind: lsp::MarkupKind::Markdown,
                                value: DocDisplay::new(e.field().doc()).to_string(),
                            },
                        )),
                        ..Default::default()
                    });

                fields.chain(methods).collect::<Vec<_>>()
            } else {
                vec![]
            };
            Ok(res.into())
        })
    }

    fn definition_at(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
        self.expr_at(loc, |at| {
            let locations = at
                .definition_span()
                .and_then(|span| Some(vec![location(span, at.sources(), ctx)?]))
                .unwrap_or_default();
            Ok(locations.into())
        })
    }

    fn workspace_symbol(
        &self,
        query: &str,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::WorkspaceSymbolResponse> {
        let sources = ast::SourceMap::from_paths_recursively(&self.workspace_folders)?;
        self.check_with(sources, |_, syms, _, sources| {
            let funcs = syms
                .free_functions()
                .filter_map(|e| Some((*e.name(), e.func().span()?)))
                .filter(|(name, _)| name.as_ref().last().is_some_and(|n| n.contains(query)))
                .filter_map(|(name, span)| {
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
        tab_size: u16,
    ) -> anyhow::Result<Vec<lsp::TextEdit>> {
        let contents = doc.buffer().contents();
        let mut map = ast::SourceMap::new();
        let id = map.push_back(doc.path(), contents.to_string());
        let file = map.get(id).unwrap();

        let settings = FormatSettings {
            indent: tab_size,
            ..Default::default()
        };
        let (module, errors) = format_document(file.source(), id, &settings);
        if let (Some(module), []) = (module, &errors[..]) {
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

    fn check_workspace(&self, ctx: &LspContext) -> anyhow::Result<()> {
        let sources = ast::SourceMap::from_paths_recursively(&self.workspace_folders)?;
        self.check(sources, ctx)
    }

    fn check(&self, sources: ast::SourceMap, ctx: &LspContext) -> anyhow::Result<()> {
        self.check_with(sources, |_, _, diags, sources| {
            self.publish_diagnostics(diags, sources, ctx)
        })
    }

    fn check_with<A>(
        &self,
        mut sources: ast::SourceMap,
        cb: impl Fn(
            &LoweredCompilationUnit<'_>,
            &Symbols<'_>,
            &[Diagnostic<'_>],
            &ast::SourceMap,
        ) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        sources.populate_boot_lib();
        let interner = TypeInterner::default();
        let syms = CompilationInputs::load_without_mapping(&self.bundle, &interner)?;
        let (unit, syms, diags) = infer_from_sources(&sources, &interner, syms);
        cb(&unit, &syms, &diags, &sources)
    }

    fn publish_diagnostics(
        &self,
        diags: &[Diagnostic<'_>],
        sources: &ast::SourceMap,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        let mut file_diags = HashMap::new();
        for diag in diags {
            file_diags
                .entry(diag.span().file)
                .or_insert_with(Vec::new)
                .push(diag);
        }

        for (_, file) in sources.files() {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(file.path())?,
                diagnostics: vec![],
                version: None,
            });
        }

        for (file, diags) in file_diags {
            let file = sources.get(file).unwrap();

            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(file.path())?,
                diagnostics: diags
                    .into_iter()
                    .filter_map(|diag| {
                        Some(lsp::Diagnostic {
                            range: range(diag.span(), file)?,
                            severity: Some(if diag.is_fatal() {
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
    ) -> anyhow::Result<A> {
        self.expr_at_with(loc, false, cb)
    }

    fn patched_expr_at<A>(
        &self,
        loc: CodeLocation<'_>,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        self.expr_at_with(loc, true, cb)
    }

    fn expr_at_with<A>(
        &self,
        loc: CodeLocation<'_>,
        patch: bool,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
    ) -> anyhow::Result<A> {
        let interner = TypeInterner::default();
        let mut sources = ast::SourceMap::from_paths_recursively(&self.workspace_folders)?;

        let mut contents = loc.doc().buffer().contents().to_string();
        if patch {
            let pos = loc.pos() as usize;
            if let Some(c) = contents[pos..].chars().next() {
                contents.replace_range(pos..pos + c.len_utf8(), &" ".repeat(c.len_utf8()));
            }
        };

        let file = sources.push_back(loc.doc().path(), contents);
        sources.populate_boot_lib();

        let mut reporter = CompileErrorReporter::default();
        let mods = parse_sources(&sources, &mut reporter);

        let symbols = CompilationInputs::load_without_mapping(&self.bundle, &interner)?;
        let typ = if let Some(ast::QueryResult::Type(&ast::Type::Named { name, .. })) =
            mods.last().and_then(|m| m.find_at(loc.pos()))
        {
            Some(name)
        } else {
            None
        };

        let (unit, syms, _) = process_sources(mods, &interner, symbols, reporter);

        let mut index = unit
            .all_functions()
            .filter(|f| f.span.file == file)
            .collect::<Vec<_>>();
        index.sort_unstable_by_key(|func| func.span);
        let (func, expr) = find_expr(&index, loc.pos()).unzip();

        let typ = typ
            .and_then(|t| unit.scopes.get(&file)?.get(t)?.id())
            .or_else(|| interner.get_index(interner.get_index_of(typ?)?));
        cb(ExprAt::new(expr, func, typ, &syms, &sources))
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

impl LanguageServer for RedscriptLanguageServer<'_> {
    fn check(&self, doc: Document<'_>, ctx: &LspContext) -> anyhow::Result<()> {
        match self.resolve_path(doc.path())? {
            FileResolution::Workspace => self.check_workspace(ctx),
            FileResolution::NonWorkspace(path) => {
                self.check(ast::SourceMap::from_files([path])?, ctx)
            }
        }
    }

    fn change_workspace_folders(
        &mut self,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
        _ctx: &LspContext,
    ) -> anyhow::Result<()> {
        self.workspace_folders.extend(added);
        for folder in removed {
            self.workspace_folders.remove(&folder);
        }
        Ok(())
    }

    fn hover(&self, loc: CodeLocation<'_>, _ctx: &LspContext) -> anyhow::Result<lsp_types::Hover> {
        self.hover_at(loc)
    }

    fn completion(
        &self,
        loc: CodeLocation<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp_types::CompletionResponse> {
        self.completion_at(loc)
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
        self.workspace_symbol(query, ctx)
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

#[derive(Debug)]
enum FileResolution {
    Workspace,
    NonWorkspace(PathBuf),
}

fn find_cache_file(game_dir: &Path) -> anyhow::Result<PathBuf> {
    let default = game_dir
        .join("r6")
        .join("cache")
        .join("modded")
        .join("final.redscripts.bk");
    if default.exists() {
        return Ok(default);
    }

    let fallback = game_dir
        .join("r6")
        .join("cache")
        .join("final.redscripts.bk");
    if fallback.exists() {
        return Ok(fallback);
    }

    let fallback = game_dir.join("r6").join("cache").join("final.redscripts");
    if fallback.exists() {
        return Ok(fallback);
    }

    bail!("cache file not found")
}
