use std::collections::{hash_map, HashMap};
use std::fmt::{self, Write};
use std::fs::{self, File};
use std::io;
use std::ops::Deref;
use std::path::{Path, PathBuf};

use buffers::Buffers;
use error::Error;
use lspower::{jsonrpc, lsp, Client, LanguageServer, LspService, Server};
use redscript::ast::{Expr, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex, ScriptBundle};
use redscript::definition::{Class, Enum};
use redscript_compiler::diagnostics::Diagnostic;
use redscript_compiler::parser;
use redscript_compiler::scope::{Reference, TypeId};
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::typechecker::{type_of, Callable, TypedAst};
use redscript_compiler::unit::CompilationUnit;
use serde::Deserialize;
use tokio::sync::{OnceCell, RwLock};

mod buffers;
mod error;
mod util;

const DOT_REDSCRIPT: &str = ".redscript-ide";

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, messages) = LspService::new(|client| Backend {
        client,
        state: RwLock::new(None),
        pool: OnceCell::new(),
        workspace_path: OnceCell::new(),
        buffers: Buffers::default(),
    });
    Server::new(stdin, stdout)
        .interleave(messages)
        .serve(service)
        .await;
}

#[derive(Debug, Deserialize, Default)]
struct DotRedscript {
    redscript_dir: Option<PathBuf>,
}

impl DotRedscript {
    pub fn load(root_dir: &Path) -> Result<Option<Self>, Error> {
        let path = root_dir.join(DOT_REDSCRIPT);
        let contents = match fs::read_to_string(path) {
            Ok(res) => res,
            Err(err) if err.kind() == io::ErrorKind::NotFound => return Ok(None),
            Err(err) => return Err(Error::Other(err.into())),
        };
        toml::from_str(&contents).map_err(Error::DotRedscriptParseError)
    }
}

#[derive(Debug, Deserialize)]
#[serde(from = "(Option<PathBuf>, Option<PathBuf>)")]
struct Config {
    script_cache_path: Option<PathBuf>,
    game_dir: Option<PathBuf>,
}

impl From<(Option<PathBuf>, Option<PathBuf>)> for Config {
    fn from((script_cache_path, game_dir): (Option<PathBuf>, Option<PathBuf>)) -> Self {
        Self {
            script_cache_path: script_cache_path.filter(|p| p.components().count() > 0),
            game_dir: game_dir.filter(|p| p.components().count() > 0),
        }
    }
}

struct Backend {
    client: Client,
    pool: OnceCell<ConstantPool>,
    workspace_path: OnceCell<PathBuf>,
    state: RwLock<Option<ServerState>>,
    buffers: Buffers,
}

struct ServerState {
    compiled_pool: ConstantPool,
}

impl Backend {
    const CONFIG_FIELDS: &'static [&'static str] =
        &["redscript.scriptCachePath", "redscript.gameDir"];

    async fn initialize(&self, uri: Option<lsp::Url>) -> Result<(), Error> {
        let mut path = uri
            .ok_or(Error::NoWorkspaceOpen)?
            .to_file_path()
            .map_err(|_| Error::NonFileUri)?;

        let dot_redscript = DotRedscript::load(&path)?.unwrap_or_default();
        if let Some(redscript_dir) = dot_redscript.redscript_dir {
            let new_path = if redscript_dir.is_absolute() {
                redscript_dir
            } else {
                path.join(redscript_dir)
            };
            path = new_path
                .try_exists()
                .map_err(|_| Error::InvalidRedscriptSourceDir(new_path.clone()))?
                .then_some(new_path.clone())
                .ok_or_else(|| Error::InvalidRedscriptSourceDir(new_path))?;
        }
        if let Err(err) = self.workspace_path.set(path) {
            self.log_error(format!("redscript LS is being initialized twice! ({err})"))
                .await;
        }
        Ok(())
    }

    async fn post_initialize(&self) -> Result<(), Error> {
        let conf = self.get_configuration().await?;
        let path = conf.script_cache_path.map(Ok).unwrap_or_else(|| {
            conf.game_dir
                .ok_or(Error::MissingConfiguration)
                .and_then(|dir| {
                    let default_bk = dir
                        .join("r6")
                        .join("cache")
                        .join("modded")
                        .join("final.redscripts.bk");
                    default_bk
                        .exists()
                        .then_some(default_bk)
                        .or_else(|| {
                            let fallback = dir.join("r6").join("cache").join("final.redscripts.bk");
                            fallback.exists().then_some(fallback)
                        })
                        .or_else(|| {
                            let fallback = dir.join("r6").join("cache").join("final.redscripts");
                            fallback.exists().then_some(fallback)
                        })
                        .ok_or_else(|| Error::RedscriptCacheNotFound(dir))
                })
        })?;

        let bundle = ScriptBundle::load(&mut File::open(path)?)?;
        self.pool.set(bundle.pool).unwrap();

        if let Some(path) = self.workspace_path.get() {
            if let Err(err) = self.typecheck_workspace(path).await {
                self.log_info(format!("initial typecheck reported an error: {err}"))
                    .await;
            }
        }
        Ok(())
    }

    async fn get_configuration(&self) -> Result<Config, Error> {
        let items = Self::CONFIG_FIELDS
            .iter()
            .map(|str| lsp::ConfigurationItem {
                section: Some((*str).to_owned()),
                scope_uri: None,
            })
            .collect();
        let conf_json = self
            .client
            .configuration(items)
            .await
            .map_err(|err| Error::Other(err.into()))?;

        let config = serde_json::from_value(serde_json::Value::Array(conf_json))
            .map_err(|err| Error::Other(err.into()))?;
        Ok(config)
    }

    async fn typecheck_workspace(&self, path: &Path) -> Result<(), Error> {
        if let Some(mut compiled_pool) = self.pool.get().cloned() {
            let files = Files::from_dir(path, &SourceFilter::None)?;

            match CompilationUnit::new_with_defaults(&mut compiled_pool)?
                .typecheck_files(&files, false, false)
            {
                Ok((_, diagnostics)) => {
                    let state = ServerState { compiled_pool };
                    *self.state.write().await = Some(state);

                    self.publish_diagnostics(diagnostics, &files).await;
                }
                Err(err) => {
                    let diagnostic = Diagnostic::from_error(err)?;
                    self.publish_diagnostics(vec![diagnostic], &files).await;
                }
            }
        } else {
            self.log_error("project not initialized".to_owned()).await;
        }
        Ok(())
    }

    async fn expr_at_location<A>(
        &self,
        pos: lsp::TextDocumentPositionParams,
        fix_parse_error: bool,
        on_expr: impl for<'a> Fn(&'a Expr<TypedAst>, TypeId, &ConstantPool) -> Result<A, Error>,
    ) -> Result<Option<A>, Error> {
        let buf = self
            .buffers
            .get(&pos.text_document.uri)
            .expect("no buffer found for URI");
        let mut pool = self.get_cloned_pool().await?;
        let mut copy = buf.contents().clone();
        let mut needle = buf
            .get_pos(pos.position.line, pos.position.character)
            .expect("position outside of the buffer");

        // very dumb heuristic to fix common parse errors
        // this removes the dot character and inserts a semicolon at the end of the line if it's missing
        if fix_parse_error {
            let idx: usize = (needle - 1).into();

            if copy.byte(idx) == b'.' {
                needle = needle - 1;

                for c in (0..idx).map(|i| copy.byte(i)).rev() {
                    needle = needle - 1;
                    if ![b' ', b'\n', b'\r', b'\t'].contains(&c) {
                        break;
                    }
                }

                let char_idx = copy.byte_to_char(idx);
                copy.remove(char_idx..char_idx + 1);

                let mut insert_colon = None;

                for (i, c) in copy.bytes_at(idx).enumerate() {
                    if c == b';' {
                        break;
                    }
                    if c == b'\n' {
                        insert_colon = Some(idx + i);
                        break;
                    }
                }

                if let Some(colon_idx) = insert_colon {
                    let char_idx = copy.byte_to_char(colon_idx);
                    copy.remove(char_idx..char_idx + 1);
                    copy.insert(char_idx, ";\n");
                };
            }
        }

        // TODO: avoid copying the string
        match parser::parse_str(&copy.to_string()) {
            Ok(module) => {
                let (functions, _) = CompilationUnit::new_with_defaults(&mut pool)?.typecheck(
                    vec![module],
                    &Files::default(),
                    false,
                    true,
                )?;
                if let Some((expr, scope)) = functions
                    .binary_search_by(|fun| fun.span.compare_pos(needle))
                    .ok()
                    .and_then(|idx| functions.get(idx))
                    .and_then(|fun| {
                        util::find_in_seq(&fun.code.exprs, needle).map(|expr| (expr, &fun.scope))
                    })
                {
                    let typ = type_of(expr, scope, &pool)?;
                    return Ok(Some(on_expr(expr, typ, &pool)?));
                }
            }
            Err(err) => {
                self.log_info(format!("encountered a parse error: {err}"))
                    .await;
            }
        }
        Ok(None)
    }

    async fn completion(
        &self,
        params: lsp::CompletionParams,
    ) -> Result<Option<lsp::CompletionResponse>, Error> {
        let matched = self
            .expr_at_location(params.text_document_position, true, |expr, typ, pool| {
                let is_static = matches!(expr, Expr::Ident(Reference::Symbol(_), _));
                match typ.unwrapped() {
                    TypeId::Class(idx) | TypeId::Struct(idx) => {
                        let completions = Self::class_completions(*idx, is_static, pool)?;
                        Ok(Some(lsp::CompletionResponse::Array(completions)))
                    }
                    TypeId::Enum(idx) if is_static => {
                        let completions = Self::enum_completions(*idx, pool)?;
                        Ok(Some(lsp::CompletionResponse::Array(completions)))
                    }
                    _ => Ok(None),
                }
            })
            .await?;

        Ok(matched.flatten())
    }

    // TODO: use this instead of calculating everything in completion
    async fn completion_resolve(
        &self,
        item: lsp::CompletionItem,
    ) -> Result<lsp::CompletionItem, Error> {
        if let Some(idx_val) = item.data.as_ref().and_then(serde_json::Value::as_u64) {
            let pool = self.get_cloned_pool().await?;
            let idx = PoolIndex::new(idx_val as u32);
            let fun = pool.function(idx)?;

            let name = pool.def_name(idx)?;
            let pretty_name = name.split(';').next().unwrap_or(&name);

            let mut snippet = String::new();
            for (i, param_idx) in fun.parameters.iter().enumerate() {
                let name = pool.def_name(*param_idx)?;
                if i != 0 {
                    write!(snippet, ", ").unwrap();
                }
                write!(snippet, "${{{}:{}}}", i + 1, name).unwrap();
            }
            let detail = util::render_function(idx, true, &pool)?;

            let item = lsp::CompletionItem {
                label: format!("{}(⠤)", pretty_name),
                kind: Some(lsp::CompletionItemKind::METHOD),
                insert_text: Some(format!("{}({})", pretty_name, snippet)),
                insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                detail: Some(detail),
                ..lsp::CompletionItem::default()
            };
            Ok(item)
        } else {
            Ok(item)
        }
    }

    async fn hover(&self, params: lsp::HoverParams) -> Result<Option<lsp::Hover>, Error> {
        let buf = self
            .buffers
            .get(&params.text_document_position_params.text_document.uri)
            .expect("No buffer found for URI");
        let matched = self
            .expr_at_location(
                params.text_document_position_params,
                true,
                |expr, typ, pool| {
                    let text = match expr {
                        Expr::Call(Callable::Function(idx), _, _, _) => {
                            util::render_function(*idx, false, pool)?
                        }
                        Expr::MethodCall(_, idx, _, _) => util::render_function(*idx, false, pool)?,
                        _ => typ.pretty(pool)?.deref().to_owned(),
                    };
                    let contents = lsp::HoverContents::Scalar(lsp::MarkedString::LanguageString(
                        lsp::LanguageString {
                            language: "redscript".to_owned(),
                            value: text,
                        },
                    ));
                    let span = expr.span();
                    let start = buf.get_loc(span.low).unwrap();
                    let end = buf.get_loc(span.high).unwrap();
                    let range = lsp::Range::new(
                        lsp::Position::new(start.0, start.1),
                        lsp::Position::new(end.0, end.1),
                    );

                    let hover = lsp::Hover {
                        contents,
                        range: Some(range),
                    };
                    Ok(Some(hover))
                },
            )
            .await?;
        Ok(matched.flatten())
    }

    async fn publish_diagnostics(&self, diagnostics: Vec<Diagnostic>, files: &Files) {
        let mut messages: HashMap<PathBuf, Vec<lsp::Diagnostic>> =
            HashMap::with_capacity(diagnostics.len());

        for diagnostic in diagnostics {
            let msg = diagnostic.to_string();
            let severity = if diagnostic.is_fatal() {
                lsp::DiagnosticSeverity::ERROR
            } else {
                lsp::DiagnosticSeverity::WARNING
            };
            let loc = files.lookup(diagnostic.span()).unwrap();

            let range = lsp::Range::new(
                lsp::Position::new(loc.start.line as u32, loc.start.col as u32),
                lsp::Position::new(loc.end.line as u32, loc.end.col as u32),
            );
            let source = Some("redscript".to_owned());
            let diagnostic =
                lsp::Diagnostic::new(range, Some(severity), None, source, msg, None, None);
            match messages.entry(loc.file.path().to_owned()) {
                hash_map::Entry::Occupied(mut entry) => {
                    entry.get_mut().push(diagnostic);
                }
                hash_map::Entry::Vacant(entry) => {
                    entry.insert(vec![diagnostic]);
                }
            }
        }

        for file in files.files() {
            let uri = lsp::Url::from_file_path(file.path()).unwrap();
            self.client.publish_diagnostics(uri, vec![], None).await;
        }

        for (path, batch) in messages {
            let uri = lsp::Url::from_file_path(path).unwrap();
            self.client.publish_diagnostics(uri, batch, None).await;
        }
    }

    fn class_completions(
        idx: PoolIndex<Class>,
        is_static: bool,
        pool: &ConstantPool,
    ) -> Result<Vec<lsp::CompletionItem>, Error> {
        let mut completions = vec![];
        let class = pool.class(idx)?;

        if !is_static {
            for idx in &class.fields {
                let field = pool.field(*idx)?;
                let type_ = pool.def_name(field.type_)?;
                let type_name = TypeName::from_repr(&type_);

                let name = pool.def_name(*idx)?;
                let item = lsp::CompletionItem {
                    label: name.deref().to_owned(),
                    kind: Some(lsp::CompletionItemKind::FIELD),
                    detail: Some(type_name.pretty().clone().to_string()),
                    ..lsp::CompletionItem::default()
                };
                completions.push(item);
            }
        }

        for idx in &class.functions {
            let fun = pool.function(*idx)?;
            if fun.flags.is_static() != is_static {
                continue;
            }
            let name = pool.def_name(*idx)?;
            let pretty_name = name.split(';').next().unwrap_or(&name);

            let mut snippet = String::new();
            for (i, param_idx) in fun.parameters.iter().enumerate() {
                let name = pool.def_name(*param_idx)?;
                if i != 0 {
                    write!(snippet, ", ").unwrap();
                }
                write!(snippet, "${{{}:{}}}", i + 1, name).unwrap();
            }
            let detail = util::render_function(*idx, true, pool)?;

            let item = lsp::CompletionItem {
                label: format!("{}(⠤)", pretty_name),
                kind: Some(lsp::CompletionItemKind::METHOD),
                insert_text: Some(format!("{}({})", pretty_name, snippet)),
                insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
                detail: Some(detail),
                ..lsp::CompletionItem::default()
            };
            completions.push(item);
        }

        if !class.base.is_undefined() {
            let base = Self::class_completions(class.base, is_static, pool)?;
            completions.extend(base);
        }
        Ok(completions)
    }

    fn enum_completions(
        idx: PoolIndex<Enum>,
        pool: &ConstantPool,
    ) -> Result<Vec<lsp::CompletionItem>, Error> {
        let mut completions = vec![];
        let enum_ = pool.enum_(idx)?;

        for member in &enum_.members {
            let name = pool.def_name(*member)?;
            let item = lsp::CompletionItem {
                label: name.deref().to_owned(),
                kind: Some(lsp::CompletionItemKind::FIELD),
                ..lsp::CompletionItem::default()
            };
            completions.push(item);
        }
        Ok(completions)
    }

    async fn get_cloned_pool(&self) -> Result<ConstantPool, Error> {
        let guard = self.state.read().await;
        let state = guard.as_ref().ok_or(Error::ServerNotInitialized)?;
        Ok(state.compiled_pool.clone())
    }

    async fn spawn_error_popup(&self, message: String) {
        let msg = lsp::ShowMessageParams {
            typ: lsp::MessageType::ERROR,
            message: message.clone(),
        };
        self.log_error(message).await;
        self.client
            .send_custom_notification::<lsp::notification::ShowMessage>(msg)
            .await;
    }

    async fn resolve_workspace(&self, url: &lsp::Url) -> Option<PathBuf> {
        let path = url.to_file_path().ok()?;
        let folders = self.client.workspace_folders().await.ok()??;
        folders.iter().find_map(|folder| {
            let folder = folder.uri.to_file_path().ok()?;
            path.starts_with(&folder).then_some(folder)
        })
    }

    async fn log_info(&self, msg: impl fmt::Display) {
        self.client.log_message(lsp::MessageType::INFO, msg).await;
    }

    async fn log_error(&self, msg: impl fmt::Display) {
        self.client.log_message(lsp::MessageType::ERROR, msg).await;
    }
}

#[lspower::async_trait]
impl LanguageServer for Backend {
    async fn initialize(
        &self,
        params: lsp::InitializeParams,
    ) -> jsonrpc::Result<lsp::InitializeResult> {
        if let Err(err) = self.initialize(params.root_uri).await {
            self.spawn_error_popup(err.to_string()).await;
        }

        let completion = lsp::CompletionOptions {
            trigger_characters: Some(vec![".".to_owned()]),
            resolve_provider: Some(true),
            ..lsp::CompletionOptions::default()
        };

        let capabilities = lsp::ServerCapabilities {
            text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(
                lsp::TextDocumentSyncKind::INCREMENTAL,
            )),
            completion_provider: Some(completion),
            hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
            ..lsp::ServerCapabilities::default()
        };

        let result = lsp::InitializeResult {
            capabilities,
            server_info: None,
        };
        Ok(result)
    }

    async fn initialized(&self, _: lsp::InitializedParams) {
        match self.post_initialize().await {
            Err(err) => self.spawn_error_popup(err.to_string()).await,
            Ok(()) => self.log_info("redscript server initialized!").await,
        }
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: lsp::DidOpenTextDocumentParams) {
        let doc = params.text_document;
        self.buffers.add(doc.uri, doc.text);
    }

    async fn did_change(&self, params: lsp::DidChangeTextDocumentParams) {
        for change in params.content_changes {
            self.buffers.update(&params.text_document.uri, change);
        }
    }

    async fn did_save(&self, params: lsp::DidSaveTextDocumentParams) {
        let Some(workspace) = self.resolve_workspace(&params.text_document.uri).await else {
            return;
        };

        if let Err(err) = self.typecheck_workspace(&workspace).await {
            self.log_info(format!("typecheck reported an error: {err}"))
                .await;
        }
    }

    async fn completion(
        &self,
        params: lsp::CompletionParams,
    ) -> jsonrpc::Result<Option<lsp::CompletionResponse>> {
        Ok(self.completion(params).await?)
    }

    async fn completion_resolve(
        &self,
        item: lsp::CompletionItem,
    ) -> jsonrpc::Result<lsp::CompletionItem> {
        Ok(self.completion_resolve(item).await?)
    }

    async fn hover(&self, params: lsp::HoverParams) -> jsonrpc::Result<Option<lsp::Hover>> {
        Ok(self.hover(params).await?)
    }
}
