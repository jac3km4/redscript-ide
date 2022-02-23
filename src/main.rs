#![feature(once_cell)]
use std::collections::{hash_map, HashMap};
use std::fs::File;
use std::ops::Deref;
use std::path::PathBuf;

use buffers::Buffers;
use error::Error;
use lspower::lsp::MessageType;
use lspower::{jsonrpc, lsp, Client, LanguageServer, LspService, Server};
use redscript::ast::{Expr, TypeName};
use redscript::bundle::{ConstantPool, PoolIndex, ScriptBundle};
use redscript::definition::{Class, Enum};
use redscript_compiler::parser;
use redscript_compiler::scope::{Reference, TypeId};
use redscript_compiler::source_map::{Files, SourceFilter};
use redscript_compiler::typechecker::{type_of, Callable, TypedAst};
use redscript_compiler::unit::{CompilationUnit, Diagnostic};
use serde::Deserialize;
use tokio::sync::{OnceCell, RwLock};

mod buffers;
mod error;
mod util;

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
    Server::new(stdin, stdout).interleave(messages).serve(service).await;
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
    const CONFIG_FIELDS: &'static [&'static str] = &["redscript.scriptCachePath", "redscript.gameDir"];

    async fn initialize(&self, uri: Option<lsp::Url>) -> Result<(), Error> {
        let path = uri
            .ok_or_else(|| Error::Server("No workspace open".to_owned()))?
            .to_file_path()
            .map_err(|_| Error::Server("Invalid workspace path".to_owned()))?;

        self.workspace_path.set(path).unwrap();
        Ok(())
    }

    async fn post_initialize(&self) -> Result<(), Error> {
        let conf = self.get_configuration().await?;
        let path = conf.script_cache_path.map(Ok).unwrap_or_else(|| {
            conf.game_dir
                .map(|dir| dir.join("r6").join("cache").join("final.redscripts.bk"))
                .ok_or_else(|| Error::Server("Missing configuration".to_string()))
        })?;

        let bundle = ScriptBundle::load(&mut File::open(path)?)?;
        self.pool.set(bundle.pool).unwrap();

        self.typecheck_workspace().await?;
        Ok(())
    }

    async fn get_configuration(&self) -> Result<Config, Error> {
        let items = Self::CONFIG_FIELDS
            .iter()
            .map(|str| lsp::ConfigurationItem {
                section: Some(str.deref().to_owned()),
                scope_uri: None,
            })
            .collect();
        let conf_json = self.client.configuration(items).await?;

        let config: Config = serde_json::from_value(serde_json::Value::Array(conf_json))?;
        Ok(config)
    }

    async fn typecheck_workspace(&self) -> Result<(), Error> {
        if let Some(mut compiled_pool) = self.pool.get().cloned() {
            let path = self.workspace_path.get().unwrap();
            let files = Files::from_dir(path, SourceFilter::None)?;

            match CompilationUnit::new(&mut compiled_pool)?.typecheck_files(&files, false, false) {
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
            self.client
                .log_message(MessageType::WARNING, "Project not initialized")
                .await;
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
            .expect("No buffer found for URI");
        let mut pool = self.get_cloned_pool().await?;
        let mut copy = buf.contents().clone();
        let mut needle = buf
            .get_pos(pos.position.line, pos.position.character)
            .expect("Position outside of the buffer");

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
                let (functions, _) = CompilationUnit::new(&mut pool)?.typecheck(vec![module], false, true)?;
                // TODO: should use binary search
                if let Some((expr, scope)) = functions
                    .iter()
                    .find(|fun| fun.span.contains(needle))
                    .and_then(|fun| util::find_in_seq(&fun.code.exprs, needle).map(|e| (e, &fun.scope)))
                {
                    let typ = type_of(expr, scope, &pool)?;
                    return Ok(Some(on_expr(expr, typ, &pool)?));
                } else {
                    self.client.log_message(lsp::MessageType::INFO, "Node not found").await;
                }
            }
            Err(err) => {
                self.client.log_message(lsp::MessageType::INFO, err).await;
            }
        }
        Ok(None)
    }

    async fn completion(&self, params: lsp::CompletionParams) -> Result<Option<lsp::CompletionResponse>, Error> {
        let matched = self
            .expr_at_location(params.text_document_position, true, |expr, typ, pool| {
                let is_static = matches!(expr, Expr::Ident(Reference::Symbol(_), _));
                match typ.unwrapped() {
                    TypeId::Class(idx) => {
                        let completions = Self::class_completions(*idx, is_static, pool)?;
                        Ok(Some(lsp::CompletionResponse::Array(completions)))
                    }
                    TypeId::Struct(idx) => {
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
    async fn completion_resolve(&self, item: lsp::CompletionItem) -> Result<lsp::CompletionItem, Error> {
        if let Some(idx_val) = item.data.as_ref().and_then(|x| x.as_u64()) {
            let pool = self.get_cloned_pool().await?;
            let idx = PoolIndex::new(idx_val as u32);
            let fun = pool.function(idx)?;

            let name = pool.def_name(idx)?;
            let pretty_name = name.split(';').next().unwrap_or(&name);

            let mut snippet = String::new();
            for (i, param_idx) in fun.parameters.iter().enumerate() {
                let name = pool.def_name(*param_idx)?;
                if i != 0 {
                    snippet.push_str(", ");
                }
                snippet.push_str(&format!("${{{}:{}}}", i + 1, name));
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
            .expr_at_location(params.text_document_position_params, true, |expr, typ, pool| {
                let text = match expr {
                    Expr::Call(Callable::Function(idx), _, _, _) => util::render_function(*idx, false, pool)?,
                    Expr::MethodCall(_, idx, _, _) => util::render_function(*idx, false, pool)?,
                    _ => typ.pretty(pool)?.to_owned().deref().to_owned(),
                };
                let contents = lsp::HoverContents::Scalar(lsp::MarkedString::LanguageString(lsp::LanguageString {
                    language: "redscript".to_owned(),
                    value: text,
                }));
                let span = expr.span();
                let start = buf.get_loc(span.low).unwrap();
                let end = buf.get_loc(span.high).unwrap();
                let range = lsp::Range::new(lsp::Position::new(start.0, start.1), lsp::Position::new(end.0, end.1));

                let hover = lsp::Hover {
                    contents,
                    range: Some(range),
                };
                Ok(Some(hover))
            })
            .await?;
        Ok(matched.flatten())
    }

    async fn publish_diagnostics(&self, diagnostics: Vec<Diagnostic>, files: &Files) {
        let mut messages: HashMap<PathBuf, Vec<lsp::Diagnostic>> = HashMap::with_capacity(diagnostics.len());

        for diagnostic in diagnostics {
            let (msg, severity, loc) = match diagnostic {
                Diagnostic::MethodConflict(_, pos) => {
                    let msg = "Conflicting method replacement".to_owned();
                    let loc = files.lookup(pos).unwrap();
                    (msg, lsp::DiagnosticSeverity::WARNING, loc)
                }
                Diagnostic::Deprecation(msg, pos) => {
                    let loc = files.lookup(pos).unwrap();
                    (msg, lsp::DiagnosticSeverity::WARNING, loc)
                }
                Diagnostic::CompileError(msg, pos) => {
                    let loc = files.lookup(pos).unwrap();
                    (msg, lsp::DiagnosticSeverity::ERROR, loc)
                }
            };

            let range = lsp::Range::new(
                lsp::Position::new(loc.start.line as u32, loc.start.col as u32),
                lsp::Position::new(loc.end.line as u32, loc.end.col as u32),
            );
            let source = Some("redscript".to_owned());
            let diagnostic = lsp::Diagnostic::new(range, Some(severity), None, source, msg, None, None);
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
                    detail: Some(type_name.pretty().to_owned().to_string()),
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
                    snippet.push_str(", ");
                }
                snippet.push_str(&format!("${{{}:{}}}", i + 1, name));
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

    fn enum_completions(idx: PoolIndex<Enum>, pool: &ConstantPool) -> Result<Vec<lsp::CompletionItem>, Error> {
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
        let state = guard
            .as_ref()
            .ok_or_else(|| Error::Server("Server not initialized".to_owned()))?;
        Ok(state.compiled_pool.clone())
    }

    async fn notify_error(&self, message: String) {
        let msg = lsp::ShowMessageParams {
            typ: lsp::MessageType::ERROR,
            message,
        };
        self.client
            .send_custom_notification::<lsp::notification::ShowMessage>(msg)
            .await
    }
}

#[lspower::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, params: lsp::InitializeParams) -> jsonrpc::Result<lsp::InitializeResult> {
        self.initialize(params.root_uri).await?;

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
            Err(err) => self.notify_error(err.to_string()).await,
            Ok(()) => {
                self.client
                    .log_message(lsp::MessageType::INFO, "Server initialized!")
                    .await
            }
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
            self.buffers.update(&params.text_document.uri, change)
        }
    }

    async fn did_save(&self, _params: lsp::DidSaveTextDocumentParams) {
        if let Err(err) = self.typecheck_workspace().await {
            self.client.log_message(lsp::MessageType::ERROR, err.to_string()).await;
        }
    }

    async fn completion(&self, params: lsp::CompletionParams) -> jsonrpc::Result<Option<lsp::CompletionResponse>> {
        Ok(self.completion(params).await?)
    }

    async fn completion_resolve(&self, item: lsp::CompletionItem) -> jsonrpc::Result<lsp::CompletionItem> {
        Ok(self.completion_resolve(item).await?)
    }

    async fn hover(&self, params: lsp::HoverParams) -> jsonrpc::Result<Option<lsp::Hover>> {
        Ok(self.hover(params).await?)
    }
}
