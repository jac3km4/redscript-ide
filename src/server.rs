use std::fmt::Display;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;

use anyhow::{Context, anyhow, bail};
use crossbeam_channel::Sender;
use lsp_server::{Connection, ErrorCode, Message, RequestId, Response, ResponseError};
use lsp_types::notification::Notification;
use lsp_types::request::Request;
use lsp_types::{self as lsp};
use serde::Serialize;
use serde_json::{Value, json};

use crate::buffers::{Buffer, Buffers};

pub struct LspServer<'a> {
    connection: Connection,
    server: Box<dyn LanguageServer + 'a>,
    buffers: Buffers,
    context: LspContext,
}

impl LspServer<'_> {
    pub fn spawn<P>(
        create_props: impl Fn(Options, &LspContext) -> anyhow::Result<P>,
        create_server: impl for<'a> Fn(
            &'a P,
            &LspContext,
        ) -> anyhow::Result<Box<dyn LanguageServer + 'a>>,
    ) -> anyhow::Result<()> {
        let (connection, _io) = Connection::stdio();
        let context = LspContext::new(connection.sender.clone());
        let (id, params) = connection.initialize_start()?;

        let init_params: lsp::InitializeParams = serde_json::from_value(params)?;
        let workspace_folders = init_params
            .workspace_folders
            .unwrap_or_default()
            .into_iter()
            .map(|folder| path_from_uri(&folder.uri))
            .collect::<Result<Vec<PathBuf>, _>>()?;
        let options = Options::new(workspace_folders, init_params.initialization_options);
        let props = create_props(options, &context)?;

        let server_capabilities = capabilities();
        let initialize_data = json!({
            "capabilities": server_capabilities,
            "serverInfo": {
                "name": env!("CARGO_PKG_NAME"),
                "version": env!("CARGO_PKG_VERSION")
            }
        });
        connection.initialize_finish(id, initialize_data)?;

        let server = create_server(&props, &context)?;
        let mut this = LspServer {
            connection,
            server,
            context: context.clone(),
            buffers: Buffers::default(),
        };

        loop {
            if let Err(err) = this.handle() {
                context.logger().error(err);
            }
        }
    }

    fn handle(&mut self) -> anyhow::Result<()> {
        match self.connection.receiver.recv()? {
            Message::Request(request) => match request.method.as_str() {
                lsp::request::HoverRequest::METHOD => {
                    let lsp::HoverParams {
                        text_document_position_params: params,
                        ..
                    } = serde_json::from_value(request.params)?;
                    let doc = self.document_location(&params.text_document.uri, params.position)?;
                    self.handle_response(request.id, self.server.hover(doc, &self.context))?;
                }
                lsp::request::Completion::METHOD => {
                    let lsp::CompletionParams {
                        text_document_position: params,
                        ..
                    } = serde_json::from_value(request.params)?;
                    let doc = self.document_location(&params.text_document.uri, params.position)?;
                    let completion = self.server.completion(doc, &self.context);
                    self.handle_response(
                        request.id,
                        completion.as_deref().map_err(|err| anyhow!("{err}")),
                    )?;
                }
                lsp::request::GotoDefinition::METHOD => {
                    let lsp::GotoDefinitionParams {
                        text_document_position_params: params,
                        ..
                    } = serde_json::from_value(request.params)?;
                    let doc = self.document_location(&params.text_document.uri, params.position)?;
                    self.handle_response(
                        request.id,
                        self.server.goto_definition(doc, &self.context),
                    )?;
                }
                lsp::request::WorkspaceSymbolRequest::METHOD => {
                    let lsp::WorkspaceSymbolParams { query, .. } =
                        serde_json::from_value(request.params)?;
                    self.handle_response(
                        request.id,
                        self.server.workspace_symbol(&query, &self.context),
                    )?;
                }
                lsp::request::Formatting::METHOD => {
                    let lsp::DocumentFormattingParams {
                        text_document,
                        options,
                        ..
                    } = serde_json::from_value(request.params)?;
                    let doc = self.document(&text_document.uri)?;
                    self.handle_response(
                        request.id,
                        self.server
                            .format(doc, options.tab_size as u16, &self.context),
                    )?;
                }
                _ => {
                    self.context
                        .logger()
                        .error(format!("Unknown request: {}", request.method));
                }
            },

            Message::Notification(notif) => match notif.method.as_str() {
                lsp::notification::DidSaveTextDocument::METHOD => {
                    let lsp::DidSaveTextDocumentParams {
                        text_document: doc, ..
                    } = serde_json::from_value(notif.params)?;
                    self.server.check(path_from_uri(&doc.uri)?, &self.context)?;
                }
                lsp::notification::DidOpenTextDocument::METHOD => {
                    let lsp::DidOpenTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    self.buffers.add(doc.uri, doc.text);
                }
                lsp::notification::DidCloseTextDocument::METHOD => {
                    let lsp::DidCloseTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    self.buffers.remove(&doc.uri);
                }
                lsp::notification::DidChangeTextDocument::METHOD => {
                    let lsp::DidChangeTextDocumentParams {
                        text_document: doc,
                        content_changes,
                    } = serde_json::from_value(notif.params)?;
                    for change in content_changes {
                        match change.range {
                            Some(range) => self.buffers.update_range(&doc.uri, range, change.text),
                            None => self.buffers.add(doc.uri.clone(), change.text),
                        }
                    }
                }
                lsp::notification::DidChangeWorkspaceFolders::METHOD => {
                    let lsp::DidChangeWorkspaceFoldersParams { event } =
                        serde_json::from_value(notif.params)?;
                    let added = event
                        .added
                        .into_iter()
                        .map(|folder| path_from_uri(&folder.uri))
                        .collect::<Result<Vec<_>, _>>()?;
                    let removed = event
                        .removed
                        .into_iter()
                        .map(|uri| path_from_uri(&uri.uri))
                        .collect::<Result<Vec<_>, _>>()?;
                    self.server
                        .change_workspace_folders(added, removed, &self.context)?;
                }
                _ => {
                    self.context.logger().info(format!(
                        "Received an unexpected notification: {}",
                        notif.method
                    ));
                }
            },
            Message::Response(response) => {
                self.context
                    .logger()
                    .info(format!("Received an unexpected response: {}", response.id));
            }
        };
        Ok(())
    }

    fn handle_response<R: Serialize>(
        &self,
        id: RequestId,
        res: anyhow::Result<R>,
    ) -> anyhow::Result<()> {
        match res {
            Ok(resp) => self.connection.sender.send(Message::Response(Response {
                id,
                result: Some(serde_json::to_value(&resp)?),
                error: None,
            }))?,
            Err(err) => self.connection.sender.send(Message::Response(Response {
                id,
                result: None,
                error: Some(ResponseError {
                    code: ErrorCode::InternalError as i32,
                    message: err.to_string(),
                    data: None,
                }),
            }))?,
        };
        Ok(())
    }

    fn document(&self, uri: &lsp::Uri) -> anyhow::Result<Document<'_>> {
        let path = path_from_uri(uri)?;
        let content = self.buffers.get(uri).context("URI not found")?;
        Ok(Document::new(path, content))
    }

    fn document_location(
        &self,
        uri: &lsp::Uri,
        pos: lsp::Position,
    ) -> anyhow::Result<CodeLocation<'_>> {
        let document = self.document(uri)?;
        let pos = document
            .body
            .get_pos(pos.line, pos.character)
            .context("position not found")?;
        Ok(CodeLocation::new(document, pos))
    }
}

pub trait LanguageServer {
    fn check(&mut self, path: PathBuf, ctx: &LspContext) -> anyhow::Result<()>;
    fn change_workspace_folders(
        &mut self,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
        ctx: &LspContext,
    ) -> anyhow::Result<()>;
    fn hover(&self, doc: CodeLocation<'_>, ctx: &LspContext) -> anyhow::Result<lsp::Hover>;
    fn completion(
        &self,
        doc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Rc<lsp::CompletionResponse>>;
    fn goto_definition(
        &self,
        doc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::GotoDefinitionResponse>;
    fn workspace_symbol(
        &self,
        query: &str,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::WorkspaceSymbolResponse>;
    fn format(
        &self,
        doc: Document<'_>,
        tab_size: u16,
        ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp::TextEdit>>;
}

#[derive(Debug, Clone)]
pub struct LspContext {
    sender: Sender<Message>,
}

impl LspContext {
    fn new(sender: Sender<Message>) -> Self {
        Self { sender }
    }

    pub fn logger(&self) -> LspLog<'_> {
        LspLog::new(self)
    }

    pub fn notify<N: lsp::notification::Notification>(&self, params: N::Params) {
        let notification = lsp_server::Notification::new(N::METHOD.into(), params);
        self.sender.send(Message::Notification(notification)).ok();
    }

    pub fn uri(&self, path: &Path) -> anyhow::Result<lsp::Uri> {
        uri_from_path(path)
    }
}

#[derive(Debug)]
pub struct LspLog<'a> {
    context: &'a LspContext,
}

impl<'a> LspLog<'a> {
    fn new(context: &'a LspContext) -> Self {
        Self { context }
    }

    pub fn info(&self, message: impl Display) {
        self.context
            .notify::<lsp::notification::LogMessage>(lsp::LogMessageParams {
                typ: lsp::MessageType::INFO,
                message: message.to_string(),
            });
    }

    pub fn error(&self, message: impl Display) {
        self.context
            .notify::<lsp::notification::LogMessage>(lsp::LogMessageParams {
                typ: lsp::MessageType::ERROR,
                message: message.to_string(),
            });
    }
}

#[derive(Debug, Clone)]
pub struct Document<'a> {
    path: PathBuf,
    body: &'a Buffer,
}

impl<'a> Document<'a> {
    fn new(path: PathBuf, body: &'a Buffer) -> Self {
        Self { path, body }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn buffer(&self) -> &Buffer {
        self.body
    }
}

#[derive(Debug, Clone)]
pub struct CodeLocation<'a> {
    document: Document<'a>,
    pos: u32,
}

impl<'a> CodeLocation<'a> {
    fn new(document: Document<'a>, pos: u32) -> Self {
        Self { document, pos }
    }

    pub fn doc(&self) -> &Document<'a> {
        &self.document
    }

    pub fn pos(&self) -> u32 {
        self.pos
    }

    pub fn with_pos(self, pos: u32) -> Self {
        Self {
            document: self.document,
            pos,
        }
    }
}

#[derive(Debug)]
pub struct Options {
    pub workspace_dirs: Vec<PathBuf>,
    pub init_options: Option<Value>,
}

impl Options {
    fn new(workspace_dirs: Vec<PathBuf>, init_options: Option<Value>) -> Self {
        Self {
            workspace_dirs,
            init_options,
        }
    }
}

fn path_from_uri(uri: &lsp::Uri) -> anyhow::Result<PathBuf> {
    if !uri
        .scheme()
        .is_some_and(|scheme| scheme.eq_lowercase("file"))
    {
        bail!("Unsupported URI scheme");
    }

    let stripped = uri.path().as_estr().decode().into_string()?;
    #[cfg(windows)]
    let path = PathBuf::from(stripped.trim_start_matches("/"));
    #[cfg(not(windows))]
    let path = PathBuf::from(&*stripped);
    Ok(path)
}

fn uri_from_path(p: &Path) -> anyhow::Result<lsp::Uri> {
    let mut path = fluent_uri::encoding::EString::new();
    path.encode::<fluent_uri::encoding::encoder::Path>(p.as_os_str().as_encoded_bytes());

    let uri = fluent_uri::Uri::builder()
        .scheme(fluent_uri::component::Scheme::new_or_panic("file"))
        .path(&path)
        .build()?;
    Ok(lsp::Uri::from_str(uri.as_str())?)
}

fn capabilities() -> lsp::ServerCapabilities {
    lsp::ServerCapabilities {
        text_document_sync: Some(lsp::TextDocumentSyncCapability::Kind(
            lsp::TextDocumentSyncKind::INCREMENTAL,
        )),
        completion_provider: Some(lsp::CompletionOptions {
            resolve_provider: Some(true),
            trigger_characters: Some(vec![".".to_owned()]),
            completion_item: Some(lsp::CompletionOptionsCompletionItem {
                label_details_support: Some(true),
            }),
            ..Default::default()
        }),
        hover_provider: Some(lsp::HoverProviderCapability::Simple(true)),
        definition_provider: Some(lsp::OneOf::Left(true)),
        workspace: Some(lsp::WorkspaceServerCapabilities {
            workspace_folders: Some(lsp::WorkspaceFoldersServerCapabilities {
                supported: Some(true),
                change_notifications: Some(lsp::OneOf::Left(true)),
            }),
            ..Default::default()
        }),
        workspace_symbol_provider: Some(lsp::OneOf::Left(true)),
        document_formatting_provider: Some(lsp::OneOf::Left(true)),
        ..Default::default()
    }
}
