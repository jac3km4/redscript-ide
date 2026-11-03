use std::fmt::Display;
use std::ops::Range;
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
    watch_files: bool,
    watched_roots: Vec<PathBuf>,
    shutdown: bool,
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
        let mut context = LspContext::new(connection.sender.clone());
        let (id, params) = connection.initialize_start()?;

        let init_params: lsp::InitializeParams = serde_json::from_value(params)?;
        context.set_diagnostic_capabilities(&init_params.capabilities);
        let watch_files = init_params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.did_change_watched_files.as_ref())
            .and_then(|watch| watch.dynamic_registration)
            .unwrap_or(false);
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
            watch_files,
            watched_roots: vec![],
            shutdown: false,
        };
        this.update_watchers()?;

        this.run()
    }

    fn run(&mut self) -> anyhow::Result<()> {
        while let Ok(message) = self.connection.receiver.recv() {
            if matches!(&message, Message::Notification(notif) if notif.method == lsp::notification::Exit::METHOD)
            {
                if !self.shutdown {
                    bail!("Received exit before shutdown");
                }
                return Ok(());
            }
            if let Err(err) = self.handle(message) {
                self.context.logger().error(err);
            }
        }
        Ok(())
    }

    fn update_watchers(&mut self) -> anyhow::Result<()> {
        if !self.watch_files {
            return Ok(());
        }
        let roots = self.server.watch_roots();
        if roots == self.watched_roots {
            return Ok(());
        }
        if !self.watched_roots.is_empty() {
            self.connection.sender.send(Message::Request(lsp_server::Request::new(
                RequestId::from("remove-source-watchers".to_owned()),
                lsp::request::UnregisterCapability::METHOD.to_owned(),
                json!({ "unregisterations": [{ "id": "source-watchers", "method": lsp::notification::DidChangeWatchedFiles::METHOD }] }),
            )))?;
        }
        let watchers: Vec<_> = roots
            .iter()
            .flat_map(|root| {
                let root = root.to_string_lossy().replace('\\', "/");
                let root = root.strip_prefix("//?/").unwrap_or(&root);
                ["**/*.reds", "**/*.[sS][cC][rR][iI][pP][tT]", ".redscript"]
                    .map(|pattern| json!({ "globPattern": format!("{root}/{pattern}") }))
            })
            .collect();
        if !watchers.is_empty() {
            self.connection
                .sender
                .send(Message::Request(lsp_server::Request::new(
                    RequestId::from("source-watchers".to_owned()),
                    lsp::request::RegisterCapability::METHOD.to_owned(),
                    lsp::RegistrationParams {
                        registrations: vec![lsp::Registration {
                            id: "source-watchers".into(),
                            method: lsp::notification::DidChangeWatchedFiles::METHOD.into(),
                            register_options: Some(json!({ "watchers": watchers })),
                        }],
                    },
                )))?;
        }
        self.watched_roots = roots;
        Ok(())
    }

    fn dispatch(&mut self, method: &str, params: Value) -> Result<Value, ResponseError> {
        if self.shutdown {
            return Err(response_error(
                ErrorCode::InvalidRequest,
                "Server is shut down",
            ));
        }
        match method {
            lsp::request::HoverRequest::METHOD => {
                let lsp::HoverParams {
                    text_document_position_params: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self
                    .document_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                response_value(self.server.hover(doc, &self.context))
            }
            lsp::request::Completion::METHOD => {
                let lsp::CompletionParams {
                    text_document_position: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self
                    .document_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                let completion = self.server.completion(doc, &self.context);
                response_value(completion.as_deref().map_err(|err| anyhow!("{err}")))
            }
            lsp::request::GotoDefinition::METHOD => {
                let lsp::GotoDefinitionParams {
                    text_document_position_params: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self
                    .document_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                response_value(self.server.goto_definition(doc, &self.context))
            }
            lsp::request::WorkspaceSymbolRequest::METHOD => {
                let lsp::WorkspaceSymbolParams { query, .. } =
                    serde_json::from_value(params).map_err(invalid_params)?;
                response_value(self.server.workspace_symbol(&query, &self.context))
            }
            lsp::request::Formatting::METHOD => {
                let lsp::DocumentFormattingParams {
                    text_document,
                    options,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self.document(&text_document.uri).map_err(invalid_params)?;
                response_value(
                    self.server
                        .format(doc, options.tab_size as u16, &self.context),
                )
            }
            lsp::request::CodeActionRequest::METHOD => {
                let lsp::CodeActionParams {
                    text_document,
                    range,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let path = path_from_uri(&text_document.uri).map_err(invalid_params)?;
                let range = self
                    .range(&text_document.uri, range)
                    .map_err(invalid_params)?;
                response_value(self.server.code_action(path, range, &self.context))
            }
            lsp::request::CodeActionResolveRequest::METHOD => {
                let action = serde_json::from_value(params).map_err(invalid_params)?;
                response_value(self.server.code_action_resolve(action, &self.context))
            }
            lsp::request::Shutdown::METHOD => {
                let () = serde_json::from_value(params).map_err(invalid_params)?;
                self.shutdown = true;
                Ok(Value::Null)
            }
            _ => Err(response_error(
                ErrorCode::MethodNotFound,
                format!("Unknown request: {method}"),
            )),
        }
    }

    fn handle(&mut self, message: Message) -> anyhow::Result<()> {
        match message {
            Message::Request(request) => {
                let result = self.dispatch(&request.method, request.params);
                let (result, error) = match result {
                    Ok(value) => (Some(value), None),
                    Err(error) => (None, Some(error)),
                };
                self.connection.sender.send(Message::Response(Response {
                    id: request.id,
                    result,
                    error,
                }))?;
            }
            Message::Notification(_) if self.shutdown => return Ok(()),
            Message::Notification(notif) => match notif.method.as_str() {
                lsp::notification::Initialized::METHOD => {}
                lsp::notification::DidSaveTextDocument::METHOD => {
                    let lsp::DidSaveTextDocumentParams {
                        text_document: doc, ..
                    } = serde_json::from_value(notif.params)?;
                    self.server.check(path_from_uri(&doc.uri)?, &self.context)?;
                }
                lsp::notification::DidOpenTextDocument::METHOD => {
                    let lsp::DidOpenTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    let uri = doc.uri;
                    self.buffers.open(uri.clone(), doc.text, doc.version);
                    self.server.document_changed(
                        path_from_uri(&uri)?,
                        self.buffers.get(&uri),
                        &self.context,
                    )?;
                }
                lsp::notification::DidCloseTextDocument::METHOD => {
                    let lsp::DidCloseTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    self.buffers.remove(&doc.uri);
                    self.server
                        .document_changed(path_from_uri(&doc.uri)?, None, &self.context)?;
                }
                lsp::notification::DidChangeTextDocument::METHOD => {
                    let lsp::DidChangeTextDocumentParams {
                        text_document: doc,
                        content_changes,
                    } = serde_json::from_value(notif.params)?;
                    if self.buffers.change(&doc.uri, doc.version, content_changes) {
                        self.server.document_changed(
                            path_from_uri(&doc.uri)?,
                            self.buffers.get(&doc.uri),
                            &self.context,
                        )?;
                    }
                }
                lsp::notification::DidChangeWatchedFiles::METHOD => {
                    let params: lsp::DidChangeWatchedFilesParams =
                        serde_json::from_value(notif.params)?;
                    let paths = params
                        .changes
                        .into_iter()
                        .map(|event| path_from_uri(&event.uri))
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    self.server.files_changed(paths, &self.context)?;
                }
                lsp::notification::DidChangeConfiguration::METHOD => {
                    self.server.files_changed(vec![], &self.context)?;
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
            Message::Response(response)
                if response.id == RequestId::from("source-watchers".to_owned())
                    || response.id == RequestId::from("remove-source-watchers".to_owned()) =>
            {
                if let Some(error) = response.error {
                    self.context.logger().error(format!(
                        "Could not register source watchers: {}",
                        error.message
                    ));
                }
            }
            Message::Response(response) => {
                self.context
                    .logger()
                    .info(format!("Received an unexpected response: {}", response.id));
            }
        };
        if !self.shutdown {
            self.update_watchers()?;
        }
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

    fn range(&self, uri: &lsp::Uri, range: lsp::Range) -> anyhow::Result<Range<u32>> {
        let document = self.document(uri)?;
        let start = document
            .body
            .get_pos(range.start.line, range.start.character)
            .context("start position not found")?;
        let end = document
            .body
            .get_pos(range.end.line, range.end.character)
            .context("end position not found")?;
        Ok(start..end)
    }
}

fn response_error(code: ErrorCode, message: impl Display) -> ResponseError {
    ResponseError {
        code: code as i32,
        message: message.to_string(),
        data: None,
    }
}

fn invalid_params(error: impl Display) -> ResponseError {
    response_error(ErrorCode::InvalidParams, error)
}

fn response_value<R: Serialize>(result: anyhow::Result<R>) -> Result<Value, ResponseError> {
    result
        .and_then(|value| Ok(serde_json::to_value(value)?))
        .map_err(|err| response_error(ErrorCode::InternalError, err))
}

pub trait LanguageServer {
    fn watch_roots(&self) -> Vec<PathBuf>;
    fn document_changed(
        &mut self,
        path: PathBuf,
        buffer: Option<&Buffer>,
        ctx: &LspContext,
    ) -> anyhow::Result<()>;
    fn files_changed(&mut self, paths: Vec<PathBuf>, ctx: &LspContext) -> anyhow::Result<()>;
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
    fn code_action(
        &self,
        path: PathBuf,
        span: Range<u32>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeActionResponse>;
    fn code_action_resolve(
        &self,
        action: lsp::CodeAction,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeAction>;
}

#[derive(Debug, Clone)]
pub struct LspContext {
    sender: Sender<Message>,
    diagnostic_tags: Vec<lsp::DiagnosticTag>,
}

impl LspContext {
    pub(crate) fn new(sender: Sender<Message>) -> Self {
        Self {
            sender,
            diagnostic_tags: vec![],
        }
    }

    pub(crate) fn set_diagnostic_capabilities(&mut self, capabilities: &lsp::ClientCapabilities) {
        self.diagnostic_tags = capabilities
            .text_document
            .as_ref()
            .and_then(|doc| doc.publish_diagnostics.as_ref())
            .and_then(|diags| diags.tag_support.as_ref())
            .map(|support| support.value_set.clone())
            .unwrap_or_default();
    }

    pub fn supports_diagnostic_tag(&self, tag: lsp::DiagnosticTag) -> bool {
        self.diagnostic_tags.contains(&tag)
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
    pub(crate) fn new(path: PathBuf, body: &'a Buffer) -> Self {
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
    pub(crate) fn new(document: Document<'a>, pos: u32) -> Self {
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

pub(crate) fn path_from_uri(uri: &lsp::Uri) -> anyhow::Result<PathBuf> {
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

fn uri_from_path(path: &Path) -> anyhow::Result<lsp::Uri> {
    let mut encoded_path = fluent_uri::encoding::EString::new();
    let path_str = path.to_string_lossy().replace('\\', "/");
    // canonicalize() uses Windows extended-length paths; these are not URI paths.
    let path_str = path_str.strip_prefix("//?/").unwrap_or(&path_str);

    #[cfg(windows)]
    encoded_path.encode::<fluent_uri::encoding::encoder::Path>(&b"/");
    encoded_path.encode::<fluent_uri::encoding::encoder::Path>(&path_str);

    let uri = fluent_uri::Uri::builder()
        .scheme(fluent_uri::component::Scheme::new_or_panic("file"))
        .authority(fluent_uri::component::Authority::EMPTY)
        .path(&encoded_path)
        .build()?;
    Ok(lsp::Uri::from_str(uri.as_str())?)
}

fn capabilities() -> lsp::ServerCapabilities {
    lsp::ServerCapabilities {
        text_document_sync: Some(lsp::TextDocumentSyncCapability::Options(
            lsp::TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(lsp::TextDocumentSyncKind::INCREMENTAL),
                save: Some(lsp::TextDocumentSyncSaveOptions::Supported(true)),
                ..Default::default()
            },
        )),
        completion_provider: Some(lsp::CompletionOptions {
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
        code_action_provider: Some(lsp::CodeActionProviderCapability::Options(
            lsp::CodeActionOptions {
                resolve_provider: Some(true),
                ..Default::default()
            },
        )),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use std::time::Duration;

    use super::*;

    struct StubServer;

    impl LanguageServer for StubServer {
        fn watch_roots(&self) -> Vec<PathBuf> {
            vec![]
        }

        fn document_changed(
            &mut self,
            _path: PathBuf,
            _buffer: Option<&Buffer>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            panic!("unexpected document change");
        }

        fn files_changed(&mut self, _paths: Vec<PathBuf>, _ctx: &LspContext) -> anyhow::Result<()> {
            panic!("unexpected file change");
        }

        fn check(&mut self, _path: PathBuf, _ctx: &LspContext) -> anyhow::Result<()> {
            panic!("unexpected check");
        }

        fn change_workspace_folders(
            &mut self,
            _added: Vec<PathBuf>,
            _removed: Vec<PathBuf>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            panic!("unexpected workspace change");
        }

        fn hover(&self, _doc: CodeLocation<'_>, _ctx: &LspContext) -> anyhow::Result<lsp::Hover> {
            bail!("stub handler failed");
        }

        fn completion(
            &self,
            _doc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<Rc<lsp::CompletionResponse>> {
            Ok(Rc::new(lsp::CompletionResponse::Array(vec![])))
        }

        fn goto_definition(
            &self,
            _doc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
            Ok(lsp::GotoDefinitionResponse::Array(vec![]))
        }

        fn workspace_symbol(
            &self,
            _query: &str,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::WorkspaceSymbolResponse> {
            Ok(lsp::WorkspaceSymbolResponse::Flat(vec![]))
        }

        fn format(
            &self,
            _doc: Document<'_>,
            _tab_size: u16,
            _ctx: &LspContext,
        ) -> anyhow::Result<Vec<lsp::TextEdit>> {
            Ok(vec![])
        }

        fn code_action(
            &self,
            _path: PathBuf,
            _span: Range<u32>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::CodeActionResponse> {
            Ok(vec![])
        }

        fn code_action_resolve(
            &self,
            action: lsp::CodeAction,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::CodeAction> {
            Ok(action)
        }
    }

    fn memory_server() -> (LspServer<'static>, Connection) {
        let (connection, client) = Connection::memory();
        let context = LspContext::new(connection.sender.clone());
        let mut buffers = Buffers::default();
        buffers.open("file:///C:/test.reds".parse().unwrap(), "é😀test".into(), 1);
        (
            LspServer {
                connection,
                server: Box::new(StubServer),
                buffers,
                context,
                watch_files: false,
                watched_roots: vec![],
                shutdown: false,
            },
            client,
        )
    }

    fn send_request(client: &Connection, id: i32, method: &str, params: Value) {
        client
            .sender
            .send(Message::Request(lsp_server::Request::new(
                id.into(),
                method.into(),
                params,
            )))
            .unwrap();
    }

    fn send_exit(client: &Connection) {
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Exit::METHOD.into(),
                Value::Null,
            )))
            .unwrap();
    }

    fn receive_response(client: &Connection, id: i32) -> Response {
        let Message::Response(response) = client
            .receiver
            .recv_timeout(Duration::from_secs(2))
            .unwrap()
        else {
            panic!("expected response");
        };
        assert_eq!(response.id, RequestId::from(id));
        assert_ne!(response.result.is_some(), response.error.is_some());
        response
    }

    #[test]
    fn requests_receive_exactly_one_response() {
        let (mut server, client) = memory_server();
        let location = |uri: &str, line, character| json!({ "textDocument": { "uri": uri }, "position": { "line": line, "character": character } });
        let cases = [
            (
                lsp::request::HoverRequest::METHOD,
                json!({}),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::Completion::METHOD,
                Value::Null,
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::GotoDefinition::METHOD,
                json!([]),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::WorkspaceSymbolRequest::METHOD,
                json!({ "query": 1 }),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::Formatting::METHOD,
                json!({}),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::CodeActionRequest::METHOD,
                json!({}),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::CodeActionResolveRequest::METHOD,
                json!({}),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::Shutdown::METHOD,
                json!({}),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::HoverRequest::METHOD,
                location("file:///C:/missing.reds", 0, 0),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::HoverRequest::METHOD,
                location("https://example.com/test.reds", 0, 0),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::HoverRequest::METHOD,
                location("file:///C:/test.reds", 9, 0),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::HoverRequest::METHOD,
                location("file:///C:/test.reds", 0, 2),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::Formatting::METHOD,
                json!({ "textDocument": { "uri": "file:///C:/missing.reds" }, "options": { "tabSize": 4, "insertSpaces": true } }),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::CodeActionRequest::METHOD,
                json!({ "textDocument": { "uri": "file:///C:/test.reds" }, "range": { "start": { "line": 9, "character": 0 }, "end": { "line": 9, "character": 1 } }, "context": { "diagnostics": [] } }),
                ErrorCode::InvalidParams,
            ),
            (
                lsp::request::CodeActionRequest::METHOD,
                json!({ "textDocument": { "uri": "file:///C:/test.reds" }, "range": { "start": { "line": 0, "character": 0 }, "end": { "line": 0, "character": 2 } }, "context": { "diagnostics": [] } }),
                ErrorCode::InvalidParams,
            ),
            ("unknown/method", Value::Null, ErrorCode::MethodNotFound),
            (
                lsp::request::HoverRequest::METHOD,
                location("file:///C:/test.reds", 0, 3),
                ErrorCode::InternalError,
            ),
        ];
        for (id, (method, params, _)) in cases.iter().enumerate() {
            send_request(&client, id as i32, method, params.clone());
        }
        let success_id = cases.len() as i32;
        send_request(
            &client,
            success_id,
            lsp::request::WorkspaceSymbolRequest::METHOD,
            json!({ "query": "" }),
        );
        send_request(
            &client,
            success_id + 1,
            lsp::request::Shutdown::METHOD,
            Value::Null,
        );
        send_exit(&client);
        server.run().unwrap();

        for (id, (_, _, code)) in cases.into_iter().enumerate() {
            let error = receive_response(&client, id as i32).error.unwrap();
            assert_eq!(error.code, code as i32);
            assert!(!error.message.is_empty());
        }
        assert_eq!(
            receive_response(&client, success_id).result,
            Some(json!([]))
        );
        assert_eq!(
            receive_response(&client, success_id + 1).result,
            Some(Value::Null)
        );
        assert!(client.receiver.try_recv().is_err(), "duplicate response");
    }

    #[test]
    fn shutdown_rejects_requests_and_ignores_notifications() {
        let (mut server, client) = memory_server();
        send_request(&client, 0, lsp::request::Shutdown::METHOD, Value::Null);
        for (id, method) in [
            lsp::request::Shutdown::METHOD,
            lsp::request::HoverRequest::METHOD,
            "unknown/method",
        ]
        .into_iter()
        .enumerate()
        {
            send_request(&client, id as i32 + 1, method, Value::Null);
        }
        for (method, params) in [
            (
                lsp::notification::DidOpenTextDocument::METHOD,
                json!({ "textDocument": { "uri": "file:///C:/ignored.reds", "languageId": "redscript", "version": 1, "text": "ignored" } }),
            ),
            (
                lsp::notification::DidChangeConfiguration::METHOD,
                json!({ "settings": {} }),
            ),
            (lsp::notification::DidSaveTextDocument::METHOD, Value::Null),
        ] {
            client
                .sender
                .send(Message::Notification(lsp_server::Notification::new(
                    method.into(),
                    params,
                )))
                .unwrap();
        }
        send_exit(&client);
        server.run().unwrap();
        assert_eq!(receive_response(&client, 0).result, Some(Value::Null));
        for id in 1..=3 {
            assert_eq!(
                receive_response(&client, id).error.unwrap().code,
                ErrorCode::InvalidRequest as i32
            );
        }
        assert!(
            server
                .buffers
                .get(&"file:///C:/ignored.reds".parse().unwrap())
                .is_none()
        );
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn exit_without_shutdown_fails() {
        let (mut server, client) = memory_server();
        send_exit(&client);
        assert!(
            server
                .run()
                .unwrap_err()
                .to_string()
                .contains("before shutdown")
        );
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn disconnected_receiver_stops() {
        let (sender, receiver) = crossbeam_channel::bounded(1);
        let thread = std::thread::spawn(move || {
            let (mut server, client) = memory_server();
            drop(client);
            sender.send(server.run()).unwrap();
        });
        receiver
            .recv_timeout(Duration::from_secs(2))
            .unwrap()
            .unwrap();
        thread.join().unwrap();
    }

    #[test]
    fn test_uri_from_windows_path() {
        let path = PathBuf::from("C:/some/path/r6");
        let uri = uri_from_path(&path).unwrap();
        assert_eq!(uri.to_string(), "file:///C:/some/path/r6");
    }

    #[test]
    fn test_uri_from_windows_path_with_backslashes() {
        let path = PathBuf::from("C:\\some\\path\\r6");
        let uri = uri_from_path(&path).unwrap();
        assert_eq!(uri.to_string(), "file:///C:/some/path/r6");
    }

    #[test]
    #[cfg(not(windows))]
    fn test_uri_from_unix_path() {
        let path = PathBuf::from("/some/path/r6");
        let uri = uri_from_path(&path).unwrap();
        assert_eq!(uri.to_string(), "file:///some/path/r6");
    }

    #[test]
    fn test_path_from_windows_uri() {
        let uri = lsp::Uri::from_str("file:///C:/some/path/r6").unwrap();
        let path = path_from_uri(&uri).unwrap();
        assert_eq!(path, PathBuf::from("C:/some/path/r6"));
    }

    #[test]
    #[cfg(not(windows))]
    fn test_path_from_unix_uri() {
        let uri = lsp::Uri::from_str("file:///some/path/r6").unwrap();
        let path = path_from_uri(&uri).unwrap();
        assert_eq!(path, PathBuf::from("/some/path/r6"));
    }
}
