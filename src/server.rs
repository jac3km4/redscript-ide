use std::fmt::Display;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::str::FromStr;
use std::time::Instant;

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
        context.set_code_action_capabilities(&init_params.capabilities);
        log_initialization(&context, &init_params);
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
        let props = create_props(options, &context).inspect_err(|err| {
            context
                .logger()
                .error(format!("Initialization options failed: {err:#}"));
        })?;

        let server_capabilities = capabilities();
        let initialize_data = json!({
            "capabilities": server_capabilities,
            "serverInfo": {
                "name": env!("CARGO_PKG_NAME"),
                "version": env!("CARGO_PKG_VERSION")
            }
        });
        connection.initialize_finish(id, initialize_data)?;

        let server = create_server(&props, &context).inspect_err(|err| {
            context
                .logger()
                .error(format!("Language server startup failed: {err:#}"));
        })?;
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
        let mut next_message = None;
        while let Ok(message) = next_message
            .take()
            .map(Ok)
            .unwrap_or_else(|| self.connection.receiver.recv())
        {
            if !self.shutdown
                && matches!(&message, Message::Notification(notif) if notif.method == lsp::notification::DidChangeTextDocument::METHOD)
            {
                let mut pending = Vec::new();
                let mut message = message;
                loop {
                    let Message::Notification(notif) = message else {
                        unreachable!();
                    };
                    match self.apply_document_change(notif.params) {
                        Ok(Some(uri)) if !pending.contains(&uri) => pending.push(uri),
                        Ok(_) => {}
                        Err(err) => self.context.logger().error(err),
                    }
                    // Only drain changes already queued; every other message is a barrier.
                    match self.connection.receiver.try_recv() {
                        Ok(Message::Notification(notif))
                            if notif.method == lsp::notification::DidChangeTextDocument::METHOD =>
                        {
                            message = Message::Notification(notif);
                        }
                        other => {
                            next_message = other.ok();
                            break;
                        }
                    }
                }
                for uri in pending {
                    let result = (|| {
                        self.server.document_changed(
                            path_from_uri(&uri)?,
                            self.buffers.get(&uri),
                            &self.context,
                        )?;
                        self.update_watchers()
                    })();
                    if let Err(err) = result {
                        self.context.logger().error(err);
                    }
                }
                continue;
            }
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
            lsp::request::DocumentSymbolRequest::METHOD => {
                let lsp::DocumentSymbolParams { text_document, .. } =
                    serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self.document(&text_document.uri).map_err(invalid_params)?;
                response_value(self.server.document_symbols(doc, &self.context))
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
                    context,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let path = path_from_uri(&text_document.uri).map_err(invalid_params)?;
                let range = self
                    .range(&text_document.uri, range)
                    .map_err(invalid_params)?;
                response_value(self.server.code_action(path, range, context, &self.context))
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
                let started = Instant::now();
                let log_summary = matches!(
                    request.method.as_str(),
                    lsp::request::HoverRequest::METHOD | lsp::request::Completion::METHOD
                );
                if log_summary {
                    let params = &request.params;
                    self.context.logger().info(format!(
                        "Request method={} id={} uri={} line={:?} character={:?}",
                        request.method,
                        request.id,
                        params
                            .pointer("/textDocument/uri")
                            .and_then(Value::as_str)
                            .unwrap_or("<missing>"),
                        params.pointer("/position/line").and_then(Value::as_u64),
                        params
                            .pointer("/position/character")
                            .and_then(Value::as_u64),
                    ));
                    if request.method == lsp::request::Completion::METHOD {
                        self.context.logger().info(format!(
                            "Completion context id={} trigger_kind={:?} trigger_character={:?}",
                            request.id,
                            params
                                .pointer("/context/triggerKind")
                                .and_then(Value::as_u64),
                            params
                                .pointer("/context/triggerCharacter")
                                .and_then(Value::as_str),
                        ));
                    }
                }
                let result = self.dispatch(&request.method, request.params);
                let (result, error) = match result {
                    Ok(value) => {
                        if log_summary {
                            self.context.logger().info(format!(
                                "Response method={} id={} {} elapsed_ms={:.3}",
                                request.method,
                                request.id,
                                response_summary(&request.method, &value),
                                started.elapsed().as_secs_f64() * 1000.0,
                            ));
                        }
                        (Some(value), None)
                    }
                    Err(error) => {
                        self.context.logger().error(format!(
                            "Request failed method={} id={} code={} message={} elapsed_ms={:.3}",
                            request.method,
                            request.id,
                            error.code,
                            error.message,
                            started.elapsed().as_secs_f64() * 1000.0,
                        ));
                        (None, Some(error))
                    }
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
                lsp::notification::Cancel::METHOD => {
                    let params: lsp::CancelParams = serde_json::from_value(notif.params)?;
                    self.context.logger().info(format!(
                        "Cancellation received id={:?}; synchronous request handling may have already sent its response (receipt order does not establish when the client cancelled)",
                        params.id,
                    ));
                }
                lsp::notification::DidSaveTextDocument::METHOD => {
                    let lsp::DidSaveTextDocumentParams {
                        text_document: doc, ..
                    } = serde_json::from_value(notif.params)?;
                    self.context.logger().info(format!(
                        "Document save uri={} version={:?}",
                        doc.uri.as_str(),
                        self.buffers.get(&doc.uri).and_then(Buffer::version),
                    ));
                    self.server.check(path_from_uri(&doc.uri)?, &self.context)?;
                }
                lsp::notification::DidOpenTextDocument::METHOD => {
                    let lsp::DidOpenTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    let uri = doc.uri;
                    self.context.logger().info(format!(
                        "Document open uri={} version={}",
                        uri.as_str(),
                        doc.version,
                    ));
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
                    self.context.logger().info(format!(
                        "Document close uri={} version={:?}",
                        doc.uri.as_str(),
                        self.buffers.get(&doc.uri).and_then(Buffer::version),
                    ));
                    self.buffers.remove(&doc.uri);
                    self.server
                        .document_changed(path_from_uri(&doc.uri)?, None, &self.context)?;
                }
                lsp::notification::DidChangeTextDocument::METHOD => {
                    if let Some(uri) = self.apply_document_change(notif.params)? {
                        self.server.document_changed(
                            path_from_uri(&uri)?,
                            self.buffers.get(&uri),
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

    fn apply_document_change(&mut self, params: Value) -> anyhow::Result<Option<lsp::Uri>> {
        let lsp::DidChangeTextDocumentParams {
            text_document: doc,
            content_changes,
        } = serde_json::from_value(params)?;
        let current_version = self.buffers.get(&doc.uri).and_then(Buffer::version);
        if self.buffers.change(&doc.uri, doc.version, content_changes) {
            self.context.logger().info(format!(
                "Document change uri={} version={}",
                doc.uri.as_str(),
                doc.version,
            ));
            Ok(Some(doc.uri))
        } else {
            self.context.logger().info(format!(
                "Document change ignored ({}) uri={} version={} current_version={current_version:?}",
                if current_version.is_some_and(|version| version >= doc.version) {
                    "stale"
                } else {
                    "not open"
                },
                doc.uri.as_str(),
                doc.version,
            ));
            Ok(None)
        }
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

fn log_initialization(context: &LspContext, params: &lsp::InitializeParams) {
    context.logger().info(format!(
        "Initializing {} version={} client={:?} workspace_folders={:?}",
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
        params.client_info,
        params.workspace_folders.as_ref().map(|folders| folders
            .iter()
            .map(|folder| folder.uri.as_str())
            .collect::<Vec<_>>()),
    ));
}

fn response_summary(method: &str, value: &Value) -> String {
    if method == lsp::request::Completion::METHOD {
        let count = value
            .as_array()
            .or_else(|| value.get("items").and_then(Value::as_array))
            .map_or(0, Vec::len);
        format!("completion_count={count}")
    } else {
        let empty = match value.get("contents") {
            None | Some(Value::Null) => true,
            Some(Value::String(text)) => text.is_empty(),
            Some(Value::Array(items)) => items.is_empty(),
            Some(contents) => contents
                .get("value")
                .and_then(Value::as_str)
                .is_some_and(str::is_empty),
        };
        format!("hover={}", if empty { "empty" } else { "nonempty" })
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
    fn document_symbols(
        &self,
        doc: Document<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<lsp::DocumentSymbolResponse>;
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
        context: lsp::CodeActionContext,
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
    code_action_literals: bool,
    code_action_data: bool,
    code_action_resolve_edit: bool,
}

impl LspContext {
    pub(crate) fn new(sender: Sender<Message>) -> Self {
        Self {
            sender,
            diagnostic_tags: vec![],
            code_action_literals: false,
            code_action_data: false,
            code_action_resolve_edit: false,
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

    pub(crate) fn set_code_action_capabilities(&mut self, capabilities: &lsp::ClientCapabilities) {
        let support = capabilities
            .text_document
            .as_ref()
            .and_then(|doc| doc.code_action.as_ref());
        self.code_action_literals =
            support.is_some_and(|s| s.code_action_literal_support.is_some());
        self.code_action_data = support.and_then(|s| s.data_support).unwrap_or(false);
        self.code_action_resolve_edit = support
            .and_then(|s| s.resolve_support.as_ref())
            .is_some_and(|s| s.properties.iter().any(|property| property == "edit"));
    }

    pub fn supports_code_action_resolve_edit(&self) -> bool {
        self.code_action_literals && self.code_action_data && self.code_action_resolve_edit
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
        document_symbol_provider: Some(lsp::OneOf::Left(true)),
        document_formatting_provider: Some(lsp::OneOf::Left(true)),
        code_action_provider: Some(lsp::CodeActionProviderCapability::Options(
            lsp::CodeActionOptions {
                code_action_kinds: Some(vec![lsp::CodeActionKind::QUICKFIX]),
                resolve_provider: Some(true),
                ..Default::default()
            },
        )),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;
    use std::time::Duration;

    use super::*;

    #[derive(Default)]
    struct StubServer {
        hover: Option<lsp::Hover>,
        completion: Option<lsp::CompletionResponse>,
        allow_document_changes: bool,
        events: Option<Rc<RefCell<Vec<String>>>>,
    }

    impl LanguageServer for StubServer {
        fn watch_roots(&self) -> Vec<PathBuf> {
            if let Some(events) = &self.events {
                events.borrow_mut().push("watchers".into());
            }
            vec![]
        }

        fn document_changed(
            &mut self,
            _path: PathBuf,
            buffer: Option<&Buffer>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            assert!(self.allow_document_changes, "unexpected document change");
            if let Some(events) = &self.events {
                let buffer = buffer.unwrap();
                events.borrow_mut().push(format!(
                    "change:{}:{:?}",
                    buffer.contents(),
                    buffer.version(),
                ));
            }
            Ok(())
        }

        fn files_changed(&mut self, _paths: Vec<PathBuf>, _ctx: &LspContext) -> anyhow::Result<()> {
            panic!("unexpected file change");
        }

        fn check(&mut self, _path: PathBuf, _ctx: &LspContext) -> anyhow::Result<()> {
            assert!(self.allow_document_changes, "unexpected check");
            if let Some(events) = &self.events {
                events.borrow_mut().push("save".into());
            }
            Ok(())
        }

        fn change_workspace_folders(
            &mut self,
            _added: Vec<PathBuf>,
            _removed: Vec<PathBuf>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            panic!("unexpected workspace change");
        }

        fn hover(&self, doc: CodeLocation<'_>, _ctx: &LspContext) -> anyhow::Result<lsp::Hover> {
            if let Some(events) = &self.events {
                events
                    .borrow_mut()
                    .push(format!("hover:{}", doc.doc().buffer().contents()));
            }
            self.hover.clone().context("stub handler failed")
        }

        fn completion(
            &self,
            _doc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<Rc<lsp::CompletionResponse>> {
            Ok(Rc::new(
                self.completion
                    .clone()
                    .unwrap_or_else(|| lsp::CompletionResponse::Array(vec![])),
            ))
        }

        fn goto_definition(
            &self,
            _doc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
            Ok(lsp::GotoDefinitionResponse::Array(vec![]))
        }

        fn document_symbols(
            &self,
            _doc: Document<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::DocumentSymbolResponse> {
            Ok(lsp::DocumentSymbolResponse::Nested(vec![]))
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
            span: Range<u32>,
            context: lsp::CodeActionContext,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::CodeActionResponse> {
            if let Some(events) = &self.events {
                events.borrow_mut().push(format!(
                    "code_action:{span:?}:{}",
                    serde_json::to_string(&context)?
                ));
            }
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
                server: Box::<StubServer>::default(),
                buffers,
                context,
                watch_files: false,
                watched_roots: vec![],
                shutdown: false,
            },
            client,
        )
    }

    #[test]
    fn code_action_dispatch_preserves_context_and_converts_utf16_range() {
        let (mut server, _client) = memory_server();
        let events = Rc::new(RefCell::new(vec![]));
        server.server = Box::new(StubServer {
            events: Some(events.clone()),
            ..Default::default()
        });
        let context = json!({
            "diagnostics": [{"range": {"start": {"line": 0, "character": 0}, "end": {"line": 0, "character": 3}}, "message": "test"}],
            "only": ["quickfix"],
            "triggerKind": 1
        });
        let result = server.dispatch(lsp::request::CodeActionRequest::METHOD, json!({
            "textDocument": {"uri": "file:///C:/test.reds"},
            "range": {"start": {"line": 0, "character": 1}, "end": {"line": 0, "character": 3}},
            "context": context
        })).unwrap();
        assert_eq!(result, json!([]));
        let expected: lsp::CodeActionContext = serde_json::from_value(context).unwrap();
        assert_eq!(
            &*events.borrow(),
            &[format!(
                "code_action:2..6:{}",
                serde_json::to_string(&expected).unwrap()
            )]
        );
        let capabilities = serde_json::to_value(capabilities()).unwrap();
        assert_eq!(
            capabilities["codeActionProvider"]["codeActionKinds"],
            json!(["quickfix"])
        );
        assert_eq!(capabilities["codeActionProvider"]["resolveProvider"], true);
    }

    #[test]
    fn code_action_lazy_edits_require_all_client_capabilities() {
        let (sender, _) = crossbeam_channel::unbounded();
        let mut ctx = LspContext::new(sender);
        assert!(!ctx.supports_code_action_resolve_edit());
        for literals in [false, true] {
            for data in [false, true] {
                for edit in [false, true] {
                    let mut support = json!({"dataSupport": data, "resolveSupport": {"properties": if edit { vec!["edit"] } else { vec!["command"] }}});
                    if literals {
                        support["codeActionLiteralSupport"] =
                            json!({"codeActionKind": {"valueSet": ["quickfix"]}});
                    }
                    ctx.set_code_action_capabilities(
                        &serde_json::from_value(json!({"textDocument": {"codeAction": support}}))
                            .unwrap(),
                    );
                    assert_eq!(
                        ctx.supports_code_action_resolve_edit(),
                        literals && data && edit
                    );
                }
            }
        }
        ctx.set_code_action_capabilities(&lsp::ClientCapabilities::default());
        assert!(!ctx.supports_code_action_resolve_edit());
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
        let response = loop {
            match client
                .receiver
                .recv_timeout(Duration::from_secs(2))
                .unwrap()
            {
                Message::Response(response) => break response,
                Message::Notification(notification) => {
                    assert_eq!(notification.method, lsp::notification::LogMessage::METHOD);
                }
                message => panic!("expected response or log, got {message:?}"),
            }
        };
        assert_eq!(response.id, RequestId::from(id));
        assert_ne!(response.result.is_some(), response.error.is_some());
        response
    }

    fn receive_log(client: &Connection, typ: lsp::MessageType) -> String {
        let Message::Notification(notification) = client
            .receiver
            .recv_timeout(Duration::from_secs(2))
            .unwrap()
        else {
            panic!("expected log notification");
        };
        assert_eq!(notification.method, lsp::notification::LogMessage::METHOD);
        let params: lsp::LogMessageParams = serde_json::from_value(notification.params).unwrap();
        assert_eq!(params.typ, typ);
        params.message
    }

    fn record_changes(server: &mut LspServer<'_>) -> Rc<RefCell<Vec<String>>> {
        let events = Rc::new(RefCell::new(Vec::new()));
        server.server = Box::new(StubServer {
            allow_document_changes: true,
            events: Some(events.clone()),
            ..Default::default()
        });
        events
    }

    fn queue_change(client: &Connection, uri: &str, version: i32, changes: Value) {
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeTextDocument::METHOD.into(),
                json!({
                    "textDocument": { "uri": uri, "version": version },
                    "contentChanges": changes,
                }),
            )))
            .unwrap();
    }

    fn queue_shutdown(client: &Connection) {
        send_request(client, 99, lsp::request::Shutdown::METHOD, Value::Null);
        send_exit(client);
    }

    #[test]
    fn queued_changes_apply_incrementally_and_rebuild_once_per_uri() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        server.watch_files = true;
        let uri = "file:///C:/test.reds";
        let other = "file:///C:/other.reds";
        server
            .buffers
            .open(other.parse().unwrap(), "other".into(), 1);
        queue_change(
            &client,
            uri,
            2,
            json!([{
                "range": { "start": { "line": 0, "character": 1 }, "end": { "line": 0, "character": 3 } },
                "text": "AB"
            }]),
        );
        queue_change(&client, other, 2, json!([{ "text": "second" }]));
        queue_change(
            &client,
            uri,
            3,
            json!([
                {
                    "range": { "start": { "line": 0, "character": 2 }, "end": { "line": 0, "character": 4 } },
                    "text": "C"
                },
                {
                    "range": { "start": { "line": 0, "character": 3 }, "end": { "line": 0, "character": 3 } },
                    "text": "!"
                }
            ]),
        );
        queue_shutdown(&client);

        server.run().unwrap();

        assert_eq!(
            *events.borrow(),
            [
                "change:éAC!est:Some(3)",
                "watchers",
                "change:second:Some(2)",
                "watchers",
            ]
        );
        for version in [2, 2, 3] {
            let log = receive_log(&client, lsp::MessageType::INFO);
            assert!(log.starts_with("Document change uri="), "{log}");
            assert!(log.contains(&format!("version={version}")), "{log}");
        }
    }

    #[test]
    fn queued_changes_do_not_cross_requests_or_other_notifications() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let uri = "file:///C:/test.reds";
        queue_change(&client, uri, 2, json!([{ "text": "before request" }]));
        send_request(
            &client,
            1,
            lsp::request::HoverRequest::METHOD,
            json!({
                "textDocument": { "uri": uri },
                "position": { "line": 0, "character": 0 }
            }),
        );
        queue_change(&client, uri, 3, json!([{ "text": "before save" }]));
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidSaveTextDocument::METHOD.into(),
                json!({ "textDocument": { "uri": uri } }),
            )))
            .unwrap();
        queue_change(&client, uri, 4, json!([{ "text": "after save" }]));
        queue_shutdown(&client);

        server.run().unwrap();

        assert_eq!(
            *events.borrow(),
            [
                "change:before request:Some(2)",
                "hover:before request",
                "change:before save:Some(3)",
                "save",
                "change:after save:Some(4)",
            ]
        );
        receive_response(&client, 1);
        receive_response(&client, 99);
    }

    #[test]
    fn queued_changes_validate_each_version_and_preserve_ignored_logs() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let uri = "file:///C:/test.reds";
        queue_change(&client, uri, 3, json!([{ "text": "accepted" }]));
        queue_change(&client, uri, 2, json!([{ "text": "stale" }]));
        queue_change(&client, uri, 3, json!([{ "text": "equal" }]));
        queue_change(
            &client,
            uri,
            4,
            json!([{
                "range": { "start": { "line": 0, "character": 8 }, "end": { "line": 0, "character": 8 } },
                "text": "!"
            }]),
        );
        queue_change(
            &client,
            "file:///C:/closed.reds",
            5,
            json!([{ "text": "closed" }]),
        );
        queue_shutdown(&client);

        server.run().unwrap();

        assert_eq!(*events.borrow(), ["change:accepted!:Some(4)"]);
        let buffer = server.buffers.get(&uri.parse().unwrap()).unwrap();
        assert_eq!(buffer.contents().to_string(), "accepted!");
        assert_eq!(buffer.version(), Some(4));
        for expected in [
            "Document change uri=",
            "Document change ignored (stale)",
            "Document change ignored (stale)",
            "Document change uri=",
            "Document change ignored (not open)",
        ] {
            let log = receive_log(&client, lsp::MessageType::INFO);
            assert!(log.starts_with(expected), "{log}");
            if expected.ends_with("(stale)") {
                assert!(log.contains("current_version=Some(3)"), "{log}");
            }
        }
    }

    #[test]
    fn queued_changes_flush_on_disconnect() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        queue_change(
            &client,
            "file:///C:/test.reds",
            2,
            json!([{ "text": "final" }]),
        );
        // Keep the log receiver alive while disconnecting the incoming channel.
        drop(client.sender);
        server.run().unwrap();
        assert_eq!(*events.borrow(), ["change:final:Some(2)"]);
    }

    #[test]
    fn initialization_logs_client_workspace_and_version() {
        let (server, client) = memory_server();
        let params = serde_json::from_value(json!({
            "capabilities": {},
            "clientInfo": { "name": "test-client", "version": "1.2" },
            "workspaceFolders": [{ "uri": "file:///C:/workspace", "name": "workspace" }],
            "initializationOptions": { "private": "DO_NOT_LOG" }
        }))
        .unwrap();
        log_initialization(&server.context, &params);
        let log = receive_log(&client, lsp::MessageType::INFO);
        for expected in [
            env!("CARGO_PKG_NAME"),
            env!("CARGO_PKG_VERSION"),
            "test-client",
            "1.2",
            "file:///C:/workspace",
        ] {
            assert!(log.contains(expected), "{log}");
        }
        assert!(!log.contains("DO_NOT_LOG"));
    }

    #[test]
    fn hover_and_completion_logs_metadata_and_summary_only() {
        let (mut server, client) = memory_server();
        for (id, method, hover_contents, completion, expected) in [
            (
                0,
                lsp::request::HoverRequest::METHOD,
                json!(""),
                json!([]),
                "hover=empty",
            ),
            (
                1,
                lsp::request::HoverRequest::METHOD,
                json!({ "kind": "markdown", "value": "PRIVATE_HOVER" }),
                json!([]),
                "hover=nonempty",
            ),
            (
                2,
                lsp::request::Completion::METHOD,
                json!(""),
                json!([{ "label": "PRIVATE_COMPLETION" }]),
                "completion_count=1",
            ),
            (
                3,
                lsp::request::Completion::METHOD,
                json!(""),
                json!({ "isIncomplete": false, "items": [] }),
                "completion_count=0",
            ),
        ] {
            server.server = Box::new(StubServer {
                hover: Some(serde_json::from_value(json!({ "contents": hover_contents })).unwrap()),
                completion: Some(serde_json::from_value(completion).unwrap()),
                ..Default::default()
            });
            server
                .handle(Message::Request(lsp_server::Request::new(
                    id.into(),
                    method.into(),
                    json!({
                        "textDocument": { "uri": "file:///C:/test.reds" },
                        "position": { "line": 0, "character": 3 },
                        "context": { "triggerKind": 2, "triggerCharacter": "." },
                        "private": "PRIVATE_PARAMS"
                    }),
                )))
                .unwrap();
            let incoming = receive_log(&client, lsp::MessageType::INFO);
            for expected in [
                format!("id={id}"),
                format!("method={method}"),
                "uri=file:///C:/test.reds".into(),
                "line=Some(0)".into(),
                "character=Some(3)".into(),
            ] {
                assert!(incoming.contains(&expected), "{incoming}");
            }
            assert!(!incoming.contains("PRIVATE"));
            if method == lsp::request::Completion::METHOD {
                let context = receive_log(&client, lsp::MessageType::INFO);
                assert!(context.contains("trigger_kind=Some(2)"));
                assert!(context.contains("trigger_character=Some(\".\")"));
            }
            let summary = receive_log(&client, lsp::MessageType::INFO);
            for expected in [
                expected,
                "elapsed_ms=",
                &format!("id={id}"),
                &format!("method={method}"),
            ] {
                assert!(summary.contains(expected), "{summary}");
            }
            assert!(!summary.contains("PRIVATE"));
            receive_response(&client, id);
            assert!(client.receiver.try_recv().is_err());
        }
    }

    #[test]
    fn cancellation_logs_request_id_without_an_unexpected_notification() {
        let (mut server, client) = memory_server();
        server
            .handle(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 42 }),
            )))
            .unwrap();
        let log = receive_log(&client, lsp::MessageType::INFO);
        assert!(log.contains("Cancellation received id=Number(42)"), "{log}");
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn document_logs_versions_and_ignored_changes_without_source() {
        let (mut server, client) = memory_server();
        server.server = Box::new(StubServer {
            allow_document_changes: true,
            ..Default::default()
        });
        let uri = "file:///C:/document.reds";
        for (method, params, expected) in [
            (
                lsp::notification::DidOpenTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "languageId": "redscript", "version": 3, "text": "PRIVATE_SOURCE" } }),
                "Document open",
            ),
            (
                lsp::notification::DidChangeTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "version": 4 }, "contentChanges": [{ "text": "PRIVATE_CHANGE" }] }),
                "Document change",
            ),
            (
                lsp::notification::DidChangeTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "version": 3 }, "contentChanges": [{ "text": "PRIVATE_STALE" }] }),
                "Document change ignored (stale)",
            ),
            (
                lsp::notification::DidChangeTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "version": 4 }, "contentChanges": [{ "text": "PRIVATE_EQUAL" }] }),
                "Document change ignored (stale)",
            ),
            (
                lsp::notification::DidSaveTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri }, "text": "PRIVATE_SAVE" }),
                "Document save",
            ),
            (
                lsp::notification::DidCloseTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri } }),
                "Document close",
            ),
            (
                lsp::notification::DidChangeTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "version": 5 }, "contentChanges": [{ "text": "PRIVATE_CLOSED" }] }),
                "Document change ignored (not open)",
            ),
        ] {
            server
                .handle(Message::Notification(lsp_server::Notification::new(
                    method.into(),
                    params.clone(),
                )))
                .unwrap();
            let log = receive_log(&client, lsp::MessageType::INFO);
            assert!(log.starts_with(expected), "{log}");
            assert!(log.contains(uri));
            if let Some(version) = params
                .pointer("/textDocument/version")
                .and_then(Value::as_i64)
            {
                assert!(log.contains(&format!("version={version}")), "{log}");
            } else {
                assert!(log.contains("version=Some(4)"), "{log}");
            }
            if expected.ends_with("(stale)") {
                assert!(log.contains("current_version=Some(4)"));
                assert_eq!(
                    server
                        .buffers
                        .get(&uri.parse().unwrap())
                        .unwrap()
                        .contents()
                        .to_string(),
                    "PRIVATE_CHANGE"
                );
            }
            assert!(!log.contains("PRIVATE"));
        }
        assert!(client.receiver.try_recv().is_err());
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

        for (id, (method, _, code)) in cases.into_iter().enumerate() {
            if method == lsp::request::HoverRequest::METHOD
                || method == lsp::request::Completion::METHOD
            {
                receive_log(&client, lsp::MessageType::INFO);
                if method == lsp::request::Completion::METHOD {
                    receive_log(&client, lsp::MessageType::INFO);
                }
            }
            let log = receive_log(&client, lsp::MessageType::ERROR);
            let error = receive_response(&client, id as i32).error.unwrap();
            for expected in [
                format!("method={method}"),
                format!("id={id}"),
                format!("code={}", error.code),
                format!("message={}", error.message),
            ] {
                assert!(log.contains(&expected), "{log}");
            }
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
            if id == 2 {
                receive_log(&client, lsp::MessageType::INFO);
            }
            let log = receive_log(&client, lsp::MessageType::ERROR);
            assert!(log.contains(&format!("id={id}")));
            assert!(log.contains("code=-32600 message=Server is shut down"));
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
