use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::Display;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, AtomicU64, Ordering};
use std::time::Instant;

use anyhow::{Context, bail};
use crossbeam_channel::{Receiver, Sender};
use lsp_server::{Connection, ErrorCode, Message, RequestId, Response, ResponseError};
use lsp_types::notification::Notification;
use lsp_types::request::Request;
use lsp_types::{self as lsp};
use serde::Serialize;
use serde_json::{Value, json};

use crate::buffers::{Buffer, Buffers, ChangeError};
use crate::diagnostics::{DiagnosticBatch, DiagnosticTracker};

/// A shared cancellation flag for a single in-flight request.
///
/// The protocol thread flips it when a `$/cancelRequest` arrives, and the
/// analysis worker observes it at cooperative checkpoints. Cancellation is
/// best-effort: work already inside upstream `redscript` cannot be interrupted,
/// but a cancelled request's result is discarded and never reaches the client.
#[derive(Debug, Clone, Default)]
pub struct CancelToken(Arc<AtomicBool>);

impl CancelToken {
    pub fn cancel(&self) {
        self.0.store(true, Ordering::SeqCst);
    }

    pub fn is_cancelled(&self) -> bool {
        self.0.load(Ordering::SeqCst)
    }
}

thread_local! {
    /// Cancellation token for the request the analysis worker is currently
    /// executing. It lives on the worker thread so query helpers in `ls.rs` can
    /// check cancellation without threading a parameter through every call.
    static CURRENT_CANCEL: RefCell<Option<CancelToken>> = const { RefCell::new(None) };
}

/// Installs `token` as the current worker cancellation token.
pub(crate) fn set_current_cancel(token: CancelToken) {
    CURRENT_CANCEL.with(|current| *current.borrow_mut() = Some(token));
}

/// Clears the current worker cancellation token.
pub(crate) fn clear_current_cancel() {
    CURRENT_CANCEL.with(|current| *current.borrow_mut() = None);
}

/// Whether the analysis worker's current request has been cancelled.
pub(crate) fn is_cancelled() -> bool {
    CURRENT_CANCEL.with(|current| {
        current
            .borrow()
            .as_ref()
            .is_some_and(CancelToken::is_cancelled)
    })
}

/// Owned snapshot of a document, sent to the analysis worker because the
/// protocol thread keeps exclusive ownership of [`Buffers`]. Cloning a
/// [`Buffer`] is cheap (a `ropey::Rope` is reference counted).
#[derive(Clone)]
pub struct OwnedDocument {
    path: PathBuf,
    buffer: Buffer,
}

impl OwnedDocument {
    fn path(&self) -> &Path {
        &self.path
    }

    fn as_doc(&self) -> Document<'_> {
        Document::new(self.path.clone(), &self.buffer)
    }
}

/// Owned snapshot of a document plus a byte offset, the worker-side equivalent
/// of [`CodeLocation`].
#[derive(Clone)]
pub struct OwnedCodeLocation {
    doc: OwnedDocument,
    pos: u32,
}

impl OwnedCodeLocation {
    fn as_loc(&self) -> CodeLocation<'_> {
        CodeLocation::new(self.doc.as_doc(), self.pos)
    }
}

/// A request routed to the analysis worker.
enum WorkerRequest {
    Hover(OwnedCodeLocation),
    Completion(OwnedCodeLocation),
    GotoDefinition(OwnedCodeLocation),
    DocumentSymbols(OwnedDocument),
    WorkspaceSymbol(String),
    Formatting {
        doc: OwnedDocument,
        tab_size: u16,
    },
    CodeAction {
        path: PathBuf,
        span: Range<u32>,
        context: lsp::CodeActionContext,
    },
    CodeActionResolve(Box<lsp::CodeAction>),
    CompletionResolve(Box<lsp::CompletionItem>),
}

/// Commands sent from the protocol thread to the analysis worker. The worker
/// owns the [`LanguageServer`] exclusively, so every state change and query
/// travels through this channel in protocol order.
enum WorkerCmd {
    DocumentsChanged {
        documents: Vec<(PathBuf, Option<Buffer>)>,
    },
    FilesChanged {
        roots_rev: u64,
        paths: Vec<PathBuf>,
    },
    ChangeWorkspaceFolders {
        roots_rev: u64,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
    },
    Check {
        state_rev: u64,
        diag_rev: u64,
        roots_rev: u64,
        path: PathBuf,
    },
    StartupDiagnostics {
        state_rev: u64,
        diag_rev: u64,
    },
    Request {
        id: RequestId,
        method: String,
        request: Box<WorkerRequest>,
        started: Instant,
        cancel: CancelToken,
    },
    /// Barrier used on shutdown so every command queued before it completes in
    /// order; the worker acknowledges once it is reached.
    Flush(Sender<()>),
}

/// Events sent from the analysis worker back to the protocol thread.
enum WorkerEvent {
    Response {
        id: RequestId,
        method: String,
        started: Instant,
        result: Result<Value, ResponseError>,
    },
    Diagnostics {
        state_rev: u64,
        diag_rev: u64,
        is_startup: bool,
        /// `None` when computation was aborted before producing a batch, so the
        /// protocol thread can decide whether to retry.
        batch: Option<DiagnosticBatch>,
    },
    WatchRoots {
        roots_rev: u64,
        roots: Vec<PathBuf>,
    },
    Error(String),
}

/// Placeholder [`LanguageServer`] left in `LspServer` while the real one runs on
/// the worker thread. The protocol loop never calls it; only synchronous test
/// helpers that run without a worker could.
#[derive(Default)]
struct NoopServer;

impl LanguageServer for NoopServer {
    fn watch_roots(&self) -> Vec<PathBuf> {
        Vec::new()
    }

    fn document_changed(
        &mut self,
        _path: PathBuf,
        _buffer: Option<&Buffer>,
        _ctx: &LspContext,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    fn files_changed(&mut self, _paths: Vec<PathBuf>, _ctx: &LspContext) -> anyhow::Result<()> {
        Ok(())
    }

    fn check(
        &mut self,
        _path: PathBuf,
        _ctx: &LspContext,
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        Ok(None)
    }

    fn change_workspace_folders(
        &mut self,
        _added: Vec<PathBuf>,
        _removed: Vec<PathBuf>,
        _ctx: &LspContext,
    ) -> anyhow::Result<()> {
        Ok(())
    }

    fn hover(
        &self,
        _loc: CodeLocation<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<Option<lsp::Hover>> {
        Ok(None)
    }

    fn completion(
        &self,
        _loc: CodeLocation<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp::CompletionResponse>> {
        Ok(Arc::new(lsp::CompletionResponse::Array(Vec::new())))
    }

    fn goto_definition(
        &self,
        _loc: CodeLocation<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
        Ok(lsp::GotoDefinitionResponse::Array(Vec::new()))
    }

    fn document_symbols(
        &self,
        _doc: Document<'_>,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::DocumentSymbolResponse> {
        Ok(lsp::DocumentSymbolResponse::Nested(Vec::new()))
    }

    fn workspace_symbol(
        &self,
        _query: &str,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::WorkspaceSymbolResponse> {
        Ok(lsp::WorkspaceSymbolResponse::Nested(Vec::new()))
    }

    fn format(
        &self,
        _doc: Document<'_>,
        _tab_size: u16,
        _ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp::TextEdit>> {
        Ok(Vec::new())
    }

    fn code_action(
        &self,
        _path: PathBuf,
        _span: Range<u32>,
        _context: lsp::CodeActionContext,
        _ctx: &LspContext,
    ) -> anyhow::Result<Vec<lsp::CodeActionOrCommand>> {
        Ok(Vec::new())
    }

    fn code_action_resolve(
        &self,
        _action: lsp::CodeAction,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::CodeAction> {
        bail!("no code actions")
    }
}

pub struct LspServer<'a> {
    connection: Connection,
    server: Box<dyn LanguageServer + 'a>,
    buffers: Buffers,
    context: LspContext,
    dynamic_watch_files: bool,
    watched_roots: Vec<PathBuf>,
    shutdown: bool,
    /// Requests dispatched to the worker that have not yet produced a response,
    /// keyed by id so `$/cancelRequest` can find and flag them.
    in_flight: HashMap<RequestId, CancelToken>,
    /// Monotonic revision of the document/workspace state. Diagnostics computed
    /// for an older revision are rejected before they reach the client.
    state_rev: u64,
    /// Monotonic revision of the watch-root state, tracked separately from
    /// `state_rev`. Document edits bump `state_rev` without changing roots, so a
    /// `WatchRoots` result must be validated against this revision instead; a
    /// root-changing command only supersedes an earlier one when its own
    /// `WatchRoots` event arrives later.
    roots_rev: u64,
    /// Monotonic revision of save-triggered checks, independent of `state_rev`
    /// so back-to-back saves only publish the latest batch.
    diag_rev: u64,
    /// The most recent saved file whose check has not yet been published. A
    /// save check can be superseded by a non-edit state change (for example a
    /// `.redscript` or dependency watcher event) while it is in flight; unlike
    /// startup diagnostics it has no other trigger, so the protocol thread
    /// retries it at the current revision. Cleared on a successful publication
    /// and on any unsaved edit, which must keep the in-flight check rejected.
    pending_check: Option<PathBuf>,
    latest_state_rev: Arc<AtomicU64>,
    latest_diag_rev: Arc<AtomicU64>,
    /// Authoritative diagnostic publication state, owned by the protocol
    /// thread while the worker computes batches.
    diagnostics: DiagnosticTracker,
    /// Whether the initial workspace diagnostics still need to be computed.
    startup_pending: bool,
    /// Set once an edit or save supersedes the initial workspace diagnostics, so
    /// they are not recomputed after a launch-time document open.
    startup_edited: bool,
}

impl LspServer<'_> {
    pub fn spawn<P>(
        prepare_init: impl Fn(Options, &LspContext) -> anyhow::Result<P>,
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
        context.set_presentation_capabilities(&init_params.capabilities);
        log_initialization(&context, &init_params);
        let dynamic_watch_files = init_params
            .capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.did_change_watched_files.as_ref())
            .and_then(|watch| watch.dynamic_registration)
            .unwrap_or(false);
        let workspace_folders =
            workspace_folder_paths(init_params.workspace_folders.unwrap_or_default(), &context)?;
        let options = Options::new(workspace_folders, init_params.initialization_options);
        let init_state = prepare_init(options, &context).inspect_err(|err| {
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

        let server = create_server(&init_state, &context).inspect_err(|err| {
            let message = format!("Language server startup failed: {err:#}");
            context.logger().error(&message);
            context.show_message(lsp::MessageType::ERROR, message);
        })?;
        let mut this = LspServer {
            connection,
            server,
            context: context.clone(),
            buffers: Buffers::default(),
            dynamic_watch_files,
            watched_roots: vec![],
            shutdown: false,
            in_flight: HashMap::new(),
            state_rev: 0,
            roots_rev: 0,
            diag_rev: 0,
            pending_check: None,
            latest_state_rev: Arc::new(AtomicU64::new(0)),
            latest_diag_rev: Arc::new(AtomicU64::new(0)),
            diagnostics: DiagnosticTracker::default(),
            startup_pending: true,
            startup_edited: false,
        };
        this.update_watchers()?;

        this.run()
    }

    /// Builds a protocol server over an in-memory connection with a caller
    /// supplied language server, so tests can drive the real protocol loop and
    /// worker thread against actual analysis.
    #[cfg(test)]
    pub(crate) fn for_test(
        server: Box<dyn LanguageServer + 'static>,
        buffers: Buffers,
        dynamic_watch_files: bool,
    ) -> (LspServer<'static>, Connection) {
        let (connection, client) = Connection::memory();
        let context = LspContext::new(connection.sender.clone());
        (
            LspServer {
                connection,
                server,
                buffers,
                context,
                dynamic_watch_files,
                watched_roots: vec![],
                shutdown: false,
                in_flight: HashMap::new(),
                state_rev: 0,
                roots_rev: 0,
                diag_rev: 0,
                pending_check: None,
                latest_state_rev: Arc::new(AtomicU64::new(0)),
                latest_diag_rev: Arc::new(AtomicU64::new(0)),
                diagnostics: DiagnosticTracker::default(),
                startup_pending: false,
                startup_edited: false,
            },
            client,
        )
    }

    fn run(&mut self) -> anyhow::Result<()> {
        let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded::<WorkerCmd>();
        let (event_tx, event_rx) = crossbeam_channel::unbounded::<WorkerEvent>();
        // The worker owns the real server; the protocol thread keeps this
        // placeholder and never calls it.
        let server = std::mem::replace(&mut self.server, Box::new(NoopServer));
        let context = self.context.clone();
        let latest_state_rev = self.latest_state_rev.clone();
        let latest_diag_rev = self.latest_diag_rev.clone();
        let dynamic_watch_files = self.dynamic_watch_files;

        std::thread::scope(|scope| {
            scope.spawn(move || {
                worker_main(
                    server,
                    context,
                    cmd_rx,
                    event_tx,
                    latest_state_rev,
                    latest_diag_rev,
                    dynamic_watch_files,
                );
            });
            self.protocol_loop(cmd_tx, event_rx)
        })
    }

    /// Protocol thread: never blocks on analysis. It reads client messages and
    /// worker events, forwards work to the worker, and rejects results computed
    /// for a state that has since been superseded.
    fn protocol_loop(
        &mut self,
        cmd_tx: Sender<WorkerCmd>,
        event_rx: Receiver<WorkerEvent>,
    ) -> anyhow::Result<()> {
        let connection_rx = self.connection.receiver.clone();
        let mut next_message: Option<Message> = None;

        if self.startup_pending {
            cmd_tx
                .send(WorkerCmd::StartupDiagnostics {
                    state_rev: 0,
                    diag_rev: 0,
                })
                .ok();
        }

        loop {
            let message = if let Some(message) = next_message.take() {
                message
            } else {
                enum Incoming {
                    Message(Message),
                    Event(WorkerEvent),
                    Disconnected,
                }
                let incoming = crossbeam_channel::select! {
                    recv(connection_rx) -> message => match message {
                        Ok(message) => Incoming::Message(message),
                        Err(_) => Incoming::Disconnected,
                    },
                    recv(event_rx) -> event => match event {
                        Ok(event) => Incoming::Event(event),
                        Err(_) => Incoming::Disconnected,
                    },
                };
                match incoming {
                    Incoming::Message(message) => message,
                    Incoming::Event(event) => {
                        self.handle_worker_event(event, Some(&cmd_tx))?;
                        continue;
                    }
                    // The client went away: returning drops `cmd_tx`, so the
                    // worker finishes its queue and exits before the scope joins.
                    Incoming::Disconnected => return Ok(()),
                }
            };

            match message {
                Message::Request(request) => {
                    if self.handle_protocol_request(request, &cmd_tx, &event_rx)? {
                        return Ok(());
                    }
                }
                Message::Notification(notif) => {
                    if self.handle_protocol_notification(
                        notif,
                        &cmd_tx,
                        &connection_rx,
                        &mut next_message,
                    )? {
                        return Ok(());
                    }
                }
                Message::Response(response) => self.handle_watcher_response(response),
            }
        }
    }

    fn handle_protocol_request(
        &mut self,
        request: lsp_server::Request,
        cmd_tx: &Sender<WorkerCmd>,
        event_rx: &Receiver<WorkerEvent>,
    ) -> anyhow::Result<bool> {
        if self.shutdown {
            // Answer `InvalidRequest` without touching the worker, keeping the
            // same request metadata log the active path emits.
            log_request_metadata(&self.context, &request);
            let error = response_error(ErrorCode::InvalidRequest, "Server is shut down");
            self.context.logger().error(format!(
                "Request failed method={} id={} code={} message={} elapsed_ms={:.3}",
                request.method, request.id, error.code, error.message, 0.0,
            ));
            self.connection.sender.send(Message::Response(Response {
                id: request.id,
                response_result: Err(error),
            }))?;
            return Ok(false);
        }
        if request.method == lsp::request::Shutdown::METHOD {
            // Validate params before acknowledging, so a malformed shutdown
            // still reports `InvalidParams`.
            if let Err(error) = serde_json::from_value::<()>(request.params) {
                let error = invalid_params(error);
                self.context.logger().error(format!(
                    "Request failed method={} id={} code={} message={} elapsed_ms={:.3}",
                    request.method, request.id, error.code, error.message, 0.0,
                ));
                self.connection.sender.send(Message::Response(Response {
                    id: request.id,
                    response_result: Err(error),
                }))?;
                return Ok(false);
            }
            // Barrier: every command queued before the flush completes in order
            // before shutdown is acknowledged.
            let (ack_tx, ack_rx) = crossbeam_channel::bounded(1);
            cmd_tx.send(WorkerCmd::Flush(ack_tx)).ok();
            loop {
                crossbeam_channel::select! {
                    recv(ack_rx) -> ack => {
                        if ack.is_ok() {
                            // The worker emitted every prior event before
                            // acknowledging, so drain them before returning.
                            while let Ok(event) = event_rx.try_recv() {
                                self.handle_worker_event(event, None)?;
                            }
                        }
                        break;
                    }
                    recv(event_rx) -> event => match event {
                        Ok(event) => self.handle_worker_event(event, None)?,
                        Err(_) => break,
                    },
                }
            }
            self.shutdown = true;
            self.connection.sender.send(Message::Response(Response {
                id: request.id,
                response_result: Ok(Value::Null),
            }))?;
            return Ok(false);
        }

        log_request_metadata(&self.context, &request);
        match self.parse_worker_request(&request.method, request.params) {
            Ok(worker_request) => {
                let cancel = CancelToken::default();
                self.in_flight.insert(request.id.clone(), cancel.clone());
                cmd_tx
                    .send(WorkerCmd::Request {
                        id: request.id,
                        method: request.method,
                        request: Box::new(worker_request),
                        started: Instant::now(),
                        cancel,
                    })
                    .ok();
            }
            Err(error) => {
                self.context.logger().error(format!(
                    "Request failed method={} id={} code={} message={} elapsed_ms={:.3}",
                    request.method, request.id, error.code, error.message, 0.0,
                ));
                self.connection.sender.send(Message::Response(Response {
                    id: request.id,
                    response_result: Err(error),
                }))?;
            }
        }
        Ok(false)
    }

    fn handle_protocol_notification(
        &mut self,
        notif: lsp_server::Notification,
        cmd_tx: &Sender<WorkerCmd>,
        connection_rx: &Receiver<Message>,
        next_message: &mut Option<Message>,
    ) -> anyhow::Result<bool> {
        if self.shutdown {
            // Once shut down, only `exit` matters.
            return Ok(notif.method == lsp::notification::Exit::METHOD);
        }
        match notif.method.as_str() {
            lsp::notification::Exit::METHOD => bail!("Received exit before shutdown"),
            lsp::notification::Initialized::METHOD => {}
            lsp::notification::DidChangeTextDocument::METHOD => {
                let mut pending: Vec<lsp::Uri> = Vec::new();
                let mut message = Message::Notification(notif);
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
                    match connection_rx.try_recv() {
                        Ok(Message::Notification(notif))
                            if notif.method == lsp::notification::DidChangeTextDocument::METHOD =>
                        {
                            message = Message::Notification(notif);
                        }
                        other => {
                            *next_message = other.ok();
                            break;
                        }
                    }
                }
                if !pending.is_empty() {
                    self.state_rev += 1;
                    self.latest_state_rev
                        .store(self.state_rev, Ordering::Release);
                    self.startup_edited = true;
                    // An unsaved edit supersedes any pending save check: the
                    // resulting batch must stay rejected until the next save.
                    self.pending_check = None;
                    let documents = self.owned_documents(&pending);
                    cmd_tx.send(WorkerCmd::DocumentsChanged { documents }).ok();
                }
            }
            lsp::notification::DidOpenTextDocument::METHOD => {
                let lsp::DidOpenTextDocumentParams { text_document: doc } =
                    serde_json::from_value(notif.params)?;
                let uri = doc.uri;
                if !is_file_uri(&uri) {
                    self.context.logger().verbose(|| {
                        format!(
                            "Document open ignored (non-file URI) uri={} version={}",
                            uri.as_str(),
                            doc.version,
                        )
                    });
                    return Ok(false);
                }
                let path = path_from_uri(&uri)?;
                self.buffers.open(uri.clone(), doc.text, doc.version);
                self.state_rev += 1;
                self.latest_state_rev
                    .store(self.state_rev, Ordering::Release);
                let documents = vec![(path, self.buffers.get(&uri).cloned())];
                cmd_tx.send(WorkerCmd::DocumentsChanged { documents }).ok();
                // Emit after the state is committed: clients and tests use this
                // log as the point where the open has superseded pending work.
                self.context.logger().verbose(|| {
                    format!("Document open uri={} version={}", uri.as_str(), doc.version,)
                });
            }
            lsp::notification::DidCloseTextDocument::METHOD => {
                let lsp::DidCloseTextDocumentParams { text_document: doc } =
                    serde_json::from_value(notif.params)?;
                if !is_file_uri(&doc.uri) {
                    self.context.logger().verbose(|| {
                        format!(
                            "Document close ignored (non-file URI) uri={}",
                            doc.uri.as_str(),
                        )
                    });
                    return Ok(false);
                }
                let path = path_from_uri(&doc.uri)?;
                let version = self.buffers.get(&doc.uri).and_then(Buffer::version);
                self.buffers.remove(&doc.uri);
                self.state_rev += 1;
                self.latest_state_rev
                    .store(self.state_rev, Ordering::Release);
                cmd_tx
                    .send(WorkerCmd::DocumentsChanged {
                        documents: vec![(path, None)],
                    })
                    .ok();
                // Emit after the state is committed so the log marks the point
                // where the close has superseded pending work.
                self.context.logger().verbose(|| {
                    format!(
                        "Document close uri={} version={:?}",
                        doc.uri.as_str(),
                        version,
                    )
                });
            }
            lsp::notification::DidSaveTextDocument::METHOD => {
                let lsp::DidSaveTextDocumentParams {
                    text_document: doc,
                    text,
                } = serde_json::from_value(notif.params)?;
                if !is_file_uri(&doc.uri) {
                    self.context.logger().verbose(|| {
                        format!(
                            "Document save ignored (non-file URI) uri={}",
                            doc.uri.as_str(),
                        )
                    });
                    return Ok(false);
                }
                // Resolve the path before advancing revisions so a malformed
                // URI cannot request a check that is never queued.
                let path = path_from_uri(&doc.uri)?;
                // A save makes the on-disk content authoritative. Reload a
                // buffer left desynced by a rejected change so diagnostics and
                // queries stop using stale text.
                self.resync_desynced_buffer(&doc.uri, &path, text, cmd_tx);
                let version = self.buffers.get(&doc.uri).and_then(Buffer::version);
                self.diag_rev += 1;
                self.latest_diag_rev.store(self.diag_rev, Ordering::Release);
                self.startup_edited = true;
                self.pending_check = Some(path.clone());
                self.roots_rev += 1;
                cmd_tx
                    .send(WorkerCmd::Check {
                        state_rev: self.state_rev,
                        diag_rev: self.diag_rev,
                        roots_rev: self.roots_rev,
                        path,
                    })
                    .ok();
                // Emit after the revision is committed so the log marks the
                // point where the save has superseded pending work.
                self.context.logger().verbose(|| {
                    format!(
                        "Document save uri={} version={:?}",
                        doc.uri.as_str(),
                        version,
                    )
                });
            }
            lsp::notification::DidChangeWatchedFiles::METHOD => {
                let params: lsp::DidChangeWatchedFilesParams =
                    serde_json::from_value(notif.params)?;
                let paths = params
                    .changes
                    .into_iter()
                    .filter(|event| is_file_uri(&event.uri))
                    .filter(|event| !self.is_redundant_watcher_event(event))
                    .map(|event| path_from_uri(&event.uri))
                    .collect::<anyhow::Result<Vec<_>>>()?;
                if !paths.is_empty() {
                    self.state_rev += 1;
                    self.roots_rev += 1;
                    self.latest_state_rev
                        .store(self.state_rev, Ordering::Release);
                    cmd_tx
                        .send(WorkerCmd::FilesChanged {
                            roots_rev: self.roots_rev,
                            paths,
                        })
                        .ok();
                }
            }
            lsp::notification::DidChangeConfiguration::METHOD => {
                self.state_rev += 1;
                self.roots_rev += 1;
                self.latest_state_rev
                    .store(self.state_rev, Ordering::Release);
                cmd_tx
                    .send(WorkerCmd::FilesChanged {
                        roots_rev: self.roots_rev,
                        paths: vec![],
                    })
                    .ok();
            }
            lsp::notification::DidChangeWorkspaceFolders::METHOD => {
                let lsp::DidChangeWorkspaceFoldersParams { event } =
                    serde_json::from_value(notif.params)?;
                let added = workspace_folder_paths(event.added, &self.context)?;
                let removed = workspace_folder_paths(event.removed, &self.context)?;
                if !added.is_empty() || !removed.is_empty() {
                    self.state_rev += 1;
                    self.roots_rev += 1;
                    self.latest_state_rev
                        .store(self.state_rev, Ordering::Release);
                    cmd_tx
                        .send(WorkerCmd::ChangeWorkspaceFolders {
                            roots_rev: self.roots_rev,
                            added,
                            removed,
                        })
                        .ok();
                }
            }
            lsp::notification::Cancel::METHOD => {
                let params: lsp::CancelParams = serde_json::from_value(notif.params)?;
                let id = match params.id {
                    lsp::NumberOrString::Number(id) => RequestId::from(id),
                    lsp::NumberOrString::String(id) => RequestId::from(id),
                };
                if let Some(token) = self.in_flight.remove(&id) {
                    token.cancel();
                    self.context
                        .logger()
                        .verbose(|| format!("Request cancelled id={id}"));
                    self.connection.sender.send(Message::Response(Response {
                        id,
                        response_result: Err(response_error(
                            ErrorCode::RequestCanceled,
                            "Request cancelled",
                        )),
                    }))?;
                } else {
                    self.context.logger().verbose(|| {
                        format!(
                            "Cancellation received id={id}; request already completed or unknown"
                        )
                    });
                }
            }
            _ => {
                self.context.logger().info(format!(
                    "Received an unexpected notification: {}",
                    notif.method
                ));
            }
        }
        Ok(false)
    }

    /// Reloads a buffer left desynced by a rejected change, because a save makes
    /// the on-disk content authoritative. Removes the buffer when the reload
    /// fails so stale text is never trusted again.
    fn resync_desynced_buffer(
        &mut self,
        uri: &lsp::Uri,
        path: &Path,
        text: Option<String>,
        cmd_tx: &Sender<WorkerCmd>,
    ) {
        if !self.buffers.is_desynced(uri) {
            return;
        }
        let reloaded = match text {
            Some(text) => Ok(text),
            None => std::fs::read_to_string(path),
        };
        let documents = match reloaded {
            Ok(text) => {
                self.buffers.resync_contents(uri, text);
                self.context.logger().verbose(|| {
                    format!(
                        "Document save resynchronized uri={} version={:?}",
                        uri.as_str(),
                        self.buffers.get(uri).and_then(Buffer::version),
                    )
                });
                vec![(path.to_path_buf(), self.buffers.get(uri).cloned())]
            }
            Err(error) => {
                self.context.logger().warn(format!(
                    "Could not reload desynced document from disk uri={} ({error}); dropping the buffer",
                    uri.as_str(),
                ));
                self.buffers.remove(uri);
                vec![(path.to_path_buf(), None)]
            }
        };
        self.state_rev += 1;
        self.latest_state_rev
            .store(self.state_rev, Ordering::Release);
        cmd_tx.send(WorkerCmd::DocumentsChanged { documents }).ok();
    }

    fn handle_worker_event(
        &mut self,
        event: WorkerEvent,
        cmd_tx: Option<&Sender<WorkerCmd>>,
    ) -> anyhow::Result<()> {
        match event {
            WorkerEvent::Response {
                id,
                method,
                started,
                result,
            } => {
                // A missing entry means the request was already cancelled and
                // its `RequestCancelled` response already sent.
                if self.in_flight.remove(&id).is_none() {
                    return Ok(());
                }
                let elapsed = started.elapsed().as_secs_f64() * 1000.0;
                match &result {
                    Ok(value) => {
                        if is_summary_method(&method) {
                            self.context.logger().verbose(|| {
                                format!(
                                    "Response method={} id={} {} elapsed_ms={:.3}",
                                    method,
                                    id,
                                    response_summary(&method, value),
                                    elapsed,
                                )
                            });
                        }
                    }
                    Err(error) => {
                        self.context.logger().error(format!(
                            "Request failed method={} id={} code={} message={} elapsed_ms={:.3}",
                            method, id, error.code, error.message, elapsed,
                        ));
                    }
                }
                self.connection.sender.send(Message::Response(Response {
                    id,
                    response_result: result,
                }))?;
            }
            WorkerEvent::Diagnostics {
                state_rev,
                diag_rev,
                is_startup,
                batch,
            } => {
                let current = state_rev == self.state_rev && diag_rev == self.diag_rev;
                match batch {
                    Some(batch) if current => {
                        self.startup_pending = false;
                        if !is_startup {
                            self.pending_check = None;
                        }
                        self.diagnostics.apply_and_flush(batch, &self.context)?;
                        return Ok(());
                    }
                    Some(_) => {
                        self.context.logger().verbose(|| {
                            format!(
                                "Rejected stale diagnostic publication state_rev={state_rev} current_state_rev={} diag_rev={diag_rev} current_diag_rev={}",
                                self.state_rev, self.diag_rev,
                            )
                        });
                    }
                    None => {
                        self.context.logger().verbose(|| {
                            format!(
                                "Diagnostic computation aborted state_rev={state_rev} current_state_rev={} diag_rev={diag_rev} current_diag_rev={}",
                                self.state_rev, self.diag_rev,
                            )
                        });
                    }
                }
                // A launch-time document open can supersede the initial batch
                // before it starts computing, and an abort can leave no batch at
                // all. Recompute at the current revision so workspace errors
                // still surface, unless an edit or save has established a newer
                // baseline.
                if is_startup
                    && self.startup_pending
                    && !self.startup_edited
                    && let Some(cmd_tx) = cmd_tx
                {
                    cmd_tx
                        .send(WorkerCmd::StartupDiagnostics {
                            state_rev: self.state_rev,
                            diag_rev: self.diag_rev,
                        })
                        .ok();
                }
                // A save check can be superseded by a non-edit state change (for
                // example a `.redscript` or dependency watcher event) while it is
                // in flight. Unlike startup diagnostics nothing else recomputes
                // it, so retry at the current revision. An unsaved edit clears
                // `pending_check`, and a newer save advances `diag_rev`; either
                // way this cannot loop.
                if !is_startup
                    && diag_rev == self.diag_rev
                    && let Some(path) = self.pending_check.clone()
                    && let Some(cmd_tx) = cmd_tx
                {
                    cmd_tx
                        .send(WorkerCmd::Check {
                            state_rev: self.state_rev,
                            diag_rev: self.diag_rev,
                            roots_rev: self.roots_rev,
                            path,
                        })
                        .ok();
                }
            }
            WorkerEvent::WatchRoots { roots_rev, roots } => {
                if roots_rev == self.roots_rev {
                    self.update_watchers_with(roots)?;
                }
            }
            WorkerEvent::Error(message) => {
                self.context.logger().error(&message);
                self.context.show_message(lsp::MessageType::ERROR, message);
            }
        }
        Ok(())
    }

    fn handle_watcher_response(&mut self, response: Response) {
        if response.id == RequestId::from("source-watchers".to_owned())
            || response.id == RequestId::from("remove-source-watchers".to_owned())
        {
            if let Err(error) = response.response_result {
                self.context.logger().error(format!(
                    "Could not register source watchers: {}",
                    error.message
                ));
            }
        } else {
            self.context
                .logger()
                .info(format!("Received an unexpected response: {}", response.id));
        }
    }

    /// Whether a watched-file event cannot alter the analyzed sources.
    ///
    /// Open buffers always override on-disk content, so a `CHANGED` or
    /// `CREATED` event for a source file that is currently open is a no-op for
    /// the analysis `SourceMap`. Dropping it avoids a spurious `state_rev` bump
    /// (which would reject an in-flight save check) and a redundant full
    /// workspace rebuild. `.redscript` files are always read from disk, and a
    /// deletion may drop a file that is still open, so both are always kept.
    fn is_redundant_watcher_event(&self, event: &lsp::FileEvent) -> bool {
        if event.typ == lsp::FileChangeType::DELETED {
            return false;
        }
        let Ok(path) = path_from_uri(&event.uri) else {
            return false;
        };
        if path.file_name().is_some_and(|name| name == ".redscript") {
            return false;
        }
        self.buffers.get(&event.uri).is_some()
    }

    fn owned_document(&self, uri: &lsp::Uri) -> anyhow::Result<OwnedDocument> {
        let path = path_from_uri(uri)?;
        let buffer = self.buffers.get(uri).context("URI not found")?.clone();
        Ok(OwnedDocument { path, buffer })
    }

    fn owned_location(
        &self,
        uri: &lsp::Uri,
        pos: lsp::Position,
    ) -> anyhow::Result<OwnedCodeLocation> {
        let doc = self.owned_document(uri)?;
        let pos = doc
            .buffer
            .byte_offset(pos.line, pos.character)
            .context("position not found")?;
        Ok(OwnedCodeLocation { doc, pos })
    }

    fn owned_documents(&self, uris: &[lsp::Uri]) -> Vec<(PathBuf, Option<Buffer>)> {
        uris.iter()
            .filter_map(|uri| match path_from_uri(uri) {
                Ok(path) => Some((path, self.buffers.get(uri).cloned())),
                Err(err) => {
                    self.context.logger().error(format!("{err:#}"));
                    None
                }
            })
            .collect()
    }

    fn parse_worker_request(
        &self,
        method: &str,
        params: Value,
    ) -> Result<WorkerRequest, ResponseError> {
        match method {
            lsp::request::HoverRequest::METHOD => {
                let lsp::HoverParams {
                    text_document_position_params: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let loc = self
                    .owned_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                Ok(WorkerRequest::Hover(loc))
            }
            lsp::request::Completion::METHOD => {
                let lsp::CompletionParams {
                    text_document_position: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let loc = self
                    .owned_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                Ok(WorkerRequest::Completion(loc))
            }
            lsp::request::GotoDefinition::METHOD => {
                let lsp::GotoDefinitionParams {
                    text_document_position_params: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let loc = self
                    .owned_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                Ok(WorkerRequest::GotoDefinition(loc))
            }
            lsp::request::DocumentSymbolRequest::METHOD => {
                let lsp::DocumentSymbolParams { text_document, .. } =
                    serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self
                    .owned_document(&text_document.uri)
                    .map_err(invalid_params)?;
                Ok(WorkerRequest::DocumentSymbols(doc))
            }
            lsp::request::WorkspaceSymbolRequest::METHOD => {
                let lsp::WorkspaceSymbolParams { query, .. } =
                    serde_json::from_value(params).map_err(invalid_params)?;
                Ok(WorkerRequest::WorkspaceSymbol(query))
            }
            lsp::request::Formatting::METHOD => {
                let lsp::DocumentFormattingParams {
                    text_document,
                    options,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self
                    .owned_document(&text_document.uri)
                    .map_err(invalid_params)?;
                Ok(WorkerRequest::Formatting {
                    doc,
                    // Guard the truncating cast; LSP `tabSize` is a `u32`.
                    tab_size: u16::try_from(options.tab_size).unwrap_or(4).max(1),
                })
            }
            lsp::request::CodeActionRequest::METHOD => {
                let lsp::CodeActionParams {
                    text_document,
                    range,
                    context,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let path = path_from_uri(&text_document.uri).map_err(invalid_params)?;
                let span = self
                    .byte_span(&text_document.uri, range)
                    .map_err(invalid_params)?;
                Ok(WorkerRequest::CodeAction {
                    path,
                    span,
                    context,
                })
            }
            lsp::request::CodeActionResolveRequest::METHOD => {
                let action = serde_json::from_value(params).map_err(invalid_params)?;
                Ok(WorkerRequest::CodeActionResolve(Box::new(action)))
            }
            lsp::request::ResolveCompletionItem::METHOD => {
                let item = serde_json::from_value(params).map_err(invalid_params)?;
                Ok(WorkerRequest::CompletionResolve(Box::new(item)))
            }
            _ => Err(response_error(
                ErrorCode::MethodNotFound,
                format!("Unknown request: {method}"),
            )),
        }
    }

    fn update_watchers(&mut self) -> anyhow::Result<()> {
        if !self.dynamic_watch_files {
            return Ok(());
        }
        let roots = self.server.watch_roots();
        self.update_watchers_with(roots)
    }

    /// Registers or clears dynamic file watchers for `roots`, skipping work
    /// when they are unchanged. Used by the protocol thread with roots computed
    /// on the worker.
    fn update_watchers_with(&mut self, roots: Vec<PathBuf>) -> anyhow::Result<()> {
        if !self.dynamic_watch_files {
            return Ok(());
        }
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

    #[cfg(test)]
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
                response_value(self.server.hover(doc, &self.context).map(|hover| {
                    hover.map(|mut hover| {
                        self.context.adapt_hover(&mut hover);
                        hover
                    })
                }))
            }
            lsp::request::Completion::METHOD => {
                let lsp::CompletionParams {
                    text_document_position: params,
                    ..
                } = serde_json::from_value(params).map_err(invalid_params)?;
                let doc = self
                    .document_location(&params.text_document.uri, params.position)
                    .map_err(invalid_params)?;
                response_value_shared(self.server.completion_for_client(doc, &self.context))
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
                response_value(
                    self.server
                        .document_symbols(doc, &self.context)
                        .map(|symbols| {
                            self.context
                                .adapt_document_symbols(symbols, &text_document.uri)
                        }),
                )
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
                response_value(self.server.format(
                    doc,
                    // Guard the truncating cast; LSP `tabSize` is a `u32`.
                    u16::try_from(options.tab_size).unwrap_or(4).max(1),
                    &self.context,
                ))
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
                    .byte_span(&text_document.uri, range)
                    .map_err(invalid_params)?;
                response_value(self.server.code_action(path, range, context, &self.context))
            }
            lsp::request::CodeActionResolveRequest::METHOD => {
                let action = serde_json::from_value(params).map_err(invalid_params)?;
                response_value(self.server.code_action_resolve(action, &self.context))
            }
            lsp::request::ResolveCompletionItem::METHOD => {
                let item = serde_json::from_value(params).map_err(invalid_params)?;
                response_value(self.server.completion_resolve(item, &self.context))
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

    #[cfg(test)]
    fn handle(&mut self, message: Message) -> anyhow::Result<()> {
        match message {
            Message::Request(request) => {
                let started = Instant::now();
                let log_summary = is_summary_method(&request.method);
                log_request_metadata(&self.context, &request);
                let response_result = self.dispatch(&request.method, request.params);
                match &response_result {
                    Ok(value) => {
                        if log_summary {
                            self.context.logger().verbose(|| {
                                format!(
                                    "Response method={} id={} {} elapsed_ms={:.3}",
                                    request.method,
                                    request.id,
                                    response_summary(&request.method, value),
                                    started.elapsed().as_secs_f64() * 1000.0,
                                )
                            });
                        }
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
                    }
                }
                self.connection.sender.send(Message::Response(Response {
                    id: request.id,
                    response_result,
                }))?;
            }
            Message::Notification(_) if self.shutdown => return Ok(()),
            Message::Notification(notif) => match notif.method.as_str() {
                lsp::notification::Initialized::METHOD => {}
                lsp::notification::Cancel::METHOD => {
                    let params: lsp::CancelParams = serde_json::from_value(notif.params)?;
                    self.context.logger().verbose(|| {
                        format!(
                            "Cancellation received id={:?}; synchronous request handling may have already sent its response (receipt order does not establish when the client cancelled)",
                            params.id,
                        )
                    });
                }
                lsp::notification::DidSaveTextDocument::METHOD => {
                    let lsp::DidSaveTextDocumentParams {
                        text_document: doc, ..
                    } = serde_json::from_value(notif.params)?;
                    if !is_file_uri(&doc.uri) {
                        self.context.logger().verbose(|| {
                            format!(
                                "Document save ignored (non-file URI) uri={}",
                                doc.uri.as_str(),
                            )
                        });
                        return Ok(());
                    }
                    self.context.logger().verbose(|| {
                        format!(
                            "Document save uri={} version={:?}",
                            doc.uri.as_str(),
                            self.buffers.get(&doc.uri).and_then(Buffer::version),
                        )
                    });
                    self.server.check(path_from_uri(&doc.uri)?, &self.context)?;
                }
                lsp::notification::DidOpenTextDocument::METHOD => {
                    let lsp::DidOpenTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    let uri = doc.uri;
                    if !is_file_uri(&uri) {
                        self.context.logger().verbose(|| {
                            format!(
                                "Document open ignored (non-file URI) uri={} version={}",
                                uri.as_str(),
                                doc.version,
                            )
                        });
                        return Ok(());
                    }
                    let path = path_from_uri(&uri)?;
                    self.context.logger().verbose(|| {
                        format!("Document open uri={} version={}", uri.as_str(), doc.version,)
                    });
                    self.buffers.open(uri.clone(), doc.text, doc.version);
                    self.server
                        .document_changed(path, self.buffers.get(&uri), &self.context)?;
                }
                lsp::notification::DidCloseTextDocument::METHOD => {
                    let lsp::DidCloseTextDocumentParams { text_document: doc } =
                        serde_json::from_value(notif.params)?;
                    if !is_file_uri(&doc.uri) {
                        self.context.logger().verbose(|| {
                            format!(
                                "Document close ignored (non-file URI) uri={}",
                                doc.uri.as_str(),
                            )
                        });
                        return Ok(());
                    }
                    let path = path_from_uri(&doc.uri)?;
                    self.context.logger().verbose(|| {
                        format!(
                            "Document close uri={} version={:?}",
                            doc.uri.as_str(),
                            self.buffers.get(&doc.uri).and_then(Buffer::version),
                        )
                    });
                    self.buffers.remove(&doc.uri);
                    self.server.document_changed(path, None, &self.context)?;
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
                        .filter(|event| is_file_uri(&event.uri))
                        .map(|event| path_from_uri(&event.uri))
                        .collect::<anyhow::Result<Vec<_>>>()?;
                    if !paths.is_empty() {
                        self.server.files_changed(paths, &self.context)?;
                    }
                }
                lsp::notification::DidChangeConfiguration::METHOD => {
                    self.server.files_changed(vec![], &self.context)?;
                }
                lsp::notification::DidChangeWorkspaceFolders::METHOD => {
                    let lsp::DidChangeWorkspaceFoldersParams { event } =
                        serde_json::from_value(notif.params)?;
                    let added = workspace_folder_paths(event.added, &self.context)?;
                    let removed = workspace_folder_paths(event.removed, &self.context)?;
                    if !added.is_empty() || !removed.is_empty() {
                        self.server
                            .change_workspace_folders(added, removed, &self.context)?;
                    }
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
                if let Err(error) = response.response_result {
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
        if !is_file_uri(&doc.uri) {
            self.context.logger().verbose(|| {
                format!(
                    "Document change ignored (non-file URI) uri={} version={}",
                    doc.uri.as_str(),
                    doc.version,
                )
            });
            return Ok(None);
        }
        let current_version = self.buffers.get(&doc.uri).and_then(Buffer::version);
        match self
            .buffers
            .try_change(&doc.uri, doc.version, content_changes)
        {
            Ok(()) => {
                self.context.logger().verbose(|| {
                    format!(
                        "Document change uri={} version={}",
                        doc.uri.as_str(),
                        doc.version,
                    )
                });
                Ok(Some(doc.uri))
            }
            Err(ChangeError::StaleVersion { .. } | ChangeError::NotOpen) => {
                self.context.logger().verbose(|| {
                    format!(
                        "Document change ignored ({}) uri={} version={} current_version={current_version:?}",
                        if current_version.is_some_and(|version| version >= doc.version) {
                            "stale"
                        } else {
                            "not open"
                        },
                        doc.uri.as_str(),
                        doc.version,
                    )
                });
                Ok(None)
            }
            Err(ChangeError::InvalidRange(range)) => {
                let message = format!(
                    "Document change rejected (invalid range {range:?}) uri={} version={} current_version={current_version:?}; reopen or save the document to resynchronize",
                    doc.uri.as_str(),
                    doc.version,
                );
                self.context.logger().error(&message);
                self.context.show_message(lsp::MessageType::ERROR, message);
                Ok(None)
            }
        }
    }

    fn document(&self, uri: &lsp::Uri) -> anyhow::Result<Document<'_>> {
        let path = path_from_uri(uri)?;
        let content = self.buffers.get(uri).context("URI not found")?;
        Ok(Document::new(path, content))
    }

    #[cfg(test)]
    fn document_location(
        &self,
        uri: &lsp::Uri,
        pos: lsp::Position,
    ) -> anyhow::Result<CodeLocation<'_>> {
        let document = self.document(uri)?;
        let pos = document
            .buffer
            .byte_offset(pos.line, pos.character)
            .context("position not found")?;
        Ok(CodeLocation::new(document, pos))
    }

    fn byte_span(&self, uri: &lsp::Uri, range: lsp::Range) -> anyhow::Result<Range<u32>> {
        let document = self.document(uri)?;
        let start = document
            .buffer
            .byte_offset(range.start.line, range.start.character)
            .context("start position not found")?;
        let end = document
            .buffer
            .byte_offset(range.end.line, range.end.character)
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

/// Like [`response_value`], but serializes a shared response in place instead of
/// cloning it first.
fn response_value_shared<R: Serialize>(
    result: anyhow::Result<Arc<R>>,
) -> Result<Value, ResponseError> {
    result
        .and_then(|value| Ok(serde_json::to_value(&*value)?))
        .map_err(|err| response_error(ErrorCode::InternalError, err))
}

/// Requests whose logs summarize the response shape instead of echoing it.
fn is_summary_method(method: &str) -> bool {
    matches!(
        method,
        lsp::request::HoverRequest::METHOD | lsp::request::Completion::METHOD
    )
}

/// Logs the redacted request metadata for hover/completion, mirroring what the
/// synchronous test request path records.
fn log_request_metadata(context: &LspContext, request: &lsp_server::Request) {
    if !is_summary_method(&request.method) {
        return;
    }
    let params = &request.params;
    context.logger().verbose(|| {
        format!(
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
        )
    });
    if request.method == lsp::request::Completion::METHOD {
        context.logger().verbose(|| {
            format!(
                "Completion context id={} trigger_kind={:?} trigger_character={:?}",
                request.id,
                params
                    .pointer("/context/triggerKind")
                    .and_then(Value::as_u64),
                params
                    .pointer("/context/triggerCharacter")
                    .and_then(Value::as_str),
            )
        });
    }
}

/// Executes a worker request against the language server, adapting the raw
/// result to the client's negotiated presentation capabilities.
fn execute_request(
    server: &mut dyn LanguageServer,
    request: WorkerRequest,
    context: &LspContext,
) -> Result<Value, ResponseError> {
    match request {
        WorkerRequest::Hover(loc) => {
            response_value(server.hover(loc.as_loc(), context).map(|hover| {
                hover.map(|mut hover| {
                    context.adapt_hover(&mut hover);
                    hover
                })
            }))
        }
        WorkerRequest::Completion(loc) => {
            response_value_shared(server.completion_for_client(loc.as_loc(), context))
        }
        WorkerRequest::GotoDefinition(loc) => {
            response_value(server.goto_definition(loc.as_loc(), context))
        }
        WorkerRequest::DocumentSymbols(doc) => {
            let uri = context.uri(doc.path()).map_err(invalid_params)?;
            response_value(
                server
                    .document_symbols(doc.as_doc(), context)
                    .map(|symbols| context.adapt_document_symbols(symbols, &uri)),
            )
        }
        WorkerRequest::WorkspaceSymbol(query) => {
            response_value(server.workspace_symbol(&query, context))
        }
        WorkerRequest::Formatting { doc, tab_size } => {
            response_value(server.format(doc.as_doc(), tab_size, context))
        }
        WorkerRequest::CodeAction {
            path,
            span,
            context: action_context,
        } => response_value(server.code_action(path, span, action_context, context)),
        WorkerRequest::CodeActionResolve(action) => {
            response_value(server.code_action_resolve(*action, context))
        }
        WorkerRequest::CompletionResolve(item) => {
            response_value(server.completion_resolve(*item, context))
        }
    }
}

/// Whether a command computes background diagnostics, which interactive
/// requests may run ahead of.
fn is_background_diagnostics(cmd: &WorkerCmd) -> bool {
    matches!(
        cmd,
        WorkerCmd::StartupDiagnostics { .. } | WorkerCmd::Check { .. }
    )
}

/// Promotes the first queued interactive request ahead of the background
/// diagnostic commands in front of it.
///
/// Only requests are moved, and only past other background diagnostics or
/// already-cancelled requests. A state mutation or the `Flush` barrier keeps its
/// position relative to the diagnostics, so causal ordering and the shutdown
/// barrier are preserved.
fn promote_request(queue: &mut VecDeque<WorkerCmd>) -> bool {
    if !queue.front().is_some_and(is_background_diagnostics) {
        return false;
    }
    let mut promoted = None;
    for (index, cmd) in queue.iter().enumerate().skip(1) {
        match cmd {
            cmd if is_background_diagnostics(cmd) => {}
            // The protocol thread already answered the cancellation.
            WorkerCmd::Request { cancel, .. } if cancel.is_cancelled() => {}
            WorkerCmd::Request { .. } => {
                promoted = Some(index);
                break;
            }
            _ => break,
        }
    }
    let Some(index) = promoted else {
        return false;
    };
    let cmd = queue.remove(index).expect("index came from the queue");
    queue.push_front(cmd);
    true
}

/// The analysis worker thread. It owns the language server exclusively and
/// processes commands in order, emitting results as [`WorkerEvent`]s.
fn worker_main<'a>(
    mut server: Box<dyn LanguageServer + 'a>,
    context: LspContext,
    cmd_rx: Receiver<WorkerCmd>,
    event_tx: Sender<WorkerEvent>,
    latest_state_rev: Arc<AtomicU64>,
    latest_diag_rev: Arc<AtomicU64>,
    dynamic_watch_files: bool,
) {
    let publish_roots =
        |roots_rev: u64, server: &dyn LanguageServer, event_tx: &Sender<WorkerEvent>| {
            if dynamic_watch_files {
                event_tx
                    .send(WorkerEvent::WatchRoots {
                        roots_rev,
                        roots: server.watch_roots(),
                    })
                    .ok();
            }
        };

    let mut queue: VecDeque<WorkerCmd> = VecDeque::new();
    loop {
        if queue.is_empty() {
            match cmd_rx.recv() {
                Ok(cmd) => queue.push_back(cmd),
                Err(_) => break,
            }
        }
        // Pull in everything already queued so a request can be promoted and a
        // burst of edits coalesced before the next command runs.
        queue.extend(cmd_rx.try_iter());
        promote_request(&mut queue);
        let cmd = queue.pop_front().expect("the queue is non-empty");
        match cmd {
            WorkerCmd::DocumentsChanged { mut documents, .. } => {
                // The protocol thread coalesces edits already buffered in the
                // connection, but edits arriving while the worker is busy enqueue
                // separate commands. Merge them so a burst triggers one rebuild.
                // A cancelled request is dropped rather than splitting the burst.
                loop {
                    match queue.front() {
                        Some(WorkerCmd::DocumentsChanged { .. }) => {
                            let Some(WorkerCmd::DocumentsChanged {
                                documents: more, ..
                            }) = queue.pop_front()
                            else {
                                unreachable!("the front is a DocumentsChanged command");
                            };
                            documents.extend(more);
                        }
                        Some(WorkerCmd::Request { cancel, .. }) if cancel.is_cancelled() => {
                            queue.pop_front();
                        }
                        _ => break,
                    }
                }
                // Keep only the newest buffer for each path: a burst of edits to
                // one document would otherwise re-apply the same overlay for
                // every queued command before the single rebuild.
                let mut latest: HashMap<PathBuf, usize> = HashMap::new();
                let mut deduped: Vec<(PathBuf, Option<Buffer>)> =
                    Vec::with_capacity(documents.len());
                for (path, buffer) in documents {
                    match latest.entry(path) {
                        std::collections::hash_map::Entry::Occupied(slot) => {
                            deduped[*slot.get()].1 = buffer;
                        }
                        std::collections::hash_map::Entry::Vacant(slot) => {
                            let path = slot.key().clone();
                            slot.insert(deduped.len());
                            deduped.push((path, buffer));
                        }
                    }
                }
                let borrowed: Vec<(PathBuf, Option<&Buffer>)> = deduped
                    .iter()
                    .map(|(path, buffer)| (path.clone(), buffer.as_ref()))
                    .collect();
                if let Err(err) = server.documents_changed(&borrowed, &context) {
                    event_tx.send(WorkerEvent::Error(format!("{err:#}"))).ok();
                }
                // Overlays never change the watched roots, so unlike the other
                // state commands this one does not re-publish them.
            }
            WorkerCmd::FilesChanged {
                roots_rev, paths, ..
            } => {
                if let Err(err) = server.files_changed(paths, &context) {
                    event_tx.send(WorkerEvent::Error(format!("{err:#}"))).ok();
                }
                publish_roots(roots_rev, &*server, &event_tx);
            }
            WorkerCmd::ChangeWorkspaceFolders {
                roots_rev,
                added,
                removed,
                ..
            } => {
                if let Err(err) = server.change_workspace_folders(added, removed, &context) {
                    event_tx.send(WorkerEvent::Error(format!("{err:#}"))).ok();
                }
                publish_roots(roots_rev, &*server, &event_tx);
            }
            WorkerCmd::Check {
                state_rev,
                diag_rev,
                roots_rev,
                path,
            } => {
                // Abort only when a save has already superseded this check; a
                // plain edit in another file must still refresh this batch's
                // cached quick fixes.
                let stale = || latest_diag_rev.load(Ordering::Acquire) != diag_rev;
                let cancel = CancelToken::default();
                set_current_cancel(cancel);
                let result = server.check_cancellable(path, &context, &stale);
                clear_current_cancel();
                match result {
                    Ok(Some(batch)) => {
                        event_tx
                            .send(WorkerEvent::Diagnostics {
                                state_rev,
                                diag_rev,
                                is_startup: false,
                                batch: Some(batch),
                            })
                            .ok();
                    }
                    Ok(None) => {}
                    Err(err) => {
                        event_tx.send(WorkerEvent::Error(format!("{err:#}"))).ok();
                    }
                }
                publish_roots(roots_rev, &*server, &event_tx);
            }
            WorkerCmd::StartupDiagnostics {
                state_rev,
                diag_rev,
            } => {
                let stale = || {
                    latest_state_rev.load(Ordering::Acquire) != state_rev
                        || latest_diag_rev.load(Ordering::Acquire) != diag_rev
                };
                let cancel = CancelToken::default();
                set_current_cancel(cancel);
                let result = server.startup_diagnostics(&context, &stale);
                clear_current_cancel();
                match result {
                    // Report aborts too so the protocol thread can retry when a
                    // launch-time document open superseded this computation.
                    Ok(batch) => {
                        event_tx
                            .send(WorkerEvent::Diagnostics {
                                state_rev,
                                diag_rev,
                                is_startup: true,
                                batch,
                            })
                            .ok();
                    }
                    Err(err) => {
                        event_tx.send(WorkerEvent::Error(format!("{err:#}"))).ok();
                    }
                }
            }
            WorkerCmd::Request {
                id,
                method,
                request,
                started,
                cancel,
            } => {
                if cancel.is_cancelled() {
                    continue;
                }
                set_current_cancel(cancel.clone());
                let result = execute_request(&mut *server, *request, &context);
                clear_current_cancel();
                // A cancelled request already received its `RequestCancelled`
                // response from the protocol thread; drop the late result.
                if cancel.is_cancelled() {
                    continue;
                }
                event_tx
                    .send(WorkerEvent::Response {
                        id,
                        method,
                        started,
                        result,
                    })
                    .ok();
            }
            WorkerCmd::Flush(ack) => {
                ack.send(()).ok();
            }
        }
    }
}

pub trait LanguageServer: Send {
    fn watch_roots(&self) -> Vec<PathBuf>;
    fn document_changed(
        &mut self,
        path: PathBuf,
        buffer: Option<&Buffer>,
        ctx: &LspContext,
    ) -> anyhow::Result<()>;
    /// Apply a batch of document changes. The default implementation forwards to
    /// [`LanguageServer::document_changed`] one document at a time; servers that
    /// can coalesce the resulting work should override this to do a single pass.
    fn documents_changed(
        &mut self,
        documents: &[(PathBuf, Option<&Buffer>)],
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        for (path, buffer) in documents {
            self.document_changed(path.clone(), *buffer, ctx)?;
        }
        Ok(())
    }
    fn files_changed(&mut self, paths: Vec<PathBuf>, ctx: &LspContext) -> anyhow::Result<()>;
    fn check(&mut self, path: PathBuf, ctx: &LspContext)
    -> anyhow::Result<Option<DiagnosticBatch>>;
    /// Like [`LanguageServer::check`], but lets `should_abort` stop work that a
    /// newer save has superseded. The default ignores it and calls [`Self::check`].
    fn check_cancellable(
        &mut self,
        path: PathBuf,
        ctx: &LspContext,
        should_abort: &(dyn Fn() -> bool + Sync),
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        let _ = should_abort;
        self.check(path, ctx)
    }
    /// Computes the initial workspace diagnostics without publishing them.
    ///
    /// The default implementation reports nothing; servers with real analysis
    /// return a batch that the protocol thread commits only if it is still
    /// current. `should_abort` lets the computation stop early once a newer
    /// edit has superseded it.
    fn startup_diagnostics(
        &mut self,
        _ctx: &LspContext,
        _should_abort: &(dyn Fn() -> bool + Sync),
    ) -> anyhow::Result<Option<DiagnosticBatch>> {
        Ok(None)
    }
    fn change_workspace_folders(
        &mut self,
        added: Vec<PathBuf>,
        removed: Vec<PathBuf>,
        ctx: &LspContext,
    ) -> anyhow::Result<()>;
    fn hover(&self, loc: CodeLocation<'_>, ctx: &LspContext) -> anyhow::Result<Option<lsp::Hover>>;
    fn completion(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp::CompletionResponse>>;
    /// Returns the completion response adapted to the client's presentation
    /// capabilities. The default clones the response and adapts it; servers that
    /// cache their completion responses should override this to reuse the
    /// adapted form instead of re-cloning and re-adapting it per request.
    fn completion_for_client(
        &self,
        loc: CodeLocation<'_>,
        ctx: &LspContext,
    ) -> anyhow::Result<Arc<lsp::CompletionResponse>> {
        let mut response = self.completion(loc, ctx)?.as_ref().clone();
        ctx.adapt_completion(&mut response);
        Ok(Arc::new(response))
    }
    /// Resolves the deferred properties of a completion item. The default
    /// returns the item unchanged; servers that defer documentation override it.
    fn completion_resolve(
        &self,
        item: lsp::CompletionItem,
        _ctx: &LspContext,
    ) -> anyhow::Result<lsp::CompletionItem> {
        Ok(item)
    }
    fn goto_definition(
        &self,
        loc: CodeLocation<'_>,
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

/// Preserve the client's preference order, with plaintext as the capability default.
fn preferred_markup(formats: Option<&[lsp::MarkupKind]>) -> lsp::MarkupKind {
    formats
        .into_iter()
        .flatten()
        .find(|kind| matches!(kind, lsp::MarkupKind::Markdown | lsp::MarkupKind::PlainText))
        .cloned()
        .unwrap_or(lsp::MarkupKind::PlainText)
}

fn plaintext_marked_string(value: &lsp::MarkedString) -> String {
    match value {
        lsp::MarkedString::String(value) => plaintext_markup(value),
        lsp::MarkedString::LanguageString(value) => value.value.clone(),
    }
}

/// Remove the Markdown structure emitted by our displays, keeping fenced code literal.
/// This is not a general Markdown parser for arbitrary user documentation.
fn plaintext_markup(markdown: &str) -> String {
    let mut lines = Vec::new();
    let mut fenced = false;
    for line in markdown.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("```") {
            if fenced {
                fenced = false;
            } else if let Some(code) = rest.strip_suffix("```") {
                // Field displays use a single-line fence rather than a code block.
                lines.push(code.to_owned());
            } else {
                fenced = true;
            }
        } else if fenced {
            lines.push(line.to_owned());
        } else if matches!(trimmed, "---" | "***" | "___") {
            lines.push(String::new());
        } else {
            let heading = trimmed.trim_start_matches('#');
            let text = if heading.len() != trimmed.len() && heading.starts_with(' ') {
                heading.trim_start()
            } else {
                line
            };
            lines.push(text.replace('`', ""));
        }
    }
    lines.join("\n").trim().to_owned()
}

/// The client capabilities that shape a completion response's presentation.
///
/// Fixed after `initialize`, so a server can cache the response it adapted for
/// these flags and reuse it across prefix edits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CompletionPresentation {
    pub(crate) snippets: bool,
    pub(crate) label_details: bool,
    pub(crate) documentation: lsp::MarkupKind,
}

#[derive(Debug, Clone)]
pub struct LspContext {
    sender: Sender<Message>,
    diagnostic_tags: Vec<lsp::DiagnosticTag>,
    code_action_literals: bool,
    code_action_data: bool,
    code_action_resolve_edit: bool,
    code_action_disabled: bool,
    workspace_edit_document_changes: bool,
    completion_snippets: bool,
    completion_label_details: bool,
    completion_documentation_format: lsp::MarkupKind,
    /// Whether the client resolves completion documentation lazily, so the
    /// server may omit it from the initial response.
    completion_resolve_documentation: bool,
    hover_format: lsp::MarkupKind,
    hierarchical_document_symbols: bool,
}

impl LspContext {
    pub(crate) fn new(sender: Sender<Message>) -> Self {
        Self {
            sender,
            diagnostic_tags: vec![],
            code_action_literals: false,
            code_action_data: false,
            code_action_resolve_edit: false,
            code_action_disabled: false,
            workspace_edit_document_changes: false,
            completion_snippets: false,
            completion_label_details: false,
            completion_documentation_format: lsp::MarkupKind::PlainText,
            completion_resolve_documentation: false,
            hover_format: lsp::MarkupKind::PlainText,
            hierarchical_document_symbols: false,
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
        self.code_action_disabled = support.and_then(|s| s.disabled_support).unwrap_or(false);
        self.workspace_edit_document_changes = capabilities
            .workspace
            .as_ref()
            .and_then(|workspace| workspace.workspace_edit.as_ref())
            .and_then(|edit| edit.document_changes)
            .unwrap_or(false);
    }

    pub(crate) fn set_presentation_capabilities(&mut self, capabilities: &lsp::ClientCapabilities) {
        let doc = capabilities.text_document.as_ref();
        self.hierarchical_document_symbols = doc
            .and_then(|doc| doc.document_symbol.as_ref())
            .and_then(|symbols| symbols.hierarchical_document_symbol_support)
            .unwrap_or(false);
        let completion = doc
            .and_then(|doc| doc.completion.as_ref())
            .and_then(|completion| completion.completion_item.as_ref());
        self.completion_snippets = completion
            .and_then(|item| item.snippet_support)
            .unwrap_or(false);
        self.completion_label_details = completion
            .and_then(|item| item.label_details_support)
            .unwrap_or(false);
        self.completion_documentation_format =
            preferred_markup(completion.and_then(|item| item.documentation_format.as_deref()));
        self.completion_resolve_documentation = completion
            .and_then(|item| item.resolve_support.as_ref())
            .is_some_and(|support| support.properties.iter().any(|p| p == "documentation"));
        self.hover_format = preferred_markup(
            doc.and_then(|doc| doc.hover.as_ref())
                .and_then(|hover| hover.content_format.as_deref()),
        );
    }

    fn adapt_document_symbols(
        &self,
        symbols: lsp::DocumentSymbolResponse,
        uri: &lsp::Uri,
    ) -> lsp::DocumentSymbolResponse {
        fn flatten(
            symbols: Vec<lsp::DocumentSymbol>,
            uri: &lsp::Uri,
            container_name: Option<&str>,
            result: &mut Vec<lsp::SymbolInformation>,
        ) {
            for symbol in symbols {
                #[allow(deprecated)]
                result.push(lsp::SymbolInformation {
                    name: symbol.name.clone(),
                    kind: symbol.kind,
                    tags: symbol.tags,
                    deprecated: symbol.deprecated,
                    location: lsp::Location::new(uri.clone(), symbol.range),
                    container_name: container_name.map(str::to_owned),
                });
                flatten(
                    symbol.children.unwrap_or_default(),
                    uri,
                    Some(&symbol.name),
                    result,
                );
            }
        }

        match symbols {
            lsp::DocumentSymbolResponse::Nested(symbols) if !self.hierarchical_document_symbols => {
                let mut result = Vec::new();
                flatten(symbols, uri, None, &mut result);
                lsp::DocumentSymbolResponse::Flat(result)
            }
            symbols => symbols,
        }
    }

    fn adapt_hover(&self, hover: &mut lsp::Hover) {
        if self.hover_format == lsp::MarkupKind::PlainText {
            let value = match &hover.contents {
                lsp::HoverContents::Markup(content) => {
                    if content.kind == lsp::MarkupKind::PlainText {
                        return;
                    }
                    plaintext_markup(&content.value)
                }
                lsp::HoverContents::Scalar(value) => plaintext_marked_string(value),
                lsp::HoverContents::Array(values) => values
                    .iter()
                    .map(plaintext_marked_string)
                    .collect::<Vec<_>>()
                    .join("\n\n"),
            };
            hover.contents = lsp::HoverContents::Markup(lsp::MarkupContent {
                kind: lsp::MarkupKind::PlainText,
                value,
            });
        }
    }

    pub(crate) fn completion_presentation(&self) -> CompletionPresentation {
        CompletionPresentation {
            snippets: self.completion_snippets,
            label_details: self.completion_label_details,
            documentation: self.completion_documentation_format.clone(),
        }
    }

    pub(crate) fn adapt_completion(&self, completion: &mut lsp::CompletionResponse) {
        let items = match completion {
            lsp::CompletionResponse::Array(items) => items,
            lsp::CompletionResponse::List(list) => &mut list.items,
        };
        for item in items {
            self.adapt_completion_item(item);
        }
    }

    /// Adapts one item to the negotiated completion presentation. Used for the
    /// items of a response and for a single item returned by
    /// `completionItem/resolve`.
    pub(crate) fn adapt_completion_item(&self, item: &mut lsp::CompletionItem) {
        if !self.completion_snippets
            && item.insert_text_format == Some(lsp::InsertTextFormat::SNIPPET)
        {
            item.insert_text = Some(item.label.clone());
            item.insert_text_format = Some(lsp::InsertTextFormat::PLAIN_TEXT);
        }
        if !self.completion_label_details {
            item.label_details = None;
        }
        self.adapt_completion_documentation(item);
    }

    /// Converts an item's Markdown documentation to plaintext when the client
    /// does not accept Markdown.
    pub(crate) fn adapt_completion_documentation(&self, item: &mut lsp::CompletionItem) {
        if self.completion_documentation_format == lsp::MarkupKind::PlainText
            && let Some(lsp::Documentation::MarkupContent(content)) = &mut item.documentation
            && content.kind == lsp::MarkupKind::Markdown
        {
            content.value = plaintext_markup(&content.value);
            content.kind = lsp::MarkupKind::PlainText;
        }
    }

    pub fn supports_lazy_completion_documentation(&self) -> bool {
        self.completion_resolve_documentation
    }

    pub fn supports_code_action_resolve_edit(&self) -> bool {
        self.code_action_literals && self.code_action_data && self.code_action_resolve_edit
    }

    pub fn supports_code_action_disabled(&self) -> bool {
        self.code_action_disabled
    }

    pub fn supports_workspace_edit_document_changes(&self) -> bool {
        self.workspace_edit_document_changes
    }

    pub fn supports_diagnostic_tag(&self, tag: &lsp::DiagnosticTag) -> bool {
        self.diagnostic_tags.contains(tag)
    }

    pub fn logger(&self) -> LspLog<'_> {
        LspLog::new(self)
    }

    pub fn show_message(&self, typ: lsp::MessageType, message: impl Display) {
        self.notify::<lsp::notification::ShowMessage>(lsp::ShowMessageParams {
            typ,
            message: message.to_string(),
        });
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

    /// Emits a high-volume diagnostic log only when verbose logging is enabled.
    ///
    /// The message closure is not called otherwise, so default builds pay no
    /// string allocation or JSON-RPC notification for these per-edit,
    /// per-request, and per-query logs. Tests always enable them so their log
    /// assertions keep working.
    #[cfg(any(test, feature = "verbose-logs"))]
    pub fn verbose(&self, message: impl FnOnce() -> String) {
        self.context
            .notify::<lsp::notification::LogMessage>(lsp::LogMessageParams {
                typ: lsp::MessageType::INFO,
                message: message(),
            });
    }

    #[cfg(not(any(test, feature = "verbose-logs")))]
    pub fn verbose(&self, _message: impl FnOnce() -> String) {}

    pub fn info(&self, message: impl Display) {
        self.context
            .notify::<lsp::notification::LogMessage>(lsp::LogMessageParams {
                typ: lsp::MessageType::INFO,
                message: message.to_string(),
            });
    }

    pub fn warn(&self, message: impl Display) {
        self.context
            .notify::<lsp::notification::LogMessage>(lsp::LogMessageParams {
                typ: lsp::MessageType::WARNING,
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
    buffer: &'a Buffer,
}

impl<'a> Document<'a> {
    pub(crate) fn new(path: PathBuf, buffer: &'a Buffer) -> Self {
        Self { path, buffer }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub fn buffer(&self) -> &Buffer {
        self.buffer
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
    pub workspace_folders: Vec<PathBuf>,
    pub init_options: Option<Value>,
}

impl Options {
    fn new(workspace_folders: Vec<PathBuf>, init_options: Option<Value>) -> Self {
        Self {
            workspace_folders,
            init_options,
        }
    }
}

fn is_file_uri(uri: &lsp::Uri) -> bool {
    uri.scheme()
        .is_some_and(|scheme| scheme.eq_lowercase("file"))
}

pub(crate) fn path_from_uri(uri: &lsp::Uri) -> anyhow::Result<PathBuf> {
    if !is_file_uri(uri) {
        bail!("Unsupported URI scheme");
    }

    let stripped = uri.path().as_estr().decode().into_string()?;
    #[cfg(windows)]
    let path = PathBuf::from(stripped.trim_start_matches('/'));
    #[cfg(not(windows))]
    let path = {
        let bytes = stripped.as_bytes();
        if bytes.len() >= 3
            && bytes[0] == b'/'
            && bytes[1].is_ascii_alphabetic()
            && bytes[2] == b':'
        {
            PathBuf::from(&stripped[1..])
        } else {
            PathBuf::from(&*stripped)
        }
    };
    Ok(path)
}

/// Resolves workspace folder URIs to paths, skipping folders that do not use the
/// `file` scheme. Clients may advertise virtual or untitled folders, which have
/// no filesystem path; ignoring them keeps initialization from aborting.
fn workspace_folder_paths(
    folders: impl IntoIterator<Item = lsp::WorkspaceFolder>,
    context: &LspContext,
) -> anyhow::Result<Vec<PathBuf>> {
    folders
        .into_iter()
        .filter(|folder| {
            let is_file = is_file_uri(&folder.uri);
            if !is_file {
                context.logger().verbose(|| {
                    format!(
                        "Ignoring workspace folder with unsupported URI scheme uri={}",
                        folder.uri.as_str(),
                    )
                });
            }
            is_file
        })
        .map(|folder| path_from_uri(&folder.uri))
        .collect()
}

fn uri_from_path(path: &Path) -> anyhow::Result<lsp::Uri> {
    let mut encoded_path = fluent_uri::encoding::EString::new();
    let path_str = path.to_string_lossy().replace('\\', "/");
    // canonicalize() uses Windows extended-length paths; these are not URI paths.
    let path_str = path_str.strip_prefix("//?/").unwrap_or(&path_str);

    if !path_str.starts_with('/') {
        encoded_path.encode::<fluent_uri::encoding::encoder::Path>(&b"/");
    }
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
            resolve_provider: Some(true),
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
    use std::collections::BTreeMap;
    use std::sync::{Barrier, Mutex};
    use std::time::Duration;

    use super::*;

    #[derive(Default)]
    struct StubServer {
        hover: Option<Option<lsp::Hover>>,
        completion: Option<lsp::CompletionResponse>,
        document_symbols: Option<lsp::DocumentSymbolResponse>,
        allow_document_changes: bool,
        events: Option<Arc<Mutex<Vec<String>>>>,
        watch_roots: Vec<PathBuf>,
        code_actions: Vec<lsp::CodeAction>,
        startup_batch: Option<DiagnosticBatch>,
        /// When set, the first startup call waits on these barriers before
        /// deciding whether to abort, so tests can supersede it deterministically.
        startup_entered: Option<Arc<Barrier>>,
        startup_release: Option<Arc<Barrier>>,
        abort_startup: bool,
    }

    impl LanguageServer for StubServer {
        fn watch_roots(&self) -> Vec<PathBuf> {
            if let Some(events) = &self.events {
                events.lock().unwrap().push("watchers".into());
            }
            self.watch_roots.clone()
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
                events.lock().unwrap().push(format!(
                    "change:{}:{:?}",
                    buffer.contents(),
                    buffer.version(),
                ));
            }
            Ok(())
        }

        fn files_changed(&mut self, paths: Vec<PathBuf>, _ctx: &LspContext) -> anyhow::Result<()> {
            assert!(self.allow_document_changes, "unexpected file change");
            if let Some(events) = &self.events {
                events
                    .lock()
                    .unwrap()
                    .push(format!("files:{}", paths.len()));
            }
            Ok(())
        }

        fn check(
            &mut self,
            _path: PathBuf,
            _ctx: &LspContext,
        ) -> anyhow::Result<Option<DiagnosticBatch>> {
            assert!(self.allow_document_changes, "unexpected check");
            if let Some(events) = &self.events {
                events.lock().unwrap().push("save".into());
            }
            Ok(None)
        }

        fn startup_diagnostics(
            &mut self,
            _ctx: &LspContext,
            should_abort: &(dyn Fn() -> bool + Sync),
        ) -> anyhow::Result<Option<DiagnosticBatch>> {
            if let Some(events) = &self.events {
                events.lock().unwrap().push("startup".into());
            }
            // Only the first attempt blocks, so a retry can proceed unattended.
            if let Some(entered) = self.startup_entered.take() {
                entered.wait();
            }
            if let Some(release) = self.startup_release.take() {
                release.wait();
            }
            if self.abort_startup && should_abort() {
                return Ok(None);
            }
            Ok(self.startup_batch.clone())
        }

        fn change_workspace_folders(
            &mut self,
            added: Vec<PathBuf>,
            removed: Vec<PathBuf>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            if let Some(events) = &self.events {
                events
                    .lock()
                    .unwrap()
                    .push(format!("folders:{}:{}", added.len(), removed.len()));
            }
            self.watch_roots.retain(|root| !removed.contains(root));
            self.watch_roots.extend(added);
            Ok(())
        }

        fn hover(
            &self,
            loc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<Option<lsp::Hover>> {
            if let Some(events) = &self.events {
                events
                    .lock()
                    .unwrap()
                    .push(format!("hover:{}", loc.doc().buffer().contents()));
            }
            self.hover.clone().context("stub handler failed")
        }

        fn completion(
            &self,
            _loc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<Arc<lsp::CompletionResponse>> {
            Ok(Arc::new(
                self.completion
                    .clone()
                    .unwrap_or_else(|| lsp::CompletionResponse::Array(vec![])),
            ))
        }

        fn goto_definition(
            &self,
            _loc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
            Ok(lsp::GotoDefinitionResponse::Array(vec![]))
        }

        fn document_symbols(
            &self,
            doc: Document<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::DocumentSymbolResponse> {
            if let Some(events) = &self.events {
                events
                    .lock()
                    .unwrap()
                    .push(format!("document_symbols:{}", doc.buffer().contents()));
            }
            Ok(self
                .document_symbols
                .clone()
                .unwrap_or_else(|| lsp::DocumentSymbolResponse::Nested(vec![])))
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
            ctx: &LspContext,
        ) -> anyhow::Result<lsp::CodeActionResponse> {
            if let Some(events) = &self.events {
                events.lock().unwrap().push(format!(
                    "code_action:{span:?}:{}",
                    serde_json::to_string(&context)?
                ));
            }
            self.code_actions
                .iter()
                .cloned()
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
            if data.get("stale").and_then(Value::as_bool).unwrap_or(false) {
                action.edit = None;
                if ctx.supports_code_action_disabled() {
                    action.disabled = Some(lsp::CodeActionDisabled {
                        reason: "stale".into(),
                    });
                }
                return Ok(action);
            }
            action.edit = Some(stub_workspace_edit(ctx));
            Ok(action)
        }
    }

    fn stub_workspace_edit(ctx: &LspContext) -> lsp::WorkspaceEdit {
        let uri: lsp::Uri = "file:///C:/test.reds".parse().unwrap();
        let edit = lsp::TextEdit::new(lsp::Range::default(), "fixed".into());
        if ctx.supports_workspace_edit_document_changes() {
            lsp::WorkspaceEdit {
                document_changes: Some(lsp::DocumentChanges::Edits(vec![lsp::TextDocumentEdit {
                    text_document: lsp::OptionalVersionedTextDocumentIdentifier {
                        uri,
                        version: Some(1),
                    },
                    edits: vec![lsp::OneOf::Left(edit)],
                }])),
                ..Default::default()
            }
        } else {
            #[allow(clippy::mutable_key_type)]
            let changes = [(uri, vec![edit])].into_iter().collect();
            lsp::WorkspaceEdit {
                changes: Some(changes),
                ..Default::default()
            }
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
                dynamic_watch_files: false,
                watched_roots: vec![],
                shutdown: false,
                in_flight: HashMap::new(),
                state_rev: 0,
                roots_rev: 0,
                diag_rev: 0,
                pending_check: None,
                latest_state_rev: Arc::new(AtomicU64::new(0)),
                latest_diag_rev: Arc::new(AtomicU64::new(0)),
                diagnostics: DiagnosticTracker::default(),
                startup_pending: false,
                startup_edited: false,
            },
            client,
        )
    }

    fn presentation_capabilities(formats: Value, enabled: bool) -> lsp::ClientCapabilities {
        serde_json::from_value(json!({
            "textDocument": {
                "completion": {"completionItem": {
                    "snippetSupport": enabled,
                    "labelDetailsSupport": enabled,
                    "documentationFormat": formats
                }},
                "hover": {"contentFormat": formats}
            }
        }))
        .unwrap()
    }

    #[test]
    fn presentation_capabilities_default_to_plaintext_without_optional_features() {
        let (server, _client) = memory_server();
        let mut ctx = server.context;
        for capabilities in [
            lsp::ClientCapabilities::default(),
            serde_json::from_value(
                json!({"textDocument": {"completion": {"completionItem": {}}, "hover": {}}}),
            )
            .unwrap(),
            presentation_capabilities(json!([]), false),
        ] {
            ctx.set_presentation_capabilities(&capabilities);
            assert!(!ctx.completion_snippets);
            assert!(!ctx.completion_label_details);
            assert_eq!(
                ctx.completion_documentation_format,
                lsp::MarkupKind::PlainText
            );
            assert_eq!(ctx.hover_format, lsp::MarkupKind::PlainText);
        }
    }

    #[test]
    fn presentation_capabilities_respect_format_preference_and_reset() {
        let (server, _client) = memory_server();
        let mut ctx = server.context;
        for (formats, expected) in [
            (json!(["markdown", "plaintext"]), lsp::MarkupKind::Markdown),
            (json!(["plaintext", "markdown"]), lsp::MarkupKind::PlainText),
            (json!(["markdown"]), lsp::MarkupKind::Markdown),
        ] {
            ctx.set_presentation_capabilities(&presentation_capabilities(formats, true));
            assert!(ctx.completion_snippets);
            assert!(ctx.completion_label_details);
            assert_eq!(ctx.completion_documentation_format, expected);
            assert_eq!(ctx.hover_format, expected);
        }
        ctx.set_presentation_capabilities(&lsp::ClientCapabilities::default());
        assert!(!ctx.completion_snippets);
        assert!(!ctx.completion_label_details);
        assert_eq!(
            ctx.completion_documentation_format,
            lsp::MarkupKind::PlainText
        );
        assert_eq!(ctx.hover_format, lsp::MarkupKind::PlainText);
    }

    #[test]
    fn completion_features_and_formats_are_negotiated_independently() {
        let (server, _client) = memory_server();
        let mut ctx = server.context;
        ctx.set_presentation_capabilities(
            &serde_json::from_value(json!({
                "textDocument": {
                    "completion": {"completionItem": {
                        "snippetSupport": true,
                        "documentationFormat": ["plaintext", "markdown"]
                    }},
                    "hover": {"contentFormat": ["markdown", "plaintext"]}
                }
            }))
            .unwrap(),
        );
        let mut completion = serde_json::from_value(json!([{
            "label": "Foo", "labelDetails": {"detail": "()"},
            "insertText": "Foo()", "insertTextFormat": 2,
            "documentation": {"kind": "markdown", "value": "---\nDocs."}
        }]))
        .unwrap();
        ctx.adapt_completion(&mut completion);
        let result = serde_json::to_value(completion).unwrap();
        assert_eq!(result[0]["insertText"], "Foo()");
        assert_eq!(result[0]["insertTextFormat"], 2);
        assert!(result[0].get("labelDetails").is_none());
        assert_eq!(
            result[0]["documentation"],
            json!({"kind": "plaintext", "value": "Docs."})
        );
        assert_eq!(ctx.hover_format, lsp::MarkupKind::Markdown);

        ctx.set_presentation_capabilities(
            &serde_json::from_value(json!({
                "textDocument": {"completion": {"completionItem": {"labelDetailsSupport": true}}}
            }))
            .unwrap(),
        );
        let mut completion = serde_json::from_value(json!([{
            "label": "Foo", "labelDetails": {"detail": "()"},
            "insertText": "Foo(${1:value})", "insertTextFormat": 2
        }]))
        .unwrap();
        ctx.adapt_completion(&mut completion);
        let result = serde_json::to_value(completion).unwrap();
        assert_eq!(result[0]["labelDetails"], json!({"detail": "()"}));
        assert_eq!(result[0]["insertText"], "Foo");
        assert_eq!(result[0]["insertTextFormat"], 1);
    }

    #[test]
    fn plaintext_removes_generated_markdown_but_preserves_code() {
        assert_eq!(
            plaintext_markup(
                "### Signature\n```\nFoo(value: Int32) -> Bool\n```\n---\nDocs with `code`.\n---\n### Symbol\n```\nFoo\n```\n"
            ),
            "Signature\nFoo(value: Int32) -> Bool\n\nDocs with code.\n\nSymbol\nFoo"
        );
        assert_eq!(
            plaintext_markup("### Field\n```value: Int32```\n---\nThe stored value.\n\n"),
            "Field\nvalue: Int32\n\nThe stored value."
        );
        assert_eq!(
            plaintext_markup("### Type\n```\nInt32\n```\n"),
            "Type\nInt32"
        );
        assert_eq!(
            plaintext_markup("```reds\n### literal\n---\n`value`\n```"),
            "### literal\n---\n`value`"
        );
        let docs = crate::display::DocDisplay::new(&["/// Example `value`."]).to_string();
        assert_eq!(plaintext_markup(&docs), "Example value.");
    }

    #[test]
    fn dispatch_negotiates_hover_and_both_completion_shapes_without_changing_source() {
        let params = json!({
            "textDocument": {"uri": "file:///C:/test.reds"},
            "position": {"line": 0, "character": 3}
        });
        let hover = json!({
            "contents": {"kind": "markdown", "value": "### Field\n```value: Int32```\n---\nDocs.\n"},
            "range": {"start": {"line": 0, "character": 0}, "end": {"line": 0, "character": 3}}
        });
        let items = json!([{
            "label": "Foo",
            "labelDetails": {"detail": "(value: Int32) -> Bool"},
            "detail": "(value: Int32) -> Bool",
            "kind": 2,
            "insertText": "Foo(${1:value})",
            "insertTextFormat": 2,
            "documentation": {"kind": "markdown", "value": "---\nDocs with `code`.\n"}
        }, {"label": "value", "insertText": "value", "insertTextFormat": 1}]);
        for list in [false, true] {
            let source = if list {
                json!({"isIncomplete": true, "items": items})
            } else {
                items.clone()
            };
            let (mut server, _client) = memory_server();
            server.server = Box::new(StubServer {
                hover: Some(Some(serde_json::from_value(hover.clone()).unwrap())),
                completion: Some(serde_json::from_value(source.clone()).unwrap()),
                ..Default::default()
            });
            // Repeated requests with changing capabilities must not mutate semantic results.
            for formats in [
                json!([]),
                json!(["markdown", "plaintext"]),
                json!(["plaintext", "markdown"]),
            ] {
                let markdown = formats == json!(["markdown", "plaintext"]);
                server
                    .context
                    .set_presentation_capabilities(&presentation_capabilities(formats, markdown));
                let result = server
                    .dispatch(lsp::request::Completion::METHOD, params.clone())
                    .unwrap();
                let result_items = if list { &result["items"] } else { &result };
                if markdown {
                    assert_eq!(result, source);
                } else {
                    assert_eq!(result_items[0]["insertText"], "Foo");
                    assert_eq!(result_items[0]["insertTextFormat"], 1);
                    assert!(result_items[0].get("labelDetails").is_none());
                    assert_eq!(result_items[0]["detail"], items[0]["detail"]);
                    assert_eq!(
                        result_items[0]["documentation"],
                        json!({"kind": "plaintext", "value": "Docs with code."})
                    );
                    assert_eq!(result_items[1], items[1]);
                    if list {
                        assert_eq!(result["isIncomplete"], true);
                    }
                }
                let result = server
                    .dispatch(lsp::request::HoverRequest::METHOD, params.clone())
                    .unwrap();
                assert_eq!(result["range"], hover["range"]);
                if markdown {
                    assert_eq!(result, hover);
                } else {
                    assert_eq!(
                        result["contents"],
                        json!({"kind": "plaintext", "value": "Field\nvalue: Int32\n\nDocs."})
                    );
                }
            }
        }
    }

    #[test]
    fn plaintext_hover_handles_legacy_contents_and_leaves_plaintext_literal() {
        let (server, _client) = memory_server();
        for (contents, expected) in [
            (json!("### Type\n```\nInt32\n```"), "Type\nInt32"),
            (
                json!(["### Type", {"language": "reds", "value": "`literal`"}]),
                "Type\n\n`literal`",
            ),
            (
                json!({"kind": "plaintext", "value": "### literal"}),
                "### literal",
            ),
        ] {
            let mut hover = serde_json::from_value(json!({"contents": contents})).unwrap();
            server.context.adapt_hover(&mut hover);
            assert_eq!(
                serde_json::to_value(hover).unwrap()["contents"],
                json!({"kind": "plaintext", "value": expected})
            );
        }
    }

    #[test]
    fn document_symbol_dispatch_negotiates_hierarchy_and_advertises_provider() {
        let uri = "file:///C:/test.reds";
        let range = json!({
            "start": {"line": 0, "character": 0},
            "end": {"line": 0, "character": 7}
        });
        // The buffer is é😀test: test starts at UTF-16 column 3, not byte offset 6.
        let selection = json!({
            "start": {"line": 0, "character": 3},
            "end": {"line": 0, "character": 7}
        });
        let source = json!([{
            "name": "é😀", "detail": "class", "kind": 5,
            "range": range, "selectionRange": selection,
            "children": [{
                "name": "test", "kind": 6, "tags": [1], "deprecated": true,
                "range": selection, "selectionRange": selection,
                "children": [{
                    "name": "local", "kind": 13,
                    "range": selection, "selectionRange": selection
                }]
            }]
        }, {
            "name": "top", "kind": 12,
            "range": selection, "selectionRange": selection
        }]);
        let flat = json!([
            {"name": "é😀", "kind": 5, "location": {"uri": uri, "range": range}},
            {"name": "test", "kind": 6, "tags": [1], "deprecated": true,
             "location": {"uri": uri, "range": selection}, "containerName": "é😀"},
            {"name": "local", "kind": 13,
             "location": {"uri": uri, "range": selection}, "containerName": "test"},
            {"name": "top", "kind": 12, "location": {"uri": uri, "range": selection}}
        ]);
        let (mut server, _client) = memory_server();
        let events = Arc::new(Mutex::new(vec![]));
        server.server = Box::new(StubServer {
            document_symbols: Some(serde_json::from_value(source.clone()).unwrap()),
            events: Some(events.clone()),
            ..Default::default()
        });
        let params = json!({"textDocument": {"uri": uri}});
        // Cover default capabilities, explicit support, explicit denial, and reset.
        for support in [None, Some(true), Some(false), Some(true), None] {
            if let Some(support) = support {
                server.context.set_presentation_capabilities(
                    &serde_json::from_value(json!({"textDocument": {"documentSymbol": {
                        "hierarchicalDocumentSymbolSupport": support
                    }}}))
                    .unwrap(),
                );
            } else {
                server
                    .context
                    .set_presentation_capabilities(&lsp::ClientCapabilities::default());
            }
            let result = server
                .dispatch(lsp::request::DocumentSymbolRequest::METHOD, params.clone())
                .unwrap();
            let expected = if support == Some(true) {
                &source
            } else {
                &flat
            };
            assert_eq!(&result, expected);
        }
        assert_eq!(
            &*events.lock().unwrap(),
            &vec!["document_symbols:é😀test".to_owned(); 5]
        );
        let capabilities = serde_json::to_value(capabilities()).unwrap();
        assert_eq!(capabilities["documentSymbolProvider"], true);

        // A server already returning SymbolInformation needs no adaptation.
        server.server = Box::new(StubServer {
            document_symbols: Some(lsp::DocumentSymbolResponse::Flat(
                serde_json::from_value(flat.clone()).unwrap(),
            )),
            ..Default::default()
        });
        assert_eq!(
            server
                .dispatch(lsp::request::DocumentSymbolRequest::METHOD, params)
                .unwrap(),
            flat
        );
    }

    #[test]
    fn code_action_dispatch_preserves_context_and_converts_utf16_range() {
        let (mut server, _client) = memory_server();
        let events = Arc::new(Mutex::new(vec![]));
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
            &*events.lock().unwrap(),
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

    #[test]
    fn code_action_dispatch_inlines_edits_without_lazy_edit_support() {
        let (mut server, _client) = memory_server();
        server.server = Box::new(StubServer {
            code_actions: vec![lsp::CodeAction {
                title: "Fix".into(),
                kind: Some(lsp::CodeActionKind::QUICKFIX),
                data: Some(json!({"fix": 1})),
                ..Default::default()
            }],
            ..Default::default()
        });
        server
            .context
            .set_code_action_capabilities(&lsp::ClientCapabilities::default());
        let actions = server
            .dispatch(
                lsp::request::CodeActionRequest::METHOD,
                code_action_params(),
            )
            .unwrap();
        let actions = actions.as_array().unwrap();
        assert_eq!(actions.len(), 1);
        assert!(actions[0]["data"].is_null());
        assert!(actions[0]["edit"]["changes"].is_object());
        assert!(actions[0]["edit"].get("documentChanges").is_none());
    }

    #[test]
    fn code_action_dispatch_and_resolve_negotiate_lazy_edits() {
        let (mut server, _client) = memory_server();
        server.server = Box::new(StubServer {
            code_actions: vec![lsp::CodeAction {
                title: "Fix".into(),
                kind: Some(lsp::CodeActionKind::QUICKFIX),
                data: Some(json!({"fix": 1})),
                ..Default::default()
            }],
            ..Default::default()
        });
        server.context.set_code_action_capabilities(
            &serde_json::from_value(json!({
                "textDocument": {"codeAction": {
                    "codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}},
                    "dataSupport": true,
                    "resolveSupport": {"properties": ["edit"]},
                    "disabledSupport": true
                }},
                "workspace": {"workspaceEdit": {"documentChanges": true}}
            }))
            .unwrap(),
        );
        let actions = server
            .dispatch(
                lsp::request::CodeActionRequest::METHOD,
                code_action_params(),
            )
            .unwrap();
        let actions = actions.as_array().unwrap();
        assert_eq!(actions.len(), 1);
        assert_eq!(actions[0]["data"], json!({"fix": 1}));
        assert!(actions[0].get("edit").is_none());

        let resolved = server
            .dispatch(
                lsp::request::CodeActionResolveRequest::METHOD,
                actions[0].clone(),
            )
            .unwrap();
        assert_eq!(resolved["data"], json!({"fix": 1}));
        assert!(resolved["edit"]["documentChanges"].is_array());
        assert!(resolved["edit"].get("changes").is_none());
    }

    #[test]
    fn code_action_resolve_honors_workspace_edit_and_disabled_capabilities() {
        let (mut server, _client) = memory_server();
        let action = |data: Value| lsp::CodeAction {
            title: "Fix".into(),
            kind: Some(lsp::CodeActionKind::QUICKFIX),
            data: Some(data),
            ..Default::default()
        };

        // Resolve support without `documentChanges` produces a `changes` edit.
        server.server = Box::new(StubServer {
            code_actions: vec![action(json!({"fix": 1}))],
            ..Default::default()
        });
        server.context.set_code_action_capabilities(
            &serde_json::from_value(json!({
                "textDocument": {"codeAction": {
                    "codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}},
                    "dataSupport": true,
                    "resolveSupport": {"properties": ["edit"]}
                }}
            }))
            .unwrap(),
        );
        let actions = server
            .dispatch(
                lsp::request::CodeActionRequest::METHOD,
                code_action_params(),
            )
            .unwrap();
        let resolved = server
            .dispatch(
                lsp::request::CodeActionResolveRequest::METHOD,
                actions.as_array().unwrap()[0].clone(),
            )
            .unwrap();
        assert!(resolved["edit"]["changes"].is_object());
        assert!(resolved["edit"].get("documentChanges").is_none());

        // A stale fix is disabled only when the client advertises disabledSupport.
        for (disabled_support, expect_disabled) in [(false, false), (true, true)] {
            server.server = Box::new(StubServer {
                code_actions: vec![action(json!({"stale": true}))],
                ..Default::default()
            });
            server.context.set_code_action_capabilities(
                &serde_json::from_value(json!({
                    "textDocument": {"codeAction": {
                        "codeActionLiteralSupport": {"codeActionKind": {"valueSet": ["quickfix"]}},
                        "dataSupport": true,
                        "disabledSupport": disabled_support,
                        "resolveSupport": {"properties": ["edit"]}
                    }}
                }))
                .unwrap(),
            );
            let actions = server
                .dispatch(
                    lsp::request::CodeActionRequest::METHOD,
                    code_action_params(),
                )
                .unwrap();
            let resolved = server
                .dispatch(
                    lsp::request::CodeActionResolveRequest::METHOD,
                    actions.as_array().unwrap()[0].clone(),
                )
                .unwrap();
            assert!(resolved.get("edit").is_none());
            assert_eq!(resolved.get("disabled").is_some(), expect_disabled);
        }
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

    fn receive_show_message(client: &Connection, typ: lsp::MessageType) -> String {
        let Message::Notification(notification) = client
            .receiver
            .recv_timeout(Duration::from_secs(2))
            .unwrap()
        else {
            panic!("expected showMessage notification");
        };
        assert_eq!(notification.method, lsp::notification::ShowMessage::METHOD);
        let params: lsp::ShowMessageParams = serde_json::from_value(notification.params).unwrap();
        assert_eq!(params.typ, typ);
        params.message
    }

    fn receive_request(client: &Connection) -> lsp_server::Request {
        loop {
            match client
                .receiver
                .recv_timeout(Duration::from_secs(2))
                .unwrap()
            {
                Message::Request(request) => return request,
                Message::Notification(notification) => {
                    assert_eq!(notification.method, lsp::notification::LogMessage::METHOD);
                }
                message => panic!("expected request, got {message:?}"),
            }
        }
    }

    fn register_watcher_globs(params: &Value) -> Vec<String> {
        let mut globs = Vec::new();
        for registration in params["registrations"].as_array().unwrap() {
            for watcher in registration["registerOptions"]["watchers"]
                .as_array()
                .unwrap()
            {
                globs.push(watcher["globPattern"].as_str().unwrap().to_owned());
            }
        }
        globs
    }

    fn code_action_params() -> Value {
        json!({
            "textDocument": {"uri": "file:///C:/test.reds"},
            "range": {"start": {"line": 0, "character": 0}, "end": {"line": 0, "character": 0}},
            "context": {"diagnostics": []}
        })
    }

    fn record_changes(server: &mut LspServer<'_>) -> Arc<Mutex<Vec<String>>> {
        let events = Arc::new(Mutex::new(Vec::new()));
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
    fn queued_changes_apply_incrementally_and_rebuild_once_per_batch() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        server.dynamic_watch_files = true;
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
            *events.lock().unwrap(),
            [
                "change:éAC!est:Some(3)",
                "change:second:Some(2)",
                // Edits only change overlays, never the watched roots.
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
            *events.lock().unwrap(),
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

        assert_eq!(*events.lock().unwrap(), ["change:accepted!:Some(4)"]);
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
    fn queued_changes_skip_rejected_versions_and_still_coalesce_per_uri() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let uri = "file:///C:/test.reds";
        queue_change(&client, uri, 2, json!([{ "text": "second" }]));
        queue_change(
            &client,
            uri,
            3,
            json!([{
                "range": { "start": { "line": 9, "character": 0 }, "end": { "line": 9, "character": 1 } },
                "text": "rejected"
            }]),
        );
        queue_change(&client, uri, 4, json!([{ "text": "fourth" }]));
        queue_shutdown(&client);

        server.run().unwrap();

        // The rejected version is logged but must not stop the batch, and the
        // URI still rebuilds exactly once with the last accepted contents.
        assert_eq!(*events.lock().unwrap(), ["change:fourth:Some(4)"]);
        let buffer = server.buffers.get(&uri.parse().unwrap()).unwrap();
        assert_eq!(buffer.contents().to_string(), "fourth");
        assert_eq!(buffer.version(), Some(4));
        receive_log(&client, lsp::MessageType::INFO);
        let error = receive_log(&client, lsp::MessageType::ERROR);
        assert!(error.contains("invalid range"), "{error}");
        receive_show_message(&client, lsp::MessageType::ERROR);
        receive_log(&client, lsp::MessageType::INFO);
        receive_response(&client, 99);
    }

    #[test]
    fn queued_changes_flush_when_a_non_change_notification_arrives() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let uri = "file:///C:/test.reds";
        queue_change(&client, uri, 2, json!([{ "text": "first" }]));
        queue_change(&client, uri, 3, json!([{ "text": "second" }]));
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeConfiguration::METHOD.into(),
                json!({"settings": {}}),
            )))
            .unwrap();
        queue_change(&client, uri, 4, json!([{ "text": "third" }]));
        queue_shutdown(&client);

        server.run().unwrap();

        // Any queued notification is a barrier: the first two changes coalesce,
        // and the third rebuilds separately.
        assert_eq!(
            *events.lock().unwrap(),
            ["change:second:Some(3)", "files:0", "change:third:Some(4)"]
        );
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
        assert_eq!(*events.lock().unwrap(), ["change:final:Some(2)"]);
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
                None,
                json!([]),
                "hover=empty",
            ),
            (
                1,
                lsp::request::HoverRequest::METHOD,
                Some(json!({ "kind": "markdown", "value": "PRIVATE_HOVER" })),
                json!([]),
                "hover=nonempty",
            ),
            (
                2,
                lsp::request::Completion::METHOD,
                None,
                json!([{ "label": "PRIVATE_COMPLETION" }]),
                "completion_count=1",
            ),
            (
                3,
                lsp::request::Completion::METHOD,
                None,
                json!({ "isIncomplete": false, "items": [] }),
                "completion_count=0",
            ),
        ] {
            server.server = Box::new(StubServer {
                hover: Some(hover_contents.map(|contents| {
                    serde_json::from_value(json!({ "contents": contents })).unwrap()
                })),
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
            let response = receive_response(&client, id);
            if id == 0 {
                assert_eq!(response.response_result.unwrap(), Value::Null);
            }
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
    fn non_file_notifications_are_ignored_without_errors_or_buffer_leaks() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let uri = "untitled:Untitled-1";
        for (method, params, expected) in [
            (
                lsp::notification::DidOpenTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "languageId": "redscript", "version": 1, "text": "func Test() {}" } }),
                "Document open ignored (non-file URI)",
            ),
            (
                lsp::notification::DidChangeTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri, "version": 2 }, "contentChanges": [{ "text": "changed" }] }),
                "Document change ignored (non-file URI)",
            ),
            (
                lsp::notification::DidSaveTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri } }),
                "Document save ignored (non-file URI)",
            ),
            (
                lsp::notification::DidCloseTextDocument::METHOD,
                json!({ "textDocument": { "uri": uri } }),
                "Document close ignored (non-file URI)",
            ),
        ] {
            server
                .handle(Message::Notification(lsp_server::Notification::new(
                    method.into(),
                    params,
                )))
                .unwrap();
            let log = receive_log(&client, lsp::MessageType::INFO);
            assert!(log.starts_with(expected), "{log}");
            assert!(server.buffers.get(&uri.parse().unwrap()).is_none());
        }
        server
            .handle(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeWatchedFiles::METHOD.into(),
                json!({
                    "changes": [
                        { "uri": "git:/repo/test.reds", "type": 2 },
                        { "uri": "file:///C:/test.reds", "type": 2 }
                    ]
                }),
            )))
            .unwrap();
        assert_eq!(*events.lock().unwrap(), ["files:1"]);
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn invalid_incremental_change_logs_error_and_preserves_buffer() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let uri = "file:///C:/test.reds";
        server
            .handle(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeTextDocument::METHOD.into(),
                json!({
                    "textDocument": { "uri": uri, "version": 2 },
                    "contentChanges": [{
                        "range": { "start": { "line": 5, "character": 0 }, "end": { "line": 5, "character": 1 } },
                        "text": "broken"
                    }]
                }),
            )))
            .unwrap();
        let log = receive_log(&client, lsp::MessageType::ERROR);
        assert!(
            log.contains("Document change rejected (invalid range"),
            "{log}"
        );
        assert!(log.contains("reopen or save"), "{log}");
        let popup = receive_show_message(&client, lsp::MessageType::ERROR);
        assert!(
            popup.contains("Document change rejected (invalid range"),
            "{popup}"
        );
        assert!(popup.contains("reopen or save"), "{popup}");
        assert!(events.lock().unwrap().is_empty());
        let buffer = server.buffers.get(&uri.parse().unwrap()).unwrap();
        assert_eq!(buffer.contents().to_string(), "é😀test");
        assert_eq!(buffer.version(), Some(1));
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
            let error = receive_response(&client, id as i32)
                .response_result
                .unwrap_err();
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
            receive_response(&client, success_id)
                .response_result
                .unwrap(),
            json!([])
        );
        assert_eq!(
            receive_response(&client, success_id + 1)
                .response_result
                .unwrap(),
            Value::Null
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
        assert_eq!(
            receive_response(&client, 0).response_result.unwrap(),
            Value::Null
        );
        for id in 1..=3 {
            if id == 2 {
                receive_log(&client, lsp::MessageType::INFO);
            }
            let log = receive_log(&client, lsp::MessageType::ERROR);
            assert!(log.contains(&format!("id={id}")));
            assert!(log.contains("code=-32600 message=Server is shut down"));
            assert_eq!(
                receive_response(&client, id)
                    .response_result
                    .unwrap_err()
                    .code,
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
        assert!(!server.shutdown);
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn shutdown_then_exit_returns_cleanly() {
        let (mut server, client) = memory_server();
        send_request(&client, 7, lsp::request::Shutdown::METHOD, Value::Null);
        send_exit(&client);
        server.run().unwrap();
        assert_eq!(
            receive_response(&client, 7).response_result.unwrap(),
            Value::Null
        );
        assert!(server.shutdown);
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn watch_roots_apply_after_intervening_document_state_changes() {
        let (mut server, client) = memory_server();
        server.dynamic_watch_files = true;
        // A root-changing command queued at `roots_rev = 1` stays current even
        // after intervening edits advance `state_rev`, which only governs
        // diagnostics and must not drop the watcher update.
        server.roots_rev = 1;
        server.state_rev = 7;
        server
            .handle_worker_event(
                WorkerEvent::WatchRoots {
                    roots_rev: 1,
                    roots: vec![PathBuf::from("C:/workspace")],
                },
                None,
            )
            .unwrap();
        let register = receive_request(&client);
        assert_eq!(register.id, RequestId::from("source-watchers".to_owned()));
        assert_eq!(
            register_watcher_globs(&register.params),
            [
                "C:/workspace/**/*.reds",
                "C:/workspace/**/*.[sS][cC][rR][iI][pP][tT]",
                "C:/workspace/.redscript",
            ]
        );

        // A superseded root update (a newer root-changing command was queued) is
        // dropped instead of clobbering the newer watchers.
        server.roots_rev = 2;
        server
            .handle_worker_event(
                WorkerEvent::WatchRoots {
                    roots_rev: 1,
                    roots: vec![PathBuf::from("C:/stale")],
                },
                None,
            )
            .unwrap();
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn dynamic_watcher_registration_tracks_workspace_folders() {
        let (mut server, client) = memory_server();
        server.dynamic_watch_files = true;
        server.server = Box::new(StubServer {
            watch_roots: vec![PathBuf::from("C:/workspace")],
            ..Default::default()
        });

        server
            .handle(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Initialized::METHOD.into(),
                Value::Null,
            )))
            .unwrap();
        let register = receive_request(&client);
        assert_eq!(register.id, RequestId::from("source-watchers".to_owned()));
        assert_eq!(register.method, lsp::request::RegisterCapability::METHOD);
        assert_eq!(register.params["registrations"][0]["id"], "source-watchers");
        assert_eq!(
            register.params["registrations"][0]["method"],
            lsp::notification::DidChangeWatchedFiles::METHOD
        );
        assert_eq!(
            register_watcher_globs(&register.params),
            [
                "C:/workspace/**/*.reds",
                "C:/workspace/**/*.[sS][cC][rR][iI][pP][tT]",
                "C:/workspace/.redscript",
            ]
        );

        server
            .handle(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeWorkspaceFolders::METHOD.into(),
                json!({"event": {
                    "added": [{"uri": "file:///C:/other", "name": "other"}],
                    "removed": [{"uri": "file:///C:/workspace", "name": "workspace"}],
                }}),
            )))
            .unwrap();
        let unregister = receive_request(&client);
        assert_eq!(
            unregister.id,
            RequestId::from("remove-source-watchers".to_owned())
        );
        assert_eq!(
            unregister.method,
            lsp::request::UnregisterCapability::METHOD
        );
        assert_eq!(
            unregister.params["unregisterations"][0]["id"],
            "source-watchers"
        );
        assert_eq!(
            unregister.params["unregisterations"][0]["method"],
            lsp::notification::DidChangeWatchedFiles::METHOD
        );
        let register = receive_request(&client);
        assert_eq!(register.id, RequestId::from("source-watchers".to_owned()));
        assert_eq!(
            register_watcher_globs(&register.params),
            [
                "C:/other/**/*.reds",
                "C:/other/**/*.[sS][cC][rR][iI][pP][tT]",
                "C:/other/.redscript",
            ]
        );

        // Acknowledging the registration must not re-register unchanged roots.
        server
            .handle(Message::Response(Response {
                id: RequestId::from("source-watchers".to_owned()),
                response_result: Ok(Value::Null),
            }))
            .unwrap();
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn watcher_registration_is_skipped_without_dynamic_registration() {
        let (mut server, client) = memory_server();
        assert!(!server.dynamic_watch_files);
        server.server = Box::new(StubServer {
            watch_roots: vec![PathBuf::from("C:/workspace")],
            ..Default::default()
        });
        for (method, params) in [
            (lsp::notification::Initialized::METHOD, Value::Null),
            (
                lsp::notification::DidChangeWorkspaceFolders::METHOD,
                json!({"event": {
                    "added": [{"uri": "file:///C:/other", "name": "other"}],
                    "removed": [{"uri": "file:///C:/workspace", "name": "workspace"}],
                }}),
            ),
        ] {
            server
                .handle(Message::Notification(lsp_server::Notification::new(
                    method.into(),
                    params,
                )))
                .unwrap();
        }
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

    #[test]
    fn workspace_folder_paths_skip_non_file_schemes() {
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let context = LspContext::new(sender);
        let folders = vec![
            lsp::WorkspaceFolder {
                uri: "file:///C:/workspace".parse().unwrap(),
                name: "workspace".into(),
            },
            lsp::WorkspaceFolder {
                uri: "untitled:Untitled-1".parse().unwrap(),
                name: "untitled".into(),
            },
            lsp::WorkspaceFolder {
                uri: "file:///C:/other".parse().unwrap(),
                name: "other".into(),
            },
        ];
        assert_eq!(
            workspace_folder_paths(folders, &context).unwrap(),
            [PathBuf::from("C:/workspace"), PathBuf::from("C:/other")]
        );
    }

    /// A stub whose `hover` and `check` block until released, so tests can drive
    /// the protocol loop while the worker is busy.
    struct BlockingStubServer {
        entered: Arc<Barrier>,
        release: Arc<Barrier>,
        events: Arc<Mutex<Vec<String>>>,
        check_batch: Option<DiagnosticBatch>,
    }

    impl LanguageServer for BlockingStubServer {
        fn watch_roots(&self) -> Vec<PathBuf> {
            Vec::new()
        }

        fn document_changed(
            &mut self,
            _path: PathBuf,
            _buffer: Option<&Buffer>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            self.events.lock().unwrap().push("change".into());
            Ok(())
        }

        fn documents_changed(
            &mut self,
            documents: &[(PathBuf, Option<&Buffer>)],
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            self.events
                .lock()
                .unwrap()
                .push(format!("batch:{}", documents.len()));
            Ok(())
        }

        fn files_changed(&mut self, _paths: Vec<PathBuf>, _ctx: &LspContext) -> anyhow::Result<()> {
            Ok(())
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
            _path: PathBuf,
            _ctx: &LspContext,
            should_abort: &(dyn Fn() -> bool + Sync),
        ) -> anyhow::Result<Option<DiagnosticBatch>> {
            self.events.lock().unwrap().push("check:start".into());
            self.entered.wait();
            self.release.wait();
            if should_abort() {
                self.events.lock().unwrap().push("check:aborted".into());
                return Ok(None);
            }
            self.events.lock().unwrap().push("check:end".into());
            Ok(self.check_batch.clone())
        }

        fn change_workspace_folders(
            &mut self,
            _added: Vec<PathBuf>,
            _removed: Vec<PathBuf>,
            _ctx: &LspContext,
        ) -> anyhow::Result<()> {
            Ok(())
        }

        fn hover(
            &self,
            _loc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<Option<lsp::Hover>> {
            self.events.lock().unwrap().push("hover:start".into());
            self.entered.wait();
            self.release.wait();
            self.events.lock().unwrap().push("hover:end".into());
            Ok(None)
        }

        fn completion(
            &self,
            _loc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<Arc<lsp::CompletionResponse>> {
            Ok(Arc::new(lsp::CompletionResponse::Array(Vec::new())))
        }

        fn goto_definition(
            &self,
            _loc: CodeLocation<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::GotoDefinitionResponse> {
            Ok(lsp::GotoDefinitionResponse::Array(Vec::new()))
        }

        fn document_symbols(
            &self,
            _doc: Document<'_>,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::DocumentSymbolResponse> {
            Ok(lsp::DocumentSymbolResponse::Nested(Vec::new()))
        }

        fn workspace_symbol(
            &self,
            _query: &str,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::WorkspaceSymbolResponse> {
            Ok(lsp::WorkspaceSymbolResponse::Nested(Vec::new()))
        }

        fn format(
            &self,
            _doc: Document<'_>,
            _tab_size: u16,
            _ctx: &LspContext,
        ) -> anyhow::Result<Vec<lsp::TextEdit>> {
            Ok(Vec::new())
        }

        fn code_action(
            &self,
            _path: PathBuf,
            _span: Range<u32>,
            _context: lsp::CodeActionContext,
            _ctx: &LspContext,
        ) -> anyhow::Result<Vec<lsp::CodeActionOrCommand>> {
            Ok(Vec::new())
        }

        fn code_action_resolve(
            &self,
            _action: lsp::CodeAction,
            _ctx: &LspContext,
        ) -> anyhow::Result<lsp::CodeAction> {
            bail!("no code actions")
        }
    }

    fn blocking_server(
        server: &mut LspServer<'static>,
        check_batch: Option<DiagnosticBatch>,
    ) -> (Arc<Barrier>, Arc<Barrier>, Arc<Mutex<Vec<String>>>) {
        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let events = Arc::new(Mutex::new(Vec::new()));
        server.server = Box::new(BlockingStubServer {
            entered: entered.clone(),
            release: release.clone(),
            events: events.clone(),
            check_batch,
        });
        (entered, release, events)
    }

    fn publish_notifications(
        receiver: &crossbeam_channel::Receiver<Message>,
    ) -> Vec<lsp::PublishDiagnosticsParams> {
        receiver
            .try_iter()
            .filter_map(|message| {
                let Message::Notification(notification) = message else {
                    return None;
                };
                (notification.method == "textDocument/publishDiagnostics")
                    .then(|| serde_json::from_value(notification.params).unwrap())
            })
            .collect()
    }

    #[test]
    fn protocol_loop_cancels_in_flight_requests_without_blocking() {
        let (mut server, client) = memory_server();
        let (entered, release, events) = blocking_server(&mut server, None);

        let uri = "file:///C:/test.reds";
        send_request(
            &client,
            1,
            lsp::request::HoverRequest::METHOD,
            json!({ "textDocument": { "uri": uri }, "position": { "line": 0, "character": 3 } }),
        );
        let handle = std::thread::spawn(move || server.run());

        // The worker is now blocked inside `hover`; the protocol thread must
        // still answer a cancellation without waiting for it.
        entered.wait();
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 1 }),
            )))
            .unwrap();
        let response = receive_response(&client, 1);
        assert_eq!(
            response.response_result.unwrap_err().code,
            ErrorCode::RequestCanceled as i32
        );

        // Let the worker finish; its late result must be dropped, not sent twice.
        release.wait();
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();
        receive_response(&client, 99);
        assert_eq!(*events.lock().unwrap(), ["hover:start", "hover:end"]);
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn stale_diagnostic_publication_is_rejected() {
        let (mut server, client) = memory_server();
        let path = PathBuf::from("C:/test.reds");
        let batch = DiagnosticBatch {
            compiler: BTreeMap::from([(
                path.clone(),
                vec![lsp::Diagnostic {
                    range: lsp::Range::default(),
                    message: "stale".into(),
                    ..Default::default()
                }],
            )]),
            read_diagnostics: BTreeMap::new(),
            config_diagnostics: BTreeMap::new(),
            versions: HashMap::new(),
        };
        let (entered, release, _events) = blocking_server(&mut server, Some(batch));

        let uri = "file:///C:/test.reds";
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidSaveTextDocument::METHOD.into(),
                json!({ "textDocument": { "uri": uri } }),
            )))
            .unwrap();
        let handle = std::thread::spawn(move || server.run());

        // The check is in flight; supersede it with an edit before it publishes.
        entered.wait();
        queue_change(&client, uri, 2, json!([{ "text": "new" }]));
        // The protocol thread handles messages in order, so once it logs this
        // unknown cancellation the edit above has already advanced the state.
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 999 }),
            )))
            .unwrap();
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Cancellation received id=") {
                break;
            }
        }
        release.wait();
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();
        receive_response(&client, 99);

        // The batch computed for the superseded revision never reached the client.
        assert!(publish_notifications(&client.receiver).is_empty());
        assert!(client.receiver.try_recv().is_err());
    }

    #[test]
    fn startup_diagnostics_are_computed_off_the_protocol_loop() {
        let (mut server, client) = memory_server();
        server.startup_pending = true;
        let path = PathBuf::from("C:/test.reds");
        server.server = Box::new(StubServer {
            startup_batch: Some(DiagnosticBatch {
                compiler: BTreeMap::from([(
                    path.clone(),
                    vec![lsp::Diagnostic {
                        range: lsp::Range::default(),
                        message: "startup".into(),
                        ..Default::default()
                    }],
                )]),
                read_diagnostics: BTreeMap::new(),
                config_diagnostics: BTreeMap::new(),
                versions: HashMap::from([(path.clone(), 1)]),
            }),
            ..Default::default()
        });

        // Shutdown is queued before the loop starts, so the startup batch is
        // drained and published while handling the shutdown barrier.
        queue_shutdown(&client);
        server.run().unwrap();

        let messages: Vec<Message> = client.receiver.try_iter().collect();
        let published: Vec<lsp::PublishDiagnosticsParams> = messages
            .iter()
            .filter_map(|message| {
                let Message::Notification(notification) = message else {
                    return None;
                };
                (notification.method == "textDocument/publishDiagnostics")
                    .then(|| serde_json::from_value(notification.params.clone()).unwrap())
            })
            .collect();
        assert_eq!(published.len(), 1);
        assert_eq!(published[0].uri, uri_from_path(&path).unwrap());
        assert_eq!(published[0].version, Some(1));
        assert_eq!(published[0].diagnostics[0].message, "startup");
        assert!(messages.iter().any(
            |message| matches!(message, Message::Response(response) if response.id == RequestId::from(99))
        ));
    }

    #[test]
    fn startup_diagnostics_retry_after_launch_time_open() {
        let (mut server, client) = memory_server();
        server.startup_pending = true;
        let path = PathBuf::from("C:/test.reds");
        let batch = DiagnosticBatch {
            compiler: BTreeMap::from([(
                path.clone(),
                vec![lsp::Diagnostic {
                    range: lsp::Range::default(),
                    message: "startup".into(),
                    ..Default::default()
                }],
            )]),
            read_diagnostics: BTreeMap::new(),
            config_diagnostics: BTreeMap::new(),
            versions: HashMap::from([(path.clone(), 1)]),
        };
        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let events = Arc::new(Mutex::new(Vec::new()));
        server.server = Box::new(StubServer {
            startup_batch: Some(batch),
            startup_entered: Some(entered.clone()),
            startup_release: Some(release.clone()),
            abort_startup: true,
            allow_document_changes: true,
            events: Some(events.clone()),
            ..Default::default()
        });

        let handle = std::thread::spawn(move || server.run());
        // The worker is computing the initial batch; supersede it with a
        // launch-time open, which is not an edit.
        entered.wait();
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidOpenTextDocument::METHOD.into(),
                json!({
                    "textDocument": {
                        "uri": "file:///C:/test.reds",
                        "languageId": "redscript",
                        "version": 1,
                        "text": "test"
                    }
                }),
            )))
            .unwrap();
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Document open") {
                break;
            }
        }
        release.wait();
        // Wait until the aborted attempt has been observed and a retry queued.
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Diagnostic computation aborted") {
                break;
            }
        }
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();

        let published = publish_notifications(&client.receiver);
        assert_eq!(published.len(), 1, "{published:?}");
        assert_eq!(published[0].uri, uri_from_path(&path).unwrap());
        assert_eq!(published[0].diagnostics[0].message, "startup");
        assert_eq!(
            events
                .lock()
                .unwrap()
                .iter()
                .filter(|event| *event == "startup")
                .count(),
            2,
            "the aborted initial computation must be retried once"
        );
    }

    #[test]
    fn worker_coalesces_queued_document_changes() {
        let (mut server, client) = memory_server();
        let (entered, release, events) = blocking_server(&mut server, None);
        let uri = "file:///C:/test.reds";
        send_request(
            &client,
            1,
            lsp::request::HoverRequest::METHOD,
            json!({ "textDocument": { "uri": uri }, "position": { "line": 0, "character": 3 } }),
        );
        let handle = std::thread::spawn(move || server.run());

        // Keep the worker busy while two change batches are enqueued. The
        // cancellations in between stop the protocol thread from merging them,
        // so the worker itself has to coalesce them.
        entered.wait();
        queue_change(&client, uri, 2, json!([{ "text": "one" }]));
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 999 }),
            )))
            .unwrap();
        queue_change(&client, uri, 3, json!([{ "text": "two" }]));
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 998 }),
            )))
            .unwrap();
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Cancellation received id=998") {
                break;
            }
        }
        release.wait();
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();
        receive_response(&client, 1);
        receive_response(&client, 99);

        let events = events.lock().unwrap();
        assert_eq!(
            events
                .iter()
                .filter(|event| event.starts_with("batch:"))
                .count(),
            1,
            "queued changes must produce a single rebuild: {events:?}"
        );
        // Both queued batches target the same document, so only its newest
        // buffer may reach the server.
        assert!(events.iter().any(|event| event == "batch:1"), "{events:?}");
    }

    #[test]
    fn promote_request_moves_only_requests_past_background_diagnostics() {
        let request = || WorkerCmd::Request {
            id: RequestId::from("1".to_owned()),
            method: lsp::request::WorkspaceSymbolRequest::METHOD.to_owned(),
            request: Box::new(WorkerRequest::WorkspaceSymbol(String::new())),
            started: Instant::now(),
            cancel: CancelToken::default(),
        };
        let startup = || WorkerCmd::StartupDiagnostics {
            state_rev: 0,
            diag_rev: 0,
        };
        let check = || WorkerCmd::Check {
            state_rev: 0,
            diag_rev: 0,
            roots_rev: 0,
            path: PathBuf::from("C:/test.reds"),
        };
        let documents = || WorkerCmd::DocumentsChanged { documents: vec![] };

        // A request is promoted past the background diagnostics in front of it.
        let mut queue = VecDeque::from([startup(), check(), request()]);
        assert!(promote_request(&mut queue));
        assert!(matches!(queue.front(), Some(WorkerCmd::Request { .. })));

        // A state mutation keeps its position, so nothing is promoted past it.
        let mut queue = VecDeque::from([startup(), documents(), request()]);
        assert!(!promote_request(&mut queue));
        assert!(matches!(
            queue.front(),
            Some(WorkerCmd::StartupDiagnostics { .. })
        ));

        // The shutdown barrier is never crossed.
        let (ack, _rx) = crossbeam_channel::unbounded();
        let mut queue = VecDeque::from([startup(), WorkerCmd::Flush(ack), request()]);
        assert!(!promote_request(&mut queue));

        // Nothing to promote when no request is queued, or the head is not a
        // background diagnostic.
        assert!(!promote_request(&mut VecDeque::from([startup(), check()])));
        assert!(!promote_request(&mut VecDeque::from([
            request(),
            startup()
        ])));
    }

    #[test]
    fn worker_runs_a_queued_request_before_background_diagnostics() {
        let (cmd_tx, cmd_rx) = crossbeam_channel::unbounded();
        let (event_tx, _event_rx) = crossbeam_channel::unbounded();
        let events = Arc::new(Mutex::new(Vec::new()));
        let entered = Arc::new(Barrier::new(2));
        let release = Arc::new(Barrier::new(2));
        let server = Box::new(StubServer {
            events: Some(events.clone()),
            startup_entered: Some(entered.clone()),
            startup_release: Some(release.clone()),
            ..Default::default()
        });
        let (sender, _receiver) = crossbeam_channel::unbounded();
        let handle = std::thread::spawn(move || {
            worker_main(
                server,
                LspContext::new(sender),
                cmd_rx,
                event_tx,
                Arc::new(AtomicU64::new(0)),
                Arc::new(AtomicU64::new(0)),
                false,
            );
        });

        // The first diagnostics blocks the worker; a newer diagnostics and an
        // interactive request queue up behind it.
        cmd_tx
            .send(WorkerCmd::StartupDiagnostics {
                state_rev: 0,
                diag_rev: 0,
            })
            .unwrap();
        entered.wait();
        cmd_tx
            .send(WorkerCmd::StartupDiagnostics {
                state_rev: 0,
                diag_rev: 0,
            })
            .unwrap();
        cmd_tx
            .send(WorkerCmd::Request {
                id: RequestId::from("1".to_owned()),
                method: lsp::request::DocumentSymbolRequest::METHOD.to_owned(),
                request: Box::new(WorkerRequest::DocumentSymbols(OwnedDocument {
                    path: PathBuf::from("C:/test.reds"),
                    buffer: Buffer::new(String::new()),
                })),
                started: Instant::now(),
                cancel: CancelToken::default(),
            })
            .unwrap();
        release.wait();
        let (ack, ack_rx) = crossbeam_channel::unbounded();
        cmd_tx.send(WorkerCmd::Flush(ack)).unwrap();
        ack_rx.recv_timeout(Duration::from_secs(10)).unwrap();
        drop(cmd_tx);
        handle.join().unwrap();

        let events = events.lock().unwrap();
        let request = events
            .iter()
            .position(|event| event.starts_with("document_symbols:"))
            .unwrap_or_else(|| panic!("the request must run: {events:?}"));
        let last_startup = events
            .iter()
            .rposition(|event| event == "startup")
            .expect("both diagnostics run");
        assert!(
            request < last_startup,
            "a queued request must run before the later diagnostics: {events:?}"
        );
    }

    #[test]
    fn queued_cancelled_request_is_skipped_by_worker() {
        let (mut server, client) = memory_server();
        let (entered, release, events) = blocking_server(&mut server, None);
        let uri = "file:///C:/test.reds";
        send_request(
            &client,
            1,
            lsp::request::HoverRequest::METHOD,
            json!({ "textDocument": { "uri": uri }, "position": { "line": 0, "character": 3 } }),
        );
        let handle = std::thread::spawn(move || server.run());

        entered.wait();
        send_request(
            &client,
            2,
            lsp::request::HoverRequest::METHOD,
            json!({ "textDocument": { "uri": uri }, "position": { "line": 0, "character": 3 } }),
        );
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 2 }),
            )))
            .unwrap();
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Request cancelled id=2") {
                break;
            }
        }
        release.wait();
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();

        let responses: Vec<Response> = client
            .receiver
            .try_iter()
            .filter_map(|message| match message {
                Message::Response(response) => Some(response),
                _ => None,
            })
            .collect();
        let cancelled = responses
            .iter()
            .find(|response| response.id == RequestId::from(2))
            .expect("cancelled request response");
        assert_eq!(
            cancelled.response_result.as_ref().unwrap_err().code,
            ErrorCode::RequestCanceled as i32
        );
        assert!(
            responses
                .iter()
                .any(|response| response.id == RequestId::from(1)
                    && response.response_result.is_ok())
        );
        // The cancelled queued request never reached the handler.
        assert_eq!(*events.lock().unwrap(), ["hover:start", "hover:end"]);
    }

    #[test]
    fn superseded_checks_abort_before_recomputing() {
        let (mut server, client) = memory_server();
        let path = PathBuf::from("C:/test.reds");
        let batch = DiagnosticBatch {
            compiler: BTreeMap::from([(
                path.clone(),
                vec![lsp::Diagnostic {
                    range: lsp::Range::default(),
                    message: "saved".into(),
                    ..Default::default()
                }],
            )]),
            read_diagnostics: BTreeMap::new(),
            config_diagnostics: BTreeMap::new(),
            versions: HashMap::from([(path.clone(), 1)]),
        };
        let (entered, release, events) = blocking_server(&mut server, Some(batch));
        let uri = "file:///C:/test.reds";
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidSaveTextDocument::METHOD.into(),
                json!({ "textDocument": { "uri": uri } }),
            )))
            .unwrap();
        let handle = std::thread::spawn(move || server.run());

        // The first check is blocked; a second save supersedes it.
        entered.wait();
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidSaveTextDocument::METHOD.into(),
                json!({ "textDocument": { "uri": uri } }),
            )))
            .unwrap();
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 999 }),
            )))
            .unwrap();
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Cancellation received id=999") {
                break;
            }
        }
        release.wait();
        // Let the superseded check finish, then serve the latest save.
        entered.wait();
        release.wait();
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();

        let events = events.lock().unwrap();
        assert_eq!(
            events
                .iter()
                .filter(|event| *event == "check:aborted")
                .count(),
            1,
            "{events:?}"
        );
        assert_eq!(
            events.iter().filter(|event| *event == "check:end").count(),
            1,
            "{events:?}"
        );
        let published = publish_notifications(&client.receiver);
        assert_eq!(published.len(), 1, "{published:?}");
        assert_eq!(published[0].diagnostics[0].message, "saved");
    }

    #[test]
    fn save_check_superseded_by_watcher_event_is_retried() {
        let (mut server, client) = memory_server();
        let path = PathBuf::from("C:/test.reds");
        let batch = DiagnosticBatch {
            compiler: BTreeMap::from([(
                path.clone(),
                vec![lsp::Diagnostic {
                    range: lsp::Range::default(),
                    message: "retried".into(),
                    ..Default::default()
                }],
            )]),
            read_diagnostics: BTreeMap::new(),
            config_diagnostics: BTreeMap::new(),
            versions: HashMap::from([(path.clone(), 1)]),
        };
        let (entered, release, events) = blocking_server(&mut server, Some(batch));
        let uri = "file:///C:/test.reds";
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidSaveTextDocument::METHOD.into(),
                json!({ "textDocument": { "uri": uri } }),
            )))
            .unwrap();
        let handle = std::thread::spawn(move || server.run());

        // The save check is in flight; a `.redscript` watcher event bumps
        // `state_rev` without touching `diag_rev`, so it must be retried.
        entered.wait();
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeWatchedFiles::METHOD.into(),
                json!({ "changes": [{ "uri": "file:///C:/ws/.redscript", "type": 2 }] }),
            )))
            .unwrap();
        // The protocol thread handles messages in order, so once it logs this
        // unknown cancellation the watcher event above has advanced the state.
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::Cancel::METHOD.into(),
                json!({ "id": 999 }),
            )))
            .unwrap();
        loop {
            let log = receive_log(&client, lsp::MessageType::INFO);
            if log.contains("Cancellation received id=999") {
                break;
            }
        }
        release.wait();
        // The superseded batch is rejected, then retried at the new revision.
        entered.wait();
        release.wait();
        queue_shutdown(&client);
        handle.join().unwrap().unwrap();

        let events = events.lock().unwrap();
        assert_eq!(
            events.iter().filter(|event| *event == "check:end").count(),
            2,
            "{events:?}"
        );
        let published = publish_notifications(&client.receiver);
        assert_eq!(published.len(), 1, "{published:?}");
        assert_eq!(published[0].diagnostics[0].message, "retried");
    }

    #[test]
    fn watcher_events_for_open_sources_are_ignored_but_config_and_deletes_are_kept() {
        let (mut server, client) = memory_server();
        let events = record_changes(&mut server);
        let open = "file:///C:/test.reds";
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeWatchedFiles::METHOD.into(),
                json!({ "changes": [
                    // Open source files already override on-disk content.
                    { "uri": open, "type": 2 },
                    { "uri": open, "type": 1 },
                    // A closed source file, the workspace config, and a
                    // deletion of an open file all still refresh the workspace.
                    { "uri": "file:///C:/closed.reds", "type": 2 },
                    { "uri": "file:///C:/ws/.redscript", "type": 2 },
                    { "uri": open, "type": 3 },
                ]}),
            )))
            .unwrap();
        queue_shutdown(&client);
        server.run().unwrap();
        assert_eq!(*events.lock().unwrap(), ["files:3"]);
    }

    #[test]
    fn end_to_end_protocol_serves_real_analysis() {
        use redscript_compiler_api::ScriptBundle;

        let temp = tempfile::tempdir().unwrap();
        let cache = temp.path().join("cache.bin");
        std::fs::write(
            &cache,
            ScriptBundle::default().into_writeable().to_bytes().unwrap(),
        )
        .unwrap();
        let hover_text = "public class E2E {\n    public let value: Int32;\n}\n\npublic func Make() -> E2E {\n    return new E2E();\n}\n";
        let hover_path = temp.path().join("hover.reds");
        std::fs::write(&hover_path, hover_text).unwrap();
        let completion_text = "public func Use() {\n    let e = new E2E();\n    e.;\n}\n";
        let completion_path = temp.path().join("completion.reds");
        std::fs::write(&completion_path, completion_text).unwrap();
        let broken_path = temp.path().join("broken.reds");
        std::fs::write(&broken_path, "public class {").unwrap();

        let real = crate::ls::RedscriptLanguageServer::new(&cache, [temp.path()]).unwrap();
        let (mut server, client) = LspServer::for_test(Box::new(real), Buffers::default(), false);
        let handle = std::thread::spawn(move || server.run());

        let open = |path: &Path, text: &str| {
            let uri = uri_from_path(path).unwrap();
            client
                .sender
                .send(Message::Notification(lsp_server::Notification::new(
                    lsp::notification::DidOpenTextDocument::METHOD.into(),
                    json!({
                        "textDocument": {
                            "uri": uri.as_str(),
                            "languageId": "redscript",
                            "version": 1,
                            "text": text,
                        }
                    }),
                )))
                .unwrap();
            uri
        };
        let hover_uri = open(&hover_path, hover_text);
        let completion_uri = open(&completion_path, completion_text);
        let broken_uri = open(&broken_path, "public class {");

        let offset = hover_text.find("-> E2E").unwrap() + 4;
        let position = crate::positions::position(hover_text, offset as u32).unwrap();
        send_request(
            &client,
            1,
            lsp::request::HoverRequest::METHOD,
            json!({ "textDocument": { "uri": hover_uri.as_str() }, "position": position }),
        );
        let hover = receive_response(&client, 1).response_result.unwrap();
        assert!(
            hover["contents"]["value"]
                .as_str()
                .is_some_and(|value| value.contains("E2E")),
            "{hover}"
        );

        let offset = completion_text.rfind("e.;").unwrap() + 2;
        let position = crate::positions::position(completion_text, offset as u32).unwrap();
        send_request(
            &client,
            2,
            lsp::request::Completion::METHOD,
            json!({ "textDocument": { "uri": completion_uri.as_str() }, "position": position }),
        );
        let completion = receive_response(&client, 2).response_result.unwrap();
        let items = completion.as_array().expect("completion array");
        assert!(
            items.iter().any(|item| item["label"] == "value"),
            "{completion}"
        );

        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidSaveTextDocument::METHOD.into(),
                json!({ "textDocument": { "uri": broken_uri.as_str() } }),
            )))
            .unwrap();
        // Saving writes the file to disk, so a client with dynamic watcher
        // registration also sends a watched-file event for the same open file
        // while the save check is in flight. It must not suppress the batch.
        client
            .sender
            .send(Message::Notification(lsp_server::Notification::new(
                lsp::notification::DidChangeWatchedFiles::METHOD.into(),
                json!({ "changes": [{ "uri": broken_uri.as_str(), "type": 2 }] }),
            )))
            .unwrap();
        let published = loop {
            match client
                .receiver
                .recv_timeout(Duration::from_secs(5))
                .unwrap()
            {
                Message::Notification(notification)
                    if notification.method == lsp::notification::PublishDiagnostics::METHOD =>
                {
                    let params: lsp::PublishDiagnosticsParams =
                        serde_json::from_value(notification.params).unwrap();
                    if params.uri == broken_uri {
                        break params;
                    }
                }
                Message::Notification(_) => {}
                message => panic!("expected diagnostics, got {message:?}"),
            }
        };
        assert!(!published.diagnostics.is_empty(), "{published:?}");

        queue_shutdown(&client);
        handle.join().unwrap().unwrap();
    }

    #[test]
    fn save_resynchronizes_a_desynced_buffer() {
        use redscript_compiler_api::ScriptBundle;

        let temp = tempfile::tempdir().unwrap();
        let cache = temp.path().join("cache.bin");
        std::fs::write(
            &cache,
            ScriptBundle::default().into_writeable().to_bytes().unwrap(),
        )
        .unwrap();
        // A standalone file: it is not part of the (empty) startup workspace, so
        // the only diagnostics publication comes from the save-triggered check.
        let external = tempfile::tempdir().unwrap();
        let path = external.path().join("solo.reds");
        std::fs::write(&path, "public class Solo {").unwrap();
        let uri = uri_from_path(&path).unwrap();

        let real = crate::ls::RedscriptLanguageServer::new(&cache, [temp.path()]).unwrap();
        let (mut server, client) = LspServer::for_test(Box::new(real), Buffers::default(), false);
        let handle = std::thread::spawn(move || server.run());

        let notify = |method: &str, params: Value| {
            client
                .sender
                .send(Message::Notification(lsp_server::Notification::new(
                    method.into(),
                    params,
                )))
                .unwrap();
        };
        let wait_for_diagnostics = || -> lsp::PublishDiagnosticsParams {
            loop {
                match client
                    .receiver
                    .recv_timeout(Duration::from_secs(5))
                    .unwrap()
                {
                    Message::Notification(notification)
                        if notification.method == lsp::notification::PublishDiagnostics::METHOD =>
                    {
                        let params: lsp::PublishDiagnosticsParams =
                            serde_json::from_value(notification.params).unwrap();
                        if params.uri == uri {
                            return params;
                        }
                    }
                    Message::Notification(_) => {}
                    message => panic!("expected diagnostics, got {message:?}"),
                }
            }
        };

        notify(
            lsp::notification::DidOpenTextDocument::METHOD,
            json!({
                "textDocument": {
                    "uri": uri.as_str(),
                    "languageId": "redscript",
                    "version": 1,
                    "text": "public class Solo {",
                }
            }),
        );
        // Save the broken contents so the client has a diagnostic to clear.
        notify(
            lsp::notification::DidSaveTextDocument::METHOD,
            json!({ "textDocument": { "uri": uri.as_str() } }),
        );
        assert!(!wait_for_diagnostics().diagnostics.is_empty());

        // Reject an out-of-range incremental change: the buffer becomes desynced
        // while its stale (syntactically broken) contents stay active.
        notify(
            lsp::notification::DidChangeTextDocument::METHOD,
            json!({
                "textDocument": { "uri": uri.as_str(), "version": 2 },
                "contentChanges": [{
                    "range": {
                        "start": {"line": 99, "character": 0},
                        "end": {"line": 99, "character": 1}
                    },
                    "text": "x"
                }]
            }),
        );

        // The user fixes the file on disk and saves; the desynced buffer must be
        // reloaded so the stale error is cleared.
        std::fs::write(&path, "public class Solo {}").unwrap();
        notify(
            lsp::notification::DidSaveTextDocument::METHOD,
            json!({ "textDocument": { "uri": uri.as_str() } }),
        );
        assert!(
            wait_for_diagnostics().diagnostics.is_empty(),
            "save must resynchronize the buffer from disk"
        );

        queue_shutdown(&client);
        handle.join().unwrap().unwrap();
    }
}
