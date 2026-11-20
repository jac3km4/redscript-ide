//! Unified diagnostic state for the language server.
//!
//! All diagnostics are partitioned by the file they belong to and by the
//! subsystem that produced them (compiler, source read/UTF-8, or `.redscript`
//! configuration). A single [`DiagnosticTracker`] owns both the active
//! diagnostics and the set of files currently published to the client, so
//! there is exactly one place responsible for emitting `publishDiagnostics`
//! and for clearing files that no longer have any diagnostics.

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::path::{Path, PathBuf};

use lsp_types as lsp;
use redscript_compiler_api::{Diagnostic, LowerError, ast};

use crate::server::LspContext;

/// A fully computed set of diagnostics ready to be applied to a
/// [`DiagnosticTracker`].
///
/// Computation happens on the analysis worker thread, which has no access to
/// the protocol thread's tracker or connection. Separating computation from
/// publication lets the protocol thread reject batches produced for a state
/// that has since been superseded, so stale results never mutate the tracker
/// or reach the client.
#[derive(Clone)]
pub(crate) struct DiagnosticBatch {
    pub(crate) compiler: BTreeMap<PathBuf, Vec<lsp::Diagnostic>>,
    pub(crate) reads: BTreeMap<PathBuf, lsp::Diagnostic>,
    pub(crate) configs: BTreeMap<PathBuf, lsp::Diagnostic>,
    /// Overlay document versions captured at the analyzed snapshot, used as the
    /// `version` field of each published notification.
    pub(crate) versions: HashMap<PathBuf, i32>,
}

/// Tracks active diagnostics per file, split by their source, together with
/// the files whose diagnostics are currently published.
///
/// The lifecycle is: rebuild produces new per-source maps, [`set_compiler`],
/// [`set_reads`], and [`set_configs`] replace the corresponding partitions,
/// and [`flush`] merges everything and emits the LSP notifications.
///
/// [`set_compiler`]: DiagnosticTracker::set_compiler
/// [`set_reads`]: DiagnosticTracker::set_reads
/// [`set_configs`]: DiagnosticTracker::set_configs
/// [`flush`]: DiagnosticTracker::flush
#[derive(Default)]
pub(crate) struct DiagnosticTracker {
    compiler: BTreeMap<PathBuf, Vec<lsp::Diagnostic>>,
    reads: BTreeMap<PathBuf, lsp::Diagnostic>,
    configs: BTreeMap<PathBuf, lsp::Diagnostic>,
    published: BTreeSet<PathBuf>,
}

impl DiagnosticTracker {
    /// Replaces the compiler diagnostics for every analyzed file. Files with
    /// no diagnostics are simply absent from the map.
    pub(crate) fn set_compiler(&mut self, diagnostics: BTreeMap<PathBuf, Vec<lsp::Diagnostic>>) {
        self.compiler = diagnostics;
    }

    /// Replaces the source read/UTF-8 diagnostics.
    pub(crate) fn set_reads(&mut self, diagnostics: BTreeMap<PathBuf, lsp::Diagnostic>) {
        self.reads = diagnostics;
    }

    /// Replaces the `.redscript` configuration diagnostics.
    pub(crate) fn set_configs(&mut self, diagnostics: BTreeMap<PathBuf, lsp::Diagnostic>) {
        self.configs = diagnostics;
    }

    /// The current source read/UTF-8 diagnostics.
    pub(crate) fn reads(&self) -> &BTreeMap<PathBuf, lsp::Diagnostic> {
        &self.reads
    }

    /// Merges the active diagnostics and publishes the diff against what was
    /// previously published.
    ///
    /// Files that currently have diagnostics are always republished so the
    /// client replaces their previous list (there is no separate "clear first"
    /// notification). Files that were published but have no active diagnostics
    /// anymore are cleared with an empty list exactly once. Notifications are
    /// emitted in deterministic path order.
    pub(crate) fn flush(
        &mut self,
        ctx: &LspContext,
        version_of: impl Fn(&Path) -> Option<i32>,
    ) -> anyhow::Result<()> {
        let active: BTreeSet<PathBuf> = self
            .compiler
            .keys()
            .chain(self.reads.keys())
            .chain(self.configs.keys())
            .cloned()
            .collect();

        let mut emissions: BTreeMap<PathBuf, Vec<lsp::Diagnostic>> = active
            .iter()
            .map(|path| (path.clone(), Vec::new()))
            .collect();
        for (path, diagnostics) in &self.compiler {
            emissions
                .get_mut(path)
                .expect("active path missing")
                .extend(diagnostics.iter().cloned());
        }
        for (path, diagnostic) in &self.reads {
            emissions
                .get_mut(path)
                .expect("active path missing")
                .push(diagnostic.clone());
        }
        for (path, diagnostic) in &self.configs {
            emissions
                .get_mut(path)
                .expect("active path missing")
                .push(diagnostic.clone());
        }
        // Clear files that were published but are no longer active.
        for path in self.published.difference(&active) {
            emissions.insert(path.clone(), Vec::new());
        }

        for (path, diagnostics) in &emissions {
            ctx.notify::<lsp::notification::PublishDiagnostics>(lsp::PublishDiagnosticsParams {
                uri: ctx.uri(path)?,
                diagnostics: diagnostics.clone(),
                version: version_of(path),
            });
        }
        self.published = active;
        Ok(())
    }

    /// Applies a precomputed [`DiagnosticBatch`] and publishes the diff.
    ///
    /// This is the worker-thread-friendly entry point: the batch is produced
    /// elsewhere and only committed here, so rejecting a stale batch leaves the
    /// tracker untouched.
    pub(crate) fn apply_and_flush(
        &mut self,
        batch: DiagnosticBatch,
        ctx: &LspContext,
    ) -> anyhow::Result<()> {
        let DiagnosticBatch {
            compiler,
            reads,
            configs,
            versions,
        } = batch;
        self.set_compiler(compiler);
        self.set_reads(reads);
        self.set_configs(configs);
        self.flush(ctx, |path| versions.get(path).copied())
    }

    /// Builds a [`DiagnosticBatch`] that carries the current source read and
    /// configuration partitions together with freshly computed compiler
    /// diagnostics and overlay `versions`.
    ///
    /// The analysis worker keeps its own tracker updated while rebuilding, so
    /// snapshotting here captures the exact partitions that must be committed
    /// atomically by [`apply_and_flush`] on the protocol thread.
    ///
    /// [`apply_and_flush`]: DiagnosticTracker::apply_and_flush
    pub(crate) fn snapshot(
        &self,
        compiler: BTreeMap<PathBuf, Vec<lsp::Diagnostic>>,
        versions: HashMap<PathBuf, i32>,
    ) -> DiagnosticBatch {
        DiagnosticBatch {
            compiler,
            reads: self.reads.clone(),
            configs: self.configs.clone(),
            versions,
        }
    }

    /// Whether `path` currently has published diagnostics.
    #[cfg(test)]
    pub(crate) fn is_published(&self, path: &Path) -> bool {
        self.published.contains(path)
    }

    /// Test hook that pretends `paths` were published in a previous flush.
    #[cfg(test)]
    pub(crate) fn force_published(&mut self, paths: impl IntoIterator<Item = PathBuf>) {
        self.published.extend(paths);
    }
}

pub(crate) fn diagnostic_tags(
    diag: &Diagnostic<'_>,
    ctx: &LspContext,
) -> Option<Vec<lsp::DiagnosticTag>> {
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

pub(crate) fn location(
    span: ast::Span,
    sources: &ast::SourceMap,
    ctx: &LspContext,
) -> Option<lsp::Location> {
    let file = sources.get(span.file)?;
    Some(lsp::Location {
        uri: ctx.uri(file.path()).ok()?,
        range: range(span, file)?,
    })
}

pub(crate) fn range(span: ast::Span, file: &ast::File) -> Option<lsp::Range> {
    Some(lsp::Range {
        start: crate::positions::file_position(file, span.start)?,
        end: crate::positions::file_position(file, span.end)?,
    })
}

#[cfg(test)]
mod tests {
    use crossbeam_channel::Receiver;
    use lsp_server::Message;

    use super::*;

    fn context() -> (LspContext, Receiver<Message>) {
        let (sender, receiver) = crossbeam_channel::unbounded();
        (LspContext::new(sender), receiver)
    }

    fn diagnostic(message: &str) -> lsp::Diagnostic {
        lsp::Diagnostic {
            range: lsp::Range::default(),
            message: message.into(),
            ..Default::default()
        }
    }

    fn publications(receiver: &Receiver<Message>) -> Vec<lsp::PublishDiagnosticsParams> {
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
    fn read_error_becomes_compiler_diagnostics_and_then_clears_without_duplicates() {
        let (ctx, receiver) = context();
        let path = PathBuf::from("/tmp/diagnostics.reds");
        let uri = ctx.uri(&path).unwrap();
        let mut tracker = DiagnosticTracker::default();

        tracker.set_reads(BTreeMap::from([(path.clone(), diagnostic("read"))]));
        tracker.flush(&ctx, |_| None).unwrap();
        let published = publications(&receiver);
        assert_eq!(published.len(), 1);
        assert_eq!(published[0].uri, uri);
        assert_eq!(published[0].diagnostics, vec![diagnostic("read")]);

        // The read error is replaced by compiler diagnostics in a single
        // notification rather than a clear followed by a publish.
        tracker.set_reads(BTreeMap::new());
        tracker.set_compiler(BTreeMap::from([(path.clone(), vec![diagnostic("syntax")])]));
        tracker.flush(&ctx, |_| None).unwrap();
        let published = publications(&receiver);
        assert_eq!(published.len(), 1);
        assert_eq!(published[0].uri, uri);
        assert_eq!(published[0].diagnostics, vec![diagnostic("syntax")]);

        tracker.set_compiler(BTreeMap::new());
        tracker.flush(&ctx, |_| None).unwrap();
        let published = publications(&receiver);
        assert_eq!(published.len(), 1);
        assert!(published[0].diagnostics.is_empty());

        // A second flush with nothing active must not repeat the clear.
        tracker.flush(&ctx, |_| None).unwrap();
        assert!(publications(&receiver).is_empty());
    }

    #[test]
    fn config_diagnostics_appear_and_clear() {
        let (ctx, receiver) = context();
        let dotfile = PathBuf::from("/tmp/.redscript");
        let uri = ctx.uri(&dotfile).unwrap();
        let mut tracker = DiagnosticTracker::default();

        tracker.set_configs(BTreeMap::from([(dotfile.clone(), diagnostic("invalid"))]));
        tracker.flush(&ctx, |_| None).unwrap();
        let published = publications(&receiver);
        assert_eq!(published.len(), 1);
        assert_eq!(published[0].uri, uri);
        assert_eq!(published[0].diagnostics, vec![diagnostic("invalid")]);

        tracker.set_configs(BTreeMap::new());
        tracker.flush(&ctx, |_| None).unwrap();
        let published = publications(&receiver);
        assert_eq!(published.len(), 1);
        assert!(published[0].diagnostics.is_empty());

        tracker.flush(&ctx, |_| None).unwrap();
        assert!(publications(&receiver).is_empty());
    }
}
