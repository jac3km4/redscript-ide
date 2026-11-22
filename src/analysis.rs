// The `#[self_referencing]` macro generates accessor methods and an internal
// struct whose field reads the dead-code lint does not attribute back to the
// declared fields; allow it for this module so the generated code stays
// warning-free.
#![allow(dead_code)]

use std::ops::Deref;
use std::path::PathBuf;
use std::sync::{Arc, LazyLock};

use hashbrown::HashMap;
use ouroboros::self_referencing;
use redscript_compiler_api::pass::{DiagnosticPass, UnusedLocals};
use redscript_compiler_api::{
    CompilationInputs, CompileErrorReporter, Diagnostic, LoweredCompilationUnit, ScriptBundle,
    SourceMapExt, Symbols, TypeFlagRegistry, TypeInterner, ast, ir,
};

use crate::dialect::{self, ParsedSources};
use crate::query::{AtContext, ExprAt};
use crate::server::LspContext;

const DIAGNOSTIC_PASSES: &[&'static (dyn DiagnosticPass + 'static)] = &[&UnusedLocals];

#[derive(Clone)]
pub(crate) enum CacheBytes {
    Embedded(&'static [u8]),
    Owned(Arc<[u8]>),
}

impl Deref for CacheBytes {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Embedded(bytes) => bytes,
            Self::Owned(bytes) => bytes,
        }
    }
}

impl From<Vec<u8>> for CacheBytes {
    fn from(bytes: Vec<u8>) -> Self {
        Self::Owned(Arc::from(bytes))
    }
}

impl CompilationCache {
    pub(crate) fn query_at<A>(
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
            let (query_context, definite_name, fallback_name) = match match_ {
                Some(ast::QueryResult::Type(&ast::Type::Named { name, .. })) => {
                    (None, Some(name), None)
                }
                Some(ast::QueryResult::Expr(&ast::Expr::Ident(name))) => {
                    (Some(AtContext::Expr), None, Some(name))
                }
                _ => (None, None, None),
            };

            let analysis = cache.analysis;
            let unit = &analysis.unit;
            let syms = &analysis.symbols;
            let func = unit
                .all_functions()
                .find(|f| f.span.file == id && f.span.contains(preceding_pos));
            let resolve = |name: &str| {
                let from_scopes = match unit.scopes.get(&id).and_then(|scope| scope.get(name)) {
                    Some(type_ref) => type_ref.id(),
                    None => None,
                };
                from_scopes
                    .or_else(|| cache.interner.get_index(cache.interner.get_index_of(name)?))
                    // The interner may know a name that has no type definition in
                    // this snapshot; callers index `Symbols` with the result.
                    .filter(|&typ| syms.get_type(typ).is_some())
            };
            // Type names in type positions always resolve. An identifier resolves
            // to a type only when no expression binding claims it, so a local,
            // parameter or field that shadows a type name takes precedence.
            let definite_type = definite_name.and_then(resolve);
            let fallback_type = fallback_name.and_then(resolve);
            ExprAt::new(None, func, definite_type, syms, cache.sources, query_context).with_expr_at(preceding_pos, |at| {
                let at = match fallback_type {
                    Some(typ) if is_type_receiver(at.expr()) => at.with_type(Some(typ)),
                    _ => at,
                };
                ctx.logger().verbose(|| {
                    format!(
                        "Query result lookup_byte_offset={preceding_pos} parsed_module={} ast_match={} function={} expression={} named_type={} receiver_type={} context={query_context:?} parse_diagnostics={} semantic_diagnostics={} phases({})",
                        analysis.parsed.modules.iter().any(|(file, _)| *file == id),
                        match_.is_some(),
                        func.is_some(),
                        at.expr().is_some(),
                        at.type_().is_some(),
                        at.expr_type().is_some(),
                        analysis.parsed.diagnostics.len(),
                        analysis.diagnostics.len(),
                        analysis.timings.summary(),
                    )
                });
                cb(at)
            })
        })
    }
}

/// Whether the expression at an identifier is a static type receiver that a
/// type-name binding should describe. Value bindings (locals, captures, fields,
/// ...) return `false` so they take precedence over a shadowed type.
fn is_type_receiver(expr: Option<&ir::Expr<'_>>) -> bool {
    match expr {
        Some(ir::Expr::Call { call, .. }) => matches!(**call, ir::Call::Static { .. }),
        None | Some(ir::Expr::Construct { .. } | ir::Expr::NewInstance { .. }) => true,
        _ => false,
    }
}

#[self_referencing(pub_extras)]
pub(crate) struct CompilationCache {
    pub(crate) cache_bytes: CacheBytes,
    pub(crate) interner: TypeInterner,
    pub(crate) sources: ast::SourceMap,
    pub(crate) file_ids: HashMap<PathBuf, ast::FileId>,

    #[borrows(cache_bytes, interner, sources)]
    #[not_covariant]
    pub(crate) analysis: DeferredAnalysis<'this>,
}

pub(crate) type DeferredAnalysis<'ctx> = LazyLock<
    AnalyzedSources<'ctx>,
    Box<dyn FnOnce() -> AnalyzedSources<'ctx> + Send + Sync + 'ctx>,
>;

pub(crate) struct AnalyzedSources<'ctx> {
    pub(crate) parsed: ParsedSources<'ctx>,
    pub(crate) unit: LoweredCompilationUnit<'ctx>,
    pub(crate) symbols: Symbols<'ctx>,
    pub(crate) diagnostics: Vec<Diagnostic<'ctx>>,
    pub(crate) timings: AnalysisTimings,
}

/// Per-phase timings for one deferred analysis, in milliseconds.
///
/// [`AnalysisTimings::phase`] only reads the clock when verbose logging is
/// enabled, so release builds record nothing (every field stays zero) and the
/// query log closure that reads them is never called.
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct AnalysisTimings {
    pub(crate) bundle_ms: f64,
    pub(crate) symbols_ms: f64,
    pub(crate) parse_ms: f64,
    pub(crate) lower_ms: f64,
    pub(crate) diagnostics_ms: f64,
}

impl AnalysisTimings {
    #[cfg(any(test, feature = "verbose-logs"))]
    fn phase<T>(slot: &mut f64, body: impl FnOnce() -> T) -> T {
        let started = std::time::Instant::now();
        let value = body();
        *slot = started.elapsed().as_secs_f64() * 1000.0;
        value
    }

    #[cfg(not(any(test, feature = "verbose-logs")))]
    fn phase<T>(_slot: &mut f64, body: impl FnOnce() -> T) -> T {
        body()
    }

    /// Renders the phases for the verbose query log.
    pub(crate) fn summary(&self) -> String {
        format!(
            "bundle_ms={:.3} symbols_ms={:.3} parse_ms={:.3} lower_ms={:.3} diagnostics_ms={:.3}",
            self.bundle_ms, self.symbols_ms, self.parse_ms, self.lower_ms, self.diagnostics_ms,
        )
    }
}

/// Narrows the work a [`CompilationCache`] performs when its analysis is first
/// forced.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum AnalysisMode {
    /// Full lowering plus the diagnostic passes; required when the snapshot's
    /// diagnostics are read.
    Diagnostics,
    /// Lowering only. Speculative query snapshots never read diagnostics, so the
    /// `UnusedLocals` pass (which visits every function in the workspace) is
    /// skipped.
    Query,
}

impl AnalysisMode {
    fn run_diagnostics(self) -> bool {
        matches!(self, Self::Diagnostics)
    }
}

impl<'ctx> AnalyzedSources<'ctx> {
    fn deferred(
        bytes: &'ctx [u8],
        sources: &'ctx ast::SourceMap,
        interner: &'ctx TypeInterner,
        mode: AnalysisMode,
    ) -> DeferredAnalysis<'ctx> {
        // Loading the base-game bundle and interning its ~30k symbols is only
        // paid when this snapshot is actually analyzed. Snapshots that are
        // replaced by a newer edit before they are forced stay free.
        LazyLock::new(Box::new(move || {
            let mut timings = AnalysisTimings::default();
            let bundle = AnalysisTimings::phase(&mut timings.bundle_ms, || {
                ScriptBundle::from_bytes(bytes).expect("cache bytes were validated before analysis")
            });
            let symbols = AnalysisTimings::phase(&mut timings.symbols_ms, || {
                CompilationInputs::load_without_mapping(
                    &bundle,
                    interner,
                    &TypeFlagRegistry::default(),
                )
                .expect("cache bytes were validated before analysis")
            });
            // A speculative query that was cancelled while the base symbols
            // loaded skips parsing and lowering: the worker discards its result.
            // Only a query snapshot may abort this way; the shared active cache
            // is always fully analyzed so later queries can reuse it.
            if mode == AnalysisMode::Query && crate::server::is_cancelled() {
                return Self::new(ParsedSources::empty(), symbols, interner, mode, timings);
            }
            let parsed = AnalysisTimings::phase(&mut timings.parse_ms, || dialect::parse(sources));
            if mode == AnalysisMode::Query && crate::server::is_cancelled() {
                return Self::new(ParsedSources::empty(), symbols, interner, mode, timings);
            }
            Self::new(parsed, symbols, interner, mode, timings)
        }))
    }

    fn new(
        parsed: ParsedSources<'ctx>,
        symbols: Symbols<'ctx>,
        interner: &'ctx TypeInterner,
        mode: AnalysisMode,
        mut timings: AnalysisTimings,
    ) -> Self {
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = AnalysisTimings::phase(&mut timings.lower_ms, || {
            parsed.process(symbols, &mut reporter, interner)
        });
        if mode.run_diagnostics() {
            AnalysisTimings::phase(&mut timings.diagnostics_ms, || {
                unit.run_diagnostics(DIAGNOSTIC_PASSES, &mut reporter);
            });
        }
        Self {
            parsed,
            unit,
            symbols,
            diagnostics: reporter.into_reported(),
            timings,
        }
    }
}

impl CompilationCache {
    /// Decodes the cache and interns its symbols once, discarding the result.
    ///
    /// [`CompilationCache::from_sources`] defers that work so a snapshot that is
    /// never analyzed stays cheap, but a corrupt configured cache must still be
    /// detected when the language server starts. Callers validate once per byte
    /// source, then rely on every later snapshot failing the same way.
    pub(crate) fn validate_cache_bytes(bytes: &[u8]) -> anyhow::Result<()> {
        let bundle = ScriptBundle::from_bytes(bytes)?;
        CompilationInputs::load_without_mapping(
            &bundle,
            &TypeInterner::default(),
            &TypeFlagRegistry::default(),
        )?;
        Ok(())
    }
}

impl CompilationCache {
    /// Test-only: whether this snapshot's analysis has already been forced. Used
    /// to assert that an unchanged snapshot is reused rather than rebuilt.
    #[cfg(test)]
    pub(crate) fn analysis_is_forced(&self) -> bool {
        self.with(|cache| LazyLock::get(cache.analysis).is_some())
    }

    pub(crate) fn analyze<A>(
        &self,
        mut cb: impl for<'ctx> FnMut(
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

    pub(crate) fn from_sources(
        cache_bytes: CacheBytes,
        sources: ast::SourceMap,
        interner: TypeInterner,
    ) -> anyhow::Result<Self> {
        Self::from_sources_with(cache_bytes, sources, interner, AnalysisMode::Diagnostics)
    }

    /// Like [`CompilationCache::from_sources`], choosing whether the snapshot
    /// runs the diagnostic passes. Query snapshots use [`AnalysisMode::Query`].
    pub(crate) fn from_sources_with(
        cache_bytes: CacheBytes,
        sources: ast::SourceMap,
        interner: TypeInterner,
        mode: AnalysisMode,
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
            move |bytes, interner, sources| {
                Ok(AnalyzedSources::deferred(
                    &bytes[..],
                    sources,
                    interner,
                    mode,
                ))
            },
        )
    }
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new(
            CacheBytes::Embedded(&[]),
            TypeInterner::default(),
            ast::SourceMap::default(),
            HashMap::default(),
            |_, interner, _| {
                LazyLock::new(Box::new(move || {
                    AnalyzedSources::new(
                        ParsedSources {
                            modules: vec![],
                            diagnostics: vec![],
                        },
                        Symbols::with_default_types(),
                        interner,
                        AnalysisMode::Diagnostics,
                        AnalysisTimings::default(),
                    )
                }))
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_send_sync<T: Send + Sync>() {}

    /// The analysis worker thread requires the compiler cache and its lazily
    /// built analysis to cross the thread boundary, so both must stay `Send`
    /// and `Sync` as upstream `redscript` changes land.
    #[test]
    fn compilation_cache_and_analysis_are_send_and_sync() {
        assert_send_sync::<CompilationCache>();
        assert_send_sync::<AnalyzedSources<'static>>();
    }

    /// Building a snapshot must not deserialize the cache; validating the bytes
    /// is the only eager step, so a corrupt configured cache is still reported
    /// at startup while an unforced snapshot stays free.
    #[test]
    fn cache_loading_is_deferred_but_validation_is_eager() {
        let corrupt: Vec<u8> = vec![0, 1, 2, 3];
        let snapshot = CompilationCache::from_sources(
            CacheBytes::Owned(Arc::from(corrupt.clone())),
            ast::SourceMap::new(),
            TypeInterner::default(),
        );
        assert!(snapshot.is_ok());
        assert!(CompilationCache::validate_cache_bytes(&corrupt).is_err());
        let valid = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        assert!(CompilationCache::validate_cache_bytes(&valid).is_ok());
    }

    /// A cancelled speculative query skips parsing and lowering, while a query
    /// that runs to completion still parses its sources.
    #[test]
    fn a_cancelled_query_snapshot_skips_parsing_and_lowering() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let modules = |cancelled: bool| {
            let sources = ast::SourceMap::new();
            sources.push_back("a.reds", "public class A {}");
            let cache = CompilationCache::from_sources_with(
                CacheBytes::Owned(Arc::from(bytes.clone())),
                sources,
                TypeInterner::default(),
                AnalysisMode::Query,
            )
            .unwrap();
            let token = crate::server::CancelToken::default();
            if cancelled {
                token.cancel();
            }
            crate::server::set_current_cancel(token);
            let count = cache.with(|cache| cache.analysis.parsed.modules.len());
            crate::server::clear_current_cancel();
            count
        };
        assert!(modules(false) > 0);
        assert_eq!(modules(true), 0);
    }

    /// Speculative query snapshots never read diagnostics, so they skip the
    /// `UnusedLocals` pass that full snapshots run.
    #[test]
    fn query_mode_skips_diagnostic_passes() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let report = |mode| {
            let sources = ast::SourceMap::new();
            sources.push_back(
                "unused.reds",
                "public func Test() -> Void { let unused = 1; }",
            );
            let cache = CompilationCache::from_sources_with(
                CacheBytes::Owned(Arc::from(bytes.clone())),
                sources,
                TypeInterner::default(),
                mode,
            )
            .unwrap();
            cache
                .analyze(|_, _, diagnostics, _| Ok(diagnostics.len()))
                .unwrap()
        };
        assert!(report(AnalysisMode::Diagnostics) > 0);
        assert_eq!(report(AnalysisMode::Query), 0);
    }
}
