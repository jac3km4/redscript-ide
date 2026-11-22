// The `#[self_referencing]` macro generates accessor methods and an internal
// struct whose field reads the dead-code lint does not attribute back to the
// declared fields; allow it for this module so the generated code stays
// warning-free.
#![allow(dead_code)]

use std::ops::Deref;
use std::path::PathBuf;
use std::sync::{Arc, LazyLock, OnceLock};

use hashbrown::HashMap;
use ouroboros::self_referencing;
use redscript_compiler_api::pass::{DiagnosticPass, UnusedLocals};
use redscript_compiler_api::{
    CompilationInputs, CompileErrorReporter, Diagnostic, LoweredCompilationUnit, QueryTarget,
    ScriptBundle, SourceMapExt, Symbols, TypeFlagRegistry, TypeInterner, ast, ir,
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

/// The decoded base-game inputs shared by every snapshot.
///
/// Building the base-game [`Symbols`] table (decoding the bundle and interning
/// its ~30k names) dominates the cost of a cold snapshot, so it happens once
/// here and is reused. The decode stays lazy (it runs on the first analysis, not
/// on snapshot construction), matching the rest of the snapshot lifecycle.
pub(crate) struct BaseEnv {
    /// The raw bundle bytes, borrowed for the server's lifetime.
    ///
    /// They are leaked rather than kept in a self-referential `ScriptBundle`,
    /// because the base [`Symbols`] borrows the interner and the bundle names
    /// for `'static`; the bundle also cannot be made owned, since the upstream
    /// `ScriptBundle::into_owned` debug-asserts that a native function has no
    /// source, which the real game bundle violates. The leak is one allocation
    /// per server.
    bytes: &'static [u8],
    decoded: OnceLock<DecodedBase>,
}

struct DecodedBase {
    /// The base interner, leaked for the server's lifetime so the base
    /// [`Symbols`] can borrow it for `'static`. Snapshots share it, so a
    /// user-defined type interns into the same table; they stay independent
    /// through their own [`Symbols`], so no definition leaks between snapshots.
    interner: &'static TypeInterner,
    /// The base-game symbol table, built once and cloned by every snapshot.
    symbols: Symbols<'static>,
}

impl BaseEnv {
    pub(crate) fn new(bytes: CacheBytes) -> Self {
        let bytes: &'static [u8] = match bytes {
            CacheBytes::Embedded(bytes) => bytes,
            CacheBytes::Owned(bytes) => Box::leak(Box::new(bytes)),
        };
        Self {
            bytes,
            decoded: OnceLock::new(),
        }
    }

    /// Decodes the bundle and interns the base-game symbols once, on first use.
    fn decode(&self) -> anyhow::Result<DecodedBase> {
        let bundle = ScriptBundle::from_bytes(self.bytes)?;
        let interner: &'static TypeInterner = Box::leak(Box::new(TypeInterner::default()));
        let symbols = CompilationInputs::load_without_mapping(
            &bundle,
            interner,
            &TypeFlagRegistry::default(),
        )?;
        Ok(DecodedBase { interner, symbols })
    }

    /// Decodes on first use and reuses the result afterwards.
    fn decoded(&self) -> &DecodedBase {
        self.decoded.get_or_init(|| {
            self.decode()
                .expect("cache bytes were validated before analysis")
        })
    }

    /// Decodes eagerly and caches the result, reporting invalid bytes.
    ///
    /// A configured cache is validated at startup this way, and the same decode
    /// then serves the first snapshot, so the work is not done twice.
    pub(crate) fn ensure_decoded(&self) -> anyhow::Result<()> {
        if self.decoded.get().is_some() {
            return Ok(());
        }
        let decoded = self.decode()?;
        self.decoded.set(decoded).ok();
        Ok(())
    }

    /// An empty base for the placeholder snapshot, with no decode required.
    fn empty() -> Self {
        Self {
            bytes: &[],
            decoded: OnceLock::from(DecodedBase {
                interner: Box::leak(Box::new(TypeInterner::default())),
                symbols: Symbols::with_default_types(),
            }),
        }
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
                let interner = cache.base.decoded().interner;
                from_scopes
                    .or_else(|| interner.get_index(interner.get_index_of(name)?))
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
    pub(crate) base: Arc<BaseEnv>,
    pub(crate) sources: ast::SourceMap,
    pub(crate) file_ids: HashMap<PathBuf, ast::FileId>,

    #[borrows(base, sources)]
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
            "symbols_ms={:.3} parse_ms={:.3} lower_ms={:.3} diagnostics_ms={:.3}",
            self.symbols_ms, self.parse_ms, self.lower_ms, self.diagnostics_ms,
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
    /// Lower only the function enclosing an IDE query. Speculative query
    /// snapshots never read diagnostics, so the `UnusedLocals` pass (which
    /// visits every function in the workspace) is skipped, and every unrelated
    /// function body stays unlifted.
    Query(QueryTarget),
}

impl AnalysisMode {
    fn run_diagnostics(self) -> bool {
        matches!(self, Self::Diagnostics)
    }

    fn query_target(self) -> Option<QueryTarget> {
        match self {
            Self::Diagnostics => None,
            Self::Query(target) => Some(target),
        }
    }

    fn is_query(self) -> bool {
        matches!(self, Self::Query(_))
    }
}

impl<'ctx> AnalyzedSources<'ctx> {
    fn deferred(
        base: &'ctx Arc<BaseEnv>,
        sources: &'ctx ast::SourceMap,
        mode: AnalysisMode,
    ) -> DeferredAnalysis<'ctx> {
        // The base-game bundle and symbol table are decoded once and shared by
        // every snapshot: forcing this snapshot only clones the copy-on-write
        // base table, which is cheap. Snapshots replaced by a newer edit before
        // they are forced stay free.
        LazyLock::new(Box::new(move || {
            let decoded = base.decoded();
            let mut timings = AnalysisTimings::default();
            let symbols =
                AnalysisTimings::phase(&mut timings.symbols_ms, || decoded.symbols.clone());
            // A speculative query that was cancelled while the base symbols
            // loaded skips parsing and lowering: the worker discards its result.
            // Only a query snapshot may abort this way; the shared active cache
            // is always fully analyzed so later queries can reuse it.
            if mode.is_query() && crate::server::is_cancelled() {
                return Self::new(
                    ParsedSources::empty(),
                    symbols,
                    decoded.interner,
                    mode,
                    timings,
                );
            }
            let parsed = AnalysisTimings::phase(&mut timings.parse_ms, || dialect::parse(sources));
            if mode.is_query() && crate::server::is_cancelled() {
                return Self::new(
                    ParsedSources::empty(),
                    symbols,
                    decoded.interner,
                    mode,
                    timings,
                );
            }
            Self::new(parsed, symbols, decoded.interner, mode, timings)
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
            parsed.process(symbols, &mut reporter, interner, mode.query_target())
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
        base: Arc<BaseEnv>,
        sources: ast::SourceMap,
    ) -> anyhow::Result<Self> {
        Self::from_sources_with(base, sources, AnalysisMode::Diagnostics)
    }

    /// Like [`CompilationCache::from_sources`], choosing whether the snapshot
    /// runs the diagnostic passes. Query snapshots use [`AnalysisMode::Query`].
    pub(crate) fn from_sources_with(
        base: Arc<BaseEnv>,
        sources: ast::SourceMap,
        mode: AnalysisMode,
    ) -> anyhow::Result<Self> {
        let file_ids = sources
            .files()
            .map(|(id, file)| (file.path().to_owned(), id))
            .collect();
        sources.populate_boot_lib();

        Self::try_new(base, sources, file_ids, move |base, sources| {
            Ok(AnalyzedSources::deferred(base, sources, mode))
        })
    }
}

impl Default for CompilationCache {
    fn default() -> Self {
        Self::new(
            Arc::new(BaseEnv::empty()),
            ast::SourceMap::default(),
            HashMap::default(),
            |base, _| {
                LazyLock::new(Box::new(move || {
                    AnalyzedSources::new(
                        ParsedSources {
                            modules: vec![],
                            diagnostics: vec![],
                        },
                        base.decoded().symbols.clone(),
                        base.decoded().interner,
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
    use redscript_compiler_api::{Aggregate, TypeSchema};
    use redscript_compiler_frontend::Shared;

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
    /// at startup while an unforced snapshot stays free. Validation warms the
    /// shared base, so the first snapshot reuses that decode.
    #[test]
    fn cache_loading_is_deferred_but_validation_is_eager() {
        let corrupt: Vec<u8> = vec![0, 1, 2, 3];
        let base = Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(corrupt))));
        let snapshot = CompilationCache::from_sources(base.clone(), ast::SourceMap::new());
        assert!(snapshot.is_ok());
        assert!(base.ensure_decoded().is_err());
        let valid = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let base = Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(valid))));
        assert!(base.ensure_decoded().is_ok());
    }

    /// A cancelled speculative query skips parsing and lowering, while a query
    /// that runs to completion still parses its sources.
    #[test]
    fn a_cancelled_query_snapshot_skips_parsing_and_lowering() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let modules = |cancelled: bool| {
            let sources = ast::SourceMap::new();
            let id = sources.push_back("a.reds", "public class A {}");
            let cache = CompilationCache::from_sources_with(
                Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes.clone())))),
                sources,
                AnalysisMode::Query(QueryTarget { file: id, pos: 0 }),
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

    /// Query snapshots lower only the function enclosing the target position,
    /// while a diagnostics snapshot lowers every function in the workspace.
    /// `query_at` only reads the enclosing function, so this is the work query
    /// mode removes.
    #[test]
    fn query_mode_lowers_only_the_target_function() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let source = "public func First() -> Int32 { return 1; }\n\
                      public func Second() -> Int32 { return 2; }";
        let target = source.find("return 2").unwrap() as u32;
        let spans = |query: bool| {
            let sources = ast::SourceMap::new();
            let id = sources.push_back("a.reds", source);
            let mode = if query {
                AnalysisMode::Query(QueryTarget {
                    file: id,
                    pos: target,
                })
            } else {
                AnalysisMode::Diagnostics
            };
            let cache = CompilationCache::from_sources_with(
                Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes.clone())))),
                sources,
                mode,
            )
            .unwrap();
            cache.with(|cache| {
                cache
                    .analysis
                    .unit
                    .all_functions()
                    .map(|func| func.span)
                    .collect::<Vec<_>>()
            })
        };
        let all = spans(false);
        assert!(all.iter().any(|span| span.contains(target)));
        let query = spans(true);
        assert_eq!(query.len(), 1, "only the target function should be lowered");
        assert!(
            query[0].contains(target),
            "the lowered span encloses the target"
        );
        assert!(
            all.len() > query.len(),
            "query mode lowers strictly less than a full snapshot"
        );
    }

    /// Snapshots built from the same base share the decoded bundle and the
    /// interner, but each resolves its own sources into an independent
    /// `Symbols`: a type defined in one snapshot never leaks into another.
    #[test]
    fn snapshots_share_the_base_but_keep_independent_symbols() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let base = Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes))));
        let types = |source: &str| {
            let sources = ast::SourceMap::new();
            sources.push_back("a.reds", source);
            let cache = CompilationCache::from_sources(base.clone(), sources).unwrap();
            cache.with(|cache| {
                cache
                    .analysis
                    .symbols
                    .types()
                    .map(|(id, _)| id.as_str().into_owned())
                    .collect::<Vec<_>>()
            })
        };
        let a = types("public class OnlyInA {}");
        let b = types("public class OnlyInB {}");
        assert!(a.iter().any(|name| name == "OnlyInA"));
        assert!(!a.iter().any(|name| name == "OnlyInB"));
        assert!(b.iter().any(|name| name == "OnlyInB"));
        assert!(!b.iter().any(|name| name == "OnlyInA"));
    }

    /// The shared aggregate behind a base type name, if it has one.
    fn shared_aggregate<'ctx>(
        symbols: &Symbols<'ctx>,
        name: &str,
    ) -> Option<Shared<Aggregate<'ctx>>> {
        symbols
            .types()
            .find(|(id, _)| id.as_str().as_ref() == name)
            .and_then(|(_, def)| match def.schema() {
                TypeSchema::Aggregate(aggregate) => Some(aggregate.clone()),
                _ => None,
            })
    }

    /// The base symbol table is built once and cloned per snapshot: an untouched
    /// base type is shared by pointer between the base and every snapshot rather
    /// than deep-copied, which is what makes reuse cheap.
    #[test]
    fn snapshots_share_the_base_symbol_table() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let base = Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes))));
        let base_aggregate = shared_aggregate(&base.decoded().symbols, "IScriptable")
            .expect("the default base defines IScriptable");
        let sources = ast::SourceMap::new();
        sources.push_back("a.reds", "public class A {}");
        let cache = CompilationCache::from_sources(base.clone(), sources).unwrap();
        let snapshot_aggregate = cache
            .with(|cache| shared_aggregate(&cache.analysis.symbols, "IScriptable"))
            .expect("the snapshot inherits IScriptable");
        assert!(
            Shared::ptr_eq(&base_aggregate, &snapshot_aggregate),
            "an untouched base aggregate is shared, not copied"
        );
    }

    /// Speculative query snapshots never read diagnostics, so they skip the
    /// `UnusedLocals` pass that full snapshots run.
    #[test]
    fn query_mode_skips_diagnostic_passes() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let report = |query: bool| {
            let sources = ast::SourceMap::new();
            let id = sources.push_back(
                "unused.reds",
                "public func Test() -> Void { let unused = 1; }",
            );
            let mode = if query {
                AnalysisMode::Query(QueryTarget { file: id, pos: 0 })
            } else {
                AnalysisMode::Diagnostics
            };
            let cache = CompilationCache::from_sources_with(
                Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes.clone())))),
                sources,
                mode,
            )
            .unwrap();
            cache
                .analyze(|_, _, diagnostics, _| Ok(diagnostics.len()))
                .unwrap()
        };
        assert!(report(false) > 0);
        assert_eq!(report(true), 0);
    }
}
