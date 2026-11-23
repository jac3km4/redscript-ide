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
use redscript_compiler_frontend::Shared;

use crate::dialect::{self, ParsedSources};
use crate::query::ExprAt;
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
    /// The raw bundle bytes, owned here so the base [`Symbols`] can borrow them
    /// without leaking them or keeping a separate self-referential bundle.
    bytes: CacheBytes,
    decoded: OnceLock<DecodedBase>,
}

/// The base-game [`Symbols`] table together with the bytes and interner it
/// borrows, held in one self-referential value so both outlive it without being
/// leaked.
///
/// The bundle itself is not stored: upstream `ScriptBundle::into_owned`
/// debug-asserts that a native function has no source, which the real game
/// bundle violates, so the raw bytes are kept and the bundle is rebuilt (and
/// dropped) inside the builder closure.
#[self_referencing]
struct DecodedBase {
    bytes: CacheBytes,
    /// The base interner, shared with every snapshot: each snapshot forks a
    /// disposable child, so a user-defined type or typo interns into the child
    /// and never accumulates in the base for the server's lifetime.
    interner: Shared<TypeInterner>,
    #[borrows(bytes, interner)]
    #[covariant]
    symbols: Symbols<'this>,
}

impl BaseEnv {
    pub(crate) fn new(bytes: CacheBytes) -> Self {
        Self {
            bytes,
            decoded: OnceLock::new(),
        }
    }

    /// Decodes the bundle and interns the base-game symbols once, on first use.
    fn decode(&self) -> anyhow::Result<DecodedBase> {
        DecodedBase::try_new(
            self.bytes.clone(),
            Shared::new(TypeInterner::default()),
            |bytes, interner| {
                let bundle = ScriptBundle::from_bytes(bytes)?;
                let symbols = CompilationInputs::load_without_mapping(
                    &bundle,
                    interner,
                    &TypeFlagRegistry::default(),
                )?;
                Ok::<_, anyhow::Error>(symbols)
            },
        )
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
            bytes: CacheBytes::Embedded(&[]),
            decoded: OnceLock::from(DecodedBase::new(
                CacheBytes::Embedded(&[]),
                Shared::new(TypeInterner::default()),
                |_, _| Symbols::with_default_types(),
            )),
        }
    }
}

impl AnalysisSnapshot {
    pub(crate) fn query_at<A>(
        &self,
        id: ast::FileId,
        pos: u32,
        cb: impl Fn(ExprAt<'_, '_>) -> anyhow::Result<A>,
        ctx: &LspContext,
    ) -> anyhow::Result<A> {
        let preceding_pos = pos.saturating_sub(1);
        self.with(|cache| {
            let ast_match = cache
                .analysis
                .parsed
                .modules
                .iter()
                .find(|(file, _)| *file == id)
                .and_then(|(_, module)| module.module().find_at(preceding_pos));
            let (in_expr_position, type_pos_name, expr_ident_name) = match ast_match {
                Some(ast::QueryResult::Type(&ast::Type::Named { name, .. })) => {
                    (false, Some(name), None)
                }
                Some(ast::QueryResult::Expr(&ast::Expr::Ident(name))) => {
                    (true, None, Some(name))
                }
                _ => (false, None, None),
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
                // Search the snapshot's forked interner: it resolves predef
                // names, the shared base parent, and this snapshot's own
                // user-defined types, which the base interner never saw.
                let interner = cache.interner.get();
                from_scopes
                    .or_else(|| interner.and_then(|interner| interner.get(name)))
                    // The interner may know a name that has no type definition in
                    // this snapshot; callers index `Symbols` with the result.
                    .filter(|&typ| syms.get_type(typ).is_some())
            };
            // Type names in type positions always resolve. An identifier resolves
            // to a type only when no expression binding claims it, so a local,
            // parameter or field that shadows a type name takes precedence.
            let named_type = type_pos_name.and_then(resolve);
            let receiver_type_candidate = expr_ident_name.and_then(resolve);
            ExprAt::new(None, func, named_type, syms, cache.sources, in_expr_position).with_expr_at(preceding_pos, |at| {
                let at = match receiver_type_candidate {
                    Some(typ) if is_type_receiver(at.expr()) => at.with_named_type(Some(typ)),
                    _ => at,
                };
                ctx.logger().verbose(|| {
                    format!(
                        "Query result lookup_byte_offset={preceding_pos} parsed_module={} ast_match={} function={} expression={} named_type={} expr_type={} in_expr_position={in_expr_position} parse_diagnostics={} semantic_diagnostics={} phases({})",
                        analysis.parsed.modules.iter().any(|(file, _)| *file == id),
                        ast_match.is_some(),
                        func.is_some(),
                        at.expr().is_some(),
                        at.named_type().is_some(),
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
pub(crate) struct AnalysisSnapshot {
    pub(crate) base: Arc<BaseEnv>,
    /// A per-snapshot child of the base interner, forked on first analysis.
    ///
    /// Forking (rather than sharing the base interner) keeps every transient
    /// user-defined type, typo, or partial identifier this snapshot interns out
    /// of the base, and off its `RwLock`. The fork is lazy so a snapshot replaced
    /// before it is forced stays free, matching the deferred analysis.
    pub(crate) interner: OnceLock<TypeInterner>,
    pub(crate) sources: ast::SourceMap,
    pub(crate) file_ids: HashMap<PathBuf, ast::FileId>,

    #[borrows(base, interner, sources)]
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

/// Narrows the work an [`AnalysisSnapshot`] performs when its analysis is first
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
        interner: &'ctx OnceLock<TypeInterner>,
        sources: &'ctx ast::SourceMap,
        mode: AnalysisMode,
    ) -> DeferredAnalysis<'ctx> {
        // The base-game bundle and symbol table are decoded once and shared by
        // every snapshot: forcing this snapshot only clones the copy-on-write
        // base table, which is cheap. Snapshots replaced by a newer edit before
        // they are forced stay free.
        LazyLock::new(Box::new(move || {
            let decoded = base.decoded();
            // Fork the base interner once per snapshot. The child shares every
            // base name by pointer but isolates the types this snapshot interns,
            // so a user-defined type or typo never reaches the shared base. The
            // base is fully decoded above, so it is never interned into after
            // forking.
            let interner = interner.get_or_init(|| TypeInterner::fork(decoded.borrow_interner()));
            let mut timings = AnalysisTimings::default();
            let symbols = AnalysisTimings::phase(&mut timings.symbols_ms, || {
                decoded.borrow_symbols().clone()
            });
            // A speculative query that was cancelled while the base symbols
            // loaded skips parsing and lowering: the worker discards its result.
            // Only a query snapshot may abort this way; the shared active cache
            // is always fully analyzed so later queries can reuse it.
            if mode.is_query() && crate::server::is_cancelled() {
                return Self::new(ParsedSources::empty(), symbols, interner, mode, timings);
            }
            let parsed = AnalysisTimings::phase(&mut timings.parse_ms, || dialect::parse(sources));
            if mode.is_query() && crate::server::is_cancelled() {
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
            parsed.lower(symbols, &mut reporter, interner, mode.query_target())
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

impl AnalysisSnapshot {
    /// Test-only: whether this snapshot's analysis has already been forced. Used
    /// to assert that an unchanged snapshot is reused rather than rebuilt.
    #[cfg(test)]
    pub(crate) fn analysis_is_forced(&self) -> bool {
        self.with(|cache| LazyLock::get(cache.analysis).is_some())
    }

    /// Runs `cb` against the snapshot's lowered unit, symbols, and diagnostics.
    ///
    /// Forcing the analysis happens on first use; callers that only need the
    /// symbol surface still pay the full lowering cost.
    pub(crate) fn with_lowered<A>(
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

    /// Like [`AnalysisSnapshot::from_sources`], choosing whether the snapshot
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

        Self::try_new(
            base,
            OnceLock::new(),
            sources,
            file_ids,
            move |base, interner, sources| {
                Ok(AnalyzedSources::deferred(base, interner, sources, mode))
            },
        )
    }
}

impl Default for AnalysisSnapshot {
    fn default() -> Self {
        Self::new(
            Arc::new(BaseEnv::empty()),
            OnceLock::new(),
            ast::SourceMap::default(),
            HashMap::default(),
            |base, interner, _| {
                LazyLock::new(Box::new(move || {
                    let decoded = base.decoded();
                    let interner =
                        interner.get_or_init(|| TypeInterner::fork(decoded.borrow_interner()));
                    AnalyzedSources::new(
                        ParsedSources {
                            modules: vec![],
                            diagnostics: vec![],
                        },
                        decoded.borrow_symbols().clone(),
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
    use redscript_compiler_api::{Aggregate, TypeSchema};
    use redscript_compiler_frontend::Shared;

    use super::*;

    fn assert_send_sync<T: Send + Sync>() {}

    /// A logging context for query entry points that only need a sink.
    fn context() -> LspContext {
        let (sender, _receiver) = crossbeam_channel::unbounded();
        LspContext::new(sender)
    }

    /// The analysis worker thread requires the compiler cache and its lazily
    /// built analysis to cross the thread boundary, so both must stay `Send`
    /// and `Sync` as upstream `redscript` changes land.
    #[test]
    fn analysis_snapshot_and_analysis_are_send_and_sync() {
        assert_send_sync::<AnalysisSnapshot>();
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
        let snapshot = AnalysisSnapshot::from_sources(base.clone(), ast::SourceMap::new());
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
            let cache = AnalysisSnapshot::from_sources_with(
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
            let cache = AnalysisSnapshot::from_sources_with(
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

    /// Snapshots built from the same base share the decoded bundle and the base
    /// interner's parent chain, but each resolves its own sources into an
    /// independent `Symbols`, and interns its user-defined types into its own
    /// forked interner: a type defined in one snapshot never leaks into another
    /// or into the shared base.
    #[test]
    fn snapshots_share_the_base_but_keep_independent_symbols() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let base = Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes))));
        let snapshot = |source: &str| {
            let sources = ast::SourceMap::new();
            sources.push_back("a.reds", source);
            AnalysisSnapshot::from_sources(base.clone(), sources).unwrap()
        };
        let a = snapshot("public class OnlyInA {}");
        let b = snapshot("public class OnlyInB {}");
        let type_names = |cache: &AnalysisSnapshot| {
            cache.with(|cache| {
                cache
                    .analysis
                    .symbols
                    .types()
                    .map(|(id, _)| id.as_str().into_owned())
                    .collect::<Vec<_>>()
            })
        };
        let a_names = type_names(&a);
        let b_names = type_names(&b);
        assert!(a_names.iter().any(|name| name == "OnlyInA"));
        assert!(!a_names.iter().any(|name| name == "OnlyInB"));
        assert!(b_names.iter().any(|name| name == "OnlyInB"));
        assert!(!b_names.iter().any(|name| name == "OnlyInA"));

        // Each snapshot interns its own types into its forked child interner,
        // never the shared base.
        let interned = |cache: &AnalysisSnapshot, name: &str| {
            cache
                .borrow_interner()
                .get()
                .is_some_and(|interner| interner.get(name).is_some())
        };
        assert!(interned(&a, "OnlyInA"));
        assert!(!interned(&a, "OnlyInB"));
        assert!(interned(&b, "OnlyInB"));
        assert!(!interned(&b, "OnlyInA"));
        assert!(
            base.decoded().borrow_interner().get("OnlyInA").is_none(),
            "a snapshot-local type must not reach the shared base interner"
        );
        assert!(
            base.decoded().borrow_interner().get("OnlyInB").is_none(),
            "a snapshot-local type must not reach the shared base interner"
        );
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
        let base_aggregate = shared_aggregate(base.decoded().borrow_symbols(), "IScriptable")
            .expect("the default base defines IScriptable");
        let sources = ast::SourceMap::new();
        sources.push_back("a.reds", "public class A {}");
        let cache = AnalysisSnapshot::from_sources(base.clone(), sources).unwrap();
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
            let cache = AnalysisSnapshot::from_sources_with(
                Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes.clone())))),
                sources,
                mode,
            )
            .unwrap();
            cache
                .with_lowered(|_, _, diagnostics, _| Ok(diagnostics.len()))
                .unwrap()
        };
        assert!(report(false) > 0);
        assert_eq!(report(true), 0);
    }

    /// `query_at` resolves a name through the snapshot's forked interner, which
    /// knows the snapshot's own user-defined types; the shared base interner
    /// never saw them. The queried file does not declare or import the type, so
    /// the per-file scope lookup misses and only the forked interner can resolve
    /// it.
    #[test]
    fn query_at_resolves_snapshot_local_types_through_the_forked_interner() {
        let bytes = ScriptBundle::default().into_writeable().to_bytes().unwrap();
        let base = Arc::new(BaseEnv::new(CacheBytes::Owned(Arc::from(bytes))));
        let sources = ast::SourceMap::new();
        sources.push_back("defs.reds", "public class SnapshotLocal {}");
        let use_source = "public func Target() -> SnapshotLocal { return new SnapshotLocal(); }";
        let id = sources.push_back("use.reds", use_source);
        let cache = AnalysisSnapshot::from_sources(base.clone(), sources).unwrap();
        let ctx = context();
        let pos = use_source.find("SnapshotLocal").unwrap() as u32 + 2;
        let resolved = cache
            .query_at(
                id,
                pos,
                |at| Ok(at.named_type().map(|typ| typ.as_str().into_owned())),
                &ctx,
            )
            .unwrap();
        assert_eq!(
            resolved.as_deref(),
            Some("SnapshotLocal"),
            "a snapshot-local type resolves via the snapshot's forked interner"
        );
        assert!(
            base.decoded()
                .borrow_interner()
                .get("SnapshotLocal")
                .is_none(),
            "the base interner must not know the snapshot-local type"
        );
    }
}
