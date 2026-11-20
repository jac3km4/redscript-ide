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
                ctx.logger().info(format!(
                "Query result lookup_byte_offset={preceding_pos} parsed_module={} ast_match={} function={} expression={} named_type={} receiver_type={} context={query_context:?} parse_diagnostics={} semantic_diagnostics={}",
                analysis.parsed.modules.iter().any(|(file, _)| *file == id),
                match_.is_some(),
                func.is_some(),
                at.expr().is_some(),
                at.type_().is_some(),
                at.expr_type().is_some(),
                analysis.parsed.diagnostics.len(),
                analysis.diagnostics.len(),
            ));
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
}

impl<'ctx> AnalyzedSources<'ctx> {
    fn deferred(
        sources: &'ctx ast::SourceMap,
        symbols: Symbols<'ctx>,
        interner: &'ctx TypeInterner,
    ) -> DeferredAnalysis<'ctx> {
        LazyLock::new(Box::new(move || {
            Self::new(dialect::parse(sources), symbols, interner)
        }))
    }

    fn new(
        parsed: ParsedSources<'ctx>,
        symbols: Symbols<'ctx>,
        interner: &'ctx TypeInterner,
    ) -> Self {
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = parsed.process(symbols, &mut reporter, interner);
        unit.run_diagnostics(DIAGNOSTIC_PASSES, &mut reporter);
        Self {
            parsed,
            unit,
            symbols,
            diagnostics: reporter.into_reported(),
        }
    }
}

impl CompilationCache {
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
            |bytes, interner, sources| {
                let bundle = ScriptBundle::from_bytes(bytes)?;
                let symbols = CompilationInputs::load_without_mapping(
                    &bundle,
                    interner,
                    &TypeFlagRegistry::default(),
                )?;
                Ok(AnalyzedSources::deferred(sources, symbols, interner))
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
}
