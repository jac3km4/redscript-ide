use std::fmt::Display;

use redscript_compiler_api::ast::{SourceMap, Span};
use redscript_compiler_api::types::Type;
use redscript_compiler_api::{LoweredFunction, Symbols, TypeId, ir};

use crate::display::ExprAtDisplay;

#[derive(Clone)]
pub struct ExprAt<'ctx, 'a> {
    expr: Option<&'a ir::Expr<'ctx>>,
    func: Option<&'a LoweredFunction<'ctx>>,
    type_: Option<TypeId<'ctx>>,
    symbols: &'a Symbols<'ctx>,
    sources: &'ctx SourceMap,
    context: Option<AtContext>,
}

impl<'ctx, 'a> ExprAt<'ctx, 'a> {
    pub fn new(
        expr: Option<&'a ir::Expr<'ctx>>,
        func: Option<&'a LoweredFunction<'ctx>>,
        type_: Option<TypeId<'ctx>>,
        symbols: &'a Symbols<'ctx>,
        sources: &'ctx SourceMap,
        context: Option<AtContext>,
    ) -> Self {
        Self {
            expr,
            func,
            type_,
            symbols,
            sources,
            context,
        }
    }

    pub fn with_expr_at<A>(&self, pos: u32, cb: impl Fn(ExprAt<'ctx, '_>) -> A) -> A {
        let query = |expr: Option<&ir::Expr<'ctx>>| {
            cb(ExprAt::new(
                expr,
                self.func,
                self.type_,
                self.symbols,
                self.sources,
                self.context,
            ))
        };
        if let Some(func) = self.func {
            with_expr_at(&func.block, pos, self.sources, query)
        } else {
            query(None)
        }
    }

    pub fn expr(&self) -> Option<&'a ir::Expr<'ctx>> {
        self.expr
    }

    pub fn func(&self) -> Option<&'a LoweredFunction<'ctx>> {
        self.func
    }

    pub fn type_(&self) -> Option<TypeId<'ctx>> {
        self.type_
    }

    pub fn symbols(&self) -> &'a Symbols<'ctx> {
        self.symbols
    }

    pub fn sources(&self) -> &'ctx SourceMap {
        self.sources
    }

    pub fn context(&self) -> Option<AtContext> {
        self.context
    }

    pub fn expr_type(&self) -> Option<Type<'ctx>> {
        if let Some(expr) = self.expr
            && let Some(func) = self.func
        {
            type_of(expr, func, self.symbols)
        } else {
            None
        }
    }

    pub fn definition_span(&self) -> Option<Span> {
        if let Some(typ) = self.type_ {
            self.symbols[typ].span()
        } else if let Some(expr) = self.expr {
            match expr {
                ir::Expr::Call { call, .. } => match &**call {
                    ir::Call::FreeFunction { function, .. } => self.symbols[*function].span(),
                    ir::Call::Instance { method, .. } | ir::Call::Static { method, .. } => {
                        self.symbols[*method].span()
                    }
                    _ => None,
                },
                ir::Expr::Field { field, .. } => self.symbols[*field].span(),
                ir::Expr::Local(local, _) | ir::Expr::Capture(local, _) => {
                    local_definition_span(self.func?, *local)
                }
                _ => None,
            }
        } else {
            None
        }
    }

    pub fn display(self) -> impl Display {
        ExprAtDisplay::new(self)
    }
}

fn local_definition_span(function: &LoweredFunction<'_>, local: ir::Local) -> Option<Span> {
    use ir::Visitor;

    fn find<'a, 'ctx>(
        locals: &'a [ir::LocalInfo<'ctx>],
        local: ir::Local,
    ) -> Option<&'a ir::LocalInfo<'ctx>> {
        locals
            .binary_search_by_key(&local, |info| info.id)
            .ok()
            .map(|index| &locals[index])
    }

    fn authored_span(info: &ir::LocalInfo<'_>) -> Option<Span> {
        // Generated temporaries can inherit source spans, but are not declarations.
        (info.name.is_some() && info.temporary_purpose.is_none())
            .then_some(info.span)
            .flatten()
    }

    if let Some(info) = find(&function.locals, local) {
        return authored_span(info);
    }

    struct FindLocal {
        local: ir::Local,
        span: Option<Span>,
    }

    impl<'ctx> Visitor<'ctx> for FindLocal {
        fn visit_new_closure(&mut self, closure: &ir::Closure<'ctx>, _: Span) {
            if self.span.is_some() {
                return;
            }
            // IDs are shared across the function and nested closures; captures
            // retain the ID of their declaration, even across several closures.
            if let Some(info) = find(&closure.locals, self.local) {
                self.span = authored_span(info);
            } else {
                self.visit_block(&closure.block);
            }
        }
    }

    let mut finder = FindLocal { local, span: None };
    finder.visit_block(&function.block);
    finder.span
}

fn with_expr_at<'ctx, A>(
    block: &ir::Block<'ctx>,
    pos: u32,
    sources: &SourceMap,
    cb: impl Fn(Option<&ir::Expr<'ctx>>) -> A,
) -> A {
    use ir::Visitor;

    let expr = block.find_at(pos);
    if let Some(ir::Expr::Local(ir::Local::This, span)) = expr
        && let Some(file) = sources.get(span.file)
        && !matches!(
            file.source().get(span.start as usize..span.end as usize),
            Some("this" | "super")
        )
    {
        // Implicit receivers inherit the member token's span during lowering.
        // Keep queries on the authored member rather than the synthetic `this`.
        struct ImplicitMember<F, A> {
            receiver_span: Span,
            cb: F,
            result: Option<A>,
        }

        impl<'ctx, F, A> Visitor<'ctx> for ImplicitMember<F, A>
        where
            F: Fn(&ir::Expr<'ctx>) -> A,
        {
            fn visit_expr(&mut self, expr: &ir::Expr<'ctx>) {
                if self.result.is_some() {
                    return;
                }
                let receiver = match expr {
                    ir::Expr::Field { receiver, .. } => Some(&**receiver),
                    ir::Expr::Call { call, .. } => call.receiver(),
                    _ => None,
                };
                if matches!(receiver, Some(ir::Expr::Local(ir::Local::This, span)) if *span == self.receiver_span)
                {
                    self.result = Some((self.cb)(expr));
                } else {
                    match expr {
                        ir::Expr::Construct { args, .. } => {
                            args.iter().for_each(|arg| self.visit_expr(arg));
                        }
                        ir::Expr::NewClosure { closure, .. } => self.visit_block(&closure.block),
                        ir::Expr::Call { call, span } => self.visit_call(call, *span),
                        ir::Expr::Field { receiver, .. } => self.visit_expr(receiver),
                        ir::Expr::Assign { place, expr, .. } => {
                            self.visit_expr(place);
                            self.visit_expr(expr);
                        }
                        ir::Expr::Index { array, index, .. } => {
                            self.visit_expr(array);
                            self.visit_expr(index);
                        }
                        ir::Expr::Conditional {
                            condition,
                            then,
                            else_,
                            ..
                        } => {
                            self.visit_expr(condition);
                            self.visit_expr(then);
                            self.visit_expr(else_);
                        }
                        ir::Expr::DynCast { expr, .. } => self.visit_expr(expr),
                        _ => {}
                    }
                }
            }
        }

        let mut visitor = ImplicitMember {
            receiver_span: *span,
            cb: |expr: &ir::Expr<'ctx>| cb(Some(expr)),
            result: None,
        };
        visitor.visit_block(block);
        if let Some(result) = visitor.result {
            return result;
        }
    }
    cb(expr)
}

#[derive(Debug, Clone, Copy)]
pub enum AtContext {
    Expr,
}

pub fn type_of<'ctx>(
    expr: &ir::Expr<'ctx>,
    func: &LoweredFunction<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Option<Type<'ctx>> {
    redscript_compiler_api::expr_type(expr, func, symbols).ok()
}

#[cfg(test)]
mod tests {
    use redscript_compiler_api::SourceMapExt;
    use redscript_compiler_frontend::{
        CompileErrorReporter, LoweredCompilationUnit, TypeInterner, infer_from_sources,
    };

    use super::*;

    fn lower<'ctx>(
        sources: &'ctx SourceMap,
        interner: &'ctx TypeInterner,
    ) -> (LoweredCompilationUnit<'ctx>, Symbols<'ctx>) {
        sources.populate_boot_lib();
        let mut reporter = CompileErrorReporter::default();
        let result = infer_from_sources(
            sources,
            Symbols::with_default_types(),
            &mut reporter,
            interner,
        );
        let diagnostics = reporter.into_reported();
        assert!(
            diagnostics.iter().all(|diagnostic| !diagnostic.is_error()),
            "{diagnostics:?}"
        );
        result
    }

    fn function<'a, 'ctx>(
        unit: &'a LoweredCompilationUnit<'ctx>,
        symbols: &Symbols<'ctx>,
    ) -> &'a LoweredFunction<'ctx> {
        let entry = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "Test")
            .unwrap();
        &unit.functions[entry.key()]
    }

    fn definition_at<'ctx>(
        function: &LoweredFunction<'ctx>,
        symbols: &Symbols<'ctx>,
        sources: &'ctx SourceMap,
        pos: usize,
    ) -> Option<Span> {
        ExprAt::new(None, Some(function), None, symbols, sources, None)
            .with_expr_at(pos as u32, |at| at.definition_span())
    }

    #[test]
    fn local_definitions_follow_shadowed_identity() {
        let source = "func Test() -> Int32 { let value = 1; if true { let value = 2; value = 3; } return value; }";
        let sources = SourceMap::new();
        let file = sources.push_back("query.reds", source);
        let interner = TypeInterner::default();
        let (unit, symbols) = lower(&sources, &interner);
        let function = function(&unit, &symbols);
        for (usage, declaration) in [("value = 3", "value = 2"), ("value;", "value = 1")] {
            let start = source.find(declaration).unwrap() as u32;
            let expected = Some(Span::new(start, start + 5, file));
            for offset in 0..5 {
                assert_eq!(
                    definition_at(
                        function,
                        &symbols,
                        &sources,
                        source.find(usage).unwrap() + offset
                    ),
                    expected
                );
            }
        }
    }

    #[test]
    fn nested_closure_locals_and_captures_find_original_declarations() {
        let source = "func Test() -> Int32 { let value = 1; let first: () -> Int32 = () -> { let value = value; let second: () -> Int32 = () -> { let inner = value; return inner; }; return second(); }; let sibling: () -> Int32 = () -> { let value = 3; return value; }; sibling(); return first(); }";
        let sources = SourceMap::new();
        let file = sources.push_back("query.reds", source);
        let interner = TypeInterner::default();
        let (unit, symbols) = lower(&sources, &interner);
        let function = function(&unit, &symbols);
        for (usage, declaration, name) in [
            ("value; let second", "value = 1", "value"),
            ("value; return inner", "value = value", "value"),
            ("inner;", "inner =", "inner"),
            ("second();", "second:", "second"),
            ("value; }; sibling", "value = 3", "value"),
        ] {
            let start = source.find(declaration).unwrap() as u32;
            assert_eq!(
                definition_at(function, &symbols, &sources, source.find(usage).unwrap()),
                Some(Span::new(start, start + name.len() as u32, file)),
                "{usage}"
            );
        }
    }

    #[test]
    fn captures_across_two_closures_find_outer_local_but_not_parameters() {
        let source = "func Test(param: Int32) -> Int32 { let value = 1; let first: (Int32) -> Int32 = (arg: Int32) -> { let second: () -> Int32 = () -> { param; arg; return value; }; return second(); }; return first(param); }";
        let sources = SourceMap::new();
        let file = sources.push_back("query.reds", source);
        let interner = TypeInterner::default();
        let (unit, symbols) = lower(&sources, &interner);
        let function = function(&unit, &symbols);
        let start = source.find("value =").unwrap() as u32;
        assert_eq!(
            definition_at(
                function,
                &symbols,
                &sources,
                source.find("value; }").unwrap()
            ),
            Some(Span::new(start, start + 5, file))
        );
        for usage in ["param;", "arg;", "param);"] {
            assert_eq!(
                definition_at(function, &symbols, &sources, source.find(usage).unwrap()),
                None,
                "{usage}"
            );
        }
    }

    #[test]
    fn synthetic_locals_and_missing_context_have_no_definition() {
        let source = "func Test() -> Int32 { [1, 2]; let total = 0; return total; }";
        let sources = SourceMap::new();
        sources.push_back("query.reds", source);
        let interner = TypeInterner::default();
        let (unit, symbols) = lower(&sources, &interner);
        let function = function(&unit, &symbols);
        let generated: Vec<_> = function
            .locals
            .iter()
            .filter(|info| info.temporary_purpose.is_some())
            .collect();
        assert!(!generated.is_empty());
        assert!(generated.iter().any(|info| info.span.is_some()));
        for info in generated {
            let expr = ir::Expr::Local(info.id, function.span);
            assert_eq!(
                ExprAt::new(Some(&expr), Some(function), None, &symbols, &sources, None)
                    .definition_span(),
                None
            );
        }
        let expr = function
            .block
            .find_at(source.rfind("total").unwrap() as u32)
            .unwrap();
        assert_eq!(
            ExprAt::new(Some(expr), None, None, &symbols, &sources, None).definition_span(),
            None
        );
        let missing = ir::Expr::Local(ir::Local::Var(u16::MAX), function.span);
        assert_eq!(
            ExprAt::new(
                Some(&missing),
                Some(function),
                None,
                &symbols,
                &sources,
                None
            )
            .definition_span(),
            None
        );
    }
}
