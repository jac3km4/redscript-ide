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
