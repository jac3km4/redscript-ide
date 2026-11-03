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
