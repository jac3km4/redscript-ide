use std::fmt::Display;

use redscript_compiler_api::ast::{SourceMap, Span};
use redscript_compiler_api::types::{Type, TypeApp};
use redscript_compiler_api::{FunctionType, LoweredFunction, PolyType, Symbols, TypeId, ir};

use crate::display::ExprAtDisplay;

#[derive(Clone)]
pub struct ExprAt<'ctx, 'a> {
    expr: Option<&'a ir::Expr<'ctx>>,
    func: Option<&'a LoweredFunction<'ctx>>,
    type_: Option<TypeId<'ctx>>,
    symbols: &'a Symbols<'ctx>,
    sources: &'ctx SourceMap,
}

impl<'ctx, 'a> ExprAt<'ctx, 'a> {
    pub fn new(
        expr: Option<&'a ir::Expr<'ctx>>,
        func: Option<&'a LoweredFunction<'ctx>>,
        type_: Option<TypeId<'ctx>>,
        symbols: &'a Symbols<'ctx>,
        sources: &'ctx SourceMap,
    ) -> Self {
        Self {
            expr,
            func,
            type_,
            symbols,
            sources,
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

    pub fn expr_type(&self) -> Option<Type<'ctx>> {
        if let (Some(expr), Some(func)) = (self.expr, self.func) {
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

pub fn type_of<'ctx>(
    expr: &ir::Expr<'ctx>,
    func: &LoweredFunction<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Option<Type<'ctx>> {
    let t = match expr {
        ir::Expr::NewClass { class_type, .. } => class_type,
        ir::Expr::NewStruct { struct_type, .. } => struct_type,
        ir::Expr::NewClosure { closure, .. } => &closure.typ,
        ir::Expr::Call { call, .. } => {
            let (fn_type, receiver_type_args, fn_type_args) = match &**call {
                ir::Call::FreeFunction {
                    function,
                    type_args,
                    ..
                } => (symbols[*function].type_(), None, type_args),
                ir::Call::Instance {
                    receiver_type,
                    method,
                    type_args,
                    ..
                } => {
                    let typ = receiver_type.coalesced(symbols).ok()?;
                    (
                        symbols[*method].type_(),
                        Some((typ.id(), typ.args().to_vec())),
                        type_args,
                    )
                }
                ir::Call::Static {
                    parent_id,
                    parent_type_args,
                    method,
                    type_args,
                    ..
                } => (
                    symbols[*method].type_(),
                    Some((
                        *parent_id,
                        parent_type_args
                            .iter()
                            .map(|t| t.coalesced(symbols).ok())
                            .collect::<Option<Vec<_>>>()?,
                    )),
                    type_args,
                ),
                ir::Call::Closure { closure_type, .. } => {
                    return closure_type.coalesced(symbols).ok()?.args().last().cloned();
                }
            };
            let fn_type_args = fn_type_args
                .iter()
                .map(|t| t.coalesced(symbols).ok())
                .collect::<Option<Vec<_>>>()?;
            return type_of_call(
                fn_type,
                receiver_type_args
                    .as_ref()
                    .map(|(id, args)| (*id, &args[..])),
                &fn_type_args,
                symbols,
            );
        }
        ir::Expr::Assign { place, .. } => return type_of(place, func, symbols),
        ir::Expr::Field {
            receiver_type,
            field,
            ..
        } => {
            let env = TypeApp::from_type(&receiver_type.coalesced(symbols).ok()?).type_env(symbols);
            return PolyType::from_type_with_env(symbols[*field].type_(), &env)
                .ok()?
                .coalesce(symbols)
                .ok();
        }
        ir::Expr::Index { array_type, .. } => match array_type.coalesced(symbols).ok()? {
            Type::Data(app) => return app.args().first().cloned(),
            _ => return None,
        },
        ir::Expr::Conditional { then, .. } => return type_of(then, func, symbols),
        ir::Expr::DynCast { target_type, .. } => target_type,
        &ir::Expr::Local(local, _) => return Some(func.find_local(local)?.clone()),
        &ir::Expr::Capture(local, _) => return Some(func.find_local(local)?.clone()),
        ir::Expr::Const(const_, _) => return Some(Type::nullary(const_.type_id())),
        _ => return None,
    };
    Some(t.coalesced(symbols).ok()?.into_type())
}

fn type_of_call<'ctx>(
    ft: &FunctionType<'ctx>,
    receiver: Option<(TypeId<'ctx>, &[Type<'ctx>])>,
    type_args: &[Type<'ctx>],
    symbols: &Symbols<'ctx>,
) -> Option<Type<'ctx>> {
    let env = receiver
        .iter()
        .flat_map(|&(id, args)| symbols[id].vars().zip(args))
        .chain(ft.type_vars().zip(type_args))
        .map(|(n, arg)| Some((n, PolyType::from_type(arg))))
        .collect::<Option<_>>()?;

    PolyType::from_type_with_env(ft.return_type(), &env)
        .ok()?
        .coalesce(symbols)
        .ok()
}
