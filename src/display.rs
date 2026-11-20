use std::fmt;

use redscript_compiler_api::types::{Type, TypeApp};
use redscript_compiler_api::{FunctionType, Symbols, ir};

use crate::query::{ExprAt, SpecializedFunctionType, specialize_function_type, type_of};

pub struct ExprAtDisplay<'ctx, 'a> {
    inner: ExprAt<'ctx, 'a>,
}

impl<'ctx, 'a> ExprAtDisplay<'ctx, 'a> {
    pub fn new(expr_at: ExprAt<'ctx, 'a>) -> Self {
        Self { inner: expr_at }
    }
}

impl fmt::Display for ExprAtDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let inner = &self.inner;
        let has_header = if let Some(expr) = inner.expr()
            && let Some(func) = inner.func()
        {
            if let ir::Expr::Call { call, .. } = expr {
                writeln!(f, "### Signature")?;
                writeln!(f, "{}", CallDisplay::new(call, inner.symbols()))?;
                true
            } else if let ir::Expr::Field { field, .. } = expr {
                match inner.symbols().get_field(*field) {
                    Some((name, field)) => {
                        writeln!(f, "### Field")?;
                        let typ = inner.expr_type().unwrap_or_else(|| field.type_().clone());
                        writeln!(f, "```{name}: {typ}```")?;
                        writeln!(f, "{}", DocDisplay::new(field.doc()))?;
                        true
                    }
                    None => false,
                }
            } else if let Some(typ) = type_of(expr, func, inner.symbols()) {
                writeln!(f, "### Type")?;
                writeln!(f, "```\n{typ}\n```")?;
                true
            } else {
                false
            }
        } else {
            false
        };
        if let Some(typ) = inner.type_()
            && let Some(def) = inner.symbols().get_type(typ)
        {
            if has_header {
                writeln!(f, "---")?;
            }
            writeln!(f, "### Symbol")?;
            writeln!(f, "```\n{typ}\n```")?;
            writeln!(f, "{}", DocDisplay::new(def.doc()))?;
        };
        Ok(())
    }
}

#[derive(Debug)]
pub struct CallDisplay<'ctx, 'a> {
    call: &'a ir::Call<'ctx>,
    symbols: &'a Symbols<'ctx>,
}

impl<'ctx, 'a> CallDisplay<'ctx, 'a> {
    pub fn new(call: &'a ir::Call<'ctx>, symbols: &'a Symbols<'ctx>) -> Self {
        Self { call, symbols }
    }
}

impl fmt::Display for CallDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // A closure whose type cannot be coalesced has no signature to render;
        // omit the block entirely rather than emitting an empty fence.
        if let ir::Call::Closure { closure_type, .. } = self.call
            && closure_type.coalesced(self.symbols).is_err()
        {
            return Ok(());
        }
        writeln!(f, "```")?;
        let doc = match self.call {
            &ir::Call::FreeFunction { function, .. } => {
                let Some((name, func)) = self.symbols.get_free_function(function) else {
                    writeln!(f, "```")?;
                    return Ok(());
                };
                writeln!(f, "{name}{}", FunctionTypeDisplay::new(func.type_()))?;
                func.doc()
            }
            ir::Call::Static {
                parent_id,
                parent_type_args,
                method,
                type_args,
                ..
            } => {
                let Some((name, func)) = self.symbols.get_method(*method) else {
                    writeln!(f, "```")?;
                    return Ok(());
                };
                let specialized = coalesce_types(parent_type_args, self.symbols)
                    .zip(coalesce_types(type_args, self.symbols))
                    .and_then(|(args, call_args)| {
                        let receiver = TypeApp::new(*parent_id, args);
                        specialize_function_type(
                            method.parent(),
                            func.type_(),
                            &receiver,
                            &call_args,
                            self.symbols,
                        )
                    });
                writeln!(
                    f,
                    "{}::{name}{}",
                    method.parent(),
                    FunctionTypeDisplay::maybe_specialized(func.type_(), specialized.as_ref())
                )?;
                func.doc()
            }
            ir::Call::Instance {
                method,
                receiver_type,
                type_args,
                ..
            } => {
                let Some((name, func)) = self.symbols.get_method(*method) else {
                    writeln!(f, "```")?;
                    return Ok(());
                };
                let specialized = receiver_type
                    .coalesced(self.symbols)
                    .ok()
                    .zip(coalesce_types(type_args, self.symbols))
                    .and_then(|(receiver, args)| {
                        specialize_function_type(
                            method.parent(),
                            func.type_(),
                            &receiver,
                            &args,
                            self.symbols,
                        )
                    });
                writeln!(
                    f,
                    "{}::{name}{}",
                    method.parent(),
                    FunctionTypeDisplay::maybe_specialized(func.type_(), specialized.as_ref())
                )?;
                func.doc()
            }
            ir::Call::Closure { closure_type, .. } => {
                if let Ok(ft) = closure_type.coalesced(self.symbols) {
                    writeln!(f, "{ft}")?;
                }
                // A closure has no documentation to append.
                &[]
            }
        };
        writeln!(f, "```")?;
        writeln!(f, "{}", DocDisplay::new(doc))?;
        Ok(())
    }
}

fn coalesce_types<'ctx>(
    types: &[ir::Type<'ctx>],
    symbols: &Symbols<'ctx>,
) -> Option<Vec<Type<'ctx>>> {
    types
        .iter()
        .map(|typ| typ.coalesced(symbols).ok())
        .collect()
}

#[derive(Debug)]
pub struct FunctionTypeDisplay<'ctx, 'a> {
    func: &'a FunctionType<'ctx>,
    specialized: Option<&'a SpecializedFunctionType<'ctx>>,
}

impl<'ctx, 'a> FunctionTypeDisplay<'ctx, 'a> {
    pub fn new(func: &'a FunctionType<'ctx>) -> Self {
        Self {
            func,
            specialized: None,
        }
    }

    /// Render a declaration with concrete parameter and return types in place of
    /// its generic parameters, falling back to the declared signature.
    pub fn maybe_specialized(
        func: &'a FunctionType<'ctx>,
        specialized: Option<&'a SpecializedFunctionType<'ctx>>,
    ) -> Self {
        Self { func, specialized }
    }
}

impl fmt::Display for FunctionTypeDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.func.params().iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            match self.specialized.and_then(|spec| spec.params.get(i)) {
                Some(typ) => {
                    let flags = param.flags();
                    if flags.is_optional() {
                        write!(f, "opt ")?;
                    }
                    if flags.is_const() {
                        write!(f, "const ")?;
                    }
                    if flags.is_out() {
                        write!(f, "out ")?;
                    }
                    if flags.is_short_circuit() {
                        write!(f, "skip ")?;
                    }
                    write!(f, "{}: {}", param.name(), typ)?;
                }
                None => write!(f, "{param}")?,
            }
        }
        let return_type = match self.specialized {
            Some(specialized) => specialized.return_type.to_string(),
            None => self.func.return_type().to_string(),
        };
        write!(f, ") -> {return_type}")?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct SnippetDisplay<'ctx, 'a> {
    name: &'ctx str,
    func: &'a FunctionType<'ctx>,
}

impl<'ctx, 'a> SnippetDisplay<'ctx, 'a> {
    pub fn new(name: &'ctx str, func: &'a FunctionType<'ctx>) -> Self {
        Self { name, func }
    }
}

impl fmt::Display for SnippetDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}(", self.name)?;
        for (i, param) in self.func.params().iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "${{{}:{}}}", i + 1, param.name())?;
        }
        write!(f, ")")?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use redscript_compiler_api::ast::SourceMap;
    use redscript_compiler_api::{CompileErrorReporter, SourceMapExt, TypeInterner};
    use redscript_compiler_frontend::types::Type;
    use redscript_compiler_frontend::{Param, ParamFlags, PolyType, infer_from_sources, predef};

    use super::*;

    #[test]
    fn function_type_preserves_parameter_qualifiers() {
        let flags = [
            ParamFlags::new(),
            ParamFlags::new().with_is_optional(true),
            ParamFlags::new().with_is_const(true),
            ParamFlags::new().with_is_out(true),
            ParamFlags::new().with_is_short_circuit(true),
            ParamFlags::new()
                .with_is_optional(true)
                .with_is_const(true)
                .with_is_out(true)
                .with_is_short_circuit(true),
        ];
        let params = flags
            .into_iter()
            .map(|flags| Param::new("value", flags, Type::nullary(predef::INT32), None))
            .collect::<Vec<_>>();
        let func = FunctionType::new([], params, Type::nullary(predef::BOOL));

        assert_eq!(
            FunctionTypeDisplay::new(&func).to_string(),
            "(value: Int32, opt value: Int32, const value: Int32, out value: Int32, skip value: Int32, opt const out skip value: Int32) -> Bool"
        );
    }

    fn field_hover(receiver: &str, expected: &str, invalid_receiver: bool) {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "display.reds",
            format!(
                r#"
                class Box<T> {{
                    /// The stored value.
                    public let value: T;
                }}
                class Derived<T> extends Box<array<T>> {{}}
                func Test(value: {receiver}) -> {expected} {{ return value.value; }}
                "#
            ),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (mut unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let diagnostics = reporter.into_reported();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let entry = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "Test")
            .unwrap();
        let function = &mut unit.functions[entry.key()];
        let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back_mut().unwrap() else {
            panic!("expected field return expression")
        };
        let ir::Expr::Field { receiver_type, .. } = &mut **expr else {
            panic!("expected field expression")
        };
        if invalid_receiver {
            *receiver_type = PolyType::nullary(predef::INT32)
                .upper_bound(&symbols)
                .unwrap()
                .into_owned()
                .into();
        }
        let ir::Stmt::Return(Some(expr), _) = function.block.stmts.back().unwrap() else {
            unreachable!()
        };
        let at = ExprAt::new(Some(expr), Some(function), None, &symbols, &sources, None);
        if invalid_receiver {
            assert!(at.expr_type().is_none());
        }
        let typ = if invalid_receiver { "T" } else { expected };
        assert_eq!(
            at.display().to_string(),
            format!("### Field\n```value: {typ}```\n---\nThe stored value.\n\n")
        );
    }

    #[test]
    fn field_hover_instantiates_generic_type_and_retains_docs() {
        field_hover("Box<Int32>", "Int32", false);
    }

    #[test]
    fn field_hover_instantiates_inherited_generic_type_and_retains_docs() {
        field_hover("Derived<Int32>", "array<Int32>", false);
    }

    #[test]
    fn field_hover_falls_back_to_declaration_type_and_retains_docs() {
        field_hover("Box<Int32>", "Int32", true);
    }

    fn call_hover(receiver: &str, call: &str, expected: &str) {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back(
            "display.reds",
            format!(
                r#"
                class Box<T> {{
                    /// The stored value.
                    public let value: T;
                    /// Reads the value.
                    public func Get() -> T {{ return this.value; }}
                    /// Replaces the value.
                    public func Replace(replacement: T) -> T {{ return replacement; }}
                }}
                class Derived<T> extends Box<array<T>> {{}}
                func Test(value: {receiver}) {{ value.{call}; }}
                "#
            ),
        );
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let diagnostics = reporter.into_reported();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let entry = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "Test")
            .unwrap();
        let function = &unit.functions[entry.key()];
        let ir::Stmt::Expr(expr) = function.block.stmts.back().unwrap() else {
            panic!("expected call expression statement")
        };
        let at = ExprAt::new(
            Some(&**expr),
            Some(function),
            None,
            &symbols,
            &sources,
            None,
        );
        let display = at.display().to_string();
        assert!(display.contains(expected), "{display}");
        assert!(!display.contains("-> T"), "{display}");
    }

    #[test]
    fn call_hover_specializes_generic_receiver_return_type() {
        call_hover("Box<Int32>", "Get()", "Box::Get() -> Int32");
    }

    #[test]
    fn call_hover_specializes_inherited_generic_return_type() {
        call_hover("Derived<Int32>", "Get()", "Box::Get() -> array<Int32>");
    }

    #[test]
    fn call_hover_specializes_generic_receiver_parameters_and_return() {
        call_hover(
            "Box<Int32>",
            "Replace(value.Get())",
            "Box::Replace(replacement: Int32) -> Int32",
        );
    }

    #[test]
    fn call_hover_specializes_inherited_generic_parameters_and_return() {
        call_hover(
            "Derived<Int32>",
            "Replace(value.Get())",
            "Box::Replace(replacement: array<Int32>) -> array<Int32>",
        );
    }

    #[test]
    fn function_type_without_parameters() {
        let func = FunctionType::new([], [], Type::nullary(predef::INT32));
        assert_eq!(FunctionTypeDisplay::new(&func).to_string(), "() -> Int32");
    }

    #[test]
    fn closure_call_hover_closes_code_fence() {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        let source =
            "func Test() -> Int32 { let f: () -> Int32 = () -> { return 1; }; return f(); }";
        sources.push_back("display.reds", source);
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        let diagnostics = reporter.into_reported();
        assert!(diagnostics.is_empty(), "{diagnostics:?}");
        let entry = symbols
            .free_functions()
            .find(|entry| entry.name().to_string() == "Test")
            .unwrap();
        let function = &unit.functions[entry.key()];
        let expr = function
            .block
            .find_at(source.find("f();").unwrap() as u32 + 1)
            .expect("closure call expression");
        let ir::Expr::Call { call, .. } = expr else {
            panic!("expected a call expression")
        };
        assert!(matches!(**call, ir::Call::Closure { .. }));
        let display = CallDisplay::new(call, &symbols).to_string();
        assert!(display.starts_with("```\n"), "{display:?}");
        assert!(display.trim_end().ends_with("```"), "{display:?}");
        assert_eq!(display.matches("```").count(), 2, "{display:?}");
    }

    #[test]
    fn missing_type_symbols_render_nothing_instead_of_panicking() {
        let sources = SourceMap::new();
        sources.populate_boot_lib();
        sources.push_back("display.reds", "func Test() -> Int32 { return 1; }");
        let interner = TypeInterner::default();
        let mut reporter = CompileErrorReporter::default();
        let (_unit, symbols) = infer_from_sources(
            &sources,
            Symbols::with_default_types(),
            &mut reporter,
            &interner,
        );
        assert!(reporter.into_reported().is_empty());
        // The interner may know a name that has no definition in this snapshot.
        let unknown = interner.intern("NoSuchType");
        assert!(symbols.get_type(unknown).is_none());
        let at = ExprAt::new(None, None, Some(unknown), &symbols, &sources, None);
        assert!(at.definition_span().is_none());
        assert_eq!(at.display().to_string(), "");
    }
}

#[derive(Debug)]
pub struct DocDisplay<'ctx, 'a> {
    doc: &'a [&'ctx str],
}

impl<'ctx, 'a> DocDisplay<'ctx, 'a> {
    pub fn new(doc: &'a [&'ctx str]) -> Self {
        Self { doc }
    }
}

impl fmt::Display for DocDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.doc.is_empty() {
            return Ok(());
        }
        writeln!(f, "---")?;
        let shared_prefix = self
            .doc
            .iter()
            .map(|&line| {
                let trimmed = line.strip_prefix("///").unwrap_or(line).trim_ascii_start();
                line.len() - trimmed.len()
            })
            .min()
            .unwrap_or(0);
        for line in self.doc {
            writeln!(f, "{}", &line[shared_prefix..])?;
        }
        Ok(())
    }
}
