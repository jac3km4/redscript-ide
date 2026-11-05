use std::fmt;

use redscript_compiler_api::{FunctionType, Symbols, ir};

use crate::query::{ExprAt, type_of};

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
                writeln!(f, "### Field")?;
                let (name, field) = inner.symbols().get_field(*field).ok_or(fmt::Error)?;
                let typ = inner.expr_type().unwrap_or_else(|| field.type_().clone());
                writeln!(f, "```{name}: {typ}```")?;
                writeln!(f, "{}", DocDisplay::new(field.doc()))?;
                true
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
        if let Some(typ) = inner.type_() {
            if has_header {
                writeln!(f, "---")?;
            }
            writeln!(f, "### Symbol")?;
            writeln!(f, "```\n{typ}\n```")?;
            writeln!(f, "{}", DocDisplay::new(inner.symbols()[typ].doc()))?;
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
        writeln!(f, "```")?;
        let doc = match self.call {
            &ir::Call::FreeFunction { function, .. } => {
                let (name, func) = self.symbols.get_free_function(function).ok_or(fmt::Error)?;
                writeln!(f, "{name}{}", FunctionTypeDisplay::new(func.type_()))?;
                func.doc()
            }
            &ir::Call::Static { method, .. } => {
                let (name, func) = self.symbols.get_method(method).ok_or(fmt::Error)?;
                writeln!(
                    f,
                    "{}::{name}{}",
                    method.parent(),
                    FunctionTypeDisplay::new(func.type_())
                )?;
                func.doc()
            }
            ir::Call::Instance { method, .. } => {
                let (name, func) = self.symbols.get_method(*method).ok_or(fmt::Error)?;
                writeln!(
                    f,
                    "{}::{name}{}",
                    method.parent(),
                    FunctionTypeDisplay::new(func.type_())
                )?;
                func.doc()
            }
            ir::Call::Closure { closure_type, .. } => {
                if let Ok(ft) = closure_type.coalesced(self.symbols) {
                    writeln!(f, "{ft}")?;
                }
                return Ok(());
            }
        };
        writeln!(f, "```")?;
        writeln!(f, "{}", DocDisplay::new(doc))?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct FunctionTypeDisplay<'ctx, 'a> {
    func: &'a FunctionType<'ctx>,
}

impl<'ctx, 'a> FunctionTypeDisplay<'ctx, 'a> {
    pub fn new(func: &'a FunctionType<'ctx>) -> Self {
        Self { func }
    }
}

impl fmt::Display for FunctionTypeDisplay<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "(")?;
        for (i, param) in self.func.params().iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{param}")?;
        }
        write!(f, ") -> {}", self.func.return_type())?;
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

    #[test]
    fn function_type_without_parameters() {
        let func = FunctionType::new([], [], Type::nullary(predef::INT32));
        assert_eq!(FunctionTypeDisplay::new(&func).to_string(), "() -> Int32");
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
