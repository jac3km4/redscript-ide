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
        let has_header = if let (Some(expr), Some(func)) = (inner.expr(), inner.func()) {
            if let ir::Expr::Call { call, .. } = expr {
                writeln!(f, "### Signature")?;
                writeln!(f, "{}", CallDisplay::new(call, inner.symbols()))?;
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
                let (name, func) = self.symbols.get_free_function(function).unwrap();
                writeln!(f, "{name}{}", FunctionTypeDisplay::new(func.type_()))?;
                func.doc()
            }
            &ir::Call::Static { method, .. } => {
                let (name, func) = self.symbols.get_method(method).unwrap();
                writeln!(
                    f,
                    "{}::{name}{}",
                    method.parent(),
                    FunctionTypeDisplay::new(func.type_())
                )?;
                func.doc()
            }
            ir::Call::Instance { method, .. } => {
                let (name, func) = self.symbols.get_method(*method).unwrap();
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
            write!(f, "{}: {}", param.name(), param.type_())?;
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
        for l in self.doc {
            writeln!(f, "{}", l.strip_prefix("///").unwrap_or(l).trim())?;
        }
        Ok(())
    }
}
