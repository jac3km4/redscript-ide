use std::collections::BTreeMap;
use std::fmt;
use std::path::PathBuf;

use hashbrown::HashMap;
use lsp_types as lsp;
use redscript_code_edit::{CodeEdit, TextEdit};
use redscript_compiler_api::ast::Span;
use redscript_compiler_api::{Diagnostic, LowerError, LoweredCompilationUnit, Symbols, ast};
use serde::{Deserialize, Serialize};

use crate::diagnostics::range;
use crate::dialect::Dialect;
use crate::server::LspContext;

pub(crate) type FixMap = HashMap<PathBuf, BTreeMap<(u32, u32), Vec<Fix>>>;

pub(crate) fn generate_fix<'ctx>(
    diag: &Diagnostic<'ctx>,
    diag_file: &ast::File,
    unit: Option<&LoweredCompilationUnit<'ctx>>,
    symbols: &Symbols<'ctx>,
    sources: &'ctx ast::SourceMap,
    ctx: &LspContext,
) -> Option<Fix> {
    if let Some(fix) = generate_ide_fix(diag, diag_file, unit, ctx) {
        return Some(fix);
    }
    let code_edit = CodeEdit::from_diagnostic(diag)
        .or_else(|| private_import_code_edit(diag, diag_file, symbols))?;
    let target_name = match &code_edit {
        CodeEdit::FixStructNewConstructor(type_id, _) | CodeEdit::MakeTypePublic(type_id) => {
            Some(type_id.simple_name().into_owned())
        }
        CodeEdit::MakeMethodPublic(method_id) => symbols
            .get_method(*method_id)
            .map(|(name, _)| name.to_owned()),
        CodeEdit::MakeFieldPublic(field_id) => symbols
            .get_field(*field_id)
            .map(|(name, _)| name.to_owned()),
    };
    let edit = TextEdit::from_code_edit(&code_edit, symbols, sources).ok()?;
    let file = sources.get(edit.file)?;
    if Dialect::of(file.path()) != Some(Dialect::Community) {
        return None;
    }
    let edit_range = range(Span::new(edit.start as _, edit.end as _, edit.file), file)?;
    Some(Fix {
        kind: FixKind::from(&code_edit),
        target_name,
        diagnostic_range: range(diag.span(), diag_file),
        diagnostic_code: Some(diag.code().into()),
        uri: ctx.uri(file.path()).ok()?,
        edit: lsp::TextEdit::new(edit_range, edit.content.to_string()),
        expected_source: file.source().to_owned(),
    })
}

fn private_import_code_edit<'ctx>(
    diag: &Diagnostic<'ctx>,
    diag_file: &ast::File,
    symbols: &Symbols<'ctx>,
) -> Option<CodeEdit<'ctx>> {
    let Diagnostic::ImportIsPrivate(name, span) = diag else {
        return None;
    };
    let raw = diag_file
        .source()
        .get(span.start as usize..span.end as usize)?
        .trim();
    let rest = raw
        .strip_prefix("import")?
        .trim_start()
        .trim_end_matches(';')
        .trim();
    let compact: String = rest.chars().filter(|ch| !ch.is_whitespace()).collect();
    let qualified = if let Some(prefix) = compact.strip_suffix(".*") {
        format!("{prefix}.{name}")
    } else if let Some((prefix, _)) = compact.split_once(".{") {
        format!("{prefix}.{name}")
    } else if compact == *name || compact.ends_with(&format!(".{name}")) {
        compact
    } else {
        return None;
    };
    let (type_id, _) = symbols
        .types()
        .find(|(id, def)| id.as_str() == qualified && def.span().is_some())?;
    Some(CodeEdit::MakeTypePublic(type_id))
}

fn generate_ide_fix<'ctx>(
    diag: &Diagnostic<'ctx>,
    diag_file: &ast::File,
    unit: Option<&LoweredCompilationUnit<'ctx>>,
    ctx: &LspContext,
) -> Option<Fix> {
    if Dialect::of(diag_file.path()) != Some(Dialect::Community) {
        return None;
    }
    let source = diag_file.source();
    let (kind, target_name, edit_span, replacement) = match diag {
        Diagnostic::LoweringError(LowerError::DeprecatedNameOf(span)) => {
            let slice = source.get(span.start as usize..span.end as usize)?;
            let inner = slice
                .strip_prefix("NameOf")?
                .trim_start()
                .strip_prefix('(')?
                .strip_suffix(')')?
                .trim();
            if !is_ascii_ident(inner) {
                return None;
            }
            (
                FixKind::ReplaceDeprecatedNameOf,
                Some(inner.to_owned()),
                *span,
                format!("NameOf<{inner}>()"),
            )
        }
        Diagnostic::LoweringError(LowerError::CallingBaseStaticMethod {
            type_id,
            name,
            span,
        }) => {
            let base_name = type_id.simple_name();
            let in_scope = if let Some(unit) = unit {
                unit.scopes
                    .get(&span.file)
                    .and_then(|scope| scope.get(&*base_name))
                    .is_some_and(|type_ref| type_ref.id() == Some(*type_id))
            } else {
                type_id.as_str() == base_name
            };
            if !in_scope {
                return None;
            }
            let slice = source.get(span.start as usize..span.end as usize)?;
            let receiver_len = slice
                .bytes()
                .take_while(|b| b.is_ascii_alphanumeric() || *b == b'_')
                .count();
            if receiver_len == 0 || !is_ascii_ident(&slice[..receiver_len]) {
                return None;
            }
            let after_dot = slice[receiver_len..]
                .trim_start()
                .strip_prefix('.')?
                .trim_start();
            let after_method = after_dot.strip_prefix(*name)?.trim_start();
            if !after_method.starts_with(['(', '<']) {
                return None;
            }
            let receiver_span = Span::new(
                span.start,
                span.start.checked_add(u32::try_from(receiver_len).ok()?)?,
                span.file,
            );
            (
                FixKind::QualifyBaseStaticMethod,
                Some(format!("{base_name}.{name}")),
                receiver_span,
                base_name.into_owned(),
            )
        }
        _ => return None,
    };
    let edit_range = range(edit_span, diag_file)?;
    Some(Fix {
        kind,
        target_name,
        diagnostic_range: range(diag.span(), diag_file),
        diagnostic_code: Some(diag.code().into()),
        uri: ctx.uri(diag_file.path()).ok()?,
        edit: lsp::TextEdit::new(edit_range, replacement),
        expected_source: source.to_owned(),
    })
}

fn is_ascii_ident(text: &str) -> bool {
    let mut chars = text.chars();
    chars
        .next()
        .is_some_and(|ch| ch.is_ascii_alphabetic() || ch == '_')
        && chars.all(|ch| ch.is_ascii_alphanumeric() || ch == '_')
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum FixScope {
    Workspace(PathBuf),
    Standalone(PathBuf),
}

#[derive(Debug, Serialize, Deserialize)]
pub(crate) enum FixRequest {
    FixOne(Box<Fix>),
    FixAll(FixKind, u64, FixScope),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub(crate) struct Fix {
    pub(crate) kind: FixKind,
    #[serde(default)]
    pub(crate) target_name: Option<String>,
    #[serde(default)]
    pub(crate) diagnostic_range: Option<lsp::Range>,
    #[serde(default)]
    pub(crate) diagnostic_code: Option<String>,
    pub(crate) uri: lsp::Uri,
    pub(crate) edit: lsp::TextEdit,
    pub(crate) expected_source: String,
}

impl Fix {
    pub(crate) fn title(&self) -> String {
        let Some(name) = &self.target_name else {
            return self.kind.to_string();
        };
        match self.kind {
            FixKind::FixStructNewConstructor => {
                format!("Fix by using `{name}` struct constructor")
            }
            FixKind::MakeMethodPublic => format!("Make method `{name}` public"),
            FixKind::MakeFieldPublic => format!("Make field `{name}` public"),
            FixKind::MakeTypePublic => format!("Make type `{name}` public"),
            FixKind::ReplaceDeprecatedNameOf => format!("Replace with `NameOf<{name}>()`"),
            FixKind::QualifyBaseStaticMethod => format!("Qualify static call as `{name}`"),
        }
    }

    pub(crate) fn matching_diagnostics(
        &self,
        context_diagnostics: &[lsp::Diagnostic],
    ) -> Vec<lsp::Diagnostic> {
        let Some(diag_range) = self.diagnostic_range else {
            return vec![];
        };
        context_diagnostics
            .iter()
            .filter(|diag| {
                diag.range == diag_range
                    && match (&diag.code, &self.diagnostic_code) {
                        (Some(lsp::NumberOrString::String(actual)), Some(expected)) => {
                            actual == expected
                        }
                        (None, None) => true,
                        _ => false,
                    }
            })
            .cloned()
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub(crate) enum FixKind {
    FixStructNewConstructor,
    MakeMethodPublic,
    MakeFieldPublic,
    MakeTypePublic,
    ReplaceDeprecatedNameOf,
    QualifyBaseStaticMethod,
}

impl From<&CodeEdit<'_>> for FixKind {
    fn from(edit: &CodeEdit<'_>) -> Self {
        match edit {
            CodeEdit::FixStructNewConstructor(_, _) => Self::FixStructNewConstructor,
            CodeEdit::MakeMethodPublic(_) => Self::MakeMethodPublic,
            CodeEdit::MakeFieldPublic(_) => Self::MakeFieldPublic,
            CodeEdit::MakeTypePublic(_) => Self::MakeTypePublic,
        }
    }
}

impl fmt::Display for FixKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::FixStructNewConstructor => write!(f, "Fix by using struct constructor"),
            Self::MakeMethodPublic => write!(f, "Make method public"),
            Self::MakeFieldPublic => write!(f, "Make field public"),
            Self::MakeTypePublic => write!(f, "Make type public"),
            Self::ReplaceDeprecatedNameOf => write!(f, "Replace deprecated NameOf syntax"),
            Self::QualifyBaseStaticMethod => {
                write!(f, "Qualify static method call with declaring type")
            }
        }
    }
}
