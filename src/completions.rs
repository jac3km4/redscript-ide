use std::ops::Range;

use lsp_types as lsp;
use redscript_compiler_api::types::{Type, predef};
use redscript_compiler_api::{Field, FunctionType, PolyType, Symbols, TypeId, TypeSchema, ast, ir};

use crate::dialect::Dialect;
use crate::display::{DocDisplay, FunctionTypeDisplay, SnippetDisplay};
use crate::query::{AtContext, ExprAt, ReceiverTypeEnvs};

pub fn method<'ctx>(
    name: &'ctx str,
    typ: &FunctionType<'ctx>,
    doc: &[&'ctx str],
) -> lsp_types::CompletionItem {
    let detail = FunctionTypeDisplay::new(typ).to_string();
    let snippet_display = SnippetDisplay::new(name, typ).to_string();
    method_with_signature(name, detail, snippet_display, doc)
}

/// Present an instance method with its declaration instantiated at `receiver`.
/// Falls back to the declared signature when the type arguments cannot be
/// resolved.
pub fn instance_method<'ctx>(
    name: &'ctx str,
    declaring_type: TypeId<'ctx>,
    typ: &FunctionType<'ctx>,
    envs: &ReceiverTypeEnvs<'ctx, '_>,
    doc: &[&'ctx str],
) -> lsp_types::CompletionItem {
    let specialized = envs.function_type(declaring_type, typ, &[]);
    let detail = FunctionTypeDisplay::maybe_specialized(typ, specialized.as_ref()).to_string();
    let snippet_display = SnippetDisplay::new(name, typ).to_string();
    method_with_signature(name, detail, snippet_display, doc)
}

fn method_with_signature(
    name: &str,
    detail: String,
    snippet: String,
    doc: &[&str],
) -> lsp::CompletionItem {
    lsp::CompletionItem {
        label: name.to_owned(),
        label_details: Some(lsp::CompletionItemLabelDetails {
            detail: Some(detail.clone()),
            description: None,
        }),
        detail: Some(detail),
        kind: Some(lsp::CompletionItemKind::METHOD),
        documentation: Some(lsp::Documentation::MarkupContent(lsp::MarkupContent {
            kind: lsp::MarkupKind::Markdown,
            value: DocDisplay::new(doc).to_string(),
        })),
        insert_text: Some(snippet),
        insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
        ..Default::default()
    }
}

/// Present an actual boot intrinsic as a receiver-bound, specialized method.
/// The semantic adapter supplies the public member name and intrinsic identity.
pub fn array_method<'ctx>(
    name: &str,
    intrinsic: ir::Intrinsic,
    element: &Type<'ctx>,
    symbols: &Symbols<'ctx>,
) -> Option<lsp::CompletionItem> {
    let function = symbols
        .free_functions()
        .find(|entry| entry.func().intrinsic() == Some(intrinsic))?;
    let typ = function.func().type_();
    let mut type_vars = typ.type_vars();
    let type_var = type_vars.next()?;
    if type_vars.next().is_some() {
        return None;
    }
    let receiver = typ.params().first()?.type_().upper_bound()?;
    if receiver.id() != predef::ARRAY {
        return None;
    }
    let env = [(type_var, PolyType::from_type(element))]
        .into_iter()
        .collect();
    let specialize = |typ| {
        PolyType::from_type_with_env(typ, &env)
            .ok()?
            .coalesce(symbols)
            .ok()
    };
    let params = typ
        .params()
        .iter()
        .skip(1)
        .map(|param| Some(format!("{}: {}", param.name(), specialize(param.type_())?)))
        .collect::<Option<Vec<_>>>()?;
    let detail = format!(
        "({}) -> {}",
        params.join(", "),
        specialize(typ.return_type())?
    );
    let placeholders = typ
        .params()
        .iter()
        .skip(1)
        .enumerate()
        .map(|(index, param)| format!("${{{}:{}}}", index + 1, param.name()))
        .collect::<Vec<_>>()
        .join(", ");
    Some(method_with_signature(
        name,
        detail,
        format!("{name}({placeholders})"),
        function.func().doc(),
    ))
}

/// Present a field with its declared type instantiated at the receiver when the
/// specialization is available.
pub fn field(
    name: &str,
    field: &Field<'_>,
    specialized: Option<&Type<'_>>,
) -> lsp_types::CompletionItem {
    let detail = match specialized {
        Some(typ) => format!(": {typ}"),
        None => format!(": {}", field.type_()),
    };
    lsp::CompletionItem {
        label: name.to_owned(),
        label_details: Some(lsp::CompletionItemLabelDetails {
            detail: Some(detail.clone()),
            description: None,
        }),
        detail: Some(detail),
        kind: Some(lsp::CompletionItemKind::FIELD),
        documentation: Some(lsp::Documentation::MarkupContent(lsp::MarkupContent {
            kind: lsp::MarkupKind::Markdown,
            value: DocDisplay::new(field.doc()).to_string(),
        })),
        ..Default::default()
    }
}

pub fn enum_member(name: &str) -> lsp_types::CompletionItem {
    lsp::CompletionItem {
        label: name.to_owned(),
        kind: Some(lsp::CompletionItemKind::ENUM_MEMBER),
        ..Default::default()
    }
}

/// The pinned REDmod adapter reports a missing statement terminator as a
/// recovery *message* rather than a typed error kind. Keep the exact match in
/// one place so a future adapter revision with structured errors only needs a
/// single change.
const REDMOD_MISSING_SEMICOLON: &str = "expected ';' after statement in statement";

/// Repair only a missing terminator immediately after the completion expression.
/// The adapter deliberately rejects recovered trees, so reparse the temporary
/// source strictly instead of allowing invalid modules into normal analysis.
pub(crate) fn repair_completion_terminator(source: &mut String, member: Range<usize>) -> bool {
    let sources = ast::SourceMap::new();
    let id = sources.push_back("completion.script", "");
    let insertion = {
        let (_, errors) = redscript_redmod_adapter::parse_file_with_metadata(source, id);
        let [redscript_redmod_adapter::Error::Parse(error)] = errors.as_slice() else {
            return false;
        };
        // The pinned parser exposes this recovery as a message, not a typed kind.
        if error.to_string() != REDMOD_MISSING_SEMICOLON {
            return false;
        }
        let span = error.span();
        if span.start != span.end {
            return false;
        }
        span.start as usize
    };
    let Some(tail) = source.get(member.end..insertion) else {
        return false;
    };
    // Closing calls/indexes may separate the receiver from its statement end.
    // Do not repair another statement or an unfinished argument/operator.
    let Some(end) = completion_statement_end(tail) else {
        return false;
    };
    let insertion = member.end + end;
    source.insert(insertion, ';');
    let valid = {
        let (module, errors) = redscript_redmod_adapter::parse_file_with_metadata(source, id);
        module.is_some() && errors.is_empty()
    };
    if !valid {
        source.remove(insertion);
    }
    valid
}

// Only traverse closing delimiters and trivia, including nested block comments.
// Return the end of the expression, before trailing comments.
fn completion_statement_end(mut tail: &str) -> Option<usize> {
    let length = tail.len();
    let mut end = 0;
    loop {
        tail = tail.trim_start();
        if tail.is_empty() {
            return Some(end);
        }
        if tail.starts_with([')', ']']) {
            tail = &tail[1..];
            end = length - tail.len();
        } else if tail.starts_with("//") {
            tail = tail
                .find([
                    '\n', '\r', '\u{000B}', '\u{000C}', '\u{0085}', '\u{2028}', '\u{2029}',
                ])
                .map_or("", |newline| &tail[newline..]);
        } else {
            tail = tail.strip_prefix("/*")?;
            let mut depth = 1;
            while depth != 0 {
                if let Some(rest) = tail.strip_prefix("/*") {
                    depth += 1;
                    tail = rest;
                } else if let Some(rest) = tail.strip_prefix("*/") {
                    depth -= 1;
                    tail = rest;
                } else {
                    tail = tail.get(tail.chars().next()?.len_utf8()..)?;
                }
            }
        }
    }
}

/// Locate a contiguous member token at the cursor, including its leading dot.
/// Unicode letters are accepted here even though the parsers currently require
/// ASCII identifiers: erase the unfinished token before parsing the receiver.
pub(crate) fn member_completion_range(source: &str, pos: u32) -> Option<Range<usize>> {
    let pos = pos as usize;
    let prefix = source.get(..pos)?;
    let is_member_char = |ch: char| ch == '_' || ch.is_alphanumeric();
    let (dot, ch) = prefix
        .char_indices()
        .rev()
        .find(|(_, ch)| !is_member_char(*ch))?;
    if ch != '.' || dot == 0 {
        return None;
    }
    let suffix = source.get(pos..)?;
    let end = pos
        + suffix
            .char_indices()
            .find(|(_, ch)| !is_member_char(*ch))
            .map_or(suffix.len(), |(index, _)| index);
    Some(dot..end)
}

pub(crate) fn generate_completions(
    at: ExprAt<'_, '_>,
    dialect: Option<Dialect>,
) -> Result<Vec<lsp_types::CompletionItem>, anyhow::Error> {
    let mut completions = vec![];

    if let Some(typ) = at.type_()
        && matches!(at.context(), Some(AtContext::Expr))
        && let Some(def) = at.symbols().get_type(typ)
    {
        match def.schema() {
            TypeSchema::Aggregate(_) => {
                let methods = at
                    .symbols()
                    .query_methods(typ)
                    .filter(|m| m.func().flags().is_static())
                    .map(|e| method(e.name(), e.func().type_(), e.func().doc()));
                completions.extend(methods);
            }
            TypeSchema::Enum(enum_) => {
                let variants = enum_.variants().map(|(name, _)| enum_member(name));
                completions.extend(variants);
            }
            _ => {}
        };
    }

    let typ = at.expr_type();
    if let Some(typ) = typ.as_ref()
        && let Some(typ) = typ.unwrap_ref_or_self().upper_bound()
    {
        if dialect == Some(Dialect::Redmod)
            && typ.id() == redscript_compiler_api::types::predef::ARRAY
            && let [element] = typ.args()
        {
            completions.extend(
                redscript_redmod_adapter::array_operations()
                    .iter()
                    .filter_map(|operation| {
                        array_method(operation.name, operation.intrinsic, element, at.symbols())
                    }),
            );
        }

        let symbols = at.symbols();
        let envs = ReceiverTypeEnvs::new(typ, symbols);

        // A receiver can expose hundreds of inherited members; let a cancelled
        // request stop formatting them instead of blocking newer work.
        crate::ls::cancelled()?;
        let methods = symbols
            .query_methods(typ.id())
            .filter(|m| !m.func().flags().is_static())
            .map(|e| {
                instance_method(
                    e.name(),
                    e.key().parent(),
                    e.func().type_(),
                    &envs,
                    e.func().doc(),
                )
            });
        completions.extend(methods);

        crate::ls::cancelled()?;
        let fields = symbols
            .base_iter_with_self(typ.id())
            .flat_map(|(declaring_type, agg)| {
                let envs = &envs;
                agg.fields().iter().map(move |e| {
                    let specialized = envs.field_type(declaring_type, e.field().type_());
                    field(e.name(), e.field(), specialized.as_ref())
                })
            });
        completions.extend(fields);
    };

    Ok(completions)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn array_method_requires_a_real_intrinsic() {
        assert!(
            array_method(
                "Size",
                ir::Intrinsic::ArraySize,
                &Type::nullary(predef::INT32),
                &Symbols::with_default_types(),
            )
            .is_none()
        );
    }
}
