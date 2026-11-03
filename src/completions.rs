use lsp_types as lsp;
use redscript_compiler_api::types::{Type, predef};
use redscript_compiler_api::{Field, FunctionType, PolyType, Symbols, ir};

use crate::display::{DocDisplay, FunctionTypeDisplay, SnippetDisplay};

pub fn method<'ctx>(
    name: &'ctx str,
    typ: &FunctionType<'ctx>,
    doc: &[&'ctx str],
) -> lsp_types::CompletionItem {
    let detail = FunctionTypeDisplay::new(typ).to_string();
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

pub fn field(name: &str, field: &Field<'_>) -> lsp_types::CompletionItem {
    let detail = format!(": {}", field.type_());
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
