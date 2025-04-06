use lsp_types as lsp;
use redscript_compiler_api::{Field, FunctionType};

use crate::display::{DocDisplay, FunctionTypeDisplay, SnippetDisplay};

pub fn method<'ctx>(
    name: &'ctx str,
    typ: &FunctionType<'ctx>,
    doc: &[&'ctx str],
) -> lsp_types::CompletionItem {
    let detail = FunctionTypeDisplay::new(typ).to_string();
    let snippet_display = SnippetDisplay::new(name, typ).to_string();
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
        insert_text: Some(snippet_display),
        insert_text_format: Some(lsp::InsertTextFormat::SNIPPET),
        ..Default::default()
    }
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
