//! Hierarchical outlines from the shared AST, independent of semantic compilation.
use std::path::Path;

use lsp_types as lsp;
use redscript_compiler_api::ast::{self, Span};

use crate::dialect::{self, Dialect};

pub(super) fn document_symbols(path: &Path, text: String) -> Vec<lsp::DocumentSymbol> {
    if Dialect::of(path).is_none() {
        return vec![];
    }
    let sources = ast::SourceMap::new();
    let id = sources.push_back(path, text);
    let parsed = dialect::parse(&sources);
    let Some(file) = sources.get(id) else {
        return vec![];
    };
    parsed
        .modules
        .iter()
        .find(|(file_id, _)| *file_id == id)
        .map(|(_, module)| items(&module.module().items, false, file))
        .unwrap_or_default()
}

fn items(
    declarations: &[(ast::SourceItemDecl<'_>, Span)],
    in_aggregate: bool,
    file: &ast::File,
) -> Vec<lsp::DocumentSymbol> {
    declarations
        .iter()
        .filter_map(|(declaration, span)| {
            let (name, name_span, kind, children) = match &declaration.item {
                ast::Item::Class(aggregate) | ast::Item::Struct(aggregate) => (
                    aggregate.name.0,
                    aggregate.name.1,
                    if matches!(declaration.item, ast::Item::Struct(_)) {
                        lsp::SymbolKind::STRUCT
                    } else {
                        lsp::SymbolKind::CLASS
                    },
                    Some(items(&aggregate.items, true, file)),
                ),
                ast::Item::Function(function) => (
                    function.name.0,
                    function.name.1,
                    if in_aggregate {
                        lsp::SymbolKind::METHOD
                    } else {
                        lsp::SymbolKind::FUNCTION
                    },
                    None,
                ),
                ast::Item::Let(field) => (field.name.0, field.name.1, lsp::SymbolKind::FIELD, None),
                ast::Item::Enum(enum_) => {
                    let children = enum_
                        .variants
                        .iter()
                        .filter_map(|(variant, span)| {
                            // The shared AST keeps the whole variant span, not its name span.
                            let name_span = Span::new(
                                span.start,
                                span.start.checked_add(variant.name.len() as u32)?,
                                span.file,
                            );
                            symbol(
                                variant.name,
                                lsp::SymbolKind::ENUM_MEMBER,
                                *span,
                                name_span,
                                None,
                                file,
                            )
                        })
                        .collect();
                    (
                        enum_.name.0,
                        enum_.name.1,
                        lsp::SymbolKind::ENUM,
                        Some(children),
                    )
                }
                ast::Item::Import(_) | ast::Item::Default(_) => return None,
            };
            symbol(name, kind, *span, name_span, children, file)
        })
        .collect()
}

fn symbol(
    name: &str,
    kind: lsp::SymbolKind,
    span: Span,
    name_span: Span,
    children: Option<Vec<lsp::DocumentSymbol>>,
    file: &ast::File,
) -> Option<lsp::DocumentSymbol> {
    if name_span.file != span.file || name_span.start < span.start || name_span.end > span.end {
        return None;
    }
    #[allow(deprecated)]
    Some(lsp::DocumentSymbol {
        name: name.to_owned(),
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range: super::range(span, file)?,
        selection_range: super::range(name_span, file)?,
        children,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn outline(path: &str, source: &str) -> Vec<lsp::DocumentSymbol> {
        document_symbols(Path::new(path), source.to_owned())
    }

    fn assert_ranges(source: &str, symbols: &[lsp::DocumentSymbol]) {
        for symbol in symbols {
            assert!(symbol.range.start <= symbol.selection_range.start);
            assert!(symbol.selection_range.end <= symbol.range.end);
            let start = source.find(&symbol.name).unwrap() as u32;
            assert_eq!(
                symbol.selection_range.start,
                crate::positions::position(source, start).unwrap()
            );
            assert_eq!(
                symbol.selection_range.end,
                crate::positions::position(source, start + symbol.name.len() as u32).unwrap()
            );
            if let Some(children) = &symbol.children {
                for child in children {
                    assert!(symbol.range.start <= child.range.start);
                    assert!(child.range.end <= symbol.range.end);
                }
                assert_ranges(source, children);
            }
        }
    }

    #[test]
    fn hierarchical_outlines_use_shared_ast_for_both_dialects() {
        for (path, source) in [
            (
                "outline.reds",
                "module Outline\nimport Other.*\n// 😀\npublic class Owner {\n public let value: MissingType;\n public func Act() { let local: Int32; }\n}\npublic struct Record { public let count: Int32; }\npublic enum Choice { First = 0, Second = 1 }\npublic func Global() {}",
            ),
            (
                "outline.sCrIpT",
                "// 😀\nclass Owner {\n var value : MissingType;\n function Act() { var local : Int32; }\n}\nstruct Record { var count : Int32; }\nenum Choice { First = 0, Second = 1 }\nfunction Global() {}",
            ),
        ] {
            let symbols = outline(path, source);
            assert_eq!(
                symbols
                    .iter()
                    .map(|s| (s.name.as_str(), s.kind))
                    .collect::<Vec<_>>(),
                [
                    ("Owner", lsp::SymbolKind::CLASS),
                    ("Record", lsp::SymbolKind::STRUCT),
                    ("Choice", lsp::SymbolKind::ENUM),
                    ("Global", lsp::SymbolKind::FUNCTION),
                ],
                "{path}: {symbols:?}"
            );
            let members = symbols[0].children.as_ref().unwrap();
            assert_eq!(
                members
                    .iter()
                    .map(|s| (s.name.as_str(), s.kind))
                    .collect::<Vec<_>>(),
                [
                    ("value", lsp::SymbolKind::FIELD),
                    ("Act", lsp::SymbolKind::METHOD),
                ]
            );
            assert!(members[1].children.is_none());
            assert_eq!(symbols[1].children.as_ref().unwrap()[0].name, "count");
            let variants = symbols[2].children.as_ref().unwrap();
            assert_eq!(
                variants
                    .iter()
                    .map(|s| (s.name.as_str(), s.kind))
                    .collect::<Vec<_>>(),
                [
                    ("First", lsp::SymbolKind::ENUM_MEMBER),
                    ("Second", lsp::SymbolKind::ENUM_MEMBER),
                ]
            );
            assert_ranges(source, &symbols);
        }
    }

    #[test]
    fn empty_invalid_and_unsupported_documents_have_empty_outlines() {
        for path in ["empty.reds", "empty.SCRIPT"] {
            assert!(outline(path, "").is_empty());
            assert!(outline(path, "class {").is_empty());
        }
        assert!(outline("unsupported.txt", "public class Hidden {}").is_empty());
    }

    #[test]
    fn selections_use_utf16_columns_and_full_declaration_ranges() {
        for (path, source) in [
            (
                "unicode.reds",
                "/* 😀é */ public class Owner { public let value: Int32; }",
            ),
            (
                "unicode.script",
                "/* 😀é */ class Owner { var value: Int32; }",
            ),
        ] {
            let symbols = outline(path, source);
            assert_eq!(symbols.len(), 1);
            assert_ranges(source, &symbols);
            assert!(symbols[0].range.start < symbols[0].selection_range.start);
            assert_eq!(
                symbols[0].range.end,
                crate::positions::position(source, source.len() as u32).unwrap()
            );
        }
    }
}
