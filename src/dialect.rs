//! IDE-only discovery and parser dispatch. Community tools keep their `.reds` loader.
use std::collections::{BTreeSet, HashSet};
use std::path::{Path, PathBuf};
use std::{fs, io};

use redscript_compiler_api::{
    CompileErrorReporter, Diagnostic, DiagnosticLevel, LoweredCompilationUnit, Symbols,
    TypeInterner, ast, parse_file,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Dialect {
    Community,
    Redmod,
}

impl Dialect {
    /// REDmod extensions are ASCII-case-insensitive; community extensions stay exact.
    pub fn of(path: &Path) -> Option<Self> {
        match path.extension()?.to_str()? {
            "reds" => Some(Self::Community),
            extension if extension.eq_ignore_ascii_case("script") => Some(Self::Redmod),
            _ => None,
        }
    }
}

pub fn identity(path: &Path) -> PathBuf {
    // Keep deleted/new files comparable with existing files, including on Windows.
    if let Ok(path) = fs::canonicalize(path) {
        return path;
    }
    if let (Some(parent), Some(name)) = (path.parent(), path.file_name())
        && let Ok(parent) = fs::canonicalize(parent)
    {
        return parent.join(name);
    }
    path.components()
        .fold(PathBuf::new(), |mut result, component| {
            match component {
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {
                    result.pop();
                }
                component => result.push(component.as_os_str()),
            }
            result
        })
}

pub fn discover(roots: impl IntoIterator<Item = impl Into<PathBuf>>) -> io::Result<Vec<PathBuf>> {
    let mut paths = BTreeSet::new();
    for root in roots {
        for entry in ignore::WalkBuilder::new(root.into())
            .follow_links(true)
            .build()
        {
            let entry = entry.map_err(io::Error::other)?;
            if entry.file_type().is_some_and(|kind| kind.is_file())
                && Dialect::of(entry.path()).is_some()
            {
                paths.insert(identity(entry.path()));
            }
        }
    }
    Ok(paths.into_iter().collect())
}

pub enum ParsedModule<'src> {
    Community(ast::SourceModule<'src>),
    Redmod(redscript_redmod_adapter::ParsedModule<'src>),
}

impl<'src> ParsedModule<'src> {
    pub fn module(&self) -> &ast::SourceModule<'src> {
        match self {
            Self::Community(module) => module,
            Self::Redmod(parsed) => &parsed.module,
        }
    }
}

pub struct ParsedSources<'src> {
    // Ownership must not be inferred from module.span(): empty modules have no span.
    pub modules: Vec<(ast::FileId, ParsedModule<'src>)>,
    pub diagnostics: Vec<Diagnostic<'src>>,
}

impl<'src> ParsedSources<'src> {
    pub fn diagnostic_files(&self) -> HashSet<ast::FileId> {
        self.diagnostics
            .iter()
            .map(|diag| diag.span().file)
            .collect()
    }

    pub fn process(
        &self,
        symbols: Symbols<'src>,
        reporter: &mut CompileErrorReporter<'src>,
        interner: &'src TypeInterner,
    ) -> (LoweredCompilationUnit<'src>, Symbols<'src>) {
        let invalid_files = self.diagnostic_files();
        let mut redmods = Vec::new();
        let mut dependencies = Vec::new();
        for (id, module) in &self.modules {
            match module {
                ParsedModule::Community(module) => dependencies.push(module.clone()),
                ParsedModule::Redmod(parsed) => {
                    // Converted modules remain queryable after validation errors,
                    // but only valid inputs participate in lowering.
                    if !invalid_files.contains(id) {
                        redmods.push(parsed.clone());
                    }
                }
            }
        }
        let (unit, symbols, errors) = redscript_redmod_adapter::process_sources(
            redmods,
            dependencies,
            symbols,
            reporter,
            interner,
        );
        reporter.report_many(errors.into_iter().map(redmod_diagnostic));
        (unit, symbols)
    }
}

fn redmod_diagnostic<'src>(error: redscript_redmod_adapter::Error) -> Diagnostic<'src> {
    let span = error.span();
    Diagnostic::Other(Box::new(error), DiagnosticLevel::Error, span)
}

pub fn parse(sources: &ast::SourceMap) -> ParsedSources<'_> {
    let mut reporter = CompileErrorReporter::default();
    let modules = sources
        .files()
        .filter_map(|(id, file)| {
            let module = match Dialect::of(file.path()) {
                Some(Dialect::Redmod) => {
                    let (parsed, errors) =
                        redscript_redmod_adapter::parse_file_with_metadata(file.source(), id);
                    reporter.report_many(errors.into_iter().map(redmod_diagnostic));
                    parsed.map(ParsedModule::Redmod)
                }
                // Includes the compiler's synthetic boot library.
                _ => parse_file(id, file, &mut reporter).map(ParsedModule::Community),
            };
            module.map(|module| (id, module))
        })
        .collect();
    ParsedSources {
        modules,
        diagnostics: reporter.into_reported(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn explicit_extensions_only() {
        assert_eq!(Dialect::of(Path::new("a.reds")), Some(Dialect::Community));
        assert_eq!(Dialect::of(Path::new("a.script")), Some(Dialect::Redmod));
        assert_eq!(Dialect::of(Path::new("a.Script")), Some(Dialect::Redmod));
        assert_eq!(Dialect::of(Path::new("a.SCRIPT")), Some(Dialect::Redmod));
        assert_eq!(Dialect::of(Path::new("a.sCrIpT")), Some(Dialect::Redmod));
        assert_eq!(Dialect::of(Path::new("a.Reds")), None);
        assert_eq!(Dialect::of(Path::new("a.REDS")), None);
        assert_eq!(Dialect::of(Path::new("a.txt")), None);
    }

    #[test]
    fn discovery_deduplicates_roots_respects_ignores_and_surfaces_errors() {
        let temp = tempfile::tempdir().unwrap();
        fs::write(temp.path().join("a.reds"), "").unwrap();
        fs::write(temp.path().join("b.script"), "").unwrap();
        fs::write(temp.path().join("ignored.script"), "").unwrap();
        fs::write(temp.path().join("mixed.Script"), "").unwrap();
        fs::write(temp.path().join("upper.SCRIPT"), "").unwrap();
        fs::write(temp.path().join("wrong.REDS"), "").unwrap();
        fs::write(temp.path().join(".ignore"), "ignored.script\n").unwrap();
        let paths = discover([temp.path(), temp.path()]).unwrap();
        assert_eq!(paths.len(), 4);
        for name in ["a.reds", "b.script", "mixed.Script", "upper.SCRIPT"] {
            assert!(paths.contains(&identity(&temp.path().join(name))));
        }
        assert!(discover([temp.path().join("missing")]).is_err());
    }

    #[test]
    fn mixed_modules_and_empty_ownership() {
        let sources = ast::SourceMap::new();
        sources.push_back("a.reds", "public class Community {}");
        let redmod = sources.push_back("b.Script", "class Redmod { var value : Int32; }");
        let empty = sources.push_back("empty.SCRIPT", "");
        let broken = sources.push_back("broken.sCrIpT", "class {");
        let parsed = parse(&sources);
        assert!(parsed.modules.iter().any(|(id, _)| *id == empty));
        assert_eq!(parsed.modules.len(), 3);
        for file in [redmod, empty] {
            assert!(
                parsed.modules.iter().any(|(id, module)| {
                    *id == file && matches!(module, ParsedModule::Redmod(_))
                })
            );
        }
        assert!(!parsed.modules.iter().any(|(id, _)| *id == broken));
        assert!(parsed.diagnostic_files().contains(&broken));
        assert!(!parsed.diagnostics.is_empty());
        assert!(parsed.diagnostics.iter().all(|diag| diag.code() == "OTHER"));
    }
}
