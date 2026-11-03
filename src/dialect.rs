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
    // Resolve the existing prefix even when several parent directories are missing.
    // This does not preserve identity across symlink creation or retargeting.
    let (base, suffix) = path
        .ancestors()
        .find_map(|ancestor| {
            let existing = if ancestor.as_os_str().is_empty() {
                Path::new(".")
            } else {
                ancestor
            };
            fs::canonicalize(existing)
                .ok()
                .map(|base| (base, path.strip_prefix(ancestor).unwrap()))
        })
        .unwrap_or_else(|| (PathBuf::new(), path));
    suffix.components().fold(base, |mut result, component| {
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

pub fn discover(
    roots: impl IntoIterator<Item = impl Into<PathBuf>>,
    mut missing_root: impl FnMut(&Path),
) -> io::Result<Vec<PathBuf>> {
    let mut paths = BTreeSet::new();
    for root in roots {
        let root = root.into();
        match fs::metadata(&root) {
            Ok(_) => {}
            Err(error) if error.kind() == io::ErrorKind::NotFound => {
                // Windows also reports NotFound for children of a regular file.
                // Only absent directories should be treated as temporarily empty.
                for ancestor in root.ancestors().skip(1) {
                    if ancestor.as_os_str().is_empty() {
                        break;
                    }
                    match fs::metadata(ancestor) {
                        Ok(metadata) if metadata.is_dir() => break,
                        Ok(_) => {
                            return Err(io::Error::new(
                                io::ErrorKind::NotADirectory,
                                format!(
                                    "source root ancestor '{}' is not a directory",
                                    ancestor.display()
                                ),
                            ));
                        }
                        Err(error) if error.kind() == io::ErrorKind::NotFound => {}
                        Err(error) => return Err(error),
                    }
                }
                missing_root(&root);
                continue;
            }
            Err(error) => return Err(error),
        }
        for entry in ignore::WalkBuilder::new(root).follow_links(true).build() {
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
        let paths = discover([temp.path(), temp.path()], |_| {
            panic!("unexpected missing root")
        })
        .unwrap();
        assert_eq!(paths.len(), 4);
        for name in ["a.reds", "b.script", "mixed.Script", "upper.SCRIPT"] {
            assert!(paths.contains(&identity(&temp.path().join(name))));
        }
        // A non-directory ancestor is an error, not a temporarily absent root.
        assert!(
            discover([temp.path().join("a.reds/child")], |_| {
                panic!("unexpected missing root")
            })
            .is_err()
        );
    }

    #[test]
    fn nested_missing_identity_survives_save_and_deletion() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("nested/deeper/file.script");
        let expected = fs::canonicalize(temp.path())
            .unwrap()
            .join("nested/deeper/file.script");
        assert_eq!(identity(&path), expected);
        assert_eq!(
            identity(&temp.path().join("nested/unused/../deeper/./file.script")),
            expected
        );
        fs::create_dir_all(path.parent().unwrap()).unwrap();
        fs::write(&path, "").unwrap();
        assert_eq!(identity(&path), expected);
        fs::remove_dir_all(temp.path().join("nested")).unwrap();
        assert_eq!(identity(&path), expected);
    }

    #[test]
    fn discovery_reports_initially_missing_roots_and_keeps_healthy_sources() {
        let temp = tempfile::tempdir().unwrap();
        let healthy = temp.path().join("healthy.script");
        fs::write(&healthy, "").unwrap();
        let missing = temp.path().join("missing/nested");
        let mut warnings = Vec::new();
        let paths = discover([missing.clone(), temp.path().to_owned()], |root| {
            warnings.push(root.to_owned());
        })
        .unwrap();
        assert_eq!(warnings, [missing]);
        assert_eq!(paths, [identity(&healthy)]);
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
