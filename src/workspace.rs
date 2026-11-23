use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use hashbrown::HashMap;
use lsp_types as lsp;
use redscript_community_formatter::FormatSettings;
use redscript_compiler_api::ast;
use redscript_dotfile::Dotfile;

use crate::dialect;

pub(crate) struct Overlay {
    pub(crate) text: Arc<str>,
    pub(crate) version: i32,
}

/// Loaded configuration for one LSP workspace folder: its `.redscript` source
/// roots and formatter settings.
#[derive(Debug, Clone)]
pub(crate) struct WorkspaceConfig {
    /// The `source_roots` declared in `.redscript`, resolved to absolute paths.
    pub(crate) roots: Vec<PathBuf>,
    pub(crate) format_settings: FormatSettings,
    /// Explicit `format.indent` from `.redscript`, if configured.
    pub(crate) format_indent: Option<u16>,
}

#[derive(Clone)]
pub(crate) struct LoadedWorkspace {
    pub(crate) workspace: WorkspaceConfig,
    pub(crate) dotfile_path: PathBuf,
    pub(crate) diagnostic: Option<lsp::Diagnostic>,
    pub(crate) warnings: Vec<String>,
}

impl WorkspaceConfig {
    pub(crate) fn from_dotfile(dir: &Path, dotfile: &Dotfile) -> Self {
        let roots = dotfile
            .expanded_source_roots()
            .into_iter()
            .map(|p| dialect::identity(&dir.join(p)))
            .collect();

        let mut settings = FormatSettings::default();
        if let Some(indent) = dotfile.format.indent {
            settings.indent = indent;
        }
        if let Some(max_width) = dotfile.format.max_width {
            settings.max_width = max_width;
        }
        if let Some(max_chain_calls) = dotfile.format.max_chain_calls {
            settings.max_chain_calls = max_chain_calls;
        }
        if let Some(max_chain_fields) = dotfile.format.max_chain_fields {
            settings.max_chain_fields = max_chain_fields;
        }
        if let Some(max_chain_operators) = dotfile.format.max_chain_operators {
            settings.max_chain_operators = max_chain_operators;
        }
        if let Some(max_chain_total) = dotfile.format.max_chain_total {
            settings.max_chain_total = max_chain_total;
        }

        Self {
            roots,
            format_settings: settings,
            format_indent: dotfile.format.indent,
        }
    }

    pub(crate) fn load(dir: &Path, fallback: Option<&Self>) -> LoadedWorkspace {
        let raw_dotfile = dir.join(".redscript");
        let dotfile_path = dialect::identity(&raw_dotfile);
        let fallback_workspace = || {
            fallback
                .cloned()
                .unwrap_or_else(|| Self::from_dotfile(dir, &Dotfile::default()))
        };

        match fs::read_to_string(&raw_dotfile) {
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => LoadedWorkspace {
                workspace: Self::from_dotfile(dir, &Dotfile::default()),
                dotfile_path,
                diagnostic: None,
                warnings: vec![],
            },
            Err(err) => {
                let message = format!("failed to read .redscript: {err}");
                let warning = format!(
                    "Failed to read '{}': {err}; keeping previous or default workspace configuration.",
                    dotfile_path.display()
                );
                LoadedWorkspace {
                    workspace: fallback_workspace(),
                    dotfile_path,
                    diagnostic: Some(config_diagnostic(lsp::Range::default(), message)),
                    warnings: vec![warning],
                }
            }
            Ok(content) => match toml::from_str::<Dotfile>(&content) {
                Ok(dotfile) => {
                    let warnings =
                        inspect_source_root_templates(&dotfile.source_roots, &dotfile_path);
                    LoadedWorkspace {
                        workspace: Self::from_dotfile(dir, &dotfile),
                        dotfile_path,
                        diagnostic: None,
                        warnings,
                    }
                }
                Err(err) => {
                    let range = err
                        .span()
                        .and_then(|span| {
                            let start = crate::positions::position(
                                &content,
                                u32::try_from(span.start).ok()?,
                            )?;
                            let end = crate::positions::position(
                                &content,
                                u32::try_from(span.end).ok()?,
                            )?;
                            Some(lsp::Range::new(start, end))
                        })
                        .unwrap_or_default();
                    let warning = format!(
                        "Invalid configuration in '{}': {}; keeping previous or default workspace configuration.",
                        dotfile_path.display(),
                        err.message()
                    );
                    LoadedWorkspace {
                        workspace: fallback_workspace(),
                        dotfile_path,
                        diagnostic: Some(config_diagnostic(range, err.message().to_owned())),
                        warnings: vec![warning],
                    }
                }
            },
        }
    }
}

pub(crate) fn collect_workspace_sources(
    roots: impl IntoIterator<Item = PathBuf>,
    overlays: &HashMap<PathBuf, Overlay>,
    is_workspace_file: impl Fn(&Path) -> bool,
    missing_roots: &mut BTreeSet<PathBuf>,
    read_diagnostics: &mut BTreeMap<PathBuf, lsp::Diagnostic>,
    io_warnings: &mut BTreeSet<String>,
) -> ast::SourceMap {
    let sources = ast::SourceMap::new();
    let mut files: BTreeMap<PathBuf, Arc<str>> = BTreeMap::new();
    let discovered = dialect::discover(roots, |issue| match issue {
        dialect::DiscoveryIssue::MissingRoot(root) => {
            missing_roots.insert(root);
        }
        dialect::DiscoveryIssue::InvalidRoot { root, error } => {
            io_warnings.insert(format!(
                "Skipping invalid source root '{}': {error}.",
                root.display()
            ));
        }
        dialect::DiscoveryIssue::WalkError { root, error } => {
            io_warnings.insert(format!(
                "Encountered filesystem error while scanning source root '{}': {error}.",
                root.display()
            ));
        }
    });
    for path in discovered {
        if let Some(overlay) = overlays.get(&path) {
            files.insert(path, overlay.text.clone());
        } else {
            match fs::read_to_string(&path) {
                Ok(text) => {
                    files.insert(path, Arc::from(text));
                }
                Err(error) if error.kind() == std::io::ErrorKind::NotFound => {}
                Err(error) => {
                    record_source_read_error(&path, &error, read_diagnostics, io_warnings);
                }
            }
        }
    }
    // Unsaved new files are absent from discovery but still own an active module.
    for (path, overlay) in overlays {
        if is_workspace_file(path) {
            read_diagnostics.remove(path);
            files.insert(path.clone(), overlay.text.clone());
        }
    }
    for (path, text) in files {
        sources.push_back(path, text.to_string());
    }
    sources
}

pub(crate) fn record_source_read_error(
    path: &Path,
    error: &std::io::Error,
    read_diagnostics: &mut BTreeMap<PathBuf, lsp::Diagnostic>,
    io_warnings: &mut BTreeSet<String>,
) {
    let (code, message, warning) = if error.kind() == std::io::ErrorKind::InvalidData {
        (
            "INVALID_UTF8",
            "file is not valid UTF-8; re-save it with UTF-8 encoding to enable analysis".to_owned(),
            format!(
                "Skipping '{}': file is not valid UTF-8 ({error}). Re-save it with UTF-8 encoding to enable analysis.",
                path.display()
            ),
        )
    } else {
        (
            "IO_ERROR",
            format!("failed to read source file: {error}"),
            format!(
                "Skipping '{}': failed to read source file ({error}).",
                path.display()
            ),
        )
    };
    io_warnings.insert(warning);
    read_diagnostics.insert(
        path.to_owned(),
        lsp::Diagnostic {
            range: lsp::Range::default(),
            severity: Some(lsp::DiagnosticSeverity::ERROR),
            code: Some(lsp::NumberOrString::String(code.into())),
            source: Some("redscript".into()),
            message,
            ..Default::default()
        },
    );
}

/// A fast, non-cryptographic 64-bit hash of source text.
///
/// Cached quick fixes compare against this instead of retaining a full copy of
/// the file contents, keeping both memory and `codeAction` payloads small.
pub(crate) fn content_hash(bytes: &[u8]) -> u64 {
    // FNV-1a, 64-bit.
    const OFFSET_BASIS: u64 = 0xcbf2_9ce4_8422_2325;
    const PRIME: u64 = 0x0000_0100_0000_01b3;
    bytes.iter().fold(OFFSET_BASIS, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(PRIME)
    })
}

pub(crate) fn config_diagnostic(range: lsp::Range, message: String) -> lsp::Diagnostic {
    lsp::Diagnostic {
        range,
        severity: Some(lsp::DiagnosticSeverity::ERROR),
        code: Some(lsp::NumberOrString::String("INVALID_CONFIG".into())),
        source: Some("redscript".into()),
        message,
        ..Default::default()
    }
}

pub(crate) fn inspect_source_root_templates(roots: &[String], dotfile_path: &Path) -> Vec<String> {
    let mut warnings = Vec::new();
    for template in roots {
        let mut chars = template.char_indices().peekable();
        while let Some((_, ch)) = chars.next() {
            if ch == '\\' {
                chars.next();
                continue;
            }
            if ch == '{' {
                let mut key = String::new();
                let mut closed = false;
                while let Some((_, inner)) = chars.next() {
                    if inner == '\\' {
                        if let Some((_, escaped)) = chars.next() {
                            key.push(escaped);
                        }
                        continue;
                    }
                    if inner == '}' {
                        closed = true;
                        break;
                    }
                    key.push(inner);
                }
                let trimmed = key.trim();
                if !closed || trimmed.is_empty() {
                    warnings.push(format!(
                        "Malformed template '{template}' in '{}' source_roots; treating it literally.",
                        dotfile_path.display()
                    ));
                    break;
                } else if std::env::var_os(trimmed).is_none() {
                    warnings.push(format!(
                        "Environment variable '{trimmed}' referenced in '{}' source_roots is not set; expanding it to an empty string.",
                        dotfile_path.display()
                    ));
                }
            }
        }
    }
    warnings
}

#[derive(Debug)]
pub(crate) enum FileResolution<'a> {
    Workspace(&'a WorkspaceConfig),
    NonWorkspace,
}

impl FileResolution<'_> {
    pub fn as_workspace(&self) -> Option<&WorkspaceConfig> {
        match self {
            Self::Workspace(ws) => Some(ws),
            Self::NonWorkspace => None,
        }
    }
}
