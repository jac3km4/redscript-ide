use std::path::{Path, PathBuf};

use anyhow::{Context, bail};
use ls::RedscriptLanguageServer;
use lsp_types as lsp;
use mimalloc::MiMalloc;
use serde_json::Value;
use server::{LspContext, LspServer};

mod analysis;
mod buffers;
mod code_actions;
mod completions;
mod diagnostics;
mod dialect;
mod display;
mod ls;
mod positions;
mod query;
mod server;
mod workspace;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

const PLACEHOLDER_GAME_DIR: &str = "/path/to/cyberpunk2077";

#[derive(Debug, PartialEq, Eq)]
enum CacheSource {
    File(PathBuf),
    GameDir(PathBuf),
    Embedded,
}

fn resolve_cache_source(init_options: Option<Value>) -> (CacheSource, Vec<String>) {
    let Some(init_options) = init_options.filter(|value| !value.is_null()) else {
        return (CacheSource::Embedded, vec![]);
    };
    let Value::Object(map) = init_options else {
        return (
            CacheSource::Embedded,
            vec![
                "Ignoring invalid initializationOptions: expected a JSON object with optional 'game_dir' or 'cache_file' paths; using bundled base-game API cache.".into(),
            ],
        );
    };

    let mut warnings = Vec::new();
    for key in map.keys() {
        match key.as_str() {
            "game_dir" | "cache_file" => {}
            "gameDir" => warnings.push(
                "Unknown initialization option 'gameDir'; use 'game_dir' instead.".into(),
            ),
            "cacheFile" => warnings.push(
                "Unknown initialization option 'cacheFile'; use 'cache_file' instead.".into(),
            ),
            other => warnings.push(format!(
                "Unknown initialization option '{other}'; supported options are 'game_dir' and 'cache_file'."
            )),
        }
    }

    let extract_path = |key: &str, warnings: &mut Vec<String>| -> Option<PathBuf> {
        match map.get(key)? {
            Value::Null => None,
            Value::String(text) if text.is_empty() => None,
            Value::String(text) => Some(PathBuf::from(text)),
            _ => {
                warnings.push(format!(
                    "Ignoring invalid '{key}' in initializationOptions: expected a path string."
                ));
                None
            }
        }
    };

    let cache_file = extract_path("cache_file", &mut warnings);
    let game_dir = extract_path("game_dir", &mut warnings);

    if let Some(cache_file) = cache_file {
        return (CacheSource::File(cache_file), warnings);
    }
    if let Some(game_dir) = game_dir {
        if game_dir == Path::new(PLACEHOLDER_GAME_DIR) {
            warnings.push(format!(
                "Configured 'game_dir' is still set to placeholder '{PLACEHOLDER_GAME_DIR}'; using bundled base-game API cache instead. Update 'game_dir' to your Cyberpunk 2077 installation root or leave it empty."
            ));
            return (CacheSource::Embedded, warnings);
        }
        return (CacheSource::GameDir(game_dir), warnings);
    }
    (CacheSource::Embedded, warnings)
}

fn load_language_server(
    dirs: &[PathBuf],
    source: &CacheSource,
    ctx: &LspContext,
) -> anyhow::Result<RedscriptLanguageServer> {
    match source {
        CacheSource::File(cache) => {
            ctx.logger().info(format!(
                "Loading analysis cache={} workspace_folders={dirs:?}",
                cache.display(),
            ));
            match RedscriptLanguageServer::new(cache, dirs) {
                Ok(ls) => Ok(ls),
                Err(err) => fallback_to_embedded(
                    dirs,
                    ctx,
                    format!(
                        "Failed to load configured cache_file '{}': {err:#}. Falling back to bundled base-game API cache.",
                        cache.display()
                    ),
                    err,
                ),
            }
        }
        CacheSource::GameDir(game_dir) => match find_cache_file(game_dir) {
            Ok(cache) => {
                ctx.logger().info(format!(
                    "Loading analysis game_dir={} cache={} workspace_folders={dirs:?}",
                    game_dir.display(),
                    cache.display(),
                ));
                match RedscriptLanguageServer::new(&cache, dirs) {
                    Ok(ls) => Ok(ls),
                    Err(err) => fallback_to_embedded(
                        dirs,
                        ctx,
                        format!(
                            "Failed to load script cache '{}' from game_dir '{}': {err:#}. Falling back to bundled base-game API cache.",
                            cache.display(),
                            game_dir.display()
                        ),
                        err,
                    ),
                }
            }
            Err(err) => fallback_to_embedded(
                dirs,
                ctx,
                format!("{err}. Falling back to bundled base-game API cache."),
                err,
            ),
        },
        CacheSource::Embedded => {
            ctx.logger().info(format!(
                "Loading analysis from embedded cache workspace_folders={dirs:?}",
            ));
            RedscriptLanguageServer::embedded(dirs)
                .context("failed to load analysis from embedded cache")
        }
    }
}

fn fallback_to_embedded(
    dirs: &[PathBuf],
    ctx: &LspContext,
    warning: String,
    original_error: anyhow::Error,
) -> anyhow::Result<RedscriptLanguageServer> {
    let embedded = RedscriptLanguageServer::embedded(dirs).map_err(|_| original_error)?;
    let message = format!(
        "{warning} The bundled cache may be out of date; set a valid 'game_dir' or 'cache_file' in initializationOptions to load your game cache."
    );
    ctx.logger().warn(&message);
    ctx.show_message(lsp::MessageType::WARNING, &message);
    Ok(embedded)
}

fn main() -> anyhow::Result<()> {
    LspServer::spawn(
        |opts, _ctx| {
            let (source, warnings) = resolve_cache_source(opts.init_options);
            Ok((opts.workspace_dirs, source, warnings))
        },
        |(dirs, source, warnings), ctx| {
            let has_warnings = !warnings.is_empty();
            for warning in warnings {
                ctx.logger().warn(warning);
                ctx.show_message(lsp::MessageType::WARNING, warning);
            }
            let mut ls = load_language_server(dirs, source, ctx)?;
            if matches!(source, CacheSource::Embedded) && !has_warnings {
                ctx.show_message(
                    lsp::MessageType::INFO,
                    "No 'game_dir' or 'cache_file' configured; using bundled base-game API cache (may be out of date). Set 'game_dir' or 'cache_file' in initializationOptions to load your game cache.",
                );
            }
            ls.log_missing_roots(ctx);
            ctx.logger().info("Language server ready for requests");
            Ok(Box::new(ls))
        },
    )
}

fn find_cache_file(game_dir: &Path) -> anyhow::Result<PathBuf> {
    let default = game_dir
        .join("r6")
        .join("cache")
        .join("modded")
        .join("final.redscripts.bk");
    if default.exists() {
        return Ok(default);
    }

    let fallback = game_dir
        .join("r6")
        .join("cache")
        .join("final.redscripts.bk");
    if fallback.exists() {
        return Ok(fallback);
    }

    let fallback = game_dir.join("r6").join("cache").join("final.redscripts");
    if fallback.exists() {
        return Ok(fallback);
    }

    bail!(
        "cache file not found under {}; expected r6/cache/modded/final.redscripts.bk, r6/cache/final.redscripts.bk, or r6/cache/final.redscripts",
        game_dir.display()
    )
}

#[cfg(test)]
mod tests {
    use serde_json::json;

    use super::*;

    #[test]
    fn resolve_cache_source_falls_back_to_embedded_when_missing_or_empty() {
        for input in [
            None,
            Some(Value::Null),
            Some(json!({})),
            Some(json!({ "game_dir": "" })),
            Some(json!({ "cache_file": "" })),
            Some(json!({ "game_dir": "", "cache_file": "" })),
        ] {
            let (source, warnings) = resolve_cache_source(input);
            assert_eq!(source, CacheSource::Embedded);
            assert!(warnings.is_empty(), "{warnings:?}");
        }
    }

    #[test]
    fn resolve_cache_source_prefers_cache_file_over_game_dir() {
        assert_eq!(
            resolve_cache_source(Some(json!({ "game_dir": "/game" }))),
            (CacheSource::GameDir(PathBuf::from("/game")), vec![])
        );
        assert_eq!(
            resolve_cache_source(Some(json!({
                "game_dir": "/game",
                "cache_file": "/custom/final.redscripts"
            }))),
            (
                CacheSource::File(PathBuf::from("/custom/final.redscripts")),
                vec![]
            )
        );
        assert_eq!(
            resolve_cache_source(Some(json!({
                "game_dir": "/game",
                "cache_file": ""
            }))),
            (CacheSource::GameDir(PathBuf::from("/game")), vec![])
        );
    }

    #[test]
    fn resolve_cache_source_warns_on_typos_invalid_types_and_placeholders() {
        let (source, warnings) = resolve_cache_source(Some(json!(["bad"])));
        assert_eq!(source, CacheSource::Embedded);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("expected a JSON object"));

        let (source, warnings) = resolve_cache_source(Some(json!({
            "gameDir": "/game",
            "cacheFile": "/cache",
            "extra": true,
            "game_dir": 123
        })));
        assert_eq!(source, CacheSource::Embedded);
        assert!(warnings.iter().any(|w| w.contains("use 'game_dir'")));
        assert!(warnings.iter().any(|w| w.contains("use 'cache_file'")));
        assert!(warnings.iter().any(|w| w.contains("'extra'")));
        assert!(
            warnings
                .iter()
                .any(|w| w.contains("Ignoring invalid 'game_dir'"))
        );

        let (source, warnings) =
            resolve_cache_source(Some(json!({ "game_dir": "/path/to/cyberpunk2077" })));
        assert_eq!(source, CacheSource::Embedded);
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("placeholder"));
    }

    #[test]
    fn load_language_server_falls_back_to_embedded_when_configured_cache_fails() {
        let dir = tempfile::tempdir().unwrap();
        let corrupt = dir.path().join("corrupt.redscripts");
        std::fs::write(&corrupt, b"not a valid bundle").unwrap();
        for source in [
            CacheSource::GameDir(dir.path().join("missing-game")),
            CacheSource::File(corrupt),
        ] {
            let (sender, receiver) = crossbeam_channel::unbounded();
            let ctx = LspContext::new(sender);
            let _ls = load_language_server(&[dir.path().to_path_buf()], &source, &ctx).unwrap();
            let messages: Vec<_> = receiver.try_iter().collect();
            assert!(messages.iter().any(|msg| {
                matches!(msg, lsp_server::Message::Notification(notif)
                    if notif.method == "window/showMessage"
                        && serde_json::from_value::<lsp::ShowMessageParams>(notif.params.clone())
                            .is_ok_and(|p| p.typ == lsp::MessageType::WARNING
                                && p.message.contains("Falling back to bundled base-game API cache")
                                && p.message.contains("out of date")
                                && p.message.contains("initializationOptions")))
            }));
        }
    }
}
