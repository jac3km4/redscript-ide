use std::path::{Path, PathBuf};

use anyhow::{Context, bail};
use ls::RedscriptLanguageServer;
use mimalloc::MiMalloc;
use serde::Deserialize;
use server::LspServer;

mod buffers;
mod completions;
mod dialect;
mod display;
mod ls;
mod positions;
mod query;
mod server;

#[global_allocator]
static GLOBAL: MiMalloc = MiMalloc;

#[derive(Debug, Default, Deserialize)]
#[serde(default)]
struct InitializationOptions {
    game_dir: Option<PathBuf>,
    cache_file: Option<PathBuf>,
}

#[derive(Debug, PartialEq, Eq)]
enum CacheSource {
    File(PathBuf),
    GameDir(PathBuf),
    Embedded,
}

fn resolve_cache_source(init_options: Option<serde_json::Value>) -> anyhow::Result<CacheSource> {
    let opts = init_options
        .map(serde_json::from_value::<InitializationOptions>)
        .transpose()?
        .unwrap_or_default();

    let non_empty = |path: Option<PathBuf>| path.filter(|p| !p.as_os_str().is_empty());

    if let Some(cache_file) = non_empty(opts.cache_file) {
        return Ok(CacheSource::File(cache_file));
    }
    if let Some(game_dir) = non_empty(opts.game_dir) {
        return Ok(CacheSource::GameDir(game_dir));
    }
    Ok(CacheSource::Embedded)
}

fn main() -> anyhow::Result<()> {
    LspServer::spawn(
        |opts, _ctx| {
            let source = resolve_cache_source(opts.init_options)?;
            Ok((opts.workspace_dirs, source))
        },
        |(dirs, source), ctx| {
            let ls = match source {
                CacheSource::File(cache) => {
                    ctx.logger().info(format!(
                        "Loading analysis cache={} workspace_folders={dirs:?}",
                        cache.display(),
                    ));
                    RedscriptLanguageServer::new(cache, dirs).with_context(|| {
                        format!("failed to load analysis from {}", cache.display())
                    })?
                }
                CacheSource::GameDir(game_dir) => {
                    let cache = find_cache_file(game_dir)?;
                    ctx.logger().info(format!(
                        "Loading analysis game_dir={} cache={} workspace_folders={dirs:?}",
                        game_dir.display(),
                        cache.display(),
                    ));
                    RedscriptLanguageServer::new(&cache, dirs).with_context(|| {
                        format!("failed to load analysis from {}", cache.display())
                    })?
                }
                CacheSource::Embedded => {
                    ctx.logger().info(format!(
                        "Loading analysis from embedded cache workspace_folders={dirs:?}",
                    ));
                    RedscriptLanguageServer::embedded(dirs)
                        .context("failed to load analysis from embedded cache")?
                }
            };
            ls.log_missing_roots(ctx);
            ls.publish_all_diagnostics(ctx)?;
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
        assert_eq!(resolve_cache_source(None).unwrap(), CacheSource::Embedded);
        assert_eq!(
            resolve_cache_source(Some(json!({}))).unwrap(),
            CacheSource::Embedded
        );
        assert_eq!(
            resolve_cache_source(Some(json!({ "game_dir": "" }))).unwrap(),
            CacheSource::Embedded
        );
        assert_eq!(
            resolve_cache_source(Some(json!({ "cache_file": "" }))).unwrap(),
            CacheSource::Embedded
        );
        assert_eq!(
            resolve_cache_source(Some(json!({ "game_dir": "", "cache_file": "" }))).unwrap(),
            CacheSource::Embedded
        );
    }

    #[test]
    fn resolve_cache_source_prefers_cache_file_over_game_dir() {
        assert_eq!(
            resolve_cache_source(Some(json!({ "game_dir": "/game" }))).unwrap(),
            CacheSource::GameDir(PathBuf::from("/game"))
        );
        assert_eq!(
            resolve_cache_source(Some(json!({
                "game_dir": "/game",
                "cache_file": "/custom/final.redscripts"
            })))
            .unwrap(),
            CacheSource::File(PathBuf::from("/custom/final.redscripts"))
        );
        assert_eq!(
            resolve_cache_source(Some(json!({
                "game_dir": "/game",
                "cache_file": ""
            })))
            .unwrap(),
            CacheSource::GameDir(PathBuf::from("/game"))
        );
    }
}
