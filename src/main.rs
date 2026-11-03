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

#[derive(Debug, Deserialize)]
struct InitializationOptions {
    game_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    LspServer::spawn(
        |opts, _ctx| {
            let game_dir = if let Some(opts) = opts
                .init_options
                .map(serde_json::from_value::<InitializationOptions>)
                .transpose()?
            {
                opts.game_dir
            } else {
                bail!("game directory initialization options were not provided");
            };
            Ok((opts.workspace_dirs, game_dir))
        },
        |(dirs, game_dir), ctx| {
            let cache = find_cache_file(game_dir)?;
            ctx.logger().info(format!(
                "Loading analysis game_dir={} cache={} workspace_folders={dirs:?}",
                game_dir.display(),
                cache.display(),
            ));
            let ls = RedscriptLanguageServer::new(&cache, dirs)
                .with_context(|| format!("failed to load analysis from {}", cache.display()))?;
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
