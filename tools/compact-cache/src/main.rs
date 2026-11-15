use std::path::PathBuf;
use std::{env, fs};

use anyhow::{Context, bail};
use redscript_io::{Definition, ScriptBundle};

fn main() -> anyhow::Result<()> {
    let mut args = env::args_os().skip(1);
    let input = PathBuf::from(
        args.next()
            .context("usage: compact-cache <input> <output>")?,
    );
    let output = PathBuf::from(
        args.next()
            .context("usage: compact-cache <input> <output>")?,
    );
    if args.next().is_some() {
        bail!("usage: compact-cache <input> <output>");
    }

    let bytes = fs::read(&input).with_context(|| format!("failed to read {}", input.display()))?;
    let mut bundle = ScriptBundle::from_bytes(&bytes)
        .with_context(|| format!("failed to parse {}", input.display()))?;

    for def in bundle.definitions_mut() {
        match def {
            Definition::Function(function) => function.set_code(vec![]),
            Definition::Local(_) | Definition::SourceFile(_) => *def = Definition::Bitfield,
            _ => {}
        }
    }

    bundle.strings_mut().clear();
    bundle.resources_mut().clear();
    bundle.tdb_ids_mut().clear();

    let out_bytes = bundle
        .into_writeable()
        .to_bytes()
        .map_err(|err| anyhow::anyhow!("failed to encode {}: {err:?}", output.display()))?;
    let compressed = miniz_oxide::deflate::compress_to_vec_zlib(&out_bytes, 10);

    if let Some(parent) = output.parent().filter(|p| !p.as_os_str().is_empty()) {
        fs::create_dir_all(parent)?;
    }
    fs::write(&output, compressed)
        .with_context(|| format!("failed to write {}", output.display()))?;

    Ok(())
}
