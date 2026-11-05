# Contributing

## Validation

Run these commands from the repository root. Use stable Rust for builds, tests,
Clippy, and documentation. Install the pinned nightly rustfmt because stable
rustfmt does not honor all repository formatting options:

```sh
rustup toolchain install nightly-2026-10-15 --profile minimal --component rustfmt
cargo +nightly-2026-10-15 fmt --package redscript-ide -- --check
cargo +stable clippy --workspace --all-targets --all-features --locked -- -D warnings
cargo +stable test --workspace --all-features --locked
cargo +stable build --release --locked
cargo +stable doc --workspace --all-features --no-deps --locked
```

To apply formatting rather than check it, run:

```sh
cargo +nightly-2026-10-15 fmt --package redscript-ide
```

Keep `--all-targets` in the Clippy command so test code is linted too.

## Working across repositories

All redscript dependencies use the same Git revision pinned in `Cargo.toml`:
`e1c44e801e46ce076a0b0bad64349313067cb5b3`. Cargo fetches that revision for local
builds and CI; no sibling checkout or `REDSCRIPT_REF` repository variable is needed.
Transitive AST/compiler packages come from the same Git workspace, and
`Cargo.lock` records the resulting graph.

When updating the compiler, change every redscript revision together, regenerate
`Cargo.lock`, and run the validation above. For local cross-repository development,
use Cargo Git-source patches for the affected packages, keeping the complete
AST/compiler graph consistent. Do not commit local path overrides or mix a Git
parser/AST with a local compiler.

Validate both repositories separately when changing the compiler and language
server. IDE tests use small synthetic bundles, not a game installation.

## REDmod integration

- Keep file discovery and parser selection in `src/dialect.rs`: `.reds` is exact,
  `.script` is ASCII-case-insensitive. Do not change the community source loader.
- Use `redscript_redmod_adapter::parse_file_with_metadata` and `process_sources`
  for all snapshots, passing community modules as dependencies. Preserve file IDs
  and complete parsed metadata when cloning.
- Keep parse, adapter, and shared compiler diagnostics. Invalid REDmod files must
  not block analysis of valid files or enter lowering through parser recovery.
- Keep completion repairs temporary and strictly reparse them. Never change the
  document or published diagnostics to make completion work.
- Build REDmod array completions from `array_operations()` and real intrinsic
  signatures, not a separate hardcoded list.
- Use the native REDmod formatter for `.script` files. Never apply community
  formatting or code edits to REDmod targets.

See the [README](README.md#redmod-support) for user-facing support and limitations.
