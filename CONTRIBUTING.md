# Contributing

## Validation

Run these commands from the repository root. Use stable Rust for builds, tests,
Clippy, and documentation. Install the pinned nightly rustfmt because stable
rustfmt does not honor all repository formatting options:

```sh
rustup toolchain install nightly-2026-10-15 --profile minimal --component rustfmt
cargo +nightly-2026-10-15 fmt --all -- --check
cargo +stable clippy --workspace --all-targets --all-features --locked -- -D warnings
cargo +stable test --workspace --all-features --locked
cargo +stable build --release --locked
cargo +stable doc --workspace --all-features --no-deps --locked
```

To apply formatting rather than check it, run:

```sh
cargo +nightly-2026-10-15 fmt --all
```

Keep `--all-targets` in the Clippy command so test code is linted too.

## Working across repositories

The `redscript` dependencies use the Git revision pinned in `Cargo.toml`, not a
neighboring checkout. Local `redscript` changes are not exercised by IDE tests
unless those dependencies are explicitly overridden. Validate both repositories
separately when working across the compiler and language server.
