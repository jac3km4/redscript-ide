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

## REDmod integration boundary

`src/dialect.rs` owns extension classification (`.reds` exact, `.script`
ASCII-case-insensitive), ignored-file traversal, canonical path deduplication and
parser dispatch. Do not broaden the
shared community `SourceMap` loader. `CompilationCache` stores explicit file/module
pairs (including empty modules) and parse diagnostics. Query caches own temporary
source storage; evaluator and inference receive the same module list.

`ParsedSources` retains community ASTs and REDmod `ParsedModule` metadata behind
explicit file IDs. `redscript_redmod_adapter::parse_file_with_metadata` supplies syntax and
capability diagnostics. Its `process_sources` adapter is the only analysis entry
point for both authoritative and speculative snapshots, with community modules
(including the boot library) passed as dependencies. Clone the complete parsed
value so written override spans and private control-flow plans survive snapshots.
Syntax or bridge-capability failures retain diagnostics but supply no shared query
tree. Successfully converted modules with validation diagnostics remain available
for lookup but are excluded from lowering to avoid blocking the entire batch.
Dependencies of failed files may still report missing symbols.

Retain both parse-time errors and adapter processing errors, alongside shared
resolution/type diagnostics. REDmod errors retain their byte spans through
`Diagnostic::Other` (`OTHER` code). This enables the adapter's partial semantic
subset, not full REDmod parity; consult its README before expanding claims.
The community formatter and code-edit generator must never edit `.script` targets.
REDmod formatting calls `redscript_redmod_formatter::format_source` directly and emits
a full-document UTF-16 edit only when valid source changes. Its fixed four-space
indentation intentionally ignores community formatter options and LSP tab size.

Array member completions read `redscript_redmod_adapter::array_operations()` rather than
maintaining a second operation-name mapping. Gate them by the requesting file's
REDmod dialect and the resolved dynamic-array receiver type. Build signatures
from the actual intrinsic symbols, substituting the element type and omitting the
receiver parameter; missing intrinsic symbols must not produce invented entries.
Normal user-class member completion remains independent of this mapping.
