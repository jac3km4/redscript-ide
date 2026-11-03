# redscript-ide

Language server for [redscript](https://github.com/jac3km4/redscript)

For development setup and validation commands, see [Contributing](CONTRIBUTING.md).

## features

- error and warning diagnostics
- autocompletion for methods and fields
- hover for function definitions and types
- go-to-definition
  - REDmod `.script` parsing and shared-frontend navigation (see limitations below)
- workspace symbols
- formatting (beta)
- debugger (requires [redscript-dap](https://github.com/jac3km4/redscript-dap))
- hooks for external tools

![ide-gif](https://user-images.githubusercontent.com/11986158/135734766-b5423e2c-cf47-4836-97ba-5c771cef7cf2.gif)

## configuration

The language server will attempt to load a TOML file named `.redscript` from every workspace folder.
This file can contain the following configuration options:

- `source_roots` specifies source directories for both `.reds` and `.script` files, defaults to `["."]`
- `format` block allows you to configure the formatter, you can find the available options [here](https://github.com/jac3km4/redscript/blob/c3d0ec6f12583eccc51b5a482583e8fb6641ce8d/crates/dotfile/src/lib.rs#L36-L43)

Here's an example `.redscript` file:

```toml
source_roots = [".", "../red4ext/plugins"]

[format]
indent = 2
max_width = 80
```

## REDmod and document lifecycle

The server discovers case-sensitive `.reds` (community REDscript) and
ASCII-case-insensitive `.script` (REDmod) extensions, including `.Script` and
`.SCRIPT`. Other extensions are ignored; dialect selection
never sniffs content or tries multiple parsers. Discovery respects the existing
ignore-file rules, reports traversal errors, and canonicalizes/deduplicates files
across overlapping roots. Configured external roots participate in workspace
analysis. Formatting configuration uses the deepest matching root; equal-depth
ties use the lexicographically first workspace path. Workspace roots share one
analysis universe, including roots shared by multiple workspaces.

Open documents retain versions and overlay disk contents across **all** open
workspace buffers. Open/change/save/close and workspace-folder changes rebuild
analysis and diagnostics; stale document changes are ignored. Source/config
watched-file batches and configuration notifications reload roots and analysis,
without replacing open buffers with disk contents. Clients supporting dynamic
watched-file registration are registered for both extensions and `.redscript`;
other clients must send watched-file notifications themselves. Standalone files
are analyzed separately and cannot replace workspace state. Closing a standalone
file drops its analysis/diagnostics. Rebuilds are synchronous, not incremental or
debounced, so very large workspaces may see latency during live checking.

REDmod syntax and adapter semantic diagnostics use the generic `OTHER` code.
Workspace, standalone and query snapshots all use `redscript-redmod-adapter`, retaining
written override intent and applying its per-file policy for function-wide local
scope, dynamic-array operations and object truthiness. Community dependencies
retain community semantics. Files with REDmod parse/capability errors retain their
diagnostics but are excluded from lowering, so valid workspace files can still
be analyzed (dependents of excluded declarations may report missing symbols).
The REDmod parser now owns a separate native `redscript-redmod-ast`; only successful
checked conversion contributes a shared query module. Malformed sources retain
syntax diagnostics but no partial shared query tree. Native `for`/`do-while`
analysis retains the adapter's control-flow plan, including in speculative queries;
written conditions, steps and bodies support hover and definition lookup.
Omitted arguments and other unimplemented semantics produce explicit capability
errors even though they parse and can be indented. Queries directly over
unconverted native-only syntax are not yet implemented.
Hover, member completion, definition lookup and workspace symbols reuse shared
analysis for the supported subset. **This is still a strict, partial REDmod implementation**, not full
semantic parity or an official compiler replacement; see the sibling
`redscript/crates/compiler/redmod/adapter/README.md` for supported features and limits.
REDmod dynamic-array receivers offer the semantic adapter's 12 supported member
operations (including `Size`, `PushBack`, and `Last`). Completion signatures and
snippets use actual boot intrinsics, omit the receiver parameter, and specialize
the array element type. These suggestions are `.script`-only and do not replace
same-named user-class methods. References/rename and general keyword/local
completion are not implemented.
REDmod formatting uses the native conservative indentation formatter: four spaces
per brace level, preserving comments, literal spelling/content, `override`, and
line endings. It does not apply community spacing/wrapping settings or the LSP
tab size, and invalid syntax produces no edits. Community-syntax code actions
remain disabled for `.script`, including cross-file action targets. Resolved
actions reject stale source snapshots and carry document versions for open edit
targets.

## usage

- ### VS Code

  Use the [Redscript IDE extension from the marketplace](https://marketplace.visualstudio.com/items?itemName=jac3km4.redscript-ide-vscode).
  Associate `"*.script": "redscript"` under `files.associations` if your extension
  version does not already attach to REDmod files.

- ### Zed

  - Install the `REDscript` extension from the marketplace.
  - Associate `.script` with the extension's language in `settings.json`:
    ```json
    "file_types": { "REDscript": ["**/*.reds", "**/*.script"] }
    ```
    This attaches the server; community syntax highlighting may not cover REDmod.
  - Configure the required `lsp.redscript-ide.initialization_options.game_dir` setting in Zed's `settings.json`:
    ```json
      "lsp": {
        "redscript-ide": {
          "initialization_options": {
            "game_dir": "D:\\Games\\Cyberpunk 2077"
          }
        }
      }
    ```
  - Ensure that you downloaded the `redscript-ide` executable and it is in your PATH.
    - You can set it by searching for 'Edit the system environment variables' on Windows, clicking on 'Environment Variables', and adding the path to the `redscript-ide` executable to the `Path` variable.

- ### IntelliJ

  Use the (work in progress) [Redscript-IntelliJ Plugin](https://github.com/pawrequest/redscript-intellij).

- ### Neovim

  Add the snippet below to your `init.lua` and replace `/path/to/redscript-ide` with the path to
  a redscript-ide executable downloaded from [here](https://github.com/jac3km4/redscript-ide/releases/latest)
  and `/path/to/cyberpunk2077` with the path to the root of your Cyberpunk 2077 installation directory:

  ```lua
  local configs = require 'lspconfig.configs'
  local util = require 'lspconfig.util'
  local lsp = require 'lspconfig'

  -- define the redscript filetype
  vim.cmd [[
      augroup RedscriptFile
          autocmd!
          autocmd BufNewFile,BufRead *.reds,*.script set filetype=reds | set syntax=swift
      augroup END
  ]]

  -- configure the redscript language server
  configs.redscript_ide = {
      default_config = {
          cmd = { '/path/to/redscript-ide' },
          filetypes = 'reds',
          init_options = {
              game_dir = '/path/to/cyberpunk2077',
          },
          root_dir = function(fname)
              return util.root_pattern('.git')(fname) or util.path.dirname(fname)
          end,
          single_file_support = true
      }
  }

  -- invoke the lsp-config setup
  lsp.redscript_ide.setup {}
  ```

- ### Helix

  Add the snippet below to your [`languages.toml`](https://docs.helix-editor.com/languages.html)
  and replace the paths, similarly to the Neovim setup:

  ```toml
  [language-server.redscript-ide]
  command = "/path/to/redscript-ide"
  config = { game_dir = "/path/to/cyberpunk2077" }

  [[language]]
  name = "redscript"
  scope = "source.reds"
  file-types = ["reds", "script"]
  # you have to have a .redscript file for workspace to be detected (it can be empty)
  roots = [".redscript"]
  auto-format = true
  comment-tokens = "//"
  block-comment-tokens = { start = "/*", end = "*/" }
  indent = { tab-width = 2, unit = "  " }
  language-servers = ["redscript-ide"]

  [language.auto-pairs]
  '(' = ')'
  '{' = '}'
  '[' = ']'
  '"' = '"'

  [[grammar]]
  name = "redscript"
  source = { git = "https://github.com/jac3km4/tree-sitter-redscript", rev = "master" }
  ```

  To get the syntax highlighting to work, you also need to rebuild the grammars and populate the queries folder:

  ```powerhsell
  # build the grammars
  helix --grammar fetch
  helix --grammar build

  # populate the queries
  # on Linux run:
  # git clone -b queries --single-branch https://github.com/jac3km4/tree-sitter-redscript "$HOME/.config/helix/runtime/queries/redscript"
  # on Windows run:
  # git clone -b queries --single-branch https://github.com/jac3km4/tree-sitter-redscript "$env:USERPROFILE\AppData\Roaming\helix\runtime\queries\redscript"
  ```
