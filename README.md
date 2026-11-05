# redscript-ide

Language server for [redscript](https://github.com/jac3km4/redscript)

For development setup and validation commands, see [Contributing](CONTRIBUTING.md).

## features

- error and warning diagnostics
- autocompletion for methods and fields
- hover for function definitions and types, including specialized generic field types
- go-to-definition for types, functions, fields, local variables, and captured locals
  - REDmod `.script` parsing and shared-frontend navigation (see limitations below)
- workspace symbols for types, functions, methods, and fields
- document symbols for outlines and breadcrumbs in both dialects
- quick fixes for supported community diagnostics
- completion snippets and Markdown documentation when supported by the client
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

## REDmod support

The server supports community REDscript (`.reds`) and REDmod (`.script`,
case-insensitive), selected by file extension. Both can share a workspace and
use the same `source_roots` configuration.

REDmod support is partial, not a replacement for the official compiler:

- Diagnostics, hover, go-to-definition, workspace symbols, and member completion
  work for the [supported language subset](https://github.com/jac3km4/redscript/blob/e1c44e801e46ce076a0b0bad64349313067cb5b3/crates/compiler/redmod/adapter/README.md).
- Array completions include operations such as `Size`, `PushBack`, and `Last`.
  Completion tolerates a missing semicolon in the current statement; other syntax
  or unsupported-feature errors can prevent semantic features from working.
- Formatting preserves source spelling and uses four-space indentation, ignoring
  community formatter settings. Invalid syntax is left unchanged.
- References, rename, keyword/local completion, and community code actions are
  not available for REDmod.

### Workspace behavior

Queries use unsaved changes from all open workspace files. Diagnostics update on
startup and save, not on every edit. Standalone files are analyzed separately.
Missing source roots produce a log warning; restore them or update `.redscript`
and refresh the workspace. Analysis can be slow in large workspaces.

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

### Debugging hover and completion in Zed

After rebuilding/installing the updated executable, restart the language server
in Zed, then run **See LSP Logs** from the command palette and select
`redscript-ide`. Inspect the server log messages (`window/logMessage`); no
extra logging configuration is required.

The logs include startup/cache/workspace details, document lifecycle events,
hover/completion request IDs, URI and zero-based UTF-16 positions, response
summaries, timings, and request errors. Query messages include the normalized
path, byte offset, document version, workspace membership, speculative member
patch, diagnostic counts, and whether an AST node, function, expression, or type
was found. Source text and hover contents are not logged, but paths are included;
redact them before sharing logs if necessary.

- No hover/completion `Request` entry: check that the file is associated with
  `REDscript` and that Zed is using the expected executable.
- No `Language server ready for requests`: inspect startup errors and the
  configured `game_dir`/cache path (also check server stderr).
- `Completion empty ... cursor is not after a member-access dot`: expected for
  non-member completion; keyword/local completion is not implemented.
- `Query result` with missing AST/function/expression or nonzero diagnostics:
  inspect file diagnostics and the REDmod limitations above. A missing named
  type alone is normal for expression queries.

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
