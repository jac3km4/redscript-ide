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

  With **Neovim 0.11 or newer**, add the following to your `init.lua`. This uses
  the built-in LSP configuration API; `nvim-lspconfig` is not required. Replace
  `/path/to/redscript-ide` with a downloaded
  [redscript-ide executable](https://github.com/jac3km4/redscript-ide/releases/latest)
  and `/path/to/cyberpunk2077` with the root of your Cyberpunk 2077 installation:

  ```lua
  vim.filetype.add({
      extension = {
          reds = 'redscript',
          script = 'redmod',
      },
  })

  vim.api.nvim_create_autocmd('FileType', {
      group = vim.api.nvim_create_augroup('RedscriptSyntax', { clear = true }),
      pattern = 'redscript',
      callback = function(args)
          vim.bo[args.buf].syntax = 'swift'
      end,
  })

  vim.lsp.config('redscript_ide', {
      cmd = { '/path/to/redscript-ide' },
      filetypes = { 'redscript', 'redmod' },
      root_markers = { '.redscript', '.git' },
      init_options = {
          game_dir = '/path/to/cyberpunk2077',
      },
  })
  vim.lsp.enable('redscript_ide')
  ```

  Both dialects use the same language server, which selects the dialect by file
  extension. Create a `.redscript` file (it can be empty) at the workspace root
  so `.reds` and `.script` files share the same project and source roots. See
  [REDmod support](#redmod-support) for supported features and limitations.

  Community REDscript (`.reds`) uses Swift syntax highlighting as an approximate
  fallback by default, while retaining the `redscript` filetype for LSP attachment.
  REDmod (`.script`) does not use this fallback. For grammar-specific Tree-sitter
  highlighting, remove the `RedscriptSyntax` autocmd above and use [tree-sitter-reds](https://github.com/jac3km4/tree-sitter-reds)
  (parser name `redscript`) for the `redscript` filetype and
  [tree-sitter-redmod](https://github.com/jac3km4/tree-sitter-redmod) (parser name
  `redmod`) for the `redmod` filetype, with each grammar's matching queries. The
  old community grammar and Swift syntax fallback do not cover REDmod correctly.

  Restart Neovim and open one file of each type. Use `:set filetype?` to verify
  detection and `:checkhealth vim.lsp` to check language-server setup.

- ### Helix

  1. Install [Helix 25.07.1 or newer](https://github.com/helix-editor/helix/releases/latest),
     Git, and a C/C++ toolchain: Visual Studio Build Tools with the C++ workload on
     Windows, or `xcode-select --install` on macOS.
  2. Run the setup command for your platform (no checkout needed). These commands
     download and execute code from GitHub; review the linked scripts first.

     **Windows ([PowerShell script](scripts/helix/setup-windows.ps1)):**
     ```powershell
     Invoke-RestMethod https://raw.githubusercontent.com/jac3km4/redscript-ide/0.2.x/scripts/helix/setup-windows.ps1 | Invoke-Expression
     ```

     **macOS ([shell script](scripts/helix/setup-macos.sh)):**
     ```sh
     curl -fsSL https://raw.githubusercontent.com/jac3km4/redscript-ide/0.2.x/scripts/helix/setup-macos.sh | sh
     ```

     The scripts build the pinned [REDscript](https://github.com/jac3km4/tree-sitter-reds)
     and [REDmod](https://github.com/jac3km4/tree-sitter-redmod) grammars and install
     matching highlighting queries. They also download the latest `redscript-ide`
     release into the Helix config directory's `bin` folder and configure its absolute
     path (no administrator privileges or `PATH` changes needed). They create your user `languages.toml` if
     missing, or back it up and append the [language configuration](scripts/helix/languages.toml),
     preserving other settings. Existing query directories are backed up too.
     If the file mentions REDscript or REDmod (even in comments), setup stops with
     instructions rather than risking duplicate configuration.

     Alternatively, run the scripts from a checkout. For a custom runtime directory,
     run a local copy with `-Runtime /path/to/runtime` on Windows or `/path/to/runtime`
     as the first argument on macOS; this does not change the user configuration path.
     To specify the game location, set the `REDSCRIPT_GAME_DIR` environment variable
     before running setup, or pass `-GameDir /path/to/game` to the Windows script
     (the second argument on macOS).
  3. Setup tries to find Cyberpunk 2077 in Steam libraries (including additional
     libraries), GOG/Epic launcher records on Windows, and common application bundle
     locations on macOS. Candidates must contain a script cache under `r6/cache`.
     If detection fails, setup warns and leaves a placeholder: edit `game_dir` in your user
     [`languages.toml`](https://docs.helix-editor.com/languages.html)
     (`%AppData%\helix\languages.toml` on Windows, `~/.config/helix/languages.toml`
     on macOS, or `$XDG_CONFIG_HOME/helix/languages.toml` if set) to point to the
     Cyberpunk 2077 installation root. Use forward slashes for Windows paths in TOML.
  4. Create a `.redscript` file (it can be empty) at your workspace root and restart
     Helix. `.reds` and `.script` use separate grammars with the shared language
     server; see [REDmod limitations](#redmod-support). Check setup with
     `hx --health redscript` and `hx --health redmod`; missing indentation and
     text-object queries are expected.
