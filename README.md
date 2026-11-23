# redscript-ide

Language server for [redscript](https://github.com/jac3km4/redscript).

For development setup and validation commands, see [Contributing](CONTRIBUTING.md).

## Features

- error and warning diagnostics
- member autocompletion for methods, fields, and enum members, including REDmod array operations
- hover for functions and types, including specialized generic field types
- go-to-definition for types, functions, fields, local variables, and captured locals
- workspace symbols for types, functions, methods, and fields
- document symbols for outlines and breadcrumbs in both dialects
- quick fixes for supported community diagnostics
- completion snippets and Markdown documentation when supported by the client
- formatting for community (`.reds`) and REDmod (`.script`) files

![ide-gif](https://user-images.githubusercontent.com/11986158/135734766-b5423e2c-cf47-4836-97ba-5c771cef7cf2.gif)

## Configuration

### Server options

The server reads `initializationOptions`, which may set `cache_file` (a path to a
`final.redscripts` script cache) or `game_dir` (the Cyberpunk 2077 installation
root). `cache_file` takes precedence; `game_dir` looks for a cache under
`r6/cache/modded/final.redscripts.bk`, `r6/cache/final.redscripts.bk`, then
`r6/cache/final.redscripts`. When neither option is set, or the configured cache
is missing or cannot be loaded, the server falls back to a bundled base-game API
cache and warns that it may be out of date.

### Workspace options

The server loads a TOML file named `.redscript` from every workspace folder. It
supports:

- `source_roots`: source directories for both `.reds` and `.script` files,
  defaulting to `["."]`. `{ENV_VAR}` references are expanded from the environment.
- `format`: community formatter settings (`indent`, `max_width`,
  `max_chain_calls`, `max_chain_fields`, `max_chain_operators`,
  `max_chain_total`). When `format.indent` is unset, the server uses the tab size
  the client negotiates. These settings do not apply to REDmod files.

Example:

```toml
source_roots = [".", "../red4ext/plugins"]

[format]
indent = 2
max_width = 80
```

## REDmod support

The server supports community REDscript (`.reds`, exact extension) and REDmod
(`.script`, case-insensitive), selected by file extension. Both can share a
workspace and the same `source_roots`.

REDmod support is partial, not a replacement for the official compiler:

- Diagnostics, hover, go-to-definition, workspace symbols, and member completion
  work for the [supported language subset](https://github.com/jac3km4/redscript/blob/1800a8d/crates/compiler/redmod/adapter/README.md).
- Array completions include operations such as `Size`, `PushBack`, and `Last`.
  Completion tolerates a missing semicolon in the current statement; other syntax
  or unsupported-feature errors can prevent semantic features from working.
- Formatting preserves source spelling and uses four-space indentation, ignoring
  community formatter settings. Invalid syntax is left unchanged.

Find references, rename, and keyword/local completion are not implemented for
either dialect. Quick-fix code actions and `.redscript` formatter settings apply
to community REDscript only.

### Workspace behavior

Queries use unsaved changes from all open workspace files. Diagnostics update on
startup and save (including `.redscript` configuration saves), not on every
edit. Standalone files are analyzed separately. Missing source roots produce a
warning; restore them or update `.redscript` and refresh the workspace. Analysis
can be slow in large workspaces.

## Usage

### VS Code

Use the [Redscript IDE extension from the marketplace](https://marketplace.visualstudio.com/items?itemName=jac3km4.redscript-ide-vscode).
Associate `"*.script": "redscript"` under `files.associations` if your extension
version does not already attach to REDmod files.

### Zed

- Install the `REDscript` extension from the marketplace.
- Associate `.script` with the extension's language in `settings.json`:
  ```json
  "file_types": { "REDscript": ["**/*.reds", "**/*.script"] }
  ```
- Ensure the `redscript-ide` executable is on your `PATH`. Optionally set
  `lsp.redscript-ide.initialization_options.game_dir` (or `cache_file`) in
  `settings.json` to load a local or modded game cache (see
  [Configuration](#configuration)).

### IntelliJ

Use the (work in progress) [Redscript-IntelliJ Plugin](https://github.com/pawrequest/redscript-intellij).

### Neovim

With **Neovim 0.11 or newer**, add the following to `init.lua`. This uses the
built-in LSP configuration API; `nvim-lspconfig` is not required. Replace
`/path/to/redscript-ide` with a downloaded
[executable](https://github.com/jac3km4/redscript-ide/releases/latest), and set
`init_options.game_dir` (or `cache_file`) as needed (see
[Configuration](#configuration)).

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

Create a `.redscript` file (it can be empty) at the workspace root so `.reds`
and `.script` files share one project and source roots. Community `.reds` files
use Swift syntax highlighting as an approximate fallback; for grammar-accurate
highlighting, remove the `RedscriptSyntax` autocmd and use
[tree-sitter-reds](https://github.com/jac3km4/tree-sitter-reds) for the
`redscript` filetype and
[tree-sitter-redmod](https://github.com/jac3km4/tree-sitter-redmod) for the
`redmod` filetype, with each grammar's matching queries. Restart Neovim, then
verify with `:set filetype?` and `:checkhealth vim.lsp`.

### Helix

1. Install [Helix 25.07.1 or newer](https://github.com/helix-editor/helix/releases/latest),
   Git, and a C/C++ toolchain: Visual Studio Build Tools with the C++ workload on
   Windows, or `xcode-select --install` on macOS.
2. Run the setup command for your platform (no checkout needed). These commands
   download and execute code from GitHub; review the linked scripts first.

   **Windows ([PowerShell script](scripts/helix/setup-windows.ps1)):**
   ```powershell
   Invoke-RestMethod https://raw.githubusercontent.com/jac3km4/redscript-ide/main/scripts/helix/setup-windows.ps1 | Invoke-Expression
   ```

   **macOS ([shell script](scripts/helix/setup-macos.sh)):**
   ```sh
   curl -fsSL https://raw.githubusercontent.com/jac3km4/redscript-ide/main/scripts/helix/setup-macos.sh | sh
   ```

   Setup installs the latest `redscript-ide` release into Helix's `bin`
   directory, builds the pinned
   [REDscript](https://github.com/jac3km4/tree-sitter-reds) and
   [REDmod](https://github.com/jac3km4/tree-sitter-redmod) grammars with their
   highlighting queries, and appends the
   [language configuration](scripts/helix/languages.toml) to your user
   `languages.toml`. It backs up any file it modifies and stops with
   instructions if that file already mentions REDscript or REDmod. Set
   `REDSCRIPT_GAME_DIR` (or pass `-GameDir` on Windows) to point at a game
   install, and pass a runtime directory as the first argument (or `-Runtime` on
   Windows) to override the default runtime location.
3. Setup auto-detects Cyberpunk 2077 in Steam libraries (plus GOG/Epic records on
   Windows and common bundle locations on macOS). If detection fails, it warns
   and leaves `game_dir` empty so the server uses the bundled base-game API
   cache. To load a local or modded game cache instead, set `game_dir` in your
   user [`languages.toml`](https://docs.helix-editor.com/languages.html) to the
   game install root.
4. Create a `.redscript` file (it can be empty) at your workspace root and
   restart Helix. `.reds` and `.script` use separate grammars with the shared
   language server; see [REDmod support](#redmod-support). Check setup with
   `hx --health redscript` and `hx --health redmod`; missing indentation and
   text-object queries are expected.
