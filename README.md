# redscript-ide

Language server for [redscript](https://github.com/jac3km4/redscript)

## features

- error and warning diagnostics
- autocompletion for methods and fields
- hover for function definitions and types
- go-to-definition
  - bonus: limited support for redmod (scripted functions)
- formatting (beta)
- debugger (requires [redscript-dap](https://github.com/jac3km4/redscript-dap))
- hooks for external tools

![ide-gif](https://user-images.githubusercontent.com/11986158/135734766-b5423e2c-cf47-4836-97ba-5c771cef7cf2.gif)

## configuration

The language server will attempt to load a TOML file named `.redscript-ide` from every workspace folder.
This file can contain some configuration options:

- `redscript_dir` maps the source directory to a different folder than the workspace folder root
- `hooks` allows you to configure hooks on certain events, an example would be creating a file to signal something to an external tool
  - for instance, you can trigger a reload of RedHotTools when the workspace successfully typechecks:
    ```toml
    [[hooks.successful_check]]
    # you can use {game_dir} to refer to the game directory if it's configured for the extension in your editor (e.g. VSCode),
    # another available variable is {workspace_dir} which refers to the workspace directory that contains the checked file
    create_file = "{game_dir}\\red4ext\\plugins\\RedHotTools\\.hot-scripts"
    ```

## usage

- ### VS Code

  Use the [Redscript IDE extension from the marketplace](https://marketplace.visualstudio.com/items?itemName=jac3km4.redscript-ide-vscode).

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
          autocmd BufNewFile,BufRead *.reds set filetype=reds | set syntax=swift
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
  file-types = ["reds"]
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
