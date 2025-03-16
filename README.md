# redscript-ide

Language server for [redscript](https://github.com/jac3km4/redscript)

## features

- error and warning diagnostics
- autocompletion for methods and fields
- hover for function definitions and types
- go-to-definition
  - bonus: limited support for redmod (scripted functions)
- workspace symbols
- formatting (beta)
- debugger (requires [redscript-dap](https://github.com/jac3km4/redscript-dap))
- hooks for external tools

![ide-gif](https://user-images.githubusercontent.com/11986158/135734766-b5423e2c-cf47-4836-97ba-5c771cef7cf2.gif)

## configuration

The language server will attempt to load a TOML file named `.redscript` from every workspace folder.
This file can contain the following configuration options:

- `source_roots` allows you to specify source directories where the compiler should look for REDscript files, defaults to `["."]`
- `format` block allows you to configure the formatter, you can find the available options [here](https://github.com/jac3km4/redscript/blob/c3d0ec6f12583eccc51b5a482583e8fb6641ce8d/crates/dotfile/src/lib.rs#L36-L43)

Here's an example `.redscript` file:

```toml
source_roots = [".", "../red4ext/plugins"]

[format]
indent = 2
max_width = 80
```

## usage

- ### VS Code

  Use the [Redscript IDE extension from the marketplace](https://marketplace.visualstudio.com/items?itemName=jac3km4.redscript-ide-vscode).

- ### Zed

  - Install the `REDscript` extension from the marketplace.
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
