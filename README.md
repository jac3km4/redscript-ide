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

![ide-gif](https://user-images.githubusercontent.com/11986158/135734766-b5423e2c-cf47-4836-97ba-5c771cef7cf2.gif)
