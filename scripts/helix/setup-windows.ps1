param(
    [string]$Runtime = (Join-Path $env:APPDATA 'helix\runtime'),
    [string]$GameDir = $env:REDSCRIPT_GAME_DIR
)

$ErrorActionPreference = 'Stop'
$languagesFile = Join-Path $env:APPDATA 'helix\languages.toml'
function Assert-NoExistingConfiguration {
    if ((Test-Path -LiteralPath $languagesFile) -and
        (Select-String -LiteralPath $languagesFile -Pattern 'redscript|redmod' -Quiet)) {
        throw @"
Cannot append Helix configuration: '$languagesFile' already mentions REDscript or REDmod.
Appending would risk duplicate language-server, language, or grammar tables. No configuration was appended.
To reinstall, back up this file, remove only the REDscript/REDmod [[language]] and [[grammar]] tables
and the [language-server.redscript-ide] table (including their settings), then rerun setup.
The safety check also matches comments; remove any remaining REDscript/REDmod mentions.
Keep unrelated language settings. To retain your current setup, cancel instead of rerunning.
"@
    }
}
Assert-NoExistingConfiguration
Get-Command hx -ErrorAction Stop | Out-Null
Get-Command git -ErrorAction Stop | Out-Null
$Runtime = [System.IO.Path]::GetFullPath($Runtime)
$work = Join-Path ([System.IO.Path]::GetTempPath()) ('redscript-helix-' + [guid]::NewGuid())
$previousAppData = $env:APPDATA
$previousRuntime = $env:HELIX_RUNTIME
$serverPath = Join-Path (Split-Path $languagesFile) 'bin\redscript-ide.exe'

function Test-GameDirectory([string]$Path) {
    if (-not $Path) { return $false }
    foreach ($cache in @('r6\cache\modded\final.redscripts.bk', 'r6\cache\final.redscripts.bk', 'r6\cache\final.redscripts')) {
        if (Test-Path -LiteralPath (Join-Path $Path $cache) -PathType Leaf) { return $true }
    }
    return $false
}

function Find-GameDirectory {
    $candidates = @()
    $steamRoots = @()
    foreach ($key in @('HKCU:\Software\Valve\Steam', 'HKLM:\SOFTWARE\WOW6432Node\Valve\Steam')) {
        $steam = Get-ItemProperty -LiteralPath $key -ErrorAction SilentlyContinue
        if ($steam.SteamPath) { $steamRoots += $steam.SteamPath }
        if ($steam.InstallPath) { $steamRoots += $steam.InstallPath }
    }
    if (${env:ProgramFiles(x86)}) { $steamRoots += Join-Path ${env:ProgramFiles(x86)} 'Steam' }
    foreach ($steamRoot in ($steamRoots | Select-Object -Unique)) {
        $libraries = @($steamRoot)
        $vdf = Join-Path $steamRoot 'steamapps\libraryfolders.vdf'
        if (Test-Path -LiteralPath $vdf) {
            $content = Get-Content -Raw -LiteralPath $vdf
            foreach ($match in [regex]::Matches($content, '"path"\s+"([^"]+)"')) {
                $libraries += $match.Groups[1].Value.Replace('\\', '\')
            }
        }
        foreach ($library in $libraries) {
            $candidates += Join-Path $library 'steamapps\common\Cyberpunk 2077'
        }
    }
    foreach ($key in @('HKLM:\SOFTWARE\GOG.com\Games\1423049311', 'HKLM:\SOFTWARE\WOW6432Node\GOG.com\Games\1423049311')) {
        $gog = Get-ItemProperty -LiteralPath $key -ErrorAction SilentlyContinue
        if ($gog.path) { $candidates += $gog.path }
    }
    if ($env:ProgramData) {
        $manifests = Join-Path $env:ProgramData 'Epic\EpicGamesLauncher\Data\Manifests'
        foreach ($manifest in (Get-ChildItem -LiteralPath $manifests -Filter '*.item' -ErrorAction SilentlyContinue)) {
            try {
                $item = Get-Content -Raw -LiteralPath $manifest.FullName | ConvertFrom-Json
                if ($item.DisplayName -eq 'Cyberpunk 2077' -and $item.InstallLocation) {
                    $candidates += $item.InstallLocation
                }
            } catch { Write-Warning "Could not read launcher manifest: $($manifest.FullName)" }
        }
    }
    foreach ($candidate in ($candidates | Select-Object -Unique)) {
        if (Test-GameDirectory $candidate) { return [System.IO.Path]::GetFullPath($candidate) }
    }
}

if ($GameDir) {
    $GameDir = [System.IO.Path]::GetFullPath($GameDir)
    if (-not (Test-GameDirectory $GameDir)) {
        throw "No script cache found under $GameDir. Supply the Cyberpunk 2077 root via -GameDir or REDSCRIPT_GAME_DIR."
    }
} else {
    $GameDir = Find-GameDirectory
}
if ($GameDir) {
    Write-Host "Using Cyberpunk 2077 installation: $GameDir"
} else {
    Write-Warning "Cyberpunk 2077 was not found. Set game_dir in $languagesFile after setup, or rerun with REDSCRIPT_GAME_DIR set before installing."
}

try {
    New-Item -ItemType Directory -Path "$work\helix" -Force | Out-Null
    if ($PSScriptRoot) {
        $languages = Get-Content -Raw (Join-Path $PSScriptRoot 'languages.toml')
    } else {
        $languages = (Invoke-WebRequest -UseBasicParsing 'https://raw.githubusercontent.com/jac3km4/redscript-ide/0.2.x/scripts/helix/languages.toml').Content
    }
    Write-Host 'Downloading the latest redscript-ide release from GitHub...'
    Invoke-WebRequest -UseBasicParsing 'https://github.com/jac3km4/redscript-ide/releases/latest/download/redscript-ide.exe' -OutFile "$work\redscript-ide.exe"
    $serverToml = ConvertTo-Json -Compress ($serverPath.Replace('\', '/'))
    $languages = $languages.Replace('"/path/to/redscript-ide"', $serverToml)
    if ($GameDir) {
        $gameToml = ConvertTo-Json -Compress ($GameDir.Replace('\', '/'))
        $languages = $languages.Replace('"/path/to/cyberpunk2077"', $gameToml)
    }
    $config = 'use-grammars = { only = ["redscript", "redmod"] }' + "`n" + $languages
    [System.IO.File]::WriteAllText("$work\helix\languages.toml", $config)

    # Isolate the build from user/project settings, including grammar filters.
    $env:APPDATA = $work
    Remove-Item Env:HELIX_RUNTIME -ErrorAction SilentlyContinue
    Push-Location $work
    try {
        & hx --grammar fetch
        if ($LASTEXITCODE -ne 0) { throw 'Helix grammar fetch failed.' }
        & hx --grammar build
        if ($LASTEXITCODE -ne 0) {
            throw 'Helix grammar build failed. Install Visual Studio Build Tools with the C++ workload.'
        }
    } finally {
        Pop-Location
    }

    New-Item -ItemType Directory -Force -Path "$Runtime\grammars", "$Runtime\queries" | Out-Null
    foreach ($language in @('redscript', 'redmod')) {
        $queries = Join-Path "$Runtime\queries" $language
        if (Test-Path $queries) {
            $backup = "$queries.backup.$([guid]::NewGuid())"
            Move-Item -LiteralPath $queries -Destination $backup
            Write-Host "Previous $language queries backed up to $backup"
        }
        New-Item -ItemType Directory -Path $queries | Out-Null
        Copy-Item "$work\helix\runtime\grammars\$language.dll" "$Runtime\grammars\$language.dll" -Force
        Copy-Item "$work\helix\runtime\grammars\sources\$language\queries\highlights.scm" "$queries\highlights.scm"
    }
    Assert-NoExistingConfiguration
    New-Item -ItemType Directory -Force -Path (Split-Path $serverPath) | Out-Null
    Copy-Item -LiteralPath "$work\redscript-ide.exe" -Destination $serverPath -Force
    New-Item -ItemType Directory -Force -Path (Split-Path $languagesFile) | Out-Null
    if (Test-Path -LiteralPath $languagesFile) {
        $backup = "$languagesFile.backup.$([guid]::NewGuid())"
        Copy-Item -LiteralPath $languagesFile -Destination $backup
        Write-Host "Previous language configuration backed up to $backup"
    }
    [System.IO.File]::AppendAllText($languagesFile, "`n" + $languages + "`n", [System.Text.UTF8Encoding]::new($false))
    Write-Host "Installed REDscript and REDmod grammars and queries in $Runtime"
    Write-Host "Configuration appended to $languagesFile"
    Write-Host "Language server installed at $serverPath (no PATH changes needed)."
    if (-not $GameDir) { Write-Warning 'Set game_dir in languages.toml before using the language server.' }
    Write-Host 'Restart Helix to load the configuration.'
} finally {
    $env:APPDATA = $previousAppData
    $env:HELIX_RUNTIME = $previousRuntime
    if (Test-Path $work) { Remove-Item -Recurse -Force $work }
}
