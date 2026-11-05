#!/bin/sh
# Download the server and build grammars in isolation before appending configuration.
set -eu

config_dir=${XDG_CONFIG_HOME:-$HOME/.config}/helix
languages_file="$config_dir/languages.toml"
check_config() {
    if [ -f "$languages_file" ] && grep -Eiq 'redscript|redmod' "$languages_file"; then
        printf "Cannot append Helix configuration: '%s' already mentions REDscript or REDmod.\n" "$languages_file" >&2
        cat >&2 <<'EOF'
Appending would risk duplicate language-server, language, or grammar tables. No configuration was appended.
To reinstall, back up this file, remove only the REDscript/REDmod [[language]] and [[grammar]] tables
and the [language-server.redscript-ide] table (including their settings), then rerun setup.
The safety check also matches comments; remove any remaining REDscript/REDmod mentions.
Keep unrelated language settings. To retain your current setup, cancel instead of rerunning.
EOF
        exit 1
    fi
}
check_config

command -v hx >/dev/null 2>&1 || { echo 'Install Helix 25.07.1 or newer first.' >&2; exit 1; }
command -v git >/dev/null 2>&1 || { echo 'Install Git first.' >&2; exit 1; }
command -v curl >/dev/null 2>&1 || { echo 'Install curl first.' >&2; exit 1; }
case $(uname -m) in
    arm64) asset=redscript-ide-aarch64-apple-darwin ;;
    x86_64) asset=redscript-ide-x86_64-apple-darwin ;;
    *) echo 'Unsupported architecture; expected arm64 or x86_64.' >&2; exit 1 ;;
esac
xcrun --find clang >/dev/null 2>&1 || { echo 'Install the command line tools with: xcode-select --install' >&2; exit 1; }

runtime=${1:-${XDG_CONFIG_HOME:-$HOME/.config}/helix/runtime}
mkdir -p "$runtime"
runtime=$(CDPATH= cd -- "$runtime" && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
trap 'exit 1' HUP INT TERM
mkdir -p "$work/helix"
curl -fL "https://github.com/jac3km4/redscript-ide/releases/latest/download/$asset" -o "$work/redscript-ide"
chmod 755 "$work/redscript-ide"

is_game_dir() {
    [ -f "$1/r6/cache/final.redscripts" ] ||
        [ -f "$1/r6/cache/final.redscripts.bk" ] ||
        [ -f "$1/r6/cache/modded/final.redscripts.bk" ]
}
# Usage: setup-macos.sh [runtime-directory [game-directory]]. Environment wins.
game_dir=${REDSCRIPT_GAME_DIR:-${2:-}}
if [ -n "$game_dir" ]; then
    if ! is_game_dir "$game_dir"; then
        printf 'No REDscript cache found under game directory: %s\n' "$game_dir" >&2
        exit 1
    fi
else
    steam="$HOME/Library/Application Support/Steam"
    printf '%s\n' "$steam/steamapps/common/Cyberpunk 2077" > "$work/game-candidates"
    if [ -f "$steam/steamapps/libraryfolders.vdf" ]; then
        # Read both modern "path" entries and legacy numbered library entries.
        awk '
            {
                count = 0; quoted = 0; value = ""
                for (i = 1; i <= length($0); i++) {
                    c = substr($0, i, 1)
                    if (quoted && c == "\\") {
                        next_char = substr($0, i + 1, 1)
                        if (next_char == "\\" || next_char == "\"") {
                            value = value next_char; i++; continue
                        }
                    }
                    if (c == "\"") {
                        if (quoted) { fields[++count] = value; value = "" }
                        quoted = !quoted
                    } else if (quoted) value = value c
                }
                if (count >= 2 && (fields[1] == "path" || fields[1] ~ /^[0-9]+$/) && fields[2] ~ /^\//)
                    print fields[2] "/steamapps/common/Cyberpunk 2077"
            }
        ' "$steam/steamapps/libraryfolders.vdf" >> "$work/game-candidates"
    fi
    printf '%s\n' "$HOME/Applications/Cyberpunk 2077.app/Contents/Resources" \
        '/Applications/Cyberpunk 2077.app/Contents/Resources' >> "$work/game-candidates"
    while IFS= read -r candidate; do
        if is_game_dir "$candidate"; then
            game_dir=$candidate
            break
        fi
    done < "$work/game-candidates"
fi
if [ -n "$game_dir" ]; then
    game_dir=$(CDPATH= cd -- "$game_dir" && pwd)
    printf 'Found game directory: %s\n' "$game_dir"
else
    game_dir=/path/to/cyberpunk2077
    printf 'Game not found. Set game_dir in %s, or rerun with REDSCRIPT_GAME_DIR pointing to the directory containing r6/cache.\n' "$languages_file" >&2
fi

if [ -f "$0" ] && [ -f "$(dirname -- "$0")/languages.toml" ]; then
    cat "$(dirname -- "$0")/languages.toml" > "$work/languages.toml"
else
    curl -fsSL https://raw.githubusercontent.com/jac3km4/redscript-ide/0.2.x/scripts/helix/languages.toml > "$work/languages.toml"
fi
mkdir -p "$config_dir/bin"
config_dir=$(CDPATH= cd -- "$config_dir" && pwd)
languages_file="$config_dir/languages.toml"
server_path="$config_dir/bin/redscript-ide"
# ENVIRON avoids awk -v interpreting backslashes in paths. Replace literally,
# rather than using gsub replacement strings (where & and backslashes are special).
REDSCRIPT_SERVER_PATH="$server_path" REDSCRIPT_GAME_PATH="$game_dir" awk '
    function toml_escape(value,    result, i, c, j) {
        result = ""
        for (i = 1; i <= length(value); i++) {
            c = substr(value, i, 1)
            if (c == "\\" || c == "\"") result = result "\\" c
            else {
                for (j = 1; j <= 31; j++)
                    if (c == sprintf("%c", j)) break
                if (j <= 31 || c == sprintf("%c", 127))
                    result = result sprintf("\\u%04X", j <= 31 ? j : 127)
                else result = result c
            }
        }
        return result
    }
    function replace_literal(line, placeholder, value,    pos) {
        pos = index(line, placeholder)
        if (pos) return substr(line, 1, pos - 1) value substr(line, pos + length(placeholder))
        return line
    }
    BEGIN {
        server = toml_escape(ENVIRON["REDSCRIPT_SERVER_PATH"])
        game = toml_escape(ENVIRON["REDSCRIPT_GAME_PATH"])
    }
    {
        if (index($0, "/path/to/redscript-ide"))
            print replace_literal($0, "/path/to/redscript-ide", server)
        else print replace_literal($0, "/path/to/cyberpunk2077", game)
    }
' "$work/languages.toml" > "$work/configured-languages.toml"
mv "$work/configured-languages.toml" "$work/languages.toml"
{
    printf '%s\n' 'use-grammars = { only = ["redscript", "redmod"] }'
    cat "$work/languages.toml"
} > "$work/helix/languages.toml"

(
    cd "$work"
    export XDG_CONFIG_HOME="$work"
    unset HELIX_RUNTIME
    hx --grammar fetch
    hx --grammar build
)

mkdir -p "$runtime/grammars" "$runtime/queries"
for language in redscript redmod; do
    queries="$runtime/queries/$language"
    if [ -e "$queries" ]; then
        backup=$(mktemp -d "$runtime/queries/$language.backup.XXXXXX")
        mv "$queries" "$backup/original"
        printf 'Previous %s queries backed up to %s/original\n' "$language" "$backup"
    fi
    mkdir -p "$queries"
    cp "$work/helix/runtime/grammars/$language.so" "$runtime/grammars/$language.so"
    cp "$work/helix/runtime/grammars/sources/$language/queries/highlights.scm" "$queries/highlights.scm"
done
check_config
mv -f "$work/redscript-ide" "$server_path"
if [ -f "$languages_file" ]; then
    backup=$(mktemp "$languages_file.backup.XXXXXX")
    cp "$languages_file" "$backup"
    printf 'Previous language configuration backed up to %s\n' "$backup"
fi
{
    printf '\n'
    cat "$work/languages.toml"
    printf '\n'
} >> "$languages_file"
printf 'Installed language server in %s\nInstalled REDscript and REDmod grammars and queries in %s\nConfiguration appended to %s\nRestart Helix to use the new configuration.\n' "$server_path" "$runtime" "$languages_file"
