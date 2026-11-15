#!/usr/bin/env sh
set -eu

cargo run --package compact-cache -- "${1:-target/corpus/final.redscripts}" "${2:-assets/api.redscripts.zlib}"
