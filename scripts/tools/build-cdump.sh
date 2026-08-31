#!/bin/sh
# Baut cdump gegen die C-Referenz. Aufgerufen von scripts/build-c-reference.ps1
# ueber WSL; steht als eigene Datei da, damit die Zitierregeln von PowerShell
# und sh sich nicht ins Gehege kommen.
#
#   build-cdump.sh <backend-verzeichnis> <cdump.c> <ausgabedatei|default> [force]
#
# Ohne libpng und ohne png.c: der Differenztest vergleicht, was nach
# ZBarcode_Encode im Symbol steht, keine Bilddateien.

set -e

backend="$1"
dumper="$2"
outbin="$3"
force="$4"

# "default" statt eines Pfades: sh loest $HOME auf, PowerShell koennte es nicht.
if [ "$outbin" = "default" ]; then
    outbin="$HOME/.cache/zint-cdiff/cdump"
fi

if [ -z "$backend" ] || [ -z "$dumper" ] || [ -z "$outbin" ]; then
    echo "build-cdump: usage: build-cdump.sh <backend> <cdump.c> <out> [force]" >&2
    exit 2
fi
if [ ! -d "$backend" ]; then
    echo "build-cdump: backend nicht gefunden: $backend" >&2
    exit 2
fi
if [ ! -f "$dumper" ]; then
    echo "build-cdump: cdump.c nicht gefunden: $dumper" >&2
    exit 2
fi
if ! command -v gcc >/dev/null 2>&1; then
    echo "build-cdump: gcc fehlt. In WSL: sudo apt install build-essential" >&2
    exit 2
fi

mkdir -p "$(dirname "$outbin")"

if [ "$force" != "force" ] && [ -x "$outbin" ] && [ "$outbin" -nt "$dumper" ]; then
    echo "build-cdump: bereits aktuell"
    exit 0
fi

srcs=$(ls "$backend"/*.c | grep -v '/png\.c$')
# shellcheck disable=SC2086
gcc -O1 -w -DZINT_NO_PNG -I"$backend" $srcs "$dumper" -o "$outbin" -lm
echo "build-cdump: gebaut"
