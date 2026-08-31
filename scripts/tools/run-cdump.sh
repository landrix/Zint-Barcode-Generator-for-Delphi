#!/bin/sh
# Laesst cdump ueber den Korpus laufen und gibt die Referenzdatei auf stdout aus.
# Aufgerufen von scripts/gen-cdiff-golden.ps1 ueber WSL.
#
#   run-cdump.sh <korpus.tsv>
#
# Die Ausgabe geht bewusst nach stdout: die Datei entsteht auf der
# Windows-Seite, damit Kodierung und Zeilenenden dort festgelegt sind.

set -e

corpus="$1"
bin="$HOME/.cache/zint-cdiff/cdump"

if [ -z "$corpus" ]; then
    echo "run-cdump: usage: run-cdump.sh <korpus.tsv>" >&2
    exit 2
fi
if [ ! -f "$corpus" ]; then
    echo "run-cdump: Korpus nicht gefunden: $corpus" >&2
    exit 2
fi
if [ ! -x "$bin" ]; then
    echo "run-cdump: cdump fehlt - erst scripts/build-c-reference.ps1 laufen lassen" >&2
    exit 2
fi

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
"$bin" "$corpus" "$tmp"
cat "$tmp"
