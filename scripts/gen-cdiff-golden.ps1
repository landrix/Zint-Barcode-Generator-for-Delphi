<#
.SYNOPSIS
  Erzeugt UnitTests/data/cdiff-golden.tsv aus der echten Zint-C-Bibliothek.

.DESCRIPTION
  Schickt jeden Fall aus UnitTests/data/cdiff-corpus.tsv durch ZBarcode_Encode
  der C-Referenz und schreibt auf, was danach im Symbol steht. Gegen diese Datei
  vergleicht UnitTests/Test_CDiff.pas den Delphi/FPC-Port.

  WICHTIG: Die Referenzdatei wird nie neu erzeugt, um einen roten Test gruen zu
  machen. Ihre Neuerzeugung ist ein eigener Commit mit eigener Begruendung -
  in gleicher Schaerfe wie "Keine Assertion entfernt, nur um gruen zu werden"
  (docs/PORTING_WORKFLOW.md, Abschnitt 6). Ein roter Differenztest heisst
  entweder, dass der Port abweicht, oder dass der Korpus einen Fall enthaelt,
  den er nicht enthalten sollte. Beides klaert man am Fall, nicht an der Datei.

  Braucht WSL und gcc. Beides ist fuer die Testgates NICHT noetig; deshalb liegt
  das Ergebnis als Datei im Repository.
#>
[CmdletBinding()]
param(
  [string]$Corpus,
  [string]$Golden,
  # cdump neu bauen, auch wenn es aktuell ist.
  [switch]$Force
)

$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path -Parent $PSScriptRoot
if (-not $Corpus) { $Corpus = Join-Path $repoRoot 'UnitTests/data/cdiff-corpus.tsv' }
if (-not $Golden) { $Golden = Join-Path $repoRoot 'UnitTests/data/cdiff-golden.tsv' }

if (-not (Test-Path -LiteralPath $Corpus)) {
  throw "gen-cdiff-golden: Korpus nicht gefunden: $Corpus"
}

function Convert-ToWslPath {
  param([string]$WindowsPath)
  $full = (Resolve-Path -LiteralPath $WindowsPath).Path
  $drive = $full.Substring(0, 1).ToLower()
  return "/mnt/$drive" + $full.Substring(2).Replace('\', '/')
}

if ($Force) {
  & (Join-Path $PSScriptRoot 'build-c-reference.ps1') -Force
} else {
  & (Join-Path $PSScriptRoot 'build-c-reference.ps1')
}

$corpusWsl = Convert-ToWslPath $Corpus
$runner = Convert-ToWslPath (Join-Path $PSScriptRoot 'tools/run-cdump.sh')

# cdump schreibt nach stdout in WSL; die Datei entsteht hier, damit sie CRLF-frei
# und in der Kodierung eindeutig ist.
# Kein 2>&1: cdump meldet die Fallzahl auf stderr, und PowerShell wuerde eine
# umgeleitete stderr-Zeile bei ErrorActionPreference = Stop als Fehler werten.
# So laeuft die Meldung direkt auf die Konsole.
$data = @(& wsl -e sh "$runner" "$corpusWsl")
$code = $LASTEXITCODE
if ($code -ne 0) {
  throw "gen-cdiff-golden: cdump fehlgeschlagen (Exitcode $code)"
}

if ($data.Count -lt 2) {
  throw "gen-cdiff-golden: cdump lieferte keine Daten"
}

$text = ($data -join "`n")
if (-not $text.EndsWith("`n")) { $text += "`n" }
[System.IO.File]::WriteAllText($Golden, $text, (New-Object System.Text.UTF8Encoding($false)))

$cases = $data.Count - 2  # Kommentarzeile und Kopfzeile
Write-Host "gen-cdiff-golden: $cases Faelle -> $Golden"
