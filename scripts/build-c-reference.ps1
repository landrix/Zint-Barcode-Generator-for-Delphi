<#
.SYNOPSIS
  Baut scripts/tools/cdump.c gegen die C-Referenz - in WSL, mit gcc.

.DESCRIPTION
  Das Ergebnis ist ein Hilfsprogramm, das aus der echten Zint-C-Bibliothek die
  Referenzwerte fuer den Differenztest ausgibt. Es wird nur gebraucht, wenn
  UnitTests/data/cdiff-golden.tsv neu erzeugt wird - nicht bei einem
  gewoehnlichen Gate-Lauf. Weder WSL noch gcc sind Voraussetzung fuer die
  Testgates; das ist der Sinn der Referenzdatei.

  Das Programm landet in ~/.cache/zint-cdiff/ innerhalb von WSL, nicht im
  Repository - es ist ein Werkzeug, kein Artefakt.

  Die eigentliche Uebersetzung steht in scripts/tools/build-cdump.sh, damit
  sich die Zitierregeln von PowerShell und sh nicht ins Gehege kommen.
#>
[CmdletBinding()]
param(
  # Neu bauen, auch wenn das Programm schon aktuell ist.
  [switch]$Force
)

$ErrorActionPreference = 'Stop'
$repoRoot = Split-Path -Parent $PSScriptRoot

function Convert-ToWslPath {
  param([string]$WindowsPath)
  $full = (Resolve-Path -LiteralPath $WindowsPath).Path
  $drive = $full.Substring(0, 1).ToLower()
  return "/mnt/$drive" + $full.Substring(2).Replace('\', '/')
}

$cRoot = Join-Path $repoRoot 'Lib/zint-master-2026-03-13-b3a3c0d'
if (-not (Test-Path -LiteralPath $cRoot)) {
  throw "C-Referenz nicht gefunden: $cRoot (siehe docs/PORTING_WORKFLOW.md, Abschnitt 2b)"
}

$backend = Convert-ToWslPath (Join-Path $cRoot 'backend')
$dumper = Convert-ToWslPath (Join-Path $PSScriptRoot 'tools/cdump.c')
$builder = Convert-ToWslPath (Join-Path $PSScriptRoot 'tools/build-cdump.sh')
# "default" laesst sh selbst $HOME/.cache/zint-cdiff/cdump waehlen; PowerShell
# koennte $HOME der WSL-Seite nicht aufloesen.
$forceArg = if ($Force) { 'force' } else { '' }

$out = & wsl -e sh "$builder" "$backend" "$dumper" 'default' $forceArg 2>&1
$code = $LASTEXITCODE
$out | ForEach-Object { Write-Host $_ }
if ($code -ne 0) {
  throw "build-c-reference: Uebersetzung in WSL fehlgeschlagen (Exitcode $code)"
}

Write-Host 'build-c-reference: cdump liegt in ~/.cache/zint-cdiff/ (in WSL)'
