<#
.SYNOPSIS
  Fuehrt beide Vollgates aus: Delphi (Win32) und Free Pascal.

.DESCRIPTION
  Eine Aenderung gilt erst als fertig, wenn beide Gates gruen sind.
  Siehe docs/PORTING_WORKFLOW.md Abschnitt 3.

  Beide Gates laufen immer durch, auch wenn das erste rot ist - sonst
  verdeckt ein Delphi-Fehler den FPC-Stand und umgekehrt.

.PARAMETER SkipDelphi
  Nur das FPC-Gate ausfuehren.

.PARAMETER SkipFpc
  Nur das Delphi-Gate ausfuehren.

.PARAMETER IncludeWsl
  Zusaetzlich das Linux-Compile-Gate (FPC 3.2.2 unter WSL) ausfuehren.
#>
param(
  [switch]$SkipDelphi,
  [switch]$SkipFpc,
  [switch]$IncludeWsl
)

$scripts = $PSScriptRoot
$results = [ordered]@{}

function Invoke-Gate {
  param([string]$Name, [scriptblock]$Action)
  Write-Host ""
  Write-Host ("=== {0} ===" -f $Name) -ForegroundColor Cyan
  try {
    & $Action
    $script:results[$Name] = "gruen"
  }
  catch {
    Write-Host $_.Exception.Message -ForegroundColor Red
    $script:results[$Name] = "ROT"
  }
}

if (-not $SkipDelphi) {
  Invoke-Gate "Delphi Win32" { & (Join-Path $scripts "build-delphi-tests.ps1") }
}
if (-not $SkipFpc) {
  Invoke-Gate "FPC aarch64-win64" { & (Join-Path $scripts "build-fpc-tests.ps1") }
}
if ($IncludeWsl) {
  Invoke-Gate "FPC Compile-Gate (WSL)" { & (Join-Path $scripts "build-fpc-tests.ps1") -Wsl }
}

Write-Host ""
Write-Host "=== Zusammenfassung ===" -ForegroundColor Cyan
foreach ($k in $results.Keys) {
  $color = if ($results[$k] -eq "gruen") { "Green" } else { "Red" }
  Write-Host ("{0,-28} {1}" -f $k, $results[$k]) -ForegroundColor $color
}

# Kein ausgefuehrtes Gate ist kein Erfolg. Ohne diese Pruefung meldete
# "gate-all.ps1 -SkipDelphi -SkipFpc" mit Exitcode 0 "Alle Gates gruen".
if ($results.Count -eq 0) {
  Write-Host ""
  throw "Kein Gate ausgefuehrt - nichts geprueft."
}

if ($results.Values -contains "ROT") {
  Write-Host ""
  throw "Mindestens ein Gate ist rot."
}
Write-Host ""
Write-Host "Alle Gates gruen." -ForegroundColor Green
