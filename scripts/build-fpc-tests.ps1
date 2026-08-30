<#
.SYNOPSIS
  Baut und startet die Testsuite unter Free Pascal.

.DESCRIPTION
  Gegenstueck zu build-delphi-tests.ps1. Beide Runner fuehren dieselben
  Testunits aus; die Framework-Unterschiede kapselt UnitTests\TestFramework_Zint.pas.

  Ohne -Wsl: FPC 3.3.1 (aarch64-win64), baut und fuehrt die Tests aus.
  Mit -Wsl:  FPC 3.2.2 unter WSL, nur Core-Compile-Gate. FPC 3.2.2 kennt den
             Modeswitch prefixedattributes nicht und kann die Testunits daher
             nicht uebersetzen; geprueft wird nur, dass die Barcode-Units
             plattformunabhaengig uebersetzen.

  Das Gate ist bewusst misstrauisch: es meldet nur gruen, wenn Build, Exitcode,
  Auswertbarkeit der Ausgabe UND die Mindest-Testzahl stimmen. Ein Gate, das
  nicht merkt, wenn Tests verschwinden, ist wertlos - siehe den in
  docs/PORTING_WORKFLOW.md dokumentierten Fall, in dem DUnitX vier Fixtures
  still uebersprungen hat.

.PARAMETER MinTests
  Erwartete Mindestzahl ausgefuehrter Tests. Unterschreitung ist ein Fehler,
  auch wenn kein einzelner Test fehlschlaegt. Beim Hinzufuegen von Tests
  hochsetzen.

.PARAMETER SkipRun
  Nur bauen, nicht ausfuehren.

.PARAMETER Wsl
  Statt des Windows-Gates das Linux-Compile-Gate ausfuehren.
#>
param(
  [string]$FpcRoot  = "D:\bin\fpc\fpcupdeluxe",
  [string]$Target   = "aarch64-win64",
  [int]$MinTests    = 908,
  [switch]$SkipRun,
  [switch]$Wsl
)

$ErrorActionPreference = "Stop"
$repoRoot = Split-Path -Parent $PSScriptRoot

# ---------------------------------------------------------------- WSL-Gate --
if ($Wsl) {
  Write-Host "FPC Compile-Gate (WSL, nur Core-Units)" -ForegroundColor Cyan

  $probe = Join-Path $env:TEMP "zint_fpc_core.lpr"
  @'
program zint_fpc_core;
{$mode objfpc}{$H+}
uses
  zint, zint_helper, zint_common, zint_large, zint_reedsol, zint_gs1,
  zint_code, zint_2of5, zint_code128, zint_upcean, zint_medical,
  zint_plessey, zint_telepen, zint_postal, zint_auspost, zint_imail,
  zint_code16k, zint_code49, zint_rss, zint_composite,
  zint_pdf417, zint_dmatrix, zint_qr, zint_aztec, zint_code1,
  zint_maxicode, zint_gridmtx, zint_dotcode, zint_sjis, zint_gb2312;
begin
  WriteLn('core units ok');
end.
'@ | Set-Content -Path $probe -Encoding ASCII

  $wslRepo  = "/mnt/" + $repoRoot.Substring(0,1).ToLower() + $repoRoot.Substring(2).Replace('\','/')
  $wslProbe = "/mnt/" + $probe.Substring(0,1).ToLower() + $probe.Substring(2).Replace('\','/')
  $out      = "/tmp/zint_fpc_core"

  # Die Binaerdatei vorher entfernen: ihre Existenz danach ist der positive
  # Erfolgsnachweis. Ohne diesen Nachweis wuerde z.B. ein fehlendes fpc
  # ("sh: 1: fpc: not found", Exitcode 127) auf kein Fehlermuster passen und
  # das Gate faelschlich gruen melden.
  $cmd = "rm -rf $out && mkdir -p $out && fpc -Mobjfpc -Sh -vn -Fu'$wslRepo' -FU$out -o$out/probe '$wslProbe' 2>&1"

  $result   = wsl -e sh -c $cmd
  $wslExit  = $LASTEXITCODE
  $errors   = $result | Select-String -Pattern 'Fatal|Error:'
  $produced = (wsl -e sh -c "test -f $out/probe && echo VORHANDEN") 2>$null

  if ($wslExit -ne 0 -or $errors -or $produced -notcontains 'VORHANDEN') {
    $result | Select-Object -Last 25 | ForEach-Object { Write-Host $_ -ForegroundColor Red }
    if ($wslExit -ne 0) { Write-Host ("wsl Exitcode: {0}" -f $wslExit) -ForegroundColor Red }
    if ($produced -notcontains 'VORHANDEN') {
      Write-Host "Keine Binaerdatei erzeugt - es wurde nichts uebersetzt." -ForegroundColor Red
    }
    throw "FPC Compile-Gate (WSL) fehlgeschlagen."
  }

  Write-Host "FPC Compile-Gate (WSL): ok" -ForegroundColor Green
  return
}

# ------------------------------------------------------------ Windows-Gate --
$fpc = Join-Path $FpcRoot "fpc\bin\$Target\fpc.exe"
if (-not (Test-Path $fpc)) { throw "fpc.exe nicht gefunden: $fpc" }

$version = (& $fpc -iV).Trim()
Write-Host "FPC $version ($Target)" -ForegroundColor Cyan
if ([version]($version -replace '[^0-9.].*$','') -lt [version]"3.3.0") {
  throw "FPC $version kann den Modeswitch prefixedattributes nicht. Fuer das Test-Gate wird 3.3.1 oder neuer benoetigt."
}

$unitDir = Join-Path $repoRoot "UnitTests\fpc\units"
$exe     = Join-Path $repoRoot "UnitTests\fpc\ZintTests.exe"
$raw     = Join-Path $repoRoot "UnitTests\fpc\ZintTests"

New-Item -ItemType Directory -Force $unitDir | Out-Null

Push-Location $repoRoot
try {
  # Alte Artefakte entfernen, sonst laeuft nach einem Build ohne .exe-Endung
  # weiterhin die vorherige Binaerdatei.
  Remove-Item $exe -Force -ErrorAction SilentlyContinue
  Remove-Item $raw -Force -ErrorAction SilentlyContinue

  $build = & $fpc -Mobjfpc -Sh -vn -B `
                  -FuUnitTests -Fu. -FUUnitTests\fpc\units `
                  -oUnitTests\fpc\ZintTests.exe UnitTests\fpc\ZintTests.lpr 2>&1
  $buildExit   = $LASTEXITCODE
  $buildErrors = $build | Select-String -Pattern 'Fatal|Error:'
  if ($buildExit -ne 0 -or $buildErrors) {
    $buildErrors | Select-Object -First 25 | ForEach-Object { Write-Host $_ -ForegroundColor Red }
    throw ("FPC-Build fehlgeschlagen (Exitcode {0})." -f $buildExit)
  }

  # FPC haengt unter Windows nicht immer .exe an.
  if (Test-Path $raw) { Move-Item $raw $exe -Force }
  if (-not (Test-Path $exe)) { throw "Test-Exe nicht gefunden: $exe" }

  if ($SkipRun) {
    Write-Host "Build ok (Testausfuehrung uebersprungen)." -ForegroundColor Green
    return
  }

  $log = Join-Path $repoRoot "UnitTests\fpc\fpc-run.log"
  & $exe --all --format=plain --skiptiming 2>&1 | Tee-Object -FilePath $log | Out-Null
  $runExit = $LASTEXITCODE

  $runLine  = Select-String -Path $log -Pattern 'Number of run tests:\s*(\d+)' | Select-Object -First 1
  $errLine  = Select-String -Path $log -Pattern 'Number of errors:\s*(\d+)'    | Select-Object -First 1
  $failLine = Select-String -Path $log -Pattern 'Number of failures:\s*(\d+)'  | Select-Object -First 1

  # Nicht auswertbare Ausgabe ist ein Fehler, kein "null Fehler". Sonst wuerde
  # eine geaenderte Beschriftung der Statistik das Gate stillschweigend gruen faerben.
  if (-not $runLine -or -not $errLine -or -not $failLine) {
    Write-Host "Zusammenfassung des FPC-Laufs nicht auswertbar." -ForegroundColor Red
    Get-Content $log -Tail 20 | ForEach-Object { Write-Host $_ }
    throw "FPC-Lauf nicht auswertbar (siehe $log)."
  }

  $r = [int]$runLine.Matches[0].Groups[1].Value
  $e = [int]$errLine.Matches[0].Groups[1].Value
  $f = [int]$failLine.Matches[0].Groups[1].Value

  Write-Host ("FPC: {0} Tests, {1} Errors, {2} Failures (erwartet mindestens {3})" -f $r, $e, $f, $MinTests)

  if (($e -gt 0) -or ($f -gt 0)) {
    Select-String -Path $log -Pattern 'Message:' | Select-Object -First 20 |
      ForEach-Object { Write-Host $_.Line.Trim() -ForegroundColor Red }
    throw "FPC-Tests fehlgeschlagen (Details in $log)."
  }

  if ($r -lt $MinTests) {
    throw ("Nur {0} Tests ausgefuehrt, erwartet mindestens {1}. Es sind Tests verschwunden - Registrierung in UnitTests\fpc\ZintTests.lpr pruefen. (Wenn die Verringerung beabsichtigt ist: -MinTests anpassen.)" -f $r, $MinTests)
  }

  if ($runExit -ne 0) {
    throw ("Testrunner endete mit Exitcode {0}, obwohl die Zusammenfassung gruen ist (moeglicher Absturz nach dem Lauf). Siehe $log." -f $runExit)
  }

  Write-Host "FPC-Gate gruen." -ForegroundColor Green
}
finally {
  Pop-Location
}
