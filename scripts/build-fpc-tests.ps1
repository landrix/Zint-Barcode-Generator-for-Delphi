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

.PARAMETER FpcRoot
  Wurzel der FPC-Installation.

.PARAMETER SkipRun
  Nur bauen, nicht ausfuehren.

.PARAMETER Wsl
  Statt des Windows-Gates das Linux-Compile-Gate ausfuehren.
#>
param(
  [string]$FpcRoot = "D:\bin\fpc\fpcupdeluxe",
  [string]$Target  = "aarch64-win64",
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

  $cmd = "mkdir -p $out && fpc -Mobjfpc -Sh -vn -Fu'$wslRepo' -FU$out -o$out/probe '$wslProbe' 2>&1"
  $result = wsl -e sh -c $cmd
  $errors = $result | Select-String -Pattern 'Fatal|Error:'

  if ($errors) {
    $errors | ForEach-Object { Write-Host $_ -ForegroundColor Red }
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

$project = Join-Path $repoRoot "UnitTests\fpc\ZintTests.lpr"
$unitDir = Join-Path $repoRoot "UnitTests\fpc\units"
$exe     = Join-Path $repoRoot "UnitTests\fpc\ZintTests.exe"

New-Item -ItemType Directory -Force $unitDir | Out-Null

Push-Location $repoRoot
try {
  # Alte Artefakte entfernen, sonst laeuft nach einem Build ohne .exe-Endung
  # weiterhin die vorherige Binaerdatei.
  Remove-Item $exe -Force -ErrorAction SilentlyContinue
  Remove-Item (Join-Path $repoRoot "UnitTests\fpc\ZintTests") -Force -ErrorAction SilentlyContinue

  $build = & $fpc -Mobjfpc -Sh -vn -B `
                  -FuUnitTests -Fu. -FUUnitTests\fpc\units `
                  -oUnitTests\fpc\ZintTests.exe UnitTests\fpc\ZintTests.lpr 2>&1
  $buildErrors = $build | Select-String -Pattern 'Fatal|Error:'
  if ($buildErrors) {
    $buildErrors | Select-Object -First 25 | ForEach-Object { Write-Host $_ -ForegroundColor Red }
    throw "FPC-Build fehlgeschlagen."
  }

  # FPC haengt unter Windows nicht immer .exe an.
  $produced = Join-Path $repoRoot "UnitTests\fpc\ZintTests"
  if (Test-Path $produced) { Move-Item $produced $exe -Force }
  if (-not (Test-Path $exe)) { throw "Test-Exe nicht gefunden: $exe" }

  if ($SkipRun) {
    Write-Host "Build ok (Testausfuehrung uebersprungen)." -ForegroundColor Green
    return
  }

  $log = Join-Path $repoRoot "UnitTests\fpc\fpc-run.log"
  & $exe --all --format=plain --skiptiming 2>&1 | Tee-Object -FilePath $log | Out-Null

  $run      = (Select-String -Path $log -Pattern 'Number of run tests:\s*(\d+)'  | Select-Object -First 1)
  $errCount = (Select-String -Path $log -Pattern 'Number of errors:\s*(\d+)'     | Select-Object -First 1)
  $failed   = (Select-String -Path $log -Pattern 'Number of failures:\s*(\d+)'   | Select-Object -First 1)

  $r = if ($run)      { [int]$run.Matches[0].Groups[1].Value }      else { 0 }
  $e = if ($errCount) { [int]$errCount.Matches[0].Groups[1].Value } else { 0 }
  $f = if ($failed)   { [int]$failed.Matches[0].Groups[1].Value }   else { 0 }

  Write-Host ("FPC: {0} Tests, {1} Errors, {2} Failures" -f $r, $e, $f)

  if ($r -eq 0) { throw "FPC-Lauf hat keine Tests ausgefuehrt (siehe $log)." }
  if (($e -gt 0) -or ($f -gt 0)) {
    Select-String -Path $log -Pattern 'Message:' | Select-Object -First 20 |
      ForEach-Object { Write-Host $_.Line.Trim() -ForegroundColor Red }
    throw "FPC-Tests fehlgeschlagen (Details in $log)."
  }
  Write-Host "FPC-Gate gruen." -ForegroundColor Green
}
finally {
  Pop-Location
}
