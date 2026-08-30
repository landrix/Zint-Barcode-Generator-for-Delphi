param(
  [string]$StudioRoot = "C:\Program Files (x86)\Embarcadero\Studio\37.0",
  [string]$Config = "Debug",
  [string]$Platform = "Win32",
  [string]$ProjectRelativePath = "UnitTests\DUnitXCmdTest.dproj",
  [string]$TestExeRelativePath = "",
  [int]$MinTests = 880,
  [switch]$SkipRun
)

$ErrorActionPreference = "Stop"

function Get-MsBuildPath {
  param([string]$StudioRoot)

  $candidates = @(
    "C:\Windows\Microsoft.NET\Framework64\v4.0.30319\MSBuild.exe",
    "C:\Windows\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe"
  )

  foreach ($candidate in $candidates) {
    if (Test-Path $candidate) {
      return $candidate
    }
  }

  # Fallback: Delphi tools can expose msbuild after rsvars.
  return "MSBuild.exe"
}

$repoRoot = Split-Path -Parent $PSScriptRoot
$rsvars = Join-Path $StudioRoot "bin\rsvars.bat"
$project = Join-Path $repoRoot $ProjectRelativePath
$testsDir = Split-Path -Parent $project

if ([string]::IsNullOrWhiteSpace($TestExeRelativePath)) {
  $TestExeRelativePath = "UnitTests\bin\{0}_{1}\DUnitXCmdTest.exe" -f $Platform, $Config
}
$exe = Join-Path $repoRoot $TestExeRelativePath
$msbuild = Get-MsBuildPath -StudioRoot $StudioRoot

if (-not (Test-Path $rsvars)) {
  throw "rsvars.bat not found: $rsvars"
}

if (-not (Test-Path $project)) {
  throw "Project not found: $project"
}

Push-Location $testsDir
try {
  # Short path avoids issues with parentheses in Program Files (x86).
  $studioShort = $StudioRoot.Replace("Program Files (x86)", "Progra~2")
  $rsvarsShort = Join-Path $studioShort "bin\rsvars.bat"
  if (-not (Test-Path $rsvarsShort)) {
    $rsvarsShort = $rsvars
  }

  $buildCmd = "call `"{0}`" && `"{1}`" DUnitXCmdTest.dproj /t:Build /p:Config={2} /p:Platform={3} /v:m" -f $rsvarsShort, $msbuild, $Config, $Platform
  cmd /c $buildCmd
  if ($LASTEXITCODE -ne 0) {
    throw "Build failed."
  }

  if ($SkipRun) {
    Write-Host "Build ok (test execution skipped)."
    return
  }

  if (-not (Test-Path $exe)) {
    throw "Test executable not found: $exe"
  }

  # Feed Enter to keep compatibility with console test runners that prompt.
  $testOutput = cmd /c "echo.|`"$exe`" 2>&1"
  $runExit = $LASTEXITCODE
  $testOutput | ForEach-Object { Write-Host $_ }

  $foundLine   = $testOutput | Select-String -Pattern 'Tests Found\s*:\s*(\d+)'   | Select-Object -First 1
  $failedLine  = $testOutput | Select-String -Pattern 'Tests Failed\s*:\s*(\d+)'  | Select-Object -First 1
  $erroredLine = $testOutput | Select-String -Pattern 'Tests Errored\s*:\s*(\d+)' | Select-Object -First 1

  # Eine fehlende Zusammenfassung darf nicht als "null Fehler" durchgehen: stuerzt
  # die Test-Exe vor der Summary ab, gaebe es sonst kein einziges Fehlersignal.
  if (-not $foundLine -or -not $failedLine -or -not $erroredLine) {
    throw ("Test summary not found in runner output (exit code {0}). The test executable probably crashed before finishing." -f $runExit)
  }

  $foundCount   = [int]([regex]::Match($foundLine.Line,   '(\d+)').Value)
  $failedCount  = [int]([regex]::Match($failedLine.Line,  '(\d+)').Value)
  $erroredCount = [int]([regex]::Match($erroredLine.Line, '(\d+)').Value)

  if (($failedCount -gt 0) -or ($erroredCount -gt 0)) {
    throw "Test execution failed."
  }

  # Verschwundene Tests sind ein Fehler, auch wenn kein einzelner fehlschlaegt.
  # Genau so blieb unbemerkt, dass DUnitX vier Fixtures still uebersprungen hat
  # (siehe docs/PORTING_WORKFLOW.md).
  if ($foundCount -lt $MinTests) {
    throw ("Only {0} tests found, expected at least {1}. Tests have disappeared - check fixture registration. (If the reduction is intended, adjust -MinTests.)" -f $foundCount, $MinTests)
  }

  Write-Host ("Delphi: {0} tests, {1} failed, {2} errored (expected at least {3})." -f $foundCount, $failedCount, $erroredCount, $MinTests)

  # Frueher wurde ein Nonzero-Exitcode hier nur als "known post-run AV caveat"
  # gewarnt. Dieser AV (Runtime error 216) stammte von TTestQR, das den Lauf
  # vorzeitig abbrach und dabei rund 60 Tests verschluckte - darunter die
  # gesamte Telepen-Suite. Seit TTestQR stillgelegt ist, endet der Runner mit
  # Exitcode 0. Ein Nonzero-Exitcode ist daher wieder ein echtes Fehlersignal.
  if ($runExit -ne 0) {
    throw ("Test runner exited with code {0} although the summary looks green. That indicates a crash during or after the run - do not ignore it (see docs/PORTING_WORKFLOW.md, section 3)." -f $runExit)
  }
}
finally {
  Pop-Location
}
