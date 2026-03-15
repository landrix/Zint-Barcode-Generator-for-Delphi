param(
  [string]$StudioRoot = "C:\Program Files (x86)\Embarcadero\Studio\37.0",
  [string]$Config = "Debug",
  [string]$Platform = "Win32",
  [string[]]$Units = @(
    "Test_Code128",
    "Test_Telepen",
    "Test_Medical",
    "Test_Plessey",
    "Test_PZN",
    "Test_Postal",
    "Test_Code",
    "Test_2of5",
    "Test_Auspost",
    "Test_QR"
  )
)

$ErrorActionPreference = "Stop"

$repoRoot = Split-Path -Parent $PSScriptRoot
$dprPath = Join-Path $repoRoot "UnitTests\DUnitXCmdTest.dpr"
$buildScript = Join-Path $repoRoot "scripts\build-delphi-tests.ps1"
$exePath = Join-Path $repoRoot ("UnitTests\bin\{0}_{1}\DUnitXCmdTest.exe" -f $Platform, $Config)

if (-not (Test-Path $dprPath)) {
  throw "DPR not found: $dprPath"
}
if (-not (Test-Path $buildScript)) {
  throw "Build script not found: $buildScript"
}

function Set-RunnerUnits {
  param(
    [string]$DprContent,
    [string[]]$SelectedUnits
  )

  if (-not $SelectedUnits -or $SelectedUnits.Count -eq 0) {
    throw "No test units selected."
  }

  $unitsBlockLines = @("  TestHelper_Zint,")
  for ($i = 0; $i -lt $SelectedUnits.Count; $i++) {
    if ($i -lt $SelectedUnits.Count - 1) {
      $unitsBlockLines += ("  {0}," -f $SelectedUnits[$i])
    } else {
      $unitsBlockLines += ("  {0}" -f $SelectedUnits[$i])
    }
  }
  $unitsBlockLines += "  ;"
  $unitsBlock = ($unitsBlockLines -join "`r`n")

  $pattern = '(?s)\s*TestHelper_Zint,\r?\n.*?\r?\n\s*;'
  $updated = [regex]::Replace($DprContent, $pattern, "`r`n$unitsBlock")

  if ($updated -eq $DprContent) {
    throw "Could not replace test unit block in DUnitXCmdTest.dpr"
  }

  return $updated
}

$originalDpr = Get-Content $dprPath -Raw
$results = @()

try {
  foreach ($unit in $Units) {
    Write-Host ""
    Write-Host ("=== Isolating with unit: {0} ===" -f $unit)

    $newDpr = Set-RunnerUnits -DprContent $originalDpr -SelectedUnits @($unit)
    Set-Content -Path $dprPath -Value $newDpr -Encoding UTF8

    & $buildScript -StudioRoot $StudioRoot -Config $Config -Platform $Platform -SkipRun

    if (-not (Test-Path $exePath)) {
      throw "Test executable not found after build: $exePath"
    }

    $runOutput = cmd /c ('"{0}" 2>&1' -f $exePath)
    $exitCode = $LASTEXITCODE

    $foundLine = $runOutput | Select-String -Pattern 'Tests Found\s*:\s*(\d+)' | Select-Object -First 1
    $failedLine = $runOutput | Select-String -Pattern 'Tests Failed\s*:\s*(\d+)' | Select-Object -First 1
    $erroredLine = $runOutput | Select-String -Pattern 'Tests Errored\s*:\s*(\d+)' | Select-Object -First 1

    $testsFound = if ($foundLine) { [int]([regex]::Match($foundLine.Line, '(\d+)').Value) } else { 0 }
    $failed = if ($failedLine) { [int]([regex]::Match($failedLine.Line, '(\d+)').Value) } else { 0 }
    $errored = if ($erroredLine) { [int]([regex]::Match($erroredLine.Line, '(\d+)').Value) } else { 0 }
    $hasAv = [bool]($runOutput | Select-String -Pattern 'EAccessViolation|AccessViolation' -Quiet)

    $results += [pscustomobject]@{
      Unit = $unit
      TestsFound = $testsFound
      Failed = $failed
      Errored = $errored
      ExitCode = $exitCode
      AccessViolation = $hasAv
    }

    $runOutput | Select-Object -Last 25 | ForEach-Object { Write-Host $_ }
    Write-Host ("Result: unit={0}, tests={1}, failed={2}, errored={3}, exit={4}, av={5}" -f $unit, $testsFound, $failed, $errored, $exitCode, $hasAv)
  }
}
finally {
  Set-Content -Path $dprPath -Value $originalDpr -Encoding UTF8
}

Write-Host ""
Write-Host "=== Isolation Summary ==="
$results | Format-Table -AutoSize

$avUnits = $results | Where-Object { $_.AccessViolation }
if ($avUnits.Count -gt 0) {
  Write-Host ""
  Write-Host "Units with AccessViolation signal:"
  $avUnits | ForEach-Object { Write-Host ("- {0}" -f $_.Unit) }
}
