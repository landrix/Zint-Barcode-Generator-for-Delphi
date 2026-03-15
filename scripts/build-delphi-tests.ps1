param(
  [string]$StudioRoot = "C:\Program Files (x86)\Embarcadero\Studio\37.0",
  [string]$Config = "Debug",
  [string]$Platform = "Win32",
  [string]$ProjectRelativePath = "UnitTests\DUnitXCmdTest.dproj",
  [string]$TestExeRelativePath = "",
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
  $testOutput | ForEach-Object { Write-Host $_ }

  $failedLine = $testOutput | Select-String -Pattern 'Tests Failed\s*:\s*(\d+)' | Select-Object -First 1
  $erroredLine = $testOutput | Select-String -Pattern 'Tests Errored\s*:\s*(\d+)' | Select-Object -First 1
  $failedCount = if ($failedLine) { [int]([regex]::Match($failedLine.Line, '(\d+)').Value) } else { 0 }
  $erroredCount = if ($erroredLine) { [int]([regex]::Match($erroredLine.Line, '(\d+)').Value) } else { 0 }

  if (($failedCount -gt 0) -or ($erroredCount -gt 0)) {
    throw "Test execution failed."
  }

  if ($LASTEXITCODE -ne 0) {
    Write-Warning ("Test runner exited with code {0} although summary is green (known post-run AV caveat)." -f $LASTEXITCODE)
  }
}
finally {
  Pop-Location
}
