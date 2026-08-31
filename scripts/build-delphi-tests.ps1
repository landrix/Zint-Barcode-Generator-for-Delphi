param(
  [string]$StudioRoot = "C:\Program Files (x86)\Embarcadero\Studio\37.0",
  [string]$Config = "Debug",
  [string]$Platform = "Win32",
  [string]$ProjectRelativePath = "UnitTests\DUnitXCmdTest.dproj",
  [string]$TestExeRelativePath = "",
  [int]$MinTests = 941,
  [int]$MaxIgnored = 0,
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
  $passedLine  = $testOutput | Select-String -Pattern 'Tests Passed\s*:\s*(\d+)'  | Select-Object -First 1
  $ignoredLine = $testOutput | Select-String -Pattern 'Tests Ignored\s*:\s*(\d+)' | Select-Object -First 1
  $failedLine  = $testOutput | Select-String -Pattern 'Tests Failed\s*:\s*(\d+)'  | Select-Object -First 1
  $erroredLine = $testOutput | Select-String -Pattern 'Tests Errored\s*:\s*(\d+)' | Select-Object -First 1

  # Eine fehlende Zusammenfassung darf nicht als "null Fehler" durchgehen: stuerzt
  # die Test-Exe vor der Summary ab, gaebe es sonst kein einziges Fehlersignal.
  if (-not $foundLine -or -not $passedLine -or -not $ignoredLine -or -not $failedLine -or -not $erroredLine) {
    throw ("Test summary not found or incomplete in runner output (exit code {0}). The test executable probably crashed before finishing." -f $runExit)
  }

  $foundCount   = [int]([regex]::Match($foundLine.Line,   '(\d+)').Value)
  $passedCount  = [int]([regex]::Match($passedLine.Line,  '(\d+)').Value)
  $ignoredCount = [int]([regex]::Match($ignoredLine.Line, '(\d+)').Value)
  $failedCount  = [int]([regex]::Match($failedLine.Line,  '(\d+)').Value)
  $erroredCount = [int]([regex]::Match($erroredLine.Line, '(\d+)').Value)

  if (($failedCount -gt 0) -or ($erroredCount -gt 0)) {
    throw "Test execution failed."
  }

  # "Found" heisst nur "entdeckt". Ignorierte Tests zaehlen mit, laufen aber nicht.
  # Gemessen wird daher an den tatsaechlich ausgefuehrten Tests - sonst genuegte
  # ein [Ignore], um das Gate gruen zu halten, ohne dass etwas geprueft wird.
  # Das FPC-Gate misst aus demselben Grund an "Number of run tests".
  $executedCount = $passedCount + $failedCount + $erroredCount

  # Der Ausweg muss echt sein: die Meldung darf nicht auf eine Moeglichkeit
  # verweisen, die es im Code nicht gibt. Wer bewusst ignoriert, hebt
  # -MaxIgnored an und senkt -MinTests - beides sichtbar im Aufruf.
  if ($ignoredCount -gt $MaxIgnored) {
    throw ("{0} test(s) were ignored (allowed: {1}). Ignored tests do not verify anything - remove the [Ignore], or document the reason and raise -MaxIgnored together with lowering -MinTests." -f $ignoredCount, $MaxIgnored)
  }

  if ($executedCount -lt $MinTests) {
    throw ("Only {0} tests executed (found {1}, ignored {2}), expected at least {3}. Tests have disappeared - check fixture registration. (If the reduction is intended, adjust -MinTests.)" -f $executedCount, $foundCount, $ignoredCount, $MinTests)
  }

  Write-Host ("Delphi: {0} executed, {1} passed, {2} failed, {3} errored, {4} ignored (expected at least {5} executed)." -f $executedCount, $passedCount, $failedCount, $erroredCount, $ignoredCount, $MinTests)

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
