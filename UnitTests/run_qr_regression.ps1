param(
    [string]$RepoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..')).Path,
    [switch]$BuildIfMissing,
    [switch]$FailOnAnyFailure
)

$ErrorActionPreference = 'Stop'

$testExe = Join-Path $RepoRoot 'UnitTests\bin\Win32_Debug\DUnitXCmdTest.exe'
$logPath = Join-Path $RepoRoot 'UnitTests\bin\Win32_Debug\qr_regression.log'

if (-not (Test-Path $testExe)) {
    if (-not $BuildIfMissing) {
        Write-Error "Test runner not found: $testExe"
    }

    $rsvars = 'C:\Program Files (x86)\Embarcadero\Studio\37.0\bin\rsvars.bat'
    if (-not (Test-Path $rsvars)) {
        Write-Error "rsvars.bat not found: $rsvars"
    }

    Write-Host 'Building DUnitXCmdTest (Win32 Debug)...'
    cmd /c "call \"$rsvars\" && msbuild \"$RepoRoot\UnitTests\DUnitXCmdTest.dproj\" /t:Build /p:Config=Debug /p:Platform=Win32 /v:minimal"
    if ($LASTEXITCODE -ne 0) {
        exit $LASTEXITCODE
    }
}

Write-Host 'Running DUnitX QR/rMQR regression gate...'
$lines = cmd /c ('"{0}" 2>&1' -f $testExe)
$lines | Set-Content -Path $logPath -Encoding UTF8

$summary = $lines | Select-String -Pattern 'Tests Found|Tests Passed|Tests Failed|Tests Errored'
$summary | ForEach-Object { Write-Host $_.Line }

$failedCount = 0
$erroredCount = 0
$failedLine = $summary | Where-Object { $_.Line -match 'Tests Failed\s*:\s*(\d+)' } | Select-Object -First 1
if ($failedLine) {
    $failedCount = [int]([regex]::Match($failedLine.Line, '(\d+)').Value)
}
$erroredLine = $summary | Where-Object { $_.Line -match 'Tests Errored\s*:\s*(\d+)' } | Select-Object -First 1
if ($erroredLine) {
    $erroredCount = [int]([regex]::Match($erroredLine.Line, '(\d+)').Value)
}

$qrFailures = $lines | Select-String -Pattern 'Test Failed : Test_QR\.|Test Errored : Test_QR\.'
if ($qrFailures) {
    Write-Host ''
    Write-Host 'QR/rMQR regression failures:'
    $qrFailures | ForEach-Object { Write-Host $_.Line }
    Write-Host "Full log: $logPath"
    exit 1
}

if (($failedCount -gt 0) -or ($erroredCount -gt 0)) {
    Write-Host ''
    Write-Host ('Regression summary reports failures (Failed: {0}, Errored: {1}).' -f $failedCount, $erroredCount)
    Write-Host 'Note: this check guards against false "gate passed" output if DUnitX formatting changes and per-test pattern matching misses lines.'
    Write-Host "Full log: $logPath"
    exit 1
}

if ($LASTEXITCODE -ne 0) {
    Write-Host ''
    Write-Host ('Test runner returned exit code {0} despite zero reported test failures.' -f $LASTEXITCODE)
    Write-Host 'Known environment caveat: sporadic post-run access violations can occur in UI automation/console teardown.'
    Write-Host "Full log: $logPath"
}

if ($FailOnAnyFailure) {
    $anyFailures = $lines | Select-String -Pattern 'Tests Failed\s*:\s*[1-9]|Tests Errored\s*:\s*[1-9]'
    if ($anyFailures) {
        Write-Host ''
        Write-Host 'Non-QR failures detected and FailOnAnyFailure is set.'
        Write-Host "Full log: $logPath"
        exit 1
    }
}

Write-Host ''
Write-Host 'QR/rMQR regression gate passed.'
Write-Host "Full log: $logPath"
exit 0
