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
$lines = & $testExe 2>&1
$lines | Set-Content -Path $logPath -Encoding UTF8

$summary = $lines | Select-String -Pattern 'Tests Found|Tests Passed|Tests Failed|Tests Errored'
$summary | ForEach-Object { Write-Host $_.Line }

$qrFailures = $lines | Select-String -Pattern 'Test Failed : Test_QR\.|Test Errored : Test_QR\.'
if ($qrFailures) {
    Write-Host ''
    Write-Host 'QR/rMQR regression failures:'
    $qrFailures | ForEach-Object { Write-Host $_.Line }
    Write-Host "Full log: $logPath"
    exit 1
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
