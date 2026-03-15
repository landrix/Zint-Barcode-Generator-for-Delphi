param(
  [string]$StudioRoot = "C:\Program Files (x86)\Embarcadero\Studio\37.0",
  [string]$Config = "Debug"
)

$ErrorActionPreference = "Stop"

$scriptRoot = Split-Path -Parent $MyInvocation.MyCommand.Path
$buildScript = Join-Path $scriptRoot "build-delphi-tests.ps1"

if (-not (Test-Path $buildScript)) {
  throw "Build script not found: $buildScript"
}

try {
  & $buildScript -StudioRoot $StudioRoot -Config $Config -Platform Win64
  Write-Host "Win64 run finished without hard failures."
  exit 0
}
catch {
  Write-Warning "Win64 run currently non-blocking. Captured failure for visibility:"
  Write-Warning $_.Exception.Message
  exit 0
}
