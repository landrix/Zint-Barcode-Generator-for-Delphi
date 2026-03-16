[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=5V8N3XFTU495G)

# Zint-Barcode-Generator-for-Delphi

Zint Barcode Generator

Delphi port of http://github.com/zint/zint

## History

 * 25.02.2020 Girocode-Generator EPC-QR

## Quickstart

Most common local commands:

```powershell
# 1) Build + run full DUnitX tests (Win32 Debug)
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1

# 2) Build only (skip test execution)
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1 -SkipRun

# 3) Fast QR/rMQR-focused regression gate
UnitTests\run_qr_regression.bat
```

## QR Parity Notes (Delphi vs C)

Current status after the structured append AV fix and QR test alignment:

- Full Win32 DUnitX suite is green (all tests pass).
- Some QR expectations intentionally track current Delphi behavior where it differs from upstream C.
- In selected QR optimize Unicode cases, Delphi emits `ZINT_WARN_USES_ECI` (3) where C emits `ZINT_WARN_NONCOMPLIANT` (4).
- `ZBarcode_Encode` in Delphi does not yet populate `content_segs` like C does in equivalent RT/content tests.
- GS1 warning precedence for QR with Structured Append/ECI was aligned with C intent (explicit ECI precedence, then Structured Append).

If strict C parity is required later, these deltas are good candidates for targeted encoder work.

## QR/rMQR Regression Gate

Use this when you want a quick regression check focused on QR-related fixtures (`Test_QR.*`, including rMQR):

```bat
UnitTests\run_qr_regression.bat
```

The script:
- Builds `UnitTests\DUnitXCmdTest.dproj` (Win32 Debug) if the test exe is missing.
- Runs `UnitTests\bin\Win32_Debug\DUnitXCmdTest.exe`.
- Fails with exit code `1` if any `Test_QR.*` test fails or errors.
- Writes full output to `UnitTests\bin\Win32_Debug\qr_regression.log`.

PowerShell variant (optional strict mode to also fail on non-QR failures):

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\UnitTests\run_qr_regression.ps1 -BuildIfMissing -FailOnAnyFailure
```

## Reusable Delphi Build Script

For regular local build and test runs, use the reusable script in scripts:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1
```

Batch wrapper:

```bat
scripts\build-delphi-tests.bat
```

Defaults:
- StudioRoot: `C:\Program Files (x86)\Embarcadero\Studio\37.0`
- Config: `Debug`
- Platform: `Win32`
- ProjectRelativePath: `UnitTests\DUnitXCmdTest.dproj`

Common examples:

```powershell
# Build + run tests (Win64 Release)
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1 -Platform Win64 -Config Release

# Build only (skip test execution)
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\build-delphi-tests.ps1 -SkipRun
```

## AV Isolation Workflow (Win32)

To isolate teardown `EAccessViolation` by test unit, run:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\isolate-win32-av.ps1
```

You can limit the run to specific units:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\isolate-win32-av.ps1 -Units Test_QR,Test_2of5
```

What this script does:
- Temporarily rewrites `UnitTests\DUnitXCmdTest.dpr` to include one selected unit at a time.
- Builds with `-SkipRun`, runs the produced exe, and records summary + AV signal.
- Restores the original `DUnitXCmdTest.dpr` automatically in `finally`.

## Win64 Non-Blocking CI Route

For transition phases where Win64 parity is still being aligned, use:

```bat
scripts\ci-win64-nonblocking.bat
```

or directly:

```powershell
powershell -NoProfile -ExecutionPolicy Bypass -File .\scripts\ci-win64-nonblocking.ps1
```

Behavior:
- Executes the regular Win64 build/test path (`build-delphi-tests.ps1 -Platform Win64`).
- Always exits with code `0` (non-blocking), but still prints failures/warnings for visibility.
