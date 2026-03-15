@echo off
setlocal

set SCRIPT=%~dp0run_qr_regression.ps1
powershell -NoProfile -ExecutionPolicy Bypass -File "%SCRIPT%" -BuildIfMissing
exit /b %ERRORLEVEL%
