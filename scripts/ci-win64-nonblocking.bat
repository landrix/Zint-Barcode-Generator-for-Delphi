@echo off
setlocal
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0ci-win64-nonblocking.ps1" %*
exit /b 0
