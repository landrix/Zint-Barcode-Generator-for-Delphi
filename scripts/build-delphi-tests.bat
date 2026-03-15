@echo off
setlocal
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0build-delphi-tests.ps1" %*
exit /b %ERRORLEVEL%
