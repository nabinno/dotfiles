@echo off
powershell.exe -NoLogo -NoProfile -ExecutionPolicy Bypass -File "%APPDATA%\claude-profiles\launch.ps1" -Mode pro %*
