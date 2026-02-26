@echo off
REM Secure proxy to the PowerShell script so Claude Code can safely whitelist this file
pwsh -NoProfile -ExecutionPolicy Bypass -File "%~dp0claude_tools.ps1" %*