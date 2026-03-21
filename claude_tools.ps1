<#
.SYNOPSIS
    Secure tool wrapper for Claude Code to interact with the XParsec repository.
#>

param(
    [Parameter(Mandatory = $true)]
    [ValidateSet("Build", "Test", "Format")]
    [string]$Action,

    [Parameter(Mandatory = $false)]
    [ValidateSet(
        "XParsec.C.Tests",
        "XParsec.CLArgs.Interactive",
        "XParsec.CLArgs.Tests",
        "XParsec.FSharp.Lexer.Tests",
        "XParsec.FSharp.Tests",
        "XParsec.Json.Tests",
        "XParsec.MessagePack.Tests",
        "XParsec.Tests"
    )]
    [string]$TestProject,

    [Parameter(Mandatory = $false)]
    [ValidateSet(
        "XParsec",
        "XParsec.CLArgs",
        "XParsec.FSharp",
        "XParsec.Json"
    )]
    [string]$SourceProject,

    [Parameter(Mandatory = $false)]
    [int]$SummaryLines = 30,

    [Parameter(Mandatory = $false)]
    [switch]$UpdateSnapshots
)

$ErrorActionPreference = "Stop"
$LogFile = "claude_tools_output.log"

# Clear previous run logs
if (Test-Path $LogFile) { Clear-Content $LogFile }

# Temporarily set the console to expect UTF-8 from external executables like 'dotnet'
$originalConsoleEncoding = [Console]::OutputEncoding
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

try {
    switch ($Action) {
        "Build" {
            if ([string]::IsNullOrWhiteSpace($SourceProject)) {
                Write-Host "Building the entire XParsec solution..." -ForegroundColor Cyan
                $buildOutput = dotnet build 2>&1
            }
            else {
                $BuildPath = "src/$SourceProject"
                Write-Host "Building project $SourceProject..." -ForegroundColor Cyan
                $buildOutput = dotnet build $BuildPath 2>&1
            }

            # Save full output to log
            $buildOutput | Out-File -FilePath $LogFile -Encoding utf8

            # Show only errors/warnings and the final summary
            $buildErrors = $buildOutput | Where-Object { "$_" -match ':\s*(error|warning)\s+\w' }
            $buildSummary = $buildOutput | Where-Object { "$_" -match '(Build succeeded|Build FAILED|Error\(s\)|Warning\(s\)|Time Elapsed)' }

            if ($buildErrors) {
                Write-Host "Full output saved to $LogFile." -ForegroundColor DarkGray
                $buildErrors
            }
            $buildSummary

            # Propagate exit code
            if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        }

        "Test" {
            if ([string]::IsNullOrWhiteSpace($TestProject)) {
                Write-Error "You must specify a -TestProject when using the Test action."
                exit 1
            }

            # Explicitly set or clear the env var to prevent leaking from prior runs
            if ($UpdateSnapshots) {
                Write-Host "Updating snapshots (UPDATE_SNAPSHOTS=1)..." -ForegroundColor Magenta
                $env:UPDATE_SNAPSHOTS = "1"
            }
            else {
                $env:UPDATE_SNAPSHOTS = $null
            }

            $TestPath = "test/$TestProject"
            Write-Host "Running tests for $TestProject..." -ForegroundColor Cyan

            $testOutput = dotnet test $TestPath 2>&1
            $testExitCode = $LASTEXITCODE

            # Save full output to log
            $testOutput | Out-File -FilePath $LogFile -Encoding utf8

            # Filter out noise lines (Skipped tests, build restore lines, blank lines)
            $filtered = $testOutput | Where-Object {
                $line = "$_"
                -not ($line -match '^\s*Skipped\s') -and
                -not ($line -match '^\s*Determining projects to restore') -and
                -not ($line -match '^\s*All projects are up-to-date') -and
                -not ($line -match '^\s*$')
            }

            if ($filtered.Count -gt $SummaryLines) {
                Write-Host "Full output saved to $LogFile." -ForegroundColor DarkGray
                $filtered | Select-Object -Last $SummaryLines
            }
            else {
                $filtered
            }

            # Propagate exit code
            if ($testExitCode -ne 0) { exit $testExitCode }
        }

        "Format" {
            Write-Host "Running Fantomas to format all F# code..." -ForegroundColor Cyan
            dotnet fantomas . 2>&1 | Tee-Object -FilePath $LogFile
            if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        }
    }
}
finally {
    # Restore the original encoding even if the script errors
    [Console]::OutputEncoding = $originalConsoleEncoding
}
