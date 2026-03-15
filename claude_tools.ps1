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

# --- FIX ENCODING HERE ---
# Temporarily set the console to expect UTF-8 from external executables like 'dotnet'
$originalConsoleEncoding = [Console]::OutputEncoding
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8

switch ($Action) {
    "Build" {
        if ([string]::IsNullOrWhiteSpace($SourceProject)) {
            Write-Host "Building the entire XParsec solution..." -ForegroundColor Cyan
            Write-Host "Full output being saved to $LogFile..." -ForegroundColor DarkGray
            
            dotnet build 2>&1 | Tee-Object -FilePath $LogFile
        }
        else {
            $BuildPath = "src/$SourceProject"
            Write-Host "Building project $SourceProject..." -ForegroundColor Cyan
            Write-Host "Full output being saved to $LogFile..." -ForegroundColor DarkGray
            
            dotnet build $BuildPath 2>&1 | Tee-Object -FilePath $LogFile
        }
    }

    "Test" {
        if ([string]::IsNullOrWhiteSpace($TestProject)) {
            Write-Error "You must specify a -TestProject when using the Test action."
            exit 1
        }
        
        # Set the environment variable for the dotnet process if the switch is present
        if ($UpdateSnapshots) {
            Write-Host "Updating snapshots (UPDATE_SNAPSHOTS=1)..." -ForegroundColor Magenta
            $env:UPDATE_SNAPSHOTS = "1"
        }

        $TestPath = "test/$TestProject"
        Write-Host "Running tests for $TestProject..." -ForegroundColor Cyan
        Write-Host "Full output saved to $LogFile. Showing last $SummaryLines lines..." -ForegroundColor DarkGray

        # Run the tests, tee the FULL output to the log file, then capture for console truncation
        $testOutput = dotnet test $TestPath 2>&1 | Tee-Object -FilePath $LogFile
        
        if ($testOutput.Count -gt $SummaryLines) {
            Write-Host "... [Output Truncated - Read $LogFile for full details] ..." -ForegroundColor Yellow
            $testOutput | Select-Object -Last $SummaryLines
        }
        else {
            $testOutput
        }
    }

    "Format" {
        Write-Host "Running Fantomas to format all F# code..." -ForegroundColor Cyan
        dotnet fantomas . 2>&1 | Tee-Object -FilePath $LogFile
    }
}
        
# Restore the original encoding
[Console]::OutputEncoding = $originalConsoleEncoding
