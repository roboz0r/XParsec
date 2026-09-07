<#
.SYNOPSIS
    Secure tool wrapper for Claude Code to interact with the XParsec repository.
#>

param(
    [Parameter(Mandatory = $true)]
    [ValidateSet("Build", "Test", "Format", "Benchmark", "Fable")]
    [string]$Action,

    [Parameter(Mandatory = $false)]
    [ValidateSet(
        "XParsec.C.Tests",
        "XParsec.CLArgs.Interactive",
        "XParsec.CLArgs.Tests",
        "XParsec.FSharp.Codegen.Clr.Tests",
        "XParsec.FSharp.Codegen.Js.Tests",
        "XParsec.FSharp.Lexer.Tests",
        "XParsec.FSharp.SemanticAnalysis.Tests",
        "XParsec.FSharp.Tests",
        "XParsec.Json.Tests",
        "XParsec.MessagePack.Tests",
        "XParsec.Tests",
        "XParsec.Toml.Tests",
        "Vesper.Tests",
        "Vesper.Block.Tests",
        "Vesper.Ts.Extractor.Tests",
        "Vesper.UnionFind.Tests"
    )]
    [string]$TestProject,

    [Parameter(Mandatory = $false)]
    [ValidateSet(
        "XParsec",
        "XParsec.CLArgs",
        "XParsec.FSharp",
        "XParsec.FSharp.Codegen.Clr",
        "XParsec.FSharp.Codegen.Common",
        "XParsec.FSharp.Codegen.Js",
        "XParsec.FSharp.SemanticAnalysis",
        "XParsec.Json",
        "XParsec.Toml",
        "Vesper.Block",
        "Vesper.Ts.Manifest.Schema",
        "Vesper.Ts.Extractor",
        "Vesper.UnionFind"
    )]
    [string]$SourceProject,

    [Parameter(Mandatory = $false)]
    [int]$SummaryLines = 30,

    [Parameter(Mandatory = $false)]
    [switch]$UpdateSnapshots,

    [Parameter(Mandatory = $false)]
    [string]$Filter,

    # BenchmarkDotNet profiler (EP = EventPipe sampling; ETW = Windows-only event tracing).
    # When set, BDN writes .speedscope.json trace files alongside the regular report.
    [Parameter(Mandatory = $false)]
    [ValidateSet("EP", "ETW")]
    [string]$Profiler,

    # BenchmarkDotNet job. Pass Short only for a smoke run, Long for a final measurement.
    [Parameter(Mandatory = $false)]
    [ValidateSet("Dry", "Short", "Medium", "Long", "Default")]
    [string]$Job = "Default"
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

            if (-not [string]::IsNullOrWhiteSpace($Filter)) {
                Write-Host "Filters are not respected by tests. Use ftest in the source"
                $testArgs = @("test", $TestPath)
            }
            else {
                $testArgs = @("test", $TestPath)
            }
            $testOutput = & dotnet @testArgs 2>&1
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

        "Benchmark" {
            if ([string]::IsNullOrWhiteSpace($Filter)) {
                Write-Error "You must specify a -Filter when using the Benchmark action. Available benchmark names: EndToEndBenchmarks, LexingBenchmarks, ParsingBenchmarks. Wildcards are allowed (e.g. '*Lexing*')."
                exit 1
            }

            $BenchmarkProject = "bench/XParsec.FSharp.Benchmarks"
            Write-Host "Running benchmarks in $BenchmarkProject with filter '$Filter'..." -ForegroundColor Cyan

            # BDN's EventPipe/ETW profilers require an isolated (out-of-process) benchmark
            # runner, so drop `-i` when a profiler is requested. Profiling additionally
            # needs BDN >= 0.15.x (0.14.0 doesn't recognize net10.0 as a runtime and fails
            # validation) and FSharp.Core >= 10.1.202 centrally pinned (BDN regenerates a
            # csproj out-of-process and the VersionOverride in the bench fsproj is not
            # enough on its own).
            $benchArgs = @(
                "run",
                "--project", $BenchmarkProject,
                "-c", "Release",
                "--",
                "-j", $Job.ToLowerInvariant(),
                "--filter", $Filter
            )

            if (-not [string]::IsNullOrWhiteSpace($Profiler)) {
                Write-Host "Profiler enabled: $Profiler (speedscope traces will be emitted alongside the report)" -ForegroundColor Yellow
                $benchArgs += @("--profiler", $Profiler)
            }
            else {
                # No profiler → keep the fast in-process toolchain to sidestep the FCS restore gotcha.
                $benchArgs += "-i"
            }

            & dotnet @benchArgs 2>&1 | Tee-Object -FilePath $LogFile
            if ($LASTEXITCODE -ne 0) { exit $LASTEXITCODE }
        }

        "Fable" {
            # Vesper.Ts.Extractor is a Fable project: `-Action Build` only runs the
            # .NET/IDE pass, NOT the F#->JS compile. This produces the runnable
            # `dist/Program.js` the extractor exercises. `Vesper.Ts.Extractor.Tests`
            # SKIPS every extractor-run test until this has been run at least once, and
            # must be re-run whenever the extractor source changes.
            $proj = if ([string]::IsNullOrWhiteSpace($SourceProject)) { "Vesper.Ts.Extractor" } else { $SourceProject }
            $projPath = "src/$proj"
            $distPath = "$projPath/dist"
            Write-Host "Fable-compiling $proj to $distPath..." -ForegroundColor Cyan

            $fableOutput = dotnet fable $projPath -o $distPath 2>&1
            $fableExitCode = $LASTEXITCODE

            # Save full output to log
            $fableOutput | Out-File -FilePath $LogFile -Encoding utf8

            # Fable is chatty (per-file progress). Surface any error lines, then the tail
            # (which carries the "compilation finished" / timing or the failure summary);
            # the full transcript stays in the log.
            $fableErrors = $fableOutput | Where-Object { "$_" -match '(?i)\b(error|failed|exception)\b' }
            if ($fableErrors) {
                Write-Host "Full output saved to $LogFile." -ForegroundColor DarkGray
                $fableErrors
            }
            $fableOutput | Where-Object { "$_" -match '\S' } | Select-Object -Last 5

            # Propagate exit code
            if ($fableExitCode -ne 0) { exit $fableExitCode }
        }
    }
}
finally {
    # Restore the original encoding even if the script errors
    [Console]::OutputEncoding = $originalConsoleEncoding
}
