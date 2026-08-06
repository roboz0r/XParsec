<#
.SYNOPSIS
    Reports comment density for F# sources (.fs / .fsi) under the current directory.

.DESCRIPTION
    Classifies every line of every F# file as code, comment or blank, and reports the
    code:comment ratio per file, worst first.

    Comments are found with an F#-aware scan rather than a regex, so `//` inside a string
    literal is not a comment, `(*)` is the multiplication operator rather than a block
    comment, and `(* … *)` nests. Regular, verbatim (`@"…"`) and triple-quoted strings are
    all tracked across line boundaries.

    Two numbers matter, and both are reported:
      * RATIO  — code lines per comment line. The house target is 6:1.
      * BLOCKS — consecutive comment lines. A block of 3+ lines should be rare; a comment
                 longer than the code it describes usually means an invariant that belongs
                 in the type system is being argued in prose instead.

.PARAMETER Path
    Directory to scan. Defaults to the current directory.

.PARAMETER Threshold
    Target code:comment ratio. Files below it are marked. Default 6.

.PARAMETER MinBlock
    Comment-block length considered too long. Default 3.

.PARAMETER Top
    Show only the N worst files. Default: show all.

.PARAMETER Detail
    List every comment block of MinBlock+ lines, with file and line number.

.PARAMETER Exclude
    Regex of paths to skip. Default skips bin/obj output directories.

.PARAMETER Csv
    Also write the per-file table to this path as CSV.

.EXAMPLE
    ./comment-density.ps1
    Every F# file under the current directory, worst ratio first.

.EXAMPLE
    ./comment-density.ps1 -Path src/XParsec.FSharp.SemanticAnalysis -Top 10 -Detail
    The ten densest files in one project, plus their long comment blocks.
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $false, Position = 0)]
    [string]$Path = ".",

    [Parameter(Mandatory = $false)]
    [double]$Threshold = 6.0,

    [Parameter(Mandatory = $false)]
    [int]$MinBlock = 3,

    [Parameter(Mandatory = $false)]
    [int]$Top = 0,

    [Parameter(Mandatory = $false)]
    [switch]$Detail,

    [Parameter(Mandatory = $false)]
    [switch]$PassThru,

    [Parameter(Mandatory = $false)]
    [string]$Exclude = '[\\/](bin|obj|node_modules|dist)[\\/]',

    [Parameter(Mandatory = $false)]
    [string]$Csv
)

$ErrorActionPreference = "Stop"

# One file's lines classified as Code / Comment / Blank, with the comment blocks located.
# The scanner carries string and block-comment state across lines, so a `//` inside a
# multi-line string literal cannot be mistaken for a comment.
function Measure-FSharpFile {
    param([string]$FilePath)

    $lines = [System.IO.File]::ReadAllLines($FilePath)

    $code = 0
    $comment = 0
    $blank = 0
    $trailing = 0

    $blockDepth = 0
    $inString = $false
    $inVerbatim = $false
    $inTriple = $false

    $blocks = [System.Collections.Generic.List[object]]::new()
    $runStart = 0
    $runLength = 0

    for ($n = 0; $n -lt $lines.Length; $n++) {
        $line = $lines[$n]
        $len = $line.Length
        $i = 0

        # A line that opens inside a block comment is comment; one that opens inside a
        # multi-line string literal is code, whatever it happens to contain.
        $sawComment = $blockDepth -gt 0
        $sawCode = $inString -or $inVerbatim -or $inTriple

        while ($i -lt $len) {
            $c = $line[$i]
            $next = if ($i + 1 -lt $len) { $line[$i + 1] } else { [char]0 }

            if ($blockDepth -gt 0) {
                if ($c -eq '*' -and $next -eq ')') { $blockDepth--; $i += 2; continue }
                if ($c -eq '(' -and $next -eq '*') { $blockDepth++; $i += 2; continue }
                $i++
                continue
            }

            if ($inTriple) {
                if ($c -eq '"' -and $next -eq '"' -and $i + 2 -lt $len -and $line[$i + 2] -eq '"') {
                    $inTriple = $false; $i += 3; continue
                }
                $i++
                continue
            }

            if ($inVerbatim) {
                if ($c -eq '"') {
                    # `""` is an escaped quote inside a verbatim string, not its end.
                    if ($next -eq '"') { $i += 2; continue }
                    $inVerbatim = $false
                }
                $i++
                continue
            }

            if ($inString) {
                if ($c -eq '\') { $i += 2; continue }
                if ($c -eq '"') { $inString = $false }
                $i++
                continue
            }

            if ($c -eq '/' -and $next -eq '/') {
                $sawComment = $true
                break
            }

            if ($c -eq '(' -and $next -eq '*') {
                # `(*)` is the multiplication operator, not an unterminated block comment.
                if ($i + 2 -lt $len -and $line[$i + 2] -eq ')') { $sawCode = $true; $i += 3; continue }
                $sawComment = $true
                $blockDepth++
                $i += 2
                continue
            }

            if ($c -eq '"') {
                $sawCode = $true
                if ($next -eq '"' -and $i + 2 -lt $len -and $line[$i + 2] -eq '"') {
                    $inTriple = $true; $i += 3; continue
                }
                $inString = $true
                $i++
                continue
            }

            if ($c -eq '@' -and $next -eq '"') {
                $sawCode = $true; $inVerbatim = $true; $i += 2; continue
            }

            if ($c -eq "'") {
                # A char literal (`'x'`, `'\n'`) versus a typar tick (`'T`).
                if ($next -eq '\') { $i += 2; while ($i -lt $len -and $line[$i] -ne "'") { $i++ }; $i++ }
                elseif ($i + 2 -lt $len -and $line[$i + 2] -eq "'") { $i += 3 }
                else { $i++ }
                $sawCode = $true
                continue
            }

            if (-not [char]::IsWhiteSpace($c)) { $sawCode = $true }
            $i++
        }

        $isCommentLine = (-not $sawCode) -and $sawComment

        if ($sawCode) {
            $code++
            if ($sawComment) { $trailing++ }
        }
        elseif ($isCommentLine) {
            $comment++
        }
        else {
            $blank++
        }

        if ($isCommentLine -and $line.Trim().Length -gt 0) {
            if ($runLength -eq 0) { $runStart = $n + 1 }
            $runLength++
        }
        else {
            if ($runLength -ge $MinBlock) {
                $blocks.Add([pscustomobject]@{ Line = $runStart; Length = $runLength })
            }
            $runLength = 0
        }
    }

    if ($runLength -ge $MinBlock) {
        $blocks.Add([pscustomobject]@{ Line = $runStart; Length = $runLength })
    }

    $ratio = if ($comment -eq 0) { [double]::PositiveInfinity } else { $code / $comment }
    $maxBlock = if ($blocks.Count -eq 0) { 0 } else { [int]($blocks | Measure-Object -Property Length -Maximum).Maximum }

    [pscustomobject]@{
        File      = $FilePath
        Code      = $code
        Comment   = $comment
        Blank     = $blank
        Ratio     = $ratio
        Trailing  = $trailing
        LongBlocks = $blocks.Count
        MaxBlock  = $maxBlock
        Blocks    = $blocks
    }
}

$target = (Resolve-Path -Path $Path).Path

# A single file is reported relative to its own directory, a directory relative to itself.
if (Test-Path -Path $target -PathType Leaf) {
    $root = Split-Path -Path $target -Parent
    $files = @(Get-Item -Path $target)
}
else {
    $root = $target
    $files =
        @(Get-ChildItem -Path $root -Recurse -File -Include *.fs, *.fsi |
          Where-Object { $_.FullName -notmatch $Exclude } |
          Sort-Object FullName)
}

if ($files.Count -eq 0) {
    Write-Host "No .fs or .fsi files found under $root" -ForegroundColor Yellow
    return
}

$results = @(foreach ($f in $files) { Measure-FSharpFile -FilePath $f.FullName })

# Worst (densest) first: a lower code:comment ratio is more comment per unit of code.
$ranked = $results | Sort-Object Ratio

# Objects instead of a table, so the caller can filter, sort and group them.
if ($PassThru) {
    $ranked
    return
}
$shown = if ($Top -gt 0) { $ranked | Select-Object -First $Top } else { $ranked }

$fmt = "{0,-52} {1,6} {2,8} {3,9} {4,7} {5,6}"
Write-Host ""
Write-Host ($fmt -f "file", "code", "comment", "ratio", "blocks", "max") -ForegroundColor Cyan
Write-Host ($fmt -f ("-" * 52), "------", "--------", "---------", "-------", "------") -ForegroundColor DarkGray

foreach ($r in $shown) {
    $rel = [System.IO.Path]::GetRelativePath($root, $r.File)
    if ($rel.Length -gt 52) { $rel = "..." + $rel.Substring($rel.Length - 49) }

    $ratioText = if ([double]::IsInfinity($r.Ratio)) { "  --  " } else { "{0,6:0.0}:1" -f $r.Ratio }
    $colour =
        if ([double]::IsInfinity($r.Ratio)) { "DarkGray" }
        elseif ($r.Ratio -lt $Threshold / 2) { "Red" }
        elseif ($r.Ratio -lt $Threshold) { "Yellow" }
        else { "Green" }

    Write-Host ($fmt -f $rel, $r.Code, $r.Comment, $ratioText, $r.LongBlocks, $r.MaxBlock) -ForegroundColor $colour
}

$totalCode = ($results | Measure-Object -Property Code -Sum).Sum
$totalComment = ($results | Measure-Object -Property Comment -Sum).Sum
$totalBlocks = ($results | Measure-Object -Property LongBlocks -Sum).Sum
$totalRatio = if ($totalComment -eq 0) { [double]::PositiveInfinity } else { $totalCode / $totalComment }
$below = @($results | Where-Object { $_.Ratio -lt $Threshold }).Count

Write-Host ""
Write-Host ("{0} files: {1} code, {2} comment, {3:0.0}:1 overall" -f $results.Count, $totalCode, $totalComment, $totalRatio)
Write-Host ("{0} below the {1}:1 target; {2} comment blocks of {3}+ lines" -f $below, $Threshold, $totalBlocks, $MinBlock)

if ($Detail) {
    Write-Host ""
    Write-Host "Comment blocks of $MinBlock+ lines:" -ForegroundColor Cyan

    foreach ($r in $shown) {
        if ($r.Blocks.Count -eq 0) { continue }
        $rel = [System.IO.Path]::GetRelativePath($root, $r.File)

        foreach ($b in ($r.Blocks | Sort-Object -Property Length -Descending)) {
            Write-Host ("  {0,-56} line {1,-6} {2} lines" -f $rel, $b.Line, $b.Length)
        }
    }
}

if ($Csv) {
    $results |
        Select-Object File, Code, Comment, Blank,
            @{ n = "Ratio"; e = { if ([double]::IsInfinity($_.Ratio)) { "" } else { "{0:0.00}" -f $_.Ratio } } },
            Trailing, LongBlocks, MaxBlock |
        Sort-Object Ratio |
        Export-Csv -Path $Csv -NoTypeInformation

    Write-Host ""
    Write-Host "Wrote $Csv"
}
