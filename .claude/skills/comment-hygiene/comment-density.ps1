<#
.SYNOPSIS
    Reports comment density for F# (.fs / .fsi / .fsx) and C# (.cs) sources.

.DESCRIPTION
    Classifies every line of every source file as code, comment or blank, and reports the
    code:comment ratio per file, worst first.

    Comments are found with a language-aware scan rather than a regex, so `//` inside a string
    literal is not a comment. The lexer switches on extension:
      * F# — `(* … *)` nests, `(*)` is the multiplication operator.
      * C# — `/* … */` does not nest, `$@"` / `@$"` open a verbatim string.
    Regular, verbatim (`@"…"`) and triple-quoted / raw (`"""…"""`) strings are tracked across
    line boundaries in both.

    Four numbers matter:
      * RATIO  — CODE lines per PROSE line. The house target is 6:1.
      * BLOCKS — consecutive prose lines. A block of 3+ lines should be rare; a comment
                 longer than the code it describes usually means an invariant that belongs
                 in the type system is being argued in prose instead.
      * PROSE  — comment lines that are not XML-doc scaffolding.
      * DELIM  — code lines holding nothing but `(){}[];,`. Excluded from CODE.

    PROSE and DELIM exist for the same reason: measuring C# on F#'s scale. Both ends of the
    ratio carry scaffolding that is not the thing being counted, and each language carries a
    different amount of it, so the raw numbers are not comparable.

    `/// <summary>` and `/// </summary>` carry no content, so an XML-documented member costs
    two comment lines before a word is written and a one-sentence summary already reads as a
    3-line block. Structural tag lines are counted separately and are transparent to block
    runs: they neither extend a block nor break one.

    A brace or a lone `)` is the same defect at the other end. C# puts far more of them on
    their own line than F# does, and a formatter that breaks one argument per line — csharpier
    is the case in hand — multiplies them. Counting those as code inflates the denominator and
    reports a genuinely comment-heavy C# file as comfortable. F# is measured the same way,
    which barely moves it: that is the evidence the correction is real rather than a thumb on
    the scale for one language.

    COMMENT (raw comment lines) and RawCode are kept on the object and in the CSV, so the
    unfiltered numbers are still available.

.PARAMETER Path
    Directory to scan. Defaults to the current directory.

.PARAMETER Language
    Restrict the scan to one language. `All` (default), `FSharp` or `CSharp`; `fs` and `cs`
    are accepted as aliases, and the whole set is case-insensitive.

.PARAMETER Threshold
    Target code:prose ratio. Files below it are marked. Default 6.

.PARAMETER MinBlock
    Comment-block length considered too long. Default 3.

.PARAMETER Top
    Show only the N worst files. Default: show all.

.PARAMETER Detail
    List every comment block of MinBlock+ lines, with file and line number.

.PARAMETER Exclude
    Regex of paths to skip. Default skips bin/obj output and generated C#.

.PARAMETER Csv
    Also write the per-file table to this path as CSV.

.EXAMPLE
    ./comment-density.ps1
    Every F# and C# file under the current directory, worst ratio first.

.EXAMPLE
    ./comment-density.ps1 -Path src/MyProject -Top 10 -Detail
    The ten densest files in one project, plus their long comment blocks.

.EXAMPLE
    ./comment-density.ps1 -Path src/MyProject -Language CSharp -Csv density.csv
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $false, Position = 0)]
    [string]$Path = ".",

    [Parameter(Mandatory = $false)]
    [ValidateSet("All", "FSharp", "CSharp", "fs", "cs")]
    [string]$Language = "All",

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
    [string]$Exclude = '[\\/](bin|obj|node_modules|fable_modules|packages|dist|\.fable)[\\/]|\.(g|designer|generated)\.cs$',

    [Parameter(Mandatory = $false)]
    [string]$Csv
)

$ErrorActionPreference = "Stop"

$Language =
    switch ($Language) {
        "fs" { "FSharp" }
        "cs" { "CSharp" }
        default { $Language }
    }

# A code line holding nothing but structure. Counted, but kept out of CODE — see .DESCRIPTION.
# Safe as a whole-line test: a line this shape can hold no string and no comment.
$DelimiterOnlyPattern = '^[(){}\[\];,]+$'

# A comment line holding only XML-doc structure. `<see cref="X"/>` and `<param name="x">Text`
# are content and do NOT match: the tag has to be structural AND alone on the line.
$TagOnlyPattern =
    '^(?:</?(?:summary|remarks|para|list|item|term|description|returns|value|example|code|' +
    'exception|typeparam|param|seealso|inheritdoc)\b[^>]*>\s*)+$'

# One file's lines classified as Code / Comment / Blank, with the prose blocks located.
# The scanner carries string and block-comment state across lines, so a `//` inside a
# multi-line string literal cannot be mistaken for a comment.
function Measure-SourceFile {
    param(
        [string]$FilePath,
        [ValidateSet("FSharp", "CSharp")][string]$Lang
    )

    $isFSharp = $Lang -eq "FSharp"
    $lines = [System.IO.File]::ReadAllLines($FilePath)

    $code = 0
    $delimiter = 0
    $comment = 0
    $prose = 0
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
                if ($isFSharp) {
                    if ($c -eq '*' -and $next -eq ')') { $blockDepth--; $i += 2; continue }
                    if ($c -eq '(' -and $next -eq '*') { $blockDepth++; $i += 2; continue }
                }
                elseif ($c -eq '*' -and $next -eq '/') { $blockDepth = 0; $i += 2; continue }
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

            if ($isFSharp) {
                if ($c -eq '(' -and $next -eq '*') {
                    # `(*)` is the multiplication operator, not an unterminated block comment.
                    if ($i + 2 -lt $len -and $line[$i + 2] -eq ')') { $sawCode = $true; $i += 3; continue }
                    $sawComment = $true
                    $blockDepth++
                    $i += 2
                    continue
                }
            }
            elseif ($c -eq '/' -and $next -eq '*') {
                $sawComment = $true
                $blockDepth = 1
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

            # C# `$@"…"` / `@$"…"`. A bare `$"…"` needs no case: `$` falls through to the
            # default and the quote opens an ordinary string on the next pass.
            if (-not $isFSharp -and (($c -eq '$' -and $next -eq '@') -or ($c -eq '@' -and $next -eq '$'))) {
                if ($i + 2 -lt $len -and $line[$i + 2] -eq '"') {
                    $sawCode = $true; $inVerbatim = $true; $i += 3; continue
                }
            }

            if ($c -eq "'") {
                # A char literal (`'x'`, `'\n'`) versus F#'s typar tick (`'T`).
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
            if ($line.Trim() -match $DelimiterOnlyPattern) { $delimiter++ } else { $code++ }
            if ($sawComment) { $trailing++ }
        }
        elseif ($isCommentLine) {
            $comment++
        }
        else {
            $blank++
        }

        if ($isCommentLine -and $line.Trim().Length -gt 0) {
            $body = $line.Trim() -replace '^(///|//|\*)\s*', ''
            $isTagOnly = $body.Length -gt 0 -and $body -match $TagOnlyPattern

            # Tag-only lines are transparent: they neither extend a block nor break one.
            if (-not $isTagOnly) {
                $prose++
                if ($runLength -eq 0) { $runStart = $n + 1 }
                $runLength++
            }
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

    $ratio = if ($prose -eq 0) { [double]::PositiveInfinity } else { $code / $prose }
    $maxBlock = if ($blocks.Count -eq 0) { 0 } else { [int]($blocks | Measure-Object -Property Length -Maximum).Maximum }

    [pscustomobject]@{
        File      = $FilePath
        Language  = $Lang
        Code      = $code
        Delimiter = $delimiter
        RawCode   = $code + $delimiter
        Comment   = $comment
        Prose     = $prose
        Blank     = $blank
        Ratio     = $ratio
        Trailing  = $trailing
        LongBlocks = $blocks.Count
        MaxBlock  = $maxBlock
        Blocks    = $blocks
    }
}

function Get-SourceLanguage {
    param([string]$Extension)
    switch ($Extension.ToLowerInvariant()) {
        ".fs" { "FSharp" }
        ".fsi" { "FSharp" }
        ".fsx" { "FSharp" }
        ".cs" { "CSharp" }
        default { $null }
    }
}

$extensions =
    switch ($Language) {
        "FSharp" { @("*.fs", "*.fsi", "*.fsx") }
        "CSharp" { @("*.cs") }
        default { @("*.fs", "*.fsi", "*.fsx", "*.cs") }
    }

$target = (Resolve-Path -Path $Path).Path

# A single file is reported relative to its own directory, a directory relative to itself.
if (Test-Path -Path $target -PathType Leaf) {
    $root = Split-Path -Path $target -Parent
    $files = @(Get-Item -Path $target)
}
else {
    $root = $target
    # -Unique: `Get-ChildItem -Recurse -Include` yields a file once per matching pattern.
    $files =
        @(Get-ChildItem -Path $root -Recurse -File -Include $extensions |
          Where-Object { $_.FullName -notmatch $Exclude } |
          Sort-Object FullName -Unique)
}

$files = @($files | Where-Object { Get-SourceLanguage $_.Extension })

if ($files.Count -eq 0) {
    Write-Host "No F# or C# files found under $root" -ForegroundColor Yellow
    return
}

$results = @(foreach ($f in $files) {
    Measure-SourceFile -FilePath $f.FullName -Lang (Get-SourceLanguage $f.Extension)
})

# Worst (densest) first: a lower code:prose ratio is more comment per unit of code.
$ranked = $results | Sort-Object Ratio

# Objects instead of a table, so the caller can filter, sort and group them.
if ($PassThru) {
    $ranked
    return
}
$shown = if ($Top -gt 0) { $ranked | Select-Object -First $Top } else { $ranked }

$fmt = "{0,-46} {1,-4} {2,6} {3,6} {4,8} {5,6} {6,9} {7,7} {8,5}"
Write-Host ""
Write-Host ($fmt -f "file", "lang", "code", "delim", "comment", "prose", "ratio", "blocks", "max") -ForegroundColor Cyan
Write-Host ($fmt -f ("-" * 46), "----", "------", "------", "--------", "------", "---------", "-------", "-----") -ForegroundColor DarkGray

foreach ($r in $shown) {
    $rel = [System.IO.Path]::GetRelativePath($root, $r.File)
    if ($rel.Length -gt 46) { $rel = "..." + $rel.Substring($rel.Length - 43) }

    $tag = if ($r.Language -eq "FSharp") { "F#" } else { "C#" }
    $ratioText = if ([double]::IsInfinity($r.Ratio)) { "  --  " } else { "{0,6:0.0}:1" -f $r.Ratio }
    $colour =
        if ([double]::IsInfinity($r.Ratio)) { "DarkGray" }
        elseif ($r.Ratio -lt $Threshold / 2) { "Red" }
        elseif ($r.Ratio -lt $Threshold) { "Yellow" }
        else { "Green" }

    Write-Host ($fmt -f $rel, $tag, $r.Code, $r.Delimiter, $r.Comment, $r.Prose, $ratioText, $r.LongBlocks, $r.MaxBlock) -ForegroundColor $colour
}

function Write-Totals {
    param([string]$Label, $Set)
    if ($Set.Count -eq 0) { return }
    $c = ($Set | Measure-Object -Property Code -Sum).Sum
    $p = ($Set | Measure-Object -Property Prose -Sum).Sum
    $b = ($Set | Measure-Object -Property LongBlocks -Sum).Sum
    $ratio = if ($p -eq 0) { [double]::PositiveInfinity } else { $c / $p }
    $below = @($Set | Where-Object { $_.Ratio -lt $Threshold }).Count
    Write-Host ("{0,-8} {1,3} files: {2,6} code, {3,5} prose, {4,5:0.0}:1 — {5} below target, {6} blocks of {7}+" -f
        $Label, $Set.Count, $c, $p, $ratio, $below, $b, $MinBlock)
}

Write-Host ""
Write-Totals "ALL" $results
$byLang = $results | Group-Object Language
if ($byLang.Count -gt 1) {
    foreach ($g in ($byLang | Sort-Object Name)) {
        $label = if ($g.Name -eq "FSharp") { "  F#" } else { "  C#" }
        Write-Totals $label @($g.Group)
    }
}

if ($Detail) {
    Write-Host ""
    Write-Host "Prose blocks of $MinBlock+ lines:" -ForegroundColor Cyan

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
        Select-Object File, Language, Code, Delimiter, RawCode, Comment, Prose, Blank,
            @{ n = "Ratio"; e = { if ([double]::IsInfinity($_.Ratio)) { "" } else { "{0:0.00}" -f $_.Ratio } } },
            Trailing, LongBlocks, MaxBlock |
        Sort-Object Ratio |
        Export-Csv -Path $Csv -NoTypeInformation

    Write-Host ""
    Write-Host "Wrote $Csv"
}
