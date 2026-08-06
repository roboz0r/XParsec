<#
.SYNOPSIS
    Lists, checks or strips FOR-REVIEW comments from F# sources (.fs / .fsi).

.DESCRIPTION
    A FOR-REVIEW comment is addressed to the REVIEWER of a change — "here is why this is
    correct", "I considered X and rejected it", "note the ordering here". That is legitimate
    communication, but it is not documentation: it answers a question that stops being asked
    once the change is accepted, and left in place it rots into a claim about code that has
    since moved.

    So it is written in a form a script can remove. Two spellings:

      LINE   //FOR-REVIEW <text>            one line
      BLOCK  (*FOR-REVIEW <text> *)         spans lines, nests, ordinary F#

    A block closes on its matching `*)`, counting nesting, so it is just an F# block comment
    with a marker at the front. An unterminated one is an F# lexer error ("end of file in
    comment"), so a malformed marker fails the build rather than going unnoticed; -Strip still
    refuses to touch such a file rather than guessing where the block ended.

    Stripping removes the marked text and drops any line left blank by the removal. It never
    touches a marker inside a string literal: the scan is F#-aware (regular, verbatim and
    triple-quoted strings are tracked across lines, and `(*)` is the multiplication operator).

.PARAMETER Path
    File or directory to scan. Defaults to the current directory.

.PARAMETER Strip
    Remove the comments and rewrite the files. Without this, nothing is modified.

.PARAMETER Check
    Exit with code 1 if any FOR-REVIEW comment is found. For a pre-commit gate.

.PARAMETER Exclude
    Regex of paths to skip. Default skips bin/obj output directories.

.EXAMPLE
    ./review-comments.ps1
    List every FOR-REVIEW comment under the current directory.

.EXAMPLE
    ./review-comments.ps1 -Check
    Fail (exit 1) if any survive — run this before committing.

.EXAMPLE
    ./review-comments.ps1 -Strip
    Remove them all, then rebuild and re-run the test suite.
#>

[CmdletBinding()]
param(
    [Parameter(Mandatory = $false, Position = 0)]
    [string]$Path = ".",

    [Parameter(Mandatory = $false)]
    [switch]$Strip,

    [Parameter(Mandatory = $false)]
    [switch]$Check,

    [Parameter(Mandatory = $false)]
    [string]$Exclude = '[\\/](bin|obj|node_modules|dist)[\\/]'
)

$ErrorActionPreference = "Stop"

$LineMarker = '//FOR-REVIEW'
$BlockOpen = '(*FOR-REVIEW'

# Every index at which $needle occurs in $line OUTSIDE a string literal. The string state is
# carried in/out so a literal spanning lines cannot hide (or fake) a marker.
function Find-MarkerIndices {
    param(
        [string]$Line,
        [string]$Needle,
        [ref]$InString,
        [ref]$InVerbatim,
        [ref]$InTriple
    )

    $hits = [System.Collections.Generic.List[int]]::new()
    $len = $Line.Length
    $i = 0

    while ($i -lt $len) {
        $c = $Line[$i]
        $next = if ($i + 1 -lt $len) { $Line[$i + 1] } else { [char]0 }

        if ($InTriple.Value) {
            if ($c -eq '"' -and $next -eq '"' -and $i + 2 -lt $len -and $Line[$i + 2] -eq '"') {
                $InTriple.Value = $false; $i += 3; continue
            }
            $i++
            continue
        }

        if ($InVerbatim.Value) {
            if ($c -eq '"') {
                # `""` is an escaped quote inside a verbatim string, not its end.
                if ($next -eq '"') { $i += 2; continue }
                $InVerbatim.Value = $false
            }
            $i++
            continue
        }

        if ($InString.Value) {
            if ($c -eq '\') { $i += 2; continue }
            if ($c -eq '"') { $InString.Value = $false }
            $i++
            continue
        }

        if ($i + $Needle.Length -le $len -and $Line.Substring($i, $Needle.Length) -eq $Needle) {
            $hits.Add($i)
            $i += $Needle.Length
            continue
        }

        if ($c -eq '"') {
            if ($next -eq '"' -and $i + 2 -lt $len -and $Line[$i + 2] -eq '"') {
                $InTriple.Value = $true; $i += 3; continue
            }
            $InString.Value = $true
            $i++
            continue
        }

        if ($c -eq '@' -and $next -eq '"') { $InVerbatim.Value = $true; $i += 2; continue }

        $i++
    }

    $hits
}

# Scan one line's comment text, tracking `(*` / `*)` nesting. Returns the index just past the
# `*)` that took the depth to zero, or -1 if the block is still open at end of line.
function Step-BlockDepth {
    param([string]$Line, [int]$From, [ref]$Depth)

    $i = $From
    $len = $Line.Length

    while ($i -lt $len) {
        if ($i + 1 -lt $len -and $Line[$i] -eq '*' -and $Line[$i + 1] -eq ')') {
            $Depth.Value--
            $i += 2
            if ($Depth.Value -le 0) { return $i }
            continue
        }

        if ($i + 1 -lt $len -and $Line[$i] -eq '(' -and $Line[$i + 1] -eq '*') {
            $Depth.Value++
            $i += 2
            continue
        }

        $i++
    }

    -1
}

# Locate every FOR-REVIEW region in one file. A block runs to its matching `*)`; a block left
# open at end of file is reported and the file is never stripped.
function Get-ReviewRegions {
    param([string]$FilePath)

    $lines = [System.IO.File]::ReadAllLines($FilePath)

    $regions = [System.Collections.Generic.List[object]]::new()

    $inString = $false
    $inVerbatim = $false
    $inTriple = $false

    $openLine = -1
    $openCol = -1
    $depth = 0
    $balanced = $true

    for ($n = 0; $n -lt $lines.Length; $n++) {
        $line = $lines[$n]

        # Inside an open block the text is comment, so no string state applies.
        if ($openLine -ge 0) {
            $d = $depth
            $endAt = Step-BlockDepth -Line $line -From 0 -Depth ([ref]$d)
            $depth = $d

            if ($endAt -ge 0) {
                $regions.Add(
                    [pscustomobject]@{
                        Kind      = "Block"
                        StartLine = $openLine
                        StartCol  = $openCol
                        EndLine   = $n
                        EndCol    = $endAt
                    })
                $openLine = -1
            }

            continue
        }

        $s = $inString
        $v = $inVerbatim
        $t = $inTriple

        $opens = Find-MarkerIndices -Line $line -Needle $BlockOpen -InString ([ref]$s) -InVerbatim ([ref]$v) -InTriple ([ref]$t)

        if ($opens.Count -gt 0) {
            $openLine = $n
            $openCol = $opens[0]

            $d = 1
            $endAt = Step-BlockDepth -Line $line -From ($openCol + $BlockOpen.Length) -Depth ([ref]$d)
            $depth = $d

            if ($endAt -ge 0) {
                $regions.Add(
                    [pscustomobject]@{
                        Kind      = "Block"
                        StartLine = $n
                        StartCol  = $openCol
                        EndLine   = $n
                        EndCol    = $endAt
                    })
                $openLine = -1
            }

            $inString = $false; $inVerbatim = $false; $inTriple = $false
            continue
        }

        $s2 = $inString
        $v2 = $inVerbatim
        $t2 = $inTriple

        $lineHits = Find-MarkerIndices -Line $line -Needle $LineMarker -InString ([ref]$s2) -InVerbatim ([ref]$v2) -InTriple ([ref]$t2)

        if ($lineHits.Count -gt 0) {
            $regions.Add(
                [pscustomobject]@{
                    Kind      = "Line"
                    StartLine = $n
                    StartCol  = $lineHits[0]
                    EndLine   = $n
                    EndCol    = $line.Length
                })
        }

        $inString = $s2
        $inVerbatim = $v2
        $inTriple = $t2
    }

    if ($openLine -ge 0) { $balanced = $false }

    [pscustomobject]@{
        File     = $FilePath
        Lines    = $lines
        Regions  = $regions
        Balanced = $balanced
        OpenAt   = $openLine
    }
}

# Cut the regions out, dropping any line the removal leaves blank. Regions are removed last
# first so earlier offsets stay valid.
function Remove-ReviewRegions {
    param([object]$Scan)

    $lines = [System.Collections.Generic.List[string]]::new()
    foreach ($l in $Scan.Lines) { $lines.Add($l) }

    $ordered = @($Scan.Regions | Sort-Object StartLine, StartCol -Descending)

    foreach ($r in $ordered) {
        $head = $lines[$r.StartLine].Substring(0, $r.StartCol)
        $tail =
            if ($r.EndCol -ge $lines[$r.EndLine].Length) { "" }
            else { $lines[$r.EndLine].Substring($r.EndCol) }

        $merged = $head + $tail

        for ($k = $r.EndLine; $k -gt $r.StartLine; $k--) { $lines.RemoveAt($k) }

        if ($merged.Trim().Length -eq 0) { $lines.RemoveAt($r.StartLine) }
        else { $lines[$r.StartLine] = $merged.TrimEnd() }
    }

    $lines
}

$target = (Resolve-Path -Path $Path).Path

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

$scans = @(foreach ($f in $files) { Get-ReviewRegions -FilePath $f.FullName })
$withAny = @($scans | Where-Object { $_.Regions.Count -gt 0 -or -not $_.Balanced })

$unbalanced = @($scans | Where-Object { -not $_.Balanced })

foreach ($u in $unbalanced) {
    $rel = [System.IO.Path]::GetRelativePath($root, $u.File)
    Write-Host ("UNBALANCED  {0}:{1} — '{2}' opened and never closed" -f $rel, ($u.OpenAt + 1), $BlockOpen) -ForegroundColor Red
}

$total = 0

foreach ($s in $withAny) {
    if ($s.Regions.Count -eq 0) { continue }

    $rel = [System.IO.Path]::GetRelativePath($root, $s.File)

    foreach ($r in $s.Regions) {
        $total++
        $preview = $s.Lines[$r.StartLine].Substring($r.StartCol)
        if ($preview.Length -gt 60) { $preview = $preview.Substring(0, 57) + "..." }

        $span = if ($r.EndLine -eq $r.StartLine) { "" } else { " (+{0} lines)" -f ($r.EndLine - $r.StartLine) }
        Write-Host ("  {0}:{1}{2}  {3}" -f $rel, ($r.StartLine + 1), $span, $preview) -ForegroundColor Yellow
    }
}

Write-Host ""

if ($total -eq 0 -and $unbalanced.Count -eq 0) {
    Write-Host "No FOR-REVIEW comments." -ForegroundColor Green
    return
}

Write-Host ("{0} FOR-REVIEW comment(s) in {1} file(s)." -f $total, @($withAny | Where-Object { $_.Regions.Count -gt 0 }).Count)

if ($unbalanced.Count -gt 0) {
    Write-Host ("{0} file(s) have an unbalanced block marker and will NOT be stripped." -f $unbalanced.Count) -ForegroundColor Red
}

if ($Strip) {
    if ($unbalanced.Count -gt 0) {
        Write-Host "Refusing to strip while a block marker is unbalanced — fix those first." -ForegroundColor Red
        exit 1
    }

    $changed = 0

    foreach ($s in $withAny) {
        if ($s.Regions.Count -eq 0) { continue }

        $out = Remove-ReviewRegions -Scan $s
        [System.IO.File]::WriteAllLines($s.File, $out)
        $changed++
    }

    Write-Host ""
    Write-Host ("Stripped {0} comment(s) from {1} file(s). Rebuild and re-run the suite." -f $total, $changed) -ForegroundColor Green
    return
}

if ($Check) {
    Write-Host "FOR-REVIEW comments must not be committed. Run with -Strip to remove them." -ForegroundColor Red
    exit 1
}
