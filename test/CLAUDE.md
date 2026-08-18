# Tests

Run suites through `./claude_tools.cmd -Action Test -TestProject <name>`; the `xparsec-dev`
skill lists the valid projects and the snapshot and filter flags.

## Debugging a test

Change `test` to `ftest` (focus) in the source to ignore other tests. Appending diagnotic information
to a file as the test runs is a reliable way to obtain information. `eprintfn` gets swallowed
by the test harness.

## Verifying a library edit

`-Filter` switches the runner to `dotnet run --no-build`, and `Build -SourceProject <lib>`
rebuilds the library's own DLL without relinking it into the test project's `bin`. A `-Filter`
run after a library edit therefore executes the stale test assembly against the old library, and
the fix appears to have had no effect. After editing library code, verify with a no-filter run,
which does a full build and relink. Use `-Filter` only when iterating on a test file against an
already-current library.

Library `eprintfn` is swallowed by the test host's output capture. A file-append diagnostic into
`./tmp/` is reliable where `eprintfn` is not.

## Reading existing output

The corpus test spawns hundreds of subprocesses and takes a long time. Read
`XParsec.FSharp.Tests/data/ms-fsharp/REPORT.txt` rather than re-running it, and read the
`.fs.parsed` goldens rather than re-parsing a file. Re-run only after a code change warrants a
fresh comparison.

## Resolving symbols in a test

Wire real contracts (`ReferencedProject.buildProvider`, `SymbolProviders.build [manifest]`)
rather than a hardcoded provider, and prove a new front-end resolution feature against a real
`.fsi` with no backstop. A hardcoded provider declares ops with ad-hoc shapes that can diverge
from the real contracts, which hides gaps in contract-backed resolution.
