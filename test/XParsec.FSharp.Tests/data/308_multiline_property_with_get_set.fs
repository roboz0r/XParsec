module Test

// Multi-line property: member name on one line, 'with get/set' on next
// From FSharpCheckerResults.fs lines 2941-2942 and progress notes

type Logger() =
    let mutable options = 0
    let mutable buildPhase = 0

    // Name and 'with set' on separate lines
    member _.DiagnosticOptions
        with set opts = options <- opts

    // Name and 'with get' on separate lines
    member _.BuildPhase
        with get () = buildPhase

    // Name and both get/set on separate lines
    member _.Value
        with get () = options
        and set v = options <- v
