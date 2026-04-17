// A type whose final member body ends with an if/else whose else-branch
// contains a nested if/exit followed by a top-level comment and another
// statement at the outer method indent, followed by a static member with
// trailing `?optional` parameters.
// Affects: fsi.fs
type FsiEvaluationSessionHostConfig() =
    abstract FormatProvider: obj

type FsiEvaluationSession(cfg: int) =

    member x.Run () =
        let interact = true
        let progress = true
        if interact then
            printfn "interactive"
        else // not interact
            if progress then
                printfn "Run: not interact, loading initial files..."

            printfn "LoadInitialFiles"

            if progress then
                printfn "Run: done..."

            exit 1

        // The Ctrl-C exception handler that we've passed to native code has
        // to be explicitly kept alive.
        System.GC.KeepAlive cfg

    static member Create(fsiConfig, argv, inReader, outWriter, errorWriter, ?collectible, ?legacyReferenceResolver) =
        new FsiEvaluationSession(fsiConfig)

    static member GetDefaultConfiguration(fsiObj: obj, useFsiAuxLib: bool) =
        { new FsiEvaluationSessionHostConfig() with
            member _.FormatProvider = fsiObj }
