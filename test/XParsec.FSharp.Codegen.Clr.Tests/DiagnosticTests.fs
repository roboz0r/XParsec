module XParsec.FSharp.Codegen.Clr.Tests.DiagnosticTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The negative direction: the suite is almost entirely happy-path, so a
// regression that *accepts* bad input — or silently changes a diagnostic — goes
// unseen. This table pins a handful of errors the front end emits, asserting on
// a stable substring of the message. Each row is a `failsWith` over a program
// that must be rejected, for the stated reason.

[<Tests>]
let tests =
    testList
        "Diagnostics"
        [
            for fragment, src in
                [
                    // an unbound name
                    "Unresolved identifier", "printfn \"%d\" missing"
                    // a string where an int is expected (the %d hole)
                    "Type mismatch", "printfn \"%d\" \"hi\""
                    // assigning a string to an int-bound value
                    "Type mismatch", "let x : int = \"hi\"\nprintfn \"%d\" x"
                    // assigning to an immutable record field
                    "Cannot assign to immutable field", "type R = { x: int }\nlet r = { x = 1 }\nr.x <- 2"
                    // `use` over a type that doesn't implement `disposable`
                    // (`System.IDisposable`) — the §3b interface-required flip.
                    "implement 'disposable'",
                    "type R() =\n    member this.value = 1\nlet run () =\n    use r = R()\n    ()\nrun ()"
                    // a destructuring `use` — only simple variable patterns are legal
                    // there (the bound value is what gets disposed), so the front end
                    // rejects it rather than letting codegen `failwithf`.
                    "simple variable patterns", "let run () =\n    use a, b = (1, 2)\n    ()\nrun ()"
                    // an 8-element tuple — `System.ValueTuple` is emitted only for
                    // arity 2–7 (8+ needs `TRest` nesting, still deferred), so the
                    // front end rejects it rather than letting codegen crash in the
                    // encoder / `ValueTupleRefs`.
                    "elements are not yet supported", "printfn \"%A\" (1, 2, 3, 4, 5, 6, 7, 8)"
                    // the destructuring side: an 8-element tuple *pattern* is caught
                    // the same way (`checkTuplePat`), not just the construction site.
                    "elements are not yet supported", "let a, b, c, d, e, f, g, h = (1, 2, 3, 4, 5, 6, 7, 8)\n()"
                    // a [<CallAtMostOnce>] parameter used more than once violates the
                    // linearity contract (the compiler can only guarantee at-most-once
                    // evaluation for a single, non-repeated use).
                    "used at most once",
                    "let inline twice (a: bool) ([<CallAtMostOnce>] b: bool) : bool = if a then b else b\nprintfn \"%b\" (twice true true)"
                    // the same attribute on a NON-inline function: it only has meaning
                    // for the splice the inliner performs, so it is rejected up front.
                    "only valid on a parameter of an 'inline' function",
                    "let notInline (a: bool) ([<CallAtMostOnce>] b: bool) : bool = if a then b else false\nprintfn \"%b\" (notInline true true)"
                ] -> test src { failsWith fragment src }

            // The duck-typed `for-in` over a source that implements only the
            // non-generic IEnumerable type-checks but is not run here.
            yield
                test "duck-typed for-in over BitArray type-checks (codegen deferred)" {
                    typeChecks "let f (ba: System.Collections.BitArray) =\n    for x in ba do\n        ()"
                }
        ]
