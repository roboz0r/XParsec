module XParsec.FSharp.Codegen.Clr.Tests.DiagnosticTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1, the negative direction (docs/codegen-test-strategy-plan.md): the
// suite is almost entirely happy-path, so a regression that *accepts* bad input
// — or silently changes a diagnostic — goes unseen. This table pins a handful of
// errors the front end already emits, asserting on a stable substring of the
// message (the analysis carries no diagnostic *codes* to key off yet). Each row
// is a `failsWith` over a program that must be rejected, for the stated reason.

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
                    // `use` over a type with no Dispose member
                    "Dispose", "type R() =\n    member this.value = 1\nlet run () =\n    use r = R()\n    ()\nrun ()"
                ] -> test src { failsWith fragment src }

            // a front-end-only positive: the duck-typed `for-in` over a source
            // that implements only the non-generic IEnumerable type-checks (its
            // codegen is deferred), so `typeChecks` — not `runs` — is the right
            // assertion. Mirrors the ForInTests §4.4 anchor.
            yield
                test "duck-typed for-in over BitArray type-checks (codegen deferred)" {
                    typeChecks "let f (ba: System.Collections.BitArray) =\n    for x in ba do\n        ()"
                }
        ]
