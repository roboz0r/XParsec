module XParsec.FSharp.Codegen.Clr.Tests.DiagnosticTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// The negative direction: the suite is almost entirely happy-path, so a regression that
// ACCEPTS bad input goes unseen. Each row is a program that must be rejected, pinned on a
// stable substring of the message.

[<Tests>]
let tests =
    testList
        "Diagnostics"
        [
            for name, fragment, src in
                [
                    "an unbound name is rejected", "Unresolved identifier", "printfn \"%d\" missing"
                    "a string against the %d hole's integer family is rejected", "one of int", "printfn \"%d\" \"hi\""
                    "a string bound to an int-annotated value is rejected",
                    "Type mismatch",
                    "let x : int = \"hi\"\nprintfn \"%d\" x"
                    "assignment to an immutable record field is rejected",
                    "Cannot assign to immutable field",
                    "type R = { x: int }\nlet r = { x = 1 }\nr.x <- 2"
                    // `disposable` is `System.IDisposable`
                    "`use` over a type that does not implement `disposable` is rejected",
                    "implement 'disposable'",
                    "type R() =\n    member this.value = 1\nlet run () =\n    use r = R()\n    ()\nrun ()"
                    // the disposed thing is the bound value, so only a simple variable
                    // pattern is legal there
                    "a destructuring `use` pattern is rejected",
                    "simple variable patterns",
                    "let run () =\n    use a, b = (1, 2)\n    ()\nrun ()"
                    // at-most-once evaluation is only guaranteed for a single, non-repeated use
                    "a [<CallAtMostOnce>] parameter used twice is rejected",
                    "used at most once",
                    "let inline twice (a: bool) ([<CallAtMostOnce>] b: bool) : bool = if a then b else b\nprintfn \"%b\" (twice true true)"
                    // the attribute only means anything for the splice the inliner performs
                    "[<CallAtMostOnce>] on a non-inline function's parameter is rejected",
                    "only valid on a parameter of an 'inline' function",
                    "let notInline (a: bool) ([<CallAtMostOnce>] b: bool) : bool = if a then b else false\nprintfn \"%b\" (notInline true true)"
                ] -> test name { failsWith fragment src }

            // `BitArray` implements only the non-generic `IEnumerable`.
            yield
                test "duck-typed for-in over BitArray type-checks (codegen deferred)" {
                    typeChecks "let f (ba: System.Collections.BitArray) =\n    for x in ba do\n        ()"
                }

            // An 8+-element tuple emits via `ValueTuple`8`'s `TRest` nesting, so tuple
            // arity is unbounded.
            yield
                test "an 8-element tuple type-checks (no arity cap)" {
                    typeChecks "let a, b, c, d, e, f, g, h = (1, 2, 3, 4, 5, 6, 7, 8)\n()"
                }
        ]
