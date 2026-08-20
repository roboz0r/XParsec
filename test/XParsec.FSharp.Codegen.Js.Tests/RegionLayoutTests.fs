module XParsec.FSharp.Codegen.Js.Tests.RegionLayoutTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Regions stamps what the TARGET does not lay out as a value, and JS lays NOTHING out as one.
// So an `int`-typed result is region-tracked here, and each program below takes the opposite
// verdict from the CLR suite's copy of it. The one verdict that crosses a target is the
// `HeapShared` a captured `let mutable` needs, and no program here has one.

let private provider =
    lazy (JsNativeSymbols.buildJsNativeContract [ vesperCorePackage ])

let private probe (input: string) =
    RegionProbe.analyse testCompiling provider.Value input

[<Tests>]
let tests =
    testList
        "RegionLayout"
        [
            test "an int-returning local closure escapes its frame" {
                let p = probe "let useLocal () = let f x = x + 1 in f 3"

                Expect.equal
                    (RegionProbe.escapeOf p "useLocal")
                    (Some CallerStack)
                    "useLocal returns a tracked int → CallerStack"

                // `f` is called in place and never escapes, but its region outlives its body's,
                // and the body returns a tracked int out of `f`'s frame.
                Expect.equal (RegionProbe.escapeOfNested p "f") (Some CallerStack) "f absorbs its tracked result"
            }

            test "pure arithmetic gets an escape entry" {
                let p = probe "let r = 1 + 2"

                Expect.equal
                    (RegionProbe.escapeOf p "r")
                    (Some LocalStack)
                    "r is tracked — no int is laid out flat here — and has no frame to escape"
            }

            test "a tuple-typed call result is tracked" {
                // JS answers an array for a tuple of any arity, and no array is laid out flat,
                // so `pair 3` is tracked where the CLR suite's copy of this program is not.
                let p = probe "let pair x = (x, x)\nlet pt = pair 3"

                Expect.equal (RegionProbe.escapeOf p "pt") (Some CallerStack) "pt is tracked — a JS tuple is an array"
            }

            test "a closure returning nested closures is HeapShared" {
                // The tracked `int` the bodies compute reaches both lambda regions, which is
                // the two-boundary rule the CLR never sees at this program.
                let p = probe "let mk x = fun y -> fun z -> x + y + z"

                Expect.equal (RegionProbe.escapeOf p "mk") (Some HeapShared) "mk's tracked result crosses 2 lambdas"
            }
        ]
