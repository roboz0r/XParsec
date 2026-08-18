module XParsec.FSharp.Codegen.Clr.Tests.RegionLayoutTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common.Tests
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Regions stamps what the TARGET does not lay out as a value. The CLR lays `int` out as one,
// so an `int`-typed result mints no region and its binding gets no escape entry at all. The
// same programs take the opposite verdicts in the JS suite, which is the point of them.

let private provider = lazy (ClrSymbolProviders.buildContract defaultPackages)

let private probe (input: string) =
    RegionProbe.analyse provider.Value input

[<Tests>]
let tests =
    testList
        "RegionLayout"
        [
            test "an int-returning local closure doesn't escape" {
                // `f` is called inside `useLocal`'s body and never escapes; `useLocal`
                // returns the `int` result of `f 3`.
                let p = probe "let useLocal () = let f x = x + 1 in f 3"

                Expect.equal (RegionProbe.escapeOf p "useLocal") (Some LocalStack) "useLocal returns int → LocalStack"

                Expect.equal (RegionProbe.escapeOfNested p "f") (Some LocalStack) "f is LocalStack"
            }

            test "pure arithmetic has no escape entry" {
                let p = probe "let r = 1 + 2"

                Expect.equal (RegionProbe.escapeOf p "r") None "r has no escape entry — the CLR lays an int flat"
            }

            test "a tuple-typed call result does not mint a region" {
                // The CLR answers `System.ValueTuple`2` for a 2-tuple, and that is a value type,
                // so `pair 3` is flat. The literal that BUILT it still allocates, inside `pair`.
                let p = probe "let pair x = (x, x)\nlet pt = pair 3"

                Expect.equal (RegionProbe.escapeOf p "pt") None "pt has no escape entry — a CLR tuple is a value"
            }

            test "a closure returning nested closures is CallerStack" {
                // `mk` returns closures, not a tracked value: the `int` they compute stamps
                // nothing, so nothing lifts `mk` past `CallerStack`.
                let p = probe "let mk x = fun y -> fun z -> x + y + z"

                Expect.equal (RegionProbe.escapeOf p "mk") (Some CallerStack) "mk returns nested closures → CallerStack"
            }
        ]
