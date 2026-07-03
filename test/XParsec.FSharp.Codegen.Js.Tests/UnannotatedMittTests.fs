module XParsec.FSharp.Codegen.Js.Tests.UnannotatedMittTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The two HONEST residues the mitt full-surface gate deliberately does NOT
// close, each pinned so a future regression (or a future closure) is visible:
//   • Wall 1 — the annotation-required POLICY for the generic factory.
//   • the undefined-vs-unit type-identity precision gap (the deferred `null`/`undefined`
//     intrinsic support, orthogonal to the conditional-fold machinery).

let private analyseErrors (input: string) : string list =
    analyseWith MittFixture.provider input |> List.map (fun d -> d.Message)

[<Tests>]
let tests =
    testList
        "UnannotatedMitt"
        [
            // ── Wall 1: annotation-required policy ──
            test "an ANNOTATED mitt() grounds Events and type-checks" {
                let input = "let e : Emitter<int> = mitt()\n"
                Expect.isEmpty (analyseErrors input) "an explicit Emitter<int> annotation grounds Events"
            }

            test "an UNANNOTATED mitt() call leaves Events ungrounded (policy: annotation required)" {
                // No annotation and no use to solve `Events` from → an unresolved TyVar. This
                // is the DELIBERATE policy (an external generic factory carries nothing to
                // infer its parameter from), not a bug to fix; it mirrors TS needing an
                // annotation / `as` at such a site. If later work infers `Events`
                // from downstream uses, flip this pin.
                let input = "let e = mitt()\nlet u = e.emit(\"x\", 1)\n"
                Expect.isNonEmpty (analyseErrors input) "an unannotated mitt() must leave Events ungrounded"
            }

            // ── no-payload emit: conditional fold works for undefined; unit is the gap ──
            test "no-payload emit type-checks for an undefined-typed event (conditional fold)" {
                let input =
                    String.concat
                        "\n"
                        [
                            "type Ev = { ping: int; tick: undefined }"
                            "let e : Emitter<Ev> = mitt()"
                            "let u = e.emit(\"tick\")"
                            ""
                        ]

                Expect.isEmpty (analyseErrors input) "undefined extends Events['tick'] ? Key : never folds to the key"
            }

            test "no-payload emit REJECTS a key whose payload is not undefined-bearing" {
                // ping:int — `undefined extends int` is false → the overload folds to `never`,
                // so a no-payload emit on `ping` must error.
                let input =
                    String.concat
                        "\n"
                        [
                            "type Ev = { ping: int; tick: undefined }"
                            "let e : Emitter<Ev> = mitt()"
                            "let u = e.emit(\"ping\")"
                            ""
                        ]

                Expect.isNonEmpty
                    (analyseErrors input)
                    "int is not undefined-bearing; no-payload emit('ping') must error"
            }

            test "KNOWN GAP: a unit-typed event is wrongly accepted by no-payload emit (undefined≠unit deferred)" {
                // The precision limit of the deferred `null`/`undefined`-intrinsic support: `unit`
                // and `undefined` are not yet distinct types, so `undefined extends Events[Key]`
                // wrongly holds for a `unit` payload. The conditional-fold machinery is correct;
                // only the undefined-vs-unit identity is imprecise. Pinned as GAP — when that
                // intrinsic support lands and this starts erroring, flip to `isNonEmpty`.
                let input =
                    String.concat
                        "\n"
                        [
                            "type Ev = { ping: int; tick: unit }"
                            "let e : Emitter<Ev> = mitt()"
                            "let u = e.emit(\"tick\")"
                            ""
                        ]

                Expect.isEmpty
                    (analyseErrors input)
                    "GAP (documented): unit is currently accepted where only undefined should be"
            }
        ]
