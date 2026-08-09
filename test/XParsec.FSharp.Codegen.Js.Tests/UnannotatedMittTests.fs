module XParsec.FSharp.Codegen.Js.Tests.UnannotatedMittTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// Two type-identity pins around mitt's generic factory: an annotation is REQUIRED to
// ground `Events`, and `undefined` is a type distinct from `unit`, so only an
// `undefined`-typed event admits the no-payload `emit` overload.

let private analyseErrors (input: string) : string list =
    analyseWith MittFixture.provider input |> List.map (fun d -> d.Message)

[<Tests>]
let tests =
    testList
        "UnannotatedMitt"
        [
            // ── annotation-required policy ──
            test "an ANNOTATED mitt() grounds Events and type-checks" {
                let input = "let e : Emitter<int> = mitt()\n"
                Expect.isEmpty (analyseErrors input) "an explicit Emitter<int> annotation grounds Events"
            }

            test "an UNANNOTATED mitt() call leaves Events ungrounded (policy: annotation required)" {
                // No annotation and no use to solve `Events` from leaves an unresolved type
                // variable. This is the DELIBERATE policy: an external generic factory
                // carries nothing to infer its parameter from, as in TS.
                let input = "let e = mitt()\nlet u = e.emit(\"x\", 1)\n"
                Expect.isNonEmpty (analyseErrors input) "an unannotated mitt() must leave Events ungrounded"
            }

            // ── no-payload emit: the conditional fold, per payload type ──
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
                // `undefined extends int` is false, so the overload folds to `never` and a
                // no-payload emit on `ping` must error.
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

            test "a unit-typed event is REJECTED by no-payload emit (undefined ≠ unit)" {
                // `unit` and `undefined` are distinct type identities, so
                // `undefined extends Events[Key]` does not hold for a `unit` payload and the
                // conditional folds to `never`. Both still emit JS `undefined` at the backend.
                let input =
                    String.concat
                        "\n"
                        [
                            "type Ev = { ping: int; tick: unit }"
                            "let e : Emitter<Ev> = mitt()"
                            "let u = e.emit(\"tick\")"
                            ""
                        ]

                Expect.isNonEmpty
                    (analyseErrors input)
                    "unit is not undefined-bearing; no-payload emit('tick') must error"
            }
        ]
