module XParsec.FSharp.Codegen.Js.Tests.UnannotatedMittTests

open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The mitt full-surface gate's residues, each pinned so a future regression is
// visible:
//   • the annotation-required POLICY for the generic factory (deliberately open).
//   • undefined-vs-unit type identity — now CLOSED (Wall 1): a `unit`-typed event is
//     correctly rejected by no-payload `emit`, an `undefined`-typed one accepted.

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

            test "a unit-typed event is REJECTED by no-payload emit (undefined≠unit now holds)" {
                // GAP CLOSED (Wall 1): `unit` and `undefined` are now distinct type
                // identities, so `undefined extends Events[Key]` NO LONGER holds for a `unit`
                // payload — the conditional folds to `never` and a no-payload `emit("tick")`
                // on a `unit`-typed event correctly errors. (Only an `undefined`-typed event
                // admits the no-payload overload; see the sibling test above.) The backend
                // repr coincidence — both `unit` and `undefined` emit JS `undefined` — is
                // unaffected; this is a type-identity distinction only.
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
