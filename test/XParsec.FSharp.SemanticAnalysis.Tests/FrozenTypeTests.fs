module XParsec.FSharp.SemanticAnalysis.Tests.FrozenTypeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The `SemType` ↔ `FrozenType` round-trip oracle. The
// bridge is the keystone of the 3B cutover: every later slice (3B-2's encoder
// flip, 3B-4's tree flip) relies on `toFrozen` / `ofFrozen` being mutual
// inverses on the post-freeze subset, so an IL-byte-identical claim can rest on
// "the encoder was proven equivalent via the round trip". This pins that.
//
// Two laws (the plan's wording):
//   * `ofFrozen >> toFrozen = id` on all `FrozenType`   (no `FrozenType` is lost)
//   * `toFrozen >> ofFrozen = id` on the post-freeze `SemType` subset
// `TyVar` — the one `SemType` case with no `FrozenType` counterpart — is the
// hard-error boundary, also asserted.

/// A deterministic, depth-bounded enumeration of `FrozenType` shapes covering
/// every constructor, including nesting and the two `TyparAxis`es. No RNG (no
/// FsCheck dependency); the set is small but exhaustive over the constructors.
let private sampleFrozenTypes: FrozenType list =
    let kRec = SymbolKeyOps.qualifiedTypeKey "Test.Box" 1
    let kUnion = SymbolKeyOps.qualifiedTypeKey "Test.Option" 1
    let kClass = SymbolKeyOps.qualifiedTypeKey "Test.Widget" 2
    // An enum is niladic (arity 0) — a leaf nominal carrying only its key.
    let kEnum = SymbolKeyOps.qualifiedTypeKey "Test.Colour" 0

    // Leaves: every nullary / typar / unknown form.
    let leaves =
        [
            FTConst("int", EqArray.empty)
            FTConst("string", EqArray.empty)
            FTTypar(TyparAxis.Declaring, 0)
            FTTypar(TyparAxis.Declaring, 3)
            FTTypar(TyparAxis.Method, 0)
            FTTypar(TyparAxis.Method, 2)
            FTUnknown "Unresolved.Head"
            // Niladic nominal enum — a key-only leaf, no args.
            FTEnum kEnum
        ]

    // One level of every branching constructor over a couple of leaves, then a
    // second level nesting branches inside branches so the recursive `EqArray.map`
    // arms are all exercised.
    let branch1 =
        [
            FTConst("[]", EqArray.singleton (FTConst("int", EqArray.empty)))
            FTFun(FTConst("int", EqArray.empty), FTTypar(TyparAxis.Method, 0))
            FTTuple(EqArray.ofList [ FTConst("int", EqArray.empty); FTTypar(TyparAxis.Declaring, 0) ])
            FTRecord(kRec, EqArray.singleton (FTTypar(TyparAxis.Declaring, 0)))
            FTUnion(kUnion, EqArray.singleton (FTConst("string", EqArray.empty)))
            FTClass(kClass, EqArray.ofList [ FTTypar(TyparAxis.Declaring, 0); FTTypar(TyparAxis.Declaring, 1) ])
            // Anonymous (structural) union — members in canonical (sorted) order, as
            // `mkUnion` will produce. The round-trip is purely structural, so the map
            // preserves the member vector either way.
            FTOr(EqArray.ofList [ FTConst("int", EqArray.empty); FTConst("string", EqArray.empty) ])
        ]

    let branch2 =
        [
            // Curried fun nesting a tuple arg and a record result.
            FTFun(
                FTTuple(EqArray.ofList [ FTConst("int", EqArray.empty); FTConst("bool", EqArray.empty) ]),
                FTFun(FTRecord(kRec, EqArray.singleton (FTTypar(TyparAxis.Method, 0))), FTConst("unit", EqArray.empty))
            )
            // Generic intrinsic carrying a union carrying a class.
            FTConst(
                "[]",
                EqArray.singleton (
                    FTUnion(
                        kUnion,
                        EqArray.singleton (
                            FTClass(kClass, EqArray.ofList [ FTConst("int", EqArray.empty); FTUnknown "X" ])
                        )
                    )
                )
            )
        ]

    leaves @ branch1 @ branch2

[<Tests>]
let tests =
    testList
        "FrozenType bridge round-trip"
        [
            test "ofFrozen >> toFrozen = id on every FrozenType shape" {
                for ft in sampleFrozenTypes do
                    Expect.equal (toFrozen (ofFrozen ft)) ft (sprintf "round-trips: %A" ft)
            }

            test "toFrozen >> ofFrozen = id on the post-freeze SemType subset" {
                // Each `ofFrozen ft` is a representative of the post-freeze subset
                // (the cases `freeze` can legally produce — no `TyVar`).
                for ft in sampleFrozenTypes do
                    let ty = ofFrozen ft
                    Expect.equal (ofFrozen (toFrozen ty)) ty (sprintf "round-trips: %A" ty)
            }

            test "every post-freeze SemType case is covered by the sample" {
                // Guards against the sample silently dropping a constructor: assert
                // the ten expected case tags all appear among `ofFrozen` images.
                let tag (ty: SemType) =
                    match ty with
                    | TyConst _ -> "TyConst"
                    | TyFun _ -> "TyFun"
                    | TyTuple _ -> "TyTuple"
                    | TyRecord _ -> "TyRecord"
                    | TyUnion _ -> "TyUnion"
                    | TyClass _ -> "TyClass"
                    | TyOr _ -> "TyOr"
                    | TyTypar _ -> "TyTypar"
                    | TyUnknown _ -> "TyUnknown"
                    | TyEnum _ -> "TyEnum"
                    | TyVar _ -> "TyVar"

                let seen = sampleFrozenTypes |> List.map (ofFrozen >> tag) |> Set.ofList

                for expected in
                    [
                        "TyConst"
                        "TyFun"
                        "TyTuple"
                        "TyRecord"
                        "TyUnion"
                        "TyClass"
                        "TyOr"
                        "TyTypar"
                        "TyUnknown"
                        "TyEnum"
                    ] do
                    Expect.isTrue (Set.contains expected seen) (sprintf "sample covers %s" expected)
            }

            test "toFrozen on a TyVar is a hard error (the sole case with no FrozenType counterpart)" {
                let metavar = TyVar(TypeVar())

                Expect.throws
                    (fun () -> toFrozen metavar |> ignore)
                    "an inference metavar must not reach the frozen boundary"
            }

            test "toFrozen rejects a TyVar nested inside an otherwise-frozen shape" {
                let nested = TyFun(TyConst("int", EqArray.empty), TyVar(TypeVar()))

                Expect.throws
                    (fun () -> toFrozen nested |> ignore)
                    "a buried metavar is still rejected (the recursion reaches it)"
            }
        ]
