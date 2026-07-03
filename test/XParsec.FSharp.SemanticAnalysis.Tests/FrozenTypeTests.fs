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
            // Anonymous (structural) union — built through the smart constructor
            // (`EqSet` members, set-semantic identity). The round-trip is purely
            // structural, so the map preserves the member set either way.
            FrozenType.MkUnion [ FTConst("int", EqArray.empty); FTConst("string", EqArray.empty) ]
            // A structural literal type (string + int), external-vocabulary only.
            FTLiteral(LiteralConst.String "GET")
            FTLiteral(LiteralConst.Int 42L)
            // A literal union — the canonical `"ping" | "pong"` shape.
            FrozenType.MkUnion [ FTLiteral(LiteralConst.String "ping"); FTLiteral(LiteralConst.String "pong") ]
            // The carried type-level computations, in their mitt shapes: `keyof Events`,
            // `Events[Key]`, and `undefined extends Events[Key] ? Key : never`. The
            // bridge must round-trip them structurally (children carry declaring/method
            // typars), so include them in the oracle.
            FTKeyOf(FTTypar(TyparAxis.Declaring, 0))
            FTIndexedAccess(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Method, 0))
            FTConditional
                {
                    Check = FTConst("undefined", EqArray.empty)
                    Extends = FTIndexedAccess(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Method, 0))
                    WhenTrue = FTTypar(TyparAxis.Method, 0)
                    WhenFalse = FTConst("never", EqArray.empty)
                }
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
                // every expected case tag appears among the `ofFrozen` images.
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
                    | TyLiteral _ -> "TyLiteral"
                    | TyKeyOf _ -> "TyKeyOf"
                    | TyIndexedAccess _ -> "TyIndexedAccess"
                    | TyConditional _ -> "TyConditional"
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
                        "TyLiteral"
                        "TyKeyOf"
                        "TyIndexedAccess"
                        "TyConditional"
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

// `iterChildren2` pairs the members of an `FTOr` — a SET, so storage order is not a
// semantic invariant across instantiation. These pin the head-keyed fallback that
// recovers the pairing when the members line up NON-positionally (the case
// `ClrEncoder.recoverOpenTypars` rests on): a positional-only walk would silently
// mis-recover an open typar buried under a reordered union member.
[<Tests>]
let iterChildren2FTOrTests =
    // A minimal mirror of `ClrEncoder.recoverOpenTypars`' descent: record what each
    // method-axis `FTTypar` slot instantiates to as `iterChildren2` pairs children.
    let recoverMethodTypars (openT: FrozenType) (instT: FrozenType) =
        let recovered = System.Collections.Generic.Dictionary<int, FrozenType>()

        let rec go (d: FrozenType) (a: FrozenType) =
            match d with
            | FTTypar(TyparAxis.Method, i) -> recovered.[i] <- a
            | _ -> FrozenType.iterChildren2 go d a

        go openT instT
        recovered

    testList
        "FrozenType.iterChildren2 FTOr pairing"
        [
            test "recovers a typar buried under a REORDERED FTOr member by head key, not position" {
                let kBox = SymbolKeyOps.qualifiedTypeKey "Test.Box" 1
                // open template `Box<!!0> | int`; instantiated view `int | Box<string>`.
                // `EqSet` preserves insertion order, so the two are stored REORDERED —
                // a positional pairing would match `Box<!!0>` against `int` and lose the
                // typar; the head-keyed fallback pairs `Box` with `Box`.
                let openOr =
                    FrozenType.MkUnion
                        [
                            FTClass(kBox, EqArray.singleton (FTTypar(TyparAxis.Method, 0)))
                            FTConst("int", EqArray.empty)
                        ]

                let instOr =
                    FrozenType.MkUnion
                        [
                            FTConst("int", EqArray.empty)
                            FTClass(kBox, EqArray.singleton (FTConst("string", EqArray.empty)))
                        ]

                let recovered = recoverMethodTypars openOr instOr

                Expect.equal recovered.Count 1 "exactly the one method typar is recovered"

                Expect.equal
                    recovered.[0]
                    (FTConst("string", EqArray.empty))
                    "!!0 recovers to `string` via head-keyed pairing, not the positional `int`"
            }

            test "fails loudly when an open FTOr member's head matches TWO instantiated members" {
                let kBox = SymbolKeyOps.qualifiedTypeKey "Test.Box" 1
                // open `int | Box<!!0>`; instantiated `Box<string> | Box<float>`. Positional
                // heads mismatch (int vs Box) so the fallback runs; the concrete `int` open
                // member has no partner, and `Box<!!0>` matches BOTH instantiated members —
                // genuinely ambiguous, so guessing is a bug: fail.
                let openOr =
                    FrozenType.MkUnion
                        [
                            FTConst("int", EqArray.empty)
                            FTClass(kBox, EqArray.singleton (FTTypar(TyparAxis.Method, 0)))
                        ]

                let instOr =
                    FrozenType.MkUnion
                        [
                            FTClass(kBox, EqArray.singleton (FTConst("string", EqArray.empty)))
                            FTClass(kBox, EqArray.singleton (FTConst("float", EqArray.empty)))
                        ]

                Expect.throws
                    (fun () -> recoverMethodTypars openOr instOr |> ignore)
                    "an ambiguous head-keyed FTOr pairing must fail, not guess"
            }
        ]
