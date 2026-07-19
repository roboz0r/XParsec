module XParsec.FSharp.SemanticAnalysis.Tests.FrozenTypeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// `ofFrozen` mints local-typar metavars through an arena; the round-trip samples
// carry no `FTLocalTypar`, so this store stays empty — it only satisfies the seam.
let private store = TypeStore()

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
    let kRec = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
    let kUnion = SymbolKeyOps.qualifiedTypeKeyOf "Test.Option" 1
    let kClass = SymbolKeyOps.qualifiedTypeKeyOf "Test.Widget" 2
    // An enum is niladic (arity 0) — a leaf nominal carrying only its key.
    let kEnum = SymbolKeyOps.qualifiedTypeKeyOf "Test.Colour" 0

    // Leaves: every nullary / typar / unknown form.
    let leaves =
        [
            FTConst(RuntimeNames.intKey, EqArray.empty)
            FTConst(RuntimeNames.stringKey, EqArray.empty)
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
            FTConst(RuntimeNames.arrayKey 1, EqArray.singleton (FTConst(RuntimeNames.intKey, EqArray.empty)))
            FTFun(FTConst(RuntimeNames.intKey, EqArray.empty), FTTypar(TyparAxis.Method, 0))
            FTTuple(EqArray.ofList [ FTConst(RuntimeNames.intKey, EqArray.empty); FTTypar(TyparAxis.Declaring, 0) ])
            FTRecord(kRec, EqArray.singleton (FTTypar(TyparAxis.Declaring, 0)))
            FTUnion(kUnion, EqArray.singleton (FTConst(RuntimeNames.stringKey, EqArray.empty)))
            FTClass(kClass, EqArray.ofList [ FTTypar(TyparAxis.Declaring, 0); FTTypar(TyparAxis.Declaring, 1) ])
            // Anonymous (structural) union — built through the smart constructor
            // (`EqSet` members, set-semantic identity). The round-trip is purely
            // structural, so the map preserves the member set either way.
            FrozenType.MkUnion
                [
                    FTConst(RuntimeNames.intKey, EqArray.empty)
                    FTConst(RuntimeNames.stringKey, EqArray.empty)
                ]
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
                    Check = FTConst(RuntimeNames.undefinedKey, EqArray.empty)
                    Extends = FTIndexedAccess(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Method, 0))
                    WhenTrue = FTTypar(TyparAxis.Method, 0)
                    WhenFalse = FTConst(RuntimeNames.opaqueKey "never", EqArray.empty)
                }
        ]

    let branch2 =
        [
            // Curried fun nesting a tuple arg and a record result.
            FTFun(
                FTTuple(
                    EqArray.ofList
                        [
                            FTConst(RuntimeNames.intKey, EqArray.empty)
                            FTConst(RuntimeNames.boolKey, EqArray.empty)
                        ]
                ),
                FTFun(
                    FTRecord(kRec, EqArray.singleton (FTTypar(TyparAxis.Method, 0))),
                    FTConst(RuntimeNames.unitKey, EqArray.empty)
                )
            )
            // Generic intrinsic carrying a union carrying a class.
            FTConst(
                RuntimeNames.arrayKey 1,
                EqArray.singleton (
                    FTUnion(
                        kUnion,
                        EqArray.singleton (
                            FTClass(
                                kClass,
                                EqArray.ofList [ FTConst(RuntimeNames.intKey, EqArray.empty); FTUnknown "X" ]
                            )
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
                    Expect.equal (toFrozen (ofFrozen store ft)) ft (sprintf "round-trips: %A" ft)
            }

            test "toFrozen >> ofFrozen = id on the post-freeze SemType subset" {
                // Each `ofFrozen ft` is a representative of the post-freeze subset
                // (the cases `freeze` can legally produce — no `TyVar`).
                for ft in sampleFrozenTypes do
                    let ty = ofFrozen store ft
                    Expect.equal (ofFrozen store (toFrozen ty)) ty (sprintf "round-trips: %A" ty)
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

                let seen = sampleFrozenTypes |> List.map (ofFrozen store >> tag) |> Set.ofList

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
                let metavar = TyVar(TypeStore().NewTypeVar())

                Expect.throws
                    (fun () -> toFrozen metavar |> ignore)
                    "an inference metavar must not reach the frozen boundary"
            }

            test "toFrozen rejects a TyVar nested inside an otherwise-frozen shape" {
                let nested =
                    TyFun(TyConst(RuntimeNames.intKey, EqArray.empty), TyVar(TypeStore().NewTypeVar()))

                Expect.throws
                    (fun () -> toFrozen nested |> ignore)
                    "a buried metavar is still rejected (the recursion reaches it)"
            }
        ]

// `FrozenType.mapVariant` — the variance-tracking rebuild skeleton. These pin the
// variance ALGEBRA directly (a pure `FrozenType -> FrozenType`, no provider
// scaffolding) so every position rule is a one-liner and every constructor arm is
// reachable — including the type-level operators (`keyof`/indexed/conditional) a
// stored member signature can only awkwardly carry. The `mapProviderTypes` decorator
// tests cover only the SURFACE mapping (which field → which root variance); the number
// POLICY is tested end-to-end elsewhere. Here the leaf is a position-witness: a marker
// `FTConst("M", [])` is replaced by `FTConst("<co|contra|inv>", [])`, so the output
// records the variance at which the marker was reached.
[<Tests>]
let mapVariantTests =
    let marker = FTConst(RuntimeNames.opaqueKey "M", EqArray.empty)

    /// Replace the marker with a witness naming the variance it was reached at; defer
    /// (recurse) everywhere else.
    let witnessLeaf (v: Variance) (t: FrozenType) : FrozenType voption =
        match t with
        | FTConst(key, args) when args.Length = 0 && SymbolKeyOps.simpleName key = DisplayName "M" ->
            let name =
                match v with
                | Variance.Co -> "co"
                | Variance.Contra -> "contra"
                | Variance.Inv -> "inv"

            ValueSome(FTConst(RuntimeNames.opaqueKey name, EqArray.empty))
        | _ -> ValueNone

    /// The reachability sample from the round-trip oracle, reused to assert a
    /// `ValueNone` leaf is the identity on EVERY constructor (faithful recursion).
    let allShapes = sampleFrozenTypes

    let run v t = FrozenType.mapVariant witnessLeaf v t

    let witness name =
        FTConst(RuntimeNames.opaqueKey name, EqArray.empty)

    testList
        "FrozenType.mapVariant"
        [
            test "Variance.Flip: co↔contra, inv self-dual" {
                Expect.equal Variance.Co.Flip Variance.Contra "co flips to contra"
                Expect.equal Variance.Contra.Flip Variance.Co "contra flips to co"
                Expect.equal Variance.Inv.Flip Variance.Inv "inv is self-dual"
            }

            test "a ValueNone leaf is the identity on every constructor shape" {
                // The skeleton must rebuild faithfully — recursion changes nothing when
                // the leaf never fires (structural arms route `FTOr` through `MkUnion`,
                // which is idempotent on already-canonical input).
                for ft in allShapes do
                    Expect.equal (run Variance.Co ft) ft (sprintf "identity: %A" ft)
            }

            test "the root variance reaches a bare marker" {
                Expect.equal (run Variance.Co marker) (witness "co") "co root"
                Expect.equal (run Variance.Contra marker) (witness "contra") "contra root"
                Expect.equal (run Variance.Inv marker) (witness "inv") "inv root"
            }

            test "variance flips at an FTFun domain and is kept for the result" {
                // `M -> M` under Co: the parameter is contravariant, the result covariant.
                Expect.equal
                    (run Variance.Co (FTFun(marker, marker)))
                    (FTFun(witness "contra", witness "co"))
                    "domain contra, result co"
            }

            test "a doubly-nested FTFun domain flips back to the enclosing variance" {
                // `(M -> _) -> _` under Co: the outer domain is contra, so ITS domain
                // (the inner `M`) flips again to co.
                let t =
                    FTFun(
                        FTFun(marker, FTConst(RuntimeNames.unitKey, EqArray.empty)),
                        FTConst(RuntimeNames.unitKey, EqArray.empty)
                    )

                Expect.equal
                    (run Variance.Co t)
                    (FTFun(
                        FTFun(witness "co", FTConst(RuntimeNames.unitKey, EqArray.empty)),
                        FTConst(RuntimeNames.unitKey, EqArray.empty)
                    ))
                    "domain-of-domain is co again"
            }

            test "a generic type ARGUMENT drops to invariant regardless of enclosing variance" {
                // `[]<M>` (a generic intrinsic) at every root variance → the arg is inv.
                for root in [ Variance.Co; Variance.Contra; Variance.Inv ] do
                    Expect.equal
                        (run root (FTConst(RuntimeNames.arrayKey 1, EqArray.singleton marker)))
                        (FTConst(RuntimeNames.arrayKey 1, EqArray.singleton (witness "inv")))
                        (sprintf "arg is inv under %A root" root)
            }

            test "invariance dominates a contravariant enclosing position" {
                let kBox = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
                // `Box<M> -> M` under Co: the domain is contra, but `Box`'s ARG is a
                // generic slot → inv wins over the contra it sits inside; the result `M`
                // stays co.
                let t = FTFun(FTClass(kBox, EqArray.singleton marker), marker)

                Expect.equal
                    (run Variance.Co t)
                    (FTFun(FTClass(kBox, EqArray.singleton (witness "inv")), witness "co"))
                    "Box arg is inv, result is co"
            }

            test "FTClass / FTRecord / FTUnion arguments are invariant" {
                let k = SymbolKeyOps.qualifiedTypeKeyOf "Test.T" 1

                for mk in
                    [
                        (fun a -> FTClass(k, a))
                        (fun a -> FTRecord(k, a))
                        (fun a -> FTUnion(k, a))
                    ] do
                    Expect.equal
                        (run Variance.Co (mk (EqArray.singleton marker)))
                        (mk (EqArray.singleton (witness "inv")))
                        "nominal args are invariant"
            }

            test "structural operators carry the enclosing variance into their children" {
                // Tuple under Contra → every element contra.
                Expect.equal
                    (run Variance.Contra (FTTuple(EqArray.ofList [ marker; marker ])))
                    (FTTuple(EqArray.ofList [ witness "contra"; witness "contra" ]))
                    "tuple carries contra"

                // keyof / indexed / conditional under Co → children co.
                Expect.equal (run Variance.Co (FTKeyOf marker)) (FTKeyOf(witness "co")) "keyof carries co"

                Expect.equal
                    (run Variance.Co (FTIndexedAccess(marker, marker)))
                    (FTIndexedAccess(witness "co", witness "co"))
                    "indexed access carries co"

                Expect.equal
                    (run
                        Variance.Co
                        (FTConditional
                            {
                                Check = marker
                                Extends = marker
                                WhenTrue = marker
                                WhenFalse = marker
                            }))
                    (FTConditional
                        {
                            Check = witness "co"
                            Extends = witness "co"
                            WhenTrue = witness "co"
                            WhenFalse = witness "co"
                        })
                    "conditional carries co into all four branches"
            }

            test "an anonymous union carries variance and re-canonicalises through MkUnion" {
                // `M | int` under Contra: the marker becomes `contra`, and the set is
                // rebuilt through `MkUnion` (structural arm via `mapChildren`).
                let t = FrozenType.MkUnion [ marker; FTConst(RuntimeNames.intKey, EqArray.empty) ]

                Expect.equal
                    (run Variance.Contra t)
                    (FrozenType.MkUnion [ witness "contra"; FTConst(RuntimeNames.intKey, EqArray.empty) ])
                    "union member carried to contra"
            }

            test "childless leaves pass through untouched" {
                for leaf in
                    [
                        FTConst(RuntimeNames.intKey, EqArray.empty)
                        FTTypar(TyparAxis.Method, 0)
                        FTLiteral(LiteralConst.String "GET")
                        FTUnknown "X"
                        FTEnum(SymbolKeyOps.qualifiedTypeKeyOf "Test.Colour" 0)
                    ] do
                    Expect.equal (run Variance.Co leaf) leaf (sprintf "leaf unchanged: %A" leaf)
            }

            test "the leaf is consulted first at NON-leaf nodes and can own the whole subtree" {
                // A leaf that fires on an FTFun replaces it wholesale — recursion never
                // descends. Proves `leaf` gets first crack at every node, not just scalars.
                let sentinel = FTConst(RuntimeNames.opaqueKey "REPLACED", EqArray.empty)

                let funLeaf (_: Variance) (t: FrozenType) : FrozenType voption =
                    match t with
                    | FTFun _ -> ValueSome sentinel
                    | _ -> ValueNone

                Expect.equal
                    (FrozenType.mapVariant funLeaf Variance.Co (FTFun(marker, marker)))
                    sentinel
                    "the FTFun subtree is owned by the leaf, undescended"
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
                let kBox = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
                // open template `Box<!!0> | int`; instantiated view `int | Box<string>`.
                // `EqSet` preserves insertion order, so the two are stored REORDERED —
                // a positional pairing would match `Box<!!0>` against `int` and lose the
                // typar; the head-keyed fallback pairs `Box` with `Box`.
                let openOr =
                    FrozenType.MkUnion
                        [
                            FTClass(kBox, EqArray.singleton (FTTypar(TyparAxis.Method, 0)))
                            FTConst(RuntimeNames.intKey, EqArray.empty)
                        ]

                let instOr =
                    FrozenType.MkUnion
                        [
                            FTConst(RuntimeNames.intKey, EqArray.empty)
                            FTClass(kBox, EqArray.singleton (FTConst(RuntimeNames.stringKey, EqArray.empty)))
                        ]

                let recovered = recoverMethodTypars openOr instOr

                Expect.equal recovered.Count 1 "exactly the one method typar is recovered"

                Expect.equal
                    recovered.[0]
                    (FTConst(RuntimeNames.stringKey, EqArray.empty))
                    "!!0 recovers to `string` via head-keyed pairing, not the positional `int`"
            }

            test "fails loudly when an open FTOr member's head matches TWO instantiated members" {
                let kBox = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
                // open `int | Box<!!0>`; instantiated `Box<string> | Box<float>`. Positional
                // heads mismatch (int vs Box) so the fallback runs; the concrete `int` open
                // member has no partner, and `Box<!!0>` matches BOTH instantiated members —
                // genuinely ambiguous, so guessing is a bug: fail.
                let openOr =
                    FrozenType.MkUnion
                        [
                            FTConst(RuntimeNames.intKey, EqArray.empty)
                            FTClass(kBox, EqArray.singleton (FTTypar(TyparAxis.Method, 0)))
                        ]

                let instOr =
                    FrozenType.MkUnion
                        [
                            FTClass(kBox, EqArray.singleton (FTConst(RuntimeNames.stringKey, EqArray.empty)))
                            FTClass(kBox, EqArray.singleton (FTConst(RuntimeNames.floatKey, EqArray.empty)))
                        ]

                Expect.throws
                    (fun () -> recoverMethodTypars openOr instOr |> ignore)
                    "an ambiguous head-keyed FTOr pairing must fail, not guess"
            }
        ]
