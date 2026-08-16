module XParsec.FSharp.SemanticAnalysis.Tests.FrozenTypeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

// The arena `ofFrozen` mints local-typar metavars through. No sample carries an `FTLocalTypar`,
// so it stays empty and only satisfies the seam.
let private store = TypeStore()

// The `SemType` ↔ `FrozenType` round-trip oracle: `ofFrozen >> toFrozen = id` over the sample
// below, and `toFrozen >> ofFrozen = id` on its post-freeze `SemType` images. `TyVar` has no
// `FrozenType` counterpart and is a hard error at the boundary.

/// A deterministic, depth-bounded enumeration of `FrozenType` constructors, nested, over both
/// `TyparAxis`es — but no `FTLocalTypar`, whose `ofFrozen` image is a `TyVar` that `toFrozen`
/// then rejects.
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
            FTUnknown(UnknownReason.UndefinedName "Unresolved.Head")
            FTEnum kEnum
        ]

    // One level of every branching constructor over a couple of leaves; `branch2` then nests
    // branches inside branches, so the recursive `EqArray.map` arms are exercised.
    let branch1 =
        [
            FTConst(RuntimeNames.arrayKey 1, EqArray.singleton (FTConst(RuntimeNames.intKey, EqArray.empty)))
            FTFun(FTConst(RuntimeNames.intKey, EqArray.empty), FTTypar(TyparAxis.Method, 0))
            FTTuple(EqArray.ofList [ FTConst(RuntimeNames.intKey, EqArray.empty); FTTypar(TyparAxis.Declaring, 0) ])
            FTRecord(kRec, EqArray.singleton (FTTypar(TyparAxis.Declaring, 0)))
            FTUnion(kUnion, EqArray.singleton (FTConst(RuntimeNames.stringKey, EqArray.empty)))
            FTClass(kClass, EqArray.ofList [ FTTypar(TyparAxis.Declaring, 0); FTTypar(TyparAxis.Declaring, 1) ])
            // Anonymous union through the smart constructor: `EqSet` disjuncts, set identity.
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
            // The type-level operators, with typar children: `keyof E`, `E[K]`, and
            // `undefined extends E[K] ? K : never`.
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
                                EqArray.ofList
                                    [
                                        FTConst(RuntimeNames.intKey, EqArray.empty)
                                        FTUnknown(UnknownReason.UndefinedName "X")
                                    ]
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
                // Each `ofFrozen ft` represents the post-freeze subset: no `TyVar`.
                for ft in sampleFrozenTypes do
                    let ty = ofFrozen store ft
                    Expect.equal (ofFrozen store (toFrozen ty)) ty (sprintf "round-trips: %A" ty)
            }

            test "every post-freeze SemType case is covered by the sample" {
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

// `FrozenType.mapVariant` — the variance algebra, pinned as a pure `FrozenType -> FrozenType`.
// The replacement is a position-witness: a marker `FTConst("M", [])` is replaced by
// `FTConst("co"|"contra"|"inv", [])`, so the output records where the marker was reached.
[<Tests>]
let mapVariantTests =
    let marker = FTConst(RuntimeNames.opaqueKey "M", EqArray.empty)

    let witnessVariance (v: Variance) (t: FrozenType) : FrozenType voption =
        match t with
        | FTConst(key, args) when args.Length = 0 && SymbolKeyOps.typeSimpleName key = DisplayName "M" ->
            let name =
                match v with
                | Variance.Co -> "co"
                | Variance.Contra -> "contra"
                | Variance.Inv -> "inv"

            ValueSome(FTConst(RuntimeNames.opaqueKey name, EqArray.empty))
        | _ -> ValueNone

    /// Reused from the round-trip oracle: reaches every constructor.
    let allShapes = sampleFrozenTypes

    let run v t =
        FrozenType.mapVariant witnessVariance v t

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

            test "a ValueNone replacement is the identity on every constructor shape" {
                // `FTOr` rebuilds through `MkUnion`, idempotent on already-canonical input, so a
                // never-firing replacement is the identity there too.
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
                // `Box<M> -> M` under Co: the domain is contra, but `Box`'s ARG is a generic
                // slot → inv wins over the contra it sits inside; the result `M` stays co.
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
                    "union disjunct carried to contra"
            }

            test "childless leaves pass through untouched" {
                for childless in
                    [
                        FTConst(RuntimeNames.intKey, EqArray.empty)
                        FTTypar(TyparAxis.Method, 0)
                        FTLiteral(LiteralConst.String "GET")
                        FTUnknown(UnknownReason.UndefinedName "X")
                        FTEnum(SymbolKeyOps.qualifiedTypeKeyOf "Test.Colour" 0)
                    ] do
                    Expect.equal (run Variance.Co childless) childless (sprintf "unchanged: %A" childless)
            }

            test "tryReplace is consulted first at INTERIOR nodes and can own the whole subtree" {
                // Firing on an FTFun replaces it wholesale — recursion never descends. Proves
                // `tryReplace` gets first crack at every node, not just childless ones.
                let sentinel = FTConst(RuntimeNames.opaqueKey "REPLACED", EqArray.empty)

                let replaceFun (_: Variance) (t: FrozenType) : FrozenType voption =
                    match t with
                    | FTFun _ -> ValueSome sentinel
                    | _ -> ValueNone

                Expect.equal
                    (FrozenType.mapVariant replaceFun Variance.Co (FTFun(marker, marker)))
                    sentinel
                    "the FTFun subtree is replaced, undescended"
            }
        ]

// `iterChildren2` pairs the disjuncts of an `FTOr` — a SET, so storage order is not preserved
// across instantiation, and a positional-only walk would mis-recover an open typar buried
// under a reordered disjunct. These pin the tyctor-keyed fallback that recovers the pairing.
[<Tests>]
let iterChildren2FTOrTests =
    // A minimal mirror of the CLR encoder's open-typar recovery: record what each method-axis
    // `FTTypar` slot instantiates to as `iterChildren2` pairs children.
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
            test "recovers a typar buried under a REORDERED FTOr disjunct by tyctor key, not position" {
                let kBox = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
                // Open `Box<!!0> | int` against instantiated `int | Box<string>`: `EqSet` keeps
                // insertion order, so a positional pairing would match `Box<!!0>` with `int` and
                // lose the typar. The tyctor-keyed fallback pairs `Box` with `Box`.
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
                    "!!0 recovers to `string` via tyctor-keyed pairing, not the positional `int`"
            }

            test "fails loudly when an open FTOr disjunct's type constructor matches TWO instantiated ones" {
                let kBox = SymbolKeyOps.qualifiedTypeKeyOf "Test.Box" 1
                // Open `int | Box<!!0>` against `Box<string> | Box<float>`: positional type
                // constructors mismatch so the fallback runs, and `Box<!!0>` then matches BOTH
                // instantiated disjuncts. Genuinely ambiguous, so guessing would be a bug.
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
                    "an ambiguous tyctor-keyed FTOr pairing must fail, not guess"
            }
        ]
