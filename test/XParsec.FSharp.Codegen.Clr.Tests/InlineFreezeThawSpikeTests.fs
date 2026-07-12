module XParsec.FSharp.Codegen.Clr.Tests.InlineFreezeThawSpikeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The landing checks for the freeze-inline prerequisites
// (docs/inline-body-freeze-thaw-plan.md).
//
// 1. A frozen `StaticOptimization` carries NO `SemType`. The clause CONSTRAINTS used
//    to be the hole: `TStaticOptClauseG.Constraints` was a monomorphic
//    `EqArray<TStaticOptConstraint>` — raw `SemType`, whose typar is a LIVE `TyVar`
//    over the inline binding's quantified root — and `TastConvert.clause` copied it
//    verbatim, so a frozen inline decl still shared the pre-freeze `UnionFind` cell.
//    `TStaticOptConstraintG<'ty>` closes that: a frozen clause is `SemType`-free BY
//    TYPE, and this test pins that the constraints are actually MAPPED, not dropped.
//
// 2. The body-local typar residue keeps its IDENTITY across freeze. Freeze's `TyVar`
//    policy no longer collapses every un-quantified root to one name-equal
//    `FTUnknown "?free-typar"`; each root is attributed to the local scheme that
//    BINDS it and freezes to `FTLocalTypar(binder, index)`; a decl-scoped thaw mints
//    one fresh cell per `(binder, index)` pair.
//
// 3. That identity is BODY-RELATIVE and survives `NodeKey` collision across units —
//    the multi-file case (docs/multi-file-compilation-units-plan.md), where a
//    `NodeKey` has no file id and keys from two units collide freely.

/// Every `TypeVar` root reachable from a *SemType* clause's constraints, zonked.
/// (There is no frozen counterpart by construction — that is the point of check 1.)
let private constraintRoots (clauses: TStaticOptClause list) : TypeVar list =
    [
        for c in clauses do
            for k in EqArray.toList c.Constraints do
                match k with
                | TStaticOptConstraint.TyconEquals(TyVar a, _) -> yield UnionFind.find a
                | TStaticOptConstraint.TyconEquals(_, TyVar b) -> yield UnionFind.find b
                | TStaticOptConstraint.IsStruct(TyVar a) -> yield UnionFind.find a
                | _ -> ()
    ]

/// Every type mentioned by a frozen clause's constraints, in order.
let private frozenConstraintTypes (clauses: Frozen.TStaticOptClause list) : FrozenType list =
    [
        for c in clauses do
            for k in EqArray.toList c.Constraints do
                match k with
                | TStaticOptConstraintG.TyconEquals(tp, req) ->
                    yield tp
                    yield req
                | TStaticOptConstraintG.IsStruct tp -> yield tp
    ]

/// Every `FTLocalTypar` leaf in a frozen type, in first-occurrence pre-order.
let rec private localLeavesIn (t: FrozenType) : (NodeKey * int) list =
    match t with
    | FTLocalTypar(binder, i) -> [ binder, i ]
    | t ->
        let acc = ResizeArray<NodeKey * int>()
        FrozenType.iterChildren (fun c -> acc.AddRange(localLeavesIn c)) t
        List.ofSeq acc

let rec private hasFTTypar (t: FrozenType) : bool =
    match t with
    | FTTypar _ -> true
    | t -> FrozenType.existsChild hasFTTypar t

/// Every `TypeVar` root in a `SemType`, in first-occurrence pre-order.
let rec private semRootsOf (t: SemType) : TypeVar list =
    match t with
    | TyVar tv -> [ UnionFind.find tv ]
    | t ->
        let acc = ResizeArray<TypeVar>()
        SemType.iterChildren (fun c -> acc.AddRange(semRootsOf c)) t
        List.ofSeq acc

/// Run over every `.ty` slot of a decl, collecting them. `TastConvert` is the
/// exhaustive functor, so this reaches every type in the tree — no hand-rolled walk
/// that a new TAST case could silently escape.
let private collectTys (d: TDeclG<'ty, 'tok>) : 'ty list =
    let acc = ResizeArray<'ty>()

    TastConvert.decl
        (fun ty ->
            acc.Add ty
            ty
        )
        d
    |> ignore

    List.ofSeq acc

/// Dedupe `TypeVar`s by REFERENCE — a `TypeVar` is a mutable cell, and cell identity
/// is what these tests are about.
let private distinctCells (tvs: TypeVar list) : TypeVar list =
    let acc = ResizeArray<TypeVar>()

    for tv in tvs do
        if not (acc |> Seq.exists (fun seen -> System.Object.ReferenceEquals(seen, tv))) then
            acc.Add tv

    List.ofSeq acc

/// A single DECL-SCOPED thaw — the shape the inline-splice consumer will use: ONE
/// freshener cache across the whole decl, keyed on the `(binder, index)` PAIR, so
/// every occurrence of one local typar maps to one fresh consumer-owned cell and two
/// distinct local typars never share one. Deliberately consults NO ambient state:
/// the leaf is interpreted against the body that carries it and nothing else.
let private thawDeclScoped (d: Frozen.TDecl) : TDecl =
    let cache = System.Collections.Generic.Dictionary<struct (NodeKey * int), SemType>()

    TastConvert.decl
        (FrozenTypeBridge.instantiateWith
            (fun i -> TyTypar(TyparAxis.Declaring, i))
            (fun j -> TyTypar(TyparAxis.Method, j))
            (fun binder k ->
                let key = struct (binder, k)

                match cache.TryGetValue key with
                | true, v -> v
                | _ ->
                    let v = TyVar(TypeVar())
                    cache.[key] <- v
                    v
            ))
        d

/// The frozen `let` decl of a single-binding program, plus its own declared type.
let private frozenLetDecl (src: string) : Frozen.TDecl =
    let ctx, tast = analyseWithCtx src
    Expect.isEmpty tast.Diagnostics "no diagnostics"

    (Freeze.run ctx tast).Decls
    |> EqArray.toList
    |> List.tryPick (fun d ->
        match d with
        | TDeclG.Let(TPatG.NamedSimple _, _, _, _) as d -> Some d
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtestf "no top-level `let` in the frozen tree of:\n%s" src)

/// Two locally-generalized `let`s. ONE would give a single local root and so could
/// not witness conflation — the old `FTUnknown "?free-typar"` gave every root the
/// same NAME, and `FTUnknown` equality is by name, so they collapsed into one
/// indistinguishable leaf.
///
/// `inline` is deliberately absent: `Freeze.run` still DROPS inline decls
/// (freeze-inline is the next stage), so an inline binding would never reach the
/// policy under test. The residue does not depend on `inline` — it is the local
/// generalisation that creates it (see `Freeze.declFreezer`).
let private twoLocalSchemes =
    String.concat
        "\n"
        [
            "let f () ="
            "    let g = fun x -> x"
            "    let h = fun y -> y"
            "    (g, g, h, h)"
        ]

[<Tests>]
let tests =
    testList
        "InlineFreezeThawSpike"
        [
            test "freeze-inline: a frozen StaticOptimization clause carries no SemType cell — constraints freeze too" {
                // The known-good SRTP inline shape (identical to StaticOptimizationTests):
                // a `when ^T : …` cascade that elaborates to a `TExpr.StaticOptimization`.
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline kindOf (x: ^T) : int ="
                            "    -1"
                            "    when ^T : int   = 1"
                            "    when ^T : float = 2"
                            "    when ^T : ^T    = 0"
                        ]

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                // Pre-freeze SemType inline decl (retained — `Freeze.run` drops inline
                // decls today, so the freeze conversion is driven directly below).
                let inlineDecl =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (fun d ->
                        match d with
                        | TDeclG.Let(_, _, true, _) -> Some d
                        | _ -> None
                    )
                    |> Option.defaultWith (fun () -> failtest "no inline decl in tast.Decls")

                let semClauses =
                    match inlineDecl with
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.StaticOptimization(cls, _, _, _), _, _), _, _) ->
                        EqArray.toList cls
                    | _ -> failtestf "expected a static-opt inline binding, got %A" inlineDecl

                // (1) PREMISE (re-anchoring StaticOptimizationTests): pre-freeze, the clause
                //     constraints hold a LIVE TyVar root — the cell freeze must not leak.
                let semRoots = constraintRoots semClauses
                Expect.isNonEmpty semRoots "pre-freeze: a clause constraint carries a live TyVar root"

                // (2) Freeze the whole decl through the functor. `onVar` stands in for the
                //     real quantEnv (`Elaborate.mkMethodQuantEnv`): this pins
                //     REPRESENTABILITY, not the typar index order, so a single placeholder
                //     leaf for the lone `^T` is faithful enough.
                let onVar (_: SemType) : FrozenType = FTTypar(TyparAxis.Method, 0)

                let frozenDecl: Frozen.TDecl =
                    TastConvert.decl (FrozenTypeBridge.toFrozenWith onVar) inlineDecl

                let frozenClauses, frozenResultTy =
                    match frozenDecl with
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.StaticOptimization(cls, _, resultTy, _), _, _), _, _) ->
                        EqArray.toList cls, resultTy
                    | _ -> failtestf "freeze lost the static-opt shape: %A" frozenDecl

                Expect.equal frozenClauses.Length semClauses.Length "all when-clauses survive freeze"

                match frozenResultTy with
                | FTConst _ -> () // `: int`
                | other -> failtestf "expected a frozen result type, got %A" other

                // (3) THE LANDING CHECK. The frozen clause's constraints are
                //     `TStaticOptConstraintG<FrozenType>`: they cannot hold a `SemType`, so
                //     the shared-cell hazard is gone BY TYPE. What is left to check
                //     dynamically is that they were MAPPED and not dropped — every
                //     constraint type is present and went through `onVar`, i.e. the live
                //     `^T` root became the frozen leaf.
                let frozenTys = frozenConstraintTypes frozenClauses

                Expect.equal
                    frozenTys.Length
                    (semClauses
                     |> List.sumBy (fun c ->
                         c.Constraints
                         |> EqArray.toList
                         |> List.sumBy (fun k ->
                             match k with
                             | TStaticOptConstraint.TyconEquals _ -> 2
                             | TStaticOptConstraint.IsStruct _ -> 1
                         )
                     ))
                    "every clause constraint survives the freeze conversion"

                Expect.isTrue
                    (frozenTys |> List.exists (fun t -> t = FTTypar(TyparAxis.Method, 0)))
                    "the constraint's typar was routed through the freeze policy — it is a frozen leaf, not a copied SemType cell"
            }

            test "freeze: two body-local schemes freeze to leaves with DISTINCT binders, and thaw to two distinct cells" {
                let fDecl = frozenLetDecl twoLocalSchemes

                let declTy =
                    match fDecl with
                    | TDeclG.Let(_, _, _, ty) -> ty
                    | _ -> failtest "unreachable"

                // `g`'s `'x` and `h`'s `'y` are each bound by their OWN local scheme, so
                // neither occurs in `f`'s type. They are two typars, and — the point of
                // carrying `binder` — they are distinguished by their BINDER, not merely by
                // an index that happens to differ.
                let leaves = collectTys fDecl |> List.collect localLeavesIn

                Expect.isNonEmpty leaves "the body-local schemes' own roots reach freeze as FTLocalTypar"

                let binders = leaves |> List.map fst |> List.distinct

                Expect.equal
                    binders.Length
                    2
                    "two locally-generalized lets ⇒ leaves with two DISTINCT binder NodeKeys (the old FTUnknown name conflated them)"

                Expect.equal
                    (leaves |> List.distinct |> List.length)
                    2
                    "…and two distinct (binder, index) pairs — each local scheme quantifies exactly one typar here"

                // The USE-SITE instantiations — the four occurrences in `(g, g, h, h)` — ARE
                // in `f`'s type, so `mkMethodQuantEnv` maps them and they ride the ordinary
                // declared axis. `f`'s own type therefore carries no residue at all.
                Expect.isTrue (hasFTTypar declTy) "f's own type names its use-site instantiations on the FTTypar axis"

                Expect.isEmpty
                    (localLeavesIn declTy)
                    "f's own type carries no local-typar residue — that is exactly why mkMethodQuantEnv cannot map it"

                // One decl-scoped thaw: one fresh cell per (binder, index), shared across
                // every occurrence of that leaf.
                let cells =
                    thawDeclScoped fDecl |> collectTys |> List.collect semRootsOf |> distinctCells

                Expect.equal
                    cells.Length
                    2
                    "a decl-scoped thaw mints exactly one fresh cell per distinct (binder, index) — two, shared across every occurrence"
            }

            test "freeze: local-typar leaves are DETERMINISTIC — the same source freezes to the same (binder, index)s" {
                // The sidecar/publishing path (docs/publishing-format-plan.md) may serialize a
                // frozen body and re-read it, so index stability rests on the freeze walk
                // order being deterministic. It is today; nothing but this test enforces it.
                let once = frozenLetDecl twoLocalSchemes |> collectTys |> List.collect localLeavesIn

                let twice =
                    frozenLetDecl twoLocalSchemes |> collectTys |> List.collect localLeavesIn

                Expect.isNonEmpty once "the fixture actually produces local typars"

                Expect.equal
                    twice
                    once
                    "two freezes of the same source yield identical (binder, index) leaves, in the same order"
            }

            test "freeze/thaw: colliding binder NodeKeys across two units do not conflate — the leaf is BODY-relative" {
                // The multi-file hazard, made concrete. A `NodeKey` is (offset, kind) with NO
                // file id, so two units' keys collide freely — deliberately (cross-file
                // references resolve by NAME, never by NodeKey). These two units are DIFFERENT
                // programs whose text is length-aligned character for character, so their
                // local-`let` binders land on the SAME NodeKey.
                //
                // SCOPE: this exercises the freeze→thaw half, which is what exists today.
                // It does NOT drive a real cross-unit SPLICE: `Freeze.run` still drops inline
                // decls, `InlineBody.Decl` is still `SemType`, and the splice machinery
                // (`Inline`/`InlineExpansion`) reads pre-freeze bodies — those are the
                // producer/consumer stages this plan schedules next. What is checkable now is
                // the property the splice will rest on: a thaw interprets a leaf against the
                // BODY that carries it and consults no ambient unit state, so equal binder
                // keys in two bodies stay two independent typars.
                let producer =
                    String.concat "\n" [ "let a () ="; "    let p = fun x -> x"; "    (p, p)" ]

                let consumer =
                    String.concat "\n" [ "let b () ="; "    let q = fun y -> y"; "    (q, q)" ]

                let pDecl = frozenLetDecl producer
                let cDecl = frozenLetDecl consumer

                let pLeaves = collectTys pDecl |> List.collect localLeavesIn |> List.distinct
                let cLeaves = collectTys cDecl |> List.collect localLeavesIn |> List.distinct

                Expect.equal pLeaves.Length 1 "the producer unit has one local scheme"
                Expect.equal cLeaves.Length 1 "the consumer unit has one local scheme"

                // The collision is REAL — assert it, or the test proves nothing.
                Expect.equal
                    (fst pLeaves.Head)
                    (fst cLeaves.Head)
                    "the two units' local binders collide on the same NodeKey (no file id in a NodeKey — by design)"

                Expect.equal
                    pLeaves.Head
                    cLeaves.Head
                    "…so the two frozen leaves are structurally EQUAL across units. That is not a bug: a frozen typar leaf is only ever interpreted against the template carrying it, exactly as FTTypar(Declaring, 0) is."

                // The consumer's own live inference cells, before anything is thawed into it.
                let consumerOwnCells =
                    let tast = analyse consumer

                    tast.Decls
                    |> EqArray.toList
                    |> List.collect collectTys
                    |> List.collect semRootsOf
                    |> distinctCells

                // Each body is thawed with its OWN decl-scoped cache (design constraint: one
                // cache per thawed decl). Despite the identical binder key, the two thaws mint
                // independent cells — nothing is keyed by NodeKey in any shared table.
                let pCells =
                    thawDeclScoped pDecl |> collectTys |> List.collect semRootsOf |> distinctCells

                let cCells =
                    thawDeclScoped cDecl |> collectTys |> List.collect semRootsOf |> distinctCells

                Expect.equal pCells.Length 1 "the producer body thaws to one fresh cell"
                Expect.equal cCells.Length 1 "the consumer body thaws to one fresh cell"

                Expect.isFalse
                    (System.Object.ReferenceEquals(pCells.Head, cCells.Head))
                    "the colliding binder key does NOT conflate the two units' local typars — each thaw mints its own cell"

                Expect.isEmpty
                    (distinctCells (pCells @ consumerOwnCells)
                     |> List.filter (fun c ->
                         consumerOwnCells |> List.exists (fun o -> System.Object.ReferenceEquals(o, c))
                         && pCells |> List.exists (fun p -> System.Object.ReferenceEquals(p, c))
                     ))
                    "the producer's thawed cells are fresh — none is a cell of the consumer's own inference state, colliding key notwithstanding"
            }
        ]
