module XParsec.FSharp.Codegen.Clr.Tests.InlineFreezeThawSpikeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The frozen inline-body channel, end to end.
//
// 1. `Freeze` PUBLISHES an inline binding (it drops it from the emittable `Decls`, and
//    that is a different question from whether it is part of the unit's vocabulary), under
//    a `SymbolKey` it MINTS from the binding's declaring module chain. An inline value is
//    the one kind of symbol that is exported but never emitted, so nothing else would ever
//    mint its identity.
//
// 2. A published body carries NO `SemType`. The clause CONSTRAINTS used to be the hole:
//    `TStaticOptClauseG.Constraints` was a monomorphic `EqArray<TStaticOptConstraint>` —
//    raw `SemType`, whose typar is a LIVE `TyVar` — and `TastConvert.clause` copied it
//    verbatim, so a frozen inline decl still shared the pre-freeze `UnionFind` cell.
//    `TStaticOptConstraintG<'ty>` closes that BY TYPE; what is left to check dynamically
//    is that the constraints are MAPPED, not dropped.
//
// 3. The body-local typar residue keeps its IDENTITY across freeze: each un-quantified
//    root is attributed to the local scheme that BINDS it and freezes to
//    `FTLocalTypar(binder, index)`; `Inline.thawBody` mints one fresh cell per
//    `(binder, index)` pair.
//
// 4. That identity is BODY-RELATIVE and survives `NodeKey` collision across units — the
//    multi-file case, where a `NodeKey` has no file id and keys from two units collide
//    freely. Both the freeze/thaw half and a REAL cross-unit splice are pinned below.

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

/// Every typar leaf of a frozen type — quantified (`FTTypar`) or body-local
/// (`FTLocalTypar`) — in first-occurrence pre-order. The leaf value IS its identity.
let rec private typarLeavesIn (t: FrozenType) : FrozenType list =
    match t with
    | FTTypar _
    | FTLocalTypar _ -> [ t ]
    | t ->
        let acc = ResizeArray<FrozenType>()
        FrozenType.iterChildren (fun c -> acc.AddRange(typarLeavesIn c)) t
        List.ofSeq acc

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

/// How many DISTINCT typar leaves a frozen decl names, across all three axes. The
/// thaw's contract in one number: it must mint exactly this many cells — one per
/// leaf, shared across every occurrence of that leaf. Deriving the count from the
/// frozen tree rather than hard-coding it is what keeps the assertion EXACT: a broken
/// cache mints MORE cells than there are leaves, which a `>=` bound would not catch.
let private distinctLeafCount (d: Frozen.TDecl) : int =
    collectTys d |> List.collect typarLeavesIn |> List.distinct |> List.length

/// The frozen unit of a source.
let private freeze (src: string) : Frozen.TastFile =
    let ctx, tast = analyseWithCtx src
    Expect.isEmpty tast.Diagnostics "no diagnostics"
    Freeze.run ctx tast

/// The frozen `let` decl of a single-binding program.
let private frozenLetDecl (src: string) : Frozen.TDecl =
    (freeze src).Decls
    |> EqArray.toList
    |> List.tryPick (fun d ->
        match d with
        | TDeclG.Let(TPatG.NamedSimple _, _, _, _) as d -> Some d
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtestf "no top-level `let` in the frozen tree of:\n%s" src)

/// The unit's sole published inline body.
let private soleInlineBody (src: string) : Frozen.TInlineValue =
    match (freeze src).InlineBodies |> EqArray.toList with
    | [ v ] -> v
    | other -> failtestf "expected exactly one published inline body, got %d, in:\n%s" (List.length other) src

/// Two locally-generalized `let`s. ONE would give a single local root and so could
/// not witness conflation — the old `FTUnknown "?free-typar"` gave every root the
/// same NAME, and `FTUnknown` equality is by name, so they collapsed into one
/// indistinguishable leaf.
///
/// `inline` is deliberately absent: the residue does not depend on it — it is the local
/// generalisation that creates it.
let private twoLocalSchemes =
    String.concat
        "\n"
        [
            "let f () ="
            "    let g = fun x -> x"
            "    let h = fun y -> y"
            "    (g, g, h, h)"
        ]

/// A module-held `let inline` — the only shape with a declaring holder chain, hence an
/// exportable identity, hence a vocabulary entry. (A top-level inline lives in the
/// anonymous Program holder and is spliceable only within its own unit.)
let private kindOfUnit (ns: string) (moduleName: string) =
    String.concat
        "\n"
        [
            "namespace " + ns
            ""
            "module " + moduleName + " ="
            "    let inline kindOf (x: ^T) : int ="
            "        -1"
            "        when ^T : int   = 1"
            "        when ^T : float = 2"
            "        when ^T : ^T    = 0"
        ]

/// Re-express a published body's decl type as a symbol `Scheme`. A `let inline`'s own
/// typars ride the METHOD axis (they are the binding's, not an enclosing type's); an
/// `ExternalSymbol.Scheme` bakes a free function's typars on the DECLARING axis, which is
/// what `instantiateSymbol` freshens. Positional, so index order is preserved.
let private asSymbolScheme (ft: FrozenType) : FrozenType =
    FrozenTypeBridge.instantiateWith
        (fun i -> TyTypar(TyparAxis.Declaring, i))
        (fun j -> TyTypar(TyparAxis.Declaring, j))
        (FrozenTypeBridge.localTyparInTemplate "InlineFreezeThawSpikeTests.asSymbolScheme")
        ft
    |> toFrozen

/// The number of distinct typar slots a template names — the symbol's `TyparArity`.
let rec private typarArity (ft: FrozenType) : int =
    match ft with
    | FTTypar(_, i) -> i + 1
    | t ->
        let mutable n = 0
        FrozenType.iterChildren (fun c -> n <- max n (typarArity c)) t
        n

/// Unit A's inline vocabulary, published as a provider over the default contract stack —
/// a multi-file provider in miniature: one `ExternalSymbol` per published body, with the
/// body FOLDED ONTO it (that is the whole interface; there is no sibling body channel).
///
/// The consumer resolves the symbol, carries its `Key`, and reaches the body through THAT
/// key. Nothing `SemType` crosses: A published `FrozenType`, and B thaws.
let private publishing (unitASource: string) : IExternalSymbolProvider =
    let ctx, tastA = analyseWithCtx unitASource
    Expect.isEmpty tastA.Diagnostics "unit A has no diagnostics"
    let unitA = Freeze.run ctx tastA

    let published = unitA.InlineBodies |> EqArray.toList

    let bodies = published |> List.map (fun v -> v.Key, v.Body) |> dict

    let symbols =
        published
        |> List.map (fun v ->
            let declTy =
                match v.Body.Decl with
                | TDeclG.Let(_, _, _, ty) -> ty
                | other -> failtestf "a published body is not a `let`: %A" other

            let binding =
                match v.Key with
                | SymbolKey.Binding b -> b
                | other -> failtestf "a published inline value is not a binding key: %A" other

            let scheme = asSymbolScheme declTy

            SymbolKeyOps.qualifiedName v.Key,
            ExternalSymbols.scheme binding.Decl binding.Name scheme (typarArity scheme) []
        )
        |> dict

    ExternalSymbolProviders.composite
        [
            ExternalSymbolProviders.ofNamedLeaf
                { ExternalSymbolProviders.NamedLeaf.empty with
                    TryLookup =
                        fun name ->
                            match symbols.TryGetValue name with
                            | true, s -> ValueSome s
                            | _ -> ValueNone
                }
            ClrSymbolProviders.buildContract defaultManifests
        ]
    |> ExternalSymbolProviders.withInlineBodies (fun k ->
        match bodies.TryGetValue k with
        | true, b -> ValueSome b
        | _ -> ValueNone
    )

/// The constant a splice left behind at `let r = …`. `kindOf`'s clause bodies are bare
/// `int` literals, so WHICH clause was selected is read straight off the spliced value.
///
/// The value is `let x = <arg> in <clause body>` — the inline's parameter beta-reduced to
/// an ordinary `Let`, exactly as an in-unit splice lowers it. A call that did NOT splice
/// leaves an `App` head instead, which reaches no `Const`, so this cannot pass by accident.
let private splicedConst (provider: IExternalSymbolProvider) (src: string) : int64 =
    let lexed, file = parseFile src
    let tast = Pipeline.analyse provider src lexed file

    Expect.isEmpty
        (tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error))
        (sprintf "no errors for:\n%s" src)

    let rec result (e: Frozen.TExpr) : int64 =
        match e with
        | TExprG.Let(_, _, body, _, _) -> result body
        | TExprG.Const(TConstValue.Integral(_, v), _, _) -> v
        | other -> failtestf "expected `r` to reduce to a spliced constant, got %A" other

    match tast.Decls |> EqArray.toList |> List.rev with
    | TDeclG.Let(_, value, _, _) :: _ -> result value
    | other -> failtestf "expected a trailing `let r = …`, got %A" other

[<Tests>]
let tests =
    testList
        "InlineFreezeThaw"
        [
            test "freeze PUBLISHES an inline binding under a minted key, and keeps it out of the emittable Decls" {
                let frozen = freeze (kindOfUnit "Lib" "Kinds")

                Expect.isEmpty
                    (frozen.Decls
                     |> EqArray.toList
                     |> List.filter (fun d ->
                         match d with
                         | TDeclG.Let(isInline = true) -> true
                         | _ -> false
                     ))
                    "an inline template is not emittable — it stays out of Decls (no backend can lower one)"

                let published = soleInlineBody (kindOfUnit "Lib" "Kinds")

                // The key is MINTED from the declaring holder chain — not recovered by
                // re-resolving a dotted spelling, which multi-file has nothing to recover
                // against. It is the identity a use-site `TExpr.External` carries.
                Expect.equal
                    published.Key
                    (SymbolKeyOps.moduleValueKey "Lib" "Kinds" "kindOf")
                    "the published identity is the binding's own containment chain"
            }

            test "a published StaticOptimization clause carries no SemType cell — its constraints freeze too" {
                let published = soleInlineBody (kindOfUnit "Lib" "Kinds")

                let clauses, resultTy =
                    match published.Body.Decl with
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.StaticOptimization(cls, _, resultTy, _), _, _), _, _) ->
                        EqArray.toList cls, resultTy
                    | other -> failtestf "freeze lost the static-opt shape: %A" other

                Expect.equal clauses.Length 3 "all three when-clauses survive freeze"

                match resultTy with
                | FTConst _ -> () // `: int`
                | other -> failtestf "expected a frozen result type, got %A" other

                // The clause's constraints are `TStaticOptConstraintG<FrozenType>`: they
                // CANNOT hold a `SemType`, so the shared-cell hazard is gone by type. What
                // is checkable dynamically is that they were MAPPED and not dropped — and
                // that the binding's own `^T` reached its self-describing leaf.
                let frozenTys = frozenConstraintTypes clauses

                // `int` / `float` / `^T : ^T` ⇒ two types per clause.
                Expect.equal frozenTys.Length 6 "every clause constraint survives the freeze conversion"

                Expect.isTrue
                    (frozenTys |> List.exists (fun t -> t = FTTypar(TyparAxis.Method, 0)))
                    "the constraint's typar is a frozen leaf, not a copied SemType cell"

                Expect.isEmpty
                    (frozenTys |> List.collect localLeavesIn)
                    "the binding's own typar is QUANTIFIED (FTTypar), never mistaken for a body-local residue"
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

                // One decl-scoped thaw: one fresh cell per distinct leaf, shared across every
                // occurrence of it. `thawBody` mints on all three axes, so the expected count
                // is every leaf the frozen decl names — not just the local ones.
                let cells =
                    Inline.thawBody fDecl |> collectTys |> List.collect semRootsOf |> distinctCells

                Expect.equal
                    cells.Length
                    (distinctLeafCount fDecl)
                    "a decl-scoped thaw mints EXACTLY one fresh cell per distinct leaf — sharing it across every occurrence"
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
                // references resolve by NAME / by KEY, never by NodeKey). These two units are
                // DIFFERENT programs whose text is length-aligned character for character, so
                // their local-`let` binders land on the SAME NodeKey.
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
                    Inline.thawBody pDecl |> collectTys |> List.collect semRootsOf |> distinctCells

                let cCells =
                    Inline.thawBody cDecl |> collectTys |> List.collect semRootsOf |> distinctCells

                let disjointFrom (xs: TypeVar list) (ys: TypeVar list) =
                    ys
                    |> List.filter (fun y -> xs |> List.exists (fun x -> System.Object.ReferenceEquals(x, y)))

                Expect.equal
                    pCells.Length
                    (distinctLeafCount pDecl)
                    "the producer body thaws to exactly one fresh cell per distinct leaf"

                Expect.equal
                    cCells.Length
                    (distinctLeafCount cDecl)
                    "the consumer body thaws to exactly one fresh cell per distinct leaf"

                Expect.isEmpty
                    (disjointFrom pCells cCells)
                    "the colliding binder key does NOT conflate the two units' local typars — each thaw mints its own cells"

                Expect.isEmpty
                    (disjointFrom consumerOwnCells pCells)
                    "the producer's thawed cells are fresh — none is a cell of the consumer's own inference state, colliding key notwithstanding"
            }

            // ─── Cross-unit SPLICE: freeze in A, splice in B ────────────────────────────
            //
            // The property the whole channel exists for. Unit A is compiled, frozen, and
            // published as a provider over the SAME contract stack; unit B then resolves A's
            // inline value BY KEY and splices its thawed body. Nothing B does can reach a cell
            // of A's — A handed out `FrozenType` only.


            test "cross-unit SPLICE: B resolves A's published inline BY KEY and splices the thawed body" {
                // Unit A is a real compilation, sharing B's `NodeKey` space (no file id).
                let provider = publishing (kindOfUnit "Lib" "Kinds")

                // Only a real splice can answer these: the clause conditions are resolved
                // against the CALL-SITE operand type, in B, over cells B minted at thaw.
                Expect.equal
                    (splicedConst provider "open Lib\nlet r : int = Kinds.kindOf 5\n")
                    1L
                    "int operand selects the int clause"

                Expect.equal
                    (splicedConst provider "open Lib\nlet r : int = Kinds.kindOf 5.0\n")
                    2L
                    "float operand selects the float clause"

                Expect.equal
                    (splicedConst provider "open Lib\nlet r : int = Kinds.kindOf true\n")
                    0L
                    "an operand no clause names falls to the `^T : ^T` catch-all"
            }


            test "cross-unit splice ≡ in-unit splice, over COLLIDING NodeKeys" {
                // A and B are compiled in the same `NodeKey` space, so A's body binders and
                // B's own collide freely. If the thaw consulted any ambient unit state — or
                // if its freshener cache were keyed by anything B also keys by — the
                // collision would surface here as a wrong clause or a type error.
                let provider = publishing (kindOfUnit "AAA" "Kind1")

                // The SAME program with the inline declared IN-unit: the reference answer
                // the cross-unit splice must reproduce.
                let inUnitAnswer =
                    splicedConst
                        (ClrSymbolProviders.buildContract defaultManifests)
                        (String.concat
                            "\n"
                            [
                                "let inline kindOf (x: ^T) : int ="
                                "    -1"
                                "    when ^T : int   = 1"
                                "    when ^T : float = 2"
                                "    when ^T : ^T    = 0"
                                "let r : int = kindOf 5.0"
                            ])

                Expect.equal
                    (splicedConst provider "open AAA\nlet r : int = Kind1.kindOf 5.0\n")
                    inUnitAnswer
                    "freeze-in-A / splice-in-B ≡ in-unit splice"
            }
        ]
