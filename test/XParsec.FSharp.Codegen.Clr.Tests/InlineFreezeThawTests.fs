module XParsec.FSharp.Codegen.Clr.Tests.InlineFreezeThawTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The frozen inline-body channel, end to end.
//
// 1. `Freeze` PUBLISHES an inline binding — additively, since the binding is ALSO emitted
//    as an ordinary module function and stays in `Decls` — under a `SymbolKey` it MINTS
//    from the binding's declaring module chain. The identity is minted rather than
//    recovered because the vocabulary channel is resolved in the front end, before any
//    backend has run to mint one as a side effect of emitting.
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
//    `FTLocalTypar(scheme, index)`; the thaw mints one fresh cell per
//    `(scheme, index)` pair.
//
// 4. That identity is BODY-RELATIVE and survives two files minting the same `SchemeId` —
//    the multi-file case, an id being an ordinal within one body and nothing more. Both
//    the freeze/thaw half and a REAL cross-file splice are pinned below.

/// Every type mentioned by a frozen clause's constraints, in order.
let private frozenConstraintTypes (clauses: TStaticOptClauseG<FrozenType, 'tok, 'id> list) : FrozenType list =
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
let rec private localLeavesIn (t: FrozenType) : (SchemeId * int) list =
    match t with
    | FTLocalTypar(scheme, i) -> [ scheme, i ]
    | t ->
        let acc = ResizeArray<SchemeId * int>()
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

/// Every metavar root (`TyVarId`) in a `SemType`, in first-occurrence pre-order.
let rec private semRootsOf (store: TypeStore) (t: SemType) : TyVarId list =
    match t with
    | TyVar tv -> [ (UnionFind.find store tv).Id ]
    | t ->
        let acc = ResizeArray<TyVarId>()
        SemType.iterChildren (fun c -> acc.AddRange(semRootsOf store c)) t
        List.ofSeq acc

/// Run over every `.ty` slot of a decl, collecting them. `TastConvert` is the
/// exhaustive functor, so this reaches every type in the tree — no hand-rolled walk
/// that a new TAST case could silently escape.
let private collectTys (d: TDeclG<'ty, 'tok, 'id>) : 'ty list =
    let acc = ResizeArray<'ty>()

    TastConvert.decl
        (fun ty ->
            acc.Add ty
            ty
        )
        id
        d
    |> ignore

    List.ofSeq acc

/// Dedupe metavar roots by id — a `TyVarId` is the metavar's identity, and cell
/// identity is what these tests are about.
let private distinctCells (tvs: TyVarId list) : TyVarId list =
    let acc = ResizeArray<TyVarId>()

    for tv in tvs do
        if not (acc |> Seq.exists (fun seen -> seen = tv)) then
            acc.Add tv

    List.ofSeq acc

/// How many DISTINCT typar leaves a frozen decl names, across all three axes. The
/// thaw's contract in one number: it must mint exactly this many cells — one per
/// leaf, shared across every occurrence of that leaf. Deriving the count from the
/// frozen tree rather than hard-coding it is what keeps the assertion EXACT: a broken
/// cache mints MORE cells than there are leaves, which a `>=` bound would not catch.
// Position-axis agnostic: leaves are a fact about the TYPES, so a pooled decl and a wire one
// (whose anchors are the producer's) answer the same number.
let private distinctLeafCount (d: TDeclG<FrozenType, 'tok, 'id>) : int =
    collectTys d |> List.collect typarLeavesIn |> List.distinct |> List.length

/// The frozen file of a source — the pools the freeze yields.
let private freezePools (src: string) : FrozenPools =
    let ctx, tast = analyseWithCtx src
    Expect.isEmpty tast.Diagnostics "no diagnostics"
    Freeze.run ctx tast

/// The same file as the DU: every assertion in this file reads whole decl trees and the
/// inline vocabulary, which is what `ofPools` re-authors.
let private freeze (src: string) : Pooled.TastFile = TastUnpool.ofPools (freezePools src)

/// The producer file a body frozen from `src` is anchored in. Rebuilt from the same text, and
/// so the same identity the analysis stamped: an origin is derived from the content.
let private sourceOf (src: string) : OriginSource =
    let lexed, _ = parseFile src
    Hashing.originSourceOfText lexed

/// Realise a wire body against the file it was frozen from — the one reading there is, a
/// `Wire.TDecl`'s anchors indexing the producer's tokens. What these tests assert about a
/// thawed body is its type CELLS and never where it sits; the file is here because without it
/// those indices mean nothing.
let private thawFrom (store: TypeStore) (src: string) (decl: Wire.TDecl) : TDecl =
    let source = sourceOf src
    InlineThaw.bodyAtOrigin store (OriginSources.ofSeq [ source ]) source.File decl

/// The frozen `let` decl of a single-binding program, unpooled the way a provider serves a
/// body (`declTree`) — the form `InlineThaw.bodyAtOrigin` takes.
let private frozenLetDecl (src: string) : Wire.TDecl =
    let pools = freezePools src
    let pool = TastPoolBuilder.openOver pools

    pools.Roots
    |> Array.tryPick (fun r ->
        match TastPoolBuilder.declTree pool r with
        | TDeclG.Let(TPatG.NamedSimple _, _, _, _) as d -> Some d
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtestf "no top-level `let` in the frozen tree of:\n%s" src)

/// The file's sole published inline body.
let private soleInlineBody (src: string) : Pooled.TInlineValue =
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

/// A `let inline` whose body is an SRTP MEMBER CONSTRAINT — the one binding shape with no
/// compiled form on any target. "The type `^T` has a static `+`" is not encodable on a CLR
/// generic parameter, so there is no signature to emit the function under; F# only makes
/// such a function callable un-inlined by passing witnesses, which this compiler does not
/// do. The node is discharged by a SPLICE (`Inline.substMapper` rewrites it to a
/// `StaticMethodCall` once `^T` is ground to a nominal that carries the member), never at
/// the definition site.
let private traitUnit (ns: string) (moduleName: string) =
    String.concat
        "\n"
        [
            "namespace " + ns
            ""
            "module " + moduleName + " ="
            "    let inline plus (x: ^T) (y: ^T) : ^T ="
            "        ((^T or ^T): (static member (+): ^T * ^T -> ^T) (x, y))"
        ]

/// A module-held `let inline` — the only shape with a declaring holder chain, hence an
/// exportable identity, hence a vocabulary entry. (A top-level inline lives in the
/// anonymous Program holder and is spliceable only within its own file.)
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
        (FrozenTypeBridge.localTyparInTemplate "InlineFreezeThawTests.asSymbolScheme")
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

/// File A's inline vocabulary, published as a provider over the default contract stack —
/// a multi-file provider in miniature: one `ExternalSymbol` per published body, with the
/// body FOLDED ONTO it (that is the whole interface; there is no sibling body channel).
///
/// The consumer resolves the symbol, carries its `Key`, and reaches the body through THAT
/// key. Nothing `SemType` crosses: A published `FrozenType`, and B thaws.
let private publishing (unitASource: string) : IExternalSymbolProvider =
    let ctx, tastA = analyseWithCtx unitASource
    Expect.isEmpty tastA.Diagnostics "file A has no diagnostics"
    let unitA = Freeze.run ctx tastA
    let pool = TastPoolBuilder.openOver unitA

    // Unpooled the way a provider serves a template — `declTree`, which re-mints the body's
    // binders into the node space a consuming file's expansion speaks — and anchored in file
    // A's own file, which is what makes the indices those bodies carry readable at B.
    let source = sourceOf unitASource

    let published =
        [
            for t in unitA.InlineTemplates ->
                t.Key, InlineBody.anchoredIn source (TastPoolBuilder.declTree pool t.Decl) t.ParamAttrs
        ]

    let bodies = dict published

    let symbols =
        published
        |> List.map (fun (key, body) ->
            let declTy =
                match body.Decl with
                | TDeclG.Let(_, _, _, ty) -> ty
                | other -> failtestf "a published body is not a `let`: %A" other

            let binding =
                match key with
                | SymbolKey.Binding b -> b
                | other -> failtestf "a published inline value is not a binding key: %A" other

            let scheme = asSymbolScheme declTy

            SymbolKeyOps.qualifiedName key,
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

/// The constant the expansion resolved `let r = …` to. `kindOf`'s clause bodies are bare `int`
/// literals, so WHICH clause was selected is read straight off the entry the call's edge names.
///
/// Following the edge is the whole shape of the answer: the call becomes a `TExpr.InlineCall`
/// naming an entry, and the entry is the resolved body under the lambdas the edge's arguments
/// are positional against. A call that resolved nothing leaves an `App` head instead, which
/// reaches no `Const`, so this cannot pass by accident.
let private resolvedConst (provider: IExternalSymbolProvider) (src: string) : int64 =
    let lexed, file = parseFile src

    let tast =
        TastUnpool.ofPools (Pipeline.analyse provider (Hashing.originSourceOfText lexed) file)

    Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) (sprintf "no errors for:\n%s" src)

    let rec result (e: Pooled.TExpr) : int64 =
        match e with
        | TExprG.InlineCall(spec = SpecializationId i) ->
            match tast.Specializations.[i].Decl with
            | TDeclG.Let(_, value, _, _) -> result value
            | other -> failtestf "an entry is a `TDecl.Let` of lambdas; got %A" other
        | TExprG.Lambda(_, body, _, _)
        | TExprG.Let(_, _, body, _, _) -> result body
        | TExprG.Const(TConstValue.Integral(_, v), _, _) -> v
        | other -> failtestf "expected `r` to reduce to a resolved constant, got %A" other

    match EqArray.tryLast tast.Decls with
    | ValueSome(TDeclG.Let(_, value, _, _)) -> result value
    | _ -> failtestf "expected a trailing `let r = …`, got %A" (EqArray.toList tast.Decls)

[<Tests>]
let tests =
    testList
        "InlineFreezeThaw"
        [
            test "freeze PUBLISHES an inline binding under a minted key, and ALSO keeps it in the emittable Decls" {
                let frozen = freeze (kindOfUnit "Lib" "Kinds")

                // An `inline` binding is an ordinary module function that is ALSO a
                // splice template — both, not either. It stays in `Decls` carrying
                // `IsInline = true`, so a use that cannot be spliced (a first-class
                // reference, a caller with no ground operand type) has something to call.
                Expect.equal
                    (frozen.Decls
                     |> EqArray.toList
                     |> List.sumBy (fun d ->
                         match d with
                         | TDeclG.Let(isInline = true) -> 1
                         | _ -> 0
                     ))
                    1
                    "the inline binding is emitted as an ordinary function, flagged inline"

                let published = soleInlineBody (kindOfUnit "Lib" "Kinds")

                // The key is MINTED from the declaring holder chain — not recovered by
                // re-resolving a dotted spelling, which multi-file has nothing to recover
                // against. It is the identity a use-site `TExpr.External` carries.
                Expect.equal
                    published.Key
                    (SymbolKeyOps.moduleValueKey "Lib" "Kinds" "kindOf")
                    "the published identity is the binding's own containment chain"
            }

            // The two constraint-shaped things a template body can carry pull APART at the
            // emit seam, and this is the pair that says so. A `StaticOptimization` is a
            // compile-time CHOICE with a `defaultExpr` fallback for "no type pinned" —
            // which is exactly what the ordinary compiled function is — so it lowers. An
            // SRTP member constraint has no IL encoding at all, so it does not.
            test "lowering emits a StaticOptimization inline, and drops an SRTP one" {
                let lowered (src: string) =
                    TastLower.lower (pooledDecls (freezePools src))
                    |> List.filter (fun d ->
                        match d with
                        | TastAccessor.DLet lv -> lv.IsInline
                        | _ -> false
                    )

                Expect.equal
                    (List.length (lowered (kindOfUnit "Lib" "Kinds")))
                    1
                    "a static-optimization body lowers — each backend emits its default clause"

                Expect.isEmpty
                    (lowered (traitUnit "Lib" "Traits"))
                    "a trait-call body is template-only: no CLR signature can carry 'has this member'"

                // …and dropping it from emission does NOT drop it from the vocabulary:
                // the splice channel is the only thing that can ever discharge the node.
                Expect.equal
                    (soleInlineBody (traitUnit "Lib" "Traits")).Key
                    (SymbolKeyOps.moduleValueKey "Lib" "Traits" "plus")
                    "the un-emittable template is still published for consumers to splice"
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

            test
                "freeze: two body-local schemes freeze to leaves with DISTINCT scheme ids, and thaw to two distinct cells" {
                let fDecl = frozenLetDecl twoLocalSchemes

                let declTy =
                    match fDecl with
                    | TDeclG.Let(_, _, _, ty) -> ty
                    | _ -> failtest "unreachable"

                // `g`'s `'x` and `h`'s `'y` are each bound by their OWN local scheme, so
                // neither occurs in `f`'s type. They are two typars, and — the point of
                // naming the scheme at all — they are distinguished by their SCHEME, not
                // merely by an index that happens to differ.
                let leaves = collectTys fDecl |> List.collect localLeavesIn

                Expect.isNonEmpty leaves "the body-local schemes' own roots reach freeze as FTLocalTypar"

                let schemes = leaves |> List.map fst |> List.distinct

                Expect.equal
                    schemes.Length
                    2
                    "two locally-generalized lets ⇒ leaves with two DISTINCT scheme ids (the old FTUnknown name conflated them)"

                Expect.equal
                    (leaves |> List.distinct |> List.length)
                    2
                    "…and two distinct (scheme, index) pairs — each local scheme quantifies exactly one typar here"

                // The USE-SITE instantiations — the four occurrences in `(g, g, h, h)` — ARE
                // in `f`'s type, so `mkMethodQuantEnv` maps them and they ride the ordinary
                // declared axis. `f`'s own type therefore carries no residue at all.
                Expect.isTrue (hasFTTypar declTy) "f's own type names its use-site instantiations on the FTTypar axis"

                Expect.isEmpty
                    (localLeavesIn declTy)
                    "f's own type carries no local-typar residue — that is exactly why mkMethodQuantEnv cannot map it"

                // One decl-scoped thaw: one fresh cell per distinct leaf, shared across every
                // occurrence of it. The thaw mints on all three axes, so the expected count
                // is every leaf the frozen decl names — not just the local ones.
                let store = TypeStore()

                let cells =
                    thawFrom store twoLocalSchemes fDecl
                    |> collectTys
                    |> List.collect (semRootsOf store)
                    |> distinctCells

                Expect.equal
                    cells.Length
                    (distinctLeafCount fDecl)
                    "a decl-scoped thaw mints EXACTLY one fresh cell per distinct leaf — sharing it across every occurrence"
            }

            test "freeze: local-typar leaves are DETERMINISTIC — the same source freezes to the same (scheme, index)s" {
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
                    "two freezes of the same source yield identical (scheme, index) leaves, in the same order"
            }

            test "freeze/thaw: colliding scheme ids across two files do not conflate — the leaf is BODY-relative" {
                // The multi-file hazard, made concrete. A `SchemeId` is an ordinal minted per
                // frozen body, so two files' ids collide freely — a leaf is only ever
                // interpreted against the body carrying it. These two files are DIFFERENT
                // programs, each with one local scheme, so both land on the SAME `SchemeId`.
                let producer =
                    String.concat "\n" [ "let a () ="; "    let p = fun x -> x"; "    (p, p)" ]

                let consumer =
                    String.concat "\n" [ "let b () ="; "    let q = fun y -> y"; "    (q, q)" ]

                let pDecl = frozenLetDecl producer
                let cDecl = frozenLetDecl consumer

                let pLeaves = collectTys pDecl |> List.collect localLeavesIn |> List.distinct
                let cLeaves = collectTys cDecl |> List.collect localLeavesIn |> List.distinct

                Expect.equal pLeaves.Length 1 "the producer file has one local scheme"
                Expect.equal cLeaves.Length 1 "the consumer file has one local scheme"

                // The collision is REAL — assert it, or the test proves nothing.
                Expect.equal
                    (fst pLeaves.Head)
                    (fst cLeaves.Head)
                    "the two files' local schemes collide on the same SchemeId (an id is body-relative — by design)"

                Expect.equal
                    pLeaves.Head
                    cLeaves.Head
                    "…so the two frozen leaves are structurally EQUAL across files. That is not a bug: a frozen typar leaf is only ever interpreted against the template carrying it, exactly as FTTypar(Declaring, 0) is."

                // A `TyVarId` indexes ONE store, so cross-store id comparison is meaningless
                // after the handle collapse — two files' cells are distinguishable only within a
                // single id space. So route the consumer's own inference AND both thaws through
                // ONE store: a leaf-keyed conflation would then surface as a REUSED (colliding)
                // id rather than hide behind separate object identities. Each thaw still
                // builds its OWN decl-scoped cache (design constraint: one cache per thawed decl),
                // so the two same-keyed thaws must still mint independent cells in that one store.
                let ctx, tast = analyseWithCtx consumer
                let store = ctx.Store

                // The consumer's own live inference cells, before anything is thawed into it.
                let consumerOwnCells =
                    tast.Decls
                    |> EqArray.toList
                    |> List.collect collectTys
                    |> List.collect (semRootsOf store)
                    |> distinctCells

                let pCells =
                    thawFrom store producer pDecl
                    |> collectTys
                    |> List.collect (semRootsOf store)
                    |> distinctCells

                let cCells =
                    thawFrom store consumer cDecl
                    |> collectTys
                    |> List.collect (semRootsOf store)
                    |> distinctCells

                let disjointFrom (xs: TyVarId list) (ys: TyVarId list) =
                    ys |> List.filter (fun y -> xs |> List.exists (fun x -> x = y))

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
                    "the colliding scheme id does NOT conflate the two files' local typars — each thaw mints its own cells"

                Expect.isEmpty
                    (disjointFrom consumerOwnCells pCells)
                    "the producer's thawed cells are fresh — none is a cell of the consumer's own inference state, colliding id notwithstanding"
            }

            // ─── Cross-file EXPANSION: freeze in A, resolve in B ────────────────────────
            //
            // The property the whole channel exists for. File A is compiled, frozen, and
            // published as a provider over the SAME contract stack; file B then resolves A's
            // inline value BY KEY and expands its thawed body. Nothing B does can reach a cell
            // of A's — A handed out `FrozenType` only.


            test "cross-file: B resolves A's published inline BY KEY and expands the thawed body" {
                // File A is a real compilation, sharing B's `NodeKey` space (no file id).
                let provider = publishing (kindOfUnit "Lib" "Kinds")

                // Only a real expansion can answer these: the clause conditions are resolved
                // against the CALL-SITE operand type, in B, over cells B minted at thaw.
                Expect.equal
                    (resolvedConst provider "open Lib\nlet r : int = Kinds.kindOf 5\n")
                    1L
                    "int operand selects the int clause"

                Expect.equal
                    (resolvedConst provider "open Lib\nlet r : int = Kinds.kindOf 5.0\n")
                    2L
                    "float operand selects the float clause"

                Expect.equal
                    (resolvedConst provider "open Lib\nlet r : int = Kinds.kindOf true\n")
                    0L
                    "an operand no clause names falls to the `^T : ^T` catch-all"
            }


            test "cross-file expansion ≡ in-file expansion, over COLLIDING NodeKeys" {
                // A and B are compiled in the same `NodeKey` space, so A's body binders and
                // B's own collide freely. If the thaw consulted any ambient file state — or
                // if its freshener cache were keyed by anything B also keys by — the
                // collision would surface here as a wrong clause or a type error.
                let provider = publishing (kindOfUnit "AAA" "Kind1")

                // The SAME program with the inline declared IN-file: the reference answer
                // the cross-file expansion must reproduce. Both are outlined — a template of
                // this file has an anchor domain to name like any other — so the two answers
                // are read the same way, through the edge.
                let inUnitAnswer =
                    resolvedConst
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
                    (resolvedConst provider "open AAA\nlet r : int = Kind1.kindOf 5.0\n")
                    inUnitAnswer
                    "freeze-in-A / expand-in-B ≡ in-file expansion"
            }
        ]
