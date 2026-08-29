module XParsec.FSharp.Codegen.Clr.Tests.InlineFreezeThawTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Freeze publishes an inline binding as `FrozenType` under a `SymbolKey` minted from its
// declaring module chain; the thaw mints one fresh cell per distinct frozen leaf. A body-local
// leaf `FTLocalTypar(scheme, index)` is BODY-relative, so two files' `SchemeId`s may collide.

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

/// Every typar leaf, quantified (`FTTypar`) or body-local (`FTLocalTypar`), in pre-order.
/// The leaf value IS its identity.
let rec private typarLeavesIn (t: FrozenType) : FrozenType list =
    match t with
    | FTTypar _
    | FTLocalTypar _ -> [ t ]
    | t ->
        let acc = ResizeArray<FrozenType>()
        FrozenType.iterChildren (fun c -> acc.AddRange(typarLeavesIn c)) t
        List.ofSeq acc

let rec private semRootsOf (store: TypeStore) (t: SemType) : TyVarId list =
    match t with
    | TyVar tv -> [ (UnionFind.find store tv).Id ]
    | t ->
        let acc = ResizeArray<TyVarId>()
        SemType.iterChildren (fun c -> acc.AddRange(semRootsOf store c)) t
        List.ofSeq acc

/// Every `.ty` slot of a decl, via the `TastConvert` functor.
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

let private distinctCells (tvs: TyVarId list) : TyVarId list =
    let acc = ResizeArray<TyVarId>()

    for tv in tvs do
        if not (acc |> Seq.exists (fun seen -> seen = tv)) then
            acc.Add tv

    List.ofSeq acc

/// The thaw's contract in one number: it must mint exactly this many cells, one per distinct
/// leaf, shared across every occurrence of that leaf.
let private distinctLeafCount (d: TDeclG<FrozenType, 'tok, 'id>) : int =
    collectTys d |> List.collect typarLeavesIn |> List.distinct |> List.length

let private freezePools (src: string) : FrozenPools =
    let ctx, tast = analyseWithCtx src
    Expect.isEmpty tast.Diagnostics "no diagnostics"
    Freeze.run ctx tast

let private freeze (src: string) : Pooled.TastFile = TastUnpool.ofPools (freezePools src)

/// The `LexedFile` a body frozen from `src` is anchored in, derived from the text, so
/// rebuilding it here yields the identity the analysis stamped.
let private sourceOf (src: string) : LexedFile =
    let lexed, _ = parseFile src
    LexedFile.ofText lexed

/// Thaw a wire body against the file it was frozen from: a `Wire.TDecl`'s anchors index
/// the declaring file's tokens.
let private thawFrom (store: TypeStore) (src: string) (decl: Wire.TDecl) : TDecl =
    let source = sourceOf src
    InlineThaw.bodyAtPath store (LexedFiles.ofSeq [ source ]) source.Path decl

/// The frozen `let` decl of a single-binding program, unpooled as a provider serves a body
/// (`declTree`), the form `thawFrom` takes.
let private frozenLetDecl (src: string) : Wire.TDecl =
    let pools = freezePools src
    let pool = TastPoolBuilder.openOver pools

    pools.Roots
    |> Seq.tryPick (fun r ->
        match TastPoolBuilder.declTree pool r with
        | TDeclG.Let(TPatG.NamedSimple _, _, _, _) as d -> Some d
        | _ -> None
    )
    |> Option.defaultWith (fun () -> failtestf "no top-level `let` in the frozen tree of:\n%s" src)

let private soleInlineBody (src: string) : Pooled.TInlineValue =
    match (freeze src).InlineBodies |> EqArray.toList with
    | [ v ] -> v
    | other -> failtestf "expected exactly one published inline body, got %d, in:\n%s" (List.length other) src

/// Two locally-generalized `let`s: ONE would give a single local root and so could not
/// witness conflation. No `inline`, because local generalisation is what creates the residue.
let private twoLocalSchemes =
    String.concat
        "\n"
        [
            "let f () ="
            "    let g = fun x -> x"
            "    let h = fun y -> y"
            "    (g, g, h, h)"
        ]

/// A `let inline` whose body is an SRTP MEMBER CONSTRAINT: "`^T` has a static `+`" is not
/// encodable on a CLR generic parameter, so there is no signature to emit it under. Only a
/// splice can discharge the node, once `^T` is ground to a nominal that carries the member.
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

/// A module-held `let inline` with a static-optimization body: three `when` clauses over
/// `^T`, each returning a bare `int` literal.
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

/// Re-express a published body's decl type as a symbol `Scheme`: a `let inline`'s own typars
/// are carried on the METHOD axis, while an `ExternalSymbol.Scheme` bakes a free function's
/// onto the DECLARING axis. Positional, so index order is preserved.
let private asSymbolScheme (ft: FrozenType) : FrozenType =
    FrozenTypeBridge.instantiateWith
        (fun i -> TyTypar(TyparAxis.Declaring, i))
        (fun j -> TyTypar(TyparAxis.Declaring, j))
        FrozenTypeBridge.localTyparInTemplate
        ft
    |> toFrozen

/// The symbol's `TyparArity`: one past the highest typar index the template references.
let rec private typarArity (ft: FrozenType) : int =
    match ft with
    | FTTypar(_, i) -> i + 1
    | t ->
        let mutable n = 0
        FrozenType.iterChildren (fun c -> n <- max n (typarArity c)) t
        n

/// File A's inline vocabulary as a provider: one `ExternalSymbol` per published body, with
/// the body folded onto it. The consumer resolves the symbol, carries its `Key`, and reaches
/// the body through that key, so nothing `SemType` crosses.
let private publishing (unitASource: string) : IExternalSymbolProvider =
    let ctx, tastA = analyseWithCtx unitASource
    Expect.isEmpty tastA.Diagnostics "file A has no diagnostics"
    let unitA = Freeze.run ctx tastA
    let pool = TastPoolBuilder.openOver unitA

    // `declTree` re-mints the body's bound variables into the node space B's expansion consumes;
    // anchoring in A's own file is what makes the indices those bodies carry readable at B.
    let source = sourceOf unitASource

    let published =
        [
            for t in unitA.InlineTemplates ->
                t.Key, InlineBody.anchoredIn source (TastPoolBuilder.declTree pool t.Decl) t.ParamAttrs
        ]

    let bodies = dict published

    // The vocabulary as a referenced assembly publishes it: B reaches `Kinds.kindOf` by
    // walking A's module, as it reaches any other referenced value.
    let surface =
        PublishedSurface.build (fun b ->
            for (key, body) in published do
                let declTy =
                    match body.Decl with
                    | TDeclG.Let(_, _, _, ty) -> ty
                    | other -> failtestf "a published body is not a `let`: %A" other

                let binding =
                    match key with
                    | SymbolKey.Binding b -> b
                    | other -> failtestf "a published inline value is not a binding key: %A" other

                let scheme = asSymbolScheme declTy

                ExternalSymbols.scheme binding.Decl binding.Name scheme (typarArity scheme) []
                |> PublishedSurfaceBuilder.addValue b ValueNone
        )

    ExternalSymbolProviders.composite
        [
            PublishedSurface.toProvider surface
            ClrSymbolProviders.buildContract defaultPackages
        ]
    |> ExternalSymbolProviders.withInlineBodies (fun k ->
        match bodies.TryGetValue k with
        | true, b -> ValueSome b
        | _ -> ValueNone
    )

/// The constant `let r = …` reduced to. `kindOf`'s clause bodies are bare `int` literals, so
/// WHICH clause the expansion selected is read off the entry the call's edge points to. An
/// unresolved call leaves an `App` node instead, which reaches no `Const`.
let private resolvedConst (provider: IExternalSymbolProvider) (src: string) : int64 =
    let lexed, file = parseFile src

    let tast =
        TastUnpool.ofPools (Pipeline.analyseFor testCompiling provider (LexedFile.ofText lexed) file)

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

                // An `inline` binding is ALSO an ordinary module function: it stays in
                // `Decls` with `IsInline = true`, so a use that cannot be spliced (a
                // first-class reference, a caller with no ground operand type) can call it.
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

                // The key is MINTED from the declaring container chain, not recovered from a
                // dotted spelling. It is the identity a use-site `TExpr.External` carries.
                Expect.equal
                    published.Key
                    (SymbolKeyOps.moduleValueKey "Lib" "Kinds" "kindOf")
                    "the published identity is the binding's own containment chain"
            }

            // A `StaticOptimization` has a `defaultExpr` fallback for "no type pinned",
            // exactly what the ordinary compiled function is, so it lowers. An SRTP member
            // constraint has no IL encoding at all, so it does not.
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
                    "a static-optimization body lowers, because each backend emits its default clause"

                Expect.isEmpty
                    (lowered (traitUnit "Lib" "Traits"))
                    "a trait-call body is template-only: no CLR signature can carry 'has this member'"

                // Dropped from emission is not dropped from the vocabulary, because a splice
                // is the only thing that can discharge the node.
                Expect.equal
                    (soleInlineBody (traitUnit "Lib" "Traits")).Key
                    (SymbolKeyOps.moduleValueKey "Lib" "Traits" "plus")
                    "the un-emittable template is still published for consumers to splice"
            }

            test "a published StaticOptimization clause has no SemType cell, because its constraints freeze too" {
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

                // The constraints are `TStaticOptConstraintG<FrozenType>`, so they cannot
                // hold a `SemType` at all. What is checkable here is that they were MAPPED
                // and not dropped, and that the binding's own `^T` reached a frozen leaf.
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
                // neither occurs in `f`'s type, and they are distinguished by SCHEME, not
                // by an index that happens to differ.
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
                    "…and two distinct (scheme, index) pairs, because each local scheme quantifies exactly one typar here"

                // The USE-SITE instantiations (the four occurrences in `(g, g, h, h)`) ARE
                // in `f`'s type, so they are mapped onto the ordinary declared axis and `f`'s
                // own type carries no residue at all.
                Expect.isTrue (hasFTTypar declTy) "f's own type names its use-site instantiations on the FTTypar axis"

                Expect.isEmpty
                    (localLeavesIn declTy)
                    "f's own type carries no local-typar residue, which is exactly why mkMethodQuantEnv cannot map it"

                // The thaw mints on all three axes, so the expected count is every leaf the
                // frozen decl carries, not just the local ones.
                let store = TypeStore()

                let cells =
                    thawFrom store twoLocalSchemes fDecl
                    |> collectTys
                    |> List.collect (semRootsOf store)
                    |> distinctCells

                Expect.equal
                    cells.Length
                    (distinctLeafCount fDecl)
                    "a decl-scoped thaw mints EXACTLY one fresh cell per distinct leaf, sharing it across every occurrence"
            }

            test "freeze: the same source freezes local-typar leaves to the same (scheme, index)s" {
                // Serializing a frozen body and re-reading it makes index stability rest on
                // the freeze walk order. Nothing but this test enforces that order.
                let once = frozenLetDecl twoLocalSchemes |> collectTys |> List.collect localLeavesIn

                let twice =
                    frozenLetDecl twoLocalSchemes |> collectTys |> List.collect localLeavesIn

                Expect.isNonEmpty once "the fixture actually produces local typars"

                Expect.equal
                    twice
                    once
                    "two freezes of the same source yield identical (scheme, index) leaves, in the same order"
            }

            test "freeze/thaw: colliding scheme ids across two files do not conflate, because the leaf is body-relative" {
                // A `SchemeId` is an ordinal minted per frozen body, so two files' ids collide
                // freely. These are DIFFERENT programs, each with one local scheme, so both
                // land on the SAME `SchemeId`.
                let declaring =
                    String.concat "\n" [ "let a () ="; "    let p = fun x -> x"; "    (p, p)" ]

                let consumer =
                    String.concat "\n" [ "let b () ="; "    let q = fun y -> y"; "    (q, q)" ]

                let pDecl = frozenLetDecl declaring
                let cDecl = frozenLetDecl consumer

                let pLeaves = collectTys pDecl |> List.collect localLeavesIn |> List.distinct
                let cLeaves = collectTys cDecl |> List.collect localLeavesIn |> List.distinct

                Expect.equal pLeaves.Length 1 "the declaring file has one local scheme"
                Expect.equal cLeaves.Length 1 "the consumer file has one local scheme"

                // Assert the collision is REAL, or the test proves nothing.
                Expect.equal
                    (fst pLeaves.Head)
                    (fst cLeaves.Head)
                    "the two files' local schemes collide on the same SchemeId (an id is body-relative by design)"

                Expect.equal
                    pLeaves.Head
                    cLeaves.Head
                    "…so the two frozen leaves are structurally EQUAL across files. That is not a bug: a frozen typar leaf is only ever interpreted against the template carrying it, exactly as FTTypar(Declaring, 0) is."

                // A `TyVarId` indexes ONE store, so route the consumer's own inference AND
                // both thaws through a SINGLE store: a leaf-keyed conflation then surfaces as
                // a REUSED id rather than hiding behind separate object identities.
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
                    thawFrom store declaring pDecl
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
                    "the declaring file body thaws to exactly one fresh cell per distinct leaf"

                Expect.equal
                    cCells.Length
                    (distinctLeafCount cDecl)
                    "the consumer body thaws to exactly one fresh cell per distinct leaf"

                Expect.isEmpty
                    (disjointFrom pCells cCells)
                    "the colliding scheme id does NOT conflate the two files' local typars, because each thaw mints its own cells"

                Expect.isEmpty
                    (disjointFrom consumerOwnCells pCells)
                    "the declaring file's thawed cells are fresh: none is a cell of the consumer's own inference state, colliding id notwithstanding"
            }

            // ─── Cross-file EXPANSION: freeze in A, resolve in B ────────────────────────
            // B resolves A's published inline value BY KEY and expands its thawed body.
            // Nothing B does reaches a cell of A's, because A handed out `FrozenType` only.


            test "cross-file: B resolves A's published inline BY KEY and expands the thawed body" {
                // File A is a real compilation, sharing B's `NodeKey` space (no file id).
                let provider = publishing (kindOfUnit "Lib" "Kinds")

                // Only a real expansion produces these values: the clause conditions are resolved
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
                // A and B share one `NodeKey` space, so A's body bound variables and B's own
                // collide freely. A thaw consulting ambient file state, or keyed by anything
                // B also keys by, would surface here as a wrong clause or a type error.
                let provider = publishing (kindOfUnit "AAA" "Kind1")

                // The SAME program with the inline declared IN-file: the reference value the
                // cross-file expansion must reproduce, read the same way, through the edge.
                let inUnitResult =
                    resolvedConst
                        (ClrSymbolProviders.buildContract defaultPackages)
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
                    inUnitResult
                    "freeze-in-A / expand-in-B ≡ in-file expansion"
            }
        ]
