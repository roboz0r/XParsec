module XParsec.FSharp.SemanticAnalysis.Tests.TastPoolsTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The pool-build faithfulness gate: freeze a program, build the pools, then walk the pools from
// the decl roots and assert the reconstruction equals the DU walk — same shapes in the same order
// and the same fan-out, so a mis-wired child edge shows up as a fan-out mismatch.

/// The dense id the pool interned a DU node's own bound variable under: an unpooled tree already
/// identifies its bound variables in the pool's own space, so this is the node's own key widened.
let private internedBoundVarId (pools: FrozenPools) (b: BoundVarKeyG<BoundVarId> voption) : BoundVarId voption =
    b
    |> ValueOption.map (fun b ->
        let id = BoundVarKey.identity b
        let (BoundVarId i) = id

        if i < 0 || i >= pools.BoundVarNames.Length then
            failtestf "the pooled node's bound variable %O occupies no bound-variable pool slot" id

        id
    )

let rec private checkPat (pools: FrozenPools) (PatPoolId i) (du: Pooled.TPat) =
    let boundVar = internedBoundVarId pools (BoundVarKey.ofPat du)
    Expect.equal pools.PatPayloads.[i] (TastPoolShapes.patPayload boundVar du) "pat payload"
    let duKids = TastPoolShapes.patChildren du
    Expect.equal (ChildColumn.count pools.PatChildren i) duKids.Length "pat child fan-out"
    Array.iter2 (checkPat pools) (ChildColumn.slice pools.PatChildren i) duKids

let rec private checkExpr (pools: FrozenPools) (ExprPoolId i) (du: Pooled.TExpr) =
    let boundVar = internedBoundVarId pools (BoundVarKey.ofExpr du)
    Expect.equal pools.ExprPayloads.[i] (TastPoolShapes.exprPayload id boundVar du) "expr payload"
    let duExprKids = TastPoolShapes.exprChildren du
    let duPatKids = TastPoolShapes.exprPatChildren du
    Expect.equal (ChildColumn.count pools.ExprChildren i) duExprKids.Length "expr child fan-out"
    Expect.equal (ChildColumn.count pools.ExprPatChildren i) duPatKids.Length "expr's pat fan-out"
    Array.iter2 (checkExpr pools) (ChildColumn.slice pools.ExprChildren i) duExprKids
    Array.iter2 (checkPat pools) (ChildColumn.slice pools.ExprPatChildren i) duPatKids

/// A type declaration's body slots in traversal order, gathered by running the same traversal
/// the pool fill and unpool run — so a newly-added body slot appears here with no edit.
let private bodySlots (td: TTypeDeclG<FrozenType, Anchor, 'id, 'body>) : 'body[] =
    let slots = ResizeArray<'body>()

    TastConvert.typeDecl
        {
            Ty = id
            Tok = id
            Id = BoundVarKey.identity
            Body =
                fun b ->
                    slots.Add b
                    b
        }
        td
    |> ignore

    slots.ToArray()

let private checkDecl (pools: FrozenPools) (DeclPoolId i) (du: Pooled.TDecl) =
    let shapeIs = Expect.equal (DeclPayload.shape pools.DeclPayloads.[i])

    match du with
    | TDeclG.Let(pattern = pattern; value = value) ->
        shapeIs DeclShape.Let "decl shape"
        Expect.equal (ChildColumn.count pools.DeclExprChildren i) 1 "let decl one value child"
        Expect.equal (ChildColumn.count pools.DeclPatChildren i) 1 "let decl one pattern child"
        checkExpr pools (ChildColumn.item pools.DeclExprChildren i 0) value
        checkPat pools (ChildColumn.item pools.DeclPatChildren i 0) pattern
    | TDeclG.Expression(expr = expr) ->
        shapeIs DeclShape.Expression "decl shape"
        Expect.equal (ChildColumn.count pools.DeclExprChildren i) 1 "expression decl one child"
        Expect.equal (ChildColumn.count pools.DeclPatChildren i) 0 "expression decl no pat child"
        checkExpr pools (ChildColumn.item pools.DeclExprChildren i 0) expr
    | TDeclG.Type duTd ->
        shapeIs DeclShape.Type "decl shape"
        Expect.equal (ChildColumn.count pools.DeclExprChildren i) 0 "type decl surfaces no expr child"
        Expect.equal (ChildColumn.count pools.DeclPatChildren i) 0 "type decl surfaces no pat child"

        // The member / preamble / ctor bodies are not children: they are named by id INSIDE the
        // payload's declaration shape, so zip the pooled ids against the DU's in slot order.
        match pools.DeclPayloads.[i] with
        | DeclPayload.Type td ->
            let ids = bodySlots td
            let bodies = bodySlots duTd
            Expect.equal ids.Length bodies.Length "one pooled body id per type-decl body slot"
            Array.iter2 (checkExpr pools) ids bodies
        | p -> failtestf "a Type decl's pool payload is %A, not DeclPayload.Type" p

/// A pooled lambda's `LambdaKey`. `FunVerdicts` is keyed by the lambda id space, not the bound
/// variable pool, and the node that carried the key is gone — so read it off the `ExprToks` column.
let private pooledLambdaKey (pools: FrozenPools) (ExprPoolId i) : LambdaKey = LambdaKey pools.ExprToks.[i]

/// The id-resolution gate: `ExprVarBoundVar` is populated EXACTLY at the `Var` slots, each to an
/// in-range `BoundVarId`, and each side table covers its source map 1:1 — whether it keeps its
/// key or has given it up for a position in the bound variable pool.
let private checkIdResolution (pools: FrozenPools) (frozen: Pooled.TastFile) =
    for i in 0 .. pools.ExprPayloads.Length - 1 do
        match ExprPayload.shape pools.ExprPayloads.[i], pools.ExprVarBoundVar.[i] with
        | ExprShape.Var, ValueSome(BoundVarId b) ->
            Expect.isTrue
                (b >= 0 && b < pools.BoundVarNames.Length)
                "Var bound variable id is an interned bound variable"
        | ExprShape.Var, ValueNone -> failtest "a Var pool entry carries no resolved bound variable id"
        | _, ValueSome _ -> failtest "a non-Var pool entry carries a bound variable id"
        | _, ValueNone -> ()

    // Each dense side table is the source map re-keyed onto its id space: same cardinality, every
    // dense key resolving to a source key. Only the KEYS are compared. `BindingValReprs` is absent
    // — it is derived off the columns rather than re-keyed, and is gated separately below.
    let checkTable (name: string) (resolve: 'id -> 'k) (dense: ('id * 'v)[]) (sourceKeys: Set<'k>) =
        Expect.equal dense.Length sourceKeys.Count (name + " dense form covers the source map 1:1")

        for (id, _) in dense do
            Expect.isTrue (Set.contains (resolve id) sourceKeys) (name + " dense key resolves to a source key")

    // A per-bound-variable COLUMN holds no key, so what is checked is the FILLED SLOTS: one per
    // source entry, at the slot of the bound variable that entry named, and aligned to the pool.
    let checkColumn (name: string) (col: BoundVarColumn<'v>) (sourceKeys: Set<BoundVarId>) =
        Expect.equal col.Length pools.BoundVarNames.Length (name + " column is aligned with the bound variable pool")

        let filled =
            [|
                for i in 0 .. col.Length - 1 do
                    if col.[i].IsSome then
                        yield BoundVarId i
            |]

        Expect.equal filled.Length sourceKeys.Count (name + " column covers the source map 1:1")

        for k in filled do
            Expect.isTrue (Set.contains k sourceKeys) (name + " filled slot is a source key's bound variable")

    // The source keys in the address space the resolvers answer in: a bound-variable-keyed table
    // is widened, `FunVerdicts` is already lambda-key-shaped.
    let keysOf (m: Map<'k, 'w>) =
        m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    let boundVarSource (m: Map<BoundVarKeyG<BoundVarId>, 'w>) = keysOf (BoundVarKey.widenMap m)

    checkTable "ModuleMembers" id pools.ModuleMembers (boundVarSource frozen.ModuleMembers)
    checkTable "ClosureReprs" id pools.ClosureReprs (boundVarSource frozen.ClosureReprs)
    checkTable "FunVerdicts" (pooledLambdaKey pools) pools.FunVerdicts (keysOf frozen.FunVerdicts)
    checkTable "GenericFnSchemes" id pools.GenericFnSchemes (boundVarSource frozen.GenericFnSchemes)
    checkColumn "BindingTyparArities" pools.BindingTyparArities (boundVarSource frozen.BindingTyparArities)

/// A binding's recorded arity is READ OFF the pooled lambda chain, so every recorded `GTuple` must
/// point to a pat some `Lambda` bears as its parameter — a re-pooled copy is structurally equal but a
/// different id. Also: one entry per `NamedSimple`-patterned `Let` root and no more.
let private checkValReprPatsAreLambdaParams (pools: FrozenPools) =
    let lambdaParams = System.Collections.Generic.HashSet<PatPoolId>()

    for i in 0 .. pools.ExprPayloads.Length - 1 do
        if pools.ExprPayloads.[i] = ExprPayload.Lambda then
            lambdaParams.Add(ChildColumn.item pools.ExprPatChildren i 0) |> ignore

    for _, vr in pools.BindingValReprs do
        for g in vr.Groups do
            match g with
            | ArgGroupG.GTuple pat ->
                Expect.isTrue (lambdaParams.Contains pat) "a tuple group names a lambda's own parameter pattern"
            | ArgGroupG.GUnit _
            | ArgGroupG.GSimple _ -> ()

    let namedLetRoots =
        pools.Roots
        |> EqArray.toArray
        |> Array.filter (fun (DeclPoolId d) ->
            DeclPayload.shape pools.DeclPayloads.[d] = DeclShape.Let
            && (let (PatPoolId pattern) = ChildColumn.item pools.DeclPatChildren d 0

                match pools.PatPayloads.[pattern] with
                | PatPayload.NamedSimple _ -> true
                | _ -> false)
        )

    Expect.equal
        pools.BindingValReprs.Length
        namedLetRoots.Length
        "one recorded arity per simple-bound-variable Let root"

// The bound variable columns' obligation: a bound variable the SOURCE WRITES is anchored at the
// token that writes it and named with the text there; one no source writes has neither — an
// inlined body's freshened bound variables are written nowhere. Returns the written count.
let private checkBoundVarNames (src: string) (pools: FrozenPools) : int =
    let lexed, _ = parseFile src
    let mutable written = 0

    for i in 0 .. pools.BoundVarNames.Length - 1 do
        match pools.BoundVarNames.[i], pools.BoundVarToks.[i].Index with
        | "", ValueNone -> ()
        | "", ValueSome t -> failtestf "bound variable %d is anchored at token %d but has no name" i (int t)
        | name, ValueNone -> failtestf "bound variable %d is named '%s' but is written nowhere" i name
        | name, ValueSome t ->
            written <- written + 1

            Expect.equal
                (lexed.GetIdentifier(t))
                name
                (sprintf "bound variable %d's name is the identifier at its own anchor" i)

    written

let private checkProgram (src: string) =
    let pools, frozen = poolsFor src
    let duDecls = EqArray.toArray frozen.Decls
    Expect.equal pools.Roots.Length duDecls.Length "one root per emittable decl"
    Array.iter2 (checkDecl pools) (EqArray.toArray pools.Roots) duDecls
    checkIdResolution pools frozen
    checkValReprPatsAreLambdaParams pools

    // The interconversion gate: `ofPools ∘ rePool` reconstructs a structurally-equal file. Both
    // sides speak the pool's own dense identity, so the comparison needs no widening — the ids the
    // re-pool assigns must be the ids the tree already bore, the two walks being the same walk.
    Expect.isTrue
        (TastFileG.structurallyEqual (TastUnpool.ofPools pools) frozen)
        "ofPools (rePool f) round-trips to a structurally-equal frozen file"

// Representative programs, spanning bound variable shapes (lambda / let-in / for), control flow
// (if / match) and the type + value forms (record decl, record literal, field access).
let private programs =
    [
        "curried fn + saturated call", "let add x y = x + y\nlet answer = add 1 40\n"
        "if/then/else + unary op", "let f x = if x > 0 then x else -x\n"
        "match with const and wildcard arms", "let classify x =\n    match x with\n    | 0 -> 1\n    | _ -> 2\n"
        "nested let-in", "let h () =\n    let a = 1 in\n    let b = 2 in\n    a + b\n"
        "record decl, literal and field get",
        "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet getX (v: R) = v.X\n"
        "for-to loop with mutable accumulator",
        "let sumTo n =\n    let mutable t = 0\n    for i = 1 to n do\n        t <- t + i\n    t\n"

        // Binding patterns that introduce NO single bound variable, or introduce one only behind
        // a wrapper the frozen tree erases: a producer filing these under the bound PATTERN's key
        // would point to a node the frozen tree does not bear.
        "module-level tuple destructuring", "let p = (1, 2)\nlet (a, b) = p\nlet s = a + b\n"
        "module-level tuple destructuring without parens", "let p = (1, 2)\nlet a, b = p\nlet s = a + b\n"
        "module-level nested destructuring", "let p = ((1, 2), 3)\nlet ((a, b), c) = p\nlet s = a + b + c\n"
        "module-level record destructuring",
        "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet { X = xx; Y = yy } = r\nlet s = xx + yy\n"
        "module-level union destructuring", "type U = | A of int\nlet u = A 1\nlet (A n) = u\nlet s = n + 1\n"
        "destructuring pattern with an as-alias", "let p = (1, 2)\nlet (a, b) as q = p\nlet s = a + b\n"
        "module-level wildcard binding", "let _ = 5\n"
        "parenthesised simple binding pattern", "let (x) = 5\nlet y = x + 1\n"
        "annotated parenthesised simple binding pattern", "let (x: int) = 5\nlet y = x + 1\n"
        "parenthesised mutable binding pattern", "let mutable (m) = 1\nlet f () = m <- m + 1\n"
        "destructuring and wildcard patterns in a named module",
        "module M\n\nmodule N =\n    let p = (1, 2)\n    let (a, b) = p\n    let (z) = a\n    let _ = b\n"

        // The same non-bound-variable patterns INSIDE a body, which reach the pool through
        // `ClosureReprs` (the escape snapshot) rather than the module-binding tables.
        "wildcard binding in a function body", "let f x =\n    let _ = x\n    x\n"
        "wildcard binding in a lambda body", "let f = fun x ->\n    let _ = x\n    x\n"
        "parenthesised binding pattern in a function body", "let f x =\n    let (y) = x\n    y\n"
        "parenthesised use binding pattern", "let f (d: System.IDisposable) =\n    use (x) = d\n    1\n"
        "destructuring let ahead of a use in a function body",
        "let f (d: System.IDisposable * int) =\n    let (a, b) = d\n    use x = a\n    b\n"

        // An `inline` binding is in `Decls` like any other and, when publishable, is ALSO a second
        // tree under `InlineTemplates`. Both are pooled roots, so the same source bound variable is
        // reached twice — the intern is idempotent, so it lands on one `BoundVarId`.
        "top-level inline binding", "module M\nlet inline f x = x + 1\nlet y = f 2\n"
        "inline binding in a named module", "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\n"
        "nullary intrinsic alias binding", "module M\n\nmodule N =\n    let undef = (# \"undefined\" #)\n"

        // Pinned so a producer change cannot start filing these under a non-bound-variable key.
        "destructuring let in a function body", "let f (p: int * int) =\n    let (a, b) = p\n    a + b\n"
        "module-level operator binding", "module M\nlet (+.) a b = a + b\n"
        "lambda with a tuple parameter", "let f = fun (a, b) -> a + b\n"
        "or-pattern arm", "let f x =\n    match x with\n    | 0 | 1 -> 9\n    | _ -> 0\n"
        "type-test as-pattern arm",
        "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | _ -> 0\n"
        "class with a ctor-param-capturing member closure",
        "type C(a: int) =\n    member this.M(x: int) =\n        let g = fun y -> y + x + a\n        g 1\n"

        // A type declaration's BODIES are the only trees reached through the declaration shape
        // rather than through a decl's child columns. Each body slot also binds keys (`this`,
        // member / ctor parameters) that no pattern node introduces.
        "class with static and instance preamble entries",
        "type C(a: int) =\n    static let s = 1\n    let b = a + 1\n    do ()\n    member this.M() = b + s\n"
        "class with a secondary constructor", "type C(x: int) =\n    new() = C(0)\n    member this.X = x\n"
        "derived class with a base-ctor call",
        "type Shape(x: int) =\n    member this.Raw = x\n\ntype Circle(r: int, t: int) =\n    inherit Shape(t)\n    member this.Radius = r\n"
        "class with an interface implementation",
        "type IBox =\n    abstract member Unwrap : unit -> int\n\ntype Box(value: int) =\n    interface IBox with\n        member this.Unwrap() : int = value\n"
    ]

[<Tests>]
let tests =
    testList
        "TastPools.build mirrors the DU"
        [
            for name, src in programs do
                test name { checkProgram src }
        ]

// WHICH FILE the anchor columns index is a column-set-wide fact, so no walk of the tree checks
// it and the round-trip gate above passes with it dropped. It must be the identity the analysis
// ran under: one rebuilt downstream from a path compares unequal, reading as "all foreign".
[<Tests>]
let unitStampTests =
    testList
        "TastPools states which file the anchors index"
        [
            test "the freeze stamps the origin it was analysed under" {
                let origin, frozen = freezeWithOrigin "let a = 1\n"

                Expect.equal frozen.Path origin.Path "the pools name the file the anchors were taken from"
            }

            test "re-pooling an unpooled tree keeps it" {
                // The unpool carries no origin — the tree has no field for one — so the fill
                // has only the pool it came out of to take it from.
                let origin, frozen = freezeWithOrigin "let a = 1\n"

                Expect.equal
                    (TastPools.rePool frozen (TastUnpool.ofPools frozen)).Path
                    origin.Path
                    "the re-fill kept the file, rather than defaulting to nobody's"
            }
        ]

// Elaboration drops a declaration it cannot translate while the side tables keyed by that
// declaration's bound variables survive, so the pool build PROJECTS each side table onto the
// bound variables it interned. The alternative, refusing to freeze the file at all, empties the
// surface it publishes and turns one fault into a fault per later file of the assembly.
[<Tests>]
let freezeProjectionTests =
    testList
        "Freezing a file whose analysis reported an error"
        [
            test "a match on an unresolved case surfaces the name error rather than a pool break" {
                let src = "let w = Wrap 1\nlet v = match w with | Wrap x -> x\n"
                let lexed, file = parseFile src
                let origin = LexedFile.inAssembly testAsm (AssemblyFileId.ofText src) lexed
                let pools = Pipeline.analyseFor testCompiling realProvider.Value origin file

                Expect.isNonEmpty
                    (pools.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "the unresolved-identifier error reaches the caller"
            }

            test "the declarations that did resolve survive the freeze" {
                let src = "let good = 1\nlet bad = notDefinedAnywhere 2\nlet alsoGood = 3\n"
                let lexed, file = parseFile src
                let origin = LexedFile.inAssembly testAsm (AssemblyFileId.ofText src) lexed
                let pools = Pipeline.analyseFor testCompiling realProvider.Value origin file

                Expect.isNonEmpty
                    (pools.Residue.Diagnostics |> List.filter Diagnostic.isError)
                    "the unresolved-identifier error reaches the caller"

                let names = Set.ofArray pools.BoundVarNames
                Expect.isTrue (names.Contains "good") "the binding before the fault is frozen"
                Expect.isTrue (names.Contains "alsoGood") "the binding after the fault is frozen"
            }
        ]

[<Tests>]
let boundVarAnchorTests =
    testList
        "TastPools names a bound variable where the source writes it"
        [
            // Each program checks the obligation on its own bound variables; the aggregate below
            // asserts the set is non-vacuous rather than passing trivially.
            for name, src in programs do
                test name {
                    let pools, _ = poolsFor src
                    checkBoundVarNames src pools |> ignore
                }

            test "the program set exercises source-written bound variables" {
                let total =
                    programs
                    |> List.sumBy (fun (_, src) ->
                        let pools, _ = poolsFor src
                        checkBoundVarNames src pools
                    )

                Expect.isGreaterThan total 0 "at least one bound variable is named at its own anchor"
            }
        ]

// The round-trip gate above proves faithfulness only for the domains the program set actually
// CONTAINS: a file with no type declaration proves nothing about pooled member bodies. So count
// member bodies, inline templates and tuple groups, and fail if the set stops populating one.

/// Counted over one program's pools. A tuple group points to a lambda parameter node rather than a
/// pooled copy, so its count is coverage of the derivation, not of a carrier of its own.
let private carrierCounts (pools: FrozenPools) =
    let memberBodies =
        pools.DeclPayloads
        |> Array.sumBy (fun p ->
            match p with
            | DeclPayload.Type td -> (bodySlots td).Length
            | DeclPayload.Let _
            | DeclPayload.Expression _ -> 0
        )

    let tupleGroups =
        pools.BindingValReprs
        |> Array.sumBy (fun (_, vr) ->
            vr.Groups
            |> List.sumBy (fun g ->
                match g with
                | ArgGroupG.GTuple _ -> 1
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> 0
            )
        )

    {|
        MemberBodies = memberBodies
        InlineTemplates = pools.InlineTemplates.Length
        TupleGroups = tupleGroups
    |}

[<Tests>]
let pooledCarrierCoverageTests =
    testList
        "TastPools holds every tree the frozen file bears"
        [
            test "the program set populates all three formerly-opaque carriers" {
                let totals =
                    programs
                    |> List.map (fun (_, src) -> carrierCounts (fst (poolsFor src)))
                    |> List.fold
                        (fun (b, i, t) c -> b + c.MemberBodies, i + c.InlineTemplates, t + c.TupleGroups)
                        (0, 0, 0)

                let (memberBodies, inlineTemplates, tupleGroups) = totals
                Expect.isGreaterThan memberBodies 0 "a type declaration's member bodies are pooled"
                Expect.isGreaterThan inlineTemplates 0 "an inline template is pooled as its own root"
                Expect.isGreaterThan tupleGroups 0 "a ValRepr tuple group's pattern is pooled"
            }

            // Every id the three carriers name must resolve into the columns — the property a
            // structural round-trip satisfies vacuously when a carrier is empty.
            test "every carrier id indexes its own pool" {
                for _, src in programs do
                    let pools, _ = poolsFor src

                    let inExprs (ExprPoolId i) =
                        Expect.isTrue (i >= 0 && i < pools.ExprPayloads.Length) "body id is an expr pool entry"

                    for p in pools.DeclPayloads do
                        match p with
                        | DeclPayload.Type td -> Array.iter inExprs (bodySlots td)
                        | DeclPayload.Let _
                        | DeclPayload.Expression _ -> ()

                    for t in pools.InlineTemplates do
                        let (DeclPoolId i) = t.Decl
                        Expect.isTrue (i >= 0 && i < pools.DeclPayloads.Length) "template id is a decl pool entry"

                    for _, vr in pools.BindingValReprs do
                        for g in vr.Groups do
                            match g with
                            | ArgGroupG.GTuple(PatPoolId i) ->
                                Expect.isTrue (i >= 0 && i < pools.PatPayloads.Length) "group id is a pat pool entry"

                                Expect.equal
                                    (PatPayload.shape pools.PatPayloads.[i])
                                    PatShape.Tuple
                                    "a GTuple names a Tuple pattern"
                            | ArgGroupG.GUnit _
                            | ArgGroupG.GSimple _ -> ()
            }
        ]

// The corpus never populates `FunVerdicts`, so the round-trip gate above never exercises the
// lambda id space. These inject a synthetic verdict keyed by a real frozen lambda's own anchor.
// The key is one-to-many over the id space, so the bearer set is asserted directly.

/// Every pooled `Lambda` id, grouped by the key its verdict resolves through.
let private lambdasByKey (pools: FrozenPools) : (LambdaKey * ExprPoolId list) list =
    [
        for i in 0 .. pools.ExprPayloads.Length - 1 do
            if ExprPayload.shape pools.ExprPayloads.[i] = ExprShape.Lambda then
                yield pooledLambdaKey pools (ExprPoolId i), ExprPoolId i
    ]
    |> List.groupBy fst
    |> List.map (fun (k, xs) -> k, List.map snd xs)

/// The first `Lambda` expr pool entry's key — the frozen lambda to key the verdict on.
let private firstLambdaKey (pools: FrozenPools) : LambdaKey =
    seq { 0 .. pools.ExprPayloads.Length - 1 }
    |> Seq.pick (fun i ->
        match ExprPayload.shape pools.ExprPayloads.[i] with
        | ExprShape.Lambda -> Some(pooledLambdaKey pools (ExprPoolId i))
        | _ -> None
    )

[<Tests>]
let funVerdictLambdaKeyTests =
    testList
        "TastPools re-keys FunVerdicts onto the lambda id space"
        [
            test "a lambda-keyed verdict pools by ExprPoolId and inverts to its NodeKey" {
                let src = "let f = fun x -> x + 1\n"
                let _, frozen = poolsFor src
                let rePool = rePoolFor src
                let lamKey = firstLambdaKey (rePool frozen)

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                let injected =
                    { frozen with
                        FunVerdicts = Map.ofList [ lamKey, verdict ]
                    }

                let pools = rePool injected

                // (a) the pool-form table is keyed by that lambda's `ExprPoolId`.
                Expect.equal pools.FunVerdicts.Length 1 "one pooled verdict"
                let (ExprPoolId i, v) = pools.FunVerdicts.[0]
                Expect.equal v verdict "pooled verdict value preserved"

                Expect.equal
                    (ExprPayload.shape pools.ExprPayloads.[i])
                    ExprShape.Lambda
                    "verdict id names a Lambda entry"

                Expect.equal (pooledLambdaKey pools (ExprPoolId i)) lamKey "verdict's ExprPoolId is the keyed lambda's"

                // (b) `ofPools` reconstructs the map under the ORIGINAL lambda NodeKey.
                let rebuilt = TastUnpool.ofPools pools
                Expect.equal rebuilt.FunVerdicts.Count 1 "one rebuilt verdict"

                Expect.equal
                    (Map.tryFind lamKey rebuilt.FunVerdicts)
                    (Some verdict)
                    "verdict rebuilt under its lambda key"
            }

            test "a FunVerdicts key that does not point to a pooled lambda is dropped" {
                let src = "let f = fun x -> x + 1\n"
                let _, frozen = poolsFor src
                let rePool = rePoolFor src

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                // A key on a token index past the end of any lexed file — a lambda-keyed entry
                // that does not point to a pooled lambda, the shape a file whose elaboration
                // dropped the enclosing declaration arrives in.
                let bogus = LambdaKey(Anchor.ofStored 1_000_000)

                let injected =
                    { frozen with
                        FunVerdicts = Map.ofList [ bogus, verdict ]
                    }

                Expect.isEmpty (rePool injected).FunVerdicts "the unresolved lambda-keyed verdict is projected away"
            }

            test "a verdict reaches EVERY pooled lambda its key identifies" {
                // A published `inline` binding makes the key one-to-many: the published template
                // and the ordinary function the binding is emitted as are two trees over the SAME
                // source, so their lambdas anchor on the same token.
                let src = "module M\n\nmodule N =\n    let inline addOne x = x + 1\n"
                let _, frozen = poolsFor src
                let rePool = rePoolFor src

                let shared, bearers =
                    rePool frozen |> lambdasByKey |> List.find (fun (_, ids) -> List.length ids > 1)

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                let pools =
                    rePool
                        { frozen with
                            FunVerdicts = Map.ofList [ shared, verdict ]
                        }

                // Every bearer takes a row. Resolving to ONE id leaves the copies codegen walks
                // with no verdict, and nothing downstream reports the miss.
                Expect.equal
                    (pools.FunVerdicts |> Array.map fst |> Set.ofArray)
                    (Set.ofList bearers)
                    "every lambda the key names has a row"

                Expect.all pools.FunVerdicts (fun (_, v) -> v = verdict) "each row carries the keyed verdict"

                // The inverse folds the rows back onto the one key they came from.
                let rebuilt = TastUnpool.ofPools pools
                Expect.equal rebuilt.FunVerdicts.Count 1 "the unpool restores the source map's arity"

                Expect.equal
                    (Map.tryFind shared rebuilt.FunVerdicts)
                    (Some verdict)
                    "verdict rebuilt under its lambda key"
            }
        ]

// The REACHABILITY half of side-table identity: an entry keyed by a real bound variable whose
// declaration was then dropped, as a producer that prunes a decl and forgets its entry leaves
// behind. The fault must identify WHICH table still holds it — several producers file into several.

/// Two module bindings that nothing references, so dropping the second takes nothing with it: a
/// surviving `Var` referencing it would fault as an incomplete enumeration — a different failure, and
/// the test would pass for the wrong reason.
let private staleEntrySrc = "module M\n\nmodule N =\n    let a = 1\n    let b = 2\n"

/// `staleEntrySrc` frozen with its LAST binding's declaration removed AND every side-table entry
/// for that bound variable stripped. Re-adding the entry to exactly one table is then what makes
/// the reported table name unambiguous.
let private lastBindingDropped () =
    let _, frozen = poolsFor staleEntrySrc
    let rePool = rePoolFor staleEntrySrc
    let decls = EqArray.toArray frozen.Decls

    let index, boundVar =
        seq { 0 .. decls.Length - 1 }
        |> Seq.rev
        |> Seq.pick (fun i ->
            match decls.[i] with
            | TDeclG.Let(pattern = pattern) ->
                match BoundVarKey.ofPat pattern with
                | ValueSome b -> Some(i, b)
                | ValueNone -> None
            | TDeclG.Expression _
            | TDeclG.Type _ -> None
        )

    {|
        BoundVar = boundVar
        // The fill the injected trees below go through, carrying the naming column of the
        // freeze they were unpooled out of.
        RePool = rePool
        // Kept so the stale entry re-added below is the producer's own value, not a
        // fabricated one — the entry is genuine; only its declaration is gone.
        Member = Map.find boundVar frozen.ModuleMembers
        Pruned =
            { frozen with
                Decls = EqArray.ofArray (Array.removeAt index decls)
                ModuleMembers = Map.remove boundVar frozen.ModuleMembers
                ClosureReprs = Map.remove boundVar frozen.ClosureReprs
                GenericFnSchemes = Map.remove boundVar frozen.GenericFnSchemes
                BindingTyparArities = Map.remove boundVar frozen.BindingTyparArities
            }
    |}

[<Tests>]
let staleSideTableEntryTests =
    testList
        "TastPools projects a side-table entry whose declaration left the tree"
        [
            // The control: pruning the declaration and its entries together pools cleanly, so the
            // projections below are what changes, not the missing declaration.
            test "a declaration pruned together with its entries pools cleanly" {
                let dropped = lastBindingDropped ()
                dropped.RePool dropped.Pruned |> ignore
            }

            test "a retained ModuleMembers entry is dropped" {
                let dropped = lastBindingDropped ()

                let injected =
                    { dropped.Pruned with
                        ModuleMembers = Map.add dropped.BoundVar dropped.Member dropped.Pruned.ModuleMembers
                    }

                Expect.equal
                    (dropped.RePool injected).ModuleMembers.Length
                    (dropped.RePool dropped.Pruned).ModuleMembers.Length
                    "the stale entry adds no row"
            }

            // The SAME dropped bound variable in a different table. `BindingTyparArities` reaches
            // the pools as a `BoundVarColumn` — no key at all — so a stale entry has no slot to
            // land in and is projected away the same way.
            test "a retained BindingTyparArities entry is dropped" {
                let dropped = lastBindingDropped ()

                let injected =
                    { dropped.Pruned with
                        BindingTyparArities = Map.add dropped.BoundVar 0 dropped.Pruned.BindingTyparArities
                    }

                Expect.equal
                    ((dropped.RePool injected).BindingTyparArities
                     |> Array.filter ValueOption.isSome
                     |> Array.length)
                    ((dropped.RePool dropped.Pruned).BindingTyparArities
                     |> Array.filter ValueOption.isSome
                     |> Array.length)
                    "the stale entry fills no slot"
            }

            // `ChildColumn` is CSR: `Start` and `Ids` are two independently length-prefixed wire
            // arrays that can disagree. A disagreement does not fault on read — it silently
            // re-parents every slot past it — so `ofStored` checks, and these gate its four rules.
            testList
                "ChildColumn.ofStored rejects a malformed stored column"
                [
                    // A well-formed one, so the rejections below are not passing vacuously.
                    test "a well-formed column is admitted and reads back" {
                        let col = ChildColumn.ofStored [| 0; 2; 2; 3 |] [| 10; 11; 12 |]
                        Expect.equal (ChildColumn.length col) 3 "three slots for four starts"
                        Expect.equal (ChildColumn.count col 0) 2 "slot 0 fan-out"
                        Expect.equal (ChildColumn.slice col 0) [| 10; 11 |] "slot 0 children"
                        Expect.equal (ChildColumn.count col 1) 0 "an empty slot"
                        Expect.equal (ChildColumn.item col 2 0) 12 "the last slot needs no special case"
                    }

                    test "an empty start array is rejected" {
                        // Not the same as the empty COLUMN, which is `[| 0 |]` — CSR always
                        // carries n+1 entries, so zero of them is no column at all.
                        Expect.throws (fun () -> ChildColumn.ofStored [||] [||] |> ignore) "no slot-start array"
                    }

                    test "a column not starting at 0 is rejected" {
                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 1; 1 |] [||] |> ignore)
                            "starts must begin at 0"
                    }

                    test "decreasing slot starts are rejected" {
                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 0; 2; 1; 2 |] [| 10; 11 |] |> ignore)
                            "starts must be monotone"
                    }

                    test "starts that do not end at the id count are rejected" {
                        // The one a truncated or over-long `Ids` array produces, and the one
                        // that would otherwise read a valid-but-wrong child of the last slot.
                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 0; 2 |] [| 10; 11; 12 |] |> ignore)
                            "a trailing id no slot claims"

                        Expect.throws
                            (fun () -> ChildColumn.ofStored [| 0; 3 |] [| 10; 11 |] |> ignore)
                            "a slot claiming ids that are not there"
                    }
                ]
        ]
