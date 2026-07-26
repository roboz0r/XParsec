module XParsec.FSharp.SemanticAnalysis.Tests.TastPoolsTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The pool-build faithfulness gate: freeze a program, build the pools, then walk the
// pools from the decl roots and assert the reconstructed tree equals the DU walk —
// same shapes in the same order and the same expr/pat child fan-out at every node. No
// pool holds a DU node (all three are struct-of-arrays), so every node is checked by its
// SHAPE column and child-id columns. The DU walk here re-derives children through
// `TastAccessor` INDEPENDENTLY of the builder (which walks the same accessor), so the two
// agreeing is the cross-check, not a tautology; the reconstruction follows the id columns
// into the dense pool arrays, so a mis-wired child edge shows up as a fan-out mismatch.

let private poolsFor (src: string) : FrozenPools * Frozen.TastFile =
    let lexed, file = parseFile src
    let frozen = Pipeline.analyseFor "TestAsm" realProvider.Value src lexed file
    TastPools.toPools frozen, frozen

let rec private checkPat (pools: FrozenPools) (PatPoolId i) (du: Frozen.TPat) =
    Expect.equal pools.PatShapes.[i] (TastAccessor.patKind du) "pat shape"
    let duKids = TastAccessor.patChildren du
    Expect.equal pools.PatChildren.[i].Length duKids.Length "pat child fan-out"
    Array.iter2 (checkPat pools) pools.PatChildren.[i] duKids

let rec private checkExpr (pools: FrozenPools) (ExprPoolId i) (du: Frozen.TExpr) =
    Expect.equal pools.ExprShapes.[i] (TastAccessor.exprKind du) "expr shape"
    let duExprKids = TastAccessor.exprChildren du
    let duPatKids = TastAccessor.exprPatChildren du
    Expect.equal pools.ExprChildren.[i].Length duExprKids.Length "expr child fan-out"
    Expect.equal pools.ExprPatChildren.[i].Length duPatKids.Length "expr's pat fan-out"
    Array.iter2 (checkExpr pools) pools.ExprChildren.[i] duExprKids
    Array.iter2 (checkPat pools) pools.ExprPatChildren.[i] duPatKids

/// A type declaration's body slots in traversal order, gathered by running the SAME
/// traversal the pool build and drain run (`TastConvert.typeDecl` at a collecting body
/// mapping). Reusing it is what keeps this check from drifting away from the set of slots
/// that are actually pooled — a newly-added body slot appears here for free.
let private bodySlots (td: TTypeDeclG<FrozenType, SyntaxToken, 'body>) : 'body[] =
    let slots = ResizeArray<'body>()

    TastConvert.typeDecl
        id
        (fun b ->
            slots.Add b
            b
        )
        td
    |> ignore

    slots.ToArray()

let private checkDecl (pools: FrozenPools) (DeclPoolId i) (du: Frozen.TDecl) =
    Expect.equal pools.DeclShapes.[i] (TastAccessor.declKind du) "decl shape"

    match TastAccessor.declKind du with
    | DeclShape.Let ->
        let v = TastAccessor.declLet du
        Expect.equal pools.DeclExprChildren.[i].Length 1 "let decl one value child"
        Expect.equal pools.DeclPatChildren.[i].Length 1 "let decl one binding child"
        checkExpr pools pools.DeclExprChildren.[i].[0] v.Value
        checkPat pools pools.DeclPatChildren.[i].[0] v.Binding
    | DeclShape.Expression ->
        Expect.equal pools.DeclExprChildren.[i].Length 1 "expression decl one child"
        Expect.equal pools.DeclPatChildren.[i].Length 0 "expression decl no pat child"
        checkExpr pools pools.DeclExprChildren.[i].[0] (TastAccessor.declExpression du)
    | DeclShape.Type ->
        Expect.equal pools.DeclExprChildren.[i].Length 0 "type decl surfaces no expr child"
        Expect.equal pools.DeclPatChildren.[i].Length 0 "type decl surfaces no pat child"

        // The member / preamble / ctor bodies are not children — they are named by id
        // INSIDE the payload's declaration shape, which is what keeps "which body fills
        // which slot" expressed by the shape. Zip the pooled ids against the DU's bodies
        // in slot order and check each pooled subtree against the one it stands for.
        match pools.DeclPayloads.[i] with
        | DeclPayload.Type td ->
            let ids = bodySlots td
            let bodies = bodySlots (TastAccessor.declType du)
            Expect.equal ids.Length bodies.Length "one pooled body id per type-decl body slot"
            Array.iter2 (checkExpr pools) ids bodies
        | p -> failtestf "a Type decl's pool payload is %A, not DeclPayload.Type" p

/// The id-resolution gate: the `ExprVarBinder` column is populated EXACTLY at the `Var`
/// slots (each to an in-range `BinderId`), and each of the seven side-table keys resolves
/// to a `BinderId`/`ExprPoolId`. A binder the enumeration missed shows up as an unresolved
/// reference (a `toPools` fault). That each `Var` resolves to its OWN binder key is proven
/// by the round-trip gate (which rebuilds every `Var.binding` from this column). The dense
/// side tables must also cover the source maps 1:1 — a dropped or duplicated key would
/// desync the rebuilt map from the original.
let private checkIdResolution (pools: FrozenPools) (frozen: Frozen.TastFile) =
    let binderKey (BinderId i) = pools.BinderKeys.[i]
    // `FunVerdicts` is keyed by the lambda id space, not the binder pool; the Node is gone,
    // so recompute the lambda key from the `ExprToks` column exactly as `ofPools` does.
    let lambdaKeyOf (ExprPoolId i) =
        NodeKey.ofToken pools.ExprToks.[i] NodeKind.ExprLambda

    for i in 0 .. pools.ExprShapes.Length - 1 do
        match pools.ExprShapes.[i], pools.ExprVarBinder.[i] with
        | ExprShape.Var, ValueSome(BinderId b) ->
            Expect.isTrue (b >= 0 && b < pools.BinderKeys.Length) "Var binder id is an interned binder"
        | ExprShape.Var, ValueNone -> failtest "a Var pool entry carries no resolved binder id"
        | _, ValueSome _ -> failtest "a non-Var pool entry carries a binder id"
        | _, ValueNone -> ()

    // Each dense side table is the source map re-keyed onto its id space: same cardinality,
    // and every dense key resolves (through the given resolver) to a NodeKey the source map
    // holds. Only the KEYS are compared — a dense value need not be the source value's type
    // (`BindingValReprs` pools its patterns), and the values' faithfulness is the
    // round-trip gate's business, not this one's.
    let checkTable (name: string) (resolve: 'id -> NodeKey) (dense: ('id * 'v)[]) (source: Map<NodeKey, 'w>) =
        Expect.equal dense.Length source.Count (name + " dense form covers the source map 1:1")

        for (id, _) in dense do
            Expect.isTrue (Map.containsKey (resolve id) source) (name + " dense key resolves to a source key")

    checkTable "ModuleMembers" binderKey pools.ModuleMembers frozen.ModuleMembers
    checkTable "TopLevelNames" binderKey pools.TopLevelNames frozen.TopLevelNames
    checkTable "ClosureReprs" binderKey pools.ClosureReprs frozen.ClosureReprs
    checkTable "FunVerdicts" lambdaKeyOf pools.FunVerdicts frozen.FunVerdicts
    checkTable "GenericFnSchemes" binderKey pools.GenericFnSchemes frozen.GenericFnSchemes
    checkTable "BindingValReprs" binderKey pools.BindingValReprs frozen.BindingValReprs
    checkTable "BindingTyparArities" binderKey pools.BindingTyparArities frozen.BindingTyparArities

/// The naming triple each binder entry carries must equal its retained `Key`'s
/// projections (`IsSynthetic`/`Offset`/`NameIndex`) — the guard that `toPools` sourced
/// `Naming` from the key itself, so the pool names a binder EXACTLY as `binderName` names
/// its `NodeKey`. This is what lets the naming data outlive the key at the backing flip.
let private checkBinderNaming (pools: FrozenPools) =
    Array.iter2
        (fun (naming: BinderNaming) (key: NodeKey) ->
            Expect.equal naming.IsSynthetic key.IsSynthetic "binder naming IsSynthetic tracks its key"
            Expect.equal naming.Offset key.Offset "binder naming Offset tracks its key"
            Expect.equal naming.NameIndex key.NameIndex "binder naming NameIndex tracks its key"
        )
        pools.BinderNamings
        pools.BinderKeys

// The mint invariant the naming-preserving backing flip rests on: a REAL (non-synthetic)
// binder's `Offset` IS the binder NODE's own token `StartIndex` (`NamedSimple.tok`, read
// via `patTok`; `ForTo.identTok`), so a real binder is name-recoverable from its node with
// no key. Walk the frozen tree via the accessor, correlate each simple binder back to the
// token that minted it, and assert equality — proving the pool's naming data is
// node-recoverable. A mismatch would contradict the plan's premise, so it FAILS the test
// (surfaced, not papered over). Returns the count of real binders checked so a test can
// assert non-vacuous coverage.
let private checkMintInvariant (frozen: Frozen.TastFile) : int =
    let mutable realBinders = 0

    let checkReal (k: NodeKey) (tok: SyntaxToken) (what: string) =
        if not k.IsSynthetic then
            realBinders <- realBinders + 1
            Expect.equal k.Offset tok.StartIndex (sprintf "real %s binder offset is its node token StartIndex" what)

    let rec walkPat (p: Frozen.TPat) =
        match TastAccessor.patBinder p with
        | ValueSome k -> checkReal k (TastAccessor.patTok p) "NamedSimple"
        | ValueNone -> ()

        for sub in TastAccessor.patChildren p do
            walkPat sub

    let rec walkExpr (e: Frozen.TExpr) =
        // `ForTo`'s loop binder is minted from `identTok`, which the `ForToView` does not
        // surface — reach it on the DU node directly (the pool retains the same node).
        match e with
        | TExprG.ForTo(var = var; identTok = identTok) -> checkReal var identTok "ForTo"
        | _ -> ()

        for pc in TastAccessor.exprPatChildren e do
            walkPat pc

        for ec in TastAccessor.exprChildren e do
            walkExpr ec

    for d in EqArray.toArray frozen.Decls do
        match TastAccessor.declKind d with
        | DeclShape.Let ->
            let v = TastAccessor.declLet d
            walkPat v.Binding
            walkExpr v.Value
        | DeclShape.Expression -> walkExpr (TastAccessor.declExpression d)
        | DeclShape.Type -> ()

    realBinders

let private checkProgram (src: string) =
    let pools, frozen = poolsFor src
    let duDecls = EqArray.toArray frozen.Decls
    Expect.equal pools.Roots.Length duDecls.Length "one root per emittable decl"
    Array.iter2 (checkDecl pools) pools.Roots duDecls
    checkIdResolution pools frozen
    checkBinderNaming pools

    // The interconversion gate: `ofPools ∘ toPools` reconstructs a structurally-equal
    // `Frozen.TastFile`. Compared DIRECTLY, DU value against DU value — the serializer is
    // no oracle here, because `FrozenCodec.flatten` itself pools the file, so flattening
    // both sides would compare `toPools (ofPools (toPools f))` with `toPools f` and prove
    // nothing about `ofPools`. `TastFileG.structurallyEqual` is the equality a whole-file
    // `=` cannot be (two fields are `IReadOnlyDictionary`, reference-equal only). The
    // rebuilt file's decl trees come from the pool ids and its side-table maps are re-keyed
    // through the binder pool (not shared from the source), so an inequality is a genuine
    // decl-tree OR key-remap divergence.
    Expect.isTrue
        (TastFileG.structurallyEqual (TastPools.ofPools pools) frozen)
        "ofPools (toPools f) round-trips to a structurally-equal frozen file"

// Representative programs, spanning binder shapes (lambda / let-in / for), control
// flow (if / match), and the type + value forms (record decl, record literal, field
// access) — enough distinct expr/pat shapes that a broken child edge in any of the
// common cases trips the walk.
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

        // Binding heads that introduce NO single binder, or introduce one only behind a
        // wrapper the frozen tree erases. Each of these once faulted `toPools`: the
        // side tables were filed under `CstKeys.ofBinding` (the head PATTERN's key),
        // which for these shapes names a node the frozen tree does not bear — a
        // `Pat.EnclosedBlock`/`Pat.As` wrapper `translatePat` drops, or a composite /
        // wildcard pattern that binds no name at all. The producers now file under the
        // binder the head introduces (`TastWalk.patBinder`), or under nothing.
        "module-level tuple destructuring", "let p = (1, 2)\nlet (a, b) = p\nlet s = a + b\n"
        "module-level tuple destructuring without parens", "let p = (1, 2)\nlet a, b = p\nlet s = a + b\n"
        "module-level nested destructuring", "let p = ((1, 2), 3)\nlet ((a, b), c) = p\nlet s = a + b + c\n"
        "module-level record destructuring",
        "type R = { X: int; Y: int }\nlet r = { X = 1; Y = 2 }\nlet { X = xx; Y = yy } = r\nlet s = xx + yy\n"
        "module-level union destructuring", "type U = | A of int\nlet u = A 1\nlet (A n) = u\nlet s = n + 1\n"
        "destructuring head with an as-alias", "let p = (1, 2)\nlet (a, b) as q = p\nlet s = a + b\n"
        "module-level wildcard binding", "let _ = 5\n"
        "parenthesised simple binding head", "let (x) = 5\nlet y = x + 1\n"
        "annotated parenthesised simple binding head", "let (x: int) = 5\nlet y = x + 1\n"
        "parenthesised mutable binding head", "let mutable (m) = 1\nlet f () = m <- m + 1\n"
        "destructuring and wildcard heads in a named module",
        "module M\n\nmodule N =\n    let p = (1, 2)\n    let (a, b) = p\n    let (z) = a\n    let _ = b\n"

        // The same non-binder heads INSIDE a body, which reach the pool through
        // `ClosureReprs` (the escape snapshot) rather than the module-binding tables.
        "wildcard binding in a function body", "let f x =\n    let _ = x\n    x\n"
        "wildcard binding in a lambda body", "let f = fun x ->\n    let _ = x\n    x\n"
        "parenthesised binding head in a function body", "let f x =\n    let (y) = x\n    y\n"
        "parenthesised use binding head", "let f (d: System.IDisposable) =\n    use (x) = d\n    1\n"
        "destructuring let ahead of a use in a function body",
        "let f (d: System.IDisposable * int) =\n    let (a, b) = d\n    use x = a\n    b\n"

        // Inline templates: `Freeze` partitions these out of `Decls`, so their binders
        // are NOT in the pooled tree — published ones ride `InlineBodies` (which the
        // pool's binder enumeration now covers), and a top-level one is published
        // nowhere, so its side-table entries leave the file with it.
        "top-level inline binding", "module M\nlet inline f x = x + 1\nlet y = f 2\n"
        "inline binding in a named module", "module M\n\nmodule N =\n    let inline f x = x + 1\n\nlet y = N.f 2\n"
        "nullary intrinsic alias binding", "module M\n\nmodule N =\n    let undef = (# \"undefined\" #)\n"

        // Shapes that were ALREADY sound, pinned here so a future producer change
        // cannot silently start filing them under a non-binder key.
        "destructuring let in a function body", "let f (p: int * int) =\n    let (a, b) = p\n    a + b\n"
        "module-level operator binding", "module M\nlet (+.) a b = a + b\n"
        "lambda with a tuple parameter", "let f = fun (a, b) -> a + b\n"
        "or-pattern arm", "let f x =\n    match x with\n    | 0 | 1 -> 9\n    | _ -> 0\n"
        "type-test as-pattern arm",
        "let f (x: int | string) =\n    match x with\n    | :? int as i -> i\n    | _ -> 0\n"
        "class with a ctor-param-capturing member closure",
        "type C(a: int) =\n    member this.M(x: int) =\n        let g = fun y -> y + x + a\n        g 1\n"

        // A type declaration's BODIES are pooled, and they are the only trees reached
        // through the declaration shape rather than through a decl's child columns. These
        // walk the remaining body slots — the static / instance preamble, a secondary
        // ctor's primary-chain args, a base-ctor call's args, an interface impl's members
        // — each of which also binds keys (`this`, member / ctor parameters) that no
        // pattern node introduces, so a `Var` naming one is exactly what the binder
        // enumeration must cover.
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

[<Tests>]
let binderNamingMintInvariantTests =
    testList
        "TastPools binder naming is node-recoverable (mint invariant)"
        [
            // Each program checks the invariant on its own binders; the aggregate asserts the
            // set is non-vacuous, so a program mix that surfaced no real binder would fail loud
            // rather than pass trivially. The `let`/`for`-heavy programs above cover both
            // `NamedSimple` and `ForTo` real binders.
            for name, src in programs do
                test name {
                    let _, frozen = poolsFor src
                    checkMintInvariant frozen |> ignore
                }

            test "the program set exercises real binders" {
                let total =
                    programs |> List.sumBy (fun (_, src) -> checkMintInvariant (snd (poolsFor src)))

                Expect.isGreaterThan total 0 "at least one real binder is correlated to its node token"
            }
        ]

// Every tree the frozen file bears is in the pools — the type declarations' member bodies,
// the inline vocabulary, and the `ValRepr` tuple-group patterns included. The round-trip
// gate above proves that faithfully, but only for the domains the program set actually
// CONTAINS: a file with no type declaration proves nothing about pooled member bodies. So
// count the three, and fail loudly if the set stops populating any of them.

/// The three formerly-opaque carriers, counted over one program's pools.
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
            // structural round-trip can satisfy vacuously if a carrier is empty, and the one
            // that would break first if a body/template/group were pooled into the wrong space.
            test "every carrier id indexes its own pool" {
                for _, src in programs do
                    let pools, _ = poolsFor src

                    let inExprs (ExprPoolId i) =
                        Expect.isTrue (i >= 0 && i < pools.ExprShapes.Length) "body id is an expr pool entry"

                    for p in pools.DeclPayloads do
                        match p with
                        | DeclPayload.Type td -> Array.iter inExprs (bodySlots td)
                        | DeclPayload.Let _
                        | DeclPayload.Expression _ -> ()

                    for t in pools.InlineTemplates do
                        let (DeclPoolId i) = t.Decl
                        Expect.isTrue (i >= 0 && i < pools.DeclShapes.Length) "template id is a decl pool entry"

                    for _, vr in pools.BindingValReprs do
                        for g in vr.Groups do
                            match g with
                            | ArgGroupG.GTuple(PatPoolId i) ->
                                Expect.isTrue (i >= 0 && i < pools.PatShapes.Length) "group id is a pat pool entry"
                                Expect.equal pools.PatShapes.[i] PatShape.Tuple "a GTuple names a Tuple pattern"
                            | ArgGroupG.GUnit _
                            | ArgGroupG.GSimple _ -> ()
            }
        ]

// The corpus never populates `FunVerdicts` (the value-struct / stack-closure emit path
// is not yet reachable), so the round-trip gate above never exercises the lambda id
// space. These inject a synthetic verdict keyed by a real frozen lambda's `lambdaKey` and
// drive the path directly: `FunVerdicts` is re-keyed onto the lambda's `ExprPoolId` (off
// the binder pool), `ofPools` inverts back to the original lambda `NodeKey`, and a key
// naming no pooled lambda faults.

/// The first `Lambda` expr pool entry's key — the frozen lambda to key the verdict on,
/// recomputed from its `ExprToks` column (the Node is gone) as `ofPools` does.
let private firstLambdaKey (pools: FrozenPools) : NodeKey =
    seq { 0 .. pools.ExprShapes.Length - 1 }
    |> Seq.pick (fun i ->
        match pools.ExprShapes.[i] with
        | ExprShape.Lambda -> Some(NodeKey.ofToken pools.ExprToks.[i] NodeKind.ExprLambda)
        | _ -> None
    )

[<Tests>]
let funVerdictLambdaKeyTests =
    testList
        "TastPools re-keys FunVerdicts onto the lambda id space"
        [
            test "a lambda-keyed verdict pools by ExprPoolId and inverts to its NodeKey" {
                let _, frozen = poolsFor "let f = fun x -> x + 1\n"
                let lamKey = firstLambdaKey (TastPools.toPools frozen)

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                let injected =
                    { frozen with
                        FunVerdicts = Map.ofList [ lamKey, verdict ]
                    }

                let pools = TastPools.toPools injected

                // (a) the pool-form table is keyed by that lambda's `ExprPoolId`.
                Expect.equal pools.FunVerdicts.Length 1 "one pooled verdict"
                let (ExprPoolId i, v) = pools.FunVerdicts.[0]
                Expect.equal v verdict "pooled verdict value preserved"
                Expect.equal pools.ExprShapes.[i] ExprShape.Lambda "verdict id names a Lambda entry"

                Expect.equal
                    (NodeKey.ofToken pools.ExprToks.[i] NodeKind.ExprLambda)
                    lamKey
                    "verdict's ExprPoolId is the keyed lambda's"

                // (b) `ofPools` reconstructs the map under the ORIGINAL lambda NodeKey.
                let rebuilt = TastPools.ofPools pools
                Expect.equal rebuilt.FunVerdicts.Count 1 "one rebuilt verdict"

                Expect.equal
                    (Map.tryFind lamKey rebuilt.FunVerdicts)
                    (Some verdict)
                    "verdict rebuilt under its lambda key"
            }

            test "a FunVerdicts key naming no pooled lambda faults in toPools" {
                let _, frozen = poolsFor "let f = fun x -> x + 1\n"

                let verdict: FunVerdict =
                    {
                        Arity = 1
                        ResultTyparPos = ValueNone
                    }

                // An `ExprLambda` key at an offset no lambda token carries — a lambda-keyed
                // entry naming no pooled lambda, the honest failure the resolver surfaces.
                let bogus = NodeKey.ofSource 1_000_000 NodeKind.ExprLambda

                let injected =
                    { frozen with
                        FunVerdicts = Map.ofList [ bogus, verdict ]
                    }

                Expect.throws (fun () -> TastPools.toPools injected |> ignore) "unresolved lambda-keyed verdict faults"
            }
        ]
