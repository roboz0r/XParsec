module XParsec.FSharp.SemanticAnalysis.Tests.TastPoolsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The pool-build faithfulness gate: freeze a program, build the pools, then walk the
// pools from the decl roots and assert the reconstructed tree equals the DU walk —
// same shapes in the same order, the same expr/pat child fan-out at every node, and
// the same underlying node at every id. The DU walk here re-derives children through
// `TastAccessor` INDEPENDENTLY of the builder (which walks the same accessor), so the
// two agreeing is the cross-check, not a tautology; the reconstruction follows the id
// columns into the dense pool arrays, so a mis-wired child edge shows up as a fan-out
// or node mismatch.

let private poolsFor (src: string) : FrozenPools * Frozen.TastFile =
    let lexed, file = parseFile src
    let frozen = Pipeline.analyseFor "TestAsm" realProvider.Value src lexed file
    TastPools.toPools frozen, frozen

/// A pool entry's `Node` must be the very DU node reached independently — reference
/// identity, so faithfulness cannot hide behind structural equality of two distinct
/// nodes.
let private sameNode (poolNode: 'a) (duNode: 'a) (what: string) =
    Expect.isTrue (System.Object.ReferenceEquals(poolNode, duNode)) (sprintf "pool %s is the DU node" what)

let rec private checkPat (pools: FrozenPools) (PatPoolId i) (du: Frozen.TPat) =
    let entry = pools.Pats.[i]
    sameNode entry.Node du "pat node"
    Expect.equal entry.Shape (TastAccessor.patKind du) "pat shape"
    let duKids = TastAccessor.patChildren du
    Expect.equal entry.PatChildren.Length duKids.Length "pat child fan-out"
    Array.iter2 (checkPat pools) entry.PatChildren duKids

let rec private checkExpr (pools: FrozenPools) (ExprPoolId i) (du: Frozen.TExpr) =
    let entry = pools.Exprs.[i]
    sameNode entry.Node du "expr node"
    Expect.equal entry.Shape (TastAccessor.exprKind du) "expr shape"
    let duExprKids = TastAccessor.exprChildren du
    let duPatKids = TastAccessor.exprPatChildren du
    Expect.equal entry.ExprChildren.Length duExprKids.Length "expr child fan-out"
    Expect.equal entry.PatChildren.Length duPatKids.Length "expr's pat fan-out"
    Array.iter2 (checkExpr pools) entry.ExprChildren duExprKids
    Array.iter2 (checkPat pools) entry.PatChildren duPatKids

let private checkDecl (pools: FrozenPools) (DeclPoolId i) (du: Frozen.TDecl) =
    let entry = pools.Decls.[i]
    sameNode entry.Node du "decl node"
    Expect.equal entry.Shape (TastAccessor.declKind du) "decl shape"

    match TastAccessor.declKind du with
    | DeclShape.Let ->
        let v = TastAccessor.declLet du
        Expect.equal entry.ExprChildren.Length 1 "let decl one value child"
        Expect.equal entry.PatChildren.Length 1 "let decl one binding child"
        checkExpr pools entry.ExprChildren.[0] v.Value
        checkPat pools entry.PatChildren.[0] v.Binding
    | DeclShape.Expression ->
        Expect.equal entry.ExprChildren.Length 1 "expression decl one child"
        Expect.equal entry.PatChildren.Length 0 "expression decl no pat child"
        checkExpr pools entry.ExprChildren.[0] (TastAccessor.declExpression du)
    | DeclShape.Type ->
        Expect.equal entry.ExprChildren.Length 0 "type decl surfaces no expr child"
        Expect.equal entry.PatChildren.Length 0 "type decl surfaces no pat child"

/// The id-resolution gate: every reference the remap rewrote — each `Var.binding` and
/// each of the seven side-table keys — must resolve to a `BinderId` whose RETAINED
/// NodeKey equals the original content key. A binder the enumeration missed shows up as
/// an unresolved reference (a `toPools` fault) or, here, as a resolved id whose key does
/// not match. The dense side tables must also cover the source maps 1:1 — a dropped or
/// duplicated key would desync the rebuilt map from the original.
let private checkIdResolution (pools: FrozenPools) (frozen: Frozen.TastFile) =
    let binderKey (BinderId i) = pools.Binders.[i].Key

    for entry in pools.Exprs do
        match entry.Shape with
        | ExprShape.Var ->
            match entry.VarBinder with
            | ValueSome bid ->
                Expect.equal
                    (binderKey bid)
                    (TastAccessor.exprVarBinding entry.Node)
                    "Var resolves to its own binder key"
            | ValueNone -> failtest "a Var pool entry carries no resolved binder id"
        | _ -> ()

    // Each dense side table is the source map re-keyed by `BinderId`: same cardinality,
    // and every dense key resolves to a NodeKey the source map holds.
    let checkTable (name: string) (dense: (BinderId * 'v)[]) (source: Map<NodeKey, 'v>) =
        Expect.equal dense.Length source.Count (name + " dense form covers the source map 1:1")

        for (bid, _) in dense do
            Expect.isTrue (Map.containsKey (binderKey bid) source) (name + " dense key resolves to a source binder")

    checkTable "ModuleMembers" pools.ModuleMembers frozen.ModuleMembers
    checkTable "TopLevelNames" pools.TopLevelNames frozen.TopLevelNames
    checkTable "ClosureReprs" pools.ClosureReprs frozen.ClosureReprs
    checkTable "FunVerdicts" pools.FunVerdicts frozen.FunVerdicts
    checkTable "GenericFnSchemes" pools.GenericFnSchemes frozen.GenericFnSchemes
    checkTable "BindingValReprs" pools.BindingValReprs frozen.BindingValReprs
    checkTable "BindingTyparArities" pools.BindingTyparArities frozen.BindingTyparArities

let private checkProgram (src: string) =
    let pools, frozen = poolsFor src
    let duDecls = EqArray.toArray frozen.Decls
    Expect.equal pools.Roots.Length duDecls.Length "one root per emittable decl"
    Array.iter2 (checkDecl pools) pools.Roots duDecls
    checkIdResolution pools frozen

    // The interconversion gate: `ofPools ∘ toPools` reconstructs a structurally-equal
    // `Frozen.TastFile`. Structural equality is by the serializer (the round-trip oracle
    // the frozen-cache tests already use) — asserting the whole file by `=` is the wrong
    // contract (side-table map ordering is free to differ). The rebuilt file's decl trees
    // come from the pool ids and its side-table maps are re-keyed through the binder pool
    // (not shared from the source), so a flatten difference is a genuine decl-tree OR
    // key-remap divergence.
    Expect.equal
        (FrozenCodec.flatten (TastPools.ofPools pools))
        (FrozenCodec.flatten frozen)
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
    ]

[<Tests>]
let tests =
    testList
        "TastPools.build mirrors the DU"
        [
            for name, src in programs do
                test name { checkProgram src }
        ]
