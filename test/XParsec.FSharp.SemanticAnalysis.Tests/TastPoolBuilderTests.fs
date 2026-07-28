module XParsec.FSharp.SemanticAnalysis.Tests.TastPoolBuilderTests

open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The stacking gate. `PoolBuilder` stacks an append-only overlay over an immutable
// `FrozenPools` and presents ONE flat id space, and everything a consumer may assume rests
// on base ids being preserved exactly — through reads and through mints. These tests pin
// that: a base id reads the base column even after the overlay has grown, every base root
// still drains to the decl it was pooled from, and an overlay node may name base children
// and drains into the right tree.

/// The value expr of the file's first `Let` decl, as a DU node — a real frozen subtree to
/// hand a mint site, and (with its root's `DeclExprChildren`) its base pool id.
let private firstLetValue (frozen: Pooled.TastFile) : Pooled.TExpr =
    EqArray.toArray frozen.Decls
    |> Array.pick (fun d ->
        match d with
        | TDeclG.Let(value = value) -> Some value
        | _ -> None
    )

/// Every base id, in every domain, resolves through the builder to EXACTLY the column
/// value the base pool holds. Run against a builder that has already grown an overlay, so
/// a layer check that got its boundary wrong (or an overlay that shadowed the base) shows
/// up here rather than only at the freeze.
let private checkBaseIdsResolve (pools: FrozenPools) (b: PoolBuilder) =
    for i in 0 .. pools.ExprPayloads.Length - 1 do
        let id = ExprPoolId i
        Expect.equal (TastPoolBuilder.exprTy b id) pools.ExprTys.[i] "base expr ty"
        Expect.equal (TastPoolBuilder.exprTok b id) (Anchor.ofColumn pools.ExprToks.[i]) "base expr tok"
        Expect.equal (TastPoolBuilder.exprChildren b id) pools.ExprChildren.[i] "base expr children"
        Expect.equal (TastPoolBuilder.exprPatChildren b id) pools.ExprPatChildren.[i] "base expr pat children"
        Expect.equal (TastPoolBuilder.exprVarBinder b id) pools.ExprVarBinder.[i] "base expr var binder"
        Expect.equal (TastPoolBuilder.exprPayload b id) pools.ExprPayloads.[i] "base expr payload"

    for i in 0 .. pools.PatPayloads.Length - 1 do
        let id = PatPoolId i
        Expect.equal (TastPoolBuilder.patTy b id) pools.PatTys.[i] "base pat ty"
        Expect.equal (TastPoolBuilder.patTok b id) (Anchor.ofColumn pools.PatToks.[i]) "base pat tok"
        Expect.equal (TastPoolBuilder.patChildren b id) pools.PatChildren.[i] "base pat children"
        Expect.equal (TastPoolBuilder.patPayload b id) pools.PatPayloads.[i] "base pat payload"

    for i in 0 .. pools.DeclPayloads.Length - 1 do
        let id = DeclPoolId i
        Expect.equal (TastPoolBuilder.declExprChildren b id) pools.DeclExprChildren.[i] "base decl expr children"
        Expect.equal (TastPoolBuilder.declPatChildren b id) pools.DeclPatChildren.[i] "base decl pat children"
        Expect.equal (TastPoolBuilder.declPayload b id) pools.DeclPayloads.[i] "base decl payload"

    for i in 0 .. pools.BinderNames.Length - 1 do
        let id = BinderId i
        Expect.equal (TastPoolBuilder.binderTok b id) (Anchor.ofColumn pools.BinderToks.[i]) "base binder anchor"

        Expect.equal
            (TastPoolBuilder.binderNaming b id)
            (BinderNaming.ofColumn pools.BinderNames.[i] id)
            "base binder naming"

/// Every base root drains to the SAME decl before and after the overlay grows — the
/// end-to-end half of id preservation, through the ONE way out of a builder that production
/// uses (`declTree`, the cross-unit inline wire's drain). An id-space boundary error shows
/// up as a wrong or missing subtree; the drain is deterministic within a builder (its
/// re-minted binder keys are the builder's), so the two drains are directly comparable.
let private drainRoots (b: PoolBuilder) : Frozen.TDecl[] =
    TastPoolBuilder.roots b |> Array.map (TastPoolBuilder.declTree b)

// Programs spanning the domains the stack has to keep straight: a binder reference across
// decls (`Var` into the binder pool), a composite expr with swappable children, a pattern
// with sub-patterns, and a `for` loop's binder.
let private programs =
    [
        "let-bound reference", "let x = 1\nlet y = x\n"
        "tuple literal and tuple pattern", "let p = (1, 2)\nlet swap q =\n    match q with\n    | (a, b) -> (b, a)\n"
        "curried fn + call", "let add x y = x + y\nlet answer = add 1 40\n"
        "for-to loop", "let sumTo n =\n    let mutable t = 0\n    for i = 1 to n do\n        t <- t + i\n    t\n"
    ]

[<Tests>]
let baseIdTests =
    testList
        "TastPoolBuilder preserves base ids"
        [
            for name, src in programs do
                test name {
                    let pools, frozen = poolsFor src
                    let b = TastPoolBuilder.openOver pools
                    let before = drainRoots b

                    // Grow the overlay: a re-pooled real subtree (which also names binder
                    // references) and a binder the base never held.
                    TastPoolBuilder.appendExprTree b (firstLetValue frozen) |> ignore
                    TastPoolBuilder.mintBinder b |> ignore

                    Expect.isGreaterThan
                        (TastPoolBuilder.exprCount b)
                        pools.ExprPayloads.Length
                        "the overlay grew past the base"

                    checkBaseIdsResolve pools b
                    Expect.equal (drainRoots b) before "every base root drains to the same decl it did before"
                }
        ]

[<Tests>]
let appendTests =
    testList
        "TastPoolBuilder appends into a fresh id space"
        [
            test "a pooled tree takes ids past the base and reads back" {
                let pools, frozen = poolsFor "let p = (1, 2)\n"
                let b = TastPoolBuilder.openOver pools
                let du = firstLetValue frozen
                let baseCount = pools.ExprPayloads.Length

                let id = TastPoolBuilder.appendExprTree b du
                let (ExprPoolId i) = id
                Expect.isGreaterThanOrEqual i baseCount "the appended root is an overlay id"

                // The binder the DU node introduces — already this pool's own id, the tree
                // being one drained from it, so the payload is checked against the identity
                // the NODE carries rather than against the payload itself.
                let duBinder = BinderKey.ofExpr du |> ValueOption.map BinderKey.identity

                // The tree is already in the stored anchor form, so the payload's own
                // anchor needs no narrowing — `Operators.id` because `id` is the pool id
                // in scope here.
                Expect.equal
                    (TastPoolBuilder.exprPayload b id)
                    (TastPools.exprPayload Operators.id duBinder du)
                    "appended payload"

                Expect.equal (TastPoolBuilder.exprTy b id) (TastWalk.exprTy du) "appended ty"
                Expect.equal (TastPoolBuilder.exprTok b id) (Anchor.ofColumn (TastWalk.exprTok du)) "appended tok"

                // `appendExprTree` pools the WHOLE tree it is handed — its children are
                // re-pooled into the overlay, not deduped against equal base rows.
                let kids = TastPoolBuilder.exprChildren b id

                Expect.equal kids.Length (TastPools.exprChildren du).Length "appended child fan-out"

                for (ExprPoolId k) in kids do
                    Expect.isGreaterThanOrEqual k baseCount "an appended child is an overlay id"
            }

            test "a minted Var resolves to the binder the base pool already interned" {
                let pools, frozen = poolsFor "let x = 1\nlet y = x\n"
                let b = TastPoolBuilder.openOver pools

                // `let y = x`'s value is the `Var` naming `x`'s binder — a reference the
                // overlay must resolve into the BASE binder pool rather than mint anew.
                let varDu =
                    EqArray.toArray frozen.Decls
                    |> Array.pick (fun d ->
                        match d with
                        | TDeclG.Let(value = TExprG.Var _ as value) -> Some value
                        | _ -> None
                    )

                let binderCountBefore = TastPoolBuilder.binderCount b
                let id = TastPoolBuilder.appendExprTree b varDu

                Expect.equal
                    (TastPoolBuilder.binderCount b)
                    binderCountBefore
                    "a reference to a known binder mints no binder"

                match TastPoolBuilder.exprVarBinder b id with
                | ValueSome binder ->
                    Expect.equal
                        binder
                        (match varDu with
                         | TExprG.Var(binding = b) -> b
                         | _ -> failtest "not a Var")
                        "the minted Var resolves to its own binder"

                    let (BinderId j) = binder
                    Expect.isLessThan j pools.BinderNames.Length "the binder id is the base pool's, not a fresh one"
                | ValueNone -> failtest "an appended Var carries no resolved binder id"
            }

            test "a minted binder extends the pool past the base" {
                let pools, _ = poolsFor "let x = 1\n"
                let b = TastPoolBuilder.openOver pools

                let id = TastPoolBuilder.mintBinder b
                let (BinderId i) = id
                Expect.equal i pools.BinderNames.Length "the minted binder takes the next flat id"

                Expect.equal
                    (TastPoolBuilder.binderNaming b id)
                    (BinderNaming.Minted id)
                    "a minted binder is named after its own slot"

                Expect.equal (TastPoolBuilder.binderTok b id) ValueNone "a minted binder anchors on nothing"

                // A second mint is a SECOND binder: there is nothing to intern against, so
                // the id space simply grows.
                Expect.notEqual (TastPoolBuilder.mintBinder b) id "each mint is its own binder"

                Expect.equal
                    (TastPoolBuilder.binderCount b)
                    (pools.BinderNames.Length + 2)
                    "exactly two binders were appended"

                // The base half of the binder space is untouched by the mints.
                for i in 0 .. pools.BinderNames.Length - 1 do
                    let baseId = BinderId i

                    Expect.equal
                        (TastPoolBuilder.binderNaming b baseId)
                        (BinderNaming.ofColumn pools.BinderNames.[i] baseId)
                        "base binder naming"
            }
        ]

[<Tests>]
let rowCopyTests =
    testList
        "TastPoolBuilder row copies rewrite without a per-case match"
        [
            test "substituted children rebuild into the expected tree" {
                let pools, frozen = poolsFor "let p = (1, 2)\n"
                Expect.equal (EqArray.toArray frozen.Decls).Length 1 "one decl"

                let b = TastPoolBuilder.openOver pools
                let root = pools.Roots.[0]
                Expect.equal (TastPoolBuilder.declShape b root) DeclShape.Let "the decl is a Let"

                let tuple = (TastPoolBuilder.declExprChildren b root).[0]
                Expect.equal (TastPoolBuilder.exprShape b tuple) ExprShape.Tuple "its value is a Tuple"

                let kids = TastPoolBuilder.exprChildren b tuple
                Expect.equal kids.Length 2 "a two-element tuple"

                // The overlay node names BASE children (the two element ids, reversed) —
                // the stack's load-bearing case: an edge minted above the boundary
                // addressing a node below it.
                let swapped =
                    TastPoolBuilder.copyExprWith
                        b
                        tuple
                        (fun row ->
                            { row with
                                Children = [| kids.[1]; kids.[0] |]
                            }
                        )

                Expect.notEqual swapped tuple "the copy is a new node"

                Expect.equal
                    (TastPoolBuilder.exprChildren b swapped)
                    [| kids.[1]; kids.[0] |]
                    "the copy carries the substituted edges"

                // Everything else is carried across untouched — the point of a row copy.
                Expect.equal (TastPoolBuilder.exprShape b swapped) ExprShape.Tuple "shape carried"
                Expect.equal (TastPoolBuilder.exprTy b swapped) (TastPoolBuilder.exprTy b tuple) "ty carried"
                Expect.equal (TastPoolBuilder.exprTok b swapped) (TastPoolBuilder.exprTok b tuple) "tok carried"

                let newRoot =
                    TastPoolBuilder.copyDeclWith
                        b
                        root
                        (fun row ->
                            { row with
                                ExprChildren = [| swapped |]
                            }
                        )

                // The oracle: the ORIGINAL root's own drain with the tuple's items reversed.
                // Taken through `declTree` so both sides speak the identity a drain hands
                // out — within one builder that is stable, so the comparison is exact.
                let original = TastPoolBuilder.declTree b root

                let expected =
                    match original with
                    | TDeclG.Let(
                        binding = binding; value = TExprG.Tuple(items, ty, tok); isInline = isInline; ty = declTy) ->
                        let reversed =
                            TExprG.Tuple(items |> EqArray.toArray |> Array.rev |> EqArray.ofArray, ty, tok)

                        TDeclG.Let(binding, reversed, isInline, declTy)
                    | _ -> failtest "the decl is not a `let` over a Tuple"

                // Drained through `declTree`: the derived decl is a node like any other,
                // reached by the id the copy returned. Nothing repoints the root — a
                // rewrite hands its caller the new id (`TastAccessor.mapDeclExpr`), which
                // is why the builder has no root-repointing seam.
                Expect.equal
                    (TastPoolBuilder.declTree b newRoot)
                    expected
                    "the derived decl is the original with the tuple's items swapped"

                Expect.equal (TastPoolBuilder.declTree b root) original "the original root is untouched by the copy"
            }

            test "a retype copies the row with a different type" {
                let pools, _ = poolsFor "let p = (1, 2)\n"
                let b = TastPoolBuilder.openOver pools
                let tuple = (TastPoolBuilder.declExprChildren b pools.Roots.[0]).[0]
                let element = (TastPoolBuilder.exprChildren b tuple).[0]
                let tupleTy = TastPoolBuilder.exprTy b tuple

                let retyped =
                    TastPoolBuilder.copyExprWith b element (fun row -> { row with Ty = tupleTy })

                Expect.notEqual retyped element "the retype is a new node"
                Expect.equal (TastPoolBuilder.exprTy b retyped) tupleTy "the retyped row carries the new type"

                Expect.equal (TastPoolBuilder.exprShape b retyped) (TastPoolBuilder.exprShape b element) "shape carried"

                Expect.equal
                    (TastPoolBuilder.exprPayload b retyped)
                    (TastPoolBuilder.exprPayload b element)
                    "payload carried"
            }

            test "a copy that changes nothing returns the original id" {
                let pools, _ = poolsFor "let p = (1, 2)\n"
                let b = TastPoolBuilder.openOver pools
                let root = pools.Roots.[0]
                let tuple = (TastPoolBuilder.declExprChildren b root).[0]

                // A rewrite walk that touches nothing must append nothing and leave every
                // id a consumer already holds pointing at the same node.
                Expect.equal
                    (TastPoolBuilder.copyExprWith
                        b
                        tuple
                        (fun row ->
                            { row with
                                Children = TastPoolBuilder.exprChildren b tuple
                            }
                        ))
                    tuple
                    "unchanged children reuse the row"

                Expect.equal
                    (TastPoolBuilder.copyExprWith
                        b
                        tuple
                        (fun row ->
                            { row with
                                Ty = TastPoolBuilder.exprTy b tuple
                            }
                        ))
                    tuple
                    "an unchanged type reuses the row"

                Expect.equal
                    (TastPoolBuilder.copyDeclWith
                        b
                        root
                        (fun row ->
                            { row with
                                ExprChildren = TastPoolBuilder.declExprChildren b root
                            }
                        ))
                    root
                    "unchanged decl edges reuse the row"

                Expect.equal (TastPoolBuilder.exprCount b) pools.ExprPayloads.Length "nothing was appended"
            }
        ]
