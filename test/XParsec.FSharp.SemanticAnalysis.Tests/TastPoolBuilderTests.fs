module XParsec.FSharp.SemanticAnalysis.Tests.TastPoolBuilderTests

open Vesper
open Expecto
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The stacking gate. `PoolBuilder` stacks an append-only overlay over an immutable `FrozenPools`
// and presents ONE flat id space, so what is pinned here is that base ids survive exactly: a base
// id reads the base column after the overlay has grown, and every base root unpools unchanged.

/// The value expr of the file's first `Let` decl, as a DU node — a real frozen subtree to
/// hand a mint site, and (with its root's `DeclExprChildren`) its base pool id.
let private firstLetValue (frozen: Pooled.TastFile) : Pooled.TExpr =
    Block.toArray frozen.Decls
    |> Array.pick (fun d ->
        match d with
        | TDeclG.Let(binding = { Value = value }) -> Some value
        | _ -> None
    )

/// Every base id, in every domain, resolves through the builder to EXACTLY the column value the
/// base pool holds — run against a builder that has already grown an overlay. Both child read
/// paths (whole list, one child by position) are asserted: each has its own layer arithmetic.
let private checkBaseIdsResolve (pools: FrozenPools) (b: PoolBuilder) =
    let inline positionally (kids: 'id[]) (byIndex: int -> 'id) (what: string) =
        for k in 0 .. kids.Length - 1 do
            Expect.equal (byIndex k) kids.[k] what

    for i in 0 .. pools.ExprPayloads.Length - 1 do
        let id = ExprPoolId i
        // The `ty` column holds a row of the base pool's own type table, so the expected value
        // is that row resolved against the table the id belongs to.
        Expect.equal (TastPoolBuilder.exprTy b id) pools.Types.[pools.ExprTys.[i]] "base expr ty"
        Expect.equal (TastPoolBuilder.exprTok b id) pools.ExprToks.[i] "base expr tok"
        let kids = ChildColumn.slice pools.ExprChildren i
        let patKids = ChildColumn.slice pools.ExprPatChildren i
        Expect.equal (TastPoolBuilder.exprChildren b id) kids "base expr children"
        Expect.equal (TastPoolBuilder.exprPatChildren b id) patKids "base expr pat children"
        Expect.equal (TastPoolBuilder.exprChildCount b id) kids.Length "base expr child count"
        positionally kids (TastPoolBuilder.exprChild b id) "base expr child by position"
        positionally patKids (TastPoolBuilder.exprPatChild b id) "base expr pat child by position"
        Expect.equal (TastPoolBuilder.exprVarBoundVar b id) pools.ExprVarBoundVar.[i] "base expr var bound variable"
        Expect.equal (TastPoolBuilder.exprPayload b id) pools.ExprPayloads.[i] "base expr payload"

    for i in 0 .. pools.PatPayloads.Length - 1 do
        let id = PatPoolId i
        Expect.equal (TastPoolBuilder.patTy b id) pools.Types.[pools.PatTys.[i]] "base pat ty"
        Expect.equal (TastPoolBuilder.patTok b id) pools.PatToks.[i] "base pat tok"
        let kids = ChildColumn.slice pools.PatChildren i
        Expect.equal (TastPoolBuilder.patChildren b id) kids "base pat children"
        positionally kids (TastPoolBuilder.patChild b id) "base pat child by position"
        Expect.equal (TastPoolBuilder.patPayload b id) pools.PatPayloads.[i] "base pat payload"

    for i in 0 .. pools.DeclPayloads.Length - 1 do
        let id = DeclPoolId i
        let kids = ChildColumn.slice pools.DeclExprChildren i
        let patKids = ChildColumn.slice pools.DeclPatChildren i
        Expect.equal (TastPoolBuilder.declExprChildren b id) kids "base decl expr children"
        Expect.equal (TastPoolBuilder.declPatChildren b id) patKids "base decl pat children"
        positionally kids (TastPoolBuilder.declExprChild b id) "base decl expr child by position"
        positionally patKids (TastPoolBuilder.declPatChild b id) "base decl pat child by position"
        Expect.equal (TastPoolBuilder.declPayload b id) pools.DeclPayloads.[i] "base decl payload"

    for i in 0 .. pools.BoundVarNames.Length - 1 do
        let id = BoundVarId i
        Expect.equal (TastPoolBuilder.boundVarTok b id) pools.BoundVarToks.[i] "base bound variable anchor"

        Expect.equal
            (TastPoolBuilder.boundVarNaming b id)
            (BoundVarNaming.ofColumn pools.BoundVarNames.[i] id)
            "base bound variable naming"

/// Every base root unpools to the SAME decl before and after the overlay grows — the end-to-end
/// half of id preservation. The unpool is deterministic within one builder (its re-minted bound
/// variable keys are the builder's), so two unpools are directly comparable.
let private unpoolRoots (b: PoolBuilder) : Wire.TDecl[] =
    TastPoolBuilder.roots b
    |> Block.toArray
    |> Array.map (fun r -> (TastPoolBuilder.unpoolDecl b r).Decl)

// Programs spanning the domains the stack has to keep straight: a bound variable reference across
// decls, a composite expr with swappable children, a sub-patterned pattern, a `for` loop variable.
let private programs =
    [
        "let-bound reference", "let x = 1\nlet y = x\n"
        "tuple literal and tuple pattern", "let p = (1, 2)\nlet swap q =\n    match q with\n    | (a, b) -> (b, a)\n"
        "curried fn + call", "let add x y = x + y\nlet total = add 1 40\n"
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
                    let before = unpoolRoots b

                    // Grow the overlay: a re-pooled real subtree (which also carries bound variable
                    // references) and a bound variable the base never held.
                    TastPoolBuilder.appendExprTree b (firstLetValue frozen) |> ignore
                    TastPoolBuilder.mintBoundVar b |> ignore

                    Expect.isGreaterThan
                        (TastPoolBuilder.exprCount b)
                        pools.ExprPayloads.Length
                        "the overlay grew past the base"

                    checkBaseIdsResolve pools b
                    Expect.equal (unpoolRoots b) before "every base root unpools to the same decl it did before"
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

                // The bound variable the DU node introduces — already this pool's own id, the
                // tree being one unpooled from it.
                let duBoundVar = BoundVarKey.ofExpr du |> ValueOption.map BoundVarKey.identity

                // The tree is already in the stored anchor form, so the anchor needs no narrowing
                // — `Operators.id` because `id` is the pool id in scope here.
                expectPayloadOf
                    (TastPoolBuilder.exprPayload b id)
                    (TastPoolShapes.exprPayload Operators.id duBoundVar du)

                Expect.equal (TastPoolBuilder.exprTy b id) (TastWalk.exprTy du) "appended ty"
                Expect.equal (TastPoolBuilder.exprTok b id) (TastWalk.exprTok du) "appended tok"

                // `appendExprTree` pools the WHOLE tree it is handed — its children are
                // re-pooled into the overlay, not deduped against equal base rows.
                let kids = TastPoolBuilder.exprChildren b id

                Expect.equal kids.Length (TastPoolShapes.exprChildren du).Length "appended child fan-out"

                for (ExprPoolId k) in kids do
                    Expect.isGreaterThanOrEqual k baseCount "an appended child is an overlay id"
            }

            test "a minted Var resolves to the bound variable the base pool already interned" {
                let pools, frozen = poolsFor "let x = 1\nlet y = x\n"
                let b = TastPoolBuilder.openOver pools

                // `let y = x`'s value is the `Var` referencing `x`'s bound variable — a reference the
                // overlay must resolve into the BASE bound variable pool rather than mint anew.
                let varDu =
                    Block.toArray frozen.Decls
                    |> Array.pick (fun d ->
                        match d with
                        | TDeclG.Let(binding = { Value = TExprG.Var _ as value }) -> Some value
                        | _ -> None
                    )

                let boundVarCountBefore = TastPoolBuilder.boundVarCount b
                let id = TastPoolBuilder.appendExprTree b varDu

                Expect.equal
                    (TastPoolBuilder.boundVarCount b)
                    boundVarCountBefore
                    "a reference to a known bound variable mints no bound variable"

                match TastPoolBuilder.exprVarBoundVar b id with
                | ValueSome boundVar ->
                    Expect.equal
                        boundVar
                        (match varDu with
                         | TExprG.Var(boundVar = b) -> b
                         | _ -> failtest "not a Var")
                        "the minted Var resolves to its own bound variable"

                    let (BoundVarId j) = boundVar

                    Expect.isLessThan
                        j
                        pools.BoundVarNames.Length
                        "the bound variable id is the base pool's, not a fresh one"
                | ValueNone -> failtest "an appended Var carries no resolved bound variable id"
            }

            test "a minted bound variable extends the pool past the base" {
                let pools, _ = poolsFor "let x = 1\n"
                let b = TastPoolBuilder.openOver pools

                let id = TastPoolBuilder.mintBoundVar b
                let (BoundVarId i) = id
                Expect.equal i pools.BoundVarNames.Length "the minted bound variable takes the next flat id"

                Expect.equal
                    (TastPoolBuilder.boundVarNaming b id)
                    (BoundVarNaming.Minted id)
                    "a minted bound variable is named after its own slot"

                Expect.equal
                    (TastPoolBuilder.boundVarTok b id)
                    Anchor.nowhere
                    "a minted bound variable anchors on nothing"

                // A second mint is a SECOND bound variable: there is nothing to intern against, so
                // the id space simply grows.
                Expect.notEqual (TastPoolBuilder.mintBoundVar b) id "each mint is its own bound variable"

                Expect.equal
                    (TastPoolBuilder.boundVarCount b)
                    (pools.BoundVarNames.Length + 2)
                    "exactly two bound variables were appended"

                // The base half of the bound variable space is untouched by the mints.
                for i in 0 .. pools.BoundVarNames.Length - 1 do
                    let baseId = BoundVarId i

                    Expect.equal
                        (TastPoolBuilder.boundVarNaming b baseId)
                        (BoundVarNaming.ofColumn pools.BoundVarNames.[i] baseId)
                        "base bound variable naming"
            }
        ]

[<Tests>]
let rowCopyTests =
    testList
        "TastPoolBuilder row copies rewrite without a per-case match"
        [
            test "substituted children rebuild into the expected tree" {
                let pools, frozen = poolsFor "let p = (1, 2)\n"
                Expect.equal (Block.toArray frozen.Decls).Length 1 "one decl"

                let b = TastPoolBuilder.openOver pools
                let root = pools.Roots.[0]
                Expect.equal (TastPoolBuilder.declShape b root) DeclShape.Let "the decl is a Let"

                let tuple = (TastPoolBuilder.declExprChildren b root).[0]
                Expect.equal (TastPoolBuilder.exprShape b tuple) ExprShape.Tuple "its value is a Tuple"

                let kids = TastPoolBuilder.exprChildren b tuple
                Expect.equal kids.Length 2 "a two-element tuple"

                // The overlay node references BASE children (the two element ids, reversed): an edge
                // minted above the boundary addressing a node below it.
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

                // The oracle: the ORIGINAL root's own unpool with the tuple's items reversed, so
                // both sides share the identity an unpool produces.
                let original = (TastPoolBuilder.unpoolDecl b root).Decl

                let expected =
                    match original with
                    | TDeclG.Let(
                        binding = { Value = TExprG.Tuple(items, ty, tok) } as binding
                        isInline = isInline
                        isRec = isRec) ->
                        let reversed =
                            TExprG.Tuple(items |> Block.toArray |> Array.rev |> Block.ofArray, ty, tok)

                        TDeclG.Let({ binding with Value = reversed }, isInline, isRec)
                    | _ -> failtest "the decl is not a `let` over a Tuple"

                // The derived decl is a node like any other, reached by the id the copy returned.
                // Nothing repoints the root: a rewrite hands its caller the new id.
                Expect.equal
                    (TastPoolBuilder.unpoolDecl b newRoot).Decl
                    expected
                    "the derived decl is the original with the tuple's items swapped"

                Expect.equal
                    (TastPoolBuilder.unpoolDecl b root).Decl
                    original
                    "the original root is untouched by the copy"
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
