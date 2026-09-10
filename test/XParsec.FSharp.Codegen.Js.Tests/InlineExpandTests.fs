module XParsec.FSharp.Codegen.Js.Tests.InlineExpandTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The EMIT-time placement of the resolved-specialization graph over the pooled tree both
// backends emit from: what the shared expansion reaches, what it copies, and where each
// copy says it was written.

/// Every expression reachable from a declaration, member bodies included.
let private declExprs (d: TastAccessor.DeclId) : TastAccessor.ExprId list =
    let acc = ResizeArray<TastAccessor.ExprId>()

    let rec walk (e: TastAccessor.ExprId) =
        acc.Add e

        for c in TastAccessor.exprChildren e do
            walk c

    TastAccessor.mapDeclBodies
        (fun e ->
            walk e
            e
        )
        d
    |> ignore

    List.ofSeq acc

let private countKind (kind: ExprShape) (decls: TastAccessor.DeclId list) : int =
    decls
    |> List.sumBy (fun d ->
        declExprs d
        |> List.filter (fun e -> TastAccessor.exprKind e = kind)
        |> List.length
    )

let private edgeCount (decls: TastAccessor.DeclId list) : int = countKind ExprShape.InlineCall decls

/// The pool over a program's frozen trees, with its own declarations.
let private opened (input: string) : PoolBuilder * TastAccessor.DeclId list =
    let pool = TastPoolBuilder.openOver (frozenOf input)
    pool, TastAccessor.roots pool |> List.ofArray

/// The JS emitter's classifier: a JS intrinsic is a source template rather than an opcode.
let private noTotalIntrinsic (_: string) : bool = false

/// Where each ENTRY-OWN node was written, walked independently of the expansion: each entry's
/// body, stopping AT a `CallerExpr`, since everything under one is the caller's material.
/// Keyed by the ENTRY's node, so a copy is looked up through the chain that reaches it.
let private entryOwners (pool: PoolBuilder) : Dictionary<TastAccessor.ExprId, InlineExpand.NodeOrigin> =
    let owners = Dictionary<TastAccessor.ExprId, InlineExpand.NodeOrigin>()

    for i in 0 .. TastPoolBuilder.specializationCount pool - 1 do
        let entry = TastAccessor.specialization pool (SpecializationId i)

        let rec walk (e: TastAccessor.ExprId) =
            match TastAccessor.exprKind e with
            | ExprShape.CallerExpr -> ()
            | _ ->
                owners.[e] <-
                    {
                        Path = entry.Path
                        At = TastAccessor.exprTok e
                    }

                for c in TastAccessor.exprChildren e do
                    walk c

        walk entry.Value

    owners

/// Every bound variable a declaration's patterns INTRODUCE, in walk order and with repeats, so
/// one shared between two expansions of an entry shows up as a duplicate.
let private introducedBoundVars (d: TastAccessor.DeclId) : BoundVarId list =
    let acc = ResizeArray<BoundVarId>()

    let rec pat (p: TastAccessor.PatId) =
        match TastAccessor.patBoundVar p with
        | ValueSome b -> acc.Add b
        | ValueNone -> ()

        for k in TastAccessor.patChildren p do
            pat k

    for e in declExprs d do
        for p in TastAccessor.exprPatChildren e do
            pat p

    List.ofSeq acc

[<Tests>]
let tests =
    testList
        "InlineExpand"
        [
            test "an edge inside a MEMBER body is expanded" {
                // A member body lives only inside a `type` decl, so an expansion driven off a
                // lowering that discards those would miss every member body and still pass.
                let input =
                    "type C(n: int) =\n    member x.Double = n * 2\nlet c = C(3)\nprintfn \"%d\" c.Double\n"

                let pool, decls = opened input

                let memberEdges =
                    decls
                    |> List.filter (fun d -> TastAccessor.declKind d = DeclShape.Type)
                    |> edgeCount

                Expect.isGreaterThan
                    memberEdges
                    0
                    "the member body really does carry an edge, or nothing below is being tested"

                Expect.equal
                    (edgeCount (InlineExpand.expand noTotalIntrinsic pool decls).Decls)
                    0
                    "every edge is placed, member bodies included"

                Expect.stringContains
                    (emit input)
                    "Math.imul"
                    "…and the multiply the member body asked for is what it emits"
            }

            test "an entry whose body references another entry expands through both" {
                // `b |> not`: `(|>)`'s entry body is itself an edge, its applied function having
                // been substituted. That is the entry-references-entry leg of the DAG, which a
                // one-level splice would leave standing.
                let input = "let b = true\nlet a = b |> not\n"
                let pool, decls = opened input

                Expect.isGreaterThan
                    (TastPoolBuilder.specializationCount pool)
                    1
                    "the fixture reaches more than one entry"

                let expansion = InlineExpand.expand noTotalIntrinsic pool decls

                Expect.equal (edgeCount expansion.Decls) 0 "no edge survives, at either level"

                Expect.stringContains (emit input) "!(" "the negation the inner entry carries is emitted"
            }

            test "the frame chain identifies, for every copied node, the entry it was written in" {
                // Asserted against an INDEPENDENT reading of the same fact (`entryOwners`), so
                // a frame the descent failed to push leaves a copy no entry owns, and one it
                // failed to pop leaves caller material attributed to the entry it sits in.
                let input = "let b = true\nlet a = b |> not\n"
                let pool, decls = opened input
                let owners = entryOwners pool
                let expansion = InlineExpand.expand noTotalIntrinsic pool decls

                Expect.isNonEmpty
                    (List.ofSeq expansion.Origins)
                    "the expansion actually recorded provenance, or what follows is vacuous"

                for KeyValue(node, origin) in expansion.Origins do
                    match InlineExpand.Derivation.tryFind expansion.Derived owners node with
                    | ValueSome written ->
                        Expect.equal
                            origin
                            written
                            "a copied node is attributed to the entry it was written in, at the position it was \
                             written at, in that file's own index space"
                    | ValueNone -> failtest "a copied node belongs to no entry — a frame was never pushed"

                // The nesting itself: `not`'s body is reached only through `(|>)`'s, so two
                // frames were live at once and both are represented in what was recorded.
                let reached =
                    [
                        for KeyValue(node, _) in expansion.Origins ->
                            match InlineExpand.Derivation.tryFind expansion.Derived owners node with
                            | ValueSome written -> written.Path
                            | ValueNone -> failtest "a copied node belongs to no entry"
                    ]

                Expect.isGreaterThan (reached |> List.distinct |> List.length) 0 "at least one declaring file is named"

                Expect.isGreaterThan (expansion.Origins.Count) 1 "more than one node came out of the table"
            }

            test "a node re-authored REPEATEDLY still points to the file it was copied from" {
                // `sum2` reads each parameter twice, so each compound argument stays bound, and
                // each collapse re-authors the operator node: the origin is filed several links
                // from the node emitted. Following one link falls back to the CALL SITE's
                // anchor: in range, wrong file.
                let pool, decls =
                    opened "let inline sum2 (x: int) (y: int) = x + y + x + y\nlet a = sum2 (1 + 2) (3 + 4)\n"

                let expansion = InlineExpand.expand noTotalIntrinsic pool decls

                let derivation = InlineExpand.Derivation.create ()
                InlineExpand.Derivation.absorb derivation expansion.Derived

                // `a`, the call site; `sum2` ahead of it is the template.
                let body =
                    match
                        expansion.Decls
                        |> List.filter (fun d -> TastAccessor.declKind d = DeclShape.Let)
                    with
                    | [ _; d ] -> (TastAccessor.declLet d).Binding.Value
                    | ds -> failtestf "expected two `let` declarations, got %d" (List.length ds)

                // The operator node as the EXPANSION left it: under every bound-variable `let`,
                // and the node the declaring file origin is filed against. Walked rather than counted,
                // since the chain's length is a fact about the declaring file's body, not about origins.
                let rec underBoundVars (e: TastAccessor.ExprId) =
                    match TastAccessor.exprKind e with
                    | ExprShape.Let -> underBoundVars (TastAccessor.exprLet e).Body
                    | _ -> e

                let copied = underBoundVars body

                Expect.notEqual copied body "the expansion left at least one bound variable `let` to collapse"

                // Through the chain, because the expansion re-authors this node too: substituting
                // an atomic argument rebuilds the path down to each reference.
                let expected =
                    match InlineExpand.Derivation.tryFind expansion.Derived expansion.Origins copied with
                    | ValueSome origin -> origin
                    | ValueNone -> failtest "the operator node came out of the table, or the chain below tests nothing"

                // Collapse the WHOLE chain, not a fixed two links.
                let rec collapseAll (e: TastAccessor.ExprId) (links: int) =
                    match JsEmitHelpers.reduceInlinableLet derivation e with
                    | Some reduced -> collapseAll reduced (links + 1)
                    | None -> e, links

                let collapsed, links = collapseAll body 0

                Expect.isGreaterThan
                    links
                    1
                    "more than one collapse, or a single link would satisfy the claim trivially"

                Expect.notEqual collapsed copied "no re-authorship left the node it was given"

                Expect.equal
                    (InlineExpand.Derivation.tryFind derivation expansion.Origins collapsed)
                    (ValueSome expected)
                    "the repeatedly-derived node resolves to the origin filed against the node it descends from"
            }

            test "two call sites at one grounding get their OWN bound variables" {
                // One entry, two edges. Each edge takes its own copy, so the bound variables, and
                // the codegen local slots they become, must not alias: a shared slot would be one
                // site's value read at the other's. `twice` reads its parameter twice, so each
                // site's compound argument stays bound rather than collapsing into its use.
                let input =
                    "let inline twice (x: int) = x + x\nlet a = twice (1 + 2)\nlet b = twice (30 + 40)\n"

                let pool, decls = opened input
                let frozenBoundVars = TastPoolBuilder.boundVarCount pool

                let expanded = (InlineExpand.expand noTotalIntrinsic pool decls).Decls

                let boundVars = expanded |> List.collect introducedBoundVars

                let minted = boundVars |> List.filter (fun (BoundVarId i) -> i >= frozenBoundVars)

                Expect.isNonEmpty
                    minted
                    "the expansion minted bound variables, or the fixture outlines nothing that binds"

                Expect.equal
                    (List.length (List.distinct boundVars))
                    (List.length boundVars)
                    "no bound variable is introduced twice — the two copies share no slot"
            }

            test "an IMMUTABLE variable argument is substituted, spending no binding" {
                // Both operands of the inlined `+` are references to an immutable parameter.
                let pool, decls = opened "let f (a: int) = a + a\n"
                let expansion = InlineExpand.expand noTotalIntrinsic pool decls
                Expect.equal (countKind ExprShape.Let expansion.Decls) 0 "the expansion binds nothing"
            }

            test "a MUTABLE local read by a body with no write is substituted" {
                // Between the binding and each use the body runs no assignment to `m`, and a
                // closure capturing `m` would have promoted it to a cell before the freeze.
                let pool, decls = opened "let g () =\n    let mutable m = 1\n    m + m\n"
                let expansion = InlineExpand.expand noTotalIntrinsic pool decls
                Expect.equal (countKind ExprShape.Let expansion.Decls) 1 "only the source `let mutable`"
            }

            test "a MUTABLE local read AHEAD of the write it snapshots needs no binding" {
                // `let x = m` is the first operand, so the read it stands for happens where the
                // binding put it, ahead of the write in the second operand.
                let pool, decls = opened "let g () =\n    let mutable m = 1\n    m + (m <- 2; m)\n"
                let expansion = InlineExpand.expand noTotalIntrinsic pool decls
                Expect.equal (countKind ExprShape.Let expansion.Decls) 1 "only the source `let mutable`"
            }

            test "a MUTABLE local stays bound where the body writes it AHEAD of the read" {
                // `rsub` reads its parameters in the opposite order to the arguments, so the
                // write in the second argument runs between `let a = m` and the read of `a`.
                // Collapsing it would read `m` at 2 and answer 8 where the binding answers 9.
                let pool, decls =
                    opened
                        "let inline rsub (a: int) (b: int) = b - a\nlet g () =\n    let mutable m = 1\n    rsub m (m <- 2; 10)\n"

                let expansion = InlineExpand.expand noTotalIntrinsic pool decls

                Expect.equal
                    (countKind ExprShape.Let expansion.Decls)
                    2
                    "the source `let mutable` and the snapshot of `m`"
            }

            test "a parameter collapses past an intrinsic only where the target says the op is total" {
                // `rsub (1 + 2) (3 + 4)` reads `b` first, so `b` collapses either way and `a`'s
                // use then follows `3 + 4`. Moving `1 + 2` past that intrinsic is sound only if
                // the intrinsic always returns, which is the classifier's verdict.
                let input =
                    "let inline rsub (a: int) (b: int) = b - a
let g () = rsub (1 + 2) (3 + 4)
"

                let pool, decls = opened input
                let barred = InlineExpand.expand noTotalIntrinsic pool decls
                Expect.equal (countKind ExprShape.Let barred.Decls) 1 "`a` stays bound ahead of a non-total op"

                let pool, decls = opened input
                let moved = InlineExpand.expand (fun _ -> true) pool decls
                Expect.equal (countKind ExprShape.Let moved.Decls) 0 "`a` collapses past a total op"
            }

            test "a `let` the user wrote keeps its binding where an inline parameter would collapse" {
                // The same compound value, read once and first: as a parameter it is a minted
                // binding and collapses; as the user's own `let` it keeps its name and storage.
                let pool, decls =
                    opened
                        "let inline inc (x: int) = x + 1
let g () = inc (1 + 2)
"

                let parameter = InlineExpand.expand (fun _ -> true) pool decls
                Expect.equal (countKind ExprShape.Let parameter.Decls) 0 "the minted parameter binding collapses"

                let pool, decls =
                    opened
                        "let g () =
    let a = 1 + 2
    a + 1
"

                let written = InlineExpand.expand (fun _ -> true) pool decls
                Expect.equal (countKind ExprShape.Let written.Decls) 1 "the user's `let` stays"
            }

            test "a MUTABLE local stays bound where a closure in the body captures the parameter" {
                // The closure would read `m` when it runs, after any later write.
                let pool, decls =
                    opened "let inline delay (x: int) = fun () -> x\nlet g () =\n    let mutable m = 1\n    delay m\n"

                let expansion = InlineExpand.expand noTotalIntrinsic pool decls

                Expect.equal
                    (countKind ExprShape.Let expansion.Decls)
                    2
                    "the source `let mutable` and the snapshot of `m`"
            }

            test "a MUTABLE local is substituted past a closure that does not capture the parameter" {
                // The lambda references neither `x` nor `m`, so substitution leaves its captures
                // unchanged.
                let pool, decls =
                    opened
                        "let inline beside (x: int) = (fun () -> 0), x + x\nlet g () =\n    let mutable m = 1\n    beside m\n"

                let expansion = InlineExpand.expand noTotalIntrinsic pool decls
                Expect.equal (countKind ExprShape.Let expansion.Decls) 1 "only the source `let mutable`"
            }

            test "a MUTABLE local captured by a closure reaches the freeze as a cell" {
                // `substitutable` finds a local mutable's writes in the body alone, which requires
                // that every closure-captured mutable is a cell by the freeze.
                let pool, _ =
                    opened "let g () =\n    let mutable m = 1\n    let f () = m\n    m + f ()\n"

                for i in 0 .. TastPoolBuilder.boundVarCount pool - 1 do
                    Expect.isFalse
                        (TastPoolBuilder.boundVarIsMutable pool (BoundVarId i))
                        (sprintf "bound variable %d is a cell rather than a mutable" i)
            }
        ]
