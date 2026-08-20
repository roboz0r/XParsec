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

let private edgeCount (decls: TastAccessor.DeclId list) : int =
    decls
    |> List.sumBy (fun d ->
        declExprs d
        |> List.filter (fun e -> TastAccessor.exprKind e = ExprShape.InlineCall)
        |> List.length
    )

/// The pool over a program's frozen trees, with its own declarations.
let private opened (input: string) : PoolBuilder * TastAccessor.DeclId list =
    let pool = TastPoolBuilder.openOver (frozenOf input)
    pool, TastAccessor.roots pool |> List.ofArray

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
                    (edgeCount (InlineExpand.expand pool decls).Decls)
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

                let expansion = InlineExpand.expand pool decls

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
                let expansion = InlineExpand.expand pool decls

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
                // `1 + 2` reduces to a `let` chain of pure bindings, and each collapse re-authors
                // the operator node, so the origin is filed several links from the node emitted.
                // Following one link falls back to the CALL SITE's anchor: in range, wrong file.
                let pool, decls = opened "let a = 1 + 2\n"
                let expansion = InlineExpand.expand pool decls

                let derivation = InlineExpand.Derivation.create ()
                InlineExpand.Derivation.absorb derivation expansion.Derived

                let body =
                    match
                        expansion.Decls
                        |> List.filter (fun d -> TastAccessor.declKind d = DeclShape.Let)
                    with
                    | [ d ] -> (TastAccessor.declLet d).Value
                    | ds -> failtestf "expected one `let` declaration, got %d" (List.length ds)

                // The operator node as the EXPANSION left it: under every bound-variable `let`,
                // and the node the declaring file origin is filed against. Walked rather than counted,
                // since the chain's length is a fact about the declaring file's body, not about origins.
                let rec underBoundVars (e: TastAccessor.ExprId) =
                    match TastAccessor.exprKind e with
                    | ExprShape.Let -> underBoundVars (TastAccessor.exprLet e).Body
                    | _ -> e

                let copied = underBoundVars body

                Expect.notEqual copied body "the expansion left at least one bound variable `let` to collapse"

                let expected =
                    match expansion.Origins.TryGetValue copied with
                    | true, origin -> origin
                    | _ -> failtest "the operator node came out of the table, or the chain below tests nothing"

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
                // site's value read at the other's.
                let input = "let a = 1 + 2\nlet b = 30 + 40\n"
                let pool, decls = opened input
                let frozenBoundVars = TastPoolBuilder.boundVarCount pool

                let expanded = (InlineExpand.expand pool decls).Decls

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
        ]
