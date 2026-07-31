module XParsec.FSharp.Codegen.Js.Tests.InlineExpandTests

open System.Collections.Generic
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The EMIT-time placement of the resolved-specialization graph: what the shared expansion
// reaches, what it copies, and what it says about where the copies came from.
//
// The table itself — what is interned, what is shared, where an entry's nodes are anchored —
// is `SpecializationTableTests`, pre-freeze. This is the other end: the pooled tree both
// backends actually emit from.

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

/// Where each ENTRY-OWN node was written, computed independently of the expansion: the walk of
/// each entry's body, stopping AT a `CallerExpr` because everything under one was written by
/// the caller and is a node of the frame outside, not of this entry.
///
/// This is the expansion's frame chain, derived a second way — a push that never happened
/// leaves a copied node with no owner here, and a pop that never happened leaves one whose
/// owner is the caller's frame rather than the entry's. Keyed by the ENTRY's node, so a copy is
/// looked up through the authorship chain that reaches it.
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
                        File = entry.Origin
                        At = ForeignAnchor.ofAnchor (TastAccessor.exprTok e)
                    }

                for c in TastAccessor.exprChildren e do
                    walk c

        walk (TastAccessor.declLet entry.Decl).Value

    owners

/// Every binder a declaration's patterns INTRODUCE, in walk order and with repeats — so a
/// binder shared between two expansions of one entry shows up as a duplicate rather than
/// being silently deduplicated.
let private introducedBinders (d: TastAccessor.DeclId) : BinderId list =
    let acc = ResizeArray<BinderId>()

    let rec pat (p: TastAccessor.PatId) =
        match TastAccessor.patBinder p with
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
                // The trap this exists for: `TastLower.lower` discards `type` decls outright,
                // and a member body lives only inside one — so an expansion hung off the
                // lowering would miss every member body and still pass its own tests.
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

            test "an entry whose body names another entry expands through both" {
                // `b |> not`: `(|>)`'s entry is an application whose HEAD the classification
                // substituted, so its body is itself an edge — the entry-references-entry leg
                // of the DAG, which a one-level splice would leave standing.
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

            test "the frame chain names, for every copied node, the entry it was written in" {
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
                            | ValueSome written -> written.File
                            | ValueNone -> failtest "a copied node belongs to no entry"
                    ]

                Expect.isGreaterThan (reached |> List.distinct |> List.length) 0 "at least one producer file is named"

                Expect.isGreaterThan (expansion.Origins.Count) 1 "more than one node came out of the table"
            }

            test "a node re-authored REPEATEDLY still names the file it was copied from" {
                // The chain, which is the whole reason provenance is resolved along one rather
                // than copied at each re-authorship. `1 + 2` beta-reduces `(+)`'s body to a
                // `let` chain of pure bindings — one per binder, and `1 + 2` now binds twice
                // over: the operator's own operands, then `int`'s `(+)` witness below it.
                // Collapsing them (`reduceInlinableLet`, what every `buildExpr` site does)
                // re-authors the operator node ONCE PER COLLAPSE — so the node the origin is
                // filed against is several links away, and a reader that followed one link
                // would fall back to the CALL SITE's anchor: in range, plausible, and the
                // wrong file.
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

                // The operator node as the EXPANSION left it: under EVERY binder `let`, and the
                // node the producer origin is filed against. Walked rather than counted — how
                // many binders the chain has is a fact about the contract's shape, not the
                // provenance claim under test.
                let rec underBinders (e: TastAccessor.ExprId) =
                    match TastAccessor.exprKind e with
                    | ExprShape.Let -> underBinders (TastAccessor.exprLet e).Body
                    | _ -> e

                let copied = underBinders body

                Expect.notEqual copied body "the expansion left at least one binder `let` to collapse"

                let expected =
                    match expansion.Origins.TryGetValue copied with
                    | true, origin -> origin
                    | _ -> failtest "the operator node came out of the table, or the chain below tests nothing"

                // Collapse the WHOLE chain, not a fixed two links: every binder the
                // expansion left is one more re-authorship, and how many there are is a fact
                // about the contract's shape, not about the provenance claim.
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

            test "two call sites at one grounding get their OWN binders" {
                // One entry, two edges (`SpecializationTable` pins the sharing). Each edge
                // takes its own copy, so the binders — and the codegen local slots they become
                // — must not alias; a shared slot is one site's value read at the other's.
                let input = "let a = 1 + 2\nlet b = 30 + 40\n"
                let pool, decls = opened input
                let frozenBinders = TastPoolBuilder.binderCount pool

                let expanded = (InlineExpand.expand pool decls).Decls

                let binders = expanded |> List.collect introducedBinders

                let minted = binders |> List.filter (fun (BinderId i) -> i >= frozenBinders)

                Expect.isNonEmpty minted "the expansion minted binders, or the fixture outlines nothing that binds"

                Expect.equal
                    (List.length (List.distinct binders))
                    (List.length binders)
                    "no binder is introduced twice — the two copies share no slot"
            }
        ]
