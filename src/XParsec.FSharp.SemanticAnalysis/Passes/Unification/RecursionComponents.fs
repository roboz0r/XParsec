namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

/// The recursion components of one lexical `let` / `let rec … and …` group. Inference
/// generalises one component at a time, and `V260` reports a `rec` keyword covering more than
/// the members need.
module internal UnificationRecursionComponents =

    /// The member index owning each bound-variable key of the group, as NameResolution keyed
    /// the binding sites.
    let private memberIndexBySite (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) =
        let byKey = Dictionary<NodeKey, int>()

        for i in 0 .. bindings.Length - 1 do
            for _, key in NameResolutionScope.bindingsOfPat ctx bindings.[i].pattern do
                byKey.[key] <- i

        byKey

    /// The member indices `value` references, as NameResolution bound its use sites.
    let private referencedSiblings
        (ctx: PassContext)
        (byKey: Dictionary<NodeKey, int>)
        (value: Expr<SyntaxToken>)
        : HashSet<int> =
        let found = HashSet<int>()

        let addAt (key: NodeKey) =
            match ctx.Bindings.Binding.TryGetValue key with
            | ValueSome rb ->
                match byKey.TryGetValue rb.BindingSite with
                | true, i -> found.Add i |> ignore
                | _ -> ()
            | ValueNone -> ()

        let walker =
            { CstWalk.identityExprWalker<unit> with
                Visit =
                    fun () e ->
                        addAt (CstKeys.ofExpr e)

                        // A field-access chain (`r.X.Y`) binds its ANCHOR under `ExprIdent` on
                        // the leading segment, separately from the whole node's key.
                        match e with
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length > 1 ->
                            addAt (NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
                        | _ -> ()
            }

        CstWalk.iterExpr walker () value
        found

    /// Partitions `bindings` into strongly connected components of the reference graph, where
    /// edge `i → j` means member `i`'s value references member `j`. Without `rec` every member
    /// is its own singleton component, in source order.
    let partition
        (ctx: PassContext)
        (isRec: SyntaxToken voption)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        : SccPartition =
        let successors: int -> seq<int> =
            match isRec with
            | ValueNone -> fun _ -> Seq.empty
            | ValueSome _ ->
                let byKey = memberIndexBySite ctx bindings
                fun i -> referencedSiblings ctx byKey bindings.[i].expr

        Digraph.OfSuccessors(bindings.Length, successors) |> Scc.compute

    /// Report `V260` once per group where `recTok` covers more than the group's members need:
    /// the group splits into several components, or a lone binding's value refers to outer
    /// names only. Also report FS1114 on each `inline` member of a group of two or more.
    let report
        (ctx: PassContext)
        (recTok: SyntaxToken)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (partition: SccPartition)
        : unit =
        if bindings.Length > 1 then
            for b in bindings do
                match b.inlineToken with
                | ValueSome inlineTok ->
                    for name, _ in NameResolutionScope.bindingsOfPat ctx b.pattern do
                        ctx.Report(inlineTok, Kind.InlineInRecGroup name)
                | ValueNone -> ()

        let names (scc: SccComponent) =
            [
                for i in scc.Members do
                    for name, _ in NameResolutionScope.bindingsOfPat ctx bindings.[i].pattern -> name
            ]

        if partition.Components.Length > 1 then
            let groups = [ for scc in partition.Components -> names scc ]
            ctx.Report(recTok, Kind.OverstatedRecursion(RecursionOverstatement.SplittableGroup groups))
        else
            match partition.Components.[0] with
            | Cycle _ -> ()
            | Acyclic _ as lone ->
                ctx.Report(recTok, Kind.OverstatedRecursion(RecursionOverstatement.RedundantRec(names lone)))
