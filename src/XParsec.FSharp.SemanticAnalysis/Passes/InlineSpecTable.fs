namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// The resolved-specialization table the expansion pass builds, and the vocabulary of ONE
// reduction as the table consumes it. The table's lifecycle: reserve a slot, build into it,
// intern it for reuse, assert on the finished graph.
module InlineSpecTable =

    /// What becomes of ONE curried parameter of an inline body at ONE call site. The three
    /// FUSED dispositions put call-site material INSIDE the body, making the reduction
    /// site-specific; only a `Survive` parameter can be a parameter of a shared entry.
    [<RequireQualifiedAccess>]
    type Disposition =
        /// Bound to a bare `External` function VALUE: pure and capture-free, so it is
        /// substituted into the body and its `let` disappears.
        | FuseExternalValue
        /// A lambda argument every use of which is a fully saturated application: spliced
        /// at each use, so its closure never exists.
        | FuseLambda
        /// Declared `[<CallAtMostOnce>]`: substituted at its single validated use rather than
        /// bound eagerly, so the argument is evaluated at most once and on demand.
        | FuseAtMostOnce
        /// Bound by an ordinary `let` (a physical splice) or abstracted by the entry (an edge).
        | Survive

    /// One curried parameter of an inline body paired with the argument the call site supplies
    /// for it.
    type InlineParam =
        {
            Key: NodeKey
            Ty: SemType
            /// The call-site argument. Unwalked in a peel; walked in a `Reduced`.
            Arg: TExpr
            /// The template's own bound variable, where an entry's abstraction is anchored. The
            /// call site's own position belongs to the EDGE instead.
            PatTok: SyntaxToken
            Disposition: Disposition
        }

    /// The reduction of one call site once its body has been walked: the fused body, and the
    /// parameters that survived it with their walked arguments.
    type Reduced =
        {
            Body: TExpr
            Survivors: InlineParam list
        }

    /// The identity two call sites must agree on to name ONE specialization entry. `Arity` is
    /// the number of parameters the site actually APPLIED: a partial application at the same
    /// types leaves the unapplied lambdas in the body, so it cannot share the saturated entry.
    type Grounding = { Key: SpecializationKey; Arity: int }

    /// A RESERVED table slot: the entry once its body has been built, and the call site in the
    /// file being compiled whose expansion began building it. `Site` is kept because an entry's
    /// own anchors index the PRODUCER's file, so nothing on it can position a diagnostic.
    [<NoEquality; NoComparison>]
    type PendingEntry =
        {
            mutable Built: TSpecialization voption
            Site: SyntaxToken
        }

    /// ONE outlined reduction, as the table consumes it: everything needed to reuse an entry
    /// or mint one, and to build the edge that names whichever it was.
    [<NoEquality; NoComparison>]
    type Outlining =
        {
            /// The call site in the file being compiled whose expansion began building this
            /// entry; the entry's own anchors index the producer's file instead.
            Site: SyntaxToken
            Grounding: Grounding
            /// May a LATER site at the same grounding name this same entry? False when the
            /// reduction fused any call-site material, or when its type arguments are not ground,
            /// because either way the entry belongs to this site rather than the template.
            Shareable: bool
            /// The file the entry's nodes stay anchored in: the template's, which for a
            /// template of this file is the file being compiled.
            Origin: OriginFile
            /// The position, anchor domain and result type of the EDGE, taken from the call site
            /// and never read off the entry: a reused entry's types belong to the thaw that built
            /// it, where this node belongs to the material the call was written in.
            EdgeTok: SyntaxToken
            EdgeOrigin: OriginFile
            EdgeTy: SemType
            /// The edge's arguments when an interned entry is REUSED. A thunk because walking
            /// them is an expansion in its own right; the minting path uses the survivors instead.
            ReuseArgs: unit -> TExpr list
            /// Build the reduction that fills a freshly reserved slot. Takes the slot, which is
            /// what a re-entrant call inside the body names, so a recursive template terminates
            /// instead of exhausting the stack.
            Build: SpecializationId -> Reduced
        }

    // Queries over a FINISHED `TSpecialization[]`: the graph invariants the finish step
    // discharges.

    /// Does the tree hold material FUSED from a call site? The question a SHAREABLE entry must
    /// answer `false`: fused material is one site's, so a second site reaching the entry that
    /// holds it would run its own call against the first site's operand.
    let containsCallerExpr (e: TExpr) : bool =
        let mutable found = false

        TastWalk.iterExpr
            { TastWalk.identityIter with
                VisitExpr =
                    fun _ n ->
                        match n with
                        | TExpr.CallerExpr _ -> found <- true
                        | _ -> ()

                        // Existence, not enumeration: the descent stops once the answer settles.
                        not found
            }
            e

        found

    /// Every specialization `e` NAMES, in walk order and with repeats: the graph's EDGE
    /// relation, read off a tree rather than stored.
    let edges (e: TExpr) : SpecializationId list =
        e
        |> TastWalk.chooseExpr (fun n ->
            match n with
            | TExpr.InlineCall(spec = spec) -> ValueSome spec
            | _ -> ValueNone
        )

    /// The slot an id names, bounds-checked. An out-of-range id is a MINTING bug, not a graph
    /// shape, so it faults rather than being reported as a diagnostic.
    let private checkedSlot (entries: TSpecialization[]) (SpecializationId i) : int =
        if i < 0 || i >= entries.Length then
            failwithf "InlineSpecTable: specialization %d is out of range (%d entries)" i entries.Length

        i

    let private entryValue (entries: TSpecialization[]) (spec: SpecializationId) : TExpr =
        snd (TSpecializationG.binding spec entries.[checkedSlot entries spec])

    /// The first cycle in the specialization graph, as the entries ON it in call order (so a
    /// direct self-reference is a one-element list). Checked on the TABLE, where a cycle is
    /// finite and inspectable, rather than during the substitution that would not terminate.
    let findCycle (entries: TSpecialization[]) : SpecializationId list voption =
        // An edge back into the current DFS path is a cycle; an edge into a FINISHED entry is
        // ordinary sharing, so a diamond is legal and must not be reported.
        let unvisited, onPath, finished = 0, 1, 2
        let state = Array.create entries.Length unvisited
        let path = ResizeArray<int>()
        let mutable found = ValueNone

        let rec visit (i: int) =
            state.[i] <- onPath
            path.Add i

            for spec in edges (entryValue entries (SpecializationId i)) do
                if ValueOption.isNone found then
                    let j = checkedSlot entries spec

                    if state.[j] = onPath then
                        let start = path.IndexOf j
                        found <- ValueSome [ for k in start .. path.Count - 1 -> SpecializationId path.[k] ]
                    elif state.[j] = unvisited then
                        visit j

            path.RemoveAt(path.Count - 1)
            state.[i] <- finished

        for i in 0 .. entries.Length - 1 do
            if ValueOption.isNone found && state.[i] = unvisited then
                visit i

        found

    /// Entries that FUSED call-site material yet are named by other than exactly ONE edge,
    /// paired with the edge count that convicts them. `roots` are the file's own declarations,
    /// whose edges count exactly as an entry's do.
    let miscountedFusedEntries (roots: TExpr seq) (entries: TSpecialization[]) : (SpecializationId * int) list =
        let counts = Array.zeroCreate<int> entries.Length

        let count (e: TExpr) =
            for spec in edges e do
                let i = checkedSlot entries spec
                counts.[i] <- counts.[i] + 1

        for r in roots do
            count r

        for i in 0 .. entries.Length - 1 do
            count (entryValue entries (SpecializationId i))

        [
            for i in 0 .. entries.Length - 1 do
                if containsCallerExpr (entryValue entries (SpecializationId i)) && counts.[i] <> 1 then
                    yield SpecializationId i, counts.[i]
        ]

    /// The resolved-specialization table one run of the pass builds. An edge's `SpecializationId`
    /// indexes the slots, so a slot is reserved before its entry exists and the array is
    /// materialised only once every build has finished.
    [<NoEquality; NoComparison>]
    type SpecTable =
        {
            Entries: ResizeArray<PendingEntry>
            /// The entries a later call site may REUSE, keyed by the grounding they agree on.
            Interned: Dictionary<Grounding, SpecializationId>
            /// Producer files a served body arrived with, retained for the whole run so that an
            /// entry's foreign anchors stay readable.
            mutable Origins: OriginSources
            Report: SyntaxToken -> Kind -> unit
            Mint: unit -> NodeKey
        }

    [<RequireQualifiedAccess>]
    module SpecTable =

        let create (report: SyntaxToken -> Kind -> unit) (mint: unit -> NodeKey) : SpecTable =
            {
                Entries = ResizeArray()
                Interned = Dictionary()
                Origins = OriginSources.empty
                Report = report
                Mint = mint
            }

        /// Retain the producer file a served body arrived with, and hand back everything
        /// retained so far, which is what a foreign anchor is READ through. Keyed by path, so a
        /// file serving many templates is retained once.
        let retainOrigin (src: OriginSource) (t: SpecTable) : OriginSources =
            t.Origins <- OriginSources.add src t.Origins
            t.Origins

        /// The entry `grounding` already names, if a previous site interned one. `shareable` is
        /// false when the reduction fused the site's own material, or when non-ground type
        /// arguments leave this thaw's inference cells inside the body.
        let private tryReuse (grounding: Grounding) (shareable: bool) (t: SpecTable) : SpecializationId voption =
            if shareable then
                match t.Interned.TryGetValue grounding with
                | true, spec -> ValueSome spec
                | _ -> ValueNone
            else
                ValueNone

        /// The TABLE form of a reduction: a new entry, its surviving parameters abstracted back
        /// into the lambda chain an `InlineCall`'s arguments are positional against. One lambda
        /// per SURVIVOR, below `Grounding.Arity` whenever a parameter fused.
        let private mintEntry (o: Outlining) (t: SpecTable) : SpecializationId * InlineParam list =
            let slot = t.Entries.Count
            let spec = SpecializationId slot

            t.Entries.Add { Built = ValueNone; Site = o.Site }

            let reduced = o.Build spec

            let value, declTy =
                List.foldBack
                    (fun (p: InlineParam) (inner, innerTy) ->
                        let lamTy = TyFun(p.Ty, innerTy)
                        TExpr.Lambda(TPat.NamedSimple(p.Key, p.Ty, p.PatTok), inner, lamTy, p.PatTok), lamTy
                    )
                    reduced.Survivors
                    (reduced.Body, TastWalk.exprTy reduced.Body)

            // `shareable` implies that no parameter fused, so a fusion bug faults here instead
            // of silently sharing one site's material across sites.
            if o.Shareable && containsCallerExpr value then
                failwithf
                    "InlineExpansion: specialization %d is shareable but marks caller material, and a closed reduction fuses nothing, so this entry's parameters were mis-classified"
                    slot

            t.Entries.[slot].Built <-
                ValueSome
                    {
                        Key = o.Grounding.Key
                        Origin = o.Origin
                        // Every consumer matches on the VALUE alone, so this bound variable is
                        // minted rather than taken from anything.
                        Decl =
                            TDecl.Let(TPat.NamedSimple(t.Mint(), declTy, TastWalk.exprTok value), value, true, declTy)
                    }

            // INTERNED only once BUILT: reuse is a decision about a finished entry, where the
            // reserved slot above is what makes an in-flight recursion terminate.
            if o.Shareable then
                t.Interned.[o.Grounding] <- spec

            spec, reduced.Survivors

        /// Outline one reduction and hand back the EDGE naming it. Reusing an interned entry and
        /// minting a fresh one differ only in where the edge's arguments come from.
        let outline (o: Outlining) (t: SpecTable) : TExpr =
            let spec, args =
                match tryReuse o.Grounding o.Shareable t with
                // A shared entry is CLOSED, so every peeled parameter survived it and the
                // site's whole argument list is the edge's argument list.
                | ValueSome spec -> spec, o.ReuseArgs()
                | ValueNone ->
                    let spec, survivors = mintEntry o t
                    spec, [ for p in survivors -> p.Arg ]

            TExpr.InlineCall(spec, EqArray.ofList args, o.EdgeOrigin, o.EdgeTy, o.EdgeTok)

        /// Materialise the table, and discharge the two facts about it that no single entry can
        /// see: acyclicity, and that a fused entry is named by exactly one edge. `declExprs` is
        /// every tree OUTSIDE the table, whose edges count exactly as an entry's own do.
        let finish (declExprs: TExpr seq) (t: SpecTable) : TSpecialization[] =
            let table =
                t.Entries
                |> Seq.mapi (fun i e ->
                    match e.Built with
                    | ValueSome entry -> entry
                    | ValueNone ->
                        failwithf
                            "InlineExpansion: specialization %d was reserved but never built, so an expansion abandoned its slot"
                            i
                )
                |> Array.ofSeq

            // A recursive `let inline` asks for an expansion that does not exist, so the cycle
            // is a diagnostic naming every binding on it and nothing expands the graph.
            match findCycle table with
            | ValueSome cycle ->
                // Named in call order, so the verdict reads as the loop runs, and positioned at
                // the site of the entry the cycle CLOSES ON, because that is a position in the
                // file being compiled, where an entry's own anchors index its template's file.
                match cycle, [ for SpecializationId i in cycle -> Inline.servedName table.[i].Key.Template ] with
                | SpecializationId closes :: _, binding :: via ->
                    t.Report t.Entries.[closes].Site (Kind.CyclicInline(binding, via))
                | _ -> failwith "InlineExpansion: a cycle names at least the binding it closes on"
            | ValueNone ->
                // The half no single reduction can see: an edge count is a fact about the
                // finished graph, where the assert as each entry is built sees only that entry.
                match miscountedFusedEntries declExprs table with
                | [] -> ()
                | bad ->
                    failwithf
                        "InlineExpansion: %A fused caller material yet are named by that many call edges, though fused material belongs to the one site that wrote it"
                        [ for (SpecializationId i, count) in bad -> table.[i].Key.Template, count ]

            table
