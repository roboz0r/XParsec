namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// The resolved-specialization table the expansion pass builds, and the vocabulary of ONE
// reduction as the table consumes it.
//
// A file of its own because the table has a lifecycle — reserve a slot, build into it, intern
// it for reuse, assert on the finished graph — and that lifecycle is only checkable where it is
// stated once. Held inside the pass it was six lines of mutable state a thousand-line closure
// could touch in any order.
//
// What is deliberately ABSENT is the walk: an `Outlining` is a position, a grounding and two
// thunks, so no operation here can come to depend on how the pass represents its expansion
// chain.
module InlineSpecTable =

    /// What becomes of ONE curried parameter of an inline body at ONE call site. The three
    /// FUSED dispositions all put call-site material INSIDE the body, which is what makes a
    /// reduction site-specific; a `Survive` parameter leaves the body closed over it and is
    /// the only kind that can be a parameter of a shared specialization entry.
    ///
    /// Decided BEFORE the body is walked (every input is the peel and the declared attributes,
    /// none of them the walk's result), which is what lets a caller reserve a table slot ahead
    /// of the recursion that fills it.
    [<RequireQualifiedAccess>]
    type Disposition =
        /// Bound to a bare `External` function VALUE — pure and capture-free, so it is
        /// substituted into the body and its `let` disappears.
        | FuseExternalValue
        /// A lambda argument every use of which is a fully saturated application head: spliced
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
            /// The template's own binder for this parameter — where an entry's abstraction is
            /// anchored, the call site's own position belonging to the EDGE and not to the
            /// parameter it applies.
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

    /// The identity two call sites must agree on to name ONE specialization entry.
    ///
    /// `Key` is the entry's own stored key (template identity + the type arguments it was
    /// resolved at). `Arity` is the number of parameters the site actually APPLIED, which the
    /// grounding does not pin: a partial application of the same template at the same types
    /// leaves a different body behind (the unapplied lambdas stay in it), and sharing the
    /// saturated site's entry with it would hand one body two arities.
    type Grounding = { Key: SpecializationKey; Arity: int }

    /// A RESERVED table slot: the entry once its body has been built, and the call site in the
    /// file being compiled whose expansion began building it.
    ///
    /// The site is kept because nothing ON the entry can position a diagnostic about it: an
    /// entry's anchors index the PRODUCER's file by design, which is the whole point of the
    /// table and exactly why a verdict here needs a position from somewhere else.
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
            /// entry — see `PendingEntry` for why the entry itself cannot supply one.
            Site: SyntaxToken
            Grounding: Grounding
            /// May a LATER site at the same grounding name this same entry? False when the
            /// reduction fused any call-site material, or when its type arguments are not
            /// ground — either way the entry belongs to this site rather than the template.
            Shareable: bool
            /// The file the entry's nodes stay anchored in — the template's, which for a
            /// template of this file is the file being compiled.
            Origin: OriginFile
            /// The position, anchor domain and result type of the EDGE — the call site's, NOT
            /// `Site`'s and not anything read off the entry: a reused entry's types belong to
            /// the thaw that built it, where this node belongs to the material the call was
            /// written in. `EdgeOrigin` is the entry's `Origin` only for a call in the very
            /// file the body came from.
            EdgeTok: SyntaxToken
            EdgeOrigin: OriginFile
            EdgeTy: SemType
            /// The edge's arguments when an interned entry is REUSED. A thunk because walking
            /// them is an expansion in its own right, and the minting path takes its arguments
            /// off the survivors `Build` leaves instead.
            ReuseArgs: unit -> TExpr list
            /// Build the reduction that fills a freshly reserved slot. Takes the slot, which is
            /// what a re-entrant call inside the body names — so the id exists before anything
            /// can recurse, and a recursive template terminates instead of exhausting the stack.
            Build: SpecializationId -> Reduced
        }

    // Queries over a FINISHED `TSpecialization[]` — the graph invariants the finish step
    // discharges.

    /// Does the tree hold material FUSED from a call site?
    ///
    /// The question a SHAREABLE entry must answer `false`. Fused material is one site's, so an
    /// entry holding it and reached from a second site would run that site's call against the
    /// first site's operand. Not a diagnostic aid but the condition under which sharing is
    /// sound at all.
    let containsCallerExpr (e: TExpr) : bool =
        let mutable found = false

        TastWalk.iterExpr
            { TastWalk.identityIter with
                VisitExpr =
                    fun _ n ->
                        match n with
                        | TExpr.CallerExpr _ -> found <- true
                        | _ -> ()

                        // Existence, not enumeration: once the answer is settled there is
                        // nothing left below to learn, so the descent stops. That makes this
                        // a PRUNING walk, and so not a plain collect.
                        not found
            }
            e

        found

    /// Every specialization `e` NAMES, in walk order and with repeats — the graph's EDGE
    /// relation, read off a tree rather than stored. One reading, so the acyclicity check and
    /// the call-edge count below cannot disagree about what an edge is.
    let edges (e: TExpr) : SpecializationId list =
        e
        |> TastWalk.chooseExpr (fun n ->
            match n with
            | TExpr.InlineCall(spec = spec) -> ValueSome spec
            | _ -> ValueNone
        )

    /// The slot an id names, bounds-checked. An out-of-range id is a MINTING bug and not a
    /// graph shape, so every walk of the table faults on it identically rather than each one
    /// inventing its own message.
    let private checkedSlot (entries: TSpecialization[]) (SpecializationId i) : int =
        if i < 0 || i >= entries.Length then
            failwithf "InlineSpecTable: specialization %d is out of range (%d entries)" i entries.Length

        i

    /// An entry's abstraction, reached by id — the entry's own reading of its binding, with the
    /// bounds check the id needs anyway.
    let private entryValue (entries: TSpecialization[]) (spec: SpecializationId) : TExpr =
        snd (TSpecializationG.binding spec entries.[checkedSlot entries spec])

    /// The first cycle in the specialization graph, as the entries ON it in call order (so a
    /// direct self-reference is a one-element list). `ValueNone` ⇒ the table is the DAG the
    /// design says it is.
    ///
    /// THE precondition of the emit-time expansion, and
    /// it has to be checked on the TABLE rather than during the walk that consumes it: an entry
    /// that reaches itself is a finite, inspectable thing here and an unbounded recursion once
    /// anything starts substituting bodies into bodies. A cyclic table is a program error (a
    /// recursive `let inline` has no expansion), so the caller reports `Kind.CyclicInline` and
    /// nothing walks the graph.
    let findCycle (entries: TSpecialization[]) : SpecializationId list voption =
        // Unvisited / on the current DFS path / finished. The middle state is the whole test:
        // an edge back into the current path is a cycle, where an edge into a FINISHED entry is
        // ordinary sharing — the table is a DAG, so a diamond is legal and must not be reported.
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
    /// paired with the edge count that convicts them. Empty ⇒ the table is sound.
    ///
    /// Fused material belongs to the ONE site that wrote it, so a second edge into the entry
    /// holding it would run that site's call against the first site's operand. `mintEntry`
    /// asserts the narrow half at the moment an entry is built (a SHAREABLE entry fuses
    /// nothing); this is the half that needs the finished graph, because an edge count is a
    /// fact about the whole table and not about one reduction.
    ///
    /// `roots` are the trees OUTSIDE the table — the file's own declarations — whose edges
    /// count exactly as an entry's do.
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

    /// The resolved-specialization table one run of the pass builds, and the whole of its
    /// lifecycle: reserve a slot before the body that fills it is built, intern an entry for
    /// reuse only once it IS built, and assert on the finished graph.
    ///
    /// Slot order is entry order — a `SpecializationId` an edge carries indexes it — so a slot
    /// is briefly empty and the array is materialised only once every build has finished.
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
            /// The run's binder counter, for the synthetic binder a built entry's `TDecl.Let`
            /// carries.
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
        /// retained so far — which is what READS a foreign anchor. Keyed by path
        /// (`OriginSources`), so a file serving many templates is retained once, and a path
        /// served at two DIFFERENT contents faults at the read rather than silently
        /// re-attributing a body.
        let retainOrigin (src: OriginSource) (t: SpecTable) : OriginSources =
            t.Origins <- OriginSources.add src t.Origins
            t.Origins

        /// The entry `grounding` already names, if a previous site interned one.
        ///
        /// Two conditions decide `shareable`, and both are about whether an entry belongs to
        /// the TEMPLATE rather than to one site. A fused reduction holds the site's own
        /// material; a non-ground grounding leaves this thaw's inference cells inside the
        /// body, which a second site linking to the entry would then share. Either way the
        /// next site resolves its own.
        let private tryReuse (grounding: Grounding) (shareable: bool) (t: SpecTable) : SpecializationId voption =
            if shareable then
                match t.Interned.TryGetValue grounding with
                | true, spec -> ValueSome spec
                | _ -> ValueNone
            else
                ValueNone

        /// The TABLE form of a reduction: a new entry, its surviving parameters abstracted
        /// back into the lambda chain an `InlineCall`'s arguments are positional against.
        /// Arity is therefore the surviving-parameter count and nothing stores it — a
        /// parameter the reduction fused is simply not a parameter of the entry.
        ///
        /// `Build` is handed the slot, so a template whose resolution reaches itself finds an id
        /// it can name and terminates, where building first would recurse until the stack goes.
        /// Every outlined reduction gets one, fused ones included: minting only for the
        /// shareable ones would leave the recursion that cannot be shared diverging.
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

            // Holds by construction — `shareable` implies a CLOSED peel, which is "no
            // parameter fused", and a nested reduction puts its own fusions in its own
            // entry — so this is the check that a fusion bug shows up as a fault here
            // instead of as a body silently shared across sites with one site's material
            // baked into it.
            if o.Shareable && containsCallerExpr value then
                failwithf
                    "InlineExpansion: specialization %d is shareable but marks caller material — a closed reduction fused nothing, so this entry's parameters were mis-classified"
                    slot

            t.Entries.[slot].Built <-
                ValueSome
                    {
                        Key = o.Grounding.Key
                        Origin = o.Origin
                        // The binder is unread — every consumer matches
                        // `TDecl.Let(_, value, _, _)` — so it is minted rather than
                        // taken from anything, exactly as a lifted member body's is.
                        Decl =
                            TDecl.Let(TPat.NamedSimple(t.Mint(), declTy, TastWalk.exprTok value), value, true, declTy)
                    }

            // INTERNED only once BUILT: `Interned` is the reuse pool, and reuse is a decision
            // about a finished entry. Termination is the frame's job — being in flight and
            // being reusable are different facts, and one table cannot answer both.
            if o.Shareable then
                t.Interned.[o.Grounding] <- spec

            spec, reduced.Survivors

        /// Outline one reduction and hand back the EDGE naming it — the whole of what an
        /// outlined call site becomes. THE outlining path: reusing an interned entry and
        /// minting a fresh one differ in nothing but where the edge's arguments come from, and
        /// a second copy of the sequence is a second place the reserve-before-build order could
        /// be got wrong.
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
        /// see. `declExprs` is every tree OUTSIDE the table — the file's own declarations —
        /// whose edges count toward an entry exactly as an entry's own do.
        let finish (declExprs: TExpr seq) (t: SpecTable) : TSpecialization[] =
            let table =
                t.Entries
                |> Seq.mapi (fun i e ->
                    match e.Built with
                    | ValueSome entry -> entry
                    | ValueNone ->
                        failwithf
                            "InlineExpansion: specialization %d was reserved but never built — an expansion abandoned its slot"
                            i
                )
                |> Array.ofSeq

            // REJECT the cycle on the table, before anything walks it. A recursive `let inline`
            // is a fact about the user's source — the expansion it asks for does not exist — so
            // it is a diagnostic naming every binding on the cycle, and nothing expands the
            // graph: splicing bodies into bodies is exactly what would not terminate. THE
            // precondition of the emit-time expansion, discharged here because a cycle is a
            // finite, inspectable thing on the finished table and an unbounded recursion once
            // anything starts substituting.
            match findCycle table with
            | ValueSome cycle ->
                // Named in call order, so the verdict reads as the loop runs, and positioned at
                // the entry the cycle CLOSES ON — a position in the file being compiled, where
                // an entry's own anchors index the file its template was written in.
                match cycle, [ for SpecializationId i in cycle -> Inline.servedName table.[i].Key.Template ] with
                | SpecializationId closes :: _, binding :: via ->
                    t.Report t.Entries.[closes].Site (Kind.CyclicInline(binding, via))
                | _ -> failwith "InlineExpansion: a cycle names at least the binding it closes on"
            | ValueNone ->
                // The half no single reduction can see: fused material belongs to the ONE site
                // that wrote it, and an edge count is a fact about the finished graph.
                // `mintEntry` asserts the narrow half as each entry is built; this is what
                // turns a fusion bug into a fault here rather than a body silently shared
                // across sites with one site's material in it.
                match miscountedFusedEntries declExprs table with
                | [] -> ()
                | bad ->
                    failwithf
                        "InlineExpansion: %A fused caller material yet are named by that many call edges — fused material belongs to the one site that wrote it"
                        [ for (SpecializationId i, count) in bad -> table.[i].Key.Template, count ]

            table
