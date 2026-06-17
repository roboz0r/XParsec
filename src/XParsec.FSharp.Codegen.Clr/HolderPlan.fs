namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// One predicted method row of the holder plan:
/// a value-bearing holder's `.cctor`, a static-method function, or the anonymous
/// "Program" holder's `.cctor` (initialises the leading-prefix top-level
/// values; sits immediately before the holder-less fns).
type MethodSlot =
    | HolderCctor of Emit.HolderKey
    | HolderFn of Emit.StaticFn
    | ProgramCctor

/// The module-level emission plan, computed
/// once — purely — from the lowered decls: which top-level bindings are module
/// values vs static-method functions, the holder emission order, the method-row
/// plan, and the module-value field-row order. The `Assembler` constructor
/// *predicts* `MethodDef` / `FieldDef` handles from positions in these lists
/// and `EmitStaticMethods` walks the *same* lists to emit, so prediction and
/// emission cannot drift.
type HolderPlan =
    {
        /// Module-level values lowered to `public static` fields on their named
        /// holders, in declaration order (see `collectModuleValues` for the
        /// classification rules); every reference is an `ldsfld` — never a
        /// `Main` local or a closure capture.
        ModuleValues: Emit.ModuleValue list
        ModuleValueKeys: HashSet<NodeKey>
        /// Top-level (implicit-"Program"-module) ground values placed in the
        /// Program holder's `.cctor` as `static initonly` fields — the leading
        /// prefix (no top-level `do` before them), in declaration order. Their
        /// initialisers run in the `.cctor` before `Main`.
        ProgramCctorValues: Emit.ModuleValue list
        /// Top-level ground values that follow a top-level `do` — written by `Main`
        /// via `stsfld` (plain mutable `static` fields), in declaration order
        /// Their keys join `MainInitValues` at emit.
        ProgramMainValues: Emit.ModuleValue list
        /// Top-level functions lowered to static methods, in declaration order
        /// (see `collectStaticFns` for the eligibility rules).
        StaticFns: Emit.StaticFn list
        StaticFnKeys: HashSet<NodeKey>
        /// Each static fn's method-axis typar count by binding key; a closure
        /// walked from a generic static fn's body inherits this.
        StaticFnTypars: Dictionary<NodeKey, int>
        /// Functions on the anonymous "Program" holder: they follow the named
        /// holders' methods (and `Main` follows them), unchanged.
        HolderlessFns: Emit.StaticFn list
        /// Holder emission order: fn-bearing holders in first-appearance order,
        /// then any value-only holder.
        OrderedNamedHolders: Emit.HolderKey list
        /// A holder's module values in declaration order (`holderValues`).
        ValuesByHolder: Map<Emit.HolderKey, Emit.ModuleValue list>
        /// Method emission plan: per named holder (in `OrderedNamedHolders`
        /// order) a `.cctor` slot when it has values then its fns, then the
        /// holder-less fns — so every holder's methods form a contiguous
        /// `MethodDef` range. A slot's position is its predicted row offset
        /// past the type/closure methods; `Main` is appended after.
        MethodPlan: MethodSlot list
        /// Module-value field rows in emission order — holder order, each
        /// holder's values in declaration order. These are the *trailing* field
        /// rows (holders emit after every type and closure field).
        ModuleValueFieldOrder: Emit.ModuleValue list
    }

module HolderPlan =

    /// A holder's module values in declaration order; `[]` for a value-less holder.
    let holderValues (plan: HolderPlan) (holder: Emit.HolderKey) : Emit.ModuleValue list =
        match Map.tryFind holder plan.ValuesByHolder with
        | Some vs -> vs
        | None -> []

    /// Classify the lowered top-level decls into module values and static-method
    /// functions, validate the values' initialisers, and fix the holder /
    /// method / field emission orders.
    let create
        (moduleMembers: Map<uint64, ModuleMemberInfo>)
        (programHolder: Emit.HolderKey)
        (topLevelNames: Map<uint64, string>)
        (refStructNsNames: HashSet<string * string>)
        (lowered: Frozen.TDecl list)
        : HolderPlan =
        let moduleValues = Emit.collectModuleValues moduleMembers lowered
        let moduleValueKeys = HashSet<NodeKey>(moduleValues |> List.map (fun mv -> mv.Key))

        // Top-level (implicit-"Program"-module) ground values — holderless `let`s in
        // an exe's last file. Collected unclassified
        // here; the leading/trailing partition runs below once `staticFnKeys`
        // is known. Their keys are real storage (Program-holder fields), so they
        // also join `resolvedTopLevel` (the capture/static-fn analysis treats them as
        // bound, never a captured local).
        let programValues =
            Emit.collectProgramValues moduleMembers programHolder topLevelNames refStructNsNames lowered

        let programValueKeys =
            HashSet<NodeKey>(programValues |> List.map (fun mv -> mv.Key))

        // A *generic* module value (`let empty : SetTree<'T> = …`) cannot become a
        // static *field* — a non-generic module holder has no type parameter to
        // type it — so it lowers to a zero-arg *generic static method* on its
        // holder (real F#'s representation of a generic value); a reference `call`s
        // its `MethodSpec`. They join the static-method machinery as 0-param fns

        let genericModuleValues =
            Emit.collectGenericModuleValues moduleMembers topLevelNames lowered

        let genericModuleValueKeys =
            HashSet<NodeKey>(genericModuleValues |> List.map (fun fn -> fn.Key))

        // Both ground (an `ldsfld` field) and generic (a `call`ed method) module
        // values are real top-level storage, never closure captures — exclude both
        // key sets from the capture/static-fn analysis so a function over them
        // stays a static method rather than capturing a non-existent local
        //.
        let resolvedTopLevel = HashSet<NodeKey>(moduleValueKeys)
        resolvedTopLevel.UnionWith genericModuleValueKeys
        resolvedTopLevel.UnionWith programValueKeys

        let collectedFns, eligibleFnKeys =
            Emit.collectStaticFns moduleMembers resolvedTopLevel lowered

        // Generic module values emit exactly like static fns (signature, body,
        // handle, holder method slot); merge them in so every downstream pass —
        // the `staticMethods` registry, the holder method plan, `discoverClosures`'
        // non-captured set — treats them uniformly. Appended last, so each lands in
        // its holder's method group after the holder's ordinary functions.
        let staticFns = collectedFns @ genericModuleValues

        let staticFnKeys = HashSet<NodeKey>(eligibleFnKeys)
        staticFnKeys.UnionWith genericModuleValueKeys

        // Leading/trailing placement: partition the top-level program values into the leading
        // prefix (`.cctor`, `initonly`) vs the values that follow a top-level
        // statement (`Main`, mutable). The classifier is the *only* site this
        // decision is made (a future effect-graph policy is a drop-in replacement): a value runs in the cctor iff no top-level code
        // that executes in `Main` precedes it — i.e. no `do` (`TDecl.Expression`) and
        // no residue `Main`-local `let` (one that is neither a module value, a static
        // fn, nor itself a program value). A named-holder value / static fn runs in a
        // *method* (its holder's cctor / a static method), not in `Main`, so it does
        // not advance the partition.
        let programByKey = Dictionary<NodeKey, Emit.ModuleValue>()

        for mv in programValues do
            programByKey.[mv.Key] <- mv

        let programCctorValues, programMainValues =
            let cctor = ResizeArray<Emit.ModuleValue>()
            let main = ResizeArray<Emit.ModuleValue>()
            let mutable seenMainCode = false

            for d in lowered do
                match d with
                | TDeclG.LetFn _ -> failwith "HolderPlan.create: LetFn must be normalised to Let by lower"
                | TDeclG.Expression _ -> seenMainCode <- true
                | TDeclG.Let(TPatG.NamedSimple(k, _, _), _, _, _) ->
                    match programByKey.TryGetValue k with
                    | true, mv -> (if seenMainCode then main else cctor).Add mv
                    | _ ->
                        // Not a program value: a static fn / named-holder value runs
                        // in a method (no Main effect); anything else is a residue
                        // Main local whose init runs in `Main`.
                        if not (staticFnKeys.Contains k || moduleValueKeys.Contains k) then
                            seenMainCode <- true
                | TDeclG.Let _ -> seenMainCode <- true // residue destructuring `let` → `Main`
                | TDeclG.Type _ -> ()

            List.ofSeq cctor, List.ofSeq main

        // Every module-value initialiser must resolve entirely to other module
        // values / static methods inside its holder `.cctor` — fail targeted
        // here rather than deep in `buildVarLoad`.
        Emit.validateModuleValueInits moduleValueKeys staticFnKeys moduleValues

        // A leading program value's `.cctor` init may also reference other
        // field-backed program values (`ldsfld`); validate against the union.
        let cctorRefKeys = HashSet<NodeKey>(moduleValueKeys)
        cctorRefKeys.UnionWith programValueKeys
        Emit.validateModuleValueInits cctorRefKeys staticFnKeys programCctorValues

        let staticFnTypars = Dictionary<NodeKey, int>()

        for fn in staticFns do
            staticFnTypars.[fn.Key] <- Emit.staticFnTypars fn

        let namedHolderGroups =
            staticFns
            |> List.choose (fun fn ->
                match fn.Holder with
                | Some h -> Some(h, fn)
                | None -> None
            )
            |> List.groupBy fst
            |> List.map (fun (h, pairs) -> h, List.map snd pairs)

        let holderlessFns = staticFns |> List.filter (fun fn -> fn.Holder.IsNone)

        let valuesByHolder = moduleValues |> List.groupBy (fun mv -> mv.Holder)

        let orderedNamedHolders =
            let fnHolders = namedHolderGroups |> List.map fst

            let valueOnly =
                valuesByHolder
                |> List.map fst
                |> List.filter (fun h -> not (List.contains h fnHolders))

            fnHolders @ valueOnly

        let valuesOf =
            let m = Map.ofList valuesByHolder

            fun h ->
                match Map.tryFind h m with
                | Some vs -> vs
                | None -> []

        let fnsOf =
            let m = Map.ofList namedHolderGroups

            fun h ->
                match Map.tryFind h m with
                | Some fns -> fns
                | None -> []

        let methodPlan =
            [
                for h in orderedNamedHolders do
                    if not (List.isEmpty (valuesOf h)) then
                        yield HolderCctor h

                    yield! fnsOf h |> List.map HolderFn

                // The Program holder's `.cctor` sits immediately before its
                // holder-less fns, so the holder's methods (cctor, fns, then `Main`)
                // form a contiguous `MethodDef` range.
                if not (List.isEmpty programCctorValues) then
                    yield ProgramCctor

                yield! holderlessFns |> List.map HolderFn
            ]

        {
            ModuleValues = moduleValues
            ModuleValueKeys = moduleValueKeys
            ProgramCctorValues = programCctorValues
            ProgramMainValues = programMainValues
            StaticFns = staticFns
            StaticFnKeys = staticFnKeys
            StaticFnTypars = staticFnTypars
            HolderlessFns = holderlessFns
            OrderedNamedHolders = orderedNamedHolders
            ValuesByHolder = Map.ofList valuesByHolder
            MethodPlan = methodPlan
            ModuleValueFieldOrder =
                [
                    for h in orderedNamedHolders do
                        yield! valuesOf h
                    // Program-holder value fields are the trailing field rows — the
                    // Program slot is the last type. `initonly` (cctor) ones
                    // first, then the `Main`-written mutable ones; both resolve to an
                    // `ldsfld` via `moduleValueFields`.
                    yield! programCctorValues
                    yield! programMainValues
                ]
        }
