namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// One predicted method row of the holder plan: a value-bearing holder's `.cctor`, a
/// static-method function, or the anonymous "Program" holder's `.cctor`, which
/// initialises the leading-prefix top-level values just before the holder-less fns.
type MethodSlot =
    | HolderCctor of Emit.HolderKey
    | HolderFn of Emit.StaticFn
    | ProgramCctor

/// The module-level emission plan, computed once — purely — from the lowered decls: which
/// top-level bindings are module values vs static-method functions, the holder emission
/// order, the method-row plan, and the module-value field-row order.
type HolderPlan =
    {
        /// The lowered decls AFTER bridging — every non-saturated reference to a
        /// static-eligible function eta-expanded to a wrapper closure. Downstream passes
        /// walk THIS list, so they see the nodes the plan was computed from.
        Lowered: TastAccessor.DeclId list
        /// Module-level values lowered to `public static` fields on their named holders,
        /// in declaration order; every reference is an `ldsfld` — never a `Main` local or
        /// a closure capture.
        ModuleValues: Emit.ModuleValue list
        ModuleValueKeys: HashSet<BinderId>
        /// Top-level (implicit-"Program"-module) ground values placed in the Program
        /// holder's `.cctor` as `static initonly` fields — the leading prefix (no top-level
        /// `do` before them), in declaration order, initialised before `Main` runs.
        ProgramCctorValues: Emit.ModuleValue list
        /// Top-level ground values that FOLLOW a top-level `do`: plain mutable `static`
        /// fields, written by `Main` via `stsfld`, in declaration order.
        ProgramMainValues: Emit.ModuleValue list
        /// Top-level functions lowered to static methods, in declaration order.
        StaticFns: Emit.StaticFn list
        StaticFnKeys: HashSet<BinderId>
        /// Each static fn's method-axis typar count by binding key; a closure
        /// walked from a generic static fn's body inherits this.
        StaticFnTypars: Dictionary<BinderId, int>
        /// Functions on the anonymous "Program" holder: they follow the named holders'
        /// methods, and `Main` follows them.
        HolderlessFns: Emit.StaticFn list
        /// The holders this plan gives METHODS or VALUES to, fn-bearing ones in
        /// first-appearance order then any value-only holder. NOT the emitted holder set:
        /// a module holding only TYPES, and an ancestor of a nested holder, get one too.
        OrderedNamedHolders: Emit.HolderKey list
        ValuesByHolder: Dictionary<Emit.HolderKey, Emit.ModuleValue list>
        /// Which methods a holder owns and in what order *within* that holder: a `.cctor`
        /// when it has values, then its fns; then the Program holder's `.cctor` and its
        /// holder-less fns. NOT a row order — `MethodDef` rows are the layout tree's.
        MethodPlan: MethodSlot list
        /// Every module-level value that gets a static FIELD — named-holder values in
        /// holder order, then the Program holder's. The field ROW order is the layout
        /// tree's, not this.
        AllModuleValues: Emit.ModuleValue list
    }

module HolderPlan =

    /// A holder's module values in declaration order; `[]` for a value-less holder.
    let holderValues (plan: HolderPlan) (holder: Emit.HolderKey) : Emit.ModuleValue list =
        match plan.ValuesByHolder.TryGetValue holder with
        | true, vs -> vs
        | _ -> []

    /// Classify the lowered top-level decls into module values and static-method
    /// functions, validate the values' initialisers, and fix the holder /
    /// method / field emission orders.
    let create
        (moduleMembers: Map<BinderId, ModuleBindingInfo>)
        // Forwarded to populate `StaticFn.Constraints`, which drives the call-site
        // phantom-typar solve; the emitted arity is re-derived independently below.
        (genericFnSchemes: Map<BinderId, FrozenConstraint list>)
        (programHolder: Emit.HolderKey)
        (refStructNsNames: HashSet<string * string>)
        (lowered0: TastAccessor.DeclId list)
        : HolderPlan =
        // How every top-level decl of this file emits (name, holder, handle key), decided
        // before bridging — which rewrites expressions inside decls but neither adds nor
        // removes a top-level binder, so the same table is valid for `lowered` below.
        let emissions = Emit.emissions moduleMembers programHolder lowered0

        // Drives bridging: a value-use of a function that survives as a static method
        // becomes a curried bridge; a capture-demoted one keeps its closure. The top-level
        // *storage* set — an `ldsfld` / `call` target, never a capture.
        let preResolvedTopLevel =
            let s = HashSet<BinderId>()

            for mv in Emit.collectModuleValues emissions lowered0 do
                s.Add mv.Key |> ignore

            for mv in Emit.collectProgramValues emissions programHolder refStructNsNames lowered0 do
                s.Add mv.Key |> ignore

            for fn in Emit.collectGenericModuleValues emissions lowered0 do
                s.Add fn.Key |> ignore

            s

        // One `gather` on the UN-bridged decls feeds both the capture-eligibility analysis
        // and the bridge's arity table; the collectors below re-gather the POST-bridge
        // decls, since bridging can turn a `let g = f` value into a lambda that peels.
        let fns0 = CompiledFns.gather lowered0
        let eligible = Emit.staticEligible preResolvedTopLevel fns0

        // Eta-expand every non-saturated reference to an eligible function so it stays a
        // flat static method and the escape becomes a wrapper closure. Everything below is
        // computed on the BRIDGED decls, which are published on the plan.
        let lowered = Emit.bridgeStaticFnEscapes eligible fns0 lowered0

        let moduleValues = Emit.collectModuleValues emissions lowered
        let moduleValueKeys = HashSet<BinderId>(moduleValues |> List.map (fun mv -> mv.Key))

        // Top-level (implicit-"Program"-module) ground values — holderless `let`s in an
        // exe's last file, collected unclassified; the leading/trailing partition runs
        // below, once `staticFnKeys` is known. Their keys are real storage, never captures.
        let programValues =
            Emit.collectProgramValues emissions programHolder refStructNsNames lowered

        let programValueKeys =
            HashSet<BinderId>(programValues |> List.map (fun mv -> mv.Key))

        // A *generic* module value (`let empty : SetTree<'T> = …`) cannot become a static
        // FIELD — a non-generic holder has no type parameter to type it — so it lowers to a
        // zero-arg generic static METHOD; a reference `call`s its `MethodSpec`.
        let genericModuleValues = Emit.collectGenericModuleValues emissions lowered

        let genericModuleValueKeys =
            HashSet<BinderId>(genericModuleValues |> List.map (fun fn -> fn.Key))

        // The static-method functions: the eligible set (computed pre-bridge) projected
        // onto the bridged decls. A binding bridging newly turned into a lambda whose key
        // was never eligible is skipped here and falls to closure discovery.
        let collectedFns =
            Emit.collectStaticFns emissions genericFnSchemes eligible (CompiledFns.gather lowered)

        // Generic module values emit exactly like static fns (signature, body, handle,
        // holder method slot); merge them in so every downstream pass treats them
        // uniformly. Appended last, so each lands after its holder's ordinary functions.
        let staticFns = collectedFns @ genericModuleValues

        let staticFnKeys = HashSet<BinderId>(eligible)
        staticFnKeys.UnionWith genericModuleValueKeys

        // Partition the top-level program values into the leading prefix (`.cctor`,
        // `initonly`) vs those following a top-level statement (`Main`, mutable): a value
        // runs in the cctor iff nothing that executes in `Main` precedes it.
        let programByKey = Dictionary<BinderId, Emit.ModuleValue>()

        for mv in programValues do
            programByKey.[mv.Key] <- mv

        let programCctorValues, programMainValues =
            let cctor = ResizeArray<Emit.ModuleValue>()
            let main = ResizeArray<Emit.ModuleValue>()
            let mutable seenMainCode = false

            for d in lowered do
                match TastAccessor.declKind d with
                | DeclShape.Expression -> seenMainCode <- true
                | DeclShape.Let ->
                    // Any pattern but a simple named binder yields `ValueNone`.
                    match TastAccessor.patBinder (TastAccessor.declLet d).Binding with
                    | ValueSome k ->
                        match programByKey.TryGetValue k with
                        | true, mv -> (if seenMainCode then main else cctor).Add mv
                        | _ ->
                            // Not a program value: a static fn / named-holder value runs in
                            // a method (no `Main` effect); anything else initialises in `Main`.
                            if not (staticFnKeys.Contains k || moduleValueKeys.Contains k) then
                                seenMainCode <- true
                    | ValueNone -> seenMainCode <- true // residue destructuring `let` → `Main`
                | DeclShape.Type -> ()

            List.ofSeq cctor, List.ofSeq main

        // Every module-value initialiser must resolve entirely to other module values /
        // static methods inside its holder `.cctor`.
        Emit.validateModuleValueInits moduleValueKeys staticFnKeys moduleValues

        // A leading program value's `.cctor` init may also reference other
        // field-backed program values (`ldsfld`); validate against the union.
        let cctorRefKeys = HashSet<BinderId>(moduleValueKeys)
        cctorRefKeys.UnionWith programValueKeys
        Emit.validateModuleValueInits cctorRefKeys staticFnKeys programCctorValues

        // The emitted generic-method arity is the max `FTTypar(Method, i)` index over
        // params + result + BODY. The body sweep catches a phantom constraint typar
        // (`fold`'s enumerator `'E`) that params and result cannot see.
        let staticFnTypars = Dictionary<BinderId, int>()

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

        // `Dictionary`, not `Map`: a `HolderKey` is equatable but not ordered — its
        // namespace path is an `EqArray`, which has no comparison.
        let valuesByHolderIndex = Dictionary<Emit.HolderKey, Emit.ModuleValue list>()

        for (h, vs) in valuesByHolder do
            valuesByHolderIndex.[h] <- vs

        let valuesOf h =
            match valuesByHolderIndex.TryGetValue h with
            | true, vs -> vs
            | _ -> []

        let fnsOf =
            let m = Dictionary<Emit.HolderKey, Emit.StaticFn list>()

            for (h, fns) in namedHolderGroups do
                m.[h] <- fns

            fun h ->
                match m.TryGetValue h with
                | true, fns -> fns
                | _ -> []

        let methodPlan =
            [
                for h in orderedNamedHolders do
                    if not (List.isEmpty (valuesOf h)) then
                        yield HolderCctor h

                    yield! fnsOf h |> List.map HolderFn

                // The Program holder's `.cctor` runs before its holder-less fns and
                // before `Main`.
                if not (List.isEmpty programCctorValues) then
                    yield ProgramCctor

                yield! holderlessFns |> List.map HolderFn
            ]

        {
            Lowered = lowered
            ModuleValues = moduleValues
            ModuleValueKeys = moduleValueKeys
            ProgramCctorValues = programCctorValues
            ProgramMainValues = programMainValues
            StaticFns = staticFns
            StaticFnKeys = staticFnKeys
            StaticFnTypars = staticFnTypars
            HolderlessFns = holderlessFns
            OrderedNamedHolders = orderedNamedHolders
            ValuesByHolder = valuesByHolderIndex
            MethodPlan = methodPlan
            AllModuleValues =
                [
                    for h in orderedNamedHolders do
                        yield! valuesOf h
                    // The Program holder's: `initonly` (cctor) ones first, then the
                    // `Main`-written mutable ones.
                    yield! programCctorValues
                    yield! programMainValues
                ]
        }
