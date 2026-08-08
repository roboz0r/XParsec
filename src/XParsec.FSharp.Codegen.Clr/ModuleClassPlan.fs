namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

/// One predicted method row of the plan: a value-bearing module class's `.cctor`, a
/// static-method function, or the anonymous "Program" class's `.cctor`, which
/// initialises the leading-prefix top-level values just before the Program-class fns.
type MethodSlot =
    | ModuleClassCctor of Emit.ModuleClassKey
    | ModuleClassFn of Emit.StaticFn
    | ProgramCctor

/// The module-level emission plan, computed once — purely — from the lowered decls: which
/// top-level bindings are module values vs static-method functions, the module class emission
/// order, the method-row plan, and the module-value field-row order.
type ModuleClassPlan =
    {
        /// The lowered decls AFTER bridging — every non-saturated reference to a
        /// static-eligible function eta-expanded to a wrapper closure. Downstream passes
        /// walk THIS list, so they see the nodes the plan was computed from.
        Lowered: TastAccessor.DeclId list
        /// Module-level values lowered to `public static` fields on their named module classes,
        /// in declaration order; every reference is an `ldsfld` — never a `Main` local or
        /// a closure capture.
        ModuleValues: Emit.ModuleValue list
        ModuleValueKeys: HashSet<BoundVarId>
        /// Top-level (implicit-"Program"-module) ground values placed in the Program
        /// module class's `.cctor` as `static initonly` fields — the leading prefix (no top-level
        /// `do` before them), in declaration order, initialised before `Main` runs.
        ProgramCctorValues: Emit.ModuleValue list
        /// Top-level ground values that FOLLOW a top-level `do`: plain mutable `static`
        /// fields, written by `Main` via `stsfld`, in declaration order.
        ProgramMainValues: Emit.ModuleValue list
        /// Top-level functions lowered to static methods, in declaration order.
        StaticFns: Emit.StaticFn list
        StaticFnKeys: HashSet<BoundVarId>
        /// Each static fn's method-axis typar count by binding key; a closure
        /// walked from a generic static fn's body inherits this.
        StaticFnTypars: Dictionary<BoundVarId, int>
        /// Functions on the anonymous "Program" class: they follow the named module classes'
        /// methods, and `Main` follows them.
        ProgramFns: Emit.StaticFn list
        /// The module classes this plan gives METHODS or VALUES to, fn-bearing ones in
        /// first-appearance order then any value-only module class. NOT the emitted module class set:
        /// a module holding only TYPES, and an ancestor of a nested module class, get one too.
        OrderedNamedClasses: Emit.ModuleClassKey list
        ValuesByClass: Dictionary<Emit.ModuleClassKey, Emit.ModuleValue list>
        /// Which methods a module class owns and in what order *within* that module class: a `.cctor`
        /// when it has values, then its fns; then the Program class's `.cctor` and its
        /// Program-class fns. NOT a row order — `MethodDef` rows are the layout tree's.
        MethodPlan: MethodSlot list
        /// Every module-level value that gets a static FIELD — named-module values in
        /// module class order, then the Program class's. The field ROW order is the layout
        /// tree's, not this.
        AllModuleValues: Emit.ModuleValue list
    }

module ModuleClassPlan =

    /// A module class's module values in declaration order; `[]` for a value-less module class.
    let moduleClassValues (plan: ModuleClassPlan) (moduleClass: Emit.ModuleClassKey) : Emit.ModuleValue list =
        match plan.ValuesByClass.TryGetValue moduleClass with
        | true, vs -> vs
        | _ -> []

    /// Classify the lowered top-level decls into module values and static-method
    /// functions, validate the values' initialisers, and fix the module class /
    /// method / field emission orders.
    let create
        (moduleMembers: Map<BoundVarId, ModuleBindingInfo>)
        // Forwarded to populate `StaticFn.Constraints`, which drives the call-site
        // phantom-typar solve; the emitted arity is re-derived independently below.
        (genericFnSchemes: Map<BoundVarId, FrozenConstraint list>)
        (programClass: Emit.ModuleClassKey)
        (refStructNsNames: HashSet<string * string>)
        (lowered0: TastAccessor.DeclId list)
        : ModuleClassPlan =
        // How every top-level decl of this file emits (name, module class, handle key), decided
        // before bridging — which rewrites expressions inside decls but neither adds nor
        // removes a top-level bound variable, so the same table is valid for `lowered` below.
        let emissions = Emit.emissions moduleMembers programClass lowered0

        // Drives bridging: a value-use of a function that survives as a static method
        // becomes a curried bridge; a capture-demoted one keeps its closure. The top-level
        // *storage* set — an `ldsfld` / `call` target, never a capture.
        let preResolvedTopLevel =
            let s = HashSet<BoundVarId>()

            for mv in Emit.collectModuleValues emissions lowered0 do
                s.Add mv.Key |> ignore

            for mv in Emit.collectProgramValues emissions programClass refStructNsNames lowered0 do
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

        let moduleValueKeys =
            HashSet<BoundVarId>(moduleValues |> List.map (fun mv -> mv.Key))

        // Top-level (implicit-"Program"-module) ground values — top-level `let`s in an
        // exe's last file, collected unclassified; the leading/trailing partition runs
        // below, once `staticFnKeys` is known. Their keys are real storage, never captures.
        let programValues =
            Emit.collectProgramValues emissions programClass refStructNsNames lowered

        let programValueKeys =
            HashSet<BoundVarId>(programValues |> List.map (fun mv -> mv.Key))

        // A *generic* module value (`let empty : SetTree<'T> = …`) cannot become a static
        // FIELD — a non-generic module class has no type parameter to type it — so it lowers to a
        // zero-arg generic static METHOD; a reference `call`s its `MethodSpec`.
        let genericModuleValues = Emit.collectGenericModuleValues emissions lowered

        let genericModuleValueKeys =
            HashSet<BoundVarId>(genericModuleValues |> List.map (fun fn -> fn.Key))

        // The static-method functions: the eligible set (computed pre-bridge) projected
        // onto the bridged decls. A binding bridging newly turned into a lambda whose key
        // was never eligible is skipped here and falls to closure discovery.
        let collectedFns =
            Emit.collectStaticFns emissions genericFnSchemes eligible (CompiledFns.gather lowered)

        // Generic module values emit exactly like static fns (signature, body, handle,
        // module class method slot); merge them in so every downstream pass treats them
        // uniformly. Appended last, so each lands after its module class's ordinary functions.
        let staticFns = collectedFns @ genericModuleValues

        let staticFnKeys = HashSet<BoundVarId>(eligible)
        staticFnKeys.UnionWith genericModuleValueKeys

        // Partition the top-level program values into the leading prefix (`.cctor`,
        // `initonly`) vs those following a top-level statement (`Main`, mutable): a value
        // runs in the cctor iff nothing that executes in `Main` precedes it.
        let programByKey = Dictionary<BoundVarId, Emit.ModuleValue>()

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
                    // Any pattern but a simple named bound variable yields `ValueNone`.
                    match TastAccessor.patBoundVar (TastAccessor.declLet d).Pattern with
                    | ValueSome k ->
                        match programByKey.TryGetValue k with
                        | true, mv -> (if seenMainCode then main else cctor).Add mv
                        | _ ->
                            // Not a program value: a static fn / named-module value runs in
                            // a method (no `Main` effect); anything else initialises in `Main`.
                            if not (staticFnKeys.Contains k || moduleValueKeys.Contains k) then
                                seenMainCode <- true
                    | ValueNone -> seenMainCode <- true // residue destructuring `let` → `Main`
                | DeclShape.Type -> ()

            List.ofSeq cctor, List.ofSeq main

        // Every module-value initialiser must resolve entirely to other module values /
        // static methods inside its module class `.cctor`.
        Emit.validateModuleValueInits moduleValueKeys staticFnKeys moduleValues

        // A leading program value's `.cctor` init may also reference other
        // field-backed program values (`ldsfld`); validate against the union.
        let cctorRefKeys = HashSet<BoundVarId>(moduleValueKeys)
        cctorRefKeys.UnionWith programValueKeys
        Emit.validateModuleValueInits cctorRefKeys staticFnKeys programCctorValues

        // The emitted generic-method arity is the max `FTTypar(Method, i)` index over
        // params + result + BODY. The body sweep catches a phantom constraint typar
        // (`fold`'s enumerator `'E`) that params and result cannot see.
        let staticFnTypars = Dictionary<BoundVarId, int>()

        for fn in staticFns do
            staticFnTypars.[fn.Key] <- Emit.staticFnTypars fn

        let namedClassGroups =
            staticFns
            |> List.choose (fun fn ->
                match fn.ModuleClass with
                | Some h -> Some(h, fn)
                | None -> None
            )
            |> List.groupBy fst
            |> List.map (fun (h, pairs) -> h, List.map snd pairs)

        let programFns = staticFns |> List.filter (fun fn -> fn.ModuleClass.IsNone)

        let valuesByClass = moduleValues |> List.groupBy (fun mv -> mv.ModuleClass)

        let orderedNamedClasses =
            let fnClasses = namedClassGroups |> List.map fst

            let valueOnly =
                valuesByClass
                |> List.map fst
                |> List.filter (fun h -> not (List.contains h fnClasses))

            fnClasses @ valueOnly

        // `Dictionary`, not `Map`: a `ModuleClassKey` is equatable but not ordered — its
        // namespace path is an `EqArray`, which has no comparison.
        let valuesByClassIndex = Dictionary<Emit.ModuleClassKey, Emit.ModuleValue list>()

        for (h, vs) in valuesByClass do
            valuesByClassIndex.[h] <- vs

        let valuesOf h =
            match valuesByClassIndex.TryGetValue h with
            | true, vs -> vs
            | _ -> []

        let fnsOf =
            let m = Dictionary<Emit.ModuleClassKey, Emit.StaticFn list>()

            for (h, fns) in namedClassGroups do
                m.[h] <- fns

            fun h ->
                match m.TryGetValue h with
                | true, fns -> fns
                | _ -> []

        let methodPlan =
            [
                for h in orderedNamedClasses do
                    if not (List.isEmpty (valuesOf h)) then
                        yield ModuleClassCctor h

                    yield! fnsOf h |> List.map ModuleClassFn

                // The Program class's `.cctor` runs before its Program-class fns and
                // before `Main`.
                if not (List.isEmpty programCctorValues) then
                    yield ProgramCctor

                yield! programFns |> List.map ModuleClassFn
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
            ProgramFns = programFns
            OrderedNamedClasses = orderedNamedClasses
            ValuesByClass = valuesByClassIndex
            MethodPlan = methodPlan
            AllModuleValues =
                [
                    for h in orderedNamedClasses do
                        yield! valuesOf h
                    // The Program class's: `initonly` (cctor) ones first, then the
                    // `Main`-written mutable ones.
                    yield! programCctorValues
                    yield! programMainValues
                ]
        }
