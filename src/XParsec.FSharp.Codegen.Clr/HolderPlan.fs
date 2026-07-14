namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

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
        /// The lowered decls AFTER `bridgeStaticFnEscapes` — every non-saturated
        /// reference to a static-eligible function eta-expanded to a wrapper closure.
        /// This is the single decl list every downstream pass (closure discovery,
        /// `buildMain`) must walk, so they see the same rewritten nodes the holder
        /// plan was computed from.
        Lowered: Frozen.TDecl list
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
        /// The holders this plan gives METHODS or VALUES to, fn-bearing ones in
        /// first-appearance order then any value-only holder. NOT the emitted holder set:
        /// a module that holds only TYPES gets a holder class too, and so does an
        /// ancestor of a nested holder — both are discovered from the type decls in
        /// `Layout.build`, which is the one place the holder TREE is built.
        OrderedNamedHolders: Emit.HolderKey list
        /// A holder's module values in declaration order (`holderValues`). A
        /// `Dictionary` because a `ModuleKey` is equatable but not ORDERED (its
        /// namespace path is an `EqArray`, which has no comparison).
        ValuesByHolder: Dictionary<Emit.HolderKey, Emit.ModuleValue list>
        /// Which methods a holder owns and in what order *within* that holder: a
        /// `.cctor` when it has values, then its fns; then the Program holder's
        /// `.cctor` and its holder-less fns. The `Assembler`'s Prepare pass walks it to
        /// bind every holder-owned method body.
        ///
        /// NOT a row order: `MethodDef` rows are the pre-order flattening of
        /// `Layout`'s `TypeNode` tree, so a holder's methods are contiguous because they
        /// hang off its node — not because a side list happened to list them together.
        MethodPlan: MethodSlot list
        /// Every module-level value that gets a static FIELD — named-holder values in
        /// holder order, then the Program holder's. The `Assembler` maps each to its
        /// written field handle; the field ROW order is the layout tree's, not this.
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
        (moduleMembers: Map<NodeKey, ModuleMemberInfo>)
        // Forwarded to `collectStaticFns` to populate
        // `StaticFn.Constraints`, which drives the call-site phantom-typar solve
        // (`EmitCall`). The emitted arity is re-derived independently by the
        // `Emit.staticFnTypars` body sweep.
        (genericFnSchemes: Map<NodeKey, FrozenConstraint list>)
        (programHolder: Emit.HolderKey)
        (topLevelNames: Map<NodeKey, string>)
        (refStructNsNames: HashSet<string * string>)
        (lowered0: Frozen.TDecl list)
        : HolderPlan =
        // The capture-only eligible set drives bridging: a value-use of a function
        // that survives as a static method becomes a curried bridge; a capture-demoted
        // function keeps its closure (no bridge). It is computed on the UN-bridged
        // decls against the top-level *storage* set (module / program / generic values
        // are `ldsfld` / `call`, never captures) and then reused verbatim by
        // `collectStaticFns` below — so the set bridging assumed and the set emitted as
        // static methods are provably identical (see `staticEligible`). A binding that
        // bridging flips from a value to a static-fn candidate (e.g. `let g = f` where
        // `f` is eligible) does not perturb the capture analysis: both module-value and
        // static-fn keys are non-capturing, so the storage set computed pre-bridge is a
        // sound seed.
        let preResolvedTopLevel =
            let s = HashSet<NodeKey>()

            for mv in Emit.collectModuleValues moduleMembers lowered0 do
                s.Add mv.Key |> ignore

            for mv in Emit.collectProgramValues moduleMembers programHolder topLevelNames refStructNsNames lowered0 do
                s.Add mv.Key |> ignore

            for fn in Emit.collectGenericModuleValues moduleMembers topLevelNames lowered0 do
                s.Add fn.Key |> ignore

            s

        // `gather` once on the un-bridged decls: the same `CompiledFn list` feeds the
        // capture-eligibility analysis AND the bridge's arity table, so they cannot
        // disagree about which functions exist. `collectStaticFns` re-gathers the
        // POST-bridge `lowered` (bridging can turn a `let g = f` value into a lambda
        // that now peels to groups), so exactly two gathers run, not three.
        let fns0 = CompiledFns.gather lowered0
        let eligible = Emit.staticEligible preResolvedTopLevel fns0

        // Eta-expand every non-saturated reference to an eligible function so it stays
        // a flat static method and the escape becomes a wrapper closure (F#/JS model).
        // Everything below is computed on the BRIDGED decls; `lowered` is published on
        // the plan so closure discovery / `buildMain` walk the same rewritten nodes.
        let lowered = Emit.bridgeStaticFnEscapes eligible fns0 lowered0

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

        // The static-method functions: the eligible set (computed pre-bridge, reused
        // here) projected onto the bridged decls. A binding bridging newly turned into
        // a lambda whose key was never eligible is skipped here and falls to closure
        // discovery.
        let collectedFns =
            Emit.collectStaticFns moduleMembers genericFnSchemes eligible (CompiledFns.gather lowered)

        // Generic module values emit exactly like static fns (signature, body,
        // handle, holder method slot); merge them in so every downstream pass —
        // the `staticMethods` registry, the holder method plan, `discoverClosures`'
        // non-captured set — treats them uniformly. Appended last, so each lands in
        // its holder's method group after the holder's ordinary functions.
        let staticFns = collectedFns @ genericModuleValues

        let staticFnKeys = HashSet<NodeKey>(eligible)
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

        // The emitted generic-method arity is the max
        // `FTTypar(Method, i)` index over params + result + BODY. The body sweep is
        // the change — it catches a phantom constraint typar (`fold`'s enumerator
        // `'E`) that param/result cannot see but that survives un-grounded in the
        // `for-in` enumerator descriptor, so `fold` emits at its true arity and the
        // call site solves `'E` from its bound. It deliberately does NOT use the
        // front-end `scheme.Quantified.Length`, which over-counts a quantified-but-
        // body-erased typar (the `SetTree.compare` regression).
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

        // `Dictionary`, not `Map`: a `ModuleKey` is equatable but not ordered.
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
                    // `Main`-written mutable ones; both resolve to an `ldsfld` via
                    // `moduleValueFields`.
                    yield! programCctorValues
                    yield! programMainValues
                ]
        }
