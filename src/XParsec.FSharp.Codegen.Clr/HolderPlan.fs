namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// One predicted method row of the holder plan (module-representation-plan §5):
/// a value-bearing holder's `.cctor`, or a static-method function.
type MethodSlot =
    | HolderCctor of Emit.HolderKey
    | HolderFn of Emit.StaticFn

/// The module-level emission plan (module-representation-plan §5), computed
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
    let create (moduleMembers: Map<uint64, ModuleMemberInfo>) (lowered: Frozen.TDecl list) : HolderPlan =
        let moduleValues = Emit.collectModuleValues moduleMembers lowered
        let moduleValueKeys = HashSet<NodeKey>(moduleValues |> List.map (fun mv -> mv.Key))

        let staticFns, staticFnKeys =
            Emit.collectStaticFns moduleMembers moduleValueKeys lowered

        // Every module-value initialiser must resolve entirely to other module
        // values / static methods inside its holder `.cctor` — fail targeted
        // here rather than deep in `buildVarLoad`.
        Emit.validateModuleValueInits moduleValueKeys staticFnKeys moduleValues

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

                yield! holderlessFns |> List.map HolderFn
            ]

        {
            ModuleValues = moduleValues
            ModuleValueKeys = moduleValueKeys
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
                ]
        }
