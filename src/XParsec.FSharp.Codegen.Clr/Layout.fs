namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis

module internal Layout =

    /// Build ONE compilation unit's contribution to the type HIERARCHY, and its slice of
    /// the ranged tables. By kind: namespace-level interfaces / unions / records / classes
    /// / enums → closures → the root modules' holders. A module's holder is immediately
    /// followed by the types it holds — in that same by-kind order — and by its nested
    /// modules' holders. The single `<Module>` pseudo-type (TypeDef row 1) and the single
    /// Program holder are NOT minted here: they belong to the assembly, so `combine` mints
    /// them once around the concatenated units. The shared `ClosureNamer` is threaded in so
    /// a multi-unit driver can keep closure TypeDef names unique assembly-wide. Reuses the
    /// existing lowering/discovery passes unchanged and carries their products.
    let buildUnit
        (closureNamer: Emit.ClosureNamer)
        (symbols: ICodegenSymbols)
        (project: ProjectInfo)
        (pools: FrozenPools)
        : UnitLayout =
        // The file's trees as columns, with an append-only overlay stacked over them.
        // Every node this emission DERIVES — the eta bridges, the closure-verdict
        // retypes, a `use`'s dispose synthetic — is appended to the overlay, and every
        // id the canonical pool already handed out keeps naming the same node, which is
        // what lets the derived nodes be minted mid-emit rather than in one batch.
        let pool = TastPoolBuilder.openOver pools
        let decls = TastAccessor.roots pool |> List.ofArray

        // The four binder-keyed side tables the CLR lowering consumes, back in their
        // `NodeKey` form. `HolderPlan`/`Emit.discoverClosures` and everything downstream
        // of them (`StaticFn`/`ModuleValue` keys, the closure capture sets) identify a
        // binding by `NodeKey`, so handing them the pool's `BinderId` form would be a
        // rekey of the whole CLR emit rather than a change at this seam.
        let moduleMembers = TastUnpool.binderKeyedMap pools pools.ModuleMembers
        let genericFnSchemes = TastUnpool.binderKeyedMap pools pools.GenericFnSchemes
        let topLevelNames = TastUnpool.binderKeyedMap pools pools.TopLevelNames
        let closureReprs = TastUnpool.binderKeyedMap pools pools.ClosureReprs

        // A source lambda's verdict, on the lambda ID SPACE: a lambda's dense id is its
        // `ExprPoolId`, so a discovered lambda's verdict is a lookup on the node itself
        // rather than a key recomputed from its token.
        let funVerdicts = Dictionary<ExprPoolId, FunVerdict>()

        for (id, v) in pools.FunVerdicts do
            funVerdicts.[id] <- v

        let lowered0 = Emit.lower decls
        // The anonymous "Program" holder's key — a module of that name in the global
        // namespace. It owns the holder-less fns + `Main` + the top-level value fields /
        // `.cctor`. Its type slot is `TypeSlotKey.Program`, not `TypeSlotKey.Holder`, so
        // this key never names a holder class: it exists only to tag those values'
        // `Holder` field.
        let programHolder =
            SymbolKeyOps.moduleKeyOf (ModuleHolder.InNamespace NamespaceKey.Global) project.ModuleName

        // `(ns, name)` of every `[<Struct; IsByRefLike>]` type — a top-level value of
        // such a type can't be a static field; computed from the pool's own decl roots
        // since `Emit.lower` strips the type decls `lowered` would carry.
        let refStructNsNames =
            [
                for d in decls do
                    match TastAccessor.declKind d with
                    | DeclShape.Type ->
                        let td = TastAccessor.declType d

                        match td.Kind with
                        | TTypeKindG.Class c when c.ValueKind = ClassValueKind.RefStruct ->
                            Emit.typeKeyNsName td.TypeKey
                        | _ -> ()
                    | _ -> ()
            ]
            |> HashSet

        // `HolderPlan.create` eta-expands every non-saturated reference to a
        // static-eligible module function (`bridgeStaticFnEscapes`) — keeping the flat
        // static method and adding a wrapper closure (F#/JS model) — and publishes the
        // bridged decls as `plan.Lowered`. That single rewritten list feeds both
        // closure discovery and `buildMain`, so they see the same nodes the plan was
        // computed from.
        let plan =
            HolderPlan.create moduleMembers genericFnSchemes programHolder topLevelNames refStructNsNames lowered0

        let lowered = plan.Lowered

        // Member bodies never pass through `Emit.lower` — they need no lowering at all;
        // they arrive from the freeze ready to emit. Partitioned **once** here and
        // published as `Partitioned`, so closure discovery and `buildMember` walk the
        // same node IDS — the `…ByNode` tables key on them.
        let partitioned = LayoutNodes.partitionTypeDecls decls

        // Closure-discovery roots from every (expanded) member body and class-preamble
        // expression, each tagged with its declaring type's typar count (0 ⇒
        // monomorphic). A preamble initialiser or `do` body is emitted into the `.ctor` /
        // `.cctor` from these very nodes, so a lambda in one (a function-valued `let`,
        // `let bump x = …`) is a closure exactly as a member body's is — omit it and its
        // construction site finds no discovered closure.
        let memberRoots =
            [
                let root
                    (td: TastAccessor.TypeDecl)
                    (methodTypars: int)
                    (body: TastAccessor.ExprId)
                    : EmitClosures.MemberClosureRoot =
                    {
                        DeclaringTypars = td.TypeParams.Length
                        MethodTypars = methodTypars
                        Body = body
                    }

                let memberRoot (td: TastAccessor.TypeDecl) (m: TastAccessor.TypeMember) =
                    root td m.MethodTypeParams.Length m.Body

                let preambleRoot (td: TastAccessor.TypeDecl) (entry: TastAccessor.PreambleEntry) =
                    match entry with
                    | TPreambleEntryG.Let l -> root td 0 l.Init
                    | TPreambleEntryG.Do e -> root td 0 e

                for ud in partitioned.Unions do
                    for m in ud.Members -> memberRoot ud.Decl m

                    for (_, ms) in ud.Interfaces do
                        for m in ms -> memberRoot ud.Decl m

                for rd in partitioned.Records do
                    for m in rd.Members -> memberRoot rd.Decl m

                for cd in partitioned.Classes do
                    for m in cd.Members -> memberRoot cd.Decl m

                    for (_, ms) in cd.Interfaces do
                        for m in ms -> memberRoot cd.Decl m

                    for entry in cd.StaticPreamble @ cd.InstancePreamble -> preambleRoot cd.Decl entry
            ]

        let closures, closureByNode =
            Emit.discoverClosures
                closureNamer
                plan.StaticFnKeys
                plan.ModuleValueKeys
                plan.StaticFnTypars
                // The node-keyed value-struct closure verdicts,
                // snapshotted in `Pipeline` like `ClosureReprs`.
                // `discoverClosures` marks a source-lambda argument a value-struct (and
                // at what flat arity) by node membership — no structural re-derivation.
                funVerdicts
                closureReprs
                lowered
                memberRoots

        // Each type's node carries its own field and method rows: the rows the writer
        // walks ARE the rows its range claims, since both are `List.collect`s over the
        // same flattening. The by-kind builders are module-level (`buildXNodes`), so this
        // stays a slim orchestration over the partition slices + discovered closures, in
        // the by-kind order the `TypeDef` table has always used.
        let interfaceNodes = LayoutNodes.buildInterfaceNodes partitioned.Interfaces

        let unionNodes = LayoutNodes.buildUnionNodes symbols partitioned.Unions
        let recordNodes = LayoutNodes.buildRecordNodes symbols partitioned.Records

        let classNodes = LayoutNodes.buildClassNodes symbols partitioned.Classes
        let enumNodes = LayoutNodes.buildEnumNodes partitioned.Enums
        let structEnumNodes = LayoutNodes.buildStructEnumNodes partitioned.StructEnums
        let closureNodes = LayoutNodes.buildClosureNodes closures

        // Every nominal type, in the by-kind order the `TypeDef` table has always used.
        // That order now applies *within* each holder (and among the roots) rather than
        // globally: filtering this list by holder preserves it.
        let nominalNodes =
            interfaceNodes
            @ unionNodes
            @ recordNodes
            @ classNodes
            @ enumNodes
            @ structEnumNodes

        // ---- Holder discovery ------------------------------------------------------
        //
        // A holder class is needed for every module that HOLDS something emitted, and
        // for every module on the way down to it — a `NestedClass` row needs its
        // enclosing `TypeDef` to exist. Three sources, and all three are necessary:
        //
        //   * the plan's holders — modules with static fns / module values;
        //   * every module named in an emitted TYPE's holder chain — a module that holds
        //     only types has no binding, so the plan never names it;
        //   * their ANCESTORS — a nested module `A.B` is a class nested in `A`'s holder,
        //     which must exist even when `A` itself holds nothing directly.
        //
        // Ancestors-first, first-appearance order, deduplicated: a parent is therefore
        // always discovered before its children.
        let orderedHolders =
            let seen = HashSet<ModuleKey>()
            let acc = ResizeArray<ModuleKey>()

            let rec add (m: ModuleKey) =
                match m.Holder with
                | ModuleHolder.InModule parent -> add parent
                | ModuleHolder.InNamespace _ -> ()

                if seen.Add m then
                    acc.Add m

            for h in plan.OrderedNamedHolders do
                add h

            for node in nominalNodes do
                match node.Enclosing with
                | ValueSome(TypeSlotKey.Holder m) -> add m
                | _ -> ()

            List.ofSeq acc

        let holderMethodRows (h: Emit.HolderKey) : MethodRow list =
            [
                // The `.cctor` initialises the holder's module values; it precedes the
                // fns exactly as `HolderPlan.MethodPlan` prepares them.
                if not (List.isEmpty (HolderPlan.holderValues plan h)) then
                    yield
                        {
                            Key = MethodKey.HolderCctor h
                            Name = ".cctor"
                            Attrs = cctorAttrs
                        }

                for fn in plan.StaticFns do
                    if fn.Holder = Some h then
                        yield
                            {
                                Key = MethodKey.StaticFn fn.SymbolKey
                                Name = fn.Name
                                Attrs = staticMethodAttrs
                            }
            ]

        // A holder node: its module-value fields (immutable ⇒ `initonly`, set only in
        // the holder `.cctor`), its methods, and — nested inside it — the types it
        // holds followed by its child holders.
        let rec holderNode (h: Emit.HolderKey) : TypeNode =
            let values = HolderPlan.holderValues plan h

            let fields =
                [
                    for mv in values ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                ]

            let held =
                nominalNodes
                |> List.filter (fun n -> n.Enclosing = ValueSome(TypeSlotKey.Holder h))

            let children =
                orderedHolders
                |> List.filter (fun m -> m.Holder = ModuleHolder.InModule h)
                |> List.map holderNode

            {
                Slot =
                    {
                        Key = TypeSlotKey.Holder h
                        Kind = TypeSlotKind.Holder(not (List.isEmpty values))
                        // A nested module's holder is a class nested in its parent's
                        // holder, so its namespace column is empty; a root module's
                        // carries the declaring namespace.
                        Namespace =
                            match h.Holder with
                            | ModuleHolder.InNamespace ns -> ns.Dotted
                            | ModuleHolder.InModule _ -> ""
                        MetaName = h.Name
                        Typars = []
                    }
                Enclosing =
                    match h.Holder with
                    | ModuleHolder.InModule parent -> ValueSome(TypeSlotKey.Holder parent)
                    | ModuleHolder.InNamespace _ -> ValueNone
                Fields = fields
                Methods = holderMethodRows h
                Nested = held @ children
            }

        let rootHolderNodes =
            orderedHolders
            |> List.filter (fun m ->
                match m.Holder with
                | ModuleHolder.InNamespace _ -> true
                | ModuleHolder.InModule _ -> false
            )
            |> List.map holderNode

        // The single `<Module>` pseudo-type and the single Program holder are minted by
        // `combine`, not here — they belong to the assembly, not a unit. This unit hands
        // over its placeable roots (namespace-level nominals, then closures, then root
        // holders — each carrying its own subtree) and the flat key set for the
        // completeness check.
        {
            Roots =
                (nominalNodes |> List.filter (fun n -> n.Enclosing.IsNone))
                @ closureNodes
                @ rootHolderNodes
            BuiltKeys =
                [
                    for n in nominalNodes -> n.Slot.Key
                    for n in closureNodes -> n.Slot.Key
                    for h in orderedHolders -> TypeSlotKey.Holder h
                ]
            Lowered = lowered
            Plan = plan
            Closures = closures
            ClosureByNode = closureByNode
            Partitioned = partitioned
            FunVerdicts = funVerdicts
            // The entry flag is the whole-assembly OutputKind decision, made by `combine`
            // (an executable's LAST file is the entry unit); a file cannot know it alone.
            EmitEntryPoint = false
        }

    /// Assemble the units into the whole `AssemblyLayout`: PREPEND the single `<Module>`
    /// pseudo-type (so it is TypeDef row 1 for the assembly), APPEND the single Program
    /// holder, flatten the concatenated roots into the `TypeDef` table, and run the
    /// completeness check ONCE over the combined set. The singular lowering products stay
    /// exposed for the Assembler; for a single unit they are that unit's.
    let combine (project: ProjectInfo) (units: UnitLayout list) : AssemblyLayout =
        // The entry unit carries the program entry point (`Main` + the anonymous "Program"
        // holder). For an executable it is the LAST file — F#'s rule that only the final
        // compilation unit may hold top-level expressions — and a library has none. Stamp
        // the flag onto exactly that unit (`buildUnit` left every unit FALSE, unaware of
        // the whole-assembly OutputKind decision) so `PrepareMain` fires once.
        let entryIndex =
            match project.OutputKind with
            | Exe -> List.length units - 1
            | Library -> -1

        let units =
            units
            |> List.mapi (fun i u ->
                { u with
                    EmitEntryPoint = (i = entryIndex)
                }
            )

        let entryUnit =
            match units |> List.tryFind (fun u -> u.EmitEntryPoint) with
            | Some u -> ValueSome u
            | None -> ValueNone

        // Only the entry file may carry top-level VALUE bindings — the anonymous "Program"
        // holder's static fields, written by its `.cctor` (leading prefix) or `Main`
        // (trailing). These come from a file's implicit-module top-level `let`s, which only
        // an executable's last file has; a non-entry unit with any is a front-end error. A
        // namespace-level `let` (a holder-less FN) is NOT top-level code — a library may
        // carry those on the Program holder — so it is aggregated below, not rejected here.
        units
        |> List.iteri (fun i u ->
            if not u.EmitEntryPoint then
                let p = u.Plan

                if
                    not (List.isEmpty p.ProgramCctorValues)
                    || not (List.isEmpty p.ProgramMainValues)
                then
                    failwithf
                        "Layout.combine: compilation unit %d of %d carries %d top-level value binding(s) but is not the entry file — only the last file of an executable may carry top-level code"
                        (i + 1)
                        (List.length units)
                        (List.length p.ProgramCctorValues + List.length p.ProgramMainValues)
        )

        // A holder `TypeSlotKey` contributed by two units is a same-FQN module split across
        // files — a front-end error the front end should already reject. Assert it here so a
        // duplicate holder TypeDef row can never reach `deriveHandles` (an opaque throw).
        let holderSeen = HashSet<TypeSlotKey>()

        for u in units do
            for k in u.BuiltKeys do
                match k with
                | TypeSlotKey.Holder _ ->
                    if not (holderSeen.Add k) then
                        failwithf
                            "Layout.combine: holder %A is contributed by more than one unit — a module's definition is split across files"
                            k
                | _ -> ()

        // The single `<Module>` pseudo-type is minted once here, not per unit, so it is
        // TypeDef row 1 for the whole assembly no matter how many units are combined.
        let moduleNode =
            {
                Slot =
                    {
                        Key = TypeSlotKey.ModulePseudo
                        Kind = TypeSlotKind.ModulePseudo
                        Namespace = ""
                        MetaName = "<Module>"
                        Typars = []
                    }
                Enclosing = ValueNone
                Fields = []
                Methods = []
                Nested = []
            }

        // The single Program holder, minted once here (not per unit). Its top-level value
        // FIELDS + `.cctor` + `Main` come from the ENTRY unit alone (only the last file of
        // an executable has top-level value bindings / `Main`; a library has neither):
        // leading-prefix values are `initonly` (written by the `.cctor`), values after a
        // top-level `do` are plain mutable `static` (written by `Main`). Its holder-less
        // static FNS aggregate across EVERY unit — a namespace-level `let` in any file lands
        // here — in unit order, each prepared by its owning unit's `MethodPlan`.
        //
        // `Main` belongs to this node's method list, which is what puts it inside the
        // Program type's `MethodList` range: the row and the range that claims it are
        // now the same list, so no ordering convention is left to preserve.
        let programFields =
            match entryUnit with
            | ValueNone -> []
            | ValueSome u ->
                let plan = u.Plan

                [
                    for mv in plan.ProgramCctorValues ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                    for mv in plan.ProgramMainValues ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = FieldAttributes.Public ||| FieldAttributes.Static
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                ]

        let hasProgramCctor =
            match entryUnit with
            | ValueSome u -> not (List.isEmpty u.Plan.ProgramCctorValues)
            | ValueNone -> false

        // Every unit's holder-less fns, in unit order, on the one Program holder.
        let holderlessFnRows =
            [
                for u in units do
                    for fn in u.Plan.HolderlessFns ->
                        {
                            Key = MethodKey.StaticFn fn.SymbolKey
                            Name = fn.Name
                            Attrs = staticMethodAttrs
                        }
            ]

        // The Program holder exists when there is any top-level code or namespace-level fn
        // to hold it: an entry point (`Main`), leading-prefix value fields, or any
        // holder-less fn across the units.
        let programNodes =
            if
                entryUnit.IsSome
                || not (List.isEmpty programFields)
                || not (List.isEmpty holderlessFnRows)
            then
                [
                    {
                        Slot =
                            {
                                Key = TypeSlotKey.Program
                                Kind = TypeSlotKind.Program hasProgramCctor
                                Namespace = ""
                                MetaName = project.ModuleName
                                Typars = []
                            }
                        Enclosing = ValueNone
                        Fields = programFields
                        Methods =
                            [
                                if hasProgramCctor then
                                    yield
                                        {
                                            Key = MethodKey.ProgramCctor
                                            Name = ".cctor"
                                            Attrs = cctorAttrs
                                        }

                                yield! holderlessFnRows

                                // `Main` is emitted iff there is an entry unit — only an
                                // executable has one, and it is what makes that unit the entry.
                                if entryUnit.IsSome then
                                    yield
                                        {
                                            Key = MethodKey.Main
                                            Name = "Main"
                                            Attrs = staticMethodAttrs
                                        }
                            ]
                        Nested = []
                    }
                ]
            else
                []

        // The roots, by kind: `<Module>` first (it must be TypeDef row 1), then the units'
        // namespace-level types / closures / root holders (each carrying its own subtree),
        // and the Program holder last.
        let roots = moduleNode :: (units |> List.collect (fun u -> u.Roots)) @ programNodes

        // The `TypeDef` table: the pre-order flattening. Every table the writer walks is
        // a projection of it, so a type's row range and the rows in that range cannot
        // disagree — there is no second enumeration to fall out of step.
        let rec flatten (n: TypeNode) : TypeNode list = n :: List.collect flatten n.Nested

        let types = List.collect flatten roots

        // The ONE invariant the derivation cannot make true by construction:
        // COMPLETENESS. Every node built above must be placed in the tree exactly once —
        // none dropped (a holder whose discovery missed it), none duplicated (a nominal
        // landing in both the roots and a module's `Nested`). Both are set questions, so
        // ask them as such — once, over the whole assembly.
        let builtKeys =
            [
                yield moduleNode.Slot.Key
                for u in units do
                    yield! u.BuiltKeys
                for n in programNodes -> n.Slot.Key
            ]

        let placedKeys = types |> List.map (fun n -> n.Slot.Key)

        if
            List.length placedKeys <> List.length builtKeys
            || not (HashSet(placedKeys).SetEquals(HashSet builtKeys))
        then
            failwithf
                "Layout: the type hierarchy places %d slots but %d were built — a slot is dropped, duplicated or invented"
                (List.length placedKeys)
                (List.length builtKeys)

        {
            Types = types
            Fields = types |> List.collect (fun n -> n.Fields)
            Methods = types |> List.collect (fun n -> n.Methods)
            // Assembly-level: does any unit carry the entry point (the PE serialises with an
            // entry point)?
            EmitEntryPoint = entryUnit.IsSome
            Units = units
        }

    /// Plan the whole assembly from every tast: build one unit per file and combine them.
    /// The ONE `ClosureNamer` is created here and threaded through every `buildUnit`, so
    /// closure TypeDef names stay unique assembly-wide across files. `combine` selects the
    /// entry unit, rejects top-level code outside it, and mints the shared `<Module>` /
    /// Program roots once. Single-unit output is byte-identical to the pre-split `build`.
    ///
    /// Unit order carries no meaning beyond that: a unit is planned from its own tast
    /// alone. In particular the `%A` `Format` row is reserved for every record / union
    /// regardless of where the `%A` interfaces are declared — they resolve local-or-external
    /// like any nominal (`ClrEnv.coreInterfaceEntity`), and IL imposes no declaration order
    /// within an assembly, so a record may precede the interface it implements.
    let buildMany (symbols: ICodegenSymbols) (project: ProjectInfo) (tasts: FrozenPools list) : AssemblyLayout =
        let closureNamer = Emit.ClosureNamer()
        let units = tasts |> List.map (buildUnit closureNamer symbols project)
        combine project units

    /// Plan the whole assembly from one tast — `buildMany` over a singleton unit list.
    let build (symbols: ICodegenSymbols) (project: ProjectInfo) (tast: FrozenPools) : AssemblyLayout =
        buildMany symbols project [ tast ]

    /// Derive every handle from the layout once: TypeDef handle = position in the
    /// pre-order flattening + 1; first-field / first-method handles by prefix-summing
    /// each node's OWN row lists — the very lists `AssemblyLayout.Fields` / `.Methods`
    /// are collected from, so the prediction and the rows are the same data (an empty
    /// range naturally points past the end of the previous owner's range).
    let deriveHandles (layout: AssemblyLayout) : LayoutHandles =
        let typeDefs = Dictionary<TypeSlotKey, TypeDefinitionHandle>()
        let firstFields = Dictionary<TypeSlotKey, FieldDefinitionHandle>()
        let firstMethods = Dictionary<TypeSlotKey, MethodDefinitionHandle>()
        let methodDefs = Dictionary<MethodKey, MethodDefinitionHandle>()
        let mutable fieldCursor = 0
        let mutable methodCursor = 0

        layout.Types
        |> List.iteri (fun i node ->
            typeDefs.Add(node.Slot.Key, MetadataTokens.TypeDefinitionHandle(i + 1))
            firstFields.Add(node.Slot.Key, MetadataTokens.FieldDefinitionHandle(fieldCursor + 1))
            firstMethods.Add(node.Slot.Key, MetadataTokens.MethodDefinitionHandle(methodCursor + 1))
            fieldCursor <- fieldCursor + List.length node.Fields
            methodCursor <- methodCursor + List.length node.Methods
        )

        layout.Methods
        |> List.iteri (fun i row -> methodDefs.Add(row.Key, MetadataTokens.MethodDefinitionHandle(i + 1)))

        {
            TypeDefs = typeDefs
            FirstFields = firstFields
            FirstMethods = firstMethods
            MethodDefs = methodDefs
            TotalFields = fieldCursor
            TotalMethods = methodCursor
        }
