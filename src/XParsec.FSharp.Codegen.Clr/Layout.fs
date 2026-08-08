namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

module internal Layout =

    /// Build ONE file's contribution to the type HIERARCHY: its namespace-level nominals,
    /// then closures, then root-module holders — each holder carrying the types it holds
    /// and its child holders. The shared `ClosureNamer` keeps closure names unique.
    let buildFile
        (closureNamer: Emit.ClosureNamer)
        (symbols: ICodegenSymbols)
        (project: ProjectInfo)
        (pools: FrozenPools)
        : FileLayout =
        // An append-only overlay over the file's frozen trees: every node this emission
        // derives is appended, and every id the frozen pool handed out keeps naming the
        // same node — so derived nodes can be minted mid-emit rather than in one batch.
        let pool = TastPoolBuilder.openOver pools

        // Inline expansion runs before any node-keyed table is built off the decls, and
        // over the DECLARATIONS rather than the lowered list: lowering drops `type` decls,
        // so an inline call inside a member body would otherwise be invisible.
        let expansion = InlineExpand.expand pool (TastAccessor.roots pool |> List.ofArray)
        let decls = expansion.Decls

        // The bound-variable-keyed side tables this lowering consumes, at the dense id the columns
        // already address a bound variable by.
        let moduleMembers = Map.ofArray pools.ModuleMembers
        let closureReprs = Map.ofArray pools.ClosureReprs
        let genericFnSchemes = Map.ofArray pools.GenericFnSchemes

        // A source lambda's verdict, keyed by NODE (id + issuing pool): two files' pools
        // both number from 0, so a bare id would not miss across files — it would
        // silently name a different node.
        let funVerdicts =
            let frozen =
                pools.FunVerdicts
                |> Array.map (fun (id, v) -> ({ Pool = pool; Id = id }: TastAccessor.ExprId), v)
                |> DenseTable.index

            let d = Dictionary<TastAccessor.ExprId, FunVerdict>(frozen)

            // Expansion re-authors every lambda with an inline call beneath it, so each
            // derived node inherits the verdict filed against the frozen node it came
            // from — without this a value-struct closure emits as an ordinary heap one.
            for (node, v) in InlineExpand.Derivation.resolveAll expansion.Derived frozen do
                d.[node] <- v

            d :> IReadOnlyDictionary<_, _>

        let lowered0 = Emit.lower decls
        // The anonymous "Program" holder's key — a module of that name in the global
        // namespace, owning the holder-less fns + `Main` + the top-level values. It tags
        // those values' `Holder` field; the type slot is `TypeSlotKey.Program`.
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

        // The plan eta-expands every non-saturated reference to a static-eligible module
        // function — keeping the flat static method, adding a wrapper closure — and
        // republishes the rewritten decls, which is what closure discovery must walk.
        let plan =
            HolderPlan.create moduleMembers genericFnSchemes programHolder refStructNsNames lowered0

        let lowered = plan.Lowered

        // Member bodies never pass through lowering — they arrive from the freeze ready to
        // emit. Partitioned once and published, so closure discovery and body emission
        // walk the same node IDS, which the `…ByNode` tables key on.
        let partitioned = LayoutNodes.partitionTypeDecls decls

        // Closure-discovery roots: every member body AND every class-preamble expression.
        // A preamble initialiser is emitted into the `.ctor` / `.cctor` from these very
        // nodes, so a lambda in one is a closure exactly as a member body's is.
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
                // Which source lambdas are value-structs, by node membership.
                funVerdicts
                closureReprs
                lowered
                memberRoots

        // Each type's node carries its own field and method rows, so the rows the writer
        // walks ARE the rows its range claims: both are `List.collect`s over the same
        // flattening.
        let interfaceNodes = LayoutNodes.buildInterfaceNodes partitioned.Interfaces

        let unionNodes = LayoutNodes.buildUnionNodes symbols partitioned.Unions
        let recordNodes = LayoutNodes.buildRecordNodes symbols partitioned.Records

        let classNodes = LayoutNodes.buildClassNodes symbols partitioned.Classes
        let enumNodes = LayoutNodes.buildEnumNodes partitioned.Enums
        let structEnumNodes = LayoutNodes.buildStructEnumNodes partitioned.StructEnums
        let closureNodes = LayoutNodes.buildClosureNodes closures

        // Every nominal type, by kind. The order applies *within* each holder (and among
        // the roots), since filtering this list by holder preserves it.
        let nominalNodes =
            interfaceNodes
            @ unionNodes
            @ recordNodes
            @ classNodes
            @ enumNodes
            @ structEnumNodes

        // ---- Holder discovery ------------------------------------------------------

        // Every module that holds an emitted binding or type needs a holder class, and so
        // does every ancestor on the way down to it — a `NestedClass` row needs its
        // enclosing `TypeDef`. Ancestors first, first-appearance order, deduplicated.
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
                // The `.cctor` initialises the holder's module values.
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

        // The `<Module>` pseudo-type and the Program holder belong to the ASSEMBLY, so
        // they are minted around the combined files, not here. This hands over the
        // placeable roots and the flat key set for the completeness check.
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
            // Which file is the entry file is a whole-assembly decision; stamped below.
            EmitEntryPoint = false
        }

    /// Assemble the files into the whole `AssemblyLayout`: PREPEND the `<Module>`
    /// pseudo-type (so it is TypeDef row 1), APPEND the Program holder, flatten the
    /// concatenated roots into the `TypeDef` table, then check completeness once.
    let combine (project: ProjectInfo) (files: FileLayout list) : AssemblyLayout =
        // The entry file carries `Main`. For an executable it is the LAST file — F#'s rule
        // that only the final file may hold top-level expressions — and a library has
        // none.
        let entryIndex =
            match project.OutputKind with
            | Exe -> List.length files - 1
            | Library -> -1

        let files =
            files
            |> List.mapi (fun i f ->
                { f with
                    EmitEntryPoint = (i = entryIndex)
                }
            )

        let entryFile =
            match files |> List.tryFind (fun f -> f.EmitEntryPoint) with
            | Some f -> ValueSome f
            | None -> ValueNone

        // Only the entry file may carry top-level VALUE bindings; a non-entry file with any
        // is a front-end error. A namespace-level `let` (a holder-less FN) is not top-level
        // code — a library may carry those — so it is aggregated below, not rejected here.
        files
        |> List.iteri (fun i f ->
            if not f.EmitEntryPoint then
                let p = f.Plan

                if
                    not (List.isEmpty p.ProgramCctorValues)
                    || not (List.isEmpty p.ProgramMainValues)
                then
                    failwithf
                        "Layout.combine: file %d of %d carries %d top-level value binding(s) but is not the entry file — only the last file of an executable may carry top-level code"
                        (i + 1)
                        (List.length files)
                        (List.length p.ProgramCctorValues + List.length p.ProgramMainValues)
        )

        // A holder contributed by two files is a same-FQN module split across files.
        // Rejected by name here rather than as an opaque duplicate-key throw when the
        // handles are derived.
        let holderSeen = HashSet<TypeSlotKey>()

        for f in files do
            for k in f.BuiltKeys do
                match k with
                | TypeSlotKey.Holder _ ->
                    if not (holderSeen.Add k) then
                        failwithf
                            "Layout.combine: holder %A is contributed by more than one file — a module's definition is split across files"
                            k
                | _ -> ()

        // Minted here, not per file, so it is TypeDef row 1 for the whole assembly no
        // matter how many files are combined.
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

        // The Program holder's value fields come from the ENTRY file alone: a value before
        // the first top-level `do` is `initonly`, written by the `.cctor`; one after it is
        // plain mutable `static`, written by `Main`.
        let programFields =
            match entryFile with
            | ValueNone -> []
            | ValueSome f ->
                let plan = f.Plan

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
            match entryFile with
            | ValueSome f -> not (List.isEmpty f.Plan.ProgramCctorValues)
            | ValueNone -> false

        // Every file's holder-less fns, in file order, on the one Program holder. Two files
        // declaring the same namespace can both declare `let f` — F# tells them apart by an
        // implicit module named after each FILE, which this front end cannot mint.
        let holderlessFnRows =
            let seen = HashSet<SymbolKey>()

            [
                for f in files do
                    for fn in f.Plan.HolderlessFns do
                        if not (seen.Add fn.SymbolKey) then
                            failwithf
                                "Layout.combine: top-level binding %s is declared by more than one file — two files declaring the same namespace cannot both hold a binding of that name (F# would distinguish them by an implicit module named after each file)"
                                (SymbolKeyOps.qualifiedName fn.SymbolKey)

                        yield
                            {
                                Key = MethodKey.StaticFn fn.SymbolKey
                                Name = fn.Name
                                Attrs = staticMethodAttrs
                            }
            ]

        // The Program holder exists only if it would hold something: `Main`, a top-level
        // value field, or a holder-less fn from any file.
        let programNodes =
            if
                entryFile.IsSome
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

                                // Only an executable has an entry file, and only it has `Main`.
                                if entryFile.IsSome then
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

        // `<Module>` first — it must be TypeDef row 1 — and the Program holder last.
        let roots = moduleNode :: (files |> List.collect (fun f -> f.Roots)) @ programNodes

        // The `TypeDef` table is this pre-order flattening, and every table the writer
        // walks is a projection of it — so a type's row range and the rows in that range
        // cannot disagree.
        let rec flatten (n: TypeNode) : TypeNode list = n :: List.collect flatten n.Nested

        let types = List.collect flatten roots

        // Completeness: every node built above must be placed in the tree exactly once —
        // none dropped (a holder discovery missed), none duplicated (a nominal landing in
        // both the roots and a module's `Nested`).
        let builtKeys =
            [
                yield moduleNode.Slot.Key
                for f in files do
                    yield! f.BuiltKeys
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
            // Whether the PE serialises with an entry point.
            EmitEntryPoint = entryFile.IsSome
            Files = files
        }

    /// Plan the whole assembly: one layout per file, combined. Beyond selecting the entry
    /// file, file ORDER carries no meaning — a file is planned from its own tast alone, and
    /// IL imposes no declaration order, so a record may precede the interface it implements.
    let buildMany (symbols: ICodegenSymbols) (project: ProjectInfo) (tasts: FrozenPools list) : AssemblyLayout =
        let closureNamer = Emit.ClosureNamer()
        let files = tasts |> List.map (buildFile closureNamer symbols project)
        combine project files

    /// Plan the whole assembly from one tast — `buildMany` over a singleton file list.
    let build (symbols: ICodegenSymbols) (project: ProjectInfo) (tast: FrozenPools) : AssemblyLayout =
        buildMany symbols project [ tast ]

    /// Derive every handle from the layout once: a TypeDef handle is its position in the
    /// pre-order flattening + 1; first-field / first-method handles prefix-sum each node's
    /// own row lists, the very lists `AssemblyLayout.Fields` / `.Methods` collect.
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
