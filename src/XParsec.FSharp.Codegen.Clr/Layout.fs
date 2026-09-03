namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common

module internal Layout =

    /// A node's own slot key followed by every key nested beneath it, pre-order.
    let rec private flattenKeys (n: TypeNode) : TypeSlotKey list =
        n.Slot.Key :: List.collect flattenKeys n.Nested

    /// Build ONE file's contribution to the type HIERARCHY: its namespace-level nominals,
    /// then closures, then root-module classes, each carrying the types it holds and its
    /// child module classes. The shared `ClosureNamer` keeps closure names unique.
    let buildFile
        (closureNamer: Emit.ClosureNamer)
        (symbols: ICodegenSymbols)
        (project: ProjectInfo)
        (pools: FrozenPools)
        : FileLayout =
        // An append-only overlay over the file's frozen trees: every node this emission
        // derives is appended, and every id the frozen pool handed out keeps denoting the
        // same node, so derived nodes can be minted mid-emit rather than in one batch.
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
        // both number from 0, so a bare id would not miss across files but would
        // silently denote a different node.
        let funVerdicts =
            let frozen =
                pools.FunVerdicts
                |> Array.map (fun (id, v) -> ({ Pool = pool; Id = id }: TastAccessor.ExprId), v)
                |> DenseTable.index

            let d = Dictionary<TastAccessor.ExprId, FunVerdict>(frozen)

            // Expansion re-authors every lambda with an inline call beneath it, so each
            // derived node inherits the verdict filed against the frozen node it came
            // from; otherwise a value-struct closure emits as an ordinary heap one.
            for (node, v) in InlineExpand.Derivation.resolveAll expansion.Derived frozen do
                d.[node] <- v

            d :> IReadOnlyDictionary<_, _>

        let lowered0 = Emit.lower decls
        // The anonymous "Program" class's key: a module of that name in the global
        // namespace, owning the fns of no named module + `Main` + the top-level values. It
        // tags those values' `ModuleClass` field; the type slot is `TypeSlotKey.Program`.
        let programClass =
            SymbolKeyOps.moduleKeyOf (ModuleContainer.InNamespace NamespaceKey.Global) project.ModuleName

        // Every `[<Struct; IsByRefLike>]` type, because a top-level value of such a type
        // can't be a static field; computed from the pool's own decl roots since
        // `Emit.lower` strips the type decls `lowered` would carry.
        let refStructKeys =
            [
                for d in decls do
                    match TastAccessor.declKind d with
                    | DeclShape.Type ->
                        let td = TastAccessor.declType d

                        match td.Kind with
                        | TTypeKindG.Class c when c.ValueKind = ClassValueKind.RefStruct -> td.TypeKey
                        | _ -> ()
                    | _ -> ()
            ]
            |> HashSet

        // The plan eta-expands every non-saturated reference to a static-eligible module
        // function, keeping the flat static method and adding a wrapper closure. Closure
        // discovery must walk the rewritten decls it republishes.
        let plan =
            ModuleClassPlan.create moduleMembers genericFnSchemes programClass refStructKeys lowered0

        let lowered = plan.Lowered

        // Member bodies never pass through lowering because it drops every `type` decl.
        // Partitioned once and published, so closure discovery and body emission walk the
        // same node IDS, which the `…ByNode` tables key on.
        let partitioned = LayoutNodes.partitionTypeDecls symbols decls

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

        let unionNodes = UnionLayoutNodes.buildUnionNodes symbols partitioned.Unions
        let recordNodes = LayoutNodes.buildRecordNodes symbols partitioned.Records

        let classNodes = LayoutNodes.buildClassNodes symbols partitioned.Classes
        let enumNodes = LayoutNodes.buildEnumNodes partitioned.Enums
        let structEnumNodes = LayoutNodes.buildStructEnumNodes partitioned.StructEnums
        let closureNodes = LayoutNodes.buildClosureNodes closures

        // Every nominal type, by kind. The order applies *within* each module class (and among
        // the roots), since filtering this list by module class preserves it.
        let nominalNodes =
            interfaceNodes
            @ unionNodes
            @ recordNodes
            @ classNodes
            @ enumNodes
            @ structEnumNodes

        // ---- Module class discovery -------------------------------------------------

        // Every module that holds an emitted binding or type needs a class, and so does
        // every ancestor on the way down to it, because a `NestedClass` row needs its
        // enclosing `TypeDef`. Ancestors first, first-appearance order, deduplicated.
        let orderedClasses =
            let seen = HashSet<ModuleKey>()
            let acc = ResizeArray<ModuleKey>()

            let rec add (m: ModuleKey) =
                match m.Container with
                | ModuleContainer.InModule parent -> add parent
                | ModuleContainer.InNamespace _ -> ()

                if seen.Add m then
                    acc.Add m

            for h in plan.OrderedNamedClasses do
                add h

            for node in nominalNodes do
                match node.Enclosing with
                | ValueSome(TypeSlotKey.ModuleClass m) -> add m
                | _ -> ()

            List.ofSeq acc

        let moduleClassMethodRows (h: Emit.ModuleClassKey) : MethodRow list =
            [
                // The `.cctor` initialises the module class's module values.
                if not (List.isEmpty (ModuleClassPlan.moduleClassValues plan h)) then
                    yield
                        {
                            Key = MethodKey.ModuleClassCctor h
                            Name = ".cctor"
                            Attrs = cctorAttrs
                        }

                for fn in plan.StaticFns do
                    if fn.ModuleClass = Some h then
                        yield
                            {
                                Key = MethodKey.StaticFn fn.SymbolKey
                                Name = fn.Name
                                Attrs = staticMethodAttrs
                            }
            ]

        // The class name a module emits as: the compiled name the residue declares for it,
        // else its source name.
        let moduleClassName (h: Emit.ModuleClassKey) : string =
            let compiled =
                EqDict.tryFind h pools.Residue.Modules
                |> ValueOption.bind (fun facts -> facts.CompiledName)

            CompiledName.Emitted(compiled, h.Name)

        // A module class node: its module-value fields (immutable ⇒ `initonly`, set only in
        // the module class `.cctor`), its methods, and the types it holds followed by its
        // child module classes.
        let rec moduleClassNode (h: Emit.ModuleClassKey) : TypeNode =
            let values = ModuleClassPlan.moduleClassValues plan h

            let fields =
                [
                    for mv in values ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = staticFieldAttrs FieldReach.Public FieldWrites.ByCtor
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                ]

            let held =
                nominalNodes
                |> List.filter (fun n -> n.Enclosing = ValueSome(TypeSlotKey.ModuleClass h))

            let children =
                orderedClasses
                |> List.filter (fun m -> m.Container = ModuleContainer.InModule h)
                |> List.map moduleClassNode

            {
                Slot =
                    {
                        Key = TypeSlotKey.ModuleClass h
                        Kind = TypeSlotKind.ModuleClass(not (List.isEmpty values))
                        // A nested module's class is nested in its parent's, so its
                        // namespace column is empty; a root module's carries the
                        // declaring namespace.
                        Namespace =
                            match h.Container with
                            | ModuleContainer.InNamespace ns -> ns.Dotted
                            | ModuleContainer.InModule _ -> ""
                        MetaName = moduleClassName h
                        Typars = []
                    }
                Enclosing =
                    match h.Container with
                    | ModuleContainer.InModule parent -> ValueSome(TypeSlotKey.ModuleClass parent)
                    | ModuleContainer.InNamespace _ -> ValueNone
                Fields = fields
                Methods = moduleClassMethodRows h
                Properties = []
                Nested = held @ children
            }

        let rootModuleClassNodes =
            orderedClasses
            |> List.filter (fun m ->
                match m.Container with
                | ModuleContainer.InNamespace _ -> true
                | ModuleContainer.InModule _ -> false
            )
            |> List.map moduleClassNode

        // The `<Module>` pseudo-type and the Program class belong to the ASSEMBLY, so
        // they are minted around the combined files, not here. This hands over the
        // placeable roots and the flat key set for the completeness check.
        {
            Roots =
                (nominalNodes |> List.filter (fun n -> n.Enclosing.IsNone))
                @ closureNodes
                @ rootModuleClassNodes
            // A nominal carries its own nested subtree (a union's case types), so the keys
            // the completeness check compares against are the FLATTENING, not the roots.
            BuiltKeys =
                [
                    for n in nominalNodes do
                        yield! flattenKeys n
                    for n in closureNodes -> n.Slot.Key
                    for h in orderedClasses -> TypeSlotKey.ModuleClass h
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
    /// pseudo-type (so it is TypeDef row 1), APPEND the Program class, flatten the
    /// concatenated roots into the `TypeDef` table, then check completeness once.
    let combine (project: ProjectInfo) (files: FileLayout list) : AssemblyLayout =
        // The entry file carries `Main`. Only the final file of an executable may hold
        // top-level expressions, so that file is the entry one; a library has no entry
        // file at all.
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
        // is a front-end error. A namespace-level `let` (a Program-class FN) is not top-level
        // code and a library may carry one, so it is aggregated below, not rejected here.
        files
        |> List.iteri (fun i f ->
            if not f.EmitEntryPoint then
                let p = f.Plan

                if
                    not (List.isEmpty p.ProgramCctorValues)
                    || not (List.isEmpty p.ProgramMainValues)
                then
                    failwithf
                        "Layout.combine: file %d of %d carries %d top-level value binding(s) but is not the entry file, and only the last file of an executable may carry top-level code"
                        (i + 1)
                        (List.length files)
                        (List.length p.ProgramCctorValues + List.length p.ProgramMainValues)
        )

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
                Properties = []
                Nested = []
            }

        // The Program class's value fields come from the ENTRY file alone: a value before
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
                            Attrs = staticFieldAttrs FieldReach.Public FieldWrites.ByCtor
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                    for mv in plan.ProgramMainValues ->
                        {
                            Key = FieldKey.ModuleValue mv.SymbolKey
                            Name = mv.Name
                            Attrs = staticFieldAttrs FieldReach.Public FieldWrites.Anywhere
                            Ty = mv.Ty
                            ClosureScope = ValueNone
                        }
                ]

        let hasProgramCctor =
            match entryFile with
            | ValueSome f -> not (List.isEmpty f.Plan.ProgramCctorValues)
            | ValueNone -> false

        // Every file's Program-class fns, in file order, on the one Program class. Two files
        // declaring the same namespace can both declare `let f`, but F# tells them apart by
        // an implicit module named after each FILE, which this front end cannot mint.
        let programFnRows =
            let seen = HashSet<SymbolKey>()

            [
                for f in files do
                    for fn in f.Plan.ProgramFns do
                        if not (seen.Add fn.SymbolKey) then
                            failwithf
                                "Layout.combine: top-level binding %s is declared by more than one file, and two files declaring the same namespace cannot both hold a binding of that name (F# would distinguish them by an implicit module named after each file)"
                                (SymbolKeyOps.qualifiedName fn.SymbolKey)

                        yield
                            {
                                Key = MethodKey.StaticFn fn.SymbolKey
                                Name = fn.Name
                                Attrs = staticMethodAttrs
                            }
            ]

        // The Program class exists only if it would hold something: `Main`, a top-level
        // value field, or a Program-class fn from any file.
        let programNodes =
            if
                entryFile.IsSome
                || not (List.isEmpty programFields)
                || not (List.isEmpty programFnRows)
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

                                yield! programFnRows

                                // Only an executable has an entry file, and only it has `Main`.
                                if entryFile.IsSome then
                                    yield
                                        {
                                            Key = MethodKey.Main
                                            Name = "Main"
                                            Attrs = staticMethodAttrs
                                        }
                            ]
                        Properties = []
                        Nested = []
                    }
                ]
            else
                []

        // `<Module>` must be TypeDef row 1, so it leads; the Program class goes last.
        let roots = moduleNode :: (files |> List.collect (fun f -> f.Roots)) @ programNodes

        // The `TypeDef` table is this pre-order flattening, and every table the writer
        // walks is a projection of it, so a type's row range and the rows in that range
        // cannot disagree.
        let rec flatten (n: TypeNode) : TypeNode list = n :: List.collect flatten n.Nested

        let types = List.collect flatten roots

        // Completeness: every node built above must be placed in the tree exactly once, so
        // none dropped (a module-class discovery miss), none duplicated (a nominal landing in
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
                "Layout: the type hierarchy places %d slots but %d were built, so a slot is dropped, duplicated or invented"
                (List.length placedKeys)
                (List.length builtKeys)

        // Two `Field` rows of one name on one type are valid metadata only where their
        // signatures differ; a same-named pair is rejected here, for records, classes,
        // closures and unions alike.
        for node in types do
            let seen = HashSet<string>()

            for f in node.Fields do
                if not (seen.Add f.Name) then
                    failwithf "Layout: type '%s' declares two fields named '%s'" node.Slot.MetaName f.Name

        {
            Types = types
            Fields = types |> List.collect (fun n -> n.Fields)
            Methods = types |> List.collect (fun n -> n.Methods)
            Properties = types |> List.collect (fun n -> n.Properties)
            // Whether the PE serialises with an entry point.
            EmitEntryPoint = entryFile.IsSome
            Files = files
        }

    /// Plan the whole assembly: one layout per file, combined. Beyond selecting the entry
    /// file, file ORDER carries no meaning, because a file is planned from its own tast
    /// alone and IL imposes no declaration order: a record may precede the interface it
    /// implements.
    let buildMany (symbols: ICodegenSymbols) (project: ProjectInfo) (tasts: FrozenPools list) : AssemblyLayout =
        let closureNamer = Emit.ClosureNamer()
        let files = tasts |> List.map (buildFile closureNamer symbols project)
        combine project files

    /// Plan the whole assembly from one tast: `buildMany` over a singleton file list.
    let build (symbols: ICodegenSymbols) (project: ProjectInfo) (tast: FrozenPools) : AssemblyLayout =
        buildMany symbols project [ tast ]

    /// Derive every handle from the layout once: a TypeDef handle is its position in the
    /// pre-order flattening + 1; first-field / first-method handles prefix-sum each node's
    /// own row lists, the very lists `AssemblyLayout.Fields` / `.Methods` collect.
    let deriveHandles (layout: AssemblyLayout) : LayoutHandles =
        let typeDefs = Dictionary<TypeSlotKey, TypeDefinitionHandle>()
        let firstFields = Dictionary<TypeSlotKey, FieldDefinitionHandle>()
        let firstMethods = Dictionary<TypeSlotKey, MethodDefinitionHandle>()
        let firstProperties = Dictionary<TypeSlotKey, PropertyDefinitionHandle>()
        let methodDefs = Dictionary<MethodKey, MethodDefinitionHandle>()
        let propertyDefs = Dictionary<PropertyKey, PropertyDefinitionHandle>()
        let mutable fieldCursor = 0
        let mutable methodCursor = 0
        let mutable propertyCursor = 0

        layout.Types
        |> List.iteri (fun i node ->
            typeDefs.Add(node.Slot.Key, MetadataTokens.TypeDefinitionHandle(i + 1))
            firstFields.Add(node.Slot.Key, MetadataTokens.FieldDefinitionHandle(fieldCursor + 1))
            firstMethods.Add(node.Slot.Key, MetadataTokens.MethodDefinitionHandle(methodCursor + 1))
            firstProperties.Add(node.Slot.Key, MetadataTokens.PropertyDefinitionHandle(propertyCursor + 1))
            fieldCursor <- fieldCursor + List.length node.Fields
            methodCursor <- methodCursor + List.length node.Methods
            propertyCursor <- propertyCursor + List.length node.Properties
        )

        layout.Methods
        |> List.iteri (fun i row -> methodDefs.Add(row.Key, MetadataTokens.MethodDefinitionHandle(i + 1)))

        layout.Properties
        |> List.iteri (fun i row -> propertyDefs.Add(row.Key, MetadataTokens.PropertyDefinitionHandle(i + 1)))

        {
            TypeDefs = typeDefs
            FirstFields = firstFields
            FirstMethods = firstMethods
            FirstProperties = firstProperties
            MethodDefs = methodDefs
            PropertyDefs = propertyDefs
            TotalFields = fieldCursor
            TotalMethods = methodCursor
            TotalProperties = propertyCursor
        }
