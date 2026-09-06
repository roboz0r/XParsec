namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open AssemblerScaffold

/// The part of a file's emission state built BEFORE the shared field pass: the
/// value-struct closure mint and the closure-verdict rewrite both feed a field's
/// signature, so neither can wait for it.
type private FilePrelude =
    {
        Layout: FileLayout
        CtorHandleByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
        CachedClosureFieldByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
        ClosureValueTypeByNode: Dictionary<TastAccessor.ExprId, FrozenType>
        ClosureTypeDefByNode: Dictionary<TastAccessor.ExprId, EntityHandle>
        Verdict: ClosureVerdictRewrite.Rewrite
    }

/// The per-file emission state the Bind / Prepare passes consume: a fresh `EmitContext`
/// (its bound-variable- and reference-keyed tables are file-local; its nominal registries and
/// row space are the Assembler's) plus the two things `EmitContext` does not carry.
type internal FileEmit =
    {
        Layout: FileLayout
        Verdict: ClosureVerdictRewrite.Rewrite
        EmitCtx: Emit.EmitContext
    }

/// The assembler over the `AssemblyLayout`. The layout enumerates every ranged-table row
/// as data (handle = position), so the *Prepare* phase can build every signature and body
/// against resolved handles, in any order, and the writers then walk the layout in order.
type internal Assembler
    (symbols: ICodegenSymbols, project: ProjectInfo, tasts: FrozenPools list, referenceAssemblies: string list) =

    let ctx = MetadataContext()
    do ctx.AddModuleAndAssembly(project.AssemblyName)

    // Identities read off the reference files, so an emitted `AssemblyRef` identifies the exact
    // artifact rather than whatever the host loaded. `referenceAssemblies` is separate from
    // `project.References` (which ships beside the output); the latter wins a name tie.
    let references =
        referenceAssemblies @ project.References
        |> List.map (fun path ->
            let an = System.Reflection.AssemblyName.GetAssemblyName path
            an.Name, an
        )
        |> Map.ofList

    // Union over every file: a `SymbolKey` identifies an intrinsic assembly-wide, so a key
    // repeated across files is the same declaration and last-wins is safe. Platform type id
    // only, because `extends` comes off the frozen base type.
    let intrinsicBindings =
        let d = Dictionary<TypeKey, PlatformTypeId>()

        for tast in tasts do
            for kv in tast.Residue.IntrinsicBindings do
                d.[kv.Key] <- kv.Value.TypeId

        d

    let provider =
        // Own-compilation intrinsics only; every other primitive's repr is read through the
        // provider, out of the dependency closure's `.fs`.
        ClrProvider(ctx, intrinsicBindings, references, symbols)

    let icodegen = provider :> ICodegenProvider
    let encodeLocals (locals: FrozenType list) = icodegen.EncodeLocalSignature locals

    // The narrow emission-side view of the provider: type/member shapes only.
    let codegenSymbols = symbols

    // One body-stream encoder for every method, because `AddMethodBody` realigns per body
    // internally and a fresh encoder per body would leave a tiny body's builder unaligned.
    let bodyStream = ctx.BodyStream

    /// Lower an IL body and stage it in the body stream, yielding the offset the
    /// `MethodDef` row points at.
    let methodBody ir : PreparedBody =
        PreparedBody.At(Cil.buildBody encodeLocals bodyStream (IlIr.lower ir))

    let layout = Layout.buildMany codegenSymbols project tasts
    let layoutHandles = Layout.deriveHandles layout

    // closure name → its `Closure`, so the type-row walk (which has only a
    // `TypeSlotKey.Closure name`) can branch a value-struct closure onto struct attrs.
    // Shared across files: closure names are unique assembly-wide.
    let closureByName = Dictionary<string, Emit.Closure>()

    let closureIsValueStruct (name: string) : bool =
        match closureByName.TryGetValue name with
        | true, c -> c.IsValueStruct
        | false, _ -> false

    // The whole field table is written up front, straight off the layout; every later
    // phase resolves def handles by `FieldKey` instead of adding rows. It spans every
    // file, so one file's body resolves a sibling file's field row through it.
    let fieldDefHandles = Dictionary<FieldKey, FieldDefinitionHandle>()

    // A numeric enum case field's `Constant` value: the case's underlying integer, boxed
    // to the authored CLR primitive. The field pass attaches it as it writes the field.
    let enumFieldConstants = Dictionary<FieldKey, obj>()

    // Shared across files, keyed by nominal `SymbolKey`: a call in one file's body
    // resolves a type or member defined in another through these.
    let unions = Dictionary<TypeKey, Emit.EmittedUnion>()
    let records = Dictionary<TypeKey, Emit.EmittedRecord>()
    let classes = Dictionary<TypeKey, Emit.EmittedClass>()
    let enums = Dictionary<TypeKey, Emit.EmittedEnum>()
    let interfaces = Dictionary<TypeKey, Emit.EmittedInterface>()

    // A module value's verdict-rewritten field-slot type, keyed by the same `SymbolKey`
    // `FieldKey.ModuleValue` carries. This is the one per-file datum the shared field pass
    // needs, as a lookup, so the pass itself stays a plain walk of `layout.Fields`.
    let moduleValueSlotType = Dictionary<SymbolKey, FrozenType>()

    // Per file, BEFORE the shared field pass: register this file's nominals with the
    // provider, mint its value-struct closure types, and build its closure-verdict
    // rewrite. All three feed a field's signature, so they must precede the pass.
    let buildPrelude (file: FileLayout) : FilePrelude =
        let partitioned = file.Partitioned
        let closures = file.Closures
        let plan = file.Plan

        for c in closures do
            closureByName.[c.Name] <- c

        NominalRegistration.apply provider layoutHandles file

        let ctorHandleByNode = Dictionary<TastAccessor.ExprId, EntityHandle>()

        let cachedClosureFieldByNode = Dictionary<TastAccessor.ExprId, EntityHandle>()

        // A captureless value-struct closure's synthetic encodable `FrozenType` and its
        // `TypeDef` handle (the `initobj` operand). Minted NOW: the module-value field
        // substitution reads the type while the up-front field pass encodes stored slots.
        let closureValueTypeByNode = Dictionary<TastAccessor.ExprId, FrozenType>()

        let closureTypeDefByNode = Dictionary<TastAccessor.ExprId, EntityHandle>()

        for c in closures do
            if c.IsValueStruct then
                let defHandle = toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure c.Name))
                let ft = provider.RegisterStackClosureValueType(c.Name, defHandle)
                closureValueTypeByNode.[c.Node] <- ft
                closureTypeDefByNode.[c.Node] <- defHandle

        // A numeric enum's case `Constant` values + its registry entry, both needed
        // before the field pass attaches the `Constant` rows.
        for ed in partitioned.Enums do
            for (caseName, v) in ed.Cases do
                let k, bits = TEnumCases.integralValue v
                enumFieldConstants.[FieldKey.EnumCaseField(ed.Decl.Key, caseName)] <- IntKind.boxed k bits

            let caseValues = Dictionary<string, TConstValue>()

            for (caseName, v) in ed.Cases do
                caseValues.[caseName] <- v

            enums.[ed.Decl.TypeKey] <-
                {
                    Repr = Emit.EmittedEnumRepr.NumericEnum(ed.Underlying, caseValues)
                }

        // A project-local seq class's `GetEnumerator` RETURN type is the enumerator over
        // the class's declaring typars; keep that template per class key. Read off THIS
        // file's decls because `env.Classes` is not yet populated at the field pass.
        let enumeratorTemplateByClass =
            let d = Dictionary<SymbolKey, FrozenType>()

            for cd in partitioned.Classes do
                let template =
                    cd.Interfaces
                    |> List.tryPick (fun (_, members) ->
                        members
                        |> List.tryPick (fun (m: TastAccessor.TypeMember) ->
                            if m.Name = "GetEnumerator" then Some m.ReturnTy else None
                        )
                    )

                match template with
                | Some t -> d.[cd.Decl.Key] <- t
                | None -> ()

            d

        // Instantiate that template by a concrete nominal's args. `ValueNone` when the
        // nominal is not a project-local seq class.
        let enumeratorOf (seqTy: FrozenType) : FrozenType voption =
            match seqTy with
            | FTClass(key, args) ->
                match enumeratorTemplateByClass.TryGetValue(SymbolKey.Type key) with
                | true, template -> ValueSome(substituteDeclaring (args.AsSpan().ToArray()) template)
                | false, _ -> ValueNone
            | _ -> ValueNone

        // The closure-verdict TAST rewrite for THIS file's bodies.
        let verdict =
            ClosureVerdictRewrite.build
                closureValueTypeByNode
                file.FunVerdicts
                enumeratorOf
                [ for mv in plan.AllModuleValues -> mv.Key, mv.Ty, mv.Init ]

        // Surface this file's slot types into the shared lookup the field pass reads. A
        // non-verdict binding maps to its declared type unchanged.
        for mv in plan.AllModuleValues do
            moduleValueSlotType.[mv.SymbolKey] <- verdict.ModuleValueSlotType mv.Key mv.Ty

        {
            Layout = file
            CtorHandleByNode = ctorHandleByNode
            CachedClosureFieldByNode = cachedClosureFieldByNode
            ClosureValueTypeByNode = closureValueTypeByNode
            ClosureTypeDefByNode = closureTypeDefByNode
            Verdict = verdict
        }

    // Every file's prelude runs BEFORE the field pass, so all nominals are registered and
    // value-struct closures minted by the time any field signature is encoded.
    let filePreludes = layout.Files |> List.map buildPrelude

    // Tables are independent (only intra-table order matters), so the whole field table
    // is written up front, straight off the layout. A generic closure's capture-field
    // signature encodes inside the ambient closure-typar scope, bracketed per slot.
    do
        for fs in layout.Fields do
            let addField () =
                // An ungrounded type constructor in a field type (a closure capture whose element typar
                // never resolved, say) surfaces here as an opaque encoder failure; identify the
                // field + type so the front-end grounding gap is pinpointable.
                let fieldSig =
                    try
                        match fs.Key with
                        | FieldKey.ClosureCached name ->
                            provider.ClosureSelfFieldSignature(
                                toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure name))
                            )
                        // The owning file's verdict may have rewritten this slot to a
                        // `<closure>$` value-struct; absent ⇒ the declared type unchanged.
                        | FieldKey.ModuleValue mvKey ->
                            let slotTy =
                                match moduleValueSlotType.TryGetValue mvKey with
                                | true, t -> t
                                | false, _ -> fs.Ty

                            provider.FieldSignature slotTy
                        | _ -> provider.FieldSignature fs.Ty
                    with ex ->
                        raise (System.Exception(sprintf "While encoding field '%s' : %A" fs.Name fs.Ty, ex))

                ctx.AddField(fs.Attrs, fs.Name, fieldSig)

            let h =
                match fs.ClosureScope with
                | ValueSome d -> provider.WithClosureTyparScope(d, addField)
                | ValueNone -> addField ()

            fieldDefHandles.Add(fs.Key, h)

            // A SIBLING file's cross-file read of this value resolves to the local `FieldDef`
            // instead of an `AssemblyRef`-scoped member ref, as `RegisterLocalModuleFn` does
            // for a function. Keyed by the `SymbolKey` a reference spells.
            match fs.Key with
            | FieldKey.ModuleValue mvKey -> provider.RegisterLocalModuleValue(mvKey, toEntity h)
            | _ -> ()

            // A numeric enum case field: attach its `Constant` row now, in field
            // order, so the `Constant.Parent` column is ascending.
            match enumFieldConstants.TryGetValue fs.Key with
            | true, boxed -> ctx.AddConstant(toEntity h, boxed) |> ignore
            | false, _ -> ()

        if ctx.FieldRowCount <> layoutHandles.TotalFields then
            failwithf
                "Layout: field table has %d rows after the field pass, layout owns %d"
                ctx.FieldRowCount
                layoutHandles.TotalFields

    // Per file, AFTER the field pass: the field-derived tables (struct-enum registry,
    // static-method refs, module-value field handles) and this file's `EmitContext`.
    let completeFile (pre: FilePrelude) : FileEmit =
        let file = pre.Layout
        let partitioned = file.Partitioned
        let plan = file.Plan

        // String/mixed enums: the per-case `static initonly` field handles drive `E.A`
        // `ldsfld`; the case literals + backing field drive `| E.A`'s field equality.
        for sed in partitioned.StructEnums do
            let caseFields = Dictionary<string, EntityHandle>()
            let caseLits = Dictionary<string, TEnumLiteral>()

            for (caseName, lit) in sed.Cases do
                caseFields.[caseName] <- toEntity fieldDefHandles.[FieldKey.EnumCaseField(sed.Decl.Key, caseName)]
                caseLits.[caseName] <- lit

            let backingField = toEntity fieldDefHandles.[FieldKey.EnumBackingField sed.Decl.Key]

            enums.[sed.Decl.TypeKey] <-
                {
                    Repr = Emit.EmittedEnumRepr.StructEnum(sed.IsMixed, backingField, caseFields, caseLits)
                }

        // Call sites resolve through the layout-derived handle, so recursion and
        // cross-calls need no emission-order discipline.
        let staticMethods = Dictionary<BoundVarId, Emit.StaticMethodRef>()

        for fn in plan.StaticFns do
            staticMethods.[fn.Key] <-
                {
                    Handle = toEntity (layoutHandles.MethodDefOf(MethodKey.StaticFn fn.SymbolKey))
                    Params = fn.Params |> CompiledFns.FlatParams.map (fun p -> p.Ty)
                    ResultTy = fn.ResultTy
                    Scheme = fn.Scheme
                    ReturnsVoid = fn.ReturnsVoid
                }

        // Module-value bindings resolve to their already-written field rows.
        let moduleValueFields = Dictionary<BoundVarId, EntityHandle>()

        for mv in plan.AllModuleValues do
            moduleValueFields.[mv.Key] <- toEntity fieldDefHandles.[FieldKey.ModuleValue mv.SymbolKey]

        // The trailing top-level values: their field is stored by `Main` (`stsfld`), not
        // a `.cctor`. Same handles as above, split out so the store can be emitted for
        // these and skipped for the cctor-initialised ones.
        let mainInitValues = Dictionary<BoundVarId, EntityHandle>()

        for mv in plan.ProgramMainValues do
            mainInitValues.[mv.Key] <- moduleValueFields.[mv.Key]

        let emitCtx: Emit.EmitContext =
            {
                Provider = icodegen
                Ctx = ctx
                Pool = file.Pool
                ClosureByNode = file.ClosureByNode
                CtorHandleByNode = pre.CtorHandleByNode
                CachedClosureFieldByNode = pre.CachedClosureFieldByNode
                ClosureValueTypeByNode = pre.ClosureValueTypeByNode
                ClosureTypeDefByNode = pre.ClosureTypeDefByNode
                Unions = unions
                Records = records
                Classes = classes
                Interfaces = interfaces
                Enums = enums
                StaticMethods = staticMethods
                ModuleValues = moduleValueFields
                MainInitValues = mainInitValues
            }

        {
            Layout = file
            Verdict = pre.Verdict
            EmitCtx = emitCtx
        }

    let files = filePreludes |> List.map completeFile

    // The Prepare phase binds every layout method row to its signature + body offset +
    // param names; the writer then walks `layout.Methods` and writes them in order.
    let prepared = Dictionary<MethodKey, PreparedMethod>()

    let closureAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit

    // A numeric enum: sealed `auto ansi` extending `System.Enum`, which supplies
    // value-type-ness + equality/hashing/compare. No `BeforeFieldInit`, because there is no
    // `.cctor`: the case fields are `literal`s in the `Constant` table.
    let enumAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass

    // A string/mixed enum's `[<Struct>]` wrapper: a sealed value type with NO
    // `BeforeFieldInit`, because its `.cctor` materialises the case singletons and must run
    // before the first case `ldsfld`.
    let structEnumAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.SequentialLayout
        ||| TypeAttributes.AnsiClass

    let interfaceAttrs =
        TypeAttributes.Interface ||| TypeAttributes.Abstract ||| TypeAttributes.Public

    // A module class / the anonymous "Program" class: an `abstract sealed` static
    // class. One owning module-value fields has a side-effecting `.cctor`; drop
    // `BeforeFieldInit` so it runs before first member access.
    let moduleClassAttrsOf (hasCctor: bool) =
        let baseAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed
            ||| TypeAttributes.AutoLayout

        if hasCctor then
            baseAttrs
        else
            baseAttrs ||| TypeAttributes.BeforeFieldInit

    // Nested visibility REPLACES the 3-bit visibility field rather than adding to it: a
    // nested type's `Public` becomes `NestedPublic` and its `NotPublic` (assembly-visible)
    // becomes `NestedAssembly`.
    let nestedAttrsOf (enclosing: TypeSlotKey voption) (attrs: TypeAttributes) =
        match enclosing with
        | ValueNone -> attrs
        | ValueSome _ ->
            let visibility = attrs &&& TypeAttributes.VisibilityMask

            let nested =
                if visibility = TypeAttributes.Public then
                    TypeAttributes.NestedPublic
                elif visibility = TypeAttributes.NotPublic then
                    TypeAttributes.NestedAssembly
                else
                    failwithf "Layout: '%A' is not a top-level visibility" visibility

            (attrs &&& ~~~TypeAttributes.VisibilityMask) ||| nested

    // `attrs` at assembly visibility: `NotPublic` at the top level, `NestedAssembly` nested.
    let assemblyVisible (attrs: TypeAttributes) =
        (attrs &&& ~~~TypeAttributes.VisibilityMask) ||| TypeAttributes.NotPublic

    // A hierarchy union's base: abstract, so every value of it is an instance of one of its
    // case types, which implement the structural slots it declares.
    let abstractBaseAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Abstract
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass
        ||| TypeAttributes.BeforeFieldInit

    // A user class opts in to `Sealed` via `[<Sealed>]`; without it the class is open.
    // A `[<Struct>]` value type is always sealed and uses sequential layout.
    let classAttrsOf (isSealed: bool) (isValueType: bool) =
        let layoutAttr =
            if isValueType then
                TypeAttributes.SequentialLayout
            else
                TypeAttributes.AutoLayout

        let baseAttrs =
            TypeAttributes.Class
            ||| TypeAttributes.Public
            ||| layoutAttr
            ||| TypeAttributes.AnsiClass
            ||| TypeAttributes.BeforeFieldInit

        if isSealed || isValueType then
            baseAttrs ||| TypeAttributes.Sealed
        else
            baseAttrs

    // A struct union's overlay: the one `ExplicitLayout` type the emitter writes.
    let explicitLayoutStructAttrs =
        (classAttrsOf true true &&& ~~~TypeAttributes.LayoutMask)
        ||| TypeAttributes.ExplicitLayout

    // `Param` rows are one global table referenced by each `MethodDefinition.ParamList`,
    // so they must be added in method order. Call this immediately before each
    // `AddMethodWithParamList`. Returns the first `Param` handle, past-the-end if none.
    let addParams (names: string list) : ParameterHandle =
        let firstParam = ctx.NextParamHandle
        names |> List.iteri (fun i n -> ctx.AddParameter(i + 1, n) |> ignore)
        firstParam

    // `GenericParam` rows can't be added inline: SRM requires them globally sorted by
    // `CodedIndex.TypeOrMethodDef(owner)`, and a method owner can sort BEFORE its
    // declaring type. Collect them and emit sorted once every handle exists.
    let genericParams = ResizeArray<GenericParamEntry>()

    let addGenericParams (owner: EntityHandle) (rows: GenericParamRow list) =
        rows
        |> List.iteri (fun i row -> genericParams.Add { Owner = owner; Index = i; Row = row })

    // Everything else a `TypeDefinition` row needs comes from the layout.
    let typeRowExtras = Dictionary<TypeSlotKey, TypeRowExtras>()

    // The frozen-attribute `CustomAttribute` rows and skips (`PrepareCustomAttributeRows`).
    // The rows are flushed once in `Finalise`; SRM sorts the table by parent, so add order
    // is free.
    let mutable attributeRows: AttributeRowPrep.Prepared = { Rows = []; Skipped = [] }

    let addSyntheticAttribute (parent: EntityHandle) (attr: SyntheticAttribute) =
        ctx.AddCustomAttribute(parent, provider.SyntheticAttributeCtor attr, SyntheticAttribute.blob attr)
        |> ignore

    member _.Provider = provider
    member _.Icodegen = icodegen

    /// The BCL members and heap strings the synthesised structural bodies call.
    member _.Structural: IStructuralHandles = provider

    /// How a structural body compares a field of type `ty`: a primitive by its own key,
    /// a project-local numeric enum by its underlying key, everything else through the
    /// BCL comparers.
    member _.FieldCompareOf(ty: FrozenType) : EmitStructural.FieldCompare =
        match ty with
        | FTConst(key, args) when args.IsEmpty -> EmitStructural.FieldCompare.ofPrimitiveKey key
        | FTEnum key ->
            match enums.TryGetValue key with
            | true,
              {
                  Repr = Emit.EmittedEnumRepr.NumericEnum(underlying, _)
              } -> EmitStructural.FieldCompare.ofPrimitiveKey underlying
            | _ -> EmitStructural.FieldCompare.Comparer
        | _ -> EmitStructural.FieldCompare.Comparer

    /// Lower an IL body and stage it in the body stream, yielding the offset the
    /// `MethodDef` row points at.
    member _.MethodBody(ir: ILBody) : PreparedBody = methodBody ir

    member _.Ctx = ctx
    member _.BodyStream = bodyStream
    member _.EncodeLocals = encodeLocals
    member _.Unions = unions
    member _.Records = records
    member _.Classes = classes

    member _.Files: FileEmit list = files

    member _.Symbols: ICodegenSymbols = codegenSymbols

    member _.LayoutHandles = layoutHandles

    /// A field's def-table handle. Whether a use site routes through a `MemberRef`
    /// instead (generic types / closures) stays the caller's policy.
    member _.FieldDef(key: FieldKey) : FieldDefinitionHandle = fieldDefHandles.[key]

    member _.MethodDef(key: MethodKey) : MethodDefinitionHandle = layoutHandles.MethodDefOf key

    /// Bind a layout method row to its signature/body; the row itself is written in
    /// layout order by the writer.
    member _.AddPrepared(key: MethodKey, m: PreparedMethod) = prepared.Add(key, m)

    member _.AddTypeRowExtras(key: TypeSlotKey, extras: TypeRowExtras) = typeRowExtras.Add(key, extras)

    /// The generalised `CustomAttribute` rows: one per frozen attribute on the assembly's
    /// type declarations, their own members, and record fields. A union-case or enum-case
    /// attribute has no metadata parent row of its own and emits no row.
    member _.PrepareCustomAttributeRows() =
        attributeRows <-
            AttributeRowPrep.prepare
                provider
                classes
                enums
                layoutHandles
                fieldDefHandles
                [ for f in files -> f.Layout.Partitioned ]

    // Monomorphic: the construction-site `newobj` targets the ctor's `Def` directly. A
    // generic closure mints a fresh `MemberRef` at the use site, so its entry stays unset.
    member this.BindClosures(f: FileEmit) =
        for c in f.Layout.Closures do
            if c.Typars = 0 then
                f.EmitCtx.CtorHandleByNode.[c.Node] <-
                    toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))

            if Emit.closureIsCached c then
                f.EmitCtx.CachedClosureFieldByNode.[c.Node] <-
                    toEntity (fieldDefHandles.[FieldKey.ClosureCached c.Name])

            // A value-struct closure's `FrozenType` + `TypeDef` handle were minted in the
            // prelude, before the field pass. `RegisterStackClosureValueType` fails on a
            // duplicate key, so this asserts presence rather than re-minting.
            if c.IsValueStruct && not (f.EmitCtx.ClosureValueTypeByNode.ContainsKey c.Node) then
                failwithf "Emit: value-struct closure '%s' was not pre-minted before the field pass" c.Name

    member this.PrepareInterfaces(f: FileEmit) =
        for (td, methods) in f.Layout.Partitioned.Interfaces do
            // The use-site table for a call on an interface-typed object arg: each method's
            // slot handle keyed by source name, to `callvirt`. Overloads share a name,
            // hence the list.
            let memberTable = Dictionary<string, EqArray<Emit.EmittedMember>>()

            methods
            |> List.iteri (fun i m ->
                let slots = abstractMethodParams m
                let paramTys = List.map snd slots
                let _, retTy = uncurry m.Signature

                let handle = toEntity (this.MethodDef(MethodKey.InterfaceMethod(td.Key, i)))

                let em: Emit.EmittedMember =
                    {
                        Handle = handle
                        IsStatic = false
                        ParamArity = List.length paramTys
                        // The IL method name (a property → its `get_<Name>` getter);
                        // the use-site table below stays keyed by the bare member name.
                        MetaName = memberMetaName m.Name m.Kind
                        ParamTys = paramTys
                        RetTy = retTy
                        MethodTyparCount = m.MethodTypeParams.Length
                    }

                memberTable.[m.Name] <-
                    match memberTable.TryGetValue m.Name with
                    | true, existing -> EqArray.append existing (EqArray.singleton em)
                    | false, _ -> EqArray.singleton em

                this.AddPrepared(
                    MethodKey.InterfaceMethod(td.Key, i),
                    {
                        Signature = abstractMethodSignature provider m
                        Body = PreparedBody.Abstract
                        ParamNames = List.map fst slots
                        MethodTypars = GenericParamRow.ofTypars m.MethodTypeParams m.MethodTyparConstraints
                    }
                )
            )

            interfaces.[td.TypeKey] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList (TTypeParam.names td.TypeParams)
                    Members = memberTable
                }

    /// Prepare each string/mixed enum's `.ctor` (stores the wrapped value) and `.cctor`
    /// (a `newobj;stsfld` per case singleton), and record its `System.ValueType` base
    /// for the `TypeDefinition` row.
    member this.PrepareStructEnums(f: FileEmit) =
        for sed in f.Layout.Partitioned.StructEnums do
            let td = sed.Decl

            let fieldTy =
                FTConst(
                    (if sed.IsMixed then
                         RuntimeNames.objKey
                     else
                         RuntimeNames.stringKey),
                    EqArray.empty
                )

            // The registry's case → literal map feeds the `| E.A` pattern's field
            // equality, not the `.cctor`, whose literals come off `sed.Cases`.
            let backingField, caseFields =
                match enums.[td.TypeKey].Repr with
                | Emit.EmittedEnumRepr.StructEnum(_, bf, cf, _) -> bf, cf
                | other -> failwithf "Emit: struct enum '%A' has a non-struct repr %A" td.Key other

            // The single-arg value-type `.ctor(value)` storing the backing field.
            let ctorBody = methodBody (Emit.buildStructCtor [ backingField ])

            this.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ fieldTy ]
                    Body = ctorBody
                    ParamNames = [ "value" ]
                    MethodTypars = []
                }
            )

            // The `.cctor` constructs each case singleton in declaration order.
            let ctorHandle = toEntity (layoutHandles.MethodDefOf(MethodKey.NominalCtor td.Key))

            // Each case pushes its literal (`ldstr`, or `ldc;box` for a mixed int) and
            // `newobj`s the wrapper over it.
            let cctorCases =
                [
                    for (caseName, lit) in sed.Cases ->
                        EmitResolve.enumLiteralPush icodegen.TypeToken ctx.UserString lit
                        @ [ ILInstr.Newobj(ctorHandle, 1) ],
                        caseFields.[caseName]
                ]

            let cctorBody = methodBody (Emit.buildCachedFieldCctor cctorCases)

            this.AddPrepared(
                MethodKey.NominalCctor td.Key,
                {
                    Signature = provider.CctorSignature()
                    Body = cctorBody
                    ParamNames = []
                    MethodTypars = []
                }
            )

            // The wrapper extends `System.ValueType` (value-type-ness); no interfaces.
            this.AddTypeRowExtras(
                TypeSlotKey.Nominal td.Key,
                {
                    Interfaces = []
                    BaseType = provider.ValueTypeBase
                }
            )

    // A generic closure enters closure-typar mode around every signature/body build, so
    // the enclosing function's `FTTypar(scope, i)` re-projects onto this class's `!i`.
    member this.PrepareClosures(f: FileEmit) =
        for c in f.Layout.Closures do
            let captureFields = Dictionary<BoundVarId, EntityHandle>()
            let isGenericClosure = c.Typars > 0
            // This closure's self-instantiation over its OWN typars (`!0 … !{n-1}`), for
            // the capture-field `MemberRef`s on its self-`TypeSpec`. A closure typar is
            // its own declaring typar, so this encodes `!i` at any closure-scope offset.
            let selfArgs = [ for i in 0 .. c.Typars - 1 -> FTTypar(closureScope c.Name, i) ]

            // A `Stack` closure's ctor does NOT chain `System.Object::.ctor`, because
            // value types have none. A captureless one's ctor is a bare `ret`: construction is
            // by-value (`initobj`), so it is never called, but the row stays for layout.
            let isStack = c.IsValueStruct

            let prepare () =
                let fieldHandles =
                    c.Captures
                    |> List.mapi (fun i (k, _) ->
                        let h = fieldDefHandles.[FieldKey.ClosureCapture(c.Name, i)]

                        // Generic closure: `stfld` (ctor) and `ldfld` (`Invoke`) reference a
                        // `MemberRef` on the self-`TypeSpec`; monomorphic keeps `Def`.
                        let handleForUse =
                            if isGenericClosure then
                                icodegen.UserClosureMemberRef(c.Name, selfArgs, ClosureMember.CaptureField i)
                            else
                                toEntity h

                        captureFields.[k] <- handleForUse
                        handleForUse
                    )

                let ctorMethodBody =
                    if isStack then
                        methodBody (Emit.buildStructCtor fieldHandles)
                    else
                        methodBody (Emit.buildChainedCtor provider.ObjectCtorRef [] fieldHandles)

                let invokeMethodBody =
                    methodBody (Emit.buildClosureInvoke f.EmitCtx c captureFields)

                this.AddPrepared(
                    MethodKey.ClosureCtor c.Name,
                    {
                        Signature = provider.ClosureCtorSignature(List.map snd c.Captures)
                        Body = ctorMethodBody
                        ParamNames = paramNames f.EmitCtx.Pool (Seq.map fst c.Captures)
                        MethodTypars = []
                    }
                )

                let invokeSignature, invokeParamNames =
                    let paramTys = c.ParamTy :: (c.ExtraParams |> List.map (fun (_, ty, _) -> ty))
                    let paramKeys = c.ParamKey :: (c.ExtraParams |> List.map (fun (k, _, _) -> k))
                    provider.InvokeSignatureN(paramTys, c.ResultTy), paramNames f.EmitCtx.Pool paramKeys

                this.AddPrepared(
                    MethodKey.ClosureInvoke c.Name,
                    {
                        Signature = invokeSignature
                        Body = invokeMethodBody
                        ParamNames = invokeParamNames
                        MethodTypars = []
                    }
                )

                // A cached closure's `.cctor` `newobj`s the ctor once and `stsfld`s the
                // singleton field that construction sites `ldsfld`.
                if Emit.closureIsCached c then
                    let ctorHandle = toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))
                    let cachedField = toEntity (fieldDefHandles.[FieldKey.ClosureCached c.Name])

                    let cctorMethodBody =
                        methodBody (Emit.buildCachedFieldCctor [ [ ILInstr.Newobj(ctorHandle, 0) ], cachedField ])

                    this.AddPrepared(
                        MethodKey.ClosureCctor c.Name,
                        {
                            Signature = provider.CctorSignature()
                            Body = cctorMethodBody
                            ParamNames = []
                            MethodTypars = []
                        }
                    )

                // `Fun\`2<param, result>` interface `TypeSpec`. The closure ambient is still
                // installed, so free typars encode to `!i`. A flat (arity ≥2) closure
                // implements the wider `Fun\`(N+1)<a, …, result>` instead.
                let ifaceSpec =
                    match c.FunArity with
                    | 1 -> provider.FunInterfaceSpec(c.ParamTy, c.ResultTy)
                    | _ ->
                        let tys =
                            (c.ParamTy :: (c.ExtraParams |> List.map (fun (_, ty, _) -> ty)))
                            @ [ c.ResultTy ]

                        provider.FlatFunInterfaceSpecN(tys)

                ifaceSpec

            let ifaceSpec =
                if isGenericClosure then
                    provider.WithClosureTyparScope(c.DeclaringTypars, prepare)
                else
                    prepare ()

            typeRowExtras.Add(
                TypeSlotKey.Closure c.Name,
                {
                    Interfaces = [ ifaceSpec ]
                    BaseType =
                        (if isStack then
                             provider.ValueTypeBase
                         else
                             provider.ObjectType)
                }
            )

    member this.PrepareStaticMethods(f: FileEmit) =
        let plan = f.Layout.Plan
        let emitCtx = f.EmitCtx
        let staticMethods = emitCtx.StaticMethods
        let moduleValueFields = emitCtx.ModuleValues
        let retypeBody = f.Verdict.RetypeBody

        let prepareStaticFn (fn: Emit.StaticFn) =
            // A generic static method's body / signature / locals embed
            // `FTTypar(ModuleFunction _, i)`, which the encoder maps to `!!i`.
            let typarCount = fn.Scheme.TyparArity

            // Retype the body so a reference to a verdict module value, or an inline
            // transformer call, dispatches on the `<closure>$` value-struct nominal
            // rather than the frozen function type.
            let fn = { fn with Body = retypeBody fn.Body }

            let staticBody = methodBody (Emit.buildStaticMethod emitCtx fn)

            let paramTys = fn.Params.Flat |> List.map (fun p -> p.Ty)

            // A `unit`-returning module function encodes genuine CLR `void`.
            let signature =
                match typarCount = 0, fn.ReturnsVoid with
                | true, false -> provider.StaticMethodSignature(paramTys, fn.ResultTy)
                | true, true -> provider.StaticMethodSignatureVoid(paramTys)
                | false, false -> provider.GenericStaticFnSignature(typarCount, paramTys, fn.ResultTy)
                | false, true -> provider.GenericMethodOnTypeSignatureVoid(typarCount, paramTys, false)

            this.AddPrepared(
                MethodKey.StaticFn fn.SymbolKey,
                {
                    Signature = signature
                    Body = staticBody
                    ParamNames = paramNames emitCtx.Pool (fn.Params.Flat |> Seq.map (fun p -> p.Slot))
                    MethodTypars = GenericParamRow.ofScheme fn.Scheme
                }
            )

        // A module class's `.cctor` `stsfld`s its module values in declaration order, the
        // static analogue of a class's `static let` cctor.
        let prepareModuleClassCctor (h: Emit.ModuleClassKey) =
            let lets =
                [
                    for mv in ModuleClassPlan.moduleClassValues plan h ->
                        Emit.PreambleStep.Store(moduleValueFields.[mv.Key], retypeBody mv.Init)
                ]

            let staticBody = methodBody (Emit.buildStaticCctor emitCtx lets)

            this.AddPrepared(
                MethodKey.ModuleClassCctor h,
                {
                    Signature = provider.CctorSignature()
                    Body = staticBody
                    ParamNames = []
                    MethodTypars = []
                }
            )

        // The anonymous "Program" class's `.cctor`: the same store recipe as a named
        // module class's, over the leading-prefix top-level values.
        let prepareProgramCctor () =
            let lets =
                [
                    for mv in plan.ProgramCctorValues ->
                        Emit.PreambleStep.Store(moduleValueFields.[mv.Key], retypeBody mv.Init)
                ]

            let staticBody = methodBody (Emit.buildStaticCctor emitCtx lets)

            this.AddPrepared(
                MethodKey.ProgramCctor,
                {
                    Signature = provider.CctorSignature()
                    Body = staticBody
                    ParamNames = []
                    MethodTypars = []
                }
            )

        for slot in plan.MethodPlan do
            match slot with
            | ModuleClassCctor h -> prepareModuleClassCctor h
            | ModuleClassFn fn -> prepareStaticFn fn
            | ProgramCctor -> prepareProgramCctor ()

    /// `Main` belongs to the ENTRY file only, the one whose layout carries the entry
    /// point. A non-entry file contributes no `Main` row, so this is a no-op for it.
    member this.PrepareMain(f: FileEmit) =
        if f.Layout.EmitEntryPoint then
            let mainDecls = f.Layout.Lowered |> List.map f.Verdict.RetypeDecl

            let mainMethodBody = methodBody (Emit.buildMain f.EmitCtx mainDecls)

            this.AddPrepared(
                MethodKey.Main,
                {
                    Signature = mainSignature ()
                    Body = mainMethodBody
                    ParamNames = [ "args" ]
                    MethodTypars = []
                }
            )

    // Per row: `Param` rows, then the `MethodDef` row that points at them. The method's
    // own `GenericParam` rows are only collected here; they are emitted sorted later.
    member this.WriteMethods() =
        for row in layout.Methods do
            let p =
                match prepared.TryGetValue row.Key with
                | true, p -> p
                | _ -> failwithf "Layout: method row '%s' (%A) was never prepared" row.Name row.Key

            let firstParam = addParams p.ParamNames

            // `-1` is SRM's "no body" RVA, for an abstract row.
            let bodyOffset =
                match row.Attrs.HasFlag MethodAttributes.Abstract, p.Body with
                | true, PreparedBody.Abstract -> -1
                | false, PreparedBody.At offset -> offset
                | true, PreparedBody.At _ ->
                    failwithf "Layout: abstract method row '%s' (%A) was prepared with a body" row.Name row.Key
                | false, PreparedBody.Abstract ->
                    failwithf "Layout: method row '%s' (%A) declares a body but was prepared abstract" row.Name row.Key

            let handle =
                ctx.AddMethodWithParamList(row.Attrs, row.Name, p.Signature, bodyOffset, firstParam)

            let predicted = layoutHandles.MethodDefOf row.Key

            if handle <> predicted then
                failwithf
                    "Layout: method '%s' predicted MethodDef row %d <> actual %d"
                    row.Name
                    (MetadataTokens.GetRowNumber(toEntity predicted))
                    (MetadataTokens.GetRowNumber(toEntity handle))

            for a in MethodKey.syntheticAttributes row.Key do
                addSyntheticAttribute (toEntity handle) a

            addGenericParams (toEntity handle) p.MethodTypars

        if prepared.Count <> layoutHandles.TotalMethods then
            failwithf
                "Layout: %d methods were prepared but the layout owns %d rows"
                prepared.Count
                layoutHandles.TotalMethods

    /// Per row: the `Property` row, then a `MethodSemantics` row per accessor declared for it.
    member this.WriteProperties() =
        for row in layout.Properties do
            let signature =
                provider.PropertySignature(row.IsInstance, row.IndexTys, row.ValueTy)

            let handle = ctx.AddProperty(row.Name, signature)
            let predicted = layoutHandles.PropertyDefOf row.Key

            if handle <> predicted then
                failwithf
                    "Layout: property '%s' predicted Property row %d <> actual %d"
                    row.Name
                    (MetadataTokens.GetRowNumber(toEntity predicted))
                    (MetadataTokens.GetRowNumber(toEntity handle))

            let bind (semantics: MethodSemanticsAttributes) (accessor: MethodKey voption) =
                match accessor with
                | ValueNone -> ()
                | ValueSome key -> ctx.AddMethodSemantics(toEntity handle, semantics, layoutHandles.MethodDefOf key)

            bind MethodSemanticsAttributes.Getter row.Getter
            bind MethodSemanticsAttributes.Setter row.Setter

        if ctx.PropertyRowCount <> layoutHandles.TotalProperties then
            failwithf
                "Layout: the Property table has %d rows but the layout owns %d"
                ctx.PropertyRowCount
                layoutHandles.TotalProperties

    member this.Finalise() : ClrArtifact =
        for struct (parent, attrCtor, blob) in attributeRows.Rows do
            ctx.AddCustomAttribute(parent, attrCtor, blob) |> ignore

        let rowOf (h: EntityHandle) = MetadataTokens.GetRowNumber h

        let verifyTypeHandle (slot: TypeSlot) (actual: TypeDefinitionHandle) =
            let predicted = layoutHandles.TypeDefOf slot.Key

            if predicted <> actual then
                failwithf
                    "Layout: slot '%s' predicted TypeDef row %d <> actual %d"
                    slot.MetaName
                    (rowOf (toEntity predicted))
                    (rowOf (toEntity actual))

        // Called while the NESTED type is written, so the `NestedClass` table comes out
        // sorted by the nested handle, which SRM validates. The enclosing type precedes
        // this node in the flattening, so its handle already resolves.
        let addNesting (node: TypeNode) (typeHandle: TypeDefinitionHandle) =
            match node.Enclosing with
            | ValueNone -> ()
            | ValueSome encl -> ctx.AddNestedType(typeHandle, layoutHandles.TypeDefOf encl)

        // `IsReadOnlyAttribute` for a value type whose every instance field is `initonly`.
        // FSC stamps it only on an explicit `[<IsReadOnly>]`; Vesper infers it because a
        // `readonly struct` spares the JIT a defensive copy at each getter call.
        let readOnlyMarkerOf (node: TypeNode) (isValueType: bool) : SyntheticAttribute list =
            let isInitOnlyOrStatic (f: FieldSlot) =
                f.Attrs.HasFlag FieldAttributes.Static
                || f.Attrs.HasFlag FieldAttributes.InitOnly

            if isValueType && List.forall isInitOnlyOrStatic node.Fields then
                [ SyntheticAttribute.IsReadOnly ]
            else
                []

        // Union, record, class and closure `TypeDefinition` rows share one recipe.
        // Walking the layout in order keeps the `InterfaceImpl` / `GenericParam` rows
        // ascending (sorted by `Class` / `TypeOrMethodDef`).
        let addNominalRow (node: TypeNode) (attrs: TypeAttributes) (markers: SyntheticAttribute list) =
            let slot = node.Slot

            let extras =
                match typeRowExtras.TryGetValue slot.Key with
                | true, e -> e
                | _ -> failwithf "Layout: type slot '%s' was never prepared" slot.MetaName

            let typeHandle =
                ctx.AddClass(
                    nestedAttrsOf node.Enclosing attrs,
                    slot.Namespace,
                    slot.MetaName,
                    extras.BaseType,
                    layoutHandles.FirstFieldOf slot.Key,
                    layoutHandles.FirstMethodOf slot.Key
                )

            verifyTypeHandle slot typeHandle
            addNesting node typeHandle

            for m in markers do
                addSyntheticAttribute (toEntity typeHandle) m

            for iface in extras.Interfaces do
                ctx.AddInterfaceImplementation(typeHandle, iface)

            addGenericParams (toEntity typeHandle) slot.Typars

        let addUnionValueTypeRow (node: TypeNode) (attrs: TypeAttributes) (markers: SyntheticAttribute list) =
            typeRowExtras.Add(
                node.Slot.Key,
                {
                    Interfaces = []
                    BaseType = provider.ValueTypeBase
                }
            )

            addNominalRow node attrs markers

        // A struct union's `Payload` and overlay types: `assembly`-visible value types,
        // read directly by match arms in this assembly.
        let addUnionPayloadRow (node: TypeNode) (attrs: TypeAttributes) =
            addUnionValueTypeRow node (assemblyVisible attrs) []

        for node in layout.Types do
            let slot = node.Slot

            match slot.Kind with
            | TypeSlotKind.ModulePseudo ->
                // `<Module>` points at method row 1: the first real method, or
                // past-the-end of the empty table in a no-method library.
                ctx.AddModuleType(layoutHandles.FirstMethodOf slot.Key)

            | TypeSlotKind.Interface ->
                // Interfaces have no fields, so the field range is empty: the prefix sum
                // hands back the row the next field-bearing type starts at.
                let typeHandle =
                    ctx.AddInterfaceType(
                        nestedAttrsOf node.Enclosing interfaceAttrs,
                        slot.Namespace,
                        slot.MetaName,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle
                addNesting node typeHandle

                addGenericParams (toEntity typeHandle) slot.Typars

            // Unions and records are always sealed; a class opts in via `[<Sealed>]` /
            // `[<Struct>]`. A union or record opts into value-type emission via `[<Struct>]`,
            // and is readonly when every instance field is `initonly`.
            | TypeSlotKind.Union(valueKind, regime) ->
                let markers = readOnlyMarkerOf node valueKind.IsValueType

                let attrs =
                    if UnionRegime.isHierarchy regime then
                        abstractBaseAttrs
                    else
                        classAttrsOf true valueKind.IsValueType

                addNominalRow node attrs markers

            // A case type is sealed, so the JIT devirtualises the structural overrides
            // wherever the receiver's exact type is known.
            | TypeSlotKind.UnionCase -> addNominalRow node (classAttrsOf true false) []

            | TypeSlotKind.UnionPayload
            | TypeSlotKind.UnionCaseData -> addUnionPayloadRow node (classAttrsOf true true)

            // Every case data struct sits at offset 0 of the overlay: one `FieldLayout` row
            // per field and no `ClassLayout` row, leaving every size to the loader.
            | TypeSlotKind.UnionOverlay ->
                addUnionPayloadRow node explicitLayoutStructAttrs

                for f in node.Fields do
                    ctx.AddFieldLayout(fieldDefHandles.[f.Key], 0)

            // The union's public consumer surface: nested-public over `assembly`-visible
            // payload types.
            | TypeSlotKind.UnionCaseView ->
                addUnionValueTypeRow node (classAttrsOf true true) (readOnlyMarkerOf node true)

            | TypeSlotKind.Record valueKind ->
                addNominalRow
                    node
                    (classAttrsOf true valueKind.IsValueType)
                    (readOnlyMarkerOf node valueKind.IsValueType)

            | TypeSlotKind.Class(isSealed, valueKind) ->
                let markers =
                    if valueKind = ClassValueKind.RefStruct then
                        [ SyntheticAttribute.IsByRefLike ]
                    else
                        []

                addNominalRow node (classAttrsOf isSealed valueKind.IsValueType) markers

            // A numeric enum: base `System.Enum`, no interfaces, no methods, so it has
            // no `TypeRowExtras` (`System.Enum` supplies eq/comp/format) and is written
            // directly rather than through `addNominalRow`.
            | TypeSlotKind.Enum ->
                let typeHandle =
                    ctx.AddClass(
                        nestedAttrsOf node.Enclosing enumAttrs,
                        slot.Namespace,
                        slot.MetaName,
                        provider.EnumBase,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle
                addNesting node typeHandle

            // A string/mixed enum: a `[<Struct>]` value type with a `.ctor` + `.cctor`.
            // Its `System.ValueType` base is stored in `TypeRowExtras`; never byref-like.
            | TypeSlotKind.StructEnum -> addNominalRow node structEnumAttrs []

            // Each closure implements its `Vesper.Fun\`2<param, result>` interface.
            | TypeSlotKind.Closure ->
                let extras =
                    match typeRowExtras.TryGetValue slot.Key with
                    | true, e -> e
                    | _ -> failwithf "Layout: closure slot '%s' was never prepared" slot.MetaName

                // A `Stack` closure is a `[<Struct>]` value type, so sealed with sequential
                // layout; the heap closure keeps the sealed-class `closureAttrs`.
                let attrs =
                    match slot.Key with
                    | TypeSlotKey.Closure name when closureIsValueStruct name -> classAttrsOf true true
                    | _ -> closureAttrs

                let closureHandle =
                    ctx.AddClass(
                        attrs,
                        slot.Namespace,
                        slot.MetaName,
                        extras.BaseType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot closureHandle

                for iface in extras.Interfaces do
                    ctx.AddInterfaceImplementation(closureHandle, iface)

                addGenericParams (toEntity closureHandle) slot.Typars

            // Named-module classes: one static class per `module Foo`, nested in its
            // parent's module class when the module nests. One owning module values takes its
            // own `FieldList`; a value-less one's empty range points past the previous.
            | TypeSlotKind.ModuleClass hasCctor ->
                let typeHandle =
                    ctx.AddClass(
                        nestedAttrsOf node.Enclosing (moduleClassAttrsOf hasCctor),
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle
                addNesting node typeHandle

            // The anonymous "Program" class owns the static methods of no named module (and
            // `Main`, when an executable) and the top-level value fields. `hasCctor` ⇔ it
            // owns leading-prefix values, so its `.cctor` must run before `Main`.
            | TypeSlotKind.Program hasCctor ->
                let typeHandle =
                    ctx.AddClass(
                        moduleClassAttrsOf hasCctor,
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle

        // The `PropertyMap` table: layout order is ascending by `Parent`, as `AddPropertyMap`
        // requires.
        for node in layout.Types do
            match node.Properties with
            | [] -> ()
            | _ ->
                ctx.AddPropertyMap(layoutHandles.TypeDefOf node.Slot.Key, layoutHandles.FirstPropertyOf node.Slot.Key)

        // Every handle now exists: add `GenericParam` rows in the order SRM
        // validates: by the owner's `TypeOrMethodDef` coded index, then index.
        genericParams
        |> Seq.sortBy (fun e -> (CodedIndex.TypeOrMethodDef e.Owner, e.Index))
        |> Seq.iter (fun e -> ctx.AddGenericParameter(e.Owner, e.Index, e.Row.Name, e.Row.Attrs) |> ignore)

        let pe =
            if layout.EmitEntryPoint then
                ctx.Serialize(layoutHandles.MethodDefOf MethodKey.Main)
            else
                ctx.SerializeLibrary()

        {
            Project = project
            Pe = pe
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            SkippedAttributeRows = attributeRows.Skipped
        }
