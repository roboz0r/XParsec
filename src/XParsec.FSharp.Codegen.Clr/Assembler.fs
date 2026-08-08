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
    (symbols: IExternalSymbolProvider, project: ProjectInfo, tasts: FrozenPools list, bclReferences: string list) =

    let ctx = MetadataContext()
    do ctx.AddModuleAndAssembly(project.AssemblyName)

    // Identities read off the reference files, so an emitted `AssemblyRef` names the exact
    // artifact rather than whatever the host loaded. `bclReferences` is separate from
    // `project.References` (which ships beside the output); the latter wins a name tie.
    let references =
        bclReferences @ project.References
        |> List.map (fun path ->
            let an = System.Reflection.AssemblyName.GetAssemblyName path
            an.Name, an
        )
        |> Map.ofList

    // Union over every file: a `SymbolKey` identifies an intrinsic assembly-wide, so a key
    // repeated across files is the same declaration and last-wins is safe. Platform repr
    // only — `extends` comes off the frozen base type.
    let intrinsicReprKeys =
        let d = Dictionary<SymbolKey, string>()

        for tast in tasts do
            for kv in tast.Residue.IntrinsicReprKeys do
                d.[kv.Key] <- kv.Value.Platform

        d

    let provider =
        // Own-compilation intrinsics only; every other primitive's repr is read through the
        // provider, out of the dependency closure's `.fs`.
        ClrProvider(ctx, intrinsicReprKeys, references, symbols)

    let icodegen = provider :> ICodegenProvider
    let encodeLocals (locals: FrozenType list) = icodegen.EncodeLocalSignature locals

    // The narrow emission-side view of the provider: type/member shapes only.
    let codegenSymbols = CodegenSymbols.ofProvider symbols

    // One body-stream encoder for every method — `AddMethodBody` realigns per body
    // internally, so a fresh encoder per body would leave a tiny body's builder unaligned.
    let bodyStream = ctx.BodyStream

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

    // A numeric enum case field's `Constant` value — the case's underlying integer, boxed
    // to the authored CLR primitive. The field pass attaches it as it writes the field.
    let enumFieldConstants = Dictionary<FieldKey, obj>()

    // Shared across files, keyed by nominal `SymbolKey`: a call in one file's body
    // resolves a type or member defined in another through these.
    let unions = Dictionary<SymbolKey, Emit.EmittedUnion>()
    let records = Dictionary<SymbolKey, Emit.EmittedRecord>()
    let classes = Dictionary<SymbolKey, Emit.EmittedClass>()
    let enums = Dictionary<SymbolKey, Emit.EmittedEnum>()
    let interfaces = Dictionary<SymbolKey, Emit.EmittedInterface>()

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

        // Register each nominal's layout-derived `TypeDefinition` handle so a field /
        // factory / local signature can `encodeType` it before the row exists. A generic
        // type also registers its shape, for `MemberRef`s on its `TypeSpec`.
        for ud in partitioned.Unions do
            let td = ud.Decl
            provider.RegisterUserType(td.TypeKey, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                let shape =
                    [
                        for c in ud.Cases ->
                            c.Name,
                            [
                                for fi in 0 .. c.Fields.Length - 1 -> sprintf "%s_%d" c.Name fi, snd c.Fields.[fi]
                            ]
                    ]

                provider.RegisterGenericUnion(td.TypeKey, td.TypeParams, shape)

        for rd in partitioned.Records do
            let td = rd.Decl
            provider.RegisterUserType(td.TypeKey, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            // A `[<Struct>]` record is a project-local value type → `VALUETYPE` (not
            // `CLASS`) in every signature, exactly as a struct class.
            if rd.ValueKind <> ClassValueKind.RefType then
                provider.RegisterUserValueType td.TypeKey

            if not td.TypeParams.IsEmpty then
                let shape = [ for f in rd.Fields -> f.Name, f.Type ]
                provider.RegisterGenericRecord(td.TypeKey, td.TypeParams, shape)

        for cd in partitioned.Classes do
            let td = cd.Decl
            provider.RegisterUserType(td.TypeKey, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if cd.ValueKind <> ClassValueKind.RefType then
                provider.RegisterUserValueType td.TypeKey

            if not td.TypeParams.IsEmpty then
                // On a generic class, ctor-param, `val`, instance-`let` and `static let`
                // fields all reach their `ldfld`/`stfld`/`ldsfld` through a `MemberRef` on
                // the open self-`TypeSpec`, so all four must be registered by name.
                let ctorParamFields = [ for p in cd.CtorParams -> p.Name, p.Type ]

                let shape =
                    ctorParamFields
                    @ [ for f in cd.Fields -> f.Name, f.Type ]
                    @ [ for l in TPreambleEntryG.lets cd.InstancePreamble -> l.Name, l.Type ]
                    @ [ for sl in TPreambleEntryG.lets cd.StaticPreamble -> sl.Name, sl.Type ]

                provider.RegisterGenericClass(td.TypeKey, td.TypeParams, List.length ctorParamFields, shape)

        // Interfaces register their `TypeDef` too, so one naming another as a member's
        // type (`IStructuralFormattable.Format(IFormatSink)`) resolves like any nominal.
        // A generic one (`IStructSeq<'E>`) also needs its slots minted on a `TypeSpec`.
        for (td, _) in partitioned.Interfaces do
            provider.RegisterUserType(td.TypeKey, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                provider.RegisterGenericClass(td.TypeKey, td.TypeParams, 0, [])

        // Every enum — numeric (a `System.Enum` subclass) or string/mixed (a `[<Struct>]`
        // wrapper) — is a project-local value type → `ELEMENT_TYPE_VALUETYPE`.
        for td in
            (partitioned.Enums |> List.map (fun ed -> ed.Decl))
            @ (partitioned.StructEnums |> List.map (fun sed -> sed.Decl)) do
            provider.RegisterUserType(td.TypeKey, toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Nominal td.Key)))
            provider.RegisterUserValueType td.TypeKey

        // Register this file's module functions, Program-class ones included, so a SIBLING
        // file's cross-file call resolves to the local `MethodDef` instead of an
        // `AssemblyRef`-scoped `MemberRef`. Keyed by the `SymbolKey` a reference spells.
        for fn in plan.StaticFns do
            let localMethodDef =
                toEntity (layoutHandles.MethodDefOf(MethodKey.StaticFn fn.SymbolKey))

            provider.RegisterLocalModuleFn(fn.SymbolKey, localMethodDef)

        // A generic closure is a real generic `TypeDefinition`; its handle lets
        // capture-field `MemberRef`s and the construction-site `newobj` both reach it.
        // Monomorphic closures use their `Def` tokens directly.
        for c in closures do
            if c.Typars > 0 then
                let handle = toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure c.Name))

                provider.RegisterClosure(
                    c.Name,
                    c.Typars,
                    c.DeclaringTypars,
                    c.Captures |> List.map snd,
                    c.ParamTy,
                    c.ResultTy,
                    handle
                )

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
                let w, bits = TEnumCases.integralValue v
                enumFieldConstants.[FieldKey.EnumCaseField(ed.Decl.Key, caseName)] <- IntWidth.boxed w bits

            let caseValues = Dictionary<string, TConstValue>()

            for (caseName, v) in ed.Cases do
                caseValues.[caseName] <- v

            enums.[ed.Decl.Key] <-
                {
                    Repr = Emit.EmittedEnumRepr.NumericEnum caseValues
                }

        // A project-local seq class's `GetEnumerator` RETURN type is the enumerator over
        // the class's declaring typars; keep that template per class key. Read off THIS
        // file's decls — `env.Classes` is not yet populated at the field pass.
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
            match fs.ClosureScope with
            | ValueSome d -> provider.EnterClosureTyparScope d
            | ValueNone -> ()

            // An ungrounded type constructor in a field type (a closure capture whose element typar
            // never resolved, say) surfaces here as an opaque encoder failure; name the
            // field + type so the front-end grounding gap is pinpointable.
            let fieldSig =
                try
                    match fs.Key with
                    | FieldKey.ClosureCached name ->
                        provider.ClosureSelfFieldSignature(toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure name)))
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

            let h = ctx.AddField(fs.Attrs, fs.Name, fieldSig)

            if fs.ClosureScope.IsSome then
                provider.ExitClosureTyparScope()

            fieldDefHandles.Add(fs.Key, h)

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

            enums.[sed.Decl.Key] <-
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
                    // The flat CLR arg count; the argument split uses `Groups.Length`,
                    // which can be smaller — a tupled group is one application, N params.
                    ParamArity = List.length fn.Params
                    Groups = fn.Groups
                    ResultTy = fn.ResultTy
                    Typars = plan.StaticFnTypars.[fn.Key]
                    ParamTys = fn.Params |> List.map (fun p -> p.Ty)
                    ReturnsVoid = fn.ReturnsVoid
                    // The frozen typar bounds, from which the call site solves the
                    // phantom method-typar slots no parameter or result mentions.
                    Constraints = fn.Constraints
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
    // value-type-ness + equality/hashing/compare. No `BeforeFieldInit` — there is no
    // `.cctor`, the case fields being `literal`s in the `Constant` table.
    let enumAttrs =
        TypeAttributes.Class
        ||| TypeAttributes.Public
        ||| TypeAttributes.Sealed
        ||| TypeAttributes.AutoLayout
        ||| TypeAttributes.AnsiClass

    // A string/mixed enum's `[<Struct>]` wrapper: a sealed value type with NO
    // `BeforeFieldInit` — its `.cctor` materialises the case singletons and must run
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

    // Nested visibility REPLACES the 3-bit visibility field rather than adding to it, so
    // a nested type's `Public` becomes `NestedPublic`.
    let nestedAttrsOf (enclosing: TypeSlotKey voption) (attrs: TypeAttributes) =
        match enclosing with
        | ValueNone -> attrs
        | ValueSome _ -> (attrs &&& ~~~TypeAttributes.VisibilityMask) ||| TypeAttributes.NestedPublic

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

    // `Param` rows are one global table referenced by each `MethodDefinition.ParamList`,
    // so they must be added in method order — call this immediately before each
    // `AddMethodWithParamList`. Returns the first `Param` handle, past-the-end if none.
    let addParams (names: string list) : ParameterHandle =
        let firstParam = ctx.NextParamHandle
        names |> List.iteri (fun i n -> ctx.AddParameter(i + 1, n) |> ignore)
        firstParam

    // `GenericParam` rows can't be added inline: SRM requires them globally sorted by
    // `CodedIndex.TypeOrMethodDef(owner)`, and a method owner can sort BEFORE its
    // declaring type. Collect them and emit sorted once every handle exists.
    let genericParams = ResizeArray<EntityHandle * int * string>()

    // Everything else a `TypeDefinition` row needs comes from the layout.
    let typeRowExtras = Dictionary<TypeSlotKey, TypeRowExtras>()

    member _.Provider = provider
    member _.Icodegen = icodegen
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
            let memberTable = Dictionary<string, Emit.EmittedMember list>()

            methods
            |> List.iteri (fun i m ->
                let paramTys = abstractMethodParamTys m
                let _, retTy = uncurry m.Signature

                let handle = toEntity (this.MethodDef(MethodKey.InterfaceMethod(td.Key, i)))

                let em: Emit.EmittedMember =
                    {
                        Handle = handle
                        IsStatic = false
                        ParamArity = List.length paramTys
                        // The IL method name (a property → its `get_<Name>` getter);
                        // the use-site table below stays keyed by the bare member name.
                        MetaName = if m.IsProperty then "get_" + m.Name else m.Name
                        ParamTys = paramTys
                        RetTy = retTy
                        MethodTyparCount = m.MethodTypeParams.Length
                    }

                memberTable.[m.Name] <-
                    match memberTable.TryGetValue m.Name with
                    | true, existing -> existing @ [ em ]
                    | false, _ -> [ em ]

                this.AddPrepared(
                    MethodKey.InterfaceMethod(td.Key, i),
                    {
                        Signature = abstractMethodSignature provider m
                        BodyOffset = -1
                        ParamNames = argNames (List.length paramTys)
                        MethodTypars = [ for n in m.MethodTypeParams -> n.TrimStart('\'') ]
                    }
                )
            )

            interfaces.[td.Key] <-
                {
                    Name = td.Name
                    Typars = EqArray.toList td.TypeParams
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
            // equality, not the `.cctor` — here the literals come off `sed.Cases`.
            let backingField, caseFields =
                match enums.[td.Key].Repr with
                | Emit.EmittedEnumRepr.StructEnum(_, bf, cf, _) -> bf, cf
                | other -> failwithf "Emit: struct enum '%A' has a non-struct repr %A" td.Key other

            // The single-arg value-type `.ctor(value)` storing the backing field.
            let ctorBody =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStructCtor [ backingField ]))

            this.AddPrepared(
                MethodKey.NominalCtor td.Key,
                {
                    Signature = provider.RecordCtorSignature [ fieldTy ]
                    BodyOffset = ctorBody
                    ParamNames = [ "value" ]
                    MethodTypars = []
                }
            )

            // The `.cctor` constructs each case singleton in declaration order.
            let ctorHandle = toEntity (layoutHandles.MethodDefOf(MethodKey.NominalCtor td.Key))

            let cctorCases =
                [
                    for (caseName, lit) in sed.Cases ->
                        caseFields.[caseName], EmitResolve.enumLiteralPush icodegen.TypeToken ctx.UserString lit
                ]

            let cctorBody =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStructEnumCctor ctorHandle cctorCases))

            this.AddPrepared(
                MethodKey.NominalCctor td.Key,
                {
                    Signature = provider.CctorSignature()
                    BodyOffset = cctorBody
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
    // the enclosing method's `FTTypar(Method, i)` re-projects onto this class's `!i`.
    member this.PrepareClosures(f: FileEmit) =
        for c in f.Layout.Closures do
            let captureFields = Dictionary<BoundVarId, EntityHandle>()
            let isGenericClosure = c.Typars > 0
            // This closure's self-instantiation over its OWN typars (`!0 … !{n-1}`), for
            // the capture-field `MemberRef`s on its self-`TypeSpec`. A closure typar is
            // its own declaring typar, so this encodes `!i` at any closure-scope offset.
            let selfArgs = [ for i in 0 .. c.Typars - 1 -> FTTypar(TyparAxis.Declaring, i) ]

            if isGenericClosure then
                provider.EnterClosureTyparScope c.DeclaringTypars

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

            // A `Stack` closure's ctor does NOT chain `System.Object::.ctor` — value
            // types have none. A captureless one's ctor is a bare `ret`: construction is
            // by-value (`initobj`), so it is never called, but the row stays for layout.
            let isStack = c.IsValueStruct

            let ctorBodyOffset =
                if isStack then
                    Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStructCtor fieldHandles))
                else
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildClosureCtor provider.ObjectCtorRef fieldHandles))

            let invokeBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildClosureInvoke f.EmitCtx c captureFields))

            this.AddPrepared(
                MethodKey.ClosureCtor c.Name,
                {
                    Signature = provider.ClosureCtorSignature(List.map snd c.Captures)
                    BodyOffset = ctorBodyOffset
                    ParamNames = argNames (List.length c.Captures)
                    MethodTypars = []
                }
            )

            // A flat closure's `Invoke` takes all `FunArity` params
            // (`Invoke(arg0, …, arg{N-1}) : result`); arity 1 reduces to `Invoke(arg0)`.
            let invokeSignature, invokeParamNames =
                let paramTys = c.ParamTy :: (c.ExtraParams |> List.map (fun (_, ty, _) -> ty))
                let names = [ for i in 0 .. c.FunArity - 1 -> sprintf "arg%d" i ]
                provider.InvokeSignatureN(paramTys, c.ResultTy), names

            this.AddPrepared(
                MethodKey.ClosureInvoke c.Name,
                {
                    Signature = invokeSignature
                    BodyOffset = invokeBodyOffset
                    ParamNames = invokeParamNames
                    MethodTypars = []
                }
            )

            // A cached closure's `.cctor` `newobj`s the ctor once and `stsfld`s the
            // singleton field that construction sites `ldsfld`.
            if Emit.closureIsCached c then
                let ctorHandle = toEntity (layoutHandles.MethodDefOf(MethodKey.ClosureCtor c.Name))
                let cachedField = toEntity (fieldDefHandles.[FieldKey.ClosureCached c.Name])

                let cctorBodyOffset =
                    Cil.buildBody
                        encodeLocals
                        bodyStream
                        (IlIr.lower (Emit.buildCachedClosureCctor ctorHandle cachedField))

                this.AddPrepared(
                    MethodKey.ClosureCctor c.Name,
                    {
                        Signature = provider.CctorSignature()
                        BodyOffset = cctorBodyOffset
                        ParamNames = []
                        MethodTypars = []
                    }
                )

            // `Fun\`2<param, result>` interface `TypeSpec` — the closure ambient is still
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

            if isGenericClosure then
                let closureHandle = toEntity (layoutHandles.TypeDefOf(TypeSlotKey.Closure c.Name))

                for i in 0 .. c.Typars - 1 do
                    genericParams.Add(closureHandle, i, sprintf "T%d" i)

                provider.ExitClosureTyparScope()

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
            // `FTTypar(Method, i)`, which the encoder maps to `!!i` — no ambient window.
            let typarCount = staticMethods.[fn.Key].Typars

            // Retype the body so a reference to a verdict module value, or an inline
            // transformer call, dispatches on the `<closure>$` value-struct nominal
            // rather than the frozen function type.
            let fn = { fn with Body = retypeBody fn.Body }

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticMethod emitCtx fn))

            let paramTys = fn.Params |> List.map (fun p -> p.Ty)

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
                    BodyOffset = bodyOffset
                    ParamNames = argNames (List.length fn.Params)
                    MethodTypars = [ for i in 0 .. typarCount - 1 -> sprintf "T%d" i ]
                }
            )

        // A module class's `.cctor` `stsfld`s its module values in declaration order — the
        // static analogue of a class's `static let` cctor.
        let prepareModuleClassCctor (h: Emit.ModuleClassKey) =
            let lets =
                [
                    for mv in ModuleClassPlan.moduleClassValues plan h ->
                        Emit.PreambleStep.Store(moduleValueFields.[mv.Key], retypeBody mv.Init)
                ]

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx lets))

            this.AddPrepared(
                MethodKey.ModuleClassCctor h,
                {
                    Signature = provider.CctorSignature()
                    BodyOffset = bodyOffset
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

            let bodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildStaticCctor emitCtx lets))

            this.AddPrepared(
                MethodKey.ProgramCctor,
                {
                    Signature = provider.CctorSignature()
                    BodyOffset = bodyOffset
                    ParamNames = []
                    MethodTypars = []
                }
            )

        for slot in plan.MethodPlan do
            match slot with
            | ModuleClassCctor h -> prepareModuleClassCctor h
            | ModuleClassFn fn -> prepareStaticFn fn
            | ProgramCctor -> prepareProgramCctor ()

    /// `Main` belongs to the ENTRY file only — the one whose layout carries the entry
    /// point. A non-entry file contributes no `Main` row, so this is a no-op for it.
    member this.PrepareMain(f: FileEmit) =
        if f.Layout.EmitEntryPoint then
            let mainDecls = f.Layout.Lowered |> List.map f.Verdict.RetypeDecl

            let mainBodyOffset =
                Cil.buildBody encodeLocals bodyStream (IlIr.lower (Emit.buildMain f.EmitCtx mainDecls))

            this.AddPrepared(
                MethodKey.Main,
                {
                    Signature = mainSignature ()
                    BodyOffset = mainBodyOffset
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

            let handle =
                ctx.AddMethodWithParamList(row.Attrs, row.Name, p.Signature, p.BodyOffset, firstParam)

            let predicted = layoutHandles.MethodDefOf row.Key

            if handle <> predicted then
                failwithf
                    "Layout: method '%s' predicted MethodDef row %d <> actual %d"
                    row.Name
                    (MetadataTokens.GetRowNumber(toEntity predicted))
                    (MetadataTokens.GetRowNumber(toEntity handle))

            p.MethodTypars
            |> List.iteri (fun i n -> genericParams.Add(toEntity handle, i, n))

        if prepared.Count <> layoutHandles.TotalMethods then
            failwithf
                "Layout: %d methods were prepared but the layout owns %d rows"
                prepared.Count
                layoutHandles.TotalMethods

    member this.Finalise() : ClrArtifact =
        let rowOf (h: EntityHandle) = MetadataTokens.GetRowNumber h

        let verifyTypeHandle (slot: TypeSlot) (actual: TypeDefinitionHandle) =
            let predicted = layoutHandles.TypeDefOf slot.Key

            if predicted <> actual then
                failwithf
                    "Layout: slot '%s' predicted TypeDef row %d <> actual %d"
                    slot.MetaName
                    (rowOf (toEntity predicted))
                    (rowOf (toEntity actual))

        // Written while the NESTED type is being written, not its enclosing one, so the
        // `NestedClass` table comes out sorted by the nested handle — what SRM validates.
        // The enclosing handle exists already: it precedes this node in the flattening.
        let addNesting (node: TypeNode) (typeHandle: TypeDefinitionHandle) =
            match node.Enclosing with
            | ValueNone -> ()
            | ValueSome encl -> ctx.AddNestedType(typeHandle, layoutHandles.TypeDefOf encl)

        // Union, record, class and closure `TypeDefinition` rows share one recipe.
        // Walking the layout in order keeps the `InterfaceImpl` / `GenericParam` rows
        // ascending (sorted by `Class` / `TypeOrMethodDef`).
        let addNominalRow (node: TypeNode) (attrs: TypeAttributes) (isByRefLike: bool) =
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

            // A `[<IsByRefLike>]` value type carries the marker attribute — a
            // parameterless custom attribute, blob = prolog `0x0001` + zero named args =
            // `01 00 00 00`. There is no `TypeAttributes` bit for it.
            if isByRefLike then
                let blob = BlobBuilder()
                blob.WriteUInt16(1us)
                blob.WriteUInt16(0us)

                ctx.AddCustomAttribute(toEntity typeHandle, provider.IsByRefLikeAttrCtor, blob)
                |> ignore

            for iface in extras.Interfaces do
                ctx.AddInterfaceImplementation(typeHandle, iface)

            slot.Typars
            |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n))

        for node in layout.Types do
            let slot = node.Slot

            match slot.Kind with
            | TypeSlotKind.ModulePseudo ->
                // `<Module>` points at method row 1 — the first real method, or
                // past-the-end of the empty table in a no-method library.
                ctx.AddModuleType(layoutHandles.FirstMethodOf slot.Key)

            | TypeSlotKind.Interface ->
                // Interfaces have no fields, so the field range is empty (the
                // prefix sum — row 1, every field-bearing kind follows).
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

                slot.Typars
                |> List.iteri (fun i n -> genericParams.Add(toEntity typeHandle, i, n))

            // Unions and records are always sealed; a class opts in via `[<Sealed>]` /
            // `[<Struct>]`. A record opts into value-type emission via `[<Struct>]`, and
            // is never byref-like.
            | TypeSlotKind.Union -> addNominalRow node (classAttrsOf true false) false
            | TypeSlotKind.Record valueKind ->
                addNominalRow node (classAttrsOf true (valueKind <> ClassValueKind.RefType)) false

            | TypeSlotKind.Class(isSealed, valueKind) ->
                let isValueType = valueKind <> ClassValueKind.RefType
                let isByRefLike = valueKind = ClassValueKind.RefStruct
                addNominalRow node (classAttrsOf isSealed isValueType) isByRefLike

            // A numeric enum: base `System.Enum`, no interfaces, no methods — so it has
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
            // Its `System.ValueType` base rides in `TypeRowExtras`; never byref-like.
            | TypeSlotKind.StructEnum _ -> addNominalRow node structEnumAttrs false

            // Each closure implements its `Vesper.Fun\`2<param, result>` interface. Its
            // `GenericParam` rows were collected under the closure-typar ambient, so
            // none are added here.
            | TypeSlotKind.Closure ->
                let extras =
                    match typeRowExtras.TryGetValue slot.Key with
                    | true, e -> e
                    | _ -> failwithf "Layout: closure slot '%s' was never prepared" slot.MetaName

                // A `Stack` closure is a `[<Struct>]` value type — sealed, sequential
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

            // Named-module classes: one static class per `module Foo`, nested in its
            // parent's module class when the module nests. One owning module values takes its
            // own `FieldList`; a value-less one's empty range points past the previous.
            | TypeSlotKind.ModuleClass hasCctor ->
                let typeHandle =
                    ctx.AddProgramType(
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
                    ctx.AddProgramType(
                        moduleClassAttrsOf hasCctor,
                        slot.Namespace,
                        slot.MetaName,
                        provider.ObjectType,
                        layoutHandles.FirstFieldOf slot.Key,
                        layoutHandles.FirstMethodOf slot.Key
                    )

                verifyTypeHandle slot typeHandle

        // Every handle now exists: add `GenericParam` rows in the order SRM
        // validates — by the owner's `TypeOrMethodDef` coded index, then index.
        genericParams
        |> Seq.sortBy (fun (owner, index, _) -> (CodedIndex.TypeOrMethodDef owner, index))
        |> Seq.iter (fun (owner, index, name) -> ctx.AddGenericParameter(owner, index, name) |> ignore)

        let pe =
            if layout.EmitEntryPoint then
                ctx.Serialize(layoutHandles.MethodDefOf MethodKey.Main)
            else
                ctx.SerializeLibrary()

        {
            AssemblyName = project.AssemblyName
            OutputPath = project.OutputPath
            Pe = pe
            ReferencedAssemblies = ctx.ReferencedAssemblyNames
            FSharpCoreDependencies = icodegen.FSharpCoreDependencies()
        }
