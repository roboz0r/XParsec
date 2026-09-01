namespace XParsec.FSharp.Codegen.Clr

open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// One file's layout-derived handles, published to the provider before any signature is
/// encoded, so a signature can `encodeType` a nominal whose `TypeDefinition` row does not
/// exist yet and a cross-file call resolves to the local `MethodDef`.
module internal NominalRegistration =

    /// Each case's payload as `(metadata name, stored type)`: a flat union's placement
    /// slots, or a hierarchy case's own fields, which `UnionMember.Field` reparents onto
    /// the case type.
    let private caseFields (ud: UnionDecl) (c: Frozen.TUnionCase) : (string * FrozenType) list =
        match ud.Placements with
        | ValueSome p ->
            [
                for a in p.CaseAccess c ->
                    let s = UnionFieldAccess.slot a
                    s.MetaName, s.Ty
            ]
        | ValueNone -> List.zip (ud.FieldNames c) [ for (_, t) in c.Fields -> t ]

    /// A hierarchy union's case types: the nested `TypeDef` handle and, for a generic
    /// union, its shape. A case redeclares the union's typars and adds none, so it
    /// registers over them as a generic class, minting the `TypeSpec`, `.ctor` and field refs.
    let private registerUnionCases (provider: ClrProvider) (handles: LayoutHandles) (ud: UnionDecl) : unit =
        let td = ud.Decl

        for c in ud.Cases do
            let caseKey = UnionCaseType.key td.TypeKey c.Name

            provider.RegisterUserType(caseKey, toEntity (handles.TypeDefOf(TypeSlotKey.UnionCase(td.Key, c.Name))))

            if not td.TypeParams.IsEmpty then
                let fields = caseFields ud c
                provider.RegisterGenericClass(caseKey, TTypeParam.names td.TypeParams, List.length fields, fields)

    let private registerUnion (provider: ClrProvider) (handles: LayoutHandles) (ud: UnionDecl) : unit =
        let td = ud.Decl
        provider.RegisterUserType(td.TypeKey, toEntity (handles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

        if ud.ValueKind.IsValueType then
            provider.RegisterUserValueType td.TypeKey

        if ud.IsHierarchy then
            registerUnionCases provider handles ud

        if not td.TypeParams.IsEmpty then
            let shape = [ for c in ud.Cases -> c.Name, caseFields ud c ]

            let slots =
                match ud.Placements with
                | ValueSome p -> [ for s in p.Slots -> s.Ty ]
                | ValueNone -> []

            provider.RegisterGenericUnion(td.TypeKey, TTypeParam.names td.TypeParams, shape, ud.ValueKind, slots)

    let private registerRecord (provider: ClrProvider) (handles: LayoutHandles) (rd: RecordDecl) : unit =
        let td = rd.Decl
        provider.RegisterUserType(td.TypeKey, toEntity (handles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

        if rd.ValueKind.IsValueType then
            provider.RegisterUserValueType td.TypeKey

        if not td.TypeParams.IsEmpty then
            let shape = [ for f in rd.Fields -> f.Name, f.Type ]
            provider.RegisterGenericRecord(td.TypeKey, TTypeParam.names td.TypeParams, shape)

    let private registerClass (provider: ClrProvider) (handles: LayoutHandles) (cd: ClassDecl) : unit =
        let td = cd.Decl
        provider.RegisterUserType(td.TypeKey, toEntity (handles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

        if cd.ValueKind.IsValueType then
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

            provider.RegisterGenericClass(
                td.TypeKey,
                TTypeParam.names td.TypeParams,
                List.length ctorParamFields,
                shape
            )

    /// Every nominal, module function and generic closure one file declares.
    let apply (provider: ClrProvider) (handles: LayoutHandles) (file: FileLayout) : unit =
        let partitioned = file.Partitioned

        for ud in partitioned.Unions do
            registerUnion provider handles ud

        for rd in partitioned.Records do
            registerRecord provider handles rd

        for cd in partitioned.Classes do
            registerClass provider handles cd

        // Interfaces register their `TypeDef` too, so one referencing another as a member's
        // type (`IStructuralFormattable.Format(IFormatSink)`) resolves like any nominal.
        // A generic one (`IStructSeq<'E>`) also needs its slots minted on a `TypeSpec`.
        for (td, _) in partitioned.Interfaces do
            provider.RegisterUserType(td.TypeKey, toEntity (handles.TypeDefOf(TypeSlotKey.Nominal td.Key)))

            if not td.TypeParams.IsEmpty then
                provider.RegisterGenericClass(td.TypeKey, TTypeParam.names td.TypeParams, 0, [])

        // Numeric enums (a `System.Enum` subclass) and string/mixed ones (a `[<Struct>]`
        // wrapper) are both project-local value types → `ELEMENT_TYPE_VALUETYPE`.
        for td in
            (partitioned.Enums |> List.map (fun ed -> ed.Decl))
            @ (partitioned.StructEnums |> List.map (fun sed -> sed.Decl)) do
            provider.RegisterUserType(td.TypeKey, toEntity (handles.TypeDefOf(TypeSlotKey.Nominal td.Key)))
            provider.RegisterUserValueType td.TypeKey

        // Register this file's module functions, Program-class ones included, so a SIBLING
        // file's cross-file call resolves to the local `MethodDef` instead of an
        // `AssemblyRef`-scoped `MemberRef`. Keyed by the `SymbolKey` a reference spells.
        for fn in file.Plan.StaticFns do
            provider.RegisterLocalModuleFn(
                fn.SymbolKey,
                toEntity (handles.MethodDefOf(MethodKey.StaticFn fn.SymbolKey))
            )

        // A generic closure is a real generic `TypeDefinition`; its handle lets
        // capture-field `MemberRef`s and the construction-site `newobj` both reach it.
        // Monomorphic closures use their `Def` tokens directly.
        for c in file.Closures do
            if c.Typars > 0 then
                provider.RegisterClosure(
                    c.Name,
                    c.Typars,
                    c.DeclaringTypars,
                    c.Captures |> List.map snd,
                    c.ParamTy,
                    c.ResultTy,
                    toEntity (handles.TypeDefOf(TypeSlotKey.Closure c.Name))
                )
