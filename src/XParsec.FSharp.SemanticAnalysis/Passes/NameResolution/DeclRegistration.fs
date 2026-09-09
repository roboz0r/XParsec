namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open Vesper
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRefStamp
open UnificationTranslate
open NameResolutionTypeRegistration

// The registry ENTRY of each record / enum / intrinsic-binding / abbreviation /
// measure definition, built from its claimed identity and its declaration's CST. A
// definition's declared STRUCTURE resolves here, against the types claimed ABOVE it plus its
// own `type … and …` group; member BODIES are typed later, in Unification.

module NameResolutionDeclRegistration =

    /// `[<CustomEquality>]` / `[<CustomComparison>]` on a record or union is out of scope:
    /// neither has an interface-impl side table to satisfy the `IEquatable<_>` /
    /// `IComparable<_>` the verdict promises, so a `Custom` verdict is a diagnostic instead.
    let rejectCustomOnDataType
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : unit =
        if eq = EqualityVerdict.Custom || cmp = ComparisonVerdict.Custom then
            ctx.Report(declTok, Kind.CustomEqualityOnRecordOrUnion)

    /// File `v` at the FRONT of `index.[name]`'s bucket: the newest declaration wins the slot.
    let prependToIndex (index: Dictionary<string, Block<'T>>) (name: string) (v: 'T) : unit =
        match index.TryGetValue name with
        | true, existing ->
            let buf = ResizeArray(existing.Length + 1)
            buf.Add v

            for i in existing do
                buf.Add i

            index.[name] <- Block.ofResizeArray buf
        | false, _ -> index.[name] <- Block.singleton v

    let registerRecordDecl
        (ctx: PassContext)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (fields: RecordFields<SyntaxToken>)
        : unit =
        let name = id.Name
        let declSite = id.DeclSite
        let typeParams = declaredTyparsOfTypeName ctx tn
        let typarConstraints = typarConstraintsOfTypeName tn

        let fieldInfos = ResizeArray<RecordFieldInfo>(fields.Length)

        underTyparScope
            ctx
            id.Key
            typeParams
            (fun () ->
                match typarConstraints with
                | ValueSome cs -> translateConstraints ctx cs
                | ValueNone -> ()

                for f in fields do
                    let (RecordField(attributes = fAttrs; mutableToken = mt; ident = fid; typ = ft)) = f
                    let fieldSite = AttributeSite.ofToken fid
                    ctx.DeclareAttributes(fieldSite, AttrTarget.RecordField, fAttrs)

                    fieldInfos.Add(RecordFieldInfo(ctx.NameOf fid, translateType ctx ft, mt.IsSome, fieldSite))
            )

        let fieldInfos = fieldInfos.ToArray()

        let info =
            RecordTypeInfo(name, typeParams, fieldInfos, id.DeclSite, typarConstraints, id.Key)

        // `[<Struct>]` record ⇒ value type. `struct … end` is a shape of its own, never a
        // record, so this is the whole verdict the group struct-field cycle check reaches.
        let isStruct = isStructAttributed ctx tn
        info.IsValueType <- isStruct

        info.Attributes <- ctx.ResolveAttributes(Attributes.attributesOfTypeName tn)
        Attributes.declareTypeDefn ctx info.DefnKind declSite.Tok info.Attributes

        rejectCustomOnDataType ctx declSite.Tok info.EqualitySupport info.ComparisonSupport

        TypeRegistry.registerRecord ctx.Types info

        // Stamp the decl-site key so the type-decl emitter recovers this record by its
        // arity-qualified `SymbolKey` rather than by the bare name.
        ctx.Resolution.ResolvedType.Set(declSite.Key, info.TypeKey)

        for fi in fieldInfos do
            prependToIndex ctx.Types.FieldIndex fi.Name info

    /// Register an enum's nominal identity and its case table, so a `(x: E)` annotation
    /// resolves to `TyEnum Key` and a qualified `E.C1` can validate the case name. Enums are
    /// non-generic and have no member side tables. The case values are resolved and their
    /// rejections reported here, once; Elaborate and the `.fsi` publisher read the table.
    let registerEnumDecl
        (ctx: PassContext)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (cases: EnumTypeCases<SyntaxToken>)
        : unit =
        let declSite = id.DeclSite
        let attrs = ctx.ResolveAttributes(Attributes.attributesOfTypeName tn)
        Attributes.declareTypeDefn ctx TypeDefnKind.Enum declSite.Tok attrs

        let resolved =
            Block.ofSeq (
                seq {
                    for EnumTypeCase(attributes = caseAttrs; ident = cid; constValue = v) in cases ->
                        let caseSite = AttributeSite.ofToken cid
                        ctx.DeclareAttributes(caseSite, AttrTarget.EnumCase, caseAttrs)
                        EnumCaseValues.resolveCase ctx.NameOf (fun t kind -> ctx.Report(t, kind)) caseSite cid v
                }
            )

        let info = EnumTypeInfo(id.Name, resolved, declSite, id.Key, attrs)
        TypeRegistry.registerEnum ctx.Types info

        // Record the decl-site identity so `Elaborate.tryEnumType`
        // recovers the SAME key the annotation path resolves to.
        ctx.Resolution.ResolvedType.Set(declSite.Key, info.TypeKey)

    /// Register a `type X = (# … #)` primitive BINDING: it lands in `IntrinsicBindings` as
    /// canon key → platform type id, so the name resolves to `TyConst key`. A `with member …`
    /// augmentation registers the type as a member host as well, without withdrawing it
    /// from `IntrinsicBindings`.
    let registerIntrinsicBindingDecl
        (ctx: PassContext)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (tag: ExternKind<SyntaxToken> voption)
        (instrParts: ImmutableArray<StringPart<SyntaxToken>>)
        (hasAugmentation: bool)
        : unit =
        let name = id.Name
        let typeId = PlatformTypeId(IntrinsicBindings.ilString ctx.NameOf instrParts)
        // Filed on the KEY axis alone, so a consumer holding a resolved intrinsic key
        // never has to project it back to a name. The `class` tag is stored on the same entry:
        // heritability is a property of this binding.
        ctx.Types.IntrinsicBindings.[TypeRegistry.intrinsicKeyOf ctx.Types name] <-
            {
                TypeId = typeId
                Heritable =
                    match tag with
                    | ValueSome(ExternKind.Class _) -> true
                    | ValueSome(ExternKind.Interface _)
                    | ValueNone -> false
            }

        if hasAugmentation then
            // The self-type key is the contract-sourced intrinsic identity, read through
            // `intrinsicKeyOf` so it agrees with the abbrev's use-site key even when the
            // declaring namespace is not `Vesper`. It also ADDRESSES the host table, so
            // the lookup key and the self-type key are the same one value.
            let selfKey = TypeRegistry.intrinsicKeyOf ctx.Types name
            let typeParams = declaredTyparsOfTypeName ctx tn

            ctx.Types.IntrinsicAbbrevHost.[selfKey] <-
                IntrinsicAbbrevInfo(name, typeParams, id.DeclSite, id.Key, selfKey)

        match tag with
        // Untagged `(# "…" #)` is an opaque value repr, never a base; a `class`-tagged
        // one already recorded `Heritable = true` above. Nothing extra either way.
        | ValueNone
        | ValueSome(ExternKind.Class _) -> ()
        // `(# interface "…" #)` parses but cannot be inherited: an interface goes in
        // `implements`, not `extends`, and has no base `.ctor` to chain to. Rejected
        // here; its `Heritable` is `false`, so it never reaches codegen's base path.
        | ValueSome(ExternKind.Interface _) ->
            ctx.Report(
                id.DeclSite.Tok,
                Kind.NotYetSupported(
                    sprintf
                        "a heritable external interface base ('(# interface \"…\" #)') on type '%s'; only '(# class \"…\" #)' may be inherited"
                        name
                )
            )

    /// Register a `type X = …` transparent alias. Only the ENTRY registers; the RHS is forced
    /// at GROUP CLOSE. An alias declares nothing of its own, so a `with member …`
    /// augmentation is diagnosed and dropped.
    let registerAbbreviationDecl
        (ctx: PassContext)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken>)
        (hasAugmentation: bool)
        : unit =
        let name = id.Name
        let typeParams = declaredTyparsOfTypeName ctx tn

        let attributes = ctx.ResolveAttributes(Attributes.attributesOfTypeName tn)
        Attributes.declareTypeDefn ctx TypeDefnKind.Abbrev id.DeclSite.Tok attributes

        if hasAugmentation then
            ctx.Report(
                id.DeclSite.Tok,
                Kind.Message(
                    sprintf
                        "Type abbreviation '%s' cannot carry augmentation members: only an inline-IL abbreviation ('type %s = (# \"…\" #) with member …') may declare members"
                        name
                        name
                )
            )

        let info =
            AbbreviationInfo(name, typeParams, rhs, id.DeclSite, typarConstraintsOfTypeName tn, id.Key, attributes)

        TypeRegistry.registerAbbrev ctx.Types info

    /// Register a `[<Measure>]` declaration; `rhs` is `ValueNone` for the body-less form.
    let registerMeasureDecl
        (ctx: PassContext)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken> voption)
        : unit =
        // A measure stores no attributes of its own.
        Attributes.declareTypeDefn
            ctx
            TypeDefnKind.Measure
            id.DeclSite.Tok
            (ctx.ResolveAttributes(Attributes.attributesOfTypeName tn))

        let body =
            match rhs with
            | ValueNone -> ValueNone
            | ValueSome t ->
                match CstKeys.measureOfType t with
                | ValueSome m -> ValueSome m
                | ValueNone ->
                    ctx.Report(
                        id.DeclSite.Tok,
                        Kind.NotYetSupported(
                            sprintf
                                "the body of measure abbreviation '%s'; a measure abbreviates a measure alone"
                                id.Name
                        )
                    )

                    ValueNone

        let info = MeasureInfo(id.Name, id.DeclSite, id.Key, body)

        // A measure-GENERIC declaration (`type Area<[<Measure>] 'u>`) is claimed at its arity,
        // so a bare reference to it is FS0033. Its entry is `Broken` from registration.
        if not (typarListOfTypeName ctx tn).IsEmpty then
            ctx.Report(id.DeclSite.Tok, Kind.NotYetSupported "a measure declaration with type parameters")
            info.State <- FillState.Broken

        TypeRegistry.registerMeasure ctx.Types info
