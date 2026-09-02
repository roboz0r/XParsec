namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The self-reference, body-building and equality/comparison row helpers that union,
/// record and class emission all reach for.
module internal NominalShared =

    let typarMarkersOf (td: TastAccessor.TypeDecl) : FrozenType list = declaringMarkers td.TypeParams.Length

    /// A reference to one of this type's own members (field / tag / ctor): a generic
    /// type reaches it through a `MemberRef` on the open self-`TypeSpec`
    /// (`Box\`1<!0>::n`), a monomorphic type through the resolved `Def` token.
    let selfMemberRef
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (kind: UserMemberKind)
        (monoHandle: EntityHandle)
        : EntityHandle =
        if not td.TypeParams.IsEmpty then
            asm.Icodegen.UserGenericMemberRef(td.TypeKey, typarMarkersOf td, kind)
        else
            monoHandle

    let bodyOf (asm: Assembler) (ir: ILBody) : PreparedBody = asm.MethodBody ir

    /// A body where the type supplies one, `PreparedBody.Abstract` where it leaves the slot
    /// for an implementor.
    let bodyOrAbstract (asm: Assembler) (ir: ILBody voption) : PreparedBody =
        match ir with
        | ValueSome body -> bodyOf asm body
        | ValueNone -> PreparedBody.Abstract

    let selfTyOf (input: NominalEmissionInput) (td: TastAccessor.TypeDecl) (ts: FrozenType list) : FrozenType =
        match input with
        | NominalEmissionInput.Union _ -> FTUnion(td.TypeKey, EqArray.ofList ts)
        | NominalEmissionInput.Record _ -> FTRecord(td.TypeKey, EqArray.ofList ts)
        | NominalEmissionInput.Class _ -> FTClass(td.TypeKey, EqArray.ofList ts)

    /// The handle the equality/comparison bodies `isinst`/`unbox.any` against: a
    /// generic type's open self-`TypeSpec`, a mono type's `TypeDef`.
    let selfTypeHandleOf (asm: Assembler) (input: NominalEmissionInput) (td: TastAccessor.TypeDecl) : EntityHandle =
        let provider = asm.Provider

        if td.TypeParams.IsEmpty then
            provider.UserTypeHandle td.TypeKey
        else
            match input with
            | NominalEmissionInput.Union _ -> provider.GenericUnionSelfSpec td.TypeKey
            | NominalEmissionInput.Record _ -> provider.GenericRecordSelfSpec td.TypeKey
            | NominalEmissionInput.Class _ -> provider.UserTypeHandle td.TypeKey

    /// A record's fields paired with their names, in declaration order: the walk every
    /// structural body over the record takes. Each call mints a generic record's
    /// `MemberRef` rows afresh; call once and share the result across bodies.
    let recordFieldRefs (asm: Assembler) (td: TastAccessor.TypeDecl) : (string * EmitStructural.StructuralField) list =
        [
            for (name, h, fty) in asm.Records.[td.TypeKey].Fields ->
                name,
                {
                    Path =
                        [
                            selfMemberRef asm td (UserMemberKind.RecordMember(RecordMember.Field name)) h
                        ]
                    Ty = fty
                    Cast = ValueNone
                }
        ]

    /// The `_tag` ref in the union's own scope, for a body on the union or on one of its
    /// case types. Callable exactly where `EmittedUnion.Tag` holds a handle.
    let tagFieldRefOf (asm: Assembler) (td: TastAccessor.TypeDecl) : EntityHandle =
        match asm.Unions.[td.TypeKey].Tag with
        | ValueSome tag -> selfMemberRef asm td (UserMemberKind.UnionMember UnionMember.Tag) tag.Field
        | ValueNone -> failwithf "Emit: union '%s' is not discriminated by a tag field" td.Name

    /// `GetHashCode` + `Equals(object)` override + typed `Equals(Self)`. Union and
    /// record differ only in the body builders; the row signatures are identical. A body
    /// of `ValueNone` is a slot the declaring type leaves abstract for its case types.
    let prepareEqualityTriple
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (selfTyMarkers: FrozenType)
        (getHashCodeIr: ILBody voption)
        equalsObjIr
        (equalsTypedIr: ILBody voption)
        =
        asm.AddPrepared(
            MethodKey.EqGetHashCode td.Key,
            {
                Signature = asm.Provider.GetHashCodeOverrideSignature()
                Body = bodyOrAbstract asm getHashCodeIr
                ParamNames = []
                MethodTypars = []
            }
        )

        asm.AddPrepared(
            MethodKey.EqEqualsObj td.Key,
            {
                Signature = asm.Provider.EqualsOverrideSignature()
                Body = bodyOf asm equalsObjIr
                ParamNames = [ "obj" ]
                MethodTypars = []
            }
        )

        asm.AddPrepared(
            MethodKey.EqEqualsTyped td.Key,
            {
                Signature = asm.Provider.EqualsTypedSignature selfTyMarkers
                Body = bodyOrAbstract asm equalsTypedIr
                ParamNames = [ "other" ]
                MethodTypars = []
            }
        )

    /// The comparison pair: typed `CompareTo(Self)` first (its handle feeds
    /// `CompareTo(object)`'s body), then the `CompareTo(object)` override. `ValueNone`
    /// leaves the typed slot abstract.
    let prepareComparisonPair
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (selfTyMarkers: FrozenType)
        (compareToTypedIr: ILBody voption)
        compareToObjIr
        =
        asm.AddPrepared(
            MethodKey.CmpCompareToTyped td.Key,
            {
                Signature = asm.Provider.CompareToTypedSignature selfTyMarkers
                Body = bodyOrAbstract asm compareToTypedIr
                ParamNames = [ "other" ]
                MethodTypars = []
            }
        )

        asm.AddPrepared(
            MethodKey.CmpCompareToObj td.Key,
            {
                Signature = asm.Provider.CompareToOverrideSignature()
                Body = bodyOf asm compareToObjIr
                ParamNames = [ "obj" ]
                MethodTypars = []
            }
        )

    /// The self-shape inputs the synthesised equality / comparison bodies share.
    type StructuralSelf =
        {
            SelfType: EntityHandle
            SelfTy: FrozenType
            Members: StructuralMembers
        }
