namespace XParsec.FSharp.Codegen.Clr

open System.Reflection.Metadata
open XParsec.FSharp.SemanticAnalysis
open AssemblerScaffold

/// The self-reference, body-building and equality/comparison row helpers that union,
/// record and class emission all reach for.
module internal NominalShared =

    let typarMarkersOf (td: TastAccessor.TypeDecl) : FrozenType list =
        declaringMarkers td.TypeKey td.TypeParams.TypeArity

    /// A reference to a member of `parent`, a type registered over `td`'s own typars: the
    /// type itself, or a case type or nested value type of a union. A generic `td` reaches
    /// it through a `MemberRef` on the open `TypeSpec` (`Box\`1<!0>::n`), a monomorphic
    /// one through the resolved `Def` token.
    let memberRefOn
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (parent: TypeKey)
        (kind: UserMemberKind)
        (monoHandle: EntityHandle)
        : EntityHandle =
        if td.TypeParams.HasTypeTypars then
            asm.Icodegen.UserGenericMemberRef(parent, typarMarkersOf td, kind)
        else
            monoHandle

    /// A reference to one of this type's own members (field / tag / ctor).
    let selfMemberRef
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (kind: UserMemberKind)
        (monoHandle: EntityHandle)
        : EntityHandle =
        memberRefOn asm td td.TypeKey kind monoHandle

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

        if not td.TypeParams.HasTypeTypars then
            provider.UserTypeHandle td.TypeKey
        else
            match input with
            | NominalEmissionInput.Union _ -> provider.GenericUnionSelfSpec td.TypeKey
            | NominalEmissionInput.Record _ -> provider.GenericRecordSelfSpec td.TypeKey
            | NominalEmissionInput.Class _ -> provider.UserTypeHandle td.TypeKey

    /// A record's fields paired with their names, in declaration order: the walk every
    /// structural body over the record takes. Each call mints a generic record's
    /// `MemberRef` rows afresh; call once and share the result across bodies. Each `Path`
    /// reaches the backing field directly.
    let recordFieldRefs (asm: Assembler) (td: TastAccessor.TypeDecl) : (string * EmitStructural.StructuralField) list =
        [
            for f in asm.Records.[td.TypeKey].Fields ->
                f.Name,
                {
                    Path =
                        [
                            selfMemberRef asm td (UserMemberKind.RecordMember(RecordMember.Field f.Name)) f.Field
                        ]
                    Ty = f.Ty
                    Cast = ValueNone
                    Compare = asm.FieldCompareOf f.Ty
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


    /// The type's own `name(Self) : retTy` structural method, as an `object`-typed override
    /// calls it: the `MethodDef` at a monomorphic type, else the `MemberRef` on the
    /// instantiated self-`TypeSpec`.
    let private typedStructuralMethod
        (asm: Assembler)
        (td: TastAccessor.TypeDecl)
        (self: StructuralSelf)
        (name: string)
        (retTy: FrozenType)
        (key: MethodKey)
        : EntityHandle =
        selfMemberRef
            asm
            td
            (UserMemberKind.Member(name, false, 0, [ self.SelfTy ], retTy))
            (toEntity (asm.MethodDef key))

    /// The type's own typed `Equals(Self)`.
    let equalsTyped (asm: Assembler) (td: TastAccessor.TypeDecl) (self: StructuralSelf) : EntityHandle =
        typedStructuralMethod asm td self "Equals" RuntimeNames.boolTy (MethodKey.EqEqualsTyped td.Key)

    /// The type's own typed `CompareTo(Self)`.
    let compareToTyped (asm: Assembler) (td: TastAccessor.TypeDecl) (self: StructuralSelf) : EntityHandle =
        typedStructuralMethod asm td self "CompareTo" RuntimeNames.intTy (MethodKey.CmpCompareToTyped td.Key)
