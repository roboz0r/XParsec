namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// CST is never mutated; all in-flight semantic information lives in these side tables.

module internal LocalSymbolKey =

    /// `name` is AS WRITTEN and `arity` the declared typar count: the `` `N `` spelling is
    /// rendered for metadata, never stored.
    let ofType (container: TypeContainer) (name: string) (arity: int) : TypeKey =
        SymbolKeyOps.typeKeyOfContainer container name arity

    /// A property's `ArgSig` is empty: its name is unique on a type, since properties do not
    /// overload by argument.
    let ofProperty (declKey: TypeKey) (name: string) : SymbolKey =
        SymbolKeyOps.memberKey declKey name EqArray.empty 0 MemberKind.Property

[<Sealed>]
type RecordFieldInfo(name: string, ty: SemType, isMutable: bool, declKey: NodeKey, attributes: TAttributes) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val DeclKey = declKey
    member val Attributes: TAttributes = attributes

/// Properties are read-only (get-only); an `AutoProperty` also lands here as `Property`.
[<RequireQualifiedAccess>]
type ClassMemberKind =
    | Method
    | Property

/// Per-member metadata for a class, union or interface-impl augmentation. A forward reference
/// within the same type captures this cell, so it must observe the seed → canonical transition.
[<Sealed>]
type TypeMemberInfo
    internal
    (
        name: string,
        kind: ClassMemberKind,
        isStatic: bool,
        ty: SemType,
        declSite: NodeSite,
        seedTypars: EqArray<string * TyVarId>,
        declaredTyparCount: int
    ) =
    member val Name = name
    member val Kind = kind
    member val IsStatic = isStatic
    member val Type = ty
    member val DeclSite = declSite

    member _.SeedTypars: EqArray<string * TyVarId> = seedTypars

    /// How many LEADING entries of `SeedTypars` are the member's explicitly-declared
    /// `<'C, …>` typars, in source order. The implicit ones behind them are not declared.
    member _.DeclaredTyparCount: int = declaredTyparCount

    member val private canonical: GeneralizedTypars voption = ValueNone with get, set

    member this.Generalise(gt: GeneralizedTypars) : unit =
        match this.canonical with
        | ValueNone -> this.canonical <- ValueSome gt
        | ValueSome _ -> invalidOp "member generalised twice"

    member this.CanonicalTypars: GeneralizedTypars =
        match this.canonical with
        | ValueSome gt -> gt
        | ValueNone -> GeneralizedTypars.empty

    /// Falls back to the seed because a forward reference within a class can call a member
    /// before it is generalised.
    member this.EffectiveMethodTypars: EqArray<string * TyVarId> =
        match this.canonical with
        | ValueSome gt -> EqArray.ofArray (GeneralizedTypars.toArray gt)
        | ValueNone -> seedTypars

    /// `true` when the source declares the member `override` or `default`.
    member val IsOverride: bool = false with get, set

/// `FieldNames` carries per-field names for named fields (`| Case of x: int * y: int`);
/// a positional field is `ValueNone`.
[<Sealed>]
type UnionCaseInfo
    (
        name: string,
        unionName: string,
        unionKey: TypeKey,
        fields: SemType[],
        fieldNames: string voption[],
        declKey: NodeKey,
        attributes: TAttributes
    ) =
    member val Name = name
    /// The declaring union's short name AS WRITTEN, compared against a written qualifier
    /// (`Choice.Choice1Of3`). `UnionKey`, not this, is the identity.
    member val UnionName = unionName
    member val UnionKey = unionKey
    member val Fields = fields
    member val FieldNames = fieldNames
    member val DeclKey = declKey
    member val Attributes: TAttributes = attributes

[<RequireQualifiedAccess>]
type InterfaceImplResolution =
    | Pending
    /// The interface's own type constructor and arguments. Only a nominal passes the
    /// interface-ness gate, so a resolved impl is destructured without a shape test.
    | Resolved of key: TypeKey * args: EqArray<SemType>
    /// The written type is not an interface, and that diagnostic has already been reported.
    | Rejected

module InterfaceImplResolution =
    /// The resolved interface AS A TYPE, for the consumers that instantiate or carry it whole.
    let tryIface (r: InterfaceImplResolution) : SemType voption =
        match r with
        | InterfaceImplResolution.Resolved(key, args) -> ValueSome(TyClass(key, args))
        | InterfaceImplResolution.Pending
        | InterfaceImplResolution.Rejected -> ValueNone

/// A registered `interface IFace with member …` block on a class or union. `DeclSite` is the
/// `interface` keyword.
[<Sealed>]
type ClassInterfaceImplInfo
    (
        interfaceCst: Type<SyntaxToken>,
        members: TypeMemberInfo[],
        elements: TypeDefnElements<SyntaxToken>,
        declSite: NodeSite
    ) =
    member val InterfaceCst = interfaceCst
    member val Members = members
    /// Each interface `MemberDefn` re-wrapped as a `TypeDefnElement.Member`, so the ordinary
    /// member walks consume the impl bodies unchanged.
    member val Elements = elements
    member val DeclSite = declSite
    member val Resolution: InterfaceImplResolution = InterfaceImplResolution.Pending with get, set

[<RequireQualifiedAccess>]
module ThisBinding =

    /// The name bound to `this` in a member body of a declaration with no `as` clause. Only a
    /// class can carry an `as`-bound name, so every other nominal host binds this.
    [<Literal>]
    let DefaultName = "this"

/// The shared surface a nominal type exposes to the interface-impl machinery, implemented by
/// the class, union, record and intrinsic-abbrev infos.
type IInterfaceImplHost =
    abstract member Key: SymbolKey
    abstract member TypeKey: TypeKey
    abstract member DeclSite: NodeSite
    abstract member TypeParams: EqArray<string * TyVarId>
    /// Source-text name bound to `this` inside member / impl bodies: `"this"` unless
    /// an `as`-bound variable renamed it.
    abstract member ThisName: string
    abstract member ThisKey: BoundVarKey
    abstract member InterfaceImpls: ClassInterfaceImplInfo[]
    abstract member Members: TypeMemberInfo[]
    abstract member EqualitySupport: EqualityVerdict
    abstract member ComparisonSupport: ComparisonVerdict
    /// The host's own nominal Self type at the given type args (`TyClass` for a class,
    /// `TyUnion` for a union), so `this` inside an impl body is typed exactly.
    abstract member MkSelfType: EqArray<SemType> -> SemType

/// Each `TypeParams` TyVar is a *prototype*, substituted out at every use site so independent
/// instantiations get independent variables; a bare `'a` field type IS its prototype.
[<Sealed>]
type RecordTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TyVarId>,
        fields: RecordFieldInfo[],
        declSite: NodeSite,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: TypeKey
    ) =
    member val Name = name
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val TypeParams = typeParams
    member val Fields = fields
    member val DeclSite = declSite
    member val TyparConstraints = typarConstraints
    /// Augmentation members (`with member …` / `static member …`); empty for a plain record.
    member val Members: TypeMemberInfo[] = [||] with get, set
    member _.ThisName = ThisBinding.DefaultName
    member val ThisKey = BoundVarKey.ofDeclaredThis declSite.Key
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set
    /// `[<Struct>]` record — a `System.ValueType`-based value type.
    member val IsValueType: bool = false with get, set
    /// The declaration's attributes, resolved and folded at registration.
    member val Attributes: TAttributes = EqArray.empty with get, set

    member this.DefnKind: TypeDefnKind = TypeDefnKind.ofRecord this.IsValueType

    /// The attribute-decided verdict, else `Structural`.
    member this.EqualitySupport: EqualityVerdict =
        AttributeVerdicts.equalitySupport this.DefnKind this.Attributes

    /// The attribute-decided verdict, else `NoComparison`.
    member this.ComparisonSupport: ComparisonVerdict =
        AttributeVerdicts.comparisonSupport this.DefnKind this.Attributes

    /// `[<RequireQualifiedAccess>]`: a bare `{ X = … }` does not resolve to this record.
    member this.IsRequireQualifiedAccess: bool =
        AttributeVerdicts.isRequireQualifiedAccess this.Attributes

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclSite = this.DeclSite
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyRecord(this.TypeKey, args)

[<Sealed>]
type UnionTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TyVarId>,
        cases: UnionCaseInfo[],
        declSite: NodeSite,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: TypeKey
    ) =
    member val Name = name
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val TypeParams = typeParams
    member val Cases = cases
    member val DeclSite = declSite
    member val TyparConstraints = typarConstraints
    /// Augmentation members (`with member …` / `static member …`); empty for a plain union.
    member val Members: TypeMemberInfo[] = [||] with get, set
    member _.ThisName = ThisBinding.DefaultName
    member val ThisKey = BoundVarKey.ofDeclaredThis declSite.Key
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set
    /// `[<Struct>]` union — a flat tag-discriminated value type.
    member val IsValueType: bool = false with get, set
    /// The declaration's attributes, resolved and folded at registration.
    member val Attributes: TAttributes = EqArray.empty with get, set

    member this.DefnKind: TypeDefnKind = TypeDefnKind.ofUnion this.IsValueType

    /// The attribute-decided verdict, else `Structural`.
    member this.EqualitySupport: EqualityVerdict =
        AttributeVerdicts.equalitySupport this.DefnKind this.Attributes

    /// The attribute-decided verdict, else `NoComparison`.
    member this.ComparisonSupport: ComparisonVerdict =
        AttributeVerdicts.comparisonSupport this.DefnKind this.Attributes

    /// `[<RequireQualifiedAccess>]`: `Color.Red` is then required, not a bare `Red`.
    member this.IsRequireQualifiedAccess: bool =
        AttributeVerdicts.isRequireQualifiedAccess this.Attributes

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclSite = this.DeclSite
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyUnion(this.TypeKey, args)

/// Host side-table for an inline intrinsic abbrev augmented with concrete `(# … #)`-bodied
/// members (`type widget = (# "object" #) with member …`). The abbrev keeps its `TyConst`
/// identity: this is not a nominal registration.
[<Sealed>]
type IntrinsicAbbrevInfo
    (name: string, typeParams: EqArray<string * TyVarId>, declSite: NodeSite, key: TypeKey, selfKey: TypeKey) =
    member val Name = name
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    /// The abbrev's INTRINSIC identity key (contract namespace, arity-suffixed): the key a
    /// use site resolves the abbrev name to, and what `MkSelfType` returns. Distinct from
    /// `Key`, the container-homed local nominal claim.
    member val SelfKey: TypeKey = selfKey
    member val TypeParams = typeParams
    member val DeclSite = declSite
    member val Members: TypeMemberInfo[] = [||] with get, set
    member _.ThisName = ThisBinding.DefaultName
    member val ThisKey = BoundVarKey.ofDeclaredThis declSite.Key
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclSite = this.DeclSite
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member _.EqualitySupport = EqualityVerdict.Reference
        member _.ComparisonSupport = ComparisonVerdict.NoComparison
        member this.MkSelfType args = TyConst(this.SelfKey, args)

/// An enum declaration (`type E = | C1 = v1 | …`): non-generic, no member side tables, a
/// closed named set of cases.
[<Sealed>]
type EnumTypeInfo(name: string, cases: EqArray<TEnumCase>, declKey: NodeKey, key: TypeKey, attributes: TAttributes) =
    member val Name = name
    /// The cases in declaration order, each with its resolved literal (`ValueNone` for a
    /// rejected value, reported at registration).
    member val Cases: EqArray<TEnumCase> = cases

    /// The case VALUES when every case is a string literal (`| Auto = "auto"`, `| A =
    /// ("auto")`), in declaration order; `ValueNone` for numeric / mixed / computed.
    member val CaseStringValues: string[] voption =
        let vals =
            [|
                for c in cases do
                    match c.Value with
                    | ValueSome(TEnumLiteral.String s) -> yield s
                    | _ -> ()
            |]

        if vals.Length = cases.Length && cases.Length > 0 then
            ValueSome vals
        else
            ValueNone

    member val DeclKey = declKey
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val Attributes: TAttributes = attributes

    member this.HasCase(n: string) =
        cases |> EqArray.exists (fun c -> c.Name = n)

[<RequireQualifiedAccess>]
type AbbreviationState =
    | NotFilled
    | InProgress
    | Filled of body: SemType
    /// Terminal without a body; each use site expands to a fresh type variable.
    | Broken

/// `State` advances lazily, so an abbrev can reference any other type declared in the same
/// group, whatever the declaration order within the module.
[<Sealed>]
type AbbreviationInfo
    (
        name: string,
        typeParams: EqArray<string * TyVarId>,
        rhsCst: Type<SyntaxToken>,
        declSite: NodeSite,
        typarConstraints: TyparConstraints<SyntaxToken> voption,
        key: TypeKey
    ) =
    member val Name = name
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val TypeParams = typeParams
    member val RhsCst = rhsCst
    member val DeclSite = declSite
    member val TyparConstraints = typarConstraints
    member val State: AbbreviationState = AbbreviationState.NotFilled with get, set

/// A primary- or secondary-constructor parameter. `Type` is always a `TyVar`, the
/// parameter's binding-site inference cell, even when the parameter is annotated.
[<Sealed>]
type ClassCtorParamInfo(name: string, ty: SemType, declSite: BoundVarSite) =
    member val Name = name
    member val Type = ty
    member val DeclSite = declSite

/// An explicit instance field declared with `val [mutable] x: T`. A `val` field is always
/// annotated, so `Type` is the RESOLVED declared type, not an inference cell.
[<Sealed>]
type ClassFieldInfo(name: string, ty: SemType, isMutable: bool, declSite: NodeSite) =
    member val Name = name
    member val Type = ty
    member val IsMutable = isMutable
    member val DeclSite = declSite

/// One `[static] let [mutable] [rec] x = <init>` of a class preamble. `Binding` is the WHOLE
/// CST binding: its pattern AND `argumentPats`, since `let f x = …` binds a FUNCTION value.
[<Sealed>]
type ClassLetInfo(name: string, ty: SemType, declKey: NodeKey, binding: Binding<SyntaxToken>, isRec: bool) =
    member val Name = name
    member val Type = ty
    member val DeclKey = declKey
    member val Binding = binding
    /// `let mutable`. An INSTANCE preamble bound variable is a mutable *field*, never a ref cell,
    /// so a closure over it captures `this`.
    member val IsMutable = binding.mutableToken.IsSome
    /// `let rec` — the bound variable is in scope of its OWN initialiser (and only then).
    member val IsRec = isRec

/// One entry of a class preamble, in declaration order: `static let a = f()` /
/// `static do g a` / `static let b = h()` runs in exactly that order.
[<RequireQualifiedAccess>]
type ClassPreambleEntry =
    | Let of ClassLetInfo
    | Do of Expr<SyntaxToken>

[<RequireQualifiedAccess>]
module ClassPreamble =
    let lets (entries: ClassPreambleEntry[]) : ClassLetInfo[] =
        entries
        |> Array.choose (
            function
            | ClassPreambleEntry.Let l -> Some l
            | ClassPreambleEntry.Do _ -> None
        )

/// A class's `inherit` clause as registration resolves it: the admitted parent and the
/// written base-ctor arguments (`ValueNone` for `inherit B` with no argument list).
type ClassInherit =
    {
        Parent: BaseParent
        /// `inherit Base(arg1, arg2)`'s `(arg1, arg2)` — the CST expression for the base
        /// constructor arguments.
        CtorArgs: Expr<SyntaxToken> voption
    }

/// A secondary constructor (`new(args) = SelfType(primaryArgs)`). `DeclKey` is synthetic,
/// minted from the `new` token so each overload is distinct.
[<Sealed>]
type ClassSecondaryCtorInfo(declKey: NodeKey, parms: ClassCtorParamInfo[], body: AdditionalConstrExpr<SyntaxToken>) =
    member val DeclKey = declKey
    member val Params = parms
    member val Body = body

[<Sealed>]
type ClassTypeInfo
    (
        name: string,
        typeParams: EqArray<string * TyVarId>,
        ctorParams: ClassCtorParamInfo[],
        members: TypeMemberInfo[],
        declSite: NodeSite,
        thisName: string,
        thisKey: BoundVarKey,
        baseKey: BoundVarKey,
        key: TypeKey
    ) =
    member val Name = name
    member val TypeKey: TypeKey = key
    member this.Key: SymbolKey = SymbolKey.Type this.TypeKey
    member val TypeParams = typeParams
    member val CtorParams = ctorParams
    member val Members = members
    member val DeclSite = declSite
    member val ThisName = thisName
    member val ThisKey = thisKey
    /// The `base` bound variable, for `base.M()` non-virtual dispatch and `inherit Base(args)`
    /// lowering. Always allocated, read only when `Base` is set.
    member val BaseKey = baseKey
    /// The `inherit` clause once resolved; `ValueNone` for a class with no `inherit` clause.
    member val Base: ClassInherit voption = ValueNone with get, set
    member val Declared: DeclaredClassFlags = DeclaredClassFlags.Default with get, set
    /// `static let` / `static do` in declaration order — the `.cctor` body.
    member val StaticPreamble: ClassPreambleEntry[] = [||] with get, set
    /// Instance `let` / `do` in declaration order: the END of the primary ctor, run after
    /// the base-ctor call. A class with no primary ctor cannot have one.
    member val InstancePreamble: ClassPreambleEntry[] = [||] with get, set
    member val SecondaryCtors: ClassSecondaryCtorInfo[] = [||] with get, set
    /// True when the class declares a *primary* constructor (`type T(args) =` / `type T() =`);
    /// false for the `val`-field form (`type T = val …; new(…) =`) whose only ctors are
    /// secondaries.
    member val HasPrimaryCtor: bool = true with get, set
    member val InterfaceImpls: ClassInterfaceImplInfo[] = [||] with get, set
    member val TyparConstraints: TyparConstraints<SyntaxToken> voption = ValueNone with get, set
    /// `[<Struct>]`, or the `type X = struct … end` shape.
    member val IsValueType: bool = false with get, set
    /// A project-local *interface* declaration (`type IFoo = abstract member …`: all members
    /// abstract, no ctor / fields / inherit / `let`-preamble), read off the syntactic shape.
    member val IsInterface: bool = false with get, set
    /// `[<IsByRefLike>]` — a byref-like (`ref struct`) value type; implies `IsValueType`.
    member val IsByRefLike: bool = false with get, set
    member val InstanceFields: ClassFieldInfo[] = [||] with get, set
    /// The declaration's attributes, resolved and folded at registration.
    member val Attributes: TAttributes = EqArray.empty with get, set

    member this.DefnKind: TypeDefnKind =
        TypeDefnKind.ofClassOrInterface this.IsInterface this.IsValueType

    /// The attribute-decided verdict, else `Reference` for a reference class / interface and
    /// `Structural` for a value type.
    member this.EqualitySupport: EqualityVerdict =
        AttributeVerdicts.equalitySupport this.DefnKind this.Attributes

    /// The attribute-decided verdict, else `NoComparison`.
    member this.ComparisonSupport: ComparisonVerdict =
        AttributeVerdicts.comparisonSupport this.DefnKind this.Attributes

    interface IInterfaceImplHost with
        member this.Key = this.Key
        member this.TypeKey = this.TypeKey
        member this.DeclSite = this.DeclSite
        member this.TypeParams = this.TypeParams
        member this.ThisName = this.ThisName
        member this.ThisKey = this.ThisKey
        member this.InterfaceImpls = this.InterfaceImpls
        member this.Members = this.Members
        member this.EqualitySupport = this.EqualitySupport
        member this.ComparisonSupport = this.ComparisonSupport
        member this.MkSelfType args = TyClass(this.TypeKey, args)

/// A member access on an *external* type that resolved through the provider, recorded per
/// member-access node. `IsStatic` distinguishes `Type.Member` from `value.Member`.
[<Struct>]
type ResolvedExternalMember =
    {
        Key: SymbolKey
        IsStatic: bool
        Storage: MemberStorage
        /// The member's DECLARED type in the object argument's instantiation: a `TyFun` for a
        /// method, the property type for a property.
        Signature: SemType
        /// How many arguments each application consumes: `M: a * b -> r` is `[2]`, the curried
        /// `M: a -> b -> r` is `[1; 1]`. `Signature` cannot tell those apart, and neither can
        /// `Key` — both members intern the flat `[a; b]`.
        ArgGroupWidths: EqArray<int>
        /// The resolved member's trailing optional-parameter defaults. Empty for a member
        /// with no omittable optionals.
        OptionalDefaults: OptionalDefault list
        /// A `[<Literal>]` / C# `const`'s declared value, which elaboration substitutes for
        /// the access. `ValueNone` for a member with a runtime slot to read.
        ConstValue: TConstValue voption
    }

    member m.IsValueMember = m.Storage.IsValueMember

    /// The access record for provider member `m`, with `signature` its instantiation at
    /// the use site.
    static member OfMember(m: ExternalMember, signature: SemType) : ResolvedExternalMember =
        {
            Key = SymbolKey.Member m.Key
            IsStatic = m.IsStatic
            Storage = m.Storage
            Signature = signature
            ArgGroupWidths = ExternalSignature.argGroupWidths m.Signature
            OptionalDefaults = m.OptionalDefaults
            ConstValue = m.ConstValue
        }

/// A member access on a *project-local* type that inference resolved, recorded per access
/// node. Both halves are needed to emit the call: `Key` pins WHICH overload, `DeclaringTy`
/// WHERE it lives.
[<Struct>]
type ResolvedLocalMember =
    {
        Key: MemberKey
        /// The declaring level's type at ITS type arguments, which for an INHERITED member is
        /// not the object argument's own. The object argument upcasts to it, so the call spells
        /// the type that emits the member.
        DeclaringTy: SemType
    }
