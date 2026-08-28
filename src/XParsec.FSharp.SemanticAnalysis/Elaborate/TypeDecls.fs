namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr
open XParsec.FSharp.SemanticAnalysis.ElaborateTypars
open XParsec.FSharp.SemanticAnalysis.ElaborateMembers
open XParsec.FSharp.SemanticAnalysis.ElaborateClassMembers

// Type-declaration surfacing for the Elaborate pass: one `try*Type` per host kind, each
// resolving its registered `*TypeInfo` onto a `TDecl.Type` paired with the typar env the
// decl quantifies. Every one leaves its types `TyVar`-shaped; the caller makes the cut.

module internal ElaborateTypeDecls =

    /// The `access` keyword token off a `type` definition's `TypeName`. Every other form
    /// (delegate, type extension, abstract type, parse failure) reports absence, i.e. `Public`.
    let typeDefnAccessToken (td: TypeDefn<SyntaxToken>) : SyntaxToken voption =
        let ofTn (TypeName(_, access, _, _, _, _)) = access

        match td with
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.Abbrev(typeName = tn) -> ofTn tn
        | _ -> ValueNone

    let private typeNameSimple (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.IsEmpty then
            ""
        else
            ctx.NameOf li.Idents.[li.Idents.Length - 1]

    /// The decl-site `NodeKey` for a single-segment `TypeName`, the same key name
    /// resolution stamps into `Resolution.ResolvedType`. `ValueNone` for a multi-segment
    /// name, which is never a project-local type and so never registered.
    let private typeNameDeclKey (ctx: PassContext) (tn: TypeName<SyntaxToken>) : NodeKey voption =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 1 then
            ValueSome(NodeKey.ofToken li.Idents.[0] NodeKind.DeclType)
        else
            ValueNone

    /// The decl's freeze env: the declaring typars, extended in place with every generic
    /// method's own as the members elaborate. The cut from `TyVar` is deferred and applied to
    /// the whole decl at once.
    let private mkDeclEnv (ctx: PassContext) (typeParams: EqArray<string * TyVarId>) : ResizeArray<TyVarId * SemType> =
        ResizeArray(mkDeclTyparEnv ctx.Store typeParams)

    /// What a host surfacer elaborates its members under: the decl's freeze env, its declaring
    /// typar names in declaration order, and its self-type.
    [<NoEquality; NoComparison>]
    type private DeclScope =
        {
            Env: ResizeArray<TyVarId * SemType>
            DeclTypars: string list
            SelfTy: SemType
        }

    /// The elaboration scope of a decl whose self-type `mkSelfTy` builds from the declaring
    /// typars' `TyVar` roots.
    let private mkDeclScope
        (ctx: PassContext)
        (typeParams: EqArray<string * TyVarId>)
        (mkSelfTy: EqArray<SemType> -> SemType)
        : DeclScope =
        {
            Env = mkDeclEnv ctx typeParams
            DeclTypars = [ for (n, _) in typeParams -> n ]
            SelfTy = mkSelfTy (declTyparArgs ctx.Store typeParams)
        }

    /// The member elaborator `scope` implies, which binds `this` to the decl's self-type and
    /// records each generic method's own typars into the freeze env.
    let private elaboratorOf (scope: DeclScope) : TTypeMember -> TTypeMember =
        mkMemberElaborator scope.SelfTy scope.DeclTypars scope.Env

    /// Classify an object-model body as an interface and build its methods from the
    /// resolved member signatures. An `Anon`/`Interface` body registers as a class.
    let private tryInterfaceMethods
        (ctx: PassContext)
        (name: string)
        (arity: int)
        (body: ObjectModelBody<SyntaxToken>)
        : (EqArray<string> * EqArray<TAbstractMethod> * (TyVarId * SemType) list) option =
        let allAbstractMethods =
            not body.elements.IsEmpty
            && body.elements
               |> Seq.forall (fun el ->
                   match el with
                   | TypeDefnElement.Member(MemberDefn.Member(defn = MethodOrPropDefn.AbstractSignature _)) -> true
                   | _ -> false
               )

        if body.inherits.IsSome || not body.classPreamble.IsEmpty || not allAbstractMethods then
            None
        else
            // The key of the type being LOWERED, minted from the module the walk is in: a
            // sibling module's same-named interface is a different type.
            match TypeRegistry.tryClassByKey ctx.Types (ctx.DeclaredTypeKey(name, arity)) with
            | ValueNone -> None
            | ValueSome info ->
                // The member signatures were typed under the class's typar scope, so they
                // share these prototype TyVars and the remap reaches every one.
                let env = mkDeclEnv ctx info.TypeParams

                let methods =
                    EqArray.ofSeq (
                        seq {
                            // Abstract methods and abstract PROPERTIES both become slots;
                            // `abstract member Current : int` emits as a `get_Current`
                            // getter, which a property impl binds to.
                            for m in info.Members do
                                // A generic method's own typars join the env so the
                                // backend encodes them against the METHOD, not the type.
                                if GeneralizedTypars.count m.CanonicalTypars > 0 then
                                    env.AddRange(GeneralizedTypars.methodEnv m.CanonicalTypars)

                                yield
                                    {
                                        Name = m.Name
                                        MethodTypeParams = EqArray.ofArray (GeneralizedTypars.names m.CanonicalTypars)
                                        Signature = m.Type
                                        IsProperty = (m.Kind = ClassMemberKind.Property)
                                    }
                        }
                    )

                Some(EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }), methods, List.ofSeq env)

    let private mkTypeDecl
        (name: string)
        (key: TypeKey)
        (ns: string option)
        (typars: EqArray<string>)
        (attrs: TAttributes)
        (kind: TTypeKind)
        : TDecl =
        TDecl.Type
            {
                Name = name
                TypeKey = key
                Namespace = ns
                TypeParams = typars
                Kind = kind
                Attributes = attrs
            }

    /// Surface a `TypeDefn.Union` as a `TDecl.Type` from the resolved `UnionTypeInfo`. Any
    /// declaring-type typar is remapped to a `TyTypar(Declaring, i)` marker.
    let private tryUnionType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (declKey: NodeKey voption)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TyVarId * SemType) list) option =
        // Resolve by the `SymbolKey` stamped at the decl site rather than re-deriving the
        // `(name, arity)` key: an arity-overloaded union does not resolve by bare name.
        let resolved =
            match declKey with
            | ValueSome k ->
                match ctx.Resolution.ResolvedType.TryGetValue k with
                | ValueSome key -> TypeRegistry.tryUnionByKey ctx.Types key
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone

        match resolved with
        | ValueNone -> None
        | ValueSome info ->
            let scope =
                mkDeclScope ctx info.TypeParams (fun args -> TyUnion(info.TypeKey, args))

            let env = scope.Env
            let declTypars = scope.DeclTypars
            let elaborateOne = elaboratorOf scope

            let cases =
                EqArray.ofSeq (
                    seq {
                        for c in info.Cases ->
                            let fields =
                                EqArray.ofSeq (
                                    seq {
                                        for i in 0 .. c.Fields.Length - 1 ->
                                            let nm =
                                                if i < c.FieldNames.Length then
                                                    c.FieldNames.[i]
                                                else
                                                    ValueNone

                                            nm, c.Fields.[i]
                                    }
                                )

                            {
                                Name = c.Name
                                Fields = fields
                                Attributes = c.Attributes
                            }
                    }
                )

            let members, interfaces =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            Some(
                mkTypeDecl
                    name
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    info.Attributes
                    (TTypeKind.Union
                        {
                            Cases = cases
                            Members = members
                            Interfaces = interfaces
                        }),
                List.ofSeq env
            )

    /// Surface a `TypeDefn.Enum` as a `TDecl.Type` from the registered `EnumTypeInfo`: the
    /// ordered case→literal table is recorded on the node, but the numeric / string / mixed
    /// variant is left DERIVABLE rather than stored. A mix of int and string case values is
    /// accepted with a warning.
    let private tryEnumType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (declKey: NodeKey voption)
        : (TDecl * (TyVarId * SemType) list) option =
        let resolved =
            match declKey with
            | ValueSome k ->
                match ctx.Resolution.ResolvedType.TryGetValue k with
                | ValueSome key -> TypeRegistry.tryEnumByKey ctx.Types key
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone

        match resolved with
        | ValueNone -> None
        | ValueSome info ->
            let tcases = info.Cases

            // Enum cases parse with `sepBy1`, so `[0]` is always there to pin the warning to.
            match TEnumCases.classify tcases with
            | ValueSome TEnumVariant.Mixed -> ctx.Report(tcases.[0].Tok, Kind.HeterogeneousEnum name)
            | _ -> ()

            match TEnumCases.firstKindConflict tcases with
            | ValueSome conflict ->
                ctx.Report(
                    conflict.Tok,
                    Kind.Message(
                        sprintf
                            "Enum '%s' mixes integral widths '%s' and '%s'; a CLR enum has a single underlying type"
                            name
                            conflict.Established
                            conflict.Offending
                    )
                )
            | ValueNone -> ()

            Some(mkTypeDecl name info.TypeKey ns (EqArray.ofList []) info.Attributes (TTypeKind.Enum tcases), [])

    /// Surface a `TypeDefn.Record` as a `TDecl.Type` from the resolved `RecordTypeInfo`.
    /// Field types are remapped through the declaring-type typars, as for a union.
    let private tryRecordType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (declKey: NodeKey voption)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TyVarId * SemType) list) option =
        let resolved =
            match declKey with
            | ValueSome k ->
                match ctx.Resolution.ResolvedType.TryGetValue k with
                | ValueSome key -> TypeRegistry.tryRecordByKey ctx.Types key
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone

        match resolved with
        | ValueNone -> None
        | ValueSome info ->
            let scope =
                mkDeclScope ctx info.TypeParams (fun args -> TyRecord(info.TypeKey, args))

            let env = scope.Env
            let declTypars = scope.DeclTypars
            let elaborateOne = elaboratorOf scope

            let fields =
                EqArray.ofSeq (
                    seq {
                        for f in info.Fields ->
                            {
                                Name = f.Name
                                Type = f.Type
                                IsMutable = f.IsMutable
                                Attributes = f.Attributes
                            }
                    }
                )

            let members, interfaces =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            let valueKind =
                if info.IsValueType then
                    RecordValueKind.Struct
                else
                    RecordValueKind.RefType

            Some(
                mkTypeDecl
                    name
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    info.Attributes
                    (TTypeKind.Record
                        {
                            Fields = fields
                            Members = members
                            Interfaces = interfaces
                            ValueKind = valueKind
                        }),
                List.ofSeq env
            )

    /// Translated through the CLASS `info` wherever the elements were written, so `this` and
    /// ctor-param references rewrite identically inside an `interface … with` body.
    let private elaborateClassElements
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (elaborateOne: TTypeMember -> TTypeMember)
        (elements: TypeDefnElements<SyntaxToken>)
        : EqArray<TTypeMember> =
        let declaring = classDeclaringType ctx info

        EqArray.ofSeq (
            seq {
                for el in elements do
                    for m in translateMemberElement ctx declaring el do
                        yield elaborateOne m
            }
        )

    /// The resolved interface `TyClass` carries THIS class's declaring typars as roots, so a
    /// generic arg like `IEnumerable<'T>` encodes against this class's own typars after the
    /// cut. Impls that failed to resolve are dropped, because that diagnostic already fired.
    let private elaborateClassInterfaces
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (elaborateOne: TTypeMember -> TTypeMember)
        : EqArray<SemType * EqArray<TTypeMember>> =
        EqArray.ofSeq (
            seq {
                for impl in info.InterfaceImpls do
                    match InterfaceImplResolution.tryIface impl.Resolution with
                    | ValueSome ifaceTy -> yield (ifaceTy, elaborateClassElements ctx info elaborateOne impl.Elements)
                    | ValueNone -> ()
            }
        )

    /// `rewrite` is the field-reference rewrite for the half these entries belong to.
    let private elaborateClassPreamble
        (ctx: PassContext)
        (rewrite: TExpr -> TExpr)
        (entries: ClassPreambleEntry[])
        : EqArray<TPreambleEntry> =
        EqArray.ofSeq (
            seq {
                for entry in entries ->
                    match entry with
                    | ClassPreambleEntry.Let l ->
                        TPreambleEntry.Let
                            {
                                Name = l.Name
                                Type = Unification.zonk ctx.Store l.Type
                                IsMutable = l.IsMutable
                                Init = translateBinding ctx l.Binding |> rewrite
                            }
                    | ClassPreambleEntry.Do e -> TPreambleEntry.Do(translateExpr ctx e |> rewrite)
            }
        )

    /// The `inherit Base(args)` invocation. It carries the derived class's primary-ctor
    /// params, the only slots the args can reference, because `this` is not constructed yet.
    let private tryBaseCtorCall
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (staticRewrite: FieldRewrite)
        : TBaseCtorCall voption =
        match info.BaseType, info.BaseCtorArgs with
        | ValueSome _, ValueSome argExpr ->
            let ctorParamKeys =
                EqArray.ofSeq (
                    seq { for p in info.CtorParams -> (p.DeclSite.BoundVar, Unification.zonk ctx.Store p.Type) }
                )

            // The args run before `this` exists, so the INSTANCE rewrite must not apply.
            // But the `.cctor` has already run, so a `static let` IS in scope here and IS a
            // field; unrewritten, its bound variable survives as a `Var` codegen has no slot for.
            let args = peelOneArg (translateExpr ctx >> rewriteFieldRefs staticRewrite) argExpr

            ValueSome
                {
                    CtorParams = ctorParamKeys
                    Args = args
                    // The chosen base ctor's identity, recorded under the args expr for an
                    // external base (`inherit exn(msg)`); `ValueNone` for a local base.
                    ChosenCtor = ctx.Resolution.ExternalCtor.TryGetValue(CstKeys.ofExpr argExpr)
                }
        | _ -> ValueNone

    /// Surface a `TypeDefn.Class` (or class-shaped `TypeDefn.Anon`) as a `TDecl.Type` from
    /// the resolved `ClassTypeInfo`. Ctor params and member signatures are remapped through
    /// the declaring-type typars, as for a record or union.
    let private tryClassType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (arity: int)
        (elements: TypeDefnElements<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) option =
        // The key of the type being LOWERED, minted from the module the walk is in rather
        // than read by name. A sibling module's same-named class is a different type, and an
        // arity-overloaded `Box\`1`/`Box\`2` does not resolve by bare name at all.
        match TypeRegistry.tryClassByKey ctx.Types (ctx.DeclaredTypeKey(name, arity)) with
        | ValueNone -> None
        | ValueSome info ->
            let scope =
                mkDeclScope ctx info.TypeParams (fun args -> TyClass(info.TypeKey, args))

            let env = scope.Env
            let declTypars = scope.DeclTypars
            let selfTy = scope.SelfTy
            let elaborateOne = elaboratorOf scope

            let ctorParams =
                EqArray.ofSeq (
                    seq {
                        for p in info.CtorParams ->
                            {
                                Name = p.Name
                                Type = p.Type
                                IsMutable = false
                                // Ctor-param attributes are an ArgSpec position and are not
                                // stored.
                                Attributes = EqArray.empty
                            }
                    }
                )

            // Explicit `val [mutable] x: T` instance fields.
            let instanceFields =
                EqArray.ofSeq (
                    seq {
                        for fld in info.InstanceFields ->
                            {
                                Name = fld.Name
                                Type = fld.Type
                                IsMutable = fld.IsMutable
                                // A `val` field's attributes are not stored.
                                Attributes = EqArray.empty
                            }
                    }
                )

            let members = elaborateClassElements ctx info elaborateOne elements
            let interfaces = elaborateClassInterfaces ctx info elaborateOne

            let staticRewrite = staticFieldRewrite info
            let instanceRewrite = instanceFieldRewrite info selfTy

            let staticPreamble =
                elaborateClassPreamble ctx (rewriteFieldRefs staticRewrite) info.StaticPreamble

            let instancePreamble =
                elaborateClassPreamble
                    ctx
                    (rewriteFieldRefs staticRewrite >> rewriteFieldRefs instanceRewrite)
                    info.InstancePreamble

            let secondaryCtors =
                EqArray.ofSeq (seq { for sc in info.SecondaryCtors -> translateSecondaryCtor ctx info.Name sc })

            // The parent's resolved `TyClass` carries THIS class's declaring typars as
            // roots, so a generic parent encodes against this class's own generic
            // parameters once the cut is made.
            let baseType = info.BaseType
            let baseCtorCall = tryBaseCtorCall ctx info staticRewrite

            Some(
                mkTypeDecl
                    name
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    info.Attributes
                    (TTypeKind.Class
                        {
                            Fields = instanceFields
                            CtorParams = ctorParams
                            Members = members
                            BaseType = baseType
                            Interfaces = interfaces
                            Declared = info.Declared
                            StaticPreamble = staticPreamble
                            InstancePreamble = instancePreamble
                            ThisKey = info.ThisKey
                            SecondaryCtors = secondaryCtors
                            BaseCtorCall = baseCtorCall
                            // A ref struct is necessarily a value type, so the two source
                            // bools collapse with `IsByRefLike` winning.
                            ValueKind =
                                if info.IsByRefLike then ClassValueKind.RefStruct
                                elif info.IsValueType then ClassValueKind.Struct
                                else ClassValueKind.RefType
                            HasPrimaryCtor = info.HasPrimaryCtor
                        }),
                List.ofSeq env
            )

    /// Surface a transparent `type t = body` as a `TDecl.Type`. The RHS comes from the
    /// registry entry the group close fills, so the decl and every use site expand the one
    /// resolved body. `None` for a cyclic abbreviation, whose fill already reported.
    let private tryAbbrevType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (arity: int)
        : (TDecl * (TyVarId * SemType) list) option =
        match TypeRegistry.tryAbbrevByKey ctx.Types (ctx.DeclaredTypeKey(name, arity)) with
        | ValueNone -> None
        | ValueSome info ->
            match info.State with
            | AbbreviationState.NotFilled
            | AbbreviationState.InProgress
            | AbbreviationState.Broken -> None
            | AbbreviationState.Filled body ->
                Some(
                    mkTypeDecl
                        name
                        info.TypeKey
                        ns
                        (EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }))
                        // A transparent alias carries no attributes of its own: every
                        // verdict is the body's.
                        EqArray.empty
                        (TTypeKind.Abbrev body),
                    mkDeclTyparEnv ctx.Store info.TypeParams
                )

    /// An INTERNAL artifact for `type X = (# … #) with member …`, consumed only by
    /// member-inline lifting and NEVER emitted. Each member's `ThisTy` is the abbrev's
    /// intrinsic `TyConst`, not a `TyClass`; `Class` is only the inertest container kind.
    let private tryIntrinsicAbbrevType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TyVarId * SemType) list) option =
        match TypeRegistry.tryIntrinsicAbbrevHostByCanon ctx.Types name with
        | ValueNone -> None
        | ValueSome info ->
            let scope =
                mkDeclScope ctx info.TypeParams (fun args -> TyConst(info.SelfKey, args))

            let env = scope.Env
            let declTypars = scope.DeclTypars
            let elaborateOne = elaboratorOf scope

            let members, _ =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            let clsG: TClass =
                {
                    Fields = EqArray.empty
                    CtorParams = EqArray.empty
                    Members = members
                    BaseType = ValueNone
                    Interfaces = EqArray.empty
                    Declared = DeclaredClassFlags.Default
                    StaticPreamble = EqArray.empty
                    InstancePreamble = EqArray.empty
                    ThisKey = info.ThisKey
                    SecondaryCtors = EqArray.empty
                    BaseCtorCall = ValueNone
                    ValueKind = ClassValueKind.RefType
                    HasPrimaryCtor = false
                }

            Some(
                mkTypeDecl name info.TypeKey ns (EqArray.ofList declTypars) EqArray.empty (TTypeKind.Class clsG),
                List.ofSeq env
            )

    /// Surface an interface-shaped, union, record, or class `TypeDefn` as a `TDecl.Type`.
    let tryTypeDecl
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (td: TypeDefn<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) option =
        let ns = DeclContainment.namespaceOpt c

        let classify tn (body: ObjectModelBody<SyntaxToken>) =
            let name = typeNameSimple ctx tn

            let arity = NameResolutionTypeRegistration.arityOfTypeName ctx tn

            match tryInterfaceMethods ctx name arity body with
            | Some(typars, methods, env) ->
                // Mint the identity registration would, from the SAME containment-derived
                // container, so a reference to the interface compares equal to this decl's key
                // wherever the interface is declared.
                let key = ctx.DeclaredTypeKey(name, typars.Length)

                let attrs =
                    match TypeRegistry.tryClassByKey ctx.Types key with
                    | ValueSome info -> info.Attributes
                    // Registration files every all-abstract decl as a `ClassTypeInfo`, so a
                    // miss is a producer bug, not an attribute-less interface.
                    | ValueNone -> failwithf "tryTypeDecl: interface '%s' has no registered ClassTypeInfo" name

                Some(mkTypeDecl name key ns typars attrs (TTypeKind.Interface methods), env)
            // Not all-abstract ⇒ class shape (`type C(x) = member …`).
            | None -> tryClassType ctx ns name arity body.elements

        match td with
        | TypeDefn.Anon(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Interface(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Class(typeName = tn; body = body) ->
            tryClassType
                ctx
                ns
                (typeNameSimple ctx tn)
                (NameResolutionTypeRegistration.arityOfTypeName ctx tn)
                body.elements
        | TypeDefn.Union(typeName = tn; extensions = ext) ->
            tryUnionType ctx ns (typeNameSimple ctx tn) (typeNameDeclKey ctx tn) ext
        | TypeDefn.Record(typeName = tn; extensions = ext) ->
            tryRecordType ctx ns (typeNameSimple ctx tn) (typeNameDeclKey ctx tn) ext
        | TypeDefn.Enum(typeName = tn) -> tryEnumType ctx ns (typeNameSimple ctx tn) (typeNameDeclKey ctx tn)
        // A transparent alias surfaces its resolved RHS; an inline intrinsic-abbrev has a
        // host in `IntrinsicAbbrevHost` and surfaces its members instead (lift-only).
        | TypeDefn.Abbrev(typeName = tn; extensions = ext) ->
            let name = typeNameSimple ctx tn

            match tryIntrinsicAbbrevType ctx ns name ext with
            | Some result -> Some result
            | None -> tryAbbrevType ctx ns name (NameResolutionTypeRegistration.arityOfTypeName ctx tn)
        | _ -> None
