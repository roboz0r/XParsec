namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRegistration
open UnificationTranslate
open SignatureResolutionContext

// The members and the class-like body a signature type declares, resolved to what a consumer
// sees. Each member is published or dropped on its own, so they are read apart from the
// declaration that holds them.

module SignatureResolutionMembers =

    /// How a published member is APPLIED, which is what decides the shape its signature
    /// freezes to.
    [<RequireQualifiedAccess>]
    type SigMemberForm =
        /// Read as a value: `member Count: int`. Its signature IS its value type.
        | Property
        /// `x.[i] <- v`: every group collapses to one parameter vector with the value last.
        | Setter
        /// Applied a group at a time.
        | Method

    /// One member a signature type body publishes. A `with get, set` clause yields two.
    [<NoEquality; NoComparison>]
    type SigMember =
        {
            Name: string
            IsStatic: bool
            Form: SigMemberForm
            TyparDefns: TyparDefns<SyntaxToken> voption
            Signature: CurriedSig<SyntaxToken>
        }

    /// The members one type body declares, in source order. An `abstract` slot publishes as
    /// an ordinary instance member, so a call on the capability and the conformance check
    /// both resolve against it.
    let sigMembers (ctx: PassContext) (elems: TypeElementsSignature<SyntaxToken>) : SigMember list =
        let acc = ResizeArray<SigMember>()

        let publish (isStatic: bool) (ms: MemberSig<SyntaxToken>) =
            let ident, tds, csig, getSet =
                match ms with
                | MemberSig.MethodOrPropSig(ident = i; typarDefns = tds; sign = s) -> i, tds, s, ValueNone
                | MemberSig.PropSig(ident = i; typarDefns = tds; sign = s; getSet = gs) -> i, tds, s, ValueSome gs

            match OperatorNames.ofDeclaredName ctx.NameOf ident with
            | ValueNone -> ()
            | ValueSome memberName ->
                let (CurriedSig(args = argGroups)) = csig
                // A property carries an object argument and nothing else, so a signature
                // with argument groups declares a method.
                let takesArgs = argGroups.Length > 0

                let readForm =
                    if takesArgs then
                        SigMemberForm.Method
                    else
                        SigMemberForm.Property

                let add (name: string) (form: SigMemberForm) =
                    acc.Add
                        {
                            Name = name
                            IsStatic = isStatic
                            Form = form
                            TyparDefns = tds
                            Signature = csig
                        }

                match getSet with
                | ValueNone -> add memberName readForm
                | ValueSome gs ->
                    let halves = AccessorNames.halvesOf ctx.NameOf gs

                    if halves.Getter.IsSome then
                        // An INDEXED getter compiles to `get_Item`; a plain one keeps its name.
                        add
                            (if takesArgs then
                                 AccessorNames.getterName memberName
                             else
                                 memberName)
                            readForm

                    if halves.Setter.IsSome then
                        add (AccessorNames.setterName memberName) SigMemberForm.Setter

        for el in elems do
            match el with
            | TypeSignatureElement.Member(signature = ms)
            | TypeSignatureElement.Abstract(signature = ms)
            | TypeSignatureElement.Override(signature = ms)
            | TypeSignatureElement.Default(signature = ms) -> publish false ms
            | TypeSignatureElement.StaticMember(signature = ms) -> publish true ms
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Interface _
            | TypeSignatureElement.Value _
            | TypeSignatureElement.Inherit _ -> ()

        List.ofSeq acc

    /// Resolve `f`, and keep the result only if nothing refused it. A member signature referencing
    /// a type this compilation cannot resolve declares nothing a consumer could call, so it is
    /// dropped, its refusals go with it, and the DROP is reported instead: `what` identifies it.
    let tryResolve (sctx: SigCtx) (what: unit -> string) (f: unit -> 'a) : 'a voption =
        let ctx = sctx.Pass
        // Collected apart, so a refusal takes its own diagnostics with it.
        let struct (result, raised) = ctx.Collecting f

        match raised |> Seq.tryFind Diagnostic.isError with
        | None ->
            ctx.Diagnostics.AddRange raised
            ValueSome result
        | Some refusal ->
            ctx.Report(
                Site.Nowhere,
                Kind.Conformance(
                    AssemblyName.toStored ctx.File.Path.Assembly,
                    ConformanceVerdict.SignatureNotPublished(sprintf "%s: %s" (what ()) (Kind.message refusal.Kind))
                )
            )

            ValueNone

    /// The published form of `m`: its signature frozen over the declaring typars under
    /// `TyparScope.Type declKey`, and over its own under `TyparScope.Member declKey`.
    let resolveMember
        (ctx: PassContext)
        (declKey: TypeKey)
        (declTypars: EqArray<DeclaredTypar>)
        (m: SigMember)
        : ExternalMember =
        classifyCurriedSigTypes ctx m.Signature
        let explicit = explicitTyparNames ctx m.TyparDefns

        let known =
            seq {
                for tp in declTypars -> tp.Name
                yield! explicit
            }

        let implicit =
            implicitTyparNames ctx known m.TyparDefns (fun it -> CstTypeWalk.iterTypeCurriedSig it m.Signature)

        let ownTypars = mkMethodTypars ctx.Store (explicit @ implicit)
        let declArity = declTypars.Length

        let domains, ret =
            underTypars ctx declTypars ownTypars (fun () -> translateSigGroups ctx m.Signature)

        let env = memberEnv ctx declKey declTypars ownTypars
        let frozenDomains = freezeDomains ctx env domains
        let frozenRet = freezeOver ctx env ret

        let signature, kind, storage =
            match m.Form with
            | SigMemberForm.Setter ->
                ExternalSignature.setter declArity ownTypars.Length frozenDomains frozenRet,
                MemberKind.Method,
                MemberStorage.Method
            | SigMemberForm.Property ->
                ExternalSignature.value (declArity, ownTypars.Length, frozenRet),
                MemberKind.Property,
                MemberStorage.Property
            | SigMemberForm.Method ->
                ExternalSignature.ofGroups (declArity, ownTypars.Length, frozenDomains, frozenRet),
                MemberKind.Method,
                MemberStorage.Method

        let argSig = ExternalSignature.argSigOf signature

        { ExternalMember.OfKey(SymbolKeyOps.memberKeyOf declKey m.Name argSig ownTypars.Length kind) with
            IsStatic = m.IsStatic
            Storage = storage
            Signature = signature
        }

    /// The members and `new: … -> T` constructors one type body publishes. A constructor is
    /// named `.ctor` and instance, as the metadata layer spells them.
    let resolveBodyMembers
        (sctx: SigCtx)
        (declKey: TypeKey)
        (declTypars: EqArray<DeclaredTypar>)
        (elems: TypeElementsSignature<SyntaxToken>)
        : ExternalMember list =
        let ctx = sctx.Pass
        let env = scopedEnv ctx (TyparScope.Type declKey) declTypars
        let declName = SymbolKeyOps.typeMetaName declKey

        let ctorOf (sign: UncurriedSig<SyntaxToken>) =
            let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = specs); returnType = retTy)) =
                sign

            classifyUncurriedSigTypes ctx sign

            let parameters, ret =
                underTypars
                    ctx
                    declTypars
                    EqArray.empty
                    (fun () ->
                        ExternalSignature.tupledParams (
                            EqArray.ofSeq (
                                seq { for ArgSpec(typ = t) in specs -> freezeOver ctx env (translateType ctx t) }
                            )
                        ),
                        freezeOver ctx env (translateType ctx retTy)
                    )

            ExternalMember.ctor
                declKey
                (ExternalSignature.make (declTypars.Length, 0, parameters, ret))
                (ExternalSignature.argSigOfParameters parameters)
                SymbolOrigin.Empty
                []

        [
            for m in sigMembers ctx elems do
                let named () =
                    sprintf "member '%s.%s'" declName m.Name

                match tryResolve sctx named (fun () -> resolveMember ctx declKey declTypars m) with
                | ValueSome published -> yield published
                | ValueNone -> ()

            for el in elems do
                match el with
                | TypeSignatureElement.Constructor(signature = sign) ->
                    let named () = sprintf "constructor of '%s'" declName

                    match tryResolve sctx named (fun () -> ctorOf sign) with
                    | ValueSome published -> yield published
                    | ValueNone -> ()
                | _ -> ()
        ]

    // --- class-like bodies ------------------------------------------------------------

    let inheritClauseOf (elems: TypeElementsSignature<SyntaxToken>) : ClassInheritsDecl<SyntaxToken> voption =
        let mutable found = ValueNone

        for e in elems do
            match e, found with
            | TypeSignatureElement.Inherit decl, ValueNone -> found <- ValueSome decl
            | _ -> ()

        found

    /// Whether `key` denotes an interface: the current group's claims first, then the
    /// shapes this signature published earlier, then the registry and provider.
    let sigIsInterfaceKey (sctx: SigCtx) (key: TypeKey) : bool =
        EqSet.contains key sctx.GroupInterfaceKeys
        || (
            match sctx.Surface.ShapesByKey.TryGetValue key with
            | true, ExternalTypeShape.Class c -> c.IsInterface
            | true, ExternalTypeShape.IntrinsicInterface _ -> true
            | true, _ -> false
            | false, _ -> BaseEligibility.isInterfaceKey sctx.Pass key
        )

    /// Each `interface <ty>` spec with the `interface` keyword token a rejected type is
    /// reported at.
    let interfaceSpecsOf (elems: TypeElementsSignature<SyntaxToken>) : (SyntaxToken * Type<SyntaxToken>) list =
        [
            for e in elems do
                match e with
                | TypeSignatureElement.Interface(InterfaceSpec.InterfaceSpec(interfaceToken = tok; typ = t)) -> tok, t
                | _ -> ()
        ]

    let freezeInterfaces
        (sctx: SigCtx)
        (declKey: TypeKey)
        (declTypars: EqArray<DeclaredTypar>)
        (types: (SyntaxToken * Type<SyntaxToken>) list)
        : EqArray<FrozenNominal> =
        let ctx = sctx.Pass
        let env = scopedEnv ctx (TyparScope.Type declKey) declTypars

        // Translated UNDER the declaring typars, not merely frozen over them: `interface
        // seq<'T>` references `'T`, and one resolved outside their scope is a fresh variable that
        // freezes to a hole no consumer can fill.
        let translated =
            underTypars ctx declTypars EqArray.empty (fun () -> [ for (tok, t) in types -> tok, translateType ctx t ])

        EqArray.ofList
            [
                for (tok, ty) in translated do
                    match
                        BaseEligibility.classifyImpl (sigIsInterfaceKey sctx) ty
                        |> BaseEligibility.admitSigImpl ctx tok
                    with
                    | ValueSome n -> NominalG.map (freezeOver ctx env) n
                    | ValueNone -> ()
            ]

    /// The surface a bodied signature publishes. `Shape.Members` is filled only for an
    /// INTERFACE, whose `interface … with` conformance check reads the shape directly; a shape's
    /// templates instantiate over the declaring type's scope, where a nominal member's own
    /// typars are not.
    [<NoEquality; NoComparison>]
    type BodiedSurface =
        {
            Shape: ExternalClassShape
            Members: ExternalMember list
            /// The declaration's attributes, resolved and constant-folded.
            Attributes: TAttributes
        }

    /// The class surface a bodied signature declares: its members, its `new` constructors, its
    /// base and its interfaces. Shared by a nominal class and by the capability surface an
    /// `extern … with` declares. PUBLISHES NOTHING; the caller files both halves.
    let bodiedClassSurface
        (sctx: SigCtx)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (isInterface: bool)
        (elems: TypeElementsSignature<SyntaxToken>)
        : BodiedSurface =
        let ctx = sctx.Pass
        let typeParams = declaredTyparsOfTypeName ctx tn
        let members = resolveBodyMembers sctx id.Key typeParams elems
        let inherits = inheritClauseOf elems

        // An INTERFACE has no base type, so its `inherit` clause is interface inheritance
        // (`enumerator inherit disposable`); a class's (`exn inherit obj`) is its base.
        let baseClause = if isInterface then ValueNone else inherits

        let interfaceTypes =
            [
                match (if isInterface then inherits else ValueNone) with
                | ValueSome(ClassInheritsDecl(inheritToken = tok; typ = t)) -> tok, t
                | ValueNone -> ()

                yield! interfaceSpecsOf elems
            ]

        let attrs = Attributes.attributesOfTypeName tn
        let resolvedAttrs = ctx.ResolveAttributes attrs
        let decoded = AttributeDecode.decodeClassAttributes resolvedAttrs

        let kind = TypeDefnKind.ofClassOrInterface isInterface decoded.IsValueType

        let foldedAttrs = Attributes.foldTypeDefn ctx kind resolvedAttrs

        let shape =
            underTypars
                ctx
                typeParams
                EqArray.empty
                (fun () ->
                    {
                        Typars = TyparList.unconstrained typeParams
                        Commitment = ClassCommitment.ofIsInterface isInterface
                        Members =
                            (if isInterface then
                                 EqArray.ofList members
                             else
                                 EqArray.empty)
                        FrozenInterfaces = freezeInterfaces sctx id.Key typeParams interfaceTypes
                        FrozenBaseType =
                            baseClause
                            |> ValueOption.bind (fun (ClassInheritsDecl(inheritToken = inhTok; typ = t)) ->
                                BaseEligibility.classify
                                    (sigIsInterfaceKey sctx)
                                    (BaseEligibility.isHeritableCanon ctx)
                                    (translateType ctx t)
                                |> BaseEligibility.admit ctx inhTok
                                |> ValueOption.map (fun parent ->
                                    NominalG.map
                                        (freezeOver ctx (scopedEnv ctx (TyparScope.Type id.Key) typeParams))
                                        parent.Nominal
                                )
                            )
                        Flags =
                            { ExternalClassFlags.Default with
                                Declared =
                                    { DeclaredClassFlags.Default with
                                        IsSealed = decoded.IsSealed
                                        IsAbstract = decoded.IsAbstract
                                        AllowNullLiteral = decoded.AllowNullLiteral
                                    }
                                IsValueType = decoded.IsValueType
                            }
                        Origin = SymbolOrigin.Empty
                    }
                )

        {
            Shape = shape
            Members = members
            Attributes = foldedAttrs
        }
