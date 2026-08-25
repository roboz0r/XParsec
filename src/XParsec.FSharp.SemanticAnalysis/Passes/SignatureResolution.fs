namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRegistration
open UnificationTranslate
open SignatureResolutionContext
open SignatureResolutionMembers

// The `.fsi` front end: its own walk, because the signature grammar is a tree of its own, over
// the resolver every other pass drives. What it fills is the surface the file publishes.

// Declarations resolve TOP-DOWN, as F# does: a signature reaches its own `type … and …`
// group and everything declared above it, and nothing below.

module SignatureResolution =

    // --- publication ------------------------------------------------------------------

    let private publishShape (sctx: SigCtx) (key: TypeKey) (shape: ExternalTypeShape) : unit =
        PublishedSurfaceBuilder.addShape sctx.Surface key shape

    let private publishMembers (sctx: SigCtx) (key: TypeKey) (members: ExternalMember list) : unit =
        PublishedSurfaceBuilder.addMembers sctx.Surface key members

    let private publishUnionCases
        (sctx: SigCtx)
        (unionKey: TypeKey)
        (rqa: bool)
        (cases: EqArray<ExternalCaseShape>)
        : unit =
        for case in cases do
            PublishedSurfaceBuilder.addUnionCase
                sctx.Surface
                {
                    UnionKey = unionKey
                    Case = case
                    IsRequireQualifiedAccess = rqa
                }

    let private publishBodiedSurface (sctx: SigCtx) (key: TypeKey) (surface: BodiedSurface) : BodiedSurface =
        publishMembers sctx key surface.Members
        surface

    // --- registration -------------------------------------------------------------------

    /// A record / union / enum / abbreviation's DETAIL is registered through the entry points
    /// the implementation walk uses; a class-like body has no implementation-side twin to
    /// share, so its surface is read at publication instead.
    let private registerSigDetail (sctx: SigCtx) (id: TypeIdentity) (decl: SigDecl) : unit =
        let ctx = sctx.Pass

        match decl with
        | SigDecl.Record(typeName = tn; fields = fields) -> registerRecordDecl ctx id tn fields
        | SigDecl.Union(typeName = tn; cases = cases) -> registerUnionDecl ctx id tn cases
        | SigDecl.Enum(typeName = tn; cases = cases) -> registerEnumDecl ctx id tn cases
        // An abbreviation entry and an `extern`'s repr are filed earlier; a class-like or
        // opaque signature registers nothing but its claim, a reference needing the identity
        // alone.
        | SigDecl.Abbrev _
        | SigDecl.IntrinsicAbbrev _
        | SigDecl.Extern _
        | SigDecl.ClassLike _
        | SigDecl.Opaque _
        | SigDecl.Delegate _
        | SigDecl.TypeExtension _ -> ()

    // --- record / union / enum ----------------------------------------------------------

    let private publishRecord
        (sctx: SigCtx)
        (id: TypeIdentity)
        (extensions: TypeExtensionElementsSignature<SyntaxToken> voption)
        : unit =
        let ctx = sctx.Pass
        let key = id.Key

        match TypeRegistry.tryRecordByKey ctx.Types key with
        | ValueNone -> ()
        | ValueSome info ->
            let env = typarEnv ctx (TyparOwner.Type info.TypeParams)
            let arity = info.TypeParams.Length

            let fields =
                EqArray.ofSeq
                    [
                        for f in info.Fields ->
                            {
                                Name = f.Name
                                IsMutable = f.IsMutable
                                Frozen = freezeOver ctx env f.Type
                            }
                            : ExternalFieldShape
                    ]

            publishShape sctx key (ExternalTypeShape.Record(arity, fields, SymbolOrigin.Empty, info.IsValueType))

            PublishedSurfaceBuilder.addRecordCandidate
                sctx.Surface
                {
                    TypeKey = key
                    TyparArity = arity
                    FieldNames = fields |> EqArray.map (fun f -> f.Name)
                    IsRequireQualifiedAccess = info.IsRequireQualifiedAccess
                }

            match extensions with
            | ValueSome(TypeExtensionElementsSignature(elements = elems)) ->
                publishMembers sctx key (resolveBodyMembers sctx key info.TypeParams elems)
            | ValueNone -> ()

    let private publishUnion
        (sctx: SigCtx)
        (id: TypeIdentity)
        (extensions: TypeExtensionElementsSignature<SyntaxToken> voption)
        : unit =
        let ctx = sctx.Pass
        let key = id.Key

        match TypeRegistry.tryUnionByKey ctx.Types key with
        | ValueNone -> ()
        | ValueSome info ->
            let env = typarEnv ctx (TyparOwner.Type info.TypeParams)
            let arity = info.TypeParams.Length

            let cases =
                EqArray.ofSeq
                    [
                        for c in info.Cases ->
                            {
                                Name = c.Name
                                FieldNames = EqArray.ofArray c.FieldNames
                                FrozenFieldTypes = EqArray.ofSeq (seq { for t in c.Fields -> freezeOver ctx env t })
                            }
                            : ExternalCaseShape
                    ]

            let extensionElems =
                match extensions with
                | ValueSome(TypeExtensionElementsSignature(elements = elems)) -> elems
                | ValueNone -> System.Collections.Immutable.ImmutableArray.Empty

            // A union's trailing `with interface <ty>` impls ride the shared extension list;
            // there is no union-specific parser field.
            let interfaces =
                freezeInterfaces ctx info.TypeParams (interfaceSpecsOf extensionElems)

            publishShape
                sctx
                key
                (ExternalTypeShape.Union(arity, cases, interfaces, SymbolOrigin.Empty, info.IsRequireQualifiedAccess))

            // A list is written only as `[]` / `::`, so indexing the cons-list's case names
            // would only shadow a user union declaring a case of the same name.
            if not (RuntimeNames.isVesperListName (SymbolKeyOps.typeMetaName key)) then
                publishUnionCases sctx key info.IsRequireQualifiedAccess cases

            publishMembers sctx key (resolveBodyMembers sctx key info.TypeParams extensionElems)

    /// Publish the registered enum's case table. One rejected case (reported at registration)
    /// downgrades the whole enum: a partial table would answer `E.C1` for the survivors and
    /// "no such case" for the rest.
    let private publishEnum (sctx: SigCtx) (id: TypeIdentity) : unit =
        let ctx = sctx.Pass

        match TypeRegistry.tryEnumByKey ctx.Types id.Key with
        | ValueNone -> ()
        | ValueSome info ->
            let shapes = ResizeArray<ExternalEnumCaseShape>(info.Cases.Length)
            let mutable broken = ValueNone

            for c in info.Cases do
                match c.Value with
                | ValueSome(TEnumLiteral.Int n) ->
                    shapes.Add
                        {
                            Name = c.Name
                            Value = ExternalEnumCaseValue.IntVal(snd (TEnumCases.integralValue n))
                        }
                | ValueSome(TEnumLiteral.String s) ->
                    shapes.Add
                        {
                            Name = c.Name
                            Value = ExternalEnumCaseValue.StringVal s
                        }
                | ValueNone -> broken <- ValueSome c.Name

            match broken with
            | ValueSome name ->
                publishShape
                    sctx
                    id.Key
                    (ExternalTypeShape.Unmodelled(
                        UnmodelledReason.ExtractionFailed(sprintf "enum case '%s' has no constant value" name),
                        0
                    ))
            | ValueNone ->
                publishShape sctx id.Key (ExternalTypeShape.Enum(EqArray.ofResizeArray shapes, SymbolOrigin.Empty))

    // --- abbreviations --------------------------------------------------------------------

    let private publishAbbrev (sctx: SigCtx) (id: TypeIdentity) : unit =
        let ctx = sctx.Pass

        match TypeRegistry.tryAbbrevByKey ctx.Types id.Key with
        | ValueNone -> ()
        | ValueSome info ->
            let env = typarEnv ctx (TyparOwner.Type info.TypeParams)

            let body =
                match info.State with
                | AbbreviationState.Filled ty -> freezeOver ctx env ty
                | AbbreviationState.NotFilled
                | AbbreviationState.InProgress
                | AbbreviationState.Broken ->
                    ExternalSignature.unfreezable (sprintf "abbreviation '%s' has no body" id.Name)

            publishShape sctx id.Key (ExternalTypeShape.Abbrev(info.TypeParams.Length, body))

    /// `type t = (# "…" #)` written in a SIGNATURE: the same primitive binding it is in an
    /// implementation, and registering its entry already filed the repr below.
    let private publishIntrinsicAbbrev
        (sctx: SigCtx)
        (id: TypeIdentity)
        (kindTag: ExternKind<SyntaxToken> voption)
        : unit =
        let ctx = sctx.Pass
        let canon = TypeRegistry.intrinsicKeyOf ctx.Types id.Name

        let platform =
            match ctx.Types.IntrinsicReprKeys.TryGetValue canon with
            | true, repr -> IntrinsicPlatform.Repr repr.Platform
            | _ -> IntrinsicPlatform.Unsupported sctx.Inputs.Target

        let declared =
            match kindTag with
            | ValueSome(ExternKind.Class _) -> DeclaredRepr.Heritable
            | _ -> DeclaredRepr.Opaque

        PublishedSurfaceBuilder.addDeclaredRepr sctx.Surface canon declared
        publishShape sctx id.Key (ExternalTypeShape.Intrinsic(IntrinsicShape.Scalar(canon, id.TyparArity, platform)))

    // --- `extern` -------------------------------------------------------------------------

    /// Which of the three surfaces an `extern` declares. They publish different shapes, and
    /// its keyword tag is the only thing that says which.
    let private externFormOfKindTag (kindTag: ExternKind<SyntaxToken> voption) : DeclaredRepr =
        match kindTag with
        | ValueSome(ExternKind.Interface _) -> DeclaredRepr.Capability
        | ValueSome(ExternKind.Class _) -> DeclaredRepr.Heritable
        | _ -> DeclaredRepr.Opaque

    /// A capability IS an interface, so its `inherit` clause is interface inheritance and
    /// its members ride the shape. Both primitives are classes.
    let private externDeclaresInterface (form: DeclaredRepr) : bool = form = DeclaredRepr.Capability

    /// The repr the paired implementation binds for this `extern`. A PRIMITIVE's is FILED on
    /// the intrinsic's own key so a use site resolves the name to it; a capability's is not,
    /// because a capability is a nominal interface that only CARRIES a platform spelling and
    /// has no intrinsic identity to file under. WHICH spelling is the implementation's
    /// business, so a target that binds none still publishes the type, marked unsupported.
    let private bindExternRepr (sctx: SigCtx) (id: TypeIdentity) (form: DeclaredRepr) : IntrinsicPlatform =
        match sctx.Inputs.Reprs.TryGetValue id.Name with
        | true, repr ->
            let fileOn (heritable: bool) =
                sctx.Pass.Types.IntrinsicReprKeys.[TypeRegistry.intrinsicKeyOf sctx.Pass.Types id.Name] <-
                    {
                        Platform = repr
                        Heritable = heritable
                    }

            match form with
            | DeclaredRepr.Capability -> ()
            | DeclaredRepr.Heritable -> fileOn true
            | DeclaredRepr.Opaque -> fileOn false

            IntrinsicPlatform.Repr repr
        | _ -> IntrinsicPlatform.Unsupported sctx.Inputs.Target

    /// A concrete member on an `extern` type must be declared `inline`: the primitive has no
    /// representation in the output to carry a method, so a use site can only splice the body
    /// its sibling `.fs` gives it.
    let private requireInlineExternMembers
        (ctx: PassContext)
        (id: TypeIdentity)
        (elems: TypeElementsSignature<SyntaxToken>)
        : unit =
        for e in elems do
            match e with
            | TypeSignatureElement.Member(inlineToken = ValueNone)
            | TypeSignatureElement.StaticMember(inlineToken = ValueNone) ->
                ctx.Report(id.DeclSite.Tok, Kind.Message(IntrinsicHost.memberNeedsInline id.Name))
            | TypeSignatureElement.Override _
            | TypeSignatureElement.Default _ ->
                ctx.Report(
                    id.DeclSite.Tok,
                    Kind.Message(IntrinsicHost.cannotDeclare id.Name IntrinsicHost.Construct.Override)
                )
            // `abstract` declares a slot and `val` storage, so neither is a body. `new` on a
            // heritable primitive (`obj` / `exn`) resolves to a target-provided constructor.
            | TypeSignatureElement.Member _
            | TypeSignatureElement.StaticMember _
            | TypeSignatureElement.Abstract _
            | TypeSignatureElement.Value _
            | TypeSignatureElement.Constructor _
            | TypeSignatureElement.Inherit _
            | TypeSignatureElement.Interface _ -> ()

    /// The `.ctor`s of a heritable primitive, rebased: they are authored against the canon but
    /// EMIT against the platform class, so only the declaring key moves; the signature stays
    /// canon.
    let private platformCtors (platform: IntrinsicPlatform) (members: ExternalMember list) : EqArray<ExternalMember> =
        match platform with
        | IntrinsicPlatform.Repr repr ->
            let platformDecl = SymbolKeyOps.qualifiedTypeKeyOf repr 0

            EqArray.ofList
                [
                    for m in members do
                        if m.Name = ".ctor" then
                            { m with
                                Key = { m.Key with Decl = platformDecl }
                            }
                ]
        | IntrinsicPlatform.Unsupported _ -> EqArray.empty

    /// The shape a CAPABILITY publishes: it IS the interface, canon'd on its own identity,
    /// with the platform spelling beside it where the target binds one. Nominal either way —
    /// a use site kinds it `TyClass` from the shape, which is why it claims a class.
    let private publishCapability
        (sctx: SigCtx)
        (id: TypeIdentity)
        (platform: IntrinsicPlatform)
        (surface: BodiedSurface voption)
        : unit =
        let shape =
            match surface with
            | ValueSome s -> s.Shape
            | ValueNone -> ExternalClassShape.basic (id.TyparArity, true, SymbolOrigin.Empty)

        match platform with
        | IntrinsicPlatform.Repr repr ->
            publishShape
                sctx
                id.Key
                (ExternalTypeShape.IntrinsicInterface
                    {
                        Canon = id.Key
                        TyparArity = id.TyparArity
                        Platform = repr
                        Members = shape.Members
                        Interfaces = shape.FrozenInterfaces
                        Origin = SymbolOrigin.Empty
                    })
        // CANON-ONLY: the same interface, reachable by its own name alone.
        | IntrinsicPlatform.Unsupported _ -> publishShape sctx id.Key (ExternalTypeShape.Class shape)

    /// A heritable primitive (`obj` / `exn`) ALWAYS carries a supertype surface: a later
    /// `inherit` has nothing else to read its heritability off, which is why a member-less
    /// `extern class` (`Attribute`) still gets one.
    let private heritableSurface
        (platform: IntrinsicPlatform)
        (surface: BodiedSurface voption)
        : IntrinsicClassSurface voption =
        match surface with
        | ValueNone ->
            ValueSome
                {
                    Heritable = true
                    BaseType = ValueNone
                    Interfaces = EqArray.empty
                    Members = EqArray.empty
                }
        | ValueSome s ->
            ValueSome
                {
                    Heritable = true
                    BaseType = s.Shape.FrozenBaseType
                    Interfaces = s.Shape.FrozenInterfaces
                    Members = platformCtors platform s.Members
                }

    /// A scalar carries one only where it DECLARES something. An untagged `extern with member …`
    /// serves its members off their own table, not the shape; its `interface` clauses do land
    /// here, and it inherits nothing.
    let private scalarSurface (surface: BodiedSurface voption) : IntrinsicClassSurface voption =
        match surface with
        | ValueNone -> ValueNone
        | ValueSome s ->
            ValueSome
                {
                    Heritable = false
                    BaseType = ValueNone
                    Interfaces = s.Shape.FrozenInterfaces
                    Members = EqArray.empty
                }

    /// The shape a PRIMITIVE `extern` publishes: its intrinsic identity, under which a use site
    /// resolves the name, carrying whichever supertype surface its form declares.
    let private publishExternPrimitive
        (sctx: SigCtx)
        (id: TypeIdentity)
        (platform: IntrinsicPlatform)
        (classSurface: IntrinsicClassSurface voption)
        : unit =
        publishShape
            sctx
            id.Key
            (ExternalTypeShape.Intrinsic
                {
                    Id =
                        {
                            Canon = TypeRegistry.intrinsicKeyOf sctx.Pass.Types id.Name
                            TyparArity = id.TyparArity
                            Platform = platform
                        }
                    Class = classSurface
                })

    let private publishExtern
        (sctx: SigCtx)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (kindTag: ExternKind<SyntaxToken> voption)
        (members: TypeExtensionElementsSignature<SyntaxToken> voption)
        : unit =
        let ctx = sctx.Pass
        let form = externFormOfKindTag kindTag
        let platform = bindExternRepr sctx id form

        // A capability's canon IS its own identity; a primitive's is the intrinsic key its
        // repr is filed on, which is what an implementation files its binding under.
        let canon =
            match form with
            | DeclaredRepr.Capability -> id.Key
            | DeclaredRepr.Heritable
            | DeclaredRepr.Opaque -> TypeRegistry.intrinsicKeyOf ctx.Types id.Name

        PublishedSurfaceBuilder.addDeclaredRepr sctx.Surface canon form

        let declared =
            match members with
            | ValueSome(TypeExtensionElementsSignature(elements = elems)) when not elems.IsEmpty -> ValueSome elems
            | _ -> ValueNone

        let surface =
            declared
            |> ValueOption.map (fun elems ->
                requireInlineExternMembers ctx id elems

                publishBodiedSurface sctx id.Key (bodiedClassSurface sctx id tn (externDeclaresInterface form) elems)
            )

        match form with
        | DeclaredRepr.Capability -> publishCapability sctx id platform surface
        | DeclaredRepr.Heritable -> publishExternPrimitive sctx id platform (heritableSurface platform surface)
        | DeclaredRepr.Opaque -> publishExternPrimitive sctx id platform (scalarSurface surface)

    // --- class-like ------------------------------------------------------------------------

    let private publishClassLike
        (sctx: SigCtx)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (form: SigClassForm)
        (elems: TypeElementsSignature<SyntaxToken>)
        : unit =
        let isInterface =
            match form with
            | SigClassForm.Interface -> true
            | SigClassForm.Struct -> false
            | SigClassForm.Bodied -> bodyIsInterface elems

        let surface =
            publishBodiedSurface sctx id.Key (bodiedClassSurface sctx id tn isInterface elems)

        let shape =
            match form with
            | SigClassForm.Struct ->
                { surface.Shape with
                    Flags =
                        { surface.Shape.Flags with
                            IsValueType = true
                        }
                }
            | SigClassForm.Bodied
            | SigClassForm.Interface -> surface.Shape

        publishShape sctx id.Key (ExternalTypeShape.Class shape)

    /// An opaque abstract type (`type T`) has no body shape. It resolves as a non-interface
    /// class, so codegen can mint a ref off the origin.
    let private publishOpaque (sctx: SigCtx) (id: TypeIdentity) : unit =
        publishShape
            sctx
            id.Key
            (ExternalTypeShape.Class(ExternalClassShape.basic (id.TyparArity, false, SymbolOrigin.Empty)))

    /// A declaration that CLAIMS no type still registers its name and the gap, so a use site
    /// can say which form is missing rather than "no such type".
    let private publishUnmodelled
        (sctx: SigCtx)
        (containment: DeclContainment<SyntaxToken>)
        (tn: TypeName<SyntaxToken>)
        (reason: UnmodelledReason)
        : unit =
        let ctx = sctx.Pass
        let (TypeName(ident = nameLi)) = tn

        if nameLi.Idents.Length = 1 then
            let arity = arityOfTypeName ctx tn
            let name = ctx.NameOf nameLi.Idents.[0]

            let key =
                SymbolKeyOps.typeKeyOfContainer (localTypeContainer ctx containment) name arity

            PublishedSurfaceBuilder.addTypeName sctx.Surface key
            publishShape sctx key (ExternalTypeShape.Unmodelled(reason, arity))

    /// The shape ONE claimed declaration publishes, once every declaration in its group has
    /// registered its detail: a field or case type may reference a sibling.
    let private publishType (sctx: SigCtx) (id: TypeIdentity) (decl: SigDecl) : unit =
        match decl with
        | SigDecl.Record(extensions = ext) -> publishRecord sctx id ext
        | SigDecl.Union(extensions = ext) -> publishUnion sctx id ext
        | SigDecl.Enum _ -> publishEnum sctx id
        | SigDecl.Abbrev _ -> publishAbbrev sctx id
        | SigDecl.IntrinsicAbbrev(kindTag = tag) -> publishIntrinsicAbbrev sctx id tag
        | SigDecl.Extern(typeName = tn; kindTag = kindTag; members = members) ->
            publishExtern sctx id tn kindTag members
        | SigDecl.ClassLike(typeName = tn; form = form; elements = elems) -> publishClassLike sctx id tn form elems
        | SigDecl.Opaque _ -> publishOpaque sctx id
        // Claimed nothing, so it is not one of the identities this runs over; it published its
        // gap at claim time.
        | SigDecl.Delegate _
        | SigDecl.TypeExtension _ -> ()

    // --- groups and vals ---------------------------------------------------------------

    let private registerSigGroup
        (sctx: SigCtx)
        (containment: DeclContainment<SyntaxToken>)
        (visibleFrom: int)
        (decls: SigDecl list)
        : unit =
        let ctx = sctx.Pass
        let claims = ResizeArray<struct (TypeIdentity * SigDecl)>()

        for decl in decls do
            match claimSigTypeIdentity ctx containment visibleFrom decl with
            | ValueSome id ->
                claims.Add(struct (id, decl))
                PublishedSurfaceBuilder.addTypeName sctx.Surface id.Key
            | ValueNone ->
                match SigDecl.unmodelledReason decl with
                | ValueSome reason -> publishUnmodelled sctx containment (SigDecl.typeName decl) reason
                // The claim itself was refused (a dotted name, or a name already taken), so
                // there is no gap to publish: the name belongs to whatever claimed it.
                | ValueNone -> ()

        // Over ALL of them, not only the claimed: a declaration that claims no type still
        // writes type names that must resolve.
        for decl in decls do
            classifyDeclaredSigTypes ctx decl

        // The abbreviation entry is filed ahead of every other kind's detail, matching the
        // implementation group's phases.
        for struct (id, decl) in claims do
            match decl with
            | SigDecl.Abbrev(typeName = tn; rhs = rhs; extensions = ext) ->
                registerAbbreviationDecl ctx id tn rhs ext.IsSome
            | SigDecl.IntrinsicAbbrev(typeName = tn; kindTag = tag; instrParts = parts; extensions = ext) ->
                registerIntrinsicReprDecl ctx id tn tag parts ext.IsSome
            | SigDecl.Record _
            | SigDecl.Union _
            | SigDecl.Enum _
            | SigDecl.Extern _
            | SigDecl.ClassLike _
            | SigDecl.Opaque _
            | SigDecl.Delegate _
            | SigDecl.TypeExtension _ -> ()

        for struct (id, decl) in claims do
            registerSigDetail sctx id decl

        // Group close: force every alias body, so one nothing referenced is still filled and
        // a cyclic pair among them diagnoses here.
        for struct (id, decl) in claims do
            match decl with
            | SigDecl.Abbrev _ ->
                match TypeRegistry.tryAbbrevByKey ctx.Types id.Key with
                | ValueSome info -> forceFill ctx info
                | ValueNone -> ()
            | _ -> ()

        for struct (id, decl) in claims do
            publishType sctx id decl

    /// Internal-or-better, matching what a frozen implementation file publishes: a `.fsi`
    /// makes a declaration inaccessible OUTSIDE its file by not declaring it, and the
    /// cross-assembly public-only cut belongs to the consumer, not to publication.
    let private isPublished (access: Access<SyntaxToken> voption) : bool =
        match access with
        | ValueNone
        | ValueSome(Access.Public _)
        | ValueSome(Access.Internal _) -> true
        | ValueSome(Access.Private _) -> false

    let private registerValSig
        (sctx: SigCtx)
        (containment: DeclContainment<SyntaxToken>)
        (valSig: ValSig<SyntaxToken>)
        : unit =
        let ctx = sctx.Pass

        let (ValSig(attributes = attrs; access = access; ident = ident; typars = tds; signature = csig)) =
            valSig

        classifyValSigTypes ctx valSig

        if isPublished access then
            let sourceName = OperatorNames.ofDeclaredName ctx.NameOf ident

            let compiledName =
                match AttributeDecode.tryCompiledName ctx.NameOf (ctx.ResolveAttributes attrs) with
                | ValueSome n -> ValueSome n
                | ValueNone -> sourceName

            match compiledName with
            | ValueNone -> ()
            | ValueSome name ->
                let decl = localContainerChain ctx containment
                let explicit = explicitTyparNames ctx tds

                let implicit =
                    implicitTyparNames ctx explicit tds (fun it -> CstTypeWalk.iterTypeCurriedSig it csig)

                let typeParams = mkTypeParams ctx.Store (explicit @ implicit)

                let domains, ret =
                    underTypars ctx EqArray.empty typeParams (fun () -> translateSigGroups ctx csig)

                let env = typarEnv ctx (TyparOwner.Value typeParams)
                let template = freezeOver ctx env (curriedFunTy domains ret)

                let constraints =
                    underTypars
                        ctx
                        EqArray.empty
                        typeParams
                        (fun () ->
                            publishedConstraints
                                ctx
                                env
                                typeParams
                                (collectWhenClauses tds (fun it -> CstTypeWalk.iterTypeCurriedSig it csig))
                        )

                // The SOURCE arity of each group: `a * b ->` is one group of width 2,
                // `(a * b) ->` is width 1. A consumer reads the grouping to decide whether a
                // value use needs a curried adapter.
                let valRepr =
                    let arities = [ for d in domains -> d.Length ]
                    let paramTys, resultTy = TastLower.peelFunDomains arities.Length template

                    if List.isEmpty arities || List.length paramTys <> List.length arities then
                        ValueNone
                    else
                        ValueSome(TastLower.externalValRepr typeParams.Length (List.zip arities paramTys) resultTy)

                let sym =
                    { ExternalSymbols.scheme decl name template typeParams.Length constraints with
                        ValRepr = valRepr
                    }

                let source =
                    sourceName
                    |> ValueOption.map (fun n ->
                        {
                            Path = DeclContainment.sourcePath ctx.NameOf containment
                            Name = n
                        }
                        : SourceSpelling
                    )

                PublishedSurfaceBuilder.addValue sctx.Surface source sym

    // --- the walk -------------------------------------------------------------------

    /// The `[<AutoOpen>]` modules a signature declares, outermost first: what a consumer
    /// resolves through with no `open` of its own. A module holding nothing is not listed.
    let private autoOpenPrefixes (ctx: PassContext) (walked: WalkedSigElem<SyntaxToken> list) : string list =
        let seen = HashSet<string>(System.StringComparer.Ordinal)
        let acc = ResizeArray<string>()

        for w in walked do
            for prefix in ctx.AutoOpenContainersOf w.Containment do
                if seen.Add prefix then
                    acc.Add prefix

        List.ofSeq acc

    let private declsOf (TypeSignatures(first = first; rest = rest)) : SigDecl list =
        [ yield sigDeclOf first; for (_, ts) in rest -> sigDeclOf ts ]

    /// Resolve one `.fsi` against the provider `ctx` carries, and hand back the surface it
    /// publishes. Diagnostics land on `ctx`, anchored in the signature's own text.
    let run (ctx: PassContext) (inputs: SignatureInputs) (file: SignatureFile<SyntaxToken>) : PublishedSurface =
        let surface = PublishedSurfaceBuilder.create ()

        let sctx =
            {
                Pass = ctx
                Surface = surface
                Inputs = inputs
            }

        let walked = CstModuleTree.walkSig ctx.NameOf ctx.Resolution.AmbientOpenScope file

        // Whole-file pre-scan: the `…Module` suffix rule reads `NominalTypeNames` at the very
        // first key mint, and a `module Foo` may textually precede the `type Foo` it collides
        // with, so every type name must be known before any name below is minted.
        for w in walked do
            match w.Elem with
            | ModuleSignatureElement.Type(typeSigs = typeSigs) ->
                for decl in declsOf typeSigs do
                    noteNominalSigTypeName ctx decl
            | _ -> ()

        surface.AmbientOpenPrefixes <- autoOpenPrefixes ctx walked

        for w in walked do
            ctx.EnterElement w

            match w.Elem with
            | ModuleSignatureElement.Type(typeToken = kw; typeSigs = typeSigs) ->
                let decls = declsOf typeSigs

                let visibleFrom = sigGroupVisibleFrom w.RecScopeOffset kw decls
                registerSigGroup sctx w.Containment visibleFrom decls
            | ModuleSignatureElement.Val valSig -> registerValSig sctx w.Containment valSig
            | ModuleSignatureElement.ValLiteral _
            | ModuleSignatureElement.Exception _
            | ModuleSignatureElement.Module _
            | ModuleSignatureElement.ModuleAbbrev _
            | ModuleSignatureElement.Import _
            | ModuleSignatureElement.CompilerDirective _
            | ModuleSignatureElement.Missing
            | ModuleSignatureElement.SkipsTokens _ -> ()

        PublishedSurface.ofBuilder surface

    /// `run` on a `PassContext` of the signature's own: `NodeKey` offsets are per-file, so a
    /// `.fsi` never shares its companion's context, and the diagnostics come back here rather
    /// than accumulating on a context the caller kept.
    let resolveFile
        (visible: IExternalSymbolProvider)
        (lexed: LexedFile)
        (inputs: SignatureInputs)
        (file: SignatureFile<SyntaxToken>)
        : PublishedSurface * Diagnostic list =
        let ctx =
            PassContext(
                visible,
                lexed,
                {
                    Name = inputs.Assembly
                    Target = inputs.Target
                }
            )

        let surface = run ctx inputs file
        surface, List.ofSeq ctx.Diagnostics
