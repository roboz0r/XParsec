namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open Vesper
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRegistration
open NameResolutionDeclRegistration
open NameResolutionUnionRegistration
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
        PublishedSurfaceBuilder.addType sctx.Surface key shape

    let private publishShapeWith
        (sctx: SigCtx)
        (key: TypeKey)
        (shape: ExternalTypeShape)
        (members: seq<ExternalMember>)
        : unit =
        PublishedSurfaceBuilder.addTypeWith sctx.Surface key shape members

    let private publishMembers (sctx: SigCtx) (key: TypeKey) (members: ExternalMember list) : unit =
        PublishedSurfaceBuilder.addMembers sctx.Surface key members

    let private publishAttributes (sctx: SigCtx) (key: TypeKey) (attributes: TAttributes) : unit =
        PublishedSurfaceBuilder.addAttributes sctx.Surface (SymbolKey.Type key) attributes

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
        | SigDecl.Measure _
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
            let env = scopedEnv ctx (TyparScope.Type key) info.TypeParams

            let fields =
                Block.ofSeq
                    [
                        for f in info.Fields ->
                            {
                                Name = f.Name
                                IsMutable = f.IsMutable
                                Frozen = freezeOver ctx env f.Type
                            }
                            : ExternalFieldShape
                    ]

            publishShape
                sctx
                key
                (ExternalTypeShape.Record
                    {
                        Typars = TyparList.unconstrained info.TypeParams
                        Fields = fields
                        Origin = SymbolOrigin.Empty
                        IsValueType = info.IsValueType
                        RequiresQualifiedAccess = info.IsRequireQualifiedAccess
                    })

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
            let env = scopedEnv ctx (TyparScope.Type key) info.TypeParams

            let cases =
                Block.ofSeq
                    [
                        for c in info.Cases ->
                            {
                                Name = c.Name
                                FieldNames = Block.ofArray c.FieldNames
                                FrozenFieldTypes = Block.ofSeq (seq { for t in c.Fields -> freezeOver ctx env t })
                            }
                            : ExternalCaseShape
                    ]

            let extensionElems =
                match extensions with
                | ValueSome(TypeExtensionElementsSignature(elements = elems)) -> elems
                | ValueNone -> System.Collections.Immutable.ImmutableArray.Empty

            // A union's trailing `with interface <ty>` impls are carried on the shared
            // extension list; there is no union-specific parser field.
            let interfaces =
                freezeInterfaces sctx key info.TypeParams (interfaceSpecsOf extensionElems)

            let members = resolveBodyMembers sctx key info.TypeParams extensionElems

            publishShapeWith
                sctx
                key
                (ExternalTypeShape.Union
                    {
                        Typars = TyparList.unconstrained info.TypeParams
                        Cases = cases
                        Interfaces = interfaces
                        Origin = SymbolOrigin.Empty
                        IsValueType = info.IsValueType
                        RequiresQualifiedAccess = info.IsRequireQualifiedAccess
                    })
                members

    let private publishEnum (sctx: SigCtx) (id: TypeIdentity) : unit =
        match TypeRegistry.tryEnumByKey sctx.Pass.Types id.Key with
        | ValueNone -> ()
        | ValueSome info -> publishShape sctx id.Key (ExternalEnumShape.ofCases info.Cases SymbolOrigin.Empty)

    // --- abbreviations --------------------------------------------------------------------

    let private publishAbbrev (sctx: SigCtx) (id: TypeIdentity) : unit =
        let ctx = sctx.Pass

        let info = TypeRegistry.abbrevOfClaim ctx.Types id
        let env = scopedEnv ctx (TyparScope.Type id.Key) info.TypeParams

        let body =
            match info.TryFilled with
            | ValueSome ty -> freezeOver ctx env ty
            | ValueNone -> ExternalSignature.unfreezable (sprintf "abbreviation '%s' has no body" id.Name)

        publishShape
            sctx
            id.Key
            (ExternalTypeShape.Abbrev
                {
                    Typars = TyparList.unconstrained info.TypeParams
                    Body = body
                })

    /// A broken measure publishes nothing; its declaration already reported.
    let private publishMeasure (sctx: SigCtx) (id: TypeIdentity) : unit =
        match (TypeRegistry.measureOfClaim sctx.Pass.Types id).TryFilled with
        | ValueSome term -> publishShape sctx id.Key (ExternalTypeShape.Measure term)
        | ValueNone -> ()

    /// `type t = (# "…" #)` written in a SIGNATURE: the same primitive binding it is in an
    /// implementation, and registering its entry already filed the binding below.
    let private publishIntrinsicAbbrev
        (sctx: SigCtx)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (kindTag: ExternKind<SyntaxToken> voption)
        : unit =
        let ctx = sctx.Pass
        let canon = TypeRegistry.intrinsicKeyOf ctx.Types id.Name

        let platform =
            match ctx.Types.IntrinsicBindings.TryGetValue canon with
            | true, binding -> IntrinsicPlatform.Bound binding.TypeId
            | _ -> IntrinsicPlatform.Unsupported sctx.Inputs.Target

        let declared =
            match kindTag with
            | ValueSome(ExternKind.Class _) -> ExternForm.Heritable
            | _ -> ExternForm.Opaque

        PublishedSurfaceBuilder.addExternForm sctx.Surface canon declared

        publishShape
            sctx
            id.Key
            (ExternalTypeShape.Intrinsic(IntrinsicShape.Scalar(canon, typarListOfTypeName ctx tn, platform)))

    // --- `extern` -------------------------------------------------------------------------

    /// Which of the three surfaces an `extern` declares. They publish different shapes, and
    /// its keyword tag is the only thing that says which.
    let private externFormOfKindTag (kindTag: ExternKind<SyntaxToken> voption) : ExternForm =
        match kindTag with
        | ValueSome(ExternKind.Interface _) -> ExternForm.Capability
        | ValueSome(ExternKind.Class _) -> ExternForm.Heritable
        | _ -> ExternForm.Opaque

    /// A capability IS an interface, so its `inherit` clause is interface inheritance and
    /// its members are carried on the shape. Both primitives are classes.
    let private externDeclaresInterface (form: ExternForm) : bool = form = ExternForm.Capability

    /// The type id the paired implementation binds for this `extern`. A PRIMITIVE's is FILED
    /// on the intrinsic's own key so a use site resolves the name to it; a capability's is
    /// not, because a capability is a nominal interface that only CARRIES a platform type id
    /// and has no intrinsic identity to file under. WHICH id is the implementation's
    /// business, so a target that binds none still publishes the type, marked unsupported.
    let private bindExternTypeId (sctx: SigCtx) (id: TypeIdentity) (form: ExternForm) : IntrinsicPlatform =
        match sctx.Inputs.Bindings.TryGetValue id.Name with
        | true, typeId ->
            let fileOn (heritable: bool) =
                sctx.Pass.Types.IntrinsicBindings.[TypeRegistry.intrinsicKeyOf sctx.Pass.Types id.Name] <-
                    {
                        TypeId = typeId
                        Heritable = heritable
                    }

            match form with
            | ExternForm.Capability -> ()
            | ExternForm.Heritable -> fileOn true
            | ExternForm.Opaque -> fileOn false

            IntrinsicPlatform.Bound typeId
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
    let private platformCtors (platform: IntrinsicPlatform) (members: ExternalMember list) : Block<ExternalMember> =
        match platform with
        | IntrinsicPlatform.Bound typeId ->
            let platformDecl = SymbolKeyOps.qualifiedTypeKeyOf typeId.Value 0

            Block.ofList
                [
                    for m in members do
                        if m.Name = ".ctor" then
                            { m with
                                Key = { m.Key with Decl = platformDecl }
                            }
                ]
        | IntrinsicPlatform.Unsupported _ -> Block.empty

    /// The shape a CAPABILITY publishes: it IS the interface, canon'd on its own identity,
    /// with the platform spelling beside it where the target binds one. Nominal either way —
    /// a use site kinds it `TyClass` from the shape, which is why it claims a class.
    let private publishCapability
        (sctx: SigCtx)
        (id: TypeIdentity)
        (typars: TyparList)
        (platform: IntrinsicPlatform)
        (surface: BodiedSurface voption)
        : unit =
        let shape =
            match surface with
            | ValueSome s -> s.Shape
            | ValueNone -> ExternalClassShape.basic (typars, ClassCommitment.Interface, SymbolOrigin.Empty)

        match platform with
        | IntrinsicPlatform.Bound typeId ->
            publishShapeWith
                sctx
                id.Key
                (ExternalTypeShape.IntrinsicInterface
                    {
                        Canon = id.Key
                        Typars = shape.Typars
                        Platform = typeId
                        Members = shape.Members
                        Interfaces = shape.FrozenInterfaces
                        Origin = SymbolOrigin.Empty
                    })
                shape.Members
        // CANON-ONLY: the same interface, reachable by its own name alone.
        | IntrinsicPlatform.Unsupported _ -> publishShapeWith sctx id.Key (ExternalTypeShape.Class shape) shape.Members

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
                    Interfaces = Block.empty
                    Members = Block.empty
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
                    Members = Block.empty
                }

    /// The shape a PRIMITIVE `extern` publishes: its intrinsic identity, under which a use site
    /// resolves the name, carrying whichever supertype surface its form declares.
    let private publishExternPrimitive
        (sctx: SigCtx)
        (id: TypeIdentity)
        (typars: TyparList)
        (platform: IntrinsicPlatform)
        (classSurface: IntrinsicClassSurface voption)
        (members: seq<ExternalMember>)
        : unit =
        publishShapeWith
            sctx
            id.Key
            (ExternalTypeShape.Intrinsic
                {
                    Id =
                        {
                            Canon = TypeRegistry.intrinsicKeyOf sctx.Pass.Types id.Name
                            Typars = typars
                            Platform = platform
                        }
                    Class = classSurface
                })
            members

    let private publishExtern
        (sctx: SigCtx)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (kindTag: ExternKind<SyntaxToken> voption)
        (members: TypeExtensionElementsSignature<SyntaxToken> voption)
        : unit =
        let ctx = sctx.Pass
        let form = externFormOfKindTag kindTag
        let platform = bindExternTypeId sctx id form

        // A capability's canon IS its own identity; a primitive's is the intrinsic key its
        // repr is filed on, which is what an implementation files its binding under.
        let canon =
            match form with
            | ExternForm.Capability -> id.Key
            | ExternForm.Heritable
            | ExternForm.Opaque -> TypeRegistry.intrinsicKeyOf ctx.Types id.Name

        PublishedSurfaceBuilder.addExternForm sctx.Surface canon form

        let declared =
            match members with
            | ValueSome(TypeExtensionElementsSignature(elements = elems)) when not elems.IsEmpty -> ValueSome elems
            | _ -> ValueNone

        let surface =
            declared
            |> ValueOption.map (fun elems ->
                requireInlineExternMembers ctx id elems
                bodiedClassSurface sctx id tn (externDeclaresInterface form) elems
            )

        // `scalarSurface` drops the shape's members, so the declared list is what reaches
        // the member table.
        let members =
            match surface with
            | ValueSome s -> s.Members
            | ValueNone -> []

        let typars =
            match surface with
            | ValueSome s -> s.Shape.Typars
            | ValueNone -> typarListOfTypeName ctx tn

        match form with
        | ExternForm.Capability -> publishCapability sctx id typars platform surface
        | ExternForm.Heritable ->
            publishExternPrimitive sctx id typars platform (heritableSurface platform surface) members
        | ExternForm.Opaque -> publishExternPrimitive sctx id typars platform (scalarSurface surface) members

    // --- class-like ------------------------------------------------------------------------

    let private publishClassLike
        (sctx: SigCtx)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (form: SigClassForm)
        (isInterface: bool)
        (elems: TypeElementsSignature<SyntaxToken>)
        : unit =
        let surface = bodiedClassSurface sctx id tn isInterface elems

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

        publishShapeWith sctx id.Key (ExternalTypeShape.Class shape) surface.Members

    /// An opaque abstract type (`type T`) has no body shape. It resolves as a non-interface
    /// class, so codegen can mint a ref off the origin, and commits its name to no family.
    let private publishOpaque (sctx: SigCtx) (id: TypeIdentity) (tn: TypeName<SyntaxToken>) : unit =
        publishShape
            sctx
            id.Key
            (ExternalTypeShape.Class(
                ExternalClassShape.basic (typarListOfTypeName sctx.Pass tn, ClassCommitment.Opaque, SymbolOrigin.Empty)
            ))

    /// The unmodelled forms refused at their own declaration, matching the implementation
    /// side's Validation verdict; the rest report at first use.
    let private reportRefusedDeclaration
        (ctx: PassContext)
        (tn: TypeName<SyntaxToken>)
        (reason: UnmodelledReason)
        : unit =
        match reason with
        | UnmodelledReason.Delegate ->
            match CstKeys.tryFirstTokenOfTypeName tn with
            | ValueSome tok -> ctx.Report(tok, Kind.NotYetSupported "`delegate` type declarations")
            | ValueNone -> ()
        | _ -> ()

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
            let typars = typarListOfTypeName ctx tn
            let name = ctx.NameOf nameLi.Idents.[0]

            let key =
                SymbolKeyOps.typeKeyOfContainer (localTypeContainer ctx containment) name typars.TypeArity

            publishShape sctx key (ExternalTypeShape.Unmodelled(reason, typars))

    /// The shape ONE claimed declaration publishes, once every declaration in its group has
    /// registered its detail: a field or case type may reference a sibling.
    let private publishType (sctx: SigCtx) (id: TypeIdentity) (decl: SigDecl) : unit =
        match decl with
        | SigDecl.Record(extensions = ext) -> publishRecord sctx id ext
        | SigDecl.Union(extensions = ext) -> publishUnion sctx id ext
        | SigDecl.Enum _ -> publishEnum sctx id
        | SigDecl.Abbrev _ -> publishAbbrev sctx id
        | SigDecl.IntrinsicAbbrev(typeName = tn; kindTag = tag) -> publishIntrinsicAbbrev sctx id tn tag
        | SigDecl.Extern(typeName = tn; kindTag = kindTag; members = members) ->
            publishExtern sctx id tn kindTag members
        | SigDecl.ClassLike(typeName = tn; form = form; elements = elems) ->
            publishClassLike sctx id tn form (SigDecl.isInterfaceForm decl) elems
        | SigDecl.Opaque tn -> publishOpaque sctx id tn
        | SigDecl.Measure _ -> publishMeasure sctx id
        // Claimed nothing, so it is not one of the identities this runs over; it published its
        // gap at claim time.
        | SigDecl.Delegate _
        | SigDecl.TypeExtension _ -> ()

    // --- groups and vals ---------------------------------------------------------------

    /// The classes this signature publishes above the element being resolved, read off the
    /// surface under construction, and the classes its references publish. A signature's
    /// walk is sequential, so a class is readable from the element after its own.
    let private attributeClasses (sctx: SigCtx) : IAttributeClassSource =
        let published =
            { new IAttributeClassSource with
                member _.Ctors attrKey =
                    match sctx.Surface.MembersByKey.TryGetValue attrKey with
                    | true, members -> AttributeClasses.ctorsOfMembers (Block.ofResizeArray members)
                    | false, _ -> Block.empty

                member _.TrySettable(attrKey, name) =
                    match sctx.Surface.MembersByKey.TryGetValue attrKey with
                    | true, members ->
                        members
                        |> Seq.filter (fun m -> m.Name = name)
                        |> Block.ofSeq
                        |> AttributeClasses.settableOfMembers name
                    | false, _ -> ValueNone
            }

        AttributeClasses.firstDeclaring published (AttributeClasses.ofStore sctx.Pass.Provider)

    let private registerSigGroup
        (sctx: SigCtx)
        (containment: DeclContainment<SyntaxToken>)
        (placement: ClaimPlacement)
        (decls: SigDecl list)
        : unit =
        let ctx = sctx.Pass
        let claims = ResizeArray<struct (TypeIdentity * SigDecl)>()
        let groupInterfaceKeys = ResizeArray<TypeKey>()

        for decl in decls do
            match claimSigTypeIdentity ctx containment placement decl with
            | ValueSome id ->
                claims.Add(struct (id, decl))

                // Interface-ness is read off the declaration's written form, so a group
                // member referencing a sibling declared below it classifies correctly.
                if SigDecl.isInterfaceForm decl then
                    groupInterfaceKeys.Add id.Key
            | ValueNone ->
                match SigDecl.unmodelledReason decl with
                | ValueSome reason ->
                    reportRefusedDeclaration ctx (SigDecl.typeName decl) reason
                    publishUnmodelled sctx containment (SigDecl.typeName decl) reason
                // The claim itself was refused (a dotted name, or a name already taken), so
                // there is no gap to publish: the name belongs to whatever claimed it.
                | ValueNone -> ()

        // The group's interface claims are visible to `sigIsInterfaceKey` in the phases
        // below, before any of the group's shapes publish.
        let sctx =
            { sctx with
                GroupInterfaceKeys = EqSet.ofSeq groupInterfaceKeys
            }

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
                registerIntrinsicBindingDecl ctx id tn tag parts ext.IsSome
            | SigDecl.Measure(typeName = tn; rhs = rhs) -> registerMeasureDecl ctx id tn rhs
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

        forceGroupBodies ctx (seq { for struct (id, _) in claims -> id })

        for struct (id, decl) in claims do
            publishType sctx id decl

        // A group member's attribute may refer to a class the group declares below it
        // (`[<AbstractClass>] type Attribute … and AbstractClassAttribute`), so the group's
        // positions are checked once every member's constructors are published.
        ctx.CheckPendingAttributes(attributeClasses sctx)

        for struct (id, _) in claims do
            match ctx.TryAttributesAt(AttributeSite.ofSite id.DeclSite) with
            | ValueSome attributes -> publishAttributes sctx id.Key attributes
            | ValueNone -> ()

    /// Internal-or-better, matching what a frozen implementation file publishes: a `.fsi`
    /// makes a declaration inaccessible OUTSIDE its file by not declaring it, and the
    /// cross-assembly public-only cut belongs to the consumer, not to publication.
    let private isPublished (access: Access<SyntaxToken> voption) : bool =
        match access with
        | ValueNone
        | ValueSome(Access.Public _)
        | ValueSome(Access.Internal _) -> true
        | ValueSome(Access.Private _) -> false

    /// What a `[<Literal>] val X: T = e` denotes, whose type equals the annotation's
    /// `template`. A `[<Literal>]` without a value, a value without `[<Literal>]`, and a
    /// value of another type are each reported at the signature.
    let private signatureLiteral
        (ctx: PassContext)
        (ident: IdentOrOp<SyntaxToken>)
        (resolvedAttrs: ResolvedAttributes)
        (template: FrozenType)
        (literalValue: (SyntaxToken * Expr<SyntaxToken>) voption)
        : TConstDenotation voption =
        match resolvedAttrs.Has RuntimeNames.literalAttributeKey, literalValue with
        | false, ValueNone -> ValueNone
        | true, ValueNone ->
            ctx.Report(CstKeys.firstTokenOfIdentOrOp ident, Kind.SignatureLiteralWithoutValue)
            ValueNone
        | false, ValueSome(eq, _) ->
            ctx.Report(eq, Kind.SignatureValueWithoutLiteral)
            ValueNone
        | true, ValueSome(_, e) ->
            ConstExprCheck.check ctx (ctx.UseSiteAt(CstKeys.ofExpr e)) (ValueSome template) e
            |> ValueOption.map TConstExpr.denotation

    let private registerValSig
        (sctx: SigCtx)
        (containment: DeclContainment<SyntaxToken>)
        (valSig: ValSig<SyntaxToken>)
        : unit =
        let ctx = sctx.Pass

        let (ValSig(
            attributes = attrs
            access = access
            ident = ident
            typars = tds
            signature = csig
            literalValue = literalValue)) =
            valSig

        classifyValSigTypes ctx valSig

        if isPublished access then
            let resolvedAttrs = ctx.ResolveAttributes attrs

            // An active-pattern name has no modelled source form, so it is skipped here as in
            // the implementation half (`MemberNames.ofBinding`).
            match OperatorNames.ofDeclaredName ctx.NameOf ident with
            | ValueNone -> ()
            | ValueSome name ->
                let decl = localContainerChain ctx containment
                let explicit = explicitTypars ctx tds

                let implicit =
                    implicitTyparNames
                        ctx
                        (List.map fst explicit)
                        tds
                        (fun it -> CstTypeWalk.iterTypeCurriedSig it csig)

                // An implicit typar is one the signature mentions without declaring, which only a
                // type position can do.
                let typeParams =
                    mkDeclaredTypars ctx.Store (explicit @ [ for n in implicit -> n, TyparKind.Type ])

                let scope = TyparScope.ModuleFunction(SymbolKeyOps.bindingKeyOf decl name)

                let domains, ret =
                    underTypars ctx [ scope, typeParams ] (fun () -> translateSigGroups ctx csig)

                let env = scopedEnv ctx scope typeParams

                let template = freezeOver ctx env (curriedFunTy domains ret)

                let generics =
                    underTypars
                        ctx
                        [ scope, typeParams ]
                        (fun () ->
                            publishedScheme
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
                        ValueSome(TastLower.externalValRepr generics.TyparArity (List.zip arities paramTys) resultTy)

                let attrElement =
                    AttrTarget.ofModuleValue (not (List.isEmpty domains)) (typeParams.Length <> 0)

                let sym =
                    { ExternalSymbols.scheme decl name template generics with
                        CompiledName = AttributeDecode.compiledNameOf ctx.NameOf name resolvedAttrs
                        ValRepr = valRepr
                        Literal = signatureLiteral ctx ident resolvedAttrs template literalValue
                    }

                PublishedSurfaceBuilder.addValue sctx.Surface sym

                let attributeSite = AttributeSite.ofToken (CstKeys.firstTokenOfIdentOrOp ident)
                ctx.DeclareAttributes(attributeSite, attrElement, resolvedAttrs)
                ctx.CheckPendingAttributes(attributeClasses sctx)

                PublishedSurfaceBuilder.addAttributes
                    sctx.Surface
                    (SymbolKey.Binding sym.Key)
                    (ctx.AttributesAt attributeSite)

    // --- the walk -------------------------------------------------------------------

    let private declsOf (ctx: PassContext) (TypeSignatures(first = first; rest = rest)) : SigDecl list =
        [ yield sigDeclOf ctx first; for (_, ts) in rest -> sigDeclOf ctx ts ]

    /// Resolve one `.fsi` against the provider `ctx` carries, and hand back the surface it
    /// publishes. Diagnostics land on `ctx`, anchored in the signature's own text.
    let run (ctx: PassContext) (inputs: SignatureInputs) (file: SignatureFile<SyntaxToken>) : PublishedSurface =
        let surface = PublishedSurfaceBuilder.create ()

        let sctx =
            {
                Pass = ctx
                Surface = surface
                Inputs = inputs
                GroupInterfaceKeys = EqSet.empty
            }

        let walked = CstModuleTree.walkSig ctx.NameOf OpenScope.empty file

        // Whole-file pre-scan: the `…Module` suffix rule reads `NominalTypeNames` at the very
        // first key mint, and a `module Foo` may textually precede the `type Foo` it collides
        // with, so every type name must be known before any name below is minted.
        for w in walked do
            match w.Elem with
            | ModuleSignatureElement.Type(typeSigs = typeSigs) ->
                for decl in declsOf ctx typeSigs do
                    noteNominalSigTypeName ctx decl
            | _ -> ()

        // Every containment registered before anything resolves: container visibility is
        // positional (`LocalContainer.VisibleFrom`), so an alias or a qualified annotation read
        // below reaches a module declared anywhere in the file, rec-hoisted ones included.
        for w in walked do
            ctx.EnterContainment(w.Containment, w.RecScopeOffset) |> ignore<ModuleContainer>

        for w in walked do
            ctx.EnterElement w

            match w.Elem with
            | ModuleSignatureElement.Type(typeToken = kw; typeSigs = typeSigs) ->
                let decls = declsOf ctx typeSigs

                let placement = sigGroupPlacement w.RecScopeOffset kw decls
                registerSigGroup sctx w.Containment placement decls
            | ModuleSignatureElement.Val valSig -> registerValSig sctx w.Containment valSig
            | ModuleSignatureElement.ModuleAbbrev abbrev -> ctx.ReportAbbrevTarget(w.Containment, abbrev)
            | ModuleSignatureElement.Import import -> ctx.ReportOpenTarget(w.Containment, import)
            | ModuleSignatureElement.Exception _
            | ModuleSignatureElement.Module _
            | ModuleSignatureElement.CompilerDirective _
            | ModuleSignatureElement.Missing
            | ModuleSignatureElement.SkipsTokens _ -> ()

        for KeyValue(m, facts) in ctx.Types.Modules do
            PublishedSurfaceBuilder.addModule
                surface
                m
                {
                    Home = SymbolHome.InFile ctx.File.Path
                    Facts = facts
                }

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
        Attributes.run ctx
        surface, List.ofSeq ctx.Diagnostics
