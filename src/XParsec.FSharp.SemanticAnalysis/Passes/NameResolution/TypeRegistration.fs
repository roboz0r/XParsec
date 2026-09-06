namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRefStamp
open UnificationTranslate

// The type-identity CLAIM of each definition, the typar model its header declares, and the
// classification of every type name a module element writes. Each kind's registry entry is
// built in `NameResolutionDeclRegistration`.

module NameResolutionTypeRegistration =

    /// A `Typar`'s source-text name; the leading `'`/`^` lives on a separate
    /// token. Anon (`_`) typars don't participate in scope.
    let typarName (ctx: PassContext) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome(ctx.NameOf id)
        | Typar.Anon _ -> ValueNone

    /// Declared typars for a `TypeName`, in source order: prefix typars (`'a Box`) first, then
    /// suffix (`Box<'a, 'b>`), each with the attribute set written on it. Skips anonymous
    /// typars. A prefix typar has no attribute slot in the grammar.
    let private typarSlotsOfTypeName
        (ctx: PassContext)
        (tn: TypeName<SyntaxToken>)
        : (string * Attributes<SyntaxToken> voption) list =
        let (TypeName(prefixTypars = pt; typarDefns = td)) = tn

        let prefix =
            [
                match pt with
                | ValueNone -> ()
                | ValueSome(PrefixTypars.Single t) ->
                    match typarName ctx t with
                    | ValueSome n -> yield n, ValueNone
                    | ValueNone -> ()
                | ValueSome(PrefixTypars.Multiple(typars = ts)) ->
                    for t in ts do
                        match typarName ctx t with
                        | ValueSome n -> yield n, ValueNone
                        | ValueNone -> ()
            ]

        let main =
            [
                match td with
                | ValueNone -> ()
                | ValueSome(TyparDefns(defns = ds)) ->
                    for TyparDefn(attributes = attrs; typar = t) in ds do
                        match typarName ctx t with
                        | ValueSome n -> yield n, attrs
                        | ValueNone -> ()
            ]

        prefix @ main

    /// `Measure` where `[<Measure>]` is written on the slot. Reports no diagnostic; callable at
    /// CLAIM time.
    let private kindOfSlot (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) : TyparKind =
        if ctx.HasAttribute(attrs, RuntimeNames.measureAttributeKey) then
            TyparKind.Measure
        else
            TyparKind.Type

    /// The declared typars, named and kinded, in source order.
    let typarListOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : TyparList =
        TyparList.ofSeq (
            seq {
                for (name, attrs) in typarSlotsOfTypeName ctx tn ->
                    {
                        TTypeParam.Name = name
                        Kind = kindOfSlot ctx attrs
                    }
            }
        )

    /// The generic arity that keys this type in the registries (`0` for a non-generic name).
    let arityOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : int =
        typarSlotsOfTypeName ctx tn |> List.length

    /// The `when 'a : …` clause on a `TypeName`, if any. Retained on the registry entry so a
    /// consumer re-entering the declaration's typar scope later need not re-walk the CST.
    let typarConstraintsOfTypeName (tn: TypeName<SyntaxToken>) : TyparConstraints<SyntaxToken> voption =
        let (TypeName(typarDefns = td)) = tn

        match td with
        | ValueSome(TyparDefns(constraints = ValueSome tc)) -> ValueSome tc
        | _ -> ValueNone

    let private newTypar (store: TypeStore) : TyVarId =
        let tv = store.NewTypeVar()
        store.SetLevel(UnionFind.find store tv, 0)
        tv

    /// A type declaration's typars, each carrying its kind and a freshly minted prototype
    /// TyVar. Stored on the registry entry. An unresolved attribute on a typar is reported here.
    let declaredTyparsOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : EqArray<DeclaredTypar> =
        EqArray.ofSeq (
            seq {
                for (name, attrs) in typarSlotsOfTypeName ctx tn do
                    ctx.ResolveAttributes attrs |> ignore

                    yield
                        {
                            Name = name
                            TyVar = newTypar ctx.Store
                            Kind = kindOfSlot ctx attrs
                        }
            }
        )

    /// Mint a prototype TyVar per name, for typars this pass reads by name alone: a MEMBER's
    /// own `<'C>` and a VALUE signature's. `[<Measure>]` on one of those is not yet modelled,
    /// so they are all type-kinded.
    let mkMethodTypars (store: TypeStore) (names: string list) : EqArray<DeclaredTypar> =
        EqArray.ofSeq (
            seq {
                for n in names ->
                    {
                        Name = n
                        TyVar = newTypar store
                        Kind = TyparKind.Type
                    }
            }
        )

    /// The container a TYPE declared in `c` sits in, under this file's module-naming rules.
    let localTypeContainer (ctx: PassContext) (c: DeclContainment<SyntaxToken>) : TypeContainer = ctx.TypeContainerOf c

    /// The container a BINDING declared in `c` sits in, and the chain a TYPE's `TypeContainer` narrows.
    let localContainerChain (ctx: PassContext) (c: DeclContainment<SyntaxToken>) : ModuleContainer =
        ctx.ContainerChainOf c

    /// The registered `ClassTypeInfo` of the class-like DECLARATION `tn` declares, recovered by
    /// the key the declaration mints in the module the walk stands in, never by its name: an
    /// arity-overloaded `Box\`1`/`Box\`2` has no bare name, and two modules may each declare `C`.
    let tryDeclaredClass (ctx: PassContext) (tn: TypeName<SyntaxToken>) : ClassTypeInfo voption =
        let (TypeName(ident = nameLi)) = tn

        if nameLi.Idents.Length = 1 then
            TypeRegistry.tryClassByKey
                ctx.Types
                (ctx.DeclaredTypeKey(ctx.NameOf nameLi.Idents.[0], arityOfTypeName ctx tn))
        else
            ValueNone

    /// The registered union / record / inline intrinsic-abbrev host the DECLARATION `tn`
    /// declares. Key-addressed for the same reason as the class-like case above.
    let tryDeclaredNonClassHost (ctx: PassContext) (tn: TypeName<SyntaxToken>) : IInterfaceImplHost voption =
        let (TypeName(ident = nameLi)) = tn
        let name = ctx.NameOf nameLi.Idents.[0]

        TypeRegistry.tryNonClassMemberHostByDecl ctx.Types (ctx.DeclaredTypeAddress(name, arityOfTypeName ctx tn))

    /// Mint the project-local `SymbolKey` for a type declaration, under the declaring
    /// containment's containment chain. The collision branch is an INTERNAL-ERROR BACKSTOP
    /// because a user duplicate is refused by the claim test upstream and never reaches the mint.
    let private stampLocalTypeKey
        (ctx: PassContext)
        (declSite: NodeSite)
        (container: ModuleContainer)
        (name: string)
        (arity: int)
        : TypeKey =
        let key = LocalSymbolKey.ofType (SymbolKeyOps.typeContainerOf container) name arity

        match TypeRegistry.recordKeyOrigin ctx.Types declSite.Key (SymbolKey.Type key) with
        | ValueSome _ ->
            ctx.Report(
                declSite.Tok,
                Kind.Message(
                    sprintf
                        "Internal error: project-local SymbolKey collision for '%s' (arity %d)"
                        (SymbolKeyOps.typeMetaName key)
                        arity
                )
            )
        | ValueNone -> ()

        key

    /// The assembly whose already-declared type a resolved external shape WITNESSES.
    /// `ValueNone` = not a competing claim: every package's `int` is THE `int`, and
    /// `Abbrev` / `Unmodelled` carry no `SymbolOrigin`.
    let private externalClaimant (shape: ExternalTypeShape) : AssemblyName voption =
        // A stamped home is a claim (its assembly name); an unstamped home makes none. A
        // prior file of this very compilation claims under the compilation's own name.
        let homeName (o: SymbolOrigin) = o.Home.AssemblyOption

        match shape with
        | ExternalTypeShape.Class info -> homeName info.Origin
        | ExternalTypeShape.Record { Origin = o }
        | ExternalTypeShape.Union { Origin = o }
        | ExternalTypeShape.Enum { Origin = o } -> homeName o
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Measure _
        | ExternalTypeShape.Unmodelled _ -> ValueNone

    /// The CS0433 analogue: a `SymbolKey` carries no home assembly, so a declaration whose key a
    /// REFERENCED assembly already publishes is refused because equal keys would let the unifier
    /// unify two different types. A shape homed under `AssemblyName` is this file's own, waived.
    let private diagnoseExternalClaim (ctx: PassContext) (declTok: SyntaxToken) (key: TypeKey) : unit =
        match ctx.Provider.TryLookupType key with
        | ValueNone -> ()
        | ValueSome shape ->
            match externalClaimant shape with
            | ValueSome asm when asm <> ctx.AssemblyName ->
                ctx.Report(
                    declTok,
                    Kind.Message(
                        sprintf
                            "The type '%s' is declared by this project and already exists in the referenced assembly '%s'. A fully-qualified name names at most one type in a compilation, so rename the type, or drop the reference to '%s'."
                            (SymbolKeyOps.typeMetaName key)
                            asm.Name
                            asm.Name
                    )
                )
            | _ -> ()

    /// `[<Measure>]` on the declaration's own header, read at CLAIM time without a verdict;
    /// `registerMeasureDecl` resolves the header's attributes for report.
    let private isMeasureAttributed (ctx: PassContext) (tn: TypeName<SyntaxToken>) : bool =
        ctx.HasAttribute(Attributes.attributesOfTypeName tn, RuntimeNames.measureAttributeKey)

    /// What a `type t [= rhs]` header declares, read the same way from an implementation and
    /// from a signature.
    [<RequireQualifiedAccess>]
    type private DeclHeader =
        /// An `(# … #)` RHS.
        | IntrinsicBinding of
            kindTag: ExternKind<SyntaxToken> voption *
            instrParts: ImmutableArray<StringPart<SyntaxToken>>
        /// A `[<Measure>]` header; `rhs` is `ValueNone` for the body-less `type m`.
        | Measure of rhs: Type<SyntaxToken> voption
        | Abbreviation of rhs: Type<SyntaxToken>
        /// A body-less `type t`.
        | Opaque

    let private declHeaderOf
        (ctx: PassContext)
        (tn: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken> voption)
        : DeclHeader =
        match rhs with
        | ValueSome(Type.ILIntrinsic(kindTag = tag; instrParts = parts)) -> DeclHeader.IntrinsicBinding(tag, parts)
        | _ when isMeasureAttributed ctx tn -> DeclHeader.Measure rhs
        | ValueSome t -> DeclHeader.Abbreviation t
        | ValueNone -> DeclHeader.Opaque

    /// The kind a header claims its name for; an opaque `type t` in an implementation claims
    /// none.
    let private claimOfHeader (tn: TypeName<SyntaxToken>) (header: DeclHeader) =
        match header with
        | DeclHeader.IntrinsicBinding _ -> ValueSome(struct (tn, TypeDeclKind.IntrinsicBinding))
        | DeclHeader.Measure _ -> ValueSome(struct (tn, TypeDeclKind.Measure))
        | DeclHeader.Abbreviation _ -> ValueSome(struct (tn, TypeDeclKind.Abbreviation))
        | DeclHeader.Opaque -> ValueNone

    /// The name a type declaration CLAIMS, and the kind it claims it for. `Interface`,
    /// `Delegate`, `TypeExtension` and a bare `AbstractType` make no claim. `[<Measure>]`
    /// claims a measure in either shape: the body-less `type m` (an `AbstractType`) and the
    /// abbreviation `type v = m / s`.
    let private tryDeclaredTypeName
        (ctx: PassContext)
        (td: TypeDefn<SyntaxToken>)
        : struct (TypeName<SyntaxToken> * TypeDeclKind) voption =
        match td with
        | TypeDefn.Record(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Record))
        | TypeDefn.Union(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Union))
        | TypeDefn.Enum(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Enum))
        | TypeDefn.AbstractType(typeName = tn) -> claimOfHeader tn (declHeaderOf ctx tn ValueNone)
        | TypeDefn.Abbrev(typeName = tn; typ = rhs) -> claimOfHeader tn (declHeaderOf ctx tn (ValueSome rhs))
        | _ ->
            match TypeDefnPatterns.tryClassLikeDecl td with
            | ValueSome d -> ValueSome(struct (d.TypeName, TypeDeclKind.Class))
            | ValueNone -> ValueNone

    /// The simple name a `TypeName` declares. `ValueNone` for the dotted/empty shapes a
    /// registrar declines.
    let private tryDeclaredSimpleName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string voption =
        let (TypeName(ident = nameLi)) = tn

        if nameLi.Idents.Length = 1 then
            ValueSome(ctx.NameOf nameLi.Idents.[0])
        else
            ValueNone

    /// `[<Struct>]` on the declaration's header: the half of the value-type verdict a
    /// header alone decides, the `type X = struct … end` SHAPE being the other half.
    let isStructAttributed (ctx: PassContext) (tn: TypeName<SyntaxToken>) : bool =
        (AttributeDecode.decodeClassAttributes (ctx.ResolveAttributes(Attributes.attributesOfTypeName tn))).IsValueType

    /// Is this declaration a VALUE type: `[<Struct>]`, or the `type X = struct … end` shape?
    /// Kind-agnostic (a record, a union and a class can each be a struct): a struct stores its
    /// fields inline, so a struct field is an IMMEDIATE containment edge, uncyclable (FS0954).
    let isValueTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : bool =
        match tryDeclaredTypeName ctx td with
        | ValueSome(struct (tn, _)) -> isStructAttributed ctx tn || TypeDefnPatterns.isStructShape td
        | ValueNone -> false

    /// Pre-scan: note the RECORD / UNION / CLASS short names this element declares into
    /// `NominalTypeNames`. Swept over the WHOLE file first because a `module Foo` may textually
    /// precede the `type Foo` whose existence renames it to `FooModule`.
    let noteNominalTypeNames (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match tryDeclaredTypeName ctx td with
                | ValueSome(struct (tn, kind)) ->
                    match tryDeclaredSimpleName ctx tn with
                    | ValueSome name ->
                        match kind with
                        | TypeDeclKind.Record
                        | TypeDeclKind.Union
                        | TypeDeclKind.Class -> TypeRegistry.noteNominalTypeName ctx.Types name
                        | TypeDeclKind.Enum
                        | TypeDeclKind.Abbreviation
                        | TypeDeclKind.Measure
                        | TypeDeclKind.IntrinsicBinding -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
        | _ -> ()

    /// The placement every claim of one `type … and …` group shares. Visible from the enclosing
    /// `rec` scope's keyword, else the group's own first token; entering after the scope's
    /// prelude under `rec`, else at that same token.
    let typeGroupPlacement
        (recScopeOffset: int voption)
        (defs: ImmutableArray<TypeDefn<SyntaxToken>>)
        : ClaimPlacement =
        match recScopeOffset with
        | ValueSome offset ->
            {
                VisibleFrom = offset
                EntersAt = BindingRank.afterPrelude
            }
        | ValueNone ->
            let mutable found = ValueNone
            let mutable i = 0

            while found.IsNone && i < defs.Length do
                found <- CstKeys.tryFirstTokenOfTypeDefn defs.[i]
                i <- i + 1

            let own =
                match found with
                | ValueSome t -> t.StartIndex
                | ValueNone -> 0

            { VisibleFrom = own; EntersAt = own }

    /// The nominal identity ONE declaration claims, from the name it writes and the kind it
    /// writes it for. Every type in a `type … and …` group is claimed before any detail
    /// registers, so `and`-joined recursion needs no deferral.
    let claimTypeName
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (placement: ClaimPlacement)
        (tn: TypeName<SyntaxToken>)
        (kind: TypeDeclKind)
        : TypeIdentity voption =
        let (TypeName(ident = nameLi)) = tn

        // A dotted / empty declared name claims nothing, so it never enters the group's
        // working set and reaches no registrar.
        if nameLi.Idents.Length <> 1 then
            ValueNone
        else

            let declSite = NodeSite.ofToken NodeKind.DeclType nameLi.Idents.[0]
            let name = ctx.NameOf declSite.Tok

            // An enum is non-generic: it claims its name at arity 0 whatever typars were
            // (illegally) written on it.
            let typarList =
                match kind with
                | TypeDeclKind.Enum -> TyparList.empty
                | _ -> typarListOfTypeName ctx tn

            let arity = typarList.Length

            // The module chain that HOLDS the declaration is part of its claim, and the
            // container its key is minted from.
            let container = localContainerChain ctx c

            if TypeRegistry.isTypeClaimed ctx.Types container name arity then
                ctx.Report(declSite.Tok, Kind.Message(sprintf "Duplicate type definition: %s" name))

                // The first claimant keeps the name; this declaration registers nothing and
                // no `SymbolKey` is minted for it.
                ValueNone
            else
                // The external claim test must ask the provider, and the provider is
                // addressed BY the key, so the mint sits below the local duplicate test.
                // An externally-claimed name is diagnosed but still CLAIMED locally.
                let key = stampLocalTypeKey ctx declSite container name arity

                diagnoseExternalClaim ctx declSite.Tok key

                let identity =
                    {
                        Name = name
                        Typars = typarList
                        Container = container
                        Kind = kind
                        DeclSite = declSite
                        Key = key
                        VisibleFrom = placement.VisibleFrom
                        EntersAt = placement.EntersAt
                    }

                TypeRegistry.claimType ctx.Types identity

                // An intrinsic binding's identity is its qualified key: `type int = (# … #)`
                // under `namespace Vesper` keys as `Vesper.int`, `seq<'T>` as
                // `Vesper.Collections.seq` at arity 1, each equal to the contract's canon key.
                if kind = TypeDeclKind.IntrinsicBinding then
                    ctx.Types.IntrinsicKeys.[name] <- SymbolKeyOps.typeKeyOfArity c.Namespace name arity

                ValueSome identity

    let claimTypeIdentity
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (placement: ClaimPlacement)
        (td: TypeDefn<SyntaxToken>)
        : ClaimedTypeDefn voption =
        match tryDeclaredTypeName ctx td with
        | ValueNone -> ValueNone
        | ValueSome(tn, kind) ->
            match claimTypeName ctx c placement tn kind with
            | ValueSome identity -> ValueSome { Identity = identity; Defn = td }
            | ValueNone -> ValueNone

    /// Report a standalone `type T with …`, whose members reach no registry. The augmentation
    /// forms written on a definition (`type T = … with member …`) are silent here.
    let rejectDetachedTypeExtension (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.TypeExtension(typeName = TypeName(ident = nameLi) as tn) when nameLi.Idents.Length > 0 ->
            match CstKeys.tryFirstTokenOfTypeName tn with
            | ValueSome tok ->
                let name = (ctx.WrittenTypeNameOf nameLi).Written

                ctx.Report(
                    tok,
                    Kind.NotYetSupported(
                        sprintf
                            "a detached type augmentation ('type %s with …'); declare its members in the definition of '%s'"
                            name
                            name
                    )
                )
            | ValueNone -> ()
        | _ -> ()

    /// The three bodied class-like spellings, which differ only in what the BODY means: a
    /// `struct` is a value type, an `interface` is one whether or not its members say so, and
    /// a bare body is an interface exactly when every member it holds is abstract.
    [<RequireQualifiedAccess>]
    type SigClassForm =
        | Bodied
        | Struct
        | Interface

    /// WHAT one type signature declares: the kind it claims its name for, and the syntax each
    /// registrar below reads.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type SigDecl =
        | Record of
            typeName: TypeName<SyntaxToken> *
            fields: RecordFields<SyntaxToken> *
            extensions: TypeExtensionElementsSignature<SyntaxToken> voption
        | Union of
            typeName: TypeName<SyntaxToken> *
            cases: UnionTypeCases<SyntaxToken> *
            extensions: TypeExtensionElementsSignature<SyntaxToken> voption
        | Enum of typeName: TypeName<SyntaxToken> * cases: EnumTypeCases<SyntaxToken>
        /// A transparent alias: `type t = u` aliases `u` and declares nothing of its own.
        | Abbrev of
            typeName: TypeName<SyntaxToken> *
            rhs: Type<SyntaxToken> *
            extensions: TypeExtensionElementsSignature<SyntaxToken> voption
        /// `type t = (# "…" #)`: a primitive BINDING rather than an alias. A signature
        /// normally writes `extern` for this and leaves the repr to its implementation, but
        /// the inline-IL spelling parses here too and claims what it claims in a `.fs`.
        | IntrinsicAbbrev of
            typeName: TypeName<SyntaxToken> *
            kindTag: ExternKind<SyntaxToken> voption *
            instrParts: ImmutableArray<StringPart<SyntaxToken>> *
            extensions: TypeExtensionElementsSignature<SyntaxToken> voption
        /// `type t = extern`: the platform supplies the representation, and which spelling is
        /// the paired implementation's business.
        | Extern of
            typeName: TypeName<SyntaxToken> *
            kindTag: ExternKind<SyntaxToken> voption *
            members: TypeExtensionElementsSignature<SyntaxToken> voption
        | ClassLike of
            typeName: TypeName<SyntaxToken> *
            form: SigClassForm *
            elements: TypeElementsSignature<SyntaxToken>
        /// `type T`, with no body: opaque, and a reference to it needs the identity alone.
        | Opaque of typeName: TypeName<SyntaxToken>
        /// A `[<Measure>]` declaration; `rhs` is `ValueNone` for the body-less form.
        | Measure of typeName: TypeName<SyntaxToken> * rhs: Type<SyntaxToken> voption
        /// This compiler models no delegate, so it claims no type; its signature still writes
        /// type names that must resolve.
        | Delegate of typeName: TypeName<SyntaxToken> * signature: DelegateSig<SyntaxToken>
        /// An augmentation of a type declared elsewhere: it claims no name of its own.
        | TypeExtension of typeName: TypeName<SyntaxToken> * elements: TypeExtensionElementsSignature<SyntaxToken>

    /// A `type t [= rhs]` signature header. `ext` is the `with …` augmentation an
    /// abbreviation form may carry.
    let private sigDeclOfHeader
        (ctx: PassContext)
        (tn: TypeName<SyntaxToken>)
        (rhs: Type<SyntaxToken> voption)
        (ext: TypeExtensionElementsSignature<SyntaxToken> voption)
        : SigDecl =
        match declHeaderOf ctx tn rhs with
        | DeclHeader.IntrinsicBinding(tag, parts) -> SigDecl.IntrinsicAbbrev(tn, tag, parts, ext)
        | DeclHeader.Measure rhs -> SigDecl.Measure(tn, rhs)
        | DeclHeader.Abbreviation rhs -> SigDecl.Abbrev(tn, rhs, ext)
        | DeclHeader.Opaque -> SigDecl.Opaque tn

    let sigDeclOf (ctx: PassContext) (ts: TypeSignature<SyntaxToken>) : SigDecl =
        match ts with
        | TypeSignature.Record(typeName = tn; fields = fs; extensions = ext) -> SigDecl.Record(tn, fs, ext)
        | TypeSignature.Union(typeName = tn; cases = cs; extensions = ext) -> SigDecl.Union(tn, cs, ext)
        | TypeSignature.Enum(typeName = tn; cases = cs) -> SigDecl.Enum(tn, cs)
        | TypeSignature.AbstractType tn -> sigDeclOfHeader ctx tn ValueNone ValueNone
        | TypeSignature.Abbrev(typeName = tn; typ = rhs; extensions = ext) -> sigDeclOfHeader ctx tn (ValueSome rhs) ext
        | TypeSignature.Extern(typeName = tn; kindTag = tag; members = ms) -> SigDecl.Extern(tn, tag, ms)
        | TypeSignature.Anon(typeName = tn; elements = els)
        | TypeSignature.Class(typeName = tn; elements = els) -> SigDecl.ClassLike(tn, SigClassForm.Bodied, els)
        | TypeSignature.Struct(typeName = tn; elements = els) -> SigDecl.ClassLike(tn, SigClassForm.Struct, els)
        | TypeSignature.Interface(typeName = tn; elements = els) -> SigDecl.ClassLike(tn, SigClassForm.Interface, els)
        | TypeSignature.Delegate(typeName = tn; signature = s) -> SigDecl.Delegate(tn, s)
        | TypeSignature.TypeExtension(typeName = tn; elements = els) -> SigDecl.TypeExtension(tn, els)

    module SigDecl =

        let typeName (decl: SigDecl) : TypeName<SyntaxToken> =
            match decl with
            | SigDecl.Record(typeName = tn)
            | SigDecl.Union(typeName = tn)
            | SigDecl.Enum(typeName = tn)
            | SigDecl.Abbrev(typeName = tn)
            | SigDecl.IntrinsicAbbrev(typeName = tn)
            | SigDecl.Extern(typeName = tn)
            | SigDecl.ClassLike(typeName = tn)
            | SigDecl.Opaque tn
            | SigDecl.Measure(typeName = tn)
            | SigDecl.Delegate(typeName = tn)
            | SigDecl.TypeExtension(typeName = tn) -> tn

        /// Whether the declaration will publish as an interface, read off its written
        /// form: the `extern interface` and `interface … end` spellings, or an all-abstract
        /// bodied form.
        let isInterfaceForm (decl: SigDecl) : bool =
            match decl with
            | SigDecl.Extern(kindTag = ValueSome(ExternKind.Interface _)) -> true
            | SigDecl.ClassLike(form = SigClassForm.Interface) -> true
            | SigDecl.ClassLike(form = SigClassForm.Bodied; elements = elems) -> TypeDefnPatterns.bodyIsInterface elems
            | _ -> false

        /// The kind this declaration claims its name for; `ValueNone` where it claims none.
        let claimedKind (decl: SigDecl) : TypeDeclKind voption =
            match decl with
            | SigDecl.Record _ -> ValueSome TypeDeclKind.Record
            | SigDecl.Union _ -> ValueSome TypeDeclKind.Union
            | SigDecl.Enum _ -> ValueSome TypeDeclKind.Enum
            | SigDecl.Abbrev _ -> ValueSome TypeDeclKind.Abbreviation
            | SigDecl.IntrinsicAbbrev _ -> ValueSome TypeDeclKind.IntrinsicBinding
            // A CAPABILITY (`extern interface`) is a nominal interface that merely carries a
            // platform spelling, and a use site resolving it through the published shape kinds
            // it that way whether the target binds one or not. The other two `extern` forms
            // are primitives.
            | SigDecl.Extern(kindTag = ValueSome(ExternKind.Interface _)) -> ValueSome TypeDeclKind.Class
            | SigDecl.Extern _ -> ValueSome TypeDeclKind.IntrinsicBinding
            | SigDecl.ClassLike _
            | SigDecl.Opaque _ -> ValueSome TypeDeclKind.Class
            | SigDecl.Measure _ -> ValueSome TypeDeclKind.Measure
            | SigDecl.Delegate _
            | SigDecl.TypeExtension _ -> ValueNone

        /// The gap a claimless declaration publishes in place of a type, so a use site says
        /// which form is missing rather than "no such type". `ValueSome` exactly where
        /// `claimedKind` is `ValueNone`.
        let unmodelledReason (decl: SigDecl) : UnmodelledReason voption =
            match decl with
            | SigDecl.Delegate _ -> ValueSome UnmodelledReason.Delegate
            | SigDecl.TypeExtension _ -> ValueSome UnmodelledReason.TypeExtension
            | SigDecl.Record _
            | SigDecl.Union _
            | SigDecl.Enum _
            | SigDecl.Abbrev _
            | SigDecl.IntrinsicAbbrev _
            | SigDecl.Extern _
            | SigDecl.ClassLike _
            | SigDecl.Opaque _
            | SigDecl.Measure _ -> ValueNone

    let claimSigTypeIdentity
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (placement: ClaimPlacement)
        (decl: SigDecl)
        : TypeIdentity voption =
        match SigDecl.claimedKind decl with
        | ValueNone -> ValueNone
        | ValueSome kind -> claimTypeName ctx c placement (SigDecl.typeName decl) kind

    /// `typeGroupPlacement` for a signature group, taking the earlier of `kw` and the first
    /// declaration's attribute token: an attribute may reference a member of the group.
    let sigGroupPlacement (recScopeOffset: int voption) (kw: SyntaxToken) (decls: SigDecl list) : ClaimPlacement =
        match recScopeOffset with
        | ValueSome offset ->
            {
                VisibleFrom = offset
                EntersAt = BindingRank.afterPrelude
            }
        | ValueNone ->
            let own =
                match decls with
                | d :: _ ->
                    match CstKeys.tryFirstTokenOfTypeName (SigDecl.typeName d) with
                    | ValueSome t -> min kw.StartIndex t.StartIndex
                    | ValueNone -> kw.StartIndex
                | [] -> kw.StartIndex

            { VisibleFrom = own; EntersAt = own }

    /// The RECORD / UNION / CLASS-like short name one signature declaration writes, which is
    /// what a `module` of the same name is renamed by.
    let noteNominalSigTypeName (ctx: PassContext) (decl: SigDecl) : unit =
        match SigDecl.claimedKind decl with
        | ValueSome TypeDeclKind.Record
        | ValueSome TypeDeclKind.Union
        | ValueSome TypeDeclKind.Class ->
            match tryDeclaredSimpleName ctx (SigDecl.typeName decl) with
            | ValueSome name -> TypeRegistry.noteNominalTypeName ctx.Types name
            | ValueNone -> ()
        | ValueSome TypeDeclKind.Enum
        | ValueSome TypeDeclKind.Abbreviation
        | ValueSome TypeDeclKind.Measure
        | ValueSome TypeDeclKind.IntrinsicBinding
        | ValueNone -> ()

    /// Classify + stamp every type name ONE `.fsi` declaration's STRUCTURE writes: its header
    /// constraints, its fields or cases, its base, its interfaces and its `val`s. A member
    /// signature is NOT structure, and is classified with the member below.
    let classifyDeclaredSigTypes (ctx: PassContext) (decl: SigDecl) : unit =
        let it = NameResolutionScope.classifyingTypeIter ctx

        let extensions (ext: TypeExtensionElementsSignature<SyntaxToken> voption) =
            match ext with
            | ValueSome(TypeExtensionElementsSignature(elements = els)) ->
                CstTypeWalk.iterTypeElementsSignatureStructure it els
            | ValueNone -> ()

        CstTypeWalk.iterTypeNameConstraints it (SigDecl.typeName decl)

        match decl with
        | SigDecl.Record(fields = fields; extensions = ext) ->
            for RecordField(typ = t) in fields do
                CstTypeWalk.iterType it t

            extensions ext
        | SigDecl.Union(cases = cases; extensions = ext) ->
            for c in cases do
                CstTypeWalk.iterTypeUnionCase it c

            extensions ext
        | SigDecl.Abbrev(rhs = rhs; extensions = ext) ->
            CstTypeWalk.iterType it rhs
            extensions ext
        | SigDecl.Measure(rhs = rhs) -> rhs |> ValueOption.iter (CstTypeWalk.iterType it)
        // An `(# … #)` RHS is an IL string: it writes no type name.
        | SigDecl.IntrinsicAbbrev(extensions = ext) -> extensions ext
        | SigDecl.Extern(members = members) -> extensions members
        | SigDecl.ClassLike(elements = els) -> CstTypeWalk.iterTypeElementsSignatureStructure it els
        | SigDecl.TypeExtension(elements = TypeExtensionElementsSignature(elements = els)) ->
            CstTypeWalk.iterTypeElementsSignatureStructure it els
        | SigDecl.Delegate(signature = DelegateSig(sign = s)) -> CstTypeWalk.iterTypeUncurriedSig it s
        // An enum case is a literal and an opaque type has no body: neither writes a type.
        | SigDecl.Enum _
        | SigDecl.Opaque _ -> ()

    let classifyValSigTypes (ctx: PassContext) (vs: ValSig<SyntaxToken>) : unit =
        CstTypeWalk.iterValSigTypes (NameResolutionScope.classifyingTypeIter ctx) vs

    /// Classify + stamp what ONE member signature writes. Held apart from the declaring
    /// type's structure above: a member is published or dropped on its own.
    let classifyCurriedSigTypes (ctx: PassContext) (cs: CurriedSig<SyntaxToken>) : unit =
        CstTypeWalk.iterTypeCurriedSig (NameResolutionScope.classifyingTypeIter ctx) cs

    /// `classifyCurriedSigTypes` for a `new: … -> T` constructor signature.
    let classifyUncurriedSigTypes (ctx: PassContext) (sign: UncurriedSig<SyntaxToken>) : unit =
        CstTypeWalk.iterTypeUncurriedSig (NameResolutionScope.classifyingTypeIter ctx) sign

    /// Classify + stamp every type name in ONE type definition's declared surface, under the
    /// scope in force at its group. The `inherit` clause is stamped but NOT diagnosed here: it
    /// resolves against the referent's registered DETAIL, so its verdict waits for group close.
    let classifyDeclaredTypes (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        let it = NameResolutionScope.classifyingTypeIter ctx

        CstTypeWalk.iterTypeDefnTypes
            it
            (NameResolutionScope.stampPatCasesDeclaring ctx)
            (CstTypeWalk.iterType (stampTypeIter ctx))
            td

    /// Carries `it` over a module-level term's body. It resolves NO value and introduces NO
    /// scope because the hooks below exist solely to reach the annotations on the patterns they bind
    /// (`fun (x: A) …`, a nested `let`'s pats, a `for`-in bound variable, a match arm's type test).
    let private classifyingExprWalker (ctx: PassContext) (it: CstTypeWalk.TypeIter) : CstWalk.ExprWalker<unit> =
        let onType = CstTypeWalk.iterType it
        let onPat = NameResolutionScope.stampPatCasesDeclaring ctx

        let onPats (ps: ImmutableArray<Pat<SyntaxToken>>) =
            for p in ps do
                onPat p

        { CstWalk.identityExprWalker with
            Visit = fun _ e -> CstWalk.iterExprEmbeddedTypes it e
            EnterFun =
                fun env pats ->
                    onPats pats
                    env
            EnterBindingRhs =
                fun env _ _ b ->
                    onPats b.argumentPats
                    env
            EnterLetBody =
                fun env bindings ->
                    for b in bindings do
                        onPat b.pattern

                    env
            EnterForIn =
                fun env p ->
                    onPat p
                    env
            EnterMatchArm =
                fun env p ->
                    onPat p
                    env
        }

    /// Fold a module `let` binding marked `[<Literal>]` and record the constant in
    /// `ctx.Resolution.LiteralValues` under the binding-site key. Folding runs at the
    /// binding's own position, so an EARLIER literal it references resolves and a later one
    /// does not; an RHS outside the constant domain is diagnosed at its first token (FS0267).
    let private registerLiteralBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        if (ctx.ResolveAttributes b.attributes).Has RuntimeNames.literalAttributeKey then
            let useSite = ctx.UseSiteAt(CstKeys.ofBinding b)

            match
                ConstFold.tryConstant
                    ctx.NameOf
                    (fun t k -> ctx.Report(t, k))
                    (AttributeFold.tryNamedConstant ctx useSite)
                    b.expr
            with
            | Ok v ->
                for (_, key) in NameResolutionScope.bindingsOfPat ctx b.pattern do
                    ctx.Resolution.LiteralValues.Set(key, v.Value)
            | Error e -> ctx.Report(CstKeys.firstTokenOfExpr b.expr, ConstFold.rejectionKind e)

    /// Classify + stamp every type name a module-level TERM writes: a `let`'s parameter and
    /// return-type annotations, and every annotation reachable in its body. Runs at the term's
    /// own position in the scan, so the registry holds exactly the types declared ABOVE it.
    let classifyTermTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        let it = NameResolutionScope.classifyingTypeIter ctx
        let walker = classifyingExprWalker ctx it

        let binding (b: Binding<SyntaxToken>) =
            NameResolutionScope.stampPatCasesDeclaring ctx b.pattern

            for p in b.argumentPats do
                NameResolutionScope.stampPatCasesDeclaring ctx p

            match b.returnType with
            | ValueSome(ReturnType(typ = t)) -> CstTypeWalk.iterType it t
            | ValueNone -> ()

            CstWalk.iterExpr walker () b.expr

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                binding b
                registerLiteralBinding ctx b
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Do(expr = e))
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | _ -> ()

    /// Run `f` under a type declaration's typar scope, its prototype TyVars keyed by the
    /// source names its header declares, so a `'a` written in the declaration's structure
    /// resolves to the registry's TyVar, and an undeclared one is diagnosed, not minted.
    let underTyparScope (ctx: PassContext) (typeParams: EqArray<DeclaredTypar>) (f: unit -> 'a) : 'a =
        let scope = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

        for tp in typeParams do
            if not (scope.ContainsKey tp.Name) then
                scope.[tp.Name] <- tp.TyVar

        use _ = ctx.PushTyparScope(scope, true)
        f ()
