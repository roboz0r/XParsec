namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRefStamp
open UnificationTranslate

// The type-identity claim and registry stamping for record / union / enum / abbreviation
// definitions. A definition's declared STRUCTURE resolves HERE, against the types claimed
// ABOVE it plus its own `type … and …` group; member BODIES are typed later, in Unification.

module NameResolutionTypeRegistration =

    /// `[<CustomEquality>]` / `[<CustomComparison>]` on a record or union is out of scope:
    /// neither has an interface-impl side table to satisfy the `IEquatable<_>` /
    /// `IComparable<_>` the verdict promises, so a `Custom` verdict is a diagnostic instead.
    let private rejectCustomOnDataType
        (ctx: PassContext)
        (declTok: SyntaxToken)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : unit =
        if eq = EqualityVerdict.Custom || cmp = ComparisonVerdict.Custom then
            ctx.Report(declTok, Kind.CustomEqualityOnRecordOrUnion)

    /// A `Typar`'s source-text name; the leading `'`/`^` lives on a separate
    /// token. Anon (`_`) typars don't participate in scope — ValueNone.
    let typarName (ctx: PassContext) (t: Typar<SyntaxToken>) : string voption =
        match t with
        | Typar.Named(ident = id)
        | Typar.Static(ident = id) -> ValueSome(ctx.NameOf id)
        | Typar.Anon _ -> ValueNone

    /// Declared typars for a `TypeName`, in source order: prefix typars (`'a Box`)
    /// first, then suffix (`Box<'a, 'b>`). Skips anonymous typars.
    let typarNamesOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string list =
        let (TypeName(prefixTypars = pt; typarDefns = td)) = tn

        let prefix =
            [
                match pt with
                | ValueNone -> ()
                | ValueSome(PrefixTypars.Single t) ->
                    match typarName ctx t with
                    | ValueSome n -> yield n
                    | ValueNone -> ()
                | ValueSome(PrefixTypars.Multiple(typars = ts)) ->
                    for t in ts do
                        match typarName ctx t with
                        | ValueSome n -> yield n
                        | ValueNone -> ()
            ]

        let main =
            [
                match td with
                | ValueNone -> ()
                | ValueSome(TyparDefns(defns = ds)) ->
                    for TyparDefn(typar = t) in ds do
                        match typarName ctx t with
                        | ValueSome n -> yield n
                        | ValueNone -> ()
            ]

        prefix @ main

    /// The generic arity that keys this type in the registries (`0` for a non-generic name).
    let arityOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : int =
        typarNamesOfTypeName ctx tn |> List.length

    /// The `when 'a : …` clause on a `TypeName`, if any. Retained on the registry entry so a
    /// consumer re-entering the declaration's typar scope later need not re-walk the CST.
    let typarConstraintsOfTypeName (tn: TypeName<SyntaxToken>) : TyparConstraints<SyntaxToken> voption =
        let (TypeName(typarDefns = td)) = tn

        match td with
        | ValueSome(TyparDefns(constraints = ValueSome tc)) -> ValueSome tc
        | _ -> ValueNone

    /// Mint a prototype TyVar per declared typar name. Stored on the registry
    /// entry and substituted out at every use site, so two instantiations share
    /// no variables.
    let mkTypeParams (store: TypeStore) (names: string list) : EqArray<string * TyVarId> =
        EqArray.ofSeq (
            seq {
                for n in names ->
                    let tv = store.NewTypeVar()
                    store.SetLevel(UnionFind.find store tv, 0)
                    n, tv
            }
        )

    /// The compiled module name of `md`, under this file's `ModuleNaming`.
    let compiledModuleName (ctx: PassContext) (md: ModuleDefn<SyntaxToken>) : string =
        ModuleRules.compiledModuleName ctx.ModuleNaming md

    /// The container a TYPE declared in `c` sits in, under this file's `ModuleNaming`.
    let localTypeContainer (ctx: PassContext) (c: DeclContainment<SyntaxToken>) : TypeContainer =
        ModuleRules.typeContainer ctx.ModuleNaming c

    /// The container a BINDING declared in `c` sits in — the same chain `localTypeContainer` reads.
    let localContainerChain (ctx: PassContext) (c: DeclContainment<SyntaxToken>) : ModuleContainer =
        ModuleRules.containerChain ctx.ModuleNaming c

    /// The registered `ClassTypeInfo` of the class-like DECLARATION `tn` declares — recovered by
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
    /// declares — the non-class sibling of `tryDeclaredClass`, key-addressed for the same reason.
    let tryDeclaredNonClassHost (ctx: PassContext) (tn: TypeName<SyntaxToken>) : IInterfaceImplHost voption =
        let (TypeName(ident = nameLi)) = tn
        let name = ctx.NameOf nameLi.Idents.[0]

        TypeRegistry.tryNonClassMemberHostByKey ctx.Types (ctx.DeclaredTypeKey(name, arityOfTypeName ctx tn)) name

    /// Mint the project-local `SymbolKey` for a type declaration, under the declaring
    /// containment's containment chain. The collision branch is an INTERNAL-ERROR BACKSTOP — a
    /// user duplicate is refused by the claim test upstream and never reaches the mint.
    let private stampLocalTypeKey
        (ctx: PassContext)
        (declSite: NodeSite)
        (container: ModuleContainer)
        (name: string)
        (arity: int)
        : TypeKey =
        let key = LocalSymbolKey.ofType (ModuleRules.typeContainerOf container) name arity

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

    /// The assembly whose already-declared type a resolved external shape WITNESSES — the
    /// `key -> assembly` oracle the collision test below reads. `None` = not a competing claim:
    /// every package's `int` is THE `int`, and `Abbrev` / `Unmodelled` carry no `SymbolOrigin`.
    let private externalClaimant (shape: ExternalTypeShape) : string option =
        // A stamped home is a claim (its assembly name); an unstamped home makes none. A
        // prior file of this very compilation claims under the compilation's own name.
        let homeName (o: SymbolOrigin) : string option =
            match o.Home.AssemblyOption with
            | ValueSome a -> Some a
            | ValueNone -> None

        match shape with
        | ExternalTypeShape.Class info -> homeName info.Origin
        | ExternalTypeShape.Record(origin = o)
        | ExternalTypeShape.Union(origin = o)
        | ExternalTypeShape.Enum(origin = o) -> homeName o
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Unmodelled _ -> None

    /// The CS0433 analogue: a `SymbolKey` carries no home assembly, so a declaration whose key a
    /// REFERENCED assembly already answers for is refused — equal keys would let the unifier
    /// unify two different types. A shape homed under `AssemblyName` is this file's own, waived.
    let private diagnoseExternalClaim (ctx: PassContext) (declTok: SyntaxToken) (key: TypeKey) : unit =
        match ctx.Provider.TryLookupType(SymbolKey.Type key) with
        | ValueNone -> ()
        | ValueSome shape ->
            match externalClaimant shape with
            | Some asm when asm <> ctx.AssemblyName ->
                ctx.Report(
                    declTok,
                    Kind.Message(
                        sprintf
                            "The type '%s' is declared by this project and already exists in the referenced assembly '%s'. A fully-qualified name names at most one type in a compilation — rename the type, or drop the reference to '%s'."
                            (SymbolKeyOps.typeMetaName key)
                            asm
                            asm
                    )
                )
            | _ -> ()

    /// The name a type declaration CLAIMS, and the kind it claims it for. `Interface`,
    /// `Delegate`, `TypeExtension` and `AbstractType` register nothing, so they claim nothing.
    let private tryDeclaredTypeName
        (td: TypeDefn<SyntaxToken>)
        : struct (TypeName<SyntaxToken> * TypeDeclKind) voption =
        match td with
        | TypeDefn.Record(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Record))
        | TypeDefn.Union(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Union))
        | TypeDefn.Enum(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Enum))
        | TypeDefn.Abbrev(typeName = tn; typ = rhs) ->
            // An `(# … #)` RHS is a primitive BINDING, not a transparent alias: it lands in
            // `IntrinsicReprKeys`, not `Abbreviation`. The claim it holds on its declared
            // name is identical either way.
            let kind =
                match rhs with
                | Type.ILIntrinsic _ -> TypeDeclKind.IntrinsicRepr
                | _ -> TypeDeclKind.Abbreviation

            ValueSome(struct (tn, kind))
        | _ ->
            match TypeDefnPatterns.tryClassLikeDecl td with
            | ValueSome d -> ValueSome(struct (d.TypeName, TypeDeclKind.Class))
            | ValueNone -> ValueNone

    /// The simple name a `TypeName` declares — `ValueNone` for the dotted/empty shapes a
    /// registrar declines.
    let private tryDeclaredSimpleName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string voption =
        let (TypeName(ident = nameLi)) = tn

        if nameLi.Idents.Length = 1 then
            ValueSome(ctx.NameOf nameLi.Idents.[0])
        else
            ValueNone

    /// Is this declaration a VALUE type — `[<Struct>]`, or the `type X = struct … end` shape?
    /// Kind-agnostic (a record, a union and a class can each be a struct): a struct stores its
    /// fields inline, so a struct field is an IMMEDIATE containment edge, uncyclable (FS0954).
    let isValueTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : bool =
        match tryDeclaredTypeName td with
        | ValueSome(struct (tn, _)) ->
            (Attributes.decodeClassAttributes ctx (Attributes.attributesOfTypeName tn)).IsValueType
            || TypeDefnPatterns.isStructShape td
        | ValueNone -> false

    /// Pre-scan: note the RECORD / UNION / CLASS short names this element declares into
    /// `NominalTypeNames`. Swept over the WHOLE file first because a `module Foo` may textually
    /// precede the `type Foo` whose existence renames it to `FooModule`.
    let noteNominalTypeNames (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match tryDeclaredTypeName td with
                | ValueSome(struct (tn, kind)) ->
                    match tryDeclaredSimpleName ctx tn with
                    | ValueSome name ->
                        match kind with
                        | TypeDeclKind.Record
                        | TypeDeclKind.Union
                        | TypeDeclKind.Class -> TypeRegistry.noteNominalTypeName ctx.Types name
                        | TypeDeclKind.Enum
                        | TypeDeclKind.Abbreviation
                        | TypeDeclKind.IntrinsicRepr -> ()
                    | ValueNone -> ()
                | ValueNone -> ()
        | _ -> ()

    /// The offset every claim of one `type … and …` group is visible from: inside a
    /// `module rec` / `namespace rec` that scope's keyword, else the group's own first token —
    /// so a use above the group cannot see it and everything the group writes can.
    let typeGroupVisibleFrom (recScopeOffset: int voption) (defs: ImmutableArray<TypeDefn<SyntaxToken>>) : int =
        match recScopeOffset with
        | ValueSome offset -> offset
        | ValueNone ->
            let mutable found = ValueNone
            let mutable i = 0

            while found.IsNone && i < defs.Length do
                found <- CstKeys.tryFirstTokenOfTypeDefn defs.[i]
                i <- i + 1

            match found with
            | ValueSome t -> t.StartIndex
            | ValueNone -> 0

    /// The nominal identity — name, arity, decl `NodeKey`, minted `SymbolKey`, visible-from
    /// offset — of ONE type declaration, whatever its kind. Every type in a `type … and …`
    /// group is claimed before any detail registers, so `and`-joined recursion needs no deferral.
    let claimTypeIdentity
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (visibleFrom: int)
        (td: TypeDefn<SyntaxToken>)
        : ClaimedTypeDefn voption =
        match tryDeclaredTypeName td with
        | ValueNone -> ValueNone
        | ValueSome(tn, kind) ->
            let (TypeName(ident = nameLi)) = tn

            // A dotted / empty declared name claims nothing — and so, being absent from the
            // group's working set, reaches no registrar.
            if nameLi.Idents.Length <> 1 then
                ValueNone
            else

                let declSite = NodeSite.ofToken NodeKind.DeclType nameLi.Idents.[0]
                let name = ctx.NameOf declSite.Tok

                // An enum is non-generic: it claims its name at arity 0 whatever typars were
                // (illegally) written on it.
                let arity =
                    match kind with
                    | TypeDeclKind.Enum -> 0
                    | _ -> arityOfTypeName ctx tn

                // The module chain that HOLDS the declaration — part of its claim, and the
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

                    let claimed =
                        {
                            Identity =
                                {
                                    Name = name
                                    TyparArity = arity
                                    Container = container
                                    Kind = kind
                                    DeclSite = declSite
                                    Key = key
                                    VisibleFrom = visibleFrom
                                }
                            Defn = td
                        }

                    TypeRegistry.claimType ctx.Types claimed.Identity

                    // An intrinsic binding's identity is its qualified key: `type int = (# … #)`
                    // under `namespace Vesper` keys as `Vesper.int`, `seq<'T>` as
                    // `Vesper.Collections.seq` at arity 1 — equal to the contract's canon key.
                    if kind = TypeDeclKind.IntrinsicRepr then
                        ctx.Types.IntrinsicKeys.[name] <- SymbolKeyOps.typeKeyArity c.Namespace name arity

                    ValueSome claimed

    /// The visitor for every type NAME written at a DECLARING position. Classify each name
    /// (a claim in scope wins, else the external universe) and diagnose a SINGLE-SEGMENT one
    /// that names neither (FS0039); a DOTTED name is judged where its path's scope is resolved.
    let private classifyingTypeIter (ctx: PassContext) : CstWalk.TypeIter =
        // `float<kg>` is a measured carrier, not a generic type applied to a type argument.
        // Neither the carrier (there is no arity-1 `float` to find) nor the measure is a type
        // reference, so classification stops here, exactly where translation stops.
        let isMeasuredCarrier (t: Type<SyntaxToken>) =
            match t with
            | Type.GenericType(longIdent = li; typeArgs = args) ->
                li.Idents.Length = 1
                && args.Length = 1
                && isNumericCarrier (ctx.NameOf li.Idents.[0])
            | _ -> false

        { CstWalk.identityTypeIter with
            VisitType =
                fun _ t ->
                    if isMeasuredCarrier t then
                        false
                    else
                        match CstKeys.ofTypeRef t with
                        | ValueSome typeRef ->
                            match classifyTypeRef ctx typeRef with
                            | TypeRefVerdict.UnknownType when typeRef.LongIdent.Idents.Length = 1 ->
                                ctx.UndefinedType(
                                    Site.ofTokenOr (Site.ofLongIdent typeRef.LongIdent) typeRef.Site.Tok,
                                    ctx.NameOf typeRef.Site.Tok
                                )
                            | TypeRefVerdict.UnknownType
                            | TypeRefVerdict.LocalType
                            | TypeRefVerdict.ExternalType _ -> ()
                        | ValueNone -> ()

                        true
        }

    /// Classify + stamp every type name in ONE type definition's declared surface, under the
    /// scope in force at its group. The `inherit` clause is stamped but NOT diagnosed here: it
    /// resolves against the referent's registered DETAIL, so its verdict waits for group close.
    let classifyDeclaredTypes (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        let it = classifyingTypeIter ctx

        CstWalk.iterTypeDefnTypes
            it
            (NameResolutionScope.stampPatCasesWith ctx it)
            (CstWalk.iterType (stampTypeIter ctx))
            td

    /// Carries `it` over a module-level term's body. It resolves NO value and introduces NO
    /// scope — the hooks below exist solely to reach the annotations on the patterns they bind
    /// (`fun (x: A) …`, a nested `let`'s pats, a `for`-in bound variable, a match arm's type test).
    let private classifyingExprWalker (ctx: PassContext) (it: CstWalk.TypeIter) : CstWalk.ExprWalker<unit> =
        let onType = CstWalk.iterType it
        let onPat = NameResolutionScope.stampPatCasesWith ctx it

        let onPats (ps: ImmutableArray<Pat<SyntaxToken>>) =
            for p in ps do
                onPat p

        { CstWalk.identityExprWalker with
            Visit = fun _ e -> CstWalk.iterExprEmbeddedTypes onType (CstWalk.iterTypeMemberSig it) e
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

    /// Classify + stamp every type name a module-level TERM writes — a `let`'s parameter and
    /// return-type annotations, and every annotation reachable in its body. Runs at the term's
    /// own position in the scan, so the registry holds exactly the types declared ABOVE it.
    let classifyTermTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        let it = classifyingTypeIter ctx
        let walker = classifyingExprWalker ctx it

        let binding (b: Binding<SyntaxToken>) =
            NameResolutionScope.stampPatCasesWith ctx it b.pattern

            for p in b.argumentPats do
                NameResolutionScope.stampPatCasesWith ctx it p

            match b.returnType with
            | ValueSome(ReturnType(typ = t)) -> CstWalk.iterType it t
            | ValueNone -> ()

            CstWalk.iterExpr walker () b.expr

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                binding b
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Do(expr = e))
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | _ -> ()

    /// Run `f` under a type declaration's typar scope — its prototype TyVars, keyed by the
    /// source names its header declares — so a `'a` written in the declaration's structure
    /// resolves to the registry's TyVar, and an undeclared one is diagnosed, not minted.
    let underTyparScope (ctx: PassContext) (typeParams: EqArray<string * TyVarId>) (f: unit -> 'a) : 'a =
        let savedScope = ctx.Resolution.TyparScope
        let savedStrict = ctx.Resolution.TyparScopeStrict
        let scope = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

        for (n, tv) in typeParams do
            if not (scope.ContainsKey n) then
                scope.[n] <- tv

        ctx.Resolution.TyparScope <- scope
        ctx.Resolution.TyparScopeStrict <- true

        try
            f ()
        finally
            ctx.Resolution.TyparScope <- savedScope
            ctx.Resolution.TyparScopeStrict <- savedStrict

    let registerRecordTypeDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Record(typeName = tn; fields = fields) ->
            let name = id.Name
            let declSite = id.DeclSite
            let typeParams = mkTypeParams ctx.Store (typarNamesOfTypeName ctx tn)
            let typarConstraints = typarConstraintsOfTypeName tn

            let fieldInfos = ResizeArray<RecordFieldInfo>(fields.Length)

            underTyparScope
                ctx
                typeParams
                (fun () ->
                    match typarConstraints with
                    | ValueSome cs -> translateConstraints ctx cs
                    | ValueNone -> ()

                    for f in fields do
                        let (RecordField(mutableToken = mt; ident = fid; typ = ft)) = f

                        fieldInfos.Add(
                            RecordFieldInfo(
                                ctx.NameOf fid,
                                translateType ctx ft,
                                mt.IsSome,
                                NodeKey.ofToken fid NodeKind.DeclType
                            )
                        )
                )

            let fieldInfos = fieldInfos.ToArray()

            let info =
                RecordTypeInfo(name, typeParams, fieldInfos, id.DeclSite, typarConstraints, id.Key)

            // `[<Struct>]` record ⇒ value type. The same struct predicate the
            // group struct-field cycle check reads, so registry and cycle check
            // agree.
            info.IsValueType <- isValueTypeDefn ctx td

            // Validate the equality / comparison attributes against the
            // record kind (FS0382 / FS0377) and read the resolved verdicts.
            let eqV, cmpV =
                Attributes.validateEqCompAttributes
                    ctx
                    Attributes.EqCompTargetKind.Record
                    declSite.Tok
                    (Attributes.attributesOfTypeName tn)

            // Explicit equality attribute wins; absent, the default is Structural when every
            // field is immutable and Reference otherwise.
            info.EqualitySupport <-
                match eqV with
                | ValueSome v -> v
                | ValueNone ->
                    if fieldInfos |> Array.forall (fun fi -> not fi.IsMutable) then
                        EqualityVerdict.Structural
                    else
                        EqualityVerdict.Reference

            // Comparison defaults to NoComparison, explicit attribute overrides.
            info.ComparisonSupport <-
                match cmpV with
                | ValueSome v -> v
                | ValueNone -> ComparisonVerdict.NoComparison

            // `[<RequireQualifiedAccess>]` keeps this record out of a cross-file
            // consumer's bare `{ X = … }` field-set index (`Elaborate` projects it
            // onto the frozen decl; `FrozenSignature` / `InferResolve` honour it).
            info.IsRequireQualifiedAccess <-
                AttributeDecode.decodeRequireQualifiedAccess ctx.NameOf (Attributes.attributesOfTypeName tn)

            rejectCustomOnDataType ctx declSite.Tok info.EqualitySupport info.ComparisonSupport

            TypeRegistry.registerRecord ctx.Types info

            // Stamp the decl-site key so the type-decl emitter recovers this record by its
            // arity-qualified `SymbolKey` rather than by the bare name.
            ctx.Resolution.ResolvedType.Set(declSite.Key, info.TypeKey)

            for fi in fieldInfos do
                match ctx.Types.FieldIndex.TryGetValue fi.Name with
                | true, infos ->
                    let buf = ResizeArray(infos.Length + 1)
                    buf.Add info

                    for i in infos do
                        buf.Add i

                    ctx.Types.FieldIndex.[fi.Name] <- EqArray.ofResizeArray buf
                | false, _ -> ctx.Types.FieldIndex.[fi.Name] <- EqArray.singleton info
        | _ -> ()

    /// A union case's ctor name: `([])` → `Empty`, `(::)` → `Cons`, an ordinary case its
    /// own text. A case with no name (`(*)`, range / active-pattern ops) yields `""`, which
    /// `inspectCaseData` reads as "drop this case".
    let private unionCaseName (ctx: PassContext) (ident: IdentOrOp<SyntaxToken>) : string =
        match OperatorNames.unionCaseCtorName ctx.NameOf ident with
        | ValueSome n -> n
        | ValueNone -> ""

    /// One union case's registrable shape: its ctor name plus, positionally, each field's source
    /// name (`ValueNone` when unnamed) and written type. GADT-SYNTAX cases (FSharp.Core's list)
    /// decompose here too, their return type read as the declaring union; true GADTs do not.
    [<NoEquality; NoComparison>]
    type private UnionCaseShape =
        {
            Name: string
            FieldNames: string voption[]
            FieldTypes: Type<SyntaxToken>[]
        }

    let private inspectCaseData (ctx: PassContext) (data: UnionTypeCaseData<SyntaxToken>) : UnionCaseShape voption =
        let named (name: string) (fieldNames: string voption[]) (fieldTypes: Type<SyntaxToken>[]) =
            if name.Length = 0 then
                ValueNone
            else
                ValueSome
                    {
                        Name = name
                        FieldNames = fieldNames
                        FieldTypes = fieldTypes
                    }

        match data with
        | UnionTypeCaseData.Nullary(name = ident)
        | UnionTypeCaseData.GadtNullary(name = ident) -> named (unionCaseName ctx ident) [||] [||]
        | UnionTypeCaseData.Nary(name = ident; fields = fields) ->
            named
                (unionCaseName ctx ident)
                [|
                    for f in fields ->
                        match f with
                        | UnionTypeField.Named(ident = id) -> ValueSome(ctx.NameOf id)
                        | UnionTypeField.Unnamed _ -> ValueNone
                |]
                [|
                    for f in fields ->
                        match f with
                        | UnionTypeField.Named(typ = t)
                        | UnionTypeField.Unnamed(typ = t) -> t
                |]
        | UnionTypeCaseData.GadtNary(name = ident; sign = UncurriedSig(args = ArgsSpec(args = specs))) ->
            named
                (unionCaseName ctx ident)
                [|
                    for ArgSpec(name = nm) in specs ->
                        match nm with
                        | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(ctx.NameOf id)
                        | ValueNone -> ValueNone
                |]
                [| for ArgSpec(typ = t) in specs -> t |]

    let registerUnionTypeDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Union(typeName = tn; cases = cases) ->
            let name = id.Name
            let declSite = id.DeclSite
            let typeParams = mkTypeParams ctx.Store (typarNamesOfTypeName ctx tn)
            let typarConstraints = typarConstraintsOfTypeName tn
            let caseInfos = ResizeArray<UnionCaseInfo>(cases.Length)

            underTyparScope
                ctx
                typeParams
                (fun () ->
                    match typarConstraints with
                    | ValueSome cs -> translateConstraints ctx cs
                    | ValueNone -> ()

                    for UnionTypeCase(data = data) in cases do
                        match inspectCaseData ctx data with
                        | ValueSome shape ->
                            let fieldTys = shape.FieldTypes |> Array.map (translateType ctx)

                            // The case carries its union's own claim KEY, so "which union
                            // declares this case" never re-resolves a name.
                            caseInfos.Add(
                                UnionCaseInfo(shape.Name, name, id.Key, fieldTys, shape.FieldNames, declSite.Key)
                            )
                        | ValueNone -> ()
                )

            let caseInfos = caseInfos.ToArray()

            let info =
                UnionTypeInfo(name, typeParams, caseInfos, id.DeclSite, typarConstraints, id.Key)

            // Validate the equality / comparison attributes against the
            // union kind (FS0382 / FS0377) and read the resolved verdicts.
            let eqV, cmpV =
                Attributes.validateEqCompAttributes
                    ctx
                    Attributes.EqCompTargetKind.Union
                    declSite.Tok
                    (Attributes.attributesOfTypeName tn)

            // Union equality defaults to Structural, explicit attribute overrides.
            info.EqualitySupport <-
                match eqV with
                | ValueSome v -> v
                | ValueNone -> EqualityVerdict.Structural

            // Comparison defaults to NoComparison, explicit attribute overrides.
            info.ComparisonSupport <-
                match cmpV with
                | ValueSome v -> v
                | ValueNone -> ComparisonVerdict.NoComparison

            // `[<RequireQualifiedAccess>]` keeps this union's cases out of a
            // cross-file consumer's bare case index (`Elaborate` projects it onto
            // the frozen decl; `FrozenSignature` honours it).
            info.IsRequireQualifiedAccess <-
                AttributeDecode.decodeRequireQualifiedAccess ctx.NameOf (Attributes.attributesOfTypeName tn)

            rejectCustomOnDataType ctx declSite.Tok info.EqualitySupport info.ComparisonSupport

            TypeRegistry.registerUnion ctx.Types info

            // Record the decl-site identity so the type-decl emitter recovers the union by its
            // arity-qualified key rather than re-deriving `(name, arity)`.
            ctx.Resolution.ResolvedType.Set(declSite.Key, info.TypeKey)

            for c in caseInfos do
                match ctx.Types.CtorIndex.TryGetValue c.Name with
                | true, infos ->
                    let buf = ResizeArray(infos.Length + 1)
                    buf.Add c

                    for i in infos do
                        buf.Add i

                    ctx.Types.CtorIndex.[c.Name] <- EqArray.ofResizeArray buf
                | false, _ -> ctx.Types.CtorIndex.[c.Name] <- EqArray.singleton c
        | _ -> ()

    /// Register an enum's nominal identity + case-name set, so a `(x: E)` annotation resolves
    /// to `TyEnum Key` and a qualified `E.C1` can validate the case name. Enums are non-generic
    /// and have no member side tables; the case→literal VALUES are resolved later, in Elaborate.
    let registerEnumTypeDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Enum(cases = cases) ->
            let name = id.Name
            let declSite = id.DeclSite
            let caseNames = [| for EnumTypeCase(ident = cid) in cases -> ctx.NameOf cid |]

            // The case VALUES, but ONLY when EVERY case is a string literal — the literal-union
            // admission runs before Elaborate resolves the full case table. The projection peels
            // a paren, so `| A = ("auto")` counts; one non-string case ⇒ `ValueNone`.
            let caseStringValues =
                let vals =
                    [|
                        for EnumTypeCase(constValue = v) in cases do
                            match StringLiterals.tryEnumCaseStringLiteral ctx v with
                            | ValueSome s -> yield s
                            | ValueNone -> ()
                    |]

                if vals.Length = cases.Length && cases.Length > 0 then
                    ValueSome vals
                else
                    ValueNone

            let info = EnumTypeInfo(name, caseNames, caseStringValues, declSite.Key, id.Key)
            TypeRegistry.registerEnum ctx.Types info

            // Record the decl-site identity so `Elaborate.tryEnumType`
            // recovers the SAME key the annotation path resolves to.
            ctx.Resolution.ResolvedType.Set(declSite.Key, info.TypeKey)
        | _ -> ()

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS:
    /// `(# "System.Int32" #)` → `"System.Int32"`.
    let private ilIntrinsicString (ctx: PassContext) (parts: ImmutableArray<StringPart<SyntaxToken>>) : string =
        let sb = System.Text.StringBuilder()
        // TODO: raise diagnostics for unsupported parts (Expr, InvalidText).
        for part in parts do
            match part with
            | StringPart.Text t
            | StringPart.EscapeSequence t
            | StringPart.FormatSpecifier t
            | StringPart.EscapePercent t
            | StringPart.VerbatimEscapeQuote t
            | StringPart.OrphanFormatSpecifier t
            | StringPart.InvalidText t -> sb.Append(ctx.NameOf t) |> ignore
            | StringPart.Expr _ -> ()

        sb.ToString()

    /// Register a `type X = …` abbreviation. An `(# … #)` RHS is a primitive BINDING, not a
    /// transparent alias: it lands in `IntrinsicReprKeys` as canon key → IL string, so the
    /// name resolves to `TyConst key`. Only the ENTRY registers; the RHS is forced at GROUP
    /// CLOSE.
    let registerAbbreviationDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Abbrev(typeName = tn; typ = rhs; extensions = ext) ->
            let name = id.Name
            let declSite = id.DeclSite
            let key = id.Key
            let typeParams = mkTypeParams ctx.Store (typarNamesOfTypeName ctx tn)

            // Only an `(# … #)` RHS may carry a `with member …` augmentation — a transparent
            // alias (`type bad = int with member …`) has no nominal identity to hang a member
            // on. Registered as a host without withdrawing the type from `IntrinsicReprKeys`.
            let registerMemberHostIfAny () =
                match ext with
                | ValueNone -> ()
                | ValueSome _ ->
                    // The self-type key is the contract-sourced intrinsic identity, read through
                    // `intrinsicKeyOf` so it agrees with the abbrev's use-site key even when the
                    // declaring namespace is not `Vesper`. It also ADDRESSES the host table, so
                    // the lookup key and the self-type key are the same one value.
                    let selfKey = TypeRegistry.intrinsicKeyOf ctx.Types name

                    ctx.Types.IntrinsicAbbrevHost.[selfKey] <-
                        IntrinsicAbbrevInfo(name, typeParams, id.DeclSite, key, selfKey)

            match rhs with
            | Type.ILIntrinsic(kindTag = tag; instrParts = parts) ->
                let repr = ilIntrinsicString ctx parts
                // Filed on the KEY axis alone, so a consumer holding a resolved intrinsic key
                // never has to project it back to a name. The `class` tag rides the same entry:
                // heritability is a property of this repr.
                ctx.Types.IntrinsicReprKeys.[TypeRegistry.intrinsicKeyOf ctx.Types name] <-
                    {
                        Platform = repr
                        Heritable =
                            match tag with
                            | ValueSome(ExternKind.Class _) -> true
                            | ValueSome(ExternKind.Interface _)
                            | ValueNone -> false
                    }

                registerMemberHostIfAny ()

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
            | _ ->
                // A transparent-alias abbrev cannot carry members: diagnose and drop the
                // augmentation, but still register the alias so references keep resolving.
                match ext with
                | ValueSome _ ->
                    ctx.Report(
                        id.DeclSite.Tok,
                        Kind.Message(
                            sprintf
                                "Type abbreviation '%s' cannot carry augmentation members: only an inline-IL abbreviation ('type %s = (# \"…\" #) with member …') may declare members"
                                name
                                name
                        )
                    )
                | ValueNone -> ()

                let info =
                    AbbreviationInfo(name, typeParams, rhs, id.DeclSite, typarConstraintsOfTypeName tn, key)

                TypeRegistry.registerAbbrev ctx.Types info
        | _ -> ()
