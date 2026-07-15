namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeHeadStamp
open UnificationTranslate

// The type-identity claim, plus registry stamping for record / union / enum /
// abbreviation type definitions.
//
// A type definition's declared STRUCTURE — field and case types, the abbreviation RHS,
// `val` fields, ctor-parameter and member-signature annotations — resolves HERE, against
// the types claimed above it plus its own `type … and …` group. Nothing below it is
// claimed yet, so a head naming a type declared below simply misses the registry and falls
// through to the external universe, exactly as F# resolves it. Member BODIES (and the
// member types they infer) stay in Unification.

module NameResolutionTypeRegistration =

    /// `[<CustomEquality>]` / `[<CustomComparison>]` on a record or union is out of
    /// scope: neither has an interface-impl side table to satisfy the
    /// `IEquatable<_>` / `IComparable<_>` requirement (union interface impls are
    /// unsupported front-to-back), so a `Custom` verdict on either is a diagnostic
    /// directing the user to a class. Shared by the record and union arms.
    let private rejectCustomOnDataType
        (ctx: PassContext)
        (declKey: NodeKey)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : unit =
        if eq = EqualityVerdict.Custom || cmp = ComparisonVerdict.Custom then
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message =
                        "[<CustomEquality>]/[<CustomComparison>] on a record or union is not supported in this compiler — wrap the type in a class that implements IEquatable<_>/IComparable<_>."
                    Code = "FS0378"
                    Severity = Severity.Error
                }

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

    /// The generic arity that keys this type in the registries (`0` for a
    /// non-generic name). The single source for the `(name, arity)` overload key —
    /// every arity-qualified `registerClass`/`tryClassArity`/duplicate-test site
    /// derives its arity through here rather than re-spelling the `List.length`.
    let arityOfTypeName (ctx: PassContext) (tn: TypeName<SyntaxToken>) : int =
        typarNamesOfTypeName ctx tn |> List.length

    /// The `when 'a : ...` clause on a `TypeName`, if any. Attached to the prototype TyVars
    /// by the declaration's own registrar (under its typar scope), and retained on the
    /// registry entry for the consumers that re-enter that scope later — a class's member
    /// bodies (`fillClassMembers`), an alias's `forceFill` — without re-walking the CST.
    let typarConstraintsOfTypeName (tn: TypeName<SyntaxToken>) : TyparConstraints<SyntaxToken> voption =
        let (TypeName(typarDefns = td)) = tn

        match td with
        | ValueSome(TyparDefns(constraints = ValueSome tc)) -> ValueSome tc
        | _ -> ValueNone

    /// Mint a prototype TyVar per declared typar name. Stored on the registry
    /// entry and substituted out at every use site, so two instantiations share
    /// no variables.
    let mkTypeParams (names: string list) : EqArray<string * TypeVar> =
        EqArray.ofSeq (
            seq {
                for n in names ->
                    let tv = TypeVar()
                    tv.Level <- 0
                    n, tv
            }
        )

    /// `ModuleRules.holderName` under this unit's `ModuleNaming` (`PassContext.ModuleNaming`).
    /// The rule itself lives in `ModuleRules` because the contract extractor — which is
    /// upstream of every pass and has no `PassContext` — is its third reader.
    let moduleHolderName (ctx: PassContext) (md: ModuleDefn<SyntaxToken>) : string =
        ModuleRules.holderName ctx.ModuleNaming md

    /// `ModuleRules.typeHolder` under this unit's `ModuleNaming`.
    let localTypeHolder (ctx: PassContext) (c: DeclContainment<SyntaxToken>) : TypeHolder =
        ModuleRules.typeHolder ctx.ModuleNaming c

    /// `ModuleRules.holderChain` under this unit's `ModuleNaming` — the holder a BINDING
    /// declared in `c` sits in. The SAME chain `localTypeHolder` reads, so a binding and a
    /// type declared in one module agree about which module holds them.
    let localHolderChain (ctx: PassContext) (c: DeclContainment<SyntaxToken>) : ModuleHolder =
        ModuleRules.holderChain ctx.ModuleNaming c

    /// The registered `ClassTypeInfo` of the class-like DECLARATION `tn` heads — recovered
    /// by the key the declaration mints in the module the walk stands in
    /// (`PassContext.DeclaredTypeKey`), never by its name.
    ///
    /// Shared by every pass that walks a class's own body (name resolution's scope walk,
    /// unification's member fill, its interface-impl resolution), so all three recover the
    /// same class. A by-name read could not: an arity-overloaded `Box\`1`/`Box\`2` does not
    /// resolve by bare name, and two sibling modules may each declare a `C`.
    let tryDeclaredClass (ctx: PassContext) (tn: TypeName<SyntaxToken>) : ClassTypeInfo voption =
        let (TypeName(ident = nameLi)) = tn

        if nameLi.Idents.Length = 1 then
            TypeRegistry.tryClassByKey
                ctx.Types
                (ctx.DeclaredTypeKey(ctx.NameOf nameLi.Idents.[0], arityOfTypeName ctx tn))
        else
            ValueNone

    /// The registered union / record / inline intrinsic-abbrev host the DECLARATION `tn`
    /// heads — the non-class sibling of `tryDeclaredClass`, and key-addressed for the same
    /// reason (`TypeRegistry.tryNonClassMemberHostByKey`).
    let tryDeclaredNonClassHost (ctx: PassContext) (tn: TypeName<SyntaxToken>) : IInterfaceImplHost voption =
        let (TypeName(ident = nameLi)) = tn
        let name = ctx.NameOf nameLi.Idents.[0]

        TypeRegistry.tryNonClassMemberHostByKey ctx.Types (ctx.DeclaredTypeKey(name, arityOfTypeName ctx tn)) name

    /// Mint a project-local `SymbolKey` for a type declaration and assert it is unique
    /// across the compilation. The key's holder is the declaring containment's chain,
    /// threaded from the module walk, so the key names exactly where the type was
    /// declared. THE sole mint site: called once per accepted claim from
    /// `claimTypeIdentity`, which hands the key to the per-kind registrar on the
    /// declaration's `TypeIdentity`.
    ///
    /// The collision branch is an INTERNAL-ERROR BACKSTOP, not a user diagnostic. The
    /// name-table claim is `(holder, name, arity)` and the key is minted from exactly those
    /// three, so a collision here means two declarations the claim test called DISTINCT
    /// collapsed onto one key — i.e. the mint dropped something the claim kept. That is a
    /// bug in this pass; a user duplicate is caught by the claim test and never reaches the
    /// stamp. `SymbolKeyOrigins` exists to witness it.
    let private stampLocalTypeKey
        (ctx: PassContext)
        (declKey: NodeKey)
        (holder: ModuleHolder)
        (name: string)
        (arity: int)
        : TypeKey =
        let key = LocalSymbolKey.ofType (ModuleRules.typeHolderOf holder) name arity

        match TypeRegistry.recordKeyOrigin ctx.Types declKey (SymbolKey.Type key) with
        | ValueSome _ ->
            ctx.Diagnostics.Add
                {
                    Key = declKey
                    Message =
                        sprintf
                            "Internal error: project-local SymbolKey collision for '%s' (arity %d)"
                            (SymbolKeyOps.typeMetaName key)
                            arity
                    Code = ""
                    Severity = Severity.Error
                }
        | ValueNone -> ()

        key

    /// The assembly whose already-declared type a resolved external shape WITNESSES —
    /// the `key -> assembly` oracle the collision test below reads. `None` means "this
    /// shape is not a competing claim", and the match is EXHAUSTIVE so a new
    /// `ExternalTypeShape` case must decide rather than silently default to it:
    ///
    ///   * `Intrinsic` / `IntrinsicInterface` — a primitive's identity IS its canon, and
    ///     every package's `int` is THE `int`. `claimTypeIdentity` mints a local
    ///     `IntrinsicRepr` key deliberately EQUAL to the contract's `intrinsicCanonKey`
    ///     (see its `IntrinsicKeys` arm), so agreement there is the design, not a clash —
    ///     the same exclusion `ReferencedProject.composeOrdered`'s cross-package sweep
    ///     makes, for the same reason.
    ///   * `Abbrev` / `Opaque` — carry no `SymbolOrigin` at all
    ///     (`ExternalSymbolProviders.stack`'s `stampType` homes only the shapes a backend
    ///     must emit a reference TO), so neither can be attributed to an assembly. Neither
    ///     contributes a nominal key the unifier could conflate either: an abbreviation is
    ///     TRANSPARENT (it expands to its body at the use site and names no type of its
    ///     own), and an `Opaque` refuses to become a `SemType` at all. If `Abbrev` ever
    ///     learns its origin, it becomes a competing claim here.
    let private externalClaimant (shape: ExternalTypeShape) : string option =
        // A stamped home is a claim (its assembly name); an unstamped home makes none —
        // matched off the `Origin` cases, never a nullable read of `.Assembly`.
        let homeName (o: SymbolOrigin) : string option =
            match o.Home with
            | Origin.InAssembly a -> Some a.Name
            | Origin.Unstamped -> None

        match shape with
        | ExternalTypeShape.Class info -> homeName info.Origin
        | ExternalTypeShape.Record(origin = o)
        | ExternalTypeShape.Union(origin = o)
        | ExternalTypeShape.Enum(origin = o) -> homeName o
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Opaque _ -> None

    /// THE premise, enforced — the CS0433 analogue. A `SymbolKey` is a nominal identity
    /// with NO home assembly in it, which is licensed by exactly one fact: within one
    /// compilation a fully-qualified name names at most one type. So a declaration whose
    /// minted key a REFERENCED assembly already answers for is refused here, at the
    /// declaration. Codegen would survive it (the local table is checked first, so local
    /// wins) but the UNIFIER would not: the two keys are equal, so it would happily unify
    /// two genuinely different types. Diagnosed, that state is unreachable.
    ///
    /// A unit's OWN contract is NOT a referenced assembly. Compiling `Vesper.List` against
    /// a provider stack that mounts `Vesper.List`'s own `.fsi` — which is precisely what
    /// `SymbolProviders.inlineBodies` does for every package's impl — the unit declares the
    /// very types its contract publishes. That is what compiling it MEANS. The shape's
    /// `SymbolOrigin` names its home assembly and `PassContext.AssemblyName` names the unit
    /// being compiled, so the two are distinguishable by construction: a shape homed HERE
    /// is the unit seeing itself.
    ///
    /// (This is the one front-end reader of `AssemblyName` that survives the assembly's
    /// removal from `SymbolKey` — it does not identify a *type*, it identifies the *unit*,
    /// which is what "own contract" is a statement about.)
    let private diagnoseExternalClaim (ctx: PassContext) (declKey: NodeKey) (key: TypeKey) : unit =
        match ctx.Provider.TryLookupType(SymbolKey.Type key) with
        | ValueNone -> ()
        | ValueSome shape ->
            match externalClaimant shape with
            | Some asm when asm <> ctx.AssemblyName ->
                ctx.Error(
                    declKey,
                    sprintf
                        "The type '%s' is declared by this project and already exists in the referenced assembly '%s'. A fully-qualified name names at most one type in a compilation — rename the type, or drop the reference to '%s'."
                        (SymbolKeyOps.typeMetaName key)
                        asm
                        asm
                )
            | _ -> ()

    /// The name a type declaration CLAIMS, and the kind it claims it for. The one
    /// enumeration of "what kinds of `TypeDefn` declare a type": exactly the shapes a
    /// per-kind registrar goes on to file (`TypeDefn.Interface`, `Delegate`,
    /// `TypeExtension`, `AbstractType` register nothing, so they claim nothing).
    let private tryDeclaredTypeName
        (td: TypeDefn<SyntaxToken>)
        : struct (TypeName<SyntaxToken> * TypeDeclKind) voption =
        match td with
        | TypeDefn.Record(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Record))
        | TypeDefn.Union(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Union))
        | TypeDefn.Enum(typeName = tn) -> ValueSome(struct (tn, TypeDeclKind.Enum))
        | TypeDefn.Abbrev(typeName = tn; typ = rhs) ->
            // An `(# … #)` RHS is a primitive BINDING, not a transparent alias: it lands in
            // `IntrinsicReprTypes`, not `Abbreviation`. The claim it holds on its declared
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

    /// Is this declaration a VALUE type — `[<Struct>]`, or the `type X = struct … end`
    /// shape? Kind-agnostic (a record, a union and a class can each be a struct), because
    /// the rule that reads it is: a struct STORES its fields inline, so a struct field is
    /// an IMMEDIATE containment edge and a cycle through one is unrepresentable (FS0954),
    /// whereas a reference-typed field is an indirection and cycles freely.
    let isValueTypeDefn (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : bool =
        match tryDeclaredTypeName td with
        | ValueSome(struct (tn, _)) ->
            (Attributes.decodeClassAttributes ctx (Attributes.attributesOfTypeName tn)).IsValueType
            || TypeDefnPatterns.isStructShape td
        | ValueNone -> false

    /// Pre-scan: note the RECORD / UNION / CLASS short names this element declares into
    /// `NominalTypeNames`. Sweeps the WHOLE unit before the registration scan, because its
    /// one reader needs the answer for a type the scan has not reached yet:
    /// `moduleHolderName`'s `…Module` suffix rule must give the same answer at key-mint
    /// time and at emit time, and a `module Foo` may textually precede the `type Foo` it
    /// collides with. Derived from `tryDeclaredTypeName`, so it cannot drift from the
    /// claims it shadows.
    ///
    /// It is the ONE scan that must precede registration. Nothing else may read the whole
    /// unit's type names ahead of the scan: doing so is how a type declared BELOW a
    /// reference becomes visible to it, which is exactly the scoping rule the top-down
    /// scan exists to enforce.
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

    /// WHERE the claims of one `type … and …` group become visible. `ModuleElem.Type` IS
    /// the group, so this is computed once per group and every claim minted from it carries
    /// the same offset (see `TypeIdentity.VisibleFrom`).
    ///
    /// Inside a `module rec` / `namespace rec` it is that scope's keyword: the declaration
    /// is visible from the top of the module, which is the whole of what `rec` means.
    /// Otherwise it is the group's own first token — so a use above the group cannot see
    /// it, and everything the group writes (a field type, a member body, an `and`-sibling's
    /// head) sits after it and can.
    ///
    /// `0` — visible from anywhere — when the group retains no token at all
    /// (`TypeDefn.Missing`): a shape that declares nothing has nothing to scope.
    ///
    /// The search carries a `voption`, not a `0` sentinel: offset 0 is a REAL position (a
    /// bare `type A = … and B = …` with no module header starts there), so a sentinel would
    /// read "found nothing" off the first group in such a file and fall through to the
    /// SECOND definition's offset — hiding the group from its own first member's body.
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

    /// Establish the nominal identity — name, arity, decl `NodeKey`, minted `SymbolKey`,
    /// and the offset it is visible from — of ONE type declaration, regardless of kind. Every type in a `type … and …` group is
    /// claimed before ANY of the group's detail registers, so:
    ///   * duplicate detection is ONE predicate over ONE name table (`TypeRegistry`'s
    ///     `TypeClaims`) — a kind added later cannot be wired into some guards and
    ///     forgotten in others, which is exactly how `enum` slipped past the abbreviation
    ///     and class registrars;
    ///   * the local `SymbolKey` has exactly ONE mint site (`stampLocalTypeKey`);
    ///   * the per-kind registrars stop owning duplicate detection — each is handed the
    ///     identity it registers under, so a rejected duplicate is never presented to one;
    ///   * a reference from one group member to another needs only the referent's key and
    ///     arity, and is therefore satisfied outright — which is what makes `and`-joined
    ///     mutual recursion work without deferral.
    /// `ValueNone` when the declaration claims nothing (a dotted/empty name, or a kind
    /// that declares no type).
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
            // group's working set, reaches no registrar. THE one place the single-ident
            // shape of a declared type name is tested.
            if nameLi.Idents.Length <> 1 then
                ValueNone
            else

                let nameTok = nameLi.Idents.[0]
                let name = ctx.NameOf nameTok
                let declKey = NodeKey.ofToken nameTok NodeKind.DeclType

                // An enum is non-generic: it claims its name at arity 0 whatever typars were
                // (illegally) written on it. THE one statement of the rule — the enum registrar
                // reads its arity off the claim.
                let arity =
                    match kind with
                    | TypeDeclKind.Enum -> 0
                    | _ -> arityOfTypeName ctx tn

                // The module chain that HOLDS the declaration — part of its claim, and the
                // holder its key is minted from. One chain, both uses, so the type the claim
                // says was declared here and the type the key names cannot come apart.
                let holder = localHolderChain ctx c

                if TypeRegistry.isTypeClaimed ctx.Types holder name arity then
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message = sprintf "Duplicate type definition: %s" name
                            Code = ""
                            Severity = Severity.Error
                        }

                    // The first claimant keeps the name and this declaration registers nothing —
                    // it is absent from the group's working set, so no registrar can reach it and
                    // no `SymbolKey` is minted for it.
                    TypeRegistry.rejectDuplicateType
                        ctx.Types
                        {
                            Name = name
                            TyparArity = arity
                            Kind = kind
                            DeclKey = declKey
                            Defn = td
                        }

                    ValueNone
                else
                    // ORDER. The LOCAL duplicate test above still runs FIRST and still gates
                    // the mint, so a rejected duplicate mints no key — the first claimant
                    // keeps the name, and a second declaration of it never reaches a
                    // registrar. The EXTERNAL claim test cannot run there: it must ask the
                    // provider, and the provider is addressed BY the key. So the mint sits
                    // between them.
                    //
                    // An externally-claimed name is diagnosed but still CLAIMED locally: the
                    // error already refuses the compilation, and keeping the claim keeps
                    // local-wins resolution intact, so every use of the type resolves to the
                    // one the source declared instead of cascading into either an "undefined
                    // type" storm or — worse — a silent bind to the external namesake this
                    // diagnostic exists to separate it from.
                    let key = stampLocalTypeKey ctx declKey holder name arity

                    diagnoseExternalClaim ctx declKey key

                    let claimed =
                        {
                            Identity =
                                {
                                    Name = name
                                    TyparArity = arity
                                    Holder = holder
                                    Kind = kind
                                    DeclKey = declKey
                                    Key = key
                                    VisibleFrom = visibleFrom
                                }
                            Defn = td
                        }

                    TypeRegistry.claimType ctx.Types claimed

                    // Contract-source an intrinsic binding's identity: mint its qualified key
                    // from the declaring namespace, so `Translate` resolves `int` to
                    // `Vesper.int` (and `seq<'T>` to `Vesper.Collections.seq` at arity 1) from
                    // the contract rather than re-deriving the namespace by name. Identity, so
                    // it is minted here; the target-representation string is detail and stays in
                    // the abbreviation registrar.
                    //
                    // The arity is part of the identity, exactly as for a record/union key — it
                    // is what makes this key EQUAL to the CONTRACT's
                    // `SymbolKeyOps.intrinsicCanonKey` (whose arity comes from PARSING the
                    // suffixed compiled name) when a unit compiles the very types its own
                    // contract publishes. Key equality is then the whole identity test.
                    //
                    // NAMESPACE-only (not the containment): an intrinsic is a primitive binding
                    // declared at namespace level (`namespace Vesper` + `type int = (# … #)`);
                    // a module-held one has no contract face to agree with.
                    if kind = TypeDeclKind.IntrinsicRepr then
                        ctx.Types.IntrinsicKeys.[name] <- SymbolKeyOps.typeKeyArity c.Namespace name arity

                    ValueSome claimed

    /// The `CstWalk.iterType` visitor for every type head written at a DECLARING position —
    /// a type definition's declared structure, and a module `let`'s annotations. Classify
    /// each written head (`classifyTypeHead` — a claim in scope wins, else the external
    /// universe), and diagnose a single-segment head that names NEITHER. F# reports the
    /// ordinary unknown-type error there (FS0039 "The type 'B' is not defined") — a forward
    /// reference across groups is not a special error class, it is simply a name nothing
    /// answers for, because `TypeClaims` holds only what is declared above the element the
    /// scan has reached (plus, inside a type group, the group itself).
    ///
    /// The diagnostic and the resolution are the SAME classification: `translateType` reads
    /// the stamp this walk wrote (external) or the registry (local), so it cannot bind a
    /// head this walk called unknown, nor bind differently from what this walk accepted.
    ///
    /// A DOTTED head names its type through a SCOPE (`A.T`), and is left to `translateType`
    /// (`resolveQualifiedTypeName`) — the reader that resolves a head against the scope its
    /// path names, so the miss is judged where the head is actually resolved. Both readers
    /// speak through `ctx.UndefinedType`, the one home for the verdict, so a head they BOTH
    /// reach is blamed once and in the same words.
    let private classifyingTypeIter (ctx: PassContext) : CstWalk.TypeIter =
        // `float<kg>` is a measured carrier, not a generic type applied to a type argument:
        // `translateType` reinterprets the WHOLE node — carrier and unit alike — as a
        // measured type and resolves neither through the type registry. Neither the carrier
        // head (which has no arity-1 shape to find) nor the unit is a type head, so
        // classification stops exactly where translation stops.
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
                        match CstKeys.ofTypeHead t with
                        | ValueSome head ->
                            match classifyTypeHead ctx head with
                            | UnknownType when head.LongIdent.Idents.Length = 1 ->
                                ctx.UndefinedType(head.Key, ctx.NameOf head.LongIdent.Idents.[0])
                            | UnknownType
                            | LocalType
                            | ExternalType -> ()
                        | ValueNone -> ()

                        true
        }

    /// Classify + stamp every type head written in ONE type definition's declared surface,
    /// under the scope in force at its group. Runs after the group's claim phase (so a
    /// sibling joined by `and` is in scope) and before its detail registers (so
    /// `translateType` reads a settled classification).
    ///
    /// The `inherit` clause is stamped but NOT diagnosed here: it is the one position
    /// resolved against the referent's registered DETAIL rather than its identity, so it is
    /// deferred to group close and raises its own unknown-type diagnostic there
    /// (`resolveInheritParent`). Diagnosing it here too would double-report one mistake.
    let classifyDeclaredTypes (ctx: PassContext) (td: TypeDefn<SyntaxToken>) : unit =
        let it = classifyingTypeIter ctx

        CstWalk.iterTypeDefnTypes
            it
            (NameResolutionScope.stampPatCasesWith ctx it)
            (CstWalk.iterType (stampTypeIter ctx))
            td

    /// The expression walker that carries `classifyingTypeIter` over a module-level term's
    /// body. It resolves NO value and introduces NO scope — `walker.Visit` reaches only the
    /// types syntactically embedded in each node, and the scope hooks exist solely to reach
    /// the annotations on the patterns they bind (`fun (x: A) …`, a nested `let`'s head and
    /// argument pats, a `for`-in binder, a match arm's type test). Value/ident resolution
    /// and body typing remain with the declaration-order body walk.
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
                        onPat b.headPat

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

    /// Classify + stamp every type head a module-level TERM writes — a `let`'s parameter and
    /// return-type annotations, and every annotation reachable in its body (`let x : A = …`,
    /// a type test, a coercion) — under the scope in force where the term is written.
    ///
    /// Types and module `let`s are ONE ordered sequence in F#, so this runs at the term's
    /// position in the top-down scan: the registry then holds exactly the types declared
    /// ABOVE it, an annotation naming a type declared below names nothing and is diagnosed,
    /// and a head written above a same-named local declaration classifies external and stays
    /// bound to the external type (`classifyTypeHead`'s stamp outranks the registry on read).
    let classifyTermTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) : unit =
        let it = classifyingTypeIter ctx
        let walker = classifyingExprWalker ctx it

        let binding (b: Binding<SyntaxToken>) =
            NameResolutionScope.stampPatCasesWith ctx it b.headPat

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

    /// Run `f` under the typar scope of a type declaration — its prototype TyVars, keyed by
    /// the source names its header declares — so a `'a` written anywhere in the
    /// declaration's structure resolves to the SAME TyVar the registry holds, and an
    /// undeclared one is diagnosed rather than silently minted (`TyparScopeStrict`).
    let underTyparScope (ctx: PassContext) (typeParams: EqArray<string * TypeVar>) (f: unit -> 'a) : 'a =
        let savedScope = ctx.Resolution.TyparScope
        let savedStrict = ctx.Resolution.TyparScopeStrict
        let scope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

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
            let declKey = id.DeclKey
            let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
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
                RecordTypeInfo(name, typeParams, fieldInfos, declKey, typarConstraints, id.Key)

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
                    declKey
                    (Attributes.attributesOfTypeName tn)

            // Explicit equality attribute wins; absent, the default
            // ⇒ Structural when every field is immutable,
            // Reference otherwise. Feeds Unification.checkConstraint and the
            // codegen triple gate (Elaborate copies it onto EqualitySupport).
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

            rejectCustomOnDataType ctx declKey info.EqualitySupport info.ComparisonSupport

            TypeRegistry.registerRecord ctx.Types info

            // Stamp the decl-site key so `Elaborate.tryRecordType` resolves this
            // record by its arity-qualified `SymbolKey` (via `tryRecordByKey`),
            // not the bare name — an arity-overloaded record (`Point`2`/`Point`3`)
            // does not resolve by bare name. Mirrors the union/enum decl-site stamp.
            ctx.Resolution.ResolvedType.Set(declKey, info.TypeKey)

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

    /// Map a union-case head to its case name. Delegates to the shared
    /// `OperatorNames.unionCaseCtorName` (the operator-named cases that matter are
    /// the cons-list ctors — `([])`→`Empty`, `(::)`→`Cons`) so the registered name
    /// can't drift from the contract extractor's. Heads we can't name (`(*)`,
    /// range/active-pattern ops) yield `""`, which `inspectCaseData` reads as "drop
    /// this case".
    let private unionCaseName (ctx: PassContext) (head: IdentOrOp<SyntaxToken>) : string =
        match OperatorNames.unionCaseCtorName ctx.NameOf head with
        | ValueSome n -> n
        | ValueNone -> ""

    /// One union case's registrable shape: its ctor name plus, positionally, each field's
    /// source name (`ValueNone` when unnamed) and written type. THE single decomposition of
    /// `UnionTypeCaseData` — names and types come out of one walk, so the registered
    /// `UnionCaseInfo.Fields` and `FieldNames` cannot fall out of index alignment. Handles
    /// plain forms, operator-named cases, and the explicit-return (GADT-syntax) forms
    /// FSharp.Core's list uses (return type treated as the declaring union; true GADTs
    /// remain out of scope).
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
        | UnionTypeCaseData.Nullary(name = head)
        | UnionTypeCaseData.GadtNullary(name = head) -> named (unionCaseName ctx head) [||] [||]
        | UnionTypeCaseData.Nary(name = head; fields = fields) ->
            named
                (unionCaseName ctx head)
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
        | UnionTypeCaseData.GadtNary(name = head; sign = UncurriedSig(args = ArgsSpec(args = specs))) ->
            named
                (unionCaseName ctx head)
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
            let declKey = id.DeclKey
            let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)
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

                            // The case is stamped with its union's own claim KEY, so
                            // "which union declares this case" never re-resolves a name
                            // (the arity-overloaded `Choice\`2`…`Choice\`7` are distinct
                            // keys, and the key says which).
                            caseInfos.Add(UnionCaseInfo(shape.Name, name, id.Key, fieldTys, shape.FieldNames, declKey))
                        | ValueNone -> ()
                )

            let caseInfos = caseInfos.ToArray()

            let info =
                UnionTypeInfo(name, typeParams, caseInfos, declKey, typarConstraints, id.Key)

            // Validate the equality / comparison attributes against the
            // union kind (FS0382 / FS0377) and read the resolved verdicts.
            let eqV, cmpV =
                Attributes.validateEqCompAttributes
                    ctx
                    Attributes.EqCompTargetKind.Union
                    declKey
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

            rejectCustomOnDataType ctx declKey info.EqualitySupport info.ComparisonSupport

            TypeRegistry.registerUnion ctx.Types info

            // Record the decl-site identity
            // so the type-decl emitter (`Elaborate.tryUnionType`) recovers the
            // union by key rather than re-deriving `(name, arity)`. `info.Key`
            // is the arity-qualified `TypeKey(None, declNs, name\`arity)`; this
            // stamp is co-populated with `ctx.Types.Union`, so the emitter's key
            // lookup is exactly as total as a `(name, arity)` one.
            ctx.Resolution.ResolvedType.Set(declKey, info.TypeKey)

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

    /// Register an enum's nominal identity + case-name set so a `(x: E)` annotation
    /// resolves to `TyEnum Key` (in `translateType`) and a qualified `E.C1` access
    /// can validate the case name. Enums are non-generic (arity 0) and have no
    /// member/augmentation side tables — the case→literal *values* are resolved
    /// later by `Elaborate.tryEnumType` (the only stage with the literal readers in
    /// compile order) and ride the surfaced `TTypeKind.Enum` node. The minted `Key`
    /// is stamped at the decl site into `ResolvedType`, mirroring `registerUnionTypeDefn`.
    let registerEnumTypeDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Enum(cases = cases) ->
            let name = id.Name
            let declKey = id.DeclKey
            let caseNames = [| for EnumTypeCase(ident = cid) in cases -> ctx.NameOf cid |]

            // The case VALUES, but ONLY when EVERY case is a string literal —
            // the literal-union admission (`subsumes`) runs before Elaborate
            // resolves the full case table, so read the string form here
            // through the SAME `StringLiterals.tryEnumCaseStringLiteral`
            // projection `Elaborate.resolveEnumCaseValue` uses (peels a
            // value-grouping paren, decodes escapes, admits verbatim/triple),
            // so a legal `| A = ("auto")` is not silently declined. A single
            // non-string case ⇒ `ValueNone` (the admission then declines and
            // the enum stays a plain nominal).
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

            let info = EnumTypeInfo(name, caseNames, caseStringValues, declKey, id.Key)
            TypeRegistry.registerEnum ctx.Types info

            // Record the decl-site identity so `Elaborate.tryEnumType`
            // recovers the SAME key the annotation path resolves to.
            ctx.Resolution.ResolvedType.Set(declKey, info.TypeKey)
        | _ -> ()

    /// Stitch the inline-IL string of a `Type.ILIntrinsic` RHS
    /// (`(# "System.Int32" #)` → `"System.Int32"`). Mirrors Elaborate.stitchLiteralString.
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

    /// An `(# … #)` RHS claims `IntrinsicRepr` and every other RHS claims `Abbreviation`
    /// (`tryDeclaredTypeName`); both are `TypeDefn.Abbrev` and both register here, the RHS
    /// deciding which side table they land in.
    ///
    /// An abbrev whose RHS is `Type.ILIntrinsic` is a *primitive binding*, not a
    /// transparent alias: recorded in IntrinsicReprTypes (name → IL string) and
    /// kept out of AbbreviationTypes, so translateType resolves the name to
    /// `TyConst name` rather than expanding the RHS.
    ///
    /// This registers the abbreviation ENTRY only — its RHS is not translated here. An RHS
    /// is the one FIELD-position reference that reads its referent's registered DETAIL
    /// (`forceFill` expands the body, so a sibling alias must already hold one), so it is
    /// forced at GROUP CLOSE, by which point every member of the group has registered. The
    /// entry itself must exist before ANY of the group's detail runs, because a record /
    /// union / class field naming the alias forces it on demand through
    /// `resolveClaimedType` — which is what makes `type R = { x: A } and A = int` (legal
    /// F#) work. `forceFill` is where an alias cycle (`type A = B and B = A` — only
    /// writable within one group, since nothing else can name a type below it) is
    /// diagnosed.
    let registerAbbreviationDefn (ctx: PassContext) (id: TypeIdentity) (td: TypeDefn<SyntaxToken>) : unit =
        match td with
        | TypeDefn.Abbrev(typeName = tn; typ = rhs; extensions = ext) ->
            let name = id.Name
            let declKey = id.DeclKey
            let key = id.Key
            let typeParams = mkTypeParams (typarNamesOfTypeName ctx tn)

            // An inline intrinsic-abbrev may carry a `with member …`
            // augmentation (`type X = (# … #) with member …`) — but ONLY an
            // ILIntrinsic RHS may. A transparent-alias abbrev with members
            // (`type bad = int with member …`) is rejected here (F# rejects it
            // too): the alias would have no distinct nominal identity to hang a
            // member on. Registered as a host in `IntrinsicAbbrevHost` so the
            // members name-resolve / type / elaborate on the shared host path,
            // WITHOUT withdrawing the type from `IntrinsicReprTypes` (its
            // `TyConst` identity is preserved at every other use site).
            let registerMemberHostIfAny () =
                match ext with
                | ValueNone -> ()
                | ValueSome _ ->
                    // The self-type key is the contract-sourced intrinsic identity
                    // (`IntrinsicKeys.[name]`, stamped by the identity pass), routed
                    // through the single `intrinsicKeyOf` resolver so `MkSelfType`
                    // cannot diverge from the abbrev's use-site key on a non-`Vesper`
                    // namespace.
                    let selfKey = TypeRegistry.intrinsicKeyOf ctx.Types name

                    ctx.Types.IntrinsicAbbrevHost.[name] <- IntrinsicAbbrevInfo(name, typeParams, declKey, key, selfKey)

            match rhs with
            | Type.ILIntrinsic(kindTag = tag; instrParts = parts) ->
                let repr = ilIntrinsicString ctx parts
                ctx.Types.IntrinsicReprTypes.[name] <- repr
                // The same binding on the KEY axis (`intrinsicKeyOf` — the identity the
                // pass above stamped), so a consumer holding a resolved intrinsic key reads
                // its repr by key and never has to project the key back to a name.
                ctx.Types.IntrinsicReprKeys.[TypeRegistry.intrinsicKeyOf ctx.Types name] <- repr
                registerMemberHostIfAny ()

                match tag with
                // Untagged `(# "…" #)` — an opaque value repr, never a base.
                | ValueNone -> ()
                // A `class`-tagged intrinsic (`(# class "…" #)`) is a HERITABLE
                // external reference base, not an opaque value repr: record the
                // name so `resolveInheritParent` admits it as a parent.
                | ValueSome(ExternKind.Class _) -> ctx.Types.HeritableExternBases.Add name |> ignore
                // `(# interface "…" #)` parses (the AST carries the species for a
                // future `extends`-less InterfaceImpl path) but has no emit path
                // yet: an interface goes in `implements`, not the `extends` column,
                // and has no base `.ctor` to chain to. Reject it here rather than
                // let it fall through and mis-emit as a class base. Not added to
                // `HeritableExternBases`, so it can never reach codegen's base path.
                | ValueSome(ExternKind.Interface _) ->
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message =
                                sprintf
                                    "Heritable external interface base ('(# interface \"…\" #)') is not yet supported (type '%s'); only '(# class \"…\" #)' may be inherited"
                                    name
                            Code = ""
                            Severity = Severity.Error
                        }
            | _ ->
                // Guardrail: a transparent-alias abbrev cannot carry members.
                // Reject with a diagnostic and drop the augmentation; the alias
                // itself still registers so ordinary references keep resolving.
                match ext with
                | ValueSome _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = declKey
                            Message =
                                sprintf
                                    "Type abbreviation '%s' cannot carry augmentation members: only an inline-IL abbreviation ('type %s = (# \"…\" #) with member …') may declare members"
                                    name
                                    name
                            Code = ""
                            Severity = Severity.Error
                        }
                | ValueNone -> ()

                let info =
                    AbbreviationInfo(name, typeParams, rhs, declKey, typarConstraintsOfTypeName tn, key)

                TypeRegistry.registerAbbrev ctx.Types info
        | _ -> ()
