namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

[<Sealed>]
type KeyedTable<'K, 'V when 'K: equality>() =
    let dict = Dictionary<'K, 'V>(HashIdentity.Structural)

    member _.Count = dict.Count

    member _.TryGetValue(key: 'K) =
        match dict.TryGetValue(key) with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    member _.Set(key: 'K, value: 'V) = dict[key] <- value

    member _.Remove(key: 'K) = dict.Remove key |> ignore

    member _.ContainsKey(key: 'K) = dict.ContainsKey key

    /// A live view of the backing dictionary, not a copy: later `Set`s show through it.
    member _.AsDictionary() : IReadOnlyDictionary<'K, 'V> = dict :> _

type SideTable<'V> = KeyedTable<NodeKey, 'V>

type BoundVarTable<'V> = KeyedTable<BoundVarKey, 'V>

type LambdaTable<'V> = KeyedTable<LambdaKey, 'V>

[<AutoOpen>]
module SideTablePatterns =

    /// Conjoin with `&` to bind a stamp while the match still selects on shape:
    /// `Pat.Named _ & Stamped ctx.Resolution.ExternalUnionCaseStamp key uc -> …`.
    [<return: Struct>]
    let (|Stamped|_|) (table: SideTable<'V>) (key: NodeKey) (_scrutinee: 'a) : 'V voption = table.TryGetValue key

type PassContextBindings =
    {
        Binding: SideTable<ResolvedBinding>
        /// Keyed by the binding's pattern `NodeKey`. Present only for a single-name or
        /// operator name that generalises; destructuring patterns and lambda parameters get none.
        Scheme: SideTable<TypeScheme>
        TypeVar: SideTable<TyVarId>
        Escape: SideTable<EscapeState>
        /// Keyed as `Escape` but orthogonal to it: a frame-local closure held in an aggregate
        /// is `RequiresHeapRepr` here, `LocalStack` in `Escape`.
        Repr: SideTable<RegionRepr>
        /// Bindings inside a named `module Foo = …`: which compiled module name (`Foo`/`FooModule`,
        /// not the anonymous "Program" one) the emitted static method belongs to.
        ModuleMembers: Dictionary<BoundVarKey, ModuleBindingInfo>
        /// Keyed by the binding's pattern `NodeKey`, in SOURCE order, which is the method-typar order.
        DeclaredTypars: SideTable<(string * TyVarId) list>
        /// Top-level EXPORTED entities only, because a type MEMBER's accessibility rides on the
        /// member. Un-thresholded: each export filter applies its own.
        Accessibility: Dictionary<SymbolKey, Accessibility>
        /// The `[<Global>]` bindings: the value IS a target global, so no definition is emitted.
        GlobalValueKeys: HashSet<SymbolKey>
        BindingTyparArities: Dictionary<BoundVarKey, int>
    }

module PassContextBindings =
    let empty () : PassContextBindings =
        {
            Binding = SideTable<_>()
            Scheme = SideTable<_>()
            TypeVar = SideTable<_>()
            Escape = SideTable<_>()
            Repr = SideTable<_>()
            ModuleMembers = Dictionary<_, _>()
            DeclaredTypars = SideTable<_>()
            Accessibility = Dictionary<_, _>()
            GlobalValueKeys = HashSet<_>()
            BindingTyparArities = Dictionary<_, _>()
        }

/// One `let`-bound value / function of a local module. `VisibleFrom` is the offset from which
/// it answers for its name: its own, or the enclosing `rec` scope's keyword when there is one.
[<Struct>]
type LocalModuleMember =
    {
        BindingSite: NodeKey
        VisibleFrom: int
    }

/// What a written type name resolves to, as NameResolution's classifying walk found it. Recorded
/// at every type reference the walk visits, so ABSENT means unvisited, not "resolved to nothing".
[<Struct; RequireQualifiedAccess>]
type TypeRefVerdict =
    /// A type declared in this file is in scope at the use site; its identity comes from the
    /// type registry, not from here.
    | LocalType
    /// Resolved to this key, at the type-arg arity written at the use site.
    | ExternalType of key: TypeKey
    /// Nothing in scope at the use site, and no external type of that spelling.
    | UnknownType

type PassContextResolution =
    {
        /// The prefixes active at the module element being analysed. `open` is declaration-level,
        /// so this stays constant inside any one expression. Seeded to `AmbientOpenScope`.
        mutable OpenScope: OpenScope
        /// The stable prelude: the referenced contracts' `[<AutoOpen>]` modules. Held apart
        /// from `OpenScope`, which each walk overwrites per element.
        mutable AmbientOpenScope: OpenScope
        /// The chain enclosing the element being analysed, set in lockstep with `OpenScope`.
        mutable EnclosingContainer: ModuleContainer voption
        /// Per-signature type-parameter scope, restored on exit. Anonymous typars (`_`) never
        /// enter it, because they are fresh per occurrence.
        mutable TyparScope: Dictionary<string, TyVarId>
        /// Prototype TyVars for the NEXT binding's own `<'C, …>` typars: on a name match the
        /// binding reuses one, so a generic member's signature and body share typar roots.
        mutable BindingTyparSeed: Dictionary<string, TyVarId> voption
        /// The enclosing type's typars, live across a member body and its nested `let`s. A
        /// binding's fresh scope seeds these first, its own `<'a>` after, shadowing on clash.
        mutable EnclosingTypars: Dictionary<string, TyVarId> voption
        /// A `'a` not already in `TyparScope` is rejected rather than introduced implicitly.
        /// Set for the type-defn fill-in walk: a record / DU may use only its declared typars.
        mutable TyparScopeStrict: bool
        /// Keyed by a member-access node (`Expr.DotLookup`), for an `<externalType>.Member` or
        /// static `Type.Member` access. Absent ⇒ a project-local member access.
        ExternalAccess: SideTable<ResolvedExternalMember>
        /// Keyed by an external construction node (`new T(args)`, `T args`, `T<'a>(args)`):
        /// the chosen `.ctor`'s key, so a backend selects that exact overload by identity.
        ExternalCtor: SideTable<SymbolKey>
        /// Keyed by a project-local instance method-call or accessor node: the member inference
        /// resolved, whose `ArgSig` distinguishes `Show(int)` from `Show(string)`.
        /// Absent ⇒ nothing project-local resolved here.
        LocalMemberCall: SideTable<ResolvedLocalMember>
        /// Keyed by an external method call: the constant defaults of the trailing
        /// optional parameters the call OMITTED, in declaration order.
        ExternalOptionalFill: SideTable<TConstValue list>
        /// Keyed by the folded `x.M(…)` call whose object argument is a typar coerced to a
        /// project-local interface (`'T :> IFace`): that interface's key and type arguments.
        TyparInterfaceCall: SideTable<TypeKey * EqArray<SemType>>
        /// Keyed by an `Expr.Ident` / `Expr.LongIdentOrOp` at an external-value use-site.
        ExternalValue: SideTable<SymbolKey>
        /// Keyed by an external value/operator use-site. The whole symbol, not just its key,
        /// because instantiating it needs the polymorphic `Scheme` / `TyparArity` / `Constraints`.
        ExternalSymbolStamp: SideTable<ExternalSymbol>
        /// Keyed by an external union-case ctor, in pattern (`Some x`) or expression
        /// (`None`, `Option.Some`) position. Absent ⇒ a bound variable, a local ctor, an RQA case.
        ExternalUnionCaseStamp: SideTable<ExternalUnionCase>
        /// Keyed by an external enum-case access `E.C1`'s anchor: the enum's nominal key, minted
        /// at arity 0 and so equal to the key an `(x: E)` annotation mints, letting them unify.
        ExternalEnumCaseStamp: SideTable<TypeKey>
        /// Keyed by an expression splicing a cross-package `let inline` body (an operator, `x?f`,
        /// `arr.[i]`, `arr.Length`): the intrinsic's key, so the splice is by KEY.
        IntrinsicKey: SideTable<SymbolKey>
        TypeTestTargets: SideTable<SemType>
        /// Keyed by a `use` binding's pattern `NodeKey`: how the bound variable is disposed.
        UseDispose: SideTable<Disposal>
        /// Keyed by a `for x in src do …` node. Absent ⇒ the interface path (which range
        /// sources also take); present for a source with only a pattern-based `GetEnumerator()`.
        ForInShape: SideTable<ForInEnumerator>
        /// Keyed by a type-naming node: a type-declaration site, a union / enum type
        /// annotation, or an expression-position type name (a static prefix, a ctor).
        ResolvedType: SideTable<TypeKey>
        /// Keyed by a written type reference, anchored on `li.Idents.[0]`.
        TypeRefVerdicts: SideTable<TypeRefVerdict>
        /// A static-access qualifier's external type key: the PREFIX of a folded `Expr.LongIdent`
        /// (`System.Console` in `System.Console.Out`), or a generic `Expr.TypeApp` target.
        ExternalStaticQualifier: SideTable<SymbolKey>
        /// Keyed by a ≥2-segment `Expr.LongIdent` whose qualifier is an external UNION or
        /// RECORD: such a type bears no static fields, so an unresolved last segment is a real miss.
        ExternalUnionRecordQualifier: SideTable<SymbolKey>
        /// A local module's short name (`SetTree`) → its directly-declared `let` bindings.
        /// Whole-file, so a reader MUST honour `VisibleFrom`.
        LocalModules: Dictionary<string, Dictionary<string, LocalModuleMember>>
        /// A local TYPE's short name (`SetIterator`) → the short name of the module it is
        /// declared inside (`SetTree`). Absent for a type at namespace / file top level.
        TypeEnclosingModule: Dictionary<string, string>
    }

module PassContextResolution =
    let create (ambient: OpenScope) : PassContextResolution =
        {
            OpenScope = ambient
            AmbientOpenScope = ambient
            EnclosingContainer = ValueNone
            TyparScope = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)
            BindingTyparSeed = ValueNone
            EnclosingTypars = ValueNone
            TyparScopeStrict = false
            ExternalAccess = SideTable<_>()
            ExternalCtor = SideTable<_>()
            LocalMemberCall = SideTable<_>()
            TyparInterfaceCall = SideTable<_>()
            ExternalOptionalFill = SideTable<_>()
            ExternalValue = SideTable<_>()
            ExternalSymbolStamp = SideTable<_>()
            ExternalUnionCaseStamp = SideTable<_>()
            ExternalEnumCaseStamp = SideTable<_>()
            IntrinsicKey = SideTable<_>()
            TypeTestTargets = SideTable<_>()
            UseDispose = SideTable<_>()
            ForInShape = SideTable<_>()
            ResolvedType = SideTable<_>()
            TypeRefVerdicts = SideTable<_>()
            ExternalStaticQualifier = SideTable<_>()
            ExternalUnionRecordQualifier = SideTable<_>()
            LocalModules = Dictionary<_, _>()
            TypeEnclosingModule = Dictionary<_, _>()
        }

/// An `x?name` site whose result var (`Root`) may escape `dynamic` through context; for
/// example `d?foo + 1` pins it to `int`, which warns. `Node` is the `?` expression itself.
type DynamicEscapeSite = { Root: TyVarId; Node: NodeSite }

/// A bare-program list literal left FLEXIBLE: the container `TypeVar` a consumer may drive,
/// its element type, and its own token, kept because settling runs after the walk, when the
/// node is gone.
type ListLiteral =
    {
        Var: TyVarId
        Elem: SemType
        Tok: SyntaxToken
    }

/// The Vesper.Core inline ACCESS intrinsics: the index-signature read+write lowering
/// (`x.[k]`, `x.[k] <- v`). An index signature has no host type to hang an accessor member
/// on, so these have no member form. `ValueNone` = the name is not in scope.
type CoreAccessIntrinsics =
    {
        GetIndex: ExternalSymbol voption
        SetIndex: ExternalSymbol voption
    }

/// Single-threaded: the side tables, `Diagnostics` and `TypeVar` graph all mutate in place.
/// Parallelism is per FILE, so one context each; only the provider crosses threads.
[<Sealed>]
type PassContext(provider: IExternalSymbolProvider, source: OriginSource) =
    // One file re-asks the same queries many times, each walking every composite layer.
    // Shadows the ctor arg, so every member below sees the memoised view.
    let provider = ExternalSymbolProviders.memoize provider

    // The ambient prefixes sit LAST, so explicit `open`s the walk prepends win.
    let ambientOpenScope =
        { OpenScope.empty with
            Prefixes = provider.AmbientOpenPrefixes
        }

    // `IntrinsicReprKeys` holds ONLY this file's own intrinsic bindings
    // (`type int = (# "System.Int32" #)`); a referenced package's ride the provider.
    let types = PassContextTypes.empty ()

    let mutable synthBoundVars = 0

    /// The STORE view (`SymbolKey → payload`), what a pass speaks once identity is resolved.
    /// Narrowed on purpose: a pass holding only this cannot reach a spelling lookup.
    member _.Provider: IExternalSymbolStore = provider

    /// The RESOLVER view (`string → identity`), the only string-lookup handle a `PassContext`
    /// exposes. Read by NameResolution, which stamps each result for later passes to read.
    member _.Resolver: IExternalSymbolResolver = provider

    /// A field name → every external record declaring it. Not a spelling lookup: a bare
    /// `{ X = … }` does not name a record, so the field set IS the identity, pinned at inference.
    member _.TryRecordsWithField(fieldName: string) : EqArray<ExternalRecordCandidate> =
        provider.TryRecordsWithField fieldName

    /// The language-capability identities (enumerable, enumerator, disposable, equatable,
    /// comparable), resolved once from contract names, so no BCL identity is hardcoded.
    member val CapabilityIds = ExternalSymbols.resolveCapabilities provider with get

    /// Resolved ONCE against the ambient prelude scope: the names are opens-insensitive, so it
    /// hits what a per-node resolve would. `lazy`, so a file with no such access pays nothing.
    member val CoreAccess: Lazy<CoreAccessIntrinsics> =
        lazy
            (let one (name: string) =
                OpenScope.tryResolve ambientOpenScope provider.TryLookup name

             {
                 GetIndex = one "GetIndex"
                 SetIndex = one "SetIndex"
             }) with get

    member val Input = source.Input
    member val Lexed = source.Lexed
    member val Origin = source.File

    /// The simple name of the assembly this file emits into; `""` where nothing is emitted.
    /// NOT part of any `SymbolKey`: nominal identity is the containment chain alone.
    member val AssemblyName = "" with get, set
    // Fully qualified: a bare `Diagnostic` here would resolve to the parser's.
    member val Diagnostics = ResizeArray<XParsec.FSharp.SemanticAnalysis.Diagnostic>() with get
    member val Types = types with get

    /// The primitive-intrinsic identities (`int`/`string`/…), each resolved lazily and cached:
    /// this file's own intrinsic keys first, then the provider via ambient `open`.
    member val Intrinsics =
        IntrinsicSet(fun name -> IntrinsicResolve.tryResolveIntrinsicType provider types.IntrinsicKeys name) with get

    /// Per-file memo of nominal `SymbolKey` → canonical intrinsic key; without it the composite
    /// provider is round-tripped per node. A non-intrinsic key caches its own identity.
    member val IntrinsicCanonCache = Dictionary<SymbolKey, SymbolKey>() with get

    /// The intrinsic axis this file analyses under: its OWN `(# … #)` declarations shadowing
    /// the provider's, per canon, so a local `int` hides the provider's `int` and leaves its
    /// `float` alone. `lazy`: the first read must come AFTER name resolution filled
    /// `IntrinsicReprKeys`.
    member val IntrinsicTypeMap: Lazy<IntrinsicTypeMap> =
        lazy (IntrinsicTypeMap.shadow (IntrinsicTypeMap.ofReprKeys types.IntrinsicReprKeys) provider.IntrinsicTypeMap) with get

    member val Bindings = PassContextBindings.empty () with get
    member val Resolution = PassContextResolution.create ambientOpenScope with get
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Keyed by an `Expr.App`, present only for a printf call lowered inline: literal format,
    /// fully applied, a `StdOut`/`StdErr`/`StringResult` sink, every specifier classifiable.
    member val PrintfApp = SideTable<PrintfSpec.PrintfSink>() with get
    /// Keyed as `PrintfApp`; only for a fully-applied `%a`/`%t` call on a writer/builder sink.
    /// `sprintf` has no entry, because its residue is the returned string.
    member val PrintfCallbackScratch = SideTable<PrintfSpec.CallbackScratch>() with get
    /// Keyed by an `Expr.App`; only for a FULLY-UNAPPLIED lowerable printf partial
    /// (`printfn "%d"`), so never `%A`/`%O`, because an unapplied hole there is an unpinned typar.
    member val PrintfPartial = SideTable<PrintfSpec.PrintfSink>() with get
    /// A `let`-bound (or ascribed) format-string literal, keyed by its BINDING-SITE `NodeKey`,
    /// which is what a use-site `Ident` resolves to, so such an `Ident` lowers like a literal.
    member val PrintfFormatLiterals = SideTable<Expr<SyntaxToken>>() with get

    /// The underlying `Expr.String` when `argExpr` is an `Ident` / `LongIdent` bound to a
    /// format-string literal. `ValueNone` for a direct literal or a non-format binding.
    member this.TryRecoverFormatLiteral(argExpr: Expr<SyntaxToken>) : Expr<SyntaxToken> voption =
        match argExpr with
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
            match this.Bindings.Binding.TryGetValue(CstKeys.ofExpr argExpr) with
            | ValueSome rb -> this.PrintfFormatLiterals.TryGetValue rb.BindingSite
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// The nodes whose type a source annotation FIXED: `(e : T)`, an annotated `let` / return
    /// / parameter, `new T(…)`, `:> T` / `:? T` / `:?> T`.
    member val private declaredTypeSites = SideTable<unit>() with get

    /// The TyVars minted for a source ANONYMOUS typar (`_`). Query one as it appears in the
    /// UN-ZONKED type: zonking a resolved hole to its inferred fill erases the marker.
    member val private inferenceHoles = HashSet<TyVarId>() with get

    /// Mark `key`'s type as source-declared. A BARE `_` (`let x : _ = …`) asks to INFER and is
    /// skipped; any written structure (`Box<_>`, `int`, `'a`) marks declared. Idempotent.
    member this.MarkTypeDeclared(key: NodeKey, annTy: SemType) =
        let isBareHole =
            match annTy with
            | TyVar tv -> this.IsInferenceHole tv
            | _ -> false

        if not isBareHole then
            this.declaredTypeSites.Set(key, ())

    /// Whether `key`'s type was written in source. A `true` node may still carry `_` holes.
    member this.IsTypeDeclared(key: NodeKey) : bool = this.declaredTypeSites.ContainsKey key

    member this.MarkInferenceHole(tv: TyVarId) = this.inferenceHoles.Add tv |> ignore

    /// Whether `tv` is a `_`-wildcard hole. Ask about the TyVar as stored in the UN-ZONKED type.
    member this.IsInferenceHole(tv: TyVarId) : bool = this.inferenceHoles.Contains tv

    /// Whether `ty` carries any `_` hole: `Box<int>` false, `Box<_>` true even with `_` pinned
    /// to `int`, because the walk follows a var's `Link` for structure but STOPS at a hole.
    member this.HasInferenceHoleIn(ty: SemType) : bool =
        let seen = HashSet<TyVarId>()

        let rec walk t =
            match t with
            | TyVar tv when this.IsInferenceHole tv -> true
            | TyVar tv ->
                if not (seen.Add tv) then
                    false
                else
                    match this.Store.Link(UnionFind.find this.Store tv) with
                    | ValueSome inner -> walk inner
                    | ValueNone -> false
            | TyClass(_, args)
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyConst(_, args) -> args |> EqArray.exists walk
            | TyFun(a, b) -> walk a || walk b
            | TyTuple items -> items |> EqArray.exists walk
            | _ -> false

        walk ty

    /// A SOURCE-lambda argument's value-struct closure verdict, recorded when the lambda lands
    /// on a typar parameter whose `:> Fun<'T,'U>` bound fires. No entry ⇒ an ordinary closure.
    member val FunVerdicts = LambdaTable<FunVerdict>() with get
    /// A generalised binding's frozen typar bounds, minted with the body's method-axis indices
    /// so the bounds' typar leaves carry them.
    member val GenericFnSchemes = BoundVarTable<FrozenConstraint list>() with get
    /// How the SOURCE writes each bound variable this file introduces: the identifier and where.
    /// Recorded at the mint: once a body is copied elsewhere its tokens spell the CALL site.
    member val BoundVarNames = BoundVarTable<BoundVarIdent>() with get
    /// Keyed by an `Expr.LibraryOnlyStaticOptimization`: the resolved `when ^T : …` constraints,
    /// the outer array aligned with the node's `clauses`, the inner one clause's `and`-joined list.
    member val StaticOpt = SideTable<EqArray<EqArray<TStaticOptConstraint>>>() with get
    /// The metavar arena: mints `TypeVar` handles with dense per-file ids, owns their tables.
    member val Store: TypeStore = TypeStore() with get

    member this.NewTypeVar() : TyVarId = this.Store.NewTypeVar()

    /// Mint a bound-variable key for a synthesised node. It has no source position, so one
    /// construct may mint several, and stays unnamed, so a backend names it after its slot.
    member _.NewSynthBoundVar() : NodeKey =
        let k = NodeKey.ofSyntheticCounter synthBoundVars NodeKind.SynthElaborateBoundVar
        synthBoundVars <- synthBoundVars + 1
        k

    /// Current let-depth (Rémy's levels): push on entering a binding group's RHSes, pop after
    /// typing them. Generalisation quantifies the TyVars above the pre-push value.
    member val CurrentLevel = 0 with get, set

    /// Each `[…]` whose container type was left FLEXIBLE: a `Vesper.Collections.List` parameter
    /// drives it, otherwise it defaults to FSharp.Core's `list`.
    member val ListLiterals = ResizeArray<ListLiteral>() with get

    /// `x?name` sites, swept once inference has settled: a `Root` that zonks to a concrete
    /// non-`dynamic` type is an implicit escape and warns unless `DynamicEscapeSuppressed`.
    member val DynamicEscapes = ResizeArray<DynamicEscapeSite>() with get

    /// `?` node keys whose escape warning an ascription on the `?` itself (`(d?foo : int)`) suppresses.
    member val DynamicEscapeSuppressed = HashSet<NodeKey>() with get

    /// Type names this file's SOURCE wrote and nothing defined. The unifier's `TyUnknown` arm
    /// stays silent for these, because its message blames a missing package, not a spelling mistake.
    member val UndefinedTypeNames = HashSet<string>() with get

    /// Written type names already blamed, so one two passes both reach is blamed once.
    member val private undefinedTypeSites = HashSet<Site>() with get

    /// Which cons-list an unpinned `[]`/`::` defaults to: `false` keeps FSharp.Core's `list`,
    /// `true` the Vesper one, for a BCL-only self-host package that has no FSharp.Core.
    member val DefaultListIsVesper = false with get, set

    /// Per module-level `let inline` binding, keyed by its function-bound-variable `NodeKey` and
    /// positionally aligned to its curried parameters. Only non-default parameters register.
    member val InlineParamAttrs = Dictionary<NodeKey, EqArray<ParamAttrs>>() with get

    /// The UNEXPANDED body of each module-level `let inline`. An `^T`-constrained body resolves
    /// its trait calls against the CALL SITE, so expanding here bakes in the generic fallback.
    member val InlineTemplates = Dictionary<NodeKey, TDecl>() with get

    /// The facts the module-naming rules read. `IsNominalTypeName` is a closure read at CALL
    /// time, so it sees a type declared textually BELOW the module it collides with.
    member _.ModuleNaming: ModuleNaming =
        {
            Lexed = source.Lexed
            IsNominalTypeName = TypeRegistry.isNominalTypeName types
        }

    /// Enter a module containment: the chain a by-name read from inside speaks from, set and
    /// returned. Every scope on the way in is noted under the SOURCE path an `open` names it by.
    member this.EnterContainment(c: DeclContainment<SyntaxToken>) : ModuleContainer =
        let scopes = ModuleRules.enclosingContainers this.ModuleNaming c

        for (path, container) in scopes do
            TypeRegistry.noteLocalContainer types path container

        let chain = scopes |> List.last |> snd
        this.Resolution.EnclosingContainer <- ValueSome chain
        chain

    /// Enter a walked module element, advancing both ambient facts a by-name read speaks
    /// against: the `open`s in scope and the module chain.
    member this.EnterElement(w: WalkedElem<SyntaxToken>) : unit =
        this.Resolution.OpenScope <- w.Scope
        this.EnterContainment w.Containment |> ignore

    /// Where a by-NAME registry read from `key` speaks from: the node's place in the file, the
    /// module chain, the `open`s. `UseSite.unbounded` is the whole-file view instead.
    member this.UseSiteAt(key: NodeKey) : UseSite =
        {
            Pos = SourcePos.ofNodeKey key
            Container = this.Resolution.EnclosingContainer
            Opens = this.Resolution.OpenScope.Locals
        }

    /// The module chain the walk stands in, which is what HOLDS a declaration written here.
    /// Before the walk enters anything, the global namespace: a declaration in an anonymous module.
    member this.CurrentContainer: ModuleContainer =
        match this.Resolution.EnclosingContainer with
        | ValueSome h -> h
        | ValueNone -> ModuleContainer.InNamespace NamespaceKey.Global

    /// The `TypeKey` a type DECLARED where the walk stands would be minted with. A pass must
    /// not find the declaration it is walking by NAME: two sibling modules may each declare `T`.
    member this.DeclaredTypeKey(name: string, arity: int) : TypeKey =
        LocalSymbolKey.ofType (ModuleRules.typeContainerOf this.CurrentContainer) name arity

    /// Source text of `token`, a backtick-escaped identifier reading as the name it spells.
    /// Empty for virtual (synthesised) tokens.
    member this.NameOf(token: SyntaxToken) : string =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenName(iT)
        | TokenIndex.Virtual -> ""

    /// Record how the source writes `boundVar`. Idempotent, and must be: a ctor parameter's key
    /// is minted twice from the same identifier. An operator's `(` is no name and records none.
    member this.SetBoundVarName(boundVar: BoundVarKey, at: SyntaxToken) : unit =
        match at.Index with
        | TokenIndex.Virtual -> ()
        | TokenIndex.Regular i ->
            match this.Lexed.GetIdentifier(i) with
            | "" -> ()
            | name -> this.BoundVarNames.Set(boundVar, { Text = name; At = Anchor.ofToken at })

    /// The LAST segment is the type's short name, everything before it the dotted SOURCE path
    /// of the qualifying scope, which is empty for a single-segment name.
    member this.WrittenTypeNameOf(li: LongIdent<SyntaxToken>) : WrittenTypeName =
        let idents = li.Idents
        let last = idents.Length - 1

        {
            Path = String.concat "." (seq { for i in 0 .. last - 1 -> this.NameOf idents.[i] })
            Name = this.NameOf idents.[last]
        }

    /// Allocation-free `NameOf`: a view of `token`'s source text, no substring copied out.
    member this.ReadableOf(token: SyntaxToken) : ReadableString =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenReadable(iT)
        | TokenIndex.Virtual -> ReadableString.Empty

    /// Report `kind` at `tok`. There is no per-severity member: the kind decides the severity.
    member this.Report(tok: SyntaxToken, kind: Kind) = this.Report(Site.ofToken tok, kind)

    /// Report `kind` at a `Site` the producer resolved itself, either `Site.Nowhere` or a span.
    member this.Report(site: Site, kind: Kind) =
        this.Diagnostics.Add(Diagnostic.create kind site [])

    /// Blame the written type name at `site`: `name` does not resolve to a type, here or
    /// outside. `site` is what once-per-name counts over, so a parser-inserted name widens
    /// it to the decl's span.
    member this.UndefinedType(site: Site, name: string) =
        this.UndefinedTypeNames.Add name |> ignore

        if this.undefinedTypeSites.Add site then
            this.Report(site, Kind.UndefinedType name)

    /// Register a `[…]` expression or `h :: t` pattern to be settled after the walk.
    member this.RegisterListLiteral(container: TyVarId, elem: SemType, tok: SyntaxToken) =
        this.ListLiterals.Add
            {
                Var = container
                Elem = elem
                Tok = tok
            }
