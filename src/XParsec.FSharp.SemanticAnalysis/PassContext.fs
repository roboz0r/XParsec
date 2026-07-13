namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The per-file side tables (all in-flight semantic information — the CST is
// never mutated) and the PassContext that carries them through the passes.

[<Sealed>]
type SideTable<'V>() =
    let dict = Dictionary<NodeKey, 'V>(HashIdentity.Structural)

    member _.Count = dict.Count

    member _.TryGetValue(key: NodeKey) =
        match dict.TryGetValue(key) with
        | true, v -> ValueSome v
        | false, _ -> ValueNone

    member _.Set(key: NodeKey, value: 'V) = dict[key] <- value

    member _.Remove(key: NodeKey) = dict.Remove key |> ignore

    member _.ContainsKey(key: NodeKey) = dict.ContainsKey key

    /// Callers must treat the returned dictionary as read-only once Elaborate starts.
    member _.AsDictionary() : IReadOnlyDictionary<NodeKey, 'V> = dict :> _

[<AutoOpen>]
module SideTablePatterns =

    /// Bind a stamp for `key` in ONE table read while the enclosing match selects
    /// on the node's syntactic shape — conjoin with `&`:
    /// `Pat.Named _ & Stamped ctx.Resolution.ExternalUnionCaseStamp key uc -> …`.
    /// The scrutinee itself is ignored (the syntactic half of the conjunction
    /// already matched it); this exists to replace the guard-then-re-lookup idiom
    /// (`when table.ContainsKey key` + a body `.Value` / re-read), whose recovery
    /// arms were unreachable by construction yet read as live paths.
    [<return: Struct>]
    let (|Stamped|_|) (table: SideTable<'V>) (key: NodeKey) (_scrutinee: 'a) : 'V voption = table.TryGetValue key

/// Type-definition side tables: the project-wide registry of records, unions,
/// classes, and abbreviations plus their reverse / member indexes. Populated by
/// `NameResolution.registerXxx`, filled in by `Unification`, read everywhere
/// downstream.

type PassContextBindings =
    {
        Binding: SideTable<ResolvedBinding>
        /// Keyed by the binding's headPat NodeKey (which is also the `BindingSite`
        /// NameResolution records). Present only for `let`-bound names that pass
        /// `shouldGeneralise` — module-level, nested, and `let rec` single-name
        /// bindings. Compound destructuring heads and lambda parameters do NOT get
        /// schemes.
        Scheme: SideTable<TypeScheme>
        TypeVar: SideTable<TypeVar>
        Escape: SideTable<EscapeState>
        /// Axis-2 representation verdict per region (a `RegionRepr`), keyed by the
        /// same binder / anon `NodeKey` as `Escape`. Populated by `Regions.run`
        /// from the second (representation) fixpoint; folded with `Escape` into the
        /// per-closure `ClosureRepr` verdict. Orthogonal to `Escape` (lifetime): a
        /// frame-local closure held in an aggregate is `LocalStack` here yet
        /// `RequiresHeapRepr` there.
        Repr: SideTable<RegionRepr>
        /// Module-level bindings inside a named `module Foo = …`: each
        /// binding's `NodeKey` → where its emitted static method belongs (a real
        /// `Foo`/`FooModule` holder type, not the anonymous "Program" holder).
        /// Populated by `Elaborate` and snapshotted into `TastFile.ModuleMembers`; the
        /// backend keys off it to name + place a module function (`ListModule::fold`).
        ModuleMembers: Dictionary<NodeKey, ModuleMemberInfo>
        /// A *top-level* (implicit-"Program"-module, `holder = None`) binding's
        /// `NodeKey` → its source name. Top-level bindings record no
        /// `ModuleMemberInfo`, so this is the only name source for a top-level value
        /// lowered to a Program-holder static field. Consulted only by the value
        /// collector, so top-level functions keep their `fn$<off>` holderless path.
        TopLevelNames: Dictionary<NodeKey, string>
        /// A `let` binding's explicitly-declared `<'b,'a>` typars, in SOURCE order,
        /// each paired with the `TypeVar` inference seeded for it. Captured by
        /// `Infer.inferBinding` while the binding's transient `TyparScope` is live
        /// (it's restored per binding, so it's gone by Elaborate). Keyed by the
        /// binding's headPat NodeKey. Elaborate's free-function method-typar minter
        /// reads this to order method typars declared-first (the F# rule); absent
        /// when the binding declared no typars.
        DeclaredTypars: SideTable<(string * TypeVar) list>
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
            TopLevelNames = Dictionary<_, _>()
            DeclaredTypars = SideTable<_>()
        }

/// The name-resolution stamp tables and the scope state the passes thread through
/// them.
///
/// **The resolve-once contract.** NameResolution is the single layer that turns a
/// written spelling into an identity: it resolves each one exactly once, opens-aware
/// (the `OpenScope.tryResolve` / `tryQualify` reach onto the resolver face), and
/// stamps the resulting identity into whichever table below names that node class.
/// Consumer passes (Unification, Elaborate) READ those stamps by node key and never
/// re-resolve a spelling — holding only the key-addressed `ctx.Provider` store face,
/// they cannot. A few tables instead carry a *type-directed* verdict reachable only
/// once the node is typed, and so are written by Unification
/// (`ExternalOptionalFill`, `TyparInterfaceCall`, `IntrinsicKey`,
/// `TypeTestTargets`, `UseDispose`, `ForInShape`); each
/// field names its writer. Every table is append-only and keyed by a CST `NodeKey`,
/// and because a `NodeKey` carries its `NodeKind`, expression / pattern / type
/// stamps at one source offset never collide.
///
/// **The other lifecycle.** `OpenScope`, `AmbientOpenScope`, `TyparScope`,
/// `BindingTyparSeed`, `EnclosingTypars` and `TyparScopeStrict` are NOT stamps but
/// mutable scope state: set, pushed and restored as the walk enters and leaves an
/// element, a binding, or a signature. `LocalModules` / `TypeEnclosingModule` are a
/// third kind again — short-name-keyed registries built by a NameResolution pre-pass
/// over the un-flattened module tree.
type PassContextResolution =
    {
        /// The `open` / auto-open namespace prefixes active at the module element
        /// currently being analysed: set per top-level element by the pass walk
        /// (`CstWalk.walkModuleTree`), read by the probe sites (`tryQualify`) so a
        /// short name resolves against the opens in scope. Constant inside any one
        /// expression (`open` is a declaration-level node). Seeded to
        /// `AmbientOpenScope`, so a pass that reads it before the walk sets a
        /// per-element scope still sees the auto-opens.
        mutable OpenScope: OpenScope
        /// The *stable* ambient prelude each pass seeds its `walkModuleTree` from:
        /// the provider's `AmbientOpenPrefixes` (the referenced-contract
        /// `[<AutoOpen>]` modules / prelude), empty when the provider surfaces none.
        /// Held apart from `OpenScope` because NameResolution and Unification must
        /// seed from the *same* prelude, which the per-element field overwrites.
        mutable AmbientOpenScope: OpenScope
        /// Per-signature type-parameter scope: each signature opens its own scope
        /// and restores the prior one on exit. Anonymous typars (`_`) never enter
        /// the scope — they're fresh per occurrence.
        mutable TyparScope: Dictionary<string, TypeVar>
        /// Prototype TyVars (keyed by source name) for the *next* binding's own
        /// `<'C, …>` typars: `inferBinding` mints a fresh scope for a binding's
        /// declared typars, but when this seed is set it reuses the prototype TyVar
        /// for a matching name instead of allocating a fresh one. `fillTypeMembers`
        /// sets it from a generic member's `TypeMemberInfo.MethodTypeParams`, so the
        /// typars flowing into the inferred signature are the same roots `Elaborate`
        /// surfaces and codegen installs as the ambient `!!i` set. `ValueNone` ⇒ the
        /// binding gets fresh typars.
        mutable BindingTyparSeed: Dictionary<string, TypeVar> voption
        /// The enclosing type's type-parameter scope (class / union typars), kept in
        /// scope across a member-body walk and its nested `let`s. `inferBinding` mints
        /// a *fresh* scope per binding (so sibling bindings' `'a`s stay distinct),
        /// which would otherwise drop the class typars `fillTypeMembers` put in scope:
        /// a generic member's *signature* annotation (`(x: 'T)`, `: Set<'T>`) would
        /// find an empty scope and — under `TyparScopeStrict` — diagnose "Free type
        /// parameter 'T". When set, `inferBinding` seeds its fresh scope with these
        /// typars first (the binding's own `<'a>` typars seed after, shadowing on a
        /// name clash). Set by `fillTypeMembers` / `fillSecondaryCtors`; `ValueNone`
        /// for a non-member binding.
        mutable EnclosingTypars: Dictionary<string, TypeVar> voption
        /// When true, `translateType` rejects any `'a` not already present in
        /// `TyparScope` rather than introducing it implicitly. Used by the type-defn
        /// fill-in walk: implicit free typars in a record / DU declaration aren't
        /// legal F# (only `<'a>`-declared typars are). Binding-level scopes keep
        /// this `false`.
        mutable TyparScopeStrict: bool
        /// Keyed by a member-access node (`Expr.DotLookup`): the resolved external
        /// member (`TryLookupMember` hit) for an `<externalType>.Member` or static
        /// `Type.Member` access. Elaborate mints a `TExpr.ExternalMember` stamping the
        /// resolved `SymbolKey`. Absent ⇒ project-local member access (resolved via
        /// `Types.Class` / `Types.Union`).
        ExternalAccess: SideTable<ResolvedExternalMember>
        /// Keyed by an external *method-call head* (the same key `ExternalAccess`
        /// stores the resolved member under): the compile-time constant defaults of
        /// the trailing optional parameters this call *omitted*, in declaration order.
        /// Written by `Unification`'s optional-argument fill
        /// (`InferExternalCall.tryFillOptionalCall`) when a call supplies fewer
        /// arguments than the member's parameter count, from the member's
        /// `ExternalMember.OptionalDefaults`; `Elaborate.translateApp` synthesises them
        /// as literal arguments so codegen sees the full tupled call
        /// (`ArrayPool<'T>.Return(arr)` ⇒ `Return(arr, false)`). Absent ⇒ a fully
        /// applied call (the common case), emitted unchanged.
        ExternalOptionalFill: SideTable<TConstValue list>
        /// Keyed by the folded `LongIdent` / `DotLookup` head of `x.M(...)`: the
        /// constraining *interface*'s `SymbolKey.TypeKey` when the receiver's type is
        /// a generic typar coerced to a project-local interface (`'T :> IFace`).
        /// Written by `Unification.resolveFieldStep`'s typar arm when it resolves the
        /// member through the typar's `Coercion` constraint; `Elaborate` mints a
        /// `TExpr.MethodCall` with `CallVia.Interface` (the declaring type is the
        /// interface; codegen emits `constrained. <typar> callvirt`). The paired
        /// `SemType list` is the interface's instantiation type arguments (`'E` in
        /// `'T :> IStructSeq<'E>`), taken from the `Coercion` constraint's target so
        /// Elaborate can thread them onto `CallVia.Interface` and codegen mint the slot
        /// on the *instantiated* interface `TypeSpec`; empty for a non-generic
        /// interface. Absent ⇒ an ordinary nominal-receiver member access.
        TyparInterfaceCall: SideTable<TypeKey * EqArray<SemType>>
        /// Keyed by an external-value use-site (the `Expr.Ident` / `Expr.LongIdentOrOp`
        /// that resolved through `IExternalSymbolProvider.TryLookup`): the resolved
        /// value's `SymbolKey.ValueKey`. Elaborate stamps it onto `TExpr.External` so
        /// codegen can do robust identity checks (e.g. "is this exactly
        /// `Vesper.Printf.printfn`?") instead of suffix-matching the source-written
        /// name.
        ExternalValue: SideTable<SymbolKey>
        /// Keyed by an external value/operator use-site: the full `ExternalSymbol` the
        /// spelling resolves to. Minting sites (all in NameResolution's walk): a value
        /// ref (`Expr.Ident` / multi-segment `Expr.LongIdentOrOp`), a `(+)`-as-value /
        /// `A.B.(+)` operator value, and the desugared/dynamic operators (`InfixApp` /
        /// `PrefixApp` reading `ctx.Desugared`, `op_Dynamic` on a `DynamicLookup`,
        /// `op_DynamicAssignment` on the enclosing dynamic `Assignment`). Read by
        /// `InferIdentExpr`'s value / `(+)`-value arms and `InferApp`'s operator sites,
        /// which call `ExternalSymbols.instantiateSymbol` on it. The whole symbol is
        /// stamped, not just its `SymbolKey` (`ExternalValue` / `IntrinsicKey` carry
        /// that for Elaborate): instantiation needs the polymorphic `Scheme` /
        /// `TyparArity` / `Constraints`, and this table is written where the SPELLING is
        /// resolved — the one place that owns `string × OpenScope → symbol`. Caching the
        /// resolved symbol there is what keeps every later pass off the resolver face; it
        /// is not a claim that no key-addressed form exists (`TryLookupByKey` is one).
        /// The value/operator companion to `ExternalUnionCaseStamp` (cases).
        /// Absent ⇒ the spelling is not an external symbol; the consumer falls to its
        /// ctor / static / operator-value / error path.
        ExternalSymbolStamp: SideTable<ExternalSymbol>
        /// Keyed by an external union-case ctor head — a *pattern* head
        /// (`CstKeys.ofPat`: the `Some x` / `Result.Ok x` of a `match` / binder) or an
        /// *expression* head (`CstKeys.ofExpr`: a bare `None` / qualified `Option.Some`
        /// used as a value / ctor function): the `ExternalUnionCase` it resolves to.
        /// NameResolution owns case recognition — it applies the opens / RQA / qualifier
        /// discipline (`ExternalUnionCase.ResolvesWith`) here; Unification's `InferPat` /
        /// `InferIdentExpr` and Elaborate's `translatePat` / `tryCtorRef` read the stamp.
        /// Absent ⇒ the head is not an external union case (a binder, a local ctor, or a
        /// bare reference to an `[<RequireQualifiedAccess>]` case, which resolves only
        /// qualified). A missed stamp where a consumer reads is a phantom binder /
        /// mis-lowering, so the pattern-stamping walk must reach every pattern position.
        ///
        /// The payload is stamped, not a `(union key, case name)` pair: a consumer needs
        /// the declaring union's key AND the matched case's per-field type builders to
        /// instantiate `TyUnion(union, freshArgs)` and unify sub-patterns. Recovering
        /// the field types from the union key alone would take a key-addressed
        /// `TryLookupType(union key)` → select-case-by-name — but a provider publishing
        /// only the reverse case index (several test fakes, any minimal contract)
        /// answers `TryLookupUnionCase` yet returns `ValueNone` for that forward lookup,
        /// so the round-trip would change behaviour.
        ExternalUnionCaseStamp: SideTable<ExternalUnionCase>
        /// Keyed by an external enum-case access `E.C1`'s head — an *expression* head
        /// (`CstKeys.ofExpr`: `E.C1` used as a value) or a *pattern* head
        /// (`CstKeys.ofPat`: `| E.C1` in a `match`): the enum's nominal `SymbolKey`. `E`
        /// qualifies opens-aware to an external `ExternalTypeShape.Enum` declaring `C1`
        /// (arity-0 — enums are never generic); the key matches the type-annotation mint
        /// for an `(x: E)` annotation, so the access/pattern and the annotation unify.
        /// Unification's `InferIdentExpr` / `InferPat` enum arms read the stamp and type
        /// the node `TyEnum key`. Elaborate needs no stamp: it reads the enum key back off
        /// the node's `TyEnum` type (`Elaborate/Resolve.enumKeyOfTy`). Absent ⇒ the head is
        /// not an external enum case (a project-local enum, handled by the sibling
        /// `ctx.Types.Enum` arm, or an unrelated qualified name). The enum-case sibling
        /// of `ExternalUnionCaseStamp`, but a bare `SymbolKey` suffices rather than a
        /// payload: an enum case is a named constant on a closed set, not a ctor arrow.
        ExternalEnumCaseStamp: SideTable<SymbolKey>
        /// Keyed by an expression Elaborate lowers to a desugared
        /// `TExpr.External(<intrinsicName>, …)` head that splices a cross-package
        /// `let inline` body — an arithmetic/comparison/custom operator
        /// (`InfixApp`/`PrefixApp`), a dynamic-access operator (`op_Dynamic` on a
        /// `DynamicLookup`, `op_DynamicAssignment` on the enclosing `Assignment`), or a
        /// synthesised element/index/length intrinsic (`GetArray`/`GetString`/`GetIndex`
        /// on an `IndexedLookup`, `SetArray`/`SetIndex` on the enclosing `Assignment`,
        /// `GetArrayLength` on the `.Length` `DotLookup` / `LongIdent` chain): the
        /// intrinsic's `SymbolKey`. Unification resolves the intrinsic's
        /// `ExternalSymbol` while typing the node (the same `OpenScope.tryResolve` that
        /// grounds the call) and records its key; Elaborate stamps it onto the minted
        /// `TExpr.External` so `InlineExpansion` splices the body by KEY. Absent ⇒ the
        /// head keeps `key = ValueNone` (`op_AddressOf` / other non-provider intrinsics,
        /// or a splice target whose symbol did not resolve — a diagnostic already fired).
        /// The operator/intrinsic twin of `ExternalValue` (resolved *value* refs),
        /// separate because these heads are minted fresh by Elaborate rather than routed
        /// through `translateIdent`'s `ExternalValue` path.
        IntrinsicKey: SideTable<SymbolKey>
        /// Keyed by a `:?` type-test expression's `NodeKey`: the resolved
        /// tested-against type (`Expr.DynamicTypeTest`'s target). The node's own
        /// inferred type is `bool` (the result), so the target type — which
        /// codegen needs for the `isinst` operand — is stashed here by
        /// Unification and read by Elaborate to populate `TExpr.TypeTest.testTy`.
        TypeTestTargets: SideTable<SemType>
        /// Keyed by a `use` binding's head-pattern `NodeKey`: how the binder is disposed.
        /// Recorded by `Unification`'s `use`-Dispose resolution and read by `Elaborate` to
        /// stamp `TExpr.Use.dispose`. Absent ⇒ `Disposal.Unresolved` — Unification reported
        /// a `use`-over-non-disposable error (or the binder's type never resolved), so no
        /// backend may lower the node.
        UseDispose: SideTable<Disposal>
        /// Keyed by a `for x in src do …` node's `NodeKey`: how the source yields
        /// its enumerator. Recorded by `Unification.inferForIn` and read by `Elaborate`
        /// to stamp `TExpr.ForIn.enumerator`. Absent ⇒ `ForInEnumerator.Interface`
        /// (range sources and the interface path); present with
        /// `ForInEnumerator.Pattern` for a source exposing only a pattern-based
        /// `GetEnumerator()`.
        ForInShape: SideTable<ForInEnumerator>
        /// Keyed by a type-reference OR an expression-position type-name node: the
        /// `SymbolKey` that reference resolves to. Minting sites:
        /// `NameResolution.registerUnionTypeDefn` stamps the *decl* site (`DeclType`
        /// key) from the union's minted `Key`; `translateType` / `resolveNamedGeneric`
        /// stamp type-annotation *use* sites (`TypeNamed` / `TypeGeneric` keys); and
        /// NameResolution's ident/long-ident walk stamps *expression* sites — a
        /// generic external-type receiver (`EqualityComparer<int>.Default`, the
        /// `Expr.TypeApp` head), a folded static-member receiver prefix
        /// (`System.Console` in `System.Console.Out`, the whole `Expr.LongIdent`
        /// node's key), and an external ctor-sugar head (`InvalidOperationException`
        /// as an `App` head).
        /// Read by the type-decl emitter (`Elaborate.tryUnionType`) and the enum use-site
        /// elaborator by type key, and by Unification's `tryExternalTypeReceiver` /
        /// `splitExternalClassPrefix` / `tryInferExternalCtorApp`, which take the
        /// stamped declaring-type key into a key-addressed `TryLookupMember` /
        /// `TryLookupMembers(_, ".ctor")`. That split IS F#'s name-resolution /
        /// type-inference seam: the static type prefix is resolved here, opens-aware,
        /// while the post-dot member name stays a string — a non-opens-sensitive
        /// post-selector.
        ResolvedType: SideTable<SymbolKey>
        /// Keyed by a written **type-annotation head** (`CstKeys.ofTypeHead` — a
        /// `NamedType`/`GenericType`/`SuffixedType` anchored on `li.Idents.[0]`): the
        /// external `SymbolKey` that head resolves to, minted by NameResolution's
        /// `tryResolveExternalTypeKey` at the syntactic type-arg arity.
        /// `Translate.tryResolveExternalTypeStamped` reads the stamp and fetches the
        /// shape through `ctx.Provider.TryLookupType key`. Distinct from
        /// `ResolvedType`: that records *expression*-position type names (ctor-sugar
        /// heads, generic static receivers) keyed by their `Expr*` `NodeKind`; this
        /// records *type*-position heads keyed by their `Type*` `NodeKind`. An abbrev
        /// head stamps its OWN key (`useSiteTypeKey` returns it); Translate dealiases
        /// on read. Absent ⇒ the head is project-local, a bare typar, or an unreachable
        /// name — Translate takes its local-registry / opaque / `TyVar` paths. (The
        /// `float<m>` measure carrier is synthesized during inference with no `Type`
        /// node to stamp, and keeps the one sanctioned resolver-face reach.)
        ResolvedTypeHead: SideTable<SymbolKey>
        /// A static-access receiver's resolved external CLASS key — the writer
        /// guarantees the Class shape, so readers dispatch with no shape re-query.
        /// Two minting forms, each keyed by its own node: a folded static-member
        /// `Expr.LongIdent` (`System.Console.Out`, `N.pickName`) stamps the receiver
        /// PREFIX (every segment but the last; read by `splitExternalClassPrefix`,
        /// then `TryLookupMember(prefixKey, lastSegment)` selects the post-dot
        /// member by key), and a generic `Expr.TypeApp` receiver *head*
        /// (`EqualityComparer<int>` in `EqualityComparer<int>.Default`, resolved at
        /// exact arity; read by `tryExternalTypeReceiver`). This is DISTINCT from
        /// `ResolvedType`, which records the type a node names *wholly* at ANY
        /// shape (a ctor-sugar head, a bare type ref, arity-based diagnostic
        /// suppression, the generic-ctor abbrev path). The two carry incompatible
        /// meanings for the SAME folded-LongIdent node —
        /// `System.InvalidOperationException` is a whole-name class (a ctor head,
        /// `ResolvedType`) while `N.pickName` is a prefix class + trailing member
        /// (here) — so they cannot share one table: a ctor-app consumer reading
        /// `ResolvedType` must NOT see the receiver prefix of a static member and
        /// mistake it for a constructible head. Absent when the receiver is not an
        /// external class (a namespace, a local field chain, an unknown qualifier,
        /// a union/record/abbrev/intrinsic receiver — each keeps its own path).
        ExternalStaticReceiver: SideTable<SymbolKey>
        /// Keyed by a ≥2-segment qualified `Expr.LongIdent` whose qualifier (every
        /// segment but the last) resolves to an external UNION or RECORD: the
        /// qualifier's resolved nominal `SymbolKey`. Unlike a class, a union/record
        /// exposes no static fields, so a `Q.member` whose `member` resolves to
        /// neither a value nor a case nor a static member is a genuine missing-member
        /// reference, not the unmodelled-static-field silence a class qualifier
        /// warrants: Unification's `tryQualifiedExternalMemberMiss` reads the stamped
        /// key to raise "Type 'Q' has no value or member 'm'" (Q = the resolved
        /// identity, not the written spelling) for such an unresolved tail. The
        /// member-miss error itself stays in Unification, where `errorTy` also types
        /// the node. Absent ⇒ the qualifier is a class (unmodelled-static silence), a
        /// namespace, or unknown; present-but-unread when the tail DID resolve (a
        /// valid case / value / static never reaches the miss path).
        ExternalUnionRecordQualifier: SideTable<SymbolKey>
        /// Project-local *module* member registry: a local module's short name
        /// (`SetTree`) → its directly-declared `let` value/function bindings (member
        /// name → the binding-site `NodeKey` `bindingsOfPat` mints for the head
        /// pattern). Populated by `NameResolution.registerLocalModules`, a pre-pass
        /// over the *un-flattened* module tree — the flattened element walk
        /// (`CstWalk.walkModuleTreeWith`) erases module boundaries, so a sibling
        /// module's function would otherwise be unresolvable. Read by the
        /// qualified-name path (`SetTree.add` resolves to the member's binding site,
        /// recorded as a use-site `Binding` entry so Unification/Elaborate treat it as an
        /// ordinary local reference) and by the nested-type body walk (an enclosing
        /// module's bindings enter the type-body scope, unqualified). The `SetTree`
        /// *module* and a same-named `SetTree<'T>` *type* coexist: this table is keyed
        /// independently of `Types.Class`.
        LocalModules: Dictionary<string, Dictionary<string, NodeKey>>
        /// Maps a local *type*'s short name (`SetIterator`) → the short name
        /// of the module it is declared inside (`SetTree`). Populated alongside
        /// `LocalModules`; consulted by the nested-type body walk to merge the
        /// enclosing module's bindings into the member-body scope. Absent for a
        /// type declared at namespace / file top level.
        TypeEnclosingModule: Dictionary<string, string>
    }

module PassContextResolution =
    let create (ambient: OpenScope) : PassContextResolution =
        {
            OpenScope = ambient
            AmbientOpenScope = ambient
            TyparScope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)
            BindingTyparSeed = ValueNone
            EnclosingTypars = ValueNone
            TyparScopeStrict = false
            ExternalAccess = SideTable<_>()
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
            ResolvedTypeHead = SideTable<_>()
            ExternalStaticReceiver = SideTable<_>()
            ExternalUnionRecordQualifier = SideTable<_>()
            LocalModules = Dictionary<_, _>()
            TypeEnclosingModule = Dictionary<_, _>()
        }

/// A `recv?name` dynamic-access site whose `^TResult` var (`Root`) may escape
/// `dynamic` to a concrete type through context (`d?foo + 1` pins it to `int`).
/// Recorded by `inferDynamicLookup`; swept post-settle by `DynamicEscape.run`,
/// which warns when `Root` zonks to a non-`dynamic` shape (the `default : dynamic`
/// did NOT fire — an unchecked assertion). `Key` is the `?` node's key, used both
/// to attribute the warning and to match a suppressing `(d?foo : T)` ascription.
type DynamicEscapeSite = { Root: TypeVar; Key: NodeKey }

/// The fixed set of Vesper.Core inline *access* intrinsics — the array/string/index
/// read+write lowering (`arr.[i]`, `arr.[i] <- v`, `arr.Length`, `s.[i]`, an
/// index-signature `x.[k]`). Each field is the resolved `ExternalSymbol` the
/// inference site instantiates (`ExternalSymbols.instantiateSymbol`) and whose `Key`
/// it threads into `IntrinsicKey` for the Elaborate/InlineExpansion splice. These names
/// live in `[<AutoOpen>]` prelude modules (`Vesper.Operators` /
/// `Vesper.StringIntrinsics` / `Vesper.IndexIntrinsics`), so they resolve through the
/// AMBIENT open scope alone — opens-insensitive — and are resolved ONCE per file
/// (`PassContext.CoreAccess`) rather than re-run per node. `ValueNone` = the name is
/// not in scope (no Vesper.Core referenced), which each reader turns into an
/// "intrinsic not in scope" diagnostic. Resolving here — not at each use site —
/// keeps `PassContext.Provider` a pure key-addressed `IExternalSymbolStore` face.
type CoreAccessIntrinsics =
    {
        GetArrayLength: ExternalSymbol voption
        GetArray: ExternalSymbol voption
        GetString: ExternalSymbol voption
        GetIndex: ExternalSymbol voption
        SetArray: ExternalSymbol voption
        SetIndex: ExternalSymbol voption
    }

/// **Thread-safety:** a `PassContext` is single-threaded — its side tables,
/// `Diagnostics` channel, and the `TypeVar` graph it owns all mutate in
/// place and are not safe to access from multiple threads. Parallelism
/// happens at file granularity by allocating one `PassContext` per file
/// and analysing them concurrently; the shared `IExternalSymbolProvider`
/// is the only object that crosses thread boundaries (and its contract
/// requires thread-safe `TryLookup`).
///
/// The bulk of the per-file state lives in three sub-records grouped by
/// concern: `Types` (project type registry), `Bindings` (per-binder side
/// tables), `Resolution` (name-resolution scopes).
[<Sealed>]
type PassContext(provider: IExternalSymbolProvider, input: string, lexed: Lexed) =
    // Seed the ambient (implicit-open) prelude from the provider's
    // `AmbientOpenPrefixes` (the referenced-contract `[<AutoOpen>]` modules /
    // FSharp.Core prelude). This is the single seam: every path that builds a
    // `PassContext` (the pipeline and the direct-construction tests alike) picks
    // it up here. Providers without an implicit prelude return `[]`, so
    // resolution is unchanged for them. The ambient sits at the tail of the
    // prefix list, so explicit `open`s the pass walk prepends are tried first.
    let ambientOpenScope =
        { OpenScope.empty with
            Prefixes = provider.AmbientOpenPrefixes
        }

    // `IntrinsicReprTypes` holds ONLY this compilation unit's own intrinsic
    // bindings (`type int = (# "System.Int32" #)`), registered by NameResolution.
    // A *referenced* package's intrinsics are no longer seeded here: they ride
    // the provider as `ExternalTypeShape.Intrinsic` shapes, read local-first /
    // provider-fallback by `subsumes.canonKey`, `translateType`, and codegen.
    let types = PassContextTypes.empty ()

    /// The **store face** (`SymbolKey → payload`) of the external-symbol contract —
    /// the default face every downstream pass (Unification, Elaborate, InlineExpansion,
    /// codegen) speaks once identity is already resolved. Narrowed from the full
    /// `IExternalSymbolProvider` on purpose: a consumer pass CANNOT reach a spelling
    /// lookup through `ctx.Provider` because the resolver face isn't on it. A genuine
    /// `string → identity` reach lives on `ctx.Resolver` and is sanctioned only for the
    /// named readers documented there. A free upcast of the same backing object.
    member _.Provider: IExternalSymbolStore = provider

    /// The **resolver face** (`string → identity`) of the external-symbol contract.
    /// Deliberately the ONLY string-lookup handle reachable from a `PassContext`, kept
    /// narrow and greppable so the resolve-once boundary stays enforced by exposure:
    /// a pass holding only the store-face `Provider` CANNOT resolve a spelling. Its
    /// sanctioned readers — pinned by `ResolverAllowlistTests` in the SA test suite —
    /// are each a genuine `string → identity` reach that survives by construction
    /// (not a `SymbolKey` round-trip):
    ///   - `NameResolution` — the resolve-once layer that owns `string × OpenScope →
    ///     SymbolKey` (and its intrinsic-key / runtime-type helpers);
    ///   - `Translate.tryResolveExternalType` — the by-name reach for the one head with
    ///     no `Type` node to carry a stamp (the synthesized `float<m>` measure carrier)
    ///     — plus its DEBUG-only stamping-gap witness;
    ///   - codegen's cross-package inline-body key interning (`SymbolProviders`) — a
    ///     by-name value lookup pending its own by-key conversion.
    /// Any NEW consumer-pass string resolution is a boundary violation: speak the
    /// key-addressed `Provider` store face instead. A free upcast of the same object.
    member _.Resolver: IExternalSymbolResolver = provider

    /// The four language-capability identities, resolved once here THROUGH THE
    /// PROVIDER (`ExternalSymbols.resolveCapabilities`) from their canonical Vesper
    /// contract names — the single carrier the `for-in`/`use` lowering and the FS0378
    /// custom-eq/comp check read. A capability the provider does not name is
    /// `ValueNone` (resolve-on-use, §5.4); the passes carry zero hardcoded BCL
    /// identities.
    member val CapabilityIds = ExternalSymbols.resolveCapabilities provider with get

    /// The Vesper.Core inline access intrinsics (`CoreAccessIntrinsics`), resolved
    /// ONCE against the ambient prelude scope. `lazy` so files that touch no array /
    /// string / index access pay nothing; the access/assignment inference sites
    /// (`inferIndexedLookup`, `resolveFieldStep`'s `.Length`, `inferAssignment`) read
    /// the stored `ExternalSymbol` by field instead of resolving the spelling through
    /// the provider per node. These names are ambient (`[<AutoOpen>]` prelude modules)
    /// and opens-insensitive, so resolving against `ambientOpenScope` yields the same
    /// hit a per-node opens-aware resolve would.
    member val CoreAccess: Lazy<CoreAccessIntrinsics> =
        lazy
            (let one (name: string) =
                OpenScope.tryResolve ambientOpenScope provider.TryLookup name

             {
                 GetArrayLength = one "GetArrayLength"
                 GetArray = one "GetArray"
                 GetString = one "GetString"
                 GetIndex = one "GetIndex"
                 SetArray = one "SetArray"
                 SetIndex = one "SetIndex"
             }) with get

    member val Input = input
    member val Lexed = lexed
    /// The simple name of the assembly this compilation unit emits into — the
    /// **home assembly** stamped onto every locally-minted nominal `SymbolKey`
    /// (`LocalSymbolKey.ofType`), so a project-local type's key equals the key a
    /// *consumer* mints for the same type from its `SymbolOrigin` (asm = the
    /// declaring assembly, invariant per type).
    /// `""` for the front-end-only / contract-scrape paths that never emit and so
    /// have no home assembly to stamp; set by `Pipeline.analyse*For`.
    member val AssemblyName = "" with get, set
    // Fully qualified: this file `open`s `XParsec.FSharp.Parser`, which also
    // defines a `Diagnostic`; with our `Diagnostic` now declared in `SideTypes.fs`
    // (ahead of `Tast.fs`) rather than in this file, the bare name would bind to
    // the parser's. The record literals in `Error`/`Warn` below resolve by field
    // labels, so only this annotation needs the qualifier.
    member val Diagnostics = ResizeArray<XParsec.FSharp.SemanticAnalysis.Diagnostic>() with get
    member val Types = types with get

    /// The primitive-intrinsic identities (`int`/`string`/…) resolved from the
    /// `prim-types-*` contract — the `SemType` analogue of `CapabilityIds`, so the passes
    /// carry no static intrinsic `SemType`s. Each field resolves lazily/cached (see
    /// `IntrinsicSet`), reading this unit's own `IntrinsicKeys` (populated by the
    /// NameResolution pre-pass) first, then the provider via ambient `open`.
    member val Intrinsics =
        IntrinsicSet(fun name -> IntrinsicResolve.tryResolveIntrinsicType provider types.IntrinsicKeys name) with get

    /// PassContext-lifetime memo of nominal `SymbolKey` → canonical intrinsic
    /// `SymbolKey`, populated lazily by `subsumes.canonKey`. `subsumes`' recursive walk
    /// would otherwise round-trip the composite provider / MetadataLoadContext per node
    /// to read an `ExternalTypeShape.Intrinsic` canon; the set is tiny and bounded, so a
    /// per-context cache keyed by the incoming key suffices. A key that is neither a local
    /// nor a provider intrinsic caches its own identity.
    member val IntrinsicCanonCache = Dictionary<SymbolKey, SymbolKey>() with get

    /// The reverse intrinsic axis `{ platform-repr -> canon }`: a platform runtime
    /// name (`"number"`) -> the `.fsi` canon identities sharing that repr. Its sole
    /// unify-time reader is `numericFamilyOr` (the JS `number`-family contravariant
    /// widening, keyed on the MULTI-canon entries) — the single-canon BCL
    /// reconciliation (`"System.Exception"` -> `exn`) that `canonKey` used to read
    /// from here now happens eagerly at resolution (`MetadataSymbols.tryBuildType`),
    /// so no BCL name reaches the unifier. Merges the provider's
    /// `IntrinsicReverseCanon` (referenced contracts) with this unit's own
    /// self-compiled intrinsics (`IntrinsicReprTypes`, inverted). `lazy` so it is
    /// built once, on the first Unification read — AFTER NameResolution has
    /// populated `IntrinsicReprTypes`.
    member val IntrinsicReverseCanon: Lazy<Dictionary<string, SymbolKey list>> =
        lazy
            (let d = Dictionary<string, SymbolKey list>()

             for KeyValue(platform, canons) in provider.IntrinsicReverseCanon do
                 d.[platform] <- canons
             // Local self-compiled intrinsics (short `.fsi` name -> platform repr):
             // invert so a raw platform name reconciles with the short identity within
             // a `--compiling-fslib` unit. Skips a degenerate `platform = short`. A local
             // repr wins over the provider's canons for the same platform (self-compiled
             // identity is authoritative within the unit), so it replaces the entry. The
             // canon value is the contract-stamped qualified identity (`IntrinsicKeys`, keyed
             // from the declaring `namespace`), so it compares EQUAL to the forward/provider
             // canons; `intrinsicKeyOf` falls back to the by-name mint only for a repr with
             // no stamped key.
             for KeyValue(short, platform) in types.IntrinsicReprTypes do
                 if platform <> short then
                     d.[platform] <- [ TypeRegistry.intrinsicKeyOf types short ]

             d) with get

    member val Bindings = PassContextBindings.empty () with get
    member val Resolution = PassContextResolution.create ambientOpenScope with get
    member val Desugared = SideTable<DesugaredForm>() with get
    /// Keyed by an `Expr.App` NodeKey; present only for printf calls lowered
    /// inline (literal format, fully applied, a `StdOut`/`StdErr`/`StringResult`
    /// sink, every specifier `PrintfHoleForm.tryClassify` accepts). Absence keeps
    /// the existing FSharp.Core path.
    member val PrintfApp = SideTable<PrintfSpec.PrintfSink>() with get
    /// Keyed by the same `Expr.App` NodeKey as `PrintfApp`; present only for a
    /// fully-applied `%a`/`%t` call on a *writer/builder* family (a `Writer` /
    /// `Builder` sink whose scratch type the provider resolves). Carries the
    /// resolved scratch class + `ToString` key Elaborate splices into the capture-first
    /// residue block. `sprintf` `%a`/`%t` has no entry — its residue is the
    /// callback's returned string; absence means "no scratch needed".
    member val PrintfCallbackScratch = SideTable<PrintfSpec.CallbackScratch>() with get
    /// Keyed by an `Expr.App` NodeKey; present only for a *fully-unapplied*
    /// lowerable printf partial (`printfn "%d"`, `printf "%d %s"`, …) — a literal
    /// format, `idx = 0`, a `StdOut`/`StdErr`/`StringResult` sink, `1..K` holes,
    /// every specifier lowerable and none `%A`/`%O` (an unapplied `%A` hole is an
    /// unpinned typar). Elaborate synthesises a Vesper closure
    /// `fun h1 … hn -> Format(sink, …)` for it (heap, 4a) instead of the
    /// FSharp.Core `PrintfFormat` cold path. Mutually exclusive with `PrintfApp`
    /// (that fires only when the call is fully applied). Absence keeps the existing
    /// FSharp.Core path.
    member val PrintfPartial = SideTable<PrintfSpec.PrintfSink>() with get
    /// E1: a `let`-bound (or ascribed) format-string literal, keyed by its
    /// BINDING-SITE NodeKey (the head pattern's key — the same key a use-site
    /// `Ident` resolves to via `Bindings.Binding`). Recorded by `Infer.inferBinding`
    /// when `tryTypeFormatLiteral` types the literal against a `PrintfFormat`
    /// annotation. The printf gate (`tryInferPrintfApp`) and Elaborate
    /// (`translatePrintfFormat`) both const-propagate through it: a format position
    /// holding such an `Ident` recovers the literal and lowers natively, exactly like
    /// a syntactic literal — there is no cold runtime for a format value in the
    /// self-host contract (the printf functions are inline-lowered intrinsics), so
    /// native lowering is the ONLY runnable path.
    member val PrintfFormatLiterals = SideTable<Expr<SyntaxToken>>() with get

    /// E1 const-propagation: if `argExpr` at a printf format position is an `Ident` /
    /// `LongIdent` bound to a format-string literal (recorded in
    /// `PrintfFormatLiterals` by `inferBinding` when `tryTypeFormatLiteral` typed it
    /// against a `PrintfFormat` annotation), return that underlying `Expr.String` so
    /// the gate / Elaborate can treat it exactly like a syntactic literal. `ValueNone`
    /// for any other shape — a direct literal (handled by the ordinary path), or a
    /// non-format binding. Consulted by BOTH the gate (`tryInferPrintfApp`) and Elaborate
    /// (`translatePrintfFormat`), so the two stay in lockstep.
    member this.TryRecoverFormatLiteral(argExpr: Expr<SyntaxToken>) : Expr<SyntaxToken> voption =
        match argExpr with
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) ->
            match this.Bindings.Binding.TryGetValue(CstKeys.ofExpr argExpr) with
            | ValueSome rb -> this.PrintfFormatLiterals.TryGetValue rb.BindingSite
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// Type provenance: the set of nodes whose type was **written by the programmer**
    /// (a source type annotation fixed it), keyed by the annotated value / pattern /
    /// binding node. A node's type is DECLARED iff `IsTypeDeclared` — every other
    /// type-bearing node (list / array / record / DU / object-expression element,
    /// unannotated `let` / lambda parameter, …) is INFERRED *by absence*, so the two
    /// classes partition the type-bearing nodes without recording the (far larger)
    /// inferred set. Recorded by `Infer`/`InferPat`/`InferTypeOps`/… at each
    /// `translateType`-of-a-source-annotation site (ascription `(e : T)`, annotated
    /// `let`/return, typed pattern & parameter `(x : T)`, `new T(…)`, `:> T` / `:? T`
    /// / `:?> T`, `match … :? T as x`). Type *declarations* (`type …`) are always
    /// explicit and carry no per-node provenance, so they are out of scope.
    ///
    /// A DECLARED node's type can still contain INFERRED positions — a `_` wildcard
    /// (`Box<_>`: `Box` declared, the arg inferred). Those are tracked per-TyVar by
    /// `inferenceHoles`: read provenance off the node's *un-zonked* annotation type and
    /// treat a `TyVar` position as inferred iff `IsInferenceHole`, every nominal /
    /// arrow / tuple / applied-with-concrete-arg position as declared. (A *named* typar
    /// `'a` in `Box<'a>` is written, so it is NOT a hole — only the anonymous `_` is.)
    member val private declaredTypeSites = SideTable<unit>() with get

    /// The `_`-wildcard TyVars minted by `translateType` for a source `Type.VarType
    /// Typar.Anon`. Reference identity: the exact node stored at the wildcard position
    /// of a declared annotation's type. Query it as it appears in the *un-zonked* type
    /// (zonking a resolved hole to its inferred fill would erase the marker).
    member val private inferenceHoles = HashSet<TypeVar>(HashIdentity.Reference) with get

    /// Mark `key`'s type as source-declared (see `declaredTypeSites`), given the
    /// annotation's translated type `annTy`. A BARE `_` (`let x : _ = …`, `(x : _)`) is
    /// a request to *infer*, not a declaration, so it is skipped — the node stays
    /// inferred. Any written structure (`Box<_>`, `int`, `'a`) marks declared, even when
    /// it contains nested `_` holes (queryable via `HasInferenceHoleIn`). Idempotent.
    member this.MarkTypeDeclared(key: NodeKey, annTy: SemType) =
        let isBareHole =
            match annTy with
            | TyVar tv -> this.IsInferenceHole tv
            | _ -> false

        if not isBareHole then
            this.declaredTypeSites.Set(key, ())

    /// Whether `key`'s type was written in source (`true`) rather than inferred
    /// (`false` — the default for every type-bearing node with no annotation). Note a
    /// `true` node may still carry inferred `_`-wildcard positions (`IsInferenceHole`).
    member this.IsTypeDeclared(key: NodeKey) : bool = this.declaredTypeSites.ContainsKey key

    /// Mark `tv` as a `_`-wildcard inference hole (see `inferenceHoles`). Idempotent.
    member this.MarkInferenceHole(tv: TypeVar) = this.inferenceHoles.Add tv |> ignore

    /// Whether `tv` is a `_`-wildcard hole — an INFERRED position inside an otherwise
    /// declared annotation type. Check the TyVar as stored in the un-zonked type.
    member this.IsInferenceHole(tv: TypeVar) : bool = this.inferenceHoles.Contains tv

    /// Whether `ty` — read from the LIVE (pre-freeze) TyVar graph, e.g.
    /// `TyVar ctx.Bindings.TypeVar.[key]` — carries any `_`-wildcard hole. Distinguishes
    /// a fully-written `Box<int>` (`false`) from a partly-inferred `Box<_>` (`true`) at a
    /// node that `IsTypeDeclared`. Follows a non-hole var's `Link` to reach structure but
    /// STOPS at a hole (its `Link` is the *inferred fill*, not part of the written type),
    /// so a resolved `Box<_>` (`_` pinned to `int`) still reports its hole. Elaborate zonks
    /// holes away, so this must run against the pre-zonk graph, not the frozen TAST.
    member this.HasInferenceHoleIn(ty: SemType) : bool =
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec walk t =
            match t with
            | TyVar tv when this.IsInferenceHole tv -> true
            | TyVar tv ->
                if not (seen.Add tv) then
                    false
                else
                    match tv.Link with
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

    /// The node-keyed value-struct closure verdict. Keyed by a
    /// SOURCE-lambda argument's NodeKey; the `FunVerdict` carries the flat `FunN`
    /// arity (always) and, for a transformer combinator, the result-typar position.
    /// Recorded in `inferApp` when an argument lambda lands on a typar parameter
    /// whose `:> Fun<a,b>`/`:> Fun<a,b,c>` coercion bound fires (the `subsumes` arm). The
    /// decision lives here (inference) as the single source of truth; the Pipeline
    /// snapshots it onto `TastFile.FunVerdicts`, codegen's `discoverClosures` reads
    /// `Arity` to size the value-struct closure's flat `Invoke`, and
    /// `ClosureVerdictRewrite` reads `ResultTyparPos` for the slot/result rewrite. A
    /// lambda with no entry is the ordinary curried closure.
    member val FunVerdicts = SideTable<FunVerdict>() with get
    /// A project-local generalised binding's
    /// `NodeKey` → its frozen typar bounds (`FrozenConstraint` list). Written by
    /// `Elaborate.translateModuleElem` at the single index-minting point (so the
    /// bounds' typar leaves carry the SAME method-axis indices the body freezes
    /// with), snapshotted by `Elaborate.run` onto `TastFile.GenericFnSchemes`. Read
    /// by the call-site phantom-typar solve (`EmitCall`).
    member val GenericFnSchemes = SideTable<FrozenConstraint list>() with get
    /// Keyed by an `Expr.LibraryOnlyStaticOptimization` NodeKey: the resolved
    /// `when ^T : …` constraints of that one clause (the `and`-joined list), with
    /// the typar / required type translated to `SemType` while the binding's typar
    /// scope is live. Elaborate reads it to build each `TExpr.StaticOptimization`
    /// clause; the typar carries the inline binding's quantified root so
    /// `Inline.inlineExpand` can substitute it at the call site.
    member val StaticOpt = SideTable<EqArray<TStaticOptConstraint>>() with get
    /// Current let-depth (Rémy's levels). Push on entering a binding group's
    /// RHSes, pop after typing them; generalisation uses the pre-push value as
    /// the threshold for "which TyVars do I quantify?".
    member val CurrentLevel = 0 with get, set
    /// Bare-program list literals: each `[…]` whose container type was left
    /// *flexible* (a fresh `TypeVar`, paired with its element type) so a consumer
    /// can drive it — `List.fold`'s `Vesper.Collections.List` parameter flips it to
    /// the Vesper list, otherwise it defaults to FSharp.Core's `list`. Drained by
    /// `Unification.resolveListLiterals` after the walk: a still-free literal links
    /// to the default list, a flipped one has its element reconciled. Programs that
    /// declare their own `list` abbrev never register here (they resolve eagerly).
    member val ListLiterals = ResizeArray<TypeVar * SemType>() with get

    /// `recv?name` dynamic-access sites, enqueued by `inferDynamicLookup` and swept
    /// post-settle by `DynamicEscape.run`. A site whose `Root` zonks to a concrete
    /// non-`dynamic` type is an implicit escape (the `default : dynamic` did not fire)
    /// and warns — unless its `Key` is in `DynamicEscapeSuppressed`.
    member val DynamicEscapes = ResizeArray<DynamicEscapeSite>() with get

    /// `?` node keys whose escape warning is suppressed by an explicit ascription
    /// directly on the `?` expression (`(d?foo : int)`), recorded by
    /// `inferTypeAnnotation`. "Name the type at the escape point."
    member val DynamicEscapeSuppressed = HashSet<NodeKey>() with get

    /// Which cons-list a *bare-program* list literal/pattern (one no consumer
    /// pinned) defaults to when drained by `Unification.resolveListLiterals`.
    /// `false` (the default) keeps FSharp.Core's `list` — the form a normal
    /// FSharp.Core-referencing program prints/interops with. `true` is set by the
    /// self-host package build (`Pipeline.analyse*ForSelfHost`): a BCL-only package
    /// has no FSharp.Core, so an unpinned `[]`/`::` must land on the Vesper
    /// cons-list to emit `Vesper.List`-only — the `withCore`-vs-not distinction the
    /// front end cannot otherwise see (it lives in codegen's `ProjectInfo`).
    member val DefaultListIsVesper = false with get, set

    /// Compiler-recognised parameter attributes (`ParamAttrs`) for each
    /// module-level `let inline` binding, keyed by the binding's function-binder
    /// `NodeKey` and positionally aligned to its curried parameters. Populated by
    /// `Elaborate` (which also validates each `[<CallAtMostOnce>]` parameter's
    /// linearity) and read by `Passes.InlineExpansion` for *local* inline call
    /// sites; the cross-package twin travels in `ExternalSymbols.InlineBody`.
    /// Only bindings with at least one non-default parameter register here.
    member val InlineParamAttrs = Dictionary<NodeKey, ParamAttrs[]>() with get

    /// Source text of `token`. Empty for virtual (synthesised) tokens.
    member this.NameOf(token: SyntaxToken) : string =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenString(iT, this.Input)
        | TokenIndex.Virtual -> ""

    /// Allocation-free sibling of `NameOf`: a `ReadableString` view of `token`'s
    /// source text, without copying out a substring. Empty for virtual tokens.
    member this.ReadableOf(token: SyntaxToken) : ReadableString =
        match token.Index with
        | TokenIndex.Regular iT -> this.Lexed.GetTokenReadable(iT, this.Input)
        | TokenIndex.Virtual -> ReadableString.Empty

    /// Record an `Error`-severity diagnostic at `key`. The canonical way to
    /// report — collapses the otherwise-ubiquitous inline `Diagnostic` literal
    /// (every call passed `Code = ""` / `Severity = Severity.Error`).
    member this.Error(key: NodeKey, msg: string) =
        this.Diagnostics.Add
            {
                Key = key
                Code = ""
                Message = msg
                Severity = Severity.Error
            }

    /// `Warning`-severity analogue of `Error`.
    member this.Warn(key: NodeKey, msg: string) =
        this.Diagnostics.Add
            {
                Key = key
                Code = ""
                Message = msg
                Severity = Severity.Warning
            }
