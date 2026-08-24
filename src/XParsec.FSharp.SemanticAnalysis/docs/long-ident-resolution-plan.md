# Long-identifier resolution: one FCS-shaped algorithm over entity-scoped queries

**Status (2026-08-23): design CONFIRMED by the user (forks A–C settled, §5); step 1 LANDED.**
Root causes reproduced; the FCS algorithm below is read from `D:\roboz0r\fsharp` (line numbers
cite `src/Compiler/Checking/NameResolution.fs` unless stated) and cross-checked with
`dotnet fsi`. The red surface is `LongIdentResolutionTests` (SemanticAnalysis) and the
`GAP` `ptest`s in `CrossFileTests` (Clr).

## 1. What was reported, and what it actually is

Three agents reported "cross-file name resolution is fundamentally broken". The stacked
provider is sound: every genuinely cross-file shape (module value, module fn, module-held union
case bare or type-qualified, namespace-level union, record) resolves through
`AnalysedAssembly.analyse`, and mistyped probes report the right mismatch, so the resolutions
are real rather than a silently-minted `TyVar`. The defects are:

1. **A case qualified by a MODULE is never a case — single-file or cross-file.** Local:
   `Elaborate/Patterns.fs:141-155` and `InferPat.fs:112-125` admit one segment, or two where
   segment 0 is the UNION's name (`TypeRegistry.localQualifiedCase`, `TypeRegistry.fs:792`,
   `c.UnionName = qualifier`). External: `Scope.fs:stampPatCasesWith` (`:199-222`) stamps 1–2
   segments and `ExternalUnionCase.ResolvesWith` (`ExternalDeclarations.fs:167-170`) requires
   the qualifier to equal `UnionKey.Name`. `M.Red` and `Test.A.M.Red` fall through in both.
2. **The pattern failure is a crash, not a diagnostic.** `Elaborate.fs:305-312` runs
   `elaborateDecls` under `try … with _ -> [], [||]` once any error exists; `translatePat`'s
   `| _ -> failwithf "TODO"` throws on the unhandled `Pat.Named`, decls become `[]`, but
   `ModuleMembers` (`:322`) is still filled, and `TastPools.toPools` (`TastPools.fs:318`)
   faults on the orphan. Any `UndefinedPatternDiscriminator` reproduces it.
3. **The expression failure is silent.** `Scope.fs:405-419` suppresses the unresolved error
   when `TryLookupUnionCase last` hits by BARE case name (qualifier ignored) but stamps only a
   2-segment type-qualified spelling; `InferIdentExpr.fs:164-176` then mints a free `TyVar`.
   `let a () : int = Test.A.M.Red` reports nothing; Clr dies with "no call recipe for external".
4. **(True cross-file, codegen) a prior file's module VALUE self-references.**
   `ClrRecipes.emitExternalCall:312-320` re-homes a FUNCTION through `env.LocalModuleFns`
   (`SymbolKey` → `MethodDef`); there is no value twin. `Assembler.ModuleValues` (`:408-411`)
   is per-file by `BoundVarId`, so the read falls to a `MemberRef` scoped by
   `openSig.Origin = SymbolHome.InFile` of the same assembly — a self-`AssemblyRef`, and a
   METHOD ref to a static field besides. `fieldDefHandles[FieldKey.ModuleValue key]`
   (`Assembler.fs:336`) already holds the assembly-wide handle.

Items 2 and 4 are bounded and independent of this design; see §7.

## 2. The shape of defects 1 and 3: segment-by-segment speculation

Every qualified-name site reconstructs the module / type / member split of a dotted string on
its own, against per-KIND string-keyed lookups. The sinks, by file (count of the speculating
calls — `TryLookupUnionCase`, `ResolvesWith`, `localQualifiedCase`, `isCaseName`, `casesNamed`,
`tryClassifyExternalType`, `tryPickExternalType`, `LocalModules`, `tryStampExternalValue`,
`isWrittenTypeNameInScope`, `tryUnionBare`, `resolveQualifiedCtor`, `tryExternalStaticLongIdent`):
`Scope.fs` 18, `ExternalSymbolProviders.fs` 15, `TypeRegistry.fs` 9, `NameResolution.fs` 7,
`TypeRefStamp.fs` 5, `InferPat.fs` 4, `InferResolve.fs` 4, and 3 each in `InferIdentExpr.fs`,
`InheritParent.fs`, `Elaborate/Patterns.fs`, `Elaborate/Resolve.fs`.

Four distinct mechanisms answer "what does `A.B.C` denote", none of them the same:

| Mechanism | Where | What it can see |
|---|---|---|
| `OpenScope.tryResolve` over `provider.TryLookup` (dotted string ⊕ each open prefix) | `Scope.fs:tryStampExternalValue:47`, `OpenScope.fs:64` | external VALUES, by qualified spelling |
| `tryPickExternalType` with `arityProbes` (name, `` name`n ``, per prefix) then `SymbolKeyOps.tryDottedInModule` | `ExternalTypeProbe.fs:68`, `PublishedSurface.fs:240`, `SymbolKeyOps.fs:162` | external TYPES; peels ONE trailing segment through `ModuleContainers` |
| `TypeRegistry.tryWinner` over `pathReaches` (`WrittenTypeName.Path` walked from the use site's container and opens) | `TypeRegistry.fs:216-330` | LOCAL types — already FCS-shaped for the type axis |
| `ctx.Resolution.LocalModules` keyed by BARE module name, last two segments only | `Scope.fs:336-360`, `PassContext.fs:185` | local module VALUES; `A.M.f` and `M.f` collapse to `M` |

And union cases have no path mechanism at all: `PublishedSurface.UnionCases` is keyed by bare
case name (`PublishedSurface.fs:123-126`, "first declaration wins"), and `ResolvesWith` admits
only the declaring type's short name as qualifier.

The user's framing (2026-08-23): analysis knows the identifier is `M.Red` or `Test.A.M.Red`; it
should ask about the whole identifier in its syntactic position and get one answer, rather than
speculating whether a segment is a namespace / module / type / member.

## 3. What FCS does (verified)

Three entry points, one per syntactic position, each over `id :: rest`. Results combine with
`+++` (`NameResolution.fs:1730`): lazy, FIRST success wins, errors accumulate only on total
failure.

**Environment** (`NameResolutionEnv`, `:395-442`). The tables that matter here:
`eModulesAndNamespaces: NameMultiMap<ModuleOrNamespaceRef>` — FIRST-segment entries only;
`eUnqualifiedItems` — values, union cases (non-RQA), active patterns, and unqualified TYPE names;
`ePatItems` — cases, active patterns, literals; `eTyconsByDemangledNameAndArity`; and a
`eFullyQualified*` twin of the module and tycon tables that `open` never changes (`global.`).

How they are filled: a file's own `namespace X.Y.Z` adds its root modules and types
(`CheckDeclarations.fs:344`); `module M = …` adds `M` to `eModulesAndNamespaces`
(`AddLocalSubModule`, `:237`); `open P` adds P's TYPES (cases into `eUnqualifiedItems` and
`ePatItems` unless the union is RQA or IL, `:1280-1300`), VALUES, and its NESTED modules as
first-segment entries (`AddModuleOrNamespaceContentsToNameEnv`, `:1455-1477`); an
`[<AutoOpen>]` module's contents are added when the module itself is (`:1444-1449`). Inside
`namespace X.Y.Z` there is an implicit open of `X.Y.Z` (`CheckDeclarations.fs:352`).

**Module path** (`ResolveLongIdentAsModuleOrNamespaceThen`, `:2606`): ONLY the first segment is
looked up in `eModulesAndNamespaces` (`:2532-2600` walks nested modules greedily, but `…Then` is
called with `[]`, `:2607`). Everything after the first segment is resolved INSIDE the entity by
the position-specific `f`, which recurses into a sub-module only when nothing else in that
module answers.

**Expression** (`ResolveExprLongIdentPrim`, `:3146`):

- Single ident (`:3166-3260`): `eUnqualifiedItems` (a value wins outright; a type name → ctor
  or type ref) → type name by arity → error.
- Compound (`:3262-3370`): if `id` is a VALUE in `eUnqualifiedItems`, take it and leave `rest`
  for dot-lookup (`:3268-3276`, "values take total priority, constructors do NOT"). Otherwise
  `moduleSearch +++ tyconSearch +++ envSearch` (`:3313`): module path first, then `id` as a
  type with `rest` inside it, then any other unqualified item with `rest` as dot-lookup.
- Inside a module (`ResolveExprLongIdentInModuleOrNamespace`, `:3009`): value (`:3013`) →
  exception (`:3017`) → union case, SUCCESS IMMEDIATELY unless RQA (`:3022-3033`) → type, with
  `rest` resolved inside it (`:3037-3069`) → sub-module recursion (`:3073-3084`) → the RQA
  case as last fallback (`:3086`). The RQA hit carries a flag; `CheckExpressions.fs:2063-2064`
  turns it into FS0035. `dotnet fsi` agrees: `M.Red` on an RQA `M.Color` is FS0035, and
  `M.Circle`, `M.Square n`, `M.Color.Red` resolve.
- Inside a type (`ResolveLongIdentInTypePrim`, `:2745`): union case (expr/pattern kinds,
  `:2755`) → anonymous-record field → intrinsic members (props, methods, IL fields, events,
  record fields, `:2770-2840`) → nested types, only when `rest` is non-empty or the flag asks
  for type refs (`:2871-2887`).

**Pattern** (`ResolvePatternLongIdentPrim`, `:3463`):

- Single ident (`:3472-3510`): `ePatItems` else a NEW variable binding (with the upper-case
  warning). A multi-segment name is NEVER a binding.
- Compound (`:3512-3538`): `tyconSearch +++ moduleSearch` — **TYPE FIRST**, the reverse of
  expression position. Leftover `rest` after the item is an error ("not a constructor or
  literal", `:3537`).
- Inside a module (`ResolvePatternLongIdentInModuleOrNamespace`, `:3381`): union case
  (`:3383`, RQA flag carried, `CheckPatterns.fs:649-650` reports it) → exception → active
  pattern → value (a literal) → type-then-`rest` inside it → ctor (only when `rest` is empty) →
  sub-module recursion (`:3444-3452`).

**Type** (`ResolveTypeLongIdentPrim`, `:3681`):

- Single ident: by name AND arity (`:3692`), with a name-only fallback for error reporting.
- Compound: `tyconSearch` (nested type under an unqualified type) collected TOGETHER with
  `modulSearch` (`AddResults`, `:3741` — all results, then the ambiguity check), not first-wins.

The structural facts to carry over: the FIRST segment is classified once against the
environment; every later segment is resolved within the entity already found; the order within
an entity is fixed by position; a bare case-name reverse index exists nowhere.

## 4. Design

### 4.1 One resolver, three entry points, one result type

A new module `Passes/NameResolution/LongIdent.fs` owns:

```fsharp
[<RequireQualifiedAccess>]
type ResolvedItem =
    | Value of ValueRef              // local binding site, or ExternalSymbol + SymbolKey
    | UnionCase of UnionCaseRef * requiresQualifiedAccess: bool
    | EnumCase of TypeKey * name: string
    | Type of TypeRef                // local TypeIdentity or external (TypeKey, shape), with arity
    | Ctor of TypeRef                // a type name in expression position
    | StaticMember of TypeRef * name: string
    | ModuleOrNamespace of Container // only a prefix resolved; the caller decides if that is an error
    | Unresolved of UnresolvedName   // which segment failed, inside which entity — the diagnostic's payload

val resolveExpr:    PassContext -> UseSite -> LongIdent<SyntaxToken> -> ResolvedItem * rest: SyntaxToken list
val resolvePattern: PassContext -> UseSite -> LongIdent<SyntaxToken> -> ResolvedItem
val resolveType:    PassContext -> UseSite -> WrittenTypeName -> arity: int -> ResolvedItem
```

`rest` on the expression form is FCS's "remaining identifiers": `r.X.Y` after `r` resolves to a
value is a field/member chain, which the existing `inferLongIdentFieldChain`
(`InferIdentExpr.fs:40-46`) already handles.

Every consumer that today reads `ExternalUnionCaseStamp`, `ExternalValue`, `ResolvedType`,
`ExternalStaticQualifier`, `ExternalEnumCaseStamp` or the `LocalModules` binding becomes a
reader of ONE stamp, `Resolution.Resolved: NodeKey → ResolvedItem`. The per-kind tables are
deleted once nothing reads them (additive swap, then delete, per the F# design rules).

### 4.2 The entity-contents query, answered by both halves

The algorithm needs exactly one question of a scope: *what does bare `name` mean inside entity
`E`, in position `P`?* — plus *which entities does first-segment `s` denote at this use site?*.
That is the interface the provider and the file's own registry both implement:

```fsharp
type IScopeContents =
    /// `s` as a module or namespace, seen from `useSite`: the file's own scopes and opens
    /// (TypeRegistry.pathReaches), then the provider's `ModuleContainers` under each open
    /// prefix and the language prelude, then `s` as a fully-qualified root.
    abstract FirstSegment: useSite: UseSite -> s: string -> Container list
    abstract ValueIn:      Container -> name: string -> ValueRef voption
    abstract UnionCaseIn:  Container -> name: string -> UnionCaseRef voption   // RQA flag on the ref
    abstract TypeIn:       Container -> name: string -> arity: int voption -> TypeRef list
    abstract SubModuleIn:  Container -> name: string -> Container voption
    abstract CaseOfType:   TypeRef -> name: string -> UnionCaseRef voption
    abstract StaticOfType: TypeRef -> name: string -> bool
    abstract NestedTypeIn: TypeRef -> name: string -> TypeRef voption
```

`Container` is the existing `ModuleContainer` (`SymbolKeys.fs:51`) — a namespace or a module
key — which both halves already speak: `TypeRegistry.LocalContainers` maps source paths to it,
and `PublishedSurface.ModuleContainers` (`PublishedSurface.fs:36,145`) maps them to
`TypeContainer.InModule m`. So `FirstSegment` for the provider is a lookup of `prefix + "." + s`
in `ModuleContainers` for each open prefix, and `SubModuleIn c s` is the same table keyed by
`moduleFullName c + "." + s`. Neither needs a new producer; the surface already publishes the
chain (`addModuleContainer`, `:65-73`).

What the surface does NOT have and must gain: **a per-container index**. `Symbols` is keyed by
qualified spelling, `UnionCases` by bare case name, `TypesByName` by compiled name. `ValueIn`,
`UnionCaseIn` and `TypeIn` need `Container → name → item`. `PublishedSurface.ofBuilder`
derives indexes already (`:181`); a container-keyed index derived from the same tables is one
more, and `toProvider` (`:230`) hands it to a new `KeyIndexedChannels` field. The bare-name
`UnionCases` table then has no reader and goes.

On the local side, `TypeRegistry` already answers `TypeIn` (`tryWinner` with a path — the
reach machinery is the FCS module walk for types). `ValueIn` replaces `LocalModules`, keyed by
`ModuleContainer` rather than the bare last segment. `UnionCaseIn` is `casesNamed` filtered to
the container.

`OpenScope` keeps `Prefixes`/`Locals`/`Abbrevs`; `FirstSegment` is where it is consulted, once,
instead of being re-applied by every probe (`candidates`, `OpenScope.fs:47`). The composite
provider (`ExternalSymbolProviders.composite`) answers `FirstSegment` by concatenating its
layers nearest-first, which preserves file-order shadowing across the stack.

### 4.3 Position-specific order, encoded once

`resolveExpr` inside a container: `ValueIn → UnionCaseIn (non-RQA wins now) → TypeIn (then rest
inside the type) → SubModuleIn (recurse) → the RQA case`. `resolvePattern` inside a container:
`UnionCaseIn → ValueIn (literal) → TypeIn (then case inside it) → SubModuleIn`. At the top:
expression is `value-in-env → module → type → env-item`; pattern is `type → module`. Type
position collects module and type results and reports ambiguity.

The RQA flag rides the `UnionCase` result; the report (`Kind` for FS0035) is the consumer's,
exactly as FCS splits `Item.UnionCase(_, showDeprecated)` from `CheckExpressions`. So
`ResolvesWith`, `bareCaseNamespaceOpen` and `IsRequireQualifiedAccess` post-filters go: a bare
`Red` is in the unqualified items only when its union is non-RQA and its container is open,
which is what `FirstSegment`-less lookup over the use site's open containers yields.

### 4.4 What the diagnostics become

`ResolvedItem.Unresolved` says WHICH segment failed and INSIDE WHAT: "`Red` is not a value,
constructor, namespace or type in module `Test.A.M`" (FCS `undefinedNameValueConstructorNamespaceOrType`,
`:3120`) rather than the two current families ("Unresolved identifier", "The type … is not
defined") chosen by whichever probe gave up last. The suppression list at `Scope.fs:112-131` —
seven disjuncts that each stand in for a later pass resolving the name — is replaced by the
stamp itself: an `Unresolved` stamp IS the report.

## 5. Design forks

**A. One environment vs. two halves behind one query.** FCS builds a single `NameResolutionEnv`
incrementally as declarations and opens are checked. We could do the same — fold the file's
declarations and every provider layer into one set of tables per scope. The cost is that the
provider stack is already the assembly's environment and `TypeRegistry` already ranks local
claims by position (`claimRank`, `TypeRegistry.fs:282`); merging would re-derive both.
**Decided (user, 2026-08-23):** two implementations of `IScopeContents` (registry, composite
provider), consulted local-first by the resolver, which is the precedence `classifyTypeRef`
(`TypeRefStamp.fs:53-60`) already encodes for types. Mid-file `open` order — a later `open`
shadows an earlier `open` AND an earlier local `let` (`dotnet fsi` prints `1 2 20` for the
fixture in `LongIdentResolutionTests`, "GAP: `open` order within a file") — is a FOLLOW-UP,
pinned there as `ptest`s with type-based assertions. When it lands, `FirstSegment`/`ValueIn`
take the `BindingRank` the registry already computes, applied across both halves.

**B. Where the stamp lives.** Keep the per-kind side tables and have the new resolver fill them
(smallest diff), or replace them with `Resolved: NodeKey → ResolvedItem` (one reader shape).
**Decided (user, 2026-08-23):** the single table, landed additively: the resolver fills BOTH
for one step, consumers migrate, per-kind tables are deleted. The per-kind tables are what let a site stamp
one kind and forget another — defect 3 is exactly `ExternalUnionCaseStamp` unfilled while the
suppression fired.

**C. Whether `rest` survives on the expression form.** FCS returns remaining identifiers and
lets the checker dot-lookup them. The alternative is to resolve the whole chain here, including
record fields and members. **Recommendation:** return `rest` — member/field resolution needs
the anchor's TYPE, which Unification owns, and `inferLongIdentFieldChain` exists.

## 6. Steps

Each step leaves the tree green and is a separate review.

1. **Pin the red surface — LANDED.** `test/XParsec.FSharp.SemanticAnalysis.Tests/LongIdentResolutionTests.fs`:
   nine cross-file shapes that resolve today as `test` (two of them mistyped, asserting the
   mismatch so a resolution is known to be typed rather than a `TyVar`), and nine `GAP` `ptest`s
   with the assertion the fix must satisfy — module-qualified case in expression position (typed)
   and in pattern position (three-segment cross-file, after `open` of the namespace, and
   single-file), the RQA case reached through its module (reported, not unresolved), the
   undefined discriminator (reported, not a crash), and the two `open`-order fixtures.
   `CrossFileTests` (Clr) gained the module-VALUE self-ref and module-qualified case as `ptest`s,
   with the type-qualified case as the running control. Both suites green: 1411 + 1546.
2. **`IScopeContents` on both halves, no consumer — LANDED.** `ExternalSymbols.fs` declares
   the query (`TryContainer` / `TryValue` / `TryUnionCase` / `TypesNamed`, all scoped to a
   `ModuleContainer`) and `IExternalSymbolResolver.Scope` exposes it; `ScopeContents.composite`
   is the nearest-first composition `stack` uses; the decorator forwards; metadata and
   named-channel sources answer `ScopeContents.empty`. `PublishedSurface.toProvider` derives
   the container-keyed indexes from the tables already published — symbols by `Key.Decl`,
   cases by `UnionKey.Container`, types by `TypeKey.Container` — and the container set from
   every key's containment chain plus each namespace prefix, so a module holding only values
   is a container too. The local half is `LocalScope.fs` over `TypeRegistry`'s claims and a
   new `Resolution.LocalModulePaths` (source path → members) filled beside `LocalModules`.

   Two things changed shape on the way. `PublishedSurface.UnionCases` is keyed by the declaring
   union's compiled name plus the case name, so every case is retained; the bare-name
   first-wins index the legacy `TryLookupUnionCase` channel answers from is derived in
   `toProvider`, and on a bare-name collision within ONE surface the winner is now the
   ordinal-first key rather than the first declaration (the cross-file order is the stack's,
   unchanged). And `FirstSegment` is not on the interface: it is the resolver's job (step 3),
   composed from `OpenScope` prefixes and `TryContainer`.

   `ScopeContentsTests` covers both halves and the composition. Suites green: 1419 / 1547 / 666.

   **Follow-up surfaced:** a `[<CompilationRepresentation(ModuleSuffix)>]` module's container
   is published under its COMPILED name only (`ModuleKey.Name` is `ListModule`), while its
   values get a source-spelling alias (`SignatureResolution.fs:672-684`) and its types none.
   `TryContainer "Vesper.List"` therefore misses on the published half until the surface
   carries the source path beside the compiled one; the local half (`LocalContainers`, keyed
   by source path) already has it.
3. **`LongIdent.resolveExpr` / `resolvePattern` / `resolveType` — LANDED.**
   `Passes/NameResolution/LongIdent.fs` (module `NameResolutionLongIdent`, compiled before
   `TypeRefStamp.fs`, which now owns the external probes too) resolves a name in FCS's order
   over both halves, local first: the first segment against the environment (`valueInEnv`,
   `caseInEnv`, `typeInEnv`, `firstSegmentContainers`), every later segment inside the entity
   found (`inContainer` with the position-specific step order, `inType`). `ResolvedItem`
   (`ResolvedItem.fs`, before `PassContext.fs`) is the one result type;
   `Resolution.Resolved: NodeKey → ResolvedItem` is the one stamp. `Scope.fs` DERIVES every
   per-kind stamp from it (`stampItem`) and owes exactly one report per resolution
   (`reportExpr`), so the seven-disjunct suppression list is gone; the printf family is the
   one remaining exemption, being a front-end intrinsic no contract declares.
   `classifyTypeRef` derives its verdict from `resolveType`. The six `GAP` `ptest`s flipped,
   the Clr module-qualified case too. Suites: 1426 / 1548 / 666.

   Consumers that had to read `Resolved` for the module-qualified LOCAL case to type and
   lower (the start of step 4): `InferPat`, `InferIdentExpr`, `Elaborate/Patterns`,
   `Elaborate/Resolve`, through `ResolvedStamps.tryUnionCase`.

   What the suites forced, and why it stays until step 5:
   - **Metadata sources expose no module structure**, so after the structured module path
     the WHOLE spelling is looked up as a value (`wholeNameValue`) BEFORE the type-first
     step. `Set.singleton` against the built `Vesper.Set` is a metadata module path, and the
     generic class `Set<'T>` would otherwise claim it as a static member and leave a TyVar.
     **Follow-up (user, 2026-08-23):** the correct shape is a consolidated signature file
     overlaid on a referenced Vesper assembly — `publishing-format-plan.md` PF1/PF3 applied
     to the reference, not only to a source package: the `.dll` is the runtime artifact and
     the `.fsi` beside it the contract the resolver reads. A referenced `Vesper.Set` then
     answers `Scope.TryContainer "Vesper.Set"` through the signature path, which also
     publishes the `ModuleSuffix` alias, and `wholeNameValue` goes with step 5. Metadata
     alone stays the channel for a non-Vesper assembly (the BCL), whose types the folded
     probes already reach.
   - **An external class's static member is not existence-checked** (`inType`); Unification
     reads members by the stamped key, and the stub providers in `ExternalTypeKeyStampTests`
     publish no members. Local nominals and external unions / records are checked.
   - **The bare-name reverse index stays the last resort** for a type-qualified case
     (`folded.indexedCase`): the stub in `ExternalUnionCaseStampTests` publishes cases but no
     types, and `OpenResolutionTests` pins `Color.Red` resolving with `Tests` NOT opened,
     which F# rejects. Both are findings for step 5, when `TryLookupUnionCase` goes.
   - `valueInEnv` reads this file's OPENED modules only; the enclosing scopes' `let`s are
     bound by the walk in declaration order (`let f x = … f …` without `rec` stays
     unresolved, as `ExpansionTests` pins).

   New diagnostic: `Kind.RequireQualifiedAccessCase` (FS0035), reported at the use in both
   positions when a case of a `[<RequireQualifiedAccess>]` union is reached other than
   through its union's name. A bare RQA case is no longer in the environment at all
   (`caseInEnv` filters it), as in FCS.

   The `ModuleSuffix` follow-up from step 2 is half closed: `PublishedSurface.scopeOf` derives
   the source-path container from the alias symbols the SIGNATURE path publishes
   (`SignatureResolution.fs:672-684`), pinned in `ScopeContentsTests`. An implementation-
   published surface (`FrozenSignature.addValue`) has no source name to alias by; the frozen
   file would have to carry a module's source name beside its compiled one.
4. **Consumers read `Resolved` — LANDED.** Every reader of a per-kind external stamp now
   projects `ctx.Resolution.Resolved`, through `ResolvedStamps.tryExternalUnionCase` /
   `tryExternalEnumCase` / `tryStaticQualifier` / `tryUnionRecordQualifier`, bound in a
   match arm by the `Resolves` pattern (which replaces the generic `Stamped`): `InferIdentExpr`, `InferPat`,
   `InferResolve`, `Elaborate/Patterns`, `Elaborate/Resolve`. `ExternalUnionCaseStamp`,
   `ExternalEnumCaseStamp`, `ExternalStaticQualifier` and `ExternalUnionRecordQualifier` are
   deleted with the `stampItem` arms that fed them; `stampItem` feeds only an external
   value's symbol (`ExternalValue` / `ExternalSymbolStamp`) and a constructible external
   class's key (`ResolvedType`). `LocalModules` and `TypeEnclosingModule` are deleted too: a
   nominal's `EnclosingModuleScope` reads `LocalModulePaths` under the container the walk
   stands in (`ctx.CurrentContainer` → `LocalContainerPaths`), the path
   `registerLocalModules` files the module's `let`s under, so the `$top` sentinel went with
   them. `Elaborate/Idents` reads `ExternalValue` and `InheritParent` probes types, so
   neither read a per-kind stamp and neither changed. The four stamp test files assert
   through the projections. Suites: 1426 / 1548 / 666.

   Carried forward, unchanged in behaviour: `tryStaticQualifier` answers only for a
   NON-generic class or intrinsic, as the deleted table did, so a generic class's static
   reached without type arguments (`Set.Empty`) still falls to the TyVar fallback; and a
   static member an external union or record DECLARES resolves (`inType`) but is typed by no
   consumer, so Unification reports it as a member miss through `tryUnionRecordQualifier`,
   as it did through the deleted table.
5. **Delete the speculation.** `TryLookupUnionCase`, `ResolvesWith`, `localQualifiedCase`,
   `isCaseName`/`casesNamed` (as global reverse lookups), `tryDottedInModule`, `arityProbes`'
   per-prefix loop, `ExternalUnionCase.UnionKey`-by-name checks. Score by the runtime checks
   removed, per the repo rule. `wholeNameValue` and `folded.indexedCase` are gated on the
   signature overlay for referenced Vesper assemblies (step 3's first follow-up): until a
   referenced `Vesper.*` DLL carries its consolidated `.fsi`, its modules are reachable only
   by the whole spelling.

## 7. Independent of this plan — LANDED (2026-08-23)

- **Elaborate degrades totally.** `Elaborate.run` empties every bound-variable-keyed side
  table (`ModuleMembers`, `GenericFnSchemes`, `BindingTyparArities`) with the decls it drops,
  so `toPools` no longer faults on an orphan; "undefined pattern discriminator is REPORTED, not
  a crash" is a running test.
- **`LocalModuleValues`.** `ClrProvider.RegisterLocalModuleValue`, filled by the assembler's
  field pass for every `FieldKey.ModuleValue`, and answered in `TryEmitCall` as a `ldsfld`
  recipe (arity 0, one push) ahead of the external-call path, so a function-typed value still
  takes its arguments through `Invoke`. "a prior file's module VALUE reads through a local
  field" runs. The Js backend had no such gap: "a cross-file module VALUE runs under Node"
  passed on the first run and stays as the control.

## 8. Settled semantics (user, 2026-08-23)

- Local-first between the registry and the provider stack is the initial rule (fork A);
  mid-file `open` order is a follow-up, pinned as `ptest`s.
- Pattern position is type-first, expression position module-first — match FCS, do not unify
  the two orders.
- An RQA case reached through its module (`M.Red`) resolves and is then reported (FS0035), as
  `dotnet fsi` does; it is not an unresolved name.
- `module A.B.C` as a whole file still homes in the global namespace (the known gap in
  [fsi-front-end-plan](fsi-front-end-plan.md) follow-ups); `FirstSegment` inherits that until
  it is fixed, which is orthogonal. The `open`-order fixtures deliberately use `namespace` +
  nested `module` so they pin only the ordering.
