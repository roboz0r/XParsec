# One round trip per external type

*UNSTARTED. Re-anchored 2026-08-30 against the code as it stands; the 2026-08-13 draft's line
references had all drifted and two of its four defects have since been closed by other work.
The steps are strictly ordered: 0 → 1 → 2 → 3a → 3b → 3c → 4. Step 0 is a CLR backend change and
steps 1-2 do not depend on it, so it can run in parallel with them; step 3b cannot start until it
lands. Step 3 changes BEHAVIOUR, not just structure, and is bounded by the settled semantics below.

Three projects are in scope. `IExternalSymbolStore` is declared in `SemanticAnalysis` and
implemented in `Codegen.Clr` (`MetadataSymbols`) and `Codegen.Js` (`TsManifestProvider`), so steps
2 and 3 touch all three, and step 0 is `Codegen.Clr` alone.*

*This plan SUPERSEDES the `ProviderDecorator` two-level-base sketch raised in review of
`f4a32ab8`: that restores the compile-time totality the decorator gave up, but the decorator
itself is what this plan deletes.*

## What the 2026-08-13 draft got right, and what has moved

Closed since, by other work:

- **The key convention.** `CodegenSymbols.reconciledLookup` and its bare/arity-suffixed double
  probe are gone (`codegen-clr-followups-plan.md` B12 — *"the second convention did not exist"*).
  The draft's step 3 and its prerequisite ordering are discharged.
- **The value-ness ladder.** `IsValueType` is no longer a type-addressed channel on
  `IExternalSymbolStore`. `Platform: IPlatformFacts voption` (`ExternalSymbols.fs:371`, `:410`) is
  a whole-provider channel that `stack` settles with one `firstHit` (`ExternalSymbolProviders.fs:322`),
  and `ICodegenSymbols.IsValueType` publishes the settled verdict. No consumer re-derives the
  order. `TypeLayout.declaredOf` (`TypeLayout.fs:126`) keeps one external rung, a `TryLookupType`
  read, which this plan re-points rather than removes.

Still open, and the reason this document survives.

## The defect

An external type's facts are reachable through TWO independent surfaces, and every transform
over a provider must be written once for each, held in step by nothing but a comment.

| surface | channels |
|---|---|
| key-addressed (`IExternalSymbolStore`, `:382`) | `TryLookupType`, `TryLookupMembers`, `TryLookupMemberByKey`, `TryLookupIndexSignature` |
| name-addressed (`IScopeContents`, `:79`, reached via `IExternalSymbolResolver.Scope`, `:358`) | `TryValue`, `UnionCasesNamed`, `TypesNamed` |

`TypesNamed` returns `struct (TypeKey * ExternalTypeShape)` — the same payload `TryLookupType`
returns, under a different address. `TryValue` returns the same `ExternalSymbol` as
`TryLookupByKey`. The cost shows up in four places:

1. **Three wrappers each spell the lockstep by hand.** `stack` (`:263`), `mapProviderTypes`
   (`:430`) and `withInlineBodies` (`:472`) each decorate `Scope` beside their key-channel
   overrides, and each carries a comment asserting the two agree. They do not agree everywhere:
   `withInlineBodies` stamps `InlineBody` onto members returned by `TryLookupMembers` /
   `TryLookupMemberByKey`, and uses `ScopeContents.mapValues`, which passes `TypesNamed` through
   untouched. The SAME member reached through a shape carries no body; reached by name or key, it
   does. Nothing consumes the difference today — `tryInlineBody` (`:550`) routes only through the
   key channels — but it is a divergence one consumer away from being a bug.

2. **A type's declared members already live on its shape, and are read off it.** All three
   member-bearing arms carry the list: `ExternalClassShape.Members` (`ExternalDeclarations.fs:468`),
   `IntrinsicClassSurface.Members` (`:536`), `IntrinsicInterfaceShape.Members` (`:617`), selected
   by the existing `(|ExternalMembers|_|)` (`ExternalSymbols.fs:487`). `Subsume.fs` takes both
   routes in one file, and they answer differently on the same type: `groundMemberNames` (`:47`,
   backing `keyof`) reads the shape at `:57` and gets the DECLARED names, while `groundMemberType`
   (`:71`, backing `T[K]`) reads `TryLookupMember` at `:81` and gets the FLATTENED set. So
   `keyof T` can omit a name that `T[K]` resolves. Step 3b closes this by moving every resolution
   reader onto the flattened channel and leaving `Shape.Members` to emission.

3. **`ProviderDecorator` defaults every channel to a forward.** It exists (`:127`) to spare four
   subclasses — `mapProviderTypes` (`:434`), `withInlineBodies` (`:474`), `memoize` (`:523`) and
   `TsManifestProvider.IndexSignatures` (`TsManifestProvider.fs:84`) — from writing eleven
   forwarding members each. That is safe in `memoize` (a missed cache is slow) and a SOUNDNESS
   hazard in `mapProviderTypes`, whose job is to put a variance polarity on every `FrozenType`
   leaving the provider. A channel added to the contract now escapes variance mapping silently.
   Before the decorator, the compiler refused to build.

4. **`stack` is already the composite-with-transform, and is already used as one.**
   `AssemblyAnalysis.fs:226` is `stack (ValueSome(SymbolHome.InFile …)) [] [ r.Published ]` — a
   single-source `stack` whose only purpose is a per-entry home stamp. The wrappers are that same
   job done in the wrong place.

## The correction the draft missed

**`TryLookupMembers(key, name)` is not a one-type query.** At the metadata source it walks the
inheritance chain: `computeMembers` (`MetadataSymbols.fs:544`) builds a most-derived-first
candidate array over the base chain (or, for an interface, `GetInterfaces()` plus `System.Object`),
probes each level, dedups overrides by signature and re-sorts most-params-first so `[0]` is the
widest overload — the ordering `IExternalSymbolStore.TryLookupMember` (`:425`) documents and
depends on. `computeType` (`:514`) fills `ExternalClassShape.Members` from
`enumerateClassMembers` (`:397`), which is `BindingFlags.DeclaredOnly` on the type alone, in
`properties; methods; indexers; fields; ctors` order.

So the draft's headline claim — *"`computeType` already has the data; `computeMembers` is a
redundant second reflection pass"* — is **false**. The two passes compute different sets. Deleting
`computeMembers` would lose inherited-member lookup, which ~27 call sites depend on
(`Infer.fs:83` reaching `Dispose` on a class implementing `disposable`, every
`InferRecordAccess.fs` probe, `Subsume.fs:81`, `TraitMembers.fs:130`).

The two sources disagree about it. `ofKeyIndexedChannels.membersNamed` (`:66`) filters one type's
`MembersByKey` entry by name and walks no chain, and every contract source fills that entry with a
type's own members: `SignatureResolution.fs:478` hands ONE list to both the shape and the member
table (`publishShapeWith … shape surface.Members`), and `PublishedSurfaceBuilder.addTypeWith`
(`PublishedSurface.fs:158`) documents it as *"the type's FULL member list in declaration order,
duplicating a shape's own"*. So contract sources answer DECLARED-ONLY and the metadata source
answers FLATTENED.

**The front end already carries a third mechanism to paper over that.**
`EngineCore.tryExternalInheritedMember` (`:616`) walks external heritage at the CALL SITE, and its
call site in `resolveFieldStep` (`InferRecordAccess.fs:347`) states the disagreement outright:

> *"A supertype a PROVIDER owns: the local chain walk reaches project-local ancestors only, and
> the TS-manifest provider stores heritage un-flattened (the CLR metadata layer already flattens)."*

So one question — "what members does this type have?" — has three answers, meeting as a
fallback ladder at `InferRecordAccess.fs:340-349`:

| mechanism | covers |
|---|---|
| `EngineCore.classChainLevels` (`:228`) | project-local `inherit` chains; stops at the first ancestor absent from `TypeRegistry` |
| `MetadataSymbols.computeMembers` (`:544`) | CLR metadata heritage, flattened inside the provider |
| `EngineCore.tryExternalInheritedMember` (`:616`) | everything else, re-derived per access |

That is the `CLAUDE.md` defect in full: a stage discards the intermediate and consumers re-derive
it, and the derivations disagree.

## The shape

`ResolvedType` folds the channels that genuinely address one type, and `Scope` is derived from it
rather than decorated beside it:

```fsharp
/// Everything a resolved external type states, from ONE fetch.
type ResolvedType =
    {
        Key: TypeKey
        Shape: ExternalTypeShape
        /// The TS `{ [k: K]: V }` signatures, `(key, value)` templates over the DECLARING
        /// typars. Both kinds may be present.
        IndexSignatures: (FrozenType * FrozenType) list
    }

type IExternalSymbolStore =
    abstract TryResolveType: key: TypeKey -> ResolvedType voption
    abstract TryLookupMembers: key: TypeKey * memberName: string -> EqArray<ExternalMember>
    abstract TryLookupMemberByKey: key: MemberKey -> ExternalMember voption
    abstract TryLookupByKey: key: BindingKey -> ExternalSymbol voption
    abstract IntrinsicTypeMap: IntrinsicTypeMap
    abstract Platform: IPlatformFacts voption
```

`ResolvedType` carries no `Members` field, and the member channels do NOT fold into it. A type has
TWO member surfaces holding different content, and step 3 settles which reader takes which:

| surface | content | read by |
|---|---|---|
| `Shape.Members` | what the declaration wrote | emission and publication |
| `TryLookupMembers` / `TryLookupMemberByKey` | the inherited closure, flattened by `stack` | all of resolution |

Writing the flattened set onto `Shape` is excluded: codegen reads `TryLookupType` to emit a type's
OWN rows (`ClrEnv.fs:442`, `:455`, `:496`, `:516`, `CodegenTypes.fs:39`, `JsExternalMembers.fs:58`,
`:68`, `:115`, `:161`), and an inherited member reaching those sites would be emitted as the type's
own.

`ResolvedType` carries no `IsValueType` field either: `IPlatformFacts` settles layout, and
re-admitting it here would rebuild the ladder that was removed.

## Steps

### 1. Spell the transform once

`stack`, `mapProviderTypes` and `withInlineBodies` each apply one conceptual rewrite to two
surfaces. Introduce a single transform record and derive both surfaces from it, so
`ScopeContents.decorate` is called in one place rather than three, and `withInlineBodies` cannot
stamp one surface and not the other.

Fix the `withInlineBodies` divergence here, as its own change with a test that reaches a member
through `TryLookupType` and through `TryLookupMemberByKey` and asserts the same `InlineBody`.

No fold of the interface yet. This step is about there being one place a rewrite is written.

### 2. `ResolvedType`, then the sources

Introduce the type and `ExternalSymbols.members` / `.indexSignatures` selectors. Add
`TryResolveType` to `IExternalSymbolStore` as a DEFAULT over `TryLookupType` +
`TryLookupIndexSignature`, so every provider keeps compiling; move consumers onto it one at a
time; then implement it natively at each source and delete the two folded channels.

Construction sites: `JsNativeSymbols.fs:109`, `PublishedSurface.fs:444`, `FrozenSignature.fs:398`
(all through `ofKeyIndexedChannels`, `:65`), plus `MetadataSymbolProvider` (`MetadataSymbols.fs:738`)
and `TsManifestProvider.IndexSignatures` (`TsManifestProvider.fs:84`), which implement the
interface directly. The contract sources already hold whole-type dictionaries
(`KeyIndexedChannels.ShapesByKey` / `.IndexSignaturesByKey`, `:33`), so for them this is a join.

`TsManifestProvider.IndexSignatures` should disappear outright: it is a `ProviderDecorator`
subclass overriding exactly one channel, and once that channel is a field of `ResolvedType` it
becomes a value the manifest publication fills.

Re-point `TypeLayout.declaredOf`'s external rung (`TypeLayout.fs:135`) to one `TryResolveType`
read.

### 3. One flattening, in the composite

Three commits. The interface narrowing and the flattening are separately reversible, and the
deletions need the flattening green in front of them.

`ResolvedType` does not change shape here. Step 2's design stands, and the member channels stay
channels, per the two-surface rule above.

#### 3a. `FrozenInterfaces` becomes the DECLARED set at every source

The field means different things per source today. `MetadataSymbols.buildClassInterfaces` (`:464`)
fills it from `t.GetInterfaces()`, which is TRANSITIVE; contract and TS sources publish the declared
clause list. `checkCapabilityInterfaceCollisions` (`Unification.fs:437`) already carries the
reconciliation and states the split outright at `:448`: *"Metadata `FrozenInterfaces` is already
transitive; the walk is what makes a contract-layer provider, which records only direct bases,
agree."*

`FrozenInterfaces` becomes the interfaces a type's own declaration names. An inherited interface is
reached by resolving a `FrozenNominal.Key` through the provider, on demand. `closeOver`
(`Unification.fs:450`) is already that walk; it becomes the shared mechanism and its comment loses
the metadata caveat.

Narrowing the metadata source is a set subtraction — `t.GetInterfaces()` less the base's and less
each listed interface's own — because reflection publishes no declared-interface API. It drops a
redundantly-declared interface (a class naming both `IEnumerable<T>` and `IEnumerable`), which a
closure-based consumer cannot observe.

`ClrRecipes.tryExternalInterfaceWitness` (`:222`) is the consumer that breaks. It picks a witness
out of `FrozenInterfaces` and is documented *"Direct-declared interfaces only"*, so a class
implementing `seq<'T>` through its base stops matching, and it takes the on-demand closure.

#### 3b. The flattening, in `stack`

`stack`'s `providers` array IS the file visibility stack, nearest first: `analyseUnits` conses each
unit's view as it publishes (`own <- pushed :: own`, `AssemblyAnalysis.fs:385`), `visibility`
(`:56`) lays that list over the reference floor, and `composite` is `stack` (`:340`). The floor's
`prelude` (`:52`) is `stack ValueNone preludeNamespaces []` over an EMPTY provider list: it serves
no symbols and contributes only ambient prefixes, so it is inert to this walk and its position in
the tail carries no shadowing. Vesper.Core is a referenced package inside `external`, not the
prelude. A type
declared in the file at index `i` inherits only from types declared at index `i` or higher, since F#
rejects a type reference forward across files. So the walk scans the INCLUSIVE SUFFIX
`providers.[i..]`, re-enters nothing at the composed top, and terminates on the illegality of an
inheritance cycle rather than on a `seen` set. `stack`'s channels are already index loops over that
array (`:284`, `:291`), so this is a start index rather than a new traversal.

`foldIntrinsicSurface` (`:206`) is the precedent: a cross-provider fold at one key, already here.

A CLASS walks `FrozenBaseType`; an INTERFACE walks `FrozenInterfaces` transitively, per 3a.
Most-derived first, overrides deduped by `(ArgSig, Kind, MethodTyparArity)`, re-sorted
most-params-first so `[0]` is the widest overload — `MetadataSymbols`' contract, kept because
`TryLookupMember` returns the HEAD of the set and 27 call sites are written against that pick.

Two behaviours carry over from `computeMembers`, each with a test: a `.ctor` is not inherited
(`:648`), and a property found at a level wins as a singleton and stops the walk (`LevelHit.Owns`,
`:609`).

**Depends on step 0.** A class does not walk its own `FrozenInterfaces`, so the metadata and
contract readings of a Vesper assembly agree only once a user interface member is emitted private
and renamed.

**Re-basing is the load-bearing part.** An inherited member's `Signature` is written over the BASE's
declaring typars, and a caller opens it with the DERIVED type's args (`Subsume.fs:83`,
`Unification.fs:371`). The metadata source is correct today only because reflection substitutes
eagerly: `commonOf st` (`:553`) runs on the CONSTRUCTED `t.BaseType`, so a `Base<string>` member
already reads `string`. A `FrozenNominal` walk has no such substitution, which is exactly why
`tryExternalInheritedMember` returns `struct (ExternalMember * EqArray<SemType>)` and
`InferRecordAccess.fs:346` commits at the supertype's args instead.

So `stack` substitutes `FTTypar(Declaring, i) := <the base nominal's args>`, composed down the
chain, onto each inherited member's `Signature`. Every existing call site then stays correct
unchanged. `FrozenTypeBridge.substituteDeclaring` (`:228`) is the primitive, but it `failwithf`s on
`FTTypar(Method, j)` and a member signature carries method typars, so it needs a variant that
substitutes the declaring axis and leaves the method axis alone.

A flattened member's `Key`, and so `Key.Decl`, stays the type that DECLARES it, because CLR
member-ref parenting needs the declarer. `Signature` and `Key` therefore sit on different declaring
axes; pin that with a test, since nothing in the type enforces it.

Re-basing also sharpens the dedup: a `Derived` overriding `Base<int>.Foo(T)` compares `Foo(int)`
against `Foo(int)` and collapses, where the un-rebased templates would not.

**The three resolution sites move onto the channel**, per the two-surface rule:

| site | today | after |
|---|---|---|
| `Subsume.groundMemberNames` (`:57`), backing `keyof` | `Shape.Members`, declared | `TryLookupMembers`, flattened |
| `Engine.tryStructuralWiden` (`:176`) | the interface's `Shape.Members` | flattened |
| `Unification.checkInterfaceConformance` (`:363`) | the interface's `Shape.Members` | flattened |

Each is a behaviour change, and each gets a test:

- `keyof` gains inherited names, `System.Object`'s included, and `T[K]` resolves every one of them.
  The whole surface, unfiltered: `keyof` and `T[K]` answering over the same set is the defect this
  closes, and a filter on one of them reopens it.
- A record widens to an external interface only by supplying the inherited members too.
- Implementing `enumerator` starts requiring `Dispose`. Probe `dotnet fsi` for F#'s exact behaviour
  and FS code before writing this one.

Expect currently-green SemanticAnalysis tests to go red here. A test that passed because
conformance ignored an inherited requirement was pinning the defect.

#### 3c. Delete the other two flattenings

`MetadataSymbols.computeMembers` (`:544`) and `membersCache` (`:280`) — the second reflection pass —
and `EngineCore.tryExternalInheritedMember` (`:616`) with the fallback arm that calls it
(`InferRecordAccess.fs:344-349`). `classChainLevels` (`:228`) stays: a project-local type is not in
the provider, so nothing else can answer for it.

Land 3b with both still in place, then delete and confirm the tests that covered them stay green. A
red one means the flattening is narrower than the walk was, and the difference is the finding.

The cost lands here: reflection's cached chain walk becomes one `TryLookupType` per base level per
lookup, against `PassContext`'s per-file memo (`:316`). Measure it in this commit, where it is
isolated, rather than inside 3b.

### 4. Fold the transforms into `stack`

`stack` gains the step-1 transform as a parameter (its `stampHome` becoming one instance) and
`mapProviderTypes` / `withInlineBodies` become the functions passed in. `memoize`'s type dictionary
becomes `TypeKey -> ResolvedType voption`, beside the member, symbol and record-field dictionaries
the surviving channels still need, and may collapse into `stack` outright. `ProviderDecorator` is
deleted.

Call sites to re-point: `NumberCovariance.fs:48`, `AssemblyAnalysis.fs:226`, `:374`,
`PackageProviders.fs:298-301`, `PassContext.fs:316`, `TsManifestProvider.fs:227`.

## Settled semantics — member resolution does not depend on the source

**A member resolves identically whether its declaring type came from a `.fsi` contract or from
reference-assembly metadata.** A `.fsi`-declared class and a BCL class take the same path.

The single exception: where F# semantics are NARROWER than .NET, and the resolution order is
provably equivalent after optimisation. Narrowness is a property of the language, so it applies to
every source alike; it is not licence for one source to answer differently from another.

### Interface members are hidden by IL VISIBILITY, not by a resolution rule

F# compiles `interface I with member …` to a PRIVATE method carrying a `.override` (MethodImpl)
row, so `Explicit().Dispose()` is `FS0039`. Nothing in the front end enforces that: the method is
`private`, and `MetadataSymbols.declaredFlags` (`:283`) is
`Public ||| Static ||| Instance ||| DeclaredOnly` with no `NonPublic`, so reflection never returns
it. C# implicit implementations are public members of the class and appear through the base chain
like any other. Either way the visibility filter carries the rule, and the shape of the walk does
not need to encode it.

**But Vesper's CLR backend does not emit it that way.** `ifaceEqualsAttrs` (`LayoutModel.fs:41-46`)
is `Public ||| Virtual ||| HideBySig ||| NewSlot ||| Final`, applied to every user
`interface … with` member via `memberRow` (`LayoutNodes.fs:129`), bound to the `InterfaceImpl` row
by name and signature. No `AddMethodImplementation` wrapper exists in `Metadata.fs`, and no
MethodImpl row is emitted anywhere in the backend.

So for a VESPER-compiled assembly the interface member is public on the class, and the two
readings of that one assembly disagree:

| the same Vesper assembly, read as… | `x.Dispose()` where `x : C` and `C` declares `interface disposable` |
|---|---|
| `.fsi` contract | not on the class shape; `Dispose` sits under `FrozenInterfaces` |
| reference-assembly metadata | public on the class, returned by the `BindingFlags.Public` walk |

That is precisely the source-dependence this section forbids, and it is an EMISSION fact, not a
resolution fact — so no flattening rule in `stack` can fix it. See the emission fork below.

`dotnet fsi` is the oracle for the rest of the narrowing surface; probe it per case rather than
deriving a rule.

### DECIDED — emit explicit interface implementations

A user `interface … with` member is always an EXPLICIT interface implementation, equivalent to
C#'s. Vesper matches F#: the method is `private hidebysig newslot virtual`, named
`<Interface>.<Member>`, and carries a `.override` (MethodImpl) row naming the interface slot.

This makes the two readings of a Vesper assembly agree by construction — the visibility filter
excludes the member from the metadata reading exactly as `FrozenInterfaces` excludes it from the
contract reading — so `stack` needs no interface rule: a class walks its base chain, an interface
walks its base interfaces, and nothing else.

It also settles the collision. A class declaring BOTH its own member and an interface member of
the same name emits two rows that cannot conflict, the explicit one delegating to the public one:

```
.method public hidebysig instance void Dispose () cil managed
.method private hidebysig newslot virtual instance void System.IDisposable.Dispose () cil managed
{
    .override method instance void [System.Runtime]System.IDisposable::Dispose()
}
```

**Synthesised structural members are NOT explicit and stay public.** Reflecting an F# record
(`MyRec`) shows its generated `Equals(T)` / `CompareTo` / `GetHashCode` all `public`, while a
user-written `interface IEquatable<Custom> with` member is emitted as
`private System.IEquatable<Custom>.Equals`, `newslot`. So `ifaceEqualsAttrs` splits in two: the
synthesised rows keep it, the user rows take the new explicit set.

### Step 0 — the emission change (prerequisite for step 3)

Scoped, and smaller than it looks because the flag is already threaded to the right place:

- **Split the attribute set.** `ifaceEqualsAttrs` (`LayoutModel.fs:41`) serves ONE user arm —
  `memberRow`'s `isIfaceImpl` branch (`LayoutNodes.fs:134`) — and five synthesised rows:
  `EqEqualsTyped` (`:170`), `CmpCompareToTyped` (`:183`), `CmpCompareToObj` (`:188`), `FmtFormat`
  (`:207`) and `CapCoSlot` (`:223`). Add `explicitIfaceImplAttrs` =
  `Private ||| Virtual ||| HideBySig ||| NewSlot` for the user arm alone. `invokeAttrs`
  (`LayoutModel.fs:63`) is a closure's own calling convention and does not change.

- **OPEN — do capability co-slots go explicit too?** `coSlotRows` (`LayoutNodes.fs:214`) is
  synthesised, but it implements a BCL interface member (`IEnumerator.MoveNext` under a user
  `interface enumerator<'T> with`), not a structural augmentation. Left public it reintroduces
  exactly the divergence step 0 closes: a metadata read of the Vesper assembly finds `MoveNext`
  public on the class, a contract read does not. The structural-members carve-out below is
  evidenced for `Equals`/`CompareTo`/`GetHashCode` and does not obviously reach this. Probe
  `dotnet fsi` for how F# emits the inherited non-generic slot and settle it before writing the
  split.
- **Qualify the name.** `memberMetaName` (`LayoutModel.fs:70`) takes the member alone and cannot
  spell `System.IDisposable.Dispose`. `memberRow` (`LayoutNodes.fs:129`) should take the
  implementing `FrozenNominal` in place of `isIfaceImpl: bool` — `NominalMembers.indexed`
  (`CodegenTypes.fs:180`) already pairs each member with its interface, so the fact is available
  and the `bool` is a discarded intermediate.
- **Emit the MethodImpl row.** `Metadata.fs` has no `AddMethodImplementation` wrapper; add one
  beside `AddInterfaceImplementation` (`:254`). The `decl` operand is a member ref on the
  interface, whose type handle `ClrProvider.fs:147` already resolves.
- **Fix the bare-name index.** `NominalEmit.fs:55-79` builds `Dictionary<string, EqArray<EmittedMember>>`
  keyed by `mem.Name` over `members @ flattenIfaceMembers …`, so an interface-impl member is
  reachable by BARE name off the class. Renaming only `MetaName` would leave the index finding it
  and minting a ref to a now-private method — invalid IL, and silent at emission. Key the explicit
  members by their qualified name, or keep them out of this index entirely.
- **Delete the ordering tie-break.** The same site orders own members before interface-impl ones
  *"so a same-signature pair (`Set.Add : Set<'T>` vs `ICollection<'T>.Add : unit`) resolves to the
  class's own member on a tie"*. With explicit implementations the two rows have different names
  and no tie exists. The hack is a consequence of the shared name, so it goes with it.

### The dispatch enumeration (done)

Four sites resolve a member by bare name on a local nominal. One breaks.

**`EmitBindings.emitLocalDispose` (`:95`, reached from `:157`) — BREAKS.** For a `use` binding on a
local type, `Disposal.ViaCapability` SYNTHESISES the key
`SymbolKeyOps.memberKey n.Key "Dispose" …` and mints a `MethodCall` with `CallVia.Self`, which
routes to `resolveInstanceMember env <class> "Dispose" []`. For a Vesper class that `Dispose`
exists ONLY as an interface impl, so the call lands on the row step 0 makes private. Its own
comment states the assumption step 0 invalidates: *"A LOCAL capability impl disposes through its
own `Dispose` method."* Under step 0 the local path must dispose through the interface slot like
the external path already does (`emitExternalDispose`, `:115`) — a `callvirt` on the capability
slot, `constrained.` for a struct.

**`EmitMember.buildMethodCall` (`:231`) / `buildPropertyGet` (`:211`) — safe.** The name comes from
a front-end-resolved key, and the front end cannot resolve an interface-impl member on a class:
`ClassTypeInfo.InterfaceImpls` (`TypeInfos.fs:479`) is a separate array from `Members` (`:447`),
and `classChainLevels` (`EngineCore.fs:228`) filters `info.Members` alone. `EmitBindings` breaks
precisely because it bypasses this by synthesising a key.

**`EmitLoops.fs:253`, `:265` — safe, worth pinning.** `ForInGetEnumG.Local` is the duck-typed
path, for a source exposing `GetEnumerator`/`MoveNext`/`Current` *without* implementing the
interface (comment at `:242`). An implementer takes `ConstrainedInterface`
(`InferControlFlow.fs:545`) or `External`. Add a test asserting a type implementing `seq` never
takes the `Local` arm, since nothing enforces it structurally today.

**`Assembler.PrepareInterfaces` (`:619`) — unaffected.** It indexes an INTERFACE's own abstract
slots, which are already the `callvirt` targets.

The JS backend is untouched: MethodImpl is a CLR concept and step 0 changes no front-end
semantics.

A negative control belongs here: disable the MethodImpl emission and confirm the verification
failure returns, so the row is shown to be load-bearing rather than incidental.

`CodegenSymbols.TryRebaseCapabilityMember` (`CodegenSymbols.fs:41`) is the corollary at the
backend: a `MemberKey.Decl` is NOT always the true declaring type (`enumerator<'T>.MoveNext` is
stamped on the derived interface, declared on the non-generic `IEnumerator`), and a member-ref
parented on the wrong one faults at runtime. Once `stack` flattens, a flattened entry keeps the
`declKey` of the level it was found at — `computeMembers` already does this, via `commonOf st`
(`MetadataSymbols.fs:553`) — so the rebase becomes a lookup rather than a repair, and is a
candidate for deletion in its own right. Not in scope here; note it and move on.

## Settled design rules

1. **A transform CONSTRUCTS a `ResolvedType`; it does not `{ r with … }` it.** Record
   copy-and-update forwards unstated fields exactly the way the decorator defaults do — it would
   move the hazard, not delete it. A variance mapper that spells every field turns "a new fact
   escaped mapping" into a compile error in the one place that matters. This is the whole reason
   the redesign beats patching `ProviderDecorator`; do not give it up for brevity. `mapShape`
   (`:377`) already spells every arm of `ExternalTypeShape` and is the model.

2. **`TryLookupByKey` (bindings) stays as it is.** It already returns one whole `ExternalSymbol`
   from one fetch. A matching `ResolvedSymbol` is optional symmetry and NOT in scope.

3. **No `Lazy` fields on `ResolvedType` up front.** Both sources that matter already resolve
   eagerly. Add per-field laziness only when a specific source is shown to need it.

4. **A shape states a declaration; the member channels state the closure.** Resolution reads the
   channels, emission and publication read `Shape.Members`, and neither set is written into the
   other. A resolution site reaching for `(|ExternalMembers|_|)` after step 3b is the smell, and
   `FrozenInterfaces` follows the same rule: the declared clause list, closed over on demand.

5. **`IScopeContents` is derived, never decorated in parallel.** The draft's rule was
   "`IExternalSymbolResolver` is untouched", written before `Scope` existed. `Scope` is now the
   second home this plan is about, so the rule inverts: after step 1, a wrapper that calls
   `ScopeContents.decorate` directly is the smell.

## Constraints

- **Three test files build mock providers as `{ KeyIndexedChannels.empty with … }`** —
  `KeyIndexedChannelsTests.fs`, `SignatureResolutionTests.fs:1116`,
  `SubtypeExternalInterfaceKeyTests.fs:47`. `KeyIndexedChannels` is the ergonomic surface,
  `IExternalSymbolStore` the contract, and only the latter changes shape: keep
  `KeyIndexedChannels` field-per-channel and derive `TryResolveType` in `ofKeyIndexedChannels`.
  (The draft's "17 test files / `NamedChannels`" constraint is void — `NamedChannels` was deleted.)
- **This is a re-indexing, not new eagerness.** If any step makes a SOURCE resolve more up front
  than it resolves today, that step is wrong. Step 3c is the one deliberate movement of work: a
  per-lookup base walk in `stack` replaces a cached reflection pass, lazily and behind the same
  memo, and it is measured in its own commit.
- `MapProviderTypesTests` is the regression gate for rule 1: it plants a marker in every position
  a provider puts a type and asserts which come back as a witness of the variance they were mapped
  at. It must keep passing channel-for-channel, and gains a `Scope` arm at step 1.

## Not in scope

- `IntrinsicTypeMap` as a provider-level channel. The canon↔repr edge is walked at ~6 independent
  sites; resolving a canon THROUGH its repr once is a natural consequence of this plan but not a
  precondition for it.
- `ICodegenSymbols` (`ExternalSymbols.fs:453`). It is a narrowing view built by
  `CodegenSymbols.ofProvider`, so it re-points at step 2 and needs no design of its own.

## Anchors (verified 2026-08-30)

- The contract: `IScopeContents` (`ExternalSymbols.fs:79`), `IExternalSymbolResolver` (`:355`),
  `IPlatformFacts` (`:371`), `IExternalSymbolStore` (`:382`), `IExternalSymbolProvider` (`:414`),
  the `TryLookupMember` / `IsValueType` extensions (`:419-433`), `ICodegenSymbols` (`:453`).
- Scope machinery: `ScopeContents.composite` (`:183`), `.mapValues` (`:292`), `.decorate` (`:305`),
  `.memoize` (`:327`); `ExternalSymbols.memberByKey` (`:481`), `(|ExternalMembers|_|)` (`:487`),
  `(|ExternalInterfaceMembers|_|)` (`:496`).
- The wrappers: `ProviderDecorator` (`ExternalSymbolProviders.fs:127`), `stack` (`:185`),
  `composite` (`:337`), `mapProviderTypes` (`:346`), `withInlineBodies` (`:458`), `memoize`
  (`:491`), `tryInlineBody` (`:550`); the four hand-written scope decorations at `:263`, `:430`,
  `:472`, `:521`.
- Channel construction: `KeyIndexedChannels` (`:33`), `ofKeyIndexedChannels` (`:65`),
  `nullProvider` (`:123`).
- The metadata source: `typeCache` / `membersCache` (`MetadataSymbols.fs:277`, `:280`),
  `declaredFlags` (`:283`), `enumerateClassMembers` (`:397`, declared-only), `computeType` (`:514`),
  `computeMembers` (`:544`, base-chain walk), the store implementation (`:738`), `IPlatformFacts`
  (`:777`).
- The three flattenings: `EngineCore.classChainLevels` (`:228`, project-local),
  `MetadataSymbols.computeMembers` (`:544`, provider-side), `EngineCore.tryExternalInheritedMember`
  (`:616`, call-site); the fallback ladder joining them at `InferRecordAccess.fs:340-349`.
- Interface-impl emission: `MethodAttrSets.ifaceEqualsAttrs` (`LayoutModel.fs:41`),
  `memberRow`'s `isIfaceImpl` arm (`LayoutNodes.fs:129`), the five synthesised rows (`:170`, `:183`,
  `:188`, `:207`, `:223`), `coSlotRows` (`:214`), `AddInterfaceImplementation` (`Metadata.fs:254`)
  — and NO MethodImpl emission anywhere in `Codegen.Clr`.
- Declared-only publication: `PublishedSurfaceBuilder.addTypeWith` (`PublishedSurface.fs:158`),
  `addMembers` (`:112`), `SignatureResolution.fs:478`, `FrozenSignature.fs:193`.
- The two routes read side by side: `Subsume.groundMemberNames` (`:47`, shape) and
  `groundMemberType` (`:71`, `TryLookupMember`).
- The file visibility stack: `AssemblyAnalysis.visibility` (`:56`), the nearest-first cons at
  `:385`, the symbol-free `prelude` (`:52`), `composite` = `stack` (`ExternalSymbolProviders.fs:340`),
  `foldIntrinsicSurface` (`:206`), the index loops at `:284` and `:291`.
- The interface split: `MetadataSymbols.buildClassInterfaces` (`:464`, transitive),
  `Unification.checkCapabilityInterfaceCollisions` (`:437`) with `closeOver` (`:450`) and its
  metadata caveat (`:448`), `ClrRecipes.tryExternalInterfaceWitness` (`:222`, direct-declared only).
- Re-basing: `FrozenTypeBridge.substituteDeclaring` (`:228`, `failwithf` on a method typar),
  `MetadataSymbols.commonOf` (`:553`) on the CONSTRUCTED base, the `.ctor` arm (`:648`) and
  `probeLevel` / `LevelHit.Owns` (`:609`).
- The resolution sites that move to the channel at 3b: `Subsume.groundMemberNames` (`:57`),
  `Engine.tryStructuralWiden` (`:176`), `Unification.checkInterfaceConformance` (`:363`) with its
  `openSignature` at `:371`.
- Shape readers that must keep the DECLARED list: `ClrEnv.fs:442`, `:455`, `:496`, `:516`,
  `CodegenTypes.fs:39`, `JsExternalMembers.fs:58`, `:68`, `:115`, `:161`.
- Layout: `ILayoutOracle` (`TypeLayout.fs:36`), `resolve` (`:108`), `declaredOf` (`:126`) with its
  external rung at `:135`.
- Composition sites: `AssemblyAnalysis.fs:52`, `:60`, `:159`, `:226`, `:374`;
  `PackageProviders.fs:125`, `:141`, `:178`, `:298-301`; `PassContext.fs:316`;
  `TsManifestProvider.fs:84`, `:224-227`; `NumberCovariance.fs:48`.
