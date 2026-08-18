# One front end for `.fsi`

**Status (2026-08-16): steps 1-4 landed; step 5 is independent and outstanding.** Spun out of
the `sig-only` discussion — this is the enabler the other three plans depend on. Delete when
step 5 lands (`feedback_plan_docs_ephemeral`).

Landing one step per session, in the order below. Each step names its own exit condition
and leaves the tree green; nothing here is meant to be handed to a subagent as bulk work.

## The gap: name resolution is implemented twice

A `.fs` goes through the real passes. A `.fsi` goes through `VesperLib.ExtractCtx`, which
resolves by **name string plus ambient prefixes** rather than by scope and `TypeKey`:

```fsharp
// AssemblyFiles.fs:231-233
let ctx = VesperLib.ExtractCtx.empty scope.Target
ctx.AmbientShapes <- (fun name -> scope.Visible.TryLookupType name |> ExternalSymbols.typeShapeOf)
ctx.DependencyAmbientPrefixes <- scope.Visible.AmbientOpenPrefixes
```

`ReferencedProject.fs:390-394` sets up the same thing for the other caller. An over-wide
lookup keyed on a written name IS a string key (`feedback_overwide_types_are_string_keys`);
the sink to narrow is `ExtractCtx`, not each call site.

The real passes already hold the key-based equivalents: `TypeRefStamp.fs:83`
(`tryResolveExternalTypeKey`), `:164` (`tryResolveAttributeTypeKey`), `:211`
(`resolveAttributes`), and `Unification/Translate.fs:154` (`translateType`), which together
replace `TypeTranslate.fs`'s `resolveTypeName` (`:176`) outright.

**Feasibility, checked.** The resolver half of `PassContext` does not depend on `Desugar`:
`UseSiteAt` (`PassContext.fs:472-477`) reads only walk-maintained resolution state, and
`Resolver` (`:255`) is the provider itself. So a walk over the `.fsi`'s CST, on a
`PassContext` built from the `.fsi`'s own `OriginSource`, can drive the real resolver with no
pass running first.

The two are a matched pair, not interchangeable: `ParseChain.ParsedSignature` carries the CST
that gets walked and the `Lexed` every `SyntaxToken` in it indexes into. The context needs the `Lexed` because `NodeKey`s derive from token
offsets and identifier text is read out of it. `analyseSignature` already mints that
`OriginSource` (`AssemblyFiles.fs:329`), today only to anchor the signature's diagnostics.

**One context per half.** `NodeKey` offsets are per-file and nothing `NodeKey`-keyed is
merged across files (`AssemblyFiles.fs:7-9`), so a paired unit has two contexts and the
signature's side tables cannot be read against the implementation's. The two halves meet at
`PublishedSurface` below, which is keyed by identity rather than by position.

## Correction: `AttributeDecode` is NOT the `.fsi` extractor's wart

`AttributeDecode.fs`'s header used to claim the cause:

> Class-shaping attributes match on the long-ident's LAST SEGMENT, not on a resolved
> `TypeKey`: the `.fsi` extractor caller has no resolver.

That was false as a diagnosis. The **implementation** side calls it too, with a resolver in
hand: `TypeRegistration.fs:237` (`IsValueType`), `:559` and `:702`
(`RequireQualifiedAccess`), and `MemberRegistration.fs:523`. So short-name matching for
`Struct` / `Sealed` / `IsByRefLike` / `RequireQualifiedAccess` / `AllowNullLiteral` is an
independent wart on both sides, and merging the front ends did not delete it — it only removed
its excuse, and step 4 rewrote the header to say what the file does rather than why. Step 4
also moved `tryCompiledName` / `hasModuleSuffix` / `isAutoOpen` in, since they are the same
short-name matching and had no other home once `VesperLib` went.

It also is not free to delete. `RuntimeNames.fs:109-121` gives key-based resolution ten
attributes, and of the five above only `AllowNullLiteral` is among them — the only one the
contract declares (`Vesper.Core/compiler-attributes.fsi:103`). The other four must be
declared in the contract and given keys before the lists can go. That is step 5, and it is
independent of steps 1-4.

## Expected dataflow (user, 2026-08-15)

Five files in compile order — `a.fsi`, `a.fs`, `b.fs`, `c.fsi`, `c.fs`:

- `a` and `c` pair; `b` does not.
- `a.fsi` and `a.fs` are both checked against the target provider.
- `a.fs` is turned into signatures + bodies.
- `a.fs`'s signatures are checked against `a.fsi`.
- The provider stacks with **`a.fsi`'s signatures and `a.fs`'s bodies**.
- `b.fs` is checked against the stacked provider, turned into signatures + bodies; no `b.fsi`,
  so no conformance check.
- The provider stacks with `b.fs`'s signatures + bodies.
- `c.fsi` and `c.fs` are checked against the stacked provider; `c.fs` → signatures + bodies;
  `c.fs`'s signatures checked against `c.fsi`.
- Codegen.

The publication half of this is already what the code does — `AssemblyFiles.fs:383-392`
("the `.fs`-derived signatures exist only to be checked against the published ones and
discarded; a `.fsi`'s are what survive") and `:399`
(`View = withInlineBodies bodies signatures`). What changes is that the `.fsi` half is
RESOLVED against the stack rather than extracted beside it.

## Decisions

### One resolver, two grammar walks

The signature grammar is a separate tree (`Signatures.fs`: `ModuleSignatureElement`,
`TypeSignature`, `ValSig`) and carries forms the implementation grammar has no spelling for
— `TypeSignature.Extern`, and the capability-interface / heritable-primitive republication
at `VesperLib.fs:396-454`. Two walks are therefore inherent. What is singular is the
resolver: `OpenScope`, `TypeRegistry` registration, `translateType`, attribute resolution by
`TypeKey`.

The payloads already coincide — sig-side `Record` / `Union` / `Enum` hold the same
`RecordFields<'T>` / `UnionTypeCases<'T>` / `EnumTypeCases<'T>` the implementation side does
— so registration can be narrowed to take payloads and be called from both grammars rather
than duplicated. That is step 2.

### Both halves produce the same type, and it is not `FrozenPools`

The two sides already converge, at the last possible moment, on the same thing:

- `FrozenSignature.toSignatures` (`:37`) builds `shapesByKey` / `membersByKey` /
  `typesByName` / `unionCaseIndex` / `recordFieldIndex` / `symbols` / `moduleContainers`
  (`:53-71`) and hands them to `KeyIndexedChannels` (`:453-478`).
- `ExtractCtx.toProvider` (`TyparCapture.fs:192`) builds the same vocabulary string-keyed
  (`:107-136`), converts it to `TypeKey`s through `ctx.TypeKeys` (`:271-280`), and hands it
  to the same `KeyIndexedChannels` (`:286-309`).

So the merge is: **name that table set, TypeKey-keyed, and have both sides fill it.**
`ExternalSymbolProviders.fs:63-78` already documents the distinction this turns on —
`KeyIndexedChannels` "HOLD their types' identities" where `NamedChannels` treat a name as
the identity (`:31-33`). The `.fsi` side being string-keyed *internally* is the whole bug;
filling a TypeKey-keyed accumulator directly is the fix.

The name is **`PublishedSurface`** — "publishes" and "surface" are both established here
(`feedback_reuse_established_verb`), and `FrozenFile.View` is already documented as what later
files resolve a file through. It names a PAIR: `PublishedSurfaceBuilder`, the mutable tables a
producer fills, and `PublishedSurface`, the frozen value that crosses a file boundary. Only
the builder exists so far; the caching section below is why the split is spelled from the
start.

`FrozenPools` is the wrong shared type. `TDeclG` (`TastDecl.fs:62-64`) is `Let` / `Expression`
/ `Type`, and `Let` requires a `value: TExprG`. A `val f: int -> int` has no value, so a
`.fsi`-derived `FrozenPools` means a new body-less case in the core TAST that never reaches
codegen and that every consumer must nonetheless match. The shared type belongs one step
later, where both sides already are.

This also makes the conformance follow-up a comparison of two values of one named type
rather than a cross-representation reconciliation.

### `PublishedSurface` is headed for content-addressed memoization (user, 2026-08-15)

The goal it serves beyond this plan: key a downstream file on the surface its predecessors
published, so an edit confined to a non-`inline` body leaves the surface hash unmoved and
every later file cuts off early. Today's key folds source BYTES — `Hashing.fs:71-74` reads
the contents of every source a package names — which by construction cannot cut off; and the
cache is single-file (`ClrDriver.compileCachedWith` takes one `source`), so the multi-file
stack has no environment in its key at all.

That makes the frozen half a VALUE: immutable, key-ordered, structurally equatable, and
hashable by canonical serialization. Three things stand between the builder's tables and it:

- **Dictionary fields defeat derived equality.** `Dictionary` does not override `Equals`, so
  a record holding them has per-field REFERENCE equality — it compiles and always
  answers "different".
- **`Symbols` holds pool handles.** `ExternalSymbol.ValRepr` reaches `PatId =
  Handle<PatPoolId> = { Pool: PoolBuilder; Id }` (`TastPoolBuilder.fs:37`), a live mutable
  pool. The SMALLEST of the three, not the largest: `TastLower.externalValRepr:345-371`
  already mints a contract val's grouping from arities and types alone, into a private pool
  "reachable only through the handles it hands out. No token spells any of them." Both
  consumers read shape only — `JsFlatFns.externalGroups` takes `vr.Groups` to flatten tuples,
  `ClrRecipes:263` the group count and kinds. A published grouping is therefore
  `(typars, [GUnit | GSimple of ty | GTuple of width], resultTy)`, a flat value; the handles
  exist because `ArgGroupG` is shared with the implementation side, where patterns are real.
  **Unverified:** that the `.fs` half agrees — `FrozenSignature.valReprToDeclaring` copies
  real pattern trees, and what survives into the published grouping needs checking.
- **Order is not contractual.** A canonical digest needs key-ordered folding, as
  `Hashing.fs:53` already does for its input set.

The hash comes from `XxHash128` over a canonical serialization, never `GetHashCode`: .NET
randomizes string hash codes per process, so an on-disk cache keyed on it misses after every
restart. `FrozenCodec` is the precedent.

Paths are already fine: a surface carries relative ones (`OriginPath` = bucket name +
`AssemblyFileId.Relative`), so its hash is checkout-portable.

**Sequencing (user, 2026-08-15).** The consumer is a prototype outside this repo, and its
integration is not planned until after the retire-sig-only work (landed 2026-08-17: every
`.fsi` pairs, the `sig-only` schema is gone), whose attribute gate reaches back to this plan —
so the chain is this plan → [attribute-representation](attribute-representation-plan.md) →
integration, and the whole `.fsi` front end lands first.

**Constraint on step 3:** the new front end fills a mutable BUILDER and freezes it, as
`PoolBuilder` → `FrozenPools` already does. What must exist early is only the BOUNDARY —
producers touch the builder, consumers touch the frozen value — because with it in place,
turning the value's dictionaries into key-ordered arrays is an internal change to one type
rather than a change to every caller. The value representation itself can wait.

The shape stops moving at **step 4**, when both producers fill one surface. That, not step 3,
is the point the prototype can build against.

### Resolution first; conformance follows

`Conformance.fs:9-11` is explicit that today it checks "Presence, not signatures", over the
parsed CSTs. A resolved `.fsi` is what would catch a contract whose declared types disagree
with the compiler's. The instance that proved it was `printf.fsi`, which declared
`printf` / `printfn` with a `'State` / `'Residue` the front end's own table contradicted,
undetected for as long as the file existed; it has since been deleted rather than fixed.
Comparing two `PublishedSurface`s is its own design and is **not** in this plan.

### Accessibility: internal-or-better on both sides, filtered at the assembly boundary

Today the two sides disagree: `FrozenSignature.fs:44-49` keeps internal-or-better,
`VesperLib.fs:474` keeps public-only. Correct F# semantics is internal-or-better for both;
the `.fsi`'s job is that an implementation declaration the signature does not make becomes
inaccessible outside the file, which `AssemblyFiles.fs:383-392` already implements by
discarding the `.fs`-derived signatures wholesale.

So public-only is wrong for the in-assembly caller (an `internal` val in an in-assembly
`.fsi` is today invisible to later files of its own assembly) and right only at the
**cross-assembly** boundary, where it belongs to the consumer, not to extraction.

Unifying on internal-or-better is part of step 3. The boundary filter is a follow-up and is
not free: nothing published carries accessibility today — `toSignatures` reads
`frozen.Residue.Accessibility` and drops privates without recording the verdict, so a
public-only filter needs accessibility to survive onto the tables first. The exposure is
theoretical for now: no contract under `src/Vesper.*` declares an `internal` member.

### No constant folding in this plan

Attribute arguments are carried as written. Folding `AttributeTargets.Class ||| AttributeTargets.Struct`
matters only once conformance compares attribute arguments, and conformance is deferred
above; [attribute-representation-plan](attribute-representation-plan.md) is the consumer
that needs values rather than syntax.

The trade to settle when conformance gets there: structural expression equality needs no
folding but rejects `A ||| B` against `B ||| A`, and `1` against `0x1`, as mismatches.

**Premise to verify then, not now:** whether an attribute is expected on both halves of a
pair at all, or whether F# takes the signature's alone. Do not design the check before
checking that against fsc.

## Constraints the merged path must keep

- **Pre-scan order.** Both callers extract intrinsic reprs from the `.fs` BEFORE the `.fsi`,
  so `type t = extern` picks `IntrinsicPlatform.Repr` over `Unsupported` (`signatureView` and
  `companionReprs`, which are the same read spelled twice).
- **Declared order within a package.**
- **Homing stays a parameter.** The in-assembly caller homes `Origin.InFile` and publishes no
  ambient prefixes; the package caller homes `Origin.InAssembly` and publishes its
  `[<AutoOpen>]` prefixes.

All three are discharged by `AssemblyFiles.foldUnits`, the single fold both callers
(`PackageProviders.buildProviderSeeded` and `AssemblyFiles.analyseWith`) now use.

## Steps

### 1. Name the shared accumulator — LANDED

`PublishedSurface.fs` holds `PublishedSurfaceBuilder`, the table set both producers fill,
with one `toProvider`. `FrozenSignature.toSignatures` fills it directly; `ExtractCtx` gained
`toSurface`, which reads a finalized context out into one, and `toProvider` is now that
composed with `PublishedSurfaceBuilder.toProvider`. The two copies of the dotted-name resolver
collapsed into `PublishedSurfaceBuilder.tryTypeKey`, and `registerModuleContainer` into
`addModuleContainer`.

The file keeps the concept's name, not the builder's, because step 3 adds the frozen
`PublishedSurface` beside it. `toProvider` hangs off the builder only until then; it belongs
on the value.

`ExtractCtx.tryTypeKey` stays: `TypeTranslate` reads the live tables mid-extraction, which
is not what a finalized surface answers.

Pure refactor. Whole suite green with no test edits.

### 2. Narrow the registration entry points — LANDED

`TypeRegistration.register{Record,Union,Enum,Abbreviation}Decl` now take `TypeIdentity`, the
`TypeName`, and the payload — `RecordFields` / `UnionTypeCases` / `EnumTypeCases` / RHS `Type`
— all of which the two grammars spell identically. `MemberRegistration` destructures at the
call site, so the kind→CST pairing is one `match id.Kind, td` there rather than an internal
`| _ -> ()` in each of the four. The `*TypeDefn` suffix went with the parameter it named.

Two things fell out. `registerRecordDecl` no longer calls `isValueTypeDefn`: only the
`[<Struct>]` half applies to a record, so the attribute read is now `isStructAttributed`,
which `isValueTypeDefn` also calls — the two cannot drift. And an abbreviation's augmentation
reaches the registrar as `hasAugmentation: bool`, because only its presence was ever read and
the two grammars' extension types differ.

`registerAbbreviationDecl` still re-matches its RHS against `Type.ILIntrinsic` to pick the
side table, duplicating the verdict `TypeIdentity.Kind` already carries. Left alone —
[semantic-analysis-followups-plan](semantic-analysis-followups-plan.md) records it, and the
sig side has no `Type.ILIntrinsic` to hand over at all (`TypeSignature.Extern` is a case of
its own), so step 3 is what decides the split.

`registerClassTypeDefn` is untouched: the sig grammar's class-like cases carry
`TypeElementsSignature`, not `ObjectModelBody`, so there is no shared payload to narrow to.

Pure refactor. Whole suite green with no test edits.

### 3. The signature front end, wired to the in-assembly caller only — LANDED

`Passes/SignatureResolution.fs` walks the `.fsi` on a `PassContext` built from its own
`OriginSource` and fills a `PublishedSurfaceBuilder` that freezes to a `PublishedSurface`.
`signatureView` (`AssemblyFiles.fs`) runs it; `buildProviderWith` still runs `VesperLib`.

**ONE walk, over both grammars.** A pair's two halves key off the containment the walk
maintains, so any rule that differed between them would put one declaration in two places —
which is why there is one `CstModuleTree.walkTree` and not two copies held to agreeing. Each
grammar supplies a `ModuleNode` view (nested module / `open` / abbrev / pass-through) and a
`ModuleGroup` reading of the file header; `walkImpl` and `walkSig` are the two ~1-line
instantiations. `DeclContainment.Modules` narrowed from `ModuleDefn` to `DeclaredModule`
(attributes + ident), which is all `ModuleRules` ever read, so both grammars feed it.
`WalkedElem` is now `WalkedIn<'T, 'Elem>` with the signature element as the other instance.
`onScope` survives on `walkImplWith` for its one caller (`Validation`).

The `.fsi` front end walks the tree ONCE: the nominal-name pre-scan, the `[<AutoOpen>]`
prefixes (`ModuleRules.autoOpenContainers`, off the containment) and the registration all read
the one list, rather than each descending into nested modules again.

Claiming is shared outright: `claimTypeIdentity` split into `claimTypeName`, which takes the
`TypeName` and the kind, and `claimSigTypeIdentity` calls it. Detail registration goes through
step 2's entry points. A class-like signature registers nothing but its CLAIM —
`Translate.resolveClaimedType` builds a `TyClass` from the identity alone — so the sig grammar's
`TypeElementsSignature`, which has no implementation-side twin, is read only for the surface.

**One reading per declaration.** `sigDeclOf` narrows a `TypeSignature` to a `SigDecl` once —
the kind it claims and the syntax each phase reads are the same choice — so claiming,
registration and publication are total matches over it rather than over a `(kind, signature)`
pair whose halves cannot disagree but must still be spelled. That is also what caught the
divergence below.

**Three things the corpus taught, none of them guessable from the grammar:**

- A VALUE quantifies its own typars on the DECLARING axis (`instantiateSymbol` substitutes
  there); a MEMBER's own go on the method axis, its declaring type owning the other.
- A nominal class's members must NOT ride `ExternalClassShape.Members`. A shape's templates are
  instantiated on the declaring axis, so a member typar there faults. Only an interface carries
  them, which is what the `interface … with` conformance check reads.
- A signature's typars are quantified in translation order: the argument/result shape first,
  then the ones only a `when` clause's TARGET mentions (`'E` in `'S :> IStructSeq<'T,'E>`),
  which stand in no parameter and no result but are quantified all the same.

**Leniency kept, but not silent.** A MEMBER whose signature names a type this compilation
cannot resolve is dropped, refusals and all — `Vesper.List`'s `GetSlice` names `int option`
while its manifest depends only on `Vesper.Core`. `tryResolve` is where that happens, so the
tolerance is one named function rather than spread through the walk. The DROP is REPORTED, as
`ConformanceVerdict.SignatureNotPublished` (`V245`): what the signature promised is absent for
every later file, and discovering that as an unresolved name three files on is worse than a
warning here. That verdict is the one `Kind.Conformance` case at WARNING severity — a gap in
what this compiler models rather than a fault in the program — so `Vesper.List` still builds.
A VAL that fails is still an error, as it was. The `list.fsi` defect is real and now has a
name; fixing it is a contract change, not a front-end one.

`tryResolve` collects through `PassContext.Collecting`, which diverts a scope's diagnostics to
a buffer of its own. Splicing an index range back out of the shared log would have been sound
only while `f` was its only writer, and nothing said so.

`PublishedSurface` is a VALUE beside the builder: key-ordered `EqArray`s under an ordinal
rendering of each key, no `Dictionary` and no `ResizeArray`, with the lookup indexes derived in
`toProvider`. `KeyIndexedChannels.MembersByKey` narrowed to `EqArray` to keep a mutable out of
the seam. Both halves of the claim — the ordering, and equality over the same content published
in a different ORDER — are asserted in `PublishedSurfaceTests`, because no call site can see
either. Structural equality is NOT yet complete: an `ExternalSymbol.ValRepr` holds pool-relative
handles, a tuple group's carrying the live `PoolBuilder`, so `Symbols` compares by contents in
neither direction. That is the `ValRepr` item under the caching section, unchanged; the test
asserts it INVERTED, so the flat-parameter-grouping fix fails there rather than landing
unnoticed.

**Both halves publish through the same fill rules.** `PublishedSurfaceBuilder` owns
`addTypeName` / `addShape` / `addMembers` / `addRecordCandidate` / `addUnionCase`; first-wins on
a compiled name, first-wins on a bare case name, and the per-field candidate multimap are stated
once rather than once per producer. A shared accumulator whose two producers each brought their
own filling rules would not have been shared.

Factored out rather than copied: `IntrinsicReprs` (the `(# … #)` reader both front ends need,
now the registrar's and the conformance check's too, and the one thing step 4 must rehome rather
than delete), `SyntaxToken.nameIn` (the token→name read, previously spelled six times), and
`OperatorNames.ofDeclaredName` / `ExternalSignature.setter`, which `VesperLib` now calls.

**Typed where the choice was load-bearing.** `TyparOwner` (`Type` / `Member` / `Value`) is how
a producer names WHAT it is freezing, so the surprising rule — a VALUE's own typars go on the
declaring axis — is a case with its reason attached rather than a call indistinguishable from
the type one. `SigMemberForm` and `ExternForm` likewise replace boolean pairs that could each
represent a state with no meaning.

The front end is three files: `SignatureResolution/Context.fs` (what it resolves against, typar
scoping and the axis cut), `SignatureResolution/Members.fs` (the members and class-like body a
type declares), and `SignatureResolution.fs` (publication, groups, vals, the walk).

Whole suite green; three tests added, for the drop report, the surface value, and the `(# … #)`
signature binding.

### 4. The package caller becomes a per-file fold — LANDED

`PackageProviders.buildProviderWith` folds each `.fsi` over the provider stack — its
dependencies plus the package's own earlier files, nearest first — exactly as
`analyseAssemblyWith` does over `.fs` units. `VesperLib.fs`, `VesperLib/TypeTranslate.fs` and
`VesperLib/TyparCapture.fs` are gone; `VesperLib/Manifest.fs` survives as `PackageSource.fs`,
the one reader that turns a manifest-named path into a parsed tree.

**The provider half of `ReferencedProject` had to move.** Everything the front end needs sits
below `PassContext` in compile order, and `Hashing` / `ConformancePass` / `PackageUnits` sit
above it — but every one of those reads MANIFESTS only. So the split is by what a caller
wants: `ReferencedProject` resolves, parses and orders manifests where it always did, and
`PackageProviders` (after `AssemblyFiles`, whose `fileSource` / `anchorDiagnostics` it shares)
turns them into providers.

`buildProviderWith` takes a dependency PROVIDER rather than a name→shape function and a prefix
list, because that is what a `PassContext` resolves against. `BuiltPackage.Diagnostics` became
`AnchoredDiagnostic list`: what used to be file-level parse failures is now everything the
front end reports, positioned in the file that reported it.

**The prelude is a source.** Every contract is written against `RuntimeNames.preludeNamespaces`,
and the extractor served that with a name-keyed short-name index rather than a scope. The fold
puts a prefix-only provider at the floor instead, so a `.fsi` in `namespace Vesper.Collections`
names `unit` exactly as a consumer of the package would.

**Attribute reads outlived their module.** `tryCompiledName`, `hasModuleSuffix` and `isAutoOpen`
are short-name attribute matching, so they moved to `AttributeDecode` and take `nameOf` like
everything else there. That file's header claimed the `.fsi` extractor as the reason for
short-name matching; the reason is gone and the matching is not, which §"Correction" above
already recorded.

**Three defects the package corpus caught, all of them upstream of this step:**

- A CAPABILITY (`extern interface`) claimed `TypeDeclKind.IntrinsicRepr`, so its own file
  kinded it `TyConst` while every consumer read the published shape and kinded it `TyClass`.
  It now claims a CLASS: a capability is a nominal interface that only CARRIES a platform
  spelling. The repr decides the SHAPE (`IntrinsicInterface` with one, a plain interface
  `Class` without — which is what the JS build's canon-only capabilities are) and nothing else,
  so `bindExternRepr` no longer files a capability under an intrinsic identity it does not have.
- `IntrinsicTypeMap` was filled from every repr the file bound, which put capability interfaces
  on the intrinsic axis. It is now DERIVED in `PublishedSurface.ofBuilder` from the published
  `Intrinsic` shapes, so no producer can put one there. `PublishedSurfaceBuilder` lost the field.
- A union's `interface seq<'T>` was TRANSLATED outside its declaring typars, so `'T` froze to
  the unfreezable hole. `freezeInterfaces` now establishes the scope itself rather than
  trusting its caller to.

**Three fixture packages were under-declared.** `int` is Vesper.Core's, and a contract now
resolves only what its own `depends-on` closure declares; the extractor's short-name index hid
that. Each names the dependency now. A `depends-on` entry names a SIBLING package directory, so
a fixture that does not live beside `src/` spells the way there — see
[the follow-up below](#follow-ups-not-in-this-plan).

`SignatureExtractorTests` is `SignatureResolutionTests`, rewritten against `SignatureResolution.run`
and the surface it publishes. Two fixtures changed meaning rather than shape: the `[<RequireQualifiedAccess>]`
one now reads the flag off the published case index (there is no `RqaTypes` side table), and the
`ValRepr` one moved off a whole-file `module TestC` (see the module-chain follow-up below).

Whole corpus green: 1409 + 1527 + 695 + the rest.

### 5. Delete the attribute name lists (independent of 1-4)

Declare `SealedAttribute`, `StructAttribute`, `IsByRefLikeAttribute` and
`RequireQualifiedAccessAttribute` in `Vesper.Core/compiler-attributes.fsi`, add their keys to
`RuntimeNames.compilerAttributeKeys`, switch `TypeRegistration.fs:237,559,702` and
`MemberRegistration.fs:523` to `resolveAttributes`, and delete `AttributeDecode.fs`.

Runnable before step 1 if preferred; it is listed last only because it is the least coupled.

## Follow-ups, not in this plan

- Accessibility carried onto `PublishedSurface`, and a public-only filter at the
  cross-assembly boundary.
- Conformance over two `PublishedSurface`s instead of two CSTs, and the folding-vs-structural
  decision for attribute arguments.
- **`Vesper.List`'s `GetSlice` names a type its package cannot resolve.** `list.fsi`'s
  `GetSlice` writes `int option` while the manifest depends on `Vesper.Core` alone, so the
  member is dropped. Either the dependency is missing or the declaration does not belong in
  that contract. (The `ResizeArray` half of this item is FIXED: it moved to `list-bcl.clr.fsi`,
  a CLR-only signature file, because the RHS is a BCL type the js contract cannot name.)
- **`Vesper.Printf`'s `Formatter` constructors name `TextWriter` / `StringBuilder`**, the same
  BCL-only case as `ResizeArray` above, and are dropped from the contract for the same reason.
- **`module A.B.C` as a whole FILE loses its module.** `CstModuleTree.walkImpl` homes such a file's
  declarations in the global namespace with no module chain, and the signature walk mirrors it
  so a pair's halves agree. `VesperLib` honoured the chain, so the package path and the
  in-assembly path disagreed; no contract is written that way, and no test covered it.
- **A bodied signature's `inherit` clause crashes the pass on a name it already diagnosed.**
  `SignatureResolution/Members.fs` narrows `FrozenBaseType` to `FrozenNominal` through
  ``OfFrozen "an `inherit` clause"``, which `failwithf`s on anything that does not name a type
  constructor. An undefined name reaches it as one: `Unification/Translate.fs:65-69` reports
  `UndefinedType` and returns `TyUnknown`, and `freezeOver` carries that to `FTUnknown`. So the
  base type faults where the interface list beside it — same walk, same freeze, `TryOfFrozen` —
  drops silently, and a diagnosed source error becomes a compiler crash rather than a message.
  Settle whether the base type should degrade like the interfaces do, or whether an unresolved
  `inherit` should be a hard error raised as a diagnostic before the freeze ever sees it. The
  narrowing itself is wanted; only its behaviour on the diagnosed path is open. This is the
  recheck the code's own TODO deferred to the `.fsi` rebase, now answered: step 4 did NOT close
  it. See [inherit-interface-plan](inherit-interface-plan.md) for the neighbouring `inherit`
  defect.
- **The later-file resolution test lost its unresolved-name half.** `SignatureResolutionTests`'s
  "a signature naming a type declared in a LATER file does not resolve" now asserts only that
  `b.fsi` publishes its own type; the rewrite in step 4 dropped the check that `a.fsi`'s val
  survives carrying `TyUnknown` for the name it could not see. The within-file case
  ("a signature naming an out-of-scope type is reported, and the val still publishes") does
  assert the carried name, so the RETENTION rule itself is covered — what is not is that
  crossing a FILE boundary obeys it, which is the whole point of that fixture.
  Restore it against `symbolOf r "needsB"` and `instantiateSymbol`.
