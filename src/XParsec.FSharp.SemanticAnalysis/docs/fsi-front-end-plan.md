# One front end for `.fsi`

**Status (2026-08-15): revised.** Spun out of the `sig-only` discussion — this is the
enabler the other three plans depend on. Delete when it lands
(`feedback_plan_docs_ephemeral`).

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

The two are a matched pair, not interchangeable: `Pipeline.ParsedSignature`
(`Pipeline.fs:102-107`) carries the CST that gets walked and the `Lexed` every `SyntaxToken`
in it indexes into. The context needs the `Lexed` because `NodeKey`s derive from token
offsets and identifier text is read out of it. `analyseSignature` already mints that
`OriginSource` (`AssemblyFiles.fs:329`), today only to anchor the signature's diagnostics.

**One context per half.** `NodeKey` offsets are per-file and nothing `NodeKey`-keyed is
merged across files (`AssemblyFiles.fs:7-9`), so a paired unit has two contexts and the
signature's side tables cannot be read against the implementation's. The two halves meet at
`PublishedSurface` below, which is keyed by identity rather than by position.

## Correction: `AttributeDecode` is NOT the `.fsi` extractor's wart

`AttributeDecode.fs:6-7` claims the cause:

> Class-shaping attributes match on the long-ident's LAST SEGMENT, not on a resolved
> `TypeKey`: the `.fsi` extractor caller has no resolver.

That is false as a diagnosis. The **implementation** side calls it too, with a resolver in
hand: `TypeRegistration.fs:237` (`IsValueType`), `:559` and `:702`
(`RequireQualifiedAccess`), and `MemberRegistration.fs:523`. Only `VesperLib.fs:49` is the
extractor. So short-name matching for `Struct` / `Sealed` / `IsByRefLike` /
`RequireQualifiedAccess` / `AllowNullLiteral` is an independent wart on both sides, and
merging the front ends does not delete it — it only removes its excuse.

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
integration is not planned until after [retire-sig-only-plan](retire-sig-only-plan.md), whose
attribute gate reaches back to this plan — so the chain is this plan →
[attribute-representation](attribute-representation-plan.md) → retire-sig-only → integration,
and the whole `.fsi` front end lands first.

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
with the compiler's — see [printf-contract-plan](printf-contract-plan.md) for a live
instance. Comparing two `PublishedSurface`s is its own design and is **not** in this plan.

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
  so `type t = extern` picks `IntrinsicPlatform.Repr` over `Unsupported`
  (`AssemblyFiles.fs:235-237`, `ReferencedProject.fs:396-405`).
- **Declared order within a package** (`ReferencedProject.fs:407-408`).
- **Homing stays a parameter.** The in-assembly caller homes `Origin.InFile` and publishes no
  ambient prefixes; the package caller homes `Origin.InAssembly` and publishes its
  `[<AutoOpen>]` prefixes (`ReferencedProject.fs:416-421`).

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

### 2. Narrow the registration entry points

Refactor `TypeRegistration.register{Record,Union,Enum,Abbreviation}*` to take the payload
(`RecordFields` / `UnionTypeCases` / `EnumTypeCases` / RHS `Type`) plus `TypeIdentity`,
rather than the whole `TypeDefn`, so the signature grammar can call them.

Implementation side only, pure refactor. **Exit:** whole suite green, no test edits.

### 3. The signature front end, wired to the in-assembly caller only

New pass: walk the `.fsi`'s `ModuleSignatureElements` on a `PassContext` built from its own
`OriginSource`, maintaining `OpenScope` and the container chain as it descends, registering types
through step 2's entry points, resolving type references through `TypeRefStamp` and
`translateType`, and filling a `PublishedSurfaceBuilder` that freezes to a `PublishedSurface`.
Publish internal-or-better.

Wire into `signatureView` (`AssemblyFiles.fs:226`) alone. `buildProviderWith` keeps using
`VesperLib`; the two front ends coexist for one step.

**Exit:** in-assembly `.fsi` behaviour unchanged except for the accessibility widening;
`SignatureExtractorTests` still covers the package path through the old extractor.

### 4. The package caller becomes a per-file fold

Convert `buildProviderWith` (`ReferencedProject.fs:384`) to fold each `.fsi` over the
provider stack — its dependencies plus the package's own earlier files — matching what
`analyseAssemblyWith` does over `.fs` units (`AssemblyFiles.fs:412-417`).

This removes three forward-reference tolerances that the shared-`ExtractCtx` design allowed:
the whole-file nominal-name pre-scan and the scope-wide `collectOpens`
(`VesperLib.fs:1519-1544`), and cross-file visibility within a package. Top-down is the
correct semantics (`feedback_no_forward_references`), so contracts that relied on any of the
three get fixed as they surface — **stop and report if that turns out to be more than minor
reordering.**

Then delete `VesperLib.fs`, `VesperLib/TypeTranslate.fs`, `VesperLib/TyparCapture.fs`, and
with them `TypeTranslate.fs:165,170`'s short-name `AutoOpen` / `RequireQualifiedAccess`
matching. `SignatureExtractorTests` / `ReferencedProjectTests` are rewritten against the new
front end here.

**Exit:** whole corpus green with one front end.

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
