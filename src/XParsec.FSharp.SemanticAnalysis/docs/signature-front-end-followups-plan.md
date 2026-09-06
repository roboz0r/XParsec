# Signature front-end follow-ups — plan

Working document. Ephemeral: delete it when the items land.

The `.fsi` front end (one resolver driving both grammars, `PublishedSurface` filled by both
producers, the package caller a per-file fold) landed 2026-08-17 and its plan doc is deleted.
These are the follow-ups that outlived it and had no other pending home. Siblings that did:
attribute-argument conformance landed as `ConformanceSurface.checkTypes` / `checkValues`, the
bodied-signature
`inherit` crash is in [inherit-interface-plan](inherit-interface-plan.md), the two
dependency-closure contract drops are in [vesper-follow-ups-plan](vesper-follow-ups-plan.md)
B5, and the weakened later-file resolution test is
[semantic-analysis-tests-followups-plan](semantic-analysis-tests-followups-plan.md) A33.

## 1. Accessibility carried onto `PublishedSurface`, and a public-only filter at the cross-assembly boundary

Both producers publish internal-or-better, which is correct F# semantics for the in-assembly
consumer: the `.fsi`'s job is that an implementation declaration the signature does not make
becomes inaccessible outside the file, which `AssemblyFiles` implements by discarding the
`.fs`-derived signatures wholesale. Public-only belongs only at the **cross-assembly**
boundary, where it is the consumer's filter, not extraction's.

That filter is not free: nothing published carries accessibility today —
`FrozenSignature.toSurface` reads `frozen.Residue.Accessibility` (`FrozenSignature.fs:46`) and
drops privates without recording the verdict — so a public-only filter needs accessibility to
survive onto the tables first. The exposure is theoretical for now: no contract under
`src/Vesper.*` declares an `internal` member.

The conformance side is settled separately: a private implementation declaration behind a
public signature declaration is reported as missing by `ConformanceSurface`, because the
signatureless surface omits it (fsc's FS0034). Carrying accessibility onto the surface is this
item's job alone.

## 2. `module A.B.C` as a whole FILE loses its module

`CstModuleTree.walkImpl` homes such a file's declarations in the global namespace with no
module chain — `ModuleGroup`'s doc (`CstModuleTree.fs:125-126`) states it — and the signature
walk mirrors it so a pair's halves agree. The deleted `VesperLib` extractor honoured the
chain, so the package path and the in-assembly path used to disagree; the walk unified them on
the WRONG reading. No contract is written that way, and no test covers it.

## 3. A published `ValRepr` is not a value, so `PublishedSurface.Symbols` has reference equality

`PublishedSurface` is a value in every table but one: an `ExternalSymbol.ValRepr` holds
pool-relative handles, a tuple group's carrying the live `PoolBuilder`
(`PatId = Handle<PatPoolId>`, `TastPoolBuilder.fs`), so the same signature published twice is
two unequal values. `PublishedSurfaceTests` (`:77-103`) asserts this INVERTED — the fix flips
that test rather than landing unnoticed.

The fix is a flat published grouping. Both consumers read shape only —
`JsFlatFns.externalGroups` takes `vr.Groups` to flatten tuples, `ClrRecipes` the group count
and kinds — and `TastLower.externalValRepr` (`TastLower.fs:381`) already mints a contract
val's grouping from arities and types alone, into a private pool reachable only through the
handles it hands out. A published grouping is therefore
`(typars, [GUnit | GSimple of ty | GTuple of width], resultTy)`, a flat value; the handles
exist because `ArgGroupG` is shared with the implementation side, where patterns are real.

**Unverified premise:** that the `.fs` half agrees — `FrozenSignature.valReprToDeclaring`
(`FrozenSignature.fs:15`) copies real pattern trees, and what survives into the published
grouping needs checking.

## 4. The design-time firewall, once the surface is a value

Item 3 is what stands between `PublishedSurface` and structural hashing. Two consumers want a
hashable surface, on different axes:

- **Design-time early cutoff — in memory, structural hashing.** Key a downstream file on the
  surface its predecessors published, so an edit confined to a non-`inline` body leaves the
  surface unmoved and every later file cuts off early. `D:\roboz0r\merkle-dag`'s
  `Rule.firewall` is exactly this shape: identity is the content hash of the result, and
  propagation stops where the surface is unchanged. That library takes values inline and has
  no codecs, so this axis needs no canonical serialization — specifying a wire hash here is
  the category error the deleted compile cache made: it served
  this design-time incremental need with wire machinery (`flatten` → `compress` → store →
  `decompress` → `thaw`), so its key folded source bytes over the whole package closure and
  could never cut off early.
- **Cross-boundary transport — persisted, canonical serialization.** A surface read by a later
  process needs a canonical encoding; `FrozenCodec` is the precedent and likely mechanism,
  with `XxHash128` over the encoding rather than `GetHashCode` (.NET randomizes string hash
  codes per process). No wire consumer exists yet; `FrozenCodec` and `Compression` are dormant, not
  dead, the round-trip and byte-identity tests keeping the format shippable until one
  arrives. Reviving this axis re-introduces the per-file content stamp: a persisted tree
  thawed against a re-read file is exactly the case the deleted `LexedFiles.tokenAt` hash
  check covered, whereas in-process a file is lexed once and the `LexedFile` retained, so
  the check could not fire. Such a stamp must cover the determinants below.

Sequencing: Merkle.Dag is a prototype outside this repo, and its integration was sequenced
after the attribute-representation work, which landed 2026-08-28. The manifest-default
migration it also needed landed 2026-08-23, so the per-file published surface the firewall
keys on already exists on every path.

Paths are already fine on both axes: a surface carries relative ones (`AssemblyFilePath` =
bucket name + `AssemblyFileId.Relative`), so it is checkout-portable.

### Determinants of a compiled file

Captured from `HashingTests.fs` before it was cut down, because these are facts about the
compiler and not about the cache that once hashed them. Whatever computes an identity on either
axis — the firewall key, or a revived per-file stamp — must cover all of them. (What survives
of that file is `AssemblyFilePathTests.fs`, pinning that `AssemblyFilePath.ofText` is a
function of the text.)

- The home assembly name, and the backend target.
- The reference assemblies, by identity and IN ORDER — resolution is first-hit by name, so a
  reorder is a different environment. A named-but-absent reference differs from no reference.
- The package set, as a SET: order- and multiplicity-insensitive.
- The self package, distinct from the same package as a reference: inside its own compile a BCL
  signature presents that package's primitives.
- The compilation defines, and which symbol is defined — they pick the `#if` branch parsed.
- The transitive `depends-on` closure, not just the roots named.
- Per target: a body only the `js` manifest lists cannot move a `clr` consumer.
- Within a package: the manifest's file ORDER, every listed `.fsi` and `.fs`, inline bodies that
  splice into a consumer's tree, intrinsic-repr companions that decide what a primitive resolves
  to, and the distinction between an absent file and a present empty one.
