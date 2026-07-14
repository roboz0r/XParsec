# Code-quality review: `938dd9da34..5412fa9f`

Eight commits reshaping `SymbolKey` into a containment chain, re-keying the type
registry by `TypeKey`, and freezing the inline-body channel. 102 files, +4246/−2633.
Build green at review time.

Type registration is handled separately in
`src/XParsec.FSharp.SemanticAnalysis/docs/top-down-registration-plan.md`; the findings
below are what remains after that work. Delete this file once they are closed.

## Status

**Closed: 1, 2, 7, 9.** Four commits (`24455419`, `18679fb6`, `3ecc37c4`, `918d66c8`) closed
finding 1 at its root rather than at the symptom the finding described. Finding 1's own
prescription — key the provider stores by `SymbolKey` — is NOT what landed, and could not
be: ten call sites mint a store key from a bare compiled name with no assembly in hand, and
`MetadataSymbols` answers by reflection, which is name-addressed by nature. The gap closed
from the other side instead: the home assembly LEFT `SymbolKey` (it was never a
disambiguator — every store face already discarded it), the arity ENTERED it as an `int`
field (the `` `N `` mangling was a CLR-ism in a language-neutral identity), `simpleName`
now returns a `DisplayName` so a display string cannot be a route back to an identity, and
a referenced assembly already claiming a project-local type's FQN is now a diagnostic —
which is what makes "one FQN names one type" checked rather than assumed. `SymbolKey`
equality is now the single identity test in the tree.

Finding 7 went with it (the round-trip it names is deleted). Finding 9's `SymbolOrigin`
complaint is void: it is no longer a one-field wrapper — it carries the home assembly
directly and is the `key -> assembly` oracle a backend consults for an `AssemblyRef` scope
or a JS import path.

**Finding 2 is CLOSED.** Its diagnosis held — `typeNestedName`'s wildcard swallowed
`TypeHolder.InModule`, so a module-held type's `qualifiedName` dropped its module — but its
prescription ("make the match exhaustive; site the WHY on the new arm") was not a fix that
could be taken on its own terms: the flat rendering was load-bearing against a backend that
wrote every `TypeDef` flat, and the contract extractor independently lost the module by
round-tripping it through a dotted string. The renderer, the emitter and the extractor were
one change, and they landed as one: the renderer is exhaustive, the contract mints an
`InModule` chain, and the CLR backend emits a module-held type as a class NESTED in its
module's holder (`Layout`'s `TypeNode` tree + `NestedClass` rows). Finding 2's own "one of
these two sites is wrong, and it is not the loud one" was the correct read: `ClrEnv`'s
`failwithf` knew the answer all along, and is now the `TypeRef` that chains through the
module's holder.

**Open, unchanged: 3, 4, 5, 6, 8, 10** and the test gaps — except that the module-blind
claim collision named under "Test gaps" is now PINNED (`DuplicateTypeNameTests`,
`LocalModuleTests`), so the eventual fix has something to flip, and its emission
precondition is met.

## What is right

Worth stating, because the core is sound and the findings below are all "this stopped
one step short", not "this is the wrong idea".

- `Origin` sits **only** on `NamespaceKey`; every other key reaches it through its
  holder chain. A module in assembly A holding a type in assembly B is unrepresentable
  rather than merely unlikely.
- `MemberKey.Decl : TypeKey` deleted three runtime `failwithf "declaring key is not a
  TypeKey"` arms by construction.
- `FrozenType` makes cell-freedom a type-level fact, and `TStaticOptConstraintG<'ty>`
  closes a hole with a type parameter rather than a check.
- `TypeRegistry`'s `registerKeyed` / `tryKeyOfArity` / `tryKeyOfBareName` are genuinely
  generic over all three kinds — one mechanism, three tables.
- `SymbolKeyTests.fs:21` pins the real invariant: three independent mint paths for the
  same type compare **equal**; `:62` pins the negative (same qualified name, different
  home assembly ⇒ distinct key).

---

## 1. The key is structured; every table that consumes it is still string-keyed

**Priority: high. This is the through-line for findings 2, 3 and 7.**

**PARTLY CLOSED — the provider half.** The `.fsi` contract leaf is now keyed by `TypeKey`
(`ExternalSymbolProviders.KeyedLeaf` / `ofKeyedLeaf`), and `qualifiedName` is off its by-key
type path. The finding doubted its own prescription because "`MetadataSymbols` answers by
reflection, which is name-addressed by nature" — but that is two populations, not one
obstacle. A leaf whose type keys are all `InNamespace` (TS manifest, JS natives, a bare-IL
scrape) may soundly answer a key by rendering it, *because there its name and its key say the
same thing*; `KeyedLeaf.ofNamed` states exactly that condition. Only the leaf that mints a
containment a name cannot express — the contract extractor, the sole producer of
`TypeHolder.InModule` — had to become key-addressed, and it has.

**STILL OPEN — the registry half.** `TypeRegistry.TypeClaims` remains
`Dictionary<string, ResizeArray<TypeIdentity>>`, keyed by a bare short name with a linear
arity scan. That is the module-blind duplicate-claim collision. Its precondition — two
sibling modules' same-named types must be distinguishable in emitted metadata — is now met
(they nest in their holders), so the pin in `LocalModuleTests` / `DuplicateTypeNameTests` is
free to be flipped.

Every provider store takes a `SymbolKey` and immediately renders it back to a flat
string. From the shared leaf derivation, so this is the shape of the layer rather than
one provider's shortcut (`SemanticAnalysis/ExternalSymbolProviders.fs:64`):

```fsharp
member _.TryLookupType(key: SymbolKey) = leaf.TryLookupType(SymbolKeyOps.qualifiedName key)
member _.TryLookupByKey key            = leaf.TryLookup(SymbolKeyOps.qualifiedName key)
```

`qualifiedName` **discards the home assembly entirely** — `ExternalSymbols.fs:850` says
so: *"it is not a tiebreaker, it is simply not consulted."* Consequently
`SymbolKeyOps.reroot`, the structural walk these commits are built around, has exactly
one production call site (`ExternalSymbolProviders.fs:177`) and zero effect on any
lookup it feeds.

Two identity notions now coexist — the unifier compares the full asm-bearing
`SymbolKey`; the store answers by the asm-blind rendered name. The seam between them is
policed by a tripwire (`Passes/Unification/Engine.fs:174`):

```fsharp
when kind1 = kind2 && ar1 = ar2 && k1 <> k2
     && SymbolKeyOps.qualifiedName k1 = SymbolKeyOps.qualifiedName k2 ->
    failwithf "SymbolKey asm-invariant violated: ..."
```

It is inside `#if DEBUG`. In release the same drift degrades into a silent "type
mismatch". A representation gap guarded by an assertion that does not ship is an
invariant that should have been a type.

The same disease reaches the registry. `TypeRegistry.TypeClaims` — its own comment calls
it *"the sole duplicate-definition test and the sole route from a use-site name+arity to
the type that owns it"* — is `Dictionary<string, ResizeArray<TypeIdentity>>`, keyed by a
bare short name with a linear `FindIndex` on arity. `TypeIdentity` **already carries
`Key: TypeKey`**. `tryKeyOfArity` (`TypeRegistry.fs:265`) then recovers the arity by
re-rendering the suffix and string-comparing it — an `int` round-tripped through a
`string`.

**Fix.** The mechanism already exists in-tree, twice:

- Key the provider leaf tables by `SymbolKey`. This codebase already does exactly that in
  `memoize` (`ExternalSymbolProviders.fs:511`) and `mergeForwardRepr` (`:114`).
- Key `TypeClaims` by `TypeKey`, reusing `registerKeyed` / `tryKeyOf*` from the same
  file. Duplicate detection becomes namespace- and module-aware for free.

Then delete `checkAsmInvariant` rather than compiling it out.

If the string index must stay for other reasons, the store must **not** accept a
`SymbolKey`: give it a distinct `LookupName` type minted only by `SymbolKeyOps`, so "the
assembly is dropped here" is a type rather than a paragraph.

---

## 2. `typeNestedName`'s wildcard silently drops the holder this PR just added

**Priority: high. Cheapest fix in this document; a one-line wildcard is hiding it.**

```fsharp
// SemanticAnalysis/SymbolKeyOps.fs:105
let rec typeNestedName (t: TypeKey) : string =
    match t.Holder with
    | TypeHolder.InType outer -> typeNestedName outer + "+" + t.Name
    | _ -> t.Name                              // <- TypeHolder.InModule lands here
```

`TypeRegistration.localTypeHolder` is now *"THE sole producer of `TypeHolder.InModule`"*
(`TypeRegistration.fs:130`), so **every project-local type declared in a module** gets
one — and falls into that wildcard.

- `typeMetaName` is not injective: `N.A.T` and `N.B.T` render identically, and
  `qualifiedName` (which every store projects through — see finding 1) inherits the loss.
- `SemanticInfo.fs:133` claims the renderer/parser pair invert each other. They do not:
  `typeKeyOf` cannot produce an `InModule` at all.
- `Codegen.Clr/ClrEnv.fs:472` handles the identical case with a deliberate `failwithf`
  — *"would silently drop `m` and emit a ref that does not bind"*. One of these two sites
  is wrong, and it is not the loud one.

`ClrEnv.fs:476` further justifies its `failwithf` with *"No producer mints this holder
yet."* `TypeRegistration.fs:130` says it is **the** producer. Two comments in the same
tree now contradict each other.

**Fix.** Make the match exhaustive. Had it been exhaustive, adding `InModule` would have
forced this decision at the one site that owns the rule. Site the WHY on the new arm
(flat `TypeDef` emission today, so the module is deliberately absent from the metadata
name), or render the chain and let `ClrEnv` keep owning the flat-emission concession.

---

## 3. A published inline body can carry a dangling `Var`

**Priority: high — silent, and produces bad codegen rather than a diagnostic.**

`Freeze.rewriteSiblingRefs` (`SemanticAnalysis/Freeze.fs:177`) rewrites `TExpr.Var →
TExpr.External` only for keys in `vocabulary`, and `vocabulary` is populated only from
decls passing `isInlineVocabulary` — i.e. sibling *inline templates*
(`Freeze.fs:200-208`). So:

```fsharp
let k = 3                      // ordinary module value — not in `vocabulary`
let inline addK x = x + k      // published body keeps `k` as a raw Var
```

The consumer splices an unbound `NodeKey`. No `failwith`, no diagnostic; it surfaces
downstream as a bad codegen local slot.

**Fix.** Rewrite against `tast.ModuleMembers` — every module-level binder has an
`info.Key`, and a non-inline one resolves at the consumer to a real compiled call, which
is correct. Then make it structural: at publish, assert the body has no free `Var`. This
is the one place that can see it.

While here: `Freeze.run` walks `tast.Decls` twice with the same
`TDecl.Let(TPat.NamedSimple …)` match (`:202-208` and `:212-215`); one fold produces both
the dictionary and the list.

---

## 4. The cross-kind cascade was added, not replaced

`Translate.resolveClaimedType` (`Passes/Unification/Translate.fs:391`) is introduced with
a doc stating the thesis — *"cross-kind precedence is a LOOKUP, never a hand-ordered
cascade"*. Both callers then fall through to a hand-ordered cascade over the same six
kinds:

- `resolveBareTypeName`, `Translate.fs:466-490` — IntrinsicRepr → Abbrev → Record → Union
  → Class, with `EqArray.init info.TypeParams.Length …` copy-pasted four times.
- `resolveNamedGeneric`, `Translate.fs:553-588` — the same five kinds again, behind a
  `resolveLocalGeneric` combinator.

The "a union additionally stamps `ResolvedType`, a record/class does not" rule is
therefore encoded three times. Adding a `TypeDeclKind` breaks `resolveClaimedType` at
compile time and is silently ignored by both cascades.

**Fix.** The lenient tail differs from the claim path only in looking the name up at
*any* arity. Add `TypeRegistry.tryTypeClaimAnyArity : … -> TypeIdentity voption` and both
tails collapse to three lines, deleting ~65 lines and `resolveLocalGeneric` with them.

---

## 5. The inline-body by-key channel was removed and re-implemented as a scan

Commit `5c45cce1` retired `IExternalSymbolStore.TryLookupInlineBody : SymbolKey ->
InlineBody voption` in favour of folding the body onto every resolved entry. The cost:
an `InlineBody` field on two hot records, eight `InlineBody = ValueNone` boilerplate
sites across `MetadataSymbols` / `JsNativeSymbols` / `TsManifestMembers` / `VesperLib`,
and a `withInlineBodies` decorator (`ExternalSymbolProviders.fs:464`). `TryLookupByKey`
has **exactly one consumer in the tree** (`Passes/InlineExpansion.fs:393`) — it exists
solely to undo the fold.

The member path is strictly worse for it: unable to ask by key, it re-runs the *name*
lookup and linearly rescans for the key it already holds
(`InlineExpansion.fs:401-409`), returning an `option<voption<InlineBody>>`. The overload
hazard that motivates the scan is one the by-key channel never had.

**Fix.** Keep the freeze/thaw — that is the real content of the commit — and restore one
identity-addressed body channel, now returning `Frozen.TInlineBody`. That deletes
`withInlineBodies`, both `InlineBody` fields, the eight defaults, the scan, and
`TryLookupByKey`. If the field is genuinely wanted, the missing piece is
`TryLookupMemberByKey : MemberKey -> ExternalMember voption`, not a scan.

**Related, and free:** `Tast.fs:1051` defines `TInlineBody` — the thawed twin, the exact
"one type, two instantiations" move the commit is built on — and it is **dead**. `thaw`
returns an anonymous `TDecl * ParamAttrs[]` tuple instead (`InlineExpansion.fs:376`), and
that tuple's churn accounts for most of the `InlineExpansion` diff. Using the type that
already exists reverts every one of those call-site edits and restores named fields.

---

## 6. Missing member projections: 10+ hand-rolled destructures with five different fallbacks

`ExternalMember.Key`, `ExternalSymbol.Key`, `IntrinsicIdentity.Canon` and
`IntrinsicInterfaceShape.Canon` are all still typed `SymbolKey`, so every consumer
re-matches — and they disagree about what a non-member key means:

`memberParamCount`, reimplemented six times:
`Codegen.Clr/ClrExternalMembers.fs:559` (`| _ -> false`), `:576` (`| _ -> 0`),
`Codegen.Clr/EmitCall.fs:356` (`failwithf`), `Codegen.Js/JsExternalMembers.fs:115`
(`failwithf`), `Passes/Unification/InferOverload.fs:115` (`| _ -> 0`),
`Passes/Unification/InferApp.fs:498` (`| _ -> false`).

`memberDeclKey`, reimplemented eight times: `Codegen.Clr/EmitMember.fs:143`,
`Codegen.Clr/EmitResolve.fs:331` / `:301`, `Codegen.Clr/EmitBindings.fs:153`,
`ClrExternalMembers.fs:191` / `:265` / `:308`, and
`Codegen.Js/JsExternalMembers.fs:23` — which returns `| _ -> key`, handing back the
**member** key as if it were the declaring key.

Note `SymbolKeyOps.fs:233` claims the `failwithf "declaring key is not a TypeKey"` checks
`ClrExternalMembers` used to carry *"are gone"*. They are not; `ClrExternalMembers.fs:191`
still has one, plus four silent `| _ -> 0` / `| _ -> false` variants.

**Fix.** Narrow those four published fields to `MemberKey` / `BindingKey` / `TypeKey`
(~a dozen sites, confined to the provider seam — *not* the ~1300-site IR widening
`SemanticInfo.fs:255` defers), and add `SymbolKeyOps.tryMember` / `memberParamCount` /
`memberDeclKey`. That deletes ~20 inline matches and forces one answer to "what does a
non-member key mean here" instead of five.

---

## 7. Surviving key → string → key round-trip

```fsharp
// Codegen.Common/SymbolProviders.fs:125
let typeKey = SymbolKeyOps.lookupKeyOfCompiledName (SymbolKeyOps.qualifiedName tdecl.Key)
match ctx.Provider.TryLookupMember(typeKey, m.Name) with
```

`tdecl.Key` is already a `SymbolKey`, and `ExternalSymbols.fs:848` says the string
round-trip is *"an implementation detail, never a call-site idiom"*. Passing `tdecl.Key`
directly is exactly equivalent — and this is the one call site where finding 2's
flattening actually mutates a key (an `InModule` type's key returns as `InNamespace`).
Delete the round-trip.

---

## 8. Mint paths that bypass `SymbolKeyOps`

Each is an identity-drift hazard — two keys for the same type that do not compare equal.

- `Codegen.Clr/ClrRecipes.fs:331` hand-builds `SymbolKey.Binding { Decl =
  ModuleHolder.InModule declModule; Name = name }`. `SymbolKeyOps.valueKey` is exactly
  this. `Codegen.Clr/ClrProvider.fs:331` destructures the same shape back by hand.
- `Codegen.Clr/MetadataSymbols.fs:276` builds a `TypeKey` record literal via its own
  recursive `DeclaringType` walk — a second nested-`TypeKey` constructor beside
  `typeKeyOf`'s `+`-parse.
- `Codegen.Js/TsManifestTypes.fs:137` (`mint`) is a third external-type mint alongside
  `externalTypeKeyOf` / `qualifiedTypeKeyOfT`; `RuntimeNames.primitiveKey` / `opaqueKey`
  a fourth.
- `Codegen.Js/JsExternalMembers.fs:260` synthesises a `BindingKey` from a `TypeKey`'s
  `NamespaceKey` — cross-kind key surgery outside the algebra module.

---

## 9. Boundary and type cleanups

- **`SymbolOrigin` is now a one-field wrapper.** `SemanticInfo.fs:283` — a record around
  a single `NamespaceKey`, whose `.Assembly` is two hops down and which sits confusingly
  beside the `Origin` DU. It costs 27 `{ Namespace = … }` literals and buys nothing.
  `ExternalTypeShape.Origin : NamespaceKey`, `NamespaceKey.Global` for `Empty`,
  `.Origin.AsmOption` for `.Assembly` — pure deletion.
- **`intrinsicKeyOf`'s defensive fallback** (`TypeRegistry.fs:223`) falls back to a
  by-name mint on a key miss, and its own doc admits it is unreachable. That is the
  "key misses ⇒ match on the name" smell in the canonical module.
- **Four parallel intrinsic side-tables** keyed by the same bare name —
  `IntrinsicReprTypes` (`TypeRegistry.fs:97`), `IntrinsicKeys` (`:107`),
  `HeritableExternBases` (`:115`), `IntrinsicAbbrevHost` (`:126`) — written from two
  passes and held together at runtime by `MemberRegistration.fs:771`'s
  `failwithf "…has no recorded intrinsic repr"`. One
  `Dictionary<string, IntrinsicTypeInfo>` makes that `failwithf` unrepresentable.
- **`UnionCaseInfo` identifies its union by `(string, int)`**, so `unionOfCase`
  (`TypeRegistry.fs:503`) is a partial function faking a total one. The union's `TypeKey`
  is in scope at the one construction site (`TypeRegistration.fs:514`). Replace
  `UnionName`/`UnionArity` with `Union: TypeKey`; `unionOfCase` becomes a dictionary read
  and loses its `failwithf`.

---

## 10. Comments, conventions, naming

The two files that *define* the algebra read as changelogs. ~38 comments across the
touched files narrate prior states of the code, which the repo convention forbids
(comments state WHY/invariants, not history). Concentrated in `SymbolKeyOps.fs` (11) and
`SemanticInfo.fs` (7):

- `SymbolKeyOps.fs:205`, `:243` — *"the mechanical successor to the old
  `SymbolKey.TypeKey(asm, ns, name)`"*
- `SymbolKeyOps.fs:233` — *"the `failwithf` checks `ClrExternalMembers` used to carry are
  gone"* (they are not — see finding 6)
- `SymbolKeyOps.fs:382-386` — a paragraph on what the deleted `originNsFor` used to mis-cut
- `SymbolKeyOps.fs:203` — section header *"Smart constructors mirroring the old tuple shapes"*
- `SemanticInfo.fs:159` — *"Was `SymbolKey.ValueKey`"*; `:44` — *"is what retires `ns = \"\"`"*;
  `:166` — *"`ClrProvider`'s hand-rolled `when ns <> \"\"` guard is now that case match"*

Each is recoverable from `git log`, and three have already gone stale:

- `SymbolKeyOps.fs:129` documents `moduleTypeKey` — **a function that does not exist
  anywhere in the codebase**.
- `SymbolKeyOps.fs:14` still calls the inline-body channel *"pre-freeze"*; commit
  `5c45cce1` froze it.
- `ClrEnv.fs:476` says no producer mints `TypeHolder.InModule`; one now does (finding 2).

Also against convention:

- **Plan-doc milestone labels in code and tests**: `LocalModuleTests.fs:7` (*"G15 / G16"*),
  `Codegen.Js.Tests/ExternMemberInlineTests.fs:11` (*"W9 Stage 1b"*),
  `SemanticAnalysis/Inline.fs:9` (*"beat (b)"*).
- **`src/XParsec.FSharp.SemanticAnalysis/docs/inline-body-freeze-thaw-plan.md` should be
  deleted.** Its own first line says the work has landed; the residue is a backlog, not a
  scope.
- **`InlineFreezeThawSpikeTests.fs` is still named a "Spike"** for landed, load-bearing
  work. Rename the file and module.
- **Two design essays embedded in type definitions** — `SemanticInfo.fs:188-210` (the
  `MemberKey.ArgSig` TODO) and `:251-261` (the "REMAINING NARROWING" note). Both describe
  future work; they belong in an issue, not in the type.

---

## Test gaps

The tests are good and pin the right invariant (see "What is right"). Two gaps:

- **No test combines an arity overload with `inherit` or with an augmentation block** —
  the exact hole the top-down registration plan closes. Add one when it lands.
- **The namespace/module-blind claim collision is unpinned.** `TypeHolder.InModule`'s doc
  comment (`SemanticInfo.fs:112-117`) records that two sibling modules declaring the same
  type name still contest one claim and the second is rejected as a duplicate. No test
  records it. Pin the current behaviour so the fix has something to flip.

Two smaller nits:

- `Codegen.Clr.Tests/ExternalMemberTests.fs:73`, `:109`, `:205`, `:415` each decompose a
  `TypeKey` into three string assertions. One `Expect.equal decl (SymbolKeyOps.typeKeyOf
  …)` tests strictly more (it pins the holder shape too), in a quarter of the lines, and
  is the same equality the unifier uses.
- `NameResolutionTests.fs` repeats the `match TypeRegistry.tryRecord … | ValueNone ->
  failtest` shape eight times. One `TestHelpers.expectRecord ctx "X"` per kind removes it.
