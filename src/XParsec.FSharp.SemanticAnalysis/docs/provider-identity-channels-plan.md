# Narrowing the string-keyed provider API — resolve once, thread the key

`IExternalSymbolProvider` conflates two responsibilities that belong at
different points in the pipeline. This note names the split, records the rule
that keeps them apart, and stages the work to enforce it. It grew out of the
`Unchecked.defaultof` inline-body fix (see `unchecked-defaultof-plan.md`),
whose root cause was a *downstream* pass re-resolving by source spelling an
identity it should already have held as a `SymbolKey`.

## The two faces

- **Resolver face** — `string -> SymbolKey`: `TryLookup`, `TryLookupType`,
  `TryLookupMember`, `TryLookupUnionCase`, `TryLookupInlineBodyByName`. Source
  spelling in, identity out. Legitimately needed **once**, at the points that
  turn written/synthesised names into identities: `NameResolution`, the
  contract extractor, and the handful of Freeze sites that *synthesise*
  references to well-known intrinsics.
- **Typed face** — `SymbolKey -> data`: `TryLookupInlineBody key`, and the
  key-addressed member/type variants. Identity in, payload out. This is what
  every *downstream* pass (Unification residue, InlineExpansion, codegen)
  should speak.

**The rule:** a source-spelling → `SymbolKey` resolution happens once, at a
resolver-face site. After that a `SymbolKey` — a proper type — flows through
the rest of the code. No consumer pass re-derives identity from a spelling.

Freeze already embodies this for the common path: it stamps the resolved
`SymbolKey` onto `TExpr.External`, and codegen/InlineExpansion key off it. The
leaks are the places that don't.

## The leak surface

Empirically (as of this note):

- **`TryLookupInlineBodyByName` has exactly one consumer** —
  `InlineExpansion.lookupExternal` (the `key = ValueNone` fallback). Every
  other occurrence is the interface decl, the one real impl
  (`SymbolProviders.withInlineBodies`), the caching passthrough, or a trivial
  `ValueNone` stub.
- It exists **only** to serve `TExpr.External(_, ValueNone, _, _)` heads.
  Those are minted at ~13 sites, in three groups:
  1. **Desugared operators** — every `a + b` / `-x` mints
     `External("op_Addition", ValueNone, …)` (`Freeze/Apply.fs`,
     `FreezeExpr.fs`). The operator bodies are cross-package inlines, so these
     splice *by name*.
  2. **Synthesised intrinsics** — `op_Dynamic`, `GetArrayLength`,
     `op_DynamicAssignment`, get/set-index (`Freeze/Access.fs`,
     `Freeze/Resolve.fs`).
  3. **Intra-body rewrite** — `rewriteInlineVars` re-points a sibling inline
     `Var` to `External(name, ValueNone, …)` (`SymbolProviders.fs`).
- A **second** by-name mechanism lives downstream of Freeze: codegen's
  `BuiltinOps` recipe recognises operators by their `External` *name* string.
  Same class of leak, larger blast radius; its own stage below.

So the by-name inline channel is not a vestige — it is the shared crutch for
compiler-synthesised heads that know an intrinsic's *name* but never captured
its *key*.

## Target design

1. **Stamp at synthesis.** Every `TExpr.External` the front end mints carries
   a resolved `SymbolKey`. For operators the key is already computed during
   Desugar/Unification — it just isn't threaded onto the node. For synthesised
   intrinsics it is a resolve-once (or a canonical `*Key` constant) at the mint
   site.
2. **Collapse the consumer to key-only.** `InlineExpansion.lookupExternal`
   becomes `provider.TryLookupInlineBody key`; the name arm and
   `TryLookupInlineBodyByName` (interface method, real impl, caching layer, all
   stubs) delete — net LOC down.
3. **Segregate the interface.** Split `IExternalSymbolProvider` so consumer
   passes receive a **key-only** face; the resolver face is in scope only where
   spelling is first resolved. A future pass then *cannot* reintroduce a string
   lookup — the method isn't reachable.

## Staged plan

- **Stage 0 — DONE.** Value inline bodies resolve their identity **once**, at
  collection (`collectInlineBodies`, against `ctx.Provider`), and carry a
  `SymbolKey voption` (`ValueInlineBody.Key`). `buildContractCached` keys
  `byKey` off that resolved key — the store builder does no source-name lookup
  of its own. This is the value-channel twin of the member channel and the
  first concrete instance of "resolve once, thread the key."
- **Stage 1 — inline name channel removal.** Stamp keys on the three groups of
  `ValueNone` mint sites (operators, synthesised intrinsics, intra-body
  rewrite — the last is directly enabled by Stage 0: build a
  `NodeKey -> SymbolKey` map alongside `inlineNames`). Then collapse
  `lookupExternal` to key-only, delete `TryLookupInlineBodyByName`, and
  segregate the interface. Bounded but real — the operator sweep touches the
  hot desugar path.
- **Stage 2 — codegen by-name → by-key.** Move `BuiltinOps` recognition off the
  `External` name string onto the stamped `SymbolKey`. Larger; separate doc.

## Premises to confirm before Stage 1

- **Operators:** does every desugared-operator head have a resolved `SymbolKey`
  reachable at its mint site (does Desugar/Unification already record one, keyed
  by the op node), or does threading it need a new side-table entry? This is the
  gating question — it decides whether Stage 1 is "thread an existing value" or
  "compute and record a new one."
- **Synthesised intrinsics:** do `op_Dynamic` / `GetArrayLength` / index ops
  have canonical `SymbolKey`s resolvable via the provider (they live in
  `ops-platform` modules) or as `RuntimeNames` `*Key` constants, so the mint
  site can stamp without a bespoke lookup?
- **Failure mode:** a *missed* stamp after the name channel is gone is a
  mis-splice or a phantom `call`, not a graceful miss — so each mint site needs
  a use-site test that exercises the splice, exactly as the `DefaultOfInline`
  suites do for the value channel.

## Non-goals

- Re-keying the type/member resolver calls that legitimately live in
  NameResolution / the extractor — those *are* the resolve-once boundary.
- Materialising a real `DefaultOf<T>()` method (tracked separately in
  `unchecked-defaultof-plan.md`, Half 2).
