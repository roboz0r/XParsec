# The array is keyed under two spellings

Open defect. Attempted and REVERTED once; this records what the attempt found so the next
one starts from the map rather than the symptom.

## The defect

`'T[]` has two `SymbolKey` spellings:

- `Vesper.[]` — what a value's type carries (`RuntimeNames.arrayKey`), minted from
  `arrayName`.
- `` Vesper.`[]` `` — what its DECLARATION files its shape under, because
  `` type 'T ``[]`` `` must spell the name backtick-escaped and the escape is carried
  verbatim into the key (`RuntimeNames.arrayContractName`).

`RuntimeNames.declarationKey` bridges the two at the lookup sites. The escape is SOURCE
SPELLING — F# emits the bare name to metadata — so it belongs nowhere near an identity.

## Why the obvious fix is not one edit

Unescaping at the key mint (`SymbolKeyOps.typeKeyOfContainer` / `typeKeyOfSegment`) is
necessary but nowhere near sufficient. The declared name reaches keys and name-indexed
tables through FOUR independent sinks, and every one must agree or a lookup silently misses:

1. `VesperLib.registerTypeDecl` — the `.fsi` contract extraction's short name.
2. `NameResolutionTypeRegistration.claimTypeIdentity` — the project-local `.fs` claim, which
   also stamps `IntrinsicKeys` (name → identity key).
3. `ElaborateTypeDecls.typeNameSimple` — the elaboration-side reader, which looks a member
   host up by the name registration filed it under.
4. `VesperLib.extractIntrinsicReprsInto` — the `.fs` repr PRE-SCAN, keyed by short name and
   probed by `ctx.IntrinsicReprs.TryGetValue`. Missing this one costs the array its platform
   repr on every target, with no diagnostic.

Plus one rule in the key algebra:

5. `SymbolKeyOps.withArity` supplies an arity the compiled name did not spell. It currently
   declines for an escaped name; unescaped, it stops declining, and the array's member decl
   key acquires an arity the use site does not mint. A structural constructor (`[]`, `[,]`,
   `byref`) takes arity 0 however it is minted — its element type rides the type's args —
   so that has to become the stated rule rather than a side effect of the escape.

## Found on the way, NOT fixed

The two halves of the array's `get_Item` disagree, and the escaped spelling was hiding it.
The `.fsi` (`array-index.js.fsi`) freezes its parameter as `int`; the `.fs`
(`array-index.js.fs`) freezes the same parameter as `FTUnknown "?unresolved-typar"`. The
inline-body store is keyed by the WHOLE member key, `ArgSig` included, so the two keys are
not equal — the contract half and the impl half only meet today because
`Contract.Provider.TryLookupMember` resolves by NAME. Any change that makes the member key
the lookup key will surface this. Worth chasing on its own: the impl's parameter type not
resolving is a bug wherever it comes from.

## Done when

- `arrayContractName` and `declarationKey` are deleted, and `arrayKey` is the only spelling.
- No key holds a backtick except as a `` `N `` arity suffix.
- This file is deleted.
