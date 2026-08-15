# `FrozenType` / `SemType` narrowings — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Continues the move `Narrow SymbolKey to TypeKey in type positions` made: where a value's
declaration admits more shapes than the position can accept, narrow the declaration and
delete the runtime check that was standing in for it.

Three candidates came out of the survey. `FTOr`'s payload, the word "member", and Part A
(nominal-only positions, now `FrozenNominal`) have all landed; git carries what they did.
Only Part B is left.

---

# Part B — `FTUnknown` / `TyUnknown` carry a bare `string`

## B0. The problem

One case, one `string` field, nine unrelated producers. The string is not a diagnostic
payload — it is read back, joined against another string space, and in one case IS the
type's identity.

**Producers, by what the string actually means:**

| Meaning | Spelling | Where |
| --- | --- | --- |
| A source-written type name nothing defined | the name itself | `Translate` |
| A leaked inference metavar | `?unresolved-typar` | `Freeze` |
| An extraction-time placeholder, filled later in the pass | `<deferred>` | `FrozenTypeBridge` |
| An external body that could not be frozen | `<unfreezable external template>` | `ExternalDeclarations` |
| Declaring-arg index past the instantiation | `<arity-mismatch>` | `FrozenTypeBridge` |
| Under-applied generic abbreviation | `<abbrev-arity-mismatch>` | `FrozenTypeBridge` |
| A structural TS type's synthesised IDENTITY | `structural:` + hash | `TsManifestTypes` |
| A range in value position | `range` | `InferApp` |
| A non-literal token in literal position | a formatted message | `InferLiterals` |
| A metavar on neither typar axis, inside a `MemberKey` | `""` (empty), four sites | `InferOverload` |

**Consumers that read the string, not just the case:**

- `Engine`'s `TyUnknown` unify arm joins it against `PassContext.UndefinedTypeNames`
  (a `HashSet<string>`) by string equality, to decide whether to report. `Translate` is the
  producer that populates that set, immediately before minting its `TyUnknown` — so the join
  is correct for exactly one of the ten producers and coincidental for the rest.
- `EmitResolve.tyCtorOf` returns the string as the type CONSTRUCTOR discriminator used for
  overload matching. Every `FTUnknown ""` from `InferOverload` therefore ties with every
  other.
- `ClrEncoder` quotes it into *"type '%s' could not be resolved during contract extraction —
  is a package dependency missing?"*, which is the right sentence for one producer.

## B1. Suspected defect — the sentinels reach the unify diagnostic

`Engine`'s arm reports *"Type '%s' could not be resolved during contract extraction — is a
package dependency missing?"* for any `TyUnknown` whose string is not in
`UndefinedTypeNames`. `TyUnknown "range"` is minted as the RESULT type of a range in value
position, so it unifies with whatever the context expects and should reach that arm — and
`range` is not a source-written type name, so the suppression cannot fire.

The correct diagnostic for that program exists: `Kind.RangeNotFirstClassValue`, reported by
`ElaborateExpr`. But Elaborate runs after Unification, so the prediction is a spurious
package-dependency error alongside the real one.

**Unverified.** The first step of Part B is a test that pins what `let x = 1..10` actually
reports today. If the prediction holds, it is the concrete payoff; if not, Part B is hygiene
only and can be scheduled accordingly.

## B2. The narrowing

Replace the `string` with a DU — `UnknownReason`, in `SemanticScalars` beside `TyparAxis`
and `LiteralConst`, since both `FrozenType` and `SemType` need it. NOT `UnknownType`: that
spelling is already a case of `TypeRefVerdict`, of the diagnostic kind in `PassContext`, and
of `Engine`'s private `DotSource` — the last in the very file Part B changes.

```fsharp
[<RequireQualifiedAccess>]
type UnknownReason =
    /// A type name the source wrote and nothing defined. The ONLY case the unifier's
    /// `UndefinedTypeNames` suppression applies to.
    | UndefinedName of name: string
    /// A metavar the front end never resolved.
    | UnresolvedTypar
    /// A contract body that could not be built at extraction time.
    | Unextracted of reason: …
    /// A type argument index past the instantiation it was applied to.
    | ArityMismatch
    /// An anonymous TS structural type, identified by the hash of its printed members.
    | Structural of hash: string
    /// A construct with no type of its own, rejected downstream where its position is known.
    | NoValueType of what: string
```

Notes on the mapping:

- `Unextracted`'s payload is left open deliberately. `UnmodelledReason` already exists, with
  an `ExtractionFailed` case, and `Translate`'s `unresolvedRefTy` already reads it off the
  external shape. Check whether it covers `<deferred>` (not yet, this pass) as well as
  `<unfreezable external template>` (never) before coining a second reason DU.
- `UndefinedName` keeps `string` for now. `WrittenTypeName` would be the honest type and
  would narrow the `UndefinedTypeNames` join with it, but `unresolvedRefTy` takes a bare
  `string` today; `ctx.WrittenTypeNameOf` exists elsewhere in the same file, so thread it
  if that is cheap and drop this note. Either way the join is already narrowed by the case
  itself: after the change the `Engine` suppression cannot apply to a sentinel.
- `Structural` is the one identity-bearing case; keeping it distinct stops a hash from ever
  being read as a type name by `ClrEncoder` or `tyCtorOf`.
- `InferOverload`'s four `FTUnknown ""` sites are a metavar on neither axis, INSIDE a
  `MemberKey.ArgSig`. Check whether they want `UnresolvedTypar` or a case of their own —
  they are a key component, so the choice decides which overloads tie. Do not fold them in
  without looking.
- `NoValueType` covers `range` and the non-literal-token case, and is the one that should
  NOT reach the contract-extraction sentence.

## B3. Consumer changes

- `Engine`'s unify arm: match `UnknownReason.UndefinedName` for the existing suppression and
  message; give the other cases their own arms, and no contract-extraction sentence.
- `ClrEncoder` and `EmitResolve.tyCtorOf`: render through a single `UnknownReason` display
  member rather than assuming the payload is a name.
- `FrozenTypeTable`'s `TypeRow.Unknown` currently interns a string id. It needs a row shape
  per case, or a discriminator alongside the interned string. This is a frozen-codec format
  change — `FrozenCodecRoundTripTests` covers it, and the sample list in that suite must gain
  one `FTUnknown` per case.

## B4. Also in scope

`test/…/Codegen.Js.Tests/FrozenCodecRoundTripTests` constructs `FTUnknown "?free-typar"` as
fixture data. That string is minted nowhere in `src/` — the live spelling is
`?unresolved-typar`. The DU deletes the possibility.

## B5. Verification

- The `let x = 1..10` diagnostic test from B1, before and after.
- `SemanticAnalysis.Tests` and `Codegen.Js.Tests` full runs; `FrozenCodecRoundTripTests` is
  load-bearing for the codec change.
