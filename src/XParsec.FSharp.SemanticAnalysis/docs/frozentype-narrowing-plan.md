# `FrozenType` / `SemType` narrowings — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Continues the move `Narrow SymbolKey to TypeKey in type positions` made: where a value's
declaration admits more shapes than the position can accept, narrow the declaration and
delete the runtime check that was standing in for it.

Three candidates came out of the survey. One has landed:

- **Landed — `FTOr`'s payload.** `TyOr` wrapped a private constructor; its frozen twin
  `FTOr of EqSet<FrozenType>` took a raw payload, so the frozen decoder in `FrozenTypeTable`
  could build a non-flattened, non-deduped set without routing through `MkUnion`. The two
  now mirror each other exactly — same private constructor, same `OfSeq` / `Disjuncts` /
  `Map`, and `FrozenType.MkUnion` reduces to the singleton collapse, as `SemType.MkUnion`
  already did.
- **Landed — the word "member".** The payload types were `UnionMembers` / `FrozenUnionMembers`
  and their contents were `members` throughout, colliding with the methods-and-properties
  sense the rest of the compiler gives that word — `Subsume` used both meanings in one file.
  They are now `TyDisjuncts` / `FTDisjuncts` with a `Disjuncts` accessor, and the union sense
  of "member" is gone from the identifiers, locals and prose that surround them. `disjunct`
  is the repo's existing word for the parts of an `|` (`Scope` already uses it for or-pattern
  disjuncts), and `alternative` was unavailable — XParsec's combinators own it.

  The manifest wire format went with it: `Schema.TypeRef.Union` carries `disjuncts`, the
  `Codec` key is `"disjuncts"`, and `SchemaVersion` is bumped to 2 — a decoder understands
  exactly one version, so a renamed field is a version change. All 25 committed manifests
  were regenerated through the Fable-built extractor; the JSON diff is 1210 `"members"` →
  `"disjuncts"` keys, every one directly under a `"k": "union"`, plus the version line.
  The `"members"` key on `interface` / `class` / `enum` exports is untouched: those are
  type members, which is the sense the word keeps.
- **Part A** below — nominal-only positions still take a wide `FrozenType`.
- **Part B** below — `FTUnknown` / `TyUnknown` carry a bare `string`.

A and B are independent: no shared file, no ordering constraint. A is the larger change and
deletes duplicated code in both backends; B is smaller and fixes a diagnostic defect.

---

# Part A — nominal-only positions take a wide `FrozenType`

## A0. The shape of the problem

`FrozenInterface` already is this narrowing, for one slot only. It is a private record over
`{ Ref; RefKey; RefArgs }` with a checked `TryOfFrozen`, and its own comment states the
rule: *"Only a nominal reference witnesses anything at a use site, so a non-nominal one
cannot be built."* Every other nominal-only position in the compiler restates that rule as a
runtime check instead.

Three independent re-implementations exist today:

- `TastLower.objArgShape` — the shared `FrozenType -> (TypeKey * FrozenType list) voption`,
  matching `FTUnion | FTRecord | FTClass`.
- `EmitResolve.nominalShape` / `nominalTypeKey` (CLR) — `objArgShape` plus
  `failwithf "Emit: %s on non-nominal type %A"`, with a `what` string naming the site.
- `EmitJsCtx.nominalKey` (JS) — `objArgShape` plus
  `failwithf "EmitJs: %s on non-nominal type %A"`, with a `what` string naming the site.

The two backend copies are the same function with a different message prefix, and the `what`
parameter exists solely to reconstruct, at run time, which construct the caller was — a fact
the caller knew statically.

Note that `FrozenInterface.TryOfFrozen` and `objArgShape` disagree on `FTConst`: the former
accepts it (an intrinsic interface such as `seq<'T>` on JS freezes as `FTConst`, and its key
is as nominal as the other three), the latter rejects it. **This fork must be settled before
any code moves** — see A4.

## A1. `FrozenNominal`

Generalise `FrozenInterface`'s payload into a `FrozenNominal` carrying the same three
fields, for the same reason: the nominal FLAVOUR has to survive (an `FTUnion` realises as a
`TyUnion`, not a `TyClass`), so the whole `FrozenType` is kept alongside the destructured
key and args.

The existing members carry over unchanged in role: `Frozen`, `Key`, `Args`, `MapArgs`,
`TryOfFrozen`, and the `OfClass` mint. `FrozenInterface` then becomes `FrozenNominal` used
in the interface slot; there is no second type.

Add one member the interface slot never needed:

- `OfFrozen : string -> FrozenType -> FrozenNominal` — the checked conversion, with the
  site name in the failure. This is the ONE surviving `failwith`, and after A3 it has one
  caller.

## A2. Base-type slots

`FrozenBaseType` on the external class shape, and the `BaseType` fields on the codegen class
row, the frozen-codec class row and the TAST decl, are all `FrozenType voption`. A base type
is nominal on every producer:

- `MetadataSymbols.buildClassBaseType` reads `Type.BaseType` through reflection and returns
  `ValueNone` for interfaces and `System.Object`.
- `TsManifestMembers.classifyHeritage` already `failwithf`s on a non-nominal heritage entry
  — *"TS cannot express a structural or union supertype, so a non-nominal entry is a corrupt
  manifest"* — and then writes the base slot from the same loop.

The consumers currently widen silently rather than loudly, which is worse than the emit
sites: `JsExternalMembers`' `exn`-repr climb walks `shape.FrozenBaseType` and a non-nominal
link would simply end the climb with `ValueNone`, and `ExternalSymbols.instantiateBaseTypeFrozen`
maps `instantiateDeclaring` over whatever is there.

Change these four fields to `FrozenNominal voption`. `instantiateBaseTypeFrozen` then takes
`.Frozen`; the climb loses its dead arm.

The `SemType` twin, `BaseType` on the class info in `TypeInfos`, is `SemType voption` and is
set from inference, so it is NOT in scope here — a base type is only known-nominal once it
is resolved, and pre-freeze it can legitimately be a `TyVar`.

## A3. The emit sites

Add `TastAccessor.exprNominalTy : ExprId -> FrozenNominal` beside the existing
`TastAccessor.exprTy`. Both backends reach the type the same way — the JS record/union/new
emitters call `TastAccessor.exprTy e` and hand the result straight to `nominalKey` — so one
accessor serves both, and the checked conversion happens once per node instead of once per
backend.

Sites that become total:

| Construct | CLR | JS |
| --- | --- | --- |
| `RecordCons` | `EmitConstruct` | `EmitJs` |
| `RecordClone` | `EmitConstruct` | `EmitJs` |
| `UnionCons` | `EmitConstruct` | `EmitJs` |
| `New` | — | `EmitJsCtx.tryNewTarget` |
| union pattern | `EmitPattern` | — |
| record pattern | `EmitPattern` | — |
| member access | `EmitResolve` | — |
| field access | `EmitResolve` | — |

`EmitResolve.nominalShape`, `EmitResolve.nominalTypeKey` and `EmitJsCtx.nominalKey` all
delete, along with every `what` string threaded to them. `recordInfoOf` / `unionInfoOf` take
a `FrozenNominal` and keep only their second failure — the one that matters, *"on a record
with no emitted type"* — which is a genuine table miss, not a shape check.

**Sites that must NOT be converted.** Three `objArgShape` callers are genuinely probing, not
asserting, and each has a defined answer for a non-nominal:

- `EmitBindings`' `use`-disposal locality test (`ValueNone -> false`).
- `EmitMember`'s declaring-typar arity (`ValueNone -> 0`).
- `EmitResolve`'s `instantiationFor` (falls back to `recoverMemberInst`).

`objArgShape` survives for these. Its return type should become `FrozenNominal voption` so
there is one destructuring, not two.

## A4. The open question — does `FrozenNominal` admit `FTConst`?

`objArgShape` excludes `FTConst`; `FrozenInterface.TryOfFrozen` includes it. Unifying the
two types forces a decision, and this is the one place the change is not behaviour-preserving.

Admitting `FTConst` is the wider option. Under it, a record/union emit site handed an
intrinsic no longer fails at the shape check but at the table lookup one line later —
*"on a record with no emitted type (key …)"* — which is arguably the better message anyway,
since it names the key. Nothing regresses that was previously caught, and the interface slot
keeps the behaviour it needs.

Rejecting `FTConst` would need `FrozenInterface` kept as a distinct type, which defeats the
exercise.

**Recommendation: admit `FTConst`,** and pin the emit-site behaviour with a test before the
refactor so the message change is a deliberate, reviewed diff rather than a surprise.

## A5. Verification

- Before touching anything: a test per construct asserting the CURRENT failure for an
  intrinsic-typed `RecordCons` / `UnionCons`, so A4's message change is visible in the diff.
- `Codegen.Clr.Tests`, `Codegen.Js.Tests`, `SemanticAnalysis.Tests` full runs (no `-Filter`,
  which is `--no-build`).
- The base-type change touches the frozen codec's class row, so
  `FrozenCodecRoundTripTests` is the load-bearing suite for A2.

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
