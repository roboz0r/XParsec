# File-order scoping: residuals

Picks up where `src/XParsec.FSharp.SemanticAnalysis/docs/top-down-registration-plan.md`
left off. That plan is complete and its doc should be deleted once this one is opened.

## What landed

F# declaration scoping is strictly ordered: a declaration sees what is above it, never
below, with two recursive-group exceptions (`type X = … and Y = …`, and a type's members).
Registration previously granted **whole-file forward visibility** — nine scans over the
element list, with `inherit` resolved by a late pass *explicitly so a parent declared later
would resolve*.

| commit | what |
|---|---|
| `7c4d7433` | Registrars are handed the identity their claim established. Fixed three silent-drop bugs (an arity-overloaded class lost its `inherit` clause; an arity-overloaded record/union lost its augmentation block) and stopped a rejected duplicate's members leaking onto the surviving claimant. |
| `b67148ff` | Nine registration scans → one top-down scan over `type … and …` groups. Type-to-type forward references rejected. |
| `f7acdd0e` | Compile the unification engine + `Translate` ahead of name resolution. Behaviour-neutral. |
| `34c93e37` | A type's structure (fields, case fields, abbrev RHS, `val`, signatures) translates where it is registered. Deleted the whole-file fill pre-passes, the placeholder-`TyVar`+`Link` dance, and the `checkTypesInScope` guard. Struct-field cycles now detectable. |
| `c7cb1b70` | A module `let`'s type annotations are classified where the `let` is written. |
| `5f7daadd` | Desugar a class preamble's initialisers and base-ctor arguments (fixed a hard crash). A type body's two-tier ordering needed no change — it already holds by construction. |

Registration is now **two** scans: the `noteNominalTypeNames` pre-scan (which must stay —
the `…Module` suffix rule needs the unit's whole nominal name set before the first key is
minted) and the single top-down scan.

**The load-bearing insight**, worth not re-deriving: the deferral of field types to a
whole-file pre-pass was a **compile-order artefact**, not a design. `Translate.fs` compiled
after `TypeRegistration.fs`, so registration *could not call* `translateType`; forward
visibility was the consequence, and `fillRecordFieldTypes`'s doc comment rationalised it
after the fact. Exactly one reference (`Translate.fs` → `NameResolutionTypeHeadStamp`)
pinned that order. Hoisting one file made the reorder free.

**The mechanism to reuse:** `NameResolutionTypeHeadStamp.classifyTypeHead` renders ONE
verdict per written type head — `LocalType` (a claim is in scope; not stamped,
`translateType` reads the registry), `ExternalType` (stamped into `ResolvedTypeHead`),
`UnknownType` (names nothing ⇒ diagnosed). `translateType` lets a **stamp outrank the
registry**, which is sound because a stamp can only exist for a head that had no claim in
scope *where it was written*. Diagnostic and binding are therefore the same verdict and
cannot disagree. Any new scoping work should extend this, not add a parallel checker.

---

## Residual 1 — everything below a declaration can still see it (THE BIG ONE)

Registration is now file-ordered, but *resolution* is not. Every by-NAME lookup face on
the registry answers against the registry as it stands **when the query runs**, and by the
time any body is walked the whole file is registered. So a name declared below a use is
still found.

Type *heads* escape this only because `classifyTypeHead` renders its verdict **during**
registration, where "claimed so far" is a live fact. Every other surface resolves later.

### What F# actually does (probed, `dotnet fsi`)

Six programs, six verdicts. These are the pinning tests; do not infer them, they are
already probed.

| # | program | F# |
|---|---|---|
| P1 | `let f () = g ()` above `let g () = 1` | **FS0039** 'g' is not defined |
| P2 | a class member calling a module `let` **below** it | **FS0039** 'helper' is not defined |
| P3 | a class member calling a module `let` **above** it | accepted |
| P4 | `match x with Alpha -> …` above `type U = Alpha \| Beta` | **accepted** — `Alpha` is a *variable pattern* (FS0049 + FS0026 "rule will never be matched") |
| P5 | `let f () = { a = 1 }` above `type R = { a: int }` | **FS0039** record label 'a' is not defined |
| P6 | every one of P1–P5 wrapped in `module rec` | **accepted** |
| P7 | `let mk () = Foo(1)` above `type Foo(n: int)` | **FS0039** 'Foo' is not defined |
| P8 | `let s () = Foo.Bar` above `type Foo` with `static member Bar` | **FS0039** 'Foo' is not defined |

P4 is not a quirk to be tolerated, and the "accepted" is not a leniency — it is the pattern
grammar working. `U.Alpha` is not in scope, so `Alpha` names nothing, so it is a variable
pattern, which is what an unrecognised ident in pattern position always is. That is the
behaviour to pin. Asserting an *error* there would be pinning a rule F# does not have.

P6 is the load-bearing one. **We currently compile every module as if it were
`module rec`.** `LocalModules` (short-name-keyed, whole-file) and
`prebindModuleFunctionSchemes` (a whole-file pre-loop in `Unification.walkElems`) are not
stray hacks — they are an *unconditional* implementation of a feature F# makes opt-in. The
parser already records the flag (`DeclarationParsing.fs:168`,
`ProgramStructureParsing.fs:28`; `CstWalk.fs:1187,1213` already destructures `isRec`).

`module rec` / `namespace rec` are wanted eventually, and `VisibleFrom` is shaped so they
cost almost nothing: the flag moves the field earlier and the lookup never branches. Types
therefore keep working under `module rec` across Steps 2–4 for free. Full `rec` semantics
are **not** in scope for this work — Step 5 gates the *value* grant, and anything beyond
that is a later job.

### The fix: `VisibleFrom` on the claim, a use-site position on the query

`NodeKey.Offset` (`NodeKey.fs:151`) **is** the source offset, and one `PassContext` covers
one file, so node keys are already a total order in file position. Nothing new is minted.

Store on each local claim a single `VisibleFrom: int`, and make every by-name lookup face
take the use site's position. The whole rule is then one comparison:

> a claim is visible at use `U`  ⟺  `claim.VisibleFrom ≤ U.Offset`

`VisibleFrom` is:
- the offset of the **first token of the claim's `type … and …` group** (the `type`
  keyword) — *not* the individual type's own offset; or
- the offset of the **innermost enclosing `rec` module**, when there is one.

Two consequences fall out for free, which is the reason for this shape:

- **The recursive-group exceptions stop being exceptions.** A member body inside the group
  sits textually after the group's `type` keyword, so it sees its own type and its
  `and`-siblings by containment. A use above the group does not. No contiguous-ordinal
  range, no special case.
- **`module rec` is a one-field change.** It moves `VisibleFrom` earlier; the lookup does
  not branch.

The same field extends to value bindings (`LocalModules` entries), which is what fixes P1
and P2 while keeping P3 and P6.

**Why a use-site position and not a bound threaded through the walk:** visibility becomes a
property of the **query**, never of ambient walk state. A pass that queries the registry
cannot forget to set a bound, because there is no bound to set — and a forgotten bound
fails *silently*, by accepting too much. It also means the answer no longer depends on
*when* a node is visited, only on *where it is*, which is what makes the walk orders below
free to change.

### Steps (independently landable)

**Step 1 — plumbing, behaviour-neutral.**
Add `VisibleFrom: int` to the claim (`TypeIdentity` / `ClaimedTypeDefn`), populated at
registration from the group's first token, or from the innermost enclosing `rec` module
when there is one. Introduce a use-site position type that **only a non-synthetic
`NodeKey` can produce** (see the synthetic-key hazard below) and thread it into the by-name
faces, with every existing call site passing an explicit unbounded value. Nothing resolves
differently yet. Land it green.

**Step 2 — the kind-agnostic name table.** Flip `tryTypeClaim` / `isTypeNameInScope` to
honour `VisibleFrom`, and pass a real position from the callers that resolve a *written*
head (`classifyTypeHead`, `Translate`, `MemberRegistration`'s `inherit` split). A caller
that observes the finished registry, or asks a question with no use site (`isTypeClaimed` —
a duplicate is a duplicate wherever written), keeps `unbounded`.

**Behaviour-neutral**, and that is the point: type heads were *already* scoped, by
registration-time classification. What this buys is that `Translate` can no longer disagree
with `classifyTypeHead` even if the walk order changes — the scoping stops depending on
when the scan runs. Pins: the recursive-group controls (a member body naming its own type;
an `and`-sibling in a member signature and in a member body), which are what prove the
group's-first-token anchoring works by containment.

**Step 3 — the kind indexes.** Flip `tryKeyOfArity` / `tryKeyOfBareName`, the funnel for the
`*Names → TypeKey` indexes. Flipping the helpers only makes them *able* to refuse; the work
is replacing `SourcePos.unbounded` with the use site at each caller — `Scope.fs`'s two
suppression sites (the diagnostic) and `InferCtor` / `InferIdentExpr` (the binding). No new
predicate, no conjunction: the suppression fires *because* `tryClass` found a class, so once
`tryClass` misses at the use site, diagnostic and binding fail together by construction.
Pins: **P7, P8**.

`tryEnum` needs its own flip — `PassContextTypes.Enum` is bare-name-keyed and never touches
the funnel. Cheap: an enum's claim sits in `TypeClaims` like any other kind's, so it can gate
on `tryTypeClaim … name 0`.

**Step 3b — put `FieldIndex` and `CtorIndex` behind faces.** These are **not registry faces
at all**: they are raw `Dictionary` field reads off `ctx.Types` (`InferResolve.fs:123,134,144`,
`InferPat.fs:40,175`, `Scope.fs:181,220`, `Elaborate/Patterns.fs:51,168`,
`Elaborate/Resolve.fs:397`). They take no `SourcePos` because they are not functions — so
neither P5 (record label → `FieldIndex`) nor P4 (union case → `CtorIndex`) is reachable from
`tryKeyOfBareName`, and Step 3's "one edit covers every kind index" is true only of the
`*Names` indexes.

This is also a hole in the by-name/by-key split: the property that split was for — *an
unscoped by-name read is impossible to write by accident* — does not hold for a raw dictionary
read, because there is no signature to demand a position. Putting both indexes behind faces
that require a `SourcePos` is what closes it, and it is the prerequisite for Steps 4 and 5,
not an optional tidy-up.

Pins: **P5**.

P7/P8 land HERE, not in Step 2 (an earlier draft of this doc said Step 2, and was wrong —
confirmed empirically). `Foo(1)` and `Foo.Bar` never touch the name table: both resolve
through `TypeRegistry.tryClass`, i.e. `tryKeyOfBareName`, a **kind index**. The name table
answers *is a local type of this name visible here*; the kind index answers *which class*.

Do not be tempted to pin P7/P8 early by gating the suppression logic in
`NameResolution/Scope.fs` on `isTypeNameInScope` while the kind index stays unscoped. That
was tried. It makes the *diagnostic* right while *binding* still resolves to the class
declared below — a second checker beside the resolution it guards, which is precisely what
"the diagnostic falls out of resolution failing" exists to avoid. Scope the index; the miss,
and therefore the diagnostic, follow.

**Step 4 — `unionOfCase`.** A separate index from the `*Names` dictionaries, so it needs
its own flip; it rides on Step 3b's `CtorIndex` face. Pins: P4 — assert that the unrecognised
uppercase ident becomes a **variable pattern**, not an error.

**Step 5 — values, and the `module rec` gate.** Give `LocalModules` entries a
`VisibleFrom`; gate `prebindModuleFunctionSchemes`' forward grant on the enclosing module's
`isRec`. Pins: P1, P2, P3, P6.

**Step 6 — performance, not correctness.** With visibility carried by the query,
`NameResolution.walkElems`' four kind-batched loops (registration → all class bodies → all
nominal bodies → all module elems) no longer encode anything. Collapse them to one. This is
a **perf** change and must be justified as one; it is not needed for any of P1–P8.

### Hazards

**Synthetic keys, and why kind cannot discriminate them.** `NodeKey.ofSynthetic` is called
with two incompatible things. Sometimes it gets a genuine **spawning source offset**
(`ElaborateExpr.fs:431`); sometimes it gets a **monotone counter** packed into the offset
slot for uniqueness (`TastLower.mintSyntheticParamKey`, and the `InlineExpansion` pass's
minter that `Inline.freshen` splices with). A counter is not a source position, so a scoped
lookup from such a node would compare garbage against a claim's `VisibleFrom`.

The tempting guard — refuse `IsSynthetic`, or allowlist kinds — is **both unsound and
over-broad**:
- *Unsound*: `SynthLambdaBody` is minted **both** ways, counter-packed at `TastLower.fs:360`
  and offset-packed at `ElaborateExpr.fs:431`. Kind tells you nothing.
- *Over-broad*: a spawning-offset synthetic (a desugared app, a `this` binder) has a real
  position and **should** resolve there. Refusing it would leave desugared nodes unable to
  resolve names at all.

Discriminate on the **value**. A source offset indexes into a `string`, so a *negative*
offset is a truly uninhabited domain — no real position can ever be one. Counter-based
minters pack into **negative** 32-bit space (set bit 31 of the offset slot); `SourcePos`
is constructible from a key **iff `Offset ≥ 0`**, and kind and the syn bit become irrelevant
to it. The invariant is then checkable rather than a convention about which kinds are safe —
which matters, because the "two kinds" the first draft of this doc named were already an
incomplete list.

**The `…ByKey` faces need no change.** A key already names a resolved type; there is no
scoping question. It is exactly the by-name faces that must take a use site: `tryTypeClaim`,
`isTypeNameInScope`, `tryClass` / `tryClassArity`, `tryRecord` / `tryRecordArity`,
`tryUnionBare` / `tryUnion`, `tryAbbrev` / `tryAbbrevArity`, `tryEnum`, `unionOfCase`.
Making precisely those require a position is the correct-by-construction split.

**Not a landmine any more, but do not disturb it in Steps 1–5.** `Unification.walkElems`'
third loop runs in declaration order to satisfy a module↔class dependency: a class member
calling an *earlier* module function needs its real generalised scheme, and a *later*
module function over the class needs the member's already-typed body. Batching either way
breaks one direction. After Step 5 that ordering no longer carries scoping information —
but it still carries the typing dependency, so Step 6 must leave it alone.

---

## Residual 2 — instance `let` bindings in a class body are not modelled at all

`MemberRegistration.extractStaticLets` (`MemberRegistration.fs:446-481`) admits only
`static let`. An **instance** `let`, and any `[static] do`, is silently skipped with no
diagnostic. So this — which F# accepts and runs — is rejected with
`Unresolved identifier: a`:

```fsharp
type C() =
    let b = 1
    let a = b + 1
    member _.A = a
```

A missing **feature**, not an ordering bug: it needs a backing field, ctor-init lowering,
and codegen in both backends. Pinned as a test asserting the wrong current behaviour, with
a comment naming what F# does (the `LocalModuleTests` convention).

Note the rejection *direction* for `let a = b` (a later `b`) happens to agree with F#, but
**for the wrong reason** — the error lands on `a` at its use site rather than on `b`,
because the preamble RHS is never walked. Do not read that agreement as the rule working.

Adjacent, and already fixed in `5f7daadd`: `Desugar` walked only `ObjectModelBody.elements`,
never `.classPreamble` or `.inherits`. Any **operator** in a `static let` initialiser, a
`do` body, or a primary `inherit Base(a + 1)` argument therefore reached Elaborate with no
`DesugaredForm` entry and threw `failwithf "InfixApp … missing DesugaredForm entry"` — a
hard crash, not a diagnostic. It survived because every existing `static let` test used a
*literal* initialiser. Worth remembering when adding instance lets: the same three
positions must be walked by every pass, and a test with a literal-only initialiser will not
catch it.

## Residual 3 — `[<Struct>]` records and unions emit as reference types

A `[<Struct>]` **record or union** registers as an ordinary `RecordTypeInfo` /
`UnionTypeInfo` with **no value-type flag** — only `ClassTypeInfo` has `IsValueType`. So a
struct record currently emits as a reference type.

Surfaced by the struct-field cycle check (`MemberRegistration.checkGroupStructFieldCycles`),
which therefore has to read struct-ness off the **CST attributes** (`isValueTypeDefn`)
rather than the registry. The cycle detection is correct; the emission gap is real and
untouched. Fixing it means putting `IsValueType` on the record/union infos and honouring it
in both backends.

---

## Residual 4 — record augmentation members are unreachable by dot-access

`InferRecordAccess.resolveFieldStep`'s `TyRecord` arm consults `info.Fields` and never
`info.Members`. So:

```fsharp
type Foo = { a: int }
    with member this.Bar () = 1
// v.Bar()  =>  "Type 'Foo' has no field 'Bar'"
```

Pre-existing, orthogonal to the scoping work, found while pinning the duplicate-type
member-lookup diagnostic. The diagnostic is at least clean (no crash), but the member is
simply not reachable.

---

## Residual 5 — a narrowing that is now cheap

`SemanticInfo.fs` notes that the type IR's nominal payloads (`SemType.TyClass/TyRecord/
TyUnion`, `FrozenType.FTClass/…`) still carry a `SymbolKey` rather than a `TypeKey`, so
three sites must narrow and fail loud on the impossible arm
(`Elaborate.Resolve.nominalDeclKey`, `Inline.nominalHeadKey`, `EmitResolve.nominalTypeKey`).

Registration now routes every local type through one mint and one claim, so the "two keys
for one type" hazard that made this risky is much reduced. Worth revisiting.

---

## Also outstanding

`docs/thermo-review-938dd9da34.md` holds the rest of the review backlog from the
`SymbolKey` reshape — none of it was addressed by this work. Its headline (findings 1 and 2)
is that the provider stores and `TypeRegistry.TypeClaims` are still addressed by a
flattened *string* projection of the structured key, with a `#if DEBUG` tripwire
(`Engine.fs:174` `checkAsmInvariant`) policing the gap and **not shipping in release**.
`TypeClaims` being name-keyed is directly adjacent to Residual 1 — if you are adding a
claim ordinal anyway, keying that table by `TypeKey` at the same time is the natural pairing.

## Working notes for whoever picks this up

- **Probe, don't infer.** `dotnet fsi --nologo x.fsx` on a throwaway script under
  repo-root `./tmp/` is the oracle for any F# semantic question. This caught a real design
  bug: a cycle check over *all* field edges would have rejected
  `type A = { x: B } and B = { y: A }`, which F# **accepts** (reference-type indirection
  breaks the cycle — only *struct-field* and *inheritance* edges cycle, FS0954). Reasoning
  missed it; the probe caught it. Codes seen: FS0037 duplicate type, FS0039 not defined
  (also what a forward reference reports — there is no special forward-reference error),
  FS0953 cyclic abbreviation, FS0954 cyclic struct-field/inheritance.
- `dotnet fsi` is fine. `Build` / `Test` must go through `./claude_tools.cmd`, and test
  suites must be run **serially** — concurrent runs race on a shared assembly and report
  bogus failures.
- When pinning a shadowing case, assert the **resolved identity**, not just acceptance —
  compare against the same program with the local declaration removed. Pinning acceptance
  alone lets a miscompile survive a green suite, which is exactly what happened before
  `34c93e37`.
