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

## Residual 1 — expression-level references still see the whole file (THE BIG ONE)

We accept programs F# rejects:

| program | F# | us |
|---|---|---|
| `let mk () = Foo(1)` above `type Foo(n: int)` | `FS0039` The value or constructor 'Foo' is not defined | **accepted, zero diagnostics** |
| `let s () = Foo.Bar` above `type Foo` with `static member Bar` | `FS0039` The value, namespace, type or module 'Foo' is not defined | **accepted, zero diagnostics** |

This is a **different surface** from everything above. Steps 4 and 5 work because type
*heads* are classified during registration, where "what has been claimed so far" is a live
fact. Constructor and static-access resolution happen in `InferIdentExpr` / ctor resolution
during the **body walk**, which runs after the entire file is registered and has no notion
of "declared above me" at all.

### The proposed fix: a claim ordinal bound

Give each claim a source ordinal (its index in `ClaimedTypeDefns`; a `type … and …` group
occupies a contiguous range). Thread a visibility bound through the body walk — "resolve
only against claims at or before my group's last ordinal" — set per element in the
declaration-order loop that **already** iterates elements in order setting
`ctx.Resolution.OpenScope`. It is the same loop, one more assignment.

The registry's lookup faces (`tryTypeClaim`, `tryRecordArity`, `tryKeyOfBareName`, …) are
already funnelled through the generic `tryKeyOf*` mechanism in `TypeRegistry.fs`, so the
bound goes in **one place**, not scattered across call sites.

A below-declared type then simply **misses**, and resolution falls through to the external
probe on its own — same shape as the type-head fix, so the diagnostic falls out of
resolution failing rather than needing its own checker.

**Landmine.** The body walk (`fillClassMembers` / `fillNominalMembers` / `walkModuleElem`
in `Unification.walkElems`; `walkClassBodies` / `walkNominalBodies` / `walkModuleElem` in
`NameResolution.walkElems`) runs in **declaration order to satisfy a module↔class
dependency**: a class member calling an earlier module function needs its real generalised
scheme, and a later module function over the class needs the member's already-typed body.
Batching either way breaks one direction. Adding a bound to that loop is fine; **reordering
it is not.**

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
