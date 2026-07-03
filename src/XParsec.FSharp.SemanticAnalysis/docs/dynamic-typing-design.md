# Dynamic typing (`dynamic`) — a disciplined F# `any`

**Status (2026-07-03).** IMPLEMENTED (core cut — the implicit-escape warning is a
staged follow-on, not yet built). Superseded the `TyDynamic`
front-end DU-case approach spiked under "Wall 2" of
[`ts-provider-implementation-plan.md`](ts-provider-implementation-plan.md) (that spike
is to be **reverted** — see *What reverts* below). Premises confirmed with the user and
a scout of the inferencer; open micro-decisions flagged inline with **[OPEN]**.

## Motivation

TS `any` must land somewhere on a Vesper value. TS treats `any` as fast-and-loose —
it flows implicitly into and out of every concrete type, and `.member` access is
unchecked. **Vesper does not.** F# is not that loose with types, and we respect that in
spirit: `dynamic` is a real, *opaque* type you must explicitly enter and explicitly
leave. The escape hatch is the F# dynamic-access operator `?` (with target typing), or
an explicit cast — never silent flow.

## The type: `dynamic` is a plain opaque JS intrinsic

```fsharp
// prim-types-dynamic.js.fsi   (files-js, JS-only — no CLR analog)
type dynamic = extern
// prim-types-dynamic.js.fs
type dynamic = (# "any" #)
```

Modelled exactly like Wall 1's `undefined`: a JS-only intrinsic scoped to the
manifest's `files-js` list, harvested (via the `files-<t>` companion harvest added in
Wall 1) as both marker and platform face. Canon identity `dynamic`; JS platform tag
`"any"`. It carries **no special unifier behaviour** — it unifies with itself by name,
like `int` or `string`, and with nothing else. There is deliberately **no `TyDynamic`
SemType/`FTDynamic` FrozenType DU case**: `dynamic` is `TyConst("dynamic")` /
`FTConst("dynamic")` everywhere, so it needs no threading through the child-walk
skeletons and mints no new absorb/subtype rules.

### No free interconversion (the F#-discipline premise)

- `let d : dynamic = someInt` — **rejected** (an `int` is not a `dynamic`). You enter
  `dynamic` from a TS-`any`-typed value, or explicitly via the `dynamic` conversion
  function (below).
- `let n : int = d` — **rejected**. You leave `dynamic` through `?` (target-typed);
  whole-value exit **[OPEN]**.
- `d.foo` (dotted) — **rejected**. `.` is for statically-known members; `dynamic` has
  none. This is the load-bearing F#-fidelity point.

`dynamic` therefore has *zero* assignability edges in `subsumes`. All of its behaviour
is in the `?` operator and the `dynamic`/`?` intrinsics.

### `retype`, and entering/leaving `dynamic`

The underlying primitive is FSharp.Core's general erasing reinterpret — added to
Vesper.Core as an intrinsic:

```fsharp
let inline retype<'T,'U> (x:'T) : 'U = (# "" x : 'U #)
```

The empty-string intrinsic `(# "" x : 'U #)` is the identity cast: it emits the value
unchanged and re-types it `'U` (the explicit `: 'U` annotation *is* the reinterpret
target). This is inherently unsafe (no runtime check) — the escape valve, not the
common path.

`dynamic` (entry) and whole-value exit are `retype` at a fixed result type:

```fsharp
// ops-dynamic.js.fsi — entry convenience, shares the type's name like int/string/box
val inline dynamic : value: ^T -> dynamic
let inline dynamic (value: ^T) : dynamic = retype value   // (# "" value : dynamic #)
```

- **Enter:** `dynamic x` — same JS value, retyped to `dynamic`. `x` alone never flows in
  silently.
- **Member exit:** `x?foo` (target-typed) — the principled, *checked-by-you* path.
- **Whole-value exit:** `retype d : int` — the explicit, unchecked escape (resolves the
  earlier open question; `retype` is the general cast, so no bespoke `undynamic` needed).

**[VERIFY at impl: the `(# "" x : 'U #)` identity-intrinsic lowering is honoured on the
JS backend — it should emit the operand verbatim.]** **[OPEN: is `retype` public
`[<AutoOpen>]` surface (a general unsafe cast, as FFI-heavy JS code may want) or
internal, with only `dynamic`/`?` exposed? FSharp.Core keeps `retype` internal.]**

## The operators: `?` and `?<-`, SRTP with a `dynamic` default

```fsharp
// ops-dynamic.js.fsi   (files-js)
val inline (?)   : target: dynamic -> name: string -> ^TResult
                       when default ^TResult : dynamic
val inline (?<-) : target: dynamic -> name: string -> value: ^TValue -> unit
```

`x ? ident` desugars (F# spec 6.4.5, already what Vesper's parser does) to
`(?) x "ident"` — the member name is a compile-time **string** literal. Then ordinary
inference + the existing SRTP `default`-constraint machinery
(`InferGeneralize.applyDefaults`, a real fixpoint defaulting pass — confirmed general,
not int-hardcoded) does the rest:

- **Unconstrained** context → `^TResult` defaults to `dynamic`. So `x?a?b` chains stay
  dynamic — "infectious", but *only through `?`*, never through assignability.
- **Pinned** context (`let n : int = x?foo`, `x?foo + 1`, an `int` parameter position)
  → `^TResult` unifies to the pinned type *before* generalisation, the default never
  fires, and `?` is the **principled escape back to static**.

`?<-` is the setter (`x?foo <- v`), in scope for the first cut.

### Implicit escape warns; explicit escape is silent **[DECIDED — STAGED follow-on; not in the first cut]**

Target-typing lets `dynamic` escape to a concrete type through *context* — `d?foo + 1`
forces `^TResult = int` via the arithmetic. That is an **unchecked assertion** (the
compiler cannot verify `d.foo` is really an `int`), so it is a candidate for an
**implicit-conversion warning** (mirroring F#'s posture on `op_Implicit`), suppressible
two ways:

- **`#nowarn`** on the relevant warning number — blanket opt-out.
- **An explicit type annotation on the `?` expression** — `(d?foo : int) + 1`. You are
  *saying* the type, so no warning.

Mechanically the warn case is precisely "a `?`-originated `^TResult` that
`applyDefaults` found already solved to a non-`dynamic` concrete type" — i.e. the
default did NOT fire. Implementation needs (a) tagging a `?`-result tyvar with its
origin so the concretisation is detectable, and (b) a suppression signal from a direct
`(… : T)` ascription wrapping the `DynamicLookup`.

**The definitional fork — what suppresses:**
- *Recommended (syntactic, simple):* only a type annotation **directly on the `?`
  expression** — `(d?foo : int)` — suppresses. `let n : int = d?foo` (annotation on the
  binding, not the expression) still **warns**, nudging `let n = (d?foo : int)`. One
  teachable rule: "name the type at the escape point." Matches your `(d?foo : int)`
  example exactly.
- *Alternative (looser):* any expected-type annotation reaching the expression —
  including `let n : int = …` and an `int` parameter position — suppresses; only
  genuinely inference-derived escapes (`d?foo + 1`) warn. More permissive, but needs
  provenance tracking on the expected type (harder to get right).

**Recommendation:** treat the warning as a **staged follow-on**, not part of the first
cut. Ship infectious-default + target-typing first (it works with zero new machinery);
add the warning once the core is proven, since it is the only part that needs new
tyvar-origin tagging + suppression plumbing. Decide the syntactic-vs-loose fork then.

### Receiver is `dynamic`, not `obj` **[DECIDED — strict `dynamic` receiver]**

FSharp.Interop.Dynamic types `(?)` as `obj -> string -> 'TResult` (permissive: `?`
works on anything). We recommend the **stricter** `dynamic -> …`: `?` is valid only on a
`dynamic` receiver, so you cannot `?`-probe a statically-typed value by accident — you
must first *be* in `dynamic`. This matches "you must go through `d?foo` / a cast."

### JS emission — bracket form

`(?)` inline body emits computed member access `$0[$1]` → `x["foo"]` (safe for any
name). `(?<-)` emits `$0[$1] = $2` → `x["foo"] = v`.
**TODO (prettify only):** when `name` is a valid JS identifier, emit dotted `x.foo`
instead of `x["foo"]`. Purely cosmetic on the output JS; not semantic.

### No FID reflection (JS-only for now)

FSharp.Interop.Dynamic inspects `typeof<'TResult>` at runtime to choose property-get vs
method-invoke via the DLR. On JS we need none of that: `x?foo` is a property get
(`x["foo"]`), and `x?foo(args)` is just `App` over that get (`x["foo"](args)`) — the JS
runtime *is* the dynamic dispatch. **FID-style reflection is the CLR story and is
deferred** (a CLR `dynamic` would need a `Dynamitey`-like runtime binder); our first and
only target here is JS.

## Extractor / provider

The extractor already maps TS `any → Schema.TypeRef.Dynamic` (serialised
`{"k":"dynamic"}`, committed golden) — unchanged. Only the F#-side deserialize changes:
`Schema.TypeRef.Dynamic` → **`FTConst("dynamic")`** (was the `FTUnknown "any"` TODO,
briefly `FTDynamic` in the reverted spike). So a TS-`any`-typed member/param/return
arrives as the opaque `dynamic` intrinsic, and the only thing you can do with it is `?`.

## What reverts (from the Wall 2 spike)

All uncommitted. Keep as reference, then unwind:
- `SemanticInfo.fs`: remove `TyDynamic`/`FTDynamic` cases + every skeleton/fold/bridge
  arm they forced.
- `Engine.fs` (`unify` absorb, `checkConstraint → Satisfied`), `Subsume.fs`
  (`TyDynamic` subtype arms) — remove; `dynamic` has no special unify/subsume behaviour.
- `InferRecordAccess.fs` — the `TyDynamic -> TyDynamic` field arm is already reverted;
  keep it reverted (`.foo` on `dynamic` errors).
- `Infer.fs` / `FreezeExpr.fs` — the native `DynamicLookup → FieldGet` arms get replaced
  by the `DynamicLookup → (?)`-call desugaring.
- `ClrEncoder.fs` / `EmitResolve.fs` / `ExternalSymbols.argTypeName` / `Regions.fs`
  forced arms — drop (no DU case to match).
- `CstKeys.firstTokenOfExpr` `DynamicLookup` arm — **keep** (node-keying is needed
  regardless of how the node is later elaborated).

## What builds

1. `dynamic` intrinsic — `prim-types-dynamic.js.fsi` + `.js.fs`, `files-js` entry.
2. `retype` (general reinterpret, `(# "" x : 'U #)`) + `(?)` / `(?<-)` + the `dynamic`
   conversion — `ops-dynamic.js.fsi` + a JS inline body (`ops-dynamic.js.fs`,
   `inline-bodies-js`): `?`/`?<-` emit `$0[$1]` / `$0[$1] = $2`; `dynamic value` =
   `retype value`. (`retype` may live in a more general ops file if made public — see
   surface [OPEN].)
3. Desugar `Expr.DynamicLookup(recv, ?, ident)` → `(?) recv "ident"`, and
   `Assignment(DynamicLookup(...), v)` → `(?<-) recv "ident" v`. **[OPEN: desugar site —
   a Desugar pass vs an `Infer`/`Freeze` arm that emits the operator App. The operator
   route is what unlocks SRTP target-typing; a bespoke arm returning `dynamic` would
   NOT.]**
4. `TsManifestTypes.fs`: `Schema.Dynamic → FTConst("dynamic")`.

## Verification / test plan

- `let d = getAny(); let y = d?foo` — `y : dynamic` (default fired).
- `let d = getAny(); let n : int = d?foo` — type-checks, `n : int` (default did NOT
  fire; the first non-int `default` target exercised anywhere — the scout flagged this
  as previously untested).
- `d?a?b` chains stay `dynamic`; emits `d["a"]["b"]`.
- `d.foo` (dotted) — ERROR.
- `let n : int = d` — ERROR (no assignability edge).
- `d?foo <- v` — emits `d["foo"] = v`; observable under Node.
- End-to-end: a `dynlib` manifest with an `any` member/param, emit + Node round-trip.

## Open questions (decide before/while implementing)

1. **[DECIDED]** `(?)` receiver — **`dynamic` (strict)**. You cannot `?`-probe a
   statically-typed value; you must first *be* in `dynamic`.
2. **[OPEN]** `retype` surface — public `[<AutoOpen>]` (a general unsafe reinterpret,
   which FFI-heavy JS code may want) vs internal-only with `dynamic`/`?` as the public
   face (FSharp.Core keeps `retype` internal). Whole-value `dynamic -> 'T` exit is
   settled: it is `retype d : 'T`.
3. **[OPEN]** Does the inline machinery accept a statically-resolved `^TResult` whose
   *only* constraint is `default` (every existing SRTP typar also carries a member
   trait)? And does a 3-arg assignment template `$0[$1] = $2` lower correctly? Both are
   implementation-time verifications, not design forks.
4. **[DEFERRED]** CLR `dynamic` (FID-style runtime binder). Out of scope; JS-only now.
