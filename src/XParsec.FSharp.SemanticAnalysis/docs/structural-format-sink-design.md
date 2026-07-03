# Structural-format sink redesign — semantic ops

Extends `IFormatSink` (`src/Vesper.Core/structural-format.fsi`) with a
*semantic* vocabulary (`BeginRecord`/`Field`/`BeginCase`/`Child`) that the
compiler-synthesised `Format` bodies call exclusively, while keeping the
existing *layout* vocabulary (`Text`/`Line`/`BeginGroup`/…) for hand-written
`IStructuralFormattable` implementors. **Scope: records and DUs** — the
per-type synthesised bodies. Collections, tuples, and seqs are *not* per-type
synthesised; they ride `Child(obj)` into the runtime dispatcher
(`FormatEnumerable`/`FormatTuple`), so they need no new ops. Child values stay
`obj`-typed throughout this pass; a typed-`Child` overload family for the BCL
primitives is a deliberately **deferred** extension (see "Deferred" below) —
we are not doing two things at once.

## Why (three forces)

1. **Versioning.** Today the synthesised body replays the layout grammar
   (`StructuralFormatRecipe.recordRecipe`) as literal sink calls — the exact
   `{ F = · }` spacing, the `+2` hang, when parens appear — so the `%A` output
   policy is compiled into every assembly, forever. If the policy ever changes,
   old assemblies keep the old layout. For the upstream pitch (FSharp.Core
   under `--reflectionfree`, where `%A` is currently a compile error —
   `CheckFormatStrings.fs:470` — and records/unions lose `ToString` —
   `IlxGen.fs:10938`; both are lines into dotnet/fsharp and drift between
   revisions, so pin them to a commit before citing in the RFC) this is
   disqualifying: FSharp.Core targets
   netstandard2.0, no default interface methods, the frozen surface must be
   right the first time. Semantic ops encode what a record/union *is*, which
   is stable by definition; the layout policy moves into the runtime where it
   can be patched.

2. **Emitted-body size.** An *n*-field record today costs ~4+6n `callvirt`s
   plus a `box` per field (`EmitStructuralFormat.lowerOp` +
   `sinkFormatField`); semantically it is `BeginRecord; (Field; Child)×n;
   EndRecord` = 2+2n calls, still one `box` per field (`Child(obj)`). A
   payload-*k* union arm drops from ~5+6k to 2+k. The call-count collapse is
   the win here; the box stays until the deferred typed-`Child` atoms land.
   This is also the concrete number for the upstream "what does this cost per
   type" question.

3. **Fidelity under erasure (why the interface leaves room for typed atoms).**
   An `obj`-typed child erases the static type. On the CLR the box preserves
   the runtime type so the dispatcher recovers it — `Child(obj)` is *correct*
   here, which is why obj-only is the right surface for this pass. On JS the
   box cannot preserve it — char vs 1-char string and int vs integral float are
   indistinguishable (the documented erasure corners in
   `structural-printer.js.fs:39-43`). A future per-type JS `Format` emitter (the
   opt-in sketched in `StructuralFormatRecipe.fs:8-9`) would need typed `Child`
   overloads to carry the static type through the call. That emitter does not
   exist yet, so the typed atoms are **deferred**, not designed away: `Child` is
   the seam they slot into when it lands. Nothing in this pass depends on them.

## The extended surface

Layout ops — **unchanged**, for custom implementors, who thereby pin their own
layout (their choice, documented): `Text`, `Line`, `SoftBreak`,
`BeginGroup`/`EndGroup`, `BeginNest`/`EndNest`,
`BeginApplication`/`EndApplication`, `FormatChild(obj)`, `FormatArg(obj)`.

Semantic ops — the only thing synthesised bodies call:

```fsharp
abstract member BeginRecord: unit -> unit
abstract member Field: name: string -> unit   // label marker; value follows via Child
abstract member EndRecord: unit -> unit
abstract member BeginCase: name: string -> unit
abstract member EndCase: unit -> unit
abstract member Child: value: obj -> unit     // sole child entry this pass; typed overloads deferred
```

Two protocol decisions do the compression:

- **`Field(name)` is a marker, not `Field(name, value)`.** The value arrives
  in the *next* `Child` call. Besides being the natural shape, this keeps the
  *eventual* typed-`Child` family (deferred) to one set (~15 methods) instead
  of one set per position (`Field`×`Arg`×`Child` would be ~45). The sink is
  already a stateful frame stack; a pending-label frame
  is natural — with one invariant it must honour: `Child`, on entry, flushes
  the pending label into the `Doc` *before* it recurses into the value.
  Otherwise a nested record's first `Field` clobbers the outer pending label.
  Today's recipe sidesteps this by emitting the label as a literal
  `Text "F = "` op *before* `FormatChild`; the marker moves that ordering into
  the sink, so the invariant has to be stated rather than assumed.

- **Frame context kills the `FormatChild`/`FormatArg` split.** The split
  exists only because today's body has no context — the sink can't tell a
  record field from a DU payload. Semantically the enclosing frame
  (`BeginRecord` vs `BeginCase`) says which position a `Child` is in, and
  because the sink builds a deferred `Doc` tree, the 1-vs-n payload form is
  decided at `EndCase` from the observed child count — no arity parameter
  needed up front. `unionCaseRecipe`'s three arms (`None` / `Some ·` /
  `Case (a, b)`) become runtime interpretation of one uniform call shape.
  Nullary case = `BeginCase(name); EndCase()`.

  One thing the child *count* does **not** settle: the arg-position
  parenthesisation of a **single** payload. `Some (Some 3)` parenthesises;
  `Some [1; 2]` and `Some 3` do not. The rule is "parenthesise the lone child
  iff it is itself a DU application" — a property of the child's *form*, not of
  the count (a list child never opens an application, so it never
  parenthesises). Today `FormatArg`/`ArgPending` carries exactly this bit, set
  *before* dispatch so the child's own `BeginApplication` reads it
  (`structural-printer.fs:276-279,487-489`). In the deferred model the child's
  `Doc` is already built by the time `EndCase` runs, so the interpreter instead
  marks the frame a payload-bearing `BeginCase` produces as *application-shaped*
  and, at a 1-child `EndCase`, parenthesises the lone child iff it carries that
  mark. Recoverable — but it is the one place the "just count the children"
  story is insufficient, and Phase B's byte-identical `%A` corpus is what
  catches a regression here.

This pass ships `Child(obj)` only. `box` on a reference type is a no-op
(ECMA-335 III.4.1), so every field — value, reference, or generic `'T`
(`Some of 'T`) — takes one uniform `box`, exactly as today. No generic
`Child<'T>` — a generic virtual method is its own AOT hazard, which defeats
the point.

### Deferred: typed `Child` atoms

Out of scope here; recorded so the seam is designed correctly. When the
per-type JS `Format` emitter (force 3) is actually built, add typed `Child`
overloads for the **IntrinsicRepr-encodable** primitives (bool, char, string,
sbyte…uint64, float32, float, decimal). Enumerate that set from `IntrinsicRepr`
— the authoritative encodable-primitive registry — **not** from
`DocLayout.formatPrimitive`: the two lists disagree in ways that matter for a
set you cannot cheaply extend once frozen. `formatPrimitive` carries
`IntPtr`→`n` / `UIntPtr`→`un`, which are *not* encodable
(`EmitJs.validatePlatformTypes` errors on `nativeint`), and it does not handle
`bool`/`char`/`string` at all (those dispatch in `DispatchInner`). So neither
"the `formatPrimitive` suffix set" nor "everything `formatPrimitive` touches"
is the right list. Typed atoms buy nothing load-bearing on the CLR (the box
round-trips the runtime type); they are a JS-fidelity feature, which is why
they wait for the JS consumer.

## What moves where

- `RuntimeFormatState` (`structural-printer.fs`) grows the semantic-frame
  interpreter: the *expansion* currently in `recordRecipe`/`unionCaseRecipe`
  becomes how it lowers semantic frames into the existing `Doc` builders. The
  grammar knowledge relocates from `Codegen.Common` into the runtime — which
  is where the JS printer already keeps it (the shape-keyed walker).

  Be honest about the single-source-of-truth cost. Today `StructuralFormatRecipe`
  *is* "the single source of truth for the output forms": the CLR emitter
  consumes it and the JS walker is checked against it. After this change the
  record/union forms live independently in `structural-printer.fs` (CLR) and
  `structural-printer.js.fs` (JS), tied only by the cross-target differential
  test. So the relocation is not a pure win — it trades one shared definition
  for two copies, and promotes the differential test from backstop to the only
  thing keeping the targets in sync. Acceptable (the JS walker already
  duplicated the forms), but state it rather than fold it into "moves into the
  runtime."
- `EmitStructuralFormat.fs` shrinks: `buildRecordFormat` emits the 2+2n
  semantic calls directly; the tag-switch dispatch stays; `lowerOp` and the
  `SinkOp` replay go. The emitter stays box-uniform — every field is `box`ed
  and handed to `Child(obj)`, exactly as `sinkFormatField` does today; no
  per-field `FrozenType` classification is introduced (that only becomes
  necessary when the deferred typed atoms land). `StructuralFormatRecipe.fs`
  either retires or survives as the differential test's oracle; given the SSOT
  cost above, keeping it as the shared oracle is the cheaper way to stay honest
  that both runtime printers encode the same forms.
- Golden `%A` output must be **byte-identical** — the layout policy is
  unchanged, only relocated. The existing corpus is the acceptance gate.

## Implementation — three phases, each builds green

The interface sits at the bottom of the dependency graph (`Vesper.Core`), so any
member change breaks every `IFormatSink` implementor in the same build. There
are **three** implementors, all near-duplicate Doc-building sinks:
`RuntimeFormatState` (`Vesper.Printf/structural-printer.fs:478`), the test
baseline oracle (`StructuralFormatBaseline.fs:355`), and — separately — the JS
walker (`structural-printer.js.fs`, not an interface impl but the same forms).
The emitter and recipe sit *downstream* and keep working against the unchanged
**layout** ops, so the interface+runtime change decouples cleanly from the
emitter rewrite. Stage it as three commits; each is independently green, which
keeps the "did byte-identical break?" bisect clean.

**Phase A — extend + implement (additive; `Vesper.Core` + `Vesper.Printf` + test
baseline only, no codegen).**

- `Vesper.Core/structural-format.fsi` + `.fs`: add `BeginRecord` / `Field` /
  `EndRecord` / `BeginCase` / `EndCase` / `Child(obj)`. **Leave** `FormatChild` /
  `FormatArg` and every layout op in place — this phase is purely additive.
- `Vesper.Printf/structural-printer.fs` (+ `.fsi`): implement the six new members
  on `RuntimeFormatState`, lowering them into the existing `Doc` builders — the
  record/union expansion currently living in `recordRecipe`/`unionCaseRecipe`,
  plus the two invariants from "The extended surface": the pending-label frame
  (flush-before-recurse) and the *application-shaped* mark for single-payload
  parenthesisation.
- `StructuralFormatBaseline.fs:355`: mirror the same six members (this is a
  second hand-written copy of the runtime — keep it faithful or the differential
  baseline drifts).
- **Gate:** `Vesper.Core` + `Vesper.Printf` + test project build; existing `%A`
  suite green (emitter untouched, new members unused). The new surface is now
  unit-testable directly — drive the sink by hand and assert the string — before
  any emitter depends on it.

**Phase B — emit semantic calls (`Codegen.Clr` + `Codegen.Common`; goldens
regen).**

- `Codegen.Clr/ICodegenProvider.fs`: add the six handles to `FormatSinkHandles`
  (record type, ~line 227). Keep `FormatChild`/`FormatArg` fields for now
  (removed in C).
- `Codegen.Clr/ClrRecipes.fs`: build them in the `formatSinkHandles` lazy block
  (~line 834) — `Field` = `sinkMember "Field" 1 (fun te -> te.String())`,
  `Child` = `sinkMember "Child" 1 (fun te -> te.Object())`, the four
  `Begin*`/`End*` are `nullary`.
- `Codegen.Clr/EmitStructuralFormat.fs`: rewrite `buildRecordFormat` /
  `buildUnionFormat` to emit `BeginRecord; (Field; box+Child)×n; EndRecord` and
  `BeginCase; (box+Child)×k; EndCase`; keep the tag-switch dispatch; delete
  `lowerOp` and the `SinkOp` plumbing (the `box` in `sinkFormatField` stays — see
  the box-uniform note above).
- `Codegen.Common/StructuralFormatRecipe.fs`: retire, or demote to the
  differential-test oracle (**recommended** — it is the shared record of the
  forms, and keeping it is the cheap answer to the SSOT cost above).
- **Goldens:** regen the emitted-IL expectations that literally spell the old
  sink calls (`StructTests.fs:1570,2026` and sibling baselines) *and* re-run the
  `%A` output corpus — which must stay **byte-identical**: only the emitted IL
  changes, never the printed string.
- **Gate:** full solution build; `%A` corpus byte-identical; JS differential test
  green.

**Phase C — retire `FormatChild`/`FormatArg` (second breaking change, isolated).**

- Decide first (this is the `FormatArg`-fate open question): keep the layout
  recursion entries for custom implementors, or retire them now while breakage
  is still free. If retiring: drop them from `structural-format.fsi/.fs`, from
  `RuntimeFormatState` + `StructuralFormatBaseline`, and from `FormatSinkHandles`
  + the `ClrRecipes` build block.
- **Gate:** same as B, in its own commit.

**JS:** nothing in A–C — the central walker is already semantic; the differential
test is the only JS-side obligation (keep it green in B). The deferred typed-
`Child` atoms and the per-type JS `Format` emitter are later, separate work.

## Open questions

- **Overloads vs suffixed names** (`Child(v: int)` vs `ChildInt32`) — *deferred
  with the typed atoms; decide when they land.* F#/BCL precedent favours
  overloads (`Utf8JsonWriter.WriteNumberValue`) and pickOverload exists — but
  overloads are the wrong choice for a *cross-target* frozen interface, because
  JS has no overload resolution: a JS sink implements the interface as attached
  members (one name = one function), so it cannot host `Child(obj)` *and*
  `Child(int)`. Since the JS per-type emitter is the *only* consumer typed
  atoms serve (force 3), overloads satisfy the target that doesn't need them
  and break the one that does. Suffixed names (`ChildInt32`) are callable from
  both targets; overloads only from the CLR. Layer on the freeze economics
  (netstandard2.0, no default-interface-methods ⇒ you cannot add a `Child`
  member later, so the set must be provably complete at freeze) and the safe
  cross-target choice is distinct names. **Lean: suffixed names.**
- **`Case(name)` nullary shortcut** — saves one call on the very common
  enum-like DU arm (`BeginCase(name); EndCase()` → `Case(name)`) at the cost of
  one more method on the frozen surface. Lean: skip; uniformity wins, the
  switch body is cold metadata either way.
- **Anonymous records (upstream only)** — no Vesper stake yet, but record a
  direction so the RFC starts from one: if `{| |}` vs `{ }` ever needs
  distinguishing at the sink, add a `kind` *enum arg* to `BeginRecord`, not a
  new `BeginAnonRecord` method. Same freeze economics as the overload question:
  on a no-DIM frozen interface an enum arg is extensible, a method is not.
  (Likely it's a runtime rendering concern and needs neither.)
- **`%+A`** — under synthesis, "print non-public state" is a compile-time
  decision, not a `BindingFlags` choice at print time. Records/unions have no
  hidden per-field state so Vesper is unaffected; the RFC needs a sentence.
