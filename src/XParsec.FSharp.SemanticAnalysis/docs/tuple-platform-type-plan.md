# Tuple platform type — let the target answer what a tuple becomes

Working document. Ephemeral: delete it when the work lands.

Line numbers only in the anchor list at the end, and verify those before editing.

*Successor to `platform-facts-plan.md`, deleted 2026-08-13 with all five of its steps landed.
This is the one thing that doc left open. Everything it decided is now in the code — the layout
ladder is `TypeLayout.resolve`, the shape projections are `TypeLayout.shapeOf` /
`.shapeOfFrozen`, and both ends read them.*

## The defect

`LayoutShape.Encoded` is the one shape the shared ladder cannot answer. A tuple is STRUCTURAL,
so it carries no `TypeKey` for the target to answer `IsValueType` under, and the front end
therefore says `Unanswered` while `EmitPattern.isValueType` answers `true` in an arm of its own.

That arm is the seam the layout work closed everywhere else. Three costs, in descending order:

1. **The backend still has a shape it classifies for itself.** Every other shape is the same
   projection at both ends and cannot diverge; this one is two hand-written answers again, and
   they already diverged once — the front end asserted "a tuple is laid out by reference on
   every target" while `ClrEncoder` emitted `System.ValueTuple`n`.
2. **`Regions.isAllocation` over-tracks a CLR tuple.** `Unanswered` stamps, so a
   `System.ValueTuple` that never heap-allocates gets a region. `ClosureReprs` — a CLR-only
   consumer — then says `Heap` where `Stack` was available.
3. **`when 'T : struct` on a tuple `Defer`s** where the CLR could answer `Satisfied`.

## Decided semantics

**The provider answers what a tuple BECOMES, and the existing ladder does the rest.** One new
channel, the same shape as `IsValueType`:

```fsharp
/// The nominal a tuple of `arity` elements becomes on the compiling target: the OUTERMOST
/// constructor, so a CLR 9-tuple answers `System.ValueTuple`8` and its nesting stays in the
/// encoder. `ValueNone` from every contract source, from a compile composing no platform, and
/// at arity < 2, which is not a tuple.
abstract TupleType: arity: int -> TypeKey voption
```

`LayoutShape.Encoded` becomes `LayoutShape.Tuple of arity: int`, and `resolve` gains one rung:

```fsharp
| LayoutShape.Tuple arity ->
    match tupleType arity with
    | ValueSome key -> platform key
    | ValueNone -> TypeLayout.Unanswered
```

**The acceptance test is a deletion:** `EmitPattern.isValueType` loses its first arm and becomes
`TypeLayout.resolve … (TypeLayout.shapeOfFrozen ty) = TypeLayout.Value`, with no shape it
answers itself. If the arm survives, the change did not land.

Arity is in the query because it is the honest identity (`ValueTuple`2` is not `ValueTuple`3`),
not because the layout answer needs it — every member of the family is a struct. It is what
makes the query reusable by the second consumer below.

### Why not a `Vesper.tuple` intrinsic key

The obvious alternative — mint a key like `RuntimeNames.arrayKey rank` and bind a repr in each
target's `prim-types-*.fs` — was rejected. It puts a `Vesper.tuple` name into the front end's
intrinsic namespace, where it can be written, shadowed and diagnosed, to carry a fact no source
ever spells. The query has no such surface: nothing can name it, and the arity-family
(`ValueTuple`1..`8`) is one function rather than eight declarations.

## Steps

1. **The channel.** Field on `NamedChannels` with a `fun _ -> ValueNone` in
   `NamedChannels.empty`, so every channel-record source gets the miss for free; abstract +
   `default` on the provider decorator base; `firstHit` fold in `stack`. This is exactly the
   three-edit shape `IsValueType` took, and the reason to copy it is that the direct
   implementors are the ones that break.
2. **The two answers.** `MetadataSymbolProvider` maps arity → `System.ValueTuple`n`, clamping
   at 8. `JsNativeSymbols` names its array type. A contract-layer source answers `ValueNone`:
   the family is a platform fact, not something a `.fsi` declares.
3. **`TypeLayout`.** Rename `Encoded` → `Tuple of arity`, thread the resolver through
   `resolve` / `ofShape` / `ofSemType`, and delete `EmitPattern`'s carve-out.
4. **The rows that prove it.** A tuple row in each backend suite's `RegionLayoutTests`, opposite
   on the two targets — the CLR tracks no region for a tuple-typed result, JS does. `RegionsTests`
   is unaffected: it composes no platform, so `TupleType` misses and tuples stay tracked there.

## What this does NOT fix

**Tuple CONSTRUCTION keeps its heap-repr sink.** `TExpr.Tuple` goes to `holds`, which mints a
region and marks `HeapReprSink` without asking `isAllocation` at all — because no composite can
carry a `ref struct` field, which is an axis-2 fact and stays true. So this change moves the
verdict for a tuple-typed RESULT (a call's, a branch's), not for the tuple literal that built
it. Anyone landing this and expecting `let p = (1, 2)` to change representation will find it
does not, and that is correct.

## The second consumer, and the generalisation

`ClrEnv.eValueTupleN` and `ClrEncoder` spell `"System", "ValueTuple`%d"` in the backend. Once
the provider owns the identity, the encoder should mint its handle FROM the provider's key
rather than from its own string — the same move A15 made for primitives: a backend must not
classify a repr fact off a name it holds itself, because the fact belongs to the repr the target
binds. **Separable**: the layout fix stands alone, and this is worth doing only if the encoder's
nesting scheme reads naturally off a key.

**Arrays are the same problem** and would take the same channel (`ArrayType: rank -> …`).
`RuntimeNames.arrayKey rank` exists as a key but binds no repr, and `ClrEncoder` encodes `"!0[]"`
directly, so an array reaches `TypeLayout` as `Primitive` with a key nothing answers for — it
lands `Unanswered` for a different reason than the tuple does. Out of scope; the point is that
the query shape should not preclude it.

## Premises

Two confirmed in the code (2026-08-13), one still open.

- **A CLR 9-tuple's outermost type is `ValueTuple`8`.** CONFIRMED: `ValueTupleRefs`' recursion
  computes `let k = if n <= 7 then n else 8`, and only the `TRest` nesting below that is
  recursive. So clamping the query at 8 answers the same constructor the encoder emits.
- **Arity < 2 is not a tuple.** CONFIRMED: `ValueTupleRefs` `failwithf`s below 2, and its own
  note says `ValueTuple`1` is reachable only as a `TRest`. The query must MISS rather than fail
  there — it is asked speculatively by a shape projection, not by an emitter that already knows
  it holds a tuple.
- **OPEN: which type JS names.** `JsNativeSymbols.IsValueType` returns `ValueSome false` for
  every key, so the layout answer is right whatever key it names — the choice is unforced until
  the encoder consumer above exists, and picking it arbitrarily now would be a guess that reads
  as a decision.

## Anchors (verify before editing)

- The shape with no answer: `LayoutShape.Encoded`, and its two readers —
  `TypeLayout.resolve`'s last arm and `EmitPattern.isValueType`'s first.
- The cost, sited: the `Unanswered -> true` arm of `Regions.isAllocation`.
- The channel to copy: `IExternalSymbolStore.IsValueType`, `NamedChannels.IsValueType` +
  `.empty`, the decorator's `abstract`/`default` pair, `stack`'s fold, and the two answers in
  `MetadataSymbolProvider` and `JsNativeSymbols`.
- Direct `IExternalSymbolStore` implementors, which a new abstract member breaks and the channel
  record does not: count them before choosing where the member goes.
- The CLR tuple identity, spelled in the backend today: `ClrEnv.eValueTupleN`,
  `ClrEncoder.ValueTupleRefs` and its arity ≥ 2 guard.
- The construction path that this does not touch: `Regions.holds`, from the `TExpr.Tuple` arm.

## Done when

- `LayoutShape` has no case a backend answers for itself, and `EmitPattern.isValueType` is one
  expression with no arm of its own.
- A tuple row in each backend suite's `RegionLayoutTests` takes opposite verdicts.
- This file is deleted.
