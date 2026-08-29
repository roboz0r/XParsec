# TraitCall support set

`TExpr.TraitCall` carries one support type where the source form carries a set. This plan
widens `supportTy: 'ty` to `supportTys: EqArray<'ty>` end to end, so an SRTP member declared
only on a non-left operand resolves.

## Root cause

`((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))` names a support *set*, and F#
searches every type in it. The TAST node (`TastExpr.fs:294`) holds one `supportTy`, and the
build site (`Elaborate/Apply.fs:221`) fills it with `args.[0]`'s type, so
`int * Vector -> Vector` — the member living on the right operand — never resolves. The
expansion search (`Inline.fs:116-148`) then has only that one candidate to try.

The information needed is already present at the build site: `peelOneArg` opens the argument
tuple into per-operand `TExpr`s, each fully typed, so the candidate set is the distinct
`TastWalk.exprTy` of every element of `args`. No new CST plumbing is required. (The CST's
`staticTypars` OrList — discarded at `ElaborateExpr.fs:326` — would additionally correlate
argument positions to named typars; see Deferred.)

## Semantic assumptions — confirm before implementing

1. **Candidate set = distinct types of `args`, in argument order.** Deduplicated via
   `EqArray`; `((^T or ^T): …)` on same-typed operands stays a one-element set, so existing
   decline tests (`ArithmeticOperatorTests.fs:311`, the `char` case) keep their meaning.
2. **First success in that order wins.** This preserves today's left bias when both operands
   could resolve. F# instead solves SRTP constraints with its own overload machinery; if
   both-resolve-differently should be an *ambiguity diagnostic* rather than left-wins, the
   search loop collects all successes instead of short-circuiting. Decide which.
3. **Zero-arg fallback unchanged.** `args.Length = 0` currently uses the node's own `ty`; that
   becomes a one-element set.

## Steps

Each step leaves the build green; steps 1–3 are one commit (the type change is not splittable
across the freeze boundary), steps 4 and 5 are one commit each.

### 1. Widen the type through declaration, walks, and pool

- `TastExpr.fs:294` — `supportTys: EqArray<'ty>`; rewrite the case doc (delete "LEFT operand's
  type ONLY").
- `TastWalk.fs:648-658` — map `f` over every element (`EqArray.map`, preserving the
  `refEq`-sharing idiom of sibling arms).
- `TastConvert.fs:107-108` — same widening.
- `TastPoolNodes.fs:277-281` — payload becomes `SupportTys: EqArray<FrozenType>`; rewrite its
  comment.
- `TastPoolShapes.fs:365-370` (freeze) and `TastUnpool.fs:157` (thaw) — pass the set through.
- `TastAccessor.fs:606-614` — add `exprTraitCallSupportTys` beside `exprTraitCallMemberName`.
- Positional patterns that only need re-ariting: `Regions.fs:347`, `TastWalk.fs:53/98/843`,
  `ArithmeticOperatorTests.fs:179`, `TastShape.fs:572-586` (render the set joined with
  ` or `; expect golden churn on any snapshot containing a TraitCall).

### 2. Wire format

- `FrozenCodec.fs:233-236` (write) / `:349-359` (read), tag `38uy`: single `TypeRef` becomes a
  length-prefixed array. Prior art: `writeEqArrayWith w writeTypeRef` at
  `FrozenCodecDecls.fs:52`.
- **There is no codec version constant** (`Cache.fs` and `CodeVersion` were deleted in
  8430062c). The change is
  a silent format break for any persisted frozen blob. Before landing, establish what persists
  frozen bytes across runs (reference-assembly manifests?) and either add a format version
  byte at the stream head as part of this step, or record why nothing persists.
- Extend the codec round-trip tests to cover a TraitCall payload; none is TraitCall-specific
  today.

### 3. Build and search

- `Elaborate/Apply.fs:197-223` — build the deduplicated set from `args`; rewrite the doc
  comment stating the single-type limitation.
- `Inline.fs:116-148` — `resolveTraitCall` loops the substituted candidates,
  per assumption 2. `UnresolvedTrait` (`Inline.fs:15-22`) carries the tried set;
  `unsupportedTrait` (`Inline.fs:397`) and `Kind.TraitNotSupported` render it
  ("neither `Vec2` nor `float` supports …") — check the `Kind` payload shape.
- `TastLower.traitCallUnresolved` (`TastLower.fs:28`) may take the set for a richer backend
  fault; optional, both backends only print the member name today.

### 4. Tests

- Positive: a right-operand-only member resolves (the doc exemplar:
  `float32 * Vector2 -> Vector2` reached with the float on the left).
- Negative control: revert the `Inline.fs` loop to first-element-only and confirm the new
  test reds while `ArithmeticOperatorTests` stays green.
- Ambiguity behaviour per the assumption 2 decision, pinned either way.

### 5. Doc cleanup

- Delete the two entries this supersedes in `semantic-analysis-followups-plan.md`
  (`TastExpr.fs:317` TraitCall; `Elaborate/Apply.fs:224` left-operand-only).
- Delete this plan.

## Deferred

Correlating argument positions to the `staticTypars` OrList (asymmetric signatures with more
than two operands). Requires threading `staticTypars` from `ElaborateExpr.fs:326` into
`translateStaticMemberInvocation`; not needed for the two-operand cases in scope.
