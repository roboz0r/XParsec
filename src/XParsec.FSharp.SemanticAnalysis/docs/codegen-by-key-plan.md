# Codegen by key — what's left

`Emit.BuiltinOps` is **retired** (2026-07-11). Every operator now emits from its
`ops-platform.clr.fs` / `comparison.clr.fs` contract body, spliced by `SymbolKey` in
`Passes.InlineExpansion` — applied *and* eta'd-as-a-value. Codegen holds no op→opcode
table and recognises no operator by name. That narrative is not repeated here: the code
is its record, and `git log` has the blow-by-blow.

What follows is only the work that is still un-done. EPHEMERAL like all
`docs/*-plan.md` — delete once these land.

## The invariant the operator work established

> **An operator's static-optimization BASE must be a safe generic default, never inline
> IL. Inline IL belongs only in per-primitive clauses. Where no safe generic default
> exists, the base must produce a compiler diagnostic.**

Arithmetic and `~-` comply: the SRTP trait call is the base, with one explicit clause per
supported primitive. Equality/ordering comply: their base is
`EqualityComparer<^T>.Default.Equals` / `Comparer<^T>.Default.Compare`. **The bitwise
family does not** — see below.

Why no `AdditionDynamic` (FSharp.Core's runtime-reflective base) was needed: `Freeze.fs`
drops inline templates outright, so an operator body is only ever spliced at a use site
and never compiled as generic code. F# needs a dynamic base because it must emit one. We
don't, which is what licenses the "or diagnose" escape hatch. **Position: go as far as
this takes us.** If the diagnostic ever fires on code that genuinely should compile, that
is the signal to reconsider — not before.

## LANDED: disjunctive dispatch

`TExpr.TraitCall` carries `supportTys: EqArray<'ty>` and `MemberSignature` carries the
declared `(^T1 or ^T2)` support set. `UnificationTraitMembers.pick` is the one search over
that set — host enumeration, read-only applicability, and the winning member with its
declaration for the key mint. `Engine.trySolveSrtpTrait` defers a bound while a support
type is unpinned and picks through it at a forced sweep (`Engine.sweepSrtpTraits`, run per
binding group and per bare module expression); `Inline.resolveTraitCall` picks through it
at expansion, with a unique applicable host rewriting and several declining as
`Kind.TraitAmbiguous`, F#'s FS0043. `int + Vector` resolves; `Vector + int` still
resolves.

**Trial-unify-and-undo is NOT required.** The reversible-store line was investigated and set
aside — see `unification-store-redesign-plan.md`. The residual genuinely-ambiguous cases (a
free *input* operand with >1 viable candidate) are handled by suspend-then-default-or-error,
which rollback would not resolve anyway. The store redesign proceeds on its own
reasonability/perf merits, not as a prerequisite here.

### Boundary: static-abstract interface members (SAIM) / generic math — two regimes, one a non-goal

Resolving `((^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3))` does **not** resolve *into*
`IAdditionOperators<TSelf,TOther,TResult>`; it resolves by **finding a member**, and a type
implementing the interface is one *source* of that member. (The shapes coincide:
`IAdditionOperators<TSelf,TOther,TResult>` *is* the heterogeneous `^T1 * ^T2 -> ^T3`, which
is the confirmation that the 3-typar signature was worth carrying.)

- **Ground operand — falls out, in scope.** A ground SAIM-implementer reaches the existing
  `TyClass` arm of `dischargeSrtpTraits` (`Engine.fs:1028`) and its static `op_Addition` is found
  like any other nominal's. Bounded extra work: an *explicit* static interface implementation
  is not a public member, so the lookup must also walk implemented interfaces' static abstract
  members and codegen must emit a **constrained** call (`constrained.` + `call`), not a direct
  one. The common case (public operators, incl. the BCL numerics) needs neither.
- **Free typar constrained by the interface — does NOT fall out; a NON-GOAL for now.**
  Generic-math-style `let f<'T when 'T :> IAdditionOperators<'T,'T,'T>> x y = x + y`
  (non-inline) must resolve `x + y` while `'T` is still a typar, via `'T`'s interface
  constraint, emitting a **real generic method** with the constraint in metadata and a
  constrained call to the static abstract member — runtime-witnessed, JIT-specialised. SRTP
  is the opposite mechanism: compile-time, `inline`-only, structural, no runtime witness. This
  codebase's operators are `inline`-spliced by key (`Freeze` drops the templates), so there is
  no generic-method-with-constraint emission today; supporting this is a new backend
  capability, not a resolution tweak.
- **The hook, if/when Regime 2 is wanted.** F#'s readiness predicate is
  `SupportOfMemberConstraintIsFullySolved` **or an IWSAM special case** (Interface With Static
  Abstract Members): a support typar carrying a matching SAIM constraint is "ready" though not
  ground, and resolves to a constrained call. That is precisely the second admission rule the
  suspension gate would grow — a second `ready` condition feeding the constrained-call codegen
  path — not a redesign of the deferral.

## Future: chained constrained `inline` (F#+ style) and inline-to-erasure CEs

Not yet implemented; a wanted capability. Two related features, and both stress the
decisions above rather than following from them for free.

**(a) Chained constrained `inline` dispatch — the F#+ witness pattern.** A witness type with
overloaded static members plus an `inline Invoke` that dispatches via SRTP
`((^M or ^I or ^R): (static member Map: _ * _ -> _))` — disjunctive over 3+ support types,
*chained* because each `inline` layer's constraint surfaces as its caller's, resolved only at
a fully-ground use site possibly many expansions deep. Two pieces this needs are **not
built**:

- **Constraint-carrying generalisation.** An `inline` binding must generalise *with its
  residual SRTP constraints intact* (like `Num a =>`) and re-dispatch them per instantiation.
  This is the machinery flagged elsewhere as "F# does this only for `inline`"; F#+ lives or
  dies on it. It interacts with Rémy levels (a carried constraint keeps its vars from
  generalising the wrong way) and is the real inference work here.
- **Recursive inline expansion to fixpoint**, resolving each layer's constraints against the
  now-concrete instantiation as it splices. The engine already splices by key; "chained +
  constrained + recursive" is the generalisation.

Two honest caveats against earlier claims in this doc:

- **Betterness does not fully collapse.** The disjunctive-dispatch section leans on "no
  implicit conversions ⇒ selection is exact/subtype match." True for BCL overloading — but
  the F#+ `Default1/Default2` hierarchy *is* an overload-**priority** mechanism, so this
  pattern needs a priority axis among candidates even with conversions rejected. Selection
  stays **read-only** (priority + `subsumes`, no tentative unification), but it is more than
  bare subtype specificity.
- **The parked rollback question could reopen here.** Each layer stays read-only *if* resolved
  outside-in with ground support and a unique best candidate — which F#+ is deliberately
  engineered to guarantee. But chained constrained resolution is the one in-scope-ish feature
  where a witness choice at one layer can fail deeper and need another — genuine cross-layer
  backtracking. F# handles that with its trial-and-undo. So this is the **named future
  trigger** for the reversible-store line parked in `unification-store-redesign-plan.md`:
  build deferral first, and only if real F#+-style usage demonstrates cross-layer backtracking
  does rollback come back on the table.

**(b) Inline-to-erasure computation expressions.** CE desugaring emits `builder.Bind/Return/
Combine/Delay/Run` calls; when those are `inline` and the builder is a singleton/struct, the
allocation and closures must reduce away to flat code. Needs: **`[<InlineIfLambda>]`** (inline
a lambda *argument* into the body rather than pass a closure — the piece that erases the
continuation), plus an **erasure/simplification pass** on the inline-expanded TAST
(beta-reduction, dead-builder elimination, copy propagation). This is an optimisation-pass
capability on top of the recursive inline expansion in (a), not a resolution change.

**Perf consequence.** F#+ is the canonical case of SRTP compile-time blow-up. That moves the
store redesign's dispatch-resolution **memoisation** (`matchTypes`/`subsumes` verdicts, member
lookups, keyed by id across instantiations) from a nice-to-have to **load-bearing** for this
feature — see `unification-store-redesign-plan.md`.

## Independent follow-ons — one commit + tests each

**The bitwise family is not an inversion — it is a rewrite.** `&&&`, `|||`, `^^^`, `~~~`,
`<<<` are bare single-expression IL bodies with **zero** clauses; `>>>` has unsigned
clauses but no SRTP. Yet the `.fsi` already declares SRTP constraints and `default ^T: int`
for all of them. Bringing them up to the language means authoring the trait-call base *and*
a full per-primitive clause list from scratch — roughly eight clauses each — so they still
violate the invariant above (a raw-IL base on a non-primitive `^T`). No consumers exist
today, so this can land per-operator, each with its own tests.

Note the arithmetic corpus was almost entirely fictional before step 4 (it covered `int`,
and `byte` on two operators) — every other primitive would have become a silent hard error
on inversion. **Fill each bitwise operator's net BEFORE inverting it**, not after.

**`decimal` has no clause and is not a nominal.** It is a `TyConst`, so it falls to the
base, fails the nominal type-constructor key lookup, and now diagnoses — where it used to emit CIL `add` on a
`System.Decimal` (garbage). The diagnostic is the correct interim state. The real fix is
a clause calling `Decimal::op_Addition`, or a nominal type-constructor key lookup that accepts
BCL `TyConst`s.

**Narrow / wide literals and clause selection — the cited mechanism was fictional.**
Corrected: there is no `Freeze.parseConst` (it is `ElaborateLiterals.parseConst`,
`Elaborate/Literals.fs:102`, throwing `"Elaborate.parseConst: non-representable literal …"`
— the width is `%A` of the token, not a literal `NumSByte` string); there is no
`Lexing.tryParseNumericLiteral` (the producer is `NumericLiterals.parseNumericLiteral` →
`IntKind.parseBits`, `NumericLiterals.fs:168`); and there is no blanket `Convert.ToUInt64`
— `parseBits` uses a **per-width** `Convert.To*` as that width's range check. Values carry an
`IntKind` witness (`TConstValue.Integral(w, bits)`); there is **no** `TConstValue.Int` case,
so the "`uint64`/`nativeint`/`unativeint` fold to `TConstValue.Int`" story is stale — it also
appears verbatim in `ArithmeticOperatorTests.fs:44-46` and should be corrected there too.
And `-56y` almost certainly **does** project: the lexer merges the sign into one `SByte`
literal (`tryMergeNegativeLiteral`, `Lexing.fs:384`) and `Convert.ToSByte("-56", 10)` accepts
a base-10 minus — so the premise "negative `sbyte`/`int16` literals do not project" is
unconfirmed and likely false. What IS verified: `ArithmeticOperatorTests` pins every width's
clause *structurally*, off annotated parameters (`let f (a: sbyte) (b: sbyte) = a + b`,
`ArithmeticOperatorTests.fs:91-119`), which need no literal. **Whether a literal can now reach
the narrow / wide-unsigned clauses needs a pipeline check, not an assertion** — resolve that
(a two-line `analyse` on `-56y` / `5UL + 5UL`) before trusting this bullet.

**Typar defaulting fires too early.** `default ^T: int` is a *last resort*. F# leaves the
typar open, lets every use site in scope constrain it, and only defaults what is *still*
free at end of scope. We default eagerly, at generalisation of the binding, before use
sites are seen:

```fsharp
let f (a: 'a) (b: 'a) = a + b
let r = f 1.1 2.2
```

- **F#**: `f : float -> float -> float`. The use site pins `float`; the annotation is
  merely warned about (FS0064, "less generic than indicated by the type annotations").
- **Us**: two errors — `Type mismatch: float vs int`. We already committed `'a := int` at
  the binding, so the `float` use site collides with it.

Fix shape: defer the `default` constraints out of per-binding generalisation —
`applyDefaults` (`InferGeneralize.fs:151-236`), invoked eagerly inside `generalise` at
`InferGeneralize.fs:300` — to an end-of-scope discharge, applied only to typars still
unconstrained. Emit the FS0064-equivalent warning when a use site narrows an explicitly
annotated typar.

This is an independent correctness bug, **not** a prerequisite for anything above: eager
defaulting grounds *more* typars than deferred defaulting would, so it can only ever
produce *fewer* un-ground residues reaching an operator's base. But it must not be allowed
to hide behind the unsupported-operator diagnostic — if that diagnostic ever fires where a
use site or a default *should* have grounded the typar, the bug is here, not there.

## The remaining by-name codegen surfaces

Not blocked by the above, and each stands on its own.

- **JS: narrow the provider handle.** `JsFlatFns.fs:52` and `externalValueRef`
  (`EmitJsContext.fs:349-352`) round-trip a key back to a string
  (`SymbolKeyOps.qualifiedName`) to reach the resolver view. Read the store by key, then
  narrow `WalkCtx.Provider` (`EmitJsContext.fs:84`, currently the broad
  `IExternalSymbolProvider`) to `IExternalSymbolStore` — which makes the resolver reach
  *structurally impossible* in JS codegen, as `PassContext.Provider` did for the front end.
  This is the step that buys an invariant, not just tidiness.
- ~~**`RuntimeNames.arrayOfListKey`**~~ — DONE. `TExpr.External` carries a `BindingKey`
  outright, so no keyless node survives to supply a string match, and neither backend
  re-narrows the key kind at emission.
- ~~**`ClrProvider.fs`'s `compiledName = "List.fold"`** string test~~ — DONE, and the
  `EmitFold` recipe went with it: `emitExternalCall` reads `fold`'s published signature
  through the general path, so the intercept bought nothing. `compiledName` is off
  `ICodegenProvider.TryEmitCall`.
- **Contract extraction stays a resolver reach**: a producer resolving its own qualified names,
  not a consumer lookup. The three sites cited here were in `Codegen.Common/SymbolProviders.fs`,
  which is now a 47-line composition shim; the extraction they named lives in
  `Passes/SignatureResolution.fs` and `PackageProviders.fs`. Narrowing
  `WalkCtx.Provider` above is what enforces the rest; there is no allowlist test to add it to —
  `ResolverAllowlistTests` has been deleted (it matched raw source text, so a doc comment
  counted as a reader).

## Known adjacent gaps

- **`let inline` is exempt from `.fsi`/`.fs` typar conformance** (`ConformanceTypars.fs:24-36`),
  by construction. That exemption is what let the one-typar arithmetic body drift from its
  3-typar contract and silently miscompile a heterogeneous operator. It is now the last
  thing standing between that class of bug and the compiler. Treat it as a gap, not a
  scope boundary.
- `Inline.isStructType` is a hardcoded primitive list, so `when ^T : struct` does not see
  user-defined structs.
- The JS backend has neither a `StaticOptimization` nor a `TraitCall` arm, and the CLR
  backend has no `TraitCall` arm — those rely wholly on the inline pass having eliminated
  the node. The CLR backend **does** have a `StaticOptimization` arm (`EmitExpr.fs:119` →
  `EmitIntrinsic.fs:146-154`), which emits the dynamic-default fallback body rather than
  selecting a clause. Where no such arm exists, the unsupported-operator diagnostic is what
  protects the gap (the driver stops on error-severity diagnostics); without it an
  unresolved node reaches a catch-all `failwithf`.

## Non-goals — where the name IS the right key

- Emitted JS identifiers (`JsExternalMembers.mangledName`, `JsRuntime.addRef`'s export
  name). The emitted identifier *is* a name.
- IL opcode mnemonics (`ilBin "add"`, `Cil.tryOpCodeOfMnemonic`) — target dialect, not
  identity. These survived `BuiltinOps`' deletion inside the contract bodies' `(# … #)`.
- The platform-repr axis (`Intrinsic.Platform` — `"System.Exception"`, `"number"`) — a
  different string axis from a source spelling, already published as data.
- Member names on a resolved declaring key; record field / union case / `.ctor` names.
- `MemberKey.argSig` overload identity; cross-package resolution caching.
