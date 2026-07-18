# Codegen by key — what's left

`Emit.BuiltinOps` is **retired** (2026-07-11). Every operator now emits from its
`ops-platform.fs` / `comparison.fs` contract body, spliced by `SymbolKey` in
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

## Deferred: disjunctive dispatch — lands with overload resolution

The arithmetic bodies are 3-typar (`^T1 -> ^T2 -> ^T3`, faithful to the `.fsi`) but the
trait call is **left-biased**: `TExpr.TraitCall` carries a single `receiver`, set to the
left operand's type. So `Vector + int` (nominal left) resolves; `int + Vector` does not —
it errors in `Engine.drainSrtpBounds`, which fires eagerly on whichever participant links
first and, for a primitive `^T1`, manufactures a homogeneous `t*t -> t` candidate that
pins `^T2 := int` before the right operand is consulted.

Making it work needs the SRTP drain to **defer** until enough participants are ground, or
to **trial-unify both candidates and undo** — and this codebase has an explicit
no-speculative-unification stop (`InferTypeOps.fs:110-113`). Overload resolution has since
landed its own speculative primitive — the read-only scratch-substitution matcher
`matchTypes` (`Passes/Unification/InferOverload.fs`) — but that is a *filtering* query that
never fires the constraint drains, so the trial-and-**undo of the real drain** this needs is
still unbuilt. **Accepted deferral: it lands with SRTPs.** Widening `TraitCall` to a
candidate set is the small half (~8 mechanical walker sites; neither backend has a
`TraitCall` arm). The unifier trial-and-undo change is the real work and belongs with the
SRTP dispatch effort.

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
base, fails `nominalHeadKey`, and now diagnoses — where it used to emit CIL `add` on a
`System.Decimal` (garbage). The diagnostic is the correct interim state. The real fix is
a clause calling `Decimal::op_Addition`, or a `nominalHeadKey` that accepts BCL `TyConst`
heads.

**Narrow signed literals do not project.** `Freeze.parseConst` throws "non-representable
literal NumSByte" on a NEGATIVE `sbyte` / `int16` literal (`-56y`, `-25536s`):
`Lexing.tryParseNumericLiteral` folds every width `TConstValue` cannot hold through
`Convert.ToUInt64`, which rejects the minus sign. `uint64` / `nativeint` / `unativeint`
literals fold to a `TConstValue.Int` instead — silently. Pre-existing and independent of
the operator work, but it is why `ArithmeticOperatorTests` pins those widths' clauses
*structurally* (off annotated parameters) and writes `int (100y + 100y)` rather than
`100y + 100y = -56y`. Those widths have no literal that can reach them.

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

Fix shape: defer the `default` constraints out of per-binding generalisation
(`InferGeneralize.fs:143-198`) to an end-of-scope drain, applied only to typars still
unconstrained. Emit the FS0064-equivalent warning when a use site narrows an explicitly
annotated typar.

This is an independent correctness bug, **not** a prerequisite for anything above: eager
defaulting grounds *more* typars than deferred defaulting would, so it can only ever
produce *fewer* un-ground residues reaching an operator's base. But it must not be allowed
to hide behind the unsupported-operator diagnostic — if that diagnostic ever fires where a
use site or a default *should* have grounded the typar, the bug is here, not there.

## The remaining by-name codegen surfaces

Not blocked by the above, and each stands on its own.

- **`ICodegenSymbols` is a string bridge over a key-addressed store**
  (`ExternalSymbols.fs:983-1002`). The bridge (`CodegenSymbols.fs:16-50`) *already*
  mints the key straight back via `lookupKeyOfCompiledName`, so the string is a pure
  round trip — the leak is the signature, not the data. Every CLR caller already holds
  a key. Requires first adding `IExternalSymbolStore.TryLookupValue : SymbolKey -> …`
  (the store face has no value channel, which is why `ClrProvider.fs:327` decomposes a
  Freeze key into strings and calls the *resolver* face).
- **JS: narrow the provider handle.** `JsFlatFns.fs:49-58` and `EmitJsContext.fs:306-312`
  round-trip a key back to a string to reach the resolver face. Read the store by key,
  then narrow `WalkCtx.Provider` (`EmitJsContext.fs:76`) to `IExternalSymbolStore` —
  which makes the resolver reach *structurally impossible* in JS codegen, as
  `PassContext.Provider` did for the front end. This is the step that buys an invariant,
  not just tidiness.
- **`RuntimeNames.arrayOfListKey`.** `ElaborateExpr.fs:655` mints
  `TExpr.External(arrayOfListName, ValueNone, …)` for an array literal — the one
  genuinely keyless head, and the sole supply for `EmitCall.fs:152`'s string match.
  Give it a well-known key (the `EmitJsContext.fs:320` `structuralFormatKey` precedent).
- **`ClrProvider.fs:314`'s `compiledName = "List.fold"`** string test; drop
  `compiledName` from `ICodegenProvider.TryEmitCall`.
- **Extend `ResolverAllowlistTests` to scan `src/XParsec.FSharp.Codegen.*`.** The only
  remaining reader should be `Codegen.Common/SymbolProviders.fs:161/208/232`, which is
  contract *extraction* (a producer resolving its own qualified names, not a consumer)
  and belongs on the allowlist with that justification.

## Known adjacent gaps

- **`let inline` is exempt from `.fsi`/`.fs` typar conformance** (`ConformanceTypars.fs:24-36`),
  by construction. That exemption is what let the one-typar arithmetic body drift from its
  3-typar contract and silently miscompile a heterogeneous operator. It is now the last
  thing standing between that class of bug and the compiler. Treat it as a gap, not a
  scope boundary.
- `Inline.isStructType` is a hardcoded primitive list, so `when ^T : struct` does not see
  user-defined structs.
- Neither backend has a `StaticOptimization` / `TraitCall` arm — both rely wholly on the
  inline pass having eliminated them. The unsupported-operator diagnostic is what protects
  this (the driver stops on error-severity diagnostics); without it an unresolved node
  reaches a catch-all `failwithf`.

## Non-goals — where the name IS the right key

- Emitted JS identifiers (`JsExternalMembers.mangledName`, `JsRuntime.addRef`'s export
  name). The emitted identifier *is* a name.
- IL opcode mnemonics (`ilBin "add"`, `Cil.tryOpCodeOfMnemonic`) — target dialect, not
  identity. These survived `BuiltinOps`' deletion inside the contract bodies' `(# … #)`.
- The platform-repr axis (`Intrinsic.Platform` — `"System.Exception"`, `"number"`) — a
  different string axis from a source spelling, already published as data.
- Member names on a resolved declaring key; record field / union case / `.ctor` names.
- `MemberKey.argSig` overload identity; cross-package resolution caching.
