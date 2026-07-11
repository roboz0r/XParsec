# Retiring `Emit.BuiltinOps` — operators come from contract bodies, or they diagnose

Successor to the (completed, deleted) name-resolution boundary plan. That work made
the front end resolve-once. This one removes the last mechanism that recognises
operators by **name string** instead of resolved identity: `Emit.BuiltinOps`.

`EmitLower.fs:31-36` already tags itself `DELETE-WHEN-COMPLETE`. This doc is the
"complete".

## Where things actually stand (verified, not assumed)

The architecture people expect is already the architecture that runs. An operator is
declared in a contract `.fs`, resolved by ordinary name resolution, and its body is
spliced at the use site with static-optimization clauses selected once the typar is
ground:

- NameResolution resolves `+` through the provider like any other external value and
  stamps the symbol (`Scope.fs:411`).
- Unification instantiates the `^T` scheme, pins `^T := int`, and stamps the
  operator's `SymbolKey` in `IntrinsicKey` — **unconditionally, primitives included**
  (`InferApp.fs:590`).
- `InlineExpansion` splices the contract body *by key*; `Inline.fs:186-191` selects
  the matching `when ^T : …` clause after substitution, and `Inline.fs:199-217`
  resolves a selected SRTP `when ^T : ^T` clause to a real `StaticMethodCall`.

Proof it is the live path, not a story: `200uy + 100uy = 44uy` passes. The `conv.u1`
truncation can only come from the spliced `when ^T : byte` clause — `BuiltinOps`
emits a bare `add`, which would give 300.

Three operator families have contract bodies today: arithmetic/bitwise and equality
(`Vesper.Core/ops-platform.fs`), and ordering (`Vesper.Comparison/comparison.fs`,
`[<AutoOpen>]`; plus an int-only `Vesper.Core/int-comparison.fs` that is deliberately
NOT auto-open and needs an explicit `open`).

The scoping rules already hold. With Core-only manifests, `2 < 3` fails with
`Unknown operator symbol: op_LessThan` — `BuiltinOps` never rescues an unresolved
operator, because Unification errors first. And `when 'T: comparison` is declared in
`comparison.fsi` and genuinely enforced by `Engine.checkConstraint` (`:651-724`):
`NoComparison` → `Violated`, a function type → `Violated`.

## The one thing `BuiltinOps` still does — and it is a bug

Its only remaining job is the **un-ground operand**, and on that path it is wrong.
`InlineExpansion`'s ground guard (`:593-596`) declines to splice, the head survives
as an `External`, and `EmitLower` matches it *by name* and emits raw IL:

```fsharp
type Tag = Tag of int
let eq a b = a = b
eq (Tag 1) (Tag 1)   // 0 — FALSE. BuiltinOps' `ceq` = reference comparison.
x = y                // 1 — TRUE. The spliced comparer base.
```

Ordering has the identical defect (`clt` on object refs — unverifiable IL).

**The guard has no justification.** `comparison.fs` claimed a free typar
"`Comparer<!0>` can't encode". That is false: a generic function calling
`EqualityComparer<'a>.Default.Equals` over a free *method* typar emits, runs, and
returns the structurally-correct answer. The base clause is always emittable.

Name-keyed dispatch is also unsound in its own right: `BuiltinOps.table` matches
**any** `op_Addition` from **any** package, so a referenced package's own `(+)` on its
own type collapses to CIL `add`.

## The design rule

> **An operator's static-optimization BASE must be a safe generic default, never
> inline IL. Inline IL belongs only in per-primitive clauses. Where no safe generic
> default exists, the base must produce a compiler diagnostic.**

Measured against that rule:

- **Equality / ordering** already comply. Their base is `EqualityComparer<^T>.Default.Equals`
  / `Comparer<^T>.Default.Compare` — safe for any `'T`, and exactly what the
  `'T: equality` / `'T: comparison` constraints promise. Nothing to write; just stop
  bypassing it.
- **Arithmetic / bitwise** violate it. Their base is `(# "add" x y : ^T #)` — raw IL
  that is meaningless on a non-primitive `^T`. The safe default they need already
  exists in the file, but it is in the wrong position: the SRTP trait call
  `(^T: (static member (+): ^T * ^T -> ^T) (x, y))` is currently the LAST clause
  (`when ^T : ^T`). **Invert it.** The trait call becomes the base; every primitive
  gets an explicit clause carrying its IL.

Inverting gives the whole story for free:

| use site | resolution |
|---|---|
| `1 + 2` | `when ^T : int` clause → `add` |
| `200uy + 100uy` | `when ^T : byte` clause → `conv.u1 (add …)` |
| `let f a b = a + b` (no use site) | `default ^T1: int` grounds it → `int` clause → `add` |
| `let f (a:'a) (b:'a) = a + b` + `f 1.1 2.2` | use site pins `float` → `float` clause |
| `setA + setB` | base → SRTP → `StaticMethodCall Set::op_Addition` |
| typar still free after all use sites AND no applicable default | base → SRTP cannot resolve → **diagnostic** |

Equality/ordering carry no default at all (`'T: equality` / `'T: comparison`), which is
why the un-ground bug shows up there and not in arithmetic.

## Blocking prerequisite — typar defaulting fires too early

`default ^T: int` is a **last resort**. F# leaves the typar open, lets every use site in
scope constrain it, and only defaults what is *still* free at end of scope. We default
eagerly, at generalisation of the binding, before use sites are seen:

```fsharp
let f (a: 'a) (b: 'a) = a + b
let r = f 1.1 2.2
```

- **F#**: `f : float -> float -> float`. The use site pins `float`; the annotation is
  merely warned about (FS0064, "less generic than indicated by the type annotations").
- **Us**: two errors — `Type mismatch: float vs int`. We already committed `'a := int`
  at the binding, so the `float` use site collides with it.

With no use site both agree on `int`, and `f 3 4` "works" only because the default
happens to *be* `int` — so the bug is invisible until someone applies such a binding at
a non-`int` primitive.

This must be fixed as a follow-up to (or alongside) the `BuiltinOps` retirement, because
it is currently *masking* the operator story: premature defaulting grounds nearly every
arithmetic binding, which is why arithmetic almost never reaches the un-ground fallback.
Deferring defaulting correctly does not create new un-ground heads (a still-free typar
defaults at end of scope, as before) — it just makes the pinned-by-use-site case work,
and leaves the SRTP base's diagnostic for the genuinely undefaultable residue.

Fix shape: defer the `default` constraints out of per-binding generalisation
(`InferGeneralize.fs:143-198`) to an end-of-scope drain, applied only to typars still
unconstrained. Emit the FS0064-equivalent warning when a use site narrows an
explicitly-annotated typar.

## Steps

**1. Splice the base unconditionally for the comparer-based families.**
Delete the ground guard's builtin special-case in `InlineExpansion.fs:593-596`. An
un-ground `=` / `<` then splices its comparer base and is *correct*. Fixes the
structural-equality bug directly.
Churn: `OperatorRoutingTests.fs:30` asserts `let f a b = a = b` lowers to a `ceq`
`ILIntrinsic` — that assertion **encodes the bug** and flips to a comparer call. Add
the `eq (Tag 1) (Tag 1) = true` regression test that currently fails.

**2. Invert the arithmetic/bitwise bodies** in `ops-platform.fs` (and its
`ops-platform.js.fs` sibling): SRTP trait call as the base; explicit `when ^T : …`
clauses for every primitive the old `add` base silently covered — `int`, `int64`,
`float`, `float32`, `uint32`, `uint64`, `nativeint`, `unativeint` — alongside the
narrow-width clauses that already exist. Same for `- * / %`, the bitwise family, and
unary `~-`.
Risk: this is the load-bearing edit. The existing dense operator corpus
(`ArithmeticOperatorTests`, `BitwiseOperatorTests`) is the net — every primitive that
was riding the base must keep its opcode.

**3. Diagnose an unresolvable trait call.** `Inline.resolveTraitCall`
(`Inline.fs:199-217`) currently returns `ValueNone` for a non-nominal receiver and
leaves a substituted `TraitCall` "for a later phase to surface loudly" — there is no
such phase, so it reaches codegen and dies in a `failwithf`. Emit a real diagnostic:
*"type X does not support the operator `+`"*. This is what makes step 2's base safe.

Scope it correctly. Defaulting (once fixed, see the prerequisite above) grounds every
ordinary use, so this diagnostic must NOT fire for a typar that a use site pinned or a
default could ground. It is only for the residue neither reaches. **Do not let it become
the place inference failures go to hide**: if a case turns up where a use site or a
default *should* have grounded the typar and didn't, that is a bug in defaulting /
unification, not something for this diagnostic to swallow. Land it after the defaulting
fix so the two are not confused.

**4. Delete `BuiltinOps` and its duplicate.** With 1-3 landed nothing reaches it.
Remove `EmitLower.BuiltinOps` (the table, `isSaturated`, `expandBuiltinOps`) and
`InlineExpansion.builtinOpArity` / `isSaturatedBuiltin` — the hand-synced pair whose
own comment says "Mirror the codegen table exactly; keep in sync". The
match-any-package `op_Addition` unsoundness dies with them.

**5. Improve the not-in-scope message.** `Unknown operator symbol: op_LessThan`
(`InferApp.fs:600`) leaks the compiled name and does not hint at the cause. It should
read like *"no definition for `<` found — is `Vesper.Comparison` referenced?"*.

## Independent follow-on: the remaining by-name codegen surfaces

Not blocked by the above, and each stands on its own.

- **`ICodegenSymbols` is a string bridge over a key-addressed store**
  (`ExternalSymbols.fs:983-1002`). The bridge (`CodegenSymbols.fs:16-50`) *already*
  mints the key straight back via `lookupKeyOfCompiledName`, so the string is a pure
  round trip — the leak is the signature, not the data. Every CLR caller already holds
  a key. Requires first adding `IExternalSymbolStore.TryLookupValue : SymbolKey -> …`
  (the store face has no value channel, which is why `ClrProvider.fs:327` decomposes a
  Freeze key into strings and calls the *resolver* face).
- **JS: narrow the provider handle.** `JsFlatFns.fs:49-58` and
  `EmitJsContext.fs:306-312` round-trip a key back to a string to reach the resolver
  face. Read the store by key, then narrow `WalkCtx.Provider`
  (`EmitJsContext.fs:76`) to `IExternalSymbolStore` — which makes the resolver reach
  *structurally impossible* in JS codegen, as `PassContext.Provider` did for the front
  end. This is the step that buys an invariant, not just tidiness.
- **`RuntimeNames.arrayOfListKey`.** `FreezeExpr.fs:655` mints
  `TExpr.External(arrayOfListName, ValueNone, …)` for an array literal — the one
  genuinely keyless head, and the sole supply for `EmitCall.fs:152`'s string match.
  Give it a well-known key (the `EmitJsContext.fs:320` `structuralFormatKey`
  precedent).
- **`ClrProvider.fs:314`'s `compiledName = "List.fold"`** string test; drop
  `compiledName` from `ICodegenProvider.TryEmitCall`.
- **Extend `ResolverAllowlistTests` to scan `src/XParsec.FSharp.Codegen.*`.** The only
  remaining reader should be `Codegen.Common/SymbolProviders.fs:161/208/232`, which is
  contract *extraction* (a producer resolving its own qualified names, not a consumer)
  and belongs on the allowlist with that justification.

## Known adjacent gaps

- `Inline.isStructType` (`Inline.fs:122-138`) is a hardcoded primitive list, so
  `when ^T : struct` does not see user-defined structs.
- JS codegen has **no** `StaticOptimization` / `TraitCall` case at all — it relies
  wholly on the inline pass having eliminated them. Step 3's diagnostic protects this;
  without it an unresolved node reaches `EmitJs.fs:516`'s catch-all `failwithf`.

## Non-goals — where the name IS the right key

- Emitted JS identifiers (`JsExternalMembers.mangledName`, `JsRuntime.addRef`'s export
  name). The emitted identifier *is* a name.
- IL opcode mnemonics (`ilBin "add"`, `Cil.tryOpCodeOfMnemonic`) — target dialect, not
  identity. These survive `BuiltinOps`' deletion inside the contract bodies' `(# … #)`.
- The platform-repr axis (`Intrinsic.Platform` — `"System.Exception"`, `"number"`) — a
  different string axis from a source spelling, already published as data.
- Member names on a resolved declaring key; record field / union case / `.ctor` names.
- `MemberKey.argSig` overload identity; cross-package resolution caching.
