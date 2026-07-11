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

## `BuiltinOps` has exactly two live consumers, and both are bugs

### 1. The un-ground operand

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
"`Comparer<!0>` can't encode". The claim is repeated verbatim in three places
(`ops-platform.fs:16`, `comparison.fs:33-36`, and this doc's ancestor) and verified in
none, so **step 1 verifies it before it deletes anything**: a generic function calling
`EqualityComparer<'a>.Default.Equals` over a free *method* typar must emit, pass
`peverify`, and return the structurally-correct answer. Everything below rests on that.

### 2. The eta-reified operator value

`(+)` used as a **value** (`List.fold (+) 0 xs`) is not an application, so
`InlineExpansion` — which only rewrites saturated `App` spines — leaves it as an
`External` leaf. Eta-reification runs *post-freeze*, in codegen
(`TastLower.fs:685-736`, `External … when isFunTy ty -> etaExpand`), and mints the
saturated `App` only there, where the inline pass can no longer see it. `BuiltinOps`
then collapses it — by name, to a monomorphic opcode. `EmitLower.fs:35` names this
consumer outright.

So the eta path is **not** covered by fixing the ground guard, and no amount of work
on the contract bodies reaches it. Eta-reification of an *inline* external has to move
**pre-freeze** (step 2) or `BuiltinOps` can never be deleted.

### And name-keyed dispatch is unsound in its own right

`BuiltinOps.table` matches **any** `op_Addition` from **any** package, so a referenced
package's own `(+)` on its own type collapses to CIL `add`.

## The design rule

> **An operator's static-optimization BASE must be a safe generic default, never
> inline IL. Inline IL belongs only in per-primitive clauses. Where no safe generic
> default exists, the base must produce a compiler diagnostic.**

Measured against that rule:

- **Equality / ordering** already comply. Their base is `EqualityComparer<^T>.Default.Equals`
  / `Comparer<^T>.Default.Compare` — safe for any `'T`, and exactly what the
  `'T: equality` / `'T: comparison` constraints promise. Nothing to write; just stop
  bypassing it.
- **Arithmetic** violates it. Its base is `(# "add" x y : ^T #)` — raw IL that is
  meaningless on a non-primitive `^T`. The safe default it needs already exists in the
  file, but it is in the wrong position: the SRTP trait call is currently the LAST
  clause (`when ^T : ^T`). **Invert it.** The trait call becomes the base; every
  primitive gets an explicit clause carrying its IL.

Inverting gives the whole story for free:

| use site | resolution |
|---|---|
| `1 + 2` | `when ^T : int` clause → `add` |
| `200uy + 100uy` | `when ^T : byte` clause → `conv.u1 (add …)` |
| `let f a b = a + b` (no use site) | `default ^T1: int` grounds it → `int` clause → `add` |
| `setA + setB` | base → SRTP → `StaticMethodCall Set::op_Addition` |
| typar still free after all use sites AND no applicable default | base → SRTP cannot resolve → **diagnostic** |

Equality/ordering carry no default at all (`'T: equality` / `'T: comparison`), which is
why the un-ground bug shows up there and not in arithmetic.

### Why no `AdditionDynamic`

FSharp.Core's base for `(+)` is not the trait call — it is `AdditionDynamic`, a runtime
reflective dispatch. F# needs one because it must be able to emit `(+)` as a **real
generic method** when it is not inlined, and a trait call over that method's own free
typar has no IL. We have no such obligation: `Freeze.fs:66` drops inline templates
outright (`TDecl.Let(isInline = true) -> false`), so an operator body is *only* ever
spliced at a use site and never compiled as generic code. That is precisely what
licenses the design rule's "or diagnose" escape hatch, and it is why the trait call can
sit in the base position here where it could not in F#.

Deliberate position: **go as far as this takes us.** If the diagnostic from step 5 ever
fires on code that genuinely should compile, that is the signal to reconsider a dynamic
base — not before.

## The `.fs` bodies are unfaithful to the `.fsi` contract

`ops-platform.fsi:40` publishes the real F# shape — three typars and a *disjunctive*
support set:

```fsharp
val inline (+): x: ^T1 -> y: ^T2 -> ^T3
    when (^T1 or ^T2): (static member (+): ^T1 * ^T2 -> ^T3)
    and default ^T2: ^T3 and default ^T3: ^T1 and default ^T3: ^T2
    and default ^T1: ^T3 and default ^T1: ^T2 and default ^T1: int
```

`ops-platform.fs:29` implements a homogeneous single-`^T` hack:
`let inline (+) (x: ^T) (y: ^T) : ^T`, whose SRTP clause is the narrow
`(^T: (static member (+): ^T * ^T -> ^T))`.

Today that narrow trait only fires in last-clause position, so the drift is mostly
inert. **Inverting makes it the sole dispatch for every non-primitive operand**, which
would bake homogeneity into every user-defined `+` and make a heterogeneous operator
(`Vector * float -> Vector`) permanently unresolvable — silently, by falling to a base
that cannot express it. The `.fsi` is right and the `.fs` must come up to it, in the
same change that moves the trait call to the base. See
`typar-fsi-fs-faithfulness-plan.md`, which tracks this drift as a species.

## Steps

**1. Verify the comparer base over a free typar, then splice the base unconditionally.**
First the spike: emit a generic `let eq a b = a = b`, run it against a DU, confirm
`EqualityComparer<!!0>` verifies and answers structurally. Only then delete the ground
guard in `InlineExpansion.fs:593-596`. Note the edit is **not** scoped to the
comparer-based families — the guard is `externalArgsGround … || not (isSaturatedBuiltin …)`,
so removing the builtin special-case makes *every* saturated builtin splice, arithmetic
included. Arithmetic is unaffected in behaviour (its spliced base is `(# "add" #)`, the
same opcode `BuiltinOps` emitted), but the intermediate state should be stated, not
discovered.

Dead with the guard: `externalArgsGround` (`:407`), `isSpliceableOperatorArg` (`:158`),
`isGroundType` (`:145`) if it has no other caller, `builtinOpArity` (`:281`) /
`isSaturatedBuiltin` (`:304`), and the only cross-module consumer of
`Inline.isNominalType` (see its `:155` comment).

Churn: `OperatorRoutingTests.fs:30` asserts `let f a b = a = b` lowers to a `ceq`
`ILIntrinsic` — that assertion **encodes the bug** and flips to a comparer call. Add
the `eq (Tag 1) (Tag 1) = true` regression test that currently fails.

**2. Eta-reify inline externals pre-freeze.** Move the eta-reification of an `External`
*with an inline body* out of codegen's post-freeze `TastLower.lower` and into
`InlineExpansion`, so `(+)` as a value becomes `fun x y -> x + y` while the pass can
still splice the body it produces. Codegen's `etaExpand` stays for genuine non-inline
externals (`List.fold` as a value), which is what it is actually for. Without this,
step 6 cannot happen.

At an eta site the operator's type is pinned by the context (`List.fold (+) 0 xs` over
an int list gives `int -> int -> int`), so the splice grounds and yields `add`. An
eta'd operator inside a generic function stays free, falls to the base, and reaches
step 5's diagnostic — which is the correct answer.

**3. Bring `+ - * / %` up to the 3-typar contract.** Rewrite the `.fs` bodies to the
`^T1 / ^T2 / ^T3` shape the `.fsi` publishes, including the `(^T1 or ^T2)` disjunctive
support set. This may require `TExpr.TraitCall` (currently a single `recvTy`,
`Inline.fs:199`) to carry a support *set* rather than one receiver; scope that first —
it is the gate on step 4, not a detail of it.

**4. Invert the arithmetic bodies** in `ops-platform.fs` (and its `ops-platform.js.fs`
sibling): SRTP trait call as the base; explicit `when ^T : …` clauses for every
primitive the old `add` base silently covered — `int`, `int64`, `float`, `float32`,
`uint32`, `uint64`, `nativeint`, `unativeint` — alongside the narrow-width clauses that
already exist. Same for `- * / %` and unary `~-`.

Then delete `Inline.clauseSelected`'s `TraitCall` special-case (`Inline.fs:178-184`).
Its sole job is stopping a primitive operand from selecting the last-position SRTP
clause; once the trait call is the *base* and every primitive has an explicit clause, no
clause body is a `TraitCall` and the conjunct is dead. The invariant its comment
advertises at `:143-146` — that `clauseSelected` and `resolveTraitCall` "can never
disagree" — dissolves with it: the base is ungated, and `resolveTraitCall` alone
decides. Leaving it in place is dead code that reads as load-bearing.

Risk: this is the load-bearing edit. The existing dense operator corpus
(`ArithmeticOperatorTests`) is the net — every primitive that was riding the base must
keep its opcode. Confirm the corpus actually covers each one before trusting it.

**5. Diagnose an unresolvable trait call.** `Inline.resolveTraitCall`
(`Inline.fs:199-217`) currently returns `ValueNone` for a non-nominal receiver and
leaves a substituted `TraitCall` "for a later phase to surface loudly" — there is no
such phase, so it reaches codegen and dies in a `failwithf`. Emit a real diagnostic:
*"type X does not support the operator `+`"*. This is what makes step 4's base safe.

**Do not let it become the place inference failures go to hide**: if a case turns up
where a use site or a default *should* have grounded the typar and didn't, that is a bug
in defaulting / unification, not something for this diagnostic to swallow.

**6. Delete `BuiltinOps`.** With 1-5 landed nothing reaches it. Remove
`EmitLower.BuiltinOps` (the table, `isSaturated`, `expandBuiltinOps`). The
match-any-package `op_Addition` unsoundness dies with it. `TastLower.lower`'s
backend-supplied `finishOps` hook becomes identity for the CLR — check whether JS still
needs it (`TastLower.fs:671-674` says JS keeps a template / `BinaryExpr`) before
deleting the parameter itself.

**7. Improve the not-in-scope message.** `Unknown operator symbol: op_LessThan`
(`InferApp.fs:600`) leaks the compiled name and does not hint at the cause. It should
read like *"no definition for `<` found — is `Vesper.Comparison` referenced?"*.

## Independent follow-ons — not blocking, one commit + tests each

**Bitwise family and `~-` are not an inversion — they are a rewrite.** `&&&`, `|||`,
`^^^`, `~~~`, `<<<` (`ops-platform.fs:94-104`) are bare single-expression IL bodies with
**zero** clauses; `>>>` has unsigned clauses but no SRTP; `~-` (`:84`) is a bare `neg`.
Yet the `.fsi` already declares SRTP constraints and `default ^T: int` for all of them
(`:116-200`). Bringing them up to the language means authoring the trait-call base *and*
a full per-primitive clause list from scratch — roughly eight clauses each. No consumers
exist today, so this can land per-operator, each with its own tests.

**`decimal` has no clause and is not a nominal.** It is a `TyConst`
(`Inline.isStructType:136`), so post-inversion it falls to the base, fails
`nominalHeadKey`, and diagnoses. Today it silently emits CIL `add` on a `System.Decimal`
— garbage — so a diagnostic is strictly an improvement, but the real fix is a clause
that calls `Decimal::op_Addition`, or a `nominalHeadKey` that accepts BCL `TyConst`
heads. Same for any other primitive not enumerated in step 4.

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

This does **not** block the steps above, and in particular does not block step 5's
diagnostic: eager defaulting grounds *more* typars than deferred defaulting, so it can
only ever produce *fewer* un-ground residues. It is an independent correctness bug that
the operator work makes easier to see, not a prerequisite for it.

## The remaining by-name codegen surfaces

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
  wholly on the inline pass having eliminated them. Step 5's diagnostic protects this;
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
