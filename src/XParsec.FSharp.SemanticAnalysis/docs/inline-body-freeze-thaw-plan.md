# Freezing the inline-body channel — design plan

*Prerequisite to [multi-file-compilation-units-plan](multi-file-compilation-units-plan.md);
lands before it.*

## Problem

`IExternalSymbolProvider` is meant to be a frozen-domain oracle — every symbol it
hands a consumer is immutable `FrozenType`, so a consumer's inference can never
reach back and mutate a producer's inference state. One member breaks that: the
inline-body channel hands out **`SemType`**.

```fsharp
// ExternalSymbols.fs:694
type InlineBody = { Decl: TDecl; ParamAttrs: ParamAttrs[] }   // TDecl = TDeclG<SemType,_>  (Tast.fs:941)

// IExternalSymbolProvider  (ExternalSymbols.fs:782, :789)
abstract TryLookupInlineBody: key: SymbolKey -> InlineBody voption
abstract TryLookupInlineBodyByName: name: string -> InlineBody voption
```

`TDecl` is `TDeclG<SemType, SyntaxToken>` — a tree whose every type annotation is a
`SemType`, and a `SemType.TyVar` is a **mutable `UnionFind` cell**. So the provider
leaks live inference cells across the boundary. Today this is tolerable only because
of *where* the cells come from: cross-package inline bodies are re-parsed from shipped
`.fs` source into extraction-fresh `SemType`, not another live compilation's tree.

That accident does not survive multi-file. There, file N's frozen unit becomes a
provider that file N+1 resolves against (see the multi-file plan), and file N's inline
bodies are its **own live `SemType`**. When N+1 splices one, the splice machinery is
end-to-end `SemType` and mutates the shared cells:

```fsharp
// Inline.fs — the whole module is SemType
substType   : Dictionary<TypeVar, SemType> -> SemType -> SemType   // UnionFind.find (path-compresses N's cells)
resolveTraitCall / resolveStaticOpt / substExpr                    // re-resolve SRTP against substituted operands
```

Safety rests on the **dynamic** invariant that `subst` is total over the body's
quantified typars (`Inline.fs:68` — *"Roots absent from `subst` stay abstract"*). An
abstract typar root that slips through — a leaked inner metavar, an ungeneralized
`let` in the body — is an N-cell that N+1's unifier will `UnionFind.union` into. That
is backward flow (N+1 → N), the exact thing the assembly-is-linear model forbids,
enforced by discipline instead of by construction.

**Root cause is age, not design.** `IExternalSymbolProvider` was written before
`FrozenType` existed. Every *other* channel has since migrated to frozen templates
realized through one seam (`instantiateWith`, below); the inline channel never did,
because inline bodies are *exempted from freeze entirely* — so there is no
`Frozen.TDecl` to hand out. This plan pays down that debt: the provider speaks only
`FrozenType`, and the immutable→mutable transition happens exactly once, on the
consumer side, minting fresh cells by construction.

## Confirmed premises (spike results)

1. **Freeze *drops* inline templates — it does not freeze them.** `Freeze.run`'s first
   act filters out `TDecl.Let(isInline = true)` before `toFrozen` runs (`Freeze.fs:60-71`),
   because a `let inline` retains `TyVar` through elaboration (the "2B exemption") and
   `toFrozen` hard-errors on a stray `TyVar` (`Freeze.fs:46`). The doc-comment is
   explicit: inline templates are *"unrepresentable in `FrozenType`"* and the publish
   path *"reads them off the pre-freeze `SemType` tree … never `analyse`'s frozen
   output"* (`Freeze.fs:23-32`).
2. **The cross-package channel confirms it.** `SymbolProviders.inlineBodies` calls the
   **pre-freeze** entry `Pipeline.analyseSemWithContextFor` and harvests
   `TDecl.Let(…, isInline = true, …)` off that `SemType` tree
   (`SymbolProviders.fs:242-244`, `collectInlineBodies` `:108`). It rewrites bound vars
   but keeps the body — including its `StaticOptimization`/`TraitCall` nodes — whole.
3. **The frozen *shapes* already exist; only their *population* is missing.**
   `StaticOptimization` (`Tast.fs:365`) and `TraitCall` (`:393`) live in the generic
   `TExprG<'ty,'tok>`; `TStaticOptClauseG<'ty,'tok>` (`:455`) is generic, with
   `Frozen.TStaticOptClause = …<FrozenType>` instantiated (`:984`). `TDeclG`/`TExprG`
   are functors over the type domain (`:527`, `:875`), so `Frozen.TDecl` and `TDecl`
   are the *same structure* at `'ty = FrozenType` vs `SemType`.
4. **⇒ Freezing an inline template is a matter of policy, not representation.** Nothing
   in the frozen IR is missing; freeze simply refuses inline decls today.

## What already exists (de-risking the consumer half)

The `FrozenType → SemType` direction is not net-new; it is a **parameterized** walk:

```fsharp
// SemanticInfo.fs:1271 — the one shared thaw, typar policy injected
let rec instantiateWith (declaring: int -> SemType) (methodVar: int -> SemType)
                        (template: FrozenType) : SemType = …
```

with two policies already in use:

- `ofFrozen = instantiateWith (TyTypar Declaring) (TyTypar Method)` — the **rigid**
  identity round-trip; typar leaves become self-describing `TyTypar` markers, no cells
  (`SemanticInfo.fs:1309`). Total over every `FrozenType` case (`:1305`).
- `methodFreshener` — mints **one fresh `TyVar` per method index**, memoized so repeated
  occurrences of an index share a cell (`SemanticInfo.fs:1333`); "Mirrors
  `Infer.instantiateMethodTypars`". This is the inline-splice policy.

And the **tree** lift already exists: `TastConvert.file f` maps a type-conversion over a
whole `TastFileG`; `TastConvert.file ofFrozen` is the named inverse of
`TastConvert.file toFrozen` that `Freeze.run` uses (`TastConvert.fs:7`, `Freeze.fs:71`).

The `Instantiate : SemType[] -> SemType` closures still hanging off `ExternalSymbol`
(`ExternalSymbols.fs:830`) are the **sibling** instance of this same pre-`FrozenType`
debt — `instantiateWith` "is the data form of the legacy `SemType[] -> SemType`
closures" (`SemanticInfo.fs:1316`). The inline-body migration should converge on the
same seam; retiring the `Instantiate` closures is out of scope here but is the same
paydown.

## The one genuinely new component: freeze-inline

A **producer**-side freeze arm that turns an inline template into a `Frozen.TDecl`
instead of dropping it. It is the inverse of `instantiateWith` at the decl level, and
it reuses machinery that already exists — the only reason it isn't already written is
that no caller needed a frozen inline body.

Steps, per inline `TDecl.Let(isInline = true)`:

1. **Quantify** its open `TyVar` roots to `FTTypar` leaves. `Elaborate.freezeTypars` /
   `remapDeclTypars` already perform exactly this cut (`TyVar → TyTypar(Declaring,i)`,
   `Elaborate.fs:146-177,479`) for generalized decls; the inline template's typars are
   a generalized scheme (`Tast.fs:926` `GenericFnSchemes`; `Inline.quantifiedTypars`,
   `Inline.fs:60`), so they are quantifiable by the same path.
2. **Freeze** the quantified body with `toFrozen`. Because step 1 leaves no un-quantified
   `TyVar`, the *existing strict policy* is total on it — the lenient `?ungrounded`
   arm (`Freeze.fs:53-58`) is **not** relaxed further. `StaticOptimization` /
   `TraitCall` / `TStaticOptClause` flow into their already-present frozen arms
   unchanged.
3. **Publish** the `Frozen.TDecl` as `InlineBody.Decl`. `collectInlineBodies` reads
   the *frozen* unit rather than the pre-freeze SemType tree; the member-body twin
   (`SymbolProviders.MemberInlineBody`, `SymbolProviders.fs:42`) freezes the same way.

### Required first: generify `TStaticOptConstraint` (spike finding)

The spike (below) proved the `StaticOptimization` *expression* freezes cleanly, but
uncovered a hidden `SemType` channel that step 2 does **not** convert:
`TStaticOptClauseG.Constraints : EqArray<TStaticOptConstraint>` is deliberately
**not** `'ty`-typed (`Tast.fs:457`, `TastConvert.fs:19-21`), and `TStaticOptConstraint`
embeds raw `SemType` (`TyconEquals of SemType * SemType`, `IsStruct of SemType`,
`SemanticInfo.fs:1496-1503`) whose typar is a **live `TyVar`** over the inline
binding's quantified root (`SemanticInfo.fs:1492`). `TastConvert.clause` copies it
verbatim, so a naively-frozen inline decl still carries `SemType` — and the very
`UnionFind` cell — the plan exists to keep off the boundary. So freeze-inline is
gated on:

- Make `TStaticOptConstraint` `'ty`-generic (`TStaticOptConstraintG<'ty>` —
  `TyconEquals of 'ty * 'ty`, `IsStruct of 'ty`), retype `TStaticOptClauseG.Constraints`
  to `EqArray<TStaticOptConstraintG<'ty>>`, and route it through `TastConvert.clause`
  so `toFrozen`/`ofFrozen` map it. The frozen form is expressible
  (`TyconEquals of FrozenType * FrozenType`) — this is bounded, mechanical work, **not**
  a fundamental blocker — but it must land *before* the interface retype or the
  "frozen" body is not actually SemType-free. The `Constraints` doc-comment's premise
  (*"no `StaticOptimization` survives the inline pass, so these never reach the frozen
  tree"*, `TastConvert.fs:20`) stops holding the moment inline templates freeze.

Consumer side (assembled from existing parts, not new machinery):

4. **Thaw at splice.** File N+1's `InlineExpansion` fetches `Frozen.TDecl`, then
   `TastConvert.file (instantiateWith declaringFreshener methodFreshener)` with **one
   freshener cache shared across the whole decl**, so every occurrence of a given typar
   leaf maps to the same fresh N+1-owned cell. The result is a `SemType` `TDecl` whose
   every cell is N+1's; today's `Inline.substType`/`freshen`/SRTP-resolution then runs
   **unchanged**.

The backward-flow hole closes by construction: a `Frozen.TDecl` contains no `TyVar`
cell to share; thaw is the sole immutable→mutable transition and it allocates.

## Interface change

- `InlineBody.Decl : Frozen.TDecl` (was `TDecl`). This is the debt being paid: the
  inline channel now speaks `FrozenType` like the rest of the oracle.
- Fold the body onto the resolved entry rather than a separate keyed lookup (the
  simplification identified in the multi-file discussion): the head that resolves a
  symbol *is* the identity-correct entry, so an optional lazy `Frozen.TDecl` field on
  `ExternalSymbol`/`ExternalMember` retires **all three** current carriers —
  `TryLookupInlineBody` (by key), `TryLookupInlineBodyByName` (the residual by-name
  fallback, `ExternalSymbols.fs:784-789`), and `MemberInlineBody`
  (`SymbolProviders.fs:42`, whose only reason to be separate — key-agreement at store
  time — is satisfied for free by the entry carrying its own finalized key). Keep the
  body field **lazy** so a consumer that never splices an inline pays nothing.

## Carrier constraint (cross-link, not restated)

Frozen inline bodies still **never live in the runtime artifact** — no F#-style
embedded `FSharpOptimizationData` resource in the `.dll`. The canonical statement and
rationale (multi-target ⇒ a CLR `.dll` is the wrong home for metadata a JS target
reads; OCaml `.cmi`/`.cmx` sidecar model) is
[publishing-format-plan](publishing-format-plan.md) §"Runtime artifact ≠ distribution
package". Freezing changes the body's *shape* (`SemType` → `FrozenType`), not its
*carrier*: intra-assembly it is a live in-memory `Frozen.TDecl` for the one build;
inter-assembly it rides the target-neutral sidecar/source channel exactly as now.

## Design constraints

1. **Freeze stays strict.** freeze-inline must quantify *before* `toFrozen`, not relax
   the `TyVar` hard-error. If a quantified inline body still carries an un-quantified
   root, that is a real inference bug and should fail loud, exactly as for a
   non-inline decl.
2. **One shared freshener cache per thawed decl.** A per-node cache would give two
   occurrences of the same typar independent cells and break the body's internal type
   links. Cache scope = the decl.
3. **SRTP survives the round-trip losslessly.** The frozen arms exist; the plan stakes
   itself on freeze *populating* them faithfully for a real `when ^T` body. Prove it
   first (spike below) before building the rest.
4. **No `SemType` re-widening.** Once `InlineBody.Decl : Frozen.TDecl`, nothing on
   `IExternalSymbolProvider` returns `SemType` except the sibling `Instantiate`
   closures (noted above). Do not add a new `SemType` escape hatch to make a splice
   site convenient — thaw is the seam.

## Opening spike — DONE

`test/XParsec.FSharp.Codegen.Clr.Tests/InlineFreezeThawSpikeTests.fs` freezes a
`when ^T : …` SRTP inline (the `kindOf` shape from `StaticOptimizationTests`) through
`TastConvert.decl toFrozen` and asserts the outcome. Result:

- **Representable (policy, not fundamental):** the `StaticOptimization` expression
  survives freeze — all clauses retained, clause bodies become `FrozenType`, the
  result type freezes (`FTConst`), the open typar becomes an `FTTypar` leaf. The core
  premise holds: an inline SRTP body *can* be frozen.
- **The one gap:** the clause `Constraints` are copied verbatim and still expose a
  `SemType` `TStaticOptConstraint` — reference-equal to the pre-freeze `UnionFind`
  cell. ⇒ the `TStaticOptConstraint` generification above is a hard prerequisite.

Not yet exercised (deferred to implementation, not the gate): the real
`mkMethodQuantEnv` quantization (the spike used a placeholder typar leaf, sufficient
for representability) and thaw-then-splice equivalence at a call site. Neither is at
risk given the expression tree froze; both are index-order / plumbing, not
representability.

The spike test asserts the gap *as it stands today* — it will (correctly) go red once
`TStaticOptConstraint` is generified, at which point it should be flipped to assert the
inverse (freeze leaves **no** shared `SemType` cell) as the landing check.

## Phasing

- **Phase 1:** freeze-inline (producer), the decl-scoped thaw glue (consumer),
  `InlineBody.Decl : Frozen.TDecl`, channel rewired through the frozen unit. Both the
  cross-package and (future) intra-assembly paths read frozen bodies. No change to
  splice semantics — same lowering, fresh cells.
- **Phase 2 (separate, enabled):** retire the `ExternalSymbol.Instantiate` closures
  onto `instantiateWith`, making `IExternalSymbolProvider` `SemType`-free in full. Out
  of scope here; same paydown.

## To verify during implementation

- Enumerate every current reader of `TryLookupInlineBody`/`…ByName`/`MemberInlineBody`
  and confirm each obtains the identity-correct entry so the folded lazy field serves
  it (operator/desugared heads reach it via `TryLookup name → .InlineBody`).
- Confirm the `null`ary intrinsic body special-case (`Inline.nullaryIntrinsicValueBody`,
  `Inline.fs:42`) survives freeze — an operand-less `(# … #)` body with no typars is the
  identity case and must thaw to itself.
- Confirm `ParamAttrs` (the `[<CallAtMostOnce>]` channel, `ExternalSymbols.fs:697`) is
  domain-neutral and rides unchanged.
