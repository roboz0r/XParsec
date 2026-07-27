# Side-table identity hardening

*Ephemeral. Scopes one body of work; delete when it lands.*

Follows the frozen-SoA pool arc, whose plan doc has been deleted now that the work landed
(the code is its record). That work made the defect below **loud** — `TastPools.toPools`
now runs on every compile and faults on it — but did not make it **impossible**. This plan
does the second part.

Cites here name a **function or field**, never a line: this plan is about premises rotting
unnoticed, and a line number is the fastest-rotting cite there is.

## The defect

**A side table keyed by a `NodeKey` that no node in the frozen tree bears.** It fired four
times during the pool arc, each time as a `failwith` in `TastPools.toPools`' `binderIdOf`
(and its lambda-space twin `lambdaIdOf`):

| # | Site | Cause |
|---|---|---|
| 1 | module-level tuple destructuring | filed under `PatEnclosedBlock`, a node `translatePat` **erases** |
| 2 | `let (x) = 5`, `let (x: int) = 5` | filed under the paren/annotation head; every reader looks up the inner binder |
| 3 | elided E1 format-alias binding | recorded `TopLevelNames` for a decl the tree never contains |
| 4 | unpublished inline template | pruned decl, retained entry |

Note #2: it was a **silent** wrong answer for as long as it existed — name and typar arity
lost, no crash — until pooling turned it into a fault. That is the shape of the risk. The
fault is a symptom we happen to get; it is not the guarantee.

There is a second, related failure this plan also addresses, because the pool arc turned up
**five** instances of it: a comment asserting a premise that has since become false (in every
case "`Freeze` partitions inline templates out of `Decls`", dead since `07393c50`). No test
can catch a false *rationale*. See "Tier 0".

## Two causes, not one

- **Kind errors** (#1, #2) — the key names a node of the wrong sort. A *type* can prevent
  these outright.
- **Reachability errors** (#3, #4) — the key names a real binder whose decl was then dropped.
  No key type can prevent these; they need a check where both tree and tables are in hand.

## Inventory

Now that the freeze yields pools, the `Map<NodeKey, _>` forms are **freeze-internal**
(`TastFileG` no longer leaves `Freeze`), so changing their key type is cheap — the blast
radius is `PassContext` → `Freeze` → `toPools`, not the whole compiler.

| Table | Pool form | Producer | Discipline today |
|---|---|---|---|
| `ModuleMembers` | `(BinderId * _)[]` | `Elaborate.elaborateBinding` | guarded by `TastWalk.patBinder` of the head pattern — **convention** |
| `TopLevelNames` | `(BinderId * _)[]` | `Elaborate.elaborateBinding` | same — **convention** |
| `BindingTyparArities` | `(BinderId * _)[]` | `Elaborate.elaborateBinding` | same — **convention** |
| `GenericFnSchemes` | `(BinderId * _)[]` | `Elaborate.recordGenericFnScheme` | `binder: NodeKey` **parameter** — weakest |
| `ClosureReprs` | `(BinderId * _)[]` | `Regions.closureReprSnapshot` | **filtered against `TastWalk.declBinders`** ✅ |
| `BindingValReprs` | `(BinderId * _)[]` | `TastPools.bindingValReprs` | **derived from the columns it ships with** ✅ |
| `FunVerdicts` | `(ExprPoolId * _)[]` | `InferApp` | lambda id space, separate concern |

Two are already honest by construction, and by **two different mechanisms** worth naming:
`closureReprSnapshot` *intersects* with the tree's binder set; `BindingValReprs` is *computed
from* the tree — it is read off the POOLED lambda spine in `toPools`, after the columns are
filled, so it cannot name a node the columns do not hold. Everything above them relies on a
human writing the right expression.

## Root cause: `NodeKey` is the over-wide type

`NodeKey` addresses any node — patterns, bindings, expressions, synthetics. A binder key is a
`NodeKey`; so is `CstKeys.ofBinding b`; so is `CstKeys.ofPat b.headPat`. `Dictionary<NodeKey,
'V>` accepts all of them.

`Elaborate.recordGenericFnScheme` is the hazard in one function: it takes
`binder: NodeKey`, whose doc has to *explain in prose* that it is "distinct from
`CstKeys.ofBinding b`, which stays the ANALYSIS key" — and then uses `CstKeys.ofBinding b`
four lines later, for the lookup. Two keys of the same type in one scope, one correct, one
catastrophic, told apart only by a comment.

## Proposal

### Tier 0 — the cheapest fix, do it first

Comments that assert a structural premise get a **cite**: name the type/field/function whose
doc is the source of truth, so drift is one grep from visible. Five of these were fiction and
every gate passed. This is not a code change; it is a review rule plus a sweep of the
binder/identity comments. Do it in the same pass as Tier 2, on the files touched.

### Tier 1 — the real fix, applied selectively: no key at all

A fact attached to a node cannot desync from it. The strongest form of that is not even a
column: a fact that is a **projection** of what it is attached to cannot be stored wrong
because it is not stored at all — `BinderNaming` is `BinderNaming.ofKey` of the binder's own
key (the `BinderNamings` column this plan originally pointed at has since been deleted for
exactly that reason), and a node's `ExprShape` is `ExprPayload.shape` of its payload.

Where a fact is genuinely independent of the node, a **column** — positionally aligned, no
key — is the next best thing. `TopLevelNames` and `BindingTyparArities` are per-binder
scalars in exactly that shape and should become columns on the binder pool.

`ModuleMembers` (a record), `GenericFnSchemes` (a list) and `ClosureReprs` are also per-binder
but not scalar; columnising them is a bigger change and is **not** proposed here. `FunVerdicts`
is already on the lambda id space.

### Tier 2 — the containment: `BinderKey`, a newtype whose only constructors are projections

```
[<Struct>] type BinderKey = private BinderKey of NodeKey
module BinderKey =
    val ofPat      : TPatG<_,_> -> BinderKey voption   // = TastWalk.patBinder
    val ofForTo    : ...        -> BinderKey
    val ofTypeDecl : ...        -> BinderKey seq       // = TTypeDeclG.boundKeys
    val toNodeKey  : BinderKey  -> NodeKey             // one-way, for the pool remap
```

The `private` constructor is the whole mechanism: `CstKeys.ofBinding b` cannot be filed into a
`Dictionary<BinderKey, _>`, because there is no way to obtain a `BinderKey` from it. `#1` and
`#2` become compile errors. The projections already exist and are already the single
definitions of "is a binder" — this only stops anything *else* reaching the sink.

Retype: the four `PassContext` binding dictionaries + `PassContext`'s `SideTable<'V>`, the
`TastFileG` map fields, and `toPools`' remap input.

### Tier 3 — the backstop for reachability, and the filter/fault distinction

**Landed as a policy statement and a test, NOT as a second walk.** The check this tier asked
for already exists: `Freeze.run` *is* `toFrozenFile … |> TastPools.toPools`, and `toPools`'
`binderIdOf` resolves every binder-keyed table's keys against the pool's own binder
enumeration, faulting by table name on a miss. A separate walk at `Freeze.run` would be a
second whole-file binder enumeration standing beside the pool's — and it would have to be a
*new* one, since `TastWalk.declBinders` deliberately excludes type-decl binders
(`BinderKey.ofTypeDecl` is the other half). So the tier's content is the policy, not the code:

- **Fault by default.** Every binder-keyed table is remapped through `binderIdOf` and so
  inherits the check; a fault is fixed at the **producer**, by pruning the entry where the
  declaration is pruned (defect #3: `Elaborate.translateModuleElem` records no binder for an
  elided E1 alias; defect #4 stopped existing when publication became additive at `07393c50`
  and no decl is pruned for it).
- **Filter** only where the producer can argue at itself that its surplus is inert —
  `Regions.closureReprSnapshot` is the one, and it filters in its own body rather than at the
  validation site, which is where the argument stays checkable.

The rule is stated once, at `binderIdOf`. The negative test cannot mint a bad key (Tier 2
made the constructor private), so it stages the defect's real shape: a genuine binder whose
declaration is removed while one table's entry stays.

## Staging

- **G.1** `BinderKey` + retype the sinks (`PassContext`, `SideTable`, `TastFileG`, `toPools`
  remap). Compiler-driven; every producer either already has a projection in hand or is a
  genuine bug. Fold the Tier 0 comment sweep into the files touched. *Gate: green.*
- **G.2** ✅ Freeze-time validation. No second check: the fault-by-default policy is stated
  at `binderIdOf`, whose side-table failure now reads as the stale-entry defect it is rather
  than borrowing the reference resolver's wording, and three tests pin that a retained entry
  for a removed declaration faults *by table name* (and that pruning both pools cleanly).
- **G.3** Columnise `TopLevelNames` and `BindingTyparArities` onto the binder pool. *Gate:
  green; blob size may move — record it, do not chase it.*

Each is independently landable; G.3 is optional and lowest value.

## Non-goals

- **The CLR emit's `NodeKey` identity.** `HolderPlan.create`, `Emit.discoverClosures` and
  everything downstream (`StaticFn`/`ModuleValue` key sets, capture sets) identify a binding
  by `NodeKey`; `Layout.buildUnit` deliberately rebuilds the maps via
  `TastUnpool.nodeKeyedSideTables` for them — one named seam, so closing it is one edit.
  Rekeying that chain is the whole CLR emit and is a separate body of work — **if it is
  worth doing at all**, which this plan does not assert.
- **`SemType` stays a DU.** The columns are monomorphic in `FrozenType` and stop at the freeze
  boundary. Nothing here changes that.
- **`| pat as name ->`** remains unsupported (`translatePat`'s `Pat.As` arm drops the alias, so
  no `TPatG` node carries the key). It is a *tree modelling* gap needing a `TPatG` case, not a
  side-table one. Out of scope; queue separately.
- **`TastAccessor.patBinderId` returning `ValueNone`** on an un-interned binder is a silent
  fall-through. Tier 3 covers the tables; this accessor is not a table and is
  sound today because every pat reachable from a root is interned. Revisit only if that
  invariant weakens.

## Gates

Build 0 warnings / 0 errors; SA **1169**, Codegen.Js **507**, Codegen.Clr **1417**, Vesper
**51**; **no golden may move** (`git status --porcelain | rg -c 'goldens/|Codegen\.Conformance/'`
→ `0`). A moved golden is a real behaviour change: stop and report it.
