# The name-resolution / opens overhaul

> **Origin.** This was the "other half" of the original *overhaul
> `IExternalSymbolProvider`* idea, split out of the (now-completed)
> `SemType → FrozenType` work as its explicitly out-of-scope follow-up. It is
> **independent of that `FrozenType` work and was never needed for `truncate`**:
> the frozen-type work touched the descriptor *payload* (what an external
> symbol's signature *is*);
> this work touches the resolver's *lookup* side (how a written name *finds* a
> symbol). The provider stays string-keyed throughout — that boundary is correct
> and this plan does not move it.

**Status (2026-06-04): the foundation already landed; this plan scopes the
residue.** When the frozen-type note was written, name resolution did not yet
maintain an open stack. It does now: `OpenScope` (`CstWalk.fs:11–19`) carries the
active prefixes (most-recent-first shadowing) plus module-abbrev aliases, the
`candidates` / `tryResolve` / `tryQualify` primitives (`CstWalk.fs:26–70`) drive
short-name → fully-qualified-candidate resolution, and `[<AutoOpen>]` is honoured
end to end (`VesperLib.fs` records auto-open prefixes →
`ReferencedProject.fs` folds them into the provider's `AmbientOpenPrefixes` →
`SideTables.fs` seeds `PassContext.Resolution.AmbientOpenScope`, probed strictly
behind explicit opens). So the load-bearing idea from the note — *"name
resolution maintaining an `open` stack, resolving `name * opens`, keeping the
provider string-keyed"* — is **done**. What remains are four discrete gaps the
note bundled in but which were never built.

## What already works (do not rebuild)

- **The open stack.** `OpenScope.Prefixes`, accumulated as `CstWalk.walkModuleTreeWith`
  walks the module tree (running accumulator for plain modules; constant prelude
  for `module rec` / `namespace rec`), stamped per element onto
  `ctx.Resolution.OpenScope` (`NameResolution.run`).
- **First-hit-wins short-name resolution.** `OpenScope.tryResolve` tries the
  abbrev-expanded name as written, then each active prefix in shadowing order;
  the value-returning sibling `tryQualify` returns the qualified name for
  interning / diagnostics. Used by `NameResolutionScope.resolveIdent` over
  `ctx.Provider.TryLookup`, and by external type / union-case resolution.
- **Module abbreviations.** `module R = A.B.C` ⇒ `OpenScope.Abbrevs`, expanded on
  the head segment of a dotted name before probing.
- **`[<AutoOpen>]` (ambient prelude).** Provider-contributed
  `AmbientOpenPrefixes`, seeded into the ambient scope and probed **behind** all
  explicit opens — matching F#'s "prelude loses to an in-file `open`" order.
- **The provider stays string-keyed.** `IExternalSymbolProvider`
  (`ExternalSymbols.fs:373–438`) takes compiled-name strings and knows nothing
  about opens; the resolver applies prefixes before probing. **This is the
  intended end state — keep it.**

## The remaining gaps

Each is independent and separately landable; ordered by leverage. None blocks the
others, and none touches the descriptor payload or the provider's string key.

### Gap 1 — `[<RequireQualifiedAccess>]` suppression — **DONE (2026-06-12)**

Landed: `ExternalUnionCase.IsRequireQualifiedAccess` (additive field), populated
from `ExtractCtx.RqaTypes` (read off the union's `[<RequireQualifiedAccess>]`
attribute during `extractUnionBody` via `isRequireQualifiedAccess`), stamped onto
the reverse case-name index in `TyparCapture.toProvider`. Bare-name suppression is
gated on `not uc.IsRequireQualifiedAccess` at every site that brings an external
case into scope without a qualifier: `Scope.fs` (`resolvesAsBareExternalCase` used
by `resolveIdent` + `isCtorName`), the Unification chokepoint
`tryExternalCasePattern` (covers bare ctor-as-value and bare patterns), and the
independent Freeze recognisers (`Patterns.isExternalUnionCase`, `Resolve.tryCtorRef`).
A bare RQA case now diagnoses "Unresolved identifier" in expression position and
falls through to a binder in pattern position (matching F#); the qualified form
(`Color.Red`) still resolves. Tested in `OpenResolutionTests` (resolution side) and
`VesperLibTests` (extraction + index side).

The original notes below describe the design as built.



**Current behaviour:** a non-qualified short name for an external union case or
module member resolves via `tryResolve` *regardless* of whether the declaring
union/module is `[<RequireQualifiedAccess>]`. F# forbids the short form for RQA
declarations (`Color.Red`, never bare `Red`); we currently accept it. This is a
*false accept* — we resolve names F# would reject.

The attribute is already detected and used on our own internal types
(`ExternalSymbols.fs` and elsewhere); what is missing is the **resolution-side
suppression** for *external* RQA declarations.

**Shape of the fix:**
- Record an `IsRequireQualifiedAccess` flag on the external shapes that can carry
  it — `ExternalTypeShape` (for modules-as-types and unions) — populated during
  extraction (`VesperLib.fs` / metadata) from the `RequireQualifiedAccess`
  attribute. Additive field, central default `false`
  ([[feedback_additive_changes_with_aliases]]).
- At the short-name resolution sites that bring *members / cases* into scope —
  `TryLookupUnionCase` consumers (`Scope.fs` bare-case path) and any member
  short-name path — reject a candidate whose declaring shape is RQA *unless the
  name was written qualified*. `tryResolve`/`tryQualify` already distinguish "bare
  name" from "prefix-applied candidate"; the RQA check keys off whether the hit
  came from the bare/abbrev-expanded head or from a prefix that fully qualifies
  the declaring type.

**Done when:** a corpus file that uses a bare RQA case name is diagnosed
"Unresolved identifier" (matching F#), and the qualified form still resolves.
Mirror `pars.fsy` semantics; reject at the right layer
([[feedback_match_fsharp_grammar]], [[feedback_relax_parser_defer_to_typecheck]]).

### Gap 2 — type-vs-module shadowing (namespace-tree semantics)

**Current behaviour:** `CstWalk.implFileElems` flattens nested modules into a
single element stream, and a `namespace`'s name is added as an implicit open
prefix, but **nested namespaces are not distinguished from nested modules**, and a
type and a module sharing a name in the same scope are not disambiguated by F#'s
"type wins for value access, module wins for member access" rules. Today the
prefix list is a flat string list with no node kind.

**Why it bites:** when a type `T` and a companion module `T` coexist (the common
F# pattern), or when an `open` brings in both a nested namespace and a like-named
type, candidate generation cannot express "this prefix is a type, that one is a
module." First-hit-wins on a flat string list approximates it but will mis-rank in
the shadowing cases.

**Shape of the fix:**
- Tag each `OpenScope.Prefixes` entry with its **node kind** (namespace / module /
  type) rather than a bare string, so `candidates` can apply F#'s precedence
  (type-vs-module) when two candidates collide. Likely a small record per prefix;
  keep the common case (a plain dotted namespace) cheap.
- Distinguish nested `namespace` from nested `module` in `walkModuleTreeWith`
  (today both just become prefixes). A nested namespace contributes a prefix to
  the *whole* file region under it; a module contributes to elements after it
  (the running-accumulator vs prelude distinction already encoded for `rec`).

**Caution:** this is the gap most likely to want a real namespace tree rather than
a prefix list. Resist building one until a corpus case forces it — the flat list
plus a kind tag may cover everything Vesper's own libraries need. Surface the gap
with a diagnostic before generalising ([[feedback_excise_cosmetic_warts]]).

### Gap 3 — resolution caching

**Current behaviour:** no caching of `name * OpenScope → symbol`. Each
`resolveIdent` rebuilds the candidate list and re-probes the provider per use
site. Side tables (`PassContextResolution`) cache the *result per `NodeKey`*, but
the resolution *work* (candidate generation + provider round-trips) is redone for
every textually-identical reference. The note's "`OpenScope.tryResolve` cache"
is the unbuilt half.

**Shape of the fix:**
- A per-`PassContext` memo keyed on `(OpenScope identity, name)` →
  resolved-symbol-or-miss, consulted inside `tryResolve` (or one layer above it
  in `resolveIdent`). `OpenScope` values are stable per element (set once by
  `walkModuleTreeWith`), so the scope can be identity- or structurally-keyed; a
  given element's scope is shared by all its references, giving a high hit rate.
- Cache **misses too** — an unresolved bare name probed against N prefixes is the
  expensive case worth memoising.
- Keep it per-file (per `PassContext`), matching the existing side-table
  lifetime; no cross-file global cache (that interacts with provider identity and
  is out of scope).

**Sequencing:** do this **after** Gaps 1–2, not before. Caching a resolver whose
*semantics* are still changing (RQA suppression, kind-tagged prefixes) just means
invalidating the cache design twice. The note already flagged this as a pure
performance refinement, not a correctness item — and the
semantic-analysis/codegen stages are too immature to benchmark meaningfully yet,
so land it when there is a measured hot path, not speculatively
([[feedback_hotspot_engineering_diminishing]]).

### Gap 4 — operator-form long idents (`A.(+)`, `(*)`) — **DONE (2026-06-12)**

Landed: `OperatorNames.qualifiedOpName` (the single shared translation, built on
`ofIdentOp` → `ofParenSymbolic`) turns `A.B.(+)` into the compiled name
`A.B.op_Addition`. The `Scope.fs` TODO arm now resolves `LongIdentOrOp.QualifiedOp`
through `tryResolve` (stamping `ExternalValue` for Freeze, diagnosing an unresolved
qualified name on a miss); the same translation feeds Unification's `qualifiedNameOf`
and Freeze's `translateIdent`, so the qualified-operator form is keyed identically
across resolver, typer, and projector. Only the *symbolic* op segment is covered; a
non-symbolic op-name (active-pattern / nil / range) still surfaces the gap. Tested
in `OpenResolutionTests`. The bare-operator form continues to resolve via the
prelude unchanged.

The original notes below describe the design as built.



**Current behaviour:** an explicit TODO at `Scope.fs:361–362` — operator-form long
idents need their own resolution story and are currently skipped rather than
resolved. A qualified operator reference like `A.B.(+)` does not route through
`tryResolve` the way a value long-ident does.

**Shape of the fix:** translate the operator segment to its compiled name
(`(+)` → `op_Addition`, matching the lexer-emitted op names —
[[reference_lexer_vs_semantic_op_names]], [[reference_xparsec_postfix_no_aux]])
*before* candidate generation, so the existing `tryResolve` machinery handles the
qualified form unchanged. The bare-operator case already resolves via the prelude
(`AmbientOpenPrefixes` carrying the arithmetic-operators module); this gap is only
the *qualified* form. Smallest of the four; do it whenever the TODO becomes a
real corpus blocker.

## Out of scope

- **Moving the provider off string keys.** Deliberately *not* done — the
  string-keyed boundary is the design, confirmed by the frozen-type split landing
  cleanly on top of it.
- **Cross-file / cross-package resolution caching.** Per-file only (Gap 3).
- **Anything in the descriptor payload.** That is the frozen-type work; this plan
  never touches `BuildSignature` / `ExternalSignature` / `FrozenType`.

## Suggested order

Gap 1 (RQA — correctness, false-accept fix) → Gap 2 (type-vs-module shadowing —
correctness, the structural one) → Gap 4 (operator long idents — small, TODO-pinned)
→ Gap 3 (caching — performance, only with a measured hot path). Each lands green
and independently; none is on the critical path for any other active workstream.

**Progress (2026-06-12):** Gaps 1 and 4 landed (see their sections). Gap 2 and
Gap 3 remain *deliberately deferred* per their own guidance — Gap 2 wants a forcing
corpus case before a flat prefix list grows a kind tag / namespace tree, and Gap 3
is a pure performance refinement to land only against a measured hot path. Neither
has a current trigger; first-hit-wins resolution and per-`NodeKey` side-table
caching cover today's libraries.

## Key files

- `CstWalk.fs:11–70` — `OpenScope` type + `candidates` / `tryResolve` /
  `tryQualify`. The center of gravity for Gaps 1, 2, 4.
- `CstWalk.fs:462–561` — `walkModuleTreeWith`: open accumulation + per-element
  scope stamping (Gap 2's namespace-vs-module distinction lives here).
- `Passes/NameResolution/Scope.fs` — `resolveIdent` (the bare-name site; Gaps 1,
  3, 4), the operator-long-ident TODO (`:361–362`), external type / union-case
  resolution.
- `Passes/NameResolution/NameResolution.fs` — `run`, ties the scope to each
  element.
- `ExternalSymbols.fs:373–438` — `IExternalSymbolProvider` (string-keyed; the
  RQA flag rides the shapes it returns, not the interface).
- `VesperLib.fs` / `ReferencedProject.fs` — attribute extraction
  (`[<AutoOpen>]` already; `[<RequireQualifiedAccess>]` for Gap 1).
- `SideTables.fs` — `PassContextResolution` (ambient-scope seeding; Gap 3's memo
  home).
