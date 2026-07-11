# A reusable CST walker with `Type` coverage — type-head resolution as its first consumer

## Why

The name-resolution boundary doctrine is *"spelling → identity resolution happens at
exactly one layer — NameResolution"* ([name-resolution-boundary-plan](name-resolution-boundary-plan.md)).
Written **type-annotation heads** are the one structural exception: they resolve late,
in Unification's `translateType` → `tryResolveExternalType` (`Passes/Unification/Translate.fs:502`),
reaching the spelling-face `ctx.Resolver` — the boundary's permanent escape hatch
(blocker 2). Closing it *structurally* means NameResolution resolves every written
external type head to a `SymbolKey` and stamps it, so Translate reads the stamp on the
**store** face and never resolves a spelling.

That was judged "build a general `Type`-node walker" and deferred. This doc scopes that
walker. The trigger is **architectural consistency**, not an unblock: no pending sprint
requires it (multi-file resolves cross-file *by name* through the existing provider
stack and its new work is a frozen-TAST → provider projection over `TastWalk`, not a
CST `Type` walker; inline-bodies is TAST-domain). It is worth doing because (a) it puts
all spelling→identity in one layer, retiring blocker 2's escape hatch; (b) it lets the
deferred resolution cache cover type annotations (the resolver face becomes the sole
string seam); and (c) the repo will want a general CST walker regardless — it has one
for TAST (`TastWalk`) and none for the CST.

## Confirmed premises (scout results — file:line)

1. **The CST type is `Type<'T>`, 18 cases** (`src/XParsec.FSharp/Expr.fs:66-101`):
   `ParenType`, `FunctionType`, `TupleType`, `StructTupleType`, `VarType`, `NamedType`,
   `GenericType`, `SuffixedType`, `DottedType`, `ArrayType`, `WhenConstrainedType`,
   `SubtypeConstraint`, `AnonymousSubtype`, `Null`, `UnionType`, `ILIntrinsic`,
   `AnonRecordType`, `MeasureType` (+ `Missing`/`SkipsTokens`).
2. **The only structurally-complete `Type` enumeration is a debug token-emitter.**
   `AstTraversal.walkType` (`src/XParsec.FSharp/AstTraversal.fs:608-738`) matches all
   cases and recurses correctly, but is parameterized by an `AstVisitor<'T>` whose hooks
   are `VisitToken`/`EnterSection`/`WriteLine` (a pretty-printer). It is `'T`-generic and
   exhaustive — a **case-coverage template**, not a reusable semantic walker.
3. **The passes' reusable CST walker is `Expr`-only.** `CstWalk` (`CstWalk.fs`) — the
   `ExprWalker<'env>` / `iterExpr` the passes drive — enumerates every `Expr` case but
   **never descends into a `Type` field** (`CstWalk.fs:268-281`: `Expr.New`/`TypeApp`/
   `TypeAnnotation`/`StaticUpcast`/`DynamicTypeTest`/`DynamicDowncast` recurse only into
   the inner *expression*). There is no `iterType`.
4. **The TAST analogue is the API-shape precedent.** `TastWalk` (`TastWalk.fs`) is the
   reusable `Mapper`/`Iter` over TAST `TExpr` with `MapType`/`VisitX`/`OverrideX` hooks.
   No CST equivalent exists.
5. **A complete `Type` descent already lives in NameResolution.**
   `implicitMemberTypars.walkTy` (`Passes/NameResolution/MemberRegistration.fs:174-209`)
   walks every recursive `Type` case (`NamedType`/`Null`/`Measure`/`ILIntrinsic` are
   leaves) to collect free typar names — a proven descent skeleton, in the right pass,
   differing only in its **leaf action** (collect typars vs. resolve+stamp a head).
6. **The per-head resolver already exists.** `tryResolveExternalTypeKey`
   (`Passes/NameResolution/Scope.fs:82`) is exactly `written head string + arity →
   external SymbolKey`, opens-aware, handling every shape in its `keyOf` (`:56-69`).
   NameResolution already stamps it at `Expr.New` (`Scope.fs:834`) and `Expr.TypeApp`
   (`:848`) via the `ResolvedType` side table.
7. **`Type` nodes hang off many parents** — the positions a walker must reach (all
   `src/XParsec.FSharp/Expr.fs` unless noted):
   - Binding: `ReturnType` (`:192`) on `Binding.returnType` (`:207`); param annotations
     live in `Pat`.
   - Patterns: `Pat.Typed` (`:446`), `Pat.TypeTest` (`:450`), `Pat.TypeTestAs` (`:451`).
   - Expressions: `Expr.TypeApp` (`:281`), `Expr.New` (`:294`), `Expr.TypeAnnotation`
     (`:318`), `Expr.StaticUpcast` (`:319`), `Expr.DynamicTypeTest` (`:320`),
     `Expr.DynamicDowncast` (`:321`).
   - Object/interface: `ObjectConstruction`/`InterfaceConstruction` (`:223-224`),
     `InterfaceImpl` (`:233`), `InterfaceSpec` (`:516`), `ClassInheritsDecl` (`:621`),
     `AdditionalConstrInitExpr.Delegated` (`:588`).
   - Anon records: `AnonRecordField` (`:115`); type args: `TypeArg.Type` (`:118`).
   - Constraints: `Constraint.Coercion`/`Enum`/`Delegate`/`Default` (`:133-169`),
     `StaticOptimizationConstraint.WhenTyparTyconEqualsTycon` (`:178`).
   - Signatures: `ArgSpec` (`:523`), `CurriedSig`/`UncurriedSig` (`:527-529`),
     `MemberSig` (`:531`), union-case field types (`:644-655`), `TypeDefn.Abbrev`
     (`:700`); in `Signatures.fs`: `ValSig` (`:11`), record-field `typ` (`:63`),
     `TypeSignature.Abbrev` (`:75`).
8. **NameResolution's walk reaches `Type` heads at exactly two opportunistic points**,
   both driven off the enclosing *expression* — `Expr.New`'s bare-`NamedType` head
   (`Scope.fs:818-837`) and `Expr.TypeApp`'s receiver + arity (`:838-851`). Every other
   position (annotations, casts, patterns, val/member sigs, inherit clauses, field
   types) is untouched by NameResolution and reached only later by `translateType`.

## The scope fork

**A general reusable CST walker.** Grow `CstWalk` into the CST analogue of
`TastWalk` — an env-threaded `Iter`/`Mapper` with per-node hooks including `VisitType`,
covering `Type`/`Pat`/signature nodes, not just `Expr`. Type-head stamping is its
**first consumer**; later it absorbs the scattered ad-hoc descents (`translateInheritArg`
`MemberRegistration.fs:652`, `walkTy` itself, Conformance's type reaches, and the
*structure* of Unification's `translateType`).

## Design

- **API shape:** mirror `TastWalk`'s `Iter`/`Mapper` records. An `env`-threaded walker
  whose hooks include `VisitType : env -> Type<'tok> -> unit` (and, for the mapper form,
  `MapType`). Recursion delegates through the walker exactly as `iterExpr` does, so a
  consumer overrides only the node kinds it cares about. Use `AstTraversal.walkType`'s
  case list (premise 2) for exhaustiveness and `walkTy`'s leaf/leaf-recursion decisions
  (premise 5) as the semantic reference. The walker must also descend the premise-7
  parents that `CstWalk` currently skips (`Pat`, signatures, `Binding.returnType`,
  inherit/interface clauses).

- **Type-head stamping consumer:** at each `NamedType` / `GenericType` head, when it
  misses every project-local registry (the same local-first order `translateType` uses),
  call `tryResolveExternalTypeKey ctx name arity` and, on a hit, stamp a new
  `SideTable<SymbolKey>` — `PassContextResolution.ResolvedTypeHead`, keyed by the `Type`
  node's `NodeKey`. Arity is the *syntactic* type-arg count (available on the CST node),
  which equals `translatedArgs.Length`. An **abbrev** stamps its own key (the resolver's
  `keyOf` already returns it); Translate dealiases on read, unchanged.

- **`Translate.tryResolveExternalType` read-side:** replace the `ctx.Resolver` probe with
  a stamp read by the `Type` node's `NodeKey`; on a hit, `ctx.Provider.TryLookupType key`
  (store face) + build the `SemType` exactly as today (arg translation, wildcard
  freshening, abbrev `instantiateDeclaring`); on a miss, the existing opaque/`TyVar`
  fallbacks. The five call sites (`Translate.fs:234/254/328/477/769`) thread the node
  key. This removes the **last** resolver reach from Unification.

- **What stays in Translate — by construction.** The `_` in `Box<_>` mints a fresh
  inference `TyVar`, and nested args are recursively-translated live `SemType`s — neither
  is expressible upstream (NameResolution has no cells). The walker stamps only the
  **immutable head identity**; the name→*type* construction remains inference-resident.
  This is the head-key / body-construction split the boundary doc already settled.

## What it closes — and what it does not

- **Closes:** blocker 2, structurally. Unification/Freeze no longer reach `ctx.Resolver`;
  Translate speaks the store face. The boundary doctrine holds without an escape hatch.
- **Enables:** the deferred resolution cache to cover type annotations — the resolver
  face becomes the single string surface it can memoize.
- **Does NOT, alone, remove `ctx.Resolver`.** Two readers remain: NameResolution (its
  legitimate home) and codegen's Stage-5 by-name channel. Literal
  "`ctx` exposes only the store" additionally needs Stage 5 done and NameResolution's own
  resolver access threaded off `ctx` (a constructor arg) so the member can be deleted —
  out of scope here.

## Risks

- **A missed `Type` position → silent fallback.** An unvisited position leaves its head
  unstamped; Translate then falls back to an opaque `TyConst`/`TyVar`, and the annotation
  quietly stops resolving to its real external type — no crash. Mitigation: derive the
  visit set directly from the premise-7 inventory and assert it against every
  `translateType` call site; a corpus pass catches the rest. (At this prototype stage
  silent regressions are an accepted risk — correctness of the architecture leads.)
- **`NodeKey` agreement.** NameResolution must stamp with the same `NodeKey` Translate
  reads. Both derive it from the `Type` node's anchoring token (`NodeKey.ofToken` /
  `CstKeys`); pin the exact token per `Type` case once and share it.

## Phasing

- **Phase 1:** the walker, `ResolvedTypeHead` side table, the Translate
  read-side, `tryResolveExternalType` off `ctx.Resolver`. Type-head stamping is the sole
  consumer. Delivers blocker-2 structural closure.
- **Phase 2:** migrate the ad-hoc `Type` descents (`translateInheritArg`,
  `walkTy`, Conformance) onto the walker; retire the duplicated case lists.

## Non-goals

- Removing `ctx.Resolver` entirely (needs codegen Stage 5 + the NameResolution rethread).
- Anything in multi-file / inline-bodies — independent; this neither blocks nor unblocks
  them.
- Changing `SemType` construction, wildcard freshening, or abbrev dealias — all stay in
  Translate.

## Key files

- `src/XParsec.FSharp/Expr.fs:66` — `Type<'T>` definition (the 18 cases + the premise-7
  parents).
- `src/XParsec.FSharp/AstTraversal.fs:608` — `walkType`, the complete case-coverage
  template (debug visitor).
- `CstWalk.fs` — the `Expr`-only reusable walker to extend (B) or leave alongside (A).
- `TastWalk.fs` — the `Iter`/`Mapper` API-shape precedent.
- `Passes/NameResolution/MemberRegistration.fs:174` (`implicitMemberTypars.walkTy`) — the
  proven NameResolution-side `Type` descent skeleton.
- `Passes/NameResolution/Scope.fs:82` (`tryResolveExternalTypeKey`) — the per-head
  resolver the stamping consumer calls.
- `Passes/Unification/Translate.fs:502` (`tryResolveExternalType`) — the read-side that
  moves from `ctx.Resolver` to the stamp + store face.
