# Recursion groups: SCC-derived `let rec` components through inference, the TAST and both backends

Ephemeral: delete when the work lands.

## Where it stands

`TastPools.poolLetValue` opens one `SelfFrame` per `let rec` binding over a single variable and
classifies the binding's `Recursion` (`NonRecursive` / `Recursive` / `TailRecursive`) from
references to that one variable inside its own value (`TastPools.fs:172-182`). The frame is live
only while the binding's own value is walked; the body is walked with the outer frames
(`TastPools.fs:131`).

`ElaborateExpr.translateLet` folds a `let rec a … and b … in body` group into nested single
`Let` nodes, `Let(a, va, Let(b, vb, body))`, each with `isRec = true`. `Elaborate.fs:233-236`
does the same at module level, one `TDecl.Let` per binding, and `poolDecl` classifies each with
an empty frame list. The group boundary is gone before the pool walk runs.

`Infer.inferBindingGroup` (`Infer.fs:412-426`) types the whole syntactic group at one level,
bars every sibling's scheme for the duration, and generalises all members together.

`EmitJs` reads `LetView.Recursion` to choose between an IIFE parameter (`NonRecursive`) and a
`const` in a block body (otherwise) for a local `let`, and trampolines a `TailRecursive`
lambda's tail self-calls. The CLR backend reads `Recursion` nowhere; `EmitClosures.walkFreeRefs`
scopes only the binding's own name over its value.

## The defects

For

```fsharp
let rec a x = if x = 0 then 0 else b (x - 1)
and b x = a x
```

1. **Both members classify `NonRecursive`.** `va` references `b`, whose frame does not exist
   yet; `vb` references `a`, whose frame has already closed. This holds at expression and module
   level alike.
2. **JS expression-position lowering throws.** `a` takes the IIFE-parameter path, and `b` is a
   `const` declared inside the IIFE body, so calling `a` raises `ReferenceError`. Module level
   runs only because `const` functions are called after module init; `EmitJs.fs:1124` still
   feeds the wrong `Recursion` to `emitFlatModuleFn`.
3. **CLR closure capture reads an unassigned slot.** `walkFreeRefs` treats `b` as a free
   variable of `a`'s closure, and `EmitBindings.buildLet` stores `a`'s closure before `b`'s slot
   is assigned.
4. **Inference rejects valid F#.** F# generalises each strongly connected component of the
   group separately, so `let rec f x = x and g () = (f 1, f "a")` is accepted (verified with
   `dotnet fsi`), while a genuine cycle `let rec f x = g x and g x = (f 1, f "a")` is rejected.
   `inferBindingGroup` treats the syntactic group as one component and rejects the first program.

Defects 1 and 4 share a cause: the compiler has no notion of a recursion component, only the
`rec` keyword. The component is the SCC of the binding reference graph within a syntactic
group, which is the first client `brainstorm-tarjan-scc.md` lists.

## Decisions to confirm

- **D1.** A recursion group is an SCC of the binding reference graph inside one syntactic
  `let rec … and …` group, in Tarjan's reverse topological order. A singleton without a
  self-edge is not recursive, whatever keyword it was written under.
- **D2.** The components are computed once, in Unification, where generalisation needs them,
  and recorded in the pass context. Elaborate attaches them to the `LetGroup` node; it does
  not re-derive them.
- **D3.** `Let` keeps describing one binding written without `and`: non-recursive, or a
  function referencing only itself, with `isRec` as the source `rec` fact and `Recursion`
  classified as today. A new node, `TExprG.LetGroup of members * components * body` and
  `TDeclG.LetGroup of members * components`, describes one lexical `let rec … and …` group.
  `members` are the bindings in source order, each a record of pattern, value and binding
  token. `components: EqArray<EqArray<int>>` partitions the member indices into strongly
  connected components in reverse topological order. Every member index appears in exactly
  one component, checked at construction. A group identity on `Let` was rejected: the pool
  walk, the JS block lowering, the CLR back-patch and `walkFreeRefs` would each re-derive the
  member set by scanning the `Let` spine, against a contiguity invariant the type cannot state
  and that `EmitJs.InlinableLet` already breaks. One `LetGroup` per component was rejected:
  the tree shape would then carry the analysis, so a split group has to be reordered and
  nested, and every consumer depends on that nesting being right.
- **D4.** The tree keeps source order. A consumer that treats the whole group as one
  recursive unit is correct and merely pessimistic: the JS block lowering and the CLR
  back-patch may ignore `components`. The pool walk, inference and the D6 warning read
  `components`, because a member referencing a sibling in another component is not recursive.
  Value bindings initialise in source order, as F# requires.
- **D4a.** The recursion fact is stored once, per member, as `Recursion`. A component's
  recursion is derivable: a component of two or more members has every member `Recursive` or
  `TailRecursive`, and a singleton component's member says whether it has a self-edge. The
  components carry no `IsRecursive` flag.
- **D5.** The SCC utility is the brainstorm's §2 core and §3 output contract only. Canonical
  numbering (§4) stays with the structural-hash work.
- **D6.** A Vesper-specific warning, `DiagCode.Vesper "V260"`, reports a `let rec … and`
  group that splits into more than one component, naming the members that can be declared
  outside the group and the component order that makes the move legal. The same code reports a
  `let rec` singleton with no self-reference. One warning per syntactic group: a group of
  several components reports the split, and a group of one non-self-referencing component
  reports the redundant `rec`. Names are quoted (`'c'`), which `RecursionGroupTests` pins.
  F# has no equivalent warning. `src/Vesper.*` transliterates FSharp.Core groups literally and
  will trigger it; whether the ports carry the warning or the manifest suppresses the code for
  them is open.

## Steps

Each step leaves the build green. Steps 1 and 2 have no dependency on each other.

### 1. Red tests — DONE

Every red test is a `ptest "GAP: …"`, so the suites stay green. The step that turns a case
green promotes it to `test`.

- `TastPoolsTests.recursionTests`: the two-member group at module level and local level, both
  members expected `Recursive`; a group whose members do not reference each other, both
  `NonRecursive` on plain `Let` nodes; a member tail-calling itself inside a group,
  `TailRecursive`. Four tests added; three red, the non-referencing group green already.
  `letRecursions` reads `TastAccessor.ELet`/`DLet` and flattens every binding to
  `name * Recursion`, so it cannot observe whether a binding is a `Let` or a `LetGroup`
  member. Step 4 reshapes it to `Let of name * Recursion | Group of members * components`
  and rewords the non-referencing case to "one lexical group with two singleton components,
  both `NonRecursive`".
- Diagnostics: `RecursionGroupTests`, new file. The splittable group warns and names the
  liftable member; a `let rec` with no self-reference warns; a genuine cycle and a
  self-referencing singleton are silent. Two red, two green.
- `GeneralisationTests`: `let rec f x = x and g () = (f 1, f "a")` clean (red); the genuine
  cycle still rejected (green).
- `FunctionEmissionTests` (JS) already pins the local group under Node with a `ptest`. The
  matching CLR `ptest` is `ClosureTests`'s last entry; unpinned it fails at emission with
  "no binding for variable BoundVarId 2" out of `EmitConstruct.buildHeapClosure`.

Dropped: "a `LetGroup` never has fewer than two members". Under D3 a `LetGroup` is a lexical
`and` group, so a singleton `let rec` is a `Let` and the invariant has no subject. The
construction invariant that remains, every member index in exactly one component, lands with
the node in step 4.

### 2. The SCC utility — DONE

`Scc.fs` sits after `Fifo.fs`, ahead of the pool walk, `Passes/Unification` and `Regions`, and
both backends inherit it.

- `Digraph.OfSuccessors(nodeCount, successors)` is the only constructor: it copies, sorts and
  deduplicates each node's successors, and rejects a negative `nodeCount` or an out-of-range
  successor. Step 3 builds its binding reference graph through it. A CSR-in constructor was
  built and then dropped — no client holds flat arrays, and it carried the whole `xadj`
  validation surface. Ascending, distinct successors make `Digraph.HasSelfEdge` a binary
  search.
- `Scc.compute : Digraph -> SccPartition`, with `Components: EqArray<SccComponent>` in reverse
  topological order and `ComponentIndex` per node, which orders the components it indexes.
  `SccPartition.ComponentOf node` takes both hops, so no consumer spells the composition and
  indexing `Components` by a node cannot be written. `SccComponent` is
  `Cycle of members: EqArray<int> | Acyclic of node: int`, so a component states whether its
  members are recursive and `Scc.isRecursive` is a match. The brainstorm's per-node self-edge
  array was dropped: a self-edge only ever decides a singleton, and a multi-member component is
  recursive regardless.
- Iterative Tarjan with an explicit frame stack, per the brainstorm's §2 pseudocode, over
  `Successors` spans with a per-frame cursor relative to the node's row.
- `SccTests`: differential against a recursive Kosaraju over 1000 random graphs, asserting
  identical partitions, reverse topological order, self-edges and `isRecursive`, plus a
  200000-node chain that pins the explicit stack.

### 3. Components in inference — DONE

`Passes/Unification/RecursionComponents.fs` sits between `Passes/NameResolution.fs` and
`InferGeneralize.fs`.

- `partition` builds the reference graph by walking each binding's value with
  `CstWalk.iterExpr` and reading `ctx.Bindings.Binding` at `CstKeys.ofExpr`, plus the
  `ExprIdent` key on a multi-segment long ident's leading segment, which is where
  `NameResolutionScope` binds a field-access chain's anchor. Every member takes a graph node,
  and every bound-variable key `NameResolutionScope.bindingsOfPat` yields for its pattern maps
  to it, so the sites the graph reads are exactly the sites NameResolution wrote. Every
  expression stamp `NameResolutionScope.visit` writes lands on one of those two keys, so the
  graph over-approximates nothing and misses nothing. Siblings are in scope of each other's
  values under `rec` only, so a group without `rec` takes the edgeless graph and no walk.
- `inferBindingGroup` takes the group's `rec` token and calls the extracted `inferComponent`
  once per `SccComponent`, in the partition's order: enter level, pre-allocate and bar the
  component's bound variables (again through `bindingsOfPat`), infer them, exit, settle
  traits, generalise. A member of a later component keeps its forward scheme while an earlier
  one types, which is sound because a component references earlier components only.
- The `SccPartition` is recorded at `ctx.Bindings.RecursionComponents`, keyed by the group's
  first pattern key, for every group including the singletons Elaborate will emit as `Let`.
  The `Cycle`/`Acyclic` classification survives to Elaborate, which maps each component
  through `.Members` when it builds the `LetGroup` node per D3.
- F# allows only simple variable patterns under `let rec` (FS0873) and rejects a parenthesised
  head outright (FS3521). Neither is diagnosed here yet; a destructuring or `as` pattern under
  `rec` types as its bound variables allow.
- `report` emits `V260` per D6, positioned on the `rec` token: `Kind.OverstatedRecursion` of
  `RecursionOverstatement.SplittableGroup` (component names in declaration order) or
  `RedundantRec` (the lone member).

`GeneralisationTests`'s two group cases and `RecursionGroupTests`'s two warning cases are
promoted. `GeneralisationTests`'s "id used inside pair" case was renamed and relaxed to assert
no ERRORS: its program is not mutually recursive, so the group now earns a `V260` warning and
`id` generalises before `pair` types. Its `'b -> 'b * 'b` assertion is unchanged.

The `src/Vesper.*` ports emit no `V260` today (measured over Core, Option, List, Comparison
and Printf), so D6's open question about suppressing the code for them has no subject yet.

### 4. `LetGroup` on the tree — DONE

- `TExprG.LetGroup of members * components * body * ty * tok` and
  `TDeclG.LetGroup of members * components`. A member is `TLetMemberG = { Pattern; Value; Ty;
  Tok }`: `Ty` is the binding's declared type, which `TDeclG.Let` carries and the signature
  projection and conformance check read per member. `components` is the `SccPartition`
  inference recorded, carried as is: `SccPartition.Create` is the checked constructor the
  codec reads through, and `SccPartition.Retain` renumbers after members are dropped and
  rejects dropping a `Cycle` member. `Let` is unchanged.
- `ElaborateExpr.translateRecGroup` translates a lexical `let rec … and …` group of two or
  more bindings through a per-binding function, drops the members it elides, and yields
  `RecGroup.Empty | Single | Group of members * components` with the recorded components
  restricted to the survivors. `translateLet` and `translateModuleLetGroup` build the `Let`
  or `LetGroup` from that. A format-literal alias member is the elided case. A non-`rec`
  `and` group stays nested `Let`s.
- The typar freeze needs one env per member: two members of one component share roots at
  different method-typar indices, so a union env is ambiguous. `DeclEnv = One | PerMember`
  replaces the flat env on the elaborated pair, and `freezeTypars` applies member `i`'s env to
  member `i`. The specialization-table freeze still takes the union, as it did before.
- `TastPools.poolLetGroup` walks one component at a time with that component's frames open.
  Every member's frame is open over every sibling value in the component, so a `Var` of a
  sibling marks the sibling's frame `Referenced` and each named member's `Recursion` is read
  off its own frame; a destructuring member is `Recursive` exactly when its component is a
  `Cycle`. The pooled payload is `ExprPayload.LetGroup` / `DeclPayload.LetGroup` of
  `LetGroupShape`, member `i`'s pattern at pat child `i` and value at expr child `i`, the
  body last. `FrozenCodec` is at format 10.
- `TastLower.lower` splits a module-level `LetGroup` into one `Let` decl per member, in source
  order, each keeping its `Recursion`, so both backends see module groups exactly as before
  step 4. `src/Vesper.Printf/structural-printer.*.fs` has module groups and stays green. An
  expression-position `LetGroup` reaches both emitters and fails there until steps 5 and 6.
- `inline` on a group member reports FS1114, as `dotnet fsi` does, because a member is never a
  splice template. `UnificationRecursionComponents.report` owns it beside `V260`. The JS
  specialization-table test that pinned a cycle verdict for a mutually recursive inline pair
  is gone; `RecursionGroupTests` pins the error. A lone `let rec inline` keeps its
  table-bounded expansion.
- `letRecursions` in `RecursionClassificationTests` returns `Let of name * Recursion | Group
  of members * components`; the four step-1 tests are promoted and a fifth pins a member
  referencing a sibling in an earlier component as `NonRecursive`.
- Every `let rec` group, singletons included, goes through `translateRecGroup`, since
  inference records a partition for every group. `RecGroup.Single` carries the source binding
  so a lone `let rec inline` keeps its `inline` flag. The non-`rec` path in `translateLet`
  and `translateModuleElem` nests one `Let` per binding, `isRec = false`.
- Decl consumers read a decl's bindings through `TastWalk.declBindings`, its module-init
  expressions through `TastWalk.declValues`, and rebuild every expression root through
  `TastWalk.mapDeclExprs`. Pool consumers read every module binding through
  `TastAccessor.rootBindings`, which presents a `Let` root as a `LetMemberView` whose `Tok`
  is the pattern's anchor.

### 4a. `TDeclG.Let` carries a `TLetMemberG` — DONE

- `TDeclG.Let of binding: TLetMemberG * isInline: bool * isRec: bool` and `TExprG.Let of
  binding: TLetMemberG * body * isRec * ty`, the expression node's `tok` dropped in favour
  of `binding.Tok`. `TLetMemberG` now documents one `let` binding, standalone or a group
  member.
- `TLetMemberG` carries `Pattern`, `Value` and `Tok`; its `Ty` is a computed member over
  the pattern's type, as is `LetMemberView.Ty`. `LetMemberShape` and `DeclPayload.Let`
  store no type: every producer wrote the pattern's type, so the stored column was a
  second derivation that `TastWalk.mapExpr` and `RefCellPromotion` had to keep in step.
- `Tok` is where the binding SITS rather than always the pattern's first token:
  `Inline.betaReduce` sites a minted binding at the call site while its pattern keeps the
  template's parameter token, and that anchor is preserved. `DeclPayload.Let` stores `Tok`
  as `LetMemberShape` does, so a decl member's anchor round-trips through the pool
  (`FormatVersion` 11).
- `TastWalk.declBindings` returns `TLetMemberG list`. `PlatformTypes.walkDecl`,
  `ResolvedTypes.walkDecl` and `ResolvedTypes.declSite` each collapse to one arm over it;
  `Regions.run` projects the pattern/value pair `withBindingGroup` takes.
- `DeclLetView` and `LetView` carry a `LetMemberView` field beside their flags, so
  `rootBindings` projects `l.Binding` and `TastAccessor.mintLetDecl` takes a
  `LetMemberView` plus the two flags.
- `TastWalk.(|InlineTemplateDecl|_|)` is a `let inline` decl bound to a simple name, as its
  bound variable and pattern; `Elaborate`, `InlineExpansion` and `Freeze.isInlineVocabulary`
  match it in place of a `Let`-then-`NamedSimple` nested match.
- `TastLower.lower` reads a top-level `Let`'s type off the member instead of its value.
- Match sites across `src` and `test` rewritten. Fantomas explodes a record pattern carrying
  a parenthesised sub-pattern onto its own lines; a test that needs both the value and the
  type binds the member with `as m` and reads `m.Ty`.

### 4b. `exprPayload` stops inventing a `Recursion` — DONE

`TastPoolShapes.exprPayload` is total over `TExprG` and returns `ExprPayload`, so at `Let`,
`App` and each `LetGroup` member it writes a `Recursion` / `AppKind` it has no evidence for:
`NonRecursive` / `Call`. `poolExprIn` then overwrites them, for `Let` and `LetGroup` by never
calling `exprPayload` at all (`TastPools.fs:126-131`) and for `App` by the tail-self-call arm
above the fall-through (`TastPools.fs:138-140`). Three costs are already visible: the warning
is written twice, on `exprPayload` and again on `ExprPayload` (`TastPoolNodes.fs:220`);
`TastPoolsTests.withoutRecFacts` is a runtime normaliser that erases the three fields so the
oracle can be compared; and `poolLetGroup` re-derives `Ty` and `sink.Anchor m.Tok` per member
(`TastPools.fs:238-245`) because what `exprPayload` computed went down the dead path.

- After 4a a member's only scalar is its `Tok`, so `LetMemberScalars` has one field and was
  not introduced: `LetMemberShape` stays `{ Tok; Recursion }` and the codec, `TastAccessor`,
  `TastUnpool` and `FormatVersion` are untouched.
- `exprPayload` returns `PayloadOfNode<'tok, 'id> = Complete of ExprPayload | Application |
  Binding of binding * body * isRec | BindingGroup of members * components * body`, so a
  placeholder verdict is a type error rather than a documented default. The classified cases
  carry the node parts the walk reads, so `poolExprIn` is one match over `PayloadOfNode` with
  no unreachable arm. `poolLetGroup` anchors each member's `Tok` itself, the single site.
- `withoutRecFacts` is deleted. `TestHelpers.expectPayloadOf` compares a pooled payload with
  the `PayloadOfNode` of an already-anchored tree: equal for `Complete`, and on `isRec` /
  member anchors / components for the classified shapes. `TastPoolsTests.checkExpr` and
  `TastPoolBuilderTests` use it.
- The three callers of `exprPayload` were swapped in one change rather than additively.

### 5. JS lowering

- Expression position: a `LetGroup` emits one block, `const m1 = …; const m2 = …; return body`,
  inside a single IIFE, members in source order. Arrows capture by reference, so declaration
  order within the block does not matter for functions. `components` is not read.
- Statement and module position: one `const` per member in source order.
- Promote the JS `ptest`.

### 6. CLR lowering

- `walkFreeRefs` scopes every `LetGroup` member name over every member value.
- `buildLet` for a `LetGroup`: allocate every member's closure and store its slot first, then
  back-patch each closure's captured sibling fields. A closure capturing only itself needs no
  patch. Reading `components` to patch only within a component is an optimisation, not a
  correctness requirement, and is deferred.
- Promote the CLR `ptest`.

### 7. Close out

- Re-check `emitFlatModuleFn`'s use of `Recursion` at module level.
- `Regions.letChainRegion` walks a maximal `Let`/`Use` chain as one binding group, and
  `Regions.run` walks every module decl as one, both introduced so a flattened `let rec …
  and …` could resolve its siblings. `LetGroup` now carries the group, so the only remaining
  effect of chain grouping is the `LetLevel` each region in the chain is minted at. Decide
  whether a per-`Let` level is the intended escape semantics; if so, collapse both to a plain
  `Let` arm plus `withBindingGroup` over `LetGroup`, pinned by a region-output test.
- `DeclEnv = One | PerMember` pairs a decl with envs by shape, and `freezeTypars` fails on a
  `LetGroup` paired with `One`. A DU co-locating each member with its env was evaluated and
  only relocates the check, since a `LetGroup` decl is still expressible beside a single env.
  The by-construction fix is for one component's members to share one typar numbering, so a
  group freezes over a single union env; that changes emitted method typar indices and is a
  separate decision.
- Update `brainstorm-tarjan-scc.md` to mark the closure client as served, point at `Scc.fs`,
  and add `type … and` group splitting as a further client over a type-reference graph.
- Delete this document.

## Out of scope

A saturated tail call to a *sibling* stays an ordinary call. `AppKind.TailSelfCall` records
self-calls only. A mutual tail loop needs a shared dispatch loop over the component's members,
which the SCC now identifies, but it is deferred until a Vesper library needs it.
