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

### 2. The SCC utility

`Scc.fs` early in the `SemanticAnalysis` compile order, ahead of `Passes/Unification`, so
`Regions` and the pool walk can also reach it, and both backends inherit it.

- Input: node count and CSR adjacency (`xadj: int[]`, `adj: int[]`).
- Output: `Components: EqArray<EqArray<int>>` in reverse topological order, and per node its
  component index and whether it has a self-edge. Expose `isRecursive: int -> bool` as the
  brainstorm specifies.
- Iterative Tarjan with an explicit frame stack, per the brainstorm's §2 pseudocode.
- Differential test against a small recursive reference implementation over random graphs,
  asserting identical partitions and a valid reverse-topological order.

### 3. Components in inference

In `inferBindingGroup`:

- Build the reference graph from the bindings' RHS identifiers through NameResolution's
  `ResolvedBinding.BindingSite`, keeping edges whose target is a sibling pattern key.
- Run `Scc`. For each component in order, run the existing body: enter level, pre-allocate the
  component's TyVars, bar polymorphic recursion within the component only, infer, exit,
  settle traits, generalise.
- Record the components per group in `ctx.Bindings`, keyed by the group's first pattern key,
  as `EqArray<EqArray<int>>` over source-order member indices, in reverse topological order.
  Self-edges are not recorded here; the pool walk classifies them per member (D4a).

- Emit the D6 warning from the recorded components: more than one component, or a single
  component without a self-edge under `rec`.

This turns the `GeneralisationTests` case green on its own.

### 4. `LetGroup` on the tree

- Add `TExprG.LetGroup of members * components * body` and
  `TDeclG.LetGroup of members * components` per D3. The `Let` path is unchanged.
- `translateLet` and `translateModuleLet` emit one `LetGroup` per lexical `and` group, members
  in source order, with the recorded components attached. A member folded out as a
  format-literal alias is removed from `members` and its index from `components`, with the
  remaining indices renumbered; an emptied component is dropped. A group left with one member
  becomes a `Let`.
- `letRecursions` in `TastPoolsTests` returns `Let of name * Recursion | Group of members *
  components`; the four step-1 group tests assert the `Group` shape.
- Thread the node through `FrozenCodec`, `TastUnpool`, `TastAccessor`, `TastWalk`,
  `RefCellPromotion`, `InlineExpansion`, `TastPoolShapes` and the CLR's `LetBoundLambda`.
- `TastPools`: `LetGroup` walks one component at a time, opening that component's member
  frames before walking any of its member values. A member of a multi-member component is
  `Recursive` unless its own frame records a saturated tail self-call. A singleton component
  classifies exactly as a `Let` does today. Opening every member's frame at once is wrong: a
  member referencing a sibling in another component would classify `Recursive`.

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
- Update `brainstorm-tarjan-scc.md` to mark the closure client as served, point at `Scc.fs`,
  and add `type … and` group splitting as a further client over a type-reference graph.
- Delete this document.

## Out of scope

A saturated tail call to a *sibling* stays an ordinary call. `AppKind.TailSelfCall` records
self-calls only. A mutual tail loop needs a shared dispatch loop over the component's members,
which the SCC now identifies, but it is deferred until a Vesper library needs it.
