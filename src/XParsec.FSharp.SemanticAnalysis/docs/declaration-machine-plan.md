# Declaration machine — plan

Working document. Ephemeral: delete it when the work lands. The design it implements is
tier 1 of [`elaboration-machine-design.md`](elaboration-machine-design.md), which is durable
and wins where the two disagree.

Line numbers are deliberately absent. Constructs and file names only.

## Scope

**In:**

- The environment as a value. One immutable record replaces the mutable fields of
  `PassContextResolution`, `PassContext.PushTyparScope`, `PassContext.PushLocalOwner` and
  every hand-written save/restore of `BindingTyparSeed` and `EnclosingTypars`.
- A frame partition over the flattened module walk, shared by the `.fsi` and `.fs` drivers,
  with a group frame for every rec scope and the rec scope's kind carried as a fact.
- The `.fsi` driver first. `SignatureResolution.run` is the same three-sweep shape as
  `NameResolution.run` with no bodies, no metavars and no post-passes, so every frame is
  headers-only and every boundary is resumable. It is the machine with the fewest moving parts
  and the place the frame partition, `Env` and `Boundary` land green before the `.fs` driver
  adds bodies.
- One `.fs` declaration-level driver over the same partition, running NameResolution and
  Unification per frame in a single loop.
- The boundary type, `Resumable | Pinned`, computed after each frame, gating per-element
  Validation and Elaborate. That last step lands only if it is frozen-output-identical.

**Out, and where it goes:**

- Expression-level frames and suspension. Tier 2, planned together with
  `engine-rewrite-plan.md` Phase B.
- Declaration-relative `NodeKey`, snapshots, surface fingerprints, the DAG. Tier 3.
- `let rec … and …` and `type … and …` as two-stage frames of their own. Both are one walked
  element today, with headers-before-bodies implemented inside `registerGroup` and
  `prebindModuleFunctionSchemes`. Tier 1 keeps them as single-element frames; tier 2 splits
  them when expression frames exist to split into.
- Closing the guess-then-correct channel between NameResolution and Unification. Within one
  declaration NameResolution still walks the whole body before inference starts, so `x.M`
  is still resolved without a type for `x` and corrected through `PendingDotAccess`. That
  channel closes in tier 2. Tier 1 makes the two passes one driver so tier 2 has one place
  to close it.
- Conformance. `ConformanceSurface` and `ConformanceBodies` compare two published surfaces
  and read neither walk.

Every step is frozen-output-identical: the CLR and JS goldens, the conformance corpus and the
SemanticAnalysis suite are change detectors, not proofs. A step that changes an output is a
finding to report, not a regression to hide. Step 0 exists so that findings are located
before the steps that would trip over them.

## Preconditions

- No other change is open in `Passes/NameResolution*`, `Passes/Unification*`,
  `Passes/SignatureResolution*`, `Passes/Validation.fs`, `Elaborate*`, `Containment.fs`,
  `CstModuleTree.fs` or `PassContext.fs`. The walk-heavy plans
  (`semantic-analysis-followups-plan.md`, `semantic-analysis-tests-followups-plan.md`,
  `provider-one-round-trip-plan.md`, `nullability-analysis-plan.md`,
  `unchecked-defaultof-plan.md`, `printf-partial-app-plan.md`) either wait for step 2 or,
  where one is urgent, add its inference concern as a function taking `infer` as a
  parameter in the existing `UnificationInferDispatch.Infer` convention and nothing else.
- Codegen, JS, TS-provider and Vesper plans are unaffected and proceed in parallel.

## The shape being rearranged

All five CST passes iterate the same flattened walk, `CstModuleTree.walkTree` through
`walkImpl` or `walkSig`, and enter each element through `PassContext.EnterElement`, which
sets `OpenScope`, `Env`, `Scopes` and `EnclosingContainer`. What differs is how many times
each pass sweeps the list.

| pass | before the walk | sweeps over the walked list | after the walk |
|---|---|---|---|
| `SignatureResolution.run` | none | three: nominal type names for the module suffix; every containment; register type groups, vals, abbreviation and open targets | publish `ctx.Types.Modules`; `Attributes.run` |
| `NameResolution.run` | `registerLocalModules` over the unflattened tree | six: nominal type names for the module suffix; every containment; register type groups, abbreviation and open targets, classify term annotations; class bodies; nominal bodies; module values | none |
| `Unification.run` | none | three: resolve interface impls; prebind module-function schemes under a rec scope; fill members and walk bodies in declaration order | `resolveListLiterals`, `applyDefaultsTo` format holes, `resolveNullLiterals`, `validateCustomEqCompImpls`, `checkDuplicateMembers` |
| `Attributes.openChecks` | | | between Unification and Validation: needs every attribute class's member types settled |
| `Validation.run` | none | one, through `walkImplWith` with a per-scope hook | `checkUnresolvedDotAccesses`, `checkValueRestriction`, `checkRecOpenPlacement` |
| `Elaborate.run` | none | one, `translateModuleElem` | `InlineExpansion.run`, `freezeTypars`, `localOwners`, `localSchemes` |
| `Attributes.run` | | | seals `AttributePositions` and enforces usage |
| `Regions.run` | | | over the elaborated TAST; unaffected |

Every multi-sweep exists for one of two reasons. Either a later sweep reads what an earlier
one registered, gated by `VisibleFrom` so that a later element is invisible anyway, or the
sweep implements rec-scope hoisting, keyed on `WalkedIn.RecScopeOffset`. The first kind
collapses to per-frame order with identical results. The second kind becomes a group frame
over the rec scope's elements. Step 0 is where that claim is tested before it is relied on.

The nominal-type-name pre-scan in `SignatureResolution.run` and `NameResolution.walkElems`
looks like a third kind and is not. It exists because `Containment.EnterContainment` writes
a module's `CompiledName` into `LocalContainer.Facts` on first entry, and the `…Module`
suffix depends on whether a `type Foo` appears anywhere in the file. Source never writes the
suffix, and `SymbolKeyOps.moduleKeyOf` keys the module by its source name, so resolution
never reads it. The only readers are `FrozenSignature` and `FrozenCodecTypes` at publish
time. Step 2 moves the suffix decision to file end, where the final `NominalTypeNames` set
is available, and deletes the pre-scan in both passes.

The mutable environment has four kinds of writer:

- `Containment.EnterElement` and `EnterContainment`, setting the four scope fields.
- `PassContext.PushTyparScope`, at nine sites, installing a `Dictionary<string, ScopedTypar>`
  that four sites then mutate in place: `UnificationInfer.inferBinding` copying the enclosing
  scope and `EnclosingTypars` into a fresh one, `InferForwardSchemes` and
  `UnificationTranslate` introducing an implicitly declared `'a` on first mention. One of the
  nine push sites is `SignatureResolutionContext.underTypars`, on the `.fsi` path.
- Hand-written save/restore of `BindingTyparSeed` and `EnclosingTypars` in
  `Unification.fillTypeMembers` and `UnificationClassCtors`, plus one mid-binding clear of
  `BindingTyparSeed` in `inferBinding` after the signature is translated, so a nested `let`
  mints fresh typars rather than reusing the member's prototypes.
- `NameResolution` setting and clearing `PendingBindings` around a `let` group's RHS.

`PushLocalOwner` and `CurrentLocalOwner` are the same LIFO pattern for a different fact and
move with them. `CurrentLevel` with `enterLevel` / `exitLevel` is inference-engine state, not
environment, and stays where it is.

## Rec scopes

F# semantics, probed with `dotnet fsi`:

- A `rec` on a module nested inside a rec scope is ignored with warning FS3199. One rec
  region spans the outermost `rec` and every module nested in it, rec-marked or not.
- A non-rec module nested in a rec scope inherits the region: a forward reference from the
  enclosing scope into the nested module's later declaration resolves.
- A `module rec` nested in a non-rec module is its own region. The enclosing module's earlier
  declarations are visible inside it by their short names; a forward reference from the
  enclosing module into it is FS0039.

`CstModuleTree.innerRecScope` mints a new offset for an inner `rec`, so today a rec-in-rec
region is two offsets and the elements of the outer region after the inner module lose
hoisting across it. That is a finding against the walk, fixed in step 2: an inner `rec` under
an inherited offset keeps the inherited offset and reports FS3199 at the inner keyword. With
that fix a rec region is exactly the maximal run of walked elements sharing an offset, and the
frame partition is flat.

A rec scope's kind is a fact the partition carries rather than a token a rule re-derives:

```fsharp
type RecScopeKind =
    | NamespaceRec
    | ModuleRec

type Frame<'Elem> =
    | Single of WalkedIn<SyntaxToken, 'Elem>
    | RecGroup of kind: RecScopeKind * offset: int * elems: WalkedIn<SyntaxToken, 'Elem> list
```

Tier 1 hoists identically under both kinds. The kind exists so that `checkRecOpenPlacement`,
the FS3199 site, and the tier 3 coalesced node read it instead of the keyword, and so that a
rule which one day distinguishes them has a case to match on. **[ASSUMPTION]** No tier 1 rule
distinguishes the two kinds; if step 0 finds one, it is recorded here and the group frame's
headers or bodies branch on the kind.

## Collections

The array-backed immutable collections, by what each guarantees about order and lookup. Each
name has exactly one type, and the type's doc comment states its row and column.

| | Sequence (values) | Set (unique keys) | Map (key → value) |
|---|---|---|---|
| **Positional** (arbitrary, duplicates) | `Block` | — | — |
| **Chronological** (insertion-ordered) | — | `Roster` | `Ledger` |
| **Canonical** (sorted, binary-searchable) | — | `Lexicon` | `Glossary` |

All four live in `Vesper.Block` beside `Block`, so the table has one home.

- `Block` exists.
- `Ledger` lands in step 1a as the type of `Env.TyparScope`, which is its first consumer.
  `EqDict` is a hash trie with no order and is what `Ledger` replaces: `Ledger` lands
  beside it behind the central alias, its nine readers swap, and `EqDict` is deleted in a
  separate change.
- `Roster` is `EqSet` renamed. `EqSet` already is an insertion-ordered, deduplicating,
  array-backed set with set-semantic equality. The rename is a separate change after 1a and
  is not on this plan's path.
- `Lexicon` and `Glossary` have no consumer in tier 1. They land with the tier 3 plan, which
  names the first ordered query: the total content order the surface fingerprint hashes in,
  and the segment-tree range lookup over declaration slots.

## Steps

Each step lands green on its own. Additive where a widely-used shape changes: the new shape
lands beside the old, readers swap, the old one is deleted before the step is called done.

### 0. Isolation tests

New cases in `FileOrderScopingTests.fs`, each a minimal program through
`Pipeline.analyseSemWithContextFor` asserting the exact diagnostic set. Each expectation is
first probed against `dotnet fsi` and the FS code quoted in the test name. A `.fsi` case goes
through `mkSignature` in `TestHelpers.fs` and asserts on the published surface and the
diagnostic set.

`.fs` cases:

- A class member body referencing a type declared below the class.
- A module function above a class calling a member of that class.
- A `:>` coercion to an interface implemented by a class declared below the site.
- A `let` annotated with a type declared below it.
- A `[<Literal>]` used in a pattern below its declaration, and one used above it.
- `let rec … and …` with a forward reference inside the group and one from outside it.
- `type … and …` with a forward reference inside the group.
- `module rec M = …` nested in a non-rec module, with a forward reference inside `M` and one
  from the enclosing module into `M`'s later declaration. The reference inside `M` to the
  enclosing module's earlier declaration is by short name; F# rejects the qualified form
  from inside the module.
- `module rec Outer` containing `module rec Inner`, with a reference from `Outer` above
  `Inner` to a declaration of `Outer` below `Inner`, and one into `Inner`'s later
  declaration. Expected: FS3199 at `Inner`'s `rec`, both references resolve.
- `module rec Outer` containing non-rec `module Inner`, the same two references. Expected:
  both resolve, no diagnostic.
- `namespace rec` with a module function above a class calling its member.
- A `module Foo` above a `type Foo` in the same container, asserting the `FooModule` suffix.
- An `open` of a module declared below it.
- An `open` of a module declared above it, whose contents include a type used below.

`.fsi` cases:

- A `val` annotated with a type declared below it.
- A `type … and …` group with a forward reference inside it.
- `module rec` in a signature with a `val` above a type referencing it.
- `module Foo` above `type Foo`, asserting the suffix on the published surface.

A case that is green today and goes red in a later step is a finding: the old sweep order
was letting a forward reference through, and the test is what pins the correct answer.

### 1. The environment record

`Env` is an immutable record, in `PassContext.fs` beside `PassContextResolution`, holding:
`OpenScope`, `ScopeEnv`, `EnclosingContainer`, `Scopes`, `TyparScope`, `TyparScopeStrict`,
`PendingBindings`, `LocalOwner`. `TyparScope` is `Ledger<string, ScopedTypar>`, so a
measure-kinded typar keeps its `ScopedTypar.Measure` slot and the scope carries its typars
in declaration order. `PassContextResolution.Env` is
today's name for the `ScopeResolution.ScopeEnv` value; it is renamed `ScopeEnv` inside the
record so `ctx.Env` is the record.

`BindingTyparSeed` and `EnclosingTypars` are not fields of `Env`. Both exist only because
`inferBinding` mints a fresh dictionary and re-seeds it: the enclosing scope is copied in
first, `EnclosingTypars` over it, the seed over that. With 1a building a binding's scope once
from `env.TyparScope`, the seed is a parameter of scope construction and `EnclosingTypars` is
`env.TyparScope` itself. The mid-binding clear in `inferBinding` disappears with the field:
a nested `let` builds its scope from the enclosing binding's `Env`, which carries no seed.

`PassContext` holds one `mutable Env` slot and exposes one seam, `Enter(env) : IDisposable`,
which installs `env` and restores the previous one on dispose. Every reader reads `ctx.Env.X`.
`EnterElement` computes an `Env` value and calls `Enter`. The old fields, `PushTyparScope`,
`PushLocalOwner` and the hand-written save/restore blocks are deleted.

Installation by mutation with LIFO restore is retained in this tier on purpose. What changes
is that the environment is a value: it can be captured, compared and snapshotted, and no
site mutates it after installation. Threading it as a parameter arrives with expression
frames in tier 2, where a frame carries it.

**1a. Typar scopes are fixed before they are entered.** The four in-place introductions are
the obstacle to an immutable scope. A binding's implicitly declared typars are collected up front by a CST
scan over exactly the type positions `UnificationTranslate` reaches under that binding's
scope: explicit `<'a, …>`, argument and return annotations, constraints, and every type
position in the body excluding nested bindings, which build their own scope. Body positions
include lambda argument annotations, `:?` and `:>` targets, explicit type applications such
as `typeof<'a>` and `f<'a>`, and typed expressions `(e : 'a)`. The scan mints one `TyVarId`
per distinct name and the scope is built once as a `Ledger`: enclosing scope, then seed,
then scanned names in source order, later entries shadowing. The explicit `<'b, 'a>` typars
are therefore the scope's own leading entries in declaration order, which is the order
Elaborate needs for a free function's declared typars. `inferBinding`'s separate `declared`
list, kept today because the scheme cannot recover that order, is deleted against the
`Ledger`.

**[ASSUMPTION, probed]** A `'a` first mentioned in a nested `let` is scoped to that nested
binding, and two sibling nested lets each mentioning `'a` are independent unless the
enclosing binding mentions `'a` itself. `inferBinding`'s copy of the enclosing scope already
matches this, and the scan's "excluding nested bindings" rule preserves it.

Land 1a in shadow mode first: the scan runs beside the old on-first-mention introduction and
asserts, at each `PushTyparScope` exit, that the pre-collected name set equals the set the
old code introduced. Any disagreement is a scan gap to fix before the old path is deleted.

Deletions this step: `PushTyparScope`, `PushLocalOwner`, the nine `PushTyparScope` call
sites including `SignatureResolutionContext.underTypars`, the four in-place `TyparScope`
writes, the save/restore pairs in `fillTypeMembers` and `UnificationClassCtors`, the
mid-binding seed clear in `inferBinding`, the `BindingTyparSeed` and `EnclosingTypars`
fields, and the remaining mutable fields of `PassContextResolution` except
`AttributeClasses`.

### 2. The frame partition and the drivers

**2a. Frames.** A new `Frames.fs` after `CstModuleTree.fs`, module `Frames`, holding
`RecScopeKind`, `Frame<'Elem>` and `partition : WalkedIn<_, 'Elem> list -> Frame<'Elem> list`.
`CstModuleTree.innerRecScope` keeps an inherited offset under an inner `rec` and reports
FS3199 at the inner keyword, so a rec region is one offset. The kind comes from whether the
region's keyword is a namespace group's or a module's. The rec-in-rec tests from step 0 go
green here.

**2b. The `.fsi` driver.** `SignatureResolution.run` iterates `partition` instead of its
three sweeps. `CompiledName` leaves `LocalContainer.Facts` and is computed where the surface
is published, from the final `NominalTypeNames` set and the module's attributes, so
`EnterContainment` reads nothing declared below the element; the pre-scan is deleted. Per
frame: headers are containment
registration and `registerSigGroup` / `registerValSig` / abbreviation and open targets for
each element of the frame, in order. There are no bodies. After each frame the boundary is
computed and asserted `Resumable`; a `Pinned` result on a signature is a bug, because a
signature mints no metavar outside its declared typars. This is where `Boundary` lands.

**2c. NameResolution per frame.** Collapse `NameResolution.walkElems` to pre-walk plus
headers-then-bodies per frame inside `NameResolution.run`, leaving the pass boundary alone.
Headers for a frame: containment registration, then type group registration, abbreviation and
open targets, term annotation classification, for each element. Bodies: class bodies, nominal
bodies, module values, for each element. A single-element frame is the same calls on one
element.

**2d. Unification per frame.** The same for Unification's three sweeps inside
`Unification.run`. Headers: `resolveInterfaceImplsForElem`, then `prebindModuleFunctionSchemes`
for a `RecGroup` frame only. Bodies: `fillClassMembers`, `fillNominalMembers`,
`walkModuleElem`, in declaration order.

**2e. The `.fs` driver.** A new `Passes/Declarations.fs` after `Elaborate.fs` in compile
order, module `Declarations`, with `run (ctx) (file) : unit`.
`Pipeline.analyseSemWithContextForCore` calls it in place of `NameResolution.run` and
`Unification.run`. The driver:

1. `registerLocalModules` over the unflattened tree, as today. The nominal-type-name
   pre-scan is gone with 2b; `noteNominalTypeNames` runs in headers and the suffix is decided
   where `FrozenSignature` publishes the module.
2. Build the walked list once and `partition` it. Unification today rebuilds the walk to
   recompute the same `OpenScope`s; that second walk is deleted.
3. For each frame, in file order:
   - **Headers.** NameResolution headers for every element of the frame, then Unification headers
     for every element.
   - **Bodies.** NameResolution bodies for every element of the frame, then Unification
     bodies for every element. Within a frame the two passes do not interleave per element:
     today every NameResolution body precedes every Unification body, and a group frame's
     later element may still have body-walk output the earlier element's inference reads.
     Interleaving per element inside a single-element frame is the same thing.
4. File end: Unification's five post-passes in today's order, then return.

Delete the two `run` walk bodies and the two private `walkElems`.

2c and 2d are where step 0's tests do their work. If a test goes red, the finding is
reported with the sweep that was letting the forward reference through, and the test's
expectation is what the driver preserves.

### 3. Boundary and per-element Validation and Elaborate

Conditional. This is the tier 3 entry point pulled forward only because it is cheap to try
once step 2 exists, and it stops at the first non-identical output.

- `Boundary = Resumable | Pinned of TyVarId list` in `PassContext.fs`, landed in 2b. After
  each frame's bodies, scan `TypeStore` roots minted since the previous boundary for one that
  is unbound and not generalized. `Pinned` names them.
- `Attributes.openChecks` needs every attribute class's member types settled. It runs once,
  before the first `Resumable` frame's Validation, and a frame whose elements declare an
  attribute class is `Pinned` until then. If that rule proves wrong the step stops here.
- For a `Resumable` frame, run Validation's per-element walk and `translateModuleElem` at
  once and accumulate the `(TDecl * DeclEnv)` list in file order. Validation's per-scope hook
  from `walkImplWith` fires at the frame that opens the scope. For a `Pinned` frame, defer
  both to file end, where the existing whole-file paths run over the deferred elements only.
- Validation's three post-checks, `Attributes.run`, `InlineExpansion.run` and `freezeTypars`
  stay at file end.

If the goldens are identical, the step lands and tier 3 starts from a driver that already
knows its boundaries. If any golden differs, the difference is recorded in the design doc
under "Resumable boundaries" as the case the condition misses, and this step is reverted
rather than patched. Likely candidates: a list literal left flexible until
`resolveListLiterals`, a `null` literal until `resolveNullLiterals`, and a format hole until
`applyDefaultsTo`. Each is a metavar the scan should see as unbound; if the scan sees it and
still differs, the elaboration of that element read a link that only the post-pass sets, and
that read is the finding.

## Gates

Every step, before it is called done:

```
./claude_tools.cmd -Action Build
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.SemanticAnalysis.Tests"
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Clr.Tests"
./claude_tools.cmd -Action Test -TestProject "XParsec.FSharp.Codegen.Js.Tests"
./claude_tools.cmd -Action Format
```

Plus the comment-hygiene pass over the diff and no `FOR-REVIEW` remaining at the commit.

## Findings expected

Listed so a red test is recognised rather than debugged from scratch.

- Rec-in-rec hoisting lost across the inner module. Step 0 pins it red; 2a fixes
  `innerRecScope` and adds FS3199.
- A forward reference that today resolves because a whole-file sweep registered it before
  any body was walked. Step 0 pins the F# answer; 2c or 2d surfaces the site.
- A typar name the pre-collection scan misses. The shadow assertion in 1a names it.
- A `Pinned` boundary on a signature. 2b's assertion names the element; the cause is a
  metavar minted outside a declared typar scope.
- A `Pinned` boundary the scan misses on an implementation. Step 3's golden diff names the
  element.

## Migration checklist before this doc is deleted

Each durable fact moves into code before the doc goes. Preference: a shape that cannot be
expressed wrongly, then a test that fails without it, then a sited comment.

- [ ] `Env` is immutable and `Enter` is its only installer. Shape.
- [ ] A typar scope is complete before it is entered. Shape: `Ledger`, no writer after
      construction, no seed or enclosing field to re-apply.
- [ ] A binding's declared typars are read from its scope in declaration order. Shape: no
      parallel `declared` list in `inferBinding`.
- [ ] The collection table lives in `Vesper.Block`'s doc, and each collection type's doc
      comment states its row and column.
- [ ] A rec region is one offset and an inner `rec` is FS3199. Test: the rec-in-rec cases in
      step 0.
- [ ] A rec scope's kind is a `RecScopeKind` on the frame. Shape.
- [ ] Rec-scope hoisting is a group frame, not a file sweep. Test: the nested `module rec`
      cases in step 0.
- [ ] Forward references outside a rec scope are FS0039. Test: step 0 cases.
- [ ] The `…Module` suffix is decided at publish, not at containment entry. Shape:
      `CompiledName` absent from `LocalContainer.Facts`. Test: the `module Foo` above
      `type Foo` cases in step 0.
- [ ] Every signature boundary is `Resumable`. Shape: the assertion in 2b.
- [ ] The boundary condition, if step 3 lands. Shape: `Boundary` DU; test: an all-resumable
      fixture and a pinned one asserting which path elaborated each element.
- [ ] The design doc's "Sequencing" entry for tier 1 is marked landed and points at both
      drivers.
