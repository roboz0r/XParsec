# Elaboration machine — one CST-driven state machine with resumable boundaries

**Status (2026-09-09).** DESIGN, not started. Records a decided direction and the invariants an
implementation must preserve. Open points are marked ⟨OPEN⟩. Semantic assumptions the user vets
are marked **[ASSUMPTION]**. This is a decision record; the implementation plan that lands it is
a separate `*-plan.md`.

Related: [`engine-rewrite-plan.md`](engine-rewrite-plan.md) Phase B (constraint solver with a
suspension worklist) is the solver half of the same design. The `Merkle.Dag` prototype
("Prototype" below) is the process-level cache this design's boundaries and fingerprints feed. [`architecture.md`](architecture.md)
keeps its governing principle; one sentence in it is superseded below.

## Summary

The four CST-driven passes — `NameResolution`, `Unification`, `Validation`, `Elaborate` — become
one defunctionalized, iterative machine driven by an explicit work stack over the CST, carrying a
persistent environment in each frame. The machine emits `TastFileG<SemType>` fragments as frames
complete. `Freeze` and the TAST-to-TAST passes stay as they are. The `.fsi` path is the same
machine with headers only: it is built first, because it has no bodies, no metavars and no
post-passes, so every boundary in it is resumable and the frame partition, environment and
boundary type land there before the `.fs` machine adds bodies and suspension.

Because F# scoping is top-down at the granularity of the recursive group, the machine's state at
a top-level declaration boundary is a snapshot. A boundary with no live metavar is resumable
after an edit, and the same condition makes the prefix freezable. Invalidation across files is
keyed on the published surface fingerprint, not on source text. A finer, replay-based
invalidation is recorded as a later refinement.

## Motivation

Three walks over the same CST exist today, each a recursive descent threaded by a function-typed
back-edge or a `let rec`:

- `UnificationInferDispatch.Infer` (`Passes/Unification/InferDispatch.fs:13`) is the back-edge
  into `UnificationInfer.infer` (`Passes/Unification/Infer.fs:166`), passed as a parameter to
  about thirty `infer*` helpers, each calling it in non-tail position.
- `ElaborateExpr.translateExpr` (`ElaborateExpr.fs:79`).
- `NameResolutionScope` walks with `Scope = Map<string, ScopeBinding>`
  (`Passes/NameResolution/Scope.fs:22`).

The split is where the re-derivation defects live. `NameResolution` resolves `x.M` with no type
for `x`, and `Unification` corrects it through the `PendingDotAccess` family. Overloads, SRTP and
dot-access are deferred families because resolution and inference were separated. In one machine,
`x.M` is a frame that suspends until the type of `x` grounds, which deletes the
guess-then-correct channel.

The environment is mutable state on `PassContextResolution` (`PassContext.fs:242-270`:
`OpenScope`, `Env`, `EnclosingContainer`, `Scopes`, `TyparScope`, `BindingTyparSeed`,
`EnclosingTypars`), restored by `IDisposable` handles from `PassContext.PushTyparScope`
(`PassContext.fs:510`). That discipline is correct only while the walk is strictly LIFO on the
host stack. A suspended frame that resumes later must carry its environment as a value.

A fifty-arm `infer` has the wide-frame shape that overflowed the parser on deep pipelines
(`ms-fsharp-progress.md`, "AST walker stack overflow"). Iteration removes the host stack from the
depth budget. This is a secondary benefit; the primary reason is suspension.

## The machine

### Work stack

The machine holds a stack of frames and a persistent environment. It peeks the next CST element,
enters the state path for that element, and pushes a completion frame that receives the results
of the sub-walks. A popped completion frame produces a `TastFileG<SemType>` fragment, or a
partial result folded into the frame above it.

Sketch, for a module holding a `let`:

1. Peek `module M`. Process the name and `=`. Push a module completion frame holding the
   environment after `M` opens and an empty declaration list.
2. Peek `let f x = body`. Process the header: name, parameters, annotations. Mint metavars. Push a
   let completion frame. Enter the body path.
3. The body path walks expressions, pushing an expression frame per sub-expression. A frame that
   needs a type not yet ground (member access, overload, SRTP) suspends: it is parked on the
   metavar it waits on, and resumes when that metavar is linked.
4. Pop the let completion: generalize, run the checks whose inputs are final, elaborate the
   `TDecl` fragment, extend the environment, fold the fragment into the module frame.
5. Pop the module completion: the module's `TDecl` list in declaration order.

Frames are immutable values. A completion frame accumulates into an immutable list, never a
`ResizeArray`. The snapshot section depends on this.

### Frame types

The frame DU is the defunctionalized form of the current `infer*` helper closures and their
continuations. Each `infer*` helper that today calls `infer` on a child and then continues
becomes a frame case holding what the continuation needs. Expression, pattern, type and
declaration frames are distinct DUs sharing the stack through one wrapper case each.

### Group frames

Top-down holds at the granularity of the recursive group, not the declaration. Three group kinds
need a two-stage frame, headers first and bodies after:

- `let rec … and …`. All headers are bound to metavars before any body is entered. Generalization
  is at group completion, not per binding.
- `type … and …`. Registration of every type in the group precedes any body.
- Class members. **[ASSUMPTION, probed]** F# allows `member this.A() = this.B() + 1` with `B`
  declared below `A` in the same type (probe below). A type's member set is one recursive
  group: all member signatures enter the environment before any member body is walked.

A group frame carries a state, `Headers` then `Bodies`, and holds per-member completion inside it.
This is a frame ordering, not a pass boundary.

A class is a three-stage frame, because its `let` bindings and its members follow different
rules. **[ASSUMPTION, probed]** Class `let` bindings are sequential: a forward reference to a
later `let` is FS0039. A class `let` may call a member declared below it (`let b = a + this.M()`
type-checks; the recursive access is a runtime `FailInit`, not a compile error). Members see every
class `let`. So the stages are:

1. Register every member signature.
2. Walk the `let` and `do` bindings in order, each extending the environment for the next.
3. Walk member bodies as one mutual group.

The `as this` self-identifier is in scope during stage 2. Its use before construction completes
is a runtime check in F#, so the machine reports it as a warning at most, never an error.

### Suspension

A frame that cannot proceed without a ground type is parked on the metavar's `TyVarId`. When
`union` or `link` grounds that root, the parked frames are re-queued. This is the wake-up
mechanism `engine-rewrite-plan.md` Phase B specifies for constraints, applied to elaboration
frames as well.

**Divergence from F#.** **[ASSUMPTION, probed]** F# resolves a dot-lookup eagerly at the program
point: `let y = x.Length` followed by `(x : string)` in the same binding is FS0072. Vesper
suspends the lookup to group completion and accepts that program. The repo rule prefers correct
semantics over parity, and suspension is strictly more accepting with the same answer wherever F#
also accepts. `PendingDotAccess` already behaves this way today.

**Quiescence.** Two frames parked on independent metavars that never ground, as in
`let g x y = x.Compute(y.Data)`, are not a cycle. They are frames whose information never arrives.
At group completion, when the work stack is empty:

1. Drain the wake-up queue.
2. Apply defaults to the group's metavars, as `InferGeneralize.applyDefaults`
   (`Passes/Unification/InferGeneralize.fs:222`) does today. Defaulting may ground a root and wake
   parked frames.
3. Drain again.
4. Every frame still parked is reported at its own site as the FS0072 family. The frame carries
   the site and the metavar it waited on, so the diagnostic names both.

**Determinism.** Wake order is source order. The queue is a `Fifo` keyed by site
(`Fifo.fs`), so two frames woken by one `union` resume in the order they were parked, which is
the order F# would have reached them. Overload resolution reads only ground operands
(read-only filter, `unification-store-redesign-plan.md`), so the answer does not depend on which
frame resumed first.

### Where each check lands

`Validation` today runs after `Unification` over final types. In the machine each check moves to
the frame where its inputs are final:

- Exhaustiveness, value restriction, assignment to immutable, `[<CallAtMostOnce>]`: group
  completion.
- Leaked metavar (`ResolvedTypes.fs`), platform representability (`PlatformTypes.fs`), dynamic
  escape (`DynamicEscape.fs`): file end, as today.

A check that lands in an earlier frame than its inputs allow produces a false diagnostic on
partially inferred code. Each check gets a test that would fail at the earlier point.

## What stays

- **The type-domain cut.** **[ASSUMPTION, probed]** A later declaration pins an earlier
  module-level binding's type: `let r = ref []` becomes `int list ref` through a use several
  declarations down, and `let mutable n = null` becomes `string`. A module-level fragment can
  hold live metavars when its frame pops, so fragments are `TastFileG<SemType>` and `Freeze.fs`
  stays the single `SemType → FrozenType` cut at file end.

  The probe, accepted by `dotnet fsi` and printing `FSharpList`1[System.Int32] 3`:

  ```fsharp
  module M =
      let r = ref []
      let mutable n = null
      type C() =
          member this.A() = this.B() + 1
          member this.B() = 2
      let use1 () = r.Value <- [1]
      let use2 () = n <- "s"
  printfn "%A %A" (M.r.Value.GetType()) (M.C().A())
  ```
- **TAST-to-TAST passes.** `Regions`, `RefCellPromotion`, `ResolvedTypes`, `PlatformTypes`,
  `DynamicEscape`, `Freeze` run over the elaborated result and are unaffected by the driver shape.
- **Conformance.** `ConformanceSurface` and `ConformanceBodies` compare two published surfaces
  and read neither walk. The `.fsi` walk itself does not stay: `SignatureResolution.run` is
  the same three-sweep shape as `NameResolution.run` over the same `walkTree`, with the same
  nominal-name pre-scan, containment sweep and rec-scope placement, and it pushes a strict
  typar scope through the same `PushTyparScope`. It becomes the headers-only instance of the
  machine and still runs first, publishing the surface the `.fs` machine reads.
- **Side tables keyed by `NodeKey`.** The fragment carries the answer for codegen. The tests
  assert through the tables via `Pipeline.analyseSemWithContextFor` (`Pipeline.fs:55`), and
  tooling queries need them. The machine keeps writing them.
- **The confined-mutation principle.** The union-find arena (`TypeStore.fs:125`) and the side
  tables stay mutable and confined to one `PassContext`.

## Superseded

`architecture.md:33`: "This is why we don't reach for persistent maps or path-copying: nothing
outside the pipeline can observe the mutation." The reasoning is about *sharing* and still governs
the arena and side tables. The environment is *captured* by frames, which is a different
requirement, and it is persistent. The sentence is to be narrowed to the arena and side tables
when the machine lands.

## Resumable boundaries

### The condition

At a top-level declaration boundary, after the group frame has generalized, every metavar minted
since the previous boundary is either quantified into a scheme or linked to a ground type, except
value-restriction deferrals and `null` literal vars settled after the walk
(`PassContext.RegisterNullLiteral`, `PassContext.fs:875`). A boundary is **resumable** exactly
when the set of unbound, non-generalized roots minted before it is empty. `TypeStore` holds every
metavar, so the condition is a scan of roots minted since the previous boundary.

The fact goes in the type: a boundary is `Resumable of Snapshot` or `Pinned of TyVarId list`.
The file-end leaked-metavar guard in `ResolvedTypes.fs` reports the same fact for the final
boundary as a diagnostic.

### Resumable ⇔ freezable prefix

A resumable boundary guarantees every `SemType` minted before it is final. `Freeze`, `Regions`
and `RefCellPromotion` are per-declaration and can run over the prefix at that boundary. The
snapshot therefore holds frozen output for the prefix plus the machine state for what is still
open. Incremental resumption and incremental emission are one mechanism.

### Snapshot contents

Everything the machine reads is either immutable or in the snapshot:

- The persistent environment. O(1).
- The work stack. At a boundary it holds the enclosing module and namespace completion frames
  with their declarations so far. Frames are immutable values, so this is O(1).
- The arena count. With no live metavar before the boundary, nothing after it can union into an
  earlier cell. Path compression on a resolved earlier cell rewrites a parent to the same root,
  which is idempotent. Resume is a reset of `nextId` (`TypeStore.fs:126`) to the boundary's
  count. The semi-persistent union-find in `Vesper.UnionFind` is not required for this.
- Side tables and diagnostics. One table fragment per top-level declaration, indexed by the
  declaration slot of the key (next section). Resume to boundary N discards the fragments after
  N. An edit inside declaration N rewrites fragment N alone. The diagnostics list
  (`PassContext.fs:410`) is segmented the same way. A single dictionary truncated by an offset
  watermark was rejected: it is a full scan or a parallel rollback log.
- External inputs. The provider and the `.fsi` surface are immutable. The `LexedFile` changes on
  edit, but prefix tokens are content-identical, so a stale reference in a captured CST node
  resolves the same names. Incremental parsing is not needed.

### Declaration-relative keys

An edit to declaration N shifts every file offset after it. With today's `NodeKey`, offset plus
kind (`NodeKey.fs:125`), the suffix's side tables and frozen fragments, which carry binding-site
keys through `BoundVarKey` (`TastExpr.fs:224`), are stale even when N's surface is unchanged.

**Decision:** the key becomes declaration slot plus declaration-local offset plus kind. A key in
`let bar` is bit-identical after an edit inside `let foo` above it, so the suffix is reused with
no relocation pass. This also segments the side tables for free. It lands as an additive swap
behind the `NodeKey` alias, per the repo's central-alias rule. Cross-file reuse does not depend
on it, because offsets are per file.

**Bit budget.** The current layout uses bits 0–31 for offset with bit 31 as the counter flag,
bits 32–47 for kind, and bit 63 for synthetic, leaving fifteen free bits. Fifteen bits is not
enough: every keystroke inside a declaration mints a new hash and therefore a new slot, so 32k
slots is a few hours of editing. The slot space is sized for days of continuous editing, on the
order of millions.

The revised layout:

| bits | field | width | note |
|---|---|---|---|
| 0–23 | local offset, or counter | 24 | 16 MB per declaration; the counter is per declaration |
| 24 | counter flag | 1 | as today's bit 31 |
| 25–36 | kind | 12 | 4096 kinds; the highest `NodeKind` today is 1009 |
| 37–62 | slot | 26 | 67M slots |
| 63 | synthetic | 1 | as today |

Trimming kind from sixteen to twelve bits is what pays for the slot: with kind left at sixteen,
the slot would be 22 bits, 4M, which is still adequate but leaves no margin. A single
declaration exceeding 16 MB of source is not a supported input. The counter-minted key space
becomes per declaration, since a key is already scoped by its slot, so 16M synthetic keys per
declaration replaces 2G per file.

**The slot is an interned content hash, not an ordinal.** An ordinal shifts when a declaration
is inserted above, which is exactly the edit that should leave the suffix intact. A declaration's
identity is the hash of its full source text, disambiguated by occurrence index for byte-identical
repeats in one file. The slot is a per-session interning of that hash: the same hash maps to the
same slot for the life of the session, a new declaration allocates a new slot, and every existing
slot is untouched. Slots are never reused. The intern table's entries for hashes no longer held
by any memo entry can be dropped with the memo eviction the prototype lacks; the slot numbers
they held stay retired.

Hashing the whole declaration rather than its header is deliberate. An inner edit changes the hash
and therefore every key inside that declaration, which costs nothing because the declaration is
re-run anyway, and it removes the need to define what a "header" is per declaration kind. A rename
is a delete plus an insert. The hash and the slot are both in-session values; nothing serialized
carries either (see "The merkle view" for what persists).

Declaration-relative offsets are also what makes a per-declaration CST slice content-addressable.
A whole-file CST is perturbed by any edit, so a content hash over it never cuts off. A slice whose
offsets are relative to its own start hashes identically wherever the declaration sits in the
file.

### The merkle view

A declaration's check is one rule in a content-addressed DAG, in the shape of the `Merkle.Dag`
prototype ("Prototype" below). The rule's inputs are two hashed values:

- `decl`: the declaration's content hash above.
- `env`: the environment the declaration is checked in, which is the merge of the surface
  deltas published by everything before it, in order.

The rule body runs the elaboration machine over that one declaration as a plain function called
once, which is the prototype's own rule for pure work inside a rule body. It yields the fragment,
the surface delta, the side-table fragment and the diagnostics, with diagnostics as a value so
they are cached rather than thrown. The surface delta is a **firewall** node: its hash is the
content hash of the fingerprint above, so an unchanged surface stops propagation. Everything
else is **derived**: hashed from the op and the input hashes, never from the payload.

**Two levels, one shape.** Declarations within a file and files within an assembly are both
ordered prefix stacks with shadowing, so both use the prototype's segment-tree rule: the
environment at position N is the merge of the O(log N) canonical ranges covering `[0, N)`, each
range merging two halves, each leaf one surface delta reached through its firewall. The
prototype measured the linear chain `env(N) = H(env(N-1), surface(N-1))` as both deeper and
slower, so that form is not used even though the trampolined engine now tolerates the depth.

**Pinned runs coalesce.** A `Pinned` boundary means a later declaration can still write an
earlier one's metavar, so the earlier declaration's result is not a function of its own inputs.
The run from a pinned declaration to the next resumable boundary is checked as one node whose
`decl` is the hash of the run's declarations together. That is the resumable condition stated
as node granularity: a file with no pinned boundary has one node per declaration, and a file
whose every boundary is pinned degrades to one node for the file.

**What each cutoff is in this shape:**

- An inner edit to a non-inline declaration with an unchanged surface changes `decl` for that
  node alone. Its firewall hash is unchanged, so every range above it and every later `env` is
  a validated hit.
- An edit to an earlier file changes only the ranges that include its surface leaf, and only if
  that firewall hash changed.
- A cross-declaration reference in a fragment is a `(declHash, localOffset, kind)` key, so the
  frozen output is a DAG of content-addressed fragments.

**Hashes are in-session.** The prototype's op identity is a process-local token and its digests
are documented as never leaving the process, so `declHash`, the slot and every DAG hash live for
one session. A disk cache is a separate concern with a separate, versioned, explicitly specified
encoding over frozen fragments and published surfaces. It never stores a DAG hash or a slot.

**The `Fifo` and the fiber are different stacks.** The machine's work stack lives inside one rule
activation. The engine's fiber stack sequences rule activations. A suspended elaboration frame
is parked inside the activation and never crosses into the engine.

## Invalidation across files

### Surface fingerprint, not source text

File N+1 resolves file N through `FrozenSignature.toSurface` (`FrozenSignature.fs:33`), the
implicit signature, or through a `.fsi` filling the same `PublishedSurfaceBuilder`
(`PublishedSurface.fs`). The downstream input is that surface plus what is layered on it:

- The published surface: types, members in declaration order, values with schemes, union cases,
  record fields, modules, attributes (covering `AutoOpen` and `RequireQualifiedAccess`), extern
  forms, implicit opens.
- Inline bodies (`InlineBodies.FileInlineBodies`, `InlineBodies.fs:69`). An inner-expression edit
  to a `let inline` changes what is spliced into every caller even when its scheme is unchanged.
- Literal values and enum case values, which fold into later patterns and attributes.

The fingerprint is a content hash of that union, and it must be **free of positional identity**.
`FrozenCodec` output is not usable as the hashed form: it writes `Anchor`s and `TypeId`s
(`FrozenCodec.fs:148`, `:543`), which shift on any edit above them, so a hash over it changes on
every keystroke and the cutoff never fires. The hashed form is a projection carrying only
`SymbolKey`, `MemberKey`, `TypeKey` and `FrozenType` values, with collections in a total content
order rather than pool discovery order, and the accessibility threshold applied before hashing so
a private rename leaves it unchanged. Inline bodies enter it with anchors stripped; with
declaration-relative keys ("Declaration-relative keys") their bound-variable keys are already
stable under edits elsewhere in the file. The prototype's `frozen-symbols.md` (see "Prototype")
lists the gates: body invariance, trivia invariance, private invariance, positional independence
under a prepended declaration, and signature and `inline` sensitivity.

A comment edit, a doc-comment edit in the `.fsi`, or a body edit to a non-inline function whose
inferred scheme is unchanged leaves the fingerprint unchanged, so downstream files are not
rechecked.

### Two-step cutoff

Vesper infers most schemes, so an inner-expression edit can change a binding's surface. The
sequence is: re-run the edited declaration, project its surface fragment, compare to the previous
fragment, and invalidate downstream only on a difference. A snapshot at a boundary is valid while
the fingerprint of every surface it was checked against is unchanged.

### Visibility

`private` declarations are absent from the surface (`FrozenSignature.fs`, internal-or-better
cut), so adding or editing one does not change the fingerprint. A new public name changes the
fingerprint even when unreferenced downstream; the replay log below is the refinement that
narrows this.

## Deferred: replay-based per-file dependencies

Sequenced after the machine and whole-surface fingerprinting. Considered only if a large assembly
shows unreferenced surface additions causing rechecks that matter.

Each name-resolution query a file makes is recorded as a value: environment, name, arity, and for
member lookups the ground object type as `FrozenType`, mapped to its `ResolvedItem`. Misses are
recorded with `Unresolved` as the answer, because a new upstream declaration turning a miss into a
hit is the shadowing case. Invalidation replays the recorded set against the new surfaces; if every
query returns its recorded answer, the file's result is unchanged.

The set suffices, not the sequence. Later queries depend on earlier answers, but the dependency is
deterministic, so identical answers to every recorded query yield an identical derivation.
Validation runs in any order and stops at the first mismatch.

Two prerequisites are met by the machine itself: resolution is a pure function of the persistent
environment and the surfaces, so a query has a self-contained input to record; and the object
type in a member query is ground when answered, so it can be recorded as `FrozenType`. The
`Resolved` side table (`PassContext.fs`) is already the answer half of this log.

## Semantic assumptions to confirm

- **[ASSUMPTION]** A class's member set is one recursive group with forward references, so all
  member signatures precede all member bodies. Probed against `dotnet fsi` ("What stays").
- **[ASSUMPTION]** A later declaration in the same file can pin an earlier module-level binding's
  metavar (value restriction deferral, `null` literal). Probed against `dotnet fsi` ("What
  stays"). This is what keeps the type-domain cut at file end.
- **[ASSUMPTION]** Class `let` bindings are sequential and may call members declared below
  them; members form one mutual group over all class `let`s. Probed against `dotnet fsi`
  ("Group frames").
- **[ASSUMPTION]** F# reports FS0072 eagerly for a dot-lookup on a metavar; Vesper suspends to
  group completion instead and accepts more programs. Probed against `dotnet fsi`
  ("Suspension"). This is the one place the design chooses against parity.
- **[ASSUMPTION, probed]** A rec region is the outermost `rec` and every module nested in it.
  F# ignores an inner `rec` under an outer one with warning FS3199, and a non-rec nested
  module inherits the region. `WalkedIn.RecScopeOffset` today mints a fresh offset for the
  inner `rec`, which splits the region; the tier 1 plan fixes the walk. A rec region is one
  group frame with a `RecScopeKind` of `NamespaceRec` or `ModuleRec` carried as a fact, so a
  rule that distinguishes them reads the kind rather than the keyword. Tier 1 hoists
  identically under both. No boundary inside a rec region is resumable.
- **[ASSUMPTION]** `open` is declaration-level and constant inside any one expression
  (`PassContext.fs:243`), so the environment extends only at declaration boundaries and binding
  sites.
- **[ASSUMPTION]** Overload and SRTP dispatch stay read-only filters on ground operands
  (`unification-store-redesign-plan.md`, "no-speculative-unification"), so a suspended frame
  resumes exactly once and never backtracks.

## Decisions

- **One frame per continuation.** Each `infer*` helper continuation becomes its own frame case
  holding exactly what it reads on resume, such as `AwaitDotTarget of target * memberName * env`.
  A coarse per-node-kind frame with an inner state index was rejected: it carries optional fields
  for every intermediate result, so a parked frame retains more than it needs and the payload is
  not self-describing.
- **Per-declaration side-table fragments.** See "Snapshot contents".
- **Declaration-relative `NodeKey`, content-addressed.** The slot is an interned hash of the
  declaration's full text. See "Declaration-relative keys" and "The merkle view".
- **One firewall per declaration node.** The surface delta's content hash covers every symbol
  the node publishes, so a `type … and …` group or a class with many members is still one
  firewall. This keeps the firewall 1:1 with the execution rule. A finer, per-symbol firewall
  buys nothing while downstream nodes merge the whole upstream environment through the segment
  tree; it becomes useful only with the deferred replay log, and it is introduced then, backed
  by the telemetry the prototype recommends.
- **A node is a declaration-level element, and a module is a range.** The DAG node is the
  innermost element the machine completes at a boundary: a `let`, a `let rec` group, a type
  group, an `open`, or a coalesced pinned run. A `module M = …` with children is not a node; it
  is the segment-tree range over its children, and its completion frame is what the machine
  pops after the last child. Node granularity does not change with nesting depth.
- **Machine state is in-session only.** The frame DU changes with every inference rule and is
  never serialized. What persists across processes is the frozen prefix and the published surface
  under a separate versioned encoding, never a DAG hash or a slot. A cold start replays from the
  last frozen boundary, which is cheap because declaration frames are coarse.

## Probes

Run with `dotnet fsi --nologo`. Each backs one **[ASSUMPTION, probed]** above.

Class `let` sequencing and member visibility. Type-checks, then fails at runtime with
`FailInit` on the `this.M()` call:

```fsharp
type C() as this =
    let a = 1
    let b = a + this.M()
    member this.M() = 2
    member this.B = b
printfn "%d" (C().B)
```

Forward reference between class `let`s is FS0039 at `later`:

```fsharp
type D() =
    let a = later + 1
    let later = 2
    member _.A = a
```

Eager FS0072 at `x.Length`, despite the annotation two lines down:

```fsharp
let f x =
    let y = x.Length
    (x : string), y
```

## Prototype

`Merkle.Dag` at `D:\roboz0r\merkle-dag` is a greenfield, un-integrated prototype of the
process-level cache, not in this repository and not under version control. What it settles for
this design:

- **Hash width and function.** xxHash128, 128 bits, in-session only. Its argument: at 64 bits,
  ten million live nodes give a collision chance near 3e-6 per session, and the failure is a
  silently wrong compile. This resolves the hash-width question; there is no cross-process hash.
- **Derived versus firewall.** A rule's hash mode is explicit in its type. Early cutoff exists
  only where a firewall was placed, so the surface delta's firewall is an architectural
  obligation, not an emergent property.
- **Depth.** A trampolined interpreter with an explicit heap stack, so native stack depth is O(1)
  for any graph. The linear prefix chain was measured against the segment tree and lost.
- **Determinism checking.** A mode that re-runs every validated hit and reports value drift and
  unrecorded dependencies. The machine must pass it: every input it reads is a demanded node or
  the rule argument, never ambient state such as a module-level cache.
- **Several remembered traces per node**, so an edit and its undo are both hits.
- **Property tests** against an independent evaluator, checked against deliberate engine
  breakage.

What it lacks, which integration must supply: eviction (the memo table grows without bound, and
a retained CST per file times retained snapshots is gigabytes), scheduling and parallelism
(nothing says "the file on screen first"), cross-fiber cycle detection, a validate-without-compute
mode, and a benchmark harness. Its `frozen-symbols.md` predates `PublishedSurface`: it names
`FrozenSignature.toProvider`, which is now `PublishedSurface.toProvider`
(`PublishedSurface.fs:423`) over `FrozenSignature.toSurface`. Its requirements transfer to
`PublishedSurface` unchanged and are the gates cited under "Surface fingerprint". Its open
"per-declaration partitioning" item is what "Resumable boundaries" supplies.

## Open points

None. Every earlier open point is resolved under "Decisions".

## Revisit triggers

- Phase B of `engine-rewrite-plan.md` lands a constraint worklist before this machine exists.
  Then the machine's suspension reuses that worklist rather than introducing a second one.
- A benchmark shows recursive descent is not the bottleneck and suspension is not needed for
  correctness. Then the group-frame and boundary parts of this design still hold, but the
  expression-level defunctionalization is not worth its readability cost.
- A rec scope narrower than a file becomes common in Vesper sources. Then the coalesced node
  for that scope is the unit of reuse, and per-declaration reuse inside it is unavailable by
  design.

## Sequencing (not a plan)

Three tiers. Tier 1 has a plan: [`declaration-machine-plan.md`](declaration-machine-plan.md).

1. **Tier 1.** Environment as a value replacing the mutable `PassContextResolution` fields and
   `PushTyparScope`; a frame partition shared by both drivers, with a group frame per rec
   region; the `.fsi` driver first, headers-only, every boundary resumable by assertion; then
   the `.fs` driver with bodies, expression walks still recursive inside a frame; the boundary
   type as a gate. Frozen-output-identical throughout.
2. **Tier 2.** Expression-level frames with suspension, replacing the `PendingDotAccess` and
   related deferred families. One plan with `engine-rewrite-plan.md` Phase B, benchmark-gated.
3. **Tier 3.** Declaration-relative `NodeKey`, read together with `codegen-by-key-plan.md`;
   boundary snapshots within a session; surface fingerprint and cutoff across files; the
   replay log. Waits on the prototype gaining eviction and scheduling.
