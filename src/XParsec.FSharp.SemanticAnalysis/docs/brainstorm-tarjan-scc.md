# SCC utility spec — iterative Tarjan + canonical numbering

**Status:** §2 core and §3 output contract are implemented in
[`Scc.fs`](../Scc.fs) — `Digraph.OfSuccessors`, `Scc.compute`, `SccPartition`,
`SccComponent` — and serve the recursive-group client. §4 canonical numbering
remains a spec, gated behind the structural-hash row.

Two details settled differently from the spec below. `Digraph.OfSuccessors` is
the only constructor: it takes a successor list per node and builds the CSR
itself, sorted and deduplicated, so no client holds flat `xadj`/`adj` arrays.
`SccComponent` is `Cycle of Block<int> | Acyclic of int`, so a component states
its own recursion and §3's per-node self-edge array is unnecessary.

## Clients (the algorithm is dual-/triple-useful)

SCC detection was first identified as a potential optimisation for **region
analysis** ([`Passes/Regions.fs`](../Passes/Regions.fs), whose solver runs a
flat fixpoint loop today) and **closure optimisation**
(mutually-recursive binding groups, [function-representation-plan](function-representation-plan.md)),
and *then again* for **structural type hashing** in the TS-consumer provider
([codegen-js-symbol-provider-plan](codegen-js-symbol-provider-plan.md)). It is
worth building once as a shared, reusable utility rather than three times:

| Client | Graph | What it needs from SCC | Status |
|---|---|---|---|
| **Closure / recursive-group analysis** | binding reference graph (`let rec … and …`) | minimal recursive groups = the SCCs; **reverse-topo order** (free from Tarjan) gives a valid emission/initialisation order. Singletons without a self-edge are non-recursive — emit directly. | **Served.** `Passes/Unification/RecursionComponents.fs` builds the graph and generalises one component at a time; the partition rides the TAST on `LetGroup` and the `V260` warning reports a splittable group. |
| **`type … and` group splitting** | type reference graph within one syntactic `type … and …` group | the same partition over type declarations: a group whose members do not reference each other is several independent types, and the `V260` treatment extends to it. | Open. |
| **Region analysis** | escape/lifetime dependency graph | SCCs collapse mutually-dependent lifetimes into one region; the condensation DAG (SCCs as super-nodes) is then a clean partial order to solve over. | Open; `Regions.solve` still runs a flat fixpoint. |
| **Structural type hashing** | structural type-reference graph (anonymous TS object types) | SCC-isolation of cyclic types, **then canonical numbering within each cyclic SCC** (the hard extra half — see §4). | Open. |

Only the structural-hash client needs canonical numbering; it stays a separate,
client-specific pass over the shared core.

## 1. Design principles

- **Observational purity.** Mutable flat arrays + a work stack *inside*; an
  immutable function signature outside. The idiom the whole codebase uses for hot
  passes.
- **Dense `int` node ids.** The utility operates on nodes `0 … n-1`. Each client
  interns its own nodes (bindings, lifetime vars, type shapes) into dense ids and
  maps results back. This keeps the core allocation-free and cache-friendly and
  decouples it from any client's node type.
- **CSR adjacency, not `int list array`.** `xadj : int[]` (length `n+1`) +
  `adj : int[]` (length `E`); node `u`'s out-edges are `adj.[xadj.[u] ..
  xadj.[u+1]-1]`. Contiguous, one allocation, no pointer-chasing. (The old sketch
  claimed "contiguous memory" while using cons lists for *both* adjacency and the
  stack — it had neither.)

## 2. Iterative, not recursive — the load-bearing fix

`let rec dfs` overflows the call stack on deep graphs (adversarial or merely
large `.d.ts` — `@types/node`, DOM). This repo has shipped recursion-driven stack
overflows before ([[feedback_errortype_spike_failed]]); a graph utility that can
see attacker-shaped input **must** use an explicit work stack.

Frame = `(node u, edgeCursor)` where `edgeCursor` is the current index into
`adj`. Two parallel `ResizeArray<int>` (or a struct stack) hold the frames. The
subtlety of iterative Tarjan is replaying the "after the child returns, update the
parent's lowlink" step:

```
for s in 0 .. n-1:
  if index.[s] <> -1: continue
  push frame (s, xadj.[s]); visit s   // visit = assign index/lowlink/timer, push to sccStack, onStack:=true

  while frames not empty:
    let u = topNode; let mutable e = topCursor
    let mutable descended = false
    while e < xadj.[u+1] && not descended:
      let v = adj.[e]
      if index.[v] = -1:
        topCursor <- e + 1            // resume here after the child completes
        push frame (v, xadj.[v]); visit v
        descended <- true             // process child next iteration
      else:
        if onStack.[v]: lowlink.[u] <- min lowlink.[u] index.[v]
        e <- e + 1
    if not descended:
      topCursor <- e
      // u is finished
      if lowlink.[u] = index.[u]: popComponent u   // pop sccStack down to u → one SCC
      pop frame
      if frames not empty:                          // propagate to parent
        let p = topNode
        lowlink.[p] <- min lowlink.[p] lowlink.[u]
```

Invariant to test: run the iterative version against a straightforward recursive
reference on a few thousand random graphs (varying density, cycle structure) and
assert identical component partitions. Iterative Tarjan is easy to get subtly
wrong; pin it with a differential test.

## 3. Output contract

- `comp : int[]` — `comp.[u]` = component id of node `u`.
- `components : int[][]` (or a CSR-style flat `int[]` + offsets) — members per
  component, **emitted in reverse topological order** (Tarjan's natural order:
  if component A has an edge to component B, B is emitted before A). Document this
  guarantee — the closure/region clients depend on it for ordering and should not
  re-sort.
- A node is **non-trivially recursive** iff its component has >1 member *or* it
  has a self-edge. Expose a cheap `isRecursive : int -> bool` so the closure
  client can take the direct-emit fast path for the common singleton case.

## 4. Canonical numbering (structural-hash client only — the genuinely hard half)

Tarjan *isolates* a cyclic SCC; it does **not** give a labelling invariant to
traversal order. To content-hash a cyclic structural type so two isomorphic
shapes hash equal regardless of how they were discovered, each node in a cyclic
SCC needs a **canonical number** independent of input order. This is graph
canonization, and it is the part the original brainstorm omitted entirely.

Pragmatic, sufficient-in-practice approach (type graphs are tiny and almost
always asymmetric):

1. **DAG fast path.** Hash acyclic types by ordinary structural recursion on a
   topological order — **no Tarjan, no canonization**. The overwhelming majority
   of types are DAGs; only self-/mutually-recursive interfaces form non-trivial
   SCCs. Run the SCC pass only to *find* those; pay canonization only inside them.
2. **Colour refinement (1-WL) within an SCC.** Seed each node's colour from its
   *local* shape modulo references (record → sorted field-name list; union →
   member arity; primitive → name). Iterate: `colour' = hash(colour, multiset of
   neighbours' colours)` to a fixpoint (≤ |SCC| rounds). If the final colours are
   all distinct, sort by colour → canonical order. This resolves the vast
   majority of real cyclic types.
3. **Tie-break for genuine symmetry.** When refinement leaves nodes with equal
   colours (a real automorphism, e.g. perfectly symmetric `A{b:B}` / `B{a:A}`),
   pick a canonical root deterministically (lowest stable colour, ties by interned
   id) and number by a canonical DFS taking edges in colour-then-id order. Full
   canonization is GI-hard in general, but bounded SCC size makes this a
   non-issue; the tie-break only has to be *deterministic*, and for true
   automorphisms any consistent choice yields the same hash.
4. **Hash** over `(canonical-number, local-shape, edges-as-canonical-numbers)` of
   each SCC node. Keep the declared/pretty name in a side table for diagnostics —
   the hash is the identity key, not human-readable (cf. don't key on opaque `%A`).

## 5. Performance notes

- Time `O(V + E)` for SCC; colour refinement is `O(|SCC|·|edges-in-SCC|·rounds)`
  but only over cyclic components, which are small and rare.
- Zero per-node heap allocation on the SCC core (flat arrays + `ResizeArray`
  stacks); the only allocations are the result arrays.
- The `int`-id interning is the client's cost; for the type-hash client it is
  also where the DAG-vs-cyclic split is cheapest to detect.

## 6. Scope

**Built:** the §2 iterative SCC core + §3 output contract, in `Scc.fs`, pinned by
`SccTests`'s differential against a recursive Kosaraju over 1000 random graphs
plus a 200000-node chain over the explicit stack.

**Build with the structural-hash row:** §4 canonical numbering, layered on the
core — gated behind the ts-consumer provider's structural-hash work, benchmarked
against real `.d.ts` graphs.
