# Partial application — a `Fun` value struct over a statically-emitted spec

**Status: design locked, not built — next epic.** The one unlanded piece of the printf
design (for the landed architecture see [printf-architecture](printf-architecture.md)).
This plan scopes zero-alloc partial application and is deleted once it lands.

Decisions locked:

- Flat-arity cap `K = 4`.
- `n > K` degrades **FSharp.Core-style — one flat-`K` head + a curried tail** (option A),
  NOT a greedy flat→flat chain. Rationale: the tail is existing curried closure codegen (no
  flat→flat residual), and A→B is a non-breaking internal change until we target binary
  compatibility of emitted assemblies — far past compiler v1. The greedy flat chain (option
  B) stays documented under Landing sequence as the future promotion.
- Within-chunk partial application is deferred (Phase B).

## Current behaviour

`let p = printfn "%d"` already *works* — but via the FSharp.Core cold path, not the
Vesper handler. The happy-path gate (`Passes/Unification/InferApp.fs`,
`tryInferPrintfApp`) accepts only fully-applied literals
(`args.Length = specs.Length + 1`); an under-applied call is left unmarked, so
`FreezeExpr.fs` keeps the `App printfn` intact and it lowers to
`Microsoft.FSharp.Core.PrintfModule`. Correct output, but it allocates the
`PrintfFormat` object + closures the rest of `Vesper.Printf` is built to avoid.

## Target design

`let p = printfn "%d"` becomes a value struct `S : Fun<int, unit>`:

- **Stateless ⇒ `default(S)`, zero heap allocation.** The format is a compile-time
  constant, **baked into the statically-emitted `Invoke` body** — exactly as the happy path
  bakes it into the call site (see §The spec-runner). So `S` carries no per-instance state,
  and `constrained.callvirt Fun::Invoke` devirtualises (rides the `Fun` work directly — see
  [function-representation-plan](function-representation-plan.md)).
- **No `PrintfFormat` on this path.** For a literal format there is no `PrintfFormat` value
  and no runtime spec — same as the happy path. `PrintfFormat` survives *only* on the cold
  path (non-literal / format-as-value), where the spec can't be baked in and is demoted to a
  static-field representation driven by the runtime spec-runner.
- **Multi-hole: one flat-`K` head + a curried tail (option A).** `printfn "%d %s"` fully
  unapplied is a *flat* value struct `Fun<int, string, unit>` (the flat arity-2 interface,
  overloading curried `Fun<_,_>` by generic arity; stateless). Matching FSharp.Core's
  `OptimizedClosures` (never nests flat closures — see the prior-art note): take **one** flat
  chunk of arity `min(K, n)`, and the codomain beyond it is the **ordinary curried `Fun`2`
  chain**, not another flat chunk:
    - `n ≤ 4` → one flat `Fun`*(n+1)*, `Invoke(h1..hn) : unit` (one dispatch, zero-alloc).
    - `n = 6` → `Fun<h1,h2,h3,h4, (h5 -> h6 -> unit)>` — a flat-4 whose codomain is the
      curried arrow `h5 -> h6 -> unit` (i.e. `Fun<h5, Fun<h6, unit>>`). Full application is
      **one** flat `Invoke(h1..h4)` returning a residual capturing `h1..h4` (the remaining
      segments are baked into the residual's statically-emitted `Invoke` body — there is no
      runtime spec to capture), then two ordinary curried applies. No flat→flat nesting.

  Library-wise `K = 4` is three flat interfaces: `Fun`3`/`Fun`4`/`Fun`5` (flat arity
  2/3/4); arity-1 is the existing curried `Fun`2`. The `n > K` residual is a plain curried
  closure — **existing closure codegen**, no `Curried`/`Flattened` adapters, no flat→flat
  value-struct. Because `> K` holes is rare for printf, the curried-tail alloc there is
  acceptable (FSharp.Core pays the same on its `Adapt` slow path).

- **Within-chunk partial application is Phase B (deferred).** Supplying FEWER than the flat
  head's arity (`printfn "%d %s" x`, a flat-2 with one arg) needs the `Curried`*k* residual
  over the head's remaining args, reusing the `Curried`/flat-`Fun` adapter machinery in
  `Vesper.Core/core-types`. So for `n ≥ 2` holes, only the fully-unapplied struct and the
  fully-saturated call land here; a proper subset (the actual "curry one arg") stays on the
  FSharp.Core path until Phase B — an immediate, natural follow-up once this lands.
  The unapplied and `n ≤ K` saturated forms stay stateless value structs where they don't
  escape; only the `n > K` tail residual captures.

- **Escape caveat.** Zero-alloc only while `p` flows into `Fun`-bounded generic positions or
  is invoked directly; an interface-typed `Fun<int, unit>` slot boxes it — it inherits
  whatever the `Fun` escape analysis decides. Note there is **no** non-`inline` single-use-let
  copy-propagation in the front end (`Passes/InlineExpansion.fs` beta-reduces only lambdas
  bound to `inline` parameters), so `let p = printfn "%d" in p 3` does **not** fuse to the
  happy-path `printfn "%d" 3` — the gate lowers the under-applied `printfn "%d"` to
  `default(S)` and `p 3` invokes it directly. Non-escaping, so still zero-alloc, but via the
  value-struct path, not by inlining to the happy path.

## Where it plugs in

- **The gate.** `tryInferPrintfApp` gains a *lowerable partial application* case: a
  literal format, under-applied, with lowerable placeholders, diverted to the
  value-struct lowering instead of falling through to FSharp.Core. **Scope: `printf`
  only for now** — same `idx = 0` restriction as the happy path.
  *Arity comes from a per-hole arg-type seam — do NOT assume holes = args.* The partial case
  peels its flat/curried arity from the saturated function type the happy path already builds
  (`PrintfSpec.appliedTypeOf`, via the per-hole `argType : FormatType -> SemType`). Today
  every lowerable hole yields exactly one arg type, so arity = hole count and the happy-path
  relation `args.Length = specs.Length + 1` is the saturation test — but the peel must key on
  the *summed per-hole arg count*, not the hole count. Dynamic width/precision (`%*d`,
  `%*.*f`) is a planned feature, out of scope this sprint (the lexer currently rejects `*` —
  see [printf-architecture](printf-architecture.md)); landing it turns `argType` into a
  hole → arg-types mapping (`%*d` = width + value, a length-2 hole), and an arity peel that
  routes through that seam absorbs it without a structural change. The rest of the family
  (`fprintf`/`sprintf`/`eprintf`, and the `idx ≠ 0` writer/builder sinks) stays on the
  FSharp.Core path; review the whole family for partial application once `printf` lands.
- **Call-side application (option A).** Saturated application of an `n > K` partial
  (`p a b c d e f`) lowers to **one** flat `Invoke(a,b,c,d)` on the head, then ordinary
  curried applies for the tail (`… e f`) — exactly FSharp.Core's `invokeFast5 … a6` shape.
  The head flat invoke generalizes the current arity-2 invoke to arity `≤ K`; the tail is the
  existing curried-apply path. No `⌈n/K⌉` flat-chunk loop, no new flat→flat residual.
- **The spec-runner.** For a literal format the spec is a compile-time constant, so the
  `Invoke` body is **statically unrolled through the same `EmitFormat.fs` lowering as the
  happy path** — one `AppendLiteral`/`AppendFormatted` per segment, no runtime walk. The
  `printfn` trailing `"\n"` and the `Flush`-vs-`ToStringAndClear` sink choice therefore fall
  out of the generated body for free, carrying the same byte-for-byte parity guarantee. What
  partial-app shares with the cold path is the **handler** (`formatter.fs`), *not* a runtime
  interpreter: the runtime spec-runner that walks a parsed spec is the **cold-path** engine
  (format-as-value / non-literal), which this epic also builds since it drives the same
  handler — but the partial-app `Invoke` does not go through it.

## Dependencies

- The `Fun` value-struct representation + escape analysis — what these structs ride
  on. `Vesper.Core/core-types` already ships the flat `Fun<'A,'B,'C>` interface (CLR
  `Fun`3`, overloading curried `Fun`2` by generic arity) and the `Curried`/`Flattened`
  adapters this plan builds on; the gating prerequisites are the flat `Fun`4`/`Fun`5` arities
  (flat arity 3/4, the `K = 4` cap) and the arity-`≤ K` generalization of the closure codegen
  (the `Emit.Closure` `Param2 voption` → bounded param-list refactor and an arity-`≤ K`
  `Invoke` signature/interface-spec family). Under option A the codomain beyond the flat head
  is the ordinary curried `Fun`2` chain, so there is no flat→flat residual to build. That
  substrate is general function-representation work, not printf-specific, and lands FIRST; the
  printf gate then produces those structs. See
  [function-representation-plan](function-representation-plan.md) for the landed closure
  representation; the code (`core-types.fsi`, `prim-types-min.fsi`) is the source of truth.
- The parsed-spec representation already exists: `PrintfSpec.fs` produces the
  typed-hole sequence that `EmitFormat.fs` unrolls.

## Landing sequence

The substrate (steps 1–3) is general `Fun` function-representation work and lands before
any printf change; the printf gate (step 4) then merely produces these structs. Option A
removes the flat→flat residual, so there is no unboxing spike on the critical path — the
`n > K` tail is the existing curried closure/apply codegen.

1. **Semantic side** — generalize the four `targs.Length = 2/3` `Fun`-arity arms
   (`Subsume.fs`, `Engine.fs`) and `funSlotArityOf` to peel ONE flat head of arity
   `min(K, spine)`, leaving the remaining arrows as the ordinary curried codomain (no
   recursion into further flat chunks); `FunVerdict.Arity` already carries the head arity.
   Now trivial post arity-keying.
2. **Library** — declare flat `Fun`3`/`Fun`4`/`Fun`5` (flat arity 2/3/4) in
   `prim-types-min.fsi/.fs`.
3. **Codegen** — the `Emit.Closure` `Param2 voption` → bounded param-list refactor (≤ `K`
   flat params); the arity-`≤ K` `Invoke` signature + interface-spec family (`Fun`(k+1)`);
   the closure peel that takes up to `K` binders into the flat head and leaves the rest a
   curried closure; the head flat `Invoke` returns that curried residual. No `⌈n/K⌉`
   flat-chunk call loop — the tail is the existing curried-apply path.
4. **printf gate** — `tryInferPrintfApp` gains the under-applied lowerable case, diverting
   to the flat-head value-struct lowering; `Invoke` bodies statically unrolled through
   `EmitFormat.fs`.

Phase B (within-chunk partial application via `Curried`*k*) and option B (greedy flat→flat
chain) are both out of this landing. Phase B is the immediate follow-up.

**Prior art — FSharp.Core `OptimizedClosures` is the direct model for option A**
(the local port, `src/XParsec.FSharp.Lib/Clr/prim-types.fs:3777–3936`). Flat
`FSharpFunc<T1..Tn,U>` for **n = 2..5** (cap 5), each *inheriting* the curried form
(`override Invoke(t) = fun u -> f.Invoke(t,u)`); `Adapt` (`:3784–3846`) = type-test-or-wrap
(≈ our `Curried`/`Flattened`, Phase B); `invokeFast{2..5}` (`:3872–3890`) = a
widest-flat-then-degrade `:?` cascade at the saturated call site. Crucially, **FSharp.Core
never nests flat closures** — `invokeFast` uses at most ONE flat `Invoke` and curries the
rest (a 6-arg call is `invokeFast5 … a6`), which is exactly option A. Reuse the *design*
(small cap, one-flat-head + curried-tail, the `Adapt` adapter for Phase B) — but NOT the
mechanism: inheritance + heap classes + runtime type-tests are what the value-struct /
static-verdict / no-inheritance-bridge design rejects (Vesper picks the flat head statically
via the verdict, never by a runtime cast, and the head is a value struct not a heap class).

**Option B (future promotion, not v1) — greedy flat→flat chain** (`n = 6` as
`Fun<_,_,_,_,Fun<_,_,_>>`, flat-4∘flat-2), for better perf on rare wide saturated calls.
The chunk-boundary residual is a **heap reference, not a value struct** — deliberately: it
captures the whole head chunk (`K` args + the spec), so it is wide (passing a pointer beats
copying `K + 1` words), and it boxes anyway the moment it flows through a `Fun`-typed slot.
The one case a value copy would save an alloc — a non-escaping, monomorphic,
immediately-saturated residual — is exactly what option A's curried tail already covers. So
B's boundary residual is **ordinary heap-closure codegen** (capture head args, expose the
next flat `Invoke`), with no return-by-value unboxing problem. B is a **non-breaking internal
change** over A (same `Fun`-typed public surface up to arity `K`; only the `> K` codomain
shape changes) until emitted-assembly binary compatibility is a goal — far past compiler v1 —
so it is deliberately deferred.

## Acceptance

- `let p = printfn "%d" in p 3` prints `3` with **no heap allocation**. There is no
  non-`inline` single-use-let inlining (see the Escape caveat), so this does *not* fuse to
  the happy-path `printfn "%d" 3`; the gate lowers `printfn "%d"` to `default(S)` and `p 3`
  invokes `S.Invoke(3)` directly — zero-alloc because `default(S)` is stateless and `p` does
  not escape. Escaping into a `Fun<int, unit>` interface slot boxes it.
- **Motivating multi-hole scenario** (forces the flat-head machinery, not just the arity-1
  struct): given `let f () = printf "%d %s %b"`, `f ()` yields the fully-unapplied flat-3
  struct `Fun<int, string, bool, unit>`; `let g d s b = f () d s b` saturates it in **one**
  flat `Invoke(d, s, b)`. The within-chunk form `let h d s = f () d s` (2 of 3 args) is
  Phase B — it needs the `Curried`*k* residual and stays on the FSharp.Core path until then.
- Multi-hole lands flat (option A): `n ≤ 4` as a single flat `Fun<…>`, zero-alloc where it
  doesn't escape; `n > 4` as one flat-`K` head + a curried tail, so `printfn "%d %s %b %f %d %s"`
  fully applied runs as one flat-4 `Invoke(h1..h4)` (returning a curried residual capturing
  those args — the remaining segments are baked into its `Invoke`, not captured) then two
  ordinary curried applies — one flat dispatch + two curried, not six curried. For `n ≥ 2`
  holes only the fully-unapplied and fully-saturated forms are zero-alloc here; supplying a
  proper subset (within-chunk `Curried`*k*, and greedy flat→flat nesting, option B) is
  deferred and still routes to FSharp.Core.
- Byte-for-byte parity with FSharp.Core's output across the spec matrix — including the
  `printfn` trailing newline, which comes from sharing the `EmitFormat` lowering.
- The cold-path spec-runner and the partial-application `Invoke` share one **handler**
  (`formatter.fs`); the `Invoke` itself is statically unrolled, not run through the runtime
  spec-runner.
- **Scope: `printf` only.** The rest of the family is a follow-up review, not this
  landing.
