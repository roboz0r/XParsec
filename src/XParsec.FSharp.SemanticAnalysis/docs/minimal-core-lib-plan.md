# Minimal core lib plan — factoring "language fundamentals" out of the FSharp.Core port

## Why

`src/XParsec.FSharp.Lib` is a near-verbatim **signature port of all of
FSharp.Core** — ~57k lines across `Clr/`, `Common/`, `Threading/`,
`Reflection/`, `Printf/` (prim-types, list, array, seq, map, set, option,
result, string, printf, sformat, async, tasks, resumable, mailbox, reflect,
events, observable, BigInteger, units of measure). It was intended to feed a
manifest-driven `IExternalSymbolProvider` (see
[compiler-clr-project.md](../../XParsec.FSharp.Lib/compiler-clr-project.md)).

Two problems make that the wrong foundation:

1. **It's far more than the language needs.** The canonical sample and the
   language fundamentals touch a tiny fraction of that surface. Porting and
   re-syncing the whole of FSharp.Core is open-ended busywork that couples us to
   FSharp.Core's exact shape (SRTP encodings, inline-IL syntax, dense
   cross-file references) for symbols we will never resolve.
2. **Emitted programs still depend on Microsoft's `FSharp.Core.dll`.** The
   codegen references real FSharp.Core types (`ClrProvider.fs` reads identities
   off the loaded `FSharp.Core`) and `Codegen.materialiseApp` copies
   `FSharp.Core.dll` next to the output. The signature port never changed that —
   it's signatures only, no runtime.

The [fsi-target-brainstorm](fsi-target-brainstorm.md) already sketches the right
shape: a small **universal `Core.fsi` contract** plus a **per-target
`Core.[Target]` implementation**, with `type int = Platform.Int32` locked in.
This plan makes the *minimal* slice of that vision concrete and uses it to
**replace the FSharp.Core dependency in emitted programs** and **retire the
verbatim port as the front-end contract**.

## Three "core" dependencies — keep them distinct

It is easy to conflate three unrelated things called "core":

| # | Dependency | Today | This plan |
|---|---|---|---|
| 1 | **Host build dep** — the XParsec compiler is itself F#, so it builds against the real `FSharp.Core.dll`. | real FSharp.Core | **unchanged** — irrelevant to emitted programs. |
| 2 | **Emitted-program runtime dep** — what `dotnet <app>.dll` loads. | Microsoft `FSharp.Core.dll` (referenced + shipped) | **replace** with our minimal core DLL. |
| 3 | **Front-end symbol contract** — what the type-checker resolves names against. | `MockBuiltins` (real path: the FSharp.Core `.fsi` port) | **replace** with the minimal contract. |

The goal: **one minimal core is the single source of truth for (2) and (3)** —
what the type-checker sees is exactly what the runtime provides, because it's
the same library described two ways (a signature contract + a runtime impl).

## The surface the language actually needs

### Tier 0 — referenced by codegen *today* (slices 1–5 break without it)

Every FSharp.Core entity the CLR backend currently mints a reference to
(`src/XParsec.FSharp.Codegen.Clr/ClrProvider.fs`):

| Current reference | Used for | Minimal-core entity |
|---|---|---|
| `Microsoft.FSharp.Core.Unit` | `unit` values / signatures | **`unit = System.ValueTuple`** — BCL zero-arity value tuple; no Vesper type to ship (D7) |
| `Microsoft.FSharp.Core.FSharpFunc\`2` (`Invoke`, protected `.ctor`) | function values, closure base (slices 2, 5) | `Vesper.Fun<'A,'B>` — one `Invoke`; closures *implement* it; no `FSharpFunc` (see D3) |
| `Microsoft.FSharp.Collections.FSharpList\`1` (`Cons`, `get_Empty`) | list construction (slice 4) | `Vesper.Collections.List<'T>` — mirrors `FSharpList` (`Cons`/`Empty`); keeps the `ClrProvider` list special-case (D8) |
| `Microsoft.FSharp.Collections.ListModule` (`Fold`) | `List.fold` (slice 5) | `List` module fns |
| `Microsoft.FSharp.Core.PrintfFormat\`4` (`.ctor(string)`) | `printfn` format object (slice 1) | `PrintfFormat<_,_,_,_>` (+ 5-typar) |
| `Microsoft.FSharp.Core.PrintfModule` (`PrintFormatLine`) | `printfn` engine (slice 1) | printf engine + entry point |
| `op_Addition` / `op_Subtraction` / `op_Multiply` | arithmetic | **no runtime type** — CIL intrinsics; contract carries SRTP `val inline (+)` for the type-checker only |
| `System.Object`, `System.IO.TextWriter` | base type, `%A`/console plumbing | **stay BCL** — not core lib |

Plus the primitive **type aliases** the contract must declare so the
type-checker resolves them: `int = System.Int32`, `float = System.Double`,
`bool`, `char`, `string`, `byte`, `int64`, `obj`, and
**`unit = System.ValueTuple`** (D7). (`int = Platform.Int32` is already locked in
per the brainstorm.) These map straight to BCL/CLI metadata in
`ClrProvider.encodeType`; only `unit` previously needed a nominal FSharp.Core
type, and `ValueTuple` removes even that.

### Tier 1 — fundamentals, near-term (not in the canonical sample)

`FSharpOption<'T>`, `FSharpResult<_,_>`; the rest of the `List` module
(`map`/`filter`/`iter`/`length`/`rev`/`append`/…); array type + `Array` core;
`string` module basics. **Structural equality / hashing** (`=`, `<>`, `hash`) for
user types stays in `Vesper.Core`; **ordering** (`< > <= >=`, `compare`, `min`,
`max`) splits into a separate **`Vesper.Comparison` package** (opt-in, not Core) —
see [operators-plan](operators-plan.md) (the partition + O1–O10),
[brainstorm-structural-equality](brainstorm-structural-equality.md), and
[brainstorm-comparison](brainstorm-comparison.md). Add as the language grows past
the sample; each is an additive contract+impl pair.

### Tier 2 — explicitly **out** (library, not language fundamentals)

`async`, `tasks`, `resumable`, `mailbox`; `event`, `observable`; reflection
(`FSharpType`/`FSharpValue`); `map`, `set`; `math/z` (BigInteger); units of
measure (`SI`); quotations / Linq / queries; `nativeptr` (target-specific,
revisit only if a target needs it). These are most of the current
`Common/` + all of `Threading/` + `Reflection/`.

## Shape of the minimal core

Two artifacts that must agree, both hand-authored and small enough to maintain
by hand (unlike the 57k-line port):

1. **The contract** (`.fsi`) — minimal signatures in the `Vesper.*` namespace,
   parsed by `XParsec.FSharp` and walked into an `IExternalSymbolProvider` (the
   loader sketched in
   [compiler-clr-project.md](../../XParsec.FSharp.Lib/compiler-clr-project.md)
   §"How … will consume it"). This is the universal, target-agnostic contract —
   decomposed one file per primitive group (see Project structure), not a single
   `Core.fsi`. Because we author it, we stay inside the parser's robustly supported syntax
   subset rather than chasing FSharp.Core's full SRTP / inline-IL surface.
2. **The runtime implementation** (`.fs`/`.ves`) — the Tier-0 types/members
   written in Vesper and compiled by *our own backend* into `Vesper.Core.dll`,
   BCL-only, **no `FSharp.Core.dll` dependency** (see D2).

These are a normal signature/implementation pair, both ours: the `.fsi` feeds
dependency (3), the compiled `.fs` satisfies dependency (2). Drift between them is
caught the same way F# catches it — our compiler checks the impl against its
signature (a conformance check we'll need to ensure is in place; until then, a
reflection round-trip test over `Vesper.Core.dll` substitutes).

**No `.fsproj`** — exactly like `XParsec.FSharp.Lib`. The operators settle it: a
`let inline (+)` value body needs inline IL gated on `--compiling-fslib` (which
doesn't compose — see the Lib's `compiler-clr-project.md`), so `Operators` is
signature-only. The impl `.fs` aren't a clean fsc oracle either: `prim-types-min.fs`'s
array intrinsic `(# "!0[]" #)` and `List.fs`'s `[]`/`::` FSharpList mirror both
hit the same `--compiling-fslib` wall (FS0042). The tree is built only by our own
backend once the self-host ladder lands; until then the `.fsi` contract is the
load-bearing artifact (migration step 1).

## Decisions

- **D1 — Namespace: `Vesper`** (sub-namespaces like `Vesper.Collections`; no
  `.Core` segment — the project/dir is `Vesper.Core`, the namespace is `Vesper`).
  `XParsec.*` stays the name of the *compiler* projects; `Vesper.*` is the
  *language and its runtime*. The source syntax users write (`List.fold`,
  `printfn`, `(+)`) is unchanged — only the *compiled* names behind them move into
  `Vesper.*`. Source files keep the `.fs` extension and stay an F# dialect (D6).
- **D2 — Runtime impl: written in Vesper and compiled by *this repo's own
  backend*** into `Vesper.Core.dll`, BCL-only (no FSharp.Core). This dogfoods the
  compiler and makes the core the first real self-hosted artifact.
  - *Why our backend, not fsc:* any fsc-compiled assembly references
    `FSharp.Core.dll`, re-introducing the dependency. Our backend emits only what
    we tell it, so a Vesper-source core self-compiled by us references only the
    BCL and its own earlier-defined types.
  - *Not a near-term step.* The backend today compiles only top-level `let`s +
    expressions + synthesised closures, and `TDecl` models only `Let` /
    `Expression`. Self-hosting the core forces a large front-end + backend
    expansion — see "Self-hosting capability ladder". There is **no bootstrap
    paradox**: the compiler itself is fsc-built; only the core and user programs
    are self-compiled. The constraint is *capability*, not a build cycle.
  - *C# interim is available* (opt-in) to get FSharp.Core out of emitted programs
    before the ladder is fully climbed; the source of truth is later rewritten in
    Vesper. Not the spine.
- **D3 — `Fun<'A,'B>` is *the* function representation; `FSharpFunc` is dropped
  from the native path.** Closures derive from `System.Object` and *implement*
  `Vesper.Fun<a,b>`; application is `callvirt Fun::Invoke`. This
  leapfrogs [function-representation-plan](function-representation-plan.md)'s
  "FSharpFunc-first" v1 and is *simpler* than slice 5's current closures (no
  `FSharpFunc` base, no protected-base-ctor chain — just `object::.ctor()` + an
  interface impl). In `ClrProvider`, `closureBaseSpec` / `fsharpFuncCtorRef` /
  `encodeType`'s `TyFun` case and the slice-2 `emitInvoke` consumption path
  retarget from `FSharpFunc\`2` to `Fun`. `FSharpFunc` returns only under a
  future **`--fsharp-compat`** flag that pulls in FSharp.Core and emits a dual
  representation (closures that also implement `FSharpFunc`, or adapter wrappers)
  for interop with FSharp.Core-shaped libraries. Out of scope here.
  - *Locked shape:* `Fun<'A,'B>` is an **interface** with a single
    `Invoke: 'A -> 'B`. Closures are **mangled reference classes** implementing
    it in the general case, **mangled value structs** where escape analysis
    proves the closure does not escape its frame (`callvirt Fun::Invoke`
    becomes a `constrained.` call). Two extensions are deferred, both additive:
    `allows ref struct` on the typar bound (waits on region analysis) and a
    higher-arity `Fun<'A,'B,'C>` to flatten curried chains — v1 emits the
    curried `Fun<'A, Fun<'B,'C>>`.
- **D4 — printf: a bespoke minimal formatter, in its own library `Vesper.Printf`**
  (`%d %s %f %b %O %A`), with `%A` for primitives + lists because slice 4
  (`printfn "%A" [1;2;3]`) must stay green. Split out of `Vesper.Core` because it
  is the heaviest tail of the self-host (strings/chars/closure-factory/`%A`
  walk — rung 3) and depends on `Vesper.Core` (`Fun`, `List`), not the other
  way round. Its format type stays a **typed, curried `PrintfFormat`-like type**
  — the `'Printer` typar encodes hole arity+types so `printfn "%d" 42`
  type-checks. `System.FormattableString` is **not** the printf format type: it
  is hole-type-erased (`object[]`) and eager, so it cannot carry the typed
  currying printf needs. (FormattableString belongs to the *interpolation*
  surface — see D9.) Written in Vesper once the ladder reaches
  strings/match/recursion. No reflection-driven record/DU pretty-printing in
  Tier 0. *Not authored in the session that introduced D7–D9.*

  One fork left open (sizes how big "minimal" is): `PrintfModule.PrintFormatLine`
  *returns the printer function* (`"%d"` ⇒ `int -> unit`), so a faithful engine
  is a **closure factory** dragging in all of rung 3. The alternative —
  **compile-time lowering** of literal formats directly to `TextWriter.Write` /
  `ToString` calls — removes `PrintfFormat`/`PrintfModule` from the runtime
  entirely for the literal case, at the cost of more codegen and losing
  format-strings-as-values. **Resolved** in
  [vesper-printf-plan](vesper-printf-plan.md): compile-time lowering on the happy
  path (it's the same handler lowering as D9 interpolation), with `PrintfFormat`
  retained only as the static-field / cold representation for partial application
  and format-as-values — so those are not lost, just off the happy path.
- **D5 — `XParsec.FSharp.Lib`: demote to reference, drop Tier-2 buckets.** Keep
  the verbatim `.fs`/`.fsi` as an archived *reference* for "what FSharp.Core
  does" and any future opt-in re-port; delete the out-of-scope buckets
  (Threading, Reflection, most of Common) once the minimal contract is live.
- **D6 — Vesper stays an F# dialect in `.fs` files, source-compatible with fsc
  where practical (a *dual-compilable* property).** Keeping the syntax means
  existing F# tooling (editors, Fantomas, FCS) works on Vesper source for free,
  and **fsc becomes an oracle**: the core's `.fs` can be built and unit-tested by
  fsc as a reference while the self-hosting ladder (D2) matures, then built
  FSharp.Core-free by our own backend for shipping. Caveats:
  - The fsc build of `Vesper.Core` *does* carry an `FSharp.Core` reference; only
    the **our-backend build is FSharp.Core-free**. Dual-compile is a source-level
    property, not a byte-level one.
  - **Sugar is compiler-relative.** `[1; 2; 3]`, `()`, and lambda values desugar
    to FSharp.Core types under fsc but to `Vesper.Core` types under our backend —
    that retargeting *is* the point. The core's own implementation prefers
    explicit forms (`Cons(1, Cons(2, Nil))`) over collection/unit/function sugar
    to stay unambiguous across both compilers.

  **Sanctioned deviations** — the two places worth breaking dual-compile when the
  payoff is high enough:
  1. **SRTP / IWSAM syntax.** F#'s SRTP surface is powerful but notoriously ugly
     (see the brainstorm's defaulted-typar `(+)` signature); IWSAM (static
     abstract members) is the modern foundation for operator / numeric-tower
     dispatch and dovetails with the `Fun` decision (D3). In the *core* this
     deviation is naturally isolated: the SRTP signatures live in the `.fsi`
     **contract**, which only our parser reads (never fsc) and which has no `.fs`
     body (operators lower to CIL intrinsics) — so cleaner contract syntax costs
     nothing against dual-compile of the implementations. User-level SRTP/IWSAM
     in `.fs` would break dual-compile for those files; a conscious, scoped trade.
  2. **Explicit region / lifetime annotations, orthogonal to types.** A separate
     annotation axis (cf. the `EscapeState` LocalStack / CallerStack / HeapShared
     axis in `SemanticInfo.fs` and [regions-plan](regions-plan.md)) — F# has
     `ref struct` / `byref` / `Span` but no general lifetime surface, so any
     explicit region syntax necessarily diverges from fsc. This is the
     surface for the `allows ref struct` / stack-only-closure extension in
     [function-representation-plan](function-representation-plan.md) §"Region /
     ref-struct extension"; deviation here is opt-in and lands well after the
     minimal core.

- **D7 — `unit = System.ValueTuple`.** Not FSharp.Core's null-valued reference
  `Unit`, and not a bespoke `type Unit = struct end`. `unit` is the nullary
  product — literally the empty tuple — so the BCL's zero-arity value tuple is
  the *principled* identity, and it's free: it's in CoreLib (same assembly as
  `System.Object`, so no new assembly ref), `ToString()` is `"()"`, all values
  are equal, hash is `0`. It deletes a type from the core and a step from the
  self-host ladder (no `Unit` to emit). Trade-offs weighed: as a **struct** it
  forgoes ref-Unit's generic-instantiation *code-sharing* (each value-type
  instantiation specialises) — but Vesper is already heading for struct/`ref
  struct` closures, so that cost shrinks where we're going, and the no-null /
  `() = default` uniformity compounds. Mitigate the instantiation count, if it
  ever bites, by erasing `unit` in non-generic positions. Codegen impact:
  `encodeType`'s unit case becomes `te.Type(eValueTuple, isValueType=true)`
  (flag flips from `false`), and `()` lowers to `default`/`initobj`, not
  `ldnull`. Not interop-identical to F#'s `unit` — irrelevant, we are
  FSharp.Core-free; a `--fsharp-compat` mode would bridge it.
- **D8 — `List<'T>` mirrors FSharp.Core's `FSharpList` verbatim** — `[]`/`::`
  cases, static `Cons` / `Empty`, instance `Head` / `Tail`. This keeps the CLR
  backend's existing list recipes unchanged: `ClrProvider.TryEmitUnionCons`
  already emits the static `Cons` / `get_Empty`, so the list **special-case
  stays** (it does *not* fold into the generic user-DU construction path). The
  cost is that the list type is special forever rather than "just another
  `TyUnion`"; the payoff is zero codegen churn at the cutover and a familiar
  surface. `List.fold` first; the rest of the module is Tier-1 additive.
- **D9 — string interpolation: ref-struct handler by default, `FormattableString`
  as an opt-in interop target — never the one representation.** `$"..."` and
  printf are *separate* surfaces; this decision is only about interpolation.
  - **Default `$"..." → string`**: lower through a **ref-struct interpolated-
    string handler** (à la `DefaultInterpolatedStringHandler` — generic
    `AppendFormatted<T>`, no boxing, writes into a pooled buffer). Zero
    alloc/box, and a stack-only type that fits the `Fun`/regions direction.
  - **`.ToFormattableString()` / FormattableString-targeted**: materialise a
    **typed subclass `VesperFormattableString<…> : System.FormattableString`**
    that stores holes in typed fields, so `Format` / `ArgumentCount` /
    `ToString(provider)` are **non-boxing** and boxing is quarantined to the
    erased `GetArguments()` / `GetArgument(int)` accessors — i.e. it only boxes
    for consumers that read holes through the erased base contract (EF Core
    `FromSqlInterpolated`, structured logging), which by their nature need
    `object[]` anyway. A bounded family (one per arity, like `ValueTuple`,
    `: ISpanFormattable` holes, `object[]` fallback beyond ~7). The default
    string handler is **write-through** (holes gone after writing) and so cannot
    produce a `FormattableString`; the FormattableString path uses a **capture**
    struct instead. Resolve by **target-typed lowering** (string target →
    write-through handler; FormattableString target → capture struct / typed
    subclass) — the same split C# makes, with the lazy-boxing subclass as our
    refinement over the eager `FormattableStringFactory.Create`. The per-shape
    generic instantiation cost lands only when interop is actually used.

### Self-hosting capability ladder (D2)

Self-compiling `Vesper.Core` needs capabilities the compiler doesn't have yet, in
dependency order. Each rung is a front-end + backend increment, testable by
compiling that slice of the core and loading the resulting DLL:

1. **Library output + nominal type / interface emission.** `compile` gains a
   library mode (no `Main` / entry point). `TDecl` grows `type` / `interface` /
   `module` declarations (front-end), and the backend emits `TypeDefinition`s
   for them — generic type parameters on *types*, interfaces, abstract +
   instance methods. Enough to emit `Fun<'A,'B>` (`unit` is now BCL
   `ValueTuple`, so there is no `Unit` to emit — D7) and to make slice-5
   closures *implement* `Fun` rather than derive from `FSharpFunc`.
2. **Union / cons-list type + recursion + `match`.** Emit the list type
   (`Cons` / `Nil`) as a real nominal type and `List.fold` (recursion + `match`
   over the union). Retargets slice-4 list construction onto our own list type.
3. **Strings + chars + the printf engine.** String/char ops, the format walk,
   and `%A` structural printing for lists. The heaviest rung; gates the
   `printfn` / `%A` cutover.

Until a rung lands, that slice of the core can stay on the C# interim so the rest
of the tree stays green.

## Project structure

The contract is **decomposed one file per primitive group** (not a monolith) so a
target includes exactly the primitives it supports; compile order is fixed in
`manifest.toml`.

```
src/
  Vesper.Core/                  ← language fundamentals. NO .fsproj (signature-
    manifest.toml                 first, like XParsec.FSharp.Lib): the operators
    README.md                     alone force contract-only — a `let inline (+)`
                                  body needs inline IL / --compiling-fslib.
    ── contract (.fsi, front-end symbol source) ──
    prim-types-min.fsi          ← int/bool/array/`Fun`/`unit = ValueTuple`
    prim-types-int.fsi          ← integer aliases (sbyte…uint64)
    prim-types-float.fsi        ← float32/float/single/double
    prim-types-string.fsi       ← char/string
    prim-types-object.fsi       ← obj/objnull
    prim-types-exn.fsi          ← exn
    prim-types-decimal.fsi      ← decimal
    prim-types-nativeint.fsi    ← nativeint/nativeptr/voidptr/ilsigptr (target-specific)
    prim-types-nd-array.fsi     ← multi-dimensional arrays
    prim-types-attr.fsi  \      ← compiler-recognised attribute hooks (stubs)
    compiler-attributes.fsi /
    core-types.fsi              ← Ref (ReferenceEquality/NoComparison, O8) +
                                  Result (opt-in StructuralComparison, O10);
                                  ValueOption removed (O9); Option/List now own
                                  packages (package-split PS1)
    ops-platform.fsi            ← arithmetic/bitwise + equality `=` `<>` (CIL intrinsics);
                                  ordering `< > <= >=` → Vesper.Comparison (operators-plan O2)
    ops-std.fsi                 ← logical/composition/pipe (target-agnostic, over bool/Fun)
    ── impl (.fs, our-backend target source → Vesper.Core.dll, BCL-only) ──
    prim-types-min.fs  prim-types-string.fs  List.fs   (partial, growing)
  Vesper.Printf/   (later)      ← printf as its own library (D4): a typed-curried
                                  PrintfFormat-like type + minimal engine.
                                  Depends on Vesper.Core. Not authored yet.
  Vesper.Core.Clr/ (interim)    ← optional C# bootstrap of the same surface until
                                  the self-hosting ladder lands; deleted after.
```

The `.fsi` files feed the front-end symbol provider (in `manifest.toml` compile
order); the `.fs` files are the target source compiled by our own backend into
`Vesper.Core.dll`. None are built by `dotnet`/`fsc`: the array intrinsic
`(# "!0[]" #)` in `prim-types-min` and the `[]`/`::` union in `List.fs` both need
`--compiling-fslib` (FS0042 otherwise — the same wall as the FSharpList port), so
there is no clean fsc oracle. Parser coverage is verified instead by
`VesperCoreContractTests` (every `.fsi`/`.fs` parses with zero recovery
diagnostics). `XParsec.FSharp.Lib/` stays as the demoted reference archive (D5).

## Migration sequence (each step independently testable)

Order chosen so the tree is green after every step and the FSharp.Core
dependency is removed last, only once the replacement is proven.

1. **Author the Tier-0 contract** (`Vesper.Core/*.fsi` + `manifest.toml`) in the
   `Vesper.*` namespace. Verify `XParsec.FSharp` parses every file and the
   declarations extract cleanly (a small test that walks them into symbols).
2. **Retarget the front-end symbol provider** to the `Vesper.*` compiled names so
   source `List.fold` / `printfn` / `(+)` resolve to `Vesper.*` symbols (source
   syntax unchanged). Can land against the contract before any runtime exists.
3. **Stand up `Vesper.Core.dll`.** Climb the self-hosting ladder (rungs 1→3),
   compiling each slice of `Vesper.Core`'s `.fs` with our backend; *or* stand up
   the C# interim DLL to unblock the codegen retarget sooner, replacing it slice
   by slice as each rung lands.
4. **Retarget `ClrProvider` + `Codegen`** — repoint the Tier-0 type refs and
   compiled-name keys from `Microsoft.FSharp.*` to `Vesper.*`; make closures
   implement `Fun` and the consumption path `callvirt Fun::Invoke` (D3);
   point `Codegen.materialiseApp` at `Vesper.Core.dll` instead of
   `FSharp.Core.dll`, and `ClrProvider`'s assembly-identity probe
   (`typeof<Microsoft.FSharp.Core.Unit>.Assembly`) at the core DLL.
5. **Re-green slices 1–5** against the new core. Acceptance: every existing
   `Slice*Tests` and `RunnableAppTests` passes, the on-disk bundle ships **only
   `Vesper.Core.dll`** (no `FSharp.Core.dll`), and the emitted PE has no
   `Microsoft.FSharp.Core` assembly reference.
6. **Slim `XParsec.FSharp.Lib`** — delete the Tier-2 buckets, leave the rest as
   an archived reference (D5), update `compiler-clr-project.md` to point at this
   plan as the live contract.

Steps 1 and 2 are independent of the runtime; 3 is the long pole (the ladder); 4
needs 1–3; 5 is the gate; 6 is cleanup.

## Risks / open questions

- **The self-hosting ladder is the dominant cost (D2).** Rungs 1–2 (type /
  interface / module / instance-member decls in `TDecl` and the backend, plus
  library output, recursion, `match`) are a large front-end + backend expansion —
  effectively "compile a real library," far beyond slices 1–5. This is the gating
  work; the C# interim exists precisely to decouple the FSharp.Core removal from
  it. Decide per-rung whether to wait for self-host or ship the interim slice.
- **printf `%A` for lists is the heavy tail (rung 3).** A focused structural
  printer for cons-lists of printable primitives is bounded but real; reflection-
  driven record/DU `%A` stays out of Tier 0. If too costly when the cutover is
  otherwise ready, keep slice 4's `%A` on the C# interim and gate the rest on a
  `%d`-only acceptance — but prefer doing `%A` properly.
- **Structural equality / comparison — now designed.** Out of Tier 0 (primitives
  use CIL `ceq` / `clt` and BCL `Equals`). Becomes load-bearing once user
  records/DUs and `Map` / `Set` arrive — Tier 1, additive. Design settled:
  equality/hashing in `Vesper.Core`, ordering in a separate `Vesper.Comparison`
  package — see [operators-plan](operators-plan.md),
  [brainstorm-structural-equality](brainstorm-structural-equality.md),
  [brainstorm-comparison](brainstorm-comparison.md).
- **Contract/impl drift.** Minimised because the `.fsi` and `.fs` are a normal
  pair our own compiler should signature-check. A **source-level conformance check**
  now exists (`SemanticAnalysis/Conformance.fs`): it compares the
  parsed `.fsi` `extern` capability set against the parsed `.fs` `(# … #)` intrinsic
  set (plus declaration presence on both sides), so drift is caught the moment both
  files parse — no compiled `Vesper.Core.dll` needed. Deeper member-signature
  conformance for nominal types is still future (it rides with the full `.fsi`/`.fs`
  member checking).
- **Parser coverage of the contract `.fsi`.** We author the contract, so we stay
  inside what `XParsec.FSharp` parses robustly — but the SRTP `(+)` signature
  (the brainstorm's defaulted-typar form) must parse and extract. Validate early
  in step 1; it already type-checks today via `MockBuiltins`, so the extraction
  path is the only new risk.
- **`unit` identity (D7).** `unit = System.ValueTuple` resolves the old
  "round-trip against our `Unit`" question, but shifts it: the codegen's unit
  case (`te.Type(eUnit, false)`) becomes `te.Type(eValueTuple, isValueType=true)`
  — the flag flips because `ValueTuple` is a struct — and `()` lowers to
  `default`/`initobj`, not `ldnull`. Confirm the printf return path
  (`PrintFormatLine<T>` with `T = unit`) and any `Fun<_, unit>` slot round-trip
  with the value-type encoding.

## Acceptance criteria

- The canonical sample
  ```fsharp
  let inline sum xs = List.fold (+) 0 xs
  let nums = [1; 2; 3; 4; 5]
  printfn "%d" (sum nums)
  ```
  builds via `Codegen.materialiseApp`, runs as `dotnet <app>.dll`, prints `15`,
  and its on-disk bundle contains `Vesper.Core.dll` **and `Vesper.List.dll`**
  (the cons-list is its own package now — [package-split-plan](package-split-plan.md)
  PS2) and **no** `FSharp.Core.dll`; the emitted PE has **no**
  `Microsoft.FSharp.Core` / `Microsoft.FSharp.Collections` assembly reference, and
  its function values are `Vesper.Fun<_,_>` (no `FSharpFunc`).
- Slices 1–4 (`printfn "hi"`, arithmetic, `inline`, list `%A`) stay green on the
  new core.
- `Vesper.Core.dll` / `Vesper.List.dll` themselves reference only the BCL (a
  self-hosted build proves the toolchain emits FSharp.Core-free libraries).
- `XParsec.FSharp.Lib` no longer carries Tier-2 buckets and is documented as a
  reference archive.

## Cross-references

- [fsi-target-brainstorm](fsi-target-brainstorm.md) — the universal-contract /
  per-target-impl architecture and the `type int = Platform.Int32` decision this
  plan instantiates minimally.
- [function-representation-plan](function-representation-plan.md) — `Fun<_,_>`
  belongs in the minimum-requirements contract; **this plan goes further than its
  "FSharpFunc-first v1"** and makes `Fun` the native representation outright,
  with `FSharpFunc` deferred to `--fsharp-compat` (D3). That plan should be
  updated to match once this lands.
- [backend-design-plan](backend-design-plan.md) §"Lowering split" — "FSharp.Core
  resolution (reference real DLL vs ship lib's impls)" is explicitly a
  target-specific concern; this plan flips the CLR target from the former to the
  latter.
- [compiler-clr-project.md](../../XParsec.FSharp.Lib/compiler-clr-project.md) —
  the existing full-port tree this plan supersedes as the contract, and the
  manifest/loader mechanics the minimal contract reuses.
```
