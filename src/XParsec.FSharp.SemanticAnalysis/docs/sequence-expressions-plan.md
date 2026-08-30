# Sequence expressions — design & plan

## Objective

Lower `seq { … }` and the list/array comprehensions (`[ for x in xs -> f x ]`, `[| … |]`) to a lazy
pull object on both targets.

*(The motivating example is spent: `truncate` delegated to CLR-Linq `Enumerable.Take` in
`seq.clr.fs` because the backend could not lower a `seq { }` body, making it the one member of
`Vesper.Seq` with no JS lowering. §"`Seq.truncate` does NOT need sequence expressions" below was
acted on — `Vesper.Seq/seq.fs:6-71` now hand-writes `TruncateSeq<'T>`, the file is target-neutral,
and the `System.Linq` dependency is gone.)*

Distinct from **general computation expressions** (`async { }`, `option { }`, a user `Builder()`),
which desugar to *builder method calls* (`Bind`/`Return`/`Combine`/`Delay`/`Zero`/`Run`/…). F# treats
sequence expressions as a compiler-intrinsic form that compiles to a state machine, NOT to
`SeqBuilder` calls. The two features share the CE *syntax* and nothing else. **Scope here is the
intrinsic sequence form only**; general CEs need their own plan and should not be conflated with this
one.

## Where this stands

- **Parser — COMPLETE.** The full CE surface already parses and is in the AST (`Expr.fs`):
  `Expr.ControlFlow(ControlFlowKeyword.Yield | YieldBang | Return | ReturnBang | Do | DoBang, expr)`,
  `LetOrUseKeyword.LetBang | UseBang`, and `ParenKind.Brace` for `{ … }`. A `seq { … }` parses as
  `App(Ident "seq", [EnclosedBlock(ParenKind.Brace, body)])`. No parser work is required.
- **Semantic analysis — ABSENT.** No pass matches `Expr.ControlFlow` at all — not `Infer`, not
  `ElaborateExpr`. Everything downstream of the parser is missing.
- **Backends — nothing.**

### Latent bug to fix first (independent of this feature)

`let!` and `use!` are not merely unimplemented — they are **silently accepted as ordinary `let` /
`use`**. `Infer.fs:310-316` and `ElaborateExpr.fs:726` match `LetOrUseKeyword.LetBang` /
`UseBang` alongside `Let` / `Use` and simply infer the binding as if the `!` were not there. That is
an unsound accept, not a diagnostic: `let! x = xs` type-checks as `let x = xs` and compiles to
something the author never wrote. It should be an explicit "not supported" error until CEs land.
`Expr.ControlFlow` by contrast is simply unhandled, which surfaces as a pass failure rather than a
wrong program — much less dangerous.

## What the iteration capability already unblocks

The `seq` / `enumerator` capability landed and is authorable by Vesper code (`Vesper.List` implements
it directly). That matters here twice over:

1. **A lowered sequence expression is just a synthesised nominal implementing the capability.** It
   needs `interface seq<'T>` + `interface enumerator<'T>` and nothing else. The CLR backend already
   synthesises the BCL co-slots for such a type (`CoSlot` / `CapabilityCoSlots`), so it interops with
   `foreach` in C# for free; the JS backend already buckets a capability impl into `[Symbol.iterator]`
   (`EmitJsTypes.partitionClassMembers`). **Neither backend needs a new type-emission concept** —
   only a new *body* shape.
2. **A lazy combinator no longer needs a state machine at all** (see below).

## `Seq.truncate` does NOT need sequence expressions — **DONE**

Landed as `TruncateSeq<'T>` in `Vesper.Seq/seq.fs` (`:6-71`), which is the `TakeSeq` shape sketched
below. `truncate` needs exactly **one lazy combinator**, and a lazy combinator is
plain Vesper code:

```fsharp
type TakeSeq<'T> =            // holds source + count; implements interface seq<'T>
type TakeEnumerator<'T> =     // holds the inner enumerator<'T> + remaining; implements interface enumerator<'T>
```

That is precisely the shape `List` / `ListEnumerator` already proves end-to-end on both targets, and
the shape `struct-seq.clr.fs` already hand-writes for `MapSeq` / `MapEnumerator` in the struct-chaining
surface. It ports to JS, and it drops the `System.Linq` dependency from `Vesper.Seq`.

**Recommendation: do `truncate` this way, independently, and do not gate it on this plan.** The same
route opens the rest of the lazy `Seq` surface (`map` / `filter` / `takeWhile` / …) — one combinator
pair each, no compiler feature. Sequence expressions remain worth building, but for *authoring
ergonomics* (arbitrary user-written lazy code), not to unblock the library.

## Design

### P1 — One front-end lowering, two backend strategies

The front end lowers a `seq { … }` body to a single new TAST node (a "generator body": the resumable
statement list, its yields, and its captured locals). The backends then diverge, because their native
idioms are genuinely different and the JS one is nearly free:

- **JS — a generator method.** `function*` + `yield` maps 1:1 onto the body. The backend already
  emits `*[Symbol.iterator]()` generator methods (`EmitJsMembers.emitIteratorMethod`), so a `seq { }`
  lowers to a generator method on the synthesised type with no state machine at all. Cheap.
- **CLR — a resumable state machine.** No such idiom exists: the body must become a class with an
  `int` state field, one field per live local, and `MoveNext` as a `switch` over states, with each
  `yield` splitting the CFG at a resume point. `for` / `while` / `try` inside the body each add
  states. This is the bulk of the work and the reason to sequence it last.

### P2 — Restricted subset first

Start with `yield`, `for … in`, `while`, `if`/`else`, and `let` (immutable) inside the body. Defer
`yield!` (flattening — needs a nested-enumerator state), `try`/`finally` (needs a fault path through
the state machine, interacting with the enumerator's inherited `disposable`), and `use`. Reject the
deferred forms with a clear diagnostic rather than mis-lowering them.

### P3 — Comprehensions desugar to the same node

`[ for x in xs -> f x ]` and `[| … |]` are the same generator body plus a materialising terminal
(`List.ofSeq` / `Seq.toArray`). They should not get an independent lowering.

### Open questions

- **Where does the synthesised type live?** The closure machinery (`EmitClosures`) already synthesises
  types with captured-variable fields and is the obvious host, but a closure has one `Invoke`; a
  generator has `GetEnumerator` + `MoveNext` + `Current` and a state field. Decide whether to extend
  closure synthesis or add a sibling.
- **Struct vs reference enumerator.** `ListEnumerator` is a `[<Struct>]`; a state machine that is
  itself the enumerator can be too, but only if it is never boxed on the hot path. Probably start
  with a reference type and revisit.
- **Does `seq` need recognising as an intrinsic?** `seq { … }` is `App(Ident "seq", …)` — the same
  shape as any function application. It must be recognised *before* overload/application inference
  tries to type `seq` as a value. Cf. how other intrinsic forms are recognised.

## Build order

1. **Reject `let!` / `use!` explicitly** (the latent-bug fix above). Independent, small, and it stops
   a wrong-program accept.
2. **`Seq.truncate` via a hand-authored `TakeSeq` / `TakeEnumerator`** — independent of everything
   below; closes the last non-portable member of `Vesper.Seq` and removes `System.Linq`.
3. **Front end**: recognise `seq { … }`, type-check the body, infer the element type, reject the
   deferred forms (P2).
4. **Lowering**: the generator-body TAST node + the synthesised capability-implementing type.
5. **JS backend**: emit the body as a generator method. Proves the whole pipeline cheaply.
6. **CLR backend**: the state machine. The bulk.
7. **Comprehensions** (P3) on top.
