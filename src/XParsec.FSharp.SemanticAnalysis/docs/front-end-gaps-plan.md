# Front-end gaps plan

Concrete in-repo work to close before IL emission can sensibly start.
Scope is the TAST surface (Desugar / Unification / Freeze) — nothing
here touches codegen. Order is **locked**: `A → D → C → B`.

The driving requirement is the canonical sample in
[il-emission-roadmap](il-emission-roadmap.md):

```fsharp
let inline sum xs = List.fold (+) 0 xs
let nums = [1; 2; 3; 4; 5]
printfn "%d" (sum nums)
```

Each item below is the smallest unit that closes one of the gaps that
sample exposes.

## A — List literal desugaring — **Done**

**Goal:** `[1; 2; 3; 4; 5]` types and freezes as nested
`UnionCons("Cons", …)` / `UnionCons("Nil", [])` over
`Microsoft.FSharp.Collections.list`.

**Scope:** a `Desugar` rule keyed on the list-literal CST node (and
the array-literal node — bundled per the decision below). No new
TAST shape needed: TAST's `TExpr.UnionCons` is the right node.

> **Reality vs. the original plan.** The plan claimed `FSharpList<_>` was
> already a registered union via the Phase 4 walk over `Common/list.fsi`
> — that turned out to be incorrect. The `'T list` abbreviation lives in
> `Clr/prim-types.fsi` (not `Common/list.fsi`), and the extractor
> currently skips its body because `caseName` returns `ValueNone` for the
> `(::)` constructor (no `Token.OpCons` entry in `opTokenToCompiled`).
> The implementation works around this by referring to the type by name
> (`TyRecord("Microsoft.FSharp.Collections.list", [elemTy])`) and
> hard-coding `"Cons"` / `"Nil"` as the case names in the freeze output.
> Fixing the registration so the type is properly minted from the
> provider is a follow-up (file under `extract-symbols-plan.md`).

**Dependencies:** none. Landed first so the test corpus can include
list-bearing programs that exercise everything downstream.

**Decisions made:**

- **Array literals `[| … |]` separately or bundled?** Bundled. Both
  literal shapes share `inferListLikeLiteral` /
  `translateListLikeLiteral`; arrays simply wrap the lowered list chain
  in `App(External "Microsoft.FSharp.Collections.ArrayModule.OfList", …)`
  so codegen sees a single lowering target.
- **Sequence expressions `seq { … }`?** Out of scope. CE desugaring is
  its own bigger problem and not needed for the sample.
- **Mismatched-paren diagnostic.** Added a Unification-pass backstop:
  `Unification.checkLiteralClose` walks the `rParen` of each list /
  array literal and emits an Error diagnostic on `ctx.Diagnostics`
  when the token is virtual (which is how `pEnclosed` recovers from
  `[| ... ]`, `[ ... |]`, `[| ...`, etc.). Parser diagnostics aren't
  visible to downstream semantic-analysis consumers, so without the
  backstop the malformed literal would type and freeze as if the
  source were correct.

**Annotation:** Desugar tags each list / array `EnclosedBlock` /
`EmptyBlock` node with `DesugaredForm.ListLiteral` or
`DesugaredForm.ArrayLiteral`. Unification and Freeze currently
dispatch on `ParenKind` directly (the annotation is redundant for
their decision); the side-table entry exists so a future consumer
that wants a single point of truth for "this node is a list literal"
doesn't have to re-derive it.

**Where the work landed:**

- `SemanticInfo.fs` — new `DesugaredForm.ListLiteral` /
  `DesugaredForm.ArrayLiteral` cases.
- `Passes/Desugar.fs` — `literalFormOfParen` + Visit-hook arm.
- `Passes/Unification.fs` — `inferListLikeLiteral`,
  `emptyListLikeLiteral`, `checkLiteralClose`.
- `Freeze.fs` — `listLiteralItems` + `translateListLikeLiteral`.

**Test gate (verified):** `let xs = [1; 2; 3]` types as
`TyRecord("Microsoft.FSharp.Collections.list", [TyConst "int"])` and
the frozen TExpr is the corresponding nested `UnionCons` chain.
`let xs = [|1; 2; 3|]` types as
`TyRecord("Microsoft.FSharp.Core.[]", [TyConst "int"])` and freezes
as `App(External "Microsoft.FSharp.Collections.ArrayModule.OfList",
<list-chain>)`. Empty literals (`[]`, `[||]`) leave the element
TyVar free for surrounding context to pin. `[1; true]` and the
mismatched-paren shape both surface diagnostics. New tests in
`DesugarTests.fs`, `UnificationTests.fs`, and `FreezeTests.fs`
cover each case.

## D — Resolved-type validation walker

**Goal:** assert that no `TExpr.*` carries a `SemType` with an
unresolved `TypeVar` reachable via union-find Link chains after
Freeze. Failures surface as `Diagnostic`s on `TastFile.Diagnostics`.

**Scope:** a new pass that runs at the *end* of `Freeze.run`, or as
a sibling validation that `Pipeline.analyse` invokes after Freeze.
Cheap walk: every TAST node has an inline `ty: SemType`; chase each
through union-find Links and assert it bottoms out in a concrete
shape (`TyConst`, `TyFun`, `TyTuple`, `TyRecord`, `TyUnion`, `TyClass`).

Not a one-shot task — it stays on indefinitely. Codegen depends on
it; turning it off lets latent generalisation bugs surface as
mysterious IL-emission errors much later.

**Dependencies:** none. Can land in parallel with §A.

**Decisions to make:**

- **Hard failure vs warning?** Hard failure. An unresolved TyVar at
  this point is a bug in inference, not a user error.
- **Free TyVars at generalised positions OK?** Yes, when they're
  bound by the enclosing `TDecl.Let`'s generalised typars. The
  walker tracks the in-scope quantification set and only fails on
  TyVars not in that set.

**Test gate:** corpus tests pass with the validator on. Any latent
inference gap surfaces as a new diagnostic and is fixed before this
item closes.

## C — `inline` semantics

**Goal:** `let inline sum xs = …` retains its body in the TAST for
per-call-site expansion at codegen time. Resolving SRTP / IWSAM
bounds against caller-side concrete types remains in `Unification`
(see [passes.md §Where `inline` fires](passes.md)).

**Scope:**

- Add `Inline: bool` to `TDecl.Let`. (Or wrap as `TDecl.LetInline of …`
  if the discriminator-style is preferred for the rest of the TAST —
  reuse whatever convention already lives on the `let` shape.)
- Retain the binding's TExpr body verbatim. The body's typars stay
  abstract; substitution at call sites happens at codegen.
- Provide an `inlineExpand : TDecl.Let → SemType[] → TExpr` helper
  that codegen calls per call site with the resolved type
  substitution.

**Dependencies:** none structurally, but lands after §A and §D so
TAST has stabilised before adding a new `TDecl` variant.

**Decisions to make (locked in design discussion):**

- **Retain bodies vs eager pre-monomorphise.** Retain bodies. Eager
  expansion forecloses the IFunc-driven JIT devirt path
  ([function-representation-plan](function-representation-plan.md)) —
  every `inline`-d call site would lose the chance to dispatch
  through a constrained typar.
- **Substitution depth.** v1 substitutes through one level of
  inlining at a time and lets codegen iterate. Nested inline-of-inline
  is fine: each callee's `Inline` flag drives its own expansion.

**Test gate:** `let inline succ x = x + 1 in succ 41` freezes with
the body retained, and a synthetic call-site expansion produces
`TExpr.App(External "op_Addition", 41, 1)` (modulo currying shape)
with `ty = TyConst "int"`.

## B — Printf format string

**Goal:** `printfn "%d" 42 : unit` types correctly. The format spec
in the literal drives the curried result type — `"%d"` produces
`PrintfFormat<(int -> unit), TextWriter, unit, unit>` with the appropriate
curried final shape.

**Scope:**

- **Format-spec parser.** New module that consumes the literal's text
  and yields a structured list of specifiers (`%d`, `%s`, `%A`, width
  / precision modifiers, etc.). Independent of the rest of the project
  — pure string → structured-spec function.
- **`PrintfFormat<_, _, _, _>` as built-in.** Registered in the
  unifier as a special type whose typar arguments are derived from
  the format spec, not from the literal's apparent type.
- **Special-case rule** in `Unification` that inspects string literals
  at `printf`-family argument positions and drives the surrounding
  application's curried result type from the format spec rather than
  from the literal's type.

**Dependencies:** none structurally. Lands after C only because it's
the largest single chunk and benefits from a fully stabilised front
end before its inference special-cases get woven in.

**Decisions to make:**

- **Recognised entry points.** v1 covers `printf`, `printfn`,
  `sprintf`, `fprintf`, `eprintf`. The recognition is keyed on the
  compiled name resolved by `IExternalSymbolProvider`, not on syntax.
- **Spec coverage.** v1 covers `%d %s %f %b %A %O %%`. Width /
  precision / flags lex correctly but don't change the typing
  result. Out of scope: `%a` (callback printer), `%t` (thunk),
  `%P` / `%M` (decimal).
- **Non-literal format strings.** Falls through to standard inference
  — `printfn fmt` where `fmt : PrintfFormat<…>` is already constructed.
  Common idiom; nothing special needed once `PrintfFormat<_>` types.

**Test gate:** `printfn "%d" 42` types as `unit` and freezes as
`TExpr.App(App(External "Microsoft.FSharp.Core.Printf.printfn",
PrintfFormat …), 42)` (precise shape TBD against the curried form
the unifier picks). `printfn "%s %d" "n" 42` shows the curried result
type runs through both args.

## What's *not* in this plan

- **E — Closure / eta lowering** moves to the codegen layer; it
  doesn't change what gets typed, only how it gets emitted. See
  [function-representation-plan](function-representation-plan.md).
- Anything bigger than the sample needs: CEs, async, classes,
  measures, records / DUs beyond what the lib already uses internally.
  Each is its own plan elsewhere in this directory.
- Optimisations of any kind. The front-end produces correct TAST;
  perf wins live at codegen or below.

## Cross-references

- [il-emission-roadmap](il-emission-roadmap.md) — the surrounding
  roadmap; this plan is the in-scope-for-this-project chunk.
- [function-representation-plan](function-representation-plan.md) —
  why §C's "retain bodies" decision is non-negotiable.
- [passes.md](passes.md) — current pipeline; §C adds the `Inline`
  marker and §D adds a new validation slot.
- [extract-symbols-plan](extract-symbols-plan.md) §Phase 4 — the
  registered `FSharpList<_>` union §A piggybacks on.
- [generalisation-plan](generalisation-plan.md) — the existing
  generalisation work §D piggybacks on; the validator's "in-scope
  quantification set" comes from there.
