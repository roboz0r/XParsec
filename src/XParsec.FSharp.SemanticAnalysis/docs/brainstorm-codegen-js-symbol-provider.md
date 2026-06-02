# Brainstorm: Codegen.JS external symbol provider via the TS toolchain

**Status:** brainstorm, not a plan. No Codegen.JS exists yet; the current
backend is CLR. This captures a design discussion so it survives until the JS
backend comes up.

## The core idea

For a future `Codegen.JS`, back its `IExternalSymbolProvider` with TypeScript's
own type machinery instead of hand-writing Fable-style bindings. Feed the
TS Compiler API (`ts.createProgram` + `TypeChecker`) the `.ts`/`.d.ts`/`.js`
surface of a library, and map the resulting `ts.Type` graph into `SemType`. The
payoff: consume already-typed JS/TS libraries with drastically fewer hand
bindings.

This is **not a new architecture** — it is `MetadataSymbols.fs` re-pointed at a
different platform. That file already does the same sentence for .NET: use the
platform's native metadata reader (`MetadataLoadContext`) as an oracle and map
its native type representation (`System.Type`) into `SemType`, following the rule
"skip rather than fake" (§6.1, `MetadataSymbols.fs:48-82`). The provider seam
(`ExternalSymbols.fs:162`) was built to absorb exactly this. So the question is
never "is the shape right" — it is "how big is the `ts.Type → SemType` gap, and
what does the process boundary cost."

## Why the JS target makes the gap small

The decisive asymmetry: **the mismatch is borne by the front-end type, not the
runtime.** Optional props, `undefined`, structural shapes, `any` — all erase to
plain JS at runtime. A lossy front-end mapping (erase a weird type to a dynamic
top) still *codegens correctly*; you only lose compile-time checking, you never
miscompile. So the CLR-era discipline generalizes verbatim: **map precisely or
erase to a dynamic/top type — never invent a wrong nominal type.**

## Mechanism: extractor + manifest, not a live tsc bridge

Do **not** RPC into a live Node checker per lookup. It fights three things:

- **Thread-safety contract.** `TryLookup`/`TryLookupType` must be concurrency-safe;
  per-file pipelines run parallel `PassContext`s (`ExternalSymbols.fs:157-161`).
  A single Node checker is single-threaded.
- **Closures can't cross the boundary.** The provider must return
  `Instantiate: int -> SemType` and builder closures `SemType[] -> SemType`
  (`ExternalSymbols.fs:91,109,133`) that mint fresh `TypeVar`s at a given
  let-depth. Those cannot be serialized out of Node — only a *data description*
  can, which the F# loader rehydrates into closures.
- **Build coupling.** A live bridge makes every .NET compile depend on a running
  Node toolchain.

So: a **TS-side extractor** (full `ts.TypeChecker`, where the hard mapping is
easy) emits a serialized **manifest**; an **F# provider deserializes** it,
rehydrating builder closures from a data schema. This is exactly what
`ReferencedProject.fs` (layer 1) already does for F# contracts. Copy its
rehydration shape.

## Mapping table

| TS construct | Policy | SemType / seam |
|---|---|---|
| Declared class/interface/alias (named) | nominal, direct | `TyClass`/`TyRecord`(`name`, args) |
| Generics `<T>`, `<T extends Foo>`, `<T = string>` | typars + args; bound → constraint; default → `ExternalConstraint.Default` | `TyClass(name, args)` + builder closures |
| Anonymous object type `{x:number}` | content-hash → synthetic nominal name (see below) | `TyRecord(hash, …)` + side-table for readable name |
| `any` | dynamic/top; infectious; member access always succeeds | **new** `TyDynamic` SemType case + unifier rules |
| `unknown` | top, forbids access until narrowed | top type, no member access |
| `never` | bottom | bottom type |
| `T \| null \| undefined` | option-shaped | erased DU **or** native union (open fork below) |
| general union `number \| string` | erased `U2`/`U3` **or** native union | open fork below |
| intersection `A & B` | erase | `TyDynamic` (or one side) |
| literal types `"GET" \| "POST"` | erase to base | `string`/`number` |
| conditional/mapped (`Partial<T>`, `ReturnType<F>`) | query the *evaluated* type from the checker | concrete `SemType` snapshot; generic form lost |
| overloaded functions | overload set via member channel | static-holder + overload set (seam change) |
| function `(a, b?, ...rest)` | curry per existing .NET convention; optional/rest → policy | `TyFun` chain |
| module specifier / `namespace` / `declare global` | re-role `SymbolOrigin` | asm→specifier, ns→namespace path, declType→class |

## Detail notes

### Structural → content-hash nominal name
Hashing the canonicalized shape *recovers structural identity inside a nominal
system*: two anonymous `{x:number,y:number}` hash to the same name and unify,
which is correct TS semantics. Dedup is free. Traps:
- **Cycles** (`interface Node { children: Node[] }`) make a naïve hash
  non-terminating — SCC the type graph, canonically number, then hash. (See
  `brainstorm-tarjan-scc.md` if it covers cycle canonicalization.)
- **Equality, not subtyping.** SemType unification is invariant/equality-based;
  a content hash gives exact-shape match. TS width/depth subtyping is lost.
  Outflow (TS values passed around) is fine; **inflow** (Vesper values into TS
  APIs) must match shape exactly or be coerced via `retype`.
- **Diagnostics:** keep the declared name / pretty-print in a side table; the
  hash is unreadable and `TyRecord` keys on name only.

### `any` is a unifier rule, not the DLR
The C# `dynamic`/DLR mapping is right *only if a .NET backend consumed TS*. For
a JS backend, late binding is native — `x.foo` is just property access, no call
sites. `any` needs a front-end `TyDynamic` with two rules: it absorbs on
unification (`unify(TyDynamic, T) = TyDynamic`, succeed, propagate no
constraints — replicate TS's infectious `any`) and permits any member access.
This is the only genuinely new `SemType` surface the whole idea requires.

### Overloads route through the member channel
A value is one curried `SemType`; members can be a set. Modeling a TS function's
N call signatures as overloaded methods on a synthetic static holder is the right
encoding — but `TryLookupMember` is singular today
(`typeName * memberName -> ExternalMember voption`, `ExternalSymbols.fs:175`), so
this is a **seam change**: return an overload set + resolve against arg types.
The win is that TS overloads and .NET method overloads (e.g. `Console.WriteLine`)
then share *one* resolution path per backend, not a bespoke JS route.

### `retype` escape hatch — mandatory, already have the mechanism
Full transparency is unachievable (ts2fable/Glutinum need manual cleanup for
exactly the rows above). The override channel is **just a higher-priority layer
in the existing `composite` stack** (first-hit-wins, `ExternalSymbols.fs:239`) —
hand-written overrides compose in front of the TS-derived provider, as
referenced-project beats referenced-assembly today. Two jobs: (a) fix a wrong/
lossy mapping, (b) supply precision where the extractor erased to `TyDynamic`.
Per-symbol, keyed by `SymbolKey`; `manifest.toml`-style file fits repo
convention. Also the manual coercion for the structural-inflow problem.

### Manifest schema — a type-description IR
Not SemType (closures don't serialize). A small JSON grammar:
`{typarRef:i}`, `{named:name,args:[…]}`, `{curriedFn:[args],ret}`, `tuple`,
`dynamic`, `structural:{hash,fields}`, `union:[…]`. The F# loader recursively
rehydrates into `SemType[] -> SemType` builders — same shape as
`ReferencedProject`. Version-stamp it (Node extractor and F# loader drift
independently). Extraction unit: batch-per-package first (cacheable), lazy-per-
symbol later.

## Open fork: erased DU vs native anonymous union

Originally proposed: `T | null | undefined` → a `Core.JS` **erased DU** (Fable
prior art: `[<Erase>]`, `U2..U9`) — nominal/typed in the front-end, zero runtime
representation. General unions (`number | string`) → `U2<…>` the same way.

**But:** the XParsec.FSharp parser already accepts arbitrary `ident1 | ident2 |
ident3` type syntax in the CST. (Upstream F# 9 only assigns meaning to `T |
null` for C#-nullable-reference-type interop; the general form is *parser
surface*, not F# semantics — consistent with the repo's "relax the parser, defer
semantic rejection to later phases" principle.) That parser surface is exactly
the room a **TAST/SemType extension for native anonymous union types** could
fill — `number | string` → a real `TyOr [number; string]`, no synthetic `U2`, no
`Core.JS` library type, no wrapping. This is a more faithful match to TS, since
TS unions *are* anonymous structural unions, and it subsumes `T | null`.

The trade-off is the deciding factor:

- **Erased DU (U2):** stays inside the current equality-based nominal model.
  Costs: explicit construction/casing, a `Core.JS` runtime type, and narrowing
  ergonomics (active patterns over the erased DU). Proven by Fable.
- **Native union (`TyOr`):** more faithful, reuses existing parser surface, no
  library type. Cost: union types are inherently a **subtyping/assignability**
  feature (`A` is assignable to `A | B`), and SemType unification is currently
  equality-based/invariant. Adopting native unions pushes the type system toward
  assignability in `unify`, plus a real flow-**narrowing** story (`typeof`-based
  match lowering). That is a deeper change than the erased-DU route.

Codegen is identical either way — both erase to the bare JS value. The fork is
purely about how much subtyping you are willing to introduce into the checker.
This is the same tension as the structural-hash "equality not subtyping" trap; a
decision on one probably wants to settle the other.

## Five gating decisions

1. **Manifest schema** — the serializable type-description IR + F# rehydrator.
   The keystone.
2. **`SemType` additions** — `TyDynamic` for `any` (required); `unknown`/`never`
   tops/bottoms; native `TyOr` *iff* the union fork goes native.
3. **Overload representation** — principal-signature (lossy) vs extend
   `ExternalMember`/`TryLookupMember` to sets + overload resolution.
4. **Union representation fork** — erased DU vs native `TyOr` (and how much
   subtyping/narrowing that pulls in).
5. **`null`/`undefined`/optional policy** — option-shaped mapping + codegen
   erasure rule.

## Inverse problem to keep in mind

Everyone frames this as "TS is richer than F#." The sharp edge is the opposite:
F#'s inference asks questions of a type that TS never answered. When Vesper code
uses a TS-sourced type in a generic needing `when 'T : equality`, comparison, or
SRTP `(+)`, the provider must answer constraint questions TS metadata simply does
not contain (`ExternalConstraint`, SRTP `MemberTrait`). Needs a default policy
(assume structural/`===` equality, or refuse generic-constrained use) — it won't
fall out of the manifest.

## Seed milestone

Mirror the CLR "printfn hi end-to-end" discipline — one vertical slice, not
breadth: a single non-generic interface with primitive-typed members + one free
function, round-tripping `.d.ts` → JSON manifest → F# provider → resolves in a
Vesper program.

## Prior-art reality check

This is ts2fable/Glutinum re-aimed at emitting `SemType` manifests instead of F#
binding source, consumed directly by the compiler. Their need for manual cleanup
is the empirical evidence that "transparent" oversells it. Honest payoff:
auto-generate most of the surface under a fixed erasure policy + a small `retype`
override file — not zero bindings. Genuine wins over ts2fable: no intermediate
compile step, lazy on-demand resolution (big for `@types/node`/DOM), types stay
live rather than frozen into a binding lib.
