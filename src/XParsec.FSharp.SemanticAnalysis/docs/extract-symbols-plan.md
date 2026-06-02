# Extract-symbols plan

Build-plan for `VesperLib.extractSymbols`, the function that walks parsed
`.fsi` files from `src/XParsec.FSharp.Lib/` and emits
`(compiledName, ExternalSymbol)` pairs into the
`IExternalSymbolProvider` table.

The plumbing around it already exists ([`VesperLib.fs`](../VesperLib.fs)):
manifests load, files lex and parse cleanly, and the provider/chain
combinators are in place. This plan describes how the walker fills the
table.

## Status

| Phase | State | Notes |
|-------|-------|-------|
| 1 — Monomorphic vals + walker skeleton | **Done** | `ExtractCtx`, path-tracking walker, primitive `TyConst` translation, function types, basic compiled-name assembly. |
| 2 — Polymorphic vals | **Done** | Free-typar collection (regular + SRTP), `Instantiate level` mints fresh `TyVar`s, `TupleType`/`SuffixedType`/`GenericType`/`ArrayType` translation, `[<CompiledName>]` honoured, `[<CompilationRepresentation(ModuleSuffix)>]` applies the `Module` suffix. |
| 3 — Cross-bucket resolution | **Done** | Per-file `open`-clause tracking, qualified-name lookup, `ctx.QualifiedTypes` index, skip-with-diagnostic on unresolved `LongIdent` (recorded in `ctx.Skipped`, separate from file-level `Diagnostics`), end-to-end chain-provider fixture. |
| 4 — Type declarations as a separate provider surface | **Done** | `IExternalSymbolProvider.TryLookupType` ships. Record fields, union cases, and abbreviation RHS bodies are extracted from `.fsi` declarations into closure-shaped builders parameterised over the type's declared typars. Classes / interfaces / enums / delegates still register only as name+arity (body extraction lands later). |
| 5a — SRTP / trait constraint capture | **Done** | `ExternalConstraint` captures `Trait`, `MemberTrait`, `Default` clauses from `WhenConstrainedType`. Trait-style constraints (Equality/Comparison/Struct/RefType/Nullness/NotNull) are *applied* to fresh `TyVar`s at instantiation time so the existing unifier honours them. `MemberTrait` and `Default` are stored as opaque markers for Phase 5b. |
| 5b.1 — Default defaulting at generalisation | **Done** | `ExternalConstraint.Default` now carries the resolved target (`SemBuilder` over the symbol's typars). `Instantiate` stamps the target onto the source `TyVar`'s new `Defaults` field; generalisation runs `applyDefaults` before quantification, chasing the chain through union-find Links and linking the source to the first concrete shape it reaches. Iterates to fixpoint (`default ^T3 : ^T1 ; default ^T1 : int` resolves in two passes). Short-name lookups (`op_Addition`) now hit the lib via an auto-open prefix table (`Microsoft.FSharp.Core.Operators`, `LanguagePrimitives.IntrinsicOperators`, etc.). Test gate: `let x = 1 + 2` types as `int` through a **lib-only** provider with no `MockBuiltins` in the chain. |
| 5b.2 — SRTP member-trait unifier + production swap | **Done** | `MemberSignature` promoted from placeholder to a real shape carrying `MemberName` + `ArgTypes` + `ReturnType` + a shared `Resolved` ref. `ExternalConstraint.MemberTrait` now carries the trait's compiled member name plus per-arg/return `SemBuilder`s; instantiation stamps the captured shape on every participating fresh TyVar's `SrtpBounds`. `Unification.drainSrtpBounds` fires from the same link sites as `drainConstraints` and dispatches against (a) a built-in numeric-primitive table for `op_Addition` / `Subtraction` / `Multiply` / `Division` / `Modulus` / `UnaryNegation` / comparison, or (b) the candidate's `ctx.ClassTypes` entry. Tupled and curried candidate shapes are both accepted. `VesperLib.defaultProvider libRoot` caches a single parsed lib per root for production callers. **`MockBuiltins` is kept** as a documented test fixture / minimal provider example. Test gate `V() + V()` (V has `static member (+) (a: V, b: V) = V()`) types as `TyClass "V"` through a lib-only provider. |

As of the most recent run, `VesperLib.buildProvider` over the live
`XParsec.FSharp.Lib` manifest extracts a substantial portion of the
val table and registers ~195 type declarations across the 28 `.fsi`
files. Phase 3's stricter resolution drops vals whose `LongIdent`
references can't be resolved (BCL types like `IEnumerable`, etc.) —
these now land in `ctx.Skipped` rather than masquerading as opaque
`TyConst`s. Tests covering Phases 1–5b all pass (350 tests, 1
pre-existing debug skip):

- Phase 1: `Operators.Not` mono extraction.
- Phase 2: `OptionModule.Map`, `ResultModule.Map`, `ListModule.Map`,
  `ArrayModule.Map`, TyVar freshness, level stamping.
- Phase 3: chained `buildProvider` + `MockBuiltins`,
  `let x = 1 + 2 : int` end-to-end.
- Phase 4: `Microsoft.FSharp.Core.option` abbreviation,
  `Microsoft.FSharp.Core.Result` union surface.
- Phase 5a: `Seq.Contains`'s `when 'T: equality` clause captured
  and applied to the fresh TyVar at instantiation; SRTP member-trait
  marker preserved on `Seq.Sum` (when extracted).
- Phase 5b.1: `let x = 1 + 2` typed as `int` through a lib-only
  provider (no `MockBuiltins` chain) — `op_Addition`'s SRTP signature
  `^T1 -> ^T2 -> ^T3` with `default ^T1 : int` resolves at
  generalisation, chaining `default ^T3 : ^T1` through to int.
  Short-name `op_Addition` resolves via auto-open prefix
  `Microsoft.FSharp.Core.Operators`.
- Phase 5b.2: `V() + V()` where `V` declares
  `static member (+) (a: V, b: V) = V()` types as `TyClass "V"`
  through a lib-only provider — `drainSrtpBounds` fires when the
  first participating typar resolves to `TyClass "V"`, looks up
  the static `op_Addition` in `ctx.ClassTypes["V"]`, and unifies
  the trait return slot. Also: `VesperLib.defaultProvider libRoot`
  caches the lib once per root for production callers.

The one persistent extraction gap is the lone `.fs` file in the lib
(`Common/SI.fs`, a `[<Measure>]` types file) — the implementation-file
parser doesn't accept it. `buildProvider` whitelists it as a known v1
limitation; the test suite ignores the resulting per-file error.

## Goal

After this plan lands, the in-memory table behind
`VesperLib.buildProvider` answers production semantic-analysis queries
— operators, FSharp.Core types, polymorphic combinators — sourced from
real `.fsi` files instead of a hand-written table.

`MockBuiltins` stays. It's a documented test fixture and the simplest
possible example of the `IExternalSymbolProvider` interface; the
pluggable-provider architecture expects more implementations (a
compiler-log "fake target", `.NET` integration via `MetadataLoadContext`,
etc.) to slot in alongside `VesperLib.buildProvider`. Removing the
in-tree minimal example would weaken that pattern. Production-path
*wiring* still moves to `VesperLib.buildProvider`; what changes is
which provider tests opt into.

Phase 5 below is the gate: the semantic-analysis test corpus must pass
when production wiring routes through `VesperLib.buildProvider`.
Individual tests can still use `MockBuiltins.provider` for isolation
(when the test is about the unifier and the lib's surface is irrelevant
or too slow). Anything not type-checking after the production swap is
either (a) a real bug in extraction, (b) a missing piece of the Lib
port (e.g. `fslib-extra-pervasives`), or (c) a missing representation
in `SemType`.

## Target awareness

`XParsec.FSharp.Lib` is the **CLR** transliteration of FSharp.Core today
(the `Clr` bucket carries the platform primitives; everything above is
target-agnostic or .NET-coupled). The long-term plan
([fsi-target-brainstorm](fsi-target-brainstorm.md)) splits this into a
**curated menu of `.fsi` capability modules** that target projects pick
from. Each target ships its own `Core.<Target>.fsproj` that references
the subset of `.fsi` files it can support, plus `Core.<Target>.fs`
implementation files whose `when ^T : foo` arms provide the actual
dispatch. Each target compiles into one FSharp.Core assembly
(`feedback_fsharpcore_one_assembly`).

The curation has a single mandatory floor: a **minimum-requirements**
`.fsi` defining the language's irreducible surface — `type int`,
`type unit`, `type bool`, core arithmetic / comparison operators, and
whatever else the parser and type checker need a name for. Every target
must reference this file (or replace it with a target-local file
declaring the same names). Everything beyond the floor — `int64`,
`decimal`, `int53`, `array`, `Option`, `Async`, SRTP-heavy operators,
SIMD, `nativeptr` — lives in its own opt-in `.fsi` module. A target
that omits `int64.fsi` simply has no `int64` in scope; user code that
mentions it gets a clean unresolved-name error from the type checker.

Three things follow for this plan:

1. **`buildProvider` stays single-arg.** The "target" is the curated
   manifest at `libRoot/manifest.toml`. `buildProvider libRoot` walks
   exactly the buckets / files that manifest lists. No `Target` enum,
   no switching inside the extractor — choosing a target is choosing
   which `libRoot` to point at (or which manifest to load), and the
   semantic-analysis layer doesn't need to know the difference.
2. **SRTP constraints are load-bearing**, not droppable. The minimum-
   requirements `.fsi` declares `(+)` as
   `^T1 -> ^T2 -> ^T3 when (^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) and default ^T3: ^T1 and default ^T1: int`.
   Without the constraint and defaults, the type checker can't resolve
   `1 + 2 : int` or reject `(1L : int64) + 1` when no `int64.fsi` is
   loaded. §A and §B below treat `WhenConstrainedType` as structured
   input the walker captures, not as trivia it discards.
3. **Static-optimisation clauses and inline IL in `Clr/prim-types.fsi`
   are transient.** They exist because the current Lib is a literal
   port of upstream FSharp.Core (`feedback_fsharpcore_port_transliterate`).
   The migration target is per-target `.fs` dispatch arms with
   intrinsic strings (`(# intrinsic "Int32.Add" x y #)`-style), keyed
   by the SRTP signatures the minimum-requirements `.fsi` will own. v1
   of `extractSymbols` reads what's there now; Phase 5 records the
   follow-up to re-cut the lib into the curated menu.

## What we have to build on

| Piece | Where | Status |
|-------|-------|--------|
| Manifest reader → ordered `LibFile` list | `VesperLib.fs` `loadAll` | Done; smoke-tested over all 5 buckets. |
| Per-file lex + parse | `VesperLib.fs` `parseFile` | Done; every `.fsi` in Lib parses clean. |
| Provider interface + helpers | `ExternalSymbols.fs` `mono` / `poly` | Done; ready to receive extracted entries. |
| Hand-built reference table | `ExternalSymbols.fs` `MockBuiltins` | Done — the shape we have to replicate, then replace. |
| `SemType` core variants | `SemanticInfo.fs:89` | Sufficient for Phase 1–3. Records/unions land in Phase 4; SRTP constraints land in Phase 5 alongside the per-target split. |
| Pratt of typars in the parser | `Expr.fs` `Type<'T>` / `Typar<'T>` | Done; CST has `VarType (Named ...)`, `FunctionType`, `TupleType`, `GenericType`, `SuffixedType`, etc. SRTP typars (`^T`) come through as `Typar.Statically`. |
| Signature CST | `Signatures.fs` | `ValSig`, `TypeSignature`, `ModuleSignatureElement` all present, including `WhenConstrainedType` for `when`-clauses. |
| Bucket layout | `src/XParsec.FSharp.Lib/manifest.toml` | Buckets are opt-in capability modules. Today's manifest references the full CLR transliteration (`Clr` + `Common` + `Threading` + `Reflection` + `Printf`). Future per-target manifests pick a subset — `Js` might keep `Common` + `Printf` and swap `Clr` for a JS-primitives bucket. The extractor doesn't care; it walks whatever the manifest lists. |

Missing pieces, in build order:
1. **Path tracking** — namespace + nested-module path as the walker descends.
2. **`Type<SyntaxToken> -> SemType` translation** — the central piece. Handles `'T`, `^T`, `_ -> _`, `_ * _`, `LongIdent`, `GenericType`, `SuffixedType`.
3. **Polymorphism / freshening** — collect free typars (regular and SRTP) in a `ValSig`; build a `poly` symbol whose `Instantiate level` mints fresh `TypeVar`s and substitutes.
4. **Compiled-name resolution** — module path + ident + `[<CompiledName(_)>]`.
5. **Type-declaration extraction** — Phase 4 only; emits entries to `ctx.UnionTypes` / `ctx.RecordTypes` alongside the val table.
6. **SRTP-constraint capture** — Phase 5; collect `when (^T or ^U): (static member …)` and `default ^T : <ty>` constraints so the type checker can resolve `1 + 2 : int` against the universal `(+)` signature without consulting per-target dispatch arms.

## Architecture

`extractSymbols` lives in `VesperLib.fs`. It is called once per parsed
file by `buildProvider`, in bucket-topo-order, with an accumulating
context so cross-file references resolve.

```fsharp
type ExtractCtx = {
    /// Compiled-name table being built up across all files.
    Symbols: Dictionary<string, ExternalSymbol>
    /// Type names declared so far (for SemType TyConst resolution).
    /// "Microsoft.FSharp.Core.Option" → arity 1, declared at this path.
    Types: Dictionary<string, TypeDeclShape>
    /// Per-file diagnostics — extraction failures don't abort the build.
    Diagnostics: ResizeArray<LibFile * string>
}

/// Walk one parsed signature file, mutating the context.
val extractSymbols : ctx: ExtractCtx -> file: LibFile -> ast: FSharpAst<SyntaxToken> -> unit
```

`buildProvider` becomes:

```fsharp
let buildProvider (libRoot: string) =
    match loadAll libRoot with
    | Result.Error e -> Result.Error e
    | Result.Ok loaded ->
        let ctx = ExtractCtx.empty()
        // `loaded.Files` is whatever the manifest at libRoot lists, in
        // topo order. Targets distinguish themselves by which .fsi
        // files their manifest references, not by a flag we pass here.
        for file in loaded.Files do
            match parseFile file with
            | Result.Ok ast -> extractSymbols ctx file ast
            | Result.Error e -> ctx.Diagnostics.Add(file, e)
        Result.Ok (ExtractCtx.toProvider ctx, List.ofSeq ctx.Diagnostics)
```

A caller targeting CLR points `libRoot` at the current
`src/XParsec.FSharp.Lib/`. A future JS target points it at its own
`Core.JS.fsproj`'s lib root (or a sibling `XParsec.FSharp.Lib.Js/`
with a manifest selecting different buckets) — same extractor, same
provider shape.

## The hard sub-problems

### A. `Type<SyntaxToken>` → `SemType`

`Type<'T>` cases that *must* translate (Phase 1–3):

| `Type<'T>` case | `SemType` mapping | Notes |
|---|---|---|
| `ParenType (_, t, _)` | recurse into `t` | trivia-only wrapper |
| `FunctionType (a, _, b)` | `TyFun (semOf a, semOf b)` | curried; `a -> b -> c` lexes left-assoc, semOf nests right-assoc |
| `TupleType (ts, _)` | `TyTuple (List.map semOf ts)` | flat |
| `VarType (Named (_, ident))` | `TyVar (lookupTypar ident)` | typar scope local to the enclosing `val` |
| `VarType Anon` | fresh `TyVar` at current level | rare in `.fsi`; treat like `_` |
| `NamedType longIdent` | `TyConst (resolveName longIdent)` | `int`, `string`, `obj` → primitive `TyConst`s. F#-defined types resolve to a canonical compiled name. |
| `GenericType (li, _, args, _, _)` | `TyRecord (resolveName li, [for a in args -> semOfArg a])` | `Map<'K,'V>`, `Option<'T>`. `TyRecord` is the misnamed-but-correct constructor today; Phase 4 may split. |
| `SuffixedType (base, longIdent)` | as `GenericType` with `[base]` | `'T list` ≡ `List<'T>` |
| `ArrayType (base, _, commas, _)` | `TyConst "<rank>d-array"` wrapping `semOf base` | rank from `commas.Length + 1`. Open issue — see §Open questions. |

Cases that get **dropped** in v1 (translate the inner type, ignore the modifier):

- `SubtypeConstraint (_, _, t)` — keep `t`.
- `AnonymousSubtype (_, t)` — keep `t`.

Cases captured but **parked** in v1 (translate the inner type, record
the modifier alongside the val for later phases to consume):

- `WhenConstrainedType (t, clauses)` — translate `t`, attach `clauses`
  to the val's metadata. Phases 1–4 ignore the clauses; Phase 5 wires
  them into `SemType` so SRTP-bearing operators like `(+)`, `(<)`
  resolve correctly under multi-target dispatch. Dropping them at this
  stage would silently break `1 + 2 : int` once `MockBuiltins` is gone.

Cases that **error/skip** in v1 (file emits a diagnostic, val is not added to the table):

- `Null _` — `'T : null` types.
- `UnionType` — `obj | null` style. (Translate to a sum once `SemType` grows the variant.)
- `ILIntrinsic _` — inline IL signatures in `Clr/prim-types-prelude.fsi`
  (array indexing) and `Clr/prim-types.fsi` (arithmetic). v1 bypasses
  with a hand-written `TyConst` table keyed by bracketed-name (`[]`,
  `[,]`, ...) for arrays, and treats the inline-IL operator bodies as
  opaque — the SRTP signature on the val is what the type checker uses
  anyway. **Long-term**: per the [target brainstorm](fsi-target-brainstorm.md),
  these declarations move from `.fsi` to per-target `.fs` files
  (`Core.CLR.fs`, `Core.JS.fs`, …) as `when ^T : foo = (# intrinsic "…" #)`
  dispatch arms, and disappear from extraction entirely.
- `MeasureType _` — measure expressions. Used by `SI.fs`; handle in a later phase.
- `AnonRecordType _` — `{| x: int |}`. Rare in `.fsi`.
- `Missing` / `SkipsTokens _` — parser-recovery artefacts.

### B. Free-typar collection and instantiation

For a `ValSig` with `typars: TyparDefns<'T> voption`:

1. Collect the declared typars (`'T`, `'U`, ...) into a list, preserving order.
2. Walk the curried signature, also collecting any **referenced typars not in the declared list** (F# allows implicit generalisation in signatures; `val map : ('T -> 'U) -> 'T list -> 'U list` is legal without `<'T, 'U>`).
3. Build a `Map<string, int>` from typar name to its position in the merged list.
4. The val's `Instantiate level`:
   ```fsharp
   fun level ->
       let fresh = Array.init typarList.Length (fun _ ->
           let tv = TypeVar()
           tv.Level <- level
           TyVar tv)
       semType |> substituteTypars (fun name -> fresh[typarMap[name]])
   ```
5. Translation produces a `SemType` template that references typars by **name**; substitution at instantiation time replaces names with fresh `TyVar`s.

For monomorphic vals (no typars used), `Instantiate _` returns the
shared `SemType` value — same as `ExternalSymbols.mono`.

**Regular vs SRTP typars** (`'T` vs `^T`). The CST distinguishes the
two — `Typar.Named` for `'T`, `Typar.Statically` for `^T`. Both go into
the same merged typar list above, but SRTP typars carry extra metadata
that Phase 5 reads:

- Their **member constraints** (`when (^T or ^U) : (static member (+) : ^T * ^U -> ^V)`).
- Their **default constraints** (`and default ^T : int`), which drive
  the type checker's behaviour when inference is stuck.

The structured `WhenConstrainedType` captured in §A above hangs off the
val's metadata; the typar map keys it by name so a Phase-5 constraint
solver can look up "what's the member constraint on `^T1`?" without
re-walking the AST.

For v1 (Phases 1–4), an SRTP typar is treated identically to a regular
typar at instantiation — fresh `TyVar` per call site — and the
constraints are stored but not yet consulted. This is enough for
generic combinators (`Option.map`, `List.choose`) but not for the
arithmetic operators in `Clr/prim-types.fsi`. Phase 5 closes that gap.

### C. Compiled-name resolution

Compiled name = `<namespace>.<module-path>.<ident>` with these overrides
in priority order:

1. `[<CompiledName("Foo")>]` on the val — replaces `<ident>` with `"Foo"`.
2. `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]` on the enclosing module — appends `"Module"` to the module name (e.g. `List` → `ListModule`).
3. Operators (`(+)`, `(<|)`) have IdentOrOp encoding in the CST; their compiled name is the conventional `op_Addition`, `op_PipeLeft`, etc. Build a lookup table from operator text to the operator-name convention.

The walker carries a `path: string list` (innermost-last) and a
`compiledOverrides: Map` capturing the `ModuleSuffix` flags seen on
ancestor modules.

For type definitions, the compiled name appends a backtick + arity for
generic types: `type Option<'T>` → `Microsoft.FSharp.Core.Option\`1`.

### D. Cross-bucket / cross-file type resolution

`Common/seq.fsi` mentions `Option<'T>`. `Option` is declared in
`Clr/option.fsi` (compiled-name `Microsoft.FSharp.Core.Option\`1`).
The walker has to:

1. When it encounters a `Type` declaration, register the canonical
   compiled name in `ctx.Types`.
2. When it encounters a `LongIdent`-based reference in a val signature,
   resolve it against `ctx.Types` first (multi-open scope rules) and
   fall through to the `open`-prefixed search if not found.

For v1 the resolver can be naive — match suffixes against `ctx.Types`
keys and pick the most specific. F#'s real lookup considers `open`s,
auto-opens, and prelude qualifiers, but most signature references in
FSharp.Core use either short-form on locals or fully-qualified
`Microsoft.FSharp.X.Y` paths. Both cases work with suffix-match.

Open scope today comes from `open Microsoft.FSharp.Core` etc. at the
top of each `.fsi`. Walk those into a per-file list of prefix candidates
to prepend during resolution.

### E. Value restriction / attribute filtering

Skip declarations carrying:

- `[<Experimental>]` — keep them, they're still usable.
- `access = AccessInternal` or `Private` — skip.
- `[<CompilerMessage>]` / `[<Obsolete>]` — keep.
- `let private` in a top-level module — skip.

The `Access<'T>` parameter on `ValSig` carries visibility directly.

## Phased delivery

Each phase is a self-contained unit of work that lands a buildable,
test-covered improvement. Don't skip ahead — Phase 1 forces decisions
that ripple into Phase 2.

### Phase 1 — Monomorphic vals in one module  *(Done)*

The original target (`Common/Random.fsi`) turned out to carry only a
static-member type, not free-standing vals — the file in our lib is
`type internal ThreadSafeRandom = static member Shared: Random`. The
walker still produces zero output for it (the only val-like thing is a
type member, which Phase 4 covers, and the type is `internal`). The
Phase 1 test instead asserts a known monomorphic val from the broader
lib: `Microsoft.FSharp.Core.Operators.Not : bool -> bool`.

Delivered:
- Path-carrying walker over `SignatureFile.Namespaces` /
  `NamedModuleSignature` / `AnonymousModuleSignature` →
  `ModuleSignatureElement.Module` / `.Val` / `.Type`.
- `typeToSemType` for `NamedType` against an explicit
  primitive-name set (`int`, `string`, `bool`, `unit`, `float`,
  `byte`, `sbyte`, `int16/32/64`, `uint*`, `nativeint`, `unativeint`,
  `float32`, `decimal`, `char`, `obj`, `objnull`, `voidptr`, `exn`)
  and `FunctionType`.
- Compiled-name assembly from `<namespace>.<module-path>.<ident>`.
- Visibility filter (skip `internal` / `private`).

Followup decisions captured here that were not in the original plan:
- The compiled-name assembly is built from a path expressed
  **innermost-last** in source order but **innermost-first** in the
  walker's `path: string list`. The walker reverses on emission so the
  rendered name reads left-to-right.
- `parseFile` now returns just the AST (back-compat for the existing
  tests); a sibling `parseFileFull` returns the `Lexed` + input string
  too so the extractor can resolve token text without re-parsing.

### Phase 2 — Polymorphic vals in one module  *(Done)*

Target: `Common/option.fsi`'s `Option.map`, `Option.bind`,
`Option.isSome`, etc. Plus `Common/result.fsi`'s `Result.map`,
`Common/list.fsi`'s `List.map`, and friends for fan-out testing.

Delivered:
- An `order-preserving `TyparCollector` that issues a fresh index per
  typar name on first sight. Explicit `<'T, 'U>` typars are registered
  before the body walk so they keep declared positions; implicit
  body-only typars pick up indices in source order.
- A *closure-builder* shape for templates rather than the "SemType
  with named TyVar prototypes" sketched in §B.4: translation produces
  a `SemBuilder = SemType[] -> SemType` that closes over the typar
  indices. `Instantiate level` allocates the fresh-TyVar array (each
  stamped at `level`) and threads it through the closure. Two calls
  thus mint disjoint `TypeVar` objects without a substitution pass.
- `VarType (Named _)` and `VarType (Static _)` both route through the
  collector. `VarType Anon _` synthesises a uniquifying name
  (`_anon<n>`) so two anonymous typars in the same signature don't
  collide.
- `SuffixedType` → `TyRecord(name, [base])`.
- `GenericType` → `TyRecord(name, args)`.
- `TupleType` and `StructTupleType` → `TyTuple`.
- `ArrayType` → `TyRecord("array", [base])` for rank 1 and
  `TyRecord("array<n>", [base])` for higher ranks.
- `[<CompiledName("Foo")>]` parses through `Type.NamedType` for the
  attribute name. The argument expression lands as
  `Expr.EnclosedBlock(Paren, Expr.String(_, parts, _), _)`, not
  `Expr.Const(Constant.Literal _)` as one might guess — the string
  parts are concatenated to recover the source text.
- `[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]`
  is detected by scanning the construction expression for a
  `LongIdent` ending in `ModuleSuffix`. Matched modules emit their
  innermost path segment with `"Module"` appended.

Followup decisions captured here that were not in the original plan:
- **Qualified, not short, type names in the emitted SemType.** The
  plan sketched `TyRecord("Option", ...)`, but every reference inside
  `.fsi` files that we extract goes through a fully-qualified path
  (`Microsoft.FSharp.Core.Option`), and `ctx.Types` stores the
  qualified compiled name. The resolver returns the qualified form
  verbatim, so `Option.map`'s extracted type is
  `TyFun(TyFun(α, β), TyFun(TyRecord("Microsoft.FSharp.Core.option",
  [α]), TyRecord("Microsoft.FSharp.Core.option", [β])))`. Phase 5
  may need to align this with whatever the Unification pass uses
  internally; the cheap fix is to canonicalise on lookup.
- **Operator-token → compiled-name table.** Mirrors
  `Passes/Desugar.fs:infixOpName` but adds the unparenthesised
  variants (`|>`, `>>`, `@`, `^`, `..`, `?`, etc.). Active-pattern
  ident-or-ops are deferred (Phase 4-ish).
- **`do ObjectConstruction.init ()` in `VesperLib`** is required.
  The init lives behind `ImplementationFile.pNamedModule`, which a
  pure-signature parse path never touches. Without forcing the init,
  attribute parsing fails on the first val in the first file with
  *RefParser was not initialized*.

Test surface: `Microsoft.FSharp.Core.OptionModule.Map`,
`Microsoft.FSharp.Core.ResultModule.Map`, `ListModule.Map`,
`ArrayModule.Map`, `OptionModule.IsSome` with level-stamping check.

### Phase 3 — Cross-bucket resolution  *(Done)*

Delivered:
- `ctx.Types` registration for every `TypeSignature` variant
  (`Abbrev` / `Record` / `Union` / `Anon` / `Class` / `Struct` /
  `Interface` / `Enum` / `Delegate` / `TypeExtension` / `AbstractType`)
  capturing `(arity, qualifiedCompiledName)` keyed by short name.
  First declaration wins on conflict.
- A resolver (`resolveTypeName`) that:
  1. Tries the qualified name verbatim against `ctx.QualifiedTypes`
     (new HashSet of all registered qualified names);
  2. Falls back to short-name lookup in `ctx.Types`;
  3. Prepends each per-file open prefix and re-checks the qualified-
     name set;
  4. Returns `Error` (caller skips val and records a per-val entry in
     `ctx.Skipped`) when none of the above resolve.
- **Per-file `open`-clause tracking.** `collectOpens` walks each
  module / namespace body once and accumulates `open Foo.Bar` prefixes
  newest-first. The walker threads them down into nested modules and
  resolves against them at every `LongIdent` reference.
- **Skip-with-diagnostic on unresolved `LongIdent`.** Per-val
  resolution failures land in `ctx.Skipped`, separate from `ctx.Diagnostics`
  (which stays reserved for file-level parse failures). The
  `buildProvider` return tuple keeps the existing shape — only parse
  failures bubble up to callers; per-val skips are introspectable but
  don't pollute the headline error count.
- **End-to-end chain-provider fixture.** `VesperLib.chain` composes
  the lib provider in front of `MockBuiltins`. A test types
  `let x = 1 + 2 : int` through the chained provider to demonstrate
  both surfaces still answer for their respective name spaces.

Phase 5b will swap `MockBuiltins.provider` for `VesperLib.buildProvider`
in `Pipeline.fs` outright; the chained fixture is the bridge between
"Phase 3 ready" and "Phase 5 cut over."

### Phase 4 — Type declarations  *(Done)*

Delivered:
- `IExternalSymbolProvider.TryLookupType` added alongside `TryLookup`,
  returning `ExternalTypeShape voption` keyed by qualified compiled
  name. `MockBuiltins`, `nullProvider`, and `VesperLib.chain` all
  implement the new method (the first two as `ValueNone`, `chain`
  as primary-then-fallback).
- `ExternalTypeShape` is a DU over the three v1 shapes:
  - `Abbrev(arity, body: SemType[] -> SemType)` — closure-builder
    over the declared typars; consumers expand at use sites.
  - `Record(arity, fields: ExternalFieldShape[])` — per-field name,
    mutability, and closure-builder.
  - `Union(arity, cases: ExternalCaseShape[])` — per-case name,
    per-field labels (`ValueNone` for positional), and per-field
    closure-builders.
- `extractAbbrevBody` / `extractRecordBody` / `extractUnionBody`
  walk each `TypeSignature` and emit a shape into `ctx.TypeShapes`.
  GADT cases and any body that references typars not declared in
  the type's prefix / `TyparDefns` skip body extraction (the
  short-name registration still lands so other types can reference
  the type nominally).
- Tests:
  - `Microsoft.FSharp.Core.option` extracts as `Abbrev(1, …)` whose
    body expands to `TyRecord("…Option", [arg])`.
  - `Microsoft.FSharp.Core.Result` extracts as `Union(2, …)` with
    `Ok` and `Error` cases.
  - `TryLookupType` returns `ValueNone` for unknown names.

Classes, interfaces, enums, and delegates still register only as
name + arity in `ctx.Types`. Body extraction for those shapes is a
follow-up — they need the `ExternalTypeShape` DU to grow new arms
and the member-lookup side-tables in `PassContext` to consume them.

### Phase 5a — SRTP / trait constraint capture  *(Done)*

Delivered:
- `ExternalConstraint` DU on `ExternalSymbol.Constraints` (new
  field):
  - `Trait of typarIndex * SemanticConstraintKind` for
    `when 'T : equality` / `comparison` / `struct` / `not struct` /
    `null` / `not null`. Applied directly to the fresh `TypeVar`
    in the `Instantiate` closure — the existing unifier honours
    these via `SemanticConstraintKind`, so trait-constrained
    external vals fully type-check today.
  - `MemberTrait of typarIndices` — opaque marker for
    `when ^T : (static member ...)`. Stored so Phase 5b's SRTP
    unifier can address the participating typars without
    re-walking the source.
  - `Default of typarIndex` — opaque marker for `default ^T : <type>`.
    (Phase 5b.1 promoted this to `Default of typarIndex * (SemType[] -> SemType)`,
    carrying the resolved target as a SemBuilder over the symbol's typars.)
- `RawConstraint` / `ConstraintCollector` internal types in
  `VesperLib.fs` thread the capture through `translateType`. The
  `WhenConstrainedType` arm now drains its `TyparConstraints` into
  the collector instead of dropping them.
- `ExternalSymbols.polyWith` helper for in-code construction of
  constraint-bearing symbols (handy for tests and future
  per-target dispatch shims).
- Tests:
  - `Seq.Contains`'s `when 'T : equality` lands on the symbol's
    `Constraints` list AND on the fresh `TypeVar` minted at
    instantiation.
  - `Seq.Sum`'s SRTP clause (if extracted under the v1 translator)
    surfaces a `MemberTrait` entry; the test is permissive about
    the val itself failing to extract, since the SRTP signature
    stresses arms the v1 translator doesn't yet model.

### Phase 5b.1 — Default-constraint defaulting at generalisation  *(Done)*

Delivered:

- `ExternalConstraint.Default` promoted from opaque marker to
  `Default of typarIndex * (SemType[] -> SemType)` carrying the
  default target as a `SemBuilder` over the symbol's typar array. The
  RawConstraint capture retains the raw `Type<SyntaxToken>` until
  `resolveConstraints` runs after the body walk, so target typar
  references resolve through the same index scheme the val signature
  uses.
- New `TypeVar.Defaults: SemType list` field. The `Instantiate`
  closure of an external symbol stamps each default's resolved target
  onto the source TyVar's `Defaults` list in declaration order;
  `migrateBounds` migrates the list on union-find collapse so the
  representative carries it.
- `Unification.applyDefaults` runs at the head of `generalise`: walks
  free TyVars in the just-typed binding, and for each one whose
  `Defaults` list is non-empty, chases each target through union-find
  Links until a concrete shape (`TyConst`/`TyFun`/`TyTuple`/…) is
  reachable. Links the source TyVar to that shape and clears its
  `Defaults`. Iterates to fixpoint — chained defaults like
  `default ^T3 : ^T1 ; default ^T1 : int` close in two passes.
- `VesperLib.toProvider` auto-open prefix table. Short-name lookups
  (`op_Addition`, `op_PipeRight`, …) fall through to
  `Microsoft.FSharp.Core.Operators` + a handful of sibling prefixes,
  so call sites (`inferInfix` in Unification.fs) don't need to know
  which module owns which symbol. Matches F#'s open-Pervasives
  behaviour for the prelude.
- Test gate: `let x = 1 + 2` types as `int` through a **lib-only**
  provider (no `MockBuiltins` in the chain). The lib's universal SRTP
  `(+)` signature `^T1 -> ^T2 -> ^T3` carries the default chain;
  generalisation defaults `^T1 ← int` first (via `default ^T1 : int`),
  then `^T3 ← ^T1 = int` on the second pass.

### Phase 5b.2 — SRTP member-trait unifier + production swap  *(Done)*

Delivered:

- **`MemberSignature` promotion.** `SemanticInfo.fs` flips
  `MemberSignature` from the `MemberSignaturePlaceholder` stub to a
  real struct: `{ MemberName; ArgTypes; ReturnType; Resolved: bool ref
  }`. The shared `Resolved` ref dedupes dispatch when multiple
  participating typars resolve in sequence — whichever fires first
  flips it; the others see the flag and skip.
- **`ExternalConstraint.MemberTrait` payload growth.** Promoted from
  `MemberTrait of typarIndices` to
  `MemberTrait of typarIndices * memberName * argBuilders * retBuilder`.
  The builders are `SemBuilder`-shaped (closures over the symbol's
  declared typar array); `Instantiate` evaluates them against the
  fresh-TyVar array per call site.
- **Extraction.** `captureConstraints` in `VesperLib.fs` now drains
  the `Constraint.MemberTrait`'s `memberSig`: extracts the compiled
  member name through `identOrOpName` (so operator-style members like
  `(+)` land as `op_Addition`), and walks the `CurriedSig`'s
  `ArgsSpec` to collect the tupled arg types plus return type as raw
  `Type<SyntaxToken>`s. `resolveConstraints` then translates each arg
  / return through the same `translateType` pipeline the val
  signature uses, so the trait's typar references resolve through the
  symbol's typar-index scheme. Failed translation drops the whole
  member-trait entry (better to under-stamp than mis-stamp).
- **Instantiation stamping.** `extractValSig`'s `Instantiate` closure
  builds a single `MemberSignature` per captured trait (one shared
  `Resolved` ref), then prepends it to every participating fresh
  TyVar's `SrtpBounds`. `migrateBounds` (already in place) keeps the
  list intact through union-find collapse.
- **`Unification.drainSrtpBounds`.** New on-unified callback that
  mirrors `drainConstraints`: fires from each of the four sites in
  `unify` where a TyVar's `Link` is set. Dispatches against:
  - **Built-in numeric primitives.** A small in-pass table
    (`numericPrimitives` × `arithmeticBinaryOps` / `comparisonBinaryOps`
    / `op_UnaryNegation`) synthesises the candidate static-member
    type — e.g. `int.op_Addition : int * int -> int`. This makes
    `1 + 2`-style traits resolve directly through the unifier rather
    than waiting on defaulting.
  - **`ctx.ClassTypes` static members.** For `TyClass(name, args)`,
    looks up the class, finds the static member matching
    `bound.MemberName`, substitutes the class's typars through the
    receiver's args, and unifies trait sig against member sig.
  - **Deferred TyVar / unknown class.** Bound stays on the root for
    the next `Link` event.

  Tupled and curried candidate shapes are both accepted: the drain
  builds the expected trait sig as `TyFun(TyTuple [...], ret)`
  primarily, falling through to a curried decomposition when the
  candidate's outer shape isn't tupled.
- **Production-path helper.** `VesperLib.defaultProvider libRoot`
  caches the parsed lib once per `libRoot` (keyed by normalised
  absolute path) via a `ConcurrentDictionary<string, Lazy<_>>` so
  production callers can request the provider without managing the
  lifecycle. Tests retain `VesperLib.buildProvider` for fresh-build
  scenarios.

`Pipeline.fs` still accepts the provider as a parameter — no
production caller exists in this repo yet, but `defaultProvider`
is the documented call site to wire in when one lands.

**Explicitly not in scope (kept as future work):**

- Deleting `MockBuiltins`. Stays as a test fixture and minimal
  provider example.
- Multi-trait `or`-list dispatch where the participating typars
  resolve to *different* concrete types (e.g. `^T1 = int`, `^T2 =
  int64`). The v1 drain picks whichever typar resolves first and
  dispatches against that side's members; the user typically wouldn't
  write such code outside FSharp.Core internals.
- Arity disagreement between the trait's declared member sig and the
  candidate's actual member type beyond unify's structural check.
  Diagnostics surface through normal unify path.

Test gate (passes):
- `Phase 5b.2: V + V dispatches via SRTP member-trait` — `V` with
  `static member (+) (a: V, b: V) = V()`, `let r = V() + V()` types
  as `TyClass "V"` through a lib-only provider (no MockBuiltins).
- `defaultProvider caches the parsed lib across calls` — two calls
  for the same `libRoot` return the same provider object.

**Follow-up, not in this plan**: the multi-target re-cut. The
[fsi-target-brainstorm](fsi-target-brainstorm.md) sketches the SRTP-
signature + per-target `.fs` dispatch shape; the refinement this plan
commits to is to *curate* the `.fsi` surface itself — break the
current literal-FSharp.Core transliteration into a menu of opt-in
capability modules (`core-types.fsi` minimum floor, `int64.fsi`,
`decimal.fsi`, `array.fsi`, `simd.fsi`, `nativeptr.fsi` with phantom-
typed address spaces, …), move inline-IL dispatch into per-target
`Core.<Target>.fs` arms keyed off intrinsic strings, and ship
`Core.JS.fsproj` / `Core.WASM.fsproj` siblings to `Core.CLR.fsproj`.
The extractor needs no change for this — each target's `.fsproj`
references a different subset of `.fsi` files and points
`buildProvider` at its own lib root.

## Open questions

- **Array types**: F# has `'T[]`, `'T[,]`, `'T[,,]` and the `array<'T>`
  alias. `SemType` doesn't currently have a dedicated array variant.
  Workable to model as `TyRecord ("Array1", [t])` / `("Array2", [t])` for
  v1 and add a proper variant later. See `prim-types-prelude.fsi` lines
  149+ for upstream's rank-by-rank declarations. Once the multi-target
  split lands, array indexing operations become per-target intrinsics
  (CLR native arrays vs JS `TypedArray` vs WASM linear memory), but the
  array *types* stay universal.
- **Static-optimisation clauses** in the current `Clr/prim-types.fsi`
  (the `(+)`/`(<)` family): these encode multiple signatures gated on
  `'T = int` / `'T = float` / etc. and are a transliteration of upstream
  FSharp.Core. Phase 5's SRTP machinery uses the unconstrained
  signature with the recorded `when … : (static member …)` and
  `default` clauses, which is the same information re-expressed.
  Independently, the [target brainstorm](fsi-target-brainstorm.md)
  migrates these declarations into per-target `.fs` files entirely —
  the .fsi keeps only the SRTP signature, and the dispatch arms move
  to `Core.[Target].fs`. Extraction is unaffected by that migration
  because it operates on the .fsi.
- **`PlatformInt` and target-specific primitives**: under the curated-
  menu model, each opt-in primitive (`int53`, `int64`, `decimal`,
  `bfloat16`, …) lives in its own `.fsi`. A target's `.fsproj`
  references whichever ones it can implement. `PlatformInt` is just
  one more entry in that menu — `platform-int-clr.fsi` defines
  `type PlatformInt = nativeint`, `platform-int-js.fsi` defines
  `type PlatformInt = int32`, and the target picks one. The extractor
  treats them all as ordinary `type` declarations through Phase 4. A
  val referencing a primitive whose `.fsi` isn't in the loaded set
  fails resolution against `ctx.Types` — which is the right user-
  facing error.
- **Anonymous typars (`_`)** in signatures: rare in F# `.fsi` proper,
  but they appear in higher-rank positions. Treat as fresh `TyVar` per
  occurrence with no name-binding.
- **Cross-bucket name conflicts**: two buckets could in principle
  declare `Microsoft.FSharp.Core.X`. The walker should warn-and-take-first
  rather than error; user can resolve by adjusting bucket compile order.
  Under the curated model, this also covers a target manifest that
  loads both a "default" capability `.fsi` and a target-specific
  override that redeclares the same name — declared order in the
  manifest decides which wins.
- **Caching**: parsing the lib is slow (39 `.fsi` files). For v1, parse
  on every `buildProvider` call. If the type-checker is invoked often,
  cache the `ExtractCtx` keyed by `(libRoot, last-write-times)`.
- **`SI.fs` parse error.** The lib's one `.fs` file — measure-type
  definitions in `Common/SI.fs` — fails the implementation parser.
  `buildProvider` records it as a per-file error and the test suite
  whitelists `SI.fs` until measure-type support lands. Once that gap
  is closed (or `SI.fs` is split into a separate target manifest),
  the whitelist can come out.

## References

- Provider interface: `ExternalSymbols.fs:33` `IExternalSymbolProvider`.
- Provider helpers used by the existing `MockBuiltins`:
  `ExternalSymbols.fs:40` `mono`, `:49` `poly`.
- The hand-written reference: `ExternalSymbols.fs:60-148`
  `MockBuiltins`. This is what Phase 5 deletes.
- AST shapes to walk:
  - `Signatures.fs:10` `ValSig`
  - `Signatures.fs:74` `TypeSignature`
  - `Signatures.fs:113` `ModuleSignatureElement`
  - `Expr.fs:66` `Type<'T>` — including `WhenConstrainedType`.
  - `Expr.fs:104` `Typar<'T>` — `Named` (`'T`) vs `Statically` (`^T`).
- Manifest loading + parsing pipeline: `VesperLib.fs` `loadAll`,
  `parseFile`, `buildProvider`.
- Layout of the source tree being parsed:
  `src/XParsec.FSharp.Lib/compiler-clr-project.md`.
- The existing `Pipeline.fs` wiring of `MockBuiltins.provider` — the
  single line that changes in Phase 5.
- Multi-target architecture this plan is sized against:
  [fsi-target-brainstorm.md](fsi-target-brainstorm.md).
