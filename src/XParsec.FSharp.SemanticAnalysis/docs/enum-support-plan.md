# Enum support — design doc (numeric + string enums)

**Status:** design **signed off, not yet started** — all five premises in the last
section are resolved (see their *Resolved* / *Confirmed* tags); ready to implement.
**Scope:** elaboration + freeze + both backends for `TypeDefn.Enum`, plus the
`TsManifestProvider` mapping that consumes it.

This is ephemeral per the repo convention: delete it once enum support lands,
folding any durable decisions into the relevant module headers / memory.

## Why this exists

The TS-extractor slice ([`ts-extraction-plan.md`](ts-extraction-plan.md), item 5)
can already *extract* a TS `enum` into the manifest (member names + constant
values, string vs numeric). But the consumer (`TsManifestProvider`) had nothing
to map it onto: it resolves the enum **type name** as `ExternalTypeShape.Opaque`
and **drops the members** behind a TODO, because **Vesper has no enum type at
all** below the parser. This doc designs that missing type. Closing it unblocks
the extractor's one remaining stubbed arm.

The forcing realisation (user): a string enum can reuse F#'s existing enum
*syntax* — `type E = | A = <lit>` — with a string literal in the value position.
That is the entry point; the work is semantic + codegen, not grammar.

## Current state (grounded)

| Layer | Numeric enum | String enum |
|---|---|---|
| **Lexer/parser** | ✅ `TypeDefn.Enum`, cases `EnumTypeCase(ident, equals, constValue: Expr)` | ✅ **already** — `constValue` is a general `Expr`, so `\| A = "x"` parses; no grammar change |
| **Name resolution** | ✅ registered (`NameResolution.fs:502`) | ✅ same path |
| **Conformance** | ⚠️ `ImplShape.Other "enum"` (`Conformance.fs:194`) — recognised, not modelled | ⚠️ same |
| **Elaborate / TAST** | ❌ no enum type node, no case elaboration | ❌ |
| **Freeze / type identity** | ❌ no `FrozenType` enum identity | ❌ |
| **CLR codegen** | ❌ | ❌ |
| **JS codegen** | ❌ | ❌ |
| **Provider consumption (TS→Vesper)** | ⚠️ `Opaque` name only, members stubbed | ⚠️ same |

**Key point:** enum *type declarations* are **greenfield below the parser for
BOTH numeric and string** — there is no numeric-enum implementation to "extend."
So this doc designs enum support once, with string enums as a first-class
variant, not a bolt-on. The grammar already being permissive (general `Expr`
value) is exactly the repo's "general grammar, reject semantics later" pattern:
the *semantic* layer is where numeric-vs-string is classified and where an
illegal value (e.g. a non-literal expression) is rejected.

## Semantic model (the premise to confirm)

An enum declaration `type E = | C1 = v1 | … | Cn = vn` introduces a **nominal
type `E` inhabited by exactly the named cases**, each case bound to a compile-time
constant literal. The variant is classified by the **types of the case values**:

1. **Numeric enum** — all `vi` are integer literals. This is the F#/CLR-standard
   enum: `E` has an integral underlying type, cases are named constants, a value
   is one of the underlying integers. Backend-faithful on the CLR as a real
   `System.Enum`.
2. **String enum** — all `vi` are string literals. `E` is a **closed, named set
   of string-literal singletons**: a value of `E` *is* one of the listed strings,
   reachable by name (`E.C1`). This is the Vesper extension (no CLR `System.Enum`
   analog) and is exactly TS string-enum semantics.
3. **Mixed enum** — case values are a mix of integer and string literals. **Not a
   hard error** (heterogeneous enums are legal, if discouraged, TS, and the grammar
   admits them for authored Vesper too): emit a **warning** and fall back to a
   uniform repr — a `[<Struct>]` wrapper over `obj` carrying the boxed int or
   string, plus the case metadata. The all-numeric and all-string cases keep their
   efficient reprs (1, 2); only genuine mixing pays the `obj`-box. This also gives
   the TS-import path a graceful landing for heterogeneous enums rather than a
   throw.

The classification and the case→literal binding are **backend-agnostic** (they
live in elaborate/freeze); the *representation* is a backend concern (per the
freeze / no-backend-knowledge rule). The front end must not bake any repr.

Open extension (NOT in v1, note for the grammar's generality): char enums are
CLR-legal (`char` is an integral enum base); leave the door open but don't build
it. Reject any non-integer / non-string literal (and any non-literal expression)
in v1 with a loud semantic error — mixing int+string is the *only* heterogeneity
admitted (as a warning); everything else is still rejected.

## Representation per backend

### Numeric enum
- **CLR:** a genuine `System.Enum` subclass with the integral underlying type and
  one `static literal` field per case. Standard, cheap.
- **JS:** an object map `{ C1: v1, … }` frozen at module scope; a value is the
  number. (Mirrors what `tsc` emits for a numeric enum, minus the reverse map
  unless a use forces it.)

### String enum (user's chosen CLR repr)
- **CLR:** a **`[<Struct>]` wrapper over `string`** — a single-field value type
  holding the case string. A struct-typed field **cannot** be an IL `literal`
  (only the underlying primitive/string can), so each case is a **`static
  initonly` (private) field initialized in the type's `.cctor`** by calling the
  struct ctor on its string literal, surfaced through a **public get-only
  property** per case (the .NET idiom, and what keeps the case set closed —
  construction is not part of the public API). The **case metadata** (name ↔
  string) is recorded at **semantic analysis** so freeze/codegen and
  pattern-matching can see the closed set. Leans on the existing `[<Struct>]`
  value-type codegen ([`project_struct_codegen`]) — unboxed by-address dispatch,
  no new mechanism. Equality is string equality on the field. **Sequencing check:**
  confirm static-initonly-struct-field + `.cctor`-init actually emits before
  committing — `project_struct_codegen` lists *instance* `val` InitOnly as
  deferred; the static-readonly flavour here is closer to the landed
  module-values-as-static-fields path, but verify it rather than assume.
- **JS:** an object map `{ C1: "v1", … }` — identical shape to the numeric case,
  values are strings. A value is the string; `E.C1` is `obj.C1`.

### Mixed enum (warning fallback)
- **CLR:** the same struct-wrapper machinery as the string case, but the field is
  `obj` (boxed int or string per case) instead of `string`; the per-case static
  initonly field / get-only property and the recorded metadata are unchanged.
  Equality is `obj` structural equality on the field.
- **JS:** an object map `{ C1: v1, … }` — JS is untyped, so a mix of numbers and
  strings is the same object-map shape; no special handling.

Same concept, three CLR reprs (one JS) → the classification belongs in the shared
front end, the reprs in each backend (the established split).

## Front-end work (greenfield)

Sequenced; each is a reviewable increment.

1. **TAST node + elaboration.** Add an enum type-declaration representation
   (parallel to how records/unions are elaborated in `Elaborate.fs`): resolve the
   case literals, classify numeric / string / mixed (warn on mixed; reject
   non-literal or non-int-non-string), record the ordered case→literal table.
   Promote `Conformance.fs`'s `ImplShape.Other
   "enum"` to a real enum impl shape.
2. **Freeze / type identity.** Give `E` a `FrozenType` nominal identity carrying
   its variant (numeric / string / mixed) + the underlying repr tag (the integral
   `IntWidth` for numeric; `string` / `obj` for the others) + the case table, so
   consumers (codegen, pattern match, the provider) read one source of truth.
   Respect the `SemType`→`FrozenType` split (case table is immutable, frozen).
   **Resolved fork (the type *reference*):** an enum is modeled as a **nominal**
   `SemType.TyEnum key` / `FrozenType.FTEnum key` arm — an additive DU case
   carrying just the `SymbolKey` (enums aren't generic, so no args), exactly
   parallel to `TyUnion`/`FTUnion`. The case table is looked up off the frozen
   `TDecl` node by key (it already rides the node, like union cases — no new
   carrier). This keeps `E` a distinct nominal type (NOT structurally `int`), which
   is what the semantic model and faithful `System.Enum` emission require. The
   per-variant *representation* (numeric→`System.Enum`, string→struct-wrapper,
   mixed→`obj`-box, JS→object map) stays a backend decision read off the case
   table + `TEnumCases.classify` — the front end bakes no repr. (Rejected: erasing
   a numeric enum's reference to `FTConst "int"`; it makes `E == int` and blocks a
   genuine `System.Enum`.)
   **This is where width lives** — from the authored literal suffix for `.fs`
   enums, or the default (`I32`) when lifting a width-less TS-manifest numeric enum.
   Enforce the CLR uniform-width invariant here: a numeric enum's cases must all
   resolve to one underlying type (authored mismatches are an error; TS imports are
   uniform by construction since they all take the default).
3. **Member/value access.** `E.C1` resolves to the case's value. Decide whether
   cases are static members on the enum type (consistent with member resolution)
   — likely yes, mirroring CLR enum field access.
4. **Pattern matching.** **v1 = equality only** (`match`/`if` against `E.Ci`,
   lowered to equality on the underlying value). Closed-enum `match`
   **exhaustiveness** is a deliberate **follow-up**, not v1 — flag the non-exhaustive
   case as the usual incomplete-match warning until it lands. *(Resolved.)*
5. **CLR codegen.** Numeric → `System.Enum` emission; string → the struct wrapper
   + static case fields. Reuse struct codegen.
6. **JS codegen.** Both variants → the frozen object map; `E.Ci` → property access.

## Provider consumption (TS manifest → Vesper enum) — closes the stub

`TsManifestProvider` today: `Export.Enum(name, members)` → `ExternalTypeShape.Opaque
0`, members dropped. Once the enum *type* exists:
- A TS **numeric** enum → a Vesper numeric enum type shape (underlying integral),
  members = the constant table.
- A TS **string** enum → a Vesper string enum type shape, members = the
  name→string table.
- **Wire schema change (do this first — it's already fragile, not hypothetical).**
  The schema today is `Export.Enum of name * (string * string option) list`, and the
  producer **stringifies numerics** (`Extractor.fs:630`, `Some(string n)`) while
  keeping strings verbatim (`:629`). So a string member `A = "42"` and a numeric
  member `A = 42` **both serialize to `Some "42"`** — the variant is unrecoverable
  on the consumer, which is exactly what classification depends on. Tag the value
  with its type — but only carry **what TS source actually expresses**: TS enum
  members are `number` (no `byte`/`int16`/`uint64` distinction exists in the source)
  or `string`. So the wire stays minimal:

  ```
  EnumValue =
    | IntVal of int64       // a TS numeric member (integer subset; see below)
    | StringVal of string
  ```
  `members: (string * EnumValue option) list` (`None` = computed/unresolvable). The
  numeric/string/mixed variant falls out of the member values, and **mixed** lands
  on the warning-fallback repr instead of a lossy guess.

  **Integral *width* is NOT a wire concern.** A CLR numeric enum has an underlying
  integral type (`byte`/…/`uint64`/`char`), and faithful `System.Enum` emission
  needs it — but that lives in the **authored-`.fs` / `FrozenType` layer**, where a
  literal suffix (`1uy`, `1L`) actually names it, *not* on the TS wire, which has no
  such notion. When the consumer maps a TS numeric enum into a `FrozenType` it
  **assigns the underlying type** (default `I32`, the faithful TS→CLR choice) — a
  policy decision at the seam, not data read off the `.d.ts`. (Also restrict the
  wire to the **integer subset**: TS technically permits non-integer numeric enum
  members; the extractor should throw loudly on a non-integer enum literal rather
  than widen `IntVal` to a float — tractable-subset discipline, consistent with the
  producer's "throw, don't swallow" rule.)

  This is a `SchemaVersion` bump; coordinate it like any seam contract change.
- This needs a real `ExternalTypeShape` for enums (or a faithful reuse of an
  existing nominal shape carrying the case table). Determine in step 2 whether the
  frozen enum identity is expressible through the existing seam or needs a new
  `ExternalTypeShape` arm — if the latter, that is a **seam contract addition**,
  coordinate like any seam change.

## Premise to confirm before coding

1. **Semantic model** — string enum = *closed named set of string-literal
   singletons*; numeric enum = CLR-standard integral enum; **mixed int+string =
   warning + `obj`-wrapper fallback (not a hard error)**; all other non-literal /
   non-int-non-string values still rejected. *(Confirmed.)*
2. **String/mixed CLR repr** — `[<Struct>]` wrapper (`string` for pure-string,
   `obj` for mixed); cases as **private `static initonly` fields `.cctor`-init'd,
   exposed via public get-only properties**; case metadata recorded at semantic
   analysis. *(Confirmed direction; the open item is the emission **sequencing
   check** in "Representation per backend", not the design.)*
3. **JS repr** — frozen object map for all variants. *(Confirmed.)*
4. **v1 pattern-matching scope** — **equality against named cases in v1;
   closed-match exhaustiveness deferred to a follow-up** (non-exhaustive match stays
   a warning until then). *(Resolved.)*
5. **Wire + seam** — tag enum member values with their type on the manifest wire
   (`EnumValue = IntVal of int64 | StringVal`, `SchemaVersion` bump) so
   numeric/string/mixed is recoverable on the consumer; **integral width is not on
   the wire** — it is assigned in the `FrozenType` layer (authored suffix, or `I32`
   default for TS imports); add an `ExternalTypeShape` enum arm if the existing
   nominal shapes can't carry the case table faithfully. *(Wire shape + width-at-
   freeze agreed; the `ExternalTypeShape` question resolves in step 2.)*

## Sequencing vs the TS-extractor slice

Independent of the remaining extractor tiers (heritage/namespace/multi-file,
generics, free-fn overloads, the member-storage seam axis). Enum *extraction* is
already done; only the *consumption* arm waits on this. Recommend: land the front
end + backends first (steps 1–6), then revisit the provider arm (it's a small
remap once the enum type exists).
