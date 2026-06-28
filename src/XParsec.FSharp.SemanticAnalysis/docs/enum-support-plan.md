# Enum support — design doc (numeric + string enums)

**Status:** design, **not started** — written for review before any code lands.
**Scope:** elaboration + freeze + both backends for `TypeDefn.Enum`, plus the
`TsManifestProvider` mapping that consumes it. **Premise to confirm before
implementing** is in the last section — do not start coding until it is signed off.

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
| **Name resolution** | ✅ registered (`NameResolution.fs:487`) | ✅ same path |
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
constant literal. Two variants, classified by the **common type of the case
values** (all cases must agree — a mixed enum is a semantic error):

1. **Numeric enum** — all `vi` are integer literals. This is the F#/CLR-standard
   enum: `E` has an integral underlying type, cases are named constants, a value
   is one of the underlying integers. Backend-faithful on the CLR as a real
   `System.Enum`.
2. **String enum** — all `vi` are string literals. `E` is a **closed, named set
   of string-literal singletons**: a value of `E` *is* one of the listed strings,
   reachable by name (`E.C1`). This is the Vesper extension (no CLR `System.Enum`
   analog) and is exactly TS string-enum semantics.

The classification and the case→literal binding are **backend-agnostic** (they
live in elaborate/freeze); the *representation* is a backend concern (per the
freeze / no-backend-knowledge rule). The front end must not bake either repr.

Open extension (NOT in v1, note for the grammar's generality): char enums are
CLR-legal (`char` is an integral enum base); leave the door open but don't build
it. Reject any non-integer / non-string literal (and any non-literal expression)
in v1 with a loud semantic error.

## Representation per backend

### Numeric enum
- **CLR:** a genuine `System.Enum` subclass with the integral underlying type and
  one `static literal` field per case. Standard, cheap.
- **JS:** an object map `{ C1: v1, … }` frozen at module scope; a value is the
  number. (Mirrors what `tsc` emits for a numeric enum, minus the reverse map
  unless a use forces it.)

### String enum (user's chosen CLR repr)
- **CLR:** a **`[<Struct>]` wrapper over `string`** — a single-field value type
  holding the case string. The **allowed cases are `static` const-like fields**
  on the struct (one per case, each returning the wrapper around its literal),
  i.e. the case set is carried as **IL const/`static readonly` fields**, and the
  **case metadata** (name ↔ string) is recorded at **semantic analysis** so
  freeze/codegen and pattern-matching can see the closed set. Leans on the
  existing `[<Struct>]` value-type codegen ([`project_struct_codegen`]) — unboxed
  by-address dispatch, no new mechanism. Equality is string equality on the field.
- **JS:** an object map `{ C1: "v1", … }` — identical shape to the numeric case,
  values are strings. A value is the string; `E.C1` is `obj.C1`.

Same concept, two reprs → the classification belongs in the shared front end, the
reprs in each backend (the established split).

## Front-end work (greenfield)

Sequenced; each is a reviewable increment.

1. **TAST node + elaboration.** Add an enum type-declaration representation
   (parallel to how records/unions are elaborated in `Elaborate.fs`): resolve the
   case literals, classify numeric vs string, reject mixed/illegal, record the
   ordered case→literal table. Promote `Conformance.fs`'s `ImplShape.Other
   "enum"` to a real enum impl shape.
2. **Freeze / type identity.** Give `E` a `FrozenType` nominal identity carrying
   its variant (numeric/string) + underlying repr tag + the case table, so
   consumers (codegen, pattern match, the provider) read one source of truth.
   Respect the `SemType`→`FrozenType` split (case table is immutable, frozen).
3. **Member/value access.** `E.C1` resolves to the case's value. Decide whether
   cases are static members on the enum type (consistent with member resolution)
   — likely yes, mirroring CLR enum field access.
4. **Pattern matching (scope check).** Decide v1 coverage: at minimum equality
   against `E.Ci`; full `match` exhaustiveness over a closed string enum is
   desirable but can be a follow-up — flag explicitly.
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
- The extractor already carries `(memberName, valueOpt)` with string-vs-numeric
  distinguishable by the value form (the producer stringifies numerics; a future
  refinement could tag the variant explicitly on the wire if disambiguation by
  value form proves fragile — call that out if it bites).
- This needs a real `ExternalTypeShape` for enums (or a faithful reuse of an
  existing nominal shape carrying the case table). Determine in step 2 whether the
  frozen enum identity is expressible through the existing seam or needs a new
  `ExternalTypeShape` arm — if the latter, that is a **seam contract addition**,
  coordinate like any seam change.

## Premise to confirm before coding

1. **Semantic model** — string enum = *closed named set of string-literal
   singletons*; numeric enum = CLR-standard integral enum; mixed/illegal rejected.
   Agreed?
2. **String-enum CLR repr** — `[<Struct>]` wrapper over `string`, case set as
   static const-like IL fields, case metadata recorded at semantic analysis.
   (User-proposed; confirm the struct-wrapper over a DU/sealed-class is the
   intended direction.)
3. **JS repr** — frozen object map for both variants. Agreed?
4. **v1 pattern-matching scope** — equality against named cases now,
   exhaustiveness as a possible follow-up? Or is closed-match exhaustiveness
   required in v1?
5. **Seam** — acceptable to add an `ExternalTypeShape` enum arm if the existing
   nominal shapes can't carry the case table faithfully?

## Sequencing vs the TS-extractor slice

Independent of the remaining extractor tiers (heritage/namespace/multi-file,
generics, free-fn overloads, the member-storage seam axis). Enum *extraction* is
already done; only the *consumption* arm waits on this. Recommend: land the front
end + backends first (steps 1–6), then revisit the provider arm (it's a small
remap once the enum type exists).
