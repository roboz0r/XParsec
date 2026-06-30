# TS extraction — residuals note

**Status:** the original `.d.ts` → manifest *producer* plan is **retired** — almost
all of it has landed. This file is now just a pointer to what's left, so nobody
re-scopes finished work. Ephemeral per the repo convention; delete it once the
residuals below close.

The producer (`Vesper.Ts.Extractor/Extractor.fs`), schema
(`Vesper.Ts.Manifest.Schema/Schema.fs`), and consumer
(`XParsec.FSharp.Codegen.Js/TsManifestProvider.fs`) now cover, end-to-end:
`Interface` / `Function` / `Class` (incl. static side + construct sigs) /
`Variable` / `Enum` (type **and** members) / `TypeAlias` / `Namespace`; `ImportShape`
detection (`Default`/`CommonJsExport`/`Namespace`/`Named`); optional & rest params;
heritage (`extends`/`implements`); `any → Dynamic`; per-member overload **emission**;
generics typar count; and the multi-file package closure (`extractPackage`).
Overload identity already interns real `argSig` for methods and ctors. The big
status table and Tier 0–4 ordering that used to live here described a much earlier
MVP and were wrong against current code — don't resurrect them.

## What actually remains

> **Failure-contract shift (DECIDED — see
> [`codegen-js-symbol-provider-plan.md`](codegen-js-symbol-provider-plan.md)
> *"Failure contract: resilient extraction with diagnostics"*).** The per-type
> `failwith`s called out below are slated to become **per-symbol diagnostics** that
> *degrade* (e.g. method-axis typar → erased to `obj` + warning) instead of aborting
> the package, with a `diagnostics` channel carried inside the manifest. The bar:
> *if a type can be named, degrade it; omit only the un-nameable.* Only the
> I/O/resolution plumbing throws (`not a module`, file/specifier unresolvable) stay
> fatal. The residuals below therefore describe the *current* loud-throw behavior and
> the work still owed for the *faithful* (non-degraded) form.

- ~~**Consumer member-overload resolution.**~~ **DONE.** Both halves landed.
  *Members* (9a): `expandMethod` mints one keyed `ExternalMember` per call signature,
  `TryLookupMembers` returns the full set, and `UnificationInferOverload.pickBestOverload`
  selects by arity + argument type — proven end-to-end by `MemberOverloadTests` (JS) on
  top of the `specs/overloads/` extractor fixture. *Free functions* (9b): N>1 free
  functions route into a synthetic per-module erased grouping type that erases to the
  bare export at JS emit (`MemberOverloadTests`/`FreeFnOverloadTests`; rationale in the
  `TsManifestProvider` synthesis block + the `EmitJs` erase branch). `singleSignature`
  now only guards malformed single-export manifests (N>1 functions never reach it).
- ~~**Enum member values.**~~ **DONE.** Enum support is essentially complete
  front-to-back (a nominal `TyEnum`/`FTEnum` type, numeric/string/mixed elaboration,
  both backends, and the wire/provider arm). The provider now maps `Export.Enum` to a
  real `ExternalTypeShape.Enum` carrying the type-tagged case table (wire `EnumValue =
  IntVal | StringVal`, SchemaVersion 1), and a TS enum's `E.C1` resolves + emits an
  imported member access in JS. The old `enum-support-plan.md` (which owned this) has
  been deleted per the ephemeral-doc convention; durable decisions live in the module
  headers + the `project_enum_support` memory. Residual follow-ups (not blocking this
  slice): closed-enum match exhaustiveness, and the CLR string/mixed wrapper's
  private-field+property encapsulation (currently public static fields — see
  `Codegen.Clr/Layout.fs` struct-enum emission).
- **Structural object types → content hash.** Provider resolves
  `TypeRef.Structural` to the stub `FTUnknown "structural:<hash>"`. Gated on SCC
  cycle-canonicalisation; defer until a fixture demands it.
- **Method-axis typars.** Generic *count* is emitted, but the producer still
  `failwith`s on a method-axis type-parameter reference (`Extractor.fs`, the typar
  arm). Revisit with the rest of generics fidelity.
- **Asymmetric accessor get/set types (TS 4.3).** Producer `failwith`s — kept loud
  deliberately; no backend analog, so pay for it only when a fixture forces it.
- **Merged class + namespace declarations.** When a `class Foo` and a `namespace
  Foo` share a name (a common real-world `.d.ts` idiom — statics-as-namespace), the
  producer keeps the class half and **drops the namespace half for v1**
  (`Extractor.fs`, the merged-declaration arm). Revisit when a target API leans on it
  (the DOM and `@types/node` both do).

## The member storage axis — LANDED (CLR), JS-readonly deferred

The **member storage axis** (`IsProperty: bool` → three-way
`MemberStorage = Field | Property | Method`) has landed for the seam and the **CLR
backend** — genuine public fields (`String.Empty`, `ValueTuple.Item1`) now resolve
(`MetadataSymbols` `GetFields` walk) and emit `ldfld`/`ldsfld`. See
[`member-storage-axis-plan.md`](member-storage-axis-plan.md) for what landed and the
one remaining piece: the **JS `readonly` fidelity** half (the `Schema.MemberKind.Field`
wire case + `Codec`/extractor classification + `SchemaVersion` bump), deferred because
JS field-vs-property has no emission consequence — pay it when a `readonly` fixture
wants the distinction.
