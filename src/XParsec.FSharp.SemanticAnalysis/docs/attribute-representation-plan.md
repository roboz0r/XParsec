# Attributes are real types with a target representation

**Status (2026-08-15): new plan.** Its dependency — key-based attribute resolution in
contracts, from the `.fsi` front-end work — landed 2026-08-17 (step 3 below records the
outcome). Delete when this plan lands (`feedback_plan_docs_ephemeral`).

## The position

An attribute performs two jobs at the language level:

1. **Direct the compiler** to a different representation or lowering — `[<ReferenceEquality>]`,
   `[<Struct>]`, `[<Global>]`.
2. **Attach static metadata** to a type or member.

Both get an IL representation. Job 1's *output* representation is target-defined, but that
decision belongs at EMIT, not upstream: the attributes land in the FrozenTast exactly as
written, and each backend decides what to do with them. Job 2 additionally needs a
target-neutral reflection API before it means anything on a non-CLR target; none exists yet,
which is why nothing has forced the issue.

Putting "JS does not reify attributes" in the manifest or the conformance pass is a target
fact in the freeze layer (`feedback_freeze_no_backend_knowledge`). Emit-time is where it goes.

## The latent bug this exposes

`MetadataSymbols.fs:449-456`:

```fsharp
/// `[<AllowNullLiteral>]` is emitted into metadata and visible in reflection-only loads.
let hasAllowNullLiteral (t: Type) : bool =
    t.CustomAttributes |> Seq.exists (fun a ->
        match a.AttributeType.FullName with
        | "Microsoft.FSharp.Core.AllowNullLiteralAttribute" -> true
        | _ -> false)
```

Broken in both directions. It matches **FSharp.Core's** name, never
`Vesper.AllowNullLiteralAttribute` (`RuntimeNames.fs:106`), so it cannot see this compiler's
own output. And no row is written anyway: `Assembler.fs:1037` adds the only `CustomAttribute`
this backend ever emits, `IsByRefLike`, off `ClrProvider.fs:35`. The doc comment asserts an
emission that does not happen.

It feeds `ExternalClassFlags.Declared.AllowNullLiteral` (`:464`), so a Vesper type marked
`[<AllowNullLiteral>]` read back through metadata comes out `false` and `null` silently stops
inhabiting it. The other nine markers have neither reader nor writer, so a Vesper-compiled
type's equality and comparison posture is invisible across an assembly boundary.

It is latent because the `.fsi` contract travels beside the DLL and is the real carrier. It
becomes live the moment a `.dll` is consumed without its contract — the publishing story.

## What has to be declared

`compiler-attributes.fsi` declares ten (`RuntimeNames.fs:98-121`). Used throughout the core
contracts and declared **nowhere** in Vesper; the new declarations go in
`prim-types-attr.fsi`, beside the `Attribute` base they inherit:

| name | used at |
|---|---|
| `AttributeUsage` | all ten declarations in `compiler-attributes.fsi` |
| `AttributeTargets` | same, as a flags enum combined with `\|\|\|` |
| `AbstractClass` | `prim-types-attr.fsi:9` — on `Attribute` itself |
| `Sealed`, `Struct`, `RequireQualifiedAccess` | `AttributeDecode.fs:11-23` |
| `AutoOpen` | `TypeTranslate.fs:166` |

`IsByRefLike` (`AttributeDecode.fs:20`) gets NO Vesper declaration. It is a BCL type,
`System.Runtime.CompilerServices.IsByRefLikeAttribute`, so on CLR it resolves by key through
the external provider; on JS the spelling diagnoses as an ordinary unresolved attribute, which
is the correct answer for a CLR-only concept.

The meta-attributes `AttributeUsage` and `AttributeTargets` are also BCL types, but unlike
`IsByRefLike` they are written in the shared contracts themselves, so they must resolve on
BOTH targets. **Landed (2026-08-17) as real Vesper declarations, not extern shims.** The
dialect has no extern-enum form, and an extern repr carries no member values for `ConstFold`
to fold, so `AttributeTargets` is a Vesper enum declared with ECMA-335's numeric values and
`AttributeUsageAttribute` a real class over it. Typing an emitted CLR blob as
`System.AttributeTargets` / `System.AttributeUsageAttribute` is a step-4/5 mapping, the same
direction as reading: an attribute row in CLR metadata carries the `System.*` key, so a
metadata reader canonicalises it to the Vesper key, the fix step 5 makes to
`hasAllowNullLiteral`.

## The bootstrap

`compiler-attributes.fsi` applies `[<AttributeUsage>]` and `[<Sealed>]` to the types declared
in that same file, and `prim-types-attr.fsi:9` applies `[<AbstractClass>]` to the root of the
hierarchy. The file defines the vocabulary it is written in. This is the part that decides the
design, and it is why key-based attribute resolution cannot simply be switched on.

## FrozenTast: attributes become the primary encoding

Today an attribute is decoded into `AttributeDecode.ClassAttributeVerdict`,
`EqualityVerdict` / `ComparisonVerdict` and assorted flags at name resolution, and is absent
from the tree the backends walk. Invert that: the frozen attribute list is what is stored,
and those verdicts become derived views over it. One representation with computed views, not
three lossy decodings — and it is what lets the CLR emit a row carrying the attribute's actual
arguments instead of reconstructing them from a `bool`.

Frozen-format change, so the codec (`FrozenCodecDecls.fs`) comes along. *(Corrected
2026-08-28: `Cache.fs` was deleted in `8430062c` and the codec carries no version constant.
Nothing in `src/` persists frozen bytes across runs, so the format break is free; the
round-trip suites — `FrozenCodecTreeRoundTripTests`, `FrozenBlobSizeTests` — are the gate.)*

## Work

1. ~~Declare the missing attribute types and `AttributeTargets`; resolve the bootstrap.~~
   **DONE (2026-08-17).** The bootstrap knot is an `and`-group in `prim-types-attr.fsi`:
   `Attribute and AbstractClassAttribute`, so `[<AbstractClass>]` on `Attribute` resolves under
   top-down scoping; the pair's bodies live in `prim-types-attr.{clr,js}.fs`, `and`-grouped
   there too, because the implementation's own `[<AbstractClass>]` is the same forward
   reference (fsc probe: ungrouped impl fails FS0039; grouped builds). Everything else —
   `AttributeTargets`, `AttributeUsage`, `Sealed` first, then the F#-only markers and the
   corpus-inert set (`Struct`, `RequireQualifiedAccess`, `AutoOpen`, `CompiledName`,
   `CompilationRepresentation{,Flags}`, `Literal`, `Measure`, `CompilerMessage`,
   `EqualityConditionalOn`, `GeneralizableValue`, `Experimental`, `DefaultAugmentation`) — is
   in `compiler-attributes.{fsi,fs}`, transliterated from FSharp.Core. Landing it surfaced one
   analysis gap: the heritable-local `inherit` arm in `MemberRegistration.fs` only resolved a
   repr to an external type, so `AbstractClassAttribute`'s `inherit Attribute()` in
   `prim-types-attr.js.fs` failed on the sentinel; it now falls back to the ctor-bearing canon,
   the same rule the provider arm already had.
2. ~~Land attributes verbatim in the FrozenTast; rebuild the verdicts as views.~~ **DONE
   (2026-08-28).** `TAttribute = { Key: TypeKey; Args: EqArray<TAttributeArg> }` with
   constant-folded `TConstValue` args (`AttributeFold.build`), stored on `TTypeDeclG`,
   `TTypeMemberG`, `TUnionCaseG`, `TRecordFieldG`, `TEnumCaseG` and codec'd; the CST first
   gained an `attributes` slot on `EnumTypeCase` (sub-step 2a). `EqualitySupport`,
   `ComparisonSupport` and `IsRequireQualifiedAccess` are now member views over `Attributes`
   via the one `AttributeVerdicts` decode both the validation pass and the views read.
   Landing it surfaced two findings: frozen class decls had hardcoded `Reference` equality,
   silently dropping equality attributes (and the honest verdict exposed a `NominalEmit` gate
   that emitted `IEquatable` rows without bodies for `[<Struct>]` classes — fixed by gating on
   the data shape); and a fold gap where an argument-bearing attribute before a sibling in one
   bracket (`[<Mark(2, "u"); RequireQualifiedAccess>]`) misfolds — open follow-up, ptest-pinned
   in `AttributeFoldTests`. *(Cleanup pass 2026-08-28: an attribute with a rejected argument is
   now omitted WHOLE, so a frozen `Args` list is never positionally shifted; `TAttributeArg`
   carries the argument's enum identity (`EnumKey`), set by the fold and kept through `|||`
   chains of one enum; the registry's stored verdicts (`TypeInfos`'s
   `EqualitySupport`/`ComparisonSupport`/`IsRequireQualifiedAccess`) became views over
   `Attributes`, same as the frozen decl's; and the per-site `AttrTarget` projection collapsed
   into `Attributes.foldAndValidateTypeDefn`.)* Still
   attribute-less: typar defns, parameters, signature `ArgSpec`/`val`s, exception decls,
   class `let`/`do` preambles, module-level `let`s (enforced but not stored), abbreviations;
   `DeclaredClassFlags`/`ClassValueKind`/`RecordValueKind`/`IsValueType` stay stored, since
   they mix attribute facts with keyword and external-metadata facts.
3. ~~Delete `AttributeDecode`'s name lists and `TypeTranslate.fs:166,171`.~~ **DONE
   (2026-08-17)**, as the `.fsi` front-end work's final step. Attribute
   resolution is `PassContext.ResolveAttributes` (site-memoised, unresolved = error on both
   worlds), `AttributeDecode` reads by key off `ResolvedAttributes`, and
   `AttributeIdentityTests` pins "an unresolved attribute is always an error". Landing it
   surfaced a scoping bug: the signature path pinned an `and`-group's visibility to the
   `type` keyword's offset, in front of which the group's own attributes sit —
   `SignatureResolution.fs` now takes the first retained token, as the impl path already did.
   Remaining silently-unresolved positions are the ones no consumer reads yet — member,
   union-case, field and enum-case attributes — which step 2's whole-tree landing covers.
4. ~~CLR: emit `CustomAttribute` rows for resolved usages, generalising the `IsByRefLike`
   machinery at `ClrEnv.fs:193` to an arbitrary attribute ctor plus blob-encoded arguments.~~
   **DONE (2026-08-28).** `Assembler.PrepareCustomAttributeRows` walks every partitioned type
   decl, its own members and record fields, resolving each `TAttribute` to a ctor handle —
   the local `TypeDef` ctor, a contract `MemberRef` (`ClrExternalMembers.externalAttributeCtor`),
   or the BCL spelling for a `ClrAttributeNames`-mapped key (`[<AttributeUsage>]` ⇒
   `System.AttributeUsageAttribute(System.AttributeTargets)`) — with the II.23.3 blob encoded
   off the folded `TConstValue`s (`AttributeBlob.tryEncode`). An unresolvable ctor or an
   argument outside the encodable constant domain skips the row rather than failing the
   compile; *(cleanup pass 2026-08-28)* each skip's reason is filed on
   `ClrArtifact.SkippedAttributeRows` (`AttributeCtorResolution`), and a named enum-typed
   argument encodes II.23.3's enum form (`0x55` + the enum type's SerString) off
   `TAttributeArg.EnumKey`. The keyword-derived `IsByRefLike` row stays as-is; the
   `[<IsByRefLike>]` spelling is deduped by key. Union-case and enum-case rows are still
   unemitted (ptest-pinned in `AttributeRowTests`).
5. ~~CLR: fix `hasAllowNullLiteral` to accept the Vesper key~~ **DONE (2026-08-28)** — it now
   matches the FSharp.Core spelling and `typeMetaName allowNullLiteralAttributeKey`. The fork
   is decided **advisory** for now: rows serve external .NET tooling and the AllowNullLiteral
   readback; the `.fsi` contract stays the Vesper→Vesper carrier and no readers for the other
   markers were added. `AttributeRowTests` pins the emit + `MetadataSymbols` round trip.
6. ~~JS: drop declarations whose base chain reaches `Attribute` at emit.~~ **DONE
   (2026-08-16).** `EmitJs`'s decl filter drops any class whose base carries an intrinsic
   repr, which reaches `Attribute` (`"!Vesper.Attribute"`) and the `exn` roster alike, before
   `EmitJsTypes`' `inherit` guard sees it. `prim-types-attr.js.fs` binds the sentinel and
   `compiler-attributes.fs` is now in the js `impl` list.
7. ~~Enforce `AttributeUsage` targets — currently decoded by nothing, so `[<Global>]` on a type
   would emit a row rather than erroring.~~ **DONE (2026-08-28).**
   `AttributeFold.build`/`resolveAndBuild` take an `AttrTarget` — the element classification
   fsc uses for FS0842, probed: a module is `Class`, a module value
   `Property|Field|ReturnValue`, a function-typed or generalised value `Method|ReturnValue`, a
   record field `Property|Field`, a union case `Method|Property`, an enum case `Field` — and
   each attribute's declared mask (local registry first, else the contract shape's
   `Attributes`; unreachable ⇒ `All`) is checked at every fold position, signature and impl
   path alike; a module `let` enforces without folding
   (`AttributeFold.enforceTargets` in `Elaborate.translateModuleLet`). Mismatch ⇒
   `Kind.AttributeTargetInvalid`, error severity under FS0842's number and wording (fsc files
   it as a warning), at the attribute's type name. `[<Global>]` on a type now errors. Landing
   it surfaced one mask fault: `[<Import>]` is written on module FUNCTIONS
   (`ops-platform-runtime.js.fs`, `comparison-runtime.js.fs`), which fsc classifies as
   methods, so `ImportAttribute`'s declared mask gains `Method`. Open follow-ups:
   `AllowMultiple` / `Inherited` enforcement (`AllowMultiple` ptest-pinned in
   `AttributeFoldTests`), and the still-unfolded positions (typar defns, parameters,
   signature `val`s, abstract member signatures, exception decls, class preambles, modules)
   pass unchecked. *(Cleanup pass 2026-08-28: each `ConstRejection` case now reports its own
   claim through `ConstFold.rejectionKind` — an out-of-range literal is no longer "not a
   valid constant expression" — and `Kind.AttributeArgNotConstant` was renamed
   `Kind.NotConstantExpression`, since a `[<Literal>]` RHS reports it too.)*
   Prerequisite (landed earlier): a `ConstFold` module in
   SemanticAnalysis, sibling of `EnumCaseValues` and in its style —
   `tryConstant: ... -> Expr<SyntaxToken> -> Result<TConstValue, ConstRejection>` *(tier
   corrected 2026-08-28: attribute arguments are held as raw CST on
   `ResolvedAttribute.Construction` and never reach elaboration, so the fold takes the CST
   expression; an identifier argument resolves through name resolution — an enum case or a
   `[<Literal>]` value, a value claim shadowing a case as expression resolution orders them —
   via a single `tryNamedConstant` lookup the caller wires)* — folding the
   closed attribute-argument constant domain: a literal; a named-constant reference (an
   enum member read from the registry or `ExternalEnumCaseShape.Value`, or a module-level
   `[<Literal>]` value, whose RHS folds through the same domain at registration into
   `Resolution.LiteralValues` under top-down scoping); `|||`/`&&&`/`^^^` on integral constants
   of one width; unary minus. Anything else in attribute position is a diagnostic (F#'s
   FS0267), not a silent pass-through — as is a `[<Literal>]` RHS outside the domain (fsc's
   FS0267 there too, probed 2026-08-28). fsc's literal-body domain is wider — `+`, `*`,
   `<<<`, string concatenation all fold there (F# 5+) — and a named literal is accepted as an
   enum case's value; both are open follow-ups, as is the literal CONTRACT leg: a `.fsi`
   carries a literal's value (`[<Literal>] val Mask: int = 3`; omitting the value is FS0876),
   but `ExternalSymbol` carries no constant yet, so a literal declared in another file or
   assembly does not fold. This needs NO provider change for enums: values already cross the
   seam as `IntVal`/`StringVal` (`ExternalDeclarations.fs:141`), and the fold rules are
   language semantics, so they live in analysis once rather than per provider. An
   `expr -> expr` evaluator seam was considered and rejected here; that shape belongs to
   backend optimisation passes downstream of freeze. Step 4's blob encoding consumes the same
   `TConstValue`.

## Consequence for the manifests

**Settled (2026-08-16).** `compiler-attributes.fsi` pairs with `compiler-attributes.fs` on BOTH
targets and needs no exemption; the `sig-only` schema is deleted (landed 2026-08-17).

That surfaced one thing worth carrying into step 1: `prim-types-attr.fsi` declared no `new`,
so `inherit Attribute()` resolved only by falling through to the platform repr, and only where
that repr names a real type. It now declares `new: unit -> Attribute` — the ctor the ten
`inherit` clauses were already calling — and both targets take the same ctor-bearing path.

## Review pass (2026-08-28, second cleanup)

- The attribute value model (`TAttribute[Arg]`, `TAttributes`) and the verdict decode
  (`TypeDefnKind`, `EqCompAttr`, `AttributeVerdicts`) moved out of `TastDecl.fs` into
  `AttributeVerdicts.fs`, compiled just before it.
- `TypeDefnKind` now carries struct-ness (`StructRecord` / `StructUnion` / `StructClass`),
  so one classification feeds both the legality matrices and the `AttrTarget` projection;
  the `isStruct` parameter on `Attributes.foldTypeDefn` / `foldAndValidateTypeDefn` is gone.
  `[<ReferenceEquality>]` on a value type now reports FS0376
  (`Kind.ReferenceEqualityOnStruct`), matching fsc, alongside the FS0842 target error the
  contract's `AttributeUsage(Class)` mask already produced. A struct UNION's kind is
  registration-side only: `TUnionG` carries no value kind yet, so the frozen `DefnKind`
  reads `Union` (struct unions are unlowered; see `struct-union-layout-plan.md`).
- **The mutable-record equality divergence is dropped**: a record with a mutable field now
  defaults to `Structural`, as fsc's does (probed: `{ mutable Y }` compares structurally,
  struct or not), so the CLR now emits its equality triple. The default is decided in the
  single `AttributeVerdicts.equalitySupport`; reinstating a divergence or a compiler option
  is a change to that one function, though an option would have to thread to every view
  read-site and would make a frozen tree's meaning depend on out-of-band state.
- `ResolvedAttribute` carries the `CstKeys.TypeRef` resolution already computed, so
  `AttributeFold` reports at `entry.TypeRef.Site` instead of re-deriving it behind a
  silent-`ValueNone` guard.
- CLR row prep moved out of `Assembler` into `AttributeRowPrep.prepare`. Skips are
  structured (`ClrArtifact.SkippedAttributeRows: SkippedAttributeRow list`, reason DU), and
  the enum-SerString fallback is gone: a named argument typed by a referenced-assembly enum
  skips its row as `ForeignEnumArgument` instead of writing a plain name that cannot bind.
  A positional enum argument no longer consults the enum name at all (the fixed-argument
  encoding follows the ctor parameter type). The property-row placement (`get_` method, not
  a `Property` row) is ptest-pinned in `AttributeRowTests` beside the case-row gap.
