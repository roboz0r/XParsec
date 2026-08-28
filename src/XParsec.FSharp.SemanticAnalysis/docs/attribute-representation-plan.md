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

Frozen-format change, so the codec and `Cache.CodeVersion` (currently `Cache.fs:52`, `31`)
come along.

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
2. Land attributes verbatim in the FrozenTast; rebuild the verdicts as views.
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
4. CLR: emit `CustomAttribute` rows for resolved usages, generalising the `IsByRefLike`
   machinery at `ClrEnv.fs:193` to an arbitrary attribute ctor plus blob-encoded arguments.
5. CLR: fix `hasAllowNullLiteral` to accept the Vesper key, and decide whether the rows are
   **authoritative** (readers for the other nine; the DLL becomes self-sufficient) or
   **advisory** (emitted for external .NET tooling, contract stays the carrier for
   Vesper→Vesper). That fork is really a publishing question.
6. ~~JS: drop declarations whose base chain reaches `Attribute` at emit.~~ **DONE
   (2026-08-16).** `EmitJs`'s decl filter drops any class whose base carries an intrinsic
   repr, which reaches `Attribute` (`"!Vesper.Attribute"`) and the `exn` roster alike, before
   `EmitJsTypes`' `inherit` guard sees it. `prim-types-attr.js.fs` binds the sentinel and
   `compiler-attributes.fs` is now in the js `impl` list.
7. Enforce `AttributeUsage` targets — currently decoded by nothing, so `[<Global>]` on a type
   would emit a row rather than erroring. Prerequisite: a `ConstFold` module in
   SemanticAnalysis, sibling of `EnumCaseValues` and in its style —
   `tryConstant: TExpr -> Result<TConstValue, ConstRejection>` — folding the closed
   attribute-argument constant domain: a literal; an enum-member reference read from the
   registry or `ExternalEnumCaseShape.Value`; `|||`/`&&&`/`^^^` on integral constants of one
   width; unary minus. Anything else in attribute position is a diagnostic (F#'s FS0267), not a
   silent pass-through. This needs NO provider change: enum values already cross the seam as
   `IntVal`/`StringVal` (`ExternalDeclarations.fs:141`), and the fold rules are language
   semantics, so they live in analysis once rather than per provider. An `expr -> expr`
   evaluator seam was considered and rejected here; that shape belongs to backend optimisation
   passes downstream of freeze. Step 4's blob encoding consumes the same `TConstValue`.

## Consequence for the manifests

**Settled (2026-08-16).** `compiler-attributes.fsi` pairs with `compiler-attributes.fs` on BOTH
targets and needs no exemption; the `sig-only` schema is deleted (landed 2026-08-17).

That surfaced one thing worth carrying into step 1: `prim-types-attr.fsi` declared no `new`,
so `inherit Attribute()` resolved only by falling through to the platform repr, and only where
that repr names a real type. It now declares `new: unit -> Attribute` — the ctor the ten
`inherit` clauses were already calling — and both targets take the same ctor-bearing path.
