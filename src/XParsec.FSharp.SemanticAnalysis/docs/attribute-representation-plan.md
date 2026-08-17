# Attributes are real types with a target representation

**Status (2026-08-15): new plan.** Depends on
[fsi-front-end-plan](fsi-front-end-plan.md) for key-based resolution in contracts. Delete
when it lands (`feedback_plan_docs_ephemeral`).

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
contracts and declared **nowhere** in Vesper:

| name | used at |
|---|---|
| `AttributeUsage` | all ten declarations in `compiler-attributes.fsi` |
| `AttributeTargets` | same, as a flags enum combined with `\|\|\|` |
| `AbstractClass` | `prim-types-attr.fsi:9` — on `Attribute` itself |
| `Sealed`, `Struct`, `IsByRefLike`, `RequireQualifiedAccess` | `AttributeDecode.fs:11-23` |
| `AutoOpen` | `TypeTranslate.fs:166` |

## The bootstrap

`compiler-attributes.fsi` applies `[<AttributeUsage>]` and `[<Sealed>]` to the types declared
in that same file, and `prim-types-attr.fsi:9` applies `[<AbstractClass>]` to the root of the
hierarchy. The file defines the vocabulary it is written in — the same shape as
`--compiling-fslib` (`feedback_fsharpcore_one_assembly`). This is the part that decides the
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

1. Declare the missing attribute types and `AttributeTargets`; resolve the bootstrap.
2. Land attributes verbatim in the FrozenTast; rebuild the verdicts as views.
3. Delete `AttributeDecode`'s name lists and `TypeTranslate.fs:166,171`.
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
   would emit a row rather than erroring.

## Consequence for the manifests

**Settled (2026-08-16).** `compiler-attributes.fsi` pairs with `compiler-attributes.fs` on BOTH
targets and needs no exemption — see [retire-sig-only-plan](retire-sig-only-plan.md).

That surfaced one thing worth carrying into step 1: `prim-types-attr.fsi` declared no `new`,
so `inherit Attribute()` resolved only by falling through to the platform repr, and only where
that repr names a real type. It now declares `new: unit -> Attribute` — the ctor the ten
`inherit` clauses were already calling — and both targets take the same ctor-bearing path.
