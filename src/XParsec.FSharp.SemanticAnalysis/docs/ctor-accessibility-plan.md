# Constructor accessibility is parsed and then dropped — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are given only where a claim needs pinning; treat them as of the commit that
added this file and re-locate by construct.

## The gap

A constructor's access modifier is accepted by the parser and reaches no later pass. Every
constructor is published to consumers and emitted to metadata as **public**, whatever the
source wrote.

```fsharp
// file1.fs
namespace CrossFile

module Lib =
    type Shape private (x: int) =
        static member Make(v: int) = Shape(v)
        member this.Raw = x

// file2.fs
open CrossFile.Lib

let s = Shape(7)        // F# rejects: FS0801. This compiler accepts it.
```

F#'s verdict on the last line is `FS0801: This type has no accessible object constructors`,
confirmed against `dotnet fsi`. This compiler resolves the constructor, types the argument,
and emits a `newobj` on a `public` `.ctor`.

The same holds for a secondary: `private new(v: int) = { V = v }` publishes and emits public.

## What the parser already has

All three constructor spellings carry the modifier token:

| Node | File | Field |
| --- | --- | --- |
| `PrimaryConstrArgs` | `XParsec.FSharp/Expr.fs:510` | `access: 'T voption` |
| `MemberDefn.AdditionalConstructor` | `XParsec.FSharp/Expr.fs:637` | `access: 'T voption` |
| `TypeSignatureElement.Constructor` | `XParsec.FSharp/Signatures.fs:25` | `access: Access<'T> voption` |

`ElaborateMembers.accessibilityOfToken` (`Elaborate/Members.fs:17`) already maps exactly that
`SyntaxToken voption` shape to `Accessibility`. Nothing needs parsing or inventing; the fact is
present and unread.

## Where it is dropped

Each row has a member-side counterpart that DOES carry accessibility, which is the template for
the constructor side.

| Layer | Constructor | Member counterpart |
| --- | --- | --- |
| Registration | `extractCtorParams` / `extractSecondaryCtors` ignore `access` (`NameResolution/MemberRegistration.fs`) | `resolveMember` reads `accessibilityOfToken memberAccess` |
| Registry info | `ClassTypeInfo` / `ClassSecondaryCtorInfo` (`TypeInfos.fs:354`) hold no accessibility | `TypeMemberInfo.Accessibility` |
| TAST | `TClassG` (`TastDecl.fs:121`) has `CtorParams` + `HasPrimaryCtor`; `TSecondaryCtorG` has `Params` / `Lets` / `PrimaryArgs` / `FieldInits`. Neither holds accessibility | `TTypeMemberG.Accessibility` |
| Frozen codec | `writeSecondaryCtor` / `readSecondaryCtor` (`FrozenCodecDecls.fs:443`) round-trip no accessibility | `FrozenCodecDecls.fs:322` writes `m.Accessibility` |
| Publication (`.fs`) | `FrozenSignature.ctorsOf` publishes every constructor unconditionally | `membersOf` drops `Accessibility.Private` |
| Publication (`.fsi`) | `SignatureResolution/Members.fs:193` `ctorOf` publishes every `new: … -> T` | same `resolveMember` cut |
| CLR emission | `MethodAttrSets.ctorAttrs` (`Codegen.Clr/LayoutModel.fs:46`) is unconditionally `MethodAttributes.Public` | — see scope note below |

## What F# means by a private constructor

Verified against `dotnet fsi`:

- `type Shape private (x: int)` makes the primary constructor accessible within the enclosing
  module (or type) only. A `static member Make` on `Shape` itself may call it; a later file may
  not.
- `private new(v: int) = { V = v }` scopes one secondary the same way. Sibling constructors of
  the same type are unaffected.
- A type with no accessible constructor at the use site reports FS0801, not a member miss or an
  argument mismatch. The wording to match is *"This type has no accessible object constructors"*.
- `internal` scopes to the assembly, which is the same threshold `FrozenSignature.toSurface`
  already calls internal-or-better.

## Staged plan

Each step leaves the tree green; each is a commit boundary.

### 1. Carry the fact from the CST to the registry

Add `Accessibility` to `ClassSecondaryCtorInfo`, and a `PrimaryCtorAccessibility` to
`ClassTypeInfo` beside `HasPrimaryCtor`. Fill both in `MemberRegistration` from
`accessibilityOfToken`. Nothing reads them yet, so the step is inert and provable only by a
registry-level test.

### 2. Carry it into the TAST and the frozen codec

Add the matching fields to `TClassG` and `TSecondaryCtorG`, thread them through
`Elaborate/TypeDecls.fs` (`translateSecondaryCtor`, the `TTypeKind.Class` construction), and
extend `writeSecondaryCtor` / `readSecondaryCtor` plus the class writer. This changes the frozen
binary format, so the cache-invalidation tests are the ones to watch.

### 3. Apply the publication cut

`FrozenSignature.ctorsOf` and `SignatureResolution/Members.fs`'s `ctorOf` drop
`Accessibility.Private`, on the same internal-or-better threshold `membersOf` uses.

At that point a later file resolving `Shape(7)` finds an EMPTY `.ctor` catalogue, and
`UnificationInferCtor.inferExternalCtorOn` already reports *"External type '%s' has no accessible
constructor"* for that case. Re-word it to F#'s FS0801 text.

**This step interacts with an existing guard.** `UnificationClassCtors.baseCtorSurfaceOf` treats
an empty catalogue on a class declared in THIS assembly as a real "declares none" and diagnoses
it, and an empty catalogue on a metadata class as "not modelled" and reports no diagnostic. Once
a private constructor makes the published catalogue legitimately empty, `inherit Shape(7)` across
files starts diagnosing — which is correct, and is what F# does.

### 4. Diagnose the same-assembly, out-of-scope use

Steps 1–3 cover a use in a LATER file, because publication is the cut. A use in the SAME file but
outside the declaring module still resolves through `TypeRegistry`, which has no accessibility
filter. `UnificationInferOverload.pickLocalCtor` is the single seam every local construction now
goes through (`new T(…)`, ctor sugar, `inherit`, and Elaborate's object-argument wrapping), so the
scope check belongs there, taking the use site the caller already holds.

### 5. Emit the modifier

`ctorAttrs` becomes a function of the constructor's accessibility.

## Scope

`MethodAttrSets` emits `MethodAttributes.Public` for every method, not only constructors:
`instanceMethodAttrs`, `staticMethodAttrs` and `abstractMethodAttrs` all hardcode it, so a
`member private` is public in metadata too. Step 5 is therefore a slice of a wider "project
accessibility into CLR visibility" job. Doing constructors alone is coherent — a public `.ctor`
on a type whose constructor F# calls private is the visible defect — but the wider sweep should
land as its own scope rather than being folded in.

The JS backend has no visibility concept to project into, so steps 1–4 are target-neutral and
step 5 is CLR-only.

## Open questions for the user

1. **Is step 4 wanted at all in this scope?** Steps 1–3 fix the cross-file case, which is the one
   with an emitted artifact behind it. Step 4 is the same-file scope rule and is a larger surface
   (it needs the use site threaded into the pick, and F#'s rule is "the enclosing module or type",
   which the registry does not currently model for constructors).
2. **`internal` on the cross-assembly cut.** `FrozenSignature.toSurface`'s comment says the
   public-only cut belongs to the consumer, not to publication. An `internal` constructor should
   therefore publish and be filtered by whatever reads a reference assembly. Confirm no such
   consumer-side filter is expected here.
3. **Should `type T = val …; new(…)` with only private secondaries publish nothing?**
   `NominalEmit` emits a parameterless primary when `HasPrimaryCtor` is false and the secondary
   list is empty (`NominalEmit.fs:162`). If every secondary is private, the published catalogue
   empties while the emitted type still has a constructor. That is F#-correct but the two rules
   are stated in different places and should be reconciled while step 3 is open.
