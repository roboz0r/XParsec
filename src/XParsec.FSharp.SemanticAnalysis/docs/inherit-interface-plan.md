# `inherit <interface>` is accepted silently — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are given only where a claim needs pinning; treat them as of the commit that
added this file and re-locate by construct.

## The bug

A class may name a project-local INTERFACE as its `inherit` parent. Nothing diagnoses it.
The class is then recorded as EXTENDING the interface rather than implementing it, and the
CLR backend emits an assembly whose `extends` column points at an interface `TypeDef` —
which the runtime rejects at type-load time.

```fsharp
type ILocal =
    abstract M: unit -> int

type D() =
    inherit ILocal
    member this.X = 1
```

Observed, front to back:

| Stage | Result |
| --- | --- |
| Name resolution / inference | **no diagnostics at all** |
| `FrozenSignature` class shape | `FrozenBaseType = ValueSome (FTClass ILocal)`, `FrozenInterfaces = []` |
| CLR codegen | succeeds, writes a PE |
| `Assembly.GetTypes()` | `ReflectionTypeLoadException` — *"Could not load type 'D' … because the parent type is an interface"* |

So the failure is a runtime type-load error on a compiler-accepted program. Note the second
row: `D` does not implement `ILocal` either, so nothing downstream compensates — the interface
is lost, not relocated.

The bug is pre-existing; it was found while auditing the comment on `FrozenSignature`'s
`FrozenBaseType`, which asserts that *"name resolution admits only a named class"*. That is
true of every `inherit` spelling except this one.

## Cause

`MemberRegistration.resolveInheritParent` resolves the written parent name with
`TypeRegistry.tryClass` (`MemberRegistration.fs:782`):

```fsharp
match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
| ValueSome info -> ValueSome(TyClass(info.TypeKey, EqArray.ofList targs))
```

`tryClass` reads the `Class` registry and applies no kind filter — and a project-local
interface IS in that registry: it is registered as a `ClassTypeInfo` whose
`IsInterface` flag is set (`ClassTypeInfo.IsInterface`, written in `MemberRegistration`
from `TypeDefnPatterns.isInterfaceShape`). The flag exists and is correct; this call
simply never reads it.

Every other bad `inherit` IS diagnosed, in the same function — a project-local type of
another kind (*"Cannot inherit from type '%s' — only classes are inheritable"*), an unknown
name, an unresolvable external repr, a qualified base, an unsupported target. An interface
slips through only because it is registered as a class.

The `interface <ty>` clause has exactly the check this one is missing: `Unification`'s
`resolveInterfaceImpls` stamps `impl.Resolved` only when the resolved `TyClass`'s key is
interface-shaped, and reports *"Type '%s' is not an interface"* otherwise. The `inherit`
clause needs the mirror of that test.

## The fix

In `resolveInheritParent`'s local-class arm, reject an `info.IsInterface` hit rather than
returning a `TyClass` for it. `ValueNone` is already the "diagnosed, do not set `BaseType`"
answer used by the sibling arms, so the shape is:

```fsharp
match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt diagKey) name with
| ValueSome info when info.IsInterface ->
    diagnose nameTok (Kind.Message(sprintf "Cannot inherit from interface '%s' — implement it with `interface %s with`" name name))
    ValueNone
| ValueSome info -> ValueSome(TyClass(info.TypeKey, EqArray.ofList targs))
| ValueNone -> …
```

Decisions to make while implementing, none of them settled here:

- **Message wording.** The sibling message is *"Cannot inherit from type '%s' — only classes
  are inheritable"*. An interface is the one case where the author has an obvious intended
  alternative, so pointing at `interface … with` is worth the extra clause — but check it
  reads well against the existing `Kind.Message` texts rather than copying the sketch above.
- **Whether a plain `Kind.Message` is right.** The neighbouring `inherit` rejections all use
  `Kind.Message`; `Unification`'s interface-side check does too. Match them unless a coded
  diagnostic is being introduced for this family anyway.
- **External interfaces.** This plan covers the project-local arm only, because that is what
  was reproduced. Before finishing, check the `resolveThroughProvider` arm: an EXTERNAL
  interface named as a base should reach the same rejection, and it is not obvious from
  reading whether `ExternalTypeShape.Class { IsInterface = true }` is filtered there. Write
  the test either way.
- **Intrinsic capabilities.** `inherit` of an intrinsic interface (a capability such as
  `disposable`) goes through the intrinsic arm, not `tryClass`. Confirm what it does today
  before deciding whether it needs the same guard.

## Verification

- A diagnostic test beside the existing *"implementing a non-interface type is rejected with
  a diagnostic"* in `Codegen.Clr.Tests/ClassTests` — that test is this one's mirror image and
  the pair belongs together.
- Cases: local interface (the repro above), external interface, and a control that a legal
  `inherit <class>` still resolves and still sets `BaseType`.
- Full `SemanticAnalysis.Tests` and `Codegen.Clr.Tests` runs. A previously-silent program
  becoming an error can surface in fixtures, so expect to fix any corpus source that was
  relying on the hole.

## Related

Once this lands, the comment on `FrozenSignature`'s class shape (and its twin in
`LayoutNodes`) becomes unconditionally true, which is the invariant `FrozenNominal.OfFrozen`
is asserting at both sites. No code change is needed there — the claim just stops having an
exception.

## Neighbouring defect: a bodied signature's `inherit` clause crashes on a name it already diagnosed

Relocated from the deleted fsi-front-end-plan (2026-08-28); it is the same `inherit` narrowing,
on the `.fsi` side.

`SignatureResolution/Members.fs` narrows `FrozenBaseType` to `FrozenNominal` through
``OfFrozen "an `inherit` clause"`` (`Members.fs:365`), which `failwithf`s on anything that does
not name a type constructor. An undefined name reaches it as one: `Unification/Translate.fs`
reports `UndefinedType` and returns `TyUnknown`, and `freezeOver` carries that to `FTUnknown`.
So the base type faults where the interface list beside it — same walk, same freeze,
`TryOfFrozen` (`Members.fs:297`) — drops silently, and a diagnosed source error becomes a
compiler crash rather than a message.

Settle whether the base type should degrade like the interfaces do, or whether an unresolved
`inherit` should be a hard error raised as a diagnostic before the freeze ever sees it. The
narrowing itself is wanted; only its behaviour on the diagnosed path is open. This is the
recheck the code's own TODO deferred to the `.fsi` rebase: the `.fsi` front end's package fold
did NOT close it.
