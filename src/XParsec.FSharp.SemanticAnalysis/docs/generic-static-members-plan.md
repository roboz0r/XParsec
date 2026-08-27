# Static members on a generic class: the written `<'args>` never reach emission

`Box<int>.P` on the CLR loads as a malformed assembly. The declaring instantiation is written
right there at the call site, and every layer between the qualifier and the `MemberRef` throws
it away, leaving codegen to *recover* it from the member's signature — which is impossible when
the class typar appears nowhere in that signature.

This is not a regression from the static-property writes. It reproduces at `ca58693d` on a plain
read with no assignment anywhere.

## Symptom

```fsharp
type Box<'T>() =
    static member P with get () = 5
printfn "%d" Box<int>.P
```

`System.BadImageFormatException: An attempt was made to load a program with an incorrect format.`

CLR only. The same source round-trips on JS, which erases generics and emits the accessor as an
ordinary free function.

## Root cause

`EmitResolve.resolveStaticMember` needs the declaring type's instantiation at the call site,
because a static member on a generic class compiles to a `MemberRef` on the class `TypeSpec`.
Its `instantiationFor` has three ways to find one and a fourth that emits invalid IL:

1. The member's RESULT is the declaring type (`Set<'T>.Empty : Set<'T>`) — read the args off it.
2. The declaring type is non-generic — no args to find.
3. `recoverMemberInst` unifies the member's open signature against the instantiated one. This
   covers a typar surfacing in a parameter or the result: `Box<'T>.Describe (x: 'T) : int` is
   tested and passes.
4. Otherwise the bare declaring typars, minting ``Box`1<!0>::get_P`` — an open typar with no
   owner.

Case 4 is the bug. `resolveStaticMember`'s own doc comment already predicts the failure it
produces, down to the exception; what it does not say is that the fallback is reachable from
ordinary source rather than only from a compiler-internal mistake.

So the failing shape is exactly: **a static member on a generic class whose declaring typars
appear nowhere in its own signature.** Get, set and method alike — a write reaches it through
`mkStaticMethodCall` for the same reason a read reaches it through `buildStaticPropertyGet`,
which passes an empty argument list and so leaves `recoverMemberInst` only the result type.

## The instantiation is written, then discarded twice

Nothing here is ambiguous. `Box<int>.P` parses as `DotLookup(TypeApp(Box, <int>), .P)`, and
NameResolution's `TypeApp` visit already stamps the resolved type. Both later passes drop it:

- Inference matches the `TypeApp` for its class name and ignores its types, so the written
  `<int>` never reaches a `unify` call.
- Elaborate's `(|TypeAppStaticMember|_|)` yields `TypeKey * string * ClassMemberKind`, and
  `TExpr.StaticPropertyGet` carries a `SymbolKey` and the member's own type. There is no slot
  for the declaring instantiation, and the member type does not imply it.

Those are two separate losses with two separate consequences, below.

## Required: inference must unify the written `<'args>`

Inference deliberately does NOT unify them today — `freshMemberInstance` says so, and the read
path has always behaved that way: the member's annotated type pins the instantiation on its own.

That is wrong, and fixing it is a requirement of this work, not an option. `C<'args>.M` names
the member AT that instantiation; discarding the args means

```fsharp
type Box<'T>() =
    static member P with get () : 'T = Unchecked.defaultof<'T> and set (w: 'T) = ()
Box<string>.P <- 3
```

is silently accepted — `'T` is instantiated fresh, the written `string` goes nowhere, and the
value pins it to `int`. That is a soundness hole, and it is the reason to fix this at all; the
malformed IL is the lesser half.

The machinery already exists and the ctor path already uses it: `tryInferExternalGenericCtorApp`
and `tryInferLocalCtorApp` both translate their `Expr.TypeApp` types through `translateType` and
consume them. Static member access is the outlier that drops them on the floor.

## Unifying is necessary but not sufficient

Unification alone will NOT stop the `BadImageFormatException`, and it is worth being exact about
why, because the two halves look like one problem.

The failing shape is *defined* by the declaring typars appearing nowhere in the member's
signature. Unifying `<int>` against the fresh declaring instance binds that instance, but it
does not change the signature: `P`'s type is `int` before and after. So `recoverMemberInst`,
which works by unifying the member's open signature against its instantiated one, still has
nothing to recover `!0` from. Path 3 cannot be made to cover case 4 — that is what makes case 4
its own case.

So the instantiation must ALSO be carried to the emit node. The good news is that doing the
unification first is what makes carrying it correct rather than a guess: once the written args
are unified into the fresh declaring instance, that instance ZONKED is the instantiation, with
no separate derivation to keep in step. Do them in that order.

## The referenced-class half: no type arguments written at all

The above is the local `Box<'T>` chain, where the instantiation is written and dropped. A
REFERENCED generic class reached with no type arguments loses it one stage earlier, in name
resolution. `ResolvedStamps.tryStaticQualifier` (`PassContext.fs:76-83`) admits
`ExternalTypeShape.Class` at `TyparArity = 0` and an intrinsic at canon arity 0, so
`splitExternalStaticPrefix` (`InferResolve.fs:373`) misses for a generic one and the access
elaborates to an unkeyed `TExpr.External` holding the written name, with no diagnostic — the
silent fall-through class the `System.Math.PI` finding belonged to. Under an annotation that node
carries the annotated type; with nothing to type it, a free type variable.

`dotnet fsi` accepts the shape and takes the instantiation from context:

```fsharp
let c : System.Collections.Generic.EqualityComparer<int> =
    System.Collections.Generic.EqualityComparer.Default
```

Pinned by `ExternalMemberTests` ("GAP a generic class's static reached without written type
arguments"), which asserts the elaborated `ExternalMember` and its declaring key rather than the
absence of diagnostics. Landing it means `tryStaticQualifier` answering at any arity and
`inferExternalStaticMember` receiving fresh declaring args to unify — the same fresh-args-then-
zonk discipline Work step 1 states for the written case, with inference supplying what the source
does not write.

## Work

1. **Unify.** `freshMemberInstance` gains a form handing back the fresh declaring args alongside
   the member type (`freshNamedInstance` already returns both). `tryLocalTypeAppStaticMember`
   translates the written types and unifies them pairwise against those args. A written arity
   that does not match the declaration is a diagnostic, not a silent drop.
2. **Carry.** `AssignTarget.StaticSlot` keeps the written types off the `Expr.TypeApp` it already
   peels — today it retains only the qualifier and slot tokens, so `C<int>.P <- v` has nowhere to
   read them from. `StaticPropertyGet` / `StaticMethodCall` then carry the zonked declaring
   instantiation to emission. Whether that rides in a new field or in the minted member key is an
   implementation call; the key already carries a declaring axis and may be the better home.
3. **Fail loudly.** `instantiationFor`'s last-resort bare-typar branch becomes a `failwithf`. A
   declaring instantiation that could not be determined must not silently mint IL that cannot
   load — which is precisely what it does today.

Step 1 needs checking against every existing `C<'args>.M` site that currently relies on the args
being inert.

## Adjacent

`resolveStaticMember`'s generic-union branch fails loudly with a milestone label in its message
(`"… is out of scope (R2)"`). Drop the label when this work touches the function.

## Done when

- A written `<'args>` on a static member access is unified into the declaring instantiation, so
  `Box<string>.P <- 3` against `P : 'T` is a type error. This is the requirement; the rest
  follows from it.
- A written arity that does not match the declaration is diagnosed.
- `Box<int>.P`, `Box<int>.P <- v` and a generic static method whose typars do not surface in its
  signature all emit loadable IL, covered by CLR tests.
- The undeterminable-instantiation path fails loudly rather than emitting bad IL.
- This file is deleted.
