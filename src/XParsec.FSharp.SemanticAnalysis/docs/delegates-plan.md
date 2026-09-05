# Delegate types — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Deferred until `typar-scope-plan.md` lands. Stage 2 is a format bump, and two format bumps
never interleave.

## The gap

No program in the corpus can declare or use a delegate: `type D = delegate of int -> int`
registers under `UnmodelledReason.Delegate`, `SignatureResolution` publishes it as
`ExternalTypeShape.Unmodelled`, and every use is a diagnostic. The `delegate<_,_>` typar
constraint therefore has no type it can hold at, and a foreign generic constrained by
`System.Delegate` cannot be instantiated from this compiler.

**Decided.** A sealed `MulticastDelegate` subclass on the CLR, a function value on JS.

**What the model carries.** A delegate declaration is its `Invoke` signature: one uncurried
argument group and a return type, over the declaration's typars. Construction is
`D(fun x -> …)` or `D(f)` and takes any function of the `Invoke` shape; invocation is
`d.Invoke(args)`. The front end types both against the signature alone, so the runtime
`.ctor(object, native int)` and the `BeginInvoke` / `EndInvoke` pair are emission facts, not
front-end ones.

## Stages

1. **Front end.** `TypeRegistration` registers a `DelegateTypeInfo` with the `Invoke`
   signature, and `UnmodelledReason.Delegate` goes. A delegate is a `TyClass` whose info is
   the delegate's, so subsumption to `System.Delegate` and `MulticastDelegate` falls out of
   the class `inherit` chain and every class-shaped verdict reads it without a new arm.
   `Infer` types construction from a lambda or a function value and invocation through
   `.Invoke`. `UnificationConstraintCheck` then answers `delegate<args, ret>` by unifying the
   `Invoke` signature with `args -> ret`, replacing the `NotYetSupported` verdict. No format
   change.
2. **Publication.** `ExternalTypeShape.Delegate` with a frozen `Invoke` signature, through
   `SignatureResolution`, `FrozenSignature` and the pool codecs, and the TS extractor maps a
   function type alias to it. A format bump, landing alone.
3. **CLR emission.** A sealed class extending `MulticastDelegate`, with `runtime managed`
   `.ctor`, `Invoke`, `BeginInvoke` and `EndInvoke` rows, calibrated against a decompiled
   `fsc` output. Construction emits `ldftn` + `newobj`; invocation is a `callvirt` to
   `Invoke`. A closure passed at construction is the existing closure class's `Invoke`.
4. **JS emission.** Construction is the function value itself and `.Invoke` is a call, so a
   delegate erases; the golden pins that no wrapper survives.

The `'a : delegate<_,_>` constraint import in `typar-scope-plan.md` waits on stage 2.

## Verify

`MetadataStructureTests` assert the four method rows and their `runtime managed`
implementation flags. The corpus gains `delegates/declare-invoke.fs` accepted on both
targets, `delegates/typar-delegate.fs` accepted on both once stage 1 lands, and a `-violated`
pair where a class and a function of the wrong arity are refused under `delegate<_,_>`.
