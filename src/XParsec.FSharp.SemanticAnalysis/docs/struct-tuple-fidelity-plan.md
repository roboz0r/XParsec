# Struct tuple fidelity — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Split out of `Codegen.Clr/docs/method-params-plan.md`, which needed to say what
`p: struct (int * int)` means in an abstract signature and found there is no answer.

---

# The defect

`struct (int * int)` is parsed, then discarded. `Unification/Translate.translateType` has cases
for `Type.TupleType` and `Type.ParenType` but **none for `Type.StructTupleType`**, so it falls
to the catch-all arm — "Shapes with no model, such as an anonymous record. A free TyVar lets
unification pin it from context." The annotation becomes a fresh type variable and inference
pins it from the use site.

```fsharp
let f (x: struct (int * int)) = x + 1
printfn "%d" (f 1)
```

We compile this clean and print `2`: `x` is inferred `int`, and the annotation contributed
nothing. F# rejects it:

```
error FS0001: The type 'int' does not match the type 'struct (int * int)'
```

An annotation that is silently dropped is worse than one that is refused, because the program
type-checks against a type the source did not write. This is the general hazard of the
`TyVar(freshTyVar ctx)` catch-all, and struct tuples are one instance of it.

# What the flag is for

The requirement is that a struct tuple stay **distinguishable from a reference tuple all the
way through the front end**. Backends may then choose to ignore the distinction: the CLR
backend emits `System.ValueTuple` for both, deliberately, and that choice stays.

So the flag is not there to change CLR emission. It is there so that

- the annotation constrains inference at all, rather than dissolving into a type variable;
- `(int * int)` and `struct (int * int)` are two types for the purpose of type checking, as
  they are in F#;
- a backend that *does* care — JS representation, or a future CLR change — has the fact
  available rather than having to reconstruct it from source it can no longer see.

# Scope

`FTTuple of items: EqArray<FrozenType>` (`SemanticInfo.fs`) and the `SemType` `TyTuple` gain a
struct-ness discriminator. `FTTuple` is matched in roughly eighteen files; most are
`FTTuple elems` patterns that become `FTTuple(elems, _)` and need no thought. The ones that do:

- **`Translate.translateType`** — add the `StructTupleType` case. This is the fix; everything
  else is carrying it.
- **`FrozenCodec` / `FrozenCodecTypes`** — encode the flag, bump `FormatVersion`.
- **Unification** — a struct tuple and a reference tuple must not unify. This is the change
  that makes the F# error above reproduce, and the one that can break existing green tests.
- **`ClrEncoder`** — explicitly ignores the flag, emitting `System.ValueTuple` for both.
  Site the decision there, because it is the surprising one.
- **`Codegen.Js` / `TsManifestTypes`** — decide whether the TS manifest surfaces the
  distinction. A TS tuple type has no struct-ness, so probably not, but the manifest is a
  published surface and the choice should be deliberate rather than inherited.

Prefer a named two-case DU over a `bool` field, so the pattern match reads
`FTTuple(elems, TupleKind.Struct)` rather than `FTTuple(elems, true)`.

# Expect red

Tightening unification so struct and reference tuples are distinct will turn some green
SemanticAnalysis and Codegen tests red. Per the repo's reading guidance that is a finding, not
a regression: any test that passes only because `struct (int * int)` silently became a type
variable was pinning the dropped annotation. Read each one before changing it.

# The catch-all — DONE

`translateType`'s catch-all returned a fresh `TyVar` for **every** unmodelled type shape, so
any annotation with no case was ignored rather than diagnosed. That arm is gone: the match is
total over `Type`, the unmodelled shapes report `NotYetSupported` as the `SuffixedType` arm
already did for postfix type application, and two arms stay silent for stated reasons —
`MeasureType`, which carries no token to report at and reaches types through `TypeArg.Measure`,
and the `Missing` / `SkipsTokens` recovery pair, where the parse reported already.

Totality is the durable part. Adding a CST type case now fails to compile here instead of
silently yielding a type variable.

Pinned by three tests in `UnificationGenericsTests`, beside the postfix-application one they
follow: struct tuple, anonymous record, flexible type. Negative control run — restoring the
`TyVar` arms fails all three.

This does **not** give struct tuples a model. `struct (int * int)` is now refused instead of
silently dropped, which is the correct interim behaviour and makes the gap visible; everything
above is still the work to make it mean something.

# To confirm before implementing

- **Does the TS manifest surface struct-ness?** Default assumption is no.
- **Which diagnostic replaces the `NotYetSupported`** once struct tuples are modelled: the arm
  is deleted, and any test asserting the refusal changes with it.
