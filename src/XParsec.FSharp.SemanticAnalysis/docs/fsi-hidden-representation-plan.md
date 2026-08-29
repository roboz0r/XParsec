# `.fsi` hidden representations — semantics record and design gate

Status: **no implementation is scheduled.** This document records the fsc semantics probed
on 2026-08-29 and the tripwire that landed with them. Adopting the model needs a proper
design first (see "What a design must answer"); do not implement from this document alone.

## The construct

A bodied `.fsi` type with members but no representation:

```fsharp
type C =
    member P: int
```

No ctor, no fields, no cases, not all-abstract. In F# this *hides the representation*: the
paired `.fs` may implement `C` as a class, record, struct or union, and a consumer of the
signature can only call the members. It is the standard F# idiom for exposing an API while
keeping a record/union representation private, so FSharp.Core-style surfaces use it; the
Vesper contracts currently do not (their bodied types all carry `new:` ctors, are
all-abstract, or are `extern`/abbrev/union/record — enumerated 2026-08-29).

## fsc semantics (probed with a scratch `.fsi`+`.fs` pair and `dotnet fsi`)

- **Kind inference is syntactic.** An `inherit` clause never forces class kind; a body
  whose members are all abstract is an interface even when the inherit target is a class
  (the target's kind only picks the diagnostic). Both front ends of this compiler now match
  that rule (`TypeDefnPatterns.bodyIsInterface` / `isInterfaceShape`).
- **Interface kind + `inherit <non-interface>`** ⇒ FS0887 "The type 'B' is not an
  interface type".
- **Class kind (ctor present) + `inherit <interface>`** ⇒ FS0946 "Cannot inherit from
  interface type. Use `interface ... with` instead." — on the `.fsi` itself.
- **Memberful-no-ctor + `inherit`** ⇒ FS0931 "Structs, interfaces, enums and delegates
  cannot inherit from other types" *plus* FS0938 "The representation of this type is hidden
  by the signature. It must be given an attribute such as `[<Sealed>]`, `[<Class>]` or
  `[<Interface>]` …". fsc refuses to infer a kind for a hidden-representation type when a
  check needs one, and demands the author state it. fsc can keep the surface vague because
  it compiles the pair together: the real representation still reaches codegen and metadata.

## What this compiler does instead

Signature resolution publishes the memberful-no-ctor form as an **unsealed reference
class** (`ExternalClassShape`, `IsInterface = false`). The guess is wrong exactly when the
paired `.fs` implements the type as a record, union or struct: consumers of the published
surface would carry the signature's kind (equality semantics, value-typeness, calling
conventions, inheritability) while the emitted type has the implementation's.

## The tripwire (landed)

`Conformance.TypeKindFamily` + `ConformanceError.TypeKindMismatch`: the syntactic
`.fsi`/`.fs` pair check compares the coarse nominal family (class / interface / record /
union / enum) wherever both sides commit to one, using the same `bodyIsInterface` /
`isInterfaceShape` judgments resolution files types under. Abbrevs, `extern`s, opaque
`type T`, delegates and type extensions commit to none and are skipped; the `struct` form
groups with class because `[<Struct>]` rides an attribute the syntactic summary cannot
read. The silent-divergence hole is now a report; pinned in `ConformanceTests`.

## What a design must answer before implementing the model

1. **Where the kind truth comes from.** This pipeline's later files consume the published
   `.fsi` surface, not the impl, so a hidden kind cannot simply be omitted — codegen and
   unification need struct-ness and sealedness. The natural channel is the existing
   `Reprs`-style pairing (the signature declares, the paired `.fs` supplies), *not* fsc's
   demand-an-attribute model, which mostly serves human readers.
2. **The published shape.** A new "opaque nominal with members" case vs a class shape
   carrying an impl-supplied kind — and what each consumer (unify, conformance of
   `interface … with`, both backends) reads off it.
3. **Construction and matching.** A hidden representation exposes no ctor and no pattern
   surface; the member-only view must be enforced at use sites in later files of the same
   assembly, where today the registry would leak the impl detail.
4. **Which attribute-stated facts to honour** (`[<Sealed>]`, `[<Class>]`, `[<Interface>]`,
   `[<Struct>]`) if the signature wants to commit without the impl, and how they interact
   with the tripwire's families.

Revisit trigger: a Vesper port that wants to hide a record/union representation behind a
member-only signature, or the tripwire firing on a real pair.
