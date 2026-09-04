# Abstract-slot method parameters — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised while reviewing the comments on `1123c6cb` ("Use source provided names in generated
members"). The comment sweep changed no code; this does, which is why it is here.

---

# The defect

`AbstractMemberShape.abstractMethodParams` (`LayoutModel.fs`) emits the wrong **number** of
`Param` rows for an abstract slot whose signature mixes a tuple group with any other group.
The implementing member is emitted with the correct number, so the two disagree and the
interface method has no implementation. The assembly is structurally well-formed and fails at
type load.

```fsharp
type IMixed =
    abstract M: a: int * b: int -> c: int -> int
type Impl() =
    interface IMixed with
        member _.M(a: int, b: int) (c: int) = a + b + c
let r = (Impl() :> IMixed).M(1, 2) 3
```

Emitted `Param` rows: `IMixed::M` gets `["a"; "b"]`, `Impl::M` gets `["a"; "b"; "c"]`. Running
it:

```
TypeLoadException: Method 'M' in type 'Impl' from assembly '…'
does not have an implementation.
```

Names are a symptom, not the defect. Because `TAbstractMethodG.ParamNames` is indexed per
source argument and the slots are indexed per curried group, the names land on the wrong slots
even where the count happens to agree.

# Root cause

Two derivations of the same arity that disagree:

- **`ParamNames`** comes from `MemberRegistration.sigArgNames`, which walks the CST's
  `ArgsSpec` / `ArgSpec` and yields **one entry per source argument**, flattening curried
  groups and tuple elements alike.
- **The slots** come from `abstractMethodParams`, which re-derives them by `uncurry`ing the
  frozen `Signature` and yields **one entry per curried group**, leaving a tuple group
  unflattened — except for a sole tuple group, special-cased to flatten.

`FrozenType` does not record where the source drew its group boundaries, so the second
derivation cannot reconstruct what the first already knows. This is the `CLAUDE.md` rule about
a stage that discards an intermediate and makes consumers re-derive it: the disagreement now
has to be fixed by passing the intermediate, which is what should have happened when it was
written.

The `if m.ParamNames.Length = elems.Length then … else EqArray.empty` guard in the sole-tuple
branch is that disagreement being detected in the one shape where it was anticipated, and
papered over by dropping every name. The general branch has no guard at all.

# The rule the emitter should implement

`dotnet fsi` reflecting over the declarations, against what we emit today:

| signature | F# emits | we emit |
| --- | --- | --- |
| `M: a: int * b: int -> c: int -> int` | `M(a, b, c)` | `M(a, b)` — slot 1 is `c`, labelled `b` |
| `N: a: int -> b: int * c: int -> int` | `N(a, b, c)` | `N(a, b)` — slot 1 is the tuple, labelled `b` |
| `P: p: (int * int) -> int` | `P(p: Tuple\`2)` | `P(arg0, arg1)` — flattened, `p` dropped |
| `Q: a: int -> b: int -> c: int -> int` | `Q(a, b, c)` | `Q(a, b, c)` ✔ |
| `Item: i: int -> int with get, set` | `set_Item(i, <unnamed>)` | `set_Item(i, arg1)`, to become `set_Item(i, value)` |

One slot per source argument, flattening every curried group and every **unparenthesised**
tuple within a group. Parenthesisation is significant: `P` is the mirror-image error, where we
flatten what F# keeps as a single tuple-typed slot.

**One deliberate divergence: the tuple type in a non-flattened position.** F# emits
`System.Tuple\`2` there; we emit `System.ValueTuple\`2`. This needs no work — `FTTuple` already
encodes to the `ValueTuple` struct family throughout `ClrEncoder`, nesting through `TRest` past
`ClrTuples.MaxArity` — but it is the reason the `P` row above must not be pinned against F#'s
own metadata verbatim.

`p: struct (int * int)` in an abstract signature has no meaning today: the annotation is
dropped to a fresh type variable before it reaches any backend. That is its own scope, tracked
in `SemanticAnalysis/docs/struct-tuple-fidelity-plan.md`. **This plan does not wait on it** —
the arity fix is correct for reference tuples regardless, and once struct-ness is carried, a
struct tuple in a non-flattened position is one slot on exactly the rule below. The CLR
backend will keep emitting `System.ValueTuple` for both kinds either way.

`ExternalDeclarations.argSigOf` already states and implements exactly this rule for signatures
read out of reference metadata — "Every group flattened in source order, so `a -> b * c -> r`
⟶ `[a; b; c]`" — alongside `argGroupWidths` for the group view. Our own abstract slots are the
only signatures that do not follow it.

# Why the suite did not catch it

- `assertWellFormedMetadata` checks table ranges, nesting and pre-order contiguity. Override
  binding is not a table invariant, so the bad assembly passes every structural check.
- The abstract/impl conformance check in `Unification` compares **types**, and the types do
  agree: both halves are `int * int -> int -> int`. Semantic analysis is right here; codegen
  is wrong. No diagnostic is expected or wanted.
- No test corpus program and nothing in `src/Vesper.*` declares an abstract signature mixing a
  tuple group with another group, so the reachable set is empty today.

# Provenance

Pre-existing. The prior `abstractMethodParamTys` had the identical three branches; `1123c6cb`
layered names onto an already-misaligned slot set and added the `EqArray.empty` fallback. The
arity bug has been latent since the abstract-slot emitter was written.

CLR-only: `Codegen.Js` does not read `TAbstractMethod`. The fix nevertheless belongs partly in
`SemanticAnalysis`, so JS inherits the corrected shape when it grows interface support.

# The CST already has what the emitter needs

Parsing the five shapes and reporting each `ArgsSpec`:

| source | `ArgsSpec` |
| --- | --- |
| `A: p: (int * int) -> int` | 1 arg, named, `ParenType` |
| `B: a: int * b: int -> int` | 2 args, both named, `NamedType` |
| `C: (int * int) -> int` | 1 arg, anonymous, `ParenType` |
| `D: int * int -> int` | 2 args, anonymous, `NamedType` |
| `E: p: struct (int * int) -> int` | 1 arg, named, `StructTupleType` |

Two independent discriminators are preserved: the `ArgsSpec.args` count, and the
`ParenType` / `StructTupleType` wrapper on `ArgSpec.typ`. Parenthesisation is not a parser
fidelity problem — the grouping survives parsing intact and is discarded downstream, by
`MemberRegistration.sigArgNames` keeping only the names and by `abstractMethodParams`
re-deriving arity from `FrozenType`. Nothing in the parser needs to change.

# Plan

**Step 1 — carry the source groups on the abstract slot.**
Replace `TAbstractMethodG.ParamNames: EqArray<string voption>` with a per-group structure
carrying, for each source group, its kind (unit / simple / tuple) and the spelled name of each
argument in it. `ArgGroupG<'ty, 'pat, 'id>` is already generic over the pattern and id
parameters that a signature has no values for, so a signature-side instantiation is the
obvious candidate; confirm it fits before minting a new DU. Produce it in
`MemberRegistration.sigArgNames`, which already walks the `ArgsSpec` grouping and currently
throws it away.

A single `ArgSpec` whose `typ` is a `ParenType` or `StructTupleType` wrapping a tuple is one
group of width one, which is what makes the `P` row emit a single tuple-typed slot keeping its
name. That falls out of walking `ArgsSpec.args` rather than the type, so it needs no separate
step.

Carries through `TypeMemberInfo` → `Elaborate/TypeDecls.tryInterfaceMethods` →
`TAbstractMethodG` → `FrozenCodecTypes`. Bump `FrozenCodec.FormatVersion` again (it went to
`3` in `1123c6cb`).

**Step 2 — drive slots and names off one value.**
Rewrite `abstractMethodParams` to expand the carried groups into slots, returning a
`CompiledFns.FlatParams`-shaped value rather than a `(string * FrozenType) list`. `FlatParams`
is the type that already fixed the same class of bug on the static-function side — see **A1**
in `codegen-clr-followups-plan.md`, where a source-group index was used against the flat
parameter list and the fix was to hand each group its own flat slots. `uncurry m.Signature`
then supplies types only, and the arity agrees by construction: the `EqArray.empty` fallback
and the `ParamNames.Length = elems.Length` guard both disappear with nothing to replace them.

A `TMemberKind.Accessor` slot in the `Setter` role carries one trailing slot beyond what its
signature spells: name it `value`. The carried groups cover the index arguments; the value is
appended, so it needs no entry in them.

Three consumers must move together, since all three must agree on the slot set:
`AssemblerScaffold.abstractMethodSignature`, the interface-method loop in `Assembler`, and the
`ParamTys` of the method row in `LayoutNodes`.

**Step 3 — pin it, metadata first.**
`MetadataStructureTests` has `paramNamesOf` and `methodParamNamesOf` already. Add the five rows
of the table above as `Param`-row assertions; that is the direct structural statement of the
defect, and per this project's `CLAUDE.md` it comes ahead of loading the assembly.

Then add a `MetadataStructure` helper asserting that **an interface slot and the member
implementing it have equal parameter counts**, driven off the `MethodImpl` rows. Today's
structural checks cover table ranges, nesting and pre-order contiguity, and override binding
is the gap that let this through; a helper closes the whole class rather than the one shape.

Keep the `IMixed` / `Impl` program executed through `runEntryPoint` as the backstop, since the
`TypeLoadException` is the consequence a reader will recognise.

# Scope and risk

Small and well-fenced. The reachable set is empty in-tree, so no golden should move — if one
does, the shape it pins is one of the broken ones and the golden is the finding. The
`FrozenCodec` bump invalidates cached blobs, which is routine.

The one judgement call is Step 1's data shape, because `TypeMemberInfo` is read by resolution
as well as elaboration.

# Interaction with A19

**A19** in `codegen-clr-followups-plan.md` records that `TypeMemberInfo.ArgNames` is
meaningful for an abstract slot only, two of three `addMember` call sites passing
`EqArray.empty`, and proposes registering an abstract slot through its own record. This plan
subsumes it: Step 1 changes that field's type and its producer, so the split A19 asks for is
the natural place to land it. Do them together; strike A19 when this lands.

# Confirmed

- **Match F# on the flattening rule.** One slot per source argument, unparenthesised tuples
  flattened, parenthesised tuples kept whole.
- **`System.ValueTuple` in non-flattened explicit tuple positions**, where F# uses
  `System.Tuple`. Already how `FTTuple` encodes; the divergence is deliberate.
- **No parser change.** The CST distinguishes every shape, as the table above shows.
- **A setter's value slot is named `value`**, diverging from F#, which leaves the `Param` row
  unnamed. It is the slot the accessor's own signature never spells, so `argName i` is wrong
  for it and a positional mint tells a consumer nothing. Add it in Step 2 where the accessor
  slots are built.

# Still open

Nothing blocking.
