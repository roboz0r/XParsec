# CLR codegen improvements — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised by the decompiled-C# conformance goldens (`test/XParsec.FSharp.Codegen.Clr.Tests/goldens/*.clr.cs`),
added when `ConformanceByteIdentityTests` gained a whole-module render beside its structural
digest. Every finding below cites the golden that shows it, so each is reproducible by reading a
committed file. Nothing here has been implemented.

**Part A** is ABI and metadata defects. **Part B** is IL quality. **Part C** is the harness.

Two representation decisions are settled and drive A2 and A3: a record field emits as a private
`initonly` field with a public getter, and a typar constraint is enforced across assemblies and
written into IL wherever the CLI can express it.

---

# Part A — ABI and metadata

## A1. A ctor-param backing field is writable

`preamble-do-order.clr.cs` renders `Ordered` as:

```csharp
internal int n;
internal readonly int a;
internal readonly int b;
```

`a` and `b` are the instance-`let` preamble bindings; `n` is the primary-ctor parameter. The
`let`s are `initonly` and the ctor parameter is not.

**Root cause.** `buildClassNodes` in `LayoutNodes` gives a ctor-param field bare
`compilerGeneratedStorage`. The `val`-field and instance-`let` cases immediately below it add
`FieldAttributes.InitOnly` when the binding is immutable, and the `let` case states the rule in a
comment: written exactly once, by the primary `.ctor`, which is what `initonly` permits.

**Fix.** Add `||| FieldAttributes.InitOnly` to the ctor-param case. A ctor parameter has no
`mutable` form in the source language, so the case needs no condition.

**Risk.** `initonly` permits a store from any instance `.ctor` of the declaring type, so
secondary-ctor chaining is unaffected. Both write sites are already inside ctors: the primary
ctor's field stores and the secondary-ctor field-init block, both in `NominalEmit`. Before
landing, confirm the closure path only ever reads the field — a closure capturing a ctor
parameter loads it, and a `let mutable` promotes to `Vesper.Ref` rather than writing back — by
checking `EmitClosures` and `ClosureVerdictRewrite` for a store keyed on
`FieldKey.ClassCtorParamField`.

**Verify.** Extend the `ClassTests` assertion that already names both backing fields to require
`IsInitOnly`, and re-render the goldens.

## A2. A record field emits as a public writable field

`record-members.clr.cs`:

```csharp
public sealed class Vec : IEquatable<Vec>, IStructuralFormattable
{
	public int X;
	public int Y;
```

`struct-record.clr.cs` is the same shape one level worse — `public struct P` with two public
mutable fields, where the struct unions in the same golden set are `public readonly struct` with
`initonly` fields throughout.

**Decided representation.** A record field emits as a `private initonly` field with a public
getter, and a `mutable` record field as a `private` field with a public getter and setter. This
is F#'s own record representation.

**Root cause.** `buildRecordNodes` in `LayoutNodes` sets `FieldAttributes.Public` for every field
with no reference to `f.IsMutable`, and emits no accessor rows; the struct-record path adds no
`IsReadOnlyAttribute`.

**Fix, staged.** Each stage leaves the suite green on its own.

1. **Accessors, fields still public.** `buildRecordNodes` gains a `get_<Name>` method row and a
   `Property` row per field, plus `set_<Name>` for a `mutable` one. Bodies in `NominalEmit`:
   `ldarg.0; ldfld; ret`, which is the same shape for a struct record, where `ldarg.0` is already
   the byref. The accessor-name minting is `AccessorNames`, and nominal types already carry
   `Property` rows, so this stage adds no new machinery.
2. **Route every consumer through the accessor.** `RecordMember.Field` in `ICodegenProvider`
   currently means "the public field"; it becomes "the field's accessor pair", and each site
   minting a `FieldDef`/`MemberRef` from it mints a method reference instead. The sites are the
   field-get path, the field-set path, `buildRecordClone` in `EmitConstruct` (which `ldfld`s each
   non-overridden field off the spilled source), and the record-pattern destructure in
   `EmitPattern`.
   The synthesised members — structural equality, hash and `Format` — are emitted *on the
   declaring type*, so they keep direct field access. That exception is deliberate and wants a
   comment at the site: a private field is reachable from the type's own body and the accessor
   would only add a call.
3. **Privatise.** `Private ||| InitOnly`, and `Private` alone for a `mutable` field.
4. **`readonly struct`.** A struct record whose every field is immutable emits
   `IsReadOnlyAttribute`, mirroring `UnionLayoutNodes`. With getters in place this also stops the
   defensive copy the JIT would otherwise make at each getter call on a non-`readonly` struct,
   which is the stage that pays for itself.

Every write to an immutable record field already happens in a ctor: a literal is `newobj`, and
`{ r with X = v }` rebuilds through the ctor. So step 3 needs no new write-path work.

**Scope boundary.** The *symbol* stays a record field. `RecordMember.Field` keeps its name and
its key; only the CLR emission behind it changes, so SemanticAnalysis and the JS backend are
untouched by this entry.

**Risk.** The assembler predicts handles by prefix sum (see this project's `CLAUDE.md`), and
step 1 adds two or three rows per field per record across `MethodDef`, `Property` and
`MethodSemantics`. That is the failure mode to watch, and `MetadataStructure.assertWellFormed`
plus the digest goldens are the guard. Step 4 changes how a struct record passes where its
address is taken, so run the `Struct` and `StructSeq` suites, not only `Record`.

**Verify.** `RecordTests` currently asserts over `GetFields(Public ||| Instance)`; those
assertions inverting to `GetProperties` is the deliverable, not a regression. The
`{ p with Y = 99 }`, `c.Count <- 42` and record-pattern behavioural tests cover both sides of the
mutable split and must stay green throughout.

## A3. Typar constraints are never emitted

`typar-struct.clr.cs`, for a program whose source reads `let onlyStruct<'a when 'a: struct> (x: 'a) = x`:

```csharp
public static T0 onlyStruct<T0>(T0 arg0)
```

No `where T0 : struct`. The same holds in `typar-not-struct.clr.cs`, `typar-null.clr.cs` and
`typar-not-null.clr.cs`.

**Decided.** A constraint is enforced across assemblies and written into IL wherever the CLI has
an encoding for it.

**Root cause, and why this is not a codegen-only fix.** Three gaps stacked:

1. `FrozenConstraint` in `SideTypes` has exactly one case — `Coercion`, `when 'a :> ty`. Every
   other constraint the CST models (`Constraint.Struct`, `ReferenceType`, `DefaultConstructor`,
   `Unmanaged`, `Nullness`, `NotNull`, `Equality`, `Comparison`, `Enum`, `Delegate`,
   `MemberTrait` in `CstTypeWalk`) is checked during elaboration and then dropped. Codegen never
   sees it. This is the discarded-intermediate shape the root `CLAUDE.md` warns about, and it is
   the first thing to fix.
2. `AddGenericParameter` in `Metadata` passes `GenericParameterAttributes.None`
   unconditionally, and no code in `src/` emits a `GenericParamConstraint` row at all.
3. `CodegenOpenSignature.Constraints` in `ExternalSymbols` is the same one-case list, so a
   constraint on a *foreign* generic is not imported either. Enforcement against a BCL or
   third-party generic is therefore absent in the other direction too.

Constraints on our own packages do reach a downstream consumer today, through re-analysis of the
`.fsi` rather than through metadata — `typar-null-allownull.fs` in the corpus pins that the
`[<AllowNullLiteral>]` answer survives the freeze. IL encoding is what a foreign consumer needs.

**Target encoding.** One row per source constraint, to be calibrated against what `fsc` emits for
the same source before it is committed to — F# parity is the goal, and `dotnet fsi` plus a
decompile of an `fsc` output is the oracle:

| Source constraint | CLI encoding |
| --- | --- |
| `'a : struct` | `NotNullableValueTypeConstraint ||| DefaultConstructorConstraint` + a `GenericParamConstraint` to `System.ValueType` |
| `'a : not struct` | `ReferenceTypeConstraint` |
| `'a : (new : unit -> 'a)` | `DefaultConstructorConstraint` |
| `'a :> Ty` | `GenericParamConstraint` to `Ty`, class or interface alike |
| `'a : enum<'u>` | `GenericParamConstraint` to `System.Enum` — confirm against `fsc` |
| `'a : delegate<_,_>` | `GenericParamConstraint` to `System.Delegate` — confirm against `fsc` |
| `'a : unmanaged` | value-type flags + `IsUnmanagedAttribute` on the parameter |
| `'a : null` / `not null` | none; nullability attributes are the only carrier, and they belong with the wider nullability work |
| `'a : equality` / `comparison` | none — F# has no CLI encoding for these either |
| SRTP member trait | none — an `inline` binding resolves it at the splice, so no generic parameter survives to carry it |

**Fix, staged.**

1. Widen `FrozenConstraint` to the kinds above, and carry them through `Freeze` and the pool
   codecs. The codec change is a format change, so it lands alone.
2. Flag bits on the existing `GenericParam` row: `struct`, `not struct`, `new()`. No new table.
3. `GenericParamConstraint` rows, which needs the table added to the assembler with its row-order
   prediction — the same prefix-sum discipline as every other table here.
4. Import the same constraints in `ExternalSymbols` so a foreign generic's constraint is
   enforced at our use sites.
5. `unmanaged` and the nullability attributes, in that order, each with its own scope.

**Verify.** Assert `GenericParam` flags and `GenericParamConstraint` rows through the
`MetadataStructure` helpers, per this project's `CLAUDE.md` preference for metadata over
reflection. The `typar-*-violated.fs` programs already pin front-end rejection and must keep
their exact diagnostics. The goldens re-render into `where T0 : struct`, which is the readable
check that stage 2 and 3 agree.

## A4. An inline splice's temporary becomes a public static field

`typar-struct.clr.cs`, from a source whose only statements are `ignore i` and `ignore b`:

```csharp
public static readonly int value$8;
public static bool value$9;
```

**Root cause.** `Inline.betaReduce` lowers each inline application argument to a `TExpr.Let`
(see B1). At top level a `let` with no exportable identity takes the residue mint from
`residueEmission` in `EmitClosures` — `value$<slot>` — and top-level values emit as static
fields on the `Program` class. So a temporary introduced by splicing `ignore` lands in the
assembly's public surface under a name no source wrote.

**Fix.** Two independent halves, either of which helps:

- Residue storage takes assembly visibility rather than `public`. A name minted because nothing
  in the source names the value cannot be part of an intended ABI.
- B1's substitution removes the binding here outright, since the argument is a `Var`.

**Verify.** A test that no `TypeDef` exposes a `public` field whose name contains `$` would pin
the first half across the whole conformance corpus.

---

# Part B — IL quality

## B1. Every operand of an inlined operator is spilled twice

`arith-int.clr.cs`, for `printfn "%d" (2 + 3)`:

```csharp
int num = 2;
int num2 = 3;
int num3 = num;
int num4 = num2;
((Formatter)(ref val)).AppendFormatted<int>(num3 + num4);
```

Four locals for two constants, and the same shape on every arithmetic line of every `arith-*`
golden. `preamble-do-order.clr.cs` shows it over a field read: `let a = n + 1` spills `this.n`
and `1`, then spills both copies again.

**Root cause.** `Inline.betaReduce` lowers each argument of an inline application to a
`TExpr.Let`, and `buildLet` in `EmitBindings` gives every `Let` a local and an `stloc`. The
primitive operators inline through two levels — `Vesper.Core`'s `let inline (+)` in
`ops-platform.clr.fs` delegates to a trait-resolved `static member`, itself spliced — so each
operand collects one binding per level.

**Fix.** Substitute in `betaReduce` rather than binding, when the argument is atomic:

1. A literal, which is unconditionally safe.
2. A `Var`, which is safe when the variable is not assigned between the binding and the use.
   The `[<CallAtMostOnce>]` machinery beside it (`substituteVar`) is the existing precedent for
   the substitution itself; the occurrence condition is what differs.
3. A field read is the tempting third case and the one to leave alone until 1 and 2 land,
   because it needs an effect ordering argument the first two do not.

**Scope note.** RyuJIT already folds `stloc`/`ldloc` copy chains, so the payoff is IL size and
reviewability, not throughput. Do not attach a performance claim to this without a benchmark.

**Also check.** `betaReduce` lives in SemanticAnalysis, so the JS backend inherits the same
bindings; confirm what its emitted JS does with them before choosing where the fix goes.

## B2. A unit-valued call in statement position reifies `()`

Every `printfn` in every golden is followed by:

```csharp
ValueTuple valueTuple = default(ValueTuple);
```

**Root cause.** `buildUnitValue` in `EmitTypes` allocates a local, `initobj`s it and pushes it
whenever a call's `CallResult` is `Void`. In statement position the pushed value is then
discarded.

**Fix.** Distinguish value position from statement position on the path that reaches
`CallResult.Void`, so a statement-position void call pushes nothing and needs no pop. The
callers to audit are the `buildUnitValue` sites in `EmitCall`, `EmitFormat`, `EmitIntrinsic`,
`EmitLoops` and `EmitMember`.

**Payoff.** One local slot and three instructions per statement, and one line of noise per
`printfn` out of every golden — which is what makes the remaining diff worth reading.

---

# Part C — harness

## C1. `Formatter` renders as `((Formatter)(ref val))..ctor(...)` — a resolution artifact

Read as C#, `preamble-do-order.clr.cs` looks like a constructor invoked on a cast address, which
reads as suspect IL. It is not.

What the emitter writes is `ldloca slot; ldc; ldc; [sink]; call instance void Formatter::.ctor`
(`buildFormat` in `EmitFormat`) — the same sequence C# itself emits for
`Formatter val = new Formatter(...)`. The rendering degrades because `Vesper.Printf.dll` cannot
be resolved: `decompilerOf` in the tests' `Decompile` seeds `UniversalAssemblyResolver` from
`AppContext.BaseDirectory`, while the Vesper packages build into repo-root `tmp/pkg-Vesper.*`.
With `Formatter` unresolved, ILSpy cannot know it is a value type and prints the raw address-call
form. A struct defined in the program under test renders normally in the same corpus —
`return new Shape(1, payload);` in `struct-union.clr.cs`.

Two independent confirmations that the IL is valid: these conformance programs run and match
their `.expected` output, and `MetadataStructure.assertWellFormed` passes over them.

**Fix.** Seed the resolver with the Vesper package output directories, or copy those DLLs beside
the test assembly, then regenerate. Expect `Formatter val = new Formatter(7, 1, Console.Out);`
and plain `val.AppendLiteral("ctor a=")` calls afterwards, which also shrinks every golden.

Until it lands, `..ctor` in a golden means "unresolved reference", not "defect".

## C2. Extending the decompiled goldens past the conformance corpus

The conformance render cost nothing measurable (44 decompiles inside a suite that runs in two
minutes either way), and it produced Part A on the first read. The next candidates, in order of
value per unit of work:

- `StructTests` and `StructSeqTests`, whose programs already live in `data/`, so a corpus entry
  and a type name are the only additions.
- `ClassTests`, where roughly 60 tests assert metadata shape alongside a runtime `Invoke`, and
  where near-identical programs are compiled two and three times over under different assembly
  names to assert different facets of the same emission.

A golden replaces a shape assertion, never a behavioural one. It cannot see IL prefixes, opcode
choice, local signatures, table row order, duplicate mints, or assembly references, so the
digest gate and the `expectNoFSharpCore` checks stay.

---

# Sequencing

A1 is a one-token change and lands first, alone.

A3 stage 1 widens a frozen type and its codec, so it wants a commit of its own before anything
depends on it. A2 stage 1 and B2 both move assembler row counts and should not be in flight at
the same time as each other, because a failure in the handle predictions is far easier to read
against one of them than against both.

B1 lands whenever convenient; it removes bindings rather than rows, and A4's first half is
independent of it.

C1 is worth doing before any of the above, because every re-render of the goldens is easier to
review once `Formatter` resolves.
