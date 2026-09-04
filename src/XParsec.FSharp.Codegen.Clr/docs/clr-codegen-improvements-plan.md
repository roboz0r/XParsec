# CLR codegen improvements — plan

Working document. Ephemeral: delete it when the work lands.

Line numbers are deliberately absent — they rot. Constructs and file names only.

Raised by the decompiled-C# conformance goldens (`test/XParsec.FSharp.Codegen.Clr.Tests/goldens/*.clr.cs`),
added when `ConformanceByteIdentityTests` gained a whole-module render beside its structural
digest. Every finding below cites the golden that shows it, so each is reproducible by reading a
committed file. A1, A2, A4, A5, B1, B2 and C1 have landed; the rest is outstanding.

**Part A** is ABI and metadata defects. **Part B** is IL quality. **Part C** is the harness.

Two representation decisions are settled and drive A2 and A3: a record field emits as a private
`initonly` field with a public getter, and a typar constraint is enforced across assemblies and
written into IL wherever the CLI can express it.

---

# Part A — ABI and metadata

## A1. A ctor-param backing field is writable — DONE

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

**Landed.** `buildClassNodes` adds `FieldAttributes.InitOnly` to the ctor-param case. The only
handles minted from `FieldKey.ClassCtorParamField` are the three in `NominalEmit` — the class's
`Fields` lookup list, the primary ctor's `stfld` refs, and the secondary-ctor
`ExplicitFieldInit` targets — so every store is inside a `.ctor`; `EmitClosures` and
`ClosureVerdictRewrite` mint none. Seven `preamble-*` and `typar-null-allownull` goldens
re-rendered to `internal readonly`, digests included, and the suite is green.

A closure capture field took the same treatment and went further, to `private initonly`.
`FieldKey.ClosureCapture` is minted once, in `PrepareClosures`, and reaches exactly two
consumers: the `stfld` list of `buildChainedCtor` / `buildStructCtor`, and the `CaptureFields`
dictionary whose only reader is `buildVarLoad`'s `ldarg.0; ldfld`. Both live on the closure
type, so the field needs no visibility beyond it, and the `FieldAccessException` hazard behind
`assembly` storage does not apply — that one is about a closure reading its *enclosing* class's
storage.

Both tightenings diverge from FSC, which emits a ctor-param backing field as writable
`assembly` (`PrintfFormat::value` in FSharp.Core) and a capture as a writable `public` field
(`QueryModule+restoreTupleProjections@353-1::v`).

**Follow-on, also landed.** Reviewing the above surfaced a third case with the identical
argument: an immutable class `static let` was emitted writable, because `buildClassNodes`
dropped `sl.IsMutable` while the instance-`let` case four lines above consulted it. Its stores
are the `.cctor` `PreambleStep.Store` in `NominalEmit` and `buildStaticFieldSet`'s `stsfld`,
and the latter is reachable only for a binding the front end saw as `mutable`. `ClassTests`
now pins the immutable and mutable forms against each other. No golden moved, because the
conformance corpus spells `static let mutable` and never the immutable form.

Nothing caught that omission, so the visibility and write-once bits moved out of the field
literals and into `FieldAttrSets` in `LayoutModel`, beside the `MethodAttrSets` that already
held the method-attribute vocabulary. Every `FieldSlot` in `LayoutNodes`, `Layout` and
`UnionLayoutNodes` now draws `Attrs` from `instanceFieldAttrs` / `staticFieldAttrs` over a
`FieldReach` and a `FieldWrites`, so a builder states both facts or fails to compile. The
`compilerGeneratedStorage` binding named in the root cause above is gone; `FieldReach.Assembly`
carries its rationale, adjacent to the `FieldReach.OwnType` case that a capture takes.

## A2. A record field emits as a public writable field — DONE

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

**Root cause.** `buildRecordNodes` in `LayoutNodes` gives every field
`instanceFieldAttrs FieldReach.Public FieldWrites.Anywhere`, discarding `f.IsMutable`, and emits
no accessor rows; the struct-record path adds no `IsReadOnlyAttribute`. Stage 3 below moves both
bits to `FieldReach.OwnType` and `writesOf f.IsMutable`.

**Fix, staged.** Each stage leaves the suite green on its own.

1. **Accessors, fields still public — DONE.** `buildRecordNodes` gains a `get_<Name>` method row and a
   `Property` row per field, plus `set_<Name>` for a `mutable` one. Bodies in `NominalEmit`:
   `ldarg.0; ldfld; ret`, which is the same shape for a struct record, where `ldarg.0` is already
   the byref. The accessor-name minting is `AccessorNames`, and nominal types already carry
   `Property` rows, so this stage adds no new machinery.
2. **Route every consumer through the accessor — DONE.** `RecordMember.Field` in `ICodegenProvider`
   currently means "the public field"; it becomes "the field's accessors", and each site
   minting a `FieldDef`/`MemberRef` from it mints a method reference instead. The sites are the
   field-get path, the field-set path, `buildRecordClone` in `EmitConstruct` (which `ldfld`s each
   non-overridden field off the spilled source), and the record-pattern destructure in
   `EmitPattern`.
   The synthesised members — structural equality, hash and `Format` — are emitted *on the
   declaring type*, so they keep direct field access. That exception is deliberate and wants a
   comment at the site: a private field is reachable from the type's own body and the accessor
   would only add a call.
3. **Privatise — DONE.** `instanceFieldAttrs FieldReach.OwnType (writesOf f.IsMutable)`.
4. **`readonly struct` — DONE.** A struct record whose every field is immutable emits
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

**Stage 1 landed.** `buildRecordNodes` emits a `get_<Name>` method row per field, a `set_<Name>`
beside it for a `mutable` one, and a `Property` row binding the pair through `MethodSemantics`.
`NominalEmit.prepareRecord` supplies the bodies from `Emit.buildFieldGetter` and the new
`buildFieldSetter`, over the field refs the `.ctor` already mints, so a generic record's accessors
reach the field through the `MemberRef` on the open self-`TypeSpec`. `getterAttrs` became
`synthAccessorAttrs`, covering both halves.

Handle prediction absorbed the new rows unchanged: `MetadataStructure.assertWellFormed` passed
and every digest but the three records' held. Two pinned row lists gained their getters — `Point`
and `M+Tally` in `MetadataStructureTests`, `N.Outer+Inner+T` in `LocalModuleTests` — and
`record-members`, `struct-record` and `typar-struct-record` re-rendered. Those three now show
`public int X;` beside `public int X => this.X;`, with `this.` disambiguating every field read: a
field and a property may share a name in metadata, and C# cannot spell it. Stage 3 privatises the
field and the rendering resolves.

`PropertyRowTests` pins the rows and the binding: the row pair per field with the mutable split, a
reflected round-trip through both halves, the second field of a generic record (the slot a raw
`FieldDefinition` token resolves wrongly), and a struct record's getter over the byref `this`.

One consequence to carry into stage 2: `MetadataSymbols.enumerateClassMembers` walks properties
and public fields alike, so a record field imported from a referenced assembly now yields two
`ExternalMember`s under one key. The property leads, which is the member stage 2 wants a consumer
to bind; stage 3 drops the field from the public surface and with it the duplicate.

**Stage 2 landed.** `RecordMember` kept `Field`, which the record's `.ctor`, accessor bodies
and structural triple still mint, and gained `Accessor of fieldName * TAccessorRole`, the same
`(name, role)` shape as `TMemberKind.Accessor` and `MethodKey.RecordFieldAccessor`.
`ClrGenerics.genericRecordMemberRef` encodes it on the instantiated `TypeSpec` through
`ClrEncoder.RecordAccessorSignature`, the one place that spells `instance FieldTy get_X()` and
`instance void set_X(FieldTy)`; the declaring side in `NominalEmit` and the imported side in
`ClrExternalMembers.externalRecordField` share it. `EmittedRecordField` in `EmitTypes` replaced
the `(name, handle, type)` triple and carries the accessor `Def` pair beside the storage.

A use site resolves one accessor by role, never the pair: `EmitResolve.recordFieldAccessor`
re-mints the requested half, and `resolveRecordField` answers a `FieldAccess` for that role,
`Storage` for a class field, `Accessor` for a record field, own or imported. A read therefore
mints no `set_X` `MemberRef` row on a generic or referenced-package record, which
`PropertyRowTests` pins. `Vesper.Ref` reaches `contents` through `get_contents` /
`set_contents`.

The four sites read `FieldAccess`: `buildFieldGet` and `buildFieldSet` in `EmitMember` `call`
the half they asked for, `buildRecordClone` calls each un-overridden field's getter on the
spilled source, and the record pattern in `EmitPattern` calls each named field's getter on the
scrutinee through the `extractGetter` it shares with a union case's `Getter` reader. A struct
record's getter takes the address, so those sites go through `EmitTypes.loadSlotAsThis` and
`buildFieldGet` through `loadStructThisPtr`. `loadStructThisPtr` addresses a struct module
value with `ldsflda` and a struct-typed class field with `ldflda` (the `ClassFieldGet` active
pattern); a record field or property result is a copy with no location and spills, as F# does.
`NominalShared.recordFieldRefs` keeps direct field access with the comment the entry asked for.

`record-members` and `struct-record` re-rendered, the former to bare `X` where `this.X` had
disambiguated a field read from the property. `PeInspection.peMethodMemberOps` decodes a
body's member-bearing instructions to `(mnemonic, member name)` over the
`System.Reflection.Emit.OpCodes` table, and `PropertyRowTests` pins each site through it: get,
set, clone, pattern, the generic second-field `MemberRef`, the struct record's getter and clone
with a runtime round-trip, and a promoted `let mutable` reaching `Vesper.Ref` through
`get_contents`.

**Stage 3 landed.** A record field's backing field is `private`, `initonly` unless `mutable`, and
takes FSC's `X@` name from `RecordBackingField.metaName` in `LayoutModel`. The name is
decided in the layout layer: `buildRecordNodes` writes the `FieldDef` row and
`NominalRegistration` stores it as `GenericRecordField.MetaName`, which `ClrGenerics` reads back
for the `RecordMember.Field` `MemberRef`. `FieldKey.RecordField` still keys on the source name.
`PropertyRowTests` pins the two `FieldAttributes` sets; the shape assertions in `RecordTests`,
`StructTests`, `SelfHostTests` and the pinned row lists read the property or the suffixed
backing field.

**Stage 4 landed.** `Assembler.readOnlyMarkerOf` stamps `IsReadOnlyAttribute` on a value
type whose every instance field is `initonly`, read off the `FieldSlot.Attrs` the layout
already carries, so the record, struct union and case view arms share one rule and the layout
model is unchanged. FSC infers no such attribute; the divergence is sited on the helper.
`StructTests` pins the three answers — an all-immutable struct record, one with a `mutable`
field, and a reference record — off one compile.

`struct-record` and `typar-struct-record` re-rendered to `public readonly struct`, and their
digests moved with the `IsReadOnlyAttribute` `TypeRef` and ctor `MemberRef` the fold walks.
The IL did not move: `struct-record`'s `{ a with X = 10 }` still reads `ldsfld a` and calls
`get_Y` on the spill, which ILSpy had been rendering as `a.Y` and now spells as the copy it is.

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

## A4. An inline splice's temporary becomes a public static field — DONE

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
- B1's substitution removes the binding here outright, since the argument is a `Var`. **Landed**:
  `value$8` and `value$9` are gone from `typar-struct.clr.cs`, and the JS counterpart `const _s5`
  from the six `typar-*` JS goldens. The visibility half above still stands on its own, covering
  a residue whose argument is not atomic.

**Verify.** One corpus-wide field-visibility sweep, over every PE the conformance suite emits
rather than over a hand-written source. It carries two assertions:

1. No `TypeDef` exposes a `public` field whose name contains `$`. This pins the first half of
   the fix above.
2. Every field named `capture<i>` on a `<closure>$*` `TypeDef` is `private initonly`.

The second assertion is not about splice residue; it belongs here because it needs the same
harness. `LayoutNodes.buildClosureNodes` gives a capture `private initonly` on the strength of
an invariant that spans the backend — `FieldKey.ClosureCapture` is minted once, in
`PrepareClosures`, and the `CaptureFields` dictionary has one consumer, `buildVarLoad`, which
emits `ldfld` and no store. A regression arrives as a NEW lowering that reads or writes a
capture from another type, and the single-source test in `CapturedMutableTests` cannot see one:
it compiles two fixed closures. A sweep over the corpus covers every closure shape the suite
already exercises — generic, `Stack`-repr, cached, and closure-inside-closure.

Where a test asserts over one program's captures, pin the expected set by name, so a lowering
change that stops emitting a closure fails rather than silently narrowing the assertion to
fewer fields.

**Landed.** `EmitTypes.ValueIdentity` states whether the source spells the name a top-level
binding emits under. `EmitClosures.Emission` carries it — `Declared` from `declaredEmission`,
`Residue` from `residueEmission` — and each `ModuleValue` takes it from its `Emission`, so a
builder cannot mint storage without deciding. `Layout.moduleValueField` builds every module
value's `Field` row — a named module class's, the Program class's `.cctor`-written and its
`Main`-written — and maps the identity to a `FieldReach` there. Residue storage is therefore
`assembly`, declared storage `public`.

`FieldVisibilitySweepTests` compiles every CLR-gated corpus program once and reads back every
`TypeDef`'s `Field` rows through `MetadataStructure.allFieldAttrs`. Both assertions above are
there: no public field carries `$` in its name, and every `capture<i>` on a `<closure>$*` type
is `private initonly`, with a non-empty check so the capture sweep cannot pass vacuously.

No corpus program shadows a top-level binding, so the residue mint gets two sources of its own
in the same file, one per storage kind: `let x = 1; let x = 2` pins `x$0` as
`assembly static initonly` beside a public `x`, and the same shadowing after a top-level `do`
pins the `Main`-written pair as plain `static`. The front end rejects `let x = 1` followed by
`let x = x + 10` at file scope with "Unresolved identifier: x", so the rebinding cannot read
what it shadows.

No golden moved and no digest moved, which is the confirmation that the corpus emits no residue
field today — B1 removed the ones `typar-struct` had.

## A5. Every parameter is named `arg<i>` — DONE

`preamble-fn-value.clr.cs`, for `let twice (f: int -> int) (x: int) = f (f x)`:

```csharp
public static int twice(Fun<int, int> arg0, int arg1)
```

They should read `f` and `x`. The same golden renders `Twice(int arg0)` for
`member this.Twice(n: int)` and `<closure>$0(Adder arg0)` for the capture of `this`. A
parameter name is ABI — a C# consumer writes `twice(f: g, x: 3)` — and it is what a debugger
and a decompiler display.

**Root cause.** `argNames` in `AssemblerScaffold` mints `arg0 … arg{n-1}` from a count. Six
`Prepared.ParamNames` sites call it, and the closure `Invoke` writes the same `arg%d` inline;
each passes a length where the source name is one dereference away.
`TastPoolBuilder.boundVarNaming` answers `Source name` or `Minted slot` for a
`BoundVarId`, which `residueEmission` in `EmitClosures` already reads, and every parameter
model carries its `BoundVarId`. The JS backend names all of these from that same pool
(`paramNameOf` in `JsFlatFns`, `boundVarNameOf` in `EmitJsMembers`), so the CLR backend is
discarding an intermediate its sibling consumes.

**Fix, staged by site.** Each stage is independent, and the first covers the golden above.

1. A module function, in `Assembler`'s static-fn prep. `fn.Params.Flat` carries a
   `StaticParam` per emitted slot, and one with `Pat = None` names from its `Slot`.
2. A member and a secondary ctor, both in `NominalEmit`, whose `Params` are
   `BoundVarId * FrozenType` pairs. The primary ctor and the record ctor already name from
   `CtorParams` and from the field list.
3. A closure's `.ctor` and `Invoke`, in `PrepareClosures` — `argNames` over the captures, and
   an inline `arg%d` over `FunArity` beside it. A capture is a `BoundVarId`, so the ctor
   parameter can carry the captured variable's name.
4. A union factory, in `UnionEmit`: `argNames arity` sits beside the `ud.FieldNames c` the case
   ctor already passes for the same vector.
5. An abstract interface method, in `Assembler`'s interface prep. `Frozen.TAbstractMethod`
   holds a `Signature` and no names, so this one needs the name carried through the freeze
   first and is the only site that is not a codegen-only change.

Stages 1 to 3 read the pool and want one helper beside `argNames`, taking the `BoundVarId` and
the slot index, so the `Minted` fallback to `arg<i>` is written once. Stage 4 reads the case
field names instead.

**Decided.** A `Param` name is the source name verbatim, and a double-backtick binding
contributes the name it spells, so ``` ``my param`` ``` emits as `my param`, space and all.
Metadata permits any string there, and a `let` binding a double-backtick parameter compiled
under `fsi` reflects its parameter as `my param`, so this is F# parity. It costs no work at
this end: `GetIdentifierSpan` in `Lexing` strips the quoting at the token, so
`BoundVarNaming.Source` already carries the bare name.

Where the source supplies no name, the generated positional name stands. That is
`BoundVarNaming.Minted`, which covers a destructuring parameter's synthetic slot, and it keeps
`arg<i>`.

**Risk.** `Param` row counts are unchanged, so handle prediction is untouched.

**Verify.** The goldens re-render `twice(Fun<int, int> f, int x)`. Pin the `Param` rows rather
than the rendering, through a `paramNamesOf` helper beside `methodAttrsOf` in
`MetadataStructure`.

**Landed.** `ParamNaming.paramNames` (in `LayoutModel`) reads `TastPoolBuilder.boundVarNaming`
off `EmitContext.Pool` and falls back to `argName i`, the one spelling of the positional name.
A module function, a member, a secondary ctor and a closure's `.ctor` / `Invoke` all take it
over their bound-variable keys; a union factory takes `ud.FieldNames c`. An abstract slot's
names travel from the front end on `TAbstractMethodG.ParamNames` (one `string voption` per
source argument, `FrozenCodec.FormatVersion` 3), and `abstractMethodParams` pairs each
metadata slot with its name and type in one place. `MetadataStructureTests` pins every site,
and `MetadataStructure.paramNamesOf` / `methodParamNamesOf` read the `Param` rows back.

---

# Part B — IL quality

## B1. Every operand of an inlined operator is spilled twice — DONE

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

**Landed.** Both `betaReduce`s bind every argument, as before: `Inline.betaReduce` pre-freeze
for a call-site lambda fused into a template body, `InlineExpand.betaReduce` post-freeze for
each `TExprG.InlineCall` edge. One pass, `InlineExpand.reduceLets`, then collapses every
`let` in the expanded declarations whose bound variable is immutable and whose value is
`substitutable` over the body: a `Const`; a `Var` referencing an immutable variable; or a
`Var` referencing a local mutable variable that the body never assigns and that no lambda in
the body would capture through the bound variable. A module-level mutable stays bound, being
writable by any call. A field read stays bound. The binding snapshots a value where
substitution would re-read it at each use, including inside a closure the body returns. The
pass runs inside `InlineExpand.expand`, so both backends and every consumer of an expansion
see the same trees under one rule.

Mutability is a fact of the bound variable and lives on its definition site:
`TPat.NamedSimple` and the pooled `PatPayload.NamedSimple` carry `isMutable`, set by
`translateBindingPat` from the `let mutable` keyword and cleared by `RefCellPromotion` when
the variable becomes a cell. The frozen pool projects it to the `BoundVarMutable` column,
filled from the pattern payloads, so a `Var` can read it through
`TastPoolBuilder.boundVarIsMutable`. A lambda parameter, a match binding and a `use` bind
immutably. `JsEmitHelpers.reduceInlinableLet` builds on `InlineExpand.substitutable`,
extending it through pure intrinsics and simple `let`s for the JS expression form.

The JS goldens moved where a binding reached the module surface: the six `typar-*` programs
lost the `const _s5 = s;` that `ignore`'s splice residue produced, which is A4's second half.
`typar-struct.clr.cs` lost the `value$8` and `value$9` fields A4 names, leaving A4's first half,
assembly visibility for residue storage, as the only part of that entry still outstanding.

`InlineExpandTests` pins both halves post-freeze: an immutable operand spends no binding, a
mutable one keeps its own. Each backend has a runtime test where a mutable local is passed to
an inline function that returns a closure and is written after the call.

## B2. A unit-valued call in statement position reifies `()` — DONE

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

**Landed.** `ExprPos` in `EmitTypes` carries the distinction and `EmitExpr.buildExprAt` is the
one dispatcher over it. An arm that takes `pos` materialises its `unit` only where a consumer
takes the value; every other arm's value goes to the trailing `ExprPos.discardTo`. Statement
position therefore returns the operand stack to its entry depth, and that invariant is what lets
a join emit its arms at the position it was itself given: `buildIfThenElse` hands both branches
the same `pos`, and `buildMatch` marks its end label at `baseDepth + pos.Pushes`.

`RecurAt` in `EmitDispatch` is the back-edge for an arm whose own position reaches a
sub-expression — a `let` / `use` / `try`-`finally` body, a `Sequential`'s last item, a branch of
a join, a `StaticOptimization`'s fallback. `Recur` still means value position, so an argument,
a scrutinee and a guard are unchanged. The `unit` sites that now read `pos` are the ones named
above plus `TConstValue.Unit` in `EmitExpr` itself.

Six discards went with it: `buildSequential`'s non-last item, the three loop bodies,
`buildUse`'s disposal call and `buildTryFinally`'s cleanup. A statement-position `try`/`finally`
also stops parking its body's result, so `buildTryFinallyRegion` mints the result local only in
value position. A `void` body in `Emit` (`buildStaticFn`, `buildMember`) emits at
`ExprPos.ofReturnsVoid`, and a preamble `do` is a plain `buildStatement`; the per-site
"values left on the stack" checks those sites used to carry are gone, since `discardTo` is the
one place that keeps the depth invariant.

44 conformance goldens re-rendered 253 lines lighter with no line added, and every `.clr.txt`
digest moved. `StatementPositionTests` pins both sides: a `void` member whose body is `()` is a
bare `ret` declaring no locals, a `void` body of unit-valued calls declares none either, and a
`unit` filling a tuple slot is still materialised. The local count comes off the body's
`LocalVarSig` through `PeInspection.peMethodLocalCount`.

---

# Part C — harness

## C1. `Formatter` renders as `((Formatter)(ref val))..ctor(...)` — a resolution artifact — DONE

Read as C#, `preamble-do-order.clr.cs` looked like a constructor invoked on a cast address, which
read as suspect IL. It was not.

What the emitter writes is `ldloca slot; ldc; ldc; [sink]; call instance void Formatter::.ctor`
(`buildFormat` in `EmitFormat`) — the same sequence C# itself emits for
`Formatter val = new Formatter(...)`. The rendering degrades because `Vesper.Printf.dll` cannot
be resolved: `decompilerOf` in the tests' `Decompile` seeded `UniversalAssemblyResolver` from
`AppContext.BaseDirectory`, while the Vesper packages build into repo-root `tmp/pkg-Vesper.*`.
With `Formatter` unresolved, ILSpy cannot know it is a value type and prints the raw address-call
form. A struct defined in the program under test renders normally in the same corpus —
`return new Shape(1, payload);` in `struct-union.clr.cs`.

Two independent confirmations that the IL is valid: these conformance programs run and match
their `.expected` output, and `MetadataStructure.assertWellFormed` passes over them.

**Landed.** `Decompile.ReferenceResolver` answers a reference from
`ProjectInfo.referenceSources`, which keys each `ProjectInfo.References` path by the assembly
name read off the file, and delegates every other name to `UniversalAssemblyResolver`. The 47
re-rendered goldens read `Formatter formatter = new Formatter(7, 1, Console.Out);
formatter.AppendLiteral("ctor a=");` and lost 237 lines net. No `.clr.txt` digest moved, which
is the check that only the rendering changed.

Resolution also let ILSpy drop the now-implicit `(object)` on the `sink.Child` calls throughout
the `StructUnion*` goldens, so the box per union child is visible only in the `.clr.txt` digest.

`assertReferencesResolve` raises when any `AssemblyRef` the PE binds fails to resolve, so a
future path or naming change fails the suite instead of silently re-rendering the goldens by
name. Emptying the reference map turns 49 tests red, which is what pins it.

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

A1 and A2 have landed. A2 stage 1 moved assembler row counts without disturbing the handle
predictions, and stages 2, 3 and 4 moved none, so the row-order ground is clear for whatever
runs next.

A3 stage 1 widens a frozen type and its codec, so it wants a commit of its own before anything
depends on it.

B1 has landed, and with it A4's second half. B2 has landed; it moved every golden's IL and no
table row. A4's first half has now landed too, moving no golden and no table row, so A3 is the
only Part A entry left.

A5 has landed in full. Its stage 5 widened `Frozen.TAbstractMethod` and moved the codec to
format version 3, so A3 stage 1 now lands on top of that version rather than racing it.

C1 has landed, so every re-render of the goldens below reads against resolved `Formatter` calls.
