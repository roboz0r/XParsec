# Keys hold semantic names

*Step 1 landed. Written 2026-08-31 against the code as it stands after step 2 of
`open-overhaul-plan.md`. Three cases are pinned as `ptest`s under
`LongIdentResolutionTests.fs`'s "a module whose compiled name differs" list.*

## The defect

`ModuleKey.Name` (`SymbolKeys.fs:71`) and `BindingKey.Name` (`:188`) hold the **compiled** name:
the `Module` suffix applied, `[<CompiledName>]` substituted. Name resolution needs the name the
source writes. So every resolution site that reaches a key has to recover a name the key
discarded, and the recovery is incomplete.

Three sites need the compiled name, and all three are emission:

| site | need |
|---|---|
| `Layout.fs:258` — `MetaName = h.Name` | the CLR module class name |
| `ClrEnv.fs:451` — `TypeRef(…, m.Name)` | an external module's TypeRef |
| `ClrRecipes.fs:237` — `binding.Name` | an external static method's name |

Four sites need the semantic name and read a compiled one:

| site | consequence |
|---|---|
| `Containers.childPath` (`:57`) | composes a compiled prefix with a source segment |
| `ExternalSymbols.prefixContainer` (`:250`) | same composition, for a written `open` |
| `InferResolve` (`:198`) | compares a compiled path against a written prefix |
| `PublishedSurface` container index (`:73`, `:398`) | keyed by compiled path |

`JsRuntime.fs:206` reads `containerFullName` for its import alias. It needs a name that is
unique and stable, and either name serves.

## The recovery that was built, and what it missed

*History: step 2 deleted every stage below. Line numbers are as of the diagnosis.*

The semantic name was recorded, serialised, read back, and compared — five stages to restore what
the mint threw away:

1. `TypeRegistry.noteLocalContainer` (`:504`) records `ModuleKey → source path` in
   `LocalContainerPaths`
2. `TypeRegistry.declaredModulePaths` (`:511`) projects it into `TastFile.ModuleSourcePaths`
3. `FrozenCodec` (`:458`) writes it into the blob
4. `FrozenFileResidue.sourcePathOf` (`TastPoolTypes.fs:139`) reads it back, and `failwithf`s
   when an entry is absent
5. `FrozenSignature.fs:338` builds a value's `SourceSpelling` from it, which
   `PublishedSurface.addValue` (`:139`) compares against `containerFullName` to decide whether to
   index the source path

Stage 5 is the hole. A module's source path enters the container index only as a side effect of a
value published from it. A module publishing only a type contributes no source path, and a
descent through a suffixed container composes a spelling neither vocabulary holds.

Confirmed by running it, cross-file within one assembly, with `module Foo` beside `type Foo`:

| written | reported |
|---|---|
| `open Test.Suffix.Foo` then `Inner`, module holds only a type | `UndefinedType "Inner"` |
| `Test.Suffix.Foo.Inner`, module holds only a type | `UndefinedType "Test.Suffix.Foo.Inner"` |
| `Test.Suffix.Foo.Bar.seed`, `Bar` nested under a suffixed module | `NoMember ("Test.Suffix.Foo", ValueOrMember, "seed")` |

`dotnet fsi` accepts all three. The same three pass once the module also declares a `let`, the
unsuffixed control passes, and every same-file equivalent passes — `LocalContainers` is keyed by
source path, so the split never surfaces within a file.

## Root cause

The compiled name is a property of a declaration, held in its identity. It reaches the key from
two mint sites and nowhere else:

- `Containment.CompiledModuleNameOf` (`Containment.fs:16`) applies the `Module` suffix
- `Elaborate.exportedBindingInfo` (`Elaborate.fs:129`) applies `[<CompiledName>]`

`ModuleBindingInfo` already carries both names (`Name`, `SourceName`), and `.BindingKey`
(`:17`) selects the compiled one. Every other producer propagates what these two minted: the
`.fsi` walk goes through the same `Containment` member, the frozen surface round-trips the key,
and the CLR reflection reader mints no `ModuleKey` at all — `ScopeContents.typeDirectory`
yields `InNamespace` only (`ExternalSymbols.fs:144`). No producer holds a compiled name without
the semantic one.

## Target shape

A key holds the semantic name. The compiled name becomes a separate published fact, present only
where it differs.

```fsharp
/// The name a declaration compiles to, carried only where it differs from the name its
/// source writes: `[<CompiledName>]` on a value, the `Module` suffix on a module.
[<Struct>]
type CompiledName = CompiledName of string
```

`CompiledName` does not carry the `` `1 `` generic-arity rendering. That is a projection of
`TypeKey.TyparArity`, which the key already holds, and `SymbolKeyOps.typeMetaName` already
applies it.

`ModuleSourcePaths` inverts: the same blob column carries the compiled name where it differs,
instead of the source path where it differs. `sourcePathOf` and its `failwithf` go away, because
the source path is in the key. `PublishedSurface.SourceNames` goes away for the same reason.

## Staged plan

**Step 1 — publish the compiled name. DONE.** `CompiledName` (`SymbolKeys.fs`) travels two
channels, both filled at the mint sites and read by nobody yet:

- a module's, through `PassContextTypes.CompiledModuleNames` → `TastFile` /
  `FrozenFileResidue.CompiledModuleNames` → the blob → `PublishedSurface.CompiledModuleNames`
- a value's, on `ExternalSymbol.CompiledName`, filled by `FrozenSignature.addValue` off
  `ModuleBindingInfo`'s two names and by `SignatureResolution.registerValSig` off the attribute

`CompiledName.OfPair` is the one comparison rule all three sites call. A `val` whose declared
name is an active pattern has no modelled source form, so `registerValSig` publishes no compiled
name for it — step 3 has to decide what such a binding's key holds before flipping
`BindingKey.Name`.

Step 2 and step 3 read these instead of the key. Covered by "publishes its compiled name"
(`LongIdentResolutionTests.fs`, the `.fs` half through the blob) and "publishes a compiled name"
(`SignatureResolutionTests.fs`, the `.fsi` half).

**Step 2 — flip `ModuleKey.Name`. DONE.** `EnclosingChainOf` mints the source name.
`Layout.moduleClassName` reads its own file's `FrozenFileResidue.CompiledModuleNames`;
`ClrEnv.externalModuleRef` reads a referenced module's through
`IScopeContents.ModuleClassNameOf` / `ICodegenSymbols.ModuleClassNameOf`, served by
`PublishedSurface.scopeOf`. `ModuleSourcePaths` and `sourcePathOf` are gone. The three cases
above are un-pended and pass.

The channel returns `ModuleClassName`, three-state: a `Compiled` class name, the declaration
`SourceName`, or `Undeclared` for a module no consulted surface publishes. `scopeOf` separates
the last two by the `containers` index it already builds, and `externalModuleRef` fails the
compile on `Undeclared` — a bare source name there would bind to no `TypeDef` and fault at
load time instead. Pinned by the two `ModuleClassNameOf` cases in `ScopeContentsTests.fs`.

Two renderings followed the key. `SymbolKeyOps.typeMetaName` spells a module-held type
`N.M+T` rather than `N.MModule+T`; the CLR ref path composes its `TypeRef` chain through
`externalModuleRef` instead, so no emission site reads that rendering. `typeMetaName` is now the
identity spelling only, and `MetadataSymbols.lookupTypeByKey` declines a module-held key rather
than handing that rendering to `asm.GetType`. The JS import alias is
`$Vesper_Collections_List_length`, and `src/Vesper.Seq/Vesper.Seq.mjs` was regenerated for the
matching `Vesper.Array` alias.

*Also collapsed under this step, because the flip is what made them redundant:*
`PublishedSurface.ModuleContainers` (with `addModuleContainer` / `addModuleChain`) mapped a
module's source path to its container, which the key now renders directly — every entry it held
duplicated one `scopeOf`'s own `noteContainer` sweep had already filed. `SourceSpelling`
collapsed to `SourceNames: BindingKey -> string`: its `Path` field was `containerFullName
sym.Key.Decl` by construction at both producers, and no consumer read it.

**Step 3 — flip `BindingKey.Name`. DONE.** `ModuleBindingInfo` carries `Name` (source) beside
`CompiledName: CompiledName voption`, matching `Containment.ModuleScope`'s shape, and
`EmittedName` is the one projection an emitter reads. The two mint sites fill it:
`Elaborate.exportedBindingInfo` off `[<CompiledName>]`, `SignatureResolution.registerValSig` off
the same attribute. `FrozenCodecTypes.writeModuleBindingInfo` carries the compiled name where
it differs, in the column that held the source name.

Four consumers wanted the emitted name, all emission: `EmitClosures.declaredEmission` and
`Elaborate`'s `emittedName` (both `info.EmittedName`, the latter feeding the `[<Global>]`
restatement check and the `[<Import>]` selector check), and `ClrRecipes.emitExternalCall`'s
`MemberRef`, which reads a referenced package's through the new
`CodegenOpenSignature.EmittedName`, filled by `CodegenSymbols.TryLookupOpenSignature` off
`ExternalSymbol.CompiledName`. Every other read — `ConformanceSurface`, `ConformanceTypars`,
`Layout.combine`'s clash message, `PublishedSurface`'s ordering key — wanted the identity, so
`SymbolKeyOps.qualifiedName` is the identity spelling and no longer an emission name.

`SourceNames` and the `IScopeContents.TryValue` dual-name match are gone. JS needed no change
and one defect went with the flip: `JsRuntime.addRef` names its `import { … }` export off
`b.Name`, and JS emits a binding under the name its source writes, so a `[<CompiledName>]`'d
value used to import a name the module never exported. `JsPackageTests` pins it end to end.

`TryValue` now takes the `BindingKey` whose two fields its `(container, name)` pair spelled,
so `scopeOf` serves it off the symbol index it already builds and `ScopeContents.memoize`
caches on one key rather than a parallel struct tuple.

`AttributeDecode.compiledNameOf` is the one decode-then-compare rule, replacing
`tryCompiledName` at both value mint sites.

*Three semantics settled under this step:*

- **An active-pattern `val` publishes nothing.** `OperatorNames.ofDeclaredName` models no
  source form for one, so `registerValSig` files no key — matching the implementation half,
  where `MemberNames.ofBinding` files no `ModuleBindingInfo`. A `[<CompiledName>]` on such a
  `val` used to publish under the attribute's name, which no use site can write. The
  consequence is that a consumer cannot resolve the pattern at all, pinned as a `ptest` in
  `SignatureResolutionTests`.
- **Conformance pairs the halves by the name each writes.** `dotnet build` refuses
  `val foo` against `[<CompiledName("foo")>] let bar` with FS0193 "Module 'V.M' requires a
  value 'foo'", and accepts the pair that shares a source name with the attribute on both.
  `ConformanceTests`'s green "a `[<CompiledName>]`'d let satisfies the val it publishes as"
  pinned the opposite and is replaced by both cases.
- **The halves must agree on the EMITTED name.** Taking the compiled name off the identity
  axis unhooked the two readings: a reference resolves through the signature's
  `ExternalSymbol.CompiledName` while the implementation emits under its own
  `ModuleBindingInfo`. `ConformanceSurface.checkValues` compares them and reports
  `CompiledNameDiffers`; `definedValues` carries the whole `ModuleBindingInfo` rather than
  its attributes alone. `[<CompiledName>]` written on one half only is silent to
  `divergentAttributes` by design, so this is the check that catches it.

**Step 4 — collapse the container index.** With both key names semantic,
`PublishedSurface.addModuleContainer` (`:72`) indexes one vocabulary rather than a union of two,
and `IScopeContents.TryContainer`'s `sourcePath` parameter (`ExternalSymbols.fs:81`) becomes
true. Separate change, per the delete-the-old-one-separately rule.

A type-level split of the two name kinds — `SourceName` and `CompiledName` as distinct types
rather than both `string` — was considered to make `childPath`'s mixed composition a compile
error. Step 2 removes that composition outright, so the split is optional after it rather than a
prerequisite.

## Semantics to confirm

1. **A module's compiled name.** Source name plus `Module`, when a nominal type of the same file
   shares the name or `[<CompilationRepresentation(ModuleSuffix)>]` is written
   (`Containment.fs:16`). Unchanged by this plan; step 2 moves where the result is stored, not
   how it is computed.

2. **Absence means identical.** `CompiledName` is absent for the overwhelming majority of
   declarations, and absence has one reading: the compiled name equals the semantic name. No
   second interpretation rides on it.

3. **Key identity does not collide under the flip.** `ModuleKey` and `TypeKey` are distinct
   types, so `module Foo` and `type Foo` in one namespace stay distinct keys once both hold
   `"Foo"`. Two source declarations distinct in source are distinct in semantic names, so the
   flip removes a spurious distinction rather than merging two identities.

4. **`Layout.fs:62` mints a key for a synthetic class.**
   `moduleKeyOf (InNamespace Global) project.ModuleName` names the program class, which has no
   source declaration. Its `Name` field is a compiled name in a semantic-name slot. Backend-local
   and harmless, or worth a distinct construct — undecided.

5. **The JS import alias.** Settled by step 2: `JsRuntime.fs:206` now mangles the source path.

## Scope and risk

Touched: `SymbolKeys`, `SymbolKeyOps`, `Containment`, `Elaborate`, `ModuleBindingInfo`,
`TypeRegistry`, `PublishedSurface`, `FrozenSignature`, `FrozenCodec`, `TastPoolTypes`,
`Containers`, `ExternalSymbols`, and in the backends `Layout`, `ClrEnv`, `ClrRecipes`,
`JsRuntime`.

The blob format changes. `FrozenCodec` carries no version stamp, so reader and writer change
together and any cached blob on disk goes stale.

Every name in every suite is in the regression surface, so
`XParsec.FSharp.SemanticAnalysis.Tests`, `XParsec.FSharp.Codegen.Clr.Tests`,
`XParsec.FSharp.Codegen.Js.Tests` and `Vesper.Tests` gate each step. `Vesper.List` is a
`ModuleSuffix` module publishing many values, so it exercises the recovery path this plan
deletes.

## Sequencing against `open-overhaul-plan.md`

After that plan's step 3, before its step 4. Step 3 changes resolution order and touches no
naming. Step 4 deletes the written-`open` string channel, and the compiled/semantic split is why
that channel is unreliable, so landing this first means step 4 deletes strings that are already
correct instead of rewriting `prefixContainer` and `childPath` twice.
