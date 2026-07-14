# Drop the home assembly from `SymbolKey`

Closes finding 1 of `docs/thermo-review-938dd9da34.md`. Ephemeral: delete once landed.

## The thesis

A `SymbolKey` is a **nominal identity**. The home assembly is a **physical location**.
Within one compilation a fully-qualified name names at most one type — F# resolution never
uses the assembly to disambiguate, and neither does this codebase. So the assembly is a
*function of* the identity, not a *component of* it.

The review (finding 1) proposed keying the provider stores by `SymbolKey` to close the gap
between "the unifier compares the full asm-bearing key" and "the store answers by the
asm-blind rendered name". That is not achievable: ten call sites mint a store key from a
bare compiled-name string with no assembly in hand (`EngineCore.fs:576/717`,
`InferApp.fs:323`, `InferRecordAccess.fs:668`, `Unification.fs:759`, `ClrEncoder.fs:135`,
`ClrEnv.fs:452`, `CodegenSymbols.fs:42/45`, `SymbolProviders.fs:136`), and `MetadataSymbols`
answers by live reflection (`asm.GetType`), which is name-addressed by nature.

Removing the field closes the gap from the other side, and makes the invariant true instead
of asserted.

## Why the field is removable

Established by survey, not assumption:

* **No lookup anywhere uses the assembly to disambiguate.** Every store face projects
  through `SymbolKeyOps.qualifiedName`, which discards it (`ExternalSymbols.fs:850` says so
  outright). Every local `TypeRegistry` table is keyed by the whole `TypeKey`, but every
  local key carries the *same* origin (`asmOf ctx.AssemblyName`), so it cannot discriminate
  there either.
* It participates in exactly **two** decisions:
  1. Structural `SymbolKey` equality in unification (`Engine.fs:337-354`). This is a **bug
     source**, not a feature: `EngineCore.fs:724` mints an interface key asm-blind from a
     bare name and stores it in a `TyClass`; `InferApp.fs:325` mints the same class of type
     asm-carrying from a resolved `Origin`. They name one type and do not compare equal.
     `checkAsmInvariant` (`Engine.fs:172`, `#if DEBUG`) exists solely to catch this, and does
     not ship.
  2. `ClrEncoder.fs:163/180/190/316`'s `keyAsm key = envAsm` local-vs-external branch —
     already redundant (`:316` reads `keyAsm key = envAsm && userTypes.ContainsKey key`), and
     already how the rest of both backends decide (`EmitMember.fs:349`, `EmitBindings.fs:143`,
     `ClrGenerics.fs:197`, `ClrProvider.fs:194`, `EmitJsContext.fs:167/203/304`,
     `EmitJs.fs:249`).

## The shape

* `Origin` leaves `NamespaceKey`. `NamespaceKey` becomes just its segmented `Path`, so
  `NamespaceKey.Global` is the empty path and nothing else.
* Every derived `.Origin` / `.AsmOption` accessor down the holder chain (`ModuleHolder`,
  `ModuleKey`, `TypeKey`, `BindingKey`, `MemberKey`, `SymbolKey` — `SemanticInfo.fs:86-273`)
  deletes, along with `SymbolKeyOps.keyAsm` / `typeAsm` / `asmOf`.
* `SymbolKeyOps.reroot` and its four recursive helpers delete, and with them the
  `stampOrigin` key-re-homing in `ExternalSymbolProviders.stack` (`:166-177`) — a leaf minting
  keys before it knows the wrapping package's assembly stops being a problem that needs a
  structural walk to repair.
* The boundary mints (`typeKeyOf`, `namespaceKey`, `qualifiedTypeKeyOf`, `externalTypeKeyOf`,
  `moduleInNamespace`, `typeKey`, `moduleValueKey`, …) lose their `asm: string option`
  parameter. `lookupKeyOfCompiledName` — which exists *only* to mint an asm-blind key for the
  store — becomes indistinguishable from `qualifiedTypeKey` and deletes.
* `SymbolOrigin` (`SemanticInfo.fs:283`) stops being a one-field wrapper around a
  `NamespaceKey` whose assembly is two hops down, and carries the assembly **directly**
  alongside the namespace. It is the `key -> assembly` oracle: the provider resolves a shape,
  and the shape's origin names its home. (This also answers finding 9's complaint about
  `SymbolOrigin`, in the opposite direction — it gains a reason to exist.)

## The assembly, where it is genuinely needed

Only **past** the local/external verdict, and only in the backends:

* **CLR**: `ClrEnv.fs:400-457` (`externalAsmRef`) needs a real `AssemblyRef` to scope a
  `TypeRef`. It already reads `info.Origin.Assembly` off the provider shape at `:457/:501/:525`.
* **JS**: `JsRuntime.fs:105-158` and `JsExternalMembers.fs:35-48` need the import path;
  `JsExternalMembers.assemblyOf` already falls back to `shape.Origin.Assembly`.
* `ClrEncoder`'s four `keyAsm key = envAsm` arms become `userTypes.ContainsKey key`. Local
  still wins over a same-named external type, because the local arm is still checked first.

## The same disease, one field over: arity is a CLR-ism in the key

`TypeKey.Name` carries the arity MANGLED INTO IT, .NET-metadata style (`` List`1 ``). That is a
CLR convention sitting inside a language-neutral identity, and it is the cause of a family of
string surgery whose whole job is to pack and unpack an integer: `arityName`, `bareName`,
`withArity`, `typeNestedName`'s backtick-probing. `TypeRegistry.tryKeyOfArity` tests an arity by
re-rendering the suffix and string-comparing it — an `int` round-tripped through a `string`.

Because the arity lives in a string, a producer can silently FORGET to spell it — and one does, on
purpose: `SymbolKeyOps.intrinsicCanonKey` mints the contract's verbatim short name with no suffix
(`seq`, not `` seq`1 ``), while the contract layer suffixes elsewhere. The gap is then bridged by
`RuntimeNames.sameTypeIgnoringArity`, a matcher comparing namespace path plus arity-STRIPPED name
instead of comparing keys — identity by approximation, exactly like the assembly.

Fix: the arity is a **field**.

```fsharp
TypeKey = { Holder: TypeHolder; Name: string; Arity: int }
```

`Name` is the plain source name (`List`, `seq`, `[]`), never mangled. The backtick becomes purely a
RENDERING concern in `typeMetaName`, at the metadata boundary that actually needs it. `arityName` /
`bareName` / `withArity` delete. `tryKeyOfArity` becomes an int compare. And
`sameTypeIgnoringArity` is not merely deleted — what it compensated for becomes
**unrepresentable**: you cannot forget an `int` field, you must supply one. Its five consumers
(`Capability.Matches`, `isVesperListKey`, `isFsharpCoreListKey`, `isStructuralFormattableKey`,
`isPrintfFormatKey`) use `=`. **`SymbolKey` equality becomes the single identity test in the tree.**

This also kills a latent bug. `withArity` reasons that "an outer already carrying a backtick means
the arity is already spelled, so do not re-append" — but that is not the CLR rule. Each metadata
segment's suffix is that segment's OWN typar count, so `` Outer`1+Inner`1 `` is legal and means
`Inner` adds one of its own. The current code would refuse to suffix a genuinely generic nested
type. Per-segment `Arity: int` makes that unwritable.

**Not `EqArray<ConstrainedTyVar>`.** Identity must be the MINIMAL discriminator. F# overloads types
on arity and nothing else — `List<'a>` and `List<'a when 'a : comparison>` are not two types. Typar
names and constraints are not identity-bearing, and putting them in the key would make equality too
fine: a key minted from a contract (which may carry no constraint detail) would stop comparing equal
to one minted from source — the same "two mint paths, one type, two keys" bug the assembly removal
just deleted. The COUNT goes in the key; the typar DETAIL stays on `RecordTypeInfo` / `ClassTypeInfo`
/ …; the type ARGUMENTS stay in the `SemType` (`TyUnion(key, args)`).

## `simpleName` is display-only, and the type says so

`SymbolKeyOps.simpleName` drops containment AND the arity suffix. It is a lossy, human-facing
projection, and it must NEVER be a route back to a key, a lookup name, a canon name, or an
intrinsic repr. "The key missed, so match on the name" is the disease this whole document is about.

Today that rule is held by a doc comment. Make the compiler hold it: `simpleName` returns

```fsharp
[<Struct>] type DisplayName = DisplayName of string
```

a single-case DU — NOT an abbreviation, so the only way back to a string is an explicit
`let (DisplayName s) = …` unwrap. No key mint and no lookup accepts a `DisplayName`, so a site that
tries to recover an identity from one does not compile, and "who is treating a display string as an
identity" becomes a grep for the unwrap.

Legitimate consumers, which unwrap and are fine:
* **human** — diagnostics, error messages, display;
* **backend name emission** — mangling an identifier the target actually emits (a JS identifier, a
  CLR member name), where the target's names genuinely carry no generic arity.

Anything else is a bug, and now a compile error.

## The premise, enforced

The thesis — *within one compilation a fully-qualified name names at most one type* — must not
be a thing we merely rely on. It becomes a **diagnostic**: a project that declares a type whose
fully-qualified name a referenced assembly already claims is an error (the CS0433 analog), not a
silent shadow.

Site: `NameResolutionTypeRegistration.claimTypeIdentity` (`TypeRegistration.fs:336-404`), where
the local key is minted and the external provider is already in hand. The claim test grows one
arm: `(name, arity)` unclaimed locally AND the minted key unanswered by the provider.

Without this, dropping the assembly would let a local `Vesper.List` and a referenced external
`Vesper.List` become one key — codegen would still be correct (the local table is checked first,
so local wins), but the unifier would unify two genuinely different types. With it, that state is
unreachable.

**Watch:** self-host. Compiling the prelude / `Vesper.Core` itself, the front end still mounts a
contract provider, and the unit declares the very types that contract publishes (`type int = (#
"System.Int32" #)`). If a compilation references its OWN contract, the check fires on every
prim-type. A unit's own contract is not a "referenced assembly" for collision purposes; if the
test suite shows this arises, make that distinction explicit rather than weakening the check.

## Accepted consequences

* **The package-not-referenced diagnostic degrades.** `InferRecordAccess.fs:324` (*"referenced
  from package 'P' but that package is not part of the compilation"*) reads the home off the key
  precisely when the store *misses*, so no shape remains to ask. It names the type's NAMESPACE
  instead. No `key -> assembly` side-table: this is not a general mechanism, it is an affordance
  for Core-library types (`int` / `float` / `string`), which the intrinsic tables already know by
  name and which should get their diagnostic from there. The front end stays assembly-free end to
  end.
* `RuntimeNames`' canonical keys drop their `Some "Vesper.List"` / `"FSharp.Core"` homes, and
  `sameTypeAsmBlind` (`:256`) collapses to plain key equality — as do `isVesperListKey`,
  `isFsharpCoreListKey`, `isStructuralFormattableKey`, `isPrintfFormatKey`, `Capability.Matches`.
* `Elaborate/Printf.fs:126`'s `localAsm` — threaded through `structuredArgFaithful` and never
  compared — deletes as the dead parameter it is.
* `checkAsmInvariant` (`Engine.fs:163-186`) deletes. Not compiled out: **unrepresentable**.
* `PassContext.AssemblyName` loses two of its three readers (`TypeRegistration.fs:154`,
  `Elaborate.fs:1819`); only codegen's own `ProjectInfo.AssemblyName` remains load-bearing.

## Order of work

1. **Core** (`SemanticInfo.fs`, `SymbolKeyOps.fs`, `RuntimeNames.fs`, `ExternalSymbols.fs`,
   `ExternalSymbolProviders.fs`, `TypeRegistry.fs`, the passes) — the type change, compiled green.
2. **Backends**, in parallel once (1) is green: `Codegen.Clr` and `Codegen.Js` touch disjoint
   files.
3. **Arity into intrinsic keys**; `sameTypeIgnoringArity` deleted.
4. **`DisplayName`** — turns the `simpleName` rule into a compile error. Sequenced AFTER (3),
   because (3)'s audit of `simpleName` call sites is exactly the list of sites that stop compiling.
5. **The collision diagnostic** in `claimTypeIdentity` — what makes the premise true rather than
   assumed.
6. **Tests**: the asm assertions in `SymbolKeyTests` / `ExternalMemberTests` /
   `MetadataSymbolsTests` / `JsNamespaceTests` / `RefsTableTests`. `SelfHostTests` and
   `CrossAssemblyEscapeTests` are the regression net for the local/external boundary.
