# Constant expressions — execution plan

Ephemeral: delete when the work lands. Replaces `const-fold-followups-plan.md`, whose five
follow-ups are absorbed into the stages below.

## Root cause

`ConstFold.tryConstant` folds attribute arguments and `[<Literal>]` bodies over the raw CST,
during name resolution, discarding the expression and keeping a scalar. Three consequences:

**Operators are matched by token.** `ConstFold.fs:146-149` treats `|||`/`&&&`/`^^^` as the
intrinsic operators whatever they resolve to, so a shadowed `(|||)` folds wrongly. Widening the
domain to `+`, `*`, `<<<` and string concatenation makes that a live defect rather than a
theoretical one.

**Name resolution is re-derived.** `AttributeFold.tryNamedConstant`, `tryLiteralValue` and
`tryEnumCase` (`AttributeFold.fs:86-190`) re-implement scope walking and shadowing so an
attribute argument can name a `[<Literal>]` or an enum case. Two derivations of one relation.

**The written expression is discarded.** `TAttributeArg.Value` (`AttributeVerdicts.fs:13-18`)
keeps a `TConstValue` and an `EnumKey` — a scalar plus one salvaged fact — so nothing downstream
can report what the author wrote. Tooling over the frozen tree has no attribute-argument story.

The domain is also narrower than F#'s: `null`, `typeof<T>`, array literals and `nameof` are all
valid in an attribute constructor and all rejected as `NotConstant`.

## Shape

A constant expression types bottom-up and is then checked against the position's declared type.
The declared type does not propagate into a literal: `fsi` refuses `[<Long(1)>]` against an
`int64` parameter with FS0267 and accepts `[<Long(1L)>]`, so a literal keeps the type its own
suffix gives it (stage 1's mapping) and an argument whose type differs from the parameter's is an
error rather than a widening. `null` is the one form with no type of its own and takes the
parameter's, as `[<S(null)>]` against a single `string` parameter shows.

| Position | Checked against |
|---|---|
| Attribute constructor argument | the attribute class's constructor parameter |
| Named argument `Prop = v` | the property's declared type |
| `[<Literal>] val X: int = 3` | the annotation |
| `[<Literal>] let X = 1` | nothing; the literal's own type governs |
| Enum case `\| A = 1` | the enum's underlying type |
| `[<DefaultParameterValue(0)>]` | the parameter's declared type |

Constructor selection stays finite: filter by argument count, check the arguments against each
candidate's parameters, take the unique success. No metavariables, no union-find. The same
checker therefore serves the `.fs` and `.fsi` paths, and attribute arguments never enter
`Unification` on either.

A `TypeOf` operand is ground by construction: `typeof<'T>` in attribute-argument position is
refused by `fsi` with FS3187 both for a member's owning type parameter and for a binding's own,
because the declaration's typars are not in scope there.

Two new types. `TConstExpr` is the checked expression, `TConstResult` its value:

```fsharp
type TConstResult =
    | Scalar   of TConstValue
    | Null
    | TypeVal  of FrozenType
    | ArrayVal of Block<TConstResult>

type TConstExpr =
    | Literal    of value: TConstValue * ty: FrozenType * tok: Anchor
    | Null       of ty: FrozenType * tok: Anchor
    | LiteralRef of binding: BindingKey * result: TConstResult * ty: FrozenType * tok: Anchor
    | EnumCase   of enumKey: TypeKey * caseName: string * result: TConstResult * tok: Anchor
    | TypeOf     of operand: FrozenType * ty: FrozenType * tok: Anchor
    | NameOf     of target: SymbolKey * name: string * ty: FrozenType * tok: Anchor
    | ArrayLit   of items: Block<TConstExpr> * ty: FrozenType * tok: Anchor
    | Unary      of op: BindingKey * operand: TConstExpr * result: TConstResult * ty: FrozenType * tok: Anchor
    | Binary     of op: BindingKey * left: TConstExpr * right: TConstExpr * result: TConstResult * ty: FrozenType * tok: Anchor
```

A site is an `Anchor`, not a `SyntaxToken`: `TAttributes` is written by `FrozenCodecConst` and
read back in another assembly, where a token index is the only form a site survives in. An
`EnumCase` carries no `ty` column: its type is `FTEnum enumKey` by construction.

`TConstDenotation` is a node's `{ Result; Ty }` with the site dropped: the form under which
two spellings of one constant compare equal. `ConformanceSurface` compares attribute arguments
by it, and the tests assert on it.

Three properties this fixes in place:

- **Every node carries its result**, so folding is a total projection off the tree rather than a
  partial function guarded by rejections. Holding a `TConstExpr` means the value exists.
  `ConstRejection` becomes ordinary check-time diagnostics.
- **Operators carry a resolved `BindingKey`**, so a shadowed operator is honoured and widening the
  domain is safe.
- **`FrozenType`, not a type parameter.** An attribute argument's types are ground by
  construction, so `TConstExpr` needs no `'ty`. `TAttributes` stays the single non-generic type
  embedded in `TDeclG` shapes (`TastDecl.fs:86,109,120,196,385`), carried unchanged across the
  `SemType`/`FrozenType` cut and across assemblies. Attributes are not pooled, so the frozen-side
  cost is `FrozenCodecTypes.fs:222-244` alone.

`TConstValue` is untouched: it stays the expression- and pattern-level scalar threaded through
`TExprG.Const`, `TPatG.Const`, the pools and both backends. `EnumKey` leaves `TAttributeArg`,
since the node's type carries it.

**Representation is target-neutral; encodability is not.** `TConstExpr` admits `decimal`,
`nativeint`, string-valued enums and every other case the front end can faithfully represent. A
backend that cannot encode a case rejects it through an interface (stage 7), so the front end
never learns what ECMA-335 II.23.3 can hold.

This departs from fsc, which refuses both in the front end: `[<Dec(1.5M)>]` is FS0073 ("internal
error: The type 'System.Decimal' may not be used as a custom attribute value") and `[<Nat(1n)>]`
is FS0267. Both refusals exist only because CLR metadata cannot hold the value, which is a fact
about one target — a front-end gate encoding it would be the mislevelling this project's
`CLAUDE.md` forbids, and would refuse the value on JS, where no attribute is emitted at all.

## Stages

### 1. Extract the literal type mapping

`UnificationInferLiterals.literalCarrier` (`Passes/Unification/InferLiterals.fs:31`) is a
`SyntaxToken -> SemType` reading `ctx.Intrinsics` and `NumericLiterals.numericKindOf`, with no
dependency on inference. Move it to a new `LiteralTypes.fs` taking `IntrinsicSet`
(`Intrinsics.fs:39`) directly, compiled after `Intrinsics.fs`; `literalCarrier` delegates.

Done when: `Infer` is unchanged in behaviour and the mapping is callable from the `.fsi` path.

### 2. The types, end to end, behaviour-preserving

Add `TastConstExpr.fs` after `TastExpr.fs` and before `AttributeVerdicts.fs`. `TAttributeArg`
becomes `{ Name: string voption; Expr: TConstExpr }`, with `Value: TConstValue voption` and
`EnumKey` computed projections.
`FrozenCodecTypes.writeTAttributeArg`/`readTAttributeArg` round-trip the tree.

Keep the existing CST producer for now, upgraded to build `TConstExpr` nodes: literal leaves take
their type from stage 1 and freeze through `FrozenTypeBridge`. No new domain cases yet.

Done when: `AttributeFoldTests` and `AttributeRowTests` pass unchanged, and a frozen tree written
and re-read preserves the argument expressions.

### 3. The resolution-directed check — LANDED

`ConstExprCheck.check` takes the `PassContext` and the `UseSite` directly and resolves every
spelling through `NameResolutionLongIdent`, the resolver expression position uses. It replaces
`ConstFold.tryConstant` and reports its own diagnostics, one per rejected expression, at the
failing subexpression's first token. The `IConstNameResolver` the plan sketched was built and
then removed: its one production implementation was a pass-through over `PassContext`, and its
only other implementation was a test stub, which the suite now replaces with real name
resolution against the `Vesper.Core` contract plus a published surface. `IExternalSymbolProvider`
is the one seam between the file under analysis and everything outside it.

Two departures from the shape written above, each deliberate:

- **`TryNamedConstant` is one member, not "literal by spelling" plus "enum case by spelling".**
  The implementation calls `NameResolutionLongIdent.resolveExpr`, so the literal-shadows-case
  precedence is the expression-position precedence rather than a second copy of it. That is the
  root cause this stage exists to remove, so splitting the member would have kept it. "Type by
  written name" arrives with stage 6's `typeof<T>`, its first consumer.
- **`expected: FrozenType voption` is not on `check`.** Nothing supplies an expectation until
  stage 5's annotated `val` literal and stage 6's `null`; adding the parameter now would ship an
  untested one. It goes on with its first consumer.
An operator folds only where its compiled name resolves to the intrinsic, so a `let (|||)` in
scope and an operator out of scope both refuse, matching fsc's FS0267 and FS0043. This turned
`compiler-attributes.fsi` red at 15 sites: it wrote `AttributeTargets.Class ||| …` above
`ops-platform.fsi`, which declared `BitwiseOperators`, and `BitwiseOperators` carries the
`[<AutoOpen>]` that file declares.

The cycle is four types wide, so `Vesper.Core` is split around it. `compiler-attributes-core.{fsi,fs}`
holds `AttributeTargets`, `AttributeUsage`, `Sealed` and `AutoOpen`; `ops-bitwise.{fsi,fs}`
holds `BitwiseOperators` directly after it, one shared implementation for both targets; the
rest of `compiler-attributes.fsi` follows and writes every mask with `|||` as FSharp.Core does.
`AutoOpen`'s own mask is the one written numerically, as
`LanguagePrimitives.EnumOfValue<int, AttributeTargets> 13`. `AttributeFoldTests` pins every
published multi-flag mask against the `System.AttributeTargets` combination FSharp.Core
declares it with.

`Vesper.Core` also gained `language-primitives.{fsi,fs}`, holding
`LanguagePrimitives.EnumOfValue` / `EnumToValue`, and `ops-std` gained the auto-opened `enum`
wrapper, FSharp.Core's own split.

The checker folds either spelling by resolving the applied name to a binding and reading the
type argument at the position `RuntimeNames.enumConversionTypeArg` gives for that key, which is
the plan's "type by written name" capability arriving with its first consumer. The node records
the key the applied name resolved to. Both spellings are refused where the applied name denotes
any other binding. An operator resolves the same way, as a one-segment spelling of its compiled
name.

**Open gap, pinned by `EnumTests`' `ptest`:** an explicit type application on the APPLIED
FUNCTION of an application pins nothing, so `let c = enum<Color> 2` leaves `^U` unresolved
where `let c: Color = enum<Color> 2` is clean. `inferTypeApp` (`InferTypeOps.fs:37`) targets a
local binding's scheme or a nominal result, and an external symbol is neither. This misses
attribute arguments entirely, which never enter `Unification`.

`Resolution.LiteralValues` is now a `SideTable<TConstExpr>` keyed by binding site, holding the
RHS as checked, so a `LiteralRef` carries the referent's own type — an enum-typed literal keeps
its `FTEnum`, which the scalar-only table could not express. `ElaborateIdents.translateIdent`
substitutes it at each use site, as `externalMemberExpr` (`ElaborateExpr.fs:29`) does for an
external one. It stays on the side table rather than moving onto `ModuleBindingInfo`: publishing
it needs a const slot on `ExternalSymbol` whose only consumer is stage 5, which lands both ends
together.

Deleted: `ConstRejection`, and `AttributeFold.tryNamedConstant`/`tryLiteralValue`/`tryEnumCase`.
`ConstFold` shrank to the literal-token projection and is renamed `ConstLiteral.tryValue`.
`LocalModuleMember` stores its `BindingKey` as `Key`, and `ResolvedValue.BindingKey` covers the
local and external arms once, which a `LiteralRef` and an operator resolution both need.
`ResolvedItem.EnumCase` carries a `ResolvedEnumCase` with the case's value, as
`ResolvedUnionCase` carries its case, so the checker projects the resolved item instead of
walking the enum's cases a second time. `AttributeFold.fs` moved after
`Passes/NameResolution/LongIdent.fs` in compile order, which is what made one derivation
possible.

A bitwise combination takes two operands of one type, so `E.A ||| 2` and `E.A ||| F.Bit`
are rejected as fsc's FS0001 rejects them, rather than folding to a bare `int`.

Regressions: `ConstExprCheckTests` over real name resolution, and two whole-pipeline tests in
`AttributeFoldTests` (a shadowed `(|||)` is FS0267, an unshadowed one folds) plus one in
`ElaborateTests` (a local `[<Literal>]` reference is the constant at its use site).

### 4. One folding site — LANDED

A declaration position is DECLARED where it is written and CHECKED there, once.
`PassContext.DeclareAttributes` folds the arguments under the walk's live environment and
files an `AttributePosition` — the element `[<AttributeUsage>]` is enforced against, the
attributes as resolved, and their checked form — keyed by `AttributeSite.ofToken` off the
element's own anchor token (`AttributeSite.ofSite` for a type declaration). A site is declared
exactly once, and `PassContext.AttributesAt` is a total lookup: a miss is a producer bug and
fails. `DeclareAttributes` is the only route to `AttributeFold.build`.

Only target ENFORCEMENT is deferred: `Passes.Attributes.run` walks the positions in source
order after every declaration is filed, so `declaredValidOn` reads an attribute class's mask
off its own checked position regardless of registration scan order, pinned by
`AttributeFoldTests`. Folding at declaration is fsc's own rule: `[<Tag(LaterLit)>]` above the
`[<Literal>] let LaterLit` is FS0039, and `type [<Tag(int E.X)>] A1() = … and E = | X = 7` is
FS0039 on `E.X` inside the recursive group, so the fold has nothing to gain from waiting.

Registry infos carry `ResolvedAttributes`, and the equality / comparison / qualified-access /
null-literal verdicts read an `AttributeKeys` projected from either the resolved or the checked
form. `RecordFieldInfo` and `UnionCaseInfo` carry the `AttributeSite` their attributes are filed
under in place of the dead `DeclKey` they had. `EnumTypeInfo.Cases` is built whole at
registration, each case's checked attributes included.

One departure from the shape written above: **the `.fsi` leg goes through the same table.**
`SignatureResolution` declares its `val` and class positions and `resolveFile` runs the pass,
so stage 5 has one mechanism to extend rather than two.

New positions: parameters (`Parameter`), declared type parameters (`TypeParameter`) and
abstract member signatures, which take `Method` or `Property` off the signature's own shape.
Member positions are declared at REGISTRATION, so an interface's abstract members — which
`Elaborate` builds from the registry rather than from `translateMemberElement` — are covered.
`AttrTarget.Unchecked` had no producer left and is gone, so `AttrTarget.mask` is total.

Two positions the plan listed are NOT in:

- **Exception declarations.** `ModuleElem.Exception` is `NotYetSupported` at
  `Validation.fs:257`; there is no registered declaration for a position to hang off.
- **Class `let` / `do` preambles.** `fsi` reports FS0842 TWICE for one class `let`, under two
  different masks (`method, field, return value` and `property, field, return value`, the
  second varying with the binding's shape), and discards attributes on a class `do` with
  FS0522. Which element a class `let` occupies is an open question for the user rather than a
  guess to encode.

### 5. The `.fsi` leg

`ModuleSignatureElement.ValLiteral` currently lands in the discard arm at
`SignatureResolution.fs:756` — a signature literal's value is parsed and thrown away, so
`ExternalMember.ConstValue` has no producer from a Vesper signature. `SigCtx`
(`Passes/SignatureResolution/Context.fs:31`) carries the `PassContext` the checker takes, and
`SignatureResolution.fs:706`'s attribute arguments already reach it through `AttributeFold.build`,
so the leg is: run `ConstExprCheck.check` over `ValLiteral`'s RHS at the signature's own use
site and publish the checked constant.

`ConformanceSurface.comparableArgs` (`ConformanceSurface.fs:46`) then compares two values derived
the same way, which it does not today.

Done when: a `[<Literal>]` declared in a referenced assembly's signature folds at a use site in
another assembly, and `.fsi`/`.fs` attribute disagreement is reported on argument expressions.

### 6. The full attribute-argument domain

Add `Null`, `TypeOf` and `ArrayLit` to the checker: `[<Foo(null)>]`, `[<Foo(typeof<T>)>]`,
`[<Foo([| 1; 2 |])>]`. `null` is a distinct node, following `TExprG.Null` (`TastExpr.fs:65`)
rather than becoming a `TConstValue` case.

Done when: each round-trips through the codec and reaches a backend.

### 7. The backend encodability gate

Declare an interface in SemanticAnalysis — a `TConstExpr` in, a verdict out — supplied to
`PassContext` by the driver the way `IExternalSymbolProvider` is. Run it at the stage-4 site so
the diagnostic is anchored in source alongside every other analysis finding.

CLR implements what `AttributeBlob.tryClassify` (`AttributeRows.fs:119-136`) decides today:
pointer-width integrals, `decimal` (fsc lowers it to `DecimalConstantAttribute`), `unit`, and
string-valued enums have no II.23.3 encoding, and a named enum-typed argument needs a full name
(`ForeignEnum`). JS accepts everything, emitting no attributes.

`AttributeRowPrep.fs:102-105` stops reporting; by the time it runs, every surviving argument is
encodable.

Done when: a `decimal` attribute argument is represented in the frozen tree, reported when
compiling for CLR, and silent when compiling for JS, with `PlatformTypes.fs` as the shape
precedent for a target-conditioned analysis diagnostic.

### 8. CLR encoding for the new cases

`AttributeBlob.tryElem` (`AttributeRows.fs:82-105`) writes scalars only. Add `SZARRAY` (`0x1D`)
and `Type` (`0x50`), including the assembly-qualified name a `Type` argument's SerString needs.

### 9. Wider operator domain

Admit `+`, `*`, `<<<` and string concatenation, now that operators resolve rather than match by
token. The constraint carried from the superseded emission plan still binds: a primitive's
arithmetic is platform-defined (the JS bodies compute in float64 behind `Math.imul` / `| 0`), so
widen only where the targets agree by construction — integral two's-complement operations,
`bool`, string concatenation — and keep `float`, `float32` and `decimal` arithmetic out.
Representing a `decimal` *literal* stays in; folding `1.0M + 2.0M` does not.

### 10. `nameof`

`nameof` has no lexer token, no Vesper.Core declaration and no recogniser anywhere in the
compiler: it arrives as `Expr.App(Expr.Ident "nameof", arg)` and is a missing language feature,
not an attribute-fold detail. It is valid in ordinary expression position too, so implement it
there and let the constant checker recognise the same node.

### 11. Closeouts

- `AttributeUsage`'s `Inherited` flag is decoded nowhere and has no ptest pin.
- `OptionalDefault.Const` (`TastExpr.fs:25-30`) holds a `TConstValue`; decide whether it widens to
  `TConstResult` once `[<DefaultParameterValue>]` is checked rather than read from metadata.
  Today `OptionalDefault` is populated only by the external declaration readers.

## Out of scope

Enum case values stay an early, literal-only fold. `EnumCaseValues.fs:24-25` states the contract:
read once at type registration, every later pass reads the registered `EnumTypeInfo.Cases`. The
value is part of the type's declared shape and type registration depends on it. fsc additionally
accepts a named literal as an enum case's value; admitting that would need a restricted
named-constant lookup at that one position, and is deliberately not planned here.

## Assumptions

1. `[<Literal>] let X = 1` takes the literal's own type. **Confirmed.**
2. A *local* `[<Literal>]` reference becomes a constant at its use sites, matching what
   `ElaborateExpr.fs:29` already does for an external one. **Confirmed.**
3. `typeof<'T>` naming a declaration's own type parameter is rejected, so a `TypeOf` operand is
   always ground. **Confirmed by probe** — FS3187, for an owning type parameter and a binding's
   own alike.
4. A declared type is not propagated into a literal; `null` is the one form that takes its type
   from the position. **Confirmed by probe** — FS0267 for `[<Long(1)>]`, accepted for
   `[<Long(1L)>]` and for a bare `null` against a `string` parameter.
5. `null`, array literals, `nameof` and string concatenation are all valid attribute arguments.
   **Confirmed by probe.**
6. Attribute constructor selection is by argument count then per-candidate checking, with an
   ambiguity reported rather than silently ordered. **Open** — not probed; overload behaviour
   with several same-arity constructors is untested.
7. `TConstExpr` in the frozen tree is a tooling surface with no codegen consumer, so no backend
   is required to read anything but the result. **Open.**
