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
error rather than a widening. `null` and `[||]` are the two forms with no type of their own
and take the parameter's, as `[<S(null)>]` against a single `string` parameter shows.

| Position | Checked against |
|---|---|
| Attribute constructor argument | the attribute class's constructor parameter |
| Named argument `Prop = v` | the property's declared type |
| `[<Literal>] val X: int = 3` | the annotation |
| `[<Literal>] let X = 1` | nothing; the literal's own type governs |
| Enum case `\| A = 1` | the enum's underlying type |
| `[<DefaultParameterValue(0)>]` | the parameter's declared type |

An `obj` position is the exception to "no widening": a parameter, property or array element
declared `obj` admits any constant, including `null`, `typeof<T>` and an array, and the CLR
blob encodes it boxed (II.23.3 `0x51`). `[<F(1)>]` against `F(x: obj)` is accepted where
`[<N(1)>]` against `N(x: int64)` is FS0267.

Constructor selection stays finite: filter by argument count, check the arguments against each
candidate's parameters, take the unique success. Where two candidates both admit every
argument, the one with no `obj` parameter at a position where the other has one wins, and
any other tie is FS0041. Probed: `M(obj)`/`M(string)` takes `M(string)` for `null` and for
`"s"`, `V(obj)`/`V(Type)` takes `V(Type)` for `typeof<int>` and for `null`, and
`B(string)`/`B(Type)` is FS0041 for `null`. No metavariables, no union-find. The same
checker therefore serves the `.fs` and `.fsi` paths, and attribute arguments never enter
`Unification` on either.

A named argument `x = v` is matched against the chosen constructor's parameter names before
the class's settable properties and fields: `[<S3(y = 2, x = 1)>]` selects `new(x: int, y: int)`
and `[<Z("a", y = 1)>]` fills either a trailing parameter or a property named `y`.

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

`Resolution.LiteralValues` is now a `BoundVarTable<TConstDenotation>` keyed by bound variable,
holding what the RHS denotes, so a `LiteralRef` carries the referent's own type — an enum-typed literal keeps
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

### 5. The `.fsi` leg — LANDED

The plan above misread the CST: the parser keeps `[<Literal>] val X: int = e` on
`ValSig.literalValue`, and `ModuleSignatureElement.ValLiteral` had no producer, so it is
deleted. The leg is therefore in `registerValSig`: `signatureLiteral` runs
`ConstExprCheck.check` over the trailing expression at the signature's own use site and
requires the checked type to equal the annotation's template, per the Shape table above.
A `[<Literal>]` without a value, a value without `[<Literal>]`, and a value of another type
are each reported at the signature.

Both publication ends landed together, as stage 3 deferred:

- `ExternalSymbol.Literal: TConstDenotation voption` is the const slot. Every consumer reads
  the value and its type alone, so the slot carries the denotation rather than the checked
  expression, whose anchors point into the declaring file. `ConstExprCheck` folds a published
  literal through it, and `ElaborateIdents.translateIdent` substitutes it at a use site
  (bare ident and `r.X` chain anchor alike) as it does for a local one.
- `ModuleBindingInfo.Literal` carries a `.fs` binding's denotation into the frozen tree,
  read off `Resolution.LiteralValues` by the binding's bound variable at
  `Elaborate.exportedBindingInfo`, so a `.fs` without a signature publishes the same slot
  through `FrozenSignature.addValue`. The side table stays: the checker reads it during name
  resolution, before `ModuleMembers` exists.

Conformance compares the two halves' literals by `TConstDenotation`: `LiteralValueDiffers`
and `LiteralOnOneHalf` are fsc's FS0034. Attribute arguments already compared by denotation
after stage 2, so the second "done when" clause held before this stage.

Regressions: `SignatureResolutionTests` (publication, the three refusals, and a published
literal at a use site in another assembly through both Elaborate and the checker),
`FrozenSignatureTests` (the `.fs` slot survives the blob), and `ConformanceTests` (FS0034).

### 6. The full attribute-argument domain

`TConstExpr.Null`, `TypeOf` and `ArrayLit`, `TConstResult.Null`, `TypeVal` and `ArrayVal`,
their codec cases in `FrozenCodecConst.fs` and their `ConformanceSurface.describeConst`
spellings all landed with stage 2. `ConstExprCheck.check` produces none of them, and
`AttributeBlob.tryElem` (`AttributeRows.fs:82`) writes scalars only. Stage 8's CLR encoding is
absorbed here: each sub-stage lands its own `Elem` form so "reaches a backend" is checkable
per sub-stage. JS emits no attributes and is done when the codec round-trip holds.

The three forms differ in what they need from the position. `typeof<T>` and a non-empty array
of one element type carry their own type and need nothing. `null`, `[||]` and any argument
against an `obj` position take their type from the constructor parameter, which the front end
does not select today: `AttributeFold.foldAttribute` folds each argument with no reference to
the constructor, and selection by arity alone lives in the CLR backend
(`AttributeRowPrep.fs:49-62`). 6a and 6b are bounded checker changes; 6c is the selection.

#### 6a. `typeof<T>`

`Expr.TypeApp` of an applied name resolving to the `typeof` intrinsic binding, with one type
argument. `tryEnumKey` (`ConstExprCheck.fs:108`) already resolves a written type through
`NameResolutionLongIdent.resolveType` and narrows to an enum; 6a needs the general form,
resolving the written `Type<SyntaxToken>` at the use site and freezing it through
`FrozenTypeBridge`. The node's `ty` is `System.Type`'s key. Recognise `typedefof<T>` in the
same arm.

Probed domain: `typeof<int>`, `typeof<int list>`, `typeof<list<_>>`, `typedefof<list<_>>`,
`typeof<int[]>`, `typeof<int * string>` and `typeof<int -> int>` are all accepted. A type
parameter operand is FS3187 (assumption 3), reported at the type argument.

CLR: `AttributeBlob` writes `Type` (`0x50`) followed by the SerString of the type's
assembly-qualified name for a referenced type, or its full name alone for a type of the
assembly under emission. A type the encoder cannot spell (an anonymous or structural form)
is `AttributeBlobRejection.UnencodableValue` until stage 7 moves the verdict upstream.

Done when: `[<L(typeof<int>)>]` round-trips through the codec, `ConstExprCheckTests` pins the
accepted domain and FS3187, and `AttributeRowTests` reads the `Type` element back from an
emitted assembly.

#### 6b. Non-empty array literals

`Expr.ArrayOrList` in its array form with at least one item. Each item is checked with
`check`; every item must have exactly the first item's type, and the node's `ty` is the
frozen array of it. fsc reports a differing item as FS0267 at the item, not FS0001:
`[<D([| 1; 2L |])>]` is FS0267 at `2L`. A nested array is FS0267 at the inner `[|`, so an
item that is itself an `ArrayLit` is refused. `[||]`, `null` items and `obj[]` positions wait
for 6c.

fsc refuses `byte[]` and `uint16[]` literals (`[<K([| 1uy |])>]` and `"s"B` are both FS0267)
while accepting every other primitive element type. II.23.3 encodes both, and the refusal is
a front-end gate on a target-neutral value, so Vesper accepts them. Recorded as a stated
parity departure alongside `decimal` and `nativeint` in the Shape section.

CLR: `SZARRAY` (`0x1D`) followed by the element's `FieldOrPropType` byte, a `uint32` count
and each element's `Elem`. An enum-typed element writes at its underlying width, as a
positional enum scalar does today. `tryClassify` becomes recursive over `TConstResult` rather
than a scalar match.

Done when: `[<D([| 1; 2 |])>]` and `[<J([| E.A; E.B ||| E.C |])>]` round-trip and read back
from an emitted assembly, and the mixed-element and nested-array refusals are pinned.

#### 6c. Constructor selection, `null`, `[||]` and `obj` positions

The front end selects the constructor and records it on `TAttribute`, so the backend reads
a handle rather than choosing by arity: `AttributeCtorResolution`'s `NoMatchingCtor`,
`AmbiguousCtor` and `NoExternalCtor` skips become front-end diagnostics at the attribute.

**Candidates.** An external class's constructors come from
`IExternalSymbolProvider.TryLookupMembers(key, ".ctor")` with ground `ExternalSignature`s. A
local class's `ClassCtorParamInfo.Type` is an inference cell (`TypeInfos.fs:421`), so a
local attribute class's parameter types are the resolved ANNOTATIONS, read the way
`ClassFieldInfo` holds a `val` field's declared type. An unannotated parameter on a class
used as an attribute is refused at the use, since the attribute's blob needs a declared type
and fsc's inference to `obj` is a fact about fsc's inference order. Record the declared
types on `ClassCtorParamInfo` as a `Declared: SemType voption` beside the cell rather than
re-reading the CST at each attribute.

**When.** A type declaration's attributes are declared at `DeclRegistration`, before
`MemberRegistration` resolves any class's annotations, so a local attribute class's
parameters are not resolvable at that moment. Selection therefore runs where target
enforcement already runs, in `Passes.Attributes.run` after every declaration is filed, and
`check` moves there with it: `AttributePosition` holds the resolved attributes and use site
at declaration and gains its checked form in `run`. The FS0039 rule stage 4 cites is a
name-resolution scoping fact carried by the `UseSite`, so the fold's result is unchanged by
running later. Write the test that proves that first: `[<Tag(LaterLit)>]` above the literal
stays FS0039 with the fold in `run`.

**Selection.** Filter candidates by positional count, then check each positional argument
against the candidate's parameter with `expected = ValueSome paramTy`, taking the stage 3
deferral: `check` gains `expected: FrozenType voption`, `ValueNone` at every existing call
site. An argument checks against a parameter when its own type equals it, or the parameter
is `obj`. `Null` and an empty `ArrayLit` are the two nodes that take `expected` as their
`ty`; `null` against a non-nullable or absent expectation is refused. Where more than one
candidate admits every argument, prefer the candidate whose parameters are not `obj` at a
position where another's are; a remaining tie is FS0041 at the attribute, and a single-
candidate mismatch is FS0001 at the argument (probed: `[<S2("s")>]` against `S2(x: int)`).
A positional count matching no candidate is FS0505.

**Named arguments.** After the constructor is chosen, `Name` resolves against its parameter
names first, then the class's settable properties and fields, each supplying the expected
type. An `obj` named argument boxes as a positional one does. `TAttributeArg` records which
kind the name resolved to, since the blob writes a named parameter positionally and a
property under `0x54`. fsc's FS3172 on an `obj`-typed property named argument is an fsc
defect and is not mirrored.

**Element expectations.** Once `expected` exists, an `ArrayLit` against `T[]` checks each
item with `expected = ValueSome T`, which is what admits `[| 1; "a" |]` and `[| null |]`
against `obj[]`, and `[||]` against any array parameter. An `ArrayLit` against `obj` is
checked with no element expectation and boxed whole.

CLR: `tryClassify` reads the recorded constructor's parameter types, and writes `0x51`
followed by the value's own `FieldOrPropType` and `Elem` at each `obj` position. `Null` for a
string, type or array parameter is the II.23.3 null form (`0xFF` for `Type` and string,
`0xFFFFFFFF` count for an array). `AttributeRowPrep`'s arity match is deleted, and
`ClrProvider.TryExternalAttributeCtor` takes the recorded member key instead of a count.

Done when: every acceptance and refusal recorded in this stage and in assumptions 6 to 9 is
pinned by a test, `AttributeFoldTests` pins the overload preference
and FS0041, the frozen tree carries the chosen constructor, and an emitted assembly reads
back a boxed `obj` argument and a `null` argument through reflection.

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

### 8. CLR encoding for the new cases — folded into 6a, 6b and 6c

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
   ambiguity reported rather than silently ordered. **Confirmed by probe, with one
   preference rule.** Same-arity candidates are distinguished by the literal's own type
   (`A(int)`/`A(int64)`/`A(string)` under `1`, `1L`, `"s"`). Where two admit the argument,
   the candidate without an `obj` parameter wins, verified by reflection on the emitted row.
   `null` between `string` and `Type` is FS0041; between `string` and `int` it selects
   `string`. A wrong count is FS0505 and a single-candidate mismatch is FS0001.
7. `TConstExpr` in the frozen tree is a tooling surface with no codegen consumer, so no backend
   is required to read anything but the result. **Open.**
8. An `obj` position admits any constant without a cast. **Confirmed by probe** — `1`,
   `null`, `typeof<int>`, `[| 1; 2 |]` and `[| "a" |]` against `F(x: obj)`, and `[| 1; "a" |]`,
   `[| null |]` and `[| typeof<int> |]` against `obj[]`. `box 1` is FS0267: the boxing is the
   position's, never written.
9. Array literal items share one type and do not nest. **Confirmed by probe** —
   `[| 1; 2L |]` and `[| 1; 2 |]` against `int64[]` are FS0267 at the item, `[| [| 1 |] |]`
   is FS0267 at the inner array, and `[||]` between `int[]` and `string[]` candidates is
   FS0041. `byte[]` and `uint16[]` are refused by fsc and accepted here (6b).
10. Nothing in the frozen tree needs the constructor today, so 6c adds it rather than
    changes an existing consumer: the CLR row writer re-selects by arity and `EmitResolve`
    re-selects for `TExpr.New` by unified types. **Open** whether `TExpr.New`'s recorded
    `Resolution.ExternalCtor` is the right precedent for the field's shape.
