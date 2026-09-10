# `nameof` — execution plan

Ephemeral: delete when the work lands.

**DEFERRED.** No stage below is started, and the constant-expression work lands without it.

## Status

`nameof` is a missing LANGUAGE FEATURE rather than an attribute-fold detail, the shape `typeof`
turned out to have: a `Vesper.Core` declaration (`reflect.fsi`), a use-site recogniser
(`ConstExprCheck.Reified`) and a backend lowering, rather than a case in the constant checker
alone. It has no lexer token, no `Vesper.Core`
declaration and no recogniser anywhere in the compiler: it arrives as
`Expr.App(Expr.Ident "nameof", arg)`, or `Expr.HighPrecedenceApp` for `nameof(x)`, and every
pass reads the applied name as an ordinary identifier, so `Scope.reportExpr`
(`Passes/NameResolution/Scope.fs:90`) reports "Unresolved identifier: nameof".

Three pieces landed with the constant tier and have no producer:

- `TConstExpr.NameOf(target: SymbolKey, name: string, ty, tok)` (`TastConstExpr.fs:32`).
- Its codec case 5 (`FrozenCodecConst.fs:149,193`).
- `TConstExpr.result` answering `Scalar(String name)` (`TastConstExpr.fs:114`), which supplies
  `AttributeBlob` and `ConformanceSurface.describeConst` with the node's spelling. Both
  backends are therefore complete for `nameof` before the front end produces one: the CLR blob
  writes a string, and JS emits no attribute.

`nameof` is valid in ordinary expression position, so the feature belongs there and the
constant checker recognises the same node — the split `typeof` took.

## What F# accepts

Probed with `dotnet fsi`. The value is the target's SOURCE name: its last segment, with
`[<CompiledName>]` unread, a generic type's arity suffix absent, and an operator under its
symbolic spelling.

| Written | Value |
|---|---|
| `nameof local`, a lambda or function parameter | `"local"` |
| `nameof M.f`, `nameof List.map` | `"f"`, `"map"` |
| `nameof M` (module), `nameof System.Collections` (namespace) | `"M"`, `"Collections"` |
| `nameof M.T`, `nameof int`, `nameof System.String` | `"T"`, `"int"`, `"String"` |
| `nameof M.G` where `G<'a,'b>` | `"G"` |
| `nameof r.Field`, `nameof M.CaseA`, `nameof M.E.Red` | `"Field"`, `"CaseA"`, `"Red"` |
| `nameof System.String.Empty`, `nameof x.Go` on an interface | `"Empty"`, `"Go"` |
| `nameof M.original` under `[<CompiledName("Renamed")>]` | `"original"` |
| `nameof (+)` | `"+"` |
| `nameof<'T>` | `"T"` |

Refusals:

- `nameof (1 + 1)` is FS3250, "Expression does not have a name".
- `nameof undefinedThing` is FS0039.
- `let f = nameof` is FS3251, "Using the 'nameof' operator as a first-class function value is
  not permitted".
- `nameof System.String.Concat` is FS0041: an overloaded member resolves as it does in
  expression position, and the ambiguity is reported there.

Positions, all accepted: an ordinary expression, an attribute argument
(`[<Mark(nameof value)>]`), a `[<Literal>]` body composed with other constant operators
(`[<Literal>] let Lit = "lit-" + nameof value`), and a PATTERN, where `| nameof value ->`
matches the string `"value"`.

A user `let nameof (x: int) = "shadow"` shadows the intrinsic, and the call answers `"shadow"`.

## Shape

**Recognition is by non-resolution.** `nameof` is a one-segment applied name resolving to no
binding in scope, which is what makes the shadowing above fall out. `typeof` is the contrasting
case: it is declared in `Vesper.Core`'s `reflect.fsi` and recognised by `BindingKey`
(`ConstExprCheck.Reified.tryOfBinding`, `ConstExprCheck.fs:376`). `nameof` admits no such
declaration, because its argument is unevaluated and may name a type, a module or a namespace,
so no signature spells it. The exemption `Scope.reportExpr` carries for the printf family
(`Scope.fs:107-109`), a front-end intrinsic typed without a provider symbol, is the precedent
for the report site.

**The argument is a name, not an expression.** Name resolution and `Unification` visit the
argument as a name to resolve; typing it would report on `nameof int` and `nameof M`, which
name a type and a module in a position that admits neither. Elaboration produces a constant, so
every later walk over the TAST — including the backends' capture analysis — sees no reference
to the target.

**One resolution across the whole name universe.** The argument resolves through
`NameResolutionLongIdent` in expression-position precedence first, then as a type or module
name, then as a namespace prefix, and the first hit supplies the name. `ConstExprCheck`'s
`(|AppliedName|_|)` (`ConstExprCheck.fs:307`) is the CST matcher, already shared by `not` and
the prefix operators.

**The target column** is the one design fork: `SymbolKey` is `Type | Binding | Member`
(`SemanticInfo.fs:84`), and the table above accepts six targets none of the three spells — a
local value or parameter, a record field, a union case, an enum case, a namespace and a type
parameter. See "Open questions".

## Stages

### 1. Expression position

The recogniser, the argument's resolution, and `TExpr.Const(TConstValue.String name)` out of
`ElaborateExpr`. `Scope.reportExpr` exempts a recognised `nameof`, and `Unification` types the
application as `string` without visiting the argument.

Done when: `nameof` over every accepted target in the table returns the source name through the
whole pipeline, a shadowed `nameof` calls the user's binding, and `nameof (1 + 1)`,
`nameof undefinedThing` and a first-class `nameof` each report at the written form.

### 2. The constant checker arm

`ConstExprCheck.checkForm` takes the same node and produces `TConstExpr.NameOf`, so an
attribute argument, a `[<Literal>]` body and a `.fsi` literal admit it. The type is `string`,
the result the name, and the CLR blob follows from the result with no encoder change.

Done when: `[<Mark(nameof value)>]` round-trips through `FrozenCodec` and reads back through
reflection off an emitted assembly, and `[<Literal>] let L = "x-" + nameof value` publishes the
concatenated string.

### 3. Pattern position

`| nameof value ->` is a constant string pattern whose spelling is a `nameof` application.
It shares stage 1's recogniser and resolution, and lowers to the string constant pattern.

### 4. Closeouts

- FS3251 for a first-class `nameof`, FS3250 for an argument with no name: both need their own
  `Kind` cases, since neither is a resolution failure.
- `nameof<'T>` reads the type application rather than an argument, so it is a second CST form.
- An overloaded member argument reports the ambiguity name resolution already computes.

## Open questions

**What identity does the node record?** The value is the name, so the `target` column exists
for the frozen tree's tooling surface: both backends read a `TConstExpr`'s result alone.
Three shapes:

1. **A `NameOfTarget` DU** covering the accepted set — a `SymbolKey` arm plus arms for a local,
   a namespace, a record field, a union case, an enum case and a type parameter, each carrying
   the identity its own kind has.
2. **`SymbolKey voption`**, `ValueNone` for the six targets a key does not spell.
3. **Drop the column**, leaving the name and the anchor.

Recommendation: 1. Option 2 has one `ValueNone` standing for six distinct interpretations,
which this repo's design rules call out as the signal for a DU, and option 3 makes a consumer
re-resolve the name from the anchor, a second derivation of what the checker already computed.
The DU's cases follow the accepted target set, so fixing that set is stage 1's first decision;
a target the DU cannot yet spell is `Kind.NotYetSupported` until an arm carries it.

**Does the expression-position node keep the target?** `TExpr.Const` erases it, as fsc's own
TAST does. Recording it would take a side table or a `TExpr` case with no consumer today.

## Assumptions

1. Recognition is by non-resolution, so a user binding named `nameof` wins. **Confirmed by
   probe** — a `let nameof` in scope answers its own result.
2. The value is the source name under `[<CompiledName>]`, the last segment of a qualified name,
   and a generic type's name without its arity. **Confirmed by probe.**
3. `nameof` accepts a local, a parameter, a module, a namespace, a type, a record field, a
   union case, an enum case, a member and an operator. **Confirmed by probe.**
4. An attribute argument, a `[<Literal>]` body and a pattern each admit `nameof`. **Confirmed
   by probe.**
5. Both backends need no new case, since the node's result is a string scalar. **Open** — held
   by `TConstExpr.result` and `AttributeBlob`'s result-directed dispatch, and unproven until
   stage 2 emits a row.
