# Measures plan

The build plan for **units of measure** in the semantic-analysis pipeline.
Same shape as [`generalisation-plan.md`](generalisation-plan.md): the parser
already produces the syntax, semantic-side is a placeholder, and the work is
mostly wiring + the abelian-group unification step.

The status quo is silent on measures. `Constant.MeasuredLiteral(value, _, _, _)`
types as `int` (drops the measure entirely —
[`Unification.fs:175`](../Passes/Unification.fs)), `Type.MeasureType` falls
through to a free TypeVar in `translateType` ([`Unification.fs:304`](../Passes/Unification.fs)),
and `MeasureTerm = MeasurePlaceholder` in [`SemanticInfo.fs:33`](../SemanticInfo.fs)
is a deliberate stub. `TypeVar.Units : MeasureTerm list` is plumbed but never
read or written.

The canonical example we want to type-check after this lands:

```fsharp
let speed (d : float<m>) (t : float<s>) = d / t
let v = speed 100.0<m> 5.0<s>      // v : float<m/s>
let bad = 1.0<kg> + 2.0<m>          // mismatch diagnostic
```

## Goal

After `Unification` finishes, every numeric expression's `SemType` carries
its **measure** — a sorted `(unitName, exponent)` exponent vector. Two
measured types unify iff their numeric carriers (`int`, `float`, …) match
**and** their measures are equal as abelian-group expressions
(after sorting and cancelling zero exponents).

Dimensionless numerics (`42`, `let x = 1.0`) carry the empty measure `[]`,
which is the abelian-group identity. So `1 + 1<m>` is a mismatch (empty vs
`[("m", 1)]`); `1 + 1` and `1<m> + 1<m>` both work.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `Measure<'T>` CST DU                   | `XParsec.FSharp/Expr.fs:734`                    | Done — Named/One/Anonymous/Typar/Juxtaposition/Power/Product/Quotient/Reciprocal/Paren. |
| `Constant.MeasuredLiteral`             | `Expr.fs:760`                                    | Done — parsed by `pMeasure` in `ConstantParsing.fs:51`. |
| `Type.MeasureType`                     | `Expr.fs:94`                                     | Done — type-level measure annotations. |
| `TypeArg.Measure` (generic measure arg)| `Expr.fs:102`                                    | Done — for `float<'u>`. |
| `TypeVar.Units : MeasureTerm list`     | `SemanticInfo.fs:45`                             | Plumbed, never used — repurpose or leave alone (see §Open questions). |
| `MeasureTerm` placeholder              | `SemanticInfo.fs:33`                             | Stub `\| MeasurePlaceholder` — replace with the exponent-vector record. |
| `inferConst` for `MeasuredLiteral`     | `Unification.fs:175`                             | Drops the measure today — must consume the `Measure<SyntaxToken>` payload. |
| `translateType` for `Type.MeasureType` | `Unification.fs:304`                             | Falls through to free TyVar today — must produce a `TyMeasured`. |

The pieces missing are:
1. A real `MeasureTerm` (sorted exponent vector + algebra: `mul`, `inv`,
   `pow`, `normalise`, `equal`).
2. `MeasureTranslation` — walk a `Measure<SyntaxToken>` CST and produce a
   `MeasureTerm`. Mirrors `translateType` for the measure CST.
3. A `SemType` carrier for measured numerics — proposed as a new variant
   `TyMeasured of name * MeasureTerm` so a measured type is structurally
   distinct from a plain `TyConst`.
4. `unify` and `zonk` updates for the new variant.
5. Measure-aware arithmetic — at minimum `+`, `-` (same-measure check); ideally
   `*`, `/` (measure combination). See §Operators.
6. TAST representation — `TyMeasured` flows through `Freeze` unchanged.

## The algorithm: abelian-group unification with ground measures

Knaughton-style: a measure is a finite map from unit names to integer
exponents. The algebra is multiplicative, so juxtaposition / `*` adds
exponents and `/` subtracts them. After every operation, **normalise**:
drop zero exponents, sort by unit name.

For v1 we restrict to **ground measures** — no measure variables. With
that restriction, "two measures unify" is just normalised structural
equality (one list comparison). Measure polymorphism (`'u`) is deferred
to a v2 of this plan (see §Out of scope).

Examples after normalisation:

| Source         | `MeasureTerm`                          |
|----------------|----------------------------------------|
| `<>` (none)    | `[]`                                   |
| `<1>`          | `[]`                                   |
| `<m>`          | `[("m", 1)]`                           |
| `<m^2>`        | `[("m", 2)]`                           |
| `<m s>`        | `[("m", 1); ("s", 1)]`                 |
| `<m/s>`        | `[("m", 1); ("s", -1)]`                |
| `<m/s^2>`      | `[("m", 1); ("s", -2)]`                |
| `<m s / s>`    | `[("m", 1)]` (after cancellation)      |
| `<kg m / kg>`  | `[("m", 1)]`                           |

## Data-model changes

### `SemanticInfo.fs`

Replace the placeholder `MeasureTerm` with a real exponent vector. A
sealed class (rather than a plain `(string * int) list` alias) so we can
hang an `Empty` / `IsDimensionless` helper and prevent callers from
forgetting to normalise:

```fsharp
/// Abelian-group expression over named unit atoms. Always stored in a
/// normalised form: exponents are sorted by unit name and zero exponents
/// are dropped. Equality is structural list equality after normalise.
/// `Empty` is the group identity (dimensionless).
[<Sealed>]
type MeasureTerm private (exponents: (string * int) list) =
    /// `exponents` must be pre-normalised. Use the static `ofList` for
    /// raw input.
    member _.Exponents = exponents
    member _.IsDimensionless = List.isEmpty exponents

    static member Empty = MeasureTerm([])

    static member ofList (raw: (string * int) list) : MeasureTerm =
        raw
        |> List.groupBy fst
        |> List.map (fun (n, xs) -> n, xs |> List.sumBy snd)
        |> List.filter (fun (_, e) -> e <> 0)
        |> List.sortBy fst
        |> MeasureTerm

    override this.Equals(other) =
        match other with
        | :? MeasureTerm as other -> this.Exponents = other.Exponents
        | _ -> false

    override this.GetHashCode() = hash exponents
```

Module-level algebra (separate from the type so the type stays minimal):

```fsharp
module MeasureTerm =
    let empty = MeasureTerm.Empty
    let isDimensionless (m: MeasureTerm) = m.IsDimensionless

    let mul (a: MeasureTerm) (b: MeasureTerm) : MeasureTerm =
        MeasureTerm.ofList (a.Exponents @ b.Exponents)

    let inv (m: MeasureTerm) : MeasureTerm =
        m.Exponents |> List.map (fun (n, e) -> n, -e) |> MeasureTerm.ofList

    let div (a: MeasureTerm) (b: MeasureTerm) : MeasureTerm = mul a (inv b)

    let pow (m: MeasureTerm) (k: int) : MeasureTerm =
        if k = 0 then empty
        else m.Exponents |> List.map (fun (n, e) -> n, e * k) |> MeasureTerm.ofList
```

New `SemType` variant — the measured numeric type. Plain `TyConst "int"`
(no measure) is **not** the same as `TyMeasured("int", MeasureTerm.empty)`;
the latter has been through the measure machinery and the former hasn't.
Numeric literals always lift into `TyMeasured` form so the measure axis is
always present for arithmetic types:

```fsharp
type SemType =
    | TyVar of TypeVar
    | TyConst of name: string
    /// Numeric type with attached measure. Non-numeric types (bool, unit,
    /// string) stay as TyConst — they have no measure axis.
    | TyMeasured of name: string * measure: MeasureTerm
    | TyFun of arg: SemType * result: SemType
    | TyTuple of items: SemType list
```

### `Unification.fs` — `inferConst` / `translateType`

`MeasuredLiteral` consumes the `Measure<SyntaxToken>` payload and
produces a `TyMeasured`:

```fsharp
| Constant.MeasuredLiteral(value = t; measure = m) ->
    let carrier = inferConstCarrier t          // numeric kind by lexer token
    let mt = translateMeasure ctx m
    TyMeasured(carrier, mt)
```

`Constant.Literal` for a numeric token also lifts into `TyMeasured` with
`MeasureTerm.empty` so `42 + 42<kg>` is a clean mismatch (empty vs `[kg]`)
rather than a type-axis mismatch (`int` vs `TyMeasured`).

`translateMeasure` mirrors `translateType`. A new private helper —
roughly 40 lines — walking the `Measure<'T>` CST:

```fsharp
let rec private translateMeasure (ctx: PassContext) (m: Measure<SyntaxToken>) : MeasureTerm =
    match m with
    | Measure.One _ -> MeasureTerm.empty
    | Measure.Named li when li.Idents.Length = 1 ->
        MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], 1 ]
    | Measure.Power(inner, _, neg, expTok) ->
        let e = parseIntToken ctx expTok
        let signed = if neg.IsSome then -e else e
        MeasureTerm.pow (translateMeasure ctx inner) signed
    | Measure.Product(l, _, r) -> MeasureTerm.mul (translateMeasure ctx l) (translateMeasure ctx r)
    | Measure.Quotient(l, _, r) -> MeasureTerm.div (translateMeasure ctx l) (translateMeasure ctx r)
    | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx inner)
    | Measure.Paren(_, inner, _) -> MeasureTerm.empty |> ignore; translateMeasure ctx inner
    | Measure.Juxtaposition(elems, _) ->
        elems |> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx m)) MeasureTerm.empty
    | Measure.Anonymous _
    | Measure.Typar _
    | Measure.Named _ ->
        // Measure variables / qualified names — diagnose; v2 territory.
        ctx.Diagnostics.Add { Key = …; Message = "Measure variables not yet supported"; Severity = Error }
        MeasureTerm.empty
```

`Type.MeasureType` falls into a similar pattern — see §Open questions for
whether `Type.MeasureType` should ever appear in expression-position type
annotations (it's more relevant inside generic args).

For binding-level type annotations like `(x : float<m>)`, `Type.AppliedType`
carries the carrier and a `TypeArg.Measure` arg. `translateType` needs to
recognise the shape and produce `TyMeasured`. The exact CST shape:

```fsharp
| Type.AppliedType(typ = carrier; typeArgs = args) when isNumericCarrier carrier && args.Length = 1 ->
    match args.[0] with
    | TypeArg.Measure m -> TyMeasured(nameOfCarrier ctx carrier, translateMeasure ctx m)
    | TypeArg.Type _ -> /* not measure-shaped — fall through */
```

### `Unification.fs` — `unify`

Two new arms next to the existing `TyConst n1, TyConst n2`:

```fsharp
| TyMeasured(n1, m1), TyMeasured(n2, m2) when n1 = n2 && m1 = m2 -> ()
| TyMeasured(n1, m1), TyMeasured(n2, m2) when n1 = n2 ->
    ctx.Diagnostics.Add {
        Key = key
        Message = sprintf "Measure mismatch: %s<%O> vs %s<%O>" n1 m1 n2 m2
        Severity = Error
    }
| TyMeasured(n1, _), TyConst n2
| TyConst n1, TyMeasured(n2, _) when n1 = n2 ->
    // Measured vs dimensionless on the same carrier — diagnose; the
    // unification can't make them equal without an inference move that
    // would be unsound (silently dropping the measure).
    ctx.Diagnostics.Add { ... "Measured value used where dimensionless expected" ... }
```

For TyVar-vs-`TyMeasured` the existing `TyVar tv, other` arm covers it —
`occursAndAdjust` walks `TyMeasured` as a leaf (no inner TyVars to check)
and the Link is set normally. `MeasureTerm` carries no TyVars so the
occurs check ignores it; level adjustment ignores it.

### `Unification.fs` — operators

This is where the v1 / v1.5 split lands. Two parts:

**v1: `+`, `-` are measure-preserving.** `inferInfix` for `op_Addition` /
`op_Subtraction` / `op_Equality` etc. consults a measure-aware dispatcher
before the provider:

```fsharp
let private tryMeasuredArith (name: string) (leftTy: SemType) (rightTy: SemType) : SemType option =
    match name, resolveStep leftTy, resolveStep rightTy with
    | ("op_Addition" | "op_Subtraction"), TyMeasured(n1, m1), TyMeasured(n2, m2)
        when n1 = n2 && m1 = m2 -> Some (TyMeasured(n1, m1))
    | _ -> None
```

If the dispatcher returns `Some`, `inferInfix` returns that and skips the
provider lookup. If `None`, fall through to the existing path. The existing
monomorphic `op_Addition : int -> int -> int` in `MockBuiltins` handles
dimensionless cases via the fall-through.

**v1.5: `*`, `/` combine measures.** Same dispatcher with a `mul`/`div`
arm:

```fsharp
| "op_Multiply", TyMeasured(n1, m1), TyMeasured(n2, m2) when n1 = n2 ->
    Some (TyMeasured(n1, MeasureTerm.mul m1 m2))
| "op_Division", TyMeasured(n1, m1), TyMeasured(n2, m2) when n1 = n2 ->
    Some (TyMeasured(n1, MeasureTerm.div m1 m2))
```

This is a special case — the operator is no longer a single typed scheme.
A cleaner v2 design replaces the dispatcher with measure-polymorphic
operator schemes that the provider returns, instantiated per use site;
that needs measure typars to land first.

### `Freeze.fs`

Trivial: `TyMeasured` flows through unchanged. The TAST inherits the new
variant via the shared `SemType`. No new `TConstValue` / `TPat` shape —
measured literals are `TConstValue.Float` (etc.) carrying the value, plus
a `TyMeasured` in the surrounding `ty` field.

## Pipeline integration

No new pass. Measures are computed inside `Unification` alongside types —
matches the spec ([§Phase 4.3](../../../semantic-analysis.md#phase-43-unification-types--units))
which already groups types and units into one phase.

Pre/postconditions unchanged. The only side-table impact is that
`ctx.TypeVar` now stores TyVars whose Link can be `TyMeasured`; consumers
that exhaustively match on `SemType` need the new case.

`docs/passes.md` may need a one-line update mentioning measures in the
Unification description, but no new contract.

## Test strategy

The headline test: `let v = speed 100.0<m> 5.0<s>` types `v : float<m/s>`.
Full set:

1. **Measured literal carries its measure.** `let x = 1.0<m>` types as
   `float<m>`.
2. **Dimensionless and measured don't unify.** `let r = 1.0 + 1.0<m>`
   emits a mismatch.
3. **Same-measure addition works.** `let r = 1.0<m> + 2.0<m>` types as
   `float<m>`.
4. **Different-measure addition fails.** `let r = 1.0<m> + 2.0<kg>`
   emits a measure-mismatch diagnostic.
5. **Type annotation with measure.** `let f (x : float<m>) = x` types
   `f : float<m> -> float<m>`.
6. **Measure normalisation.** `<m s / s>` and `<m>` unify (cancellation).
7. **Measure normalisation: power.** `<m^2 / m>` and `<m>` unify.
8. **Generalisation × measures.** `let id = fun x -> x in id 1.0<m>, id 1.0<s>`
   types as `float<m> * float<s>` (id's scheme has a type variable; each
   use-site mints fresh, the measures attach at the use site).
9. *(v1.5)* **Measured multiplication.** `let area = 3.0<m> * 4.0<m>` types
   as `float<m^2>`.
10. *(v1.5)* **Measured division.** `let v = 100.0<m> / 5.0<s>` types as
    `float<m/s>`.

Each test follows the `CoverageTests.fs` / `GeneralisationTests.fs` pattern
— `analyse + declType + Expect.equal + Expect.isEmpty Diagnostics`.

Add `tyMeasured` / `mFloat` builders to `MockBuiltins` for terser test
assertions.

## TAST representation

The frozen TAST carries inferred types inline, so the new `TyMeasured`
variant just flows through `Freeze`. `TConstValue` doesn't need to grow —
a measured literal is `TConstValue.Float 1.0` with a surrounding
`TyMeasured("float", [("m", 1)])` in `ty`. Downstream consumers that want
to know "this float is in metres" read the `ty` field. The
`TastShape.prettyDecl` helper in the tests needs a render arm for
`TyMeasured` — `"float<m>"` etc.

## Open questions

- **`TypeVar.Units` field — keep or drop?** The plan above puts the
  measure on `SemType` (in `TyMeasured`), not on `TypeVar`. That contradicts
  [`docs/typevar.md`](typevar.md)'s "Axis 2: TypeVar.Units". Reasons to
  prefer the `SemType` carrier: (a) zonking preserves measure naturally —
  no special case for "TyVar resolves to TyConst but carries measure on
  the way out"; (b) once measure typars land, they're just typars and can
  go in a typar-substitution table without needing a separate "measure
  variable" slot; (c) one less mutable axis on the hot `TypeVar` class.
  Reason to keep `TypeVar.Units`: SRTP-style "this typar must carry a
  measure" bounds. Defer the decision — for v1, leave the field in place
  unused; revisit when measure typars land.
- **Measure typars (`'u`).** Deferred to v2. The unification algorithm
  for abelian groups with variables is well-known
  (Knaughton 1995, used by FCS) but considerably more involved than
  ground-term equality. Land v1 (ground measures) first, write tests
  that exercise non-trivial cancellation, then layer typars on top.
- **Generic measured types from the provider.** `Microsoft.FSharp.Core.float<'u>`
  is the canonical example. The provider returns a `TyMeasured` for an
  external measured value, but the carrier name comes from the provider's
  symbol table — needs a "I know about `float<…>`" hook. For v1 the
  built-in numeric primitives suffice; cross this when records / DUs
  push us toward a real named-type registry.
- **`Type.MeasureType` placement.** This appears in CSTs for raw
  `Type.MeasureType` nodes — currently only inside generic args, but
  the parser allows it elsewhere. If we see it bare in a type annotation
  (`let x : <m> = …`) it's almost certainly a parse-side mistake;
  diagnose loudly rather than silently producing a free TyVar.

## Out of scope for this plan

- **Measure typars and measure-polymorphic schemes.** v2. Requires the
  abelian-group word problem with variables — solvable but bigger than
  ground unification. Once landed, measure-polymorphic `(*) : 'a<'m1> -> 'a<'m2> -> 'a<'m1 'm2>`
  replaces the dispatcher in §Operators.
- **Automatic measure conversion.** F# doesn't do `m -> ft` implicitly
  and neither do we.
- **Measure attribute on types from the provider.** `[<Measure>] type kg`
  declarations are part of named-type definition, which lands when
  records / DUs do.
- **Negative literal exponents in measure-power.** `<m^-2>` is supported
  by the parser via the `neg` token in `Measure.Power`; `translateMeasure`
  handles it. Nothing else to do.
- **Float-valued exponents.** `<m^0.5>` — F# doesn't support; we don't either.
- **Integration with SRTPs.** "`^T` is numeric and carries a measure" is
  an SRTP-shaped bound that needs the on-unified callback machinery from
  [`docs/typevar.md`](typevar.md). Lands with SRTPs.
