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

After `Unification` finishes, every numeric `TypeVar` carries its
**measure** in the `TypeVar.Units` slot — a sorted `(unitName, exponent)`
exponent vector where each exponent is an arbitrary-precision rational
(`bigint/bigint`, normalised to lowest terms). Two measured types unify
iff their numeric carriers (`int`, `float`, …) match **and** their
measures are equal as abelian-group expressions (after sorting,
normalising each exponent's rational, and cancelling zero exponents).

Dimensionless numerics (`42`, `let x = 1.0`) carry the empty measure `[]`,
which is the abelian-group identity. So `1 + 1<m>` is a mismatch (empty
vs `[("m", 1/1)]`); `1 + 1` and `1<m> + 1<m>` both work.

**Why rational exponents from day one.** F# measure exponents are rational
in the underlying type system — `sqrt : float<'u^2> -> float<'u>` is
expressible, and FCS represents exponents as rationals internally. Surface
syntax only permits integer-literal exponents today, but the *type-level*
operations (square root, fractional powers via library functions when they
land) can introduce non-integer exponents. Picking the wider representation
now avoids a retrofit later, and comparing types stays exact (no float
rounding) because rationals are kept in lowest terms.

## What we have to build on

| Piece                                  | Where                                           | Status |
|----------------------------------------|-------------------------------------------------|--------|
| `Measure<'T>` CST DU                   | `XParsec.FSharp/Expr.fs:734`                    | Done — Named/One/Anonymous/Typar/Juxtaposition/Power/Product/Quotient/Reciprocal/Paren. |
| `Constant.MeasuredLiteral`             | `Expr.fs:760`                                    | Done — parsed by `pMeasure` in `ConstantParsing.fs:51`. |
| `Type.MeasureType`                     | `Expr.fs:94`                                     | Done — type-level measure annotations. |
| `TypeArg.Measure` (generic measure arg)| `Expr.fs:102`                                    | Done — for `float<'u>`. |
| `TypeVar.Units : MeasureTerm list`     | `SemanticInfo.fs:45`                             | Plumbed, never used. v1 narrows this to a single `MeasureTerm voption` (a measure is one abelian-group expression, not a list of them) — see §Data-model. |
| `MeasureTerm` placeholder              | `SemanticInfo.fs:33`                             | Stub `\| MeasurePlaceholder` — replace with the exponent-vector struct. |
| `inferConst` for `MeasuredLiteral`     | `Unification.fs:175`                             | Drops the measure today — must consume the `Measure<SyntaxToken>` payload and stamp `Units` on the resulting TyVar. |
| `translateType` for `Type.MeasureType` | `Unification.fs:304`                             | Falls through to free TyVar today — must read the measure and stamp `Units` on the materialised TyVar. |

The pieces missing are:
1. `Rational` — arbitrary-precision rational built on `System.Numerics.BigInteger`, kept in lowest terms with positive denominator. Equality and hashing are structural.
2. A real `MeasureTerm` (sorted `(unitName, Rational)` exponent vector + algebra: `mul`, `inv`, `pow`, `normalise`, `equal`).
3. `translateMeasure` — walk a `Measure<SyntaxToken>` CST and produce a `MeasureTerm`. Mirrors `translateType` for the measure CST.
4. `Units` axis flowing through `union` / `unify` / zonk — analogous to the existing `IfaceBounds` / `SrtpBounds` migration but with abelian-group equality instead of list-append.
5. Measure-aware arithmetic — at minimum `+`, `-` (same-measure check); ideally `*`, `/` (measure combination). See §Operators.
6. TAST representation — pretty-printing `<m s / s^2>` etc.

## The algorithm: abelian-group unification with ground measures

Knaughton-style: a measure is a finite map from unit names to rational
exponents. The algebra is multiplicative, so juxtaposition / `*` adds
exponents and `/` subtracts them. After every operation, **normalise**:
each exponent gets reduced to lowest terms, zero exponents drop out, and
entries sort by unit name. The result is a canonical form — equality is
structural list equality.

For v1 we restrict to **ground measures** — no measure variables. With
that restriction, "two measures unify" is one list comparison. Measure
polymorphism (`'u`) is deferred to v2 (see §Out of scope).

Examples after normalisation (exponents shown as `n` for `n/1`, `n/d`
otherwise):

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
| `<m^2>^(1/2)`  | `[("m", 1)]` (rational power; future)  |
| `<m^4 / m^2>`  | `[("m", 2)]`                           |

## Data-model changes

### `SemanticInfo.fs`

A `Rational` struct first — arbitrary precision over `BigInteger`,
canonical form (gcd-reduced, positive denominator):

```fsharp
/// Arbitrary-precision rational. Always stored in canonical form:
/// `gcd(|Numerator|, Denominator) = 1` and `Denominator > 0`. Construct
/// via `Rational.create`; equality and hashing are structural.
[<Struct>]
type Rational =
    val Numerator: bigint
    val Denominator: bigint

    static member create (n: bigint) (d: bigint) : Rational =
        if d.IsZero then invalidArg "d" "Rational denominator must be nonzero"
        let sign = if d.Sign < 0 then bigint -1 else bigint 1
        let n' = n * sign
        let d' = d * sign
        let g = bigint.GreatestCommonDivisor(bigint.Abs n', d')
        Rational(Numerator = n' / g, Denominator = d' / g)

    static member Zero = Rational(Numerator = bigint 0, Denominator = bigint 1)
    static member One  = Rational(Numerator = bigint 1, Denominator = bigint 1)

    member this.IsZero = this.Numerator.IsZero
    static member (+) (a: Rational, b: Rational) : Rational = …
    static member (-) (a: Rational, b: Rational) : Rational = …
    static member (~-) (a: Rational) : Rational = Rational.create -a.Numerator a.Denominator
    static member (*) (a: Rational, b: Rational) : Rational = …
```

Replace the placeholder `MeasureTerm` with the exponent-vector struct.
Sealed class (not a plain `(string * Rational) list` alias) so callers
can't forget to normalise:

```fsharp
/// Abelian-group expression over named unit atoms. Always stored in a
/// normalised form: each exponent is in canonical Rational form, zero
/// exponents are dropped, and entries are sorted by unit name. Equality
/// is structural list equality after normalise. `Empty` is the group
/// identity (dimensionless).
[<Sealed>]
type MeasureTerm private (exponents: (string * Rational) list) =
    /// `exponents` must be pre-normalised. Use the static `ofList` for
    /// raw input.
    member _.Exponents = exponents
    member _.IsDimensionless = List.isEmpty exponents

    static member Empty = MeasureTerm([])

    static member ofList (raw: (string * Rational) list) : MeasureTerm =
        raw
        |> List.groupBy fst
        |> List.map (fun (n, xs) -> n, xs |> List.fold (fun acc (_, r) -> acc + r) Rational.Zero)
        |> List.filter (fun (_, e) -> not e.IsZero)
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

    /// `k` is `Rational` so `pow m (Rational.create 1 2)` (square root) works
    /// once we have a callsite that produces one. Surface syntax only ever
    /// passes integer `k` today.
    let pow (m: MeasureTerm) (k: Rational) : MeasureTerm =
        if k.IsZero then empty
        else m.Exponents |> List.map (fun (n, e) -> n, e * k) |> MeasureTerm.ofList
```

### Where the measure lives on the type

Measures attach to `TypeVar.Units`, matching
[`docs/typevar.md`](typevar.md)'s three-axis design — *not* to a new
`SemType` variant. Two reasons:

1. **SRTP / IWSAM bounds compose on `TypeVar`.** A future SRTP like
   `when ^T : (static member Sqrt : ^T -> ^T)` needs to inspect *all*
   constraints on the variable in one place; moving units to `SemType`
   splits the constraint set across two axes and forces the on-unified
   callback to look in two places to know what's required.
2. **Zonking stays simple.** TypeVar's `Link` resolves to a `TyConst`
   for the carrier (`int`, `float`); the measure rides alongside on the
   union-find root. Adding a new `SemType` variant means every consumer
   (`Freeze`, `zonk`, pattern-match exhaustiveness on `SemType`) needs
   a new arm — and `TyMeasured("float", [])` vs `TyConst "float"` becomes
   an awkward two-way equality.

Narrow the existing `Units : MeasureTerm list` field to `Units :
MeasureTerm voption` — a measure is a single abelian-group expression,
not a list of them. `ValueNone` means "not (yet) constrained as a
measured type"; `ValueSome MeasureTerm.empty` means "dimensionless
numeric"; `ValueSome [(m, 1)]` means measured. Authoritative on the
union-find root.

```fsharp
and [<Sealed>] TypeVar() =
    // … existing fields …
    /// Measure constraint on this variable, when known to be a numeric
    /// type. Authoritative on the union-find root — call UnionFind.find
    /// before reading. `union` merges measures via abelian-group equality;
    /// a mismatch on union is a diagnostic. Most TypeVars never get a
    /// measure (function types, tuples, non-numeric values) and stay
    /// `ValueNone`.
    member val Units: MeasureTerm voption = ValueNone with get, set
```

`SemType` itself is **unchanged** — no new variant. The measure rides on
the `TyVar` wrapping the carrier.

### `Unification.fs` — `inferConst` / `translateType`

`MeasuredLiteral` consumes the `Measure<SyntaxToken>` payload, mints a
TyVar at the current level, links it to the numeric carrier, and stamps
the measure on the TyVar:

```fsharp
| Constant.MeasuredLiteral(value = t; measure = m) ->
    let carrier = inferLiteralCarrier t        // TyConst "int" / "float" / …
    let mt = translateMeasure ctx m
    let tv = freshTyVar ctx
    tv.Link <- ValueSome carrier
    tv.Units <- ValueSome mt
    TyVar tv
```

`Constant.Literal` for a numeric token stays plain `TyConst "int"` /
`TyConst "float"` — no TyVar wrapper, no measure. The measure axis only
materialises when there's an explicit measure annotation. (Unification
then treats `TyConst "int"` against a measure-bearing TyVar as a
mismatch via the rules below — see §unify.)

`translateMeasure` mirrors `translateType`. A new private helper —
roughly 40 lines — walking the `Measure<'T>` CST:

```fsharp
let rec private translateMeasure (ctx: PassContext) (m: Measure<SyntaxToken>) : MeasureTerm =
    match m with
    | Measure.One _ -> MeasureTerm.empty
    | Measure.Named li when li.Idents.Length = 1 ->
        MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], Rational.One ]
    | Measure.Power(inner, _, neg, expTok) ->
        let n = parseIntToken ctx expTok                // bigint
        let signed = if neg.IsSome then -n else n
        MeasureTerm.pow (translateMeasure ctx inner) (Rational.create signed (bigint 1))
    | Measure.Product(l, _, r) ->
        MeasureTerm.mul (translateMeasure ctx l) (translateMeasure ctx r)
    | Measure.Quotient(l, _, r) ->
        MeasureTerm.div (translateMeasure ctx l) (translateMeasure ctx r)
    | Measure.Reciprocal(_, inner) ->
        MeasureTerm.inv (translateMeasure ctx inner)
    | Measure.Paren(_, inner, _) ->
        translateMeasure ctx inner
    | Measure.Juxtaposition(elems, _) ->
        elems
        |> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx m)) MeasureTerm.empty
    | Measure.Anonymous _
    | Measure.Typar _
    | Measure.Named _ ->
        // Multi-segment Named (qualified unit) and measure typars are v2.
        ctx.Diagnostics.Add {
            Key = key  // measure node key — see CstKeys
            Message = "Measure typars / qualified unit names not yet supported"
            Severity = Error
        }
        MeasureTerm.empty
```

For binding-level type annotations like `(x : float<m>)`, the CST shape
is `Type.AppliedType(carrier, [TypeArg.Measure m])`. `translateType`
recognises that shape and returns a `TyVar` carrying the measure:

```fsharp
| Type.AppliedType(typ = carrier; typeArgs = args)
        when isNumericCarrier carrier && args.Length = 1 ->
    match args.[0] with
    | TypeArg.Measure m ->
        let mt = translateMeasure ctx m
        let tv = freshTyVar ctx
        tv.Link <- ValueSome (translateType ctx carrier)
        tv.Units <- ValueSome mt
        TyVar tv
    | TypeArg.Type _ -> /* not measure-shaped — fall through */
```

`isNumericCarrier` recognises the built-in numeric type names (`int`,
`float`, `int64`, etc.). v2 generalises this when user-defined
`[<Measure>]` type declarations come online.

### `Unification.fs` — `unify` and the `Units` axis

The TyVar-TyVar arm already merges via `UnionFind.union`. After the
union, propagate `Units`: the surviving root takes the union of the two
roots' `Units`, with a diagnostic on disagreement. Wire this alongside
the existing `migrateBounds` call:

```fsharp
| TyVar tv1, TyVar tv2 ->
    let r1 = UnionFind.find tv1
    let r2 = UnionFind.find tv2
    let unitsA = r1.Units
    let unitsB = r2.Units
    UnionFind.union r1 r2
    let newRoot = UnionFind.find r1
    let merged = if obj.ReferenceEquals(newRoot, r1) then r2 else r1
    migrateBounds newRoot merged
    // Measure axis:
    match unitsA, unitsB with
    | ValueNone, ValueNone -> ()
    | ValueSome m, ValueNone
    | ValueNone, ValueSome m -> newRoot.Units <- ValueSome m
    | ValueSome m1, ValueSome m2 when m1 = m2 -> newRoot.Units <- ValueSome m1
    | ValueSome m1, ValueSome m2 ->
        newRoot.Units <- ValueSome m1   // pick one to keep zonking sane
        ctx.Diagnostics.Add {
            Key = key
            Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
            Severity = Error
        }
```

For the `TyVar tv, other` arm, when `other` is a `TyConst` carrier
(`TyConst "int"`, etc.), check that `tv`'s `Units` (if any) is acceptable:

```fsharp
| TyVar tv, (TyConst _ as other)
| (TyConst _ as other), TyVar tv ->
    let root = UnionFind.find tv
    if occursAndAdjust root other then
        … existing occurs diagnostic …
    else
        // Setting Link to a non-measured TyConst means: this var is now
        // known to be dimensionless. If we already inferred a measure
        // for it, the assignment contradicts the measure.
        match root.Units with
        | ValueSome m when not m.IsDimensionless ->
            ctx.Diagnostics.Add {
                Key = key
                Message = sprintf "Dimensionless %s used where <%O> expected" (formatTy other) m
                Severity = Error
            }
        | _ -> ()
        root.Link <- ValueSome other
```

`MeasureTerm` carries no TyVars; `occursAndAdjust` and the level walk
are unchanged.

### `Unification.fs` — operators

The v1 / v1.5 split lands here. Two parts:

**v1: `+`, `-` are measure-preserving.** `inferInfix` for `op_Addition` /
`op_Subtraction` / `op_Equality` etc. checks for a measure-bearing operand
before the provider lookup:

```fsharp
// Pseudocode — read measures off the unified roots after typing both
// sides. If either side carries Units, both must, and they must agree.
let private tryMeasuredArith
    (ctx: PassContext) (name: string)
    (leftTy: SemType) (rightTy: SemType) : SemType option =
    let leftUnits  = unitsOf (resolveStep leftTy)        // ValueOption<MeasureTerm>
    let rightUnits = unitsOf (resolveStep rightTy)
    match name, leftUnits, rightUnits with
    | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 when m1 = m2 ->
        // Result carries the same measure as the operands.
        Some (freshTyVarWith ctx (carrierOf leftTy) (ValueSome m1) |> TyVar)
    | ("op_Addition" | "op_Subtraction"), ValueNone, ValueNone -> None  // dimensionless: provider path
    | _ -> None
```

If the dispatcher returns `Some`, `inferInfix` uses that and skips the
provider lookup. If `None`, fall through to the existing path. The
existing monomorphic `op_Addition : int -> int -> int` in `MockBuiltins`
handles the all-dimensionless case via fall-through. Helper functions
`unitsOf` and `carrierOf` walk a resolved `SemType` to read the
measure/carrier off the union-find root.

**v1.5: `*`, `/` combine measures.** Same dispatcher with a `mul`/`div`
arm:

```fsharp
| "op_Multiply", ValueSome m1, ValueSome m2 ->
    Some (freshTyVarWith ctx (carrierOf leftTy) (ValueSome (MeasureTerm.mul m1 m2)) |> TyVar)
| "op_Division", ValueSome m1, ValueSome m2 ->
    Some (freshTyVarWith ctx (carrierOf leftTy) (ValueSome (MeasureTerm.div m1 m2)) |> TyVar)
| "op_Multiply", ValueSome m, ValueNone
| "op_Multiply", ValueNone, ValueSome m ->
    // dimensionless × measured: measured. (Real F# requires the dimensionless
    // side to be the literal `int`/`float` carrier.)
    Some (freshTyVarWith ctx (carrierOf leftTy) (ValueSome m) |> TyVar)
```

This is a special case — the operator is no longer a single typed scheme.
A cleaner v2 design replaces the dispatcher with measure-polymorphic
operator schemes that the provider returns, instantiated per use site;
that needs measure typars to land first.

### `Freeze.fs`

Trivial: measured types flow through as `TyVar` carrying `Units` on the
root, which `zonk` resolves to the carrier `TyConst` while the measure
stays on the root. No new `TConstValue` / `TPat` shape — measured
literals are `TConstValue.Float` (etc.) carrying the value, and the
surrounding `ty` field is a `TyVar` whose union-find root carries both
the `Link = TyConst "float"` and `Units = ValueSome [(m, 1)]`.

`TastShape.prettyDecl` in the tests gains a render arm for measure-bearing
TyVars — `"float<m>"`, `"int<kg/s>"`, etc. Pretty-printing reads
`UnionFind.find tv |> fun r -> r.Link, r.Units`.

## Pipeline integration

No new pass. Measures are computed inside `Unification` alongside types —
matches the spec ([§Phase 4.3](../../../semantic-analysis.md#phase-43-unification-types--units))
which already groups types and units into one phase.

Pre/postconditions unchanged. No `SemType` variants added — consumers
that exhaustively match on `SemType` are unaffected. The only ripple is
in places that *read* `TypeVar.Units`: pretty-printing and any future
codegen that lowers measures.

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

Add a small `Measured` helper in `TestHelpers.fs` that, given a CST-produced
type, asserts `(carrier, measure)` against a TyVar's root's `(Link, Units)`.
Equality on `MeasureTerm` is structural so `Expect.equal` works directly.
Tests will likely want a `Rational.ofInt` / `m` literal helper to make the
expected vectors readable: `[("m", Rational.ofInt 1)]` rather than the raw
`bigint`/`bigint` construction.

## TAST representation

The frozen TAST carries inferred types inline. Measured types ride as
`TyVar` carrying `Units` on their union-find root — no new TAST node,
no new `TConstValue` / `TPat` shape. A measured literal is
`TConstValue.Float 1.0` and its surrounding `ty` is a `TyVar` whose
root has `Link = TyConst "float"` and `Units = ValueSome [(m, 1)]`.
Downstream consumers (codegen, target lowering) that want "is this a
metre?" read the root's `Units` after `UnionFind.find`.

## Open questions

- **Measure typars (`'u`).** Deferred to v2. The unification algorithm
  for abelian groups with variables is well-known
  (Knaughton 1995, used by FCS) but considerably more involved than
  ground-term equality. Land v1 (ground measures) first, write tests
  that exercise non-trivial cancellation, then layer typars on top. Once
  they land, the `Units` axis becomes the natural place for measure
  variables too — they're just unsolved abelian-group terms, the same
  way an unsolved `Link` is the type axis.
- **`Rational` allocation cost.** Each `Rational` holds two `BigInteger`s.
  For tiny exponents (which is essentially all of them in practice) we
  pay a heap allocation per `BigInteger` past the inline-small path.
  Probably negligible — measures are rare relative to types — but worth
  measuring once we have a corpus. Easy escape hatch: a packed
  small-rational struct with a fallback to `BigInteger` past `int64`
  range, similar to BCL's `BigInteger` itself. Not worth doing
  speculatively.
- **Generic measured types from the provider.** `Microsoft.FSharp.Core.float<'u>`
  is the canonical example. The provider returns a measure-bearing
  `SemType` for an external measured value — fine, the helper that
  builds it sets `Units` on the TyVar. The carrier name still comes from
  the provider's symbol table, so this needs a "I know about `float<…>`"
  hook. For v1 the built-in numeric primitives suffice; cross this when
  records / DUs push us toward a real named-type registry.
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
- **Rational exponent literals at the surface.** The parser's
  `Measure.Power` carries an integer-literal token (`int: 'T`) — there's
  no `<m^(1/2)>` syntax. The `Rational` representation is wider than the
  surface so library functions like `sqrt` can produce rational exponents
  later without a representation change. Surface-syntax rational
  exponents (if F# ever adds them) would extend `Measure.Power`'s grammar,
  not the semantic representation.
- **Integration with SRTPs.** "`^T` is numeric and carries a measure" is
  an SRTP-shaped bound that needs the on-unified callback machinery from
  [`docs/typevar.md`](typevar.md). Lands with SRTPs — and is one of the
  reasons measures live on `TypeVar`, so SRTP bounds and the `Units`
  axis can be inspected in one place.
