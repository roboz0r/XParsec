namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

/// The numeric / string / mixed classification of an enum, DERIVED from its
/// resolved case literals — never stored on the `Enum` node. `TEnumCases.classify`
/// is the single source of truth, recomputed wherever a consumer needs it.
[<RequireQualifiedAccess>]
type TEnumVariant =
    | Numeric
    | String
    | Mixed

/// Pure derivations over a frozen enum's case→literal table. The numeric / string
/// / mixed variant and the underlying integral width are computed on demand here
/// (the "classify, don't bake" rule), so the `TTypeKind.Enum` node stays the single
/// source of truth and the elaborator / freeze / both backends can't drift.
module TEnumCases =
    /// Derive the enum's variant from its resolved case literals. `ValueNone` when
    /// no case resolved to a legal literal (every case errored) — lets a caller
    /// distinguish "empty / all-illegal" from a real classification.
    let classify (cases: EqArray<TEnumCaseG<'tok>>) : TEnumVariant voption =
        let mutable anyInt = false
        let mutable anyStr = false

        for c in cases do
            match c.Value with
            | ValueSome(TEnumLiteral.Int _) -> anyInt <- true
            | ValueSome(TEnumLiteral.String _) -> anyStr <- true
            | ValueNone -> ()

        match anyInt, anyStr with
        | true, true -> ValueSome TEnumVariant.Mixed
        | true, false -> ValueSome TEnumVariant.Numeric
        | false, true -> ValueSome TEnumVariant.String
        | false, false -> ValueNone

    /// The width and value of an integral enum-case literal. `TEnumLiteral.Int` carries
    /// nothing else — the elaborator rejects every non-integral constant, and every
    /// pointer-width one (no `System.Enum` is based on those) — so the residual arm is a
    /// producer bug, and this is the single place that says so.
    ///
    /// Public because a backend needs the same answer to load the constant and type it
    /// (`Codegen.Clr`'s `EmitResolve.enumIntLoad`). The type's name is `IntWidth.name`,
    /// stated once, so the elaborator's choice of underlying type and the emitter's cannot
    /// disagree.
    let integralValue (v: TConstValue) : IntWidth * int64 =
        match v with
        | TConstValue.Integral(w, bits) when IntWidth.isEnumBase w -> w, bits
        | other -> failwithf "TEnumCases.integralValue: non-integral enum literal %A" other

    /// Just the width — what the enum's underlying type is derived from.
    let integralWidth (v: TConstValue) : IntWidth = fst (integralValue v)

    /// `true` for an authored *explicit* integral width, vs the unsuffixed `int` default —
    /// which is width-flexible (adopts the single explicit width present, else stays `int`)
    /// and so never drives a width conflict.
    let private isExplicitWidth (v: TConstValue) : bool =
        match v with
        | TConstValue.Integral(IntWidth.Int32, _) -> false
        | _ -> true

    /// Derive the enum's underlying primitive type NAME (DERIVED, not stored, like
    /// `classify`): all-numeric → the single authored explicit width if any, else
    /// `int`; all-string → `string`; mixed → `obj`; no resolved case → `ValueNone`.
    /// Total — it takes the first explicit width and does NOT enforce the uniform-width
    /// invariant; `Elaborate` reports a genuine conflict via `firstWidthConflict`.
    let underlyingTypeName (cases: EqArray<TEnumCaseG<'tok>>) : string voption =
        match classify cases with
        | ValueNone -> ValueNone
        | ValueSome TEnumVariant.String -> ValueSome "string"
        | ValueSome TEnumVariant.Mixed -> ValueSome RuntimeNames.objAbbrevName
        | ValueSome TEnumVariant.Numeric ->
            let mutable explicit = ValueNone

            for c in cases do
                match c.Value with
                | ValueSome(TEnumLiteral.Int v) when isExplicitWidth v ->
                    if explicit.IsNone then
                        explicit <- ValueSome(IntWidth.name (integralWidth v))
                | _ -> ()

            match explicit with
            | ValueSome w -> ValueSome w
            | ValueNone -> ValueSome "int"

    /// The CLR uniform-width invariant: a `System.Enum` has exactly ONE underlying
    /// type, so two cases with DIFFERENT *explicit* integral widths (`| A = 1uy | B =
    /// 2L`) are illegal. Returns the first offending case's token with the conflicting
    /// width names (first-seen, then the mismatch), else `ValueNone`. Unsuffixed `Int`
    /// cases never conflict; string / mixed enums carry no integral width.
    let firstWidthConflict (cases: EqArray<TEnumCaseG<'tok>>) : ('tok * string * string) voption =
        let mutable seen = ValueNone
        let mutable result = ValueNone

        for c in cases do
            match c.Value with
            | ValueSome(TEnumLiteral.Int v) when isExplicitWidth v && result.IsNone ->
                let w = IntWidth.name (integralWidth v)

                match seen with
                | ValueNone -> seen <- ValueSome w
                | ValueSome w0 ->
                    if w0 <> w then
                        result <- ValueSome(c.Tok, w0, w)
            | _ -> ()

        result
