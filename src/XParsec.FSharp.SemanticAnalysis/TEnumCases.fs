namespace XParsec.FSharp.SemanticAnalysis

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

    /// The canonical primitive NAME an integral enum-case literal contributes to the
    /// enum's underlying type — a member of `RuntimeNames.numericTypeNames`, the same
    /// `TConstValue`→type-name projection as `FreezeExpr.constType`. The four integral
    /// `TConstValue` cases are the only ones a `TEnumLiteral.Int` carries (the
    /// elaborator rejects every other constant), so the residual arm is a producer bug.
    let private integralWidthName (v: TConstValue) : string =
        match v with
        | TConstValue.Int _ -> "int"
        | TConstValue.UInt _ -> "uint32"
        | TConstValue.Int64 _ -> "int64"
        | TConstValue.Byte _ -> "byte"
        | other -> failwithf "TEnumCases.integralWidthName: non-integral enum literal %A" other

    /// `true` for an authored *explicit* integral width (`UInt`/`Int64`/`Byte`), vs
    /// the unsuffixed `Int` default — which is width-flexible (adopts the single
    /// explicit width present, else stays `int`) and so never drives a width conflict.
    let private isExplicitWidth (v: TConstValue) : bool =
        match v with
        | TConstValue.Int _ -> false
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
                        explicit <- ValueSome(integralWidthName v)
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
                let w = integralWidthName v

                match seen with
                | ValueNone -> seen <- ValueSome w
                | ValueSome w0 ->
                    if w0 <> w then
                        result <- ValueSome(c.Tok, w0, w)
            | _ -> ()

        result
