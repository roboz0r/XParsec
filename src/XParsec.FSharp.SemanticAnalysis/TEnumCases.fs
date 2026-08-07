namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

[<RequireQualifiedAccess>]
type TEnumVariant =
    | Numeric
    | String
    | Mixed

/// Pure derivations over a frozen enum's case→literal table: the variant and the
/// underlying integral width, computed on demand rather than baked onto the enum node.
module TEnumCases =
    /// `ValueNone` when no case resolved to a legal literal (every case errored).
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

    let integralValue (v: TConstValue) : IntWidth * int64 =
        match v with
        | TConstValue.Integral(w, bits) when IntWidth.isEnumBase w -> w, bits
        | other -> failwithf "TEnumCases.integralValue: non-integral enum literal %A" other

    let integralWidth (v: TConstValue) : IntWidth = fst (integralValue v)

    /// Unsuffixed `int` (`IntWidth.Int32`) is NOT explicit: it adopts whatever explicit
    /// width the enum has, else stays `int`.
    let private isExplicitWidth (v: TConstValue) : bool =
        match v with
        | TConstValue.Integral(IntWidth.Int32, _) -> false
        | _ -> true

    /// The underlying primitive TYPE: all-numeric → the first explicit width if any,
    /// else `int`; all-string → `string`; mixed → `obj`; no resolved case → `ValueNone`.
    let underlyingTypeKey (cases: EqArray<TEnumCaseG<'tok>>) : SymbolKey voption =
        match classify cases with
        | ValueNone -> ValueNone
        | ValueSome TEnumVariant.String -> ValueSome RuntimeNames.stringKey
        | ValueSome TEnumVariant.Mixed -> ValueSome RuntimeNames.objKey
        | ValueSome TEnumVariant.Numeric ->
            let mutable explicit = ValueNone

            for c in cases do
                match c.Value with
                | ValueSome(TEnumLiteral.Int v) when isExplicitWidth v ->
                    if explicit.IsNone then
                        explicit <- ValueSome(RuntimeNames.intWidthKey (integralWidth v))
                | _ -> ()

            match explicit with
            | ValueSome w -> ValueSome w
            | ValueNone -> ValueSome RuntimeNames.intKey

    /// How a width is SPELLED to the user, off the same identity the enum is typed by, so
    /// the message cannot name a type the enum was not given.
    let private widthDisplayName (v: TConstValue) : string =
        let (DisplayName n) =
            SymbolKeyOps.simpleName (RuntimeNames.intWidthKey (integralWidth v))

        n

    /// A `System.Enum` has exactly ONE underlying type, so `| A = 1uy | B = 2L` is illegal.
    /// Returns the first offending case's token with the two width names (first-seen, then
    /// the mismatch). Unsuffixed `int` cases never conflict.
    let firstWidthConflict (cases: EqArray<TEnumCaseG<'tok>>) : ('tok * string * string) voption =
        let mutable seen = ValueNone
        let mutable result = ValueNone

        for c in cases do
            match c.Value with
            | ValueSome(TEnumLiteral.Int v) when isExplicitWidth v && result.IsNone ->
                let w = widthDisplayName v

                match seen with
                | ValueNone -> seen <- ValueSome w
                | ValueSome w0 ->
                    if w0 <> w then
                        result <- ValueSome(c.Tok, w0, w)
            | _ -> ()

        result
