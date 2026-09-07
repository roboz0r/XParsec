namespace XParsec.FSharp.SemanticAnalysis

open Vesper
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
    let classify (cases: Block<TEnumCaseG<'tok>>) : TEnumVariant voption =
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

    let integralValue (v: TConstValue) : IntKind * int64 =
        match v with
        | TConstValue.Integral(k, bits) when IntKind.isEnumBase k -> k, bits
        | other -> failwithf "TEnumCases.integralValue: non-integral enum literal %A" other

    let integralKind (v: TConstValue) : IntKind = fst (integralValue v)

    /// Unsuffixed `int` (`IntKind.Int32`) is NOT explicit: it adopts whatever explicit
    /// kind the enum has, else stays `int`.
    let private isExplicitKind (v: TConstValue) : bool =
        match v with
        | TConstValue.Integral(IntKind.Int32, _) -> false
        | _ -> true

    /// The underlying primitive TYPE of a numeric enum: the first explicit kind if any,
    /// else `int`.
    let numericUnderlyingTypeKey (cases: Block<TEnumCaseG<'tok>>) : TypeKey =
        let mutable explicit = ValueNone

        for c in cases do
            match c.Value with
            | ValueSome(TEnumLiteral.Int v) when isExplicitKind v ->
                if explicit.IsNone then
                    explicit <- ValueSome(RuntimeNames.intKindKey (integralKind v))
            | _ -> ()

        match explicit with
        | ValueSome key -> key
        | ValueNone -> RuntimeNames.intKey

    /// The underlying primitive TYPE: all-numeric → the first explicit kind if any,
    /// else `int`; all-string → `string`; mixed → `obj`; no resolved case → `ValueNone`.
    let underlyingTypeKey (cases: Block<TEnumCaseG<'tok>>) : TypeKey voption =
        match classify cases with
        | ValueNone -> ValueNone
        | ValueSome TEnumVariant.String -> ValueSome RuntimeNames.stringKey
        | ValueSome TEnumVariant.Mixed -> ValueSome RuntimeNames.objKey
        | ValueSome TEnumVariant.Numeric -> ValueSome(numericUnderlyingTypeKey cases)

    /// How a kind is SPELLED to the user, off the same identity the enum is typed by, so
    /// the message cannot cite a type the enum was not given.
    let private kindDisplayName (v: TConstValue) : string =
        let (DisplayName n) =
            SymbolKeyOps.typeSimpleName (RuntimeNames.intKindKey (integralKind v))

        n

    /// The first case whose explicit kind disagrees with an earlier one's.
    type KindConflict<'tok> =
        {
            Tok: 'tok
            /// The kind the earlier explicitly-suffixed case established.
            Established: string
            /// The kind the offending case carries instead.
            Offending: string
        }

    /// A `System.Enum` has exactly ONE underlying type, so `| A = 1uy | B = 2L` is illegal.
    let firstKindConflict (cases: Block<TEnumCaseG<'tok>>) : KindConflict<'tok> voption =
        let mutable seen = ValueNone
        let mutable result = ValueNone

        for c in cases do
            match c.Value with
            | ValueSome(TEnumLiteral.Int v) when isExplicitKind v && result.IsNone ->
                let w = kindDisplayName v

                match seen with
                | ValueNone -> seen <- ValueSome w
                | ValueSome w0 ->
                    if w0 <> w then
                        result <-
                            ValueSome
                                {
                                    Tok = c.Tok
                                    Established = w0
                                    Offending = w
                                }
            | _ -> ()

        result
