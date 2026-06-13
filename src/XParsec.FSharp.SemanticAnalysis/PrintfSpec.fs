namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// Printf format-string analysis — see docs/front-end-gaps-plan.md §B.
//
// The format grammar itself is NOT re-implemented here: the lexer already
// classifies each `%[flags][width][.precision][type]` placeholder into a
// `FormatType` (see `Lexing.parseFormatSpecifier`). This module supplies only
// the *semantic* layer on top of that.
//
// Named `PrintfSpec` (not `Printf`) so it never collides with the bare
// `Printf` module auto-opened from `Microsoft.FSharp.Core`.

module PrintfSpec =

    /// The format literal at a printf call site freezes to a `New` of this type
    /// (a single `value: string` constructor).
    [<Literal>]
    let printfFormatName = "Microsoft.FSharp.Core.PrintfFormat"

    /// The assembly that hosts the canonical printf family
    /// (`Vesper.Printf.printfn` and friends). Codegen identity check. A `TExpr.External` carrying a
    /// `ValueKey` with this assembly is the canonical printf; anything else
    /// (project-local shadow `MyMod.printfn`, alternate-library printf, an
    /// unkeyed bare `printfn` from a test mock) routes through the standard
    /// external-call path.
    [<Literal>]
    let canonicalPrintfAssembly = "Vesper.Printf"

    /// `ValueSome short-name` (`"printf"`, `"printfn"`, …) when `key` is a
    /// canonical Vesper.Printf entry-point key, `ValueNone` otherwise.
    /// Codegen uses it to decide whether the cold-printf recipe applies — a
    /// `MyMod.printfn 1` resolves to `ValueKey(None, "MyMod", "printfn")`,
    /// which doesn't match here and falls through to the normal external-call
    /// path.
    let canonicalPrintfShortName (key: SymbolKey) : string voption =
        match key with
        | SymbolKey.ValueKey(Some asm, _, name) when asm = canonicalPrintfAssembly -> ValueSome name
        | _ -> ValueNone

    /// True iff `key` is the canonical `Vesper.Printf.printfn` (the only
    /// printf family member the *cold* printf recipe targets today —
    /// `printf`/`sprintf`/`eprintf*` either don't exist as cold paths or
    /// route through the Vesper.Formatter inline lowering already).
    let isCanonicalPrintfn (key: SymbolKey) : bool =
        match canonicalPrintfShortName key with
        | ValueSome "printfn" -> true
        | _ -> false

    let private tyUnit: SemType = TyConst("unit", EqArray.empty)
    let private tyString: SemType = TyConst("string", EqArray.empty)
    let private tyTextWriter: SemType = TyConst("System.IO.TextWriter", EqArray.empty)

    /// Target-agnostic classification of a printf entry point's output sink,
    /// resolved from the entry-point name. Recorded on `PassContext.PrintfApp`
    /// for the calls P1 lowers inline; see docs/vesper-printf-plan.md.
    [<RequireQualifiedAccess>]
    type PrintfSink =
        | StdOut of newline: bool
        | StdErr of newline: bool
        | StringResult

    /// P1 happy-path families only — all with the format at arg 0. `fprintf` /
    /// `bprintf` and every other name return `ValueNone`, keeping the existing
    /// FSharp.Core path. Keyed on the last `.`-segment so `Printf.printfn` and
    /// bare `printfn` both hit (mirrors `tryFamily`).
    let sinkOf (name: string) : PrintfSink voption =
        let short =
            let dot = name.LastIndexOf '.'
            if dot < 0 then name else name.Substring(dot + 1)

        match short with
        | "printf" -> ValueSome(PrintfSink.StdOut false)
        | "printfn" -> ValueSome(PrintfSink.StdOut true)
        | "eprintf" -> ValueSome(PrintfSink.StdErr false)
        | "eprintfn" -> ValueSome(PrintfSink.StdErr true)
        | "sprintf" -> ValueSome PrintfSink.StringResult
        | _ -> ValueNone

    /// `Ty` alone can't disambiguate (`%o` and `%u` are both `int`-typed), so
    /// `tryHoleFormat` tags each hole with a kind. `Formatted` is the default
    /// (everything that maps onto `AppendFormatted<T>` under a .NET format
    /// string); the others need a dedicated handler member because they have no
    /// such mapping.
    ///
    /// Lives here (not in `Tast.fs`) because `PrintfSpec.fs` compiles before
    /// `Tast.fs` and `Tast.HoleSpec.Kind` references it.
    [<RequireQualifiedAccess>]
    type HoleKind =
        /// `AppendFormatted<Ty>(v [,alignment] [,format])`.
        | Formatted
        /// `AppendBool(v, alignment)` — writes `true`/`false` (lowercase;
        /// `bool.ToString` capitalises, so this can't go through `Formatted`).
        | BoolText
        /// `AppendUnsigned(v, alignment)` — the `int` argument's bits
        /// reinterpreted as `uint` (F# `%u`).
        | Unsigned
        /// `AppendOctal(v, alignment)` — `Convert.ToString(v, 8)` (.NET has no
        /// octal format string); two's-complement for negatives, matching F#.
        | Octal
        /// `AppendZeroPaddedFloat(v, format, width)` — F# `%0w.pf`: format the
        /// float via `format` (an `"F<prec>"` string), then zero-pad *after any
        /// sign* to a total field of `width` chars. Dedicated because no .NET
        /// float format zero-pads to a total width. The width rides in
        /// `HoleSpec.Alignment` and the `"F<prec>"` body in `HoleSpec.Format`.
        | ZeroPaddedFloat

    /// Map a parsed format placeholder to the `(HoleKind, .NET format string,
    /// alignment)` triple the happy-path handler call uses, or `ValueNone` for
    /// specifiers lowered via the FSharp.Core cold path instead. Every lowered
    /// specifier's `Formatter` output matches F# `printf` byte-for-byte under
    /// `InvariantCulture` — parity is the gate.
    ///
    /// Lowered (P2): `%s` `%d`/`%i` (`Formatted`, no format); `%f` (`"F<prec>"`,
    /// default 6); `%e`/`%E` (`"e<prec>"`/`"E<prec>"`); `%x`/`%X` (`"x"`/`"X"`),
    /// `%B` (.NET 8 `"B"`) two's-complement; `%O` (`ToString` via `Formatted`);
    /// `%u` (`Unsigned`), `%o` (`Octal`), `%b` (`BoolText`) — the last three via
    /// dedicated handler members. Width with no flag ⇒ alignment; `-` ⇒ negative
    /// alignment (left-justify); `0` ⇒ a width-bearing `"D5"`/`"x8"`/`"B8"` for
    /// the integer bases (mutually exclusive with alignment).
    ///
    /// Lowered (A1): `%c` (`Formatted`, no format — `char.ToString()` is the
    /// one-char string) and `%M` (`Formatted`, no format — `decimal` is
    /// `ISpanFormattable`, `TryFormat` under Invariant matches F# `%M`), now that
    /// char / decimal literals round-trip through the TAST const subset.
    ///
    /// Lowered (B1): the `+` / space forced-sign flags on the signed
    /// decimal-integer (`%+d`/`% d`) and fixed-point-float (`%+.2f`/`% .2f`)
    /// specifiers, via a custom .NET *section* format string (`"+0;-0"` /
    /// `" 0;-0"`) — still a `Formatted` hole, optionally with a width-as-alignment.
    ///
    /// Lowered (B2): `0`-on-float (`%08.2f`) via the `ZeroPaddedFloat` handler
    /// member — .NET has no float format that zero-pads to a total width, so it
    /// formats the `"F<prec>"` body then inserts `0`s after any sign to reach the
    /// field width.
    ///
    /// Cold path: `%g`/`%G` (.NET `"G"` uppercases the exponent, F# wants
    /// lowercase `e`); `%A` (structural — P3); `%a`/`%t` (callbacks); `0`-on-`%e`
    /// (exponent zero-pad parity is subtle — only `%f` is lowered); `+`/space on
    /// anything but `%d`/`%f` (exponent / scale-preserving forms don't
    /// section-format faithfully) and `+`/space combined with `0` (zero-pad);
    /// zero-pad on the handler-member specifiers (`%05u`/`%05o`/`%05b`) and on
    /// `%s`/`%O` (meaningless); `%M` with a precision (`%.2M` — unusual F#
    /// semantics).
    let tryHoleFormat (p: FormatPlaceholder) : (HoleKind * string option * int option) voption =
        let flags = p.Flags
        let has (c: char) = flags.IndexOf c >= 0
        let zeroPad = has '0'
        let leftAlign = has '-'
        let plusSign = has '+'
        let spaceSign = has ' '

        let width =
            match p.Width with
            | ValueSome w -> Some(int w)
            | ValueNone -> None

        if plusSign || spaceSign then
            // B1: forced-sign flags via a custom .NET *section* format string
            // (`"+0;-0"` / `" 0;-0"`). The positive section carries the forced
            // `+`/space; the negative section keeps `-`; zero takes the positive
            // section (matching F# `%+d 0 = "+0"`, `% d 0 = " 0"`). Only the
            // signed decimal-integer and fixed-point-float specifiers render this
            // way byte-for-byte — the exponent (`%e`, whose custom-format exponent
            // width diverges from `"e6"`) and scale-preserving (`%M`) forms, every
            // non-numeric type, and the sign+zero-pad combination stay cold. A
            // width (with or without `-`) rides as a handler alignment, padding
            // applied after the section format.
            if zeroPad then
                ValueNone
            else
                let signSection = if plusSign then "+" else " "

                let alignment =
                    match width with
                    | Some w -> Some(if leftAlign then -w else w)
                    | None -> None

                let body =
                    match p.Type with
                    | FormatType.DecimalInt -> Some "0"
                    | FormatType.FloatDecimal ->
                        let precision =
                            match p.Precision with
                            | ValueSome pr -> int pr
                            | ValueNone -> 6

                        Some(
                            if precision <= 0 then
                                "0"
                            else
                                "0." + System.String('0', precision)
                        )
                    | _ -> None

                match body with
                | Some b -> ValueSome(HoleKind.Formatted, Some(signSection + b + ";-" + b), alignment)
                | None -> ValueNone
        elif (leftAlign || zeroPad) && width.IsNone then
            // `-` / `0` are meaningless without a width.
            ValueNone
        elif leftAlign && zeroPad then
            // Their interaction (left-align wins, zero-pad ignored) is easy to
            // get subtly wrong — defer rather than risk a parity miss.
            ValueNone
        else
            // No flag (or just `-`): width becomes a handler alignment (negative
            // ⇒ left-justify). Zero-pad uses a width-bearing .NET format string
            // instead — the two are mutually exclusive on one hole.
            let alignment =
                if zeroPad then
                    None
                else
                    match width with
                    | Some w -> Some(if leftAlign then -w else w)
                    | None -> None

            // Width-bearing .NET integer format for a zero-pad request. Only
            // reached when `zeroPad`, so `width` is guaranteed present.
            let zeroPadFormat (letter: string) = letter + string width.Value

            let formatted fmt =
                ValueSome(HoleKind.Formatted, fmt, alignment)

            match p.Type with
            | FormatType.String
            | FormatType.Object -> if zeroPad then ValueNone else formatted None
            | FormatType.DecimalInt -> formatted (if zeroPad then Some(zeroPadFormat "D") else None)
            | FormatType.UnsignedHex ->
                let letter = if p.TypeChar = 'X' then "X" else "x"
                formatted (Some(if zeroPad then zeroPadFormat letter else letter))
            | FormatType.UnsignedBinary -> formatted (Some(if zeroPad then zeroPadFormat "B" else "B"))
            | FormatType.FloatDecimal ->
                let precision =
                    match p.Precision with
                    | ValueSome pr -> int pr
                    | ValueNone -> 6

                let fformat = "F" + string precision

                if zeroPad then
                    // B2: no .NET float format zero-pads to a total width, so a
                    // dedicated handler member formats the `"F<prec>"` body, then
                    // inserts `0`s after any sign to reach the field width. `width`
                    // is guaranteed present here (the `zeroPad && width.IsNone`
                    // guard above). It rides in `Alignment`; the body in `Format`.
                    ValueSome(HoleKind.ZeroPaddedFloat, Some fformat, Some width.Value)
                else
                    formatted (Some fformat)
            | FormatType.FloatExponential ->
                if zeroPad then
                    ValueNone
                else
                    let precision =
                        match p.Precision with
                        | ValueSome pr -> int pr
                        | ValueNone -> 6

                    let letter = if p.TypeChar = 'E' then "E" else "e"
                    formatted (Some(letter + string precision))
            // Handler members: no .NET format string (alignment only) ⇒ zero-pad defers.
            | FormatType.UnsignedDecimalInt ->
                if zeroPad then
                    ValueNone
                else
                    ValueSome(HoleKind.Unsigned, None, alignment)
            | FormatType.UnsignedOctal ->
                if zeroPad then
                    ValueNone
                else
                    ValueSome(HoleKind.Octal, None, alignment)
            | FormatType.Bool ->
                if zeroPad then
                    ValueNone
                else
                    ValueSome(HoleKind.BoolText, None, alignment)
            | FormatType.Char ->
                // `char` is not `IFormattable`, so `AppendFormatted<char>` falls
                // to `ToString()` → the one-char string. Alignment via the
                // `(T, alignment)` overload; zero-pad on `%c` is meaningless.
                if zeroPad then ValueNone else formatted None
            | FormatType.Decimal ->
                // `decimal` is `ISpanFormattable`; `TryFormat` under Invariant
                // matches F# `%M`. F# `%M` precision semantics are unusual and
                // zero-pad has no faithful float-style mapping → defer both.
                if zeroPad || p.Precision.IsSome then
                    ValueNone
                else
                    formatted None
            | FormatType.FloatCompact
            | FormatType.Structured
            | FormatType.FormatFunction
            | FormatType.Text -> ValueNone

    /// SemType of the argument a specifier consumes, or `ValueNone` for the
    /// specifiers v1 doesn't type. `%A`/`%O` both consume a polymorphic argument
    /// (a fresh type variable); the `%A`-vs-`%O` distinction, irrelevant to
    /// typing, stays in the `FormatType` for codegen. `%a`/`%t` aren't modelled
    /// yet, so they return `ValueNone` and the caller falls through to standard
    /// inference.
    let argType (fresh: unit -> SemType) (t: FormatType) : SemType voption =
        match t with
        | FormatType.Bool -> ValueSome(TyConst("bool", EqArray.empty))
        | FormatType.String -> ValueSome tyString
        | FormatType.Char -> ValueSome(TyConst("char", EqArray.empty))
        | FormatType.DecimalInt
        | FormatType.UnsignedDecimalInt
        | FormatType.UnsignedHex
        | FormatType.UnsignedOctal
        | FormatType.UnsignedBinary -> ValueSome(TyConst("int", EqArray.empty))
        | FormatType.FloatExponential
        | FormatType.FloatDecimal
        | FormatType.FloatCompact -> ValueSome(TyConst("float", EqArray.empty))
        | FormatType.Decimal -> ValueSome(TyConst("decimal", EqArray.empty))
        | FormatType.Object
        | FormatType.Structured -> ValueSome(fresh ())
        | FormatType.FormatFunction
        | FormatType.Text -> ValueNone

    /// `FormatArgIndex` is the positional slot of the format string (0 for
    /// `printf`/`sprintf`/…; 1 for `fprintf`, after the `TextWriter`). `Tail` is
    /// the final result of the curried printer (`unit` for the writing families,
    /// `string` for `sprintf`). `LeadingArgTypes` is the expected types of the
    /// arguments before the format (length = `FormatArgIndex`).
    type Family =
        {
            FormatArgIndex: int
            Tail: SemType
            State: SemType
            Residue: SemType
            Result: SemType
            LeadingArgTypes: SemType list
        }

    let private writerFamily (formatArgIndex: int) (leading: SemType list) : Family =
        {
            FormatArgIndex = formatArgIndex
            Tail = tyUnit
            State = tyTextWriter
            Residue = tyUnit
            Result = tyUnit
            LeadingArgTypes = leading
        }

    let private stringFamily: Family =
        {
            FormatArgIndex = 0
            Tail = tyString
            State = tyUnit
            Residue = tyString
            Result = tyString
            LeadingArgTypes = []
        }

    /// Keyed by source short name. `bprintf` / the `k*`-continuation family are
    /// out of scope — they carry extra leading arguments and aren't needed by
    /// the canonical sample.
    let families: Map<string, Family> =
        [
            "printf", writerFamily 0 []
            "printfn", writerFamily 0 []
            "eprintf", writerFamily 0 []
            "eprintfn", writerFamily 0 []
            "fprintf", writerFamily 1 [ tyTextWriter ]
            "fprintfn", writerFamily 1 [ tyTextWriter ]
            "sprintf", stringFamily
        ]
        |> Map.ofList

    /// Keyed on the last `.`-separated segment so `Printf.printfn` and a bare
    /// `printfn` both hit.
    let tryFamily (name: string) : Family voption =
        let short =
            let dot = name.LastIndexOf '.'
            if dot < 0 then name else name.Substring(dot + 1)

        match Map.tryFind short families with
        | Some f -> ValueSome f
        | None -> ValueNone

    let formatType (printer: SemType) (fam: Family) : SemType =
        TyClass(RuntimeNames.printfFormatKey, EqArray.ofList [ printer; fam.State; fam.Residue; fam.Result ])

    /// Curry resolved argument types onto the family's tail.
    let printerType (argTypes: SemType list) (fam: Family) : SemType =
        List.foldBack (fun a r -> TyFun(a, r)) argTypes fam.Tail

    /// Shape: `leading… -> PrintfFormat<printer,…> -> printer`, plus the format
    /// type and printer type computed along the way. `ValueNone` when any
    /// specifier isn't typeable in v1 (`%a` / `%t`) — the caller then defers to
    /// standard inference.
    let appliedTypeOf
        (fresh: unit -> SemType)
        (specs: FormatType list)
        (fam: Family)
        : (SemType * SemType * SemType) voption =
        let rec mapAll acc specs =
            match specs with
            | [] -> ValueSome(List.rev acc)
            | s :: rest ->
                match argType fresh s with
                | ValueSome t -> mapAll (t :: acc) rest
                | ValueNone -> ValueNone

        match mapAll [] specs with
        | ValueNone -> ValueNone
        | ValueSome argTypes ->
            let printer = printerType argTypes fam
            let fmt = formatType printer fam

            let fnTy =
                List.foldBack (fun a r -> TyFun(a, r)) (fam.LeadingArgTypes @ [ fmt ]) printer

            ValueSome(fnTy, fmt, printer)

    /// The entry point's *generic* signature `leading… -> PrintfFormat<'T,…>
    /// -> 'T`. Used by the test `MockBuiltins` so the non-literal fallback
    /// (and plain name resolution) sees a coherent type.
    let genericSignature (fresh: unit -> SemType) (fam: Family) : SemType =
        let printer = fresh ()
        let fmt = formatType printer fam
        List.foldBack (fun a r -> TyFun(a, r)) (fam.LeadingArgTypes @ [ fmt ]) printer
