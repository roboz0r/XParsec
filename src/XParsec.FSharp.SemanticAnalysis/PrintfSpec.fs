namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// Printf format-string analysis — see docs/front-end-gaps-plan.md §B.
//
// The format grammar itself is NOT re-implemented here: the lexer already
// classifies each `%[flags][width][.precision][type]` placeholder into a
// `FormatType` (see `Lexing.parseFormatSpecifier`). This module supplies only
// the *semantic* layer on top of that:
//   1. `argType` — the `FormatType -> SemType` typing map (which value type a
//      specifier consumes), collapsing the cases that type identically (all
//      integer bases -> int; `%A`/`%O` -> a fresh type variable) while leaving
//      the `FormatType` itself to carry the finer distinction for codegen.
//   2. A small table describing the printf entry points (`printf`, `printfn`,
//      `sprintf`, `fprintf`, `eprintf`, …) — which positional argument carries
//      the format string, and the `PrintfFormat<_,_,_,_>` type-arg shape each
//      family imposes.
//
// Named `PrintfSpec` (not `Printf`) so it never collides with the bare
// `Printf` module auto-opened from `Microsoft.FSharp.Core`.

module PrintfSpec =

    /// Canonical compiled name for the `PrintfFormat<_,_,_,_>` type. The
    /// format literal at a printf call site freezes to a `New` of this type
    /// (a single `value: string` constructor), and the entry points' format
    /// parameter is `PrintfFormat<'Printer, 'State, 'Residue, 'Result>`.
    [<Literal>]
    let printfFormatName = "Microsoft.FSharp.Core.PrintfFormat"

    let private tyUnit: SemType = TyConst "unit"
    let private tyString: SemType = TyConst "string"
    let private tyTextWriter: SemType = TyConst "System.IO.TextWriter"

    /// SemType of the argument a specifier consumes, or `ValueNone` for the
    /// specifiers v1 doesn't type. `%A` (`Structured`) and `%O` (`Object`)
    /// both consume a polymorphic argument — a fresh type variable from
    /// `fresh` — and the `%A`-vs-`%O` distinction, irrelevant to typing, stays
    /// in the `FormatType` for codegen. `%a` (callback) and `%t` (thunk)
    /// involve the `State`/`Residue` typars and aren't modelled yet, so they
    /// return `ValueNone`; the caller then falls through to standard inference.
    let argType (fresh: unit -> SemType) (t: FormatType) : SemType voption =
        match t with
        | FormatType.Bool -> ValueSome(TyConst "bool")
        | FormatType.String -> ValueSome tyString
        | FormatType.Char -> ValueSome(TyConst "char")
        | FormatType.DecimalInt
        | FormatType.UnsignedDecimalInt
        | FormatType.UnsignedHex
        | FormatType.UnsignedOctal
        | FormatType.UnsignedBinary -> ValueSome(TyConst "int")
        | FormatType.FloatExponential
        | FormatType.FloatDecimal
        | FormatType.FloatCompact -> ValueSome(TyConst "float")
        | FormatType.Decimal -> ValueSome(TyConst "decimal")
        | FormatType.Object
        | FormatType.Structured -> ValueSome(fresh ())
        | FormatType.FormatFunction
        | FormatType.Text -> ValueNone

    /// Per-entry-point description. `FormatArgIndex` is the positional slot of
    /// the format string (0 for `printf`/`sprintf`/…; 1 for `fprintf`, after
    /// the `TextWriter`). `Tail` is the final result of the curried printer
    /// (`unit` for the writing families, `string` for `sprintf`). `State` /
    /// `Residue` / `Result` are the remaining `PrintfFormat` type args.
    /// `LeadingArgTypes` is the expected types of the arguments before the
    /// format (length = `FormatArgIndex`).
    type Family =
        {
            FormatArgIndex: int
            Tail: SemType
            State: SemType
            Residue: SemType
            Result: SemType
            LeadingArgTypes: SemType list
        }

    /// TextWriter-backed families (`printf`, `printfn`, `eprintf`, …): the
    /// printer ends in `unit`, the format is `PrintfFormat<_, TextWriter,
    /// unit, unit>`.
    let private writerFamily (formatArgIndex: int) (leading: SemType list) : Family =
        {
            FormatArgIndex = formatArgIndex
            Tail = tyUnit
            State = tyTextWriter
            Residue = tyUnit
            Result = tyUnit
            LeadingArgTypes = leading
        }

    /// `sprintf` builds a string: printer ends in `string`, the format is
    /// `PrintfFormat<_, unit, string, string>`.
    let private stringFamily: Family =
        {
            FormatArgIndex = 0
            Tail = tyString
            State = tyUnit
            Residue = tyString
            Result = tyString
            LeadingArgTypes = []
        }

    /// Recognised printf entry points, keyed by source short name. v1 covers
    /// `printf`, `printfn`, `sprintf`, `fprintf`, `eprintf` (plus the `…n`
    /// newline variants). `bprintf` / the `k*`-continuation family are out of
    /// scope — they carry extra leading arguments and aren't needed by the
    /// canonical sample.
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

    /// Resolve a (possibly qualified) name to its printf family, keyed on the
    /// last `.`-separated segment so `Printf.printfn` and a bare `printfn`
    /// both hit. `ValueNone` for non-printf names.
    let tryFamily (name: string) : Family voption =
        let short =
            let dot = name.LastIndexOf '.'
            if dot < 0 then name else name.Substring(dot + 1)

        match Map.tryFind short families with
        | Some f -> ValueSome f
        | None -> ValueNone

    /// Build `PrintfFormat<printer, state, residue, result>`.
    let formatType (printer: SemType) (fam: Family) : SemType =
        TyClass(printfFormatName, [ printer; fam.State; fam.Residue; fam.Result ])

    /// Curry resolved argument types onto the family's tail, e.g. `[int] ->
    /// int -> unit`, `[string; int] -> string -> int -> unit`.
    let printerType (argTypes: SemType list) (fam: Family) : SemType =
        List.foldBack (fun a r -> TyFun(a, r)) argTypes fam.Tail

    /// The entry point's full curried type for a known specifier list, plus
    /// the format type and printer type computed along the way. Shape:
    /// `leading… -> PrintfFormat<printer,…> -> printer`. `ValueNone` when any
    /// specifier isn't typeable in v1 (`%a` / `%t`) — the caller then defers
    /// to standard inference. `fresh` supplies a type variable per polymorphic
    /// (`%A` / `%O`) argument.
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
    /// -> 'T`, where `'T` is a single fresh printer type variable. Used by
    /// `MockBuiltins` to register the symbol so the non-literal fallback (and
    /// plain name resolution) sees a coherent type.
    let genericSignature (fresh: unit -> SemType) (fam: Family) : SemType =
        let printer = fresh ()
        let fmt = formatType printer fam
        List.foldBack (fun a r -> TyFun(a, r)) (fam.LeadingArgTypes @ [ fmt ]) printer
