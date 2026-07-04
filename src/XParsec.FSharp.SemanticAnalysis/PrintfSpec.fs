namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// Printf format-string analysis.
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

    let private tyTextWriter: SemType =
        TyConst(RuntimeNames.textWriterTypeName, EqArray.empty)

    /// Target-agnostic classification of a printf entry point's output sink,
    /// resolved from the entry-point name. Recorded on `PassContext.PrintfApp`
    /// for the calls P1 lowers inline.
    [<RequireQualifiedAccess>]
    type PrintfSink =
        | StdOut of newline: bool
        | StdErr of newline: bool
        | StringResult
        /// `fprintf` / `fprintfn` — a `TextWriter` leading argument (arg 0, the
        /// format at arg 1). `newline` records the trailing `\n` (`fprintfn`
        /// sets it). Freeze reads the writer sink kind to recover the format arg
        /// index (writer ⇒ 1, else 0).
        | Writer of newline: bool

    /// Sink for a natively-lowered printf family member. The console/string
    /// families put the format at arg 0; `fprintf`/`fprintfn` (`Writer`) put a
    /// `TextWriter` at arg 0 and the format at arg 1. `bprintf` and every other
    /// name return `ValueNone`, keeping the existing FSharp.Core path. Keyed on
    /// the last `.`-segment so `Printf.printfn` and bare `printfn` both hit
    /// (mirrors `tryFamily`).
    let sinkOf (name: string) : PrintfSink voption =
        let short =
            let dot = name.LastIndexOf '.'
            if dot < 0 then name else name.Substring(dot + 1)

        match short with
        | "printf" -> ValueSome(PrintfSink.StdOut false)
        | "printfn" -> ValueSome(PrintfSink.StdOut true)
        | "eprintf" -> ValueSome(PrintfSink.StdErr false)
        | "eprintfn" -> ValueSome(PrintfSink.StdErr true)
        | "fprintf" -> ValueSome(PrintfSink.Writer false)
        | "fprintfn" -> ValueSome(PrintfSink.Writer true)
        | "sprintf" -> ValueSome PrintfSink.StringResult
        | _ -> ValueNone

    /// The CLR `Vesper.Formatter` handler member a hole dispatches to. `Ty` alone
    /// can't disambiguate (`%o` and `%u` are both `int`-typed), so the CLR-only
    /// projection (`Codegen.Clr.ClrHoleFormat.toDotNetFormat`) tags each `Field`
    /// hole with a kind. `Formatted` is the default (everything that maps onto
    /// `AppendFormatted<T>` under a .NET format string); the others need a dedicated
    /// handler member because they have no such mapping. `%A` is emitted from
    /// `PrintfHoleForm.HoleForm.PercentA` directly, so `Structured` never flows
    /// through `toDotNetFormat`.
    ///
    /// Lives here (a SemType-layer module) rather than in a codegen assembly so it
    /// compiles before its CLR-backend consumers; the `.NET format string` dialect
    /// these kinds describe is reconstructed only in `ClrHoleFormat`.
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
        /// float format zero-pads to a total width. `toDotNetFormat` returns the
        /// `"F<prec>"` body as the format string and the field width in the
        /// alignment slot.
        | ZeroPaddedFloat
        /// `AppendStructured<v>(v, widthBudget, sizeBudget)` — F# `%A`. No .NET
        /// format string; the runtime engine (`Vesper.Printf.StructuralPrinter`)
        /// renders the value as copy-pasteable source. `EmitFormat` resolves the
        /// width / size budgets from `PrintfHoleForm.HoleForm.PercentA`
        /// (`percentAWidth` / `percentASize`), so this kind is never produced by
        /// `toDotNetFormat`.
        | Structured

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

    /// True when the specifier consumes an argument of a *fixed concrete* type.
    /// Excludes `%a`/`%t` (`FormatFunction`/`Text` — no arg type at all) and
    /// `%A`/`%O` (`Object`/`Structured` — a fresh typar, which is unpinned when the
    /// printf partial is left unapplied). A fully-unapplied partial over such a hole
    /// would be a *generic* value struct, out of scope for the 4a heap-closure
    /// lowering; `argType` returns `fresh ()`/`ValueNone` for exactly these.
    let hasConcreteArgType (t: FormatType) : bool =
        match t with
        | FormatType.Object
        | FormatType.Structured
        | FormatType.FormatFunction
        | FormatType.Text -> false
        | _ -> true

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

    /// Rewrite the family's writer slot — the by-name `TyConst(System.IO.TextWriter)`
    /// placeholder (`State`, and the `LeadingArgTypes` writer of `fprintf`/`fprintfn`)
    /// — to `writerTy`. The gate passes the provider-resolved `TyClass(TextWriter)`
    /// (the SAME `TypeKey` a real `Console.Out` argument carries), so a leading writer
    /// arg reconciles with the slot under plain `unify` (core `unify` compares
    /// `TyClass` by key equality, and never a `TyClass` against a `TyConst`). Kept
    /// here — and provider-free — so `PrintfSpec` stays a pure SemType module: the
    /// gate owns the `ctx.Provider` resolution and hands in the resolved type.
    let substituteWriter (writerTy: SemType) (fam: Family) : Family =
        let sub (t: SemType) =
            if t = tyTextWriter then writerTy else t

        { fam with
            State = sub fam.State
            LeadingArgTypes = fam.LeadingArgTypes |> List.map sub
        }

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
