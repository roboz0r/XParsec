namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// The `%[flags][width][.precision][type]` grammar belongs to the lexer — a
// placeholder arrives already classified as a `FormatType`, and only its typing
// happens here.

module PrintfSpec =

    /// A format literal freezes to a `New` of this type, taking the raw string.
    let printfFormatName: string =
        SymbolKeyOps.bareName (SymbolKeyOps.typeMetaName RuntimeNames.printfFormatKey)

    let private tyUnit: SemType = TyConst(RuntimeNames.unitKey, EqArray.empty)

    let private tyString: SemType = TyConst(RuntimeNames.stringKey, EqArray.empty)

    let private tyInt: SemType = TyConst(RuntimeNames.intKey, EqArray.empty)

    let private tyTextWriter: SemType =
        TyConst(RuntimeNames.opaqueKey RuntimeNames.textWriterTypeName, EqArray.empty)

    let private tyStringBuilder: SemType =
        TyConst(RuntimeNames.opaqueKey RuntimeNames.stringBuilderTypeName, EqArray.empty)

    let private tyStringWriter: SemType =
        TyConst(RuntimeNames.opaqueKey RuntimeNames.stringWriterTypeName, EqArray.empty)

    /// Where a printf entry point writes, resolved from its name alone.
    [<RequireQualifiedAccess>]
    type PrintfSink =
        | StdOut of newline: bool
        | StdErr of newline: bool
        | StringResult
        /// `fprintf` / `fprintfn` — a `TextWriter` at arg 0, the format at arg 1.
        | Writer of newline: bool
        /// `bprintf` — a `StringBuilder` at arg 0, the format at arg 1. F# has no
        /// `bprintfn`, hence no newline flag.
        | Builder

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
        | "bprintf" -> ValueSome PrintfSink.Builder
        | "sprintf" -> ValueSome PrintfSink.StringResult
        | _ -> ValueNone

    /// The CLR `Vesper.Formatter` handler member a hole dispatches to. A hole's
    /// type alone can't pick one (`%o` and `%u` are both `int`-typed).
    [<RequireQualifiedAccess>]
    type HoleKind =
        /// `AppendFormatted<Ty>(v [,alignment] [,format])`.
        | Formatted
        /// `AppendBool(v, alignment)` — lowercase `true`/`false`; `bool.ToString`
        /// capitalises, so `Formatted` cannot produce it.
        | BoolText
        /// `AppendUnsigned(v, alignment)` — the `int` argument's bits
        /// reinterpreted as `uint` (F# `%u`).
        | Unsigned
        /// `AppendOctal(v, alignment)` — `Convert.ToString(v, 8)` (.NET has no
        /// octal format string); two's-complement for negatives, matching F#.
        | Octal
        /// `AppendZeroPaddedUnsigned(v, width)` — F# `%05u`: unsigned decimal,
        /// zero-padded to a total field of `width` (which rides in the alignment slot).
        | UnsignedZeroPad
        /// `AppendZeroPaddedOctal(v, width)` — F# `%08o`: two's-complement octal,
        /// zero-padded to a total field of `width` (which rides in the alignment slot).
        | OctalZeroPad
        /// `AppendZeroPaddedFloat(v, format, width)` — F# `%0w.pf`: format via an
        /// `"F<prec>"` string, then zero-pad AFTER ANY SIGN to a total field of `width`.
        | ZeroPaddedFloat
        /// `AppendRightZeroPaddedFloat(v, format, width)` — F# `%-0w.Nf`: F#'s
        /// left-align plus zero-pad fills the RIGHT, past the digits, out to `width`.
        | RightZeroPaddedFloat
        /// `AppendStructured<v>(v, widthBudget, sizeBudget)` — F# `%A`: a runtime engine
        /// renders the value as copy-pasteable source, the two budgets off the placeholder.
        | Structured

    /// `%a` and `%t` — the only letters typed from the family's `'State`/`'Residue`
    /// rather than from a standalone value.
    let isCallbackHole (t: FormatType) : bool =
        match t with
        | FormatType.FormatFunction
        | FormatType.Text -> true
        | _ -> false

    /// The SemType of the VALUE argument a plain-value letter consumes; `%A`/`%O`
    /// both give a fresh type variable.
    let private argType (fresh: unit -> SemType) (t: FormatType) : SemType =
        match t with
        | FormatType.Bool -> TyConst(RuntimeNames.boolKey, EqArray.empty)
        | FormatType.String -> tyString
        | FormatType.Char -> TyConst(RuntimeNames.charKey, EqArray.empty)
        | FormatType.DecimalInt
        | FormatType.UnsignedDecimalInt
        | FormatType.UnsignedHex
        | FormatType.UnsignedOctal
        | FormatType.UnsignedBinary -> tyInt
        | FormatType.FloatExponential
        | FormatType.FloatDecimal
        | FormatType.FloatCompact -> TyConst(RuntimeNames.floatKey, EqArray.empty)
        | FormatType.Decimal -> TyConst(RuntimeNames.decimalKey, EqArray.empty)
        | FormatType.Object
        | FormatType.Structured -> fresh ()
        | FormatType.FormatFunction
        | FormatType.Text -> failwith "PrintfSpec.argType: %a/%t are typed by argTypes, never here"

    /// Every argument a placeholder consumes, in APPLICATION order: one `int` per `Star`
    /// dimension (width before precision, `sprintf "%*.*f" w p v`), then the value.
    let argTypes
        (fresh: unit -> SemType)
        (state: SemType)
        (residue: SemType)
        (p: FormatPlaceholder)
        : SemType list voption =
        match p.Type with
        | FormatType.FormatFunction ->
            let tv = fresh ()
            ValueSome [ TyFun(state, TyFun(tv, residue)); tv ]
        | FormatType.Text -> ValueSome [ TyFun(state, residue) ]
        | _ ->
            let value = argType fresh p.Type

            let starDim d =
                match d with
                | FormatDim.Star -> [ tyInt ]
                | FormatDim.Absent
                | FormatDim.Literal _ -> []

            ValueSome(starDim p.Width @ starDim p.Precision @ [ value ])

    /// The specifier consumes an argument of FIXED CONCRETE type. False for `%A`/`%O`
    /// (a fresh typar) and for `%a`/`%t` (a function over `'State`/`'Residue`).
    let hasConcreteArgType (t: FormatType) : bool =
        match t with
        | FormatType.Object
        | FormatType.Structured -> false
        | t -> not (isCallbackHole t)

    /// EXACTLY ONE argument, of fixed concrete type — what a one-arg-per-hole peel needs.
    /// A star dimension yields a leading `int` too, so a star hole is concrete yet multi-arg.
    let isUnaryConcreteHole (p: FormatPlaceholder) : bool =
        hasConcreteArgType p.Type
        && p.Width <> FormatDim.Star
        && p.Precision <> FormatDim.Star

    /// `FormatArgIndex` is the format string's positional slot: 0 for `printf`/`sprintf`,
    /// 1 for `fprintf`. `Tail` is the curried printer's final result — `unit` when
    /// writing, `string` for `sprintf`.
    type Family =
        {
            FormatArgIndex: int
            Tail: SemType
            State: SemType
            Residue: SemType
            Result: SemType
            /// Types expected before the format; length = `FormatArgIndex`.
            LeadingArgTypes: SemType list
            /// The concrete sink a capture-first `%a`/`%t` hole instantiates. A writer
            /// family's `State` is the ABSTRACT `TextWriter` (can't be `new`d), so its
            /// scratch is `StringWriter`; `sprintf` needs none.
            ScratchSink: SemType
        }

    let private writerFamily (formatArgIndex: int) (leading: SemType list) : Family =
        {
            FormatArgIndex = formatArgIndex
            Tail = tyUnit
            State = tyTextWriter
            Residue = tyUnit
            Result = tyUnit
            LeadingArgTypes = leading
            ScratchSink = tyStringWriter
        }

    let private builderFamily (formatArgIndex: int) (leading: SemType list) : Family =
        {
            FormatArgIndex = formatArgIndex
            Tail = tyUnit
            State = tyStringBuilder
            Residue = tyUnit
            Result = tyUnit
            LeadingArgTypes = leading
            ScratchSink = tyStringBuilder
        }

    let private stringFamily: Family =
        {
            FormatArgIndex = 0
            Tail = tyString
            State = tyUnit
            Residue = tyString
            Result = tyString
            LeadingArgTypes = []
            ScratchSink = tyUnit
        }

    /// Keyed by source short name. The `k*` continuation forms are absent — they
    /// carry an extra leading continuation argument.
    let families: Map<string, Family> =
        [
            "printf", writerFamily 0 []
            "printfn", writerFamily 0 []
            "eprintf", writerFamily 0 []
            "eprintfn", writerFamily 0 []
            "fprintf", writerFamily 1 [ tyTextWriter ]
            "fprintfn", writerFamily 1 [ tyTextWriter ]
            "bprintf", builderFamily 1 [ tyStringBuilder ]
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

    /// Rewrite the by-name sink slots (`State`, `ScratchSink`, the matching
    /// `LeadingArgTypes` entry) to whatever `resolve` gives: a real writer argument
    /// resolves to a `TyClass`, and `unify` has no `TyClass`/`TyConst` arm.
    let resolveExternalSlots (resolve: string -> SemType voption) (fam: Family) : Family =
        let sub (t: SemType) =
            let resolveName name =
                match resolve name with
                | ValueSome resolved -> resolved
                | ValueNone -> t

            if t = tyTextWriter then
                resolveName RuntimeNames.textWriterTypeName
            elif t = tyStringBuilder then
                resolveName RuntimeNames.stringBuilderTypeName
            elif t = tyStringWriter then
                resolveName RuntimeNames.stringWriterTypeName
            else
                t

        { fam with
            State = sub fam.State
            ScratchSink = sub fam.ScratchSink
            LeadingArgTypes = fam.LeadingArgTypes |> List.map sub
        }

    /// Whether a `%a`/`%t` hole can lower on this target: the family's `'State` sink must be
    /// nameable here. Takes a resolved `Family` — a slot still a by-name `TyConst` is not.
    let callbackSinkAvailable (fam: Family) : bool =
        match fam.State with
        | TyClass _ -> true
        | s -> s = tyUnit

    /// Whether a `%a`/`%t` call lowers CAPTURE-FIRST into a scratch sink rather than
    /// splicing the callback's returned string. `sprintf` alone has `ScratchSink = unit`.
    let familyNeedsScratch (fam: Family) : bool = fam.ScratchSink <> tyUnit

    /// Everything a writer/builder `%a`/`%t` call splices into its residue block
    /// `{ let s = new ScratchClassName() in cb s [value]; s.ToString() }`.
    type CallbackScratch =
        {
            /// `System.IO.StringWriter` / `System.Text.StringBuilder`.
            ScratchClassName: string
            /// The `New` result type, and the object-argument type of the `ToString` call.
            ScratchTy: SemType
            /// The interned `ToString()` member key codegen mints the ref off.
            ToStringKey: SymbolKey
        }

    let formatType (printer: SemType) (fam: Family) : SemType =
        TyClass(RuntimeNames.printfFormatKey, EqArray.ofList [ printer; fam.State; fam.Residue; fam.Result ])

    let printerType (argTypes: SemType list) (fam: Family) : SemType =
        List.foldBack (fun a r -> TyFun(a, r)) argTypes fam.Tail

    /// Curried arguments the placeholders consume — the sum of per-hole `argTypes` lengths,
    /// NOT the hole count: a star hole consumes 2–3. Only the count is read, so the dummy
    /// `fresh` and `tyUnit` state/residue below never surface.
    let totalArity (specs: FormatPlaceholder list) : int =
        specs
        |> List.sumBy (fun p ->
            match argTypes (fun () -> tyUnit) tyUnit tyUnit p with
            | ValueSome ts -> ts.Length
            | ValueNone -> 0
        )

    /// `leading… -> PrintfFormat<printer,…> -> printer`, plus the format type and printer
    /// type computed on the way. Star dimensions fold their leading `int`s in, so the
    /// printer curries width and precision ahead of the value.
    let appliedTypeOf
        (fresh: unit -> SemType)
        (specs: FormatPlaceholder list)
        (fam: Family)
        : (SemType * SemType * SemType) voption =
        let rec mapAll acc specs =
            match specs with
            | [] -> ValueSome(List.rev acc)
            | p :: rest ->
                match argTypes fresh fam.State fam.Residue p with
                | ValueSome ts -> mapAll (List.rev ts @ acc) rest
                | ValueNone -> ValueNone

        match mapAll [] specs with
        | ValueNone -> ValueNone
        | ValueSome flatArgTypes ->
            let printer = printerType flatArgTypes fam
            let fmt = formatType printer fam

            let fnTy =
                List.foldBack (fun a r -> TyFun(a, r)) (fam.LeadingArgTypes @ [ fmt ]) printer

            ValueSome(fnTy, fmt, printer)

    /// The PRINTER type (`arg1 -> … -> result`) the specifiers denote where the position
    /// already fixes `state`/`residue`/`result` — a format-typed annotation, not an
    /// application. The printer alone: the caller keeps its own format type.
    let printerFromSlots
        (fresh: unit -> SemType)
        (specs: FormatPlaceholder list)
        (state: SemType)
        (residue: SemType)
        (result: SemType)
        : SemType voption =
        let fam =
            {
                FormatArgIndex = 0
                Tail = result
                State = state
                Residue = residue
                Result = result
                LeadingArgTypes = []
                ScratchSink = tyUnit
            }

        match appliedTypeOf fresh specs fam with
        | ValueSome(_, _, printer) -> ValueSome printer
        | ValueNone -> ValueNone
