namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer

// The `%[flags][width][.precision][type]` grammar belongs to the lexer, so a
// placeholder arrives already classified as a `FormatType` and only its typing
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

    /// The last `.`-separated segment: `Printf.printfn` and a bare `printfn` both
    /// yield `printfn`.
    let private lastSegment (name: string) : string =
        let dot = name.LastIndexOf '.'
        if dot < 0 then name else name.Substring(dot + 1)

    let sinkOf (name: string) : PrintfSink voption =
        match lastSegment name with
        | "printf" -> ValueSome(PrintfSink.StdOut false)
        | "printfn" -> ValueSome(PrintfSink.StdOut true)
        | "eprintf" -> ValueSome(PrintfSink.StdErr false)
        | "eprintfn" -> ValueSome(PrintfSink.StdErr true)
        | "fprintf" -> ValueSome(PrintfSink.Writer false)
        | "fprintfn" -> ValueSome(PrintfSink.Writer true)
        | "bprintf" -> ValueSome PrintfSink.Builder
        | "sprintf" -> ValueSome PrintfSink.StringResult
        | _ -> ValueNone

    /// `%a` and `%t` — the only letters typed from the family's `'State`/`'Residue`
    /// rather than from a standalone value.
    let isCallbackHole (t: FormatType) : bool =
        match t with
        | FormatType.FormatFunction
        | FormatType.Text -> true
        | _ -> false

    /// A metavar a placeholder's value argument needs, where the type letter alone does not
    /// fix the type. The caller mints it, so the constraint and the default that make a
    /// family flexible are attached where a `PassContext` is available.
    [<RequireQualifiedAccess>]
    type FormatHoleTy =
        /// `%A` / `%O`, and a `%a` callback's own value: unconstrained.
        | Free
        /// `%d` `%i` `%u` `%x` `%X` `%o` `%B`: any integer type, `int` by default.
        | IntegerFamily
        /// `%f` `%e` `%E` `%g` `%G`: `float`, `float32` or `decimal`, `float` by default.
        | FloatFamily

    /// The metavar a type letter's value argument needs; `ValueNone` where the letter fixes
    /// the type outright (`%s`, `%c`, `%b`, `%M`) or consumes no standalone value.
    let holeTyOf (t: FormatType) : FormatHoleTy voption =
        match t with
        | FormatType.DecimalInt
        | FormatType.UnsignedDecimalInt
        | FormatType.UnsignedHex
        | FormatType.UnsignedOctal
        | FormatType.UnsignedBinary -> ValueSome FormatHoleTy.IntegerFamily
        | FormatType.FloatExponential
        | FormatType.FloatDecimal
        | FormatType.FloatCompact -> ValueSome FormatHoleTy.FloatFamily
        | FormatType.Object
        | FormatType.Structured -> ValueSome FormatHoleTy.Free
        | FormatType.Bool
        | FormatType.String
        | FormatType.Char
        | FormatType.Decimal
        | FormatType.FormatFunction
        | FormatType.Text -> ValueNone

    /// The types a family admits, the DEFAULT leading; `ValueNone` for the unconstrained
    /// `Free` hole.
    let familyKeys (h: FormatHoleTy) : EqArray<TypeKey> voption =
        match h with
        | FormatHoleTy.Free -> ValueNone
        | FormatHoleTy.IntegerFamily -> ValueSome RuntimeNames.integerFormatKeys
        | FormatHoleTy.FloatFamily -> ValueSome RuntimeNames.floatFormatKeys

    /// The type a family settles on where nothing else pins it.
    let familyDefault (keys: EqArray<TypeKey>) : TypeKey = keys.Underlying.[0]

    /// The SemType of the VALUE argument a plain-value letter consumes.
    let private argType (mint: FormatHoleTy -> SemType) (t: FormatType) : SemType =
        match holeTyOf t with
        | ValueSome h -> mint h
        | ValueNone ->
            match t with
            | FormatType.Bool -> TyConst(RuntimeNames.boolKey, EqArray.empty)
            | FormatType.String -> tyString
            | FormatType.Char -> TyConst(RuntimeNames.charKey, EqArray.empty)
            | FormatType.Decimal -> TyConst(RuntimeNames.decimalKey, EqArray.empty)
            | _ -> failwith "PrintfSpec.argType: %a/%t are typed by argTypes, never here"

    /// Every argument a placeholder consumes, in APPLICATION order: one `int` per `Star`
    /// dimension (width before precision, `sprintf "%*.*f" w p v`), then the value.
    let argTypes
        (mint: FormatHoleTy -> SemType)
        (state: SemType)
        (residue: SemType)
        (p: FormatPlaceholder)
        : SemType list voption =
        match p.Type with
        | FormatType.FormatFunction ->
            let tv = mint FormatHoleTy.Free
            ValueSome [ TyFun(state, TyFun(tv, residue)); tv ]
        | FormatType.Text -> ValueSome [ TyFun(state, residue) ]
        | _ ->
            let value = argType mint p.Type

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

    /// EXACTLY ONE argument, of fixed concrete type, which is what a one-arg-per-hole peel needs.
    /// A star dimension yields a leading `int` too, so a star hole is concrete yet multi-arg.
    let isUnaryConcreteHole (p: FormatPlaceholder) : bool =
        hasConcreteArgType p.Type
        && p.Width <> FormatDim.Star
        && p.Precision <> FormatDim.Star

    /// `FormatArgIndex` is the format string's positional slot: 0 for `printf`/`sprintf`,
    /// 1 for `fprintf`. `Codomain` is what the curried printer returns once every hole is
    /// applied: `unit` when writing, `string` for `sprintf`.
    type Family =
        {
            FormatArgIndex: int
            Codomain: SemType
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
            Codomain = tyUnit
            State = tyTextWriter
            Residue = tyUnit
            Result = tyUnit
            LeadingArgTypes = leading
            ScratchSink = tyStringWriter
        }

    let private builderFamily (formatArgIndex: int) (leading: SemType list) : Family =
        {
            FormatArgIndex = formatArgIndex
            Codomain = tyUnit
            State = tyStringBuilder
            Residue = tyUnit
            Result = tyUnit
            LeadingArgTypes = leading
            ScratchSink = tyStringBuilder
        }

    let private stringFamily: Family =
        {
            FormatArgIndex = 0
            Codomain = tyString
            State = tyUnit
            Residue = tyString
            Result = tyString
            LeadingArgTypes = []
            ScratchSink = tyUnit
        }

    /// Keyed by source short name. The `k*` continuation forms are absent because their
    /// leading continuation is typed by the call site's result, not by a fixed type like
    /// `fprintf`'s `TextWriter`.
    ///
    /// PROVISIONAL: hardcoded because no contract declares the family. A printf runtime
    /// restores a `printf.fsi` declaring all eight, and this table goes with it.
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
        match Map.tryFind (lastSegment name) families with
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
    /// nameable here. Takes a resolved `Family`, because an unresolved sink is still a
    /// by-name `TyConst` and fails this test.
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

    /// The shape Elaborate lowers one printf-family `Expr.App` to. An application carrying
    /// none of these keeps the generic FSharp.Core printf shape.
    [<RequireQualifiedAccess>]
    type PrintfLowering =
        /// Fully applied, lowering to a `TExpr.Format`. `Scratch` is present only for a
        /// `%a`/`%t` hole on a writer or builder sink; `sprintf`'s residue is its own result.
        | Full of sink: PrintfSink * scratch: CallbackScratch voption
        /// Fully unapplied (`printfn "%d"`), lowering to a synthesised closure. Never
        /// `%A`/`%O`, because an unapplied hole there is an unpinned typar.
        | Partial of sink: PrintfSink

    let formatType (printer: SemType) (fam: Family) : SemType =
        TyClass(RuntimeNames.printfFormatKey, EqArray.ofList [ printer; fam.State; fam.Residue; fam.Result ])

    let printerType (argTypes: SemType list) (fam: Family) : SemType =
        List.foldBack (fun a r -> TyFun(a, r)) argTypes fam.Codomain

    /// Curried arguments the placeholders consume: the sum of per-hole `argTypes` lengths,
    /// NOT the hole count, since a star hole consumes 2–3. Only the count is read, so the dummy
    /// `mint` and `tyUnit` state/residue below never surface.
    let totalArity (specs: FormatPlaceholder list) : int =
        specs
        |> List.sumBy (fun p ->
            match argTypes (fun _ -> tyUnit) tyUnit tyUnit p with
            | ValueSome ts -> ts.Length
            | ValueNone -> 0
        )

    /// `leading… -> PrintfFormat<printer,…> -> printer`, plus the format type and printer
    /// type computed on the way. Star dimensions fold their leading `int`s in, so the
    /// printer curries width and precision ahead of the value.
    let appliedTypeOf
        (mint: FormatHoleTy -> SemType)
        (specs: FormatPlaceholder list)
        (fam: Family)
        : (SemType * SemType * SemType) voption =
        let rec mapAll acc specs =
            match specs with
            | [] -> ValueSome(List.rev acc)
            | p :: rest ->
                match argTypes mint fam.State fam.Residue p with
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

    /// The PRINTER type (`arg1 -> … -> result`) the specifiers denote at a format-typed
    /// annotation rather than an application, where the position already fixes
    /// `state`/`residue`/`result`. The printer alone: the caller keeps its own format type.
    let printerFromSlots
        (mint: FormatHoleTy -> SemType)
        (specs: FormatPlaceholder list)
        (state: SemType)
        (residue: SemType)
        (result: SemType)
        : SemType voption =
        let fam =
            {
                FormatArgIndex = 0
                Codomain = result
                State = state
                Residue = residue
                Result = result
                LeadingArgTypes = []
                ScratchSink = tyUnit
            }

        match appliedTypeOf mint specs fam with
        | ValueSome(_, _, printer) -> ValueSome printer
        | ValueNone -> ValueNone
