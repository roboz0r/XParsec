namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open Vesper

// The `%[flags][width][.precision][type]` grammar belongs to the lexer, so a
// placeholder arrives already classified as a `FormatType` and only its typing
// happens here.

module PrintfSpec =

    /// A format literal freezes to a `New` of this type, taking the raw string.
    let printfFormatName: string =
        SymbolKeyOps.bareName (SymbolKeyOps.typeMetaName RuntimeNames.printfFormatKey)

    let private tyUnit: SemType = TyConst(RuntimeNames.unitKey, Block.empty)

    let private tyString: SemType = TyConst(RuntimeNames.stringKey, Block.empty)

    let private tyInt: SemType = TyConst(RuntimeNames.intKey, Block.empty)

    let private tyTextWriter: SemType =
        TyConst(RuntimeNames.platformKey RuntimeNames.textWriterTypeId, Block.empty)

    let private tyStringBuilder: SemType =
        TyConst(RuntimeNames.platformKey RuntimeNames.stringBuilderTypeId, Block.empty)

    let private tyStringWriter: SemType =
        TyConst(RuntimeNames.platformKey RuntimeNames.stringWriterTypeId, Block.empty)

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

    /// How a type letter's arguments are typed.
    [<RequireQualifiedAccess>]
    type FormatArgTy =
        /// A metavar the caller mints.
        | Metavar of FormatHoleTy
        /// `%s` `%c` `%b` `%M`: the letter fixes the type.
        | Fixed of TypeKey
        /// `%a`: a `'State -> 'T -> 'Residue` printer, then a `'T` value.
        | PrinterAndValue
        /// `%t`: a `'State -> 'Residue` printer alone.
        | Printer

    let argTyOf (t: FormatType) : FormatArgTy =
        match t with
        | FormatType.DecimalInt
        | FormatType.UnsignedDecimalInt
        | FormatType.UnsignedHex
        | FormatType.UnsignedOctal
        | FormatType.UnsignedBinary -> FormatArgTy.Metavar FormatHoleTy.IntegerFamily
        | FormatType.FloatExponential
        | FormatType.FloatDecimal
        | FormatType.FloatCompact -> FormatArgTy.Metavar FormatHoleTy.FloatFamily
        | FormatType.Object
        | FormatType.Structured -> FormatArgTy.Metavar FormatHoleTy.Free
        | FormatType.Bool -> FormatArgTy.Fixed RuntimeNames.boolKey
        | FormatType.String -> FormatArgTy.Fixed RuntimeNames.stringKey
        | FormatType.Char -> FormatArgTy.Fixed RuntimeNames.charKey
        | FormatType.Decimal -> FormatArgTy.Fixed RuntimeNames.decimalKey
        | FormatType.FormatFunction -> FormatArgTy.PrinterAndValue
        | FormatType.Text -> FormatArgTy.Printer

    /// `%a` and `%t` — the only letters typed from the family's `'State`/`'Residue`
    /// rather than from a standalone value.
    let isCallbackHole (t: FormatType) : bool =
        match argTyOf t with
        | FormatArgTy.PrinterAndValue
        | FormatArgTy.Printer -> true
        | FormatArgTy.Metavar _
        | FormatArgTy.Fixed _ -> false

    /// The types a family admits, the DEFAULT leading; `ValueNone` for the unconstrained
    /// `Free` hole.
    let familyKeys (h: FormatHoleTy) : Block<TypeKey> voption =
        match h with
        | FormatHoleTy.Free -> ValueNone
        | FormatHoleTy.IntegerFamily -> ValueSome RuntimeNames.integerFormatKeys
        | FormatHoleTy.FloatFamily -> ValueSome RuntimeNames.floatFormatKeys

    /// The type a family settles on where nothing else pins it.
    let familyDefault (keys: Block<TypeKey>) : TypeKey = keys.[0]

    /// Every argument a placeholder consumes, in APPLICATION order: one `int` per `Star`
    /// dimension (width before precision, `sprintf "%*.*f" w p v`), then the value. A
    /// `%a`/`%t` printer takes no star dimension.
    let argTypes
        (mint: FormatHoleTy -> SemType)
        (state: SemType)
        (residue: SemType)
        (p: FormatPlaceholder)
        : SemType list =
        let withStarDims (value: SemType) =
            let starDim d =
                match d with
                | FormatDim.Star -> [ tyInt ]
                | FormatDim.Absent
                | FormatDim.Literal _ -> []

            starDim p.Width @ starDim p.Precision @ [ value ]

        match argTyOf p.Type with
        | FormatArgTy.PrinterAndValue ->
            let tv = mint FormatHoleTy.Free
            [ TyFun(state, TyFun(tv, residue)); tv ]
        | FormatArgTy.Printer -> [ TyFun(state, residue) ]
        | FormatArgTy.Metavar h -> withStarDims (mint h)
        | FormatArgTy.Fixed key -> withStarDims (TyConst(key, Block.empty))

    /// The specifier consumes an argument of FIXED CONCRETE type. False for `%A`/`%O`
    /// (a fresh typar) and for `%a`/`%t` (a function over `'State`/`'Residue`).
    let hasConcreteArgType (t: FormatType) : bool =
        match argTyOf t with
        | FormatArgTy.Fixed _ -> true
        | FormatArgTy.Metavar FormatHoleTy.IntegerFamily
        | FormatArgTy.Metavar FormatHoleTy.FloatFamily -> true
        | FormatArgTy.Metavar FormatHoleTy.Free
        | FormatArgTy.PrinterAndValue
        | FormatArgTy.Printer -> false

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
    let resolveExternalSlots (resolve: PlatformTypeId -> SemType voption) (fam: Family) : Family =
        let sub (t: SemType) =
            let resolveId id =
                match resolve id with
                | ValueSome resolved -> resolved
                | ValueNone -> t

            if t = tyTextWriter then
                resolveId RuntimeNames.textWriterTypeId
            elif t = tyStringBuilder then
                resolveId RuntimeNames.stringBuilderTypeId
            elif t = tyStringWriter then
                resolveId RuntimeNames.stringWriterTypeId
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
        TyClass(RuntimeNames.printfFormatKey, Block.ofList [ printer; fam.State; fam.Residue; fam.Result ])

    let printerType (argTypes: SemType list) (fam: Family) : SemType =
        List.foldBack (fun a r -> TyFun(a, r)) argTypes fam.Codomain

    /// Curried arguments the placeholders consume: the sum of per-hole `argTypes` lengths,
    /// NOT the hole count, since a star hole consumes 2–3. Only the count is read, so the dummy
    /// `mint` and `tyUnit` state/residue below never surface.
    let totalArity (specs: FormatPlaceholder list) : int =
        specs
        |> List.sumBy (fun p -> (argTypes (fun _ -> tyUnit) tyUnit tyUnit p).Length)

    /// The three types a printf-family application takes on.
    type AppliedTypes =
        {
            /// `leading… -> PrintfFormat<printer,…> -> printer`.
            FnTy: SemType
            FormatTy: SemType
            /// `arg1 -> … -> codomain`.
            Printer: SemType
        }

    /// The types a call to `fam` with these specifiers carries. Star dimensions fold their
    /// leading `int`s in, so the printer curries width and precision ahead of the value.
    let appliedTypeOf (mint: FormatHoleTy -> SemType) (specs: FormatPlaceholder list) (fam: Family) : AppliedTypes =
        let flatArgTypes = specs |> List.collect (argTypes mint fam.State fam.Residue)
        let printer = printerType flatArgTypes fam
        let fmt = formatType printer fam

        {
            FnTy = List.foldBack (fun a r -> TyFun(a, r)) (fam.LeadingArgTypes @ [ fmt ]) printer
            FormatTy = fmt
            Printer = printer
        }

    /// The PRINTER type (`arg1 -> … -> result`) the specifiers denote at a format-typed
    /// annotation rather than an application, where the position already fixes
    /// `state`/`residue`/`result`. The printer alone: the caller keeps its own format type.
    let printerFromSlots
        (mint: FormatHoleTy -> SemType)
        (specs: FormatPlaceholder list)
        (state: SemType)
        (residue: SemType)
        (result: SemType)
        : SemType =
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

        (appliedTypeOf mint specs fam).Printer
