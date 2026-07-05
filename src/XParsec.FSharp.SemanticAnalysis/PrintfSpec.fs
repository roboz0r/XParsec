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
    let private tyInt: SemType = TyConst("int", EqArray.empty)

    let private tyTextWriter: SemType =
        TyConst(RuntimeNames.textWriterTypeName, EqArray.empty)

    let private tyStringBuilder: SemType =
        TyConst(RuntimeNames.stringBuilderTypeName, EqArray.empty)

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
        /// `bprintf` — a `StringBuilder` leading argument (arg 0, the format at
        /// arg 1). No `bprintfn` exists in F#, so there is no trailing newline.
        /// Freeze recovers the format arg index (builder ⇒ 1, else 0) the same way.
        | Builder

    /// Sink for a natively-lowered printf family member. The console/string
    /// families put the format at arg 0; `fprintf`/`fprintfn` (`Writer`) put a
    /// `TextWriter` at arg 0 and the format at arg 1; `bprintf` (`Builder`) puts a
    /// `StringBuilder` at arg 0 and the format at arg 1. Every other name returns
    /// `ValueNone`, keeping the existing FSharp.Core path. Keyed on the last
    /// `.`-segment so `Printf.printfn` and bare `printfn` both hit (mirrors `tryFamily`).
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
        /// `AppendZeroPaddedUnsigned(v, width)` — F# `%05u`: unsigned decimal,
        /// zero-padded to a total field of `width` (the width rides in the
        /// alignment slot as a `Const`, like `ZeroPaddedFloat`). Distinct from
        /// `Unsigned` because zero-pad and space-pad share no operand slot.
        | UnsignedZeroPad
        /// `AppendZeroPaddedOctal(v, width)` — F# `%08o`: two's-complement octal,
        /// zero-padded to a total field of `width` (the width rides in the
        /// alignment slot as a `Const`). Distinct from `Octal` for the same reason.
        | OctalZeroPad
        /// `AppendZeroPaddedFloat(v, format, width)` — F# `%0w.pf`: format the
        /// float via `format` (an `"F<prec>"` string), then zero-pad *after any
        /// sign* to a total field of `width` chars. Dedicated because no .NET
        /// float format zero-pads to a total width. `toDotNetFormat` returns the
        /// `"F<prec>"` body as the format string and the field width in the
        /// alignment slot.
        | ZeroPaddedFloat
        /// `AppendRightZeroPaddedFloat(v, format, width)` — F# `%-0w.Nf`: format the
        /// float via `format` (an `"F<prec>"` string), then zero-pad on the RIGHT (past
        /// the digits) to a total field of `width`. Dedicated because F#'s left-align +
        /// zero-pad fills the right with zeros — no .NET format nor field alignment does
        /// this. `toDotNetFormat` returns the `"F<prec>"` body and the width in the
        /// alignment slot, exactly like `ZeroPaddedFloat`.
        | RightZeroPaddedFloat
        /// `AppendStructured<v>(v, widthBudget, sizeBudget)` — F# `%A`. No .NET
        /// format string; the runtime engine (`Vesper.Printf.StructuralPrinter`)
        /// renders the value as copy-pasteable source. `EmitFormat` resolves the
        /// width / size budgets from `PrintfHoleForm.HoleForm.PercentA`
        /// (`percentAWidth` / `percentASize`), so this kind is never produced by
        /// `toDotNetFormat`.
        | Structured

    /// The two printer-callback letters — `%a` (`FormatFunction`) and `%t` (`Text`).
    /// They're the only letters `argTypes` types from the family's `'State`/`'Residue`
    /// rather than a standalone value, and the only ones excluded from every "consumes
    /// a plain value" path. Centralised so that "these two are special" has one home
    /// instead of an inline `= FormatFunction || = Text` at each call site.
    let isCallbackHole (t: FormatType) : bool =
        match t with
        | FormatType.FormatFunction
        | FormatType.Text -> true
        | _ -> false

    /// SemType of the *value* argument a plain-value type letter consumes. `%A`/`%O`
    /// both consume a polymorphic argument (a fresh type variable); the `%A`-vs-`%O`
    /// distinction, irrelevant to typing, stays in the `FormatType` for codegen.
    /// `%a`/`%t` never reach here — their shape (two entries for `%a`; a
    /// `'State`/`'Residue` dependency for both) can't fit one `SemType` with no
    /// `Family` in scope, so `argTypes` intercepts them before this helper and types
    /// them directly. The `isCallbackHole` arm is therefore unreachable.
    ///
    /// Private: no cross-pass signature carries a bare `FormatType` — star-ness
    /// lives on the placeholder, so callers go through `argTypes`, which folds in
    /// the star-dimension arguments this letter-keyed helper knows nothing about.
    let private argType (fresh: unit -> SemType) (t: FormatType) : SemType =
        match t with
        | FormatType.Bool -> TyConst("bool", EqArray.empty)
        | FormatType.String -> tyString
        | FormatType.Char -> TyConst("char", EqArray.empty)
        | FormatType.DecimalInt
        | FormatType.UnsignedDecimalInt
        | FormatType.UnsignedHex
        | FormatType.UnsignedOctal
        | FormatType.UnsignedBinary -> tyInt
        | FormatType.FloatExponential
        | FormatType.FloatDecimal
        | FormatType.FloatCompact -> TyConst("float", EqArray.empty)
        | FormatType.Decimal -> TyConst("decimal", EqArray.empty)
        | FormatType.Object
        | FormatType.Structured -> fresh ()
        | FormatType.FormatFunction
        | FormatType.Text -> failwith "PrintfSpec.argType: %a/%t are typed by argTypes, never here"

    /// Every argument a placeholder consumes, in *application* order: a leading
    /// `int` per `Star` dimension (width before precision, matching F#'s
    /// `sprintf "%*.*f" width precision value`), then the value argument. `%a`/`%t`
    /// depend on the family's `state`/`residue`, so those are passed in explicitly
    /// (the `Family` record is declared below this helper, so threading the two
    /// SemTypes avoids a forward reference). `%a` (`FormatFunction`) consumes a
    /// printer `'State -> 'T -> 'Residue` *and* the value `'T` — the SAME fresh
    /// typar node in both positions, so they unify to one type; `%t` (`Text`)
    /// consumes just `'State -> 'Residue`. Neither carries width/precision in F#,
    /// so both branch out at the top and skip the star-fold. This is the per-hole
    /// typing seam: arity is `List.length`, never a hole count.
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

    /// True when the specifier consumes an argument of a *fixed concrete* type.
    /// Excludes `%a`/`%t` (`FormatFunction`/`Text`) and `%A`/`%O`
    /// (`Object`/`Structured` — a fresh typar, which is unpinned when the printf
    /// partial is left unapplied). Though `argTypes` now types `%a`/`%t` (a printer
    /// arrow over the family's `'State`/`'Residue`, plus a fresh value typar for
    /// `%a`), they are still not *concrete*: a fully-unapplied partial over such a
    /// hole would be a *generic* value struct, out of scope for the 4a heap-closure
    /// lowering, so they must stay `false` here and route cold.
    let hasConcreteArgType (t: FormatType) : bool =
        match t with
        | FormatType.Object
        | FormatType.Structured -> false
        | t -> not (isCallbackHole t)

    /// A hole admissible for the flat value-struct closure lowering (partial-app
    /// 4a): it consumes *exactly one* argument of fixed concrete type. The Fun-K
    /// peel that lowering drives assumes one arg per hole; a star dimension makes
    /// `argTypes` yield an extra leading `int`, so a star hole is all-concrete yet
    /// multi-arg — `hasConcreteArgType` alone would silently admit it the moment
    /// the classify gate starts accepting star. Gating on this stated invariant
    /// keeps extending the peel to multi-arg holes a deliberate later decision.
    let isUnaryConcreteHole (p: FormatPlaceholder) : bool =
        hasConcreteArgType p.Type
        && p.Width <> FormatDim.Star
        && p.Precision <> FormatDim.Star

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

    let private builderFamily (formatArgIndex: int) (leading: SemType list) : Family =
        {
            FormatArgIndex = formatArgIndex
            Tail = tyUnit
            State = tyStringBuilder
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

    /// Keyed by source short name. The `k*`-continuation family is out of scope —
    /// it carries an extra leading continuation argument.
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

    /// Rewrite the family's external leading-arg slots — the by-name
    /// `TyConst(System.IO.TextWriter)` (`fprintf`/`fprintfn`) or
    /// `TyConst(System.Text.StringBuilder)` (`bprintf`) placeholders (`State`, and
    /// the matching `LeadingArgTypes` entry) — to the provider-resolved `TyClass`
    /// the `resolve` callback returns for each slot's nominal name. The gate passes
    /// a `resolve` that yields the SAME `TypeKey` a real `Console.Out` /
    /// `StringBuilder()` argument carries, so a leading sink arg reconciles with the
    /// slot under plain `unify` (core `unify` compares `TyClass` by key equality, and
    /// never a `TyClass` against a `TyConst`). A slot whose name `resolve` can't map
    /// (`ValueNone`) keeps its by-name `TyConst` (no regression). Kept provider-free
    /// so `PrintfSpec` stays a pure SemType module: the gate owns the `ctx.Provider`
    /// resolution and hands in the callback.
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
            else
                t

        { fam with
            State = sub fam.State
            LeadingArgTypes = fam.LeadingArgTypes |> List.map sub
        }

    /// Whether a `%a`/`%t` callback hole can lower on this target — i.e. whether the
    /// family's `'State` sink type is nameable here. Read a `Family` that has already
    /// been through `resolveExternalSlots`: `sprintf` needs no external sink
    /// (`State = unit`), and the writer/builder families are available exactly when
    /// their by-name `TyConst` slot was rewritten to a provider-resolved `TyClass`. A
    /// slot that stayed a by-name `TyConst` (e.g. a JS provider with no `TextWriter`)
    /// is NOT available, so the gate rejects rather than mis-lower.
    let callbackSinkAvailable (fam: Family) : bool =
        match fam.State with
        | TyClass _ -> true
        | s -> s = tyUnit

    let formatType (printer: SemType) (fam: Family) : SemType =
        TyClass(RuntimeNames.printfFormatKey, EqArray.ofList [ printer; fam.State; fam.Residue; fam.Result ])

    /// Curry resolved argument types onto the family's tail.
    let printerType (argTypes: SemType list) (fam: Family) : SemType =
        List.foldBack (fun a r -> TyFun(a, r)) argTypes fam.Tail

    /// Total number of curried arguments the placeholders consume — the sum of
    /// per-hole `argTypes` lengths. The happy-path marker gate reads this
    /// (`args.Length = totalArity + 1`) instead of a hole count: a star hole
    /// consumes 2–3 args, so holes = args no longer holds. Only the arg COUNT is
    /// read here, and it's type-independent (2 for `%a`, 1 for `%t`, N for a star
    /// hole), so the constant `fresh` and the dummy `tyUnit` state/residue are
    /// safe — `List.length` never inspects the SemTypes.
    let totalArity (specs: FormatPlaceholder list) : int =
        specs
        |> List.sumBy (fun p ->
            match argTypes (fun () -> tyUnit) tyUnit tyUnit p with
            | ValueSome ts -> ts.Length
            | ValueNone -> 0
        )

    /// Shape: `leading… -> PrintfFormat<printer,…> -> printer`, plus the format
    /// type and printer type computed along the way. `ValueNone` only when a
    /// placeholder isn't typeable (no letter is left untypeable now that `argTypes`
    /// covers `%a`/`%t` from `fam.State`/`fam.Residue`) — the caller then defers to
    /// standard inference. Star dimensions concat their leading `int`s into the
    /// printer arrow via `argTypes`, so the printer curries width/precision before
    /// the value.
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
