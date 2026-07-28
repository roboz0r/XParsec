namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module internal UnificationInferLiterals =

    /// Peel paren / annotation wrappers to a plain syntactic STRING constant's
    /// value (interpolation / non-literal → `ValueNone`). THE one const-string
    /// peeler behind call-site constant propagation (the printf-format precedent):
    /// the literal-slot admission (`InferApp`) and the keyof-bounded method-typar
    /// seams (`InferExternalCall`) both read syntax through here, so what counts
    /// as "a constant" cannot drift between them. Int falls under the same seam
    /// once literal-int slots are exercised; strings are the shape mitt / the
    /// acceptance test need, so int is deferred.
    let rec constStringArg (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        match e with
        | Expr.EnclosedBlock(expr = inner)
        | Expr.TypeAnnotation(expr = inner) -> constStringArg ctx inner
        | Expr.String(kind = StringKind.String _; parts = parts) when parts.Length = 1 ->
            match parts.[0] with
            | StringPart.Text t -> ValueSome(ctx.NameOf t)
            | _ -> ValueNone
        | _ -> ValueNone

    /// The type a literal token carries. Pulled out of `inferConst` so the measured-literal
    /// arm can stamp this onto a TyVar's `Link` while the measure rides on `Units`.
    ///
    /// A numeric token is read through its classified `NumericKind` rather than by
    /// enumerating the token cases: the radix axis (`10y` / `0x0Ay` / `0o12y` / `0b1010y`)
    /// is not part of a literal's TYPE, and `numericKind` has already collapsed it. An
    /// integral kind then names an `IntWidth`, whose type is `ctx.Intrinsics.OfIntWidth` —
    /// the same width→type projection freeze uses for the constant itself, so a literal's
    /// inferred type and its frozen `TConstValue`'s type are the same fact, asked once.
    let literalCarrier (ctx: PassContext) (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> ctx.Intrinsics.Bool
        | Token.CharLiteral -> ctx.Intrinsics.Char
        | tok ->

            let unknown () =
                TyUnknown(sprintf "non-literal token %A in literal position" tok)

            match NumericLiterals.numericKindOf tok with
            | ValueNone -> unknown ()
            | ValueSome kind ->

                match IntWidth.ofNumericKind kind with
                | ValueSome w -> ctx.Intrinsics.OfIntWidth w
                | ValueNone ->

                    match kind with
                    | NumericKind.IEEE32 -> ctx.Intrinsics.Float32
                    | NumericKind.IEEE64 -> ctx.Intrinsics.Float
                    | NumericKind.Decimal -> ctx.Intrinsics.Decimal
                    | NumericKind.BigIntegerQ
                    | NumericKind.BigIntegerR
                    | NumericKind.BigIntegerZ
                    | NumericKind.BigIntegerI
                    | NumericKind.BigIntegerN
                    | NumericKind.BigIntegerG -> ctx.Intrinsics.BigInt
                    // `ReservedNumericLiteral`, or a `NumericKind` outside the declared set (a 5-bit
                    // field, so F# cannot prove this exhaustive): a suffix F# gives no meaning to names
                    // no type.
                    | _ -> unknown ()

    let inferConst (ctx: PassContext) (c: Constant<SyntaxToken>) : SemType =
        match c with
        | Constant.Literal t -> literalCarrier ctx t
        | Constant.MeasuredLiteral(value = t; measure = m) ->
            let carrier = literalCarrier ctx t
            let mt = translateMeasure ctx t m
            let tv = freshTyVar ctx
            let root = UnionFind.find ctx.Store tv
            ctx.Store.SetLink(root, ValueSome carrier)
            ctx.Store.SetUnits(root, ValueSome mt)
            TyVar tv

    /// Reads `Units` straight off the root — does NOT use `resolveStep`,
    /// which would follow a measured TyVar through its `Link` to the bare
    /// carrier and drop the measure.
    let unitsOf (store: TypeStore) (t: SemType) : MeasureTerm voption =
        match t with
        | TyVar tv -> store.Units(UnionFind.find store tv)
        | _ -> ValueNone

    /// A free variable (no Link) is returned as-is so a later unification can
    /// pin it.
    let carrierOf (store: TypeStore) (t: SemType) : SemType =
        match resolveStep store t with
        | TyVar tv ->
            let root = UnionFind.find store tv

            match store.Link root with
            | ValueSome link -> link
            | ValueNone -> TyVar root.Id
        | other -> other

    let freshTyVarWith (ctx: PassContext) (carrier: SemType) (units: MeasureTerm voption) : TyVarId =
        let tv = freshTyVar ctx
        let root = UnionFind.find ctx.Store tv
        ctx.Store.SetLink(root, ValueSome carrier)
        ctx.Store.SetUnits(root, units)
        tv

    let isComparisonOp (name: string) : bool =
        match name with
        | "op_Equality"
        | "op_Inequality"
        | "op_LessThan"
        | "op_GreaterThan"
        | "op_LessThanOrEqual"
        | "op_GreaterThanOrEqual" -> true
        | _ -> false

    /// Fires before the provider lookup in `inferInfix` so measured
    /// arithmetic / comparison operators get measure-correct result types and
    /// a dedicated "Measure mismatch" diagnostic rather than a generic
    /// carrier-type mismatch. Returns `None` for the all-dimensionless case
    /// (or operators we don't dispatch); the caller falls through to the
    /// provider path.
    let tryMeasuredArith
        (ctx: PassContext)
        (tok: SyntaxToken)
        (name: string)
        (leftTy: SemType)
        (rightTy: SemType)
        : SemType option =
        let leftUnits = unitsOf ctx.Store leftTy
        let rightUnits = unitsOf ctx.Store rightTy

        match leftUnits, rightUnits with
        | ValueNone, ValueNone -> None
        | _ ->
            let carrier = carrierOf ctx.Store leftTy
            // Carriers must agree even between measured operands (no
            // `float<m> + int<m>`). Surface that as a normal type mismatch.
            unify ctx tok carrier (carrierOf ctx.Store rightTy)

            match name, leftUnits, rightUnits with
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 when m1.Equals m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 ->
                ctx.Error(tok, sprintf "Measure mismatch: <%O> vs <%O>" m1 m2)

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m, ValueNone
            | ("op_Addition" | "op_Subtraction"), ValueNone, ValueSome m ->
                ctx.Error(tok, sprintf "Measure mismatch: dimensionless vs <%O>" m)

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Multiply", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.mul m1 m2))))
            | "op_Multiply", ValueSome m, ValueNone
            | "op_Multiply", ValueNone, ValueSome m -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.div m1 m2))))
            | "op_Division", ValueSome m, ValueNone -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueNone, ValueSome m ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.inv m))))
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name && m1.Equals m2 -> Some ctx.Intrinsics.Bool
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name ->
                ctx.Error(tok, sprintf "Measure mismatch: <%O> vs <%O>" m1 m2)

                Some ctx.Intrinsics.Bool
            | name, ValueSome m, ValueNone
            | name, ValueNone, ValueSome m when isComparisonOp name ->
                ctx.Error(tok, sprintf "Measure mismatch: dimensionless vs <%O>" m)

                Some ctx.Intrinsics.Bool
            | _ -> None

    /// Reuses the lexer's canonical placeholder parser
    /// (`Lexing.parseFormatSpecifierView`) so no second copy of the format
    /// grammar lives here. `ValueNone` when the string carries interpolation
    /// holes or lexer-error parts (not a simple format literal), so the
    /// printf special-case falls through to standard inference.
    let formatSpecifiers (ctx: PassContext) (e: Expr<SyntaxToken>) : FormatPlaceholder list voption =
        match e with
        | Expr.String(parts = parts) ->
            let acc = ResizeArray<FormatPlaceholder>()
            let mutable ok = true

            for part in parts do
                match part with
                | StringPart.Text _
                | StringPart.EscapeSequence _
                | StringPart.EscapePercent _
                | StringPart.VerbatimEscapeQuote _ -> ()
                | StringPart.FormatSpecifier t ->
                    match Lexing.parseFormatSpecifierView (ctx.ReadableOf t) with
                    | ValueSome placeholder -> acc.Add placeholder
                    | ValueNone -> ok <- false
                | StringPart.Expr _
                | StringPart.OrphanFormatSpecifier _
                | StringPart.InvalidText _ -> ok <- false

            if ok then ValueSome(List.ofSeq acc) else ValueNone
        | _ -> ValueNone

    /// Whether every specifier is one the happy path lowers inline
    /// (`PrintfHoleForm.tryClassify`); a `false` keeps the FSharp.Core cold
    /// path. Folds over `formatSpecifiers`' already-parsed placeholders so the
    /// part-walk and its rejections (interpolation holes, orphan specifiers,
    /// lexer-error parts → `ValueNone`) happen once and can't drift from the
    /// typing walk. `%%` escapes arrive as raw `Text` and never reach here.
    let lowerablePlaceholders (placeholders: FormatPlaceholder list) : bool =
        placeholders |> List.forall (fun p -> (PrintfHoleForm.tryClassify p).IsSome)

    /// E1(a): type a *format-string literal* that sits at a position whose EXPECTED
    /// type is already a `PrintfFormat<Printer,State,Residue,Result>` family — a
    /// format-typed `let` annotation (`let fmt : StringFormat<_> = "%d"`) or an
    /// ascription (`("%d" : Fmt)`). Real F# accepts these (an *unannotated*
    /// `let fmt = "%d"` is plain `string` and does NOT flow the format type back — so
    /// only the annotated/ascribed forms reach here); our compiler otherwise rejects
    /// them (`string` vs `PrintfFormat` mismatch).
    ///
    /// Parses the specifiers and computes the *printer* type from them + the
    /// annotation's own `State`/`Residue`/`Result` slots (`PrintfSpec.printerFromSlots`
    /// — the same `argTypes`/`printerType` machinery the printf gate uses), then
    /// unifies it against the expected `Printer` slot (`args.[0]`) — pinning a
    /// `StringFormat<_>` wildcard printer from the specifiers, and diagnosing a printer
    /// that disagrees with an explicit annotation (`StringFormat<int->string>` vs a
    /// `%s` body). Unifying only the printer slot (not a whole synthesised format type)
    /// sidesteps the two `PrintfFormat` faces: the annotation resolves to
    /// `Vesper.Printf.PrintfFormat`, the gate synthesises `FSharp.Core`'s. On success
    /// returns `expected` verbatim, so the caller stamps the annotation's OWN resolved
    /// format type onto the literal node — the type a downstream `sprintf fmt` (typed
    /// by the provider) unifies against. `ValueNone` (not a format literal, or the
    /// expected type isn't a `PrintfFormat`) falls through to the caller's ordinary
    /// `unify`, so this is strictly additive.
    let tryTypeFormatLiteral
        (ctx: PassContext)
        (tok: SyntaxToken)
        (litExpr: Expr<SyntaxToken>)
        (expected: SemType)
        : SemType voption =
        match resolveStep ctx.Store expected with
        | TyClass(fmtKey, args) when RuntimeNames.isPrintfFormatKey fmtKey && args.Length = 4 ->
            match formatSpecifiers ctx litExpr with
            | ValueSome specs ->
                let fresh () = TyVar(freshTyVar ctx)

                match PrintfSpec.printerFromSlots fresh specs args.[1] args.[2] args.[3] with
                | ValueSome printer ->
                    unify ctx tok printer args.[0]
                    ValueSome expected
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone
