namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module internal UnificationInferLiterals =

    /// A syntactic string constant's value, seen through paren / annotation wrappers,
    /// the shared notion of "a constant" behind call-site constant propagation.
    let rec constStringArg (ctx: PassContext) (e: Expr<SyntaxToken>) : string voption =
        match e with
        | Expr.EnclosedBlock(expr = inner)
        | Expr.TypeAnnotation(expr = inner) -> constStringArg ctx inner
        | Expr.String(kind = StringKind.String _; parts = parts) when parts.Length = 1 ->
            match parts.[0] with
            | StringPart.Text t -> ValueSome(ctx.NameOf t)
            | _ -> ValueNone
        | _ -> ValueNone

    /// The type a literal token carries. A numeric token is read through its classified
    /// `NumericKind` rather than by token case, because radix is not part of a literal's
    /// type: `10y` / `0x0Ay` / `0o12y` / `0b1010y` all carry `sbyte`.
    let literalCarrier (ctx: PassContext) (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> ctx.Intrinsics.Bool
        | Token.CharLiteral -> ctx.Intrinsics.Char
        | tok ->

            // No constant to carry, so no type either: a non-numeric token here, or below a
            // suffix F# reserves. Elaborating the same token throws rather than reporting.
            let unknown () = TyUnknown UnknownReason.NoValueType

            match NumericLiterals.numericKindOf tok with
            | ValueNone -> unknown ()
            | ValueSome kind ->

                match IntKind.ofNumericKind kind with
                | ValueSome w -> ctx.Intrinsics.OfIntKind w
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
                    // `NumericKind` is an enum, so the wildcard is required: a reserved or
                    // otherwise meaningless suffix does not map to a type.
                    | _ -> unknown ()

    let inferConst (ctx: PassContext) (c: Constant<SyntaxToken>) : SemType =
        match c with
        | Constant.Literal t -> literalCarrier ctx t
        | Constant.MeasuredLiteral(value = t; measure = m) ->
            ctx.MeasuredTy(literalCarrier ctx t, translateMeasure ctx t m)

    /// Reads `Units` straight off the root, never through `resolveStep`,
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

    let isComparisonOp (name: string) : bool =
        match name with
        | OperatorData.OpEquality
        | OperatorData.OpInequality
        | OperatorData.OpLessThan
        | OperatorData.OpGreaterThan
        | OperatorData.OpLessThanOrEqual
        | OperatorData.OpGreaterThanOrEqual -> true
        | _ -> false

    /// Measure-correct result types and a "Measure mismatch" diagnostic for arithmetic
    /// and comparison on measured operands. `None` when both operands are dimensionless
    /// (or the operator is not one of these), leaving the caller's ordinary path.
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
            | (OperatorData.OpAddition | OperatorData.OpSubtraction), ValueSome m1, ValueSome m2 when m1.Equals m2 ->
                Some(ctx.MeasuredTy(carrier, m1))
            | (OperatorData.OpAddition | OperatorData.OpSubtraction), ValueSome m1, ValueSome m2 ->
                ctx.Report(tok, Kind.MeasureMismatch(string m1, string m2))

                Some(ctx.MeasuredTy(carrier, m1))
            | (OperatorData.OpAddition | OperatorData.OpSubtraction), ValueSome m, ValueNone
            | (OperatorData.OpAddition | OperatorData.OpSubtraction), ValueNone, ValueSome m ->
                ctx.Report(tok, Kind.DimensionlessMeasureMismatch(string m))

                Some(ctx.MeasuredTy(carrier, m))
            | OperatorData.OpMultiply, ValueSome m1, ValueSome m2 ->
                Some(ctx.MeasuredTy(carrier, MeasureTerm.mul m1 m2))
            | OperatorData.OpMultiply, ValueSome m, ValueNone
            | OperatorData.OpMultiply, ValueNone, ValueSome m -> Some(ctx.MeasuredTy(carrier, m))
            | OperatorData.OpDivision, ValueSome m1, ValueSome m2 ->
                Some(ctx.MeasuredTy(carrier, MeasureTerm.div m1 m2))
            | OperatorData.OpDivision, ValueSome m, ValueNone -> Some(ctx.MeasuredTy(carrier, m))
            | OperatorData.OpDivision, ValueNone, ValueSome m -> Some(ctx.MeasuredTy(carrier, MeasureTerm.inv m))
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name && m1.Equals m2 -> Some ctx.Intrinsics.Bool
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name ->
                ctx.Report(tok, Kind.MeasureMismatch(string m1, string m2))

                Some ctx.Intrinsics.Bool
            | name, ValueSome m, ValueNone
            | name, ValueNone, ValueSome m when isComparisonOp name ->
                ctx.Report(tok, Kind.DimensionlessMeasureMismatch(string m))

                Some ctx.Intrinsics.Bool
            | _ -> None

    /// `ValueNone` when the string is not a simple format literal (interpolation holes,
    /// orphan specifiers or lexer-error parts), so the printf special-case falls through
    /// to standard inference.
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

    /// The metavar a printf placeholder's value argument types as. A family hole carries the
    /// types its specifier accepts as a `OneOf` constraint, plus the one it settles on where
    /// nothing else pins it: `%d` takes any integer, and `printfn "%d"` alone is `int -> unit`.
    let freshHoleTy (ctx: PassContext) (declKey: NodeKey) (h: PrintfSpec.FormatHoleTy) : SemType =
        let tv = ctx.FreshTyVar()
        let root = UnionFind.find ctx.Store tv

        let family (keys: EqArray<TypeKey>) =
            ctx.Store.Constraints.Append(
                root,
                {
                    Kind = SemanticConstraintKind.OneOf keys
                    DeclKey = declKey
                }
            )

            ctx.Store.Defaults.Append(root, TyConst(PrintfSpec.familyDefault keys, EqArray.empty))
            ctx.FormatHoles.Add tv

        match PrintfSpec.familyKeys h with
        | ValueNone -> ()
        | ValueSome keys -> family keys

        TyVar tv

    /// Whether every specifier is one the inline lowering handles; a `false` keeps the
    /// FSharp.Core cold path. A `%%` escape is its own string part, never a placeholder.
    let lowerablePlaceholders (placeholders: FormatPlaceholder list) : bool =
        placeholders
        |> List.forall (fun p ->
            match PrintfHoleForm.classify p with
            | PrintfHoleForm.HoleVerdict.Lowerable _ -> true
            | PrintfHoleForm.HoleVerdict.Residual
            | PrintfHoleForm.HoleVerdict.SignLeftAlignZeroPad
            | PrintfHoleForm.HoleVerdict.OversizedDimension -> false
        )

    /// Types a format-string literal whose EXPECTED type is already a
    /// `PrintfFormat<Printer,State,Residue,Result>` (`let fmt : StringFormat<_> = "%d"`).
    /// Only the `Printer` slot is unified; `State` / `Residue` / `Result` come from the
    /// annotation and drive the printer's derivation from the specifiers.
    let tryTypeFormatLiteral
        (ctx: PassContext)
        (tok: SyntaxToken)
        (litExpr: Expr<SyntaxToken>)
        (expected: SemType)
        : SemType voption =
        match resolveStep ctx.Store expected with
        | TyClass(fmtKey, args) when fmtKey = RuntimeNames.printfFormatKey && args.Length = 4 ->
            match formatSpecifiers ctx litExpr with
            | ValueSome specs ->
                let mint = freshHoleTy ctx (CstKeys.ofExpr litExpr)

                let printer = PrintfSpec.printerFromSlots mint specs args.[1] args.[2] args.[3]
                unify ctx tok printer args.[0]
                ValueSome expected
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// The list type a `[…]` literal or a cons/list PATTERN carries. A program declaring its
    /// own `'T list` abbreviation resolves eagerly to that RHS; a bare program leaves the
    /// container flexible for a later consumer to pin (`RegisterListLiteral`).
    let listLiteralTy (ctx: PassContext) (tok: SyntaxToken) (elemTy: SemType) : SemType =
        match TypeRegistry.tryAbbrevSpelling ctx.Types UseSite.unbounded RuntimeNames.vesperListAbbrevKey with
        | ValueSome info -> expandAbbreviation ctx tok info (forceFill ctx info) (EqArray.singleton elemTy)
        | ValueNone ->
            let tv = ctx.FreshTyVar()

            ctx.RegisterListLiteral((UnionFind.find ctx.Store tv).Id, elemTy, tok)
            TyVar tv
