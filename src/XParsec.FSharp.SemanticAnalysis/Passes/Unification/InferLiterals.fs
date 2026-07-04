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

    /// Pulled out of `inferConst` so the measured-literal arm can stamp this
    /// onto a TyVar's `Link` while the measure rides on `Units`.
    let literalCarrier (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> BuiltinTypes.tyBool
        | Token.CharLiteral -> BuiltinTypes.tyChar
        | Token.NumSByte
        | Token.NumSByteHex
        | Token.NumSByteOctal
        | Token.NumSByteBinary -> BuiltinTypes.tySByte
        | Token.NumByte
        | Token.NumByteHex
        | Token.NumByteOctal
        | Token.NumByteBinary -> BuiltinTypes.tyByte
        | Token.NumInt16
        | Token.NumInt16Hex
        | Token.NumInt16Octal
        | Token.NumInt16Binary -> BuiltinTypes.tyInt16
        | Token.NumUInt16
        | Token.NumUInt16Hex
        | Token.NumUInt16Octal
        | Token.NumUInt16Binary -> BuiltinTypes.tyUInt16
        | Token.NumInt32
        | Token.NumInt32Hex
        | Token.NumInt32Octal
        | Token.NumInt32Binary -> BuiltinTypes.tyInt
        | Token.NumUInt32
        | Token.NumUInt32Hex
        | Token.NumUInt32Octal
        | Token.NumUInt32Binary -> BuiltinTypes.tyUInt32
        | Token.NumInt64
        | Token.NumInt64Hex
        | Token.NumInt64Octal
        | Token.NumInt64Binary -> BuiltinTypes.tyInt64
        | Token.NumUInt64
        | Token.NumUInt64Hex
        | Token.NumUInt64Octal
        | Token.NumUInt64Binary -> BuiltinTypes.tyUInt64
        | Token.NumNativeInt
        | Token.NumNativeIntHex
        | Token.NumNativeIntOctal
        | Token.NumNativeIntBinary -> BuiltinTypes.tyNativeInt
        | Token.NumUNativeInt
        | Token.NumUNativeIntHex
        | Token.NumUNativeIntOctal
        | Token.NumUNativeIntBinary -> BuiltinTypes.tyUNativeInt
        | Token.NumIEEE32
        | Token.NumIEEE32Hex
        | Token.NumIEEE32Octal
        | Token.NumIEEE32Binary -> BuiltinTypes.tyFloat32
        | Token.NumIEEE64
        | Token.NumIEEE64Hex
        | Token.NumIEEE64Octal
        | Token.NumIEEE64Binary -> BuiltinTypes.tyFloat
        | Token.NumDecimal
        | Token.NumDecimalHex
        | Token.NumDecimalOctal
        | Token.NumDecimalBinary -> BuiltinTypes.tyDecimal
        | Token.NumBigIntegerQ
        | Token.NumBigIntegerR
        | Token.NumBigIntegerZ
        | Token.NumBigIntegerI
        | Token.NumBigIntegerN
        | Token.NumBigIntegerG -> BuiltinTypes.tyBigInt
        | _ -> TyUnknown(sprintf "non-literal token %A in literal position" t.Token)

    let inferConst (ctx: PassContext) (c: Constant<SyntaxToken>) : SemType =
        match c with
        | Constant.Literal t -> literalCarrier t
        | Constant.MeasuredLiteral(value = t; measure = m) ->
            let carrier = literalCarrier t
            let diagKey = NodeKey.ofToken t NodeKind.ExprConst
            let mt = translateMeasure ctx diagKey m
            let tv = freshTyVar ctx
            tv.Link <- ValueSome carrier
            tv.Units <- ValueSome mt
            TyVar tv

    /// Reads `Units` straight off the root — does NOT use `resolveStep`,
    /// which would follow a measured TyVar through its `Link` to the bare
    /// carrier and drop the measure.
    let unitsOf (t: SemType) : MeasureTerm voption =
        match t with
        | TyVar tv -> (UnionFind.find tv).Units
        | _ -> ValueNone

    /// A free variable (no Link) is returned as-is so a later unification can
    /// pin it.
    let carrierOf (t: SemType) : SemType =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome link -> link
            | ValueNone -> TyVar root
        | other -> other

    let freshTyVarWith (ctx: PassContext) (carrier: SemType) (units: MeasureTerm voption) : TypeVar =
        let tv = freshTyVar ctx
        tv.Link <- ValueSome carrier
        tv.Units <- units
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
        (key: NodeKey)
        (name: string)
        (leftTy: SemType)
        (rightTy: SemType)
        : SemType option =
        let leftUnits = unitsOf leftTy
        let rightUnits = unitsOf rightTy

        match leftUnits, rightUnits with
        | ValueNone, ValueNone -> None
        | _ ->
            let carrier = carrierOf leftTy
            // Carriers must agree even between measured operands (no
            // `float<m> + int<m>`). Surface that as a normal type mismatch.
            unify ctx key carrier (carrierOf rightTy)

            match name, leftUnits, rightUnits with
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 when m1.Equals m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Code = ""
                        Severity = Severity.Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m, ValueNone
            | ("op_Addition" | "op_Subtraction"), ValueNone, ValueSome m ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Code = ""
                        Severity = Severity.Error
                    }

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
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name && m1.Equals m2 -> Some BuiltinTypes.tyBool
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Code = ""
                        Severity = Severity.Error
                    }

                Some BuiltinTypes.tyBool
            | name, ValueSome m, ValueNone
            | name, ValueNone, ValueSome m when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Code = ""
                        Severity = Severity.Error
                    }

                Some BuiltinTypes.tyBool
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
