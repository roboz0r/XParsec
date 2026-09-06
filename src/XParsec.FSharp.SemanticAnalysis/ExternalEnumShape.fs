namespace XParsec.FSharp.SemanticAnalysis

/// Projection of an elaborated enum's case table to its published `ExternalTypeShape`.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExternalEnumShape =
    let private tryCase (c: TEnumCaseG<'tok>) : ExternalEnumCaseShape voption =
        match c.Value with
        | ValueSome(TEnumLiteral.Int v) ->
            let kind, bits = TEnumCases.integralValue v

            ValueSome
                {
                    Name = c.Name
                    Value = ExternalEnumCaseValue.IntVal(kind, bits)
                }
        | ValueSome(TEnumLiteral.String s) ->
            ValueSome
                {
                    Name = c.Name
                    Value = ExternalEnumCaseValue.StringVal s
                }
        | ValueNone -> ValueNone

    /// `Unmodelled` when any case lacks a constant value (the error is reported at
    /// elaboration) or the enum has no cases.
    let ofCases (cases: EqArray<TEnumCaseG<'tok>>) (origin: SymbolOrigin) : ExternalTypeShape =
        let shapes = ResizeArray<ExternalEnumCaseShape>(cases.Length)
        let mutable broken = ValueNone

        for c in cases do
            match tryCase c with
            | ValueSome shape -> shapes.Add shape
            | ValueNone ->
                if broken.IsNone then
                    broken <- ValueSome c.Name

        match broken, TEnumCases.underlyingTypeKey cases with
        | ValueSome name, _ ->
            ExternalTypeShape.Unmodelled(
                UnmodelledReason.ExtractionFailed(sprintf "enum case '%s' has no constant value" name),
                EqArray.empty
            )
        | ValueNone, ValueNone ->
            ExternalTypeShape.Unmodelled(UnmodelledReason.ExtractionFailed "enum has no cases", EqArray.empty)
        | ValueNone, ValueSome underlying ->
            ExternalTypeShape.Enum
                {
                    Cases = EqArray.ofResizeArray shapes
                    Underlying = underlying
                    Origin = origin
                }
