namespace XParsec.FSharp.SemanticAnalysis

open Vesper

/// Projection of an enum's case table, the registry's or the frozen tree's, to its published
/// `ExternalTypeShape`.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExternalEnumShape =
    let private tryCase (c: IEnumCase<'tok>) : ExternalEnumCaseShape voption =
        match c.Value with
        | ValueSome(TEnumLiteral.Int v) ->
            ValueSome
                {
                    Name = c.Name
                    Value = ExternalEnumCaseValue.IntVal(TEnumCases.integralValue v)
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
    let ofCases (cases: Block<#IEnumCase<'tok>>) (origin: SymbolOrigin) : ExternalTypeShape =
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
                TyparList.empty
            )
        | ValueNone, ValueNone ->
            ExternalTypeShape.Unmodelled(UnmodelledReason.ExtractionFailed "enum has no cases", TyparList.empty)
        | ValueNone, ValueSome underlying ->
            ExternalTypeShape.Enum
                {
                    Cases = Block.ofResizeArray shapes
                    Underlying = underlying
                    Origin = origin
                }
