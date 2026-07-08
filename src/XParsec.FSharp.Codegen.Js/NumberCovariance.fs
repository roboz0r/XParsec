namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// Resolves the TS `number` token against the Vesper numeric types by the variance of
/// the position it occupies. A JS `number` value IS a `float`, but it is WIDER than any
/// one Vesper numeric — it stands for the whole `int|float|float32|…` family that shares
/// the JS `number` repr. So a `number` read (covariant) is `float`; a `number` written
/// (contravariant parameter) stays the token, left for the arg seam to widen to the
/// family; a `number` in an invariant generic slot — read AND written — is the family
/// union. `float` as the covariant target is asserted, not assumed: `float` must itself
/// repr to `number`, else this fails loud.
module NumberCovariance =

    [<Literal>]
    let private NumberToken = "number"

    [<Literal>]
    let private FloatCanon = "float"

    let wrap (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        match inner.IntrinsicForwardRepr.TryGetValue(RuntimeNames.floatKey) with
        | true, r when r = NumberToken -> ()
        | other ->
            failwithf
                "NumberCovariance: `%s` must repr to `%s` for the covariant `%s → %s` identity, but its repr is %A"
                FloatCanon
                NumberToken
                NumberToken
                FloatCanon
                other

        let familyUnion =
            match Map.tryFind NumberToken inner.IntrinsicReverseCanon with
            | Some(_ :: _ as canons) -> FrozenType.MkUnion(seq { for c in canons -> FTConst(c, EqArray.empty) })
            | _ -> FTConst(RuntimeNames.opaqueKey NumberToken, EqArray.empty)

        let resolveNumber (v: Variance) (t: FrozenType) : FrozenType voption =
            match t with
            | FTConst(key, args) when SymbolKeyOps.simpleName key = NumberToken && args.Length = 0 ->
                match v with
                | Variance.Co -> ValueSome(FTConst(RuntimeNames.floatKey, EqArray.empty))
                | Variance.Inv -> ValueSome familyUnion
                | Variance.Contra -> ValueSome t
            | _ -> ValueNone

        inner |> ExternalSymbols.mapProviderTypes (FrozenType.mapVariant resolveNumber)
