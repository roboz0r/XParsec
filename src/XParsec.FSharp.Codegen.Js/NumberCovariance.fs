namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// Resolves the TS `number` token by the variance of its position: a JS `number` is a `float`
/// but stands for the whole `int|float|float32|…` family sharing that platform type. Read →
/// `float`; written → the token, for the arg seam to widen; invariant (read AND written) →
/// the union.
module NumberCovariance =

    let private NumberToken = PlatformTypeId "number"

    /// How `float` is SPELLED in the assertion below, taken off its own identity so the message
    /// cannot refer to a type other than the one that was checked.
    let private floatCanonName = RuntimeNames.floatKey.Name

    let wrap (inner: IExternalSymbolProvider) : IExternalSymbolProvider =
        match IntrinsicTypeMap.tryPlatform RuntimeNames.floatKey inner.IntrinsicTypeMap with
        | ValueSome(IntrinsicPlatform.Bound id) when id = NumberToken -> ()
        | other ->
            failwithf
                "NumberCovariance: `%s` must bind to `%s` for the covariant `%s → %s` identity, but its binding is %A"
                floatCanonName
                NumberToken.Value
                NumberToken.Value
                floatCanonName
                other

        // The `number` token is a manifest-owned platform type id, never a Vesper identity:
        // matching by KEY means a Vesper type also named `number` cannot be mistaken for it.
        let numberKey = RuntimeNames.platformKey NumberToken

        let familyUnion =
            match IntrinsicTypeMap.canonsOf NumberToken inner.IntrinsicTypeMap with
            | EqEmpty -> FTConst(numberKey, EqArray.empty)
            | canons -> FrozenType.MkUnion(seq { for c in canons -> FTConst(c, EqArray.empty) })

        let resolveNumber (v: Variance) (t: FrozenType) : FrozenType voption =
            match t with
            | FTConst(key, args) when key = numberKey && args.Length = 0 ->
                match v with
                | Variance.Co -> ValueSome(FTConst(RuntimeNames.floatKey, EqArray.empty))
                | Variance.Inv -> ValueSome familyUnion
                | Variance.Contra -> ValueSome t
            | _ -> ValueNone

        inner
        |> ExternalSymbolProviders.mapProviderTypes (FrozenType.mapVariant resolveNumber)
