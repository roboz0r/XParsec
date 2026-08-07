namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer

module internal IntrinsicResolve =

    let private intrinsicCanon (shape: ExternalTypeShape) : SymbolKey voption =
        match shape with
        | ExternalTypeShape.Intrinsic { Id = { Canon = c } } -> ValueSome(SymbolKey.Type c)
        | _ -> ValueNone

    let tryResolveIntrinsicKey
        (provider: IExternalSymbolResolver)
        (intrinsicKeys: Dictionary<string, SymbolKey>)
        (name: string)
        : SymbolKey option =
        match intrinsicKeys.TryGetValue name with
        | true, k -> Some k
        | _ ->
            match ExternalSymbols.tryPickRuntimeType provider intrinsicCanon name with
            | ValueSome c -> Some c
            | ValueNone -> None

    let tryResolveIntrinsicType
        (provider: IExternalSymbolResolver)
        (intrinsicKeys: Dictionary<string, SymbolKey>)
        (name: string)
        : SemType option =
        tryResolveIntrinsicKey provider intrinsicKeys name
        |> Option.map (fun k -> TyConst(k, EqArray.empty))

/// `int`/`string`/`bool`/… resolved from the `prim-types-*` contract. Each member resolves
/// on first access: a self-host file's own intrinsics are registered only after this is built.
type IntrinsicSet(tryResolve: string -> SemType option) =
    let cache = Dictionary<SymbolKey, SemType>()

    let get (canon: SymbolKey) : SemType =
        match cache.TryGetValue canon with
        | true, t -> t
        | _ ->
            let name = SymbolKeyOps.intrinsicName canon

            match tryResolve name with
            | Some t ->
                cache.[canon] <- t
                t
            | None ->
                failwithf
                    "intrinsic '%s' is not resolvable from the prim-types contract in scope (no local binding, no ambient-open provider entry)"
                    name

    member _.Int = get RuntimeNames.intKey
    member _.Int64 = get RuntimeNames.int64Key
    member _.Byte = get RuntimeNames.byteKey
    member _.SByte = get RuntimeNames.sbyteKey
    member _.Int16 = get RuntimeNames.int16Key
    member _.UInt16 = get RuntimeNames.uint16Key
    member _.UInt32 = get RuntimeNames.uint32Key
    member _.UInt64 = get RuntimeNames.uint64Key
    member _.NativeInt = get RuntimeNames.nativeintKey
    member _.UNativeInt = get RuntimeNames.unativeintKey

    member _.OfIntWidth(w: IntWidth) : SemType = get (RuntimeNames.intWidthKey w)

    member _.Float = get RuntimeNames.floatKey
    member _.Float32 = get RuntimeNames.float32Key
    member _.Bool = get RuntimeNames.boolKey
    member _.Char = get RuntimeNames.charKey
    member _.Decimal = get RuntimeNames.decimalKey
    member _.Unit = get RuntimeNames.unitKey
    member _.String = get RuntimeNames.stringKey
    /// From the `prim-types-bigint` contract (CLR `System.Numerics.BigInteger`, JS `bigint`).
    /// The type a `NumBigInteger*` literal token pins to.
    member _.BigInt = get RuntimeNames.bigintKey
    /// The JS-only absence sentinel, from `prim-types-undefined.js.fsi` (no CLR analog).
    member _.Undefined = get RuntimeNames.undefinedKey
