namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer

module internal IntrinsicResolve =

    let private intrinsicCanon (shape: ExternalTypeShape) : TypeKey voption =
        match shape with
        | ExternalTypeShape.Intrinsic { Id = { Canon = c } } -> ValueSome c
        | _ -> ValueNone

    let tryResolveIntrinsicKey
        (provider: IExternalSymbolResolver)
        (intrinsicKeys: Dictionary<string, TypeKey>)
        (name: string)
        : TypeKey option =
        match intrinsicKeys.TryGetValue name with
        | true, k -> Some k
        | _ ->
            match ExternalSymbols.tryPickRuntimeType provider intrinsicCanon name with
            | ValueSome c -> Some c
            | ValueNone -> None

    let tryResolveIntrinsicType
        (provider: IExternalSymbolResolver)
        (intrinsicKeys: Dictionary<string, TypeKey>)
        (name: string)
        : SemType option =
        tryResolveIntrinsicKey provider intrinsicKeys name
        |> Option.map (fun k -> TyConst(k, EqArray.empty))

/// `int`/`string`/`bool`/… resolved from the `prim-types-*` contract. Each member resolves
/// on first access: a self-host file's own intrinsics are registered only after this is built.
type IntrinsicSet(tryResolve: string -> SemType option) =
    let cache = Dictionary<TypeKey, SemType>()

    // A TARGET-OPTIONAL identity (`RuntimeNames.isTargetOptionalPrimitiveKey`) whose
    // contract is absent mints its canon key anyway: the omission is the target not
    // supporting it, and `PlatformTypes` reports each mention as `UnsupportedOnTarget`.
    // Only the `prim-types-min` trio must resolve, and a miss there fails loudly.
    let get (canon: TypeKey) : SemType =
        match cache.TryGetValue canon with
        | true, t -> t
        | _ ->
            let name = canon.Name

            match tryResolve name with
            | Some t ->
                cache.[canon] <- t
                t
            | None when RuntimeNames.isTargetOptionalPrimitiveKey canon ->
                let t = TyConst(canon, EqArray.empty)
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
