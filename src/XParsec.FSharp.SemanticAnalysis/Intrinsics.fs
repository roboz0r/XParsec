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
    let cache = Dictionary<string, SemType>(System.StringComparer.Ordinal)

    let get (name: string) : SemType =
        match cache.TryGetValue name with
        | true, t -> t
        | _ ->
            match tryResolve name with
            | Some t ->
                cache.[name] <- t
                t
            | None ->
                failwithf
                    "intrinsic '%s' is not resolvable from the prim-types contract in scope (no local binding, no ambient-open provider entry)"
                    name

    member _.Int = get "int"
    member _.Int64 = get "int64"
    member _.Byte = get "byte"
    member _.SByte = get "sbyte"
    member _.Int16 = get "int16"
    member _.UInt16 = get "uint16"
    member _.UInt32 = get "uint32"
    member _.UInt64 = get "uint64"
    member _.NativeInt = get "nativeint"
    member _.UNativeInt = get "unativeint"

    member _.OfIntWidth(w: IntWidth) : SemType = get (IntWidth.name w)

    member _.Float = get "float"
    member _.Float32 = get "float32"
    member _.Bool = get "bool"
    member _.Char = get "char"
    member _.Decimal = get "decimal"
    member _.Unit = get "unit"
    member _.String = get "string"
    /// From the `prim-types-bigint` contract (CLR `System.Numerics.BigInteger`, JS `bigint`).
    /// The type a `NumBigInteger*` literal token pins to.
    member _.BigInt = get "bigint"
    /// The JS-only absence sentinel, from `prim-types-undefined.js.fsi` (no CLR analog).
    member _.Undefined = get "undefined"
