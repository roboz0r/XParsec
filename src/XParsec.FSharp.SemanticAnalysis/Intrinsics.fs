namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic
open XParsec.FSharp.Lexer

// The primitive-intrinsic identity surface: how a pass resolves `int`/`string`/…
// to their CONTRACT-sourced identities (never authored from a hardcoded name
// set), and the lazily-resolved bag a `PassContext` exposes as `ctx.Intrinsics`.
// Lives in its own compile unit between `ExternalSymbols` (the provider shapes it
// reads) and `PassContext` (which instantiates it) — neither an external-symbol
// concern nor a side table.

/// Try to resolve ONE intrinsic name to its identity the way a written `int` annotation resolves:
/// this unit's own registered intrinsics first (`intrinsicKeys` — the
/// `PassContextTypes.IntrinsicKeys` index, self-host), else the provider through
/// `ExternalSymbols.tryPickRuntimeType` (bare name, then each `AmbientOpenPrefixes` entry,
/// scanning PAST a non-intrinsic hit — a composited FSharp.Core-lib `int` abbreviation must not
/// shadow the Vesper intrinsic). The resolved key is read OFF the matched shape
/// (every intrinsic shape carries the authoritative canon), never re-minted from the
/// ambient prefix. `None` when neither names it — the honest answer, so the miss policy stays with
/// the one caller (`IntrinsicSet.get`) rather than a silent by-name mint here.
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

/// The primitive-intrinsic identity bag, the `SemType` analogue of `ctx.CapabilityIds`:
/// `int`/`string`/`bool`/… resolved ONCE from the `prim-types-*` contract (never authored),
/// so the front end carries no static intrinsic `SemType`s. Each field resolves lazily on first
/// access and caches — laziness matters because a self-host unit's own intrinsics
/// (`IntrinsicKeys`) are only populated by the NameResolution pre-pass AFTER the `PassContext`
/// is built, and because a test that never types an `int` never forces its resolution (so a
/// minimal fake provider need only satisfy the intrinsics its test actually exercises).
/// `tryResolve` is HONEST — `None` means "the prim-types contract in scope does not name this
/// intrinsic"; there is no silent by-name fallback that would keep the hardcoded shadow set alive
/// or mask a genuine contract gap. The miss POLICY lives here in one place: `get` raises a loud,
/// named error. A contract that lacks a primitive the compiler needs (`int`) is a build-config
/// fault, not a user error, so failing loudly is correct. An intrinsic with no contract yet has
/// NO member here; a member is added the moment its contract lands (as `undefined` and `bigint`
/// both were), so this surface never offers a guaranteed loud-fail.
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

    /// The type of an integral WIDTH. Freeze types an integral constant through this, so a
    /// constant's width and the type it freezes at cannot disagree: both are
    /// `IntWidth.name`, which is also the name the elaborator gives an enum's underlying
    /// type and the name the CLR backend loads its cases at.
    member _.OfIntWidth(w: IntWidth) : SemType = get (IntWidth.name w)

    member _.Float = get "float"
    member _.Float32 = get "float32"
    member _.Bool = get "bool"
    member _.Char = get "char"
    member _.Decimal = get "decimal"
    member _.Unit = get "unit"
    member _.String = get "string"
    /// The arbitrary-precision integer `bigint`, resolved from `prim-types-bigint`
    /// (CLR `System.Numerics.BigInteger`, JS `bigint`). The type a `NumBigInteger*`
    /// literal token pins to.
    member _.BigInt = get "bigint"
    /// The JS-only absence sentinel `undefined`, resolved from `prim-types-undefined.js`
    /// (a `files-js` contract with no CLR analog). Forcing this on a stack that has not
    /// loaded the JS contract is a loud fail BY DESIGN — every consumer that reaches for it
    /// (the omitted-optional fill) is JS-only, so the contract is always in scope there.
    member _.Undefined = get "undefined"
