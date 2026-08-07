module XParsec.FSharp.Codegen.Clr.Tests.IntrinsicReprConformanceTests

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

// Repr-encodability conformance.
//
// A Vesper primitive's CLR representation lives in ONE place, the `.fs` `(# … #)`
// declaration (`type int = (# "System.Int32" #)`), extracted into the contract
// provider's forward `{ canon → platform-repr }` map. The IL encoder
// (`ClrEncoder`'s scalar arm) turns that repr string into a value-type token via
// `IntrinsicRepr.tryEncodeValueType`. Nothing structurally forces the two to agree:
// a `.fs` could declare a scalar primitive whose repr the encoder doesn't know, and
// today that surfaces only as a `failwithf "no IL encoding for intrinsic
// representation …"` at the FIRST use site that encodes it — easy to miss.
//
// This pins the invariant up front: every DIRECTLY-ENCODABLE scalar primitive the
// CLR contract extracts must be encodable by `IntrinsicRepr.isEncodableValueType`
// (the pure, encoder-free predicate single-sourced with `tryEncodeValueType`). The
// two TypeRef-backed scalars (`decimal` → `System.Decimal`, `unit` →
// `System.ValueTuple`) are asserted as the explicit exceptions the encoder handles
// through their own arms. Reads the reprs FROM the contract, so a `.fs` repr edit
// (a typo, a new primitive) that the encoder can't follow fails HERE.
//
// SCOPE: the directly-encodable scalar value types only — the closed set
// `tryEncodeValueType` exists for. `nativeint` / `unativeint` ARE in it: their reprs
// (`native int` / `unsigned native int`) are the ECMA-335 element types
// `ELEMENT_TYPE_I` / `_U`, which the encoder writes directly. The reference
// (`obj`/`exn`/capability), the remaining pointer (`voidptr`/`nativeptr`/`ilsigptr`),
// and structural (`'T[]`) intrinsics encode through other (nominal / SZArray) arms,
// not this one, and are out of scope.

/// The contract's extracted forward `{ canon → platform-repr }` map for the default
/// CLR stack — the single source of each primitive's IL representation.
let private forwardRepr =
    (ClrSymbolProviders.buildContract [ TestHelpers.vesperCoreManifest ]).IntrinsicForwardRepr

/// Look a bare canon name up in the forward axis, which is now keyed by the qualified
/// intrinsic `SymbolKey` (`Vesper.int`) — bridge from the bare `.fsi` name.
let private tryRepr (canon: string) : string option =
    match forwardRepr.TryGetValue(RuntimeNames.primitiveKey canon) with
    | true, repr -> Some repr
    | _ -> None

/// The directly-encodable scalar value-type primitives, by their `.fsi` canon name.
/// Each must extract a repr `tryEncodeValueType` writes. (Aliases like `int32`/`uint`
/// are abbreviations that dealias to these, not intrinsics — they carry no own repr.)
let private directScalarCanons =
    [
        "int"
        "uint32"
        "int64"
        "uint64"
        "sbyte"
        "byte"
        "int16"
        "uint16"
        "float"
        "float32"
        "bool"
        "char"
        "string"
        // The pointer-width pair. Unlike every other entry their repr is an IL
        // signature spelling, not a BCL name — `native int` / `unsigned native int` are
        // element types in their own right, so they encode directly (`IntPtr()` /
        // `UIntPtr()`) rather than through a `TypeRef` to `System.IntPtr`.
        "nativeint"
        "unativeint"
    ]

[<Tests>]
let tests =
    testList
        "IntrinsicReprConformance"
        [
            test "every directly-encodable scalar primitive's extracted repr is encodable" {
                for canon in directScalarCanons do
                    match tryRepr canon with
                    | Some repr ->
                        Expect.isTrue
                            (IntrinsicRepr.isEncodableValueType repr)
                            (sprintf
                                "scalar primitive '%s' extracts repr '%s', which the IL encoder cannot encode"
                                canon
                                repr)
                    | None ->
                        failtestf "scalar primitive '%s' has no extracted CLR repr in the contract forward map" canon
            }

            test "the TypeRef-backed scalars are extracted but NOT direct value types" {
                // `decimal` / `unit` are the two scalars the encoder writes through a
                // dedicated `TypeRef` arm (`eDecimal` / `eValueTuple`), so they are
                // deliberately absent from `isEncodableValueType`. Pin the reprs (so a
                // `.fs` change surfaces) and the not-a-direct-value-type fact.
                Expect.equal (tryRepr "decimal") (Some "System.Decimal") "decimal → System.Decimal"
                Expect.equal (tryRepr "unit") (Some "System.ValueTuple") "unit → System.ValueTuple"

                Expect.isFalse
                    (IntrinsicRepr.isEncodableValueType "System.Decimal")
                    "System.Decimal is TypeRef-backed, not a direct value type"

                Expect.isFalse
                    (IntrinsicRepr.isEncodableValueType "System.ValueTuple")
                    "System.ValueTuple is TypeRef-backed, not a direct value type"
            }
        ]
