module MessagePackTests

open System
open System.IO

open Expecto

open XParsec

open MessagePack
open MessagePack.ArrayParsers


let testParse input format value =
    let actualFormat =
        let reader = Reader.ofArray input ()
        pFormat () reader

    let actualValue =
        let reader = Reader.ofArray input ()
        pObject () reader

    match actualFormat with
    | Ok f -> "Format" |> Expect.equal f.Parsed format
    | Error e -> failwithf "%A" e

    match actualValue with
    | Ok v -> "Value" |> Expect.equal v.Parsed value
    | Error e -> failwithf "%A" e

    let actualFormat =
        let reader = Reader.ofStream (new MemoryStream(input)) 128 ()
        pFormat () reader

    let actualValue =
        let reader = Reader.ofStream (new MemoryStream(input)) 128 ()
        pObject () reader

    match actualFormat with
    | Ok f -> "Format Stream" |> Expect.equal f.Parsed format
    | Error e -> failwithf "%A" e

    match actualValue with
    | Ok v -> "Value Stream" |> Expect.equal v.Parsed value
    | Error e -> failwithf "%A" e

[<Tests>]
let tests =
    testList
        "MessagePackParsing"
        [
            test "ParseNil" {
                let input, format, value = [| 0xc0uy |], Formats.Nil, MsgPackValue.Nil
                testParse input format value
            }
            test "ParseTrue" {
                let input, format, value = [| 0xc3uy |], Formats.True, MsgPackValue.True
                testParse input format value
            }
            test "ParseFalse" {
                let input, format, value = [| 0xc2uy |], Formats.False, MsgPackValue.False
                testParse input format value
            }
            test "ParsePositiveFixInt" {
                for i in 0uy .. 127uy do
                    let input, format, value = [| i |], Formats.PositiveFixInt i, MsgPackValue.UInt8 i
                    testParse input format value
            }
            test "ParseNegativeFixInt" {
                for i in -31y .. -1y do
                    let b = byte i
                    let input, format, value = [| b |], Formats.NegativeFixInt b, MsgPackValue.Int8 i
                    testParse input format value
            }
            test "ParseUInt8" {
                let input, format, value =
                    [| 0xccuy; 0xffuy |], Formats.UInt8, MsgPackValue.UInt8 255uy

                testParse input format value
            }
            test "ParseUInt16" {
                let input, format, value =
                    [| 0xcduy; 0xffuy; 0xffuy |], Formats.UInt16, MsgPackValue.UInt16 65535us

                testParse input format value
            }
            test "ParseUInt32" {
                let input, format, value =
                    [| 0xceuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy |], Formats.UInt32, MsgPackValue.UInt32 4294967295ul

                testParse input format value
            }
            test "ParseUInt64" {
                let input, format, value =
                    [| 0xcfuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy; 0xffuy |],
                    Formats.UInt64,
                    MsgPackValue.UInt64 18446744073709551615UL

                testParse input format value
            }
            test "ParseInt8" {
                let input, format, value =
                    [| 0xd0uy; 0x80uy |], Formats.Int8, MsgPackValue.Int8 -128y

                testParse input format value
            }
            test "ParseInt16" {
                let input, format, value =
                    [| 0xd1uy; 0x80uy; 0x00uy |], Formats.Int16, MsgPackValue.Int16 -32768s

                testParse input format value
            }
            test "ParseInt32" {
                let input, format, value =
                    [| 0xd2uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy |], Formats.Int32, MsgPackValue.Int32 -2147483648

                testParse input format value
            }
            test "ParseInt64" {
                let input, format, value =
                    [| 0xd3uy; 0x80uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |],
                    Formats.Int64,
                    MsgPackValue.Int64 -9223372036854775808L

                testParse input format value
            }
            test "ParseFloat32" {
                let input, format, value =
                    [| 0xcauy; 0x3fuy; 0x80uy; 0x00uy; 0x00uy |], Formats.Float32, MsgPackValue.Float32 1.0f

                testParse input format value
            }
            test "ParseFloat64" {
                let input, format, value =
                    [| 0xcbuy; 0x3fuy; 0xf0uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy |],
                    Formats.Float64,
                    MsgPackValue.Float64 1.0

                testParse input format value
            }
            test "ParseStr8" {
                let input, format, value =
                    [| 0xd9uy; 0x05uy; 0x48uy; 0x65uy; 0x6cuy; 0x6cuy; 0x6fuy |], Formats.Str8, MsgPackValue.Str "Hello"

                testParse input format value
            }
            test "ParseStr16" {
                let input, format, value =
                    [| 0xdauy; 0x00uy; 0x05uy; 0x48uy; 0x65uy; 0x6cuy; 0x6cuy; 0x6fuy |],
                    Formats.Str16,
                    MsgPackValue.Str "Hello"

                testParse input format value
            }
            test "ParseStr32" {
                let input, format, value =
                    [|
                        0xdbuy
                        0x00uy
                        0x00uy
                        0x00uy
                        0x05uy
                        0x48uy
                        0x65uy
                        0x6cuy
                        0x6cuy
                        0x6fuy
                    |],
                    Formats.Str32,
                    MsgPackValue.Str "Hello"

                testParse input format value
            }
            test "ParseBin8" {
                let input, format, value =
                    [| 0xc4uy; 0x03uy; 0x01uy; 0x02uy; 0x03uy |],
                    Formats.Bin8,
                    MsgPackValue.Bin [| 0x01uy; 0x02uy; 0x03uy |]

                testParse input format value
            }
            test "ParseBin16" {
                let input, format, value =
                    [| 0xc5uy; 0x00uy; 0x03uy; 0x01uy; 0x02uy; 0x03uy |],
                    Formats.Bin16,
                    MsgPackValue.Bin [| 0x01uy; 0x02uy; 0x03uy |]

                testParse input format value
            }
            test "ParseBin32" {
                let input, format, value =
                    [| 0xc6uy; 0x00uy; 0x00uy; 0x00uy; 0x03uy; 0x01uy; 0x02uy; 0x03uy |],
                    Formats.Bin32,
                    MsgPackValue.Bin [| 0x01uy; 0x02uy; 0x03uy |]

                testParse input format value
            }
        ]
