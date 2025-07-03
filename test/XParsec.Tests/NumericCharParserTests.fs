module NumericCharParserTests

#if FABLE_COMPILER
open Fable.Pyxpecto
#else
open Expecto
#endif

open System
open XParsec
open XParsec.CharParsers

#if FABLE_COMPILER
module Expect =
    let isLessThan x y msg = Expect.isTrue (x < y) msg
#endif

let intInput =
    [|
        "0", Ok 0L
        "000", Ok 0L
        "+0", Ok 0L
        "-0", Ok 0L
        "123", Ok 123L
        "", Error EndOfInput
        " ", Error ParseError.intInvalid
        "abc", Error ParseError.intInvalid
        "+", Error EndOfInput
        "-", Error EndOfInput

        "000000000000000000123", Ok 123L
        "-42", Ok -42L
        "999999999", Ok 999999999L
        "-1000000000", Ok -1000000000L
        "9223372036854775807", Ok Int64.MaxValue
        "-9223372036854775808", Ok Int64.MinValue
        "9223372036854775808", Error ParseError.intOutOfRange
        "-9223372036854775809", Error ParseError.intOutOfRange
        "127", Ok(int64 SByte.MaxValue)
        "-128", Ok(int64 SByte.MinValue)
        "128", Ok 128L
        "-129", Ok -129L
        "32767", Ok(int64 Int16.MaxValue)
        "-32768", Ok(int64 Int16.MinValue)
        "32768", Ok 32768L
        "-32769", Ok -32769L
        "2147483647", Ok(int64 Int32.MaxValue)
        "-2147483648", Ok(int64 Int32.MinValue)
        "2147483648", Ok 2147483648L
        "-2147483649", Ok -2147483649L
        "18446744073709551615", Error ParseError.intOutOfRange

        // Hexadecimal
        "0x1A", Ok 26L
        "0XFF", Ok 255L
        "0X0000000000000000000FF", Ok 255L
        "-0x10", Ok -16L
        "0x7fffffffffffffff", Ok Int64.MaxValue
        "-0x8000000000000000", Ok Int64.MinValue
        "0x8000000000000000", Error ParseError.intOutOfRange
        "-0x8000000000000001", Error ParseError.intOutOfRange

        // Hexadecimal for SByte
        "0x7F", Ok(int64 SByte.MaxValue)
        "-0x80", Ok(int64 SByte.MinValue)
        "0x80", Ok 128L
        "-0x81", Ok -129L

        // Hexadecimal for Int16
        "0x7FFF", Ok(int64 Int16.MaxValue)
        "-0x8000", Ok(int64 Int16.MinValue)
        "0x8000", Ok 32768L
        "-0x8001", Ok -32769L

        // Hexadecimal for Int32
        "0x7FFFFFFF", Ok(int64 Int32.MaxValue)
        "-0x80000000", Ok(int64 Int32.MinValue)
        "0x80000000", Ok 2147483648L
        "-0x80000001", Ok -2147483649L

        // Hexadecimal for Int64
        "0x7FFFFFFFFFFFFFFF", Ok Int64.MaxValue
        "-0x8000000000000000", Ok Int64.MinValue
        "0x8000000000000000", Error ParseError.intOutOfRange
        "-0x8000000000000001", Error ParseError.intOutOfRange

        // Octal
        "0o10", Ok 8L
        "+0o10", Ok 8L
        "0O77", Ok 63L
        "-0o20", Ok -16L
        "0o777777777777777777777", Ok 9223372036854775807L
        "-0o1000000000000000000000", Ok -9223372036854775808L
        "0o1000000000000000000000", Error ParseError.intOutOfRange
        "-0o1000000000000000000001", Error ParseError.intOutOfRange

        // Octal for SByte
        "0o177", Ok(int64 SByte.MaxValue)
        "-0o200", Ok(int64 SByte.MinValue)
        "0o200", Ok 128L
        "-0o201", Ok -129L

        // Octal for Int16
        "0o77777", Ok(int64 Int16.MaxValue)
        "-0o100000", Ok(int64 Int16.MinValue)
        "0o100000", Ok 32768L
        "-0o100001", Ok -32769L

        // Octal for Int32
        "0o17777777777", Ok(int64 Int32.MaxValue)
        "-0o20000000000", Ok(int64 Int32.MinValue)
        "0o20000000000", Ok 2147483648L
        "-0o20000000001", Ok -2147483649L

        // Binary
        "0b1010", Ok 10L
        "+0b1010", Ok 10L
        "0B1111", Ok 15L
        "-0b100", Ok -4L

        // Binary for SByte
        "0b01111111", Ok(int64 SByte.MaxValue)
        "-0b10000000", Ok(int64 SByte.MinValue)
        "0b10000000", Ok 128L
        "-0b10000001", Ok -129L

        // Binary for Int16
        "0b0111111111111111", Ok(int64 Int16.MaxValue)
        "-0b1000000000000000", Ok(int64 Int16.MinValue)
        "0b1000000000000000", Ok 32768L
        "-0b1000000000000001", Ok -32769L

        // Binary for Int32
        "0b01111111111111111111111111111111", Ok(int64 Int32.MaxValue)
        "-0b10000000000000000000000000000000", Ok(int64 Int32.MinValue)
        "0b10000000000000000000000000000000", Ok 2147483648L
        "-0b10000000000000000000000000000001", Ok -2147483649L
    |]

let uintInput =
    [|
        "0", Ok 0UL
        "000", Ok 0UL
        "+0", Error ParseError.intInvalid
        "-0", Error ParseError.intInvalid
        "123", Ok 123UL
        "", Error EndOfInput
        " ", Error ParseError.intInvalid
        "abc", Error ParseError.intInvalid
        "+", Error ParseError.intInvalid
        "-", Error ParseError.intInvalid

        "000000000000000000123", Ok 123UL
        "-42", Error ParseError.intInvalid
        "999999999", Ok 999999999UL
        "255", Ok(uint64 Byte.MaxValue)
        "256", Ok 256UL
        "65535", Ok(uint64 UInt16.MaxValue)
        "65536", Ok 65536UL
        "4294967295", Ok(uint64 UInt32.MaxValue)
        "4294967296", Ok 4294967296UL
        "18446744073709551615", Ok UInt64.MaxValue
        "18446744073709551616", Error ParseError.intOutOfRange

        // Hexadecimal
        "0x1A", Ok 26UL
        "0XFF", Ok 255UL
        "0X0000000000000000000FF", Ok 255UL
        "0xffffffffffffffff", Ok UInt64.MaxValue
        "0x10000000000000000", Error ParseError.intOutOfRange
        // sprintf "%x" UInt64.MaxValue

        // Hexadecimal for Byte
        "0xFF", Ok(uint64 Byte.MaxValue)
        "0x100", Ok 256UL

        // Hexadecimal for UInt16
        "0xFFFF", Ok(uint64 UInt16.MaxValue)
        "0x10000", Ok 65536UL

        // Hexadecimal for UInt32
        "0xFFFFFFFF", Ok(uint64 UInt32.MaxValue)
        "0x100000000", Ok 4294967296UL

        // Hexadecimal for UInt64
        "0xFFFFFFFFFFFFFFFF", Ok UInt64.MaxValue
        "0x10000000000000000", Error ParseError.intOutOfRange

        // Octal
        "0o10", Ok 8UL
        "+0o10", Error ParseError.intInvalid
        "0O77", Ok 63UL

        // Octal for Byte
        "0o377", Ok(uint64 Byte.MaxValue)
        "0o400", Ok 256UL

        // Octal for UInt16
        "0o177777", Ok(uint64 UInt16.MaxValue)
        "0o200000", Ok 65536UL

        // Octal for UInt32
        "0o37777777777", Ok(uint64 UInt32.MaxValue)
        "0o40000000000", Ok 4294967296UL

        // Octal for UInt64
        "0o1777777777777777777777", Ok UInt64.MaxValue
        "0o2000000000000000000000", Error ParseError.intOutOfRange

        // sprintf "%B" (uint64 UInt16.MaxValue)
        // Binary
        "0b1010", Ok 10UL
        "+0b1010", Error ParseError.intInvalid
        "0B1111", Ok 15UL
        "-0b100", Error ParseError.intInvalid

        // Binary for Byte
        "0b11111111", Ok(uint64 Byte.MaxValue)
        "0b100000000", Ok 256UL

        // Binary for UInt16
        "0b1111111111111111", Ok(uint64 UInt16.MaxValue)
        "0b10000000000000000", Ok 65536UL

        // Binary for UInt32
        "0b11111111111111111111111111111111", Ok(uint64 UInt32.MaxValue)
        "0b100000000000000000000000000000000", Ok 4294967296UL

        // Binary for UInt64
        "0b1111111111111111111111111111111111111111111111111111111111111111", Ok UInt64.MaxValue
        "0b10000000000000000000000000000000000000000000000000000000000000000", Error ParseError.intOutOfRange
    |]

let floatInput =
    [|
        "0.0", Ok 0.0
        "123.456", Ok 123.456
        "-123.456", Ok -123.456
        "-123.045e6", Ok -123.045e6
        "1e10", Ok 1e10
        "+1e10", Ok 1e10
        "-1e10", Ok -1e10
        "1E-10", Ok 1E-10
        "-1E-10", Ok -1E-10
        "3.40282347E+38", Ok 3.40282347E+38 // Float.MaxValue
        "-3.40282347E+38", Ok -3.40282347E+38 // Float.MinValue
        "NaN", Ok System.Double.NaN
        "-NaN", Ok System.Double.NaN
        "Infinity", Ok System.Double.PositiveInfinity
        "-Infinity", Ok System.Double.NegativeInfinity
        "Inf", Ok System.Double.PositiveInfinity
        "-Inf", Ok System.Double.NegativeInfinity

        // Min / Max values
        "1.7976931348623157E+308", Ok 1.7976931348623157E+308 // Double.MaxValue
        "-1.7976931348623157E+308", Ok -1.7976931348623157E+308 // Double.MinValue
        "1.7976931348623159E+308", Ok Double.PositiveInfinity // Exceeds Double.MaxValue
        "-1.7976931348623159E+308", Ok Double.NegativeInfinity // Exceeds Double.MinValue

        // Rounded values
        "123456789.0123456789012345678901234567890", Ok 123456789.0123456789012345678901234567890 // Precision test
        "123456789.0123456789012345678901234567890e10", Ok 123456789.0123456789012345678901234567890e10 // Precision test with exponent

        // Hexadecimal
        "0x1", Ok 0x1 // Hexadecimal representation of 1
        "0xaff", Ok 0xaff // Hexadecimal representation of 2815
        "0x123p45", Ok(float 0x123 * Double.Exp2 45)
        "0x12.3p45", Ok(float 0x123 * Double.Exp2 41)
        "0xA.B", Ok(float 0xAB * Double.Exp2 -4)
        "0xa.bcp-2", Ok(float 0xabc * Double.Exp2 -10)
        "0x123.456P+78", Ok(float 0x123456 * Double.Exp2 66)
        "", Error EndOfInput // Empty input
        "+", Error EndOfInput
        "-", Error EndOfInput
        "-1e", Error EndOfInput
        "0x", Error EndOfInput // Incomplete hexadecimal
        "+0x", Error EndOfInput
        "-0x", Error EndOfInput
    |]

let confirm msg input expected p =
    let reader = Reader.ofString input ()
    let result = p reader

    match expected with
    | Ok expected ->
        match result with
        | Ok result ->
            msg |> Expect.equal result.Parsed expected
            msg |> Expect.equal reader.Index (int64 input.Length)
        | Error e -> failwithf "%s Expected Ok %A, but got Error %A" msg expected e
    | Error expected ->
        match result with
        | Ok ok -> failwithf "%s Expected error %A, but got Ok. %A" msg expected ok
        | Error resultError -> msg |> Expect.equal resultError.Errors expected

let confirmFloat msg input expected p =
    let reader = Reader.ofString input ()
    let result = p reader

    match expected with
    | Ok expected ->
        if Double.IsNaN expected then
            match result with
            | Ok result ->
                msg |> Expect.isTrue (Double.IsNaN result.Parsed)
                msg |> Expect.equal reader.Index (int64 input.Length)
            | Error e -> failwithf "%s Expected Ok NaN, but got Error %A" msg e
        elif expected = Double.PositiveInfinity || expected = Double.NegativeInfinity then
            match result with
            | Ok result ->
                msg |> Expect.equal result.Parsed expected
                msg |> Expect.equal reader.Index (int64 input.Length)
            | Error e -> failwithf "%s Expected Ok %A, but got Error %A" msg expected e
        else
            match result with
            | Ok result ->
                let relativeDifference =
                    if expected = 0.0 then
                        Math.Abs(result.Parsed - expected)
                    else
                        Math.Abs((result.Parsed - expected) / expected)

                // We expct the parsed value to be exactly equal, but check for relative difference
                // first to identify floating point precision issues
                $"{msg} {result.Parsed} {expected}"
                |> Expect.isLessThan relativeDifference 1e-15

                msg |> Expect.equal result.Parsed expected
                msg |> Expect.equal reader.Index (int64 input.Length)
            | Error e -> failwithf "%s Expected Ok %A, but got Error %A" msg expected e
    | Error expected ->
        match result with
        | Ok ok -> failwithf "%s Expected error %A, but got Ok. %A" msg expected ok
        | Error resultError -> msg |> Expect.equal resultError.Errors expected

#if !FABLE_COMPILER
[<Tests>]
#endif
let tests =
    testList
        "NumericCharParserTests"
        [
            test "PInt64" {
                for input, expected in intInput do
                    let msg = $"Parsing '{input}' with pint64 failed."
                    confirm msg input expected pint64
            }

            test "PInt32" {
                let inRange expected =
                    match expected with
                    | Ok v ->
                        if v < int64 Int32.MinValue || v > int64 Int32.MaxValue then
                            Error ParseError.intOutOfRange
                        else
                            Ok(int32 v)
                    | Error e -> Error e

                for input, expected in intInput do
                    let expected = inRange expected
                    let msg = $"Parsing '{input}' with pint32 failed."
                    confirm msg input expected pint32
            }

            test "PInt16" {
                let inRange expected =
                    match expected with
                    | Ok v ->
                        if v < int64 Int16.MinValue || v > int64 Int16.MaxValue then
                            Error ParseError.intOutOfRange
                        else
                            Ok(int16 v)
                    | Error e -> Error e

                for input, expected in intInput do
                    let expected = inRange expected
                    let msg = $"Parsing '{input}' with pint16 failed."
                    confirm msg input expected pint16
            }

            test "PInt8" {
                let inRange expected =
                    match expected with
                    | Ok v ->
                        if v < int64 SByte.MinValue || v > int64 SByte.MaxValue then
                            Error ParseError.intOutOfRange
                        else
                            Ok(sbyte v)
                    | Error e -> Error e

                for input, expected in intInput do
                    let expected = inRange expected
                    let msg = $"Parsing '{input}' with pint8 failed."
                    confirm msg input expected pint8
            }

            test "PUInt64" {
                for input, expected in uintInput do
                    let msg = $"Parsing '{input}' with puint64 failed."
                    confirm msg input expected puint64
            }

            test "PUInt32" {
                let inRange expected =
                    match expected with
                    | Ok v ->
                        if v > uint64 UInt32.MaxValue then
                            Error ParseError.intOutOfRange
                        else
                            Ok(uint32 v)
                    | Error e -> Error e

                for input, expected in uintInput do
                    let expected = inRange expected
                    let msg = $"Parsing '{input}' with puint32 failed."
                    confirm msg input expected puint32
            }

            test "PUInt16" {
                let inRange expected =
                    match expected with
                    | Ok v ->
                        if v > uint64 UInt16.MaxValue then
                            Error ParseError.intOutOfRange
                        else
                            Ok(uint16 v)
                    | Error e -> Error e

                for input, expected in uintInput do
                    let expected = inRange expected
                    let msg = $"Parsing '{input}' with puint16 failed."
                    confirm msg input expected puint16
            }

            test "PUInt8" {
                let inRange expected =
                    match expected with
                    | Ok v ->
                        if v > uint64 Byte.MaxValue then
                            Error ParseError.intOutOfRange
                        else
                            Ok(byte v)
                    | Error e -> Error e

                for input, expected in uintInput do
                    let expected = inRange expected
                    let msg = $"Parsing '{input}' with puint8 failed."
                    confirm msg input expected puint8
            }

            test "PFloat" {
                for input, expected in floatInput do
                    let msg = $"Parsing '{input}' with pfloat failed."
                    confirmFloat msg input expected pfloat
            }
        ]
