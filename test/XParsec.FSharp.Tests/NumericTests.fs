module XParsec.FSharp.Lexer.Tests.NumericTests

open System
open System.Text

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing

let intPartCases =
    [|
        // Hexadecimal
        // Simple cases
        ("0x42", NumericBase.Hex)
        ("0XFF", NumericBase.Hex)
        ("0x0", NumericBase.Hex)
        ("0x1", NumericBase.Hex)

        // Case-insensitivity
        ("0xabcdef", NumericBase.Hex)
        ("0XABCDEF", NumericBase.Hex)
        ("0xDeAdBeEf", NumericBase.Hex)

        // With underscores
        ("0x123_456", NumericBase.Hex)
        ("0XDEAD_BEEF", NumericBase.Hex)
        ("0xFF_FF__FF", NumericBase.Hex) // Multiple consecutive underscores
        ("0x9_", NumericBase.Hex) // Trailing underscore should be ignored by the numeric part

        // Octal
        // Simple cases
        ("0o777", NumericBase.Octal)
        ("0O123", NumericBase.Octal)
        ("0o0", NumericBase.Octal)
        ("0o1", NumericBase.Octal)

        // With underscores
        ("0o123_456", NumericBase.Octal)
        ("0O7_1_1", NumericBase.Octal)
        ("0o1__2__3", NumericBase.Octal)
        ("0o7_", NumericBase.Octal)

        // Binary
        // Simple cases
        ("0b1010", NumericBase.Binary)
        ("0B1111", NumericBase.Binary)
        ("0b0", NumericBase.Binary)
        ("0b1", NumericBase.Binary)

        // With underscores
        ("0b1000_0001", NumericBase.Binary)
        ("0B1110__0101", NumericBase.Binary)
        ("0b1_0_1_0", NumericBase.Binary)
        ("0b1_", NumericBase.Binary)

        // Should resolve to Decimal (only contains 0 or 1 but no base prefix)
        ("0", NumericBase.Decimal)
        ("1", NumericBase.Decimal)
        ("10110", NumericBase.Decimal)
        ("111_000", NumericBase.Decimal)

        // Should resolve to Decimal (contains digits 2-7)
        ("7", NumericBase.Decimal)
        ("12345670", NumericBase.Decimal)
        ("1_2_3_4_5_6_7", NumericBase.Decimal)
        ("777", NumericBase.Decimal)

        // Should resolve to Decimal (contains 8 or 9)
        ("9", NumericBase.Decimal)
        ("8", NumericBase.Decimal)
        ("123945", NumericBase.Decimal)
        ("1_000_000", NumericBase.Decimal)
        ("9876543210", NumericBase.Decimal)

    |]

// regexp digit = [0-9]
// regexp hexdigit = digit | [A-F] | [a-f]
// regexp octaldigit = [0-7]
// regexp bitdigit = [0-1]

// regexp int =
//     | digit+                                     For example, 34
//     | int _ int                                  For example, 123_456_789

// regexp hexint =
//     | hexdigit+                                  For example, 1f
//     | hexint _ hexint                            For example, abc_456_78

// regexp octalint =
//     | octaldigit+                                For example, 34
//     | octalint _ octalint                        For example, 123_4_711

// regexp bitint =
//     | bitdigit+                                  For example, 11
//     | bitint _ bitint                            For example, 1110_0101_0100

// regexp xint =
//     | 0 (x|X) hexint                             For example, 0x22_1f
//     | 0 (o|O) octalint                           For example, 0o42
//     | 0 (b|B) bitint                             For example, 0b10010

// token sbyte = ( int | xint ) 'y'                 For example, 34y
// token byte = ( int | xint ) ' uy'                For example, 34uy
// token int16 = ( int | xint ) 's'                 For example, 34s
// token uint16 = ( int | xint ) 'us'               For example, 34us
// token int32 = ( int | xint ) 'l'                 For example, 34l
// token uint32 = ( int | xint ) 'ul'               For example, 34ul
//              | ( int | xint ) 'u'                For example, 34u
// token nativeint = ( int | xint ) 'n'             For example, 34n
// token unativeint = ( int | xint ) 'un'           For example, 34un
// token int64 = ( int | xint ) 'L'                 For example, 34L
// token uint64 = ( int | xint ) 'UL'               For example, 34UL
//              | ( int | xint ) 'uL'               For example, 34uL

// token float =
//     int . int?
//     int (. int?)? (e|E) (+|-)? int

// token ieee32 =
//     | float [Ff]                                 For example, 3.0F or 3.0f
//     | xint 'lf'                                  For example, 0x00000000lf
// token ieee64 =
//     | float                                      For example, 3.0
//     | xint 'LF'                                  For example, 0x0000000000000000LF

// token bignum = int ('Q' | ' R' | 'Z' | 'I' | 'N' | 'G')
//                                                  For example, 34742626263193832612536171N

// token decimal = ( float | int ) [Mm]

let integerLiteralCases =
    [|
        // Decimal integer literals
        ("0", Token.NumInt32)
        ("42", Token.NumInt32)
        ("123456", Token.NumInt32)
        ("1_000_000", Token.NumInt32)
        ("2147483647", Token.NumInt32) // Max int32

        // SByte
        ("127y", Token.NumSByte)
        // TODO: Enable when negative literals are supported
        // ("-128y", Token.NumSByte)
        ("0y", Token.NumSByte)
        ("0x7Fy", Token.NumSByteHex)
        ("0b01111111y", Token.NumSByteBinary)
        ("0o177y", Token.NumSByteOctal)

        // Byte
        ("255uy", Token.NumByte)
        ("0uy", Token.NumByte)
        ("0xFFuy", Token.NumByteHex)
        ("0b11111111uy", Token.NumByteBinary)
        ("0o377uy", Token.NumByteOctal)

        // Int16
        ("32767s", Token.NumInt16)
        // ("-32768s", Token.NumInt16)
        ("0s", Token.NumInt16)
        ("0x7FFFs", Token.NumInt16Hex)
        ("0b0111111111111111s", Token.NumInt16Binary)
        ("0o77777s", Token.NumInt16Octal)

        // UInt16
        ("65535us", Token.NumUInt16)
        ("0us", Token.NumUInt16)
        ("0xFFFFus", Token.NumUInt16Hex)
        ("0b1111111111111111us", Token.NumUInt16Binary)
        ("0o177777us", Token.NumUInt16Octal)

        // Int32
        ("2147483647l", Token.NumInt32)
        // ("-2147483648l", Token.NumInt32)
        ("0l", Token.NumInt32)
        ("0x7FFFFFFFl", Token.NumInt32Hex)
        ("0b01111111111111111111111111111111l", Token.NumInt32Binary)
        ("0o17777777777l", Token.NumInt32Octal)

        // UInt32
        ("4294967295ul", Token.NumUInt32)
        ("0ul", Token.NumUInt32)
        ("0xFFFFFFFFul", Token.NumUInt32Hex)
        ("0b11111111111111111111111111111111ul", Token.NumUInt32Binary)
        ("0o37777777777ul", Token.NumUInt32Octal)
        ("42u", Token.NumUInt32)
        ("0x2Au", Token.NumUInt32Hex)
        ("0b101010u", Token.NumUInt32Binary)
        ("0o52u", Token.NumUInt32Octal)

        // NativeInt
        ("123n", Token.NumNativeInt)
        ("0x7Fn", Token.NumNativeIntHex)
        ("0b1010n", Token.NumNativeIntBinary)
        ("0o77n", Token.NumNativeIntOctal)

        // UNativeInt
        ("123un", Token.NumUNativeInt)
        ("0x7Fun", Token.NumUNativeIntHex)
        ("0b1010un", Token.NumUNativeIntBinary)
        ("0o77un", Token.NumUNativeIntOctal)

        // Int64
        ("9223372036854775807L", Token.NumInt64)
        // ("-9223372036854775808L", Token.NumInt64)
        ("0L", Token.NumInt64)
        ("0x7FFFFFFFFFFFFFFFL", Token.NumInt64Hex)
        ("0b0111111111111111111111111111111111111111111111111111111111111111L", Token.NumInt64Binary)
        ("0o777777777777777777777L", Token.NumInt64Octal)

        // UInt64
        ("18446744073709551615UL", Token.NumUInt64)
        ("0UL", Token.NumUInt64)
        ("0xFFFFFFFFFFFFFFFFUL", Token.NumUInt64Hex)
        ("0b1111111111111111111111111111111111111111111111111111111111111111UL", Token.NumUInt64Binary)
        ("0o1777777777777777777777UL", Token.NumUInt64Octal)
        ("42uL", Token.NumUInt64)
        ("0x2AuL", Token.NumUInt64Hex)
        ("0b101010uL", Token.NumUInt64Binary)
        ("0o52uL", Token.NumUInt64Octal)

        // BigNum (arbitrary precision integer) literals
        // Note: Hex, Octal, and Binary forms are not supported for BigNum in this implementation
        ("123Q", Token.NumBigIntegerQ)
        ("456R", Token.NumBigIntegerR)
        ("789Z", Token.NumBigIntegerZ)
        ("101112I", Token.NumBigIntegerI)
        ("131415N", Token.NumBigIntegerN)
        ("161718G", Token.NumBigIntegerG)
        ("0Q", Token.NumBigIntegerQ)
        ("0R", Token.NumBigIntegerR)
        ("0Z", Token.NumBigIntegerZ)
        ("0I", Token.NumBigIntegerI)
        ("0N", Token.NumBigIntegerN)
        ("0G", Token.NumBigIntegerG)
    |]

let floatLiteralCases =
    [|
        // Simple decimal floats
        ("0.0", Token.NumIEEE64)
        ("1.0", Token.NumIEEE64)
        ("123.456", Token.NumIEEE64)
        ("1_234.567_89", Token.NumIEEE64)
        ("0.", Token.NumIEEE64)
        ("42.", Token.NumIEEE64)

        // Scientific notation
        ("1e10", Token.NumIEEE64)
        ("1E10", Token.NumIEEE64)
        ("1.23e-4", Token.NumIEEE64)
        ("1.23E+4", Token.NumIEEE64)
        ("1e+10", Token.NumIEEE64)
        ("1e-10", Token.NumIEEE64)
        ("1_2.3_4e5_6", Token.NumIEEE64)

        // Float32 suffix
        ("3.14f", Token.NumIEEE32)
        ("2.718F", Token.NumIEEE32)
        ("1e10f", Token.NumIEEE32)
        ("1.23e-4F", Token.NumIEEE32)

        // Float64 suffix (explicit)
        ("0b0101LF", Token.NumIEEE64Binary)
        ("0o1234567LF", Token.NumIEEE64Octal)
        ("0x1921fb54442d18LF", Token.NumIEEE64Hex)

        // Float32 hex form
        ("0b0101lf", Token.NumIEEE32Binary)
        ("0o1234567lf", Token.NumIEEE32Octal)
        ("0x19442d18lf", Token.NumIEEE32Hex)

        // Decimal suffix
        ("123M", Token.NumDecimal)
        ("123.45M", Token.NumDecimal)
        ("0.0m", Token.NumDecimal)
        ("1e10M", Token.NumDecimal)
        ("1.23e-4m", Token.NumDecimal)
    |]


// Tests for integer literals with different bases and formats


[<Tests>]
let tests1 =
    testList
        "Integer Fragment Tests"
        [
            for input, expectedBase in intPartCases do
                test $"Lexing numeric literal '{input}'" {
                    let reader = Reader.ofString input (LexBuilder.init ())

                    match NumericLiterals.pXIntBase reader with
                    | Ok { Parsed = actualBase } ->
                        "" |> Expect.equal actualBase expectedBase
                        "" |> Expect.equal reader.Index (input.TrimEnd('_').Length)
                    | Error err -> failtestf "Lexing failed: %A" err
                }
        ]


[<Tests>]
let tests2 =
    testList
        "Numeric Literal Tests"
        [
            for input, expectedToken in integerLiteralCases do
                test $"Lexing numeric literal '{input}'" {
                    // printfn "Testing numeric literal: '%s' (Token: %O)" input expectedToken
                    let expected =
                        [
                            PositionedToken.Create(expectedToken, 0)
                            PositionedToken.Create(Token.EOF, input.Length)
                        ]

                    testLexed input expected
                }

            for input, expectedToken in floatLiteralCases do
                test $"Lexing numeric literal '{input}'" {
                    // printfn "Testing numeric literal: '%s' (Token: %O)" input expectedToken
                    let expected =
                        [
                            PositionedToken.Create(expectedToken, 0)
                            PositionedToken.Create(Token.EOF, input.Length)
                        ]

                    testLexed input expected
                }
        ]
