module ByteParsersTests
#if !FABLE_COMPILER
open System
open System.Buffers.Binary

open Expecto

open XParsec
open XParsec.ByteParsers

let zeroes = Array.zeroCreate<byte> 16
let odds = Array.init 16 (fun i -> 1uy + (byte i))
let evens = Array.init 16 (fun i -> 2uy + (byte i))

let testParser<'T when 'T: equality> p (input, expected) =
    let reader = Reader.ofArray input ()

    match p reader with
    | Ok result ->
        "" |> Expect.equal (result.Parsed: 'T) expected
        "" |> Expect.equal reader.Index sizeof<'T>
    | Error e -> failwithf "%A" e


[<Tests>]
let tests =
    testList
        "ByteParsersTests"
        [
            test "pFloat16BE" {
                [
                    zeroes, Half.Zero
                    odds, BinaryPrimitives.ReadHalfBigEndian odds
                    evens, BinaryPrimitives.ReadHalfBigEndian evens
                ]
                |> List.iter (testParser pFloat16BE)
            }
            test "pFloat16LE" {
                [
                    zeroes, Half.Zero
                    odds, BinaryPrimitives.ReadHalfLittleEndian odds
                    evens, BinaryPrimitives.ReadHalfLittleEndian evens
                ]
                |> List.iter (testParser pFloat16LE)
            }
            test "pFloat32BE" {
                [
                    zeroes, 0.0f
                    odds, BinaryPrimitives.ReadSingleBigEndian odds
                    evens, BinaryPrimitives.ReadSingleBigEndian evens
                ]
                |> List.iter (testParser pFloat32BE)
            }
            test "pFloat32LE" {
                [
                    zeroes, 0.0f
                    odds, BinaryPrimitives.ReadSingleLittleEndian odds
                    evens, BinaryPrimitives.ReadSingleLittleEndian evens
                ]
                |> List.iter (testParser pFloat32LE)
            }
            test "pFloat64BE" {
                [
                    zeroes, 0.0
                    odds, BinaryPrimitives.ReadDoubleBigEndian odds
                    evens, BinaryPrimitives.ReadDoubleBigEndian evens
                ]
                |> List.iter (testParser pFloat64BE)
            }
            test "pFloat64LE" {
                [
                    zeroes, 0.0
                    odds, BinaryPrimitives.ReadDoubleLittleEndian odds
                    evens, BinaryPrimitives.ReadDoubleLittleEndian evens
                ]
                |> List.iter (testParser pFloat64LE)
            }
            test "pUInt16BE" {
                [
                    zeroes, 0us
                    odds, BinaryPrimitives.ReadUInt16BigEndian odds
                    evens, BinaryPrimitives.ReadUInt16BigEndian evens
                ]
                |> List.iter (testParser pUInt16BE)
            }
            test "pUInt16LE" {
                [
                    zeroes, 0us
                    odds, BinaryPrimitives.ReadUInt16LittleEndian odds
                    evens, BinaryPrimitives.ReadUInt16LittleEndian evens
                ]
                |> List.iter (testParser pUInt16LE)
            }
            test "pUInt32BE" {
                [
                    zeroes, 0u
                    odds, BinaryPrimitives.ReadUInt32BigEndian odds
                    evens, BinaryPrimitives.ReadUInt32BigEndian evens
                ]
                |> List.iter (testParser pUInt32BE)
            }
            test "pUInt32LE" {
                [
                    zeroes, 0u
                    odds, BinaryPrimitives.ReadUInt32LittleEndian odds
                    evens, BinaryPrimitives.ReadUInt32LittleEndian evens
                ]
                |> List.iter (testParser pUInt32LE)
            }
            test "pUInt64BE" {
                [
                    zeroes, 0UL
                    odds, BinaryPrimitives.ReadUInt64BigEndian odds
                    evens, BinaryPrimitives.ReadUInt64BigEndian evens
                ]
                |> List.iter (testParser pUInt64BE)
            }
            test "pUInt64LE" {
                [
                    zeroes, 0UL
                    odds, BinaryPrimitives.ReadUInt64LittleEndian odds
                    evens, BinaryPrimitives.ReadUInt64LittleEndian evens
                ]
                |> List.iter (testParser pUInt64LE)
            }
            test "pUInt128BE" {
                [
                    zeroes, UInt128.Zero
                    odds, BinaryPrimitives.ReadUInt128BigEndian odds
                    evens, BinaryPrimitives.ReadUInt128BigEndian evens
                ]
                |> List.iter (testParser pUInt128BE)
            }
            test "pUInt128LE" {
                [
                    zeroes, UInt128.Zero
                    odds, BinaryPrimitives.ReadUInt128LittleEndian odds
                    evens, BinaryPrimitives.ReadUInt128LittleEndian evens
                ]
                |> List.iter (testParser pUInt128LE)
            }
            test "pUIntPtrBE" {
                [
                    zeroes, UIntPtr.Zero
                    odds, BinaryPrimitives.ReadUIntPtrBigEndian odds
                    evens, BinaryPrimitives.ReadUIntPtrBigEndian evens
                ]
                |> List.iter (testParser pUIntPtrBE)
            }
            test "pUIntPtrLE" {
                [
                    zeroes, UIntPtr.Zero
                    odds, BinaryPrimitives.ReadUIntPtrLittleEndian odds
                    evens, BinaryPrimitives.ReadUIntPtrLittleEndian evens
                ]
                |> List.iter (testParser pUIntPtrLE)
            }
            test "pInt16BE" {
                [
                    zeroes, 0s
                    odds, BinaryPrimitives.ReadInt16BigEndian odds
                    evens, BinaryPrimitives.ReadInt16BigEndian evens
                ]
                |> List.iter (testParser pInt16BE)
            }
            test "pInt16LE" {
                [
                    zeroes, 0s
                    odds, BinaryPrimitives.ReadInt16LittleEndian odds
                    evens, BinaryPrimitives.ReadInt16LittleEndian evens
                ]
                |> List.iter (testParser pInt16LE)
            }
            test "pInt32BE" {
                [
                    zeroes, 0
                    odds, BinaryPrimitives.ReadInt32BigEndian odds
                    evens, BinaryPrimitives.ReadInt32BigEndian evens
                ]
                |> List.iter (testParser pInt32BE)
            }
            test "pInt32LE" {
                [
                    zeroes, 0
                    odds, BinaryPrimitives.ReadInt32LittleEndian odds
                    evens, BinaryPrimitives.ReadInt32LittleEndian evens
                ]
                |> List.iter (testParser pInt32LE)
            }
            test "pInt64BE" {
                [
                    zeroes, 0L
                    odds, BinaryPrimitives.ReadInt64BigEndian odds
                    evens, BinaryPrimitives.ReadInt64BigEndian evens
                ]
                |> List.iter (testParser pInt64BE)
            }
            test "pInt64LE" {
                [
                    zeroes, 0L
                    odds, BinaryPrimitives.ReadInt64LittleEndian odds
                    evens, BinaryPrimitives.ReadInt64LittleEndian evens
                ]
                |> List.iter (testParser pInt64LE)
            }
            test "pInt128BE" {
                [
                    zeroes, Int128.Zero
                    odds, BinaryPrimitives.ReadInt128BigEndian odds
                    evens, BinaryPrimitives.ReadInt128BigEndian evens
                ]
                |> List.iter (testParser pInt128BE)
            }
            test "pInt128LE" {
                [
                    zeroes, Int128.Zero
                    odds, BinaryPrimitives.ReadInt128LittleEndian odds
                    evens, BinaryPrimitives.ReadInt128LittleEndian evens
                ]
                |> List.iter (testParser pInt128LE)
            }
            test "pIntPtrBE" {
                [
                    zeroes, IntPtr.Zero
                    odds, BinaryPrimitives.ReadIntPtrBigEndian odds
                    evens, BinaryPrimitives.ReadIntPtrBigEndian evens
                ]
                |> List.iter (testParser pIntPtrBE)
            }
            test "pIntPtrLE" {
                [
                    zeroes, IntPtr.Zero
                    odds, BinaryPrimitives.ReadIntPtrLittleEndian odds
                    evens, BinaryPrimitives.ReadIntPtrLittleEndian evens
                ]
                |> List.iter (testParser pIntPtrLE)
            }

            test "Composite Read Odds" {
                let input = odds

                let p = tuple5 pFloat16BE pUInt16LE pInt16BE pFloat32LE pUInt32BE

                let expected =
                    let span = input.AsSpan()

                    struct (BinaryPrimitives.ReadHalfBigEndian span,
                            BinaryPrimitives.ReadUInt16LittleEndian(span.Slice(2)),
                            BinaryPrimitives.ReadInt16BigEndian(span.Slice(4)),
                            BinaryPrimitives.ReadSingleLittleEndian(span.Slice(6)),
                            BinaryPrimitives.ReadUInt32BigEndian(span.Slice(10)))

                let reader = Reader.ofArray input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed expected
                    "" |> Expect.equal reader.Index 14
                | Error e -> failwithf "%A" e
            }

            test "Composite Read Evens" {
                let input = [| yield! evens; yield! evens |]

                let p = tuple5 pFloat32BE pUInt32LE pInt32BE pFloat64LE pUInt64BE

                let expected =
                    let span = input.AsSpan()

                    struct (BinaryPrimitives.ReadSingleBigEndian span,
                            BinaryPrimitives.ReadUInt32LittleEndian(span.Slice(4)),
                            BinaryPrimitives.ReadInt32BigEndian(span.Slice(8)),
                            BinaryPrimitives.ReadDoubleLittleEndian(span.Slice(12)),
                            BinaryPrimitives.ReadUInt64BigEndian(span.Slice(20)))

                let reader = Reader.ofArray input ()
                let result = p reader

                match result with
                | Ok result ->
                    "" |> Expect.equal result.Parsed expected
                    "" |> Expect.equal reader.Index 28
                | Error e -> failwithf "%A" e
            }
        ]
#endif
