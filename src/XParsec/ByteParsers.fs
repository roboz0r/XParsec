module XParsec.ByteParsers
#if !FABLE_COMPILER
open System
open Parsers
open System.Buffers.Binary

let private gotFewerBytes (expected: int) (actual: int) =
    Message(sprintf "Expected %d bytes but only %d bytes are available." expected actual)

#if NET8_0_OR_GREATER
let pFloat16BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<Half, byte, 'State> =
    let width = 2
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadHalfBigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pFloat16LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<Half, byte, 'State> =
    let width = 2
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadHalfLittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pFloat32BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<float32, byte, 'State> =
    let width = 4
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadSingleBigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pFloat32LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<float32, byte, 'State> =
    let width = 4
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadSingleLittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pFloat64BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<float, byte, 'State> =
    let width = 8
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadDoubleBigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pFloat64LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<float, byte, 'State> =
    let width = 8
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadDoubleLittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader
#endif

let pUInt16BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<uint16, byte, 'State> =
    let width = 2
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt16BigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUInt16LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<uint16, byte, 'State> =
    let width = 2
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt16LittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUInt32BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<uint32, byte, 'State> =
    let width = 4
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt32BigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUInt32LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<uint32, byte, 'State> =
    let width = 4
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt32LittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUInt64BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<uint64, byte, 'State> =
    let width = 8
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt64BigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUInt64LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<uint64, byte, 'State> =
    let width = 8
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt64LittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

#if NET8_0_OR_GREATER
let pUInt128BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<UInt128, byte, 'State> =
    let width = 16
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt128BigEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUInt128LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<UInt128, byte, 'State> =
    let width = 16
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUInt128LittleEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUIntPtrBE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<UIntPtr, byte, 'State> =
    let width = IntPtr.Size
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUIntPtrBigEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pUIntPtrLE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<UIntPtr, byte, 'State> =
    let width = IntPtr.Size
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadUIntPtrLittleEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader
#endif

let pInt16BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<int16, byte, 'State> =
    let width = 2
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt16BigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pInt16LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<int16, byte, 'State> =
    let width = 2
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt16LittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pInt32BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<int32, byte, 'State> =
    let width = 4
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt32BigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pInt32LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<int32, byte, 'State> =
    let width = 4
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt32LittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pInt64BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<int64, byte, 'State> =
    let width = 8
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt64BigEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pInt64LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<int64, byte, 'State> =
    let width = 8
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt64LittleEndian(span)) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

#if NET8_0_OR_GREATER
let pInt128BE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<Int128, byte, 'State> =
    let width = 16
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt128BigEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pInt128LE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<Int128, byte, 'State> =
    let width = 16
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadInt128LittleEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pIntPtrBE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<IntPtr, byte, 'State> =
    let width = IntPtr.Size
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadIntPtrBigEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader

let pIntPtrLE (reader: Reader<byte, 'State, 'Input, 'InputSlice>) : ParseResult<IntPtr, byte, 'State> =
    let width = IntPtr.Size
    let span = reader.PeekN width

    if span.Length = width then
        reader.SkipN width
        preturn (BinaryPrimitives.ReadIntPtrLittleEndian span) reader
    elif span.Length = 0 then
        fail EndOfInput reader
    else
        fail (gotFewerBytes width span.Length) reader
#endif
#endif
