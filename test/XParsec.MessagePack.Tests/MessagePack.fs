module MessagePack

open System
open System.Collections.Immutable

// Implementation based on https://github.com/msgpack/msgpack/blob/8aa09e2a6a9180a49fc62ecfefe149f063cc5e4b/spec.md

[<RequireQualifiedAccess>]
[<Struct>]
type Formats =
    | PositiveFixInt of value: byte
    | FixMap of value: byte
    | FixArray of value: byte
    | FixStr of value: byte
    | Nil
    | NeverUsed
    | False
    | True
    | Bin8
    | Bin16
    | Bin32
    | Ext8
    | Ext16
    | Ext32
    | Float32
    | Float64
    | UInt8
    | UInt16
    | UInt32
    | UInt64
    | Int8
    | Int16
    | Int32
    | Int64
    | FixExt1
    | FixExt2
    | FixExt4
    | FixExt8
    | FixExt16
    | Str8
    | Str16
    | Str32
    | Array16
    | Array32
    | Map16
    | Map32
    | NegativeFixInt of value: byte

[<RequireQualifiedAccess>]
type MsgPackValue =
    | Nil
    | True
    | False
    | Bin of byte[]
    | Ext of typeTag: byte * data: byte[]
    | Float32 of float32
    | Float64 of float
    | Int8 of int8
    | Int16 of int16
    | Int32 of int
    | Int64 of int64
    | UInt8 of uint8
    | UInt16 of uint16
    | UInt32 of uint32
    | UInt64 of uint64
    | Str of string
    | Array of ImmutableArray<MsgPackValue>
    | FixMap of byte
    | Map of ImmutableArray<struct (MsgPackValue * MsgPackValue)>


module ArrayParsers =
    open System.Buffers.Binary
    open XParsec
    open XParsec.Parsers

    type PState = unit

    let pFormat () : Parser<_, byte, _, _, _> =
        parser {
            let! x = pid

            return
                match x with
                | x when x &&& 0b10000000uy = 0b00000000uy -> Formats.PositiveFixInt x
                | x when x &&& 0b11110000uy = 0b10000000uy -> Formats.FixMap x
                | x when x &&& 0b11110000uy = 0b10010000uy -> Formats.FixArray x
                | x when x &&& 0b11100000uy = 0b10100000uy -> Formats.FixStr x
                | 0xc0uy -> Formats.Nil
                | 0xc1uy -> Formats.NeverUsed
                | 0xc2uy -> Formats.False
                | 0xc3uy -> Formats.True
                | 0xc4uy -> Formats.Bin8
                | 0xc5uy -> Formats.Bin16
                | 0xc6uy -> Formats.Bin32
                | 0xc7uy -> Formats.Ext8
                | 0xc8uy -> Formats.Ext16
                | 0xc9uy -> Formats.Ext32
                | 0xcauy -> Formats.Float32
                | 0xcbuy -> Formats.Float64
                | 0xccuy -> Formats.UInt8
                | 0xcduy -> Formats.UInt16
                | 0xceuy -> Formats.UInt32
                | 0xcfuy -> Formats.UInt64
                | 0xd0uy -> Formats.Int8
                | 0xd1uy -> Formats.Int16
                | 0xd2uy -> Formats.Int32
                | 0xd3uy -> Formats.Int64
                | 0xd4uy -> Formats.FixExt1
                | 0xd5uy -> Formats.FixExt2
                | 0xd6uy -> Formats.FixExt4
                | 0xd7uy -> Formats.FixExt8
                | 0xd8uy -> Formats.FixExt16
                | 0xd9uy -> Formats.Str8
                | 0xdauy -> Formats.Str16
                | 0xdbuy -> Formats.Str32
                | 0xdcuy -> Formats.Array16
                | 0xdduy -> Formats.Array32
                | 0xdeuy -> Formats.Map16
                | 0xdfuy -> Formats.Map32
                | x when x &&& 0b11100000uy = 0b11100000uy -> Formats.NegativeFixInt x
                | _ -> failwith "Unreachable"
        }


    let pUInt8 (reader: Reader<byte, PState, _, _>) = pid reader

    let gotFewerBytes (expected: int) (actual: int) =
        Message(sprintf "Expected %d bytes but only %d bytes are available." expected actual)

    let pUInt16 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 2

        if span.Length = 2 then
            reader.SkipN 2
            preturn (BinaryPrimitives.ReadUInt16BigEndian(span)) reader
        elif span.Length = 1 then
            fail (gotFewerBytes 2 1) reader
        else
            fail EndOfInput reader

    let pUInt32 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 4

        if span.Length = 4 then
            reader.SkipN 4
            preturn (BinaryPrimitives.ReadUInt32BigEndian(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes 4 span.Length) reader

    let pUInt64 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 8

        if span.Length = 8 then
            reader.SkipN 8
            preturn (BinaryPrimitives.ReadUInt64BigEndian(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes 8 span.Length) reader

    let pInt8 (reader: Reader<byte, PState, _, _>) =
        match reader.TryRead() with
        | ValueSome b -> preturn (int8 b) reader
        | ValueNone -> fail EndOfInput reader

    let pInt16 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 2

        if span.Length = 2 then
            reader.SkipN 2
            preturn (BinaryPrimitives.ReadInt16BigEndian(span)) reader
        elif span.Length = 1 then
            fail (gotFewerBytes 2 1) reader
        else
            fail EndOfInput reader

    let pInt32 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 4

        if span.Length = 4 then
            reader.SkipN 4
            preturn (BinaryPrimitives.ReadInt32BigEndian(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes 4 span.Length) reader

    let pInt64 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 8

        if span.Length = 8 then
            reader.SkipN 8
            preturn (BinaryPrimitives.ReadInt64BigEndian(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes 8 span.Length) reader

    let pFloat32 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 4

        if span.Length = 4 then
            reader.SkipN 4
            preturn (BinaryPrimitives.ReadSingleBigEndian(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes 4 span.Length) reader

    let pFloat64 (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN 8

        if span.Length = 8 then
            reader.SkipN 8
            preturn (BinaryPrimitives.ReadDoubleBigEndian(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes 8 span.Length) reader

    let private pStr (length: int) (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN length

        if span.Length = length then
            reader.SkipN length
            preturn (System.Text.Encoding.UTF8.GetString(span)) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes length span.Length) reader

    let pFixStr (value: byte) (reader: Reader<byte, PState, _, _>) =
        let length = int value &&& 0b00011111
        pStr length reader

    let pStr8 () : Parser<_, byte, _, _, _> =
        parser {
            let! length = pUInt8 |>> int
            let! value = pStr length
            return value
        }

    let pStr16 () : Parser<_, byte, _, _, _> =
        parser {
            let! length = pUInt16 |>> int
            let! value = pStr length
            return value
        }

    let pStr32 () : Parser<_, byte, _, _, _> =
        parser {
            let! length = pUInt32

            if length > uint32 Int32.MaxValue then
                return! fail (Message "String length exceeds Int32.MaxValue.")
            else
                return! pStr (int length)
        }

    let private pBin (length: int) (reader: Reader<byte, PState, _, _>) =
        let span = reader.PeekN length

        if span.Length = length then
            reader.SkipN length
            preturn (span.ToArray()) reader
        elif span.Length = 0 then
            fail EndOfInput reader
        else
            fail (gotFewerBytes length span.Length) reader

    let pBin8 () : Parser<_, byte, _, _, _> =
        parser {
            let! length = pUInt8 |>> int
            let! value = pBin length
            return value
        }

    let pBin16 () : Parser<_, byte, _, _, _> =
        parser {
            let! length = pUInt16 |>> int
            let! value = pBin length
            return value
        }

    let pBin32 () : Parser<_, byte, _, _, _> =
        parser {
            let! length = pUInt32

            if length > uint32 Int32.MaxValue then
                return! fail (Message "Binary length exceeds Int32.MaxValue.")
            else
                return! pBin (int length)
        }

    let rec pObject () : Parser<MsgPackValue, _, _, _, _> =
        parser {
            let pObject = pObject ()
            let! format = pFormat ()

            match format with
            | Formats.PositiveFixInt(value) -> return MsgPackValue.UInt8 value
            | Formats.NegativeFixInt(value) -> return MsgPackValue.Int8(int8 value)
            | Formats.Nil -> return MsgPackValue.Nil
            | Formats.NeverUsed -> return MsgPackValue.Nil
            | Formats.False -> return MsgPackValue.False
            | Formats.True -> return MsgPackValue.True
            | Formats.Bin8 -> return! pBin8 () |>> MsgPackValue.Bin
            | Formats.Bin16 -> return! pBin16 () |>> MsgPackValue.Bin
            | Formats.Bin32 -> return! pBin32 () |>> MsgPackValue.Bin
            | Formats.Ext8 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin8 ()
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.Ext16 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin16 ()
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.Ext32 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin32 ()
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.Float32 -> return! pFloat32 |>> MsgPackValue.Float32
            | Formats.Float64 -> return! pFloat64 |>> MsgPackValue.Float64
            | Formats.UInt8 -> return! pUInt8 |>> MsgPackValue.UInt8
            | Formats.UInt16 -> return! pUInt16 |>> MsgPackValue.UInt16
            | Formats.UInt32 -> return! pUInt32 |>> MsgPackValue.UInt32
            | Formats.UInt64 -> return! pUInt64 |>> MsgPackValue.UInt64
            | Formats.Int8 -> return! pInt8 |>> MsgPackValue.Int8
            | Formats.Int16 -> return! pInt16 |>> MsgPackValue.Int16
            | Formats.Int32 -> return! pInt32 |>> MsgPackValue.Int32
            | Formats.Int64 -> return! pInt64 |>> MsgPackValue.Int64
            | Formats.FixExt1 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin 1
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.FixExt2 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin 2
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.FixExt4 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin 4
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.FixExt8 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin 8
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.FixExt16 ->
                return!
                    parser {
                        let! typeTag = pUInt8
                        let! data = pBin 16
                        return MsgPackValue.Ext(typeTag, data)
                    }
            | Formats.FixStr(value) -> return! pFixStr value |>> MsgPackValue.Str
            | Formats.Str8 -> return! pStr8 () |>> MsgPackValue.Str
            | Formats.Str16 -> return! pStr16 () |>> MsgPackValue.Str
            | Formats.Str32 -> return! pStr32 () |>> MsgPackValue.Str
            | Formats.FixArray(value) ->
                return!
                    parser {
                        let length = int value &&& 0b00001111
                        let! elements = parray length pObject
                        return MsgPackValue.Array(elements)
                    }
            | Formats.Array16 ->
                return!
                    parser {
                        let! length = pUInt16 |>> int
                        let! elements = parray length pObject
                        return MsgPackValue.Array(elements)
                    }
            | Formats.Array32 ->
                return!
                    parser {
                        let! length = pUInt32

                        if length > uint32 Int32.MaxValue then
                            return! fail (Message "Array length exceeds Int32.MaxValue.")
                        else
                            let! elements = parray (int length) pObject
                            return MsgPackValue.Array(elements)
                    }
            | Formats.FixMap(value) ->
                return!
                    parser {
                        let length = int value &&& 0b00001111
                        let! elements = parray length (pObject .>>. pObject)
                        return MsgPackValue.Map(elements)
                    }
            | Formats.Map16 ->
                return!
                    parser {
                        let! length = pUInt16 |>> int
                        let! elements = parray length (pObject .>>. pObject)
                        return MsgPackValue.Map(elements)
                    }
            | Formats.Map32 ->
                return!
                    parser {
                        let! length = pUInt32

                        if length > uint32 Int32.MaxValue then
                            return! fail (Message "Map length exceeds Int32.MaxValue.")
                        else
                            let! elements = parray (int length) (pObject .>>. pObject)
                            return MsgPackValue.Map(elements)
                    }
        }
