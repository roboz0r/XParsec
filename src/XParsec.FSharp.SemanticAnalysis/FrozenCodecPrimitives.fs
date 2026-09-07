namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open Vesper
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The codec's WRITE seam: the stream and the file's type/key tables as ONE value. A type
/// reaches the wire as the id of its row and interning it is what MINTS that row, so a writer
/// emitting a type needs both; pairing them makes a cross-file intern unstateable.
[<Struct>]
type FrozenWriter =
    {
        Out: BinaryWriter
        Types: FrozenTypeTableBuilder
    }

    member inline this.Write(v: byte) = this.Out.Write v
    member inline this.Write(v: uint16) = this.Out.Write v
    member inline this.Write(v: int) = this.Out.Write v
    member inline this.Write(v: int64) = this.Out.Write v
    member inline this.Write(v: float) = this.Out.Write v
    member inline this.Write(v: float32) = this.Out.Write v
    member inline this.Write(v: decimal) = this.Out.Write v
    member inline this.Write(v: bool) = this.Out.Write v
    member inline this.Write(v: char) = this.Out.Write v
    member inline this.Write(v: string) = this.Out.Write v
    member inline this.Write(buffer: byte[], index: int, count: int) = this.Out.Write(buffer, index, count)

/// The codec's READ seam, the mirror of `FrozenWriter`. The tables are FINISHED here rather
/// than under construction: reading resolves an id, and a blob that named a row its own tables
/// do not carry is a corrupt blob, not a row to mint.
[<Struct>]
type FrozenReader =
    {
        In: BinaryReader
        Types: FrozenTypeTable
    }

    member inline this.ReadByte() = this.In.ReadByte()
    member inline this.ReadUInt16() = this.In.ReadUInt16()
    member inline this.ReadInt32() = this.In.ReadInt32()
    member inline this.ReadInt64() = this.In.ReadInt64()
    member inline this.ReadDouble() = this.In.ReadDouble()
    member inline this.ReadSingle() = this.In.ReadSingle()
    member inline this.ReadDecimal() = this.In.ReadDecimal()
    member inline this.ReadBoolean() = this.In.ReadBoolean()
    member inline this.ReadChar() = this.In.ReadChar()
    member inline this.ReadString() = this.In.ReadString()
    member inline this.ReadBytes(count: int) = this.In.ReadBytes count

/// The bottom of the FROZEN binary codec: the seam above, the generic length- and
/// tag-prefixed container conventions, and the value structs that carry no children. It
/// mentions no frozen domain beyond those leaves.
module FrozenCodecPrimitives =

    // ── stream primitives ──────────────────────────────────────────────────

    /// Serialize `x` to a fresh byte array through `write`, interning into `types`. The bytes
    /// and the tables come back separately: `write` may have appended rows, and only the
    /// caller knows where in the blob those belong.
    let toBytes (types: FrozenTypeTableBuilder) (write: FrozenWriter -> 'a -> unit) (x: 'a) : byte[] =
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)
        write { Out = bw; Types = types } x
        bw.Flush()
        ms.ToArray()

    /// Deserialize a value from `bytes` through `read`, resolving type ids against `types`.
    let ofBytes (types: FrozenTypeTable) (read: FrozenReader -> 'a) (bytes: byte[]) : 'a =
        use ms = new MemoryStream(bytes)
        use br = new BinaryReader(ms)
        read { In = br; Types = types }

    /// Length prefix, then each element. `readArrayWith` is the inverse, returning a bare
    /// array, which every `Block` reader wraps.
    let writeBlockWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (xs: Block<'a>) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let readArrayWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : 'a[] =
        let n = r.ReadInt32()
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        arr

    /// Members in the set's own stored order, which for a union is the order it was declared in.
    /// Nothing here re-derives set identity; the `EqSet` the reader lands them in holds it.
    let writeEqSetWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (xs: EqSet<'a>) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let readEqSetWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : EqSet<'a> =
        EqSet.ofSeq (readArrayWith r readElem)

    /// Same length prefix as its two siblings above; the reader builds AT the final length
    /// and freezes in place, so the immutability costs no copy.
    let writeImmutableWith
        (w: FrozenWriter)
        (writeElem: FrozenWriter -> 'a -> unit)
        (xs: System.Collections.Immutable.ImmutableArray<'a>)
        =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let readImmutableWith
        (r: FrozenReader)
        (readElem: FrozenReader -> 'a)
        : System.Collections.Immutable.ImmutableArray<'a> =
        let n = r.ReadInt32()
        let b = System.Collections.Immutable.ImmutableArray.CreateBuilder<'a>(n)

        for _ in 1..n do
            b.Add(readElem r)

        // Count = Capacity by construction, so this hands over the buffer rather than copying.
        b.MoveToImmutable()

    let readBlockWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : Block<'a> =
        Block.ofImmutable (readImmutableWith r readElem)

    // ── container helpers (option / voption / list) ────────────────────────

    let writeArrayWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (xs: 'a[]) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let writeOptionWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (x: 'a option) =
        match x with
        | None -> w.Write 0uy
        | Some v ->
            w.Write 1uy
            writeElem w v

    let readOptionWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : 'a option =
        match r.ReadByte() with
        | 0uy -> None
        | 1uy -> Some(readElem r)
        | b -> failwithf "FrozenCodec: unknown option tag %d" b

    let writeVOptionWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (x: 'a voption) =
        match x with
        | ValueNone -> w.Write 0uy
        | ValueSome v ->
            w.Write 1uy
            writeElem w v

    let readVOptionWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : 'a voption =
        match r.ReadByte() with
        | 0uy -> ValueNone
        | 1uy -> ValueSome(readElem r)
        | b -> failwithf "FrozenCodec: unknown voption tag %d" b

    let writeListWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (xs: 'a list) =
        w.Write(List.length xs)

        for x in xs do
            writeElem w x

    let readListWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : 'a list =
        let n = r.ReadInt32()
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        List.ofArray arr

    // ── value structs (leaves that carry no children) ──────────────────────

    /// A case tag plus that case's token indices, NORMALISED on the way out and again on the
    /// way in, so an inverted or degenerate range cannot round-trip in two spellings.
    let writeSite (w: FrozenWriter) (s: Site) =
        match Site.normalise s with
        | Site.Nowhere -> w.Write 0uy
        | Site.At t ->
            w.Write 1uy
            w.Write(int t)
        | Site.Between(first, last) ->
            w.Write 2uy
            w.Write(int first)
            w.Write(int last)
        | Site.After t ->
            w.Write 3uy
            w.Write(int t)

    let readSite (r: FrozenReader) : Site =
        match r.ReadByte() with
        | 0uy -> Site.Nowhere
        | 1uy -> Site.At(r.ReadInt32() * 1<token>)
        | 2uy ->
            let first = r.ReadInt32() * 1<token>
            let last = r.ReadInt32() * 1<token>
            Site.between first last
        | 3uy -> Site.After(r.ReadInt32() * 1<token>)
        | b -> failwithf "FrozenCodec: unknown Site tag %d" b

    /// A bound variable's dense pool index: the identity every pooled REFERENCE to a definition
    /// site is written as.
    let writeBoundVarId (w: FrozenWriter) (BoundVarId i) = w.Write i
    let readBoundVarId (r: FrozenReader) : BoundVarId = BoundVarId(r.ReadInt32())

    /// A declaration shape's key SLOT: the same id on the wire, re-admitted as the bound variable key
    /// the slot is typed by, because a decoded column carries no key to project one from.
    let writeBoundVarSlot (w: FrozenWriter) (k: BoundVarKeyG<BoundVarId>) =
        writeBoundVarId w (BoundVarKey.identity k)

    let readBoundVarSlot (r: FrozenReader) : BoundVarKeyG<BoundVarId> =
        BoundVarKey.ofInterned (readBoundVarId r)

    /// A bare token index, absence and all. The blob is keyed by the source hash, so a reader
    /// resolves it against the same `Lexed` the writer indexed, which is what keeps the
    /// token's text, span and kind out of the blob entirely.
    let writeAnchor (w: FrozenWriter) (a: Anchor) = w.Write(Anchor.toStored a)
    let readAnchor (r: FrozenReader) : Anchor = Anchor.ofStored (r.ReadInt32())


    let writeTyparKind (w: FrozenWriter) (k: TyparKind) =
        match k with
        | TyparKind.Type -> w.Write 0uy
        | TyparKind.Measure -> w.Write 1uy

    let readTyparKind (r: FrozenReader) : TyparKind =
        match r.ReadByte() with
        | 0uy -> TyparKind.Type
        | 1uy -> TyparKind.Measure
        | b -> failwithf "FrozenCodec: unknown TyparKind tag %d" b

    /// A `bigint` as its length-prefixed little-endian two's-complement bytes.
    let writeBigInteger (w: FrozenWriter) (n: System.Numerics.BigInteger) =
        let bytes = n.ToByteArray()
        w.Write bytes.Length
        w.Write(bytes, 0, bytes.Length)

    let readBigInteger (r: FrozenReader) : System.Numerics.BigInteger =
        System.Numerics.BigInteger(r.ReadBytes(r.ReadInt32()))

    /// A canonical rational as its numerator and denominator.
    let writeRational (w: FrozenWriter) (q: Rational) =
        writeBigInteger w q.Numerator
        writeBigInteger w q.Denominator

    let readRational (r: FrozenReader) : Rational =
        let n = readBigInteger r
        let d = readBigInteger r
        Rational.create (n, d)

    let writeStringList (w: FrozenWriter) (xs: string list) =
        writeListWith w (fun w (s: string) -> w.Write s) xs

    let readStringList (r: FrozenReader) : string list =
        readListWith r (fun r -> r.ReadString())

    let writeStringVOption (w: FrozenWriter) (s: string voption) =
        writeVOptionWith w (fun w (s: string) -> w.Write s) s

    let readStringVOption (r: FrozenReader) : string voption =
        readVOptionWith r (fun r -> r.ReadString())

    let writeStringArray (w: FrozenWriter) (xs: Block<string>) =
        writeBlockWith w (fun w (s: string) -> w.Write s) xs

    let readStringArray (r: FrozenReader) : Block<string> =
        Block.ofArray (readArrayWith r (fun r -> r.ReadString()))
