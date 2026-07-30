namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The codec's WRITE seam: the stream, and the unit's type/key tables, as ONE value.
///
/// The tables belong here and not in a parameter because a type reaches the wire only as the
/// id of its row, and interning it is what mints that row — so every writer that names a type
/// needs both, and the pairing between them is an invariant rather than a convention each call
/// site restates. `FrozenCodec.writePools` has two streams live at once (the body buffer and
/// the blob) and one builder; bundling is what makes "emit into this stream while interning
/// into that unit's tables" unstateable rather than merely unwritten.
///
/// The `Write` overloads forward the stream verbatim. They exist so that the whole frozen
/// codec is written against this seam and none of it against a bare `BinaryWriter`, which is
/// what keeps the tables out of every signature that does not mention a type.
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

/// The codec's READ seam — the mirror of `FrozenWriter`. The tables are FINISHED here rather
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

/// The bottom of the FROZEN binary codec: the `FrozenWriter`/`FrozenReader` seam above, the
/// generic length- and tag-prefixed container conventions every domain reuses, and the
/// value structs that carry no children. It names no FROZEN domain beyond those leaves, so
/// it reads none of its siblings: `FrozenCodecRows`, `FrozenCodecTypes`, `FrozenCodecDecls`
/// and `FrozenCodec` all bottom out here, never the reverse.
module FrozenCodecPrimitives =

    // ── stream primitives ──────────────────────────────────────────────────

    /// Serialize `x` to a fresh byte array through `write`, against a sink interning into
    /// `types`. The bytes and the tables come back separately on purpose: what `write` met
    /// may have appended rows, and only the caller knows where those belong in the blob
    /// (`FrozenCodec.writePools` emits them in FRONT of the body it interned them from).
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

    /// Length-prefixed `EqArray` writer — the emit mirror of `readArrayWith`. Generic over
    /// the element writer, so every array domain (FrozenType children, string paths) shares
    /// the one length+loop convention.
    let writeEqArrayWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (xs: EqArray<'a>) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let readArrayWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : 'a[] =
        let n = r.ReadInt32()
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        arr

    /// The `EqSet` twin — same length prefix, members in the set's own stored order, which
    /// for a union is the order it was declared in. The reader lands them back in an `EqSet`,
    /// which is where set identity lives; nothing here re-derives it.
    let writeEqSetWith (w: FrozenWriter) (writeElem: FrozenWriter -> 'a -> unit) (xs: EqSet<'a>) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let readEqSetWith (r: FrozenReader) (readElem: FrozenReader -> 'a) : EqSet<'a> =
        EqSet.ofSeq (readArrayWith r readElem)

    /// The `ImmutableArray` twin — the shape the unit's stored type/key tables take
    /// (`FrozenTypeRows`), and so the only container the row codec frames with. Same length
    /// prefix as its two siblings above; the reader builds AT the final length and freezes
    /// in place, so the immutability costs no copy.
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

    // ── container helpers (option / voption / list) ────────────────────────
    //
    // All length- or tag-prefixed, mirroring `writeEqArrayWith`/`readArrayWith`: the
    // reader consumes exactly what the writer emitted, in order. The keyed container —
    // the `SymbolKey`-keyed dictionary — sits with the key cluster it needs, in
    // `FrozenCodecTypes`.

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
        // Read into a mutable buffer in emit order, then freeze to a list — a list
        // comprehension over `1..n` would also read in order, but the explicit loop
        // makes the writer/reader order correspondence unmistakable.
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        List.ofArray arr

    // ── value structs (leaves that carry no children) ──────────────────────

    /// A diagnostic's position: a case tag plus that case's token indices. Like an
    /// anchor, this resolves against the SAME `Lexed` the writer indexed, because the
    /// blob is keyed by the source hash.
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

    /// A binder's dense pool index — the identity every pooled REFERENCE to a definition
    /// site is written as.
    let writeBinderId (w: FrozenWriter) (BinderId i) = w.Write i
    let readBinderId (r: FrozenReader) : BinderId = BinderId(r.ReadInt32())

    /// A declaration shape's key SLOT: the same id on the wire, re-admitted as the binder
    /// key the slot is typed by (`BinderKey.ofInterned` — the one seam that rebuilds one
    /// from a bare identity, because a decoded column has no key to project from).
    let writeBinderSlot (w: FrozenWriter) (k: BinderKeyG<BinderId>) = writeBinderId w (BinderKey.identity k)

    let readBinderSlot (r: FrozenReader) : BinderKeyG<BinderId> = BinderKey.ofInterned (readBinderId r)

    /// A node's anchor: a bare token index (`Anchor`), absence and all. The blob is keyed
    /// by the source hash, so the `Lexed` a reader resolves it against is the same one the
    /// writer indexed — which is what lets the token's text, span and kind stay out of the
    /// blob entirely rather than being written beside every node.
    let writeAnchor (w: FrozenWriter) (a: Anchor) = w.Write(Anchor.toStored a)
    let readAnchor (r: FrozenReader) : Anchor = Anchor.ofStored (r.ReadInt32())

    /// The file a set of FOREIGN anchors index (`TSpecializationG.Origin`) — the case the
    /// paragraph above does not cover. Those anchors index a file that is NOT the one the blob
    /// is keyed by, so the identity of that file, and a hash of the contents the indices were
    /// taken against, have to be in the blob: they are the only thing a later build can check
    /// its re-read of that file against.
    let writeOriginFile (w: FrozenWriter) (f: OriginFile) =
        w.Write f.Path.BucketName
        w.Write f.Path.Relative
        w.Write f.Path.Absolute
        w.Write f.Content.Hex

    let readOriginFile (r: FrozenReader) : OriginFile =
        let bucket = r.ReadString()
        let relative = r.ReadString()
        let absolute = r.ReadString()
        let hex = r.ReadString()

        {
            Path =
                {
                    BucketName = bucket
                    Relative = relative
                    Absolute = absolute
                }
            Content = InputHash.ofHex hex
        }

    let writeTyparAxis (w: FrozenWriter) (a: TyparAxis) =
        match a with
        | TyparAxis.Declaring -> w.Write 0uy
        | TyparAxis.Method -> w.Write 1uy

    let readTyparAxis (r: FrozenReader) : TyparAxis =
        match r.ReadByte() with
        | 0uy -> TyparAxis.Declaring
        | 1uy -> TyparAxis.Method
        | b -> failwithf "FrozenCodec: unknown TyparAxis tag %d" b

    let writeStringList (w: FrozenWriter) (xs: string list) =
        writeListWith w (fun w (s: string) -> w.Write s) xs

    let readStringList (r: FrozenReader) : string list =
        readListWith r (fun r -> r.ReadString())

    let writeStringArray (w: FrozenWriter) (xs: EqArray<string>) =
        writeEqArrayWith w (fun w (s: string) -> w.Write s) xs

    let readStringArray (r: FrozenReader) : EqArray<string> =
        EqArray.ofArray (readArrayWith r (fun r -> r.ReadString()))
