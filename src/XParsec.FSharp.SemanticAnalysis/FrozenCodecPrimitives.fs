namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The bottom of the FROZEN binary codec: the `BinaryWriter`/`BinaryReader` seam, the
/// generic length- and tag-prefixed container conventions every domain reuses, and the
/// value structs that carry no children. It names no FROZEN domain beyond those leaves, so
/// it reads none of its siblings: `FrozenCodecTypes`, `FrozenCodecDecls` and `FrozenCodec`
/// all bottom out here, never the reverse.
module FrozenCodecPrimitives =

    // ── stream primitives ──────────────────────────────────────────────────

    /// Serialize `x` to a fresh byte array through `write`.
    let toBytes (write: BinaryWriter -> 'a -> unit) (x: 'a) : byte[] =
        use ms = new MemoryStream()
        use bw = new BinaryWriter(ms)
        write bw x
        bw.Flush()
        ms.ToArray()

    /// Deserialize a value from `bytes` through `read`.
    let ofBytes (read: BinaryReader -> 'a) (bytes: byte[]) : 'a =
        use ms = new MemoryStream(bytes)
        use br = new BinaryReader(ms)
        read br

    /// Length-prefixed `EqArray` writer — the emit mirror of `readArrayWith`. Generic over
    /// the element writer, so every array domain (FrozenType children, string paths) shares
    /// the one length+loop convention.
    let writeEqArrayWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (xs: EqArray<'a>) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let readArrayWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a[] =
        let n = r.ReadInt32()
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        arr

    // ── container helpers (option / voption / list) ────────────────────────
    //
    // All length- or tag-prefixed, mirroring `writeEqArrayWith`/`readArrayWith`: the
    // reader consumes exactly what the writer emitted, in order. The keyed container —
    // the `SymbolKey`-keyed dictionary — sits with the key cluster it needs, in
    // `FrozenCodecTypes`.

    let writeArrayWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (xs: 'a[]) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let writeOptionWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (x: 'a option) =
        match x with
        | None -> w.Write 0uy
        | Some v ->
            w.Write 1uy
            writeElem w v

    let readOptionWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a option =
        match r.ReadByte() with
        | 0uy -> None
        | 1uy -> Some(readElem r)
        | b -> failwithf "FrozenCodec: unknown option tag %d" b

    let writeVOptionWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (x: 'a voption) =
        match x with
        | ValueNone -> w.Write 0uy
        | ValueSome v ->
            w.Write 1uy
            writeElem w v

    let readVOptionWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a voption =
        match r.ReadByte() with
        | 0uy -> ValueNone
        | 1uy -> ValueSome(readElem r)
        | b -> failwithf "FrozenCodec: unknown voption tag %d" b

    let writeListWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (xs: 'a list) =
        w.Write(List.length xs)

        for x in xs do
            writeElem w x

    let readListWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a list =
        let n = r.ReadInt32()
        // Read into a mutable buffer in emit order, then freeze to a list — a list
        // comprehension over `1..n` would also read in order, but the explicit loop
        // makes the writer/reader order correspondence unmistakable.
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        List.ofArray arr

    // ── value structs (leaves that carry no children) ──────────────────────

    /// `NodeKey.Raw` verbatim — the frozen-form dense-id change is a later phase; here
    /// the 64-bit content key is stored as-is, so the key round-trips bit-for-bit.
    let writeNodeKey (w: BinaryWriter) (k: NodeKey) = w.Write k.Raw
    let readNodeKey (r: BinaryReader) : NodeKey = NodeKey(r.ReadUInt64())

    /// A binder's dense pool index — the identity every pooled REFERENCE to a definition
    /// site is written as.
    let writeBinderId (w: BinaryWriter) (BinderId i) = w.Write i
    let readBinderId (r: BinaryReader) : BinderId = BinderId(r.ReadInt32())

    /// A declaration shape's key SLOT: the same id on the wire, re-admitted as the binder
    /// key the slot is typed by (`BinderKey.ofInterned` — the one seam that rebuilds one
    /// from a bare identity, because a decoded column has no key to project from).
    let writeBinderSlot (w: BinaryWriter) (k: BinderKeyG<BinderId>) = writeBinderId w (BinderKey.identity k)

    let readBinderSlot (r: BinaryReader) : BinderKeyG<BinderId> = BinderKey.ofInterned (readBinderId r)

    /// A `SyntaxToken` is its source offset + `Token` enum + `TokenIndex` case; the
    /// `PositionedToken` is rebuilt from `(Token, StartIndex)` via its factory, and the
    /// struct's full value equality makes `read (write t) = t` well-defined.
    let writeSyntaxToken (w: BinaryWriter) (t: SyntaxToken) =
        w.Write t.StartIndex
        w.Write(uint16 t.Token)

        match t.Index with
        | TokenIndex.Regular i ->
            w.Write 0uy
            w.Write(int i)
        | TokenIndex.Virtual -> w.Write 1uy

    let readSyntaxToken (r: BinaryReader) : SyntaxToken =
        let startIndex = r.ReadInt32()
        let token: Token = LanguagePrimitives.EnumOfValue(r.ReadUInt16())
        let positioned = PositionedToken.Create(token, startIndex)

        let index =
            match r.ReadByte() with
            | 0uy -> TokenIndex.Regular(r.ReadInt32() * 1<token>)
            | 1uy -> TokenIndex.Virtual
            | b -> failwithf "FrozenCodec: unknown TokenIndex tag %d" b

        {
            PositionedToken = positioned
            Index = index
        }

    let writeLiteralConst (w: BinaryWriter) (v: LiteralConst) =
        match v with
        | LiteralConst.String s ->
            w.Write 0uy
            w.Write s
        | LiteralConst.Int n ->
            w.Write 1uy
            w.Write n

    let readLiteralConst (r: BinaryReader) : LiteralConst =
        match r.ReadByte() with
        | 0uy -> LiteralConst.String(r.ReadString())
        | 1uy -> LiteralConst.Int(r.ReadInt64())
        | b -> failwithf "FrozenCodec: unknown LiteralConst tag %d" b

    let writeTyparAxis (w: BinaryWriter) (a: TyparAxis) =
        match a with
        | TyparAxis.Declaring -> w.Write 0uy
        | TyparAxis.Method -> w.Write 1uy

    let readTyparAxis (r: BinaryReader) : TyparAxis =
        match r.ReadByte() with
        | 0uy -> TyparAxis.Declaring
        | 1uy -> TyparAxis.Method
        | b -> failwithf "FrozenCodec: unknown TyparAxis tag %d" b

    let writeStringArray (w: BinaryWriter) (xs: EqArray<string>) =
        writeEqArrayWith w (fun w (s: string) -> w.Write s) xs

    let readStringArray (r: BinaryReader) : EqArray<string> =
        EqArray.ofArray (readArrayWith r (fun r -> r.ReadString()))
