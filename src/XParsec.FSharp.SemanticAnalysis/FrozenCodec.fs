namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// A hand-rolled structural binary (de)serializer for the FROZEN domain, layered:
/// the leaf domains (`FrozenType` and the `SymbolKey`/`TypeKey` key cluster it
/// reaches, plus `NodeKey`/`SyntaxToken`), the non-tree declaration shell and scalar
/// clusters a pool payload rides, and — on top of both — the `FrozenPools` COLUMN codec
/// that `flatten`/`thaw` actually store. A plain `BinaryWriter`/`BinaryReader` over a
/// `MemoryStream`; the blob is Brotli-wrapped at the store seam (`Compression`), so
/// nothing here hand-rolls varints or bit-packing.
///
/// The stored form is the pools, not the DU, and the pools are what the front end now
/// yields: `flatten` IS the column writers and `thaw` their inverse, with no conversion on
/// either side. There is NO recursive
/// `TExpr`/`TDecl`/`TPat` tree codec: every tree the file bears is in the columns, so
/// wherever a subtree used to be inlined — a `type` declaration's member bodies, an inline
/// template's decl, a `ValRepr`'s tuple group — a pool id is written instead.
///
/// Two invariants the writer/reader pair upholds:
///   * The writer's `match` is EXHAUSTIVE with no catch-all, so a new `FrozenType`
///     or key case fails to compile here rather than silently mis-serializing; the
///     reader mirrors the same byte-tag discipline case for case.
///   * The reader reconstructs each DU case DIRECTLY, never through a normalizing
///     smart constructor. `FTOr` in particular is rebuilt as `FTOr (EqSet.ofSeq …)`,
///     NOT via `FrozenType.MkUnion` (which flattens / collapses): the gate is
///     STRUCTURAL `read (write x) = x`, so the exact stored set must survive.
[<RequireQualifiedAccess>]
module FrozenCodec =

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
    let private writeEqArrayWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (xs: EqArray<'a>) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let private readArrayWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a[] =
        let n = r.ReadInt32()
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        arr

    // ── value structs (leaves that carry no children) ──────────────────────

    /// `NodeKey.Raw` verbatim — the frozen-form dense-id change is a later phase; here
    /// the 64-bit content key is stored as-is, so the key round-trips bit-for-bit.
    let writeNodeKey (w: BinaryWriter) (k: NodeKey) = w.Write k.Raw
    let readNodeKey (r: BinaryReader) : NodeKey = NodeKey(r.ReadUInt64())

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

    let private writeLiteralConst (w: BinaryWriter) (v: LiteralConst) =
        match v with
        | LiteralConst.String s ->
            w.Write 0uy
            w.Write s
        | LiteralConst.Int n ->
            w.Write 1uy
            w.Write n

    let private readLiteralConst (r: BinaryReader) : LiteralConst =
        match r.ReadByte() with
        | 0uy -> LiteralConst.String(r.ReadString())
        | 1uy -> LiteralConst.Int(r.ReadInt64())
        | b -> failwithf "FrozenCodec: unknown LiteralConst tag %d" b

    let private writeTyparAxis (w: BinaryWriter) (a: TyparAxis) =
        match a with
        | TyparAxis.Declaring -> w.Write 0uy
        | TyparAxis.Method -> w.Write 1uy

    let private readTyparAxis (r: BinaryReader) : TyparAxis =
        match r.ReadByte() with
        | 0uy -> TyparAxis.Declaring
        | 1uy -> TyparAxis.Method
        | b -> failwithf "FrozenCodec: unknown TyparAxis tag %d" b

    let private writeStringArray (w: BinaryWriter) (xs: EqArray<string>) =
        writeEqArrayWith w (fun w (s: string) -> w.Write s) xs

    let private readStringArray (r: BinaryReader) : EqArray<string> =
        EqArray.ofArray (readArrayWith r (fun r -> r.ReadString()))

    // ── the key cluster + FrozenType (one mutually recursive group) ─────────
    //
    // The domains interlock — a `MemberKey.ArgSig` is `EqArray<FrozenType>`, and an
    // `FTConst` carries a `SymbolKey` — so their writers (and readers) are one `rec`
    // group. Readers use explicit `let` sequencing, never positional constructor-arg
    // evaluation, so field read order provably matches the writer's emit order — and each
    // reader sits directly under the writer it must mirror, so "provably" means readable
    // side by side rather than checked across a file.

    let rec writeFrozenType (w: BinaryWriter) (t: FrozenType) =
        match t with
        | FTConst(key, args) ->
            w.Write 0uy
            writeSymbolKey w key
            writeFtArray w args
        | FTFun(arg, result) ->
            w.Write 1uy
            writeFrozenType w arg
            writeFrozenType w result
        | FTTuple items ->
            w.Write 2uy
            writeFtArray w items
        | FTRecord(key, args) ->
            w.Write 3uy
            writeTypeKey w key
            writeFtArray w args
        | FTUnion(key, args) ->
            w.Write 4uy
            writeTypeKey w key
            writeFtArray w args
        | FTClass(key, args) ->
            w.Write 5uy
            writeTypeKey w key
            writeFtArray w args
        | FTEnum key ->
            w.Write 6uy
            writeTypeKey w key
        | FTOr members ->
            w.Write 7uy
            w.Write members.Length

            for i in 0 .. members.Length - 1 do
                writeFrozenType w members.[i]
        | FTLiteral value ->
            w.Write 8uy
            writeLiteralConst w value
        | FTKeyOf ty ->
            w.Write 9uy
            writeFrozenType w ty
        | FTIndexedAccess(objTy, index) ->
            w.Write 10uy
            writeFrozenType w objTy
            writeFrozenType w index
        | FTConditional payload ->
            w.Write 11uy
            writeFrozenType w payload.Check
            writeFrozenType w payload.Extends
            writeFrozenType w payload.WhenTrue
            writeFrozenType w payload.WhenFalse
        | FTTypar(axis, index) ->
            w.Write 12uy
            writeTyparAxis w axis
            w.Write index
        | FTLocalTypar(binder, index) ->
            w.Write 13uy
            writeNodeKey w binder
            w.Write index
        | FTUnknown name ->
            w.Write 14uy
            w.Write name

    and readFrozenType (r: BinaryReader) : FrozenType =
        match r.ReadByte() with
        | 0uy ->
            let key = readSymbolKey r
            let args = readFtArray r
            FTConst(key, args)
        | 1uy ->
            let arg = readFrozenType r
            let result = readFrozenType r
            FTFun(arg, result)
        | 2uy -> FTTuple(readFtArray r)
        | 3uy ->
            let key = readTypeKey r
            let args = readFtArray r
            FTRecord(key, args)
        | 4uy ->
            let key = readTypeKey r
            let args = readFtArray r
            FTUnion(key, args)
        | 5uy ->
            let key = readTypeKey r
            let args = readFtArray r
            FTClass(key, args)
        | 6uy -> FTEnum(readTypeKey r)
        // Rebuilt DIRECTLY, never through `FrozenType.MkUnion` (which flattens /
        // dedupes / collapses): the stored member set must survive verbatim for the
        // structural round-trip gate.
        | 7uy -> FTOr(EqSet.ofSeq (readArrayWith r readFrozenType))
        | 8uy -> FTLiteral(readLiteralConst r)
        | 9uy -> FTKeyOf(readFrozenType r)
        | 10uy ->
            let objTy = readFrozenType r
            let index = readFrozenType r
            FTIndexedAccess(objTy, index)
        | 11uy ->
            let check = readFrozenType r
            let extends = readFrozenType r
            let whenTrue = readFrozenType r
            let whenFalse = readFrozenType r

            FTConditional
                {
                    Check = check
                    Extends = extends
                    WhenTrue = whenTrue
                    WhenFalse = whenFalse
                }
        | 12uy ->
            let axis = readTyparAxis r
            let index = r.ReadInt32()
            FTTypar(axis, index)
        | 13uy ->
            let binder = readNodeKey r
            let index = r.ReadInt32()
            FTLocalTypar(binder, index)
        | 14uy -> FTUnknown(r.ReadString())
        | b -> failwithf "FrozenCodec: unknown FrozenType tag %d" b

    and private writeFtArray (w: BinaryWriter) (xs: EqArray<FrozenType>) = writeEqArrayWith w writeFrozenType xs

    and private readFtArray (r: BinaryReader) : EqArray<FrozenType> =
        EqArray.ofArray (readArrayWith r readFrozenType)

    and writeSymbolKey (w: BinaryWriter) (k: SymbolKey) =
        match k with
        | SymbolKey.Type tk ->
            w.Write 0uy
            writeTypeKey w tk
        | SymbolKey.Binding bk ->
            w.Write 1uy
            writeBindingKey w bk
        | SymbolKey.Member mk ->
            w.Write 2uy
            writeMemberKey w mk

    and readSymbolKey (r: BinaryReader) : SymbolKey =
        match r.ReadByte() with
        | 0uy -> SymbolKey.Type(readTypeKey r)
        | 1uy -> SymbolKey.Binding(readBindingKey r)
        | 2uy -> SymbolKey.Member(readMemberKey r)
        | b -> failwithf "FrozenCodec: unknown SymbolKey tag %d" b

    and writeTypeKey (w: BinaryWriter) (k: TypeKey) =
        writeTypeHolder w k.Holder
        w.Write k.Name
        w.Write k.TyparArity

    and readTypeKey (r: BinaryReader) : TypeKey =
        let holder = readTypeHolder r
        let name = r.ReadString()
        let arity = r.ReadInt32()

        {
            Holder = holder
            Name = name
            TyparArity = arity
        }

    and private writeTypeHolder (w: BinaryWriter) (h: TypeHolder) =
        match h with
        | TypeHolder.InNamespace ns ->
            w.Write 0uy
            writeNamespaceKey w ns
        | TypeHolder.InModule m ->
            w.Write 1uy
            writeModuleKey w m
        | TypeHolder.InType outer ->
            w.Write 2uy
            writeTypeKey w outer

    and private readTypeHolder (r: BinaryReader) : TypeHolder =
        match r.ReadByte() with
        | 0uy -> TypeHolder.InNamespace(readNamespaceKey r)
        | 1uy -> TypeHolder.InModule(readModuleKey r)
        | 2uy -> TypeHolder.InType(readTypeKey r)
        | b -> failwithf "FrozenCodec: unknown TypeHolder tag %d" b

    and private writeNamespaceKey (w: BinaryWriter) (ns: NamespaceKey) = writeStringArray w ns.Path

    and private readNamespaceKey (r: BinaryReader) : NamespaceKey = { Path = readStringArray r }

    and private writeModuleKey (w: BinaryWriter) (m: ModuleKey) =
        writeModuleHolder w m.Holder
        w.Write m.Name

    and private readModuleKey (r: BinaryReader) : ModuleKey =
        let holder = readModuleHolder r
        let name = r.ReadString()
        { Holder = holder; Name = name }

    and private writeModuleHolder (w: BinaryWriter) (h: ModuleHolder) =
        match h with
        | ModuleHolder.InNamespace ns ->
            w.Write 0uy
            writeNamespaceKey w ns
        | ModuleHolder.InModule parent ->
            w.Write 1uy
            writeModuleKey w parent

    and private readModuleHolder (r: BinaryReader) : ModuleHolder =
        match r.ReadByte() with
        | 0uy -> ModuleHolder.InNamespace(readNamespaceKey r)
        | 1uy -> ModuleHolder.InModule(readModuleKey r)
        | b -> failwithf "FrozenCodec: unknown ModuleHolder tag %d" b

    and private writeBindingKey (w: BinaryWriter) (b: BindingKey) =
        writeModuleHolder w b.Decl
        w.Write b.Name

    and private readBindingKey (r: BinaryReader) : BindingKey =
        let decl = readModuleHolder r
        let name = r.ReadString()
        { Decl = decl; Name = name }

    and private writeMemberKey (w: BinaryWriter) (m: MemberKey) =
        writeTypeKey w m.Decl
        w.Write m.Name
        writeFtArray w m.ArgSig
        w.Write m.MethodTyparArity
        writeMemberKind w m.Kind

    and private readMemberKey (r: BinaryReader) : MemberKey =
        let decl = readTypeKey r
        let name = r.ReadString()
        let argSig = readFtArray r
        let methodTyparArity = r.ReadInt32()
        let kind = readMemberKind r

        {
            Decl = decl
            Name = name
            ArgSig = argSig
            MethodTyparArity = methodTyparArity
            Kind = kind
        }

    and private writeMemberKind (w: BinaryWriter) (k: MemberKind) =
        match k with
        | MemberKind.Method -> w.Write 0uy
        | MemberKind.Property -> w.Write 1uy
        | MemberKind.InterfaceMethod iface ->
            w.Write 2uy
            writeTypeKey w iface
        | MemberKind.ExplicitInterfaceImpl iface ->
            w.Write 3uy
            writeTypeKey w iface

    and private readMemberKind (r: BinaryReader) : MemberKind =
        match r.ReadByte() with
        | 0uy -> MemberKind.Method
        | 1uy -> MemberKind.Property
        | 2uy -> MemberKind.InterfaceMethod(readTypeKey r)
        | 3uy -> MemberKind.ExplicitInterfaceImpl(readTypeKey r)
        | b -> failwithf "FrozenCodec: unknown MemberKind tag %d" b

    // ── container helpers (option / voption / list / dictionary) ───────────
    //
    // All length- or tag-prefixed, mirroring `writeEqArrayWith`/`readArrayWith`: the
    // reader consumes exactly what the writer emitted, in order. The two
    // `IReadOnlyDictionary<SymbolKey,_>` fields serialize as a length-prefixed
    // (key, value) sequence — no canonical order is imposed (the cache key hashes
    // inputs, not the blob), so emit order is free and read rebuilds an unordered map.

    let private writeArrayWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (xs: 'a[]) =
        w.Write xs.Length

        for i in 0 .. xs.Length - 1 do
            writeElem w xs.[i]

    let private writeOptionWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (x: 'a option) =
        match x with
        | None -> w.Write 0uy
        | Some v ->
            w.Write 1uy
            writeElem w v

    let private readOptionWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a option =
        match r.ReadByte() with
        | 0uy -> None
        | 1uy -> Some(readElem r)
        | b -> failwithf "FrozenCodec: unknown option tag %d" b

    let private writeVOptionWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (x: 'a voption) =
        match x with
        | ValueNone -> w.Write 0uy
        | ValueSome v ->
            w.Write 1uy
            writeElem w v

    let private readVOptionWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a voption =
        match r.ReadByte() with
        | 0uy -> ValueNone
        | 1uy -> ValueSome(readElem r)
        | b -> failwithf "FrozenCodec: unknown voption tag %d" b

    let private writeListWith (w: BinaryWriter) (writeElem: BinaryWriter -> 'a -> unit) (xs: 'a list) =
        w.Write(List.length xs)

        for x in xs do
            writeElem w x

    let private readListWith (r: BinaryReader) (readElem: BinaryReader -> 'a) : 'a list =
        let n = r.ReadInt32()
        // Read into a mutable buffer in emit order, then freeze to a list — a list
        // comprehension over `1..n` would also read in order, but the explicit loop
        // makes the writer/reader order correspondence unmistakable.
        let arr = Array.zeroCreate n

        for i in 0 .. n - 1 do
            arr.[i] <- readElem r

        List.ofArray arr

    /// The two `IReadOnlyDictionary<SymbolKey,_>` fields — rebuilt on read as a
    /// concrete `Dictionary` exposed through the read-only face, exactly how
    /// `Elaborate` constructs `IntrinsicReprKeys` / `Accessibility`.
    let private writeSymbolDict
        (w: BinaryWriter)
        (writeVal: BinaryWriter -> 'v -> unit)
        (d: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>)
        =
        w.Write d.Count

        for KeyValue(k, v) in d do
            writeSymbolKey w k
            writeVal w v

    let private readSymbolDict
        (r: BinaryReader)
        (readVal: BinaryReader -> 'v)
        : System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v> =
        let n = r.ReadInt32()
        let d = System.Collections.Generic.Dictionary<SymbolKey, 'v>(n)

        for _ in 1..n do
            let k = readSymbolKey r
            let v = readVal r
            d.[k] <- v

        d :> System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>

    // ── non-generic leaf payloads the tree / side tables carry ──────────────

    let private writeIntWidth (w: BinaryWriter) (iw: IntWidth) =
        match iw with
        | IntWidth.SByte -> w.Write 0uy
        | IntWidth.Byte -> w.Write 1uy
        | IntWidth.Int16 -> w.Write 2uy
        | IntWidth.UInt16 -> w.Write 3uy
        | IntWidth.Int32 -> w.Write 4uy
        | IntWidth.UInt32 -> w.Write 5uy
        | IntWidth.Int64 -> w.Write 6uy
        | IntWidth.UInt64 -> w.Write 7uy
        | IntWidth.NativeInt -> w.Write 8uy
        | IntWidth.UNativeInt -> w.Write 9uy

    let private readIntWidth (r: BinaryReader) : IntWidth =
        match r.ReadByte() with
        | 0uy -> IntWidth.SByte
        | 1uy -> IntWidth.Byte
        | 2uy -> IntWidth.Int16
        | 3uy -> IntWidth.UInt16
        | 4uy -> IntWidth.Int32
        | 5uy -> IntWidth.UInt32
        | 6uy -> IntWidth.Int64
        | 7uy -> IntWidth.UInt64
        | 8uy -> IntWidth.NativeInt
        | 9uy -> IntWidth.UNativeInt
        | b -> failwithf "FrozenCodec: unknown IntWidth tag %d" b

    let private writeTConstValue (w: BinaryWriter) (v: TConstValue) =
        match v with
        | TConstValue.Integral(width, bits) ->
            w.Write 0uy
            writeIntWidth w width
            w.Write bits
        | TConstValue.Float d ->
            w.Write 1uy
            w.Write d
        | TConstValue.Float32 f ->
            w.Write 2uy
            w.Write f
        | TConstValue.Bool b ->
            w.Write 3uy
            w.Write b
        | TConstValue.Char c ->
            w.Write 4uy
            w.Write c
        | TConstValue.Decimal d ->
            w.Write 5uy
            w.Write d
        | TConstValue.String s ->
            w.Write 6uy
            w.Write s
        | TConstValue.Unit -> w.Write 7uy

    let private readTConstValue (r: BinaryReader) : TConstValue =
        match r.ReadByte() with
        | 0uy ->
            let width = readIntWidth r
            let bits = r.ReadInt64()
            TConstValue.Integral(width, bits)
        | 1uy -> TConstValue.Float(r.ReadDouble())
        | 2uy -> TConstValue.Float32(r.ReadSingle())
        | 3uy -> TConstValue.Bool(r.ReadBoolean())
        | 4uy -> TConstValue.Char(r.ReadChar())
        | 5uy -> TConstValue.Decimal(r.ReadDecimal())
        | 6uy -> TConstValue.String(r.ReadString())
        | 7uy -> TConstValue.Unit
        | b -> failwithf "FrozenCodec: unknown TConstValue tag %d" b

    let private writeSeverity (w: BinaryWriter) (s: Severity) =
        match s with
        | Severity.Error -> w.Write 0uy
        | Severity.Warning -> w.Write 1uy
        | Severity.Info -> w.Write 2uy

    let private readSeverity (r: BinaryReader) : Severity =
        match r.ReadByte() with
        | 0uy -> Severity.Error
        | 1uy -> Severity.Warning
        | 2uy -> Severity.Info
        | b -> failwithf "FrozenCodec: unknown Severity tag %d" b

    // Qualified: this module `open`s `XParsec.FSharp.Parser`, which also declares a
    // `Diagnostic`; the bare name would bind to the parser's — the same shadowing the
    // `TastFileG.Diagnostics` field annotation guards against.
    let private writeDiagnostic (w: BinaryWriter) (d: XParsec.FSharp.SemanticAnalysis.Diagnostic) =
        writeNodeKey w d.Key
        w.Write d.Code
        w.Write d.Message
        writeSeverity w d.Severity

    let private readDiagnostic (r: BinaryReader) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
        let key = readNodeKey r
        let code = r.ReadString()
        let message = r.ReadString()
        let severity = readSeverity r

        {
            Key = key
            Code = code
            Message = message
            Severity = severity
        }

    let private writeAccessibility (w: BinaryWriter) (a: Accessibility) =
        match a with
        | Accessibility.Public -> w.Write 0uy
        | Accessibility.Internal -> w.Write 1uy
        | Accessibility.Private -> w.Write 2uy

    let private readAccessibility (r: BinaryReader) : Accessibility =
        match r.ReadByte() with
        | 0uy -> Accessibility.Public
        | 1uy -> Accessibility.Internal
        | 2uy -> Accessibility.Private
        | b -> failwithf "FrozenCodec: unknown Accessibility tag %d" b

    let private writeClassValueKind (w: BinaryWriter) (k: ClassValueKind) =
        match k with
        | ClassValueKind.RefType -> w.Write 0uy
        | ClassValueKind.Struct -> w.Write 1uy
        | ClassValueKind.RefStruct -> w.Write 2uy

    let private readClassValueKind (r: BinaryReader) : ClassValueKind =
        match r.ReadByte() with
        | 0uy -> ClassValueKind.RefType
        | 1uy -> ClassValueKind.Struct
        | 2uy -> ClassValueKind.RefStruct
        | b -> failwithf "FrozenCodec: unknown ClassValueKind tag %d" b

    let private writeEqualityVerdict (w: BinaryWriter) (v: EqualityVerdict) =
        match v with
        | EqualityVerdict.Structural -> w.Write 0uy
        | EqualityVerdict.Reference -> w.Write 1uy
        | EqualityVerdict.Custom -> w.Write 2uy
        | EqualityVerdict.NoEquality -> w.Write 3uy

    let private readEqualityVerdict (r: BinaryReader) : EqualityVerdict =
        match r.ReadByte() with
        | 0uy -> EqualityVerdict.Structural
        | 1uy -> EqualityVerdict.Reference
        | 2uy -> EqualityVerdict.Custom
        | 3uy -> EqualityVerdict.NoEquality
        | b -> failwithf "FrozenCodec: unknown EqualityVerdict tag %d" b

    let private writeComparisonVerdict (w: BinaryWriter) (v: ComparisonVerdict) =
        match v with
        | ComparisonVerdict.Structural -> w.Write 0uy
        | ComparisonVerdict.Custom -> w.Write 1uy
        | ComparisonVerdict.NoComparison -> w.Write 2uy

    let private readComparisonVerdict (r: BinaryReader) : ComparisonVerdict =
        match r.ReadByte() with
        | 0uy -> ComparisonVerdict.Structural
        | 1uy -> ComparisonVerdict.Custom
        | 2uy -> ComparisonVerdict.NoComparison
        | b -> failwithf "FrozenCodec: unknown ComparisonVerdict tag %d" b

    let private writeMemberStorage (w: BinaryWriter) (s: MemberStorage) =
        match s with
        | MemberStorage.Field -> w.Write 0uy
        | MemberStorage.Property -> w.Write 1uy
        | MemberStorage.Method -> w.Write 2uy

    let private readMemberStorage (r: BinaryReader) : MemberStorage =
        match r.ReadByte() with
        | 0uy -> MemberStorage.Field
        | 1uy -> MemberStorage.Property
        | 2uy -> MemberStorage.Method
        | b -> failwithf "FrozenCodec: unknown MemberStorage tag %d" b

    let private writeTMemberKind (w: BinaryWriter) (k: TMemberKind) =
        match k with
        | TMemberKind.Method -> w.Write 0uy
        | TMemberKind.Property -> w.Write 1uy

    let private readTMemberKind (r: BinaryReader) : TMemberKind =
        match r.ReadByte() with
        | 0uy -> TMemberKind.Method
        | 1uy -> TMemberKind.Property
        | b -> failwithf "FrozenCodec: unknown TMemberKind tag %d" b

    let private writeClosureRepr (w: BinaryWriter) (c: ClosureRepr) =
        match c with
        | ClosureRepr.Heap -> w.Write 0uy
        | ClosureRepr.Stack -> w.Write 1uy

    let private readClosureRepr (r: BinaryReader) : ClosureRepr =
        match r.ReadByte() with
        | 0uy -> ClosureRepr.Heap
        | 1uy -> ClosureRepr.Stack
        | b -> failwithf "FrozenCodec: unknown ClosureRepr tag %d" b

    let private writeFunVerdict (w: BinaryWriter) (v: FunVerdict) =
        w.Write v.Arity
        writeVOptionWith w (fun w (i: int) -> w.Write i) v.ResultTyparPos

    let private readFunVerdict (r: BinaryReader) : FunVerdict =
        let arity = r.ReadInt32()
        let resultTyparPos = readVOptionWith r (fun r -> r.ReadInt32())

        {
            Arity = arity
            ResultTyparPos = resultTyparPos
        }

    /// A frozen typar bound — its `target` is a `FrozenType`, so this reuses the leaf
    /// `writeFrozenType`/`readFrozenType` defined above.
    let private writeFrozenConstraint (w: BinaryWriter) (c: FrozenConstraint) =
        match c with
        | FrozenConstraint.Coercion(typarIndex, target) ->
            w.Write 0uy
            w.Write typarIndex
            writeFrozenType w target

    let private readFrozenConstraint (r: BinaryReader) : FrozenConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typarIndex = r.ReadInt32()
            let target = readFrozenType r
            FrozenConstraint.Coercion(typarIndex, target)
        | b -> failwithf "FrozenCodec: unknown FrozenConstraint tag %d" b

    let private writeModuleBindingInfo (w: BinaryWriter) (m: ModuleBindingInfo) =
        writeModuleKey w m.Holder
        w.Write m.Name

    let private readModuleBindingInfo (r: BinaryReader) : ModuleBindingInfo =
        let holder = readModuleKey r
        let name = r.ReadString()
        { Holder = holder; Name = name }

    let private writeIntrinsicReprInfo (w: BinaryWriter) (i: IntrinsicReprInfo) =
        w.Write i.Platform
        w.Write i.Heritable

    let private readIntrinsicReprInfo (r: BinaryReader) : IntrinsicReprInfo =
        let platform = r.ReadString()
        let heritable = r.ReadBoolean()

        {
            Platform = platform
            Heritable = heritable
        }

    let private writeTEnumLiteral (w: BinaryWriter) (l: TEnumLiteral) =
        match l with
        | TEnumLiteral.Int value ->
            w.Write 0uy
            writeTConstValue w value
        | TEnumLiteral.String value ->
            w.Write 1uy
            w.Write value

    let private readTEnumLiteral (r: BinaryReader) : TEnumLiteral =
        match r.ReadByte() with
        | 0uy -> TEnumLiteral.Int(readTConstValue r)
        | 1uy -> TEnumLiteral.String(r.ReadString())
        | b -> failwithf "FrozenCodec: unknown TEnumLiteral tag %d" b

    let private writeParamAttrs (w: BinaryWriter) (a: ParamAttrs) = w.Write a.CallAtMostOnce

    let private readParamAttrs (r: BinaryReader) : ParamAttrs = { CallAtMostOnce = r.ReadBoolean() }

    /// A member's own method typars: each entry is the source name + the typar's
    /// frozen type (`FTTypar(Method, i)`), position = ABI index. Plain frozen data —
    /// no union-find cell rides the tree, so this round-trips structurally.
    let private writeMethodTypeParams (w: BinaryWriter) (mtps: EqArray<string * FrozenType>) =
        writeEqArrayWith
            w
            (fun w (n: string, ty) ->
                w.Write n
                writeFrozenType w ty
            )
            mtps

    let private readMethodTypeParams (r: BinaryReader) : EqArray<string * FrozenType> =
        EqArray.ofArray (readArrayWith r (fun r -> let n = r.ReadString() in n, readFrozenType r))

    // ── the printf hole-form cluster (a `HoleSpec` payload) ─────────────────

    let private writePrintWidth (w: BinaryWriter) (p: PrintfHoleForm.PrintWidth) =
        match p with
        | PrintfHoleForm.PrintWidth.Default -> w.Write 0uy
        | PrintfHoleForm.PrintWidth.Never -> w.Write 1uy
        | PrintfHoleForm.PrintWidth.Cols n ->
            w.Write 2uy
            w.Write n
        | PrintfHoleForm.PrintWidth.Star -> w.Write 3uy

    let private readPrintWidth (r: BinaryReader) : PrintfHoleForm.PrintWidth =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.PrintWidth.Default
        | 1uy -> PrintfHoleForm.PrintWidth.Never
        | 2uy -> PrintfHoleForm.PrintWidth.Cols(r.ReadInt32())
        | 3uy -> PrintfHoleForm.PrintWidth.Star
        | b -> failwithf "FrozenCodec: unknown PrintWidth tag %d" b

    let private writePrintSize (w: BinaryWriter) (p: PrintfHoleForm.PrintSize) =
        match p with
        | PrintfHoleForm.PrintSize.Default -> w.Write 0uy
        | PrintfHoleForm.PrintSize.Cols n ->
            w.Write 1uy
            w.Write n
        | PrintfHoleForm.PrintSize.Star -> w.Write 2uy

    let private readPrintSize (r: BinaryReader) : PrintfHoleForm.PrintSize =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.PrintSize.Default
        | 1uy -> PrintfHoleForm.PrintSize.Cols(r.ReadInt32())
        | 2uy -> PrintfHoleForm.PrintSize.Star
        | b -> failwithf "FrozenCodec: unknown PrintSize tag %d" b

    let private writePrec (w: BinaryWriter) (p: PrintfHoleForm.Prec) =
        match p with
        | PrintfHoleForm.Prec.Const n ->
            w.Write 0uy
            w.Write n
        | PrintfHoleForm.Prec.Star -> w.Write 1uy

    let private readPrec (r: BinaryReader) : PrintfHoleForm.Prec =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.Prec.Const(r.ReadInt32())
        | 1uy -> PrintfHoleForm.Prec.Star
        | b -> failwithf "FrozenCodec: unknown Prec tag %d" b

    let private writeRadix (w: BinaryWriter) (radix: PrintfHoleForm.Radix) =
        match radix with
        | PrintfHoleForm.Radix.Hex upper ->
            w.Write 0uy
            w.Write upper
        | PrintfHoleForm.Radix.Binary -> w.Write 1uy
        | PrintfHoleForm.Radix.Octal -> w.Write 2uy

    let private readRadix (r: BinaryReader) : PrintfHoleForm.Radix =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.Radix.Hex(r.ReadBoolean())
        | 1uy -> PrintfHoleForm.Radix.Binary
        | 2uy -> PrintfHoleForm.Radix.Octal
        | b -> failwithf "FrozenCodec: unknown Radix tag %d" b

    let private writeAlignment (w: BinaryWriter) (a: PrintfHoleForm.Alignment) =
        match a with
        | PrintfHoleForm.Alignment.None -> w.Write 0uy
        | PrintfHoleForm.Alignment.Const n ->
            w.Write 1uy
            w.Write n
        | PrintfHoleForm.Alignment.Star leftJustify ->
            w.Write 2uy
            w.Write leftJustify

    let private readAlignment (r: BinaryReader) : PrintfHoleForm.Alignment =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.Alignment.None
        | 1uy -> PrintfHoleForm.Alignment.Const(r.ReadInt32())
        | 2uy -> PrintfHoleForm.Alignment.Star(r.ReadBoolean())
        | b -> failwithf "FrozenCodec: unknown Alignment tag %d" b

    let private writeFieldFormat (w: BinaryWriter) (f: PrintfHoleForm.FieldFormat) =
        match f with
        | PrintfHoleForm.FieldFormat.Verbatim -> w.Write 0uy
        | PrintfHoleForm.FieldFormat.DecimalZeroPad width ->
            w.Write 1uy
            w.Write width
        | PrintfHoleForm.FieldFormat.IntRadix(radix, zeroPad) ->
            w.Write 2uy
            writeRadix w radix
            writeOptionWith w (fun w (n: int) -> w.Write n) zeroPad
        | PrintfHoleForm.FieldFormat.Unsigned zeroPad ->
            w.Write 3uy
            writeOptionWith w (fun w (n: int) -> w.Write n) zeroPad
        | PrintfHoleForm.FieldFormat.Bool -> w.Write 4uy
        | PrintfHoleForm.FieldFormat.Fixed precision ->
            w.Write 5uy
            writePrec w precision
        | PrintfHoleForm.FieldFormat.FixedZeroPad(precision, width) ->
            w.Write 6uy
            w.Write precision
            w.Write width
        | PrintfHoleForm.FieldFormat.FixedRightZeroPad(precision, width) ->
            w.Write 7uy
            w.Write precision
            w.Write width
        | PrintfHoleForm.FieldFormat.Exponential(precision, upper) ->
            w.Write 8uy
            writePrec w precision
            w.Write upper
        | PrintfHoleForm.FieldFormat.Compact(precision, upper) ->
            w.Write 9uy
            writePrec w precision
            w.Write upper
        | PrintfHoleForm.FieldFormat.ExpCompactZeroPad(precision, width, typeChar) ->
            w.Write 10uy
            w.Write precision
            w.Write width
            w.Write typeChar
        | PrintfHoleForm.FieldFormat.ForcedSign(space, precision, typeChar, zeroPad) ->
            w.Write 11uy
            w.Write space
            writePrec w precision
            w.Write typeChar
            writeOptionWith w (fun w (n: int) -> w.Write n) zeroPad

    let private readFieldFormat (r: BinaryReader) : PrintfHoleForm.FieldFormat =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.FieldFormat.Verbatim
        | 1uy -> PrintfHoleForm.FieldFormat.DecimalZeroPad(r.ReadInt32())
        | 2uy ->
            let radix = readRadix r
            let zeroPad = readOptionWith r (fun r -> r.ReadInt32())
            PrintfHoleForm.FieldFormat.IntRadix(radix, zeroPad)
        | 3uy -> PrintfHoleForm.FieldFormat.Unsigned(readOptionWith r (fun r -> r.ReadInt32()))
        | 4uy -> PrintfHoleForm.FieldFormat.Bool
        | 5uy -> PrintfHoleForm.FieldFormat.Fixed(readPrec r)
        | 6uy ->
            let precision = r.ReadInt32()
            let width = r.ReadInt32()
            PrintfHoleForm.FieldFormat.FixedZeroPad(precision, width)
        | 7uy ->
            let precision = r.ReadInt32()
            let width = r.ReadInt32()
            PrintfHoleForm.FieldFormat.FixedRightZeroPad(precision, width)
        | 8uy ->
            let precision = readPrec r
            let upper = r.ReadBoolean()
            PrintfHoleForm.FieldFormat.Exponential(precision, upper)
        | 9uy ->
            let precision = readPrec r
            let upper = r.ReadBoolean()
            PrintfHoleForm.FieldFormat.Compact(precision, upper)
        | 10uy ->
            let precision = r.ReadInt32()
            let width = r.ReadInt32()
            let typeChar = r.ReadChar()
            PrintfHoleForm.FieldFormat.ExpCompactZeroPad(precision, width, typeChar)
        | 11uy ->
            let space = r.ReadBoolean()
            let precision = readPrec r
            let typeChar = r.ReadChar()
            let zeroPad = readOptionWith r (fun r -> r.ReadInt32())
            PrintfHoleForm.FieldFormat.ForcedSign(space, precision, typeChar, zeroPad)
        | b -> failwithf "FrozenCodec: unknown FieldFormat tag %d" b

    let private writeHoleForm (w: BinaryWriter) (h: PrintfHoleForm.HoleForm) =
        match h with
        | PrintfHoleForm.HoleForm.PercentA(width, size) ->
            w.Write 0uy
            writePrintWidth w width
            writePrintSize w size
        | PrintfHoleForm.HoleForm.Field(fmt, alignment) ->
            w.Write 1uy
            writeFieldFormat w fmt
            writeAlignment w alignment
        | PrintfHoleForm.HoleForm.Callback hasValue ->
            w.Write 2uy
            w.Write hasValue

    let private readHoleForm (r: BinaryReader) : PrintfHoleForm.HoleForm =
        match r.ReadByte() with
        | 0uy ->
            let width = readPrintWidth r
            let size = readPrintSize r
            PrintfHoleForm.HoleForm.PercentA(width, size)
        | 1uy ->
            let fmt = readFieldFormat r
            let alignment = readAlignment r
            PrintfHoleForm.HoleForm.Field(fmt, alignment)
        | 2uy -> PrintfHoleForm.HoleForm.Callback(r.ReadBoolean())
        | b -> failwithf "FrozenCodec: unknown HoleForm tag %d" b

    let private writeHoleSpecSource (w: BinaryWriter) (s: HoleSpecSource) =
        match s with
        | HoleSpecSource.Classified form ->
            w.Write 0uy
            writeHoleForm w form
        | HoleSpecSource.RawFormat fmt ->
            w.Write 1uy
            writeOptionWith w (fun w (s: string) -> w.Write s) fmt

    let private readHoleSpecSource (r: BinaryReader) : HoleSpecSource =
        match r.ReadByte() with
        | 0uy -> HoleSpecSource.Classified(readHoleForm r)
        | 1uy -> HoleSpecSource.RawFormat(readOptionWith r (fun r -> r.ReadString()))
        | b -> failwithf "FrozenCodec: unknown HoleSpecSource tag %d" b

    // A `HoleSpec` carries no sub-expression (its `Ty` is a `FrozenType`, its `Tok` a
    // token), so it is a leaf ahead of the tree group even though the format SEGMENT
    // that holds it is not.
    let private writeHoleSpec (w: BinaryWriter) (h: Frozen.HoleSpec) =
        writeFrozenType w h.Ty
        writeHoleSpecSource w h.Source
        writeSyntaxToken w h.Tok

    let private readHoleSpec (r: BinaryReader) : Frozen.HoleSpec =
        let ty = readFrozenType r
        let source = readHoleSpecSource r
        let tok = readSyntaxToken r
        { Ty = ty; Source = source; Tok = tok }

    // ── the leaf type-declaration payloads (no sub-expression) ──────────────

    let private writeAbstractMethod (w: BinaryWriter) (m: Frozen.TAbstractMethod) =
        w.Write m.Name
        writeStringArray w m.MethodTypeParams
        writeFrozenType w m.Signature
        w.Write m.IsProperty

    let private readAbstractMethod (r: BinaryReader) : Frozen.TAbstractMethod =
        let name = r.ReadString()
        let methodTypeParams = readStringArray r
        let signature = readFrozenType r
        let isProperty = r.ReadBoolean()

        {
            Name = name
            MethodTypeParams = methodTypeParams
            Signature = signature
            IsProperty = isProperty
        }

    let private writeUnionCase (w: BinaryWriter) (c: Frozen.TUnionCase) =
        w.Write c.Name

        writeEqArrayWith
            w
            (fun w (nameOpt: string voption, ty) ->
                writeVOptionWith w (fun w (s: string) -> w.Write s) nameOpt
                writeFrozenType w ty
            )
            c.Fields

    let private readUnionCase (r: BinaryReader) : Frozen.TUnionCase =
        let name = r.ReadString()

        let fields =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let nameOpt = readVOptionWith r (fun r -> r.ReadString())
                        let ty = readFrozenType r
                        nameOpt, ty
                    )
            )

        { Name = name; Fields = fields }

    let private writeRecordField (w: BinaryWriter) (f: Frozen.TRecordField) =
        w.Write f.Name
        writeFrozenType w f.Type
        w.Write f.IsMutable

    let private readRecordField (r: BinaryReader) : Frozen.TRecordField =
        let name = r.ReadString()
        let ty = readFrozenType r
        let isMutable = r.ReadBoolean()

        {
            Name = name
            Type = ty
            IsMutable = isMutable
        }

    let private writeEnumCase (w: BinaryWriter) (c: Frozen.TEnumCase) =
        w.Write c.Name
        writeVOptionWith w writeTEnumLiteral c.Value
        writeSyntaxToken w c.Tok

    let private readEnumCase (r: BinaryReader) : Frozen.TEnumCase =
        let name = r.ReadString()
        let value = readVOptionWith r readTEnumLiteral
        let tok = readSyntaxToken r

        {
            Name = name
            Value = value
            Tok = tok
        }

    // ── the declaration shell + the scalar clusters riding a pool payload ───
    //
    // Each writer is followed IMMEDIATELY by its reader. They have to agree field for
    // field, in order, and nothing but review makes them: putting the pair on one screen
    // is the whole of that review. (They were two groups, a couple of hundred lines
    // apart, when a `TTypeMember`'s ten-field emit order had to be checked against a
    // reader you could not see at the same time.)
    //
    // Instantiated at `<FrozenType, SyntaxToken>` — every `'ty` payload rides
    // `writeFrozenType`, every `'tok` rides `writeSyntaxToken` (both defined in the
    // leaf group above). Each writer's `match` is EXHAUSTIVE with no catch-all, so a
    // new case fails to compile here; each reader reconstructs the case / record
    // DIRECTLY (never a normalizing smart constructor) with `let`-sequenced field
    // reads that provably mirror the writer's emit order.
    //
    // There is NO recursive `TExpr`/`TPat`/`TDecl` tree codec any more: every tree in the
    // file is in the pool columns, so an expression is written as an `ExprPoolId` and a
    // pattern as a `PatPoolId` wherever one used to be inlined — a `type` declaration's
    // member bodies, an inline template's decl, a `ValRepr`'s tuple group. What remains
    // here is the declaration SHELL (which the pooled `Type` payload still carries whole)
    // plus the scalar clusters an `ExprPayload` rides (`Disposal`, `CallVia`,
    // `ForInEnumerator`, the static-opt constraints).

    /// The pool ids, written as plain `int`s — the blob is Brotli-compressed at the store
    /// seam, which absorbs their width redundancy far more cheaply than a bespoke varint
    /// would pay for in reader complexity. Defined here rather than with the column codec
    /// below because the declaration shell names its bodies by id.
    let private writeExprPoolId (w: BinaryWriter) (ExprPoolId i) = w.Write i
    let private readExprPoolId (r: BinaryReader) : ExprPoolId = ExprPoolId(r.ReadInt32())
    let private writePatPoolId (w: BinaryWriter) (PatPoolId i) = w.Write i
    let private readPatPoolId (r: BinaryReader) : PatPoolId = PatPoolId(r.ReadInt32())
    let private writeDeclPoolId (w: BinaryWriter) (DeclPoolId i) = w.Write i
    let private readDeclPoolId (r: BinaryReader) : DeclPoolId = DeclPoolId(r.ReadInt32())

    let rec private writeDisposal (w: BinaryWriter) (d: Disposal) =
        match d with
        | Disposal.ViaCapability slot ->
            w.Write 0uy
            writeSymbolKey w slot
        | Disposal.ViaOwnMember key ->
            w.Write 1uy
            writeSymbolKey w key
        | Disposal.Unresolved -> w.Write 2uy

    and private readDisposal (r: BinaryReader) : Disposal =
        match r.ReadByte() with
        | 0uy -> Disposal.ViaCapability(readSymbolKey r)
        | 1uy -> Disposal.ViaOwnMember(readSymbolKey r)
        | 2uy -> Disposal.Unresolved
        | b -> failwithf "FrozenCodec: unknown Disposal tag %d" b

    and private writeCallVia (w: BinaryWriter) (v: CallVia<FrozenType>) =
        match v with
        | CallVia.Self -> w.Write 0uy
        | CallVia.Base -> w.Write 1uy
        | CallVia.Interface ifaceArgs ->
            w.Write 2uy
            writeEqArrayWith w writeFrozenType ifaceArgs

    and private readCallVia (r: BinaryReader) : CallVia<FrozenType> =
        match r.ReadByte() with
        | 0uy -> CallVia.Self
        | 1uy -> CallVia.Base
        | 2uy -> CallVia.Interface(EqArray.ofArray (readArrayWith r readFrozenType))
        | b -> failwithf "FrozenCodec: unknown CallVia tag %d" b

    and private writeStaticOptConstraint (w: BinaryWriter) (c: Frozen.TStaticOptConstraint) =
        match c with
        | TStaticOptConstraintG.TyconEquals(typar, required) ->
            w.Write 0uy
            writeFrozenType w typar
            writeFrozenType w required
        | TStaticOptConstraintG.IsStruct typar ->
            w.Write 1uy
            writeFrozenType w typar

    and private readStaticOptConstraint (r: BinaryReader) : Frozen.TStaticOptConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typar = readFrozenType r
            let required = readFrozenType r
            TStaticOptConstraintG.TyconEquals(typar, required)
        | 1uy -> TStaticOptConstraintG.IsStruct(readFrozenType r)
        | b -> failwithf "FrozenCodec: unknown TStaticOptConstraint tag %d" b

    and private writeForInEnumerator (w: BinaryWriter) (e: Frozen.ForInEnumerator) =
        match e with
        | ForInEnumeratorG.Interface -> w.Write 0uy
        | ForInEnumeratorG.Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose) ->
            w.Write 1uy
            writeFrozenType w enumeratorTy
            writeForInGetEnum w getEnumerator
            writeForInEnumMembers w members
            w.Write isValueType
            w.Write dispose

    and private readForInEnumerator (r: BinaryReader) : Frozen.ForInEnumerator =
        match r.ReadByte() with
        | 0uy -> ForInEnumeratorG.Interface
        | 1uy ->
            let enumeratorTy = readFrozenType r
            let getEnumerator = readForInGetEnum r
            let members = readForInEnumMembers r
            let isValueType = r.ReadBoolean()
            let dispose = r.ReadBoolean()
            ForInEnumeratorG.Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose)
        | b -> failwithf "FrozenCodec: unknown ForInEnumerator tag %d" b

    and private writeForInGetEnum (w: BinaryWriter) (g: ForInGetEnumG<FrozenType>) =
        match g with
        | ForInGetEnumG.External getEnumerator ->
            w.Write 0uy
            writeSymbolKey w getEnumerator
        | ForInGetEnumG.Local -> w.Write 1uy
        | ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKey w iface
            writeEqArrayWith w writeFrozenType ifaceArgs

    and private readForInGetEnum (r: BinaryReader) : ForInGetEnumG<FrozenType> =
        match r.ReadByte() with
        | 0uy -> ForInGetEnumG.External(readSymbolKey r)
        | 1uy -> ForInGetEnumG.Local
        | 2uy ->
            let iface = readTypeKey r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readFrozenType)
            ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInGetEnum tag %d" b

    and private writeForInEnumMembers (w: BinaryWriter) (m: ForInEnumMembersG<FrozenType>) =
        match m with
        | ForInEnumMembersG.External(moveNext, current) ->
            w.Write 0uy
            writeSymbolKey w moveNext
            writeSymbolKey w current
        | ForInEnumMembersG.Local -> w.Write 1uy
        | ForInEnumMembersG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKey w iface
            writeEqArrayWith w writeFrozenType ifaceArgs

    and private readForInEnumMembers (r: BinaryReader) : ForInEnumMembersG<FrozenType> =
        match r.ReadByte() with
        | 0uy ->
            let moveNext = readSymbolKey r
            let current = readSymbolKey r
            ForInEnumMembersG.External(moveNext, current)
        | 1uy -> ForInEnumMembersG.Local
        | 2uy ->
            let iface = readTypeKey r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readFrozenType)
            ForInEnumMembersG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInEnumMembers tag %d" b

    // The `type` declaration shell — the one declaration shape a pool payload still
    // carries whole (`DeclPayload.Type`). Its member / preamble / ctor bodies are pool
    // ids, so this group bottoms out at `writeExprPoolId` where it once recursed into
    // `writeExpr`.

    and private writeTypeDecl (w: BinaryWriter) (td: PooledTypeDecl) =
        w.Write td.Name
        writeTypeKey w td.TypeKey
        writeOptionWith w (fun w (s: string) -> w.Write s) td.Namespace
        writeStringArray w td.TypeParams
        w.Write td.IsRequireQualifiedAccess
        writeTypeKind w td.Kind
        writeEqualityVerdict w td.EqualitySupport
        writeComparisonVerdict w td.ComparisonSupport

    and private readTypeDecl (r: BinaryReader) : PooledTypeDecl =
        let name = r.ReadString()
        let typeKey = readTypeKey r
        let ns = readOptionWith r (fun r -> r.ReadString())
        let typeParams = readStringArray r
        let isRqa = r.ReadBoolean()
        let kind = readTypeKind r
        let equalitySupport = readEqualityVerdict r
        let comparisonSupport = readComparisonVerdict r

        {
            Name = name
            TypeKey = typeKey
            Namespace = ns
            TypeParams = typeParams
            IsRequireQualifiedAccess = isRqa
            Kind = kind
            EqualitySupport = equalitySupport
            ComparisonSupport = comparisonSupport
        }

    and private writeTypeKind (w: BinaryWriter) (k: TTypeKindG<FrozenType, SyntaxToken, ExprPoolId>) =
        match k with
        | TTypeKindG.Interface methods ->
            w.Write 0uy
            writeEqArrayWith w writeAbstractMethod methods
        | TTypeKindG.Union(cases, members, interfaces) ->
            w.Write 1uy
            writeEqArrayWith w writeUnionCase cases
            writeEqArrayWith w writeTypeMember members
            writeInterfaces w interfaces
        | TTypeKindG.Record(fields, members, interfaces, valueKind) ->
            w.Write 2uy
            writeEqArrayWith w writeRecordField fields
            writeEqArrayWith w writeTypeMember members
            writeInterfaces w interfaces
            writeClassValueKind w valueKind
        | TTypeKindG.Class c ->
            w.Write 3uy
            writeClass w c
        | TTypeKindG.Enum cases ->
            w.Write 4uy
            writeEqArrayWith w writeEnumCase cases

    and private readTypeKind (r: BinaryReader) : TTypeKindG<FrozenType, SyntaxToken, ExprPoolId> =
        match r.ReadByte() with
        | 0uy -> TTypeKindG.Interface(EqArray.ofArray (readArrayWith r readAbstractMethod))
        | 1uy ->
            let cases = EqArray.ofArray (readArrayWith r readUnionCase)
            let members = EqArray.ofArray (readArrayWith r readTypeMember)
            let interfaces = readInterfaces r
            TTypeKindG.Union(cases, members, interfaces)
        | 2uy ->
            let fields = EqArray.ofArray (readArrayWith r readRecordField)
            let members = EqArray.ofArray (readArrayWith r readTypeMember)
            let interfaces = readInterfaces r
            let valueKind = readClassValueKind r
            TTypeKindG.Record(fields, members, interfaces, valueKind)
        | 3uy -> TTypeKindG.Class(readClass r)
        | 4uy -> TTypeKindG.Enum(EqArray.ofArray (readArrayWith r readEnumCase))
        | b -> failwithf "FrozenCodec: unknown TTypeKind tag %d" b

    // Each `interfaces` entry pairs a resolved interface type with its typed member
    // bodies — shared by the class / union / record arms.
    and private writeInterfaces
        (w: BinaryWriter)
        (interfaces: EqArray<FrozenType * EqArray<TTypeMemberG<FrozenType, ExprPoolId>>>)
        =
        writeEqArrayWith
            w
            (fun w (ty, mems) ->
                writeFrozenType w ty
                writeEqArrayWith w writeTypeMember mems
            )
            interfaces

    and private readInterfaces (r: BinaryReader) : EqArray<FrozenType * EqArray<TTypeMemberG<FrozenType, ExprPoolId>>> =
        EqArray.ofArray (
            readArrayWith
                r
                (fun r ->
                    let ty = readFrozenType r
                    let mems = EqArray.ofArray (readArrayWith r readTypeMember)
                    ty, mems
                )
        )

    and private writeClass (w: BinaryWriter) (c: TClassG<FrozenType, ExprPoolId>) =
        writeEqArrayWith w writeRecordField c.Fields
        writeEqArrayWith w writeRecordField c.CtorParams
        writeEqArrayWith w writeTypeMember c.Members
        writeVOptionWith w writeFrozenType c.BaseType
        writeInterfaces w c.Interfaces
        w.Write c.IsSealed
        writeEqArrayWith w writePreambleEntry c.StaticPreamble
        writeEqArrayWith w writePreambleEntry c.InstancePreamble
        writeNodeKey w c.ThisKey
        writeEqArrayWith w writeSecondaryCtor c.SecondaryCtors
        writeVOptionWith w writeBaseCtorCall c.BaseCtorCall
        writeClassValueKind w c.ValueKind
        w.Write c.HasPrimaryCtor

    and private readClass (r: BinaryReader) : TClassG<FrozenType, ExprPoolId> =
        let fields = EqArray.ofArray (readArrayWith r readRecordField)
        let ctorParams = EqArray.ofArray (readArrayWith r readRecordField)
        let members = EqArray.ofArray (readArrayWith r readTypeMember)
        let baseType = readVOptionWith r readFrozenType
        let interfaces = readInterfaces r
        let isSealed = r.ReadBoolean()
        let staticPreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let instancePreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let thisKey = readNodeKey r
        let secondaryCtors = EqArray.ofArray (readArrayWith r readSecondaryCtor)
        let baseCtorCall = readVOptionWith r readBaseCtorCall
        let valueKind = readClassValueKind r
        let hasPrimaryCtor = r.ReadBoolean()

        {
            Fields = fields
            CtorParams = ctorParams
            Members = members
            BaseType = baseType
            Interfaces = interfaces
            IsSealed = isSealed
            StaticPreamble = staticPreamble
            InstancePreamble = instancePreamble
            ThisKey = thisKey
            SecondaryCtors = secondaryCtors
            BaseCtorCall = baseCtorCall
            ValueKind = valueKind
            HasPrimaryCtor = hasPrimaryCtor
        }

    and private writeTypeMember (w: BinaryWriter) (m: TTypeMemberG<FrozenType, ExprPoolId>) =
        w.Write m.Name
        w.Write m.IsStatic
        writeAccessibility w m.Accessibility
        writeTMemberKind w m.Kind
        w.Write m.IsOverride
        writeVOptionWith w writeNodeKey m.ThisKey
        writeVOptionWith w writeNodeKey m.BaseKey
        writeFrozenType w m.ThisTy

        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeNodeKey w k
                writeFrozenType w ty
            )
            m.Params

        writeExprPoolId w m.Body
        writeFrozenType w m.ReturnTy
        writeMethodTypeParams w m.MethodTypeParams

    and private readTypeMember (r: BinaryReader) : TTypeMemberG<FrozenType, ExprPoolId> =
        let name = r.ReadString()
        let isStatic = r.ReadBoolean()
        let accessibility = readAccessibility r
        let kind = readTMemberKind r
        let isOverride = r.ReadBoolean()
        let thisKey = readVOptionWith r readNodeKey
        let baseKey = readVOptionWith r readNodeKey
        let thisTy = readFrozenType r

        let parameters =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readNodeKey r
                        let ty = readFrozenType r
                        k, ty
                    )
            )

        let body = readExprPoolId r
        let returnTy = readFrozenType r
        let methodTypeParams = readMethodTypeParams r

        {
            Name = name
            IsStatic = isStatic
            Accessibility = accessibility
            Kind = kind
            IsOverride = isOverride
            ThisKey = thisKey
            BaseKey = baseKey
            ThisTy = thisTy
            Params = parameters
            Body = body
            ReturnTy = returnTy
            MethodTypeParams = methodTypeParams
        }

    and private writeClassLet (w: BinaryWriter) (l: TClassLetG<FrozenType, ExprPoolId>) =
        w.Write l.Name
        writeFrozenType w l.Type
        w.Write l.IsMutable
        writeExprPoolId w l.Init

    and private readClassLet (r: BinaryReader) : TClassLetG<FrozenType, ExprPoolId> =
        let name = r.ReadString()
        let ty = readFrozenType r
        let isMutable = r.ReadBoolean()
        let init = readExprPoolId r

        {
            Name = name
            Type = ty
            IsMutable = isMutable
            Init = init
        }

    and private writePreambleEntry (w: BinaryWriter) (p: TPreambleEntryG<FrozenType, ExprPoolId>) =
        match p with
        | TPreambleEntryG.Let l ->
            w.Write 0uy
            writeClassLet w l
        | TPreambleEntryG.Do e ->
            w.Write 1uy
            writeExprPoolId w e

    and private readPreambleEntry (r: BinaryReader) : TPreambleEntryG<FrozenType, ExprPoolId> =
        match r.ReadByte() with
        | 0uy -> TPreambleEntryG.Let(readClassLet r)
        | 1uy -> TPreambleEntryG.Do(readExprPoolId r)
        | b -> failwithf "FrozenCodec: unknown TPreambleEntry tag %d" b

    and private writeCtorLet (w: BinaryWriter) (cl: TCtorLetG<FrozenType, ExprPoolId>) =
        writeNodeKey w cl.Binder
        writeFrozenType w cl.Type
        writeExprPoolId w cl.Init

    and private readCtorLet (r: BinaryReader) : TCtorLetG<FrozenType, ExprPoolId> =
        let binder = readNodeKey r
        let ty = readFrozenType r
        let init = readExprPoolId r

        {
            Binder = binder
            Type = ty
            Init = init
        }

    and private writeCtorFieldInit (w: BinaryWriter) (fi: TCtorFieldInitG<ExprPoolId>) =
        w.Write fi.Field
        writeExprPoolId w fi.Init

    and private readCtorFieldInit (r: BinaryReader) : TCtorFieldInitG<ExprPoolId> =
        let field = r.ReadString()
        let init = readExprPoolId r
        { Field = field; Init = init }

    and private writeSecondaryCtor (w: BinaryWriter) (sc: TSecondaryCtorG<FrozenType, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeNodeKey w k
                writeFrozenType w ty
            )
            sc.Params

        writeEqArrayWith w writeCtorLet sc.Lets
        writeEqArrayWith w writeExprPoolId sc.PrimaryArgs
        writeEqArrayWith w writeCtorFieldInit sc.FieldInits

    and private readSecondaryCtor (r: BinaryReader) : TSecondaryCtorG<FrozenType, ExprPoolId> =
        let parameters =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readNodeKey r
                        let ty = readFrozenType r
                        k, ty
                    )
            )

        let lets = EqArray.ofArray (readArrayWith r readCtorLet)
        let primaryArgs = EqArray.ofArray (readArrayWith r readExprPoolId)
        let fieldInits = EqArray.ofArray (readArrayWith r readCtorFieldInit)

        {
            Params = parameters
            Lets = lets
            PrimaryArgs = primaryArgs
            FieldInits = fieldInits
        }

    and private writeBaseCtorCall (w: BinaryWriter) (bc: TBaseCtorCallG<FrozenType, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeNodeKey w k
                writeFrozenType w ty
            )
            bc.CtorParams

        writeEqArrayWith w writeExprPoolId bc.Args
        writeVOptionWith w writeSymbolKey bc.ChosenCtor

    and private readBaseCtorCall (r: BinaryReader) : TBaseCtorCallG<FrozenType, ExprPoolId> =
        let ctorParams =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readNodeKey r
                        let ty = readFrozenType r
                        k, ty
                    )
            )

        let args = EqArray.ofArray (readArrayWith r readExprPoolId)
        let chosenCtor = readVOptionWith r readSymbolKey

        {
            CtorParams = ctorParams
            Args = args
            ChosenCtor = chosenCtor
        }

    /// A published template: its identity and parameter attributes, its declaration named
    /// by pool id like any other root.
    and private writeInlineTemplate (w: BinaryWriter) (v: PooledInlineValue) =
        writeSymbolKey w v.Key
        writeDeclPoolId w v.Decl
        writeArrayWith w writeParamAttrs v.ParamAttrs

    and private readInlineTemplate (r: BinaryReader) : PooledInlineValue =
        let key = readSymbolKey r
        let decl = readDeclPoolId r
        let paramAttrs = readArrayWith r readParamAttrs

        {
            Key = key
            Decl = decl
            ParamAttrs = paramAttrs
        }

    and private writeArgGroup (w: BinaryWriter) (g: ArgGroupG<FrozenType, PatPoolId>) =
        match g with
        | ArgGroupG.GUnit ty ->
            w.Write 0uy
            writeFrozenType w ty
        | ArgGroupG.GSimple(slot, ty) ->
            w.Write 1uy
            writeNodeKey w slot
            writeFrozenType w ty
        | ArgGroupG.GTuple pat ->
            w.Write 2uy
            writePatPoolId w pat

    and private readArgGroup (r: BinaryReader) : ArgGroupG<FrozenType, PatPoolId> =
        match r.ReadByte() with
        | 0uy -> ArgGroupG.GUnit(readFrozenType r)
        | 1uy ->
            let slot = readNodeKey r
            let ty = readFrozenType r
            ArgGroupG.GSimple(slot, ty)
        | 2uy -> ArgGroupG.GTuple(readPatPoolId r)
        | b -> failwithf "FrozenCodec: unknown ArgGroup tag %d" b

    and private writeValRepr (w: BinaryWriter) (v: PooledValRepr) =
        w.Write v.Typars
        writeListWith w writeArgGroup v.Groups
        writeFrozenType w v.ResultTy

    and private readValRepr (r: BinaryReader) : PooledValRepr =
        let typars = r.ReadInt32()
        let groups = readListWith r readArgGroup
        let resultTy = readFrozenType r

        {
            Typars = typars
            Groups = groups
            ResultTy = resultTy
        }

    // ── the pool columns (the stored wire form) ─────────────────────────────
    //
    // One length-prefixed column per `FrozenPools` field, emitted in record-declaration
    // order so the writer and the reader read as the same list side by side. Ids
    // (`ExprPoolId`/`PatPoolId`/`DeclPoolId`/`BinderId`) are plain `int`s: the blob is
    // Brotli-compressed at the store seam, which absorbs their width redundancy far more
    // cheaply than a bespoke varint would pay for in reader complexity.
    //
    // The payload tag writers below are EXHAUSTIVE with no catch-all — the same discipline
    // `TastPools.exprPayload`/`substituteExpr` hold — so a new payload case fails to
    // compile here rather than serializing as a silent alias. The SHAPE tags are all-nullary
    // and single-sourced from one array instead; see `shapeTags` for how completeness is
    // held there.

    let private writeBinderId (w: BinaryWriter) (BinderId i) = w.Write i
    let private readBinderId (r: BinaryReader) : BinderId = BinderId(r.ReadInt32())

    /// A jagged child-id column — one length-prefixed id list per pool slot. Generic over
    /// the id codec, so the expr-child and pat-child columns of all three domains share the
    /// one nesting convention rather than repeating it per domain.
    let private writeIdColumn (w: BinaryWriter) (writeId: BinaryWriter -> 'id -> unit) (col: 'id[][]) =
        writeArrayWith w (fun w ids -> writeArrayWith w writeId ids) col

    let private readIdColumn (r: BinaryReader) (readId: BinaryReader -> 'id) : 'id[][] =
        readArrayWith r (fun r -> readArrayWith r readId)

    /// A dense side table — the `(id, value)` association a `Map<NodeKey,_>` was re-keyed
    /// to. Generic over BOTH codecs, so the six `BinderId`-keyed tables and the one
    /// `ExprPoolId`-keyed (`FunVerdicts`) share this single pair.
    let private writeDenseTable
        (w: BinaryWriter)
        (writeId: BinaryWriter -> 'id -> unit)
        (writeVal: BinaryWriter -> 'v -> unit)
        (xs: ('id * 'v)[])
        =
        writeArrayWith
            w
            (fun w (id, v) ->
                writeId w id
                writeVal w v
            )
            xs

    let private readDenseTable
        (r: BinaryReader)
        (readId: BinaryReader -> 'id)
        (readVal: BinaryReader -> 'v)
        : ('id * 'v)[] =
        readArrayWith
            r
            (fun r ->
                let id = readId r
                let v = readVal r
                id, v
            )

    // A shape tag is one byte, and — unlike every other tagged case in this file — its
    // numbering is written down ONCE. `ExprShape`/`PatShape`/`DeclShape` are all-nullary,
    // so the whole codec for one is a single declaration-ordered array: an entry's INDEX
    // is the byte it stores as, and the same array inverts a byte on read. A
    // writer/reader disagreement is no longer expressible, where before it was two
    // hand-typed numberings that had to agree by eye.
    //
    // What that costs, and how it is bought back: the writer is no longer an exhaustive
    // match, so a new case does not fail to COMPILE here. `shapeTags` instead checks at
    // module init that the array is a BIJECTION onto the type's cases — no duplicates,
    // and exactly as many entries as the type declares — so a case added without
    // extending the array faults the first time anything touches the codec rather than
    // serializing as a silent alias of another shape. The array's ORDER is the wire
    // format: append to it, never permute it.

    let private shapeTags (name: string) (byTag: 'a[]) : ('a -> byte) * (BinaryReader -> 'a) =
        let toTag = System.Collections.Generic.Dictionary<'a, byte>(byTag.Length)

        byTag |> Array.iteri (fun i c -> toTag.[c] <- byte i)

        if toTag.Count <> byTag.Length then
            failwithf "FrozenCodec: the %s tag table lists a case twice" name

        let declared = Reflection.FSharpType.GetUnionCases(typeof<'a>).Length

        if byTag.Length <> declared then
            failwithf "FrozenCodec: %s declares %d cases but its tag table lists %d" name declared byTag.Length

        let read (r: BinaryReader) =
            let b = r.ReadByte()

            if int b >= byTag.Length then
                failwithf "FrozenCodec: unknown %s tag %d" name b

            byTag.[int b]

        (fun s -> toTag.[s]), read

    let private exprShapeTag, readExprShape =
        shapeTags
            "ExprShape"
            [|
                ExprShape.Const
                ExprShape.Var
                ExprShape.External
                ExprShape.Lambda
                ExprShape.App
                ExprShape.Let
                ExprShape.Use
                ExprShape.IfThenElse
                ExprShape.Tuple
                ExprShape.Sequential
                ExprShape.While
                ExprShape.ForTo
                ExprShape.ForIn
                ExprShape.Match
                ExprShape.TryWith
                ExprShape.TryFinally
                ExprShape.Assignment
                ExprShape.Null
                ExprShape.Range
                ExprShape.RecordCons
                ExprShape.RecordClone
                ExprShape.FieldGet
                ExprShape.FieldSet
                ExprShape.UnionCons
                ExprShape.New
                ExprShape.MethodCall
                ExprShape.PropertyGet
                ExprShape.StaticMethodCall
                ExprShape.StaticPropertyGet
                ExprShape.StaticFieldGet
                ExprShape.StaticFieldSet
                ExprShape.ExternalMember
                ExprShape.Format
                ExprShape.ILIntrinsic
                ExprShape.StaticOptimization
                ExprShape.Upcast
                ExprShape.Downcast
                ExprShape.TypeTest
                ExprShape.TraitCall
            |]

    let private patShapeTag, readPatShape =
        shapeTags
            "PatShape"
            [|
                PatShape.NamedSimple
                PatShape.Wildcard
                PatShape.Tuple
                PatShape.Const
                PatShape.Record
                PatShape.Union
                PatShape.TypeTestAs
                PatShape.Null
                PatShape.EnumCase
                PatShape.Or
            |]

    let private declShapeTag, readDeclShape =
        shapeTags "DeclShape" [| DeclShape.Let; DeclShape.Expression; DeclShape.Type |]

    let private writeExprShape (w: BinaryWriter) (s: ExprShape) = w.Write(exprShapeTag s)
    let private writePatShape (w: BinaryWriter) (s: PatShape) = w.Write(patShapeTag s)
    let private writeDeclShape (w: BinaryWriter) (s: DeclShape) = w.Write(declShapeTag s)

    let private writeFormatSinkShape (w: BinaryWriter) (s: FormatSinkShape) =
        match s with
        | FormatSinkShape.ToStdOut newline ->
            w.Write 0uy
            w.Write newline
        | FormatSinkShape.ToStdErr newline ->
            w.Write 1uy
            w.Write newline
        | FormatSinkShape.ToWriter newline ->
            w.Write 2uy
            w.Write newline
        | FormatSinkShape.ToBuilder -> w.Write 3uy
        | FormatSinkShape.ToString -> w.Write 4uy

    let private readFormatSinkShape (r: BinaryReader) : FormatSinkShape =
        match r.ReadByte() with
        | 0uy -> FormatSinkShape.ToStdOut(r.ReadBoolean())
        | 1uy -> FormatSinkShape.ToStdErr(r.ReadBoolean())
        | 2uy -> FormatSinkShape.ToWriter(r.ReadBoolean())
        | 3uy -> FormatSinkShape.ToBuilder
        | 4uy -> FormatSinkShape.ToString
        | b -> failwithf "FrozenCodec: unknown FormatSinkShape tag %d" b

    let private writeFormatSegShape (w: BinaryWriter) (s: FormatSegShape) =
        match s with
        | FormatSegShape.Lit text ->
            w.Write 0uy
            w.Write text
        | FormatSegShape.Hole spec ->
            w.Write 1uy
            writeHoleSpec w spec
        | FormatSegShape.DynHole(hasWidth, hasPrecision, spec) ->
            w.Write 2uy
            w.Write hasWidth
            w.Write hasPrecision
            writeHoleSpec w spec
        | FormatSegShape.CallbackHole spec ->
            w.Write 3uy
            writeHoleSpec w spec

    let private readFormatSegShape (r: BinaryReader) : FormatSegShape =
        match r.ReadByte() with
        | 0uy -> FormatSegShape.Lit(r.ReadString())
        | 1uy -> FormatSegShape.Hole(readHoleSpec r)
        | 2uy ->
            let hasWidth = r.ReadBoolean()
            let hasPrecision = r.ReadBoolean()
            let spec = readHoleSpec r
            FormatSegShape.DynHole(hasWidth, hasPrecision, spec)
        | 3uy -> FormatSegShape.CallbackHole(readHoleSpec r)
        | b -> failwithf "FrozenCodec: unknown FormatSegShape tag %d" b

    let private writeExprPayload (w: BinaryWriter) (p: ExprPayload) =
        match p with
        | ExprPayload.Const value ->
            w.Write 0uy
            writeTConstValue w value
        | ExprPayload.Var -> w.Write 1uy
        | ExprPayload.External p ->
            w.Write 2uy
            w.Write p.CompiledName
            writeVOptionWith w writeSymbolKey p.Key
        | ExprPayload.Lambda -> w.Write 3uy
        | ExprPayload.App -> w.Write 4uy
        | ExprPayload.Let -> w.Write 5uy
        | ExprPayload.Use dispose ->
            w.Write 6uy
            writeDisposal w dispose
        | ExprPayload.IfThenElse -> w.Write 7uy
        | ExprPayload.Tuple -> w.Write 8uy
        | ExprPayload.Sequential -> w.Write 9uy
        | ExprPayload.While -> w.Write 10uy
        | ExprPayload.ForTo p ->
            w.Write 11uy
            writeNodeKey w p.Var
            writeSyntaxToken w p.IdentTok
        | ExprPayload.ForIn enumerator ->
            w.Write 12uy
            writeForInEnumerator w enumerator
        | ExprPayload.Match guardPresent ->
            w.Write 13uy
            writeArrayWith w (fun w (g: bool) -> w.Write g) guardPresent
        | ExprPayload.TryWith guardPresent ->
            w.Write 14uy
            writeArrayWith w (fun w (g: bool) -> w.Write g) guardPresent
        | ExprPayload.TryFinally -> w.Write 15uy
        | ExprPayload.Assignment -> w.Write 16uy
        | ExprPayload.Null -> w.Write 17uy
        | ExprPayload.Range hasStep ->
            w.Write 18uy
            w.Write hasStep
        | ExprPayload.RecordCons fieldNames ->
            w.Write 19uy
            writeArrayWith w (fun w (n: string) -> w.Write n) fieldNames
        | ExprPayload.RecordClone overrideNames ->
            w.Write 20uy
            writeArrayWith w (fun w (n: string) -> w.Write n) overrideNames
        | ExprPayload.FieldGet fieldName ->
            w.Write 21uy
            w.Write fieldName
        | ExprPayload.FieldSet fieldName ->
            w.Write 22uy
            w.Write fieldName
        | ExprPayload.UnionCons caseName ->
            w.Write 23uy
            w.Write caseName
        | ExprPayload.New p ->
            w.Write 24uy
            w.Write p.ClassName
            writeVOptionWith w writeSymbolKey p.Key
        | ExprPayload.MethodCall p ->
            w.Write 25uy
            writeSymbolKey w p.Key
            writeCallVia w p.Via
        | ExprPayload.PropertyGet p ->
            w.Write 26uy
            writeSymbolKey w p.Key
            writeCallVia w p.Via
        | ExprPayload.StaticMethodCall key ->
            w.Write 27uy
            writeSymbolKey w key
        | ExprPayload.StaticPropertyGet key ->
            w.Write 28uy
            writeSymbolKey w key
        | ExprPayload.StaticFieldGet p ->
            w.Write 29uy
            writeSymbolKey w p.DeclKey
            w.Write p.FieldName
        | ExprPayload.StaticFieldSet p ->
            w.Write 30uy
            writeSymbolKey w p.DeclKey
            w.Write p.FieldName
        | ExprPayload.ExternalMember p ->
            w.Write 31uy
            w.Write p.HasReceiver
            writeSymbolKey w p.Key
            w.Write p.MemberName
            writeMemberStorage w p.Storage
        | ExprPayload.Format p ->
            w.Write 32uy
            writeFormatSinkShape w p.Sink
            writeArrayWith w writeFormatSegShape p.Segments
        | ExprPayload.ILIntrinsic p ->
            w.Write 33uy
            w.Write p.OpCode
            writeVOptionWith w writeFrozenType p.TypeOperand
        | ExprPayload.StaticOptimization clauseConstraints ->
            w.Write 34uy
            writeArrayWith w (fun w cs -> writeEqArrayWith w writeStaticOptConstraint cs) clauseConstraints
        | ExprPayload.Upcast -> w.Write 35uy
        | ExprPayload.Downcast -> w.Write 36uy
        | ExprPayload.TypeTest testTy ->
            w.Write 37uy
            writeFrozenType w testTy
        | ExprPayload.TraitCall p ->
            w.Write 38uy
            writeFrozenType w p.Receiver
            w.Write p.MemberName

    let private readExprPayload (r: BinaryReader) : ExprPayload =
        match r.ReadByte() with
        | 0uy -> ExprPayload.Const(readTConstValue r)
        | 1uy -> ExprPayload.Var
        | 2uy ->
            let compiledName = r.ReadString()
            let key = readVOptionWith r readSymbolKey

            ExprPayload.External
                {|
                    CompiledName = compiledName
                    Key = key
                |}
        | 3uy -> ExprPayload.Lambda
        | 4uy -> ExprPayload.App
        | 5uy -> ExprPayload.Let
        | 6uy -> ExprPayload.Use(readDisposal r)
        | 7uy -> ExprPayload.IfThenElse
        | 8uy -> ExprPayload.Tuple
        | 9uy -> ExprPayload.Sequential
        | 10uy -> ExprPayload.While
        | 11uy ->
            let var = readNodeKey r
            let identTok = readSyntaxToken r

            ExprPayload.ForTo {| Var = var; IdentTok = identTok |}
        | 12uy -> ExprPayload.ForIn(readForInEnumerator r)
        | 13uy -> ExprPayload.Match(readArrayWith r (fun r -> r.ReadBoolean()))
        | 14uy -> ExprPayload.TryWith(readArrayWith r (fun r -> r.ReadBoolean()))
        | 15uy -> ExprPayload.TryFinally
        | 16uy -> ExprPayload.Assignment
        | 17uy -> ExprPayload.Null
        | 18uy -> ExprPayload.Range(r.ReadBoolean())
        | 19uy -> ExprPayload.RecordCons(readArrayWith r (fun r -> r.ReadString()))
        | 20uy -> ExprPayload.RecordClone(readArrayWith r (fun r -> r.ReadString()))
        | 21uy -> ExprPayload.FieldGet(r.ReadString())
        | 22uy -> ExprPayload.FieldSet(r.ReadString())
        | 23uy -> ExprPayload.UnionCons(r.ReadString())
        | 24uy ->
            let className = r.ReadString()
            let key = readVOptionWith r readSymbolKey
            ExprPayload.New {| ClassName = className; Key = key |}
        | 25uy ->
            let key = readSymbolKey r
            let via = readCallVia r
            ExprPayload.MethodCall {| Key = key; Via = via |}
        | 26uy ->
            let key = readSymbolKey r
            let via = readCallVia r
            ExprPayload.PropertyGet {| Key = key; Via = via |}
        | 27uy -> ExprPayload.StaticMethodCall(readSymbolKey r)
        | 28uy -> ExprPayload.StaticPropertyGet(readSymbolKey r)
        | 29uy ->
            let declKey = readSymbolKey r
            let fieldName = r.ReadString()

            ExprPayload.StaticFieldGet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | 30uy ->
            let declKey = readSymbolKey r
            let fieldName = r.ReadString()

            ExprPayload.StaticFieldSet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | 31uy ->
            let hasReceiver = r.ReadBoolean()
            let key = readSymbolKey r
            let memberName = r.ReadString()
            let storage = readMemberStorage r

            ExprPayload.ExternalMember
                {|
                    HasReceiver = hasReceiver
                    Key = key
                    MemberName = memberName
                    Storage = storage
                |}
        | 32uy ->
            let sink = readFormatSinkShape r
            let segments = readArrayWith r readFormatSegShape
            ExprPayload.Format {| Sink = sink; Segments = segments |}
        | 33uy ->
            let opCode = r.ReadString()
            let typeOperand = readVOptionWith r readFrozenType

            ExprPayload.ILIntrinsic
                {|
                    OpCode = opCode
                    TypeOperand = typeOperand
                |}
        | 34uy ->
            ExprPayload.StaticOptimization(
                readArrayWith r (fun r -> EqArray.ofArray (readArrayWith r readStaticOptConstraint))
            )
        | 35uy -> ExprPayload.Upcast
        | 36uy -> ExprPayload.Downcast
        | 37uy -> ExprPayload.TypeTest(readFrozenType r)
        | 38uy ->
            let receiver = readFrozenType r
            let memberName = r.ReadString()

            ExprPayload.TraitCall
                {|
                    Receiver = receiver
                    MemberName = memberName
                |}
        | b -> failwithf "FrozenCodec: unknown ExprPayload tag %d" b

    let private writePatPayload (w: BinaryWriter) (p: PatPayload) =
        match p with
        | PatPayload.NamedSimple binding ->
            w.Write 0uy
            writeNodeKey w binding
        | PatPayload.Wildcard -> w.Write 1uy
        | PatPayload.Null -> w.Write 2uy
        | PatPayload.Tuple -> w.Write 3uy
        | PatPayload.Or -> w.Write 4uy
        | PatPayload.Const value ->
            w.Write 5uy
            writeTConstValue w value
        | PatPayload.Record fieldNames ->
            w.Write 6uy
            writeArrayWith w (fun w (n: string) -> w.Write n) fieldNames
        | PatPayload.Union caseName ->
            w.Write 7uy
            w.Write caseName
        | PatPayload.TypeTestAs testTy ->
            w.Write 8uy
            writeFrozenType w testTy
        | PatPayload.EnumCase p ->
            w.Write 9uy
            writeSymbolKey w p.EnumKey
            w.Write p.CaseName

    let private readPatPayload (r: BinaryReader) : PatPayload =
        match r.ReadByte() with
        | 0uy -> PatPayload.NamedSimple(readNodeKey r)
        | 1uy -> PatPayload.Wildcard
        | 2uy -> PatPayload.Null
        | 3uy -> PatPayload.Tuple
        | 4uy -> PatPayload.Or
        | 5uy -> PatPayload.Const(readTConstValue r)
        | 6uy -> PatPayload.Record(readArrayWith r (fun r -> r.ReadString()))
        | 7uy -> PatPayload.Union(r.ReadString())
        | 8uy -> PatPayload.TypeTestAs(readFrozenType r)
        | 9uy ->
            let enumKey = readSymbolKey r
            let caseName = r.ReadString()

            PatPayload.EnumCase
                {|
                    EnumKey = enumKey
                    CaseName = caseName
                |}
        | b -> failwithf "FrozenCodec: unknown PatPayload tag %d" b

    let private writeDeclPayload (w: BinaryWriter) (p: DeclPayload) =
        match p with
        | DeclPayload.Let p ->
            w.Write 0uy
            w.Write p.IsInline
            writeFrozenType w p.Ty
        | DeclPayload.Expression ty ->
            w.Write 1uy
            writeFrozenType w ty
        | DeclPayload.Type td ->
            w.Write 2uy
            writeTypeDecl w td

    let private readDeclPayload (r: BinaryReader) : DeclPayload =
        match r.ReadByte() with
        | 0uy ->
            let isInline = r.ReadBoolean()
            let ty = readFrozenType r
            DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | 1uy -> DeclPayload.Expression(readFrozenType r)
        | 2uy -> DeclPayload.Type(readTypeDecl r)
        | b -> failwithf "FrozenCodec: unknown DeclPayload tag %d" b

    /// The three not-yet-pooled fields, verbatim — none of them a tree, so this writer
    /// bottoms out entirely in the leaf codecs.
    let private writeResidue (w: BinaryWriter) (res: FrozenFileResidue) =
        writeListWith w writeDiagnostic res.Diagnostics
        writeSymbolDict w writeIntrinsicReprInfo res.IntrinsicReprKeys
        writeSymbolDict w writeAccessibility res.Accessibility

    let private readResidue (r: BinaryReader) : FrozenFileResidue =
        let diagnostics = readListWith r readDiagnostic
        let intrinsicReprKeys = readSymbolDict r readIntrinsicReprInfo
        let accessibility = readSymbolDict r readAccessibility

        {
            Diagnostics = diagnostics
            IntrinsicReprKeys = intrinsicReprKeys
            Accessibility = accessibility
        }

    let private writePools (w: BinaryWriter) (p: FrozenPools) =
        writeArrayWith w writeExprShape p.ExprShapes
        writeArrayWith w writeFrozenType p.ExprTys
        writeArrayWith w writeSyntaxToken p.ExprToks
        writeIdColumn w writeExprPoolId p.ExprChildren
        writeIdColumn w writePatPoolId p.ExprPatChildren
        writeArrayWith w (fun w b -> writeVOptionWith w writeBinderId b) p.ExprVarBinder
        writeArrayWith w writeExprPayload p.ExprPayloads
        writeArrayWith w writePatShape p.PatShapes
        writeArrayWith w writeFrozenType p.PatTys
        writeArrayWith w writeSyntaxToken p.PatToks
        writeIdColumn w writePatPoolId p.PatChildren
        writeArrayWith w writePatPayload p.PatPayloads
        writeArrayWith w writeDeclShape p.DeclShapes
        writeIdColumn w writeExprPoolId p.DeclExprChildren
        writeIdColumn w writePatPoolId p.DeclPatChildren
        writeArrayWith w writeDeclPayload p.DeclPayloads
        writeArrayWith w writeDeclPoolId p.Roots
        writeArrayWith w writeInlineTemplate p.InlineTemplates
        writeArrayWith w writeNodeKey p.BinderKeys
        writeResidue w p.Residue
        writeDenseTable w writeBinderId writeModuleBindingInfo p.ModuleMembers
        writeDenseTable w writeBinderId (fun w (s: string) -> w.Write s) p.TopLevelNames
        writeDenseTable w writeBinderId writeClosureRepr p.ClosureReprs
        writeDenseTable w writeExprPoolId writeFunVerdict p.FunVerdicts
        writeDenseTable w writeBinderId (fun w cs -> writeListWith w writeFrozenConstraint cs) p.GenericFnSchemes
        writeDenseTable w writeBinderId writeValRepr p.BindingValReprs
        writeDenseTable w writeBinderId (fun w (i: int) -> w.Write i) p.BindingTyparArities

    let private readPools (r: BinaryReader) : FrozenPools =
        let exprShapes = readArrayWith r readExprShape
        let exprTys = readArrayWith r readFrozenType
        let exprToks = readArrayWith r readSyntaxToken
        let exprChildren = readIdColumn r readExprPoolId
        let exprPatChildren = readIdColumn r readPatPoolId
        let exprVarBinder = readArrayWith r (fun r -> readVOptionWith r readBinderId)
        let exprPayloads = readArrayWith r readExprPayload
        let patShapes = readArrayWith r readPatShape
        let patTys = readArrayWith r readFrozenType
        let patToks = readArrayWith r readSyntaxToken
        let patChildren = readIdColumn r readPatPoolId
        let patPayloads = readArrayWith r readPatPayload
        let declShapes = readArrayWith r readDeclShape
        let declExprChildren = readIdColumn r readExprPoolId
        let declPatChildren = readIdColumn r readPatPoolId
        let declPayloads = readArrayWith r readDeclPayload
        let roots = readArrayWith r readDeclPoolId
        let inlineTemplates = readArrayWith r readInlineTemplate
        let binderKeys = readArrayWith r readNodeKey
        let residue = readResidue r
        let moduleMembers = readDenseTable r readBinderId readModuleBindingInfo

        let topLevelNames = readDenseTable r readBinderId (fun r -> r.ReadString())

        let closureReprs = readDenseTable r readBinderId readClosureRepr
        let funVerdicts = readDenseTable r readExprPoolId readFunVerdict

        let genericFnSchemes =
            readDenseTable r readBinderId (fun r -> readListWith r readFrozenConstraint)

        let bindingValReprs = readDenseTable r readBinderId readValRepr

        let bindingTyparArities = readDenseTable r readBinderId (fun r -> r.ReadInt32())

        {
            ExprShapes = exprShapes
            ExprTys = exprTys
            ExprToks = exprToks
            ExprChildren = exprChildren
            ExprPatChildren = exprPatChildren
            ExprVarBinder = exprVarBinder
            ExprPayloads = exprPayloads
            PatShapes = patShapes
            PatTys = patTys
            PatToks = patToks
            PatChildren = patChildren
            PatPayloads = patPayloads
            DeclShapes = declShapes
            DeclExprChildren = declExprChildren
            DeclPatChildren = declPatChildren
            DeclPayloads = declPayloads
            Roots = roots
            InlineTemplates = inlineTemplates
            BinderKeys = binderKeys
            Residue = residue
            ModuleMembers = moduleMembers
            TopLevelNames = topLevelNames
            ClosureReprs = closureReprs
            FunVerdicts = funVerdicts
            GenericFnSchemes = genericFnSchemes
            BindingValReprs = bindingValReprs
            BindingTyparArities = bindingTyparArities
        }

    // ── the whole frozen file (top-level entry points) ──────────────────────

    /// Flatten an entire frozen file to a byte blob: write the columns. The pools ARE the
    /// stored form, so this is the column writers and nothing else. No interning and no
    /// compression — `Compression` wraps the blob at the store seam, and the cache key
    /// hashes INPUTS, not the blob, so no byte canonicalization is owed here. `thaw` is the
    /// exact inverse.
    let flatten (pools: FrozenPools) : byte[] = toBytes writePools pools

    /// Rebuild the frozen file's pools from a `flatten` blob. The `Residue`'s two
    /// `IReadOnlyDictionary` fields come back as concrete `Dictionary`s (reference
    /// equality), so a whole-record `=` on a thawed file is NOT sound — compare through
    /// `TastUnpool.ofPools` and `TastFileG.structurallyEqual`.
    let thaw (bytes: byte[]) : FrozenPools = ofBytes readPools bytes
