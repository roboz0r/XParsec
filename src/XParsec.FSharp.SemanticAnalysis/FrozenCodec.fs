namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// A hand-rolled structural binary (de)serializer for the FROZEN leaf domains —
/// `FrozenType` and the `SymbolKey`/`TypeKey` key cluster it reaches, plus the two
/// value structs a frozen node anchors on (`NodeKey`, `SyntaxToken`). Deliberately
/// throwaway (a plain recursive `BinaryWriter`/`BinaryReader` over a `MemoryStream`):
/// it exists only to prove the leaf domains round-trip structurally, ahead of the
/// tree (`TExpr`/`TDecl`/`TPat`) and side-table (de)serialization that extends this
/// same module.
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
    // evaluation, so field read order provably matches the writer's emit order.

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

    and private writeFtArray (w: BinaryWriter) (xs: EqArray<FrozenType>) = writeEqArrayWith w writeFrozenType xs

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

    and writeTypeKey (w: BinaryWriter) (k: TypeKey) =
        writeTypeHolder w k.Holder
        w.Write k.Name
        w.Write k.TyparArity

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

    and private writeNamespaceKey (w: BinaryWriter) (ns: NamespaceKey) = writeStringArray w ns.Path

    and private writeModuleKey (w: BinaryWriter) (m: ModuleKey) =
        writeModuleHolder w m.Holder
        w.Write m.Name

    and private writeModuleHolder (w: BinaryWriter) (h: ModuleHolder) =
        match h with
        | ModuleHolder.InNamespace ns ->
            w.Write 0uy
            writeNamespaceKey w ns
        | ModuleHolder.InModule parent ->
            w.Write 1uy
            writeModuleKey w parent

    and private writeBindingKey (w: BinaryWriter) (b: BindingKey) =
        writeModuleHolder w b.Decl
        w.Write b.Name

    and private writeMemberKey (w: BinaryWriter) (m: MemberKey) =
        writeTypeKey w m.Decl
        w.Write m.Name
        writeFtArray w m.ArgSig
        w.Write m.MethodTyparArity
        writeMemberKind w m.Kind

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

    let rec readFrozenType (r: BinaryReader) : FrozenType =
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

    and private readFtArray (r: BinaryReader) : EqArray<FrozenType> =
        EqArray.ofArray (readArrayWith r readFrozenType)

    and readSymbolKey (r: BinaryReader) : SymbolKey =
        match r.ReadByte() with
        | 0uy -> SymbolKey.Type(readTypeKey r)
        | 1uy -> SymbolKey.Binding(readBindingKey r)
        | 2uy -> SymbolKey.Member(readMemberKey r)
        | b -> failwithf "FrozenCodec: unknown SymbolKey tag %d" b

    and readTypeKey (r: BinaryReader) : TypeKey =
        let holder = readTypeHolder r
        let name = r.ReadString()
        let arity = r.ReadInt32()

        {
            Holder = holder
            Name = name
            TyparArity = arity
        }

    and private readTypeHolder (r: BinaryReader) : TypeHolder =
        match r.ReadByte() with
        | 0uy -> TypeHolder.InNamespace(readNamespaceKey r)
        | 1uy -> TypeHolder.InModule(readModuleKey r)
        | 2uy -> TypeHolder.InType(readTypeKey r)
        | b -> failwithf "FrozenCodec: unknown TypeHolder tag %d" b

    and private readNamespaceKey (r: BinaryReader) : NamespaceKey = { Path = readStringArray r }

    and private readModuleKey (r: BinaryReader) : ModuleKey =
        let holder = readModuleHolder r
        let name = r.ReadString()
        { Holder = holder; Name = name }

    and private readModuleHolder (r: BinaryReader) : ModuleHolder =
        match r.ReadByte() with
        | 0uy -> ModuleHolder.InNamespace(readNamespaceKey r)
        | 1uy -> ModuleHolder.InModule(readModuleKey r)
        | b -> failwithf "FrozenCodec: unknown ModuleHolder tag %d" b

    and private readBindingKey (r: BinaryReader) : BindingKey =
        let decl = readModuleHolder r
        let name = r.ReadString()
        { Decl = decl; Name = name }

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

    and private readMemberKind (r: BinaryReader) : MemberKind =
        match r.ReadByte() with
        | 0uy -> MemberKind.Method
        | 1uy -> MemberKind.Property
        | 2uy -> MemberKind.InterfaceMethod(readTypeKey r)
        | 3uy -> MemberKind.ExplicitInterfaceImpl(readTypeKey r)
        | b -> failwithf "FrozenCodec: unknown MemberKind tag %d" b

    // ── container helpers (option / voption / list / map / dictionary) ──────
    //
    // All length- or tag-prefixed, mirroring `writeEqArrayWith`/`readArrayWith`: the
    // reader consumes exactly what the writer emitted, in order. `Map<NodeKey,_>` and
    // the two `IReadOnlyDictionary<SymbolKey,_>` fields serialize as a length-prefixed
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

    let private writeNodeKeyMap (w: BinaryWriter) (writeVal: BinaryWriter -> 'v -> unit) (m: Map<NodeKey, 'v>) =
        w.Write m.Count

        for KeyValue(k, v) in m do
            writeNodeKey w k
            writeVal w v

    let private readNodeKeyMap (r: BinaryReader) (readVal: BinaryReader -> 'v) : Map<NodeKey, 'v> =
        let n = r.ReadInt32()
        let mutable acc = Map.empty

        for _ in 1..n do
            let k = readNodeKey r
            let v = readVal r
            acc <- Map.add k v acc

        acc

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

    // ── the term / declaration tree (one mutually recursive writer group) ───
    //
    // Instantiated at `<FrozenType, SyntaxToken>` — every `'ty` payload rides
    // `writeFrozenType`, every `'tok` rides `writeSyntaxToken` (both defined in the
    // leaf group above). Each writer's `match` is EXHAUSTIVE with no catch-all, so a
    // new case fails to compile here; each reader reconstructs the case / record
    // DIRECTLY (never a normalizing smart constructor) with `let`-sequenced field
    // reads that provably mirror the writer's emit order.

    let rec private writeExpr (w: BinaryWriter) (e: Frozen.TExpr) =
        match e with
        | TExprG.Const(value, ty, tok) ->
            w.Write 0uy
            writeTConstValue w value
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Var(binding, ty, tok) ->
            w.Write 1uy
            writeNodeKey w binding
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.External(compiledName, key, ty, tok) ->
            w.Write 2uy
            w.Write compiledName
            writeVOptionWith w writeSymbolKey key
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Lambda(param, body, ty, tok) ->
            w.Write 3uy
            writePat w param
            writeExpr w body
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.App(fn, arg, ty, tok) ->
            w.Write 4uy
            writeExpr w fn
            writeExpr w arg
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Let(binding, value, body, ty, tok) ->
            w.Write 5uy
            writePat w binding
            writeExpr w value
            writeExpr w body
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Use(binding, value, body, dispose, ty, tok) ->
            w.Write 6uy
            writePat w binding
            writeExpr w value
            writeExpr w body
            writeDisposal w dispose
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.IfThenElse(cond, thenExpr, elseExpr, ty, tok) ->
            w.Write 7uy
            writeExpr w cond
            writeExpr w thenExpr
            writeExpr w elseExpr
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Tuple(items, ty, tok) ->
            w.Write 8uy
            writeEqArrayWith w writeExpr items
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Sequential(items, ty, tok) ->
            w.Write 9uy
            writeEqArrayWith w writeExpr items
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.While(cond, body, ty, tok) ->
            w.Write 10uy
            writeExpr w cond
            writeExpr w body
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.ForTo(var, identTok, startExpr, endExpr, body, ty, tok) ->
            w.Write 11uy
            writeNodeKey w var
            writeSyntaxToken w identTok
            writeExpr w startExpr
            writeExpr w endExpr
            writeExpr w body
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.ForIn(pat, source, body, enumerator, ty, tok) ->
            w.Write 12uy
            writePat w pat
            writeExpr w source
            writeExpr w body
            writeForInEnumerator w enumerator
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Match(scrutinee, arms, ty, tok) ->
            w.Write 13uy
            writeExpr w scrutinee
            writeEqArrayWith w writeMatchArm arms
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.TryWith(body, arms, ty, tok) ->
            w.Write 14uy
            writeExpr w body
            writeEqArrayWith w writeMatchArm arms
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.TryFinally(body, cleanup, ty, tok) ->
            w.Write 15uy
            writeExpr w body
            writeExpr w cleanup
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Assignment(lhs, rhs, ty, tok) ->
            w.Write 16uy
            writeExpr w lhs
            writeExpr w rhs
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Null(ty, tok) ->
            w.Write 17uy
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Range(startExpr, step, stopExpr, ty, tok) ->
            w.Write 18uy
            writeExpr w startExpr
            writeOptionWith w writeExpr step
            writeExpr w stopExpr
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.RecordCons(fields, ty, tok) ->
            w.Write 19uy

            writeEqArrayWith
                w
                (fun w (name: string, e) ->
                    w.Write name
                    writeExpr w e
                )
                fields

            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.RecordClone(source, overrides, ty, tok) ->
            w.Write 20uy
            writeExpr w source

            writeEqArrayWith
                w
                (fun w (name: string, e) ->
                    w.Write name
                    writeExpr w e
                )
                overrides

            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.FieldGet(receiver, fieldName, ty, tok) ->
            w.Write 21uy
            writeExpr w receiver
            w.Write fieldName
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.FieldSet(receiver, fieldName, value, ty, tok) ->
            w.Write 22uy
            writeExpr w receiver
            w.Write fieldName
            writeExpr w value
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.UnionCons(caseName, args, ty, tok) ->
            w.Write 23uy
            w.Write caseName
            writeEqArrayWith w writeExpr args
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.New(className, key, args, ty, tok) ->
            w.Write 24uy
            w.Write className
            writeVOptionWith w writeSymbolKey key
            writeEqArrayWith w writeExpr args
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.MethodCall(receiver, key, via, args, ty, tok) ->
            w.Write 25uy
            writeExpr w receiver
            writeSymbolKey w key
            writeCallVia w via
            writeEqArrayWith w writeExpr args
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.PropertyGet(receiver, key, via, ty, tok) ->
            w.Write 26uy
            writeExpr w receiver
            writeSymbolKey w key
            writeCallVia w via
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.StaticMethodCall(key, args, ty, tok) ->
            w.Write 27uy
            writeSymbolKey w key
            writeEqArrayWith w writeExpr args
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.StaticPropertyGet(key, ty, tok) ->
            w.Write 28uy
            writeSymbolKey w key
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.StaticFieldGet(declKey, fieldName, ty, tok) ->
            w.Write 29uy
            writeSymbolKey w declKey
            w.Write fieldName
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.StaticFieldSet(declKey, fieldName, value, ty, tok) ->
            w.Write 30uy
            writeSymbolKey w declKey
            w.Write fieldName
            writeExpr w value
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.ExternalMember(receiver, key, memberName, storage, ty, tok) ->
            w.Write 31uy
            writeVOptionWith w writeExpr receiver
            writeSymbolKey w key
            w.Write memberName
            writeMemberStorage w storage
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Format(sink, segments, ty, tok) ->
            w.Write 32uy
            writeFormatSink w sink
            writeEqArrayWith w writeFormatSeg segments
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.ILIntrinsic(opCode, typeOperand, args, ty, tok) ->
            w.Write 33uy
            w.Write opCode
            writeVOptionWith w writeFrozenType typeOperand
            writeEqArrayWith w writeExpr args
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.StaticOptimization(clauses, defaultExpr, ty, tok) ->
            w.Write 34uy
            writeEqArrayWith w writeStaticOptClause clauses
            writeExpr w defaultExpr
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Upcast(source, ty, tok) ->
            w.Write 35uy
            writeExpr w source
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.Downcast(source, ty, tok) ->
            w.Write 36uy
            writeExpr w source
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.TypeTest(source, testTy, ty, tok) ->
            w.Write 37uy
            writeExpr w source
            writeFrozenType w testTy
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TExprG.TraitCall(receiver, memberName, args, ty, tok) ->
            w.Write 38uy
            writeFrozenType w receiver
            w.Write memberName
            writeEqArrayWith w writeExpr args
            writeFrozenType w ty
            writeSyntaxToken w tok

    and private writePat (w: BinaryWriter) (p: Frozen.TPat) =
        match p with
        | TPatG.NamedSimple(binding, ty, tok) ->
            w.Write 0uy
            writeNodeKey w binding
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Wildcard(ty, tok) ->
            w.Write 1uy
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Tuple(items, ty, tok) ->
            w.Write 2uy
            writeEqArrayWith w writePat items
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Const(value, ty, tok) ->
            w.Write 3uy
            writeTConstValue w value
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Record(fields, ty, tok) ->
            w.Write 4uy

            writeEqArrayWith
                w
                (fun w (name: string, sub) ->
                    w.Write name
                    writePat w sub
                )
                fields

            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Union(caseName, fields, ty, tok) ->
            w.Write 5uy
            w.Write caseName
            writeEqArrayWith w writePat fields
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.TypeTestAs(testTy, inner, ty, tok) ->
            w.Write 6uy
            writeFrozenType w testTy
            writePat w inner
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Null(ty, tok) ->
            w.Write 7uy
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.EnumCase(enumKey, caseName, ty, tok) ->
            w.Write 8uy
            writeSymbolKey w enumKey
            w.Write caseName
            writeFrozenType w ty
            writeSyntaxToken w tok
        | TPatG.Or(alts, ty, tok) ->
            w.Write 9uy
            writeEqArrayWith w writePat alts
            writeFrozenType w ty
            writeSyntaxToken w tok

    and private writeDisposal (w: BinaryWriter) (d: Disposal) =
        match d with
        | Disposal.ViaCapability slot ->
            w.Write 0uy
            writeSymbolKey w slot
        | Disposal.ViaOwnMember key ->
            w.Write 1uy
            writeSymbolKey w key
        | Disposal.Unresolved -> w.Write 2uy

    and private writeCallVia (w: BinaryWriter) (v: CallVia<FrozenType>) =
        match v with
        | CallVia.Self -> w.Write 0uy
        | CallVia.Base -> w.Write 1uy
        | CallVia.Interface ifaceArgs ->
            w.Write 2uy
            writeEqArrayWith w writeFrozenType ifaceArgs

    and private writeMatchArm (w: BinaryWriter) (arm: Frozen.TMatchArm) =
        writePat w arm.Pat
        writeOptionWith w writeExpr arm.Guard
        writeExpr w arm.Body

    and private writeFormatSink (w: BinaryWriter) (s: Frozen.FormatSink) =
        match s with
        | FormatSinkG.ToStdOut newline ->
            w.Write 0uy
            w.Write newline
        | FormatSinkG.ToStdErr newline ->
            w.Write 1uy
            w.Write newline
        | FormatSinkG.ToWriter(writer, newline) ->
            w.Write 2uy
            writeExpr w writer
            w.Write newline
        | FormatSinkG.ToBuilder builder ->
            w.Write 3uy
            writeExpr w builder
        | FormatSinkG.ToString -> w.Write 4uy

    and private writeFormatSeg (w: BinaryWriter) (seg: Frozen.FormatSeg) =
        match seg with
        | FormatSegG.Lit s ->
            w.Write 0uy
            w.Write s
        | FormatSegG.Hole(spec, e) ->
            w.Write 1uy
            writeHoleSpec w spec
            writeExpr w e
        | FormatSegG.DynHole hole ->
            w.Write 2uy
            writeDynHole w hole
        | FormatSegG.CallbackHole(spec, residue) ->
            w.Write 3uy
            writeHoleSpec w spec
            writeExpr w residue

    and private writeDynHole (w: BinaryWriter) (h: Frozen.DynFormatHole) =
        writeVOptionWith w writeExpr h.Width
        writeVOptionWith w writeExpr h.Precision
        writeHoleSpec w h.Spec
        writeExpr w h.Value

    and private writeStaticOptConstraint (w: BinaryWriter) (c: Frozen.TStaticOptConstraint) =
        match c with
        | TStaticOptConstraintG.TyconEquals(typar, required) ->
            w.Write 0uy
            writeFrozenType w typar
            writeFrozenType w required
        | TStaticOptConstraintG.IsStruct typar ->
            w.Write 1uy
            writeFrozenType w typar

    and private writeStaticOptClause (w: BinaryWriter) (c: Frozen.TStaticOptClause) =
        writeEqArrayWith w writeStaticOptConstraint c.Constraints
        writeExpr w c.Body

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

    // Declaration cluster.

    and private writeDecl (w: BinaryWriter) (d: Frozen.TDecl) =
        match d with
        | TDeclG.Let(binding, value, isInline, ty) ->
            w.Write 0uy
            writePat w binding
            writeExpr w value
            w.Write isInline
            writeFrozenType w ty
        | TDeclG.Expression(expr, ty) ->
            w.Write 1uy
            writeExpr w expr
            writeFrozenType w ty
        | TDeclG.Type td ->
            w.Write 2uy
            writeTypeDecl w td

    and private writeTypeDecl (w: BinaryWriter) (td: Frozen.TTypeDecl) =
        w.Write td.Name
        writeTypeKey w td.TypeKey
        writeOptionWith w (fun w (s: string) -> w.Write s) td.Namespace
        writeStringArray w td.TypeParams
        w.Write td.IsRequireQualifiedAccess
        writeTypeKind w td.Kind
        writeEqualityVerdict w td.EqualitySupport
        writeComparisonVerdict w td.ComparisonSupport

    and private writeTypeKind (w: BinaryWriter) (k: Frozen.TTypeKind) =
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

    // Each `interfaces` entry pairs a resolved interface type with its typed member
    // bodies — shared by the class / union / record arms.
    and private writeInterfaces (w: BinaryWriter) (interfaces: EqArray<FrozenType * EqArray<Frozen.TTypeMember>>) =
        writeEqArrayWith
            w
            (fun w (ty, mems) ->
                writeFrozenType w ty
                writeEqArrayWith w writeTypeMember mems
            )
            interfaces

    and private writeClass (w: BinaryWriter) (c: Frozen.TClass) =
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

    and private writeTypeMember (w: BinaryWriter) (m: Frozen.TTypeMember) =
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

        writeExpr w m.Body
        writeFrozenType w m.ReturnTy
        writeMethodTypeParams w m.MethodTypeParams

    and private writeClassLet (w: BinaryWriter) (l: Frozen.TClassLet) =
        w.Write l.Name
        writeFrozenType w l.Type
        w.Write l.IsMutable
        writeExpr w l.Init

    and private writePreambleEntry (w: BinaryWriter) (p: Frozen.TPreambleEntry) =
        match p with
        | TPreambleEntryG.Let l ->
            w.Write 0uy
            writeClassLet w l
        | TPreambleEntryG.Do e ->
            w.Write 1uy
            writeExpr w e

    and private writeCtorLet (w: BinaryWriter) (cl: Frozen.TCtorLet) =
        writeNodeKey w cl.Binder
        writeFrozenType w cl.Type
        writeExpr w cl.Init

    and private writeCtorFieldInit (w: BinaryWriter) (fi: Frozen.TCtorFieldInit) =
        w.Write fi.Field
        writeExpr w fi.Init

    and private writeSecondaryCtor (w: BinaryWriter) (sc: Frozen.TSecondaryCtor) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeNodeKey w k
                writeFrozenType w ty
            )
            sc.Params

        writeEqArrayWith w writeCtorLet sc.Lets
        writeEqArrayWith w writeExpr sc.PrimaryArgs
        writeEqArrayWith w writeCtorFieldInit sc.FieldInits

    and private writeBaseCtorCall (w: BinaryWriter) (bc: Frozen.TBaseCtorCall) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeNodeKey w k
                writeFrozenType w ty
            )
            bc.CtorParams

        writeEqArrayWith w writeExpr bc.Args
        writeVOptionWith w writeSymbolKey bc.ChosenCtor

    and private writeInlineBody (w: BinaryWriter) (b: Frozen.TInlineBody) =
        writeDecl w b.Decl
        writeArrayWith w writeParamAttrs b.ParamAttrs

    and private writeInlineValue (w: BinaryWriter) (v: Frozen.TInlineValue) =
        writeSymbolKey w v.Key
        writeInlineBody w v.Body

    and private writeStaticParam (w: BinaryWriter) (p: Frozen.StaticParam) =
        writeNodeKey w p.Slot
        writeFrozenType w p.Ty
        writeOptionWith w writePat p.Pat

    and private writeArgGroup (w: BinaryWriter) (g: Frozen.ArgGroup) =
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
            writePat w pat

    and private writeValRepr (w: BinaryWriter) (v: Frozen.ValRepr) =
        w.Write v.Typars
        writeListWith w writeArgGroup v.Groups
        writeFrozenType w v.ResultTy

    and private writeCompiledReturn (w: BinaryWriter) (c: Frozen.CompiledReturn) =
        match c with
        | CompiledReturnG.RVoid -> w.Write 0uy
        | CompiledReturnG.RValue ty ->
            w.Write 1uy
            writeFrozenType w ty

    and private writeCompiledForm (w: BinaryWriter) (c: Frozen.CompiledForm) =
        writeListWith w writeStaticParam c.Params
        writeCompiledReturn w c.Return

    // ── the mirror reader group ─────────────────────────────────────────────

    let rec private readExpr (r: BinaryReader) : Frozen.TExpr =
        match r.ReadByte() with
        | 0uy ->
            let value = readTConstValue r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Const(value, ty, tok)
        | 1uy ->
            let binding = readNodeKey r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Var(binding, ty, tok)
        | 2uy ->
            let compiledName = r.ReadString()
            let key = readVOptionWith r readSymbolKey
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.External(compiledName, key, ty, tok)
        | 3uy ->
            let param = readPat r
            let body = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Lambda(param, body, ty, tok)
        | 4uy ->
            let fn = readExpr r
            let arg = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.App(fn, arg, ty, tok)
        | 5uy ->
            let binding = readPat r
            let value = readExpr r
            let body = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Let(binding, value, body, ty, tok)
        | 6uy ->
            let binding = readPat r
            let value = readExpr r
            let body = readExpr r
            let dispose = readDisposal r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Use(binding, value, body, dispose, ty, tok)
        | 7uy ->
            let cond = readExpr r
            let thenExpr = readExpr r
            let elseExpr = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.IfThenElse(cond, thenExpr, elseExpr, ty, tok)
        | 8uy ->
            let items = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Tuple(items, ty, tok)
        | 9uy ->
            let items = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Sequential(items, ty, tok)
        | 10uy ->
            let cond = readExpr r
            let body = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.While(cond, body, ty, tok)
        | 11uy ->
            let var = readNodeKey r
            let identTok = readSyntaxToken r
            let startExpr = readExpr r
            let endExpr = readExpr r
            let body = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.ForTo(var, identTok, startExpr, endExpr, body, ty, tok)
        | 12uy ->
            let pat = readPat r
            let source = readExpr r
            let body = readExpr r
            let enumerator = readForInEnumerator r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.ForIn(pat, source, body, enumerator, ty, tok)
        | 13uy ->
            let scrutinee = readExpr r
            let arms = EqArray.ofArray (readArrayWith r readMatchArm)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Match(scrutinee, arms, ty, tok)
        | 14uy ->
            let body = readExpr r
            let arms = EqArray.ofArray (readArrayWith r readMatchArm)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.TryWith(body, arms, ty, tok)
        | 15uy ->
            let body = readExpr r
            let cleanup = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.TryFinally(body, cleanup, ty, tok)
        | 16uy ->
            let lhs = readExpr r
            let rhs = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Assignment(lhs, rhs, ty, tok)
        | 17uy ->
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Null(ty, tok)
        | 18uy ->
            let startExpr = readExpr r
            let step = readOptionWith r readExpr
            let stopExpr = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Range(startExpr, step, stopExpr, ty, tok)
        | 19uy ->
            let fields =
                EqArray.ofArray (
                    readArrayWith
                        r
                        (fun r ->
                            let name = r.ReadString()
                            let e = readExpr r
                            name, e
                        )
                )

            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.RecordCons(fields, ty, tok)
        | 20uy ->
            let source = readExpr r

            let overrides =
                EqArray.ofArray (
                    readArrayWith
                        r
                        (fun r ->
                            let name = r.ReadString()
                            let e = readExpr r
                            name, e
                        )
                )

            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.RecordClone(source, overrides, ty, tok)
        | 21uy ->
            let receiver = readExpr r
            let fieldName = r.ReadString()
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.FieldGet(receiver, fieldName, ty, tok)
        | 22uy ->
            let receiver = readExpr r
            let fieldName = r.ReadString()
            let value = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.FieldSet(receiver, fieldName, value, ty, tok)
        | 23uy ->
            let caseName = r.ReadString()
            let args = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.UnionCons(caseName, args, ty, tok)
        | 24uy ->
            let className = r.ReadString()
            let key = readVOptionWith r readSymbolKey
            let args = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.New(className, key, args, ty, tok)
        | 25uy ->
            let receiver = readExpr r
            let key = readSymbolKey r
            let via = readCallVia r
            let args = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.MethodCall(receiver, key, via, args, ty, tok)
        | 26uy ->
            let receiver = readExpr r
            let key = readSymbolKey r
            let via = readCallVia r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.PropertyGet(receiver, key, via, ty, tok)
        | 27uy ->
            let key = readSymbolKey r
            let args = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.StaticMethodCall(key, args, ty, tok)
        | 28uy ->
            let key = readSymbolKey r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.StaticPropertyGet(key, ty, tok)
        | 29uy ->
            let declKey = readSymbolKey r
            let fieldName = r.ReadString()
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.StaticFieldGet(declKey, fieldName, ty, tok)
        | 30uy ->
            let declKey = readSymbolKey r
            let fieldName = r.ReadString()
            let value = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.StaticFieldSet(declKey, fieldName, value, ty, tok)
        | 31uy ->
            let receiver = readVOptionWith r readExpr
            let key = readSymbolKey r
            let memberName = r.ReadString()
            let storage = readMemberStorage r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.ExternalMember(receiver, key, memberName, storage, ty, tok)
        | 32uy ->
            let sink = readFormatSink r
            let segments = EqArray.ofArray (readArrayWith r readFormatSeg)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Format(sink, segments, ty, tok)
        | 33uy ->
            let opCode = r.ReadString()
            let typeOperand = readVOptionWith r readFrozenType
            let args = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.ILIntrinsic(opCode, typeOperand, args, ty, tok)
        | 34uy ->
            let clauses = EqArray.ofArray (readArrayWith r readStaticOptClause)
            let defaultExpr = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.StaticOptimization(clauses, defaultExpr, ty, tok)
        | 35uy ->
            let source = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Upcast(source, ty, tok)
        | 36uy ->
            let source = readExpr r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.Downcast(source, ty, tok)
        | 37uy ->
            let source = readExpr r
            let testTy = readFrozenType r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.TypeTest(source, testTy, ty, tok)
        | 38uy ->
            let receiver = readFrozenType r
            let memberName = r.ReadString()
            let args = EqArray.ofArray (readArrayWith r readExpr)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TExprG.TraitCall(receiver, memberName, args, ty, tok)
        | b -> failwithf "FrozenCodec: unknown TExpr tag %d" b

    and private readPat (r: BinaryReader) : Frozen.TPat =
        match r.ReadByte() with
        | 0uy ->
            let binding = readNodeKey r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.NamedSimple(binding, ty, tok)
        | 1uy ->
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Wildcard(ty, tok)
        | 2uy ->
            let items = EqArray.ofArray (readArrayWith r readPat)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Tuple(items, ty, tok)
        | 3uy ->
            let value = readTConstValue r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Const(value, ty, tok)
        | 4uy ->
            let fields =
                EqArray.ofArray (
                    readArrayWith
                        r
                        (fun r ->
                            let name = r.ReadString()
                            let sub = readPat r
                            name, sub
                        )
                )

            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Record(fields, ty, tok)
        | 5uy ->
            let caseName = r.ReadString()
            let fields = EqArray.ofArray (readArrayWith r readPat)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Union(caseName, fields, ty, tok)
        | 6uy ->
            let testTy = readFrozenType r
            let inner = readPat r
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.TypeTestAs(testTy, inner, ty, tok)
        | 7uy ->
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Null(ty, tok)
        | 8uy ->
            let enumKey = readSymbolKey r
            let caseName = r.ReadString()
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.EnumCase(enumKey, caseName, ty, tok)
        | 9uy ->
            let alts = EqArray.ofArray (readArrayWith r readPat)
            let ty = readFrozenType r
            let tok = readSyntaxToken r
            TPatG.Or(alts, ty, tok)
        | b -> failwithf "FrozenCodec: unknown TPat tag %d" b

    and private readDisposal (r: BinaryReader) : Disposal =
        match r.ReadByte() with
        | 0uy -> Disposal.ViaCapability(readSymbolKey r)
        | 1uy -> Disposal.ViaOwnMember(readSymbolKey r)
        | 2uy -> Disposal.Unresolved
        | b -> failwithf "FrozenCodec: unknown Disposal tag %d" b

    and private readCallVia (r: BinaryReader) : CallVia<FrozenType> =
        match r.ReadByte() with
        | 0uy -> CallVia.Self
        | 1uy -> CallVia.Base
        | 2uy -> CallVia.Interface(EqArray.ofArray (readArrayWith r readFrozenType))
        | b -> failwithf "FrozenCodec: unknown CallVia tag %d" b

    and private readMatchArm (r: BinaryReader) : Frozen.TMatchArm =
        let pat = readPat r
        let guard = readOptionWith r readExpr
        let body = readExpr r

        {
            Pat = pat
            Guard = guard
            Body = body
        }

    and private readFormatSink (r: BinaryReader) : Frozen.FormatSink =
        match r.ReadByte() with
        | 0uy -> FormatSinkG.ToStdOut(r.ReadBoolean())
        | 1uy -> FormatSinkG.ToStdErr(r.ReadBoolean())
        | 2uy ->
            let writer = readExpr r
            let newline = r.ReadBoolean()
            FormatSinkG.ToWriter(writer, newline)
        | 3uy -> FormatSinkG.ToBuilder(readExpr r)
        | 4uy -> FormatSinkG.ToString
        | b -> failwithf "FrozenCodec: unknown FormatSink tag %d" b

    and private readFormatSeg (r: BinaryReader) : Frozen.FormatSeg =
        match r.ReadByte() with
        | 0uy -> FormatSegG.Lit(r.ReadString())
        | 1uy ->
            let spec = readHoleSpec r
            let e = readExpr r
            FormatSegG.Hole(spec, e)
        | 2uy -> FormatSegG.DynHole(readDynHole r)
        | 3uy ->
            let spec = readHoleSpec r
            let residue = readExpr r
            FormatSegG.CallbackHole(spec, residue)
        | b -> failwithf "FrozenCodec: unknown FormatSeg tag %d" b

    and private readDynHole (r: BinaryReader) : Frozen.DynFormatHole =
        let width = readVOptionWith r readExpr
        let precision = readVOptionWith r readExpr
        let spec = readHoleSpec r
        let value = readExpr r

        {
            Width = width
            Precision = precision
            Spec = spec
            Value = value
        }

    and private readStaticOptConstraint (r: BinaryReader) : Frozen.TStaticOptConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typar = readFrozenType r
            let required = readFrozenType r
            TStaticOptConstraintG.TyconEquals(typar, required)
        | 1uy -> TStaticOptConstraintG.IsStruct(readFrozenType r)
        | b -> failwithf "FrozenCodec: unknown TStaticOptConstraint tag %d" b

    and private readStaticOptClause (r: BinaryReader) : Frozen.TStaticOptClause =
        let constraints = EqArray.ofArray (readArrayWith r readStaticOptConstraint)
        let body = readExpr r

        {
            Constraints = constraints
            Body = body
        }

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

    and private readForInGetEnum (r: BinaryReader) : ForInGetEnumG<FrozenType> =
        match r.ReadByte() with
        | 0uy -> ForInGetEnumG.External(readSymbolKey r)
        | 1uy -> ForInGetEnumG.Local
        | 2uy ->
            let iface = readTypeKey r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readFrozenType)
            ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInGetEnum tag %d" b

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

    and private readDecl (r: BinaryReader) : Frozen.TDecl =
        match r.ReadByte() with
        | 0uy ->
            let binding = readPat r
            let value = readExpr r
            let isInline = r.ReadBoolean()
            let ty = readFrozenType r
            TDeclG.Let(binding, value, isInline, ty)
        | 1uy ->
            let expr = readExpr r
            let ty = readFrozenType r
            TDeclG.Expression(expr, ty)
        | 2uy -> TDeclG.Type(readTypeDecl r)
        | b -> failwithf "FrozenCodec: unknown TDecl tag %d" b

    and private readTypeDecl (r: BinaryReader) : Frozen.TTypeDecl =
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

    and private readTypeKind (r: BinaryReader) : Frozen.TTypeKind =
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

    and private readInterfaces (r: BinaryReader) : EqArray<FrozenType * EqArray<Frozen.TTypeMember>> =
        EqArray.ofArray (
            readArrayWith
                r
                (fun r ->
                    let ty = readFrozenType r
                    let mems = EqArray.ofArray (readArrayWith r readTypeMember)
                    ty, mems
                )
        )

    and private readClass (r: BinaryReader) : Frozen.TClass =
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

    and private readTypeMember (r: BinaryReader) : Frozen.TTypeMember =
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

        let body = readExpr r
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

    and private readClassLet (r: BinaryReader) : Frozen.TClassLet =
        let name = r.ReadString()
        let ty = readFrozenType r
        let isMutable = r.ReadBoolean()
        let init = readExpr r

        {
            Name = name
            Type = ty
            IsMutable = isMutable
            Init = init
        }

    and private readPreambleEntry (r: BinaryReader) : Frozen.TPreambleEntry =
        match r.ReadByte() with
        | 0uy -> TPreambleEntryG.Let(readClassLet r)
        | 1uy -> TPreambleEntryG.Do(readExpr r)
        | b -> failwithf "FrozenCodec: unknown TPreambleEntry tag %d" b

    and private readCtorLet (r: BinaryReader) : Frozen.TCtorLet =
        let binder = readNodeKey r
        let ty = readFrozenType r
        let init = readExpr r

        {
            Binder = binder
            Type = ty
            Init = init
        }

    and private readCtorFieldInit (r: BinaryReader) : Frozen.TCtorFieldInit =
        let field = r.ReadString()
        let init = readExpr r
        { Field = field; Init = init }

    and private readSecondaryCtor (r: BinaryReader) : Frozen.TSecondaryCtor =
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
        let primaryArgs = EqArray.ofArray (readArrayWith r readExpr)
        let fieldInits = EqArray.ofArray (readArrayWith r readCtorFieldInit)

        {
            Params = parameters
            Lets = lets
            PrimaryArgs = primaryArgs
            FieldInits = fieldInits
        }

    and private readBaseCtorCall (r: BinaryReader) : Frozen.TBaseCtorCall =
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

        let args = EqArray.ofArray (readArrayWith r readExpr)
        let chosenCtor = readVOptionWith r readSymbolKey

        {
            CtorParams = ctorParams
            Args = args
            ChosenCtor = chosenCtor
        }

    and private readInlineBody (r: BinaryReader) : Frozen.TInlineBody =
        let decl = readDecl r
        let paramAttrs = readArrayWith r readParamAttrs
        { Decl = decl; ParamAttrs = paramAttrs }

    and private readInlineValue (r: BinaryReader) : Frozen.TInlineValue =
        let key = readSymbolKey r
        let body = readInlineBody r
        { Key = key; Body = body }

    and private readStaticParam (r: BinaryReader) : Frozen.StaticParam =
        let slot = readNodeKey r
        let ty = readFrozenType r
        let pat = readOptionWith r readPat
        { Slot = slot; Ty = ty; Pat = pat }

    and private readArgGroup (r: BinaryReader) : Frozen.ArgGroup =
        match r.ReadByte() with
        | 0uy -> ArgGroupG.GUnit(readFrozenType r)
        | 1uy ->
            let slot = readNodeKey r
            let ty = readFrozenType r
            ArgGroupG.GSimple(slot, ty)
        | 2uy -> ArgGroupG.GTuple(readPat r)
        | b -> failwithf "FrozenCodec: unknown ArgGroup tag %d" b

    and private readValRepr (r: BinaryReader) : Frozen.ValRepr =
        let typars = r.ReadInt32()
        let groups = readListWith r readArgGroup
        let resultTy = readFrozenType r

        {
            Typars = typars
            Groups = groups
            ResultTy = resultTy
        }

    and private readCompiledReturn (r: BinaryReader) : Frozen.CompiledReturn =
        match r.ReadByte() with
        | 0uy -> CompiledReturnG.RVoid
        | 1uy -> CompiledReturnG.RValue(readFrozenType r)
        | b -> failwithf "FrozenCodec: unknown CompiledReturn tag %d" b

    and private readCompiledForm (r: BinaryReader) : Frozen.CompiledForm =
        let parameters = readListWith r readStaticParam
        let ret = readCompiledReturn r
        { Params = parameters; Return = ret }

    // ── the whole frozen file (top-level entry points) ──────────────────────

    let private writeTastFile (w: BinaryWriter) (f: Frozen.TastFile) =
        writeEqArrayWith w writeDecl f.Decls
        writeListWith w writeDiagnostic f.Diagnostics
        writeSymbolDict w writeIntrinsicReprInfo f.IntrinsicReprKeys
        writeNodeKeyMap w writeModuleBindingInfo f.ModuleMembers
        writeNodeKeyMap w (fun w (s: string) -> w.Write s) f.TopLevelNames
        writeNodeKeyMap w writeClosureRepr f.ClosureReprs
        writeNodeKeyMap w writeFunVerdict f.FunVerdicts
        writeNodeKeyMap w (fun w cs -> writeListWith w writeFrozenConstraint cs) f.GenericFnSchemes
        writeEqArrayWith w writeInlineValue f.InlineBodies
        writeSymbolDict w writeAccessibility f.Accessibility
        writeNodeKeyMap w writeValRepr f.BindingValReprs
        writeNodeKeyMap w (fun w (i: int) -> w.Write i) f.BindingTyparArities

    let private readTastFile (r: BinaryReader) : Frozen.TastFile =
        let decls = EqArray.ofArray (readArrayWith r readDecl)
        let diagnostics = readListWith r readDiagnostic
        let intrinsicReprKeys = readSymbolDict r readIntrinsicReprInfo
        let moduleMembers = readNodeKeyMap r readModuleBindingInfo
        let topLevelNames = readNodeKeyMap r (fun r -> r.ReadString())
        let closureReprs = readNodeKeyMap r readClosureRepr
        let funVerdicts = readNodeKeyMap r readFunVerdict

        let genericFnSchemes =
            readNodeKeyMap r (fun r -> readListWith r readFrozenConstraint)

        let inlineBodies = EqArray.ofArray (readArrayWith r readInlineValue)
        let accessibility = readSymbolDict r readAccessibility
        let bindingValReprs = readNodeKeyMap r readValRepr
        let bindingTyparArities = readNodeKeyMap r (fun r -> r.ReadInt32())

        {
            Decls = decls
            Diagnostics = diagnostics
            IntrinsicReprKeys = intrinsicReprKeys
            ModuleMembers = moduleMembers
            TopLevelNames = topLevelNames
            ClosureReprs = closureReprs
            FunVerdicts = funVerdicts
            GenericFnSchemes = genericFnSchemes
            InlineBodies = inlineBodies
            Accessibility = accessibility
            BindingValReprs = bindingValReprs
            BindingTyparArities = bindingTyparArities
        }

    /// Flatten an entire frozen file to a length-prefixed byte blob — VERBATIM (no
    /// interning / compression / pools). `thaw` is its exact structural inverse.
    let flatten (f: Frozen.TastFile) : byte[] = toBytes writeTastFile f

    /// Rebuild the frozen file from a `flatten` blob. The two `IReadOnlyDictionary`
    /// fields come back as concrete `Dictionary`s (reference equality), so a
    /// whole-record `=` is NOT sound on them — compare those two fields as key→value
    /// sets (see the round-trip gate).
    let thaw (bytes: byte[]) : Frozen.TastFile = ofBytes readTastFile bytes
