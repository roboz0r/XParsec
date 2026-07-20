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

    and private readFtArray (r: BinaryReader) : EqArray<FrozenType> = EqArray.ofArray (readArrayWith r readFrozenType)

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
