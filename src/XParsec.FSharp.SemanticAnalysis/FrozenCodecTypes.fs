namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives

/// The FROZEN type domain: `FrozenType` and the `SymbolKey`/`TypeKey` cluster it interlocks
/// with, the non-generic leaf payloads the tree and the side tables carry, the printf
/// hole-form cluster, and the leaf type-declaration payloads. Every writer here bottoms out
/// in `FrozenCodecPrimitives`; `FrozenCodecDecls` (the declaration shell) and `FrozenCodec`
/// (the pool columns) read this module, never the reverse.
module FrozenCodecTypes =

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
        | FTLocalTypar(SchemeId scheme, index) ->
            w.Write 13uy
            w.Write scheme
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
            let scheme = r.ReadInt32()
            let index = r.ReadInt32()
            FTLocalTypar(SchemeId scheme, index)
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

    // ── the `SymbolKey`-keyed container ─────────────────────────────────────
    //
    // The one container helper that names a domain: it rides `writeSymbolKey` above, so it
    // cannot sit with the generic containers in `FrozenCodecPrimitives`. The two
    // `IReadOnlyDictionary<SymbolKey,_>` fields serialize as a length-prefixed
    // (key, value) sequence — no canonical order is imposed (the cache key hashes
    // inputs, not the blob), so emit order is free and read rebuilds an unordered map.

    /// The two `IReadOnlyDictionary<SymbolKey,_>` fields — rebuilt on read as a
    /// concrete `Dictionary` exposed through the read-only face, exactly how
    /// `Elaborate` constructs `IntrinsicReprKeys` / `Accessibility`.
    let writeSymbolDict
        (w: BinaryWriter)
        (writeVal: BinaryWriter -> 'v -> unit)
        (d: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>)
        =
        w.Write d.Count

        for KeyValue(k, v) in d do
            writeSymbolKey w k
            writeVal w v

    let readSymbolDict
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

    let writeTConstValue (w: BinaryWriter) (v: TConstValue) =
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

    let readTConstValue (r: BinaryReader) : TConstValue =
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

    // `Diagnostic` is qualified below rather than aliased; see the type's declaration for
    // why the bare name would otherwise be the parser's.
    let private writeLabel (w: BinaryWriter) (l: Label) =
        writeSite w l.Site
        w.Write l.Message

    let private readLabel (r: BinaryReader) : Label =
        let site = readSite r
        let message = r.ReadString()
        { Site = site; Message = message }

    let writeDiagnostic (w: BinaryWriter) (d: XParsec.FSharp.SemanticAnalysis.Diagnostic) =
        writeSite w d.Site
        w.Write d.Code
        w.Write d.Message
        writeSeverity w d.Severity
        writeListWith w writeLabel d.Related

    let readDiagnostic (r: BinaryReader) : XParsec.FSharp.SemanticAnalysis.Diagnostic =
        let site = readSite r
        let code = r.ReadString()
        let message = r.ReadString()
        let severity = readSeverity r
        let related = readListWith r readLabel

        {
            Code = code
            Message = message
            Severity = severity
            Site = site
            Related = related
        }

    let writeAccessibility (w: BinaryWriter) (a: Accessibility) =
        match a with
        | Accessibility.Public -> w.Write 0uy
        | Accessibility.Internal -> w.Write 1uy
        | Accessibility.Private -> w.Write 2uy

    let readAccessibility (r: BinaryReader) : Accessibility =
        match r.ReadByte() with
        | 0uy -> Accessibility.Public
        | 1uy -> Accessibility.Internal
        | 2uy -> Accessibility.Private
        | b -> failwithf "FrozenCodec: unknown Accessibility tag %d" b

    let writeClassValueKind (w: BinaryWriter) (k: ClassValueKind) =
        match k with
        | ClassValueKind.RefType -> w.Write 0uy
        | ClassValueKind.Struct -> w.Write 1uy
        | ClassValueKind.RefStruct -> w.Write 2uy

    let readClassValueKind (r: BinaryReader) : ClassValueKind =
        match r.ReadByte() with
        | 0uy -> ClassValueKind.RefType
        | 1uy -> ClassValueKind.Struct
        | 2uy -> ClassValueKind.RefStruct
        | b -> failwithf "FrozenCodec: unknown ClassValueKind tag %d" b

    let writeEqualityVerdict (w: BinaryWriter) (v: EqualityVerdict) =
        match v with
        | EqualityVerdict.Structural -> w.Write 0uy
        | EqualityVerdict.Reference -> w.Write 1uy
        | EqualityVerdict.Custom -> w.Write 2uy
        | EqualityVerdict.NoEquality -> w.Write 3uy

    let readEqualityVerdict (r: BinaryReader) : EqualityVerdict =
        match r.ReadByte() with
        | 0uy -> EqualityVerdict.Structural
        | 1uy -> EqualityVerdict.Reference
        | 2uy -> EqualityVerdict.Custom
        | 3uy -> EqualityVerdict.NoEquality
        | b -> failwithf "FrozenCodec: unknown EqualityVerdict tag %d" b

    let writeComparisonVerdict (w: BinaryWriter) (v: ComparisonVerdict) =
        match v with
        | ComparisonVerdict.Structural -> w.Write 0uy
        | ComparisonVerdict.Custom -> w.Write 1uy
        | ComparisonVerdict.NoComparison -> w.Write 2uy

    let readComparisonVerdict (r: BinaryReader) : ComparisonVerdict =
        match r.ReadByte() with
        | 0uy -> ComparisonVerdict.Structural
        | 1uy -> ComparisonVerdict.Custom
        | 2uy -> ComparisonVerdict.NoComparison
        | b -> failwithf "FrozenCodec: unknown ComparisonVerdict tag %d" b

    let writeMemberStorage (w: BinaryWriter) (s: MemberStorage) =
        match s with
        | MemberStorage.Field -> w.Write 0uy
        | MemberStorage.Property -> w.Write 1uy
        | MemberStorage.Method -> w.Write 2uy

    let readMemberStorage (r: BinaryReader) : MemberStorage =
        match r.ReadByte() with
        | 0uy -> MemberStorage.Field
        | 1uy -> MemberStorage.Property
        | 2uy -> MemberStorage.Method
        | b -> failwithf "FrozenCodec: unknown MemberStorage tag %d" b

    let writeTMemberKind (w: BinaryWriter) (k: TMemberKind) =
        match k with
        | TMemberKind.Method -> w.Write 0uy
        | TMemberKind.Property -> w.Write 1uy

    let readTMemberKind (r: BinaryReader) : TMemberKind =
        match r.ReadByte() with
        | 0uy -> TMemberKind.Method
        | 1uy -> TMemberKind.Property
        | b -> failwithf "FrozenCodec: unknown TMemberKind tag %d" b

    let writeClosureRepr (w: BinaryWriter) (c: ClosureRepr) =
        match c with
        | ClosureRepr.Heap -> w.Write 0uy
        | ClosureRepr.Stack -> w.Write 1uy

    let readClosureRepr (r: BinaryReader) : ClosureRepr =
        match r.ReadByte() with
        | 0uy -> ClosureRepr.Heap
        | 1uy -> ClosureRepr.Stack
        | b -> failwithf "FrozenCodec: unknown ClosureRepr tag %d" b

    let writeFunVerdict (w: BinaryWriter) (v: FunVerdict) =
        w.Write v.Arity
        writeVOptionWith w (fun w (i: int) -> w.Write i) v.ResultTyparPos

    let readFunVerdict (r: BinaryReader) : FunVerdict =
        let arity = r.ReadInt32()
        let resultTyparPos = readVOptionWith r (fun r -> r.ReadInt32())

        {
            Arity = arity
            ResultTyparPos = resultTyparPos
        }

    /// A frozen typar bound — its `target` is a `FrozenType`, so this reuses the leaf
    /// `writeFrozenType`/`readFrozenType` defined above.
    let writeFrozenConstraint (w: BinaryWriter) (c: FrozenConstraint) =
        match c with
        | FrozenConstraint.Coercion(typarIndex, target) ->
            w.Write 0uy
            w.Write typarIndex
            writeFrozenType w target

    let readFrozenConstraint (r: BinaryReader) : FrozenConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typarIndex = r.ReadInt32()
            let target = readFrozenType r
            FrozenConstraint.Coercion(typarIndex, target)
        | b -> failwithf "FrozenCodec: unknown FrozenConstraint tag %d" b

    let writeModuleBindingInfo (w: BinaryWriter) (m: ModuleBindingInfo) =
        writeModuleKey w m.Holder
        w.Write m.Name

    let readModuleBindingInfo (r: BinaryReader) : ModuleBindingInfo =
        let holder = readModuleKey r
        let name = r.ReadString()
        { Holder = holder; Name = name }

    let writeIntrinsicReprInfo (w: BinaryWriter) (i: IntrinsicReprInfo) =
        w.Write i.Platform
        w.Write i.Heritable

    let readIntrinsicReprInfo (r: BinaryReader) : IntrinsicReprInfo =
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

    let writeParamAttrs (w: BinaryWriter) (a: ParamAttrs) = w.Write a.CallAtMostOnce

    let readParamAttrs (r: BinaryReader) : ParamAttrs = { CallAtMostOnce = r.ReadBoolean() }

    /// A member's own method typars: each entry is the source name + the typar's
    /// frozen type (`FTTypar(Method, i)`), position = ABI index. Plain frozen data —
    /// no union-find cell rides the tree, so this round-trips structurally.
    let writeMethodTypeParams (w: BinaryWriter) (mtps: EqArray<string * FrozenType>) =
        writeEqArrayWith
            w
            (fun w (n: string, ty) ->
                w.Write n
                writeFrozenType w ty
            )
            mtps

    let readMethodTypeParams (r: BinaryReader) : EqArray<string * FrozenType> =
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
    let writeHoleSpec (w: BinaryWriter) (h: Pooled.HoleSpec) =
        writeFrozenType w h.Ty
        writeHoleSpecSource w h.Source
        writeAnchor w h.Tok

    let readHoleSpec (r: BinaryReader) : Pooled.HoleSpec =
        let ty = readFrozenType r
        let source = readHoleSpecSource r
        let tok = readAnchor r
        { Ty = ty; Source = source; Tok = tok }

    // ── the leaf type-declaration payloads (no sub-expression) ──────────────

    let writeAbstractMethod (w: BinaryWriter) (m: Frozen.TAbstractMethod) =
        w.Write m.Name
        writeStringArray w m.MethodTypeParams
        writeFrozenType w m.Signature
        w.Write m.IsProperty

    let readAbstractMethod (r: BinaryReader) : Frozen.TAbstractMethod =
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

    let writeUnionCase (w: BinaryWriter) (c: Frozen.TUnionCase) =
        w.Write c.Name

        writeEqArrayWith
            w
            (fun w (nameOpt: string voption, ty) ->
                writeVOptionWith w (fun w (s: string) -> w.Write s) nameOpt
                writeFrozenType w ty
            )
            c.Fields

    let readUnionCase (r: BinaryReader) : Frozen.TUnionCase =
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

    let writeRecordField (w: BinaryWriter) (f: Frozen.TRecordField) =
        w.Write f.Name
        writeFrozenType w f.Type
        w.Write f.IsMutable

    let readRecordField (r: BinaryReader) : Frozen.TRecordField =
        let name = r.ReadString()
        let ty = readFrozenType r
        let isMutable = r.ReadBoolean()

        {
            Name = name
            Type = ty
            IsMutable = isMutable
        }

    let writeEnumCase (w: BinaryWriter) (c: TEnumCaseG<Anchor>) =
        w.Write c.Name
        writeVOptionWith w writeTEnumLiteral c.Value
        writeAnchor w c.Tok

    let readEnumCase (r: BinaryReader) : TEnumCaseG<Anchor> =
        let name = r.ReadString()
        let value = readVOptionWith r readTEnumLiteral
        let tok = readAnchor r

        {
            Name = name
            Value = value
            Tok = tok
        }
