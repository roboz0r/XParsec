namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecRows

/// The FROZEN type domain: the reference codec every other module reaches a type through,
/// the non-generic payloads the tree and the side tables carry, the printf hole-form
/// cluster, and the type-declaration payloads with no sub-expression. NOTHING here is
/// written structurally.
module FrozenCodecTypes =

    // ── a REFERENCE into the file's tables ──────────────────────────────────

    /// The write side INTERNS where the read side resolves: the `ty` columns were interned at
    /// freeze, but a payload can carry a type they never did (an `ILIntrinsic` operand, a
    /// signature, a `ValRepr` result), so the tables can only go out AFTER the body.
    let writeTypeRef (w: FrozenWriter) (t: FrozenType) = writeTypeId w (w.Types.Intern t)

    let readTypeRef (r: FrozenReader) : FrozenType = r.Types.[readTypeId r]

    let writeSymbolRef (w: FrozenWriter) (k: SymbolKey) =
        writeSymbolId w (w.Types.InternSymbol k)

    let readSymbolRef (r: FrozenReader) : SymbolKey = r.Types.[readSymbolId r]

    let writeTypeKeyRef (w: FrozenWriter) (k: TypeKey) =
        writeTypeKeyId w (w.Types.InternTypeKey k)

    let readTypeKeyRef (r: FrozenReader) : TypeKey = r.Types.[readTypeKeyId r]

    /// The file a set of anchors index, which need NOT be the file the blob is keyed by, so
    /// that file's identity, and a hash of the contents the indices were taken against, have
    /// to be in the blob. A reference like the three above: interned once per file.
    let writeFilePathRef (w: FrozenWriter) (f: AssemblyFilePath) =
        writeFilePathId w (w.Types.InternFilePath f)

    let readFilePathRef (r: FrozenReader) : AssemblyFilePath = r.Types.[readFilePathId r]

    // ── the `SymbolKey`-keyed container ─────────────────────────────────────

    /// A length-prefixed entry sequence in the dictionary's own enumeration order: no
    /// canonical order is imposed, so the read side rebuilds an unordered `Dictionary`
    /// behind the read-only view.
    let writeSymbolDict
        (w: FrozenWriter)
        (writeVal: FrozenWriter -> 'v -> unit)
        (d: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>)
        =
        w.Write d.Count

        for KeyValue(k, v) in d do
            writeSymbolRef w k
            writeVal w v

    let readSymbolDict
        (r: FrozenReader)
        (readVal: FrozenReader -> 'v)
        : System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v> =
        let n = r.ReadInt32()
        let d = System.Collections.Generic.Dictionary<SymbolKey, 'v>(n)

        for _ in 1..n do
            let k = readSymbolRef r
            let v = readVal r

            if not (d.TryAdd(k, v)) then
                failwithf "readSymbolDict: key %O appears twice" k

        d :> System.Collections.Generic.IReadOnlyDictionary<SymbolKey, 'v>

    /// `writeSymbolDict` over the narrow key, for a table only nominal TYPES address.
    let writeTypeKeyDict
        (w: FrozenWriter)
        (writeVal: FrozenWriter -> 'v -> unit)
        (d: System.Collections.Generic.IReadOnlyDictionary<TypeKey, 'v>)
        =
        w.Write d.Count

        for KeyValue(k, v) in d do
            writeTypeKeyRef w k
            writeVal w v

    let readTypeKeyDict
        (r: FrozenReader)
        (readVal: FrozenReader -> 'v)
        : System.Collections.Generic.IReadOnlyDictionary<TypeKey, 'v> =
        let n = r.ReadInt32()
        let d = System.Collections.Generic.Dictionary<TypeKey, 'v>(n)

        for _ in 1..n do
            let k = readTypeKeyRef r
            let v = readVal r

            if not (d.TryAdd(k, v)) then
                failwithf "readTypeKeyDict: key %O appears twice" k

        d :> System.Collections.Generic.IReadOnlyDictionary<TypeKey, 'v>

    /// The membership-only twin of `writeSymbolDict`, a `SymbolKey` set with no payload.
    let writeSymbolSet (w: FrozenWriter) (s: System.Collections.Generic.IReadOnlySet<SymbolKey>) =
        w.Write s.Count

        for k in s do
            writeSymbolRef w k

    let readSymbolSet (r: FrozenReader) : System.Collections.Generic.IReadOnlySet<SymbolKey> =
        let n = r.ReadInt32()
        let s = System.Collections.Generic.HashSet<SymbolKey>(n)

        for _ in 1..n do
            let k = readSymbolRef r

            if not (s.Add k) then
                failwithf "readSymbolSet: key %O appears twice" k

        s :> System.Collections.Generic.IReadOnlySet<SymbolKey>

    // ── non-generic payloads the tree / side tables carry ──────────────

    let private writeIntKind (w: FrozenWriter) (k: IntKind) =
        match k with
        | IntKind.SByte -> w.Write 0uy
        | IntKind.Byte -> w.Write 1uy
        | IntKind.Int16 -> w.Write 2uy
        | IntKind.UInt16 -> w.Write 3uy
        | IntKind.Int32 -> w.Write 4uy
        | IntKind.UInt32 -> w.Write 5uy
        | IntKind.Int64 -> w.Write 6uy
        | IntKind.UInt64 -> w.Write 7uy
        | IntKind.NativeInt -> w.Write 8uy
        | IntKind.UNativeInt -> w.Write 9uy

    let private readIntKind (r: FrozenReader) : IntKind =
        match r.ReadByte() with
        | 0uy -> IntKind.SByte
        | 1uy -> IntKind.Byte
        | 2uy -> IntKind.Int16
        | 3uy -> IntKind.UInt16
        | 4uy -> IntKind.Int32
        | 5uy -> IntKind.UInt32
        | 6uy -> IntKind.Int64
        | 7uy -> IntKind.UInt64
        | 8uy -> IntKind.NativeInt
        | 9uy -> IntKind.UNativeInt
        | b -> failwithf "FrozenCodec: unknown IntKind tag %d" b

    let writeTConstValue (w: FrozenWriter) (v: TConstValue) =
        match v with
        | TConstValue.Integral(width, bits) ->
            w.Write 0uy
            writeIntKind w width
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

    let readTConstValue (r: FrozenReader) : TConstValue =
        match r.ReadByte() with
        | 0uy ->
            let width = readIntKind r
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

    let writeAccessibility (w: FrozenWriter) (a: Accessibility) =
        match a with
        | Accessibility.Public -> w.Write 0uy
        | Accessibility.Internal -> w.Write 1uy
        | Accessibility.Private -> w.Write 2uy

    let readAccessibility (r: FrozenReader) : Accessibility =
        match r.ReadByte() with
        | 0uy -> Accessibility.Public
        | 1uy -> Accessibility.Internal
        | 2uy -> Accessibility.Private
        | b -> failwithf "FrozenCodec: unknown Accessibility tag %d" b

    let writeClassValueKind (w: FrozenWriter) (k: ClassValueKind) =
        match k with
        | ClassValueKind.RefType -> w.Write 0uy
        | ClassValueKind.Struct -> w.Write 1uy
        | ClassValueKind.RefStruct -> w.Write 2uy

    let readClassValueKind (r: FrozenReader) : ClassValueKind =
        match r.ReadByte() with
        | 0uy -> ClassValueKind.RefType
        | 1uy -> ClassValueKind.Struct
        | 2uy -> ClassValueKind.RefStruct
        | b -> failwithf "FrozenCodec: unknown ClassValueKind tag %d" b

    let writeRecordValueKind (w: FrozenWriter) (k: RecordValueKind) =
        match k with
        | RecordValueKind.RefType -> w.Write 0uy
        | RecordValueKind.Struct -> w.Write 1uy

    let readRecordValueKind (r: FrozenReader) : RecordValueKind =
        match r.ReadByte() with
        | 0uy -> RecordValueKind.RefType
        | 1uy -> RecordValueKind.Struct
        | b -> failwithf "FrozenCodec: unknown RecordValueKind tag %d" b

    let writeEqualityVerdict (w: FrozenWriter) (v: EqualityVerdict) =
        match v with
        | EqualityVerdict.Structural -> w.Write 0uy
        | EqualityVerdict.Reference -> w.Write 1uy
        | EqualityVerdict.Custom -> w.Write 2uy
        | EqualityVerdict.NoEquality -> w.Write 3uy

    let readEqualityVerdict (r: FrozenReader) : EqualityVerdict =
        match r.ReadByte() with
        | 0uy -> EqualityVerdict.Structural
        | 1uy -> EqualityVerdict.Reference
        | 2uy -> EqualityVerdict.Custom
        | 3uy -> EqualityVerdict.NoEquality
        | b -> failwithf "FrozenCodec: unknown EqualityVerdict tag %d" b

    let writeComparisonVerdict (w: FrozenWriter) (v: ComparisonVerdict) =
        match v with
        | ComparisonVerdict.Structural -> w.Write 0uy
        | ComparisonVerdict.Custom -> w.Write 1uy
        | ComparisonVerdict.NoComparison -> w.Write 2uy

    let readComparisonVerdict (r: FrozenReader) : ComparisonVerdict =
        match r.ReadByte() with
        | 0uy -> ComparisonVerdict.Structural
        | 1uy -> ComparisonVerdict.Custom
        | 2uy -> ComparisonVerdict.NoComparison
        | b -> failwithf "FrozenCodec: unknown ComparisonVerdict tag %d" b

    let writeMemberStorage (w: FrozenWriter) (s: MemberStorage) =
        match s with
        | MemberStorage.Field -> w.Write 0uy
        | MemberStorage.Property -> w.Write 1uy
        | MemberStorage.Method -> w.Write 2uy

    let readMemberStorage (r: FrozenReader) : MemberStorage =
        match r.ReadByte() with
        | 0uy -> MemberStorage.Field
        | 1uy -> MemberStorage.Property
        | 2uy -> MemberStorage.Method
        | b -> failwithf "FrozenCodec: unknown MemberStorage tag %d" b

    let writeTMemberKind (w: FrozenWriter) (k: TMemberKind) =
        match k with
        | TMemberKind.Method -> w.Write 0uy
        | TMemberKind.Property -> w.Write 1uy

    let readTMemberKind (r: FrozenReader) : TMemberKind =
        match r.ReadByte() with
        | 0uy -> TMemberKind.Method
        | 1uy -> TMemberKind.Property
        | b -> failwithf "FrozenCodec: unknown TMemberKind tag %d" b

    let writeClosureRepr (w: FrozenWriter) (c: ClosureRepr) =
        match c with
        | ClosureRepr.Heap -> w.Write 0uy
        | ClosureRepr.Stack -> w.Write 1uy

    let readClosureRepr (r: FrozenReader) : ClosureRepr =
        match r.ReadByte() with
        | 0uy -> ClosureRepr.Heap
        | 1uy -> ClosureRepr.Stack
        | b -> failwithf "FrozenCodec: unknown ClosureRepr tag %d" b

    let writeFunVerdict (w: FrozenWriter) (v: FunVerdict) =
        w.Write v.Arity
        writeVOptionWith w (fun w (i: int) -> w.Write i) v.ResultTyparPos

    let readFunVerdict (r: FrozenReader) : FunVerdict =
        let arity = r.ReadInt32()
        let resultTyparPos = readVOptionWith r (fun r -> r.ReadInt32())

        {
            Arity = arity
            ResultTyparPos = resultTyparPos
        }

    /// A frozen typar bound. Its `target` is a `FrozenType`, so this rides `writeTypeRef`.
    let writeFrozenConstraint (w: FrozenWriter) (c: FrozenConstraint) =
        match c with
        | FrozenConstraint.Coercion(typarIndex, target) ->
            w.Write 0uy
            w.Write typarIndex
            writeTypeRef w target

    let readFrozenConstraint (r: FrozenReader) : FrozenConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typarIndex = r.ReadInt32()
            let target = readTypeRef r
            FrozenConstraint.Coercion(typarIndex, target)
        | b -> failwithf "FrozenCodec: unknown FrozenConstraint tag %d" b

    let writeModuleBindingInfo (w: FrozenWriter) (m: ModuleBindingInfo) = writeSymbolRef w m.Key

    let readModuleBindingInfo (r: FrozenReader) : ModuleBindingInfo =
        match readSymbolRef r with
        | SymbolKey.Binding bk -> { Container = bk.Decl; Name = bk.Name }
        | k -> failwithf "FrozenCodec: a ModuleBindingInfo stored a non-Binding key: %A" k

    let writeIntrinsicReprInfo (w: FrozenWriter) (i: IntrinsicReprInfo) =
        w.Write i.Platform
        w.Write i.Heritable

    let readIntrinsicReprInfo (r: FrozenReader) : IntrinsicReprInfo =
        let platform = r.ReadString()
        let heritable = r.ReadBoolean()

        {
            Platform = platform
            Heritable = heritable
        }

    let private writeTEnumLiteral (w: FrozenWriter) (l: TEnumLiteral) =
        match l with
        | TEnumLiteral.Int value ->
            w.Write 0uy
            writeTConstValue w value
        | TEnumLiteral.String value ->
            w.Write 1uy
            w.Write value

    let private readTEnumLiteral (r: FrozenReader) : TEnumLiteral =
        match r.ReadByte() with
        | 0uy -> TEnumLiteral.Int(readTConstValue r)
        | 1uy -> TEnumLiteral.String(r.ReadString())
        | b -> failwithf "FrozenCodec: unknown TEnumLiteral tag %d" b

    let writeParamAttrs (w: FrozenWriter) (a: ParamAttrs) = w.Write a.CallAtMostOnce

    let readParamAttrs (r: FrozenReader) : ParamAttrs = { CallAtMostOnce = r.ReadBoolean() }

    /// A member's own method typars: each entry is the source name plus the typar's frozen
    /// type (`FTTypar(Method, i)`), and its POSITION is the ABI index.
    let writeMethodTypeParams (w: FrozenWriter) (mtps: EqArray<string * FrozenType>) =
        writeEqArrayWith
            w
            (fun w (n: string, ty) ->
                w.Write n
                writeTypeRef w ty
            )
            mtps

    let readMethodTypeParams (r: FrozenReader) : EqArray<string * FrozenType> =
        EqArray.ofArray (readArrayWith r (fun r -> let n = r.ReadString() in n, readTypeRef r))

    // ── the printf hole-form cluster (a `HoleSpec` payload) ─────────────────

    let private writePrintWidth (w: FrozenWriter) (p: PrintfHoleForm.PrintWidth) =
        match p with
        | PrintfHoleForm.PrintWidth.Default -> w.Write 0uy
        | PrintfHoleForm.PrintWidth.Never -> w.Write 1uy
        | PrintfHoleForm.PrintWidth.Cols n ->
            w.Write 2uy
            w.Write n
        | PrintfHoleForm.PrintWidth.Star -> w.Write 3uy

    let private readPrintWidth (r: FrozenReader) : PrintfHoleForm.PrintWidth =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.PrintWidth.Default
        | 1uy -> PrintfHoleForm.PrintWidth.Never
        | 2uy -> PrintfHoleForm.PrintWidth.Cols(r.ReadInt32())
        | 3uy -> PrintfHoleForm.PrintWidth.Star
        | b -> failwithf "FrozenCodec: unknown PrintWidth tag %d" b

    let private writePrintSize (w: FrozenWriter) (p: PrintfHoleForm.PrintSize) =
        match p with
        | PrintfHoleForm.PrintSize.Default -> w.Write 0uy
        | PrintfHoleForm.PrintSize.Cols n ->
            w.Write 1uy
            w.Write n
        | PrintfHoleForm.PrintSize.Star -> w.Write 2uy

    let private readPrintSize (r: FrozenReader) : PrintfHoleForm.PrintSize =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.PrintSize.Default
        | 1uy -> PrintfHoleForm.PrintSize.Cols(r.ReadInt32())
        | 2uy -> PrintfHoleForm.PrintSize.Star
        | b -> failwithf "FrozenCodec: unknown PrintSize tag %d" b

    let private writePrec (w: FrozenWriter) (p: PrintfHoleForm.Prec) =
        match p with
        | PrintfHoleForm.Prec.Const n ->
            w.Write 0uy
            w.Write n
        | PrintfHoleForm.Prec.Star -> w.Write 1uy

    let private readPrec (r: FrozenReader) : PrintfHoleForm.Prec =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.Prec.Const(r.ReadInt32())
        | 1uy -> PrintfHoleForm.Prec.Star
        | b -> failwithf "FrozenCodec: unknown Prec tag %d" b

    let private writeRadix (w: FrozenWriter) (radix: PrintfHoleForm.Radix) =
        match radix with
        | PrintfHoleForm.Radix.Hex upper ->
            w.Write 0uy
            w.Write upper
        | PrintfHoleForm.Radix.Binary -> w.Write 1uy
        | PrintfHoleForm.Radix.Octal -> w.Write 2uy

    let private readRadix (r: FrozenReader) : PrintfHoleForm.Radix =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.Radix.Hex(r.ReadBoolean())
        | 1uy -> PrintfHoleForm.Radix.Binary
        | 2uy -> PrintfHoleForm.Radix.Octal
        | b -> failwithf "FrozenCodec: unknown Radix tag %d" b

    let private writeAlignment (w: FrozenWriter) (a: PrintfHoleForm.Alignment) =
        match a with
        | PrintfHoleForm.Alignment.None -> w.Write 0uy
        | PrintfHoleForm.Alignment.Const n ->
            w.Write 1uy
            w.Write n
        | PrintfHoleForm.Alignment.Star leftJustify ->
            w.Write 2uy
            w.Write leftJustify

    let private readAlignment (r: FrozenReader) : PrintfHoleForm.Alignment =
        match r.ReadByte() with
        | 0uy -> PrintfHoleForm.Alignment.None
        | 1uy -> PrintfHoleForm.Alignment.Const(r.ReadInt32())
        | 2uy -> PrintfHoleForm.Alignment.Star(r.ReadBoolean())
        | b -> failwithf "FrozenCodec: unknown Alignment tag %d" b

    let private writeFieldFormat (w: FrozenWriter) (f: PrintfHoleForm.FieldFormat) =
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

    let private readFieldFormat (r: FrozenReader) : PrintfHoleForm.FieldFormat =
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

    let private writeHoleForm (w: FrozenWriter) (h: PrintfHoleForm.HoleForm) =
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

    let private readHoleForm (r: FrozenReader) : PrintfHoleForm.HoleForm =
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

    let private writeHoleSpecSource (w: FrozenWriter) (s: HoleSpecSource) =
        match s with
        | HoleSpecSource.Classified form ->
            w.Write 0uy
            writeHoleForm w form
        | HoleSpecSource.RawFormat fmt ->
            w.Write 1uy
            writeOptionWith w (fun w (s: string) -> w.Write s) fmt

    let private readHoleSpecSource (r: FrozenReader) : HoleSpecSource =
        match r.ReadByte() with
        | 0uy -> HoleSpecSource.Classified(readHoleForm r)
        | 1uy -> HoleSpecSource.RawFormat(readOptionWith r (fun r -> r.ReadString()))
        | b -> failwithf "FrozenCodec: unknown HoleSpecSource tag %d" b

    // A `HoleSpec` carries no sub-expression, its three fields being a type, a source and an
    // anchor, so it belongs here even though the format SEGMENT that holds it does not.
    let writeHoleSpec (w: FrozenWriter) (h: Pooled.HoleSpec) =
        writeTypeRef w h.Ty
        writeHoleSpecSource w h.Source
        writeAnchor w h.Tok

    let readHoleSpec (r: FrozenReader) : Pooled.HoleSpec =
        let ty = readTypeRef r
        let source = readHoleSpecSource r
        let tok = readAnchor r
        { Ty = ty; Source = source; Tok = tok }

    // ── the type-declaration payloads with no sub-expression ──────────────

    let writeAbstractMethod (w: FrozenWriter) (m: Frozen.TAbstractMethod) =
        w.Write m.Name
        writeStringArray w m.MethodTypeParams
        writeTypeRef w m.Signature
        w.Write m.IsProperty

    let readAbstractMethod (r: FrozenReader) : Frozen.TAbstractMethod =
        let name = r.ReadString()
        let methodTypeParams = readStringArray r
        let signature = readTypeRef r
        let isProperty = r.ReadBoolean()

        {
            Name = name
            MethodTypeParams = methodTypeParams
            Signature = signature
            IsProperty = isProperty
        }

    let writeUnionCase (w: FrozenWriter) (c: Frozen.TUnionCase) =
        w.Write c.Name

        writeEqArrayWith
            w
            (fun w (nameOpt: string voption, ty) ->
                writeVOptionWith w (fun w (s: string) -> w.Write s) nameOpt
                writeTypeRef w ty
            )
            c.Fields

    let readUnionCase (r: FrozenReader) : Frozen.TUnionCase =
        let name = r.ReadString()

        let fields =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let nameOpt = readVOptionWith r (fun r -> r.ReadString())
                        let ty = readTypeRef r
                        nameOpt, ty
                    )
            )

        { Name = name; Fields = fields }

    let writeRecordField (w: FrozenWriter) (f: Frozen.TRecordField) =
        w.Write f.Name
        writeTypeRef w f.Type
        w.Write f.IsMutable

    let readRecordField (r: FrozenReader) : Frozen.TRecordField =
        let name = r.ReadString()
        let ty = readTypeRef r
        let isMutable = r.ReadBoolean()

        {
            Name = name
            Type = ty
            IsMutable = isMutable
        }

    let writeEnumCase (w: FrozenWriter) (c: TEnumCaseG<Anchor>) =
        w.Write c.Name
        writeVOptionWith w writeTEnumLiteral c.Value
        writeAnchor w c.Tok

    let readEnumCase (r: FrozenReader) : TEnumCaseG<Anchor> =
        let name = r.ReadString()
        let value = readVOptionWith r readTEnumLiteral
        let tok = readAnchor r

        {
            Name = name
            Value = value
            Tok = tok
        }
