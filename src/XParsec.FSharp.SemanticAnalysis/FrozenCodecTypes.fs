namespace XParsec.FSharp.SemanticAnalysis

open Vesper
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

    let writeMemberKeyRef (w: FrozenWriter) (k: MemberKey) =
        writeMemberKeyId w (w.Types.InternMemberKey k)

    let readMemberKeyRef (r: FrozenReader) : MemberKey = r.Types.[readMemberKeyId r]

    let writeBindingKeyRef (w: FrozenWriter) (k: BindingKey) =
        writeBindingKeyId w (w.Types.InternBindingKey k)

    let readBindingKeyRef (r: FrozenReader) : BindingKey = r.Types.[readBindingKeyId r]

    /// A measure term as its `(base-measure key, exponent)` pairs.
    let writeMeasureTerm (w: FrozenWriter) (m: MeasureTerm) =
        writeListWith
            w
            (fun w (key, exponent) ->
                writeTypeKeyRef w key
                writeRational w exponent
            )
            m.Exponents

    let readMeasureTerm (r: FrozenReader) : MeasureTerm =
        readListWith
            r
            (fun r ->
                let key = readTypeKeyRef r
                let exponent = readRational r
                key, exponent
            )
        |> MeasureTerm.OfList

    let writeModuleRef (w: FrozenWriter) (m: ModuleKey) =
        writeModuleId w (w.Types.InternModule m)

    let readModuleRef (r: FrozenReader) : ModuleKey = r.Types.[readModuleId r]

    /// The file a set of anchors index, which need NOT be the file the blob is keyed by, so
    /// that file's identity, and a hash of the contents the indices were taken against, have
    /// to be in the blob. A reference like the three above: interned once per file.
    let writeFilePathRef (w: FrozenWriter) (f: AssemblyFilePath) =
        writeFilePathId w (w.Types.InternFilePath f)

    let readFilePathRef (r: FrozenReader) : AssemblyFilePath = r.Types.[readFilePathId r]

    // ── the key-REFERENCE-keyed container ───────────────────────────────────

    /// A length-prefixed entry sequence in the dictionary's own enumeration order. `writeKey`
    /// interns its key, so a key costs one id.
    let private writeRefDict
        (w: FrozenWriter)
        (writeKey: FrozenWriter -> 'k -> unit)
        (writeVal: FrozenWriter -> 'v -> unit)
        (d: EqDict<'k, 'v>)
        =
        w.Write d.Count

        for KeyValue(k, v) in d do
            writeKey w k
            writeVal w v

    /// `label` prefixes the duplicate-key failure message with the calling codec.
    let private readRefDict
        (r: FrozenReader)
        (label: string)
        (readKey: FrozenReader -> 'k)
        (readVal: FrozenReader -> 'v)
        : EqDict<'k, 'v> =
        let n = r.ReadInt32()
        let d = System.Collections.Generic.Dictionary<'k, 'v>(n)

        for _ in 1..n do
            let k = readKey r
            let v = readVal r

            if not (d.TryAdd(k, v)) then
                failwithf "%s: key %O appears twice" label k

        EqDict.ofSeq d

    let writeSymbolDict w writeVal (d: EqDict<SymbolKey, 'v>) =
        writeRefDict w writeSymbolRef writeVal d

    let readSymbolDict r readVal : EqDict<SymbolKey, 'v> =
        readRefDict r "readSymbolDict" readSymbolRef readVal

    /// `writeSymbolDict` over the narrow key, for a table only nominal TYPES address.
    let writeTypeKeyDict w writeVal (d: EqDict<TypeKey, 'v>) =
        writeRefDict w writeTypeKeyRef writeVal d

    let readTypeKeyDict r readVal : EqDict<TypeKey, 'v> =
        readRefDict r "readTypeKeyDict" readTypeKeyRef readVal

    /// `writeSymbolDict` over the narrow key, for a table keyed only by declared MODULES.
    let writeModuleDict w writeVal (d: EqDict<ModuleKey, 'v>) =
        writeRefDict w writeModuleRef writeVal d

    let readModuleDict r readVal : EqDict<ModuleKey, 'v> =
        readRefDict r "readModuleDict" readModuleRef readVal

    /// The membership-only twin of `writeSymbolDict`, a `SymbolKey` set with no payload.
    let writeSymbolSet (w: FrozenWriter) (s: EqSet<SymbolKey>) =
        w.Write s.Length

        for k in s do
            writeSymbolRef w k

    let readSymbolSet (r: FrozenReader) : EqSet<SymbolKey> =
        let n = r.ReadInt32()
        let s = System.Collections.Generic.HashSet<SymbolKey>(n)

        for _ in 1..n do
            let k = readSymbolRef r

            if not (s.Add k) then
                failwithf "readSymbolSet: key %O appears twice" k

        EqSet.ofSeq s

    // ── non-generic payloads the tree / side tables carry ──────────────

    /// A kind tag, then the value at its own width.
    let private writeIntValue (w: FrozenWriter) (v: IntValue) =
        match v with
        | IntValue.SByte n ->
            w.Write 0uy
            w.Write n
        | IntValue.Byte n ->
            w.Write 1uy
            w.Write n
        | IntValue.Int16 n ->
            w.Write 2uy
            w.Write n
        | IntValue.UInt16 n ->
            w.Write 3uy
            w.Write n
        | IntValue.Int32 n ->
            w.Write 4uy
            w.Write n
        | IntValue.UInt32 n ->
            w.Write 5uy
            w.Write n
        | IntValue.Int64 n ->
            w.Write 6uy
            w.Write n
        | IntValue.UInt64 n ->
            w.Write 7uy
            w.Write n
        | IntValue.NativeInt n ->
            w.Write 8uy
            w.Write n
        | IntValue.UNativeInt n ->
            w.Write 9uy
            w.Write n

    let private readIntValue (r: FrozenReader) : IntValue =
        match r.ReadByte() with
        | 0uy -> IntValue.SByte(r.ReadSByte())
        | 1uy -> IntValue.Byte(r.ReadByte())
        | 2uy -> IntValue.Int16(r.ReadInt16())
        | 3uy -> IntValue.UInt16(r.ReadUInt16())
        | 4uy -> IntValue.Int32(r.ReadInt32())
        | 5uy -> IntValue.UInt32(r.ReadUInt32())
        | 6uy -> IntValue.Int64(r.ReadInt64())
        | 7uy -> IntValue.UInt64(r.ReadUInt64())
        | 8uy -> IntValue.NativeInt(r.ReadInt64())
        | 9uy -> IntValue.UNativeInt(r.ReadUInt64())
        | b -> failwithf "FrozenCodec: unknown IntValue tag %d" b

    let writeTConstValue (w: FrozenWriter) (v: TConstValue) =
        match v with
        | TConstValue.Integral n ->
            w.Write 0uy
            writeIntValue w n
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
        | 0uy -> TConstValue.Integral(readIntValue r)
        | 1uy -> TConstValue.Float(r.ReadDouble())
        | 2uy -> TConstValue.Float32(r.ReadSingle())
        | 3uy -> TConstValue.Bool(r.ReadBoolean())
        | 4uy -> TConstValue.Char(r.ReadChar())
        | 5uy -> TConstValue.Decimal(r.ReadDecimal())
        | 6uy -> TConstValue.String(r.ReadString())
        | 7uy -> TConstValue.Unit
        | b -> failwithf "FrozenCodec: unknown TConstValue tag %d" b

    let private writeTAttributeArg (w: FrozenWriter) (a: TAttributeArg) =
        writeStringVOption w a.Name
        writeTConstValue w a.Value
        writeVOptionWith w writeTypeKeyRef a.EnumKey

    let private readTAttributeArg (r: FrozenReader) : TAttributeArg =
        let name = readStringVOption r
        let value = readTConstValue r
        let enumKey = readVOptionWith r readTypeKeyRef

        {
            Name = name
            Value = value
            EnumKey = enumKey
        }

    let private writeTAttribute (w: FrozenWriter) (a: TAttribute) =
        writeTypeKeyRef w a.Key
        writeBlockWith w writeTAttributeArg a.Args

    let private readTAttribute (r: FrozenReader) : TAttribute =
        let key = readTypeKeyRef r
        let args = Block.ofArray (readArrayWith r readTAttributeArg)
        { Key = key; Args = args }

    let writeTAttributes (w: FrozenWriter) (attrs: TAttributes) = writeBlockWith w writeTAttribute attrs

    let readTAttributes (r: FrozenReader) : TAttributes =
        Block.ofArray (readArrayWith r readTAttribute)

    let writeModuleFacts (w: FrozenWriter) (facts: ModuleFacts) =
        writeVOptionWith w (fun w (CompiledName n) -> w.Write n) facts.CompiledName
        w.Write facts.RequiresQualifiedAccess
        w.Write facts.IsAutoOpen

    let readModuleFacts (r: FrozenReader) : ModuleFacts =
        let compiledName = readVOptionWith r (fun r -> CompiledName(r.ReadString()))
        let requiresQualifiedAccess = r.ReadBoolean()
        let isAutoOpen = r.ReadBoolean()

        {
            CompiledName = compiledName
            RequiresQualifiedAccess = requiresQualifiedAccess
            IsAutoOpen = isAutoOpen
        }

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

    let writeNominalValueKind (w: FrozenWriter) (k: NominalValueKind) =
        match k with
        | NominalValueKind.RefType -> w.Write 0uy
        | NominalValueKind.Struct -> w.Write 1uy

    let readNominalValueKind (r: FrozenReader) : NominalValueKind =
        match r.ReadByte() with
        | 0uy -> NominalValueKind.RefType
        | 1uy -> NominalValueKind.Struct
        | b -> failwithf "FrozenCodec: unknown NominalValueKind tag %d" b

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
        | TMemberKind.Accessor(prop, role) ->
            w.Write 2uy
            w.Write prop

            match role with
            | TAccessorRole.Getter -> w.Write 0uy
            | TAccessorRole.Setter -> w.Write 1uy

    let readTMemberKind (r: FrozenReader) : TMemberKind =
        match r.ReadByte() with
        | 0uy -> TMemberKind.Method
        | 1uy -> TMemberKind.Property
        | 2uy ->
            let prop = r.ReadString()

            let role =
                match r.ReadByte() with
                | 0uy -> TAccessorRole.Getter
                | 1uy -> TAccessorRole.Setter
                | b -> failwithf "FrozenCodec: unknown TAccessorRole tag %d" b

            TMemberKind.Accessor(prop, role)
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

    /// A typar constraint kind: a tag, then each embedded type through `writeTypeRef`.
    let writeConstraintKind (w: FrozenWriter) (kind: TyparConstraintKindG<FrozenType>) =
        match kind with
        | TyparConstraintKindG.Coercion target ->
            w.Write 0uy
            writeTypeRef w target
        | TyparConstraintKindG.Equality -> w.Write 1uy
        | TyparConstraintKindG.Comparison -> w.Write 2uy
        | TyparConstraintKindG.Struct -> w.Write 3uy
        | TyparConstraintKindG.ReferenceType -> w.Write 4uy
        | TyparConstraintKindG.Nullness -> w.Write 5uy
        | TyparConstraintKindG.NotNull -> w.Write 6uy
        | TyparConstraintKindG.DefaultConstructor -> w.Write 7uy
        | TyparConstraintKindG.Unmanaged -> w.Write 8uy
        | TyparConstraintKindG.Enum underlying ->
            w.Write 9uy
            writeTypeRef w underlying
        | TyparConstraintKindG.Delegate(args, ret) ->
            w.Write 10uy
            writeTypeRef w args
            writeTypeRef w ret

    let readConstraintKind (r: FrozenReader) : TyparConstraintKindG<FrozenType> =
        match r.ReadByte() with
        | 0uy -> TyparConstraintKindG.Coercion(readTypeRef r)
        | 1uy -> TyparConstraintKindG.Equality
        | 2uy -> TyparConstraintKindG.Comparison
        | 3uy -> TyparConstraintKindG.Struct
        | 4uy -> TyparConstraintKindG.ReferenceType
        | 5uy -> TyparConstraintKindG.Nullness
        | 6uy -> TyparConstraintKindG.NotNull
        | 7uy -> TyparConstraintKindG.DefaultConstructor
        | 8uy -> TyparConstraintKindG.Unmanaged
        | 9uy -> TyparConstraintKindG.Enum(readTypeRef r)
        | 10uy ->
            let args = readTypeRef r
            let ret = readTypeRef r
            TyparConstraintKindG.Delegate(args, ret)
        | b -> failwithf "FrozenCodec: unknown TyparConstraintKind tag %d" b

    let writeTyparName (w: FrozenWriter) (name: TyparName) =
        match name with
        | TyparName.Written s ->
            w.Write 0uy
            w.Write s
        | TyparName.Positional i ->
            w.Write 1uy
            w.Write i

    let readTyparName (r: FrozenReader) : TyparName =
        match r.ReadByte() with
        | 0uy -> TyparName.Written(r.ReadString())
        | 1uy -> TyparName.Positional(r.ReadInt32())
        | b -> failwithf "FrozenCodec: unknown TyparName tag %d" b

    let private writeTypeTypar (w: FrozenWriter) (t: TypeTypar) =
        writeTyparName w t.Name
        writeEqSetWith w writeConstraintKind t.Constraints.Kinds
        writeBlockWith w writeTypeRef t.Constraints.Defaults

    let private readTypeTypar (r: FrozenReader) : TypeTypar =
        let name = readTyparName r
        let constraints = readEqSetWith r readConstraintKind
        let defaults = readBlockWith r readTypeRef

        {
            Name = name
            Constraints =
                {
                    Kinds = constraints
                    Defaults = defaults
                }
        }

    let private writeTyparSlot (w: FrozenWriter) (slot: TyparSlot) =
        match slot with
        | TyparSlot.Type i ->
            w.Write 0uy
            w.Write i
        | TyparSlot.Measure i ->
            w.Write 1uy
            w.Write i

    let private readTyparSlot (r: FrozenReader) : TyparSlot =
        match r.ReadByte() with
        | 0uy -> TyparSlot.Type(r.ReadInt32())
        | 1uy -> TyparSlot.Measure(r.ReadInt32())
        | b -> failwithf "FrozenCodec: unknown TyparSlot tag %d" b

    /// A typar list: the type-kinded parameters with their constraints, the measure-kinded
    /// ones, then the source order.
    let writeTyparList (w: FrozenWriter) (typars: TyparList) =
        writeBlockWith w writeTypeTypar typars.Types
        writeBlockWith w (fun w (m: MeasureTypar) -> writeTyparName w m.Name) typars.Measures
        writeBlockWith w writeTyparSlot typars.Order

    let readTyparList (r: FrozenReader) : TyparList =
        let types = readBlockWith r readTypeTypar
        let measures = readBlockWith r (fun r -> { MeasureTypar.Name = readTyparName r })
        let order = readBlockWith r readTyparSlot

        {
            Types = types
            Measures = measures
            Order = order
        }

    let private writeMemberTrait (w: FrozenWriter) (t: MemberTrait) =
        writeBlockWith w (fun w (i: int) -> w.Write i) t.TyparIndices
        w.Write t.MemberName
        writeBlockWith w writeTypeRef t.ArgTypes
        writeTypeRef w t.ReturnType

    let private readMemberTrait (r: FrozenReader) : MemberTrait =
        let indices = readBlockWith r (fun r -> r.ReadInt32())
        let name = r.ReadString()
        let args = readBlockWith r readTypeRef
        let ret = readTypeRef r

        {
            TyparIndices = indices
            MemberName = name
            ArgTypes = args
            ReturnType = ret
        }

    /// A function scheme: its typars, then its traits.
    let writeFunctionScheme (w: FrozenWriter) (s: FunctionScheme) =
        writeTyparList w s.Typars
        writeBlockWith w writeMemberTrait s.Traits

    let readFunctionScheme (r: FrozenReader) : FunctionScheme =
        let typars = readTyparList r
        let traits = readBlockWith r readMemberTrait
        FunctionScheme.create typars traits

    let writeLocalBindingId (w: FrozenWriter) (LocalBindingId i) = w.Write i

    let readLocalBindingId (r: FrozenReader) : LocalBindingId = LocalBindingId(r.ReadInt32())

    let writeLocalOwner (w: FrozenWriter) (o: LocalOwner) =
        match o with
        | LocalOwner.Member key ->
            w.Write 0uy
            writeMemberKeyRef w key
        | LocalOwner.ModuleFunction key ->
            w.Write 1uy
            writeBindingKeyRef w key
        | LocalOwner.Local id ->
            w.Write 2uy
            writeLocalBindingId w id
        | LocalOwner.Initialiser -> w.Write 3uy
        | LocalOwner.Spliced template ->
            w.Write 4uy
            writeSymbolRef w template

    let readLocalOwner (r: FrozenReader) : LocalOwner =
        match r.ReadByte() with
        | 0uy -> LocalOwner.Member(readMemberKeyRef r)
        | 1uy -> LocalOwner.ModuleFunction(readBindingKeyRef r)
        | 2uy -> LocalOwner.Local(readLocalBindingId r)
        | 3uy -> LocalOwner.Initialiser
        | 4uy -> LocalOwner.Spliced(readSymbolRef r)
        | b -> failwithf "FrozenCodec: unknown LocalOwner tag %d" b

    let writeLocalScheme (w: FrozenWriter) (s: LocalScheme) =
        writeLocalBindingId w s.Id
        w.Write s.TyparArity

    let readLocalScheme (r: FrozenReader) : LocalScheme =
        let id = readLocalBindingId r

        { Id = id; TyparArity = r.ReadInt32() }

    let writeModuleBindingInfo (w: FrozenWriter) (m: ModuleBindingInfo) =
        writeSymbolRef w m.Key
        writeVOptionWith w (fun w (CompiledName n) -> w.Write n) m.CompiledName
        writeTAttributes w m.Attributes

    let readModuleBindingInfo (r: FrozenReader) : ModuleBindingInfo =
        let key = readSymbolRef r
        let compiledName = readVOptionWith r (fun r -> CompiledName(r.ReadString()))
        let attributes = readTAttributes r

        match key with
        | SymbolKey.Binding bk ->
            {
                Container = bk.Decl
                Name = bk.Name
                CompiledName = compiledName
                Attributes = attributes
            }
        | k -> failwithf "FrozenCodec: a ModuleBindingInfo stored a non-Binding key: %A" k

    let writeIntrinsicBindingInfo (w: FrozenWriter) (i: IntrinsicBindingInfo) =
        w.Write i.TypeId.Value
        w.Write i.Heritable

    let readIntrinsicBindingInfo (r: FrozenReader) : IntrinsicBindingInfo =
        let typeId = PlatformTypeId(r.ReadString())
        let heritable = r.ReadBoolean()

        {
            TypeId = typeId
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
        writeTyparList w m.MethodTypars
        writeTypeRef w m.Signature
        writeBlockWith w writeStringVOption m.ParamNames
        writeTMemberKind w m.Kind
        w.Write m.IsStatic

    let readAbstractMethod (r: FrozenReader) : Frozen.TAbstractMethod =
        let name = r.ReadString()
        let methodTypars = readTyparList r
        let signature = readTypeRef r
        let paramNames = readBlockWith r readStringVOption
        let kind = readTMemberKind r
        let isStatic = r.ReadBoolean()

        {
            Name = name
            MethodTypars = methodTypars
            Signature = signature
            ParamNames = paramNames
            Kind = kind
            IsStatic = isStatic
        }

    let writeUnionCase (w: FrozenWriter) (c: Frozen.TUnionCase) =
        w.Write c.Name

        writeBlockWith
            w
            (fun w (nameOpt: string voption, ty) ->
                writeStringVOption w nameOpt
                writeTypeRef w ty
            )
            c.Fields

        writeTAttributes w c.Attributes

    let readUnionCase (r: FrozenReader) : Frozen.TUnionCase =
        let name = r.ReadString()

        let fields =
            Block.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let nameOpt = readStringVOption r
                        let ty = readTypeRef r
                        nameOpt, ty
                    )
            )

        let attributes = readTAttributes r

        {
            Name = name
            Fields = fields
            Attributes = attributes
        }

    let writeRecordField (w: FrozenWriter) (f: Frozen.TRecordField) =
        w.Write f.Name
        writeTypeRef w f.Type
        w.Write f.IsMutable
        writeTAttributes w f.Attributes

    let readRecordField (r: FrozenReader) : Frozen.TRecordField =
        let name = r.ReadString()
        let ty = readTypeRef r
        let isMutable = r.ReadBoolean()
        let attributes = readTAttributes r

        {
            Name = name
            Type = ty
            IsMutable = isMutable
            Attributes = attributes
        }

    let writeEnumCase (w: FrozenWriter) (c: TEnumCaseG<Anchor>) =
        w.Write c.Name
        writeVOptionWith w writeTEnumLiteral c.Value
        writeAnchor w c.Tok
        writeTAttributes w c.Attributes

    let readEnumCase (r: FrozenReader) : TEnumCaseG<Anchor> =
        let name = r.ReadString()
        let value = readVOptionWith r readTEnumLiteral
        let tok = readAnchor r
        let attributes = readTAttributes r

        {
            Name = name
            Value = value
            Tok = tok
            Attributes = attributes
        }
