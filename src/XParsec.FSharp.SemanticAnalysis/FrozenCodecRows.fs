namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives

/// The file's interned type / key TABLES on the wire: the ROW form of `FrozenType` and the
/// `SymbolKey`/`TypeKey` cluster, every child already a row id. The one place in the codec a
/// type's structure is spelled out; every other occurrence of it costs a plain `int`.
module FrozenCodecRows =

    // ── row ids ────────────────────────────────────────────────────────────
    //
    // One pair per row array; each is an `int` on the wire and a distinct type off it.

    let private writeStrId (w: FrozenWriter) (StrId i) = w.Write i
    let private readStrId (r: FrozenReader) : StrId = StrId(r.ReadInt32())

    let private writeNamespaceId (w: FrozenWriter) (NamespaceId i) = w.Write i
    let private readNamespaceId (r: FrozenReader) : NamespaceId = NamespaceId(r.ReadInt32())

    /// Public: a table keyed by the declaring module writes its key as a module-table row.
    let writeModuleId (w: FrozenWriter) (ModuleId i) = w.Write i
    let readModuleId (r: FrozenReader) : ModuleId = ModuleId(r.ReadInt32())

    /// Public: a nominal type key crosses the wire on its own, as a `PooledTypeDecl`'s
    /// identity or a constrained interface, and not only inside a type.
    let writeTypeKeyId (w: FrozenWriter) (TypeKeyId i) = w.Write i
    let readTypeKeyId (r: FrozenReader) : TypeKeyId = TypeKeyId(r.ReadInt32())

    let private writeBindingKeyId (w: FrozenWriter) (BindingKeyId i) = w.Write i
    let private readBindingKeyId (r: FrozenReader) : BindingKeyId = BindingKeyId(r.ReadInt32())

    let private writeMemberKeyId (w: FrozenWriter) (MemberKeyId i) = w.Write i
    let private readMemberKeyId (r: FrozenReader) : MemberKeyId = MemberKeyId(r.ReadInt32())

    /// A row of the file's symbol-key table. Public: a symbol REFERENCE is written as one.
    let writeSymbolId (w: FrozenWriter) (SymbolId i) = w.Write i
    let readSymbolId (r: FrozenReader) : SymbolId = SymbolId(r.ReadInt32())

    /// A row of the file's type table: the identity a `ty` column entry holds.
    let writeTypeId (w: FrozenWriter) (TypeId i) = w.Write i
    let readTypeId (r: FrozenReader) : TypeId = TypeId(r.ReadInt32())

    /// A row of the file's file-path table. Public: a specialization entry writes the file
    /// its anchors index as one.
    let writeFilePathId (w: FrozenWriter) (FilePathId i) = w.Write i
    let readFilePathId (r: FrozenReader) : FilePathId = FilePathId(r.ReadInt32())

    let private writeTypeIds (w: FrozenWriter) (xs: EqArray<TypeId>) = writeEqArrayWith w writeTypeId xs

    let private readTypeIds (r: FrozenReader) : EqArray<TypeId> =
        EqArray.ofArray (readArrayWith r readTypeId)

    // ── the rows ───────────────────────────────────────────────────────────

    let private writeNamespaceRow (w: FrozenWriter) (segs: EqArray<StrId>) = writeEqArrayWith w writeStrId segs

    let private readNamespaceRow (r: FrozenReader) : EqArray<StrId> =
        EqArray.ofArray (readArrayWith r readStrId)

    let private writeModuleContainerRow (w: FrozenWriter) (h: ModuleContainerRow) =
        match h with
        | ModuleContainerRow.InNamespace ns ->
            w.Write 0uy
            writeNamespaceId w ns
        | ModuleContainerRow.InModule parent ->
            w.Write 1uy
            writeModuleId w parent

    let private readModuleContainerRow (r: FrozenReader) : ModuleContainerRow =
        match r.ReadByte() with
        | 0uy -> ModuleContainerRow.InNamespace(readNamespaceId r)
        | 1uy -> ModuleContainerRow.InModule(readModuleId r)
        | b -> failwithf "FrozenCodec: unknown ModuleContainerRow tag %d" b

    let private writeModuleRow (w: FrozenWriter) (row: ModuleRow) =
        writeModuleContainerRow w row.Container
        writeStrId w row.Name

    let private readModuleRow (r: FrozenReader) : ModuleRow =
        let container = readModuleContainerRow r
        let name = readStrId r
        { Container = container; Name = name }

    let private writeTypeContainerRow (w: FrozenWriter) (h: TypeContainerRow) =
        match h with
        | TypeContainerRow.InNamespace ns ->
            w.Write 0uy
            writeNamespaceId w ns
        | TypeContainerRow.InModule parent ->
            w.Write 1uy
            writeModuleId w parent
        | TypeContainerRow.InType outer ->
            w.Write 2uy
            writeTypeKeyId w outer

    let private readTypeContainerRow (r: FrozenReader) : TypeContainerRow =
        match r.ReadByte() with
        | 0uy -> TypeContainerRow.InNamespace(readNamespaceId r)
        | 1uy -> TypeContainerRow.InModule(readModuleId r)
        | 2uy -> TypeContainerRow.InType(readTypeKeyId r)
        | b -> failwithf "FrozenCodec: unknown TypeContainerRow tag %d" b

    let private writeTypeKeyRow (w: FrozenWriter) (row: TypeKeyRow) =
        writeTypeContainerRow w row.Container
        writeStrId w row.Name
        w.Write row.TyparArity

    let private readTypeKeyRow (r: FrozenReader) : TypeKeyRow =
        let container = readTypeContainerRow r
        let name = readStrId r
        let arity = r.ReadInt32()

        {
            Container = container
            Name = name
            TyparArity = arity
        }

    let private writeBindingKeyRow (w: FrozenWriter) (row: BindingKeyRow) =
        writeModuleContainerRow w row.Decl
        writeStrId w row.Name

    let private readBindingKeyRow (r: FrozenReader) : BindingKeyRow =
        let decl = readModuleContainerRow r
        let name = readStrId r
        { Decl = decl; Name = name }

    let private writeTyparScopeRow (w: FrozenWriter) (s: TyparScopeRow) =
        match s with
        | TyparScopeRow.Type key ->
            w.Write 0uy
            writeTypeKeyId w key
        | TyparScopeRow.Member(owner, ordinal) ->
            w.Write 1uy
            writeTypeKeyId w owner
            w.Write ordinal
        | TyparScopeRow.ModuleFunction key ->
            w.Write 2uy
            writeBindingKeyId w key
        | TyparScopeRow.LocalFunction id ->
            w.Write 3uy
            w.Write id

    let private readTyparScopeRow (r: FrozenReader) : TyparScopeRow =
        match r.ReadByte() with
        | 0uy -> TyparScopeRow.Type(readTypeKeyId r)
        | 1uy ->
            let owner = readTypeKeyId r
            let ordinal = r.ReadInt32()
            TyparScopeRow.Member(owner, ordinal)
        | 2uy -> TyparScopeRow.ModuleFunction(readBindingKeyId r)
        | 3uy -> TyparScopeRow.LocalFunction(r.ReadInt32())
        | b -> failwithf "FrozenCodec: unknown TyparScopeRow tag %d" b

    let private writeMemberKindRow (w: FrozenWriter) (k: MemberKindRow) =
        match k with
        | MemberKindRow.Method -> w.Write 0uy
        | MemberKindRow.Property -> w.Write 1uy
        | MemberKindRow.InterfaceMethod iface ->
            w.Write 2uy
            writeTypeKeyId w iface
        | MemberKindRow.ExplicitInterfaceImpl iface ->
            w.Write 3uy
            writeTypeKeyId w iface

    let private readMemberKindRow (r: FrozenReader) : MemberKindRow =
        match r.ReadByte() with
        | 0uy -> MemberKindRow.Method
        | 1uy -> MemberKindRow.Property
        | 2uy -> MemberKindRow.InterfaceMethod(readTypeKeyId r)
        | 3uy -> MemberKindRow.ExplicitInterfaceImpl(readTypeKeyId r)
        | b -> failwithf "FrozenCodec: unknown MemberKindRow tag %d" b

    let private writeMemberKeyRow (w: FrozenWriter) (row: MemberKeyRow) =
        writeTypeKeyId w row.Decl
        writeStrId w row.Name
        writeTypeIds w row.ArgSig
        w.Write row.MethodTyparArity
        writeMemberKindRow w row.Kind

    let private readMemberKeyRow (r: FrozenReader) : MemberKeyRow =
        let decl = readTypeKeyId r
        let name = readStrId r
        let argSig = readTypeIds r
        let methodTyparArity = r.ReadInt32()
        let kind = readMemberKindRow r

        {
            Decl = decl
            Name = name
            ArgSig = argSig
            MethodTyparArity = methodTyparArity
            Kind = kind
        }

    let private writeSymbolRow (w: FrozenWriter) (row: SymbolRow) =
        match row with
        | SymbolRow.Type tk ->
            w.Write 0uy
            writeTypeKeyId w tk
        | SymbolRow.Binding bk ->
            w.Write 1uy
            writeBindingKeyId w bk
        | SymbolRow.Member mk ->
            w.Write 2uy
            writeMemberKeyId w mk

    let private readSymbolRow (r: FrozenReader) : SymbolRow =
        match r.ReadByte() with
        | 0uy -> SymbolRow.Type(readTypeKeyId r)
        | 1uy -> SymbolRow.Binding(readBindingKeyId r)
        | 2uy -> SymbolRow.Member(readMemberKeyId r)
        | b -> failwithf "FrozenCodec: unknown SymbolRow tag %d" b

    let private writeLiteralRow (w: FrozenWriter) (v: LiteralRow) =
        match v with
        | LiteralRow.String s ->
            w.Write 0uy
            writeStrId w s
        | LiteralRow.Int n ->
            w.Write 1uy
            w.Write n

    let private readLiteralRow (r: FrozenReader) : LiteralRow =
        match r.ReadByte() with
        | 0uy -> LiteralRow.String(readStrId r)
        | 1uy -> LiteralRow.Int(r.ReadInt64())
        | b -> failwithf "FrozenCodec: unknown LiteralRow tag %d" b

    let private writeUnknownReasonRow (w: FrozenWriter) (v: UnknownReasonRow) =
        match v with
        | UnknownReasonRow.UndefinedName name ->
            w.Write 0uy
            writeStrId w name
        | UnknownReasonRow.UnfreezableExternal what ->
            w.Write 1uy
            writeStrId w what
        | UnknownReasonRow.UnresolvedTypar -> w.Write 2uy
        | UnknownReasonRow.Deferred -> w.Write 3uy
        | UnknownReasonRow.ArityMismatch -> w.Write 4uy
        | UnknownReasonRow.NoValueType -> w.Write 5uy

    let private readUnknownReasonRow (r: FrozenReader) : UnknownReasonRow =
        match r.ReadByte() with
        | 0uy -> UnknownReasonRow.UndefinedName(readStrId r)
        | 1uy -> UnknownReasonRow.UnfreezableExternal(readStrId r)
        | 2uy -> UnknownReasonRow.UnresolvedTypar
        | 3uy -> UnknownReasonRow.Deferred
        | 4uy -> UnknownReasonRow.ArityMismatch
        | 5uy -> UnknownReasonRow.NoValueType
        | b -> failwithf "FrozenCodec: unknown UnknownReasonRow tag %d" b

    let private writeTypeRow (w: FrozenWriter) (row: TypeRow) =
        match row with
        | TypeRow.Const(key, args) ->
            w.Write 0uy
            writeTypeKeyId w key
            writeTypeIds w args
        | TypeRow.Fun(arg, result) ->
            w.Write 1uy
            writeTypeId w arg
            writeTypeId w result
        | TypeRow.Tuple items ->
            w.Write 2uy
            writeTypeIds w items
        | TypeRow.Record(key, args) ->
            w.Write 3uy
            writeTypeKeyId w key
            writeTypeIds w args
        | TypeRow.Union(key, args) ->
            w.Write 4uy
            writeTypeKeyId w key
            writeTypeIds w args
        | TypeRow.Class(key, args) ->
            w.Write 5uy
            writeTypeKeyId w key
            writeTypeIds w args
        | TypeRow.Enum key ->
            w.Write 6uy
            writeTypeKeyId w key
        | TypeRow.Or members ->
            w.Write 7uy
            writeEqSetWith w writeTypeId members
        | TypeRow.Literal value ->
            w.Write 8uy
            writeLiteralRow w value
        | TypeRow.KeyOf ty ->
            w.Write 9uy
            writeTypeId w ty
        | TypeRow.IndexedAccess(objTy, index) ->
            w.Write 10uy
            writeTypeId w objTy
            writeTypeId w index
        | TypeRow.Conditional payload ->
            w.Write 11uy
            writeTypeId w payload.Check
            writeTypeId w payload.Extends
            writeTypeId w payload.WhenTrue
            writeTypeId w payload.WhenFalse
        | TypeRow.Typar(scope, index) ->
            w.Write 12uy
            writeTyparScopeRow w scope
            w.Write index
        | TypeRow.Unknown reason ->
            w.Write 14uy
            writeUnknownReasonRow w reason
        | TypeRow.Measure atoms ->
            w.Write 15uy

            writeEqArrayWith
                w
                (fun w (a: MeasureAtomRow) ->
                    writeTypeKeyId w a.Atom
                    writeRational w a.Exponent
                )
                atoms

    // Rebuilt case for case with NO normalisation, because the stored row is already the canonical
    // one the freeze interned. `Or` in particular keeps the stored member sequence verbatim.
    let private readTypeRow (r: FrozenReader) : TypeRow =
        match r.ReadByte() with
        | 0uy ->
            let key = readTypeKeyId r
            let args = readTypeIds r
            TypeRow.Const(key, args)
        | 1uy ->
            let arg = readTypeId r
            let result = readTypeId r
            TypeRow.Fun(arg, result)
        | 2uy -> TypeRow.Tuple(readTypeIds r)
        | 3uy ->
            let key = readTypeKeyId r
            let args = readTypeIds r
            TypeRow.Record(key, args)
        | 4uy ->
            let key = readTypeKeyId r
            let args = readTypeIds r
            TypeRow.Union(key, args)
        | 5uy ->
            let key = readTypeKeyId r
            let args = readTypeIds r
            TypeRow.Class(key, args)
        | 6uy -> TypeRow.Enum(readTypeKeyId r)
        | 7uy -> TypeRow.Or(readEqSetWith r readTypeId)
        | 8uy -> TypeRow.Literal(readLiteralRow r)
        | 9uy -> TypeRow.KeyOf(readTypeId r)
        | 10uy ->
            let objTy = readTypeId r
            let index = readTypeId r
            TypeRow.IndexedAccess(objTy, index)
        | 11uy ->
            let check = readTypeId r
            let extends = readTypeId r
            let whenTrue = readTypeId r
            let whenFalse = readTypeId r

            TypeRow.Conditional
                {
                    Check = check
                    Extends = extends
                    WhenTrue = whenTrue
                    WhenFalse = whenFalse
                }
        | 12uy ->
            let scope = readTyparScopeRow r
            let index = r.ReadInt32()
            TypeRow.Typar(scope, index)
        | 14uy -> TypeRow.Unknown(readUnknownReasonRow r)
        | 15uy ->
            TypeRow.Measure(
                readEqArrayWith
                    r
                    (fun r ->
                        let atom = readTypeKeyId r
                        let exponent = readRational r

                        { Atom = atom; Exponent = exponent }
                    )
            )
        | b -> failwithf "FrozenCodec: unknown TypeRow tag %d" b

    let private writeFilePathRow (w: FrozenWriter) (row: FilePathRow) =
        writeStrId w row.Assembly
        writeStrId w row.Relative

    let private readFilePathRow (r: FrozenReader) : FilePathRow =
        let assembly = readStrId r
        let relative = readStrId r

        {
            Assembly = assembly
            Relative = relative
        }

    // ── the nine arrays ────────────────────────────────────────────────────

    /// The whole of the file's type/key tables, in `FrozenTypeRows` declaration order, which is
    /// also MINT order, so a row's children are rows of a table already at least this far read.
    /// The materialised side is derived on read and never stored.
    let writeTypeRows (w: FrozenWriter) (rows: FrozenTypeRows) =
        writeImmutableWith w (fun w (s: string) -> w.Write s) rows.Strings
        writeImmutableWith w writeNamespaceRow rows.Namespaces
        writeImmutableWith w writeModuleRow rows.Modules
        writeImmutableWith w writeTypeKeyRow rows.TypeKeys
        writeImmutableWith w writeBindingKeyRow rows.Bindings
        writeImmutableWith w writeMemberKeyRow rows.Members
        writeImmutableWith w writeSymbolRow rows.Symbols
        writeImmutableWith w writeTypeRow rows.Types
        writeImmutableWith w writeFilePathRow rows.FilePaths

    let readTypeRows (r: FrozenReader) : FrozenTypeRows =
        let strings = readImmutableWith r (fun r -> r.ReadString())
        let namespaces = readImmutableWith r readNamespaceRow
        let modules = readImmutableWith r readModuleRow
        let typeKeys = readImmutableWith r readTypeKeyRow
        let bindings = readImmutableWith r readBindingKeyRow
        let members = readImmutableWith r readMemberKeyRow
        let symbols = readImmutableWith r readSymbolRow
        let types = readImmutableWith r readTypeRow
        let filePaths = readImmutableWith r readFilePathRow

        {
            Strings = strings
            Namespaces = namespaces
            Modules = modules
            TypeKeys = typeKeys
            Bindings = bindings
            Members = members
            Symbols = symbols
            Types = types
            FilePaths = filePaths
        }
