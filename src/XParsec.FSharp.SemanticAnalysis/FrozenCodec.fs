namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecRows
open XParsec.FSharp.SemanticAnalysis.FrozenCodecDiagnostics
open XParsec.FSharp.SemanticAnalysis.FrozenCodecTypes
open XParsec.FSharp.SemanticAnalysis.FrozenCodecDecls

/// A hand-rolled structural binary (de)serializer for the FROZEN domain. The stored form is
/// the pool COLUMNS, not the DU: `flatten` IS the column writers and `thaw` their inverse,
/// with no tree codec and no conversion on either side.
[<RequireQualifiedAccess>]
module FrozenCodec =

    // ── the pool columns (the stored wire form) ─────────────────────────────
    //
    // A node's SHAPE tag has no writer, because it projects from the payload, whose tag IS written.

    /// A child-id column: the slot starts, then the one flat id array they delimit. Two flat
    /// arrays, so the wire carries ONE length prefix per column where the jagged form carried
    /// one per slot.
    let private writeChildColumn (w: FrozenWriter) (writeId: FrozenWriter -> 'id -> unit) (col: ChildColumn<'id>) =
        writeArrayWith w (fun w (s: int) -> w.Write s) (ChildColumn.starts col)
        writeArrayWith w writeId (ChildColumn.ids col)

    let private readChildColumn (r: FrozenReader) (readId: FrozenReader -> 'id) : ChildColumn<'id> =
        let start = readArrayWith r (fun r -> r.ReadInt32())
        let ids = readArrayWith r readId

        ChildColumn.ofStored start ids

    /// A decoded child column delimits the pool it is indexed by, and no other count. That is
    /// the check `ChildColumn.ofStored` cannot make from one column in isolation. A truncated
    /// column is internally consistent and would run off the end at some later node.
    let private checkSlots (name: string) (poolSize: int) (col: ChildColumn<'id>) =
        if ChildColumn.length col <> poolSize then
            failwithf "FrozenCodec: %s delimits %d slots but its pool holds %d" name (ChildColumn.length col) poolSize

    /// One optional value per bound variable slot, in bound-variable pool order. NO id is written,
    /// because the slot's POSITION is the bound variable, so the wire carries a presence byte per slot.
    let private writeBoundVarColumn (w: FrozenWriter) (writeVal: FrozenWriter -> 'v -> unit) (col: BoundVarColumn<'v>) =
        writeArrayWith w (fun w v -> writeVOptionWith w writeVal v) col

    let private readBoundVarColumn (r: FrozenReader) (readVal: FrozenReader -> 'v) : BoundVarColumn<'v> =
        readArrayWith r (fun r -> readVOptionWith r readVal)

    /// A dense side table: the `(id, value)` pairs a keyed map was re-keyed to.
    let private writeDenseTable
        (w: FrozenWriter)
        (writeId: FrozenWriter -> 'id -> unit)
        (writeVal: FrozenWriter -> 'v -> unit)
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
        (r: FrozenReader)
        (readId: FrozenReader -> 'id)
        (readVal: FrozenReader -> 'v)
        : ('id * 'v)[] =
        readArrayWith
            r
            (fun r ->
                let id = readId r
                let v = readVal r
                id, v
            )

    let private writeFormatSinkShape (w: FrozenWriter) (s: FormatSinkShape) =
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

    let private readFormatSinkShape (r: FrozenReader) : FormatSinkShape =
        match r.ReadByte() with
        | 0uy -> FormatSinkShape.ToStdOut(r.ReadBoolean())
        | 1uy -> FormatSinkShape.ToStdErr(r.ReadBoolean())
        | 2uy -> FormatSinkShape.ToWriter(r.ReadBoolean())
        | 3uy -> FormatSinkShape.ToBuilder
        | 4uy -> FormatSinkShape.ToString
        | b -> failwithf "FrozenCodec: unknown FormatSinkShape tag %d" b

    let private writeFormatSegShape (w: FrozenWriter) (s: FormatSegShape) =
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

    let private readFormatSegShape (r: FrozenReader) : FormatSegShape =
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

    let private writeExprPayload (w: FrozenWriter) (p: ExprPayload) =
        match p with
        | ExprPayload.Const value ->
            w.Write 0uy
            writeTConstValue w value
        | ExprPayload.Var -> w.Write 1uy
        | ExprPayload.External p ->
            w.Write 2uy
            w.Write p.CompiledName
            writeVOptionWith w writeSymbolRef p.Key
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
            writeBoundVarId w p.Var
            writeAnchor w p.IdentTok
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
            writeVOptionWith w writeSymbolRef p.Key
        | ExprPayload.MethodCall p ->
            w.Write 25uy
            writeSymbolRef w p.Key
            writeCallVia w p.Via
        | ExprPayload.PropertyGet p ->
            w.Write 26uy
            writeSymbolRef w p.Key
            writeCallVia w p.Via
        | ExprPayload.StaticMethodCall key ->
            w.Write 27uy
            writeSymbolRef w key
        | ExprPayload.StaticPropertyGet key ->
            w.Write 28uy
            writeSymbolRef w key
        | ExprPayload.StaticFieldGet p ->
            w.Write 29uy
            writeTypeKeyRef w p.DeclKey
            w.Write p.FieldName
        | ExprPayload.StaticFieldSet p ->
            w.Write 30uy
            writeTypeKeyRef w p.DeclKey
            w.Write p.FieldName
        | ExprPayload.ExternalMember p ->
            w.Write 31uy
            w.Write p.HasObjArg
            writeSymbolRef w p.Key
            w.Write p.MemberName
            writeMemberStorage w p.Storage
            writeEqArrayWith w (fun w (n: int) -> w.Write n) p.ArgGroupWidths
        | ExprPayload.Format p ->
            w.Write 32uy
            writeFormatSinkShape w p.Sink
            writeArrayWith w writeFormatSegShape p.Segments
        | ExprPayload.ILIntrinsic p ->
            w.Write 33uy
            w.Write p.OpCode
            writeVOptionWith w writeTypeRef p.TypeOperand
        | ExprPayload.StaticOptimization clauseConstraints ->
            w.Write 34uy
            writeArrayWith w (fun w cs -> writeEqArrayWith w writeStaticOptConstraint cs) clauseConstraints
        | ExprPayload.Upcast -> w.Write 35uy
        | ExprPayload.Downcast -> w.Write 36uy
        | ExprPayload.TypeTest testTy ->
            w.Write 37uy
            writeTypeRef w testTy
        | ExprPayload.TraitCall p ->
            w.Write 38uy
            writeTypeRef w p.SupportTy
            w.Write p.MemberName
        | ExprPayload.InlineCall p ->
            w.Write 39uy
            writeSpecializationId w p.Spec
            writeOriginRef w p.Origin
        | ExprPayload.CallerExpr origin ->
            w.Write 40uy
            writeOriginRef w origin
        | ExprPayload.ArrayLit -> w.Write 41uy

    let private readExprPayload (r: FrozenReader) : ExprPayload =
        match r.ReadByte() with
        | 0uy -> ExprPayload.Const(readTConstValue r)
        | 1uy -> ExprPayload.Var
        | 2uy ->
            let compiledName = r.ReadString()
            let key = readVOptionWith r readSymbolRef

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
            let var = readBoundVarId r
            let identTok = readAnchor r

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
            let key = readVOptionWith r readSymbolRef
            ExprPayload.New {| ClassName = className; Key = key |}
        | 25uy ->
            let key = readSymbolRef r
            let via = readCallVia r
            ExprPayload.MethodCall {| Key = key; Via = via |}
        | 26uy ->
            let key = readSymbolRef r
            let via = readCallVia r
            ExprPayload.PropertyGet {| Key = key; Via = via |}
        | 27uy -> ExprPayload.StaticMethodCall(readSymbolRef r)
        | 28uy -> ExprPayload.StaticPropertyGet(readSymbolRef r)
        | 29uy ->
            let declKey = readTypeKeyRef r
            let fieldName = r.ReadString()

            ExprPayload.StaticFieldGet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | 30uy ->
            let declKey = readTypeKeyRef r
            let fieldName = r.ReadString()

            ExprPayload.StaticFieldSet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | 31uy ->
            let hasObjArg = r.ReadBoolean()
            let key = readSymbolRef r
            let memberName = r.ReadString()
            let storage = readMemberStorage r
            let argGroupWidths = readEqArrayWith r (fun r -> r.ReadInt32())

            ExprPayload.ExternalMember
                {|
                    HasObjArg = hasObjArg
                    Key = key
                    MemberName = memberName
                    Storage = storage
                    ArgGroupWidths = argGroupWidths
                |}
        | 32uy ->
            let sink = readFormatSinkShape r
            let segments = readArrayWith r readFormatSegShape
            ExprPayload.Format {| Sink = sink; Segments = segments |}
        | 33uy ->
            let opCode = r.ReadString()
            let typeOperand = readVOptionWith r readTypeRef

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
        | 37uy -> ExprPayload.TypeTest(readTypeRef r)
        | 38uy ->
            let supportTy = readTypeRef r
            let memberName = r.ReadString()

            ExprPayload.TraitCall
                {|
                    SupportTy = supportTy
                    MemberName = memberName
                |}
        | 39uy ->
            let spec = readSpecializationId r

            ExprPayload.InlineCall
                {|
                    Spec = spec
                    Origin = readOriginRef r
                |}
        | 40uy -> ExprPayload.CallerExpr(readOriginRef r)
        | 41uy -> ExprPayload.ArrayLit
        | b -> failwithf "FrozenCodec: unknown ExprPayload tag %d" b

    let private writePatPayload (w: FrozenWriter) (p: PatPayload) =
        match p with
        | PatPayload.NamedSimple boundVar ->
            w.Write 0uy
            writeBoundVarId w boundVar
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
            writeTypeRef w testTy
        | PatPayload.EnumCase p ->
            w.Write 9uy
            writeTypeKeyRef w p.EnumKey
            w.Write p.CaseName

    let private readPatPayload (r: FrozenReader) : PatPayload =
        match r.ReadByte() with
        | 0uy -> PatPayload.NamedSimple(readBoundVarId r)
        | 1uy -> PatPayload.Wildcard
        | 2uy -> PatPayload.Null
        | 3uy -> PatPayload.Tuple
        | 4uy -> PatPayload.Or
        | 5uy -> PatPayload.Const(readTConstValue r)
        | 6uy -> PatPayload.Record(readArrayWith r (fun r -> r.ReadString()))
        | 7uy -> PatPayload.Union(r.ReadString())
        | 8uy -> PatPayload.TypeTestAs(readTypeRef r)
        | 9uy ->
            let enumKey = readTypeKeyRef r
            let caseName = r.ReadString()

            PatPayload.EnumCase
                {|
                    EnumKey = enumKey
                    CaseName = caseName
                |}
        | b -> failwithf "FrozenCodec: unknown PatPayload tag %d" b

    let private writeDeclPayload (w: FrozenWriter) (p: DeclPayload) =
        match p with
        | DeclPayload.Let p ->
            w.Write 0uy
            w.Write p.IsInline
            writeTypeRef w p.Ty
        | DeclPayload.Expression ty ->
            w.Write 1uy
            writeTypeRef w ty
        | DeclPayload.Type td ->
            w.Write 2uy
            writeTypeDecl w td

    let private readDeclPayload (r: FrozenReader) : DeclPayload =
        match r.ReadByte() with
        | 0uy ->
            let isInline = r.ReadBoolean()
            let ty = readTypeRef r
            DeclPayload.Let {| IsInline = isInline; Ty = ty |}
        | 1uy -> DeclPayload.Expression(readTypeRef r)
        | 2uy -> DeclPayload.Type(readTypeDecl r)
        | b -> failwithf "FrozenCodec: unknown DeclPayload tag %d" b

    /// The un-pooled fields, verbatim, because none of them is a tree.
    let private writeResidue (w: FrozenWriter) (res: FrozenFileResidue) =
        writeListWith w writeDiagnostic res.Diagnostics
        writeTypeKeyDict w writeIntrinsicReprInfo res.IntrinsicReprKeys
        writeSymbolSet w res.GlobalValueKeys
        writeSymbolDict w writeAccessibility res.Accessibility

    let private readResidue (r: FrozenReader) : FrozenFileResidue =
        let diagnostics = readListWith r readDiagnostic
        let intrinsicReprKeys = readTypeKeyDict r readIntrinsicReprInfo
        let globalValueKeys = readSymbolSet r
        let accessibility = readSymbolDict r readAccessibility

        {
            Diagnostics = diagnostics
            IntrinsicReprKeys = intrinsicReprKeys
            GlobalValueKeys = globalValueKeys
            Accessibility = accessibility
        }

    /// Every column and side table, in `FrozenPools` declaration order, but NOT the tables
    /// the types in them are ids into, which `writePools` puts in front of this.
    let private writeBody (w: FrozenWriter) (p: FrozenPools) =
        writeOriginRef w p.Origin
        writeArrayWith w writeTypeId p.ExprTys
        writeArrayWith w writeAnchor p.ExprToks
        writeChildColumn w writeExprPoolId p.ExprChildren
        writeChildColumn w writePatPoolId p.ExprPatChildren
        writeArrayWith w (fun w b -> writeVOptionWith w writeBoundVarId b) p.ExprVarBoundVar
        writeArrayWith w writeExprPayload p.ExprPayloads
        writeArrayWith w writeTypeId p.PatTys
        writeArrayWith w writeAnchor p.PatToks
        writeChildColumn w writePatPoolId p.PatChildren
        writeArrayWith w writePatPayload p.PatPayloads
        writeChildColumn w writeExprPoolId p.DeclExprChildren
        writeChildColumn w writePatPoolId p.DeclPatChildren
        writeArrayWith w writeDeclPayload p.DeclPayloads
        writeArrayWith w writeDeclPoolId p.Roots
        writeArrayWith w writeInlineTemplate p.InlineTemplates
        writeArrayWith w writeSpecialization p.Specializations
        writeArrayWith w (fun w (s: string) -> w.Write s) p.BoundVarNames
        writeArrayWith w writeAnchor p.BoundVarToks
        writeResidue w p.Residue
        writeDenseTable w writeBoundVarId writeModuleBindingInfo p.ModuleMembers
        writeDenseTable w writeBoundVarId writeClosureRepr p.ClosureReprs
        writeDenseTable w writeExprPoolId writeFunVerdict p.FunVerdicts

        writeDenseTable w writeBoundVarId (fun w cs -> writeListWith w writeFrozenConstraint cs) p.GenericFnSchemes

        writeDenseTable w writeBoundVarId writeValRepr p.BindingValReprs
        writeBoundVarColumn w (fun w (i: int) -> w.Write i) p.BindingTyparArities

    let private writePools (w: FrozenWriter) (p: FrozenPools) =
        // The tables must be READ first, but are not KNOWN until the body has been written: a
        // payload embeds types the `ty` columns never carried, and interning them is what
        // appends the rows. So the body is buffered and the finished rows go out in front.
        let body = toBytes w.Types writeBody p
        writeTypeRows w w.Types.Rows
        w.Write(body, 0, body.Length)

    let private readPools (r: FrozenReader) : FrozenPools =
        // The tables come first and everything below resolves against THEM, so the reader is
        // rebound to the file's own before a single column is touched.
        let types = FrozenTypeTable.OfRows(readTypeRows r)
        let r = { r with Types = types }
        let origin = readOriginRef r
        let exprTys = readArrayWith r readTypeId
        let exprToks = readArrayWith r readAnchor
        let exprChildren = readChildColumn r readExprPoolId
        let exprPatChildren = readChildColumn r readPatPoolId
        let exprVarBoundVar = readArrayWith r (fun r -> readVOptionWith r readBoundVarId)
        let exprPayloads = readArrayWith r readExprPayload
        let patTys = readArrayWith r readTypeId
        let patToks = readArrayWith r readAnchor
        let patChildren = readChildColumn r readPatPoolId
        let patPayloads = readArrayWith r readPatPayload
        let declExprChildren = readChildColumn r readExprPoolId
        let declPatChildren = readChildColumn r readPatPoolId
        let declPayloads = readArrayWith r readDeclPayload
        let roots = readArrayWith r readDeclPoolId
        let inlineTemplates = readArrayWith r readInlineTemplate
        let specializations = readArrayWith r readSpecialization
        let boundVarNames = readArrayWith r (fun r -> r.ReadString())
        let boundVarToks = readArrayWith r readAnchor
        let residue = readResidue r
        let moduleMembers = readDenseTable r readBoundVarId readModuleBindingInfo
        let closureReprs = readDenseTable r readBoundVarId readClosureRepr
        let funVerdicts = readDenseTable r readExprPoolId readFunVerdict

        let genericFnSchemes =
            readDenseTable r readBoundVarId (fun r -> readListWith r readFrozenConstraint)

        let bindingValReprs = readDenseTable r readBoundVarId readValRepr
        let bindingTyparArities = readBoundVarColumn r (fun r -> r.ReadInt32())

        checkSlots "ExprChildren" exprPayloads.Length exprChildren
        checkSlots "ExprPatChildren" exprPayloads.Length exprPatChildren
        checkSlots "PatChildren" patPayloads.Length patChildren
        checkSlots "DeclExprChildren" declPayloads.Length declExprChildren
        checkSlots "DeclPatChildren" declPayloads.Length declPatChildren

        {
            Origin = origin
            Types = types
            ExprTys = exprTys
            ExprToks = exprToks
            ExprChildren = exprChildren
            ExprPatChildren = exprPatChildren
            ExprVarBoundVar = exprVarBoundVar
            ExprPayloads = exprPayloads
            PatTys = patTys
            PatToks = patToks
            PatChildren = patChildren
            PatPayloads = patPayloads
            DeclExprChildren = declExprChildren
            DeclPatChildren = declPatChildren
            DeclPayloads = declPayloads
            Roots = roots
            InlineTemplates = inlineTemplates
            Specializations = specializations
            BoundVarNames = boundVarNames
            BoundVarToks = boundVarToks
            Residue = residue
            ModuleMembers = moduleMembers
            ClosureReprs = closureReprs
            FunVerdicts = funVerdicts
            GenericFnSchemes = genericFnSchemes
            BindingValReprs = bindingValReprs
            BindingTyparArities = bindingTyparArities
        }

    // ── the whole frozen file (top-level entry points) ──────────────────────

    /// The file's type/key tables, then the columns as they stand. The only work is EXTENDING
    /// the tables with types a payload embeds; every already-minted id still identifies the row
    /// it did, so `thaw` inverts this and re-flattening reproduces the blob byte for byte.
    let flatten (pools: FrozenPools) : byte[] =
        toBytes (FrozenTypeTableBuilder.OfRows pools.Types.Rows) writePools pools

    /// Rebuild the frozen file's pools from a `flatten` blob. It starts on the EMPTY tables:
    /// the blob's own have not been read yet, and reading them rebinds the reader.
    let thaw (bytes: byte[]) : FrozenPools =
        ofBytes FrozenTypeTable.Empty readPools bytes
