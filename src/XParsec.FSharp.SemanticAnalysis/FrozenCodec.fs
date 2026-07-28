namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecTypes
open XParsec.FSharp.SemanticAnalysis.FrozenCodecDecls

/// A hand-rolled structural binary (de)serializer for the FROZEN domain, layered across
/// four modules: the leaf domains (`FrozenCodecPrimitives` for the stream seam and the value
/// structs, `FrozenCodecTypes` for `FrozenType` and the `SymbolKey`/`TypeKey` key cluster it
/// reaches), the non-tree declaration shell and scalar clusters a pool payload rides
/// (`FrozenCodecDecls`), and — on top of both — the `FrozenPools` COLUMN codec here that
/// `flatten`/`thaw` actually store. A plain `BinaryWriter`/`BinaryReader` over a
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
    // compile here rather than serializing as a silent alias. A node's SHAPE tag has no
    // writer at all: it is a projection of the payload (`ExprPayload.shape`), so the
    // payload's tag byte already carries it.

    let private writeBinderId (w: BinaryWriter) (BinderId i) = w.Write i
    let private readBinderId (r: BinaryReader) : BinderId = BinderId(r.ReadInt32())

    /// A jagged child-id column — one length-prefixed id list per pool slot. Generic over
    /// the id codec, so the expr-child and pat-child columns of all three domains share the
    /// one nesting convention rather than repeating it per domain.
    let private writeIdColumn (w: BinaryWriter) (writeId: BinaryWriter -> 'id -> unit) (col: 'id[][]) =
        writeArrayWith w (fun w ids -> writeArrayWith w writeId ids) col

    let private readIdColumn (r: BinaryReader) (readId: BinaryReader -> 'id) : 'id[][] =
        readArrayWith r (fun r -> readArrayWith r readId)

    /// A per-binder column (`BinderColumn`) — one optional value per binder slot, in
    /// `BinderKeys` order. NO id is written: the slot's position IS the binder, which is
    /// the whole property of the column form, so the wire carries a presence byte where the
    /// keyed form carried four id bytes plus a value.
    let private writeBinderColumn (w: BinaryWriter) (writeVal: BinaryWriter -> 'v -> unit) (col: BinderColumn<'v>) =
        writeArrayWith w (fun w v -> writeVOptionWith w writeVal v) col

    let private readBinderColumn (r: BinaryReader) (readVal: BinaryReader -> 'v) : BinderColumn<'v> =
        readArrayWith r (fun r -> readVOptionWith r readVal)

    /// A dense side table — the `(id, value)` association a `Map<NodeKey,_>` was re-keyed
    /// to. Generic over BOTH codecs, so the `BinderId`-keyed tables and the one
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
            writeBinderId w p.Var
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
            let var = readBinderId r
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
        | PatPayload.NamedSimple binder ->
            w.Write 0uy
            writeBinderId w binder
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
        | 0uy -> PatPayload.NamedSimple(readBinderId r)
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
        writeArrayWith w writeFrozenType p.ExprTys
        writeArrayWith w writeSyntaxToken p.ExprToks
        writeIdColumn w writeExprPoolId p.ExprChildren
        writeIdColumn w writePatPoolId p.ExprPatChildren
        writeArrayWith w (fun w b -> writeVOptionWith w writeBinderId b) p.ExprVarBinder
        writeArrayWith w writeExprPayload p.ExprPayloads
        writeArrayWith w writeFrozenType p.PatTys
        writeArrayWith w writeSyntaxToken p.PatToks
        writeIdColumn w writePatPoolId p.PatChildren
        writeArrayWith w writePatPayload p.PatPayloads
        writeIdColumn w writeExprPoolId p.DeclExprChildren
        writeIdColumn w writePatPoolId p.DeclPatChildren
        writeArrayWith w writeDeclPayload p.DeclPayloads
        writeArrayWith w writeDeclPoolId p.Roots
        writeArrayWith w writeInlineTemplate p.InlineTemplates
        writeArrayWith w writeNodeKey p.BinderKeys
        writeResidue w p.Residue
        writeDenseTable w writeBinderId writeModuleBindingInfo p.ModuleMembers
        writeDenseTable w writeBinderId writeClosureRepr p.ClosureReprs
        writeDenseTable w writeExprPoolId writeFunVerdict p.FunVerdicts
        writeDenseTable w writeBinderId (fun w cs -> writeListWith w writeFrozenConstraint cs) p.GenericFnSchemes
        writeDenseTable w writeBinderId writeValRepr p.BindingValReprs
        writeBinderColumn w (fun w (s: string) -> w.Write s) p.TopLevelNames
        writeBinderColumn w (fun w (i: int) -> w.Write i) p.BindingTyparArities

    let private readPools (r: BinaryReader) : FrozenPools =
        let exprTys = readArrayWith r readFrozenType
        let exprToks = readArrayWith r readSyntaxToken
        let exprChildren = readIdColumn r readExprPoolId
        let exprPatChildren = readIdColumn r readPatPoolId
        let exprVarBinder = readArrayWith r (fun r -> readVOptionWith r readBinderId)
        let exprPayloads = readArrayWith r readExprPayload
        let patTys = readArrayWith r readFrozenType
        let patToks = readArrayWith r readSyntaxToken
        let patChildren = readIdColumn r readPatPoolId
        let patPayloads = readArrayWith r readPatPayload
        let declExprChildren = readIdColumn r readExprPoolId
        let declPatChildren = readIdColumn r readPatPoolId
        let declPayloads = readArrayWith r readDeclPayload
        let roots = readArrayWith r readDeclPoolId
        let inlineTemplates = readArrayWith r readInlineTemplate
        let binderKeys = readArrayWith r readNodeKey
        let residue = readResidue r
        let moduleMembers = readDenseTable r readBinderId readModuleBindingInfo
        let closureReprs = readDenseTable r readBinderId readClosureRepr
        let funVerdicts = readDenseTable r readExprPoolId readFunVerdict

        let genericFnSchemes =
            readDenseTable r readBinderId (fun r -> readListWith r readFrozenConstraint)

        let bindingValReprs = readDenseTable r readBinderId readValRepr
        let topLevelNames = readBinderColumn r (fun r -> r.ReadString())
        let bindingTyparArities = readBinderColumn r (fun r -> r.ReadInt32())

        {
            ExprTys = exprTys
            ExprToks = exprToks
            ExprChildren = exprChildren
            ExprPatChildren = exprPatChildren
            ExprVarBinder = exprVarBinder
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
            BinderKeys = binderKeys
            Residue = residue
            ModuleMembers = moduleMembers
            ClosureReprs = closureReprs
            FunVerdicts = funVerdicts
            GenericFnSchemes = genericFnSchemes
            BindingValReprs = bindingValReprs
            TopLevelNames = topLevelNames
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
