namespace XParsec.FSharp.SemanticAnalysis

open Vesper
open XParsec.FSharp.Lexer

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecRows

/// The CONSTANT tier on the wire: the scalar carried by the tree and the side tables, the
/// checked constant expression, and the attribute lists embedded in every declaration shape.
/// Every payload goes out inline; its `ty` / `tok` columns travel through the shared type
/// table and the file's token indices.
module FrozenCodecConst =

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

    let rec private writeTConstResult (w: FrozenWriter) (v: TConstResult) =
        match v with
        | TConstResult.Scalar c ->
            w.Write 0uy
            writeTConstValue w c
        | TConstResult.Null -> w.Write 1uy
        | TConstResult.TypeVal t ->
            w.Write 2uy
            writeTypeRef w t
        | TConstResult.ArrayVal items ->
            w.Write 3uy
            writeBlockWith w writeTConstResult items

    let rec private readTConstResult (r: FrozenReader) : TConstResult =
        match r.ReadByte() with
        | 0uy -> TConstResult.Scalar(readTConstValue r)
        | 1uy -> TConstResult.Null
        | 2uy -> TConstResult.TypeVal(readTypeRef r)
        | 3uy -> TConstResult.ArrayVal(readBlockWith r readTConstResult)
        | b -> failwithf "FrozenCodec: unknown TConstResult tag %d" b

    let rec private writeTConstExpr (w: FrozenWriter) (e: TConstExpr) =
        let tail (ty: FrozenType) (tok: Anchor) =
            writeTypeRef w ty
            writeAnchor w tok

        match e with
        | TConstExpr.Literal(value, ty, tok) ->
            w.Write 0uy
            writeTConstValue w value
            tail ty tok
        | TConstExpr.Null(ty, tok) ->
            w.Write 1uy
            tail ty tok
        | TConstExpr.LiteralRef(binding, result, ty, tok) ->
            w.Write 2uy
            writeBindingKeyRef w binding
            writeTConstResult w result
            tail ty tok
        | TConstExpr.EnumCase(enumKey, caseName, result, tok) ->
            w.Write 3uy
            writeTypeKeyRef w enumKey
            w.Write caseName
            writeTConstResult w result
            writeAnchor w tok
        | TConstExpr.TypeOf(operand, ty, tok) ->
            w.Write 4uy
            writeTypeRef w operand
            tail ty tok
        | TConstExpr.NameOf(target, name, ty, tok) ->
            w.Write 5uy
            writeSymbolRef w target
            w.Write name
            tail ty tok
        | TConstExpr.ArrayLit(items, ty, tok) ->
            w.Write 6uy
            writeBlockWith w writeTConstExpr items
            tail ty tok
        | TConstExpr.Unary(op, operand, result, ty, tok) ->
            w.Write 7uy
            writeBindingKeyRef w op
            writeTConstExpr w operand
            writeTConstResult w result
            tail ty tok
        | TConstExpr.Binary(op, left, right, result, ty, tok) ->
            w.Write 8uy
            writeBindingKeyRef w op
            writeTConstExpr w left
            writeTConstExpr w right
            writeTConstResult w result
            tail ty tok

    let rec private readTConstExpr (r: FrozenReader) : TConstExpr =
        match r.ReadByte() with
        | 0uy ->
            let value = readTConstValue r
            TConstExpr.Literal(value, readTypeRef r, readAnchor r)
        | 1uy -> TConstExpr.Null(readTypeRef r, readAnchor r)
        | 2uy ->
            let binding = readBindingKeyRef r
            let result = readTConstResult r
            TConstExpr.LiteralRef(binding, result, readTypeRef r, readAnchor r)
        | 3uy ->
            let enumKey = readTypeKeyRef r
            let caseName = r.ReadString()
            let result = readTConstResult r
            TConstExpr.EnumCase(enumKey, caseName, result, readAnchor r)
        | 4uy ->
            let operand = readTypeRef r
            TConstExpr.TypeOf(operand, readTypeRef r, readAnchor r)
        | 5uy ->
            let target = readSymbolRef r
            let name = r.ReadString()
            TConstExpr.NameOf(target, name, readTypeRef r, readAnchor r)
        | 6uy ->
            let items = readBlockWith r readTConstExpr
            TConstExpr.ArrayLit(items, readTypeRef r, readAnchor r)
        | 7uy ->
            let op = readBindingKeyRef r
            let operand = readTConstExpr r
            let result = readTConstResult r
            TConstExpr.Unary(op, operand, result, readTypeRef r, readAnchor r)
        | 8uy ->
            let op = readBindingKeyRef r
            let left = readTConstExpr r
            let right = readTConstExpr r
            let result = readTConstResult r
            TConstExpr.Binary(op, left, right, result, readTypeRef r, readAnchor r)
        | b -> failwithf "FrozenCodec: unknown TConstExpr tag %d" b

    let writeTConstDenotation (w: FrozenWriter) (d: TConstDenotation) =
        writeTConstResult w d.Result
        writeTypeRef w d.Ty

    let readTConstDenotation (r: FrozenReader) : TConstDenotation =
        let result = readTConstResult r
        { Result = result; Ty = readTypeRef r }

    let private writeTAttributeArgTarget (w: FrozenWriter) (t: TAttributeArgTarget) =
        match t with
        | TAttributeArgTarget.Parameter index ->
            w.Write 0uy
            w.Write index
        | TAttributeArgTarget.Member(TAttributeMember.Property(name, ty)) ->
            w.Write 1uy
            w.Write name
            writeTypeRef w ty
        | TAttributeArgTarget.Member(TAttributeMember.Field(name, ty)) ->
            w.Write 2uy
            w.Write name
            writeTypeRef w ty

    let private readTAttributeArgTarget (r: FrozenReader) : TAttributeArgTarget =
        match r.ReadByte() with
        | 0uy -> TAttributeArgTarget.Parameter(r.ReadInt32())
        | 1uy ->
            let name = r.ReadString()
            TAttributeArgTarget.Member(TAttributeMember.Property(name, readTypeRef r))
        | 2uy ->
            let name = r.ReadString()
            TAttributeArgTarget.Member(TAttributeMember.Field(name, readTypeRef r))
        | b -> failwithf "FrozenCodec: unknown TAttributeArgTarget tag %d" b

    let private writeTAttributeArg (w: FrozenWriter) (a: TAttributeArg) =
        writeTAttributeArgTarget w a.Target
        writeTConstExpr w a.Expr

    let private readTAttributeArg (r: FrozenReader) : TAttributeArg =
        let target = readTAttributeArgTarget r

        {
            Target = target
            Expr = readTConstExpr r
        }

    let private writeTAttribute (w: FrozenWriter) (a: TAttribute) =
        writeTypeKeyRef w a.Key
        writeMemberKeyRef w a.Ctor
        writeBlockWith w writeTAttributeArg a.Args

    let private readTAttribute (r: FrozenReader) : TAttribute =
        let key = readTypeKeyRef r
        let ctor = readMemberKeyRef r
        let args = Block.ofArray (readArrayWith r readTAttributeArg)

        { Key = key; Ctor = ctor; Args = args }

    let writeTAttributes (w: FrozenWriter) (attrs: TAttributes) = writeBlockWith w writeTAttribute attrs

    let readTAttributes (r: FrozenReader) : TAttributes =
        Block.ofArray (readArrayWith r readTAttribute)
