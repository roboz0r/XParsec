namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecTypes

/// The declaration shell and the scalar clusters a pool payload rides, the shapes whose
/// sub-trees are named by pool id instead of being inlined: a `type` declaration's member
/// bodies, an inline template's decl, a `ValRepr`'s tuple group.
module FrozenCodecDecls =

    // ── the declaration shell + the scalar clusters riding a pool payload ───
    //
    // Each writer is followed IMMEDIATELY by its reader; nothing else keeps them in step.

    /// Pool ids are plain `int`s: the blob is Brotli-compressed at the store seam, which
    /// absorbs their width redundancy more cheaply than a varint costs in reader complexity.
    let writeExprPoolId (w: FrozenWriter) (ExprPoolId i) = w.Write i
    let readExprPoolId (r: FrozenReader) : ExprPoolId = ExprPoolId(r.ReadInt32())
    let writePatPoolId (w: FrozenWriter) (PatPoolId i) = w.Write i
    let readPatPoolId (r: FrozenReader) : PatPoolId = PatPoolId(r.ReadInt32())
    let writeDeclPoolId (w: FrozenWriter) (DeclPoolId i) = w.Write i
    let readDeclPoolId (r: FrozenReader) : DeclPoolId = DeclPoolId(r.ReadInt32())
    // Not a pool COLUMN index but a root-array one; same width, same reasoning.
    let writeSpecializationId (w: FrozenWriter) (SpecializationId i) = w.Write i

    let readSpecializationId (r: FrozenReader) : SpecializationId = SpecializationId(r.ReadInt32())

    let rec writeDisposal (w: FrozenWriter) (d: Disposal) =
        match d with
        | Disposal.ViaCapability slot ->
            w.Write 0uy
            writeSymbolRef w slot
        | Disposal.ViaOwnMember key ->
            w.Write 1uy
            writeSymbolRef w key
        | Disposal.Unresolved -> w.Write 2uy

    and readDisposal (r: FrozenReader) : Disposal =
        match r.ReadByte() with
        | 0uy -> Disposal.ViaCapability(readSymbolRef r)
        | 1uy -> Disposal.ViaOwnMember(readSymbolRef r)
        | 2uy -> Disposal.Unresolved
        | b -> failwithf "FrozenCodec: unknown Disposal tag %d" b

    and writeCallVia (w: FrozenWriter) (v: CallVia<FrozenType>) =
        match v with
        | CallVia.Self -> w.Write 0uy
        | CallVia.Base -> w.Write 1uy
        | CallVia.Interface ifaceArgs ->
            w.Write 2uy
            writeEqArrayWith w writeTypeRef ifaceArgs

    and readCallVia (r: FrozenReader) : CallVia<FrozenType> =
        match r.ReadByte() with
        | 0uy -> CallVia.Self
        | 1uy -> CallVia.Base
        | 2uy -> CallVia.Interface(EqArray.ofArray (readArrayWith r readTypeRef))
        | b -> failwithf "FrozenCodec: unknown CallVia tag %d" b

    and writeStaticOptConstraint (w: FrozenWriter) (c: Frozen.TStaticOptConstraint) =
        match c with
        | TStaticOptConstraintG.TyconEquals(typar, required) ->
            w.Write 0uy
            writeTypeRef w typar
            writeTypeRef w required
        | TStaticOptConstraintG.IsStruct typar ->
            w.Write 1uy
            writeTypeRef w typar

    and readStaticOptConstraint (r: FrozenReader) : Frozen.TStaticOptConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typar = readTypeRef r
            let required = readTypeRef r
            TStaticOptConstraintG.TyconEquals(typar, required)
        | 1uy -> TStaticOptConstraintG.IsStruct(readTypeRef r)
        | b -> failwithf "FrozenCodec: unknown TStaticOptConstraint tag %d" b

    and writeForInEnumerator (w: FrozenWriter) (e: Frozen.ForInEnumerator) =
        match e with
        | ForInEnumeratorG.Interface -> w.Write 0uy
        | ForInEnumeratorG.Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose) ->
            w.Write 1uy
            writeTypeRef w enumeratorTy
            writeForInGetEnum w getEnumerator
            writeForInEnumMembers w members
            w.Write isValueType
            w.Write dispose

    and readForInEnumerator (r: FrozenReader) : Frozen.ForInEnumerator =
        match r.ReadByte() with
        | 0uy -> ForInEnumeratorG.Interface
        | 1uy ->
            let enumeratorTy = readTypeRef r
            let getEnumerator = readForInGetEnum r
            let members = readForInEnumMembers r
            let isValueType = r.ReadBoolean()
            let dispose = r.ReadBoolean()
            ForInEnumeratorG.Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose)
        | b -> failwithf "FrozenCodec: unknown ForInEnumerator tag %d" b

    and private writeForInGetEnum (w: FrozenWriter) (g: ForInGetEnumG<FrozenType>) =
        match g with
        | ForInGetEnumG.External getEnumerator ->
            w.Write 0uy
            writeSymbolRef w getEnumerator
        | ForInGetEnumG.Local -> w.Write 1uy
        | ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKeyRef w iface
            writeEqArrayWith w writeTypeRef ifaceArgs

    and private readForInGetEnum (r: FrozenReader) : ForInGetEnumG<FrozenType> =
        match r.ReadByte() with
        | 0uy -> ForInGetEnumG.External(readSymbolRef r)
        | 1uy -> ForInGetEnumG.Local
        | 2uy ->
            let iface = readTypeKeyRef r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readTypeRef)
            ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInGetEnum tag %d" b

    and private writeForInEnumMembers (w: FrozenWriter) (m: ForInEnumMembersG<FrozenType>) =
        match m with
        | ForInEnumMembersG.External(moveNext, current) ->
            w.Write 0uy
            writeSymbolRef w moveNext
            writeSymbolRef w current
        | ForInEnumMembersG.Local -> w.Write 1uy
        | ForInEnumMembersG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKeyRef w iface
            writeEqArrayWith w writeTypeRef ifaceArgs

    and private readForInEnumMembers (r: FrozenReader) : ForInEnumMembersG<FrozenType> =
        match r.ReadByte() with
        | 0uy ->
            let moveNext = readSymbolRef r
            let current = readSymbolRef r
            ForInEnumMembersG.External(moveNext, current)
        | 1uy -> ForInEnumMembersG.Local
        | 2uy ->
            let iface = readTypeKeyRef r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readTypeRef)
            ForInEnumMembersG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInEnumMembers tag %d" b

    // The `type` declaration shell, the one declaration shape a pool payload still carries
    // whole. Its member / preamble / ctor bodies bottom out at `writeExprPoolId`.

    and writeTypeDecl (w: FrozenWriter) (td: PooledTypeDecl) =
        w.Write td.Name
        writeTypeKeyRef w td.TypeKey
        writeOptionWith w (fun w (s: string) -> w.Write s) td.Namespace
        writeStringArray w td.TypeParams
        w.Write td.IsRequireQualifiedAccess
        writeTypeKind w td.Kind
        writeEqualityVerdict w td.EqualitySupport
        writeComparisonVerdict w td.ComparisonSupport

    and readTypeDecl (r: FrozenReader) : PooledTypeDecl =
        let name = r.ReadString()
        let typeKey = readTypeKeyRef r
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

    and private writeTypeKind (w: FrozenWriter) (k: TTypeKindG<FrozenType, Anchor, BoundVarId, ExprPoolId>) =
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

    and private readTypeKind (r: FrozenReader) : TTypeKindG<FrozenType, Anchor, BoundVarId, ExprPoolId> =
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
    // bodies. Shared by the class / union / record arms.
    and private writeInterfaces
        (w: FrozenWriter)
        (interfaces: EqArray<FrozenType * EqArray<TTypeMemberG<FrozenType, BoundVarId, ExprPoolId>>>)
        =
        writeEqArrayWith
            w
            (fun w (ty, mems) ->
                writeTypeRef w ty
                writeEqArrayWith w writeTypeMember mems
            )
            interfaces

    and private readInterfaces
        (r: FrozenReader)
        : EqArray<FrozenType * EqArray<TTypeMemberG<FrozenType, BoundVarId, ExprPoolId>>> =
        EqArray.ofArray (
            readArrayWith
                r
                (fun r ->
                    let ty = readTypeRef r
                    let mems = EqArray.ofArray (readArrayWith r readTypeMember)
                    ty, mems
                )
        )

    and private writeDeclaredFlags (w: FrozenWriter) (d: DeclaredClassFlags) =
        w.Write d.IsSealed
        w.Write d.IsAbstract
        w.Write d.AllowNullLiteral

    and private readDeclaredFlags (r: FrozenReader) : DeclaredClassFlags =
        let isSealed = r.ReadBoolean()
        let isAbstract = r.ReadBoolean()
        let allowNullLiteral = r.ReadBoolean()

        {
            IsSealed = isSealed
            IsAbstract = isAbstract
            AllowNullLiteral = allowNullLiteral
        }

    and private writeClass (w: FrozenWriter) (c: TClassG<FrozenType, BoundVarId, ExprPoolId>) =
        writeEqArrayWith w writeRecordField c.Fields
        writeEqArrayWith w writeRecordField c.CtorParams
        writeEqArrayWith w writeTypeMember c.Members
        writeVOptionWith w writeTypeRef c.BaseType
        writeInterfaces w c.Interfaces
        writeDeclaredFlags w c.Declared
        writeEqArrayWith w writePreambleEntry c.StaticPreamble
        writeEqArrayWith w writePreambleEntry c.InstancePreamble
        writeBoundVarSlot w c.ThisKey
        writeEqArrayWith w writeSecondaryCtor c.SecondaryCtors
        writeVOptionWith w writeBaseCtorCall c.BaseCtorCall
        writeClassValueKind w c.ValueKind
        w.Write c.HasPrimaryCtor

    and private readClass (r: FrozenReader) : TClassG<FrozenType, BoundVarId, ExprPoolId> =
        let fields = EqArray.ofArray (readArrayWith r readRecordField)
        let ctorParams = EqArray.ofArray (readArrayWith r readRecordField)
        let members = EqArray.ofArray (readArrayWith r readTypeMember)
        let baseType = readVOptionWith r readTypeRef
        let interfaces = readInterfaces r
        let declared = readDeclaredFlags r
        let staticPreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let instancePreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let thisKey = readBoundVarSlot r
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
            Declared = declared
            StaticPreamble = staticPreamble
            InstancePreamble = instancePreamble
            ThisKey = thisKey
            SecondaryCtors = secondaryCtors
            BaseCtorCall = baseCtorCall
            ValueKind = valueKind
            HasPrimaryCtor = hasPrimaryCtor
        }

    and private writeTypeMember (w: FrozenWriter) (m: TTypeMemberG<FrozenType, BoundVarId, ExprPoolId>) =
        w.Write m.Name
        w.Write m.IsStatic
        writeAccessibility w m.Accessibility
        w.Write m.IsInline
        writeTMemberKind w m.Kind
        w.Write m.IsOverride
        writeVOptionWith w writeBoundVarSlot m.ThisKey
        writeVOptionWith w writeBoundVarSlot m.BaseKey
        writeTypeRef w m.ThisTy

        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBoundVarSlot w k
                writeTypeRef w ty
            )
            m.Params

        writeExprPoolId w m.Body
        writeTypeRef w m.ReturnTy
        writeMethodTypeParams w m.MethodTypeParams

    and private readTypeMember (r: FrozenReader) : TTypeMemberG<FrozenType, BoundVarId, ExprPoolId> =
        let name = r.ReadString()
        let isStatic = r.ReadBoolean()
        let accessibility = readAccessibility r
        let isInline = r.ReadBoolean()
        let kind = readTMemberKind r
        let isOverride = r.ReadBoolean()
        let thisKey = readVOptionWith r readBoundVarSlot
        let baseKey = readVOptionWith r readBoundVarSlot
        let thisTy = readTypeRef r

        let parameters =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readBoundVarSlot r
                        let ty = readTypeRef r
                        k, ty
                    )
            )

        let body = readExprPoolId r
        let returnTy = readTypeRef r
        let methodTypeParams = readMethodTypeParams r

        {
            Name = name
            IsStatic = isStatic
            Accessibility = accessibility
            IsInline = isInline
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

    and private writeClassLet (w: FrozenWriter) (l: TClassLetG<FrozenType, ExprPoolId>) =
        w.Write l.Name
        writeTypeRef w l.Type
        w.Write l.IsMutable
        writeExprPoolId w l.Init

    and private readClassLet (r: FrozenReader) : TClassLetG<FrozenType, ExprPoolId> =
        let name = r.ReadString()
        let ty = readTypeRef r
        let isMutable = r.ReadBoolean()
        let init = readExprPoolId r

        {
            Name = name
            Type = ty
            IsMutable = isMutable
            Init = init
        }

    and private writePreambleEntry (w: FrozenWriter) (p: TPreambleEntryG<FrozenType, ExprPoolId>) =
        match p with
        | TPreambleEntryG.Let l ->
            w.Write 0uy
            writeClassLet w l
        | TPreambleEntryG.Do e ->
            w.Write 1uy
            writeExprPoolId w e

    and private readPreambleEntry (r: FrozenReader) : TPreambleEntryG<FrozenType, ExprPoolId> =
        match r.ReadByte() with
        | 0uy -> TPreambleEntryG.Let(readClassLet r)
        | 1uy -> TPreambleEntryG.Do(readExprPoolId r)
        | b -> failwithf "FrozenCodec: unknown TPreambleEntry tag %d" b

    and private writeCtorLet (w: FrozenWriter) (cl: TCtorLetG<FrozenType, BoundVarId, ExprPoolId>) =
        writeBoundVarSlot w cl.BoundVar
        writeTypeRef w cl.Type
        writeExprPoolId w cl.Init

    and private readCtorLet (r: FrozenReader) : TCtorLetG<FrozenType, BoundVarId, ExprPoolId> =
        let boundVar = readBoundVarSlot r
        let ty = readTypeRef r
        let init = readExprPoolId r

        {
            BoundVar = boundVar
            Type = ty
            Init = init
        }

    and private writeCtorFieldInit (w: FrozenWriter) (fi: TCtorFieldInitG<ExprPoolId>) =
        w.Write fi.Field
        writeExprPoolId w fi.Init

    and private readCtorFieldInit (r: FrozenReader) : TCtorFieldInitG<ExprPoolId> =
        let field = r.ReadString()
        let init = readExprPoolId r
        { Field = field; Init = init }

    and private writeSecondaryCtor (w: FrozenWriter) (sc: TSecondaryCtorG<FrozenType, BoundVarId, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBoundVarSlot w k
                writeTypeRef w ty
            )
            sc.Params

        writeEqArrayWith w writeCtorLet sc.Lets
        writeEqArrayWith w writeExprPoolId sc.PrimaryArgs
        writeEqArrayWith w writeCtorFieldInit sc.FieldInits

    and private readSecondaryCtor (r: FrozenReader) : TSecondaryCtorG<FrozenType, BoundVarId, ExprPoolId> =
        let parameters =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readBoundVarSlot r
                        let ty = readTypeRef r
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

    and private writeBaseCtorCall (w: FrozenWriter) (bc: TBaseCtorCallG<FrozenType, BoundVarId, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBoundVarSlot w k
                writeTypeRef w ty
            )
            bc.CtorParams

        writeEqArrayWith w writeExprPoolId bc.Args
        writeVOptionWith w writeSymbolRef bc.ChosenCtor

    and private readBaseCtorCall (r: FrozenReader) : TBaseCtorCallG<FrozenType, BoundVarId, ExprPoolId> =
        let ctorParams =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readBoundVarSlot r
                        let ty = readTypeRef r
                        k, ty
                    )
            )

        let args = EqArray.ofArray (readArrayWith r readExprPoolId)
        let chosenCtor = readVOptionWith r readSymbolRef

        {
            CtorParams = ctorParams
            Args = args
            ChosenCtor = chosenCtor
        }

    /// A published template: its identity and parameter attributes, its declaration named
    /// by pool id like any other root.
    and writeInlineTemplate (w: FrozenWriter) (v: PooledInlineValue) =
        writeSymbolRef w v.Key
        writeDeclPoolId w v.Decl
        writeEqArrayWith w writeParamAttrs v.ParamAttrs

    and readInlineTemplate (r: FrozenReader) : PooledInlineValue =
        let key = readSymbolRef r
        let decl = readDeclPoolId r
        let paramAttrs = readEqArrayWith r readParamAttrs

        {
            Key = key
            Decl = decl
            ParamAttrs = paramAttrs
        }

    /// A resolved-specialization entry: the grounding it is keyed by, the file its anchors
    /// index, then its declaration by pool id. The key's type arguments are INTERNED here,
    /// because the `ty` columns never carried them, and so is the origin.
    and writeSpecialization (w: FrozenWriter) (s: PooledSpecialization) =
        writeSymbolRef w s.Key.Template
        writeEqArrayWith w writeTypeRef s.Key.TypeArgs
        writeOriginRef w s.Origin
        writeDeclPoolId w s.Decl

    and readSpecialization (r: FrozenReader) : PooledSpecialization =
        let template = readSymbolRef r
        let typeArgs = EqArray.ofArray (readArrayWith r readTypeRef)
        let origin = readOriginRef r
        let decl = readDeclPoolId r

        {
            Key =
                {
                    Template = template
                    TypeArgs = typeArgs
                }
            Origin = origin
            Decl = decl
        }

    and private writeArgGroup (w: FrozenWriter) (g: ArgGroupG<FrozenType, PatPoolId, BoundVarId>) =
        match g with
        | ArgGroupG.GUnit ty ->
            w.Write 0uy
            writeTypeRef w ty
        | ArgGroupG.GSimple(slot, ty) ->
            w.Write 1uy
            writeBoundVarId w slot
            writeTypeRef w ty
        | ArgGroupG.GTuple pat ->
            w.Write 2uy
            writePatPoolId w pat

    and private readArgGroup (r: FrozenReader) : ArgGroupG<FrozenType, PatPoolId, BoundVarId> =
        match r.ReadByte() with
        | 0uy -> ArgGroupG.GUnit(readTypeRef r)
        | 1uy ->
            let slot = readBoundVarId r
            let ty = readTypeRef r
            ArgGroupG.GSimple(slot, ty)
        | 2uy -> ArgGroupG.GTuple(readPatPoolId r)
        | b -> failwithf "FrozenCodec: unknown ArgGroup tag %d" b

    and writeValRepr (w: FrozenWriter) (v: PooledValRepr) =
        w.Write v.Typars
        writeListWith w writeArgGroup v.Groups
        writeTypeRef w v.ResultTy

    and readValRepr (r: FrozenReader) : PooledValRepr =
        let typars = r.ReadInt32()
        let groups = readListWith r readArgGroup
        let resultTy = readTypeRef r

        {
            Typars = typars
            Groups = groups
            ResultTy = resultTy
        }
