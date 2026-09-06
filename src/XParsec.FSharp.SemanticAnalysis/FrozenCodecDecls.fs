namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecTypes

/// The declaration shell and the scalar clusters carried on a pool payload, the shapes whose
/// sub-trees are identified by pool id instead of being inlined: a `type` declaration's member
/// bodies, an inline template's decl, a `ValRepr`'s tuple group.
module FrozenCodecDecls =

    // ── the declaration shell + the scalar clusters carried on a pool payload ───
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

    let writeDisposal (w: FrozenWriter) (d: Disposal) =
        match d with
        | Disposal.ViaCapability slot ->
            w.Write 0uy
            writeSymbolRef w slot
        | Disposal.ViaOwnMember key ->
            w.Write 1uy
            writeSymbolRef w key
        | Disposal.Unresolved -> w.Write 2uy

    let readDisposal (r: FrozenReader) : Disposal =
        match r.ReadByte() with
        | 0uy -> Disposal.ViaCapability(readSymbolRef r)
        | 1uy -> Disposal.ViaOwnMember(readSymbolRef r)
        | 2uy -> Disposal.Unresolved
        | b -> failwithf "FrozenCodec: unknown Disposal tag %d" b

    let writeRecursion (w: FrozenWriter) (r: Recursion) =
        match r with
        | Recursion.NonRecursive -> w.Write 0uy
        | Recursion.Recursive -> w.Write 1uy
        | Recursion.TailRecursive -> w.Write 2uy

    let readRecursion (r: FrozenReader) : Recursion =
        match r.ReadByte() with
        | 0uy -> Recursion.NonRecursive
        | 1uy -> Recursion.Recursive
        | 2uy -> Recursion.TailRecursive
        | b -> failwithf "FrozenCodec: unknown Recursion tag %d" b

    let writeAppKind (w: FrozenWriter) (k: AppKind) =
        match k with
        | AppKind.Call -> w.Write 0uy
        | AppKind.TailSelfCall -> w.Write 1uy

    let readAppKind (r: FrozenReader) : AppKind =
        match r.ReadByte() with
        | 0uy -> AppKind.Call
        | 1uy -> AppKind.TailSelfCall
        | b -> failwithf "FrozenCodec: unknown AppKind tag %d" b

    let writeCallVia (w: FrozenWriter) (v: CallVia<FrozenType>) =
        match v with
        | CallVia.Self -> w.Write 0uy
        | CallVia.Base -> w.Write 1uy
        | CallVia.Interface ifaceArgs ->
            w.Write 2uy
            writeEqArrayWith w writeTypeRef ifaceArgs

    let readCallVia (r: FrozenReader) : CallVia<FrozenType> =
        match r.ReadByte() with
        | 0uy -> CallVia.Self
        | 1uy -> CallVia.Base
        | 2uy -> CallVia.Interface(EqArray.ofArray (readArrayWith r readTypeRef))
        | b -> failwithf "FrozenCodec: unknown CallVia tag %d" b

    let writeStaticOptConstraint (w: FrozenWriter) (c: Frozen.TStaticOptConstraint) =
        match c with
        | TStaticOptConstraintG.TyconEquals(typar, required) ->
            w.Write 0uy
            writeTypeRef w typar
            writeTypeRef w required
        | TStaticOptConstraintG.IsStruct typar ->
            w.Write 1uy
            writeTypeRef w typar

    let readStaticOptConstraint (r: FrozenReader) : Frozen.TStaticOptConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typar = readTypeRef r
            let required = readTypeRef r
            TStaticOptConstraintG.TyconEquals(typar, required)
        | 1uy -> TStaticOptConstraintG.IsStruct(readTypeRef r)
        | b -> failwithf "FrozenCodec: unknown TStaticOptConstraint tag %d" b

    let private writeForInGetEnum (w: FrozenWriter) (g: Frozen.ForInGetEnum) =
        match g with
        | ForInGetEnumG.External getEnumerator ->
            w.Write 0uy
            writeSymbolRef w getEnumerator
        | ForInGetEnumG.Local -> w.Write 1uy
        | ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKeyRef w iface
            writeEqArrayWith w writeTypeRef ifaceArgs

    let private readForInGetEnum (r: FrozenReader) : Frozen.ForInGetEnum =
        match r.ReadByte() with
        | 0uy -> ForInGetEnumG.External(readSymbolRef r)
        | 1uy -> ForInGetEnumG.Local
        | 2uy ->
            let iface = readTypeKeyRef r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readTypeRef)
            ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInGetEnum tag %d" b

    let private writeForInEnumMembers (w: FrozenWriter) (m: Frozen.ForInEnumMembers) =
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

    let writeForInEnumerator (w: FrozenWriter) (e: Frozen.ForInEnumerator) =
        match e with
        | ForInEnumeratorG.Interface -> w.Write 0uy
        | ForInEnumeratorG.Pattern p ->
            w.Write 1uy
            writeTypeRef w p.EnumeratorTy
            writeForInGetEnum w p.GetEnumerator
            writeForInEnumMembers w p.Members
            w.Write p.IsValueType
            w.Write p.Dispose

    let private readForInEnumMembers (r: FrozenReader) : Frozen.ForInEnumMembers =
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

    let readForInEnumerator (r: FrozenReader) : Frozen.ForInEnumerator =
        match r.ReadByte() with
        | 0uy -> ForInEnumeratorG.Interface
        | 1uy ->
            let enumeratorTy = readTypeRef r
            let getEnumerator = readForInGetEnum r
            let members = readForInEnumMembers r
            let isValueType = r.ReadBoolean()
            let dispose = r.ReadBoolean()

            ForInEnumeratorG.Pattern
                {
                    EnumeratorTy = enumeratorTy
                    GetEnumerator = getEnumerator
                    Members = members
                    IsValueType = isValueType
                    Dispose = dispose
                }
        | b -> failwithf "FrozenCodec: unknown ForInEnumerator tag %d" b

    let private writeDeclaredFlags (w: FrozenWriter) (d: DeclaredClassFlags) =
        w.Write d.IsSealed
        w.Write d.IsAbstract
        w.Write d.AllowNullLiteral

    let private readDeclaredFlags (r: FrozenReader) : DeclaredClassFlags =
        let isSealed = r.ReadBoolean()
        let isAbstract = r.ReadBoolean()
        let allowNullLiteral = r.ReadBoolean()

        {
            IsSealed = isSealed
            IsAbstract = isAbstract
            AllowNullLiteral = allowNullLiteral
        }

    let private writeTypeMember (w: FrozenWriter) (m: TTypeMemberG<FrozenType, BoundVarId, ExprPoolId>) =
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
        writeEqSetWith w writeFrozenConstraint m.MethodTyparConstraints
        writeTAttributes w m.Attributes

    // Each `interfaces` entry pairs a resolved interface type with its typed member
    // bodies. Shared by the class / union / record arms.
    let private writeInterfaces
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

    let private readTypeMember (r: FrozenReader) : TTypeMemberG<FrozenType, BoundVarId, ExprPoolId> =
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
        let methodTyparConstraints = readEqSetWith r readFrozenConstraint
        let attributes = readTAttributes r

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
            MethodTyparConstraints = methodTyparConstraints
            Attributes = attributes
        }

    let private readInterfaces
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

    let private writeClassLet (w: FrozenWriter) (l: TClassLetG<FrozenType, ExprPoolId>) =
        w.Write l.Name
        writeTypeRef w l.Type
        w.Write l.IsMutable
        writeExprPoolId w l.Init

    let private readClassLet (r: FrozenReader) : TClassLetG<FrozenType, ExprPoolId> =
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

    let private writePreambleEntry (w: FrozenWriter) (p: TPreambleEntryG<FrozenType, ExprPoolId>) =
        match p with
        | TPreambleEntryG.Let l ->
            w.Write 0uy
            writeClassLet w l
        | TPreambleEntryG.Do e ->
            w.Write 1uy
            writeExprPoolId w e

    let private readPreambleEntry (r: FrozenReader) : TPreambleEntryG<FrozenType, ExprPoolId> =
        match r.ReadByte() with
        | 0uy -> TPreambleEntryG.Let(readClassLet r)
        | 1uy -> TPreambleEntryG.Do(readExprPoolId r)
        | b -> failwithf "FrozenCodec: unknown TPreambleEntry tag %d" b

    let private writeCtorLet (w: FrozenWriter) (cl: TCtorLetG<FrozenType, BoundVarId, ExprPoolId>) =
        writeBoundVarSlot w cl.BoundVar
        writeTypeRef w cl.Type
        writeExprPoolId w cl.Init

    let private readCtorLet (r: FrozenReader) : TCtorLetG<FrozenType, BoundVarId, ExprPoolId> =
        let boundVar = readBoundVarSlot r
        let ty = readTypeRef r
        let init = readExprPoolId r

        {
            BoundVar = boundVar
            Type = ty
            Init = init
        }

    let private writeCtorFieldInit (w: FrozenWriter) (fi: TCtorFieldInitG<ExprPoolId>) =
        w.Write fi.Field
        writeExprPoolId w fi.Init

    let private readCtorFieldInit (r: FrozenReader) : TCtorFieldInitG<ExprPoolId> =
        let field = r.ReadString()
        let init = readExprPoolId r
        { Field = field; Init = init }

    let private writeSecondaryCtor (w: FrozenWriter) (sc: TSecondaryCtorG<FrozenType, BoundVarId, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBoundVarSlot w k
                writeTypeRef w ty
            )
            sc.Params

        writeEqArrayWith w writeCtorLet sc.Lets

        match sc.Body with
        | TSecondaryCtorBodyG.Chain primaryArgs ->
            w.Write 0uy
            writeEqArrayWith w writeExprPoolId primaryArgs
        | TSecondaryCtorBodyG.ExplicitFieldInit fieldInits ->
            w.Write 1uy
            writeEqArrayWith w writeCtorFieldInit fieldInits

    let private readSecondaryCtor (r: FrozenReader) : TSecondaryCtorG<FrozenType, BoundVarId, ExprPoolId> =
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

        let body =
            match r.ReadByte() with
            | 0uy -> TSecondaryCtorBodyG.Chain(EqArray.ofArray (readArrayWith r readExprPoolId))
            | 1uy -> TSecondaryCtorBodyG.ExplicitFieldInit(EqArray.ofArray (readArrayWith r readCtorFieldInit))
            | b -> failwithf "FrozenCodec: unknown TSecondaryCtorBody tag %d" b

        {
            Params = parameters
            Lets = lets
            Body = body
        }

    let private writeBaseCtorCall (w: FrozenWriter) (bc: TBaseCtorCallG<FrozenType, BoundVarId, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBoundVarSlot w k
                writeTypeRef w ty
            )
            bc.CtorParams

        writeEqArrayWith w writeExprPoolId bc.Args
        writeVOptionWith w writeSymbolRef bc.ChosenCtor

    let private writeBase (w: FrozenWriter) (b: TBaseG<FrozenType, BoundVarId, ExprPoolId>) =
        (match b.Parent with
         | BaseParentG.Class _ -> w.Write 0uy
         | BaseParentG.PrimitiveCanon _ -> w.Write 1uy)

        writeTypeKeyRef w b.Parent.Key
        writeEqArrayWith w writeTypeRef b.Parent.Args
        writeVOptionWith w writeBaseCtorCall b.Ctor

    let private writeClass (w: FrozenWriter) (c: TClassG<FrozenType, BoundVarId, ExprPoolId>) =
        writeEqArrayWith w writeRecordField c.Fields
        writeEqArrayWith w writeRecordField c.CtorParams
        writeEqArrayWith w writeTypeMember c.Members
        writeVOptionWith w writeBase c.Base
        writeInterfaces w c.Interfaces
        writeDeclaredFlags w c.Declared
        writeEqArrayWith w writePreambleEntry c.StaticPreamble
        writeEqArrayWith w writePreambleEntry c.InstancePreamble
        writeBoundVarSlot w c.ThisKey
        writeEqArrayWith w writeSecondaryCtor c.SecondaryCtors
        writeClassValueKind w c.ValueKind
        w.Write c.HasPrimaryCtor

    let private writeTypeKind (w: FrozenWriter) (k: TTypeKindG<FrozenType, Anchor, BoundVarId, ExprPoolId>) =
        match k with
        | TTypeKindG.Interface methods ->
            w.Write 0uy
            writeEqArrayWith w writeAbstractMethod methods
        | TTypeKindG.Union u ->
            w.Write 1uy
            writeEqArrayWith w writeUnionCase u.Cases
            writeEqArrayWith w writeTypeMember u.Members
            writeInterfaces w u.Interfaces
            writeNominalValueKind w u.ValueKind
        | TTypeKindG.Record rec' ->
            w.Write 2uy
            writeEqArrayWith w writeRecordField rec'.Fields
            writeEqArrayWith w writeTypeMember rec'.Members
            writeInterfaces w rec'.Interfaces
            writeNominalValueKind w rec'.ValueKind
        | TTypeKindG.Class c ->
            w.Write 3uy
            writeClass w c
        | TTypeKindG.Enum cases ->
            w.Write 4uy
            writeEqArrayWith w writeEnumCase cases
        | TTypeKindG.Abbrev body ->
            w.Write 5uy
            writeTypeRef w body
        | TTypeKindG.Measure term ->
            w.Write 6uy
            writeMeasureTerm w term

    let private writeTypeParams (w: FrozenWriter) (ps: EqArray<TTypeParam>) =
        writeEqArrayWith
            w
            (fun w (p: TTypeParam) ->
                w.Write p.Name
                writeTyparKind w p.Kind
            )
            ps

    let private readTypeParams (r: FrozenReader) : EqArray<TTypeParam> =
        EqArray.ofArray (
            readArrayWith
                r
                (fun r ->
                    let name = r.ReadString()
                    let kind = readTyparKind r

                    { Name = name; Kind = kind }: TTypeParam
                )
        )

    let writeTypeDecl (w: FrozenWriter) (td: PooledTypeDecl) =
        w.Write td.Name
        writeTypeKeyRef w td.TypeKey
        writeTypeParams w td.TypeParams
        writeEqSetWith w writeFrozenConstraint td.TyparConstraints
        writeTypeKind w td.Kind
        writeTAttributes w td.Attributes

    let private readBaseCtorCall (r: FrozenReader) : TBaseCtorCallG<FrozenType, BoundVarId, ExprPoolId> =
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

    let rec private readBase (r: FrozenReader) : TBaseG<FrozenType, BoundVarId, ExprPoolId> =
        let tag = r.ReadByte()
        let key = readTypeKeyRef r
        let args = EqArray.ofArray (readArrayWith r readTypeRef)

        let parent =
            match tag with
            | 0uy -> BaseParentG.Class(NominalG.ofClass key args)
            | 1uy -> BaseParentG.PrimitiveCanon(NominalG.ofConst key args)
            | b -> failwithf "readBase: unknown base-parent tag %d" b

        let ctor = readVOptionWith r readBaseCtorCall

        { Parent = parent; Ctor = ctor }

    let private readClass (r: FrozenReader) : TClassG<FrozenType, BoundVarId, ExprPoolId> =
        let fields = EqArray.ofArray (readArrayWith r readRecordField)
        let ctorParams = EqArray.ofArray (readArrayWith r readRecordField)
        let members = EqArray.ofArray (readArrayWith r readTypeMember)
        let baseNode = readVOptionWith r readBase
        let interfaces = readInterfaces r
        let declared = readDeclaredFlags r
        let staticPreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let instancePreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let thisKey = readBoundVarSlot r
        let secondaryCtors = EqArray.ofArray (readArrayWith r readSecondaryCtor)
        let valueKind = readClassValueKind r
        let hasPrimaryCtor = r.ReadBoolean()

        {
            Fields = fields
            CtorParams = ctorParams
            Members = members
            Base = baseNode
            Interfaces = interfaces
            Declared = declared
            StaticPreamble = staticPreamble
            InstancePreamble = instancePreamble
            ThisKey = thisKey
            SecondaryCtors = secondaryCtors
            ValueKind = valueKind
            HasPrimaryCtor = hasPrimaryCtor
        }

    let private readTypeKind (r: FrozenReader) : TTypeKindG<FrozenType, Anchor, BoundVarId, ExprPoolId> =
        match r.ReadByte() with
        | 0uy -> TTypeKindG.Interface(EqArray.ofArray (readArrayWith r readAbstractMethod))
        | 1uy ->
            let cases = EqArray.ofArray (readArrayWith r readUnionCase)
            let members = EqArray.ofArray (readArrayWith r readTypeMember)
            let interfaces = readInterfaces r
            let valueKind = readNominalValueKind r

            TTypeKindG.Union
                {
                    Cases = cases
                    Members = members
                    Interfaces = interfaces
                    ValueKind = valueKind
                }
        | 2uy ->
            let fields = EqArray.ofArray (readArrayWith r readRecordField)
            let members = EqArray.ofArray (readArrayWith r readTypeMember)
            let interfaces = readInterfaces r
            let valueKind = readNominalValueKind r

            TTypeKindG.Record
                {
                    Fields = fields
                    Members = members
                    Interfaces = interfaces
                    ValueKind = valueKind
                }
        | 3uy -> TTypeKindG.Class(readClass r)
        | 4uy -> TTypeKindG.Enum(EqArray.ofArray (readArrayWith r readEnumCase))
        | 5uy -> TTypeKindG.Abbrev(readTypeRef r)
        | 6uy -> TTypeKindG.Measure(readMeasureTerm r)
        | b -> failwithf "FrozenCodec: unknown TTypeKind tag %d" b

    let readTypeDecl (r: FrozenReader) : PooledTypeDecl =
        let name = r.ReadString()
        let typeKey = readTypeKeyRef r
        let typeParams = readTypeParams r
        let typarConstraints = readEqSetWith r readFrozenConstraint
        let kind = readTypeKind r
        let attributes = readTAttributes r

        {
            Name = name
            TypeKey = typeKey
            TypeParams = typeParams
            TyparConstraints = typarConstraints
            Kind = kind
            Attributes = attributes
        }

    /// A published template: its identity and parameter attributes, its declaration named
    /// by pool id like any other root.
    let writeInlineTemplate (w: FrozenWriter) (v: PooledInlineValue) =
        writeSymbolRef w v.Key
        writeDeclPoolId w v.Decl
        writeEqArrayWith w writeParamAttrs v.ParamAttrs

    let readInlineTemplate (r: FrozenReader) : PooledInlineValue =
        let key = readSymbolRef r
        let decl = readDeclPoolId r
        let paramAttrs = readEqArrayWith r readParamAttrs

        {
            Key = key
            Decl = decl
            ParamAttrs = paramAttrs
        }

    /// A resolved-specialization entry: the grounding it is keyed by, the file its anchors
    /// index, then its bound pattern and abstraction by pool id. The key's type arguments are
    /// INTERNED here, because the `ty` columns never carried them, and so is the origin.
    let writeSpecialization (w: FrozenWriter) (s: PooledSpecialization) =
        writeSymbolRef w s.Key.Template
        writeEqArrayWith w writeTypeRef s.Key.TypeArgs
        writeFilePathRef w s.Path
        writePatPoolId w s.Pat
        writeExprPoolId w s.Value

    let readSpecialization (r: FrozenReader) : PooledSpecialization =
        let template = readSymbolRef r
        let typeArgs = EqArray.ofArray (readArrayWith r readTypeRef)
        let origin = readFilePathRef r
        let pat = readPatPoolId r
        let value = readExprPoolId r

        {
            Key =
                {
                    Template = template
                    TypeArgs = typeArgs
                }
            Path = origin
            Pat = pat
            Value = value
        }

    let private writeArgGroup (w: FrozenWriter) (g: ArgGroupG<FrozenType, PatPoolId, BoundVarId>) =
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

    let private readArgGroup (r: FrozenReader) : ArgGroupG<FrozenType, PatPoolId, BoundVarId> =
        match r.ReadByte() with
        | 0uy -> ArgGroupG.GUnit(readTypeRef r)
        | 1uy ->
            let slot = readBoundVarId r
            let ty = readTypeRef r
            ArgGroupG.GSimple(slot, ty)
        | 2uy -> ArgGroupG.GTuple(readPatPoolId r)
        | b -> failwithf "FrozenCodec: unknown ArgGroup tag %d" b

    let writeValRepr (w: FrozenWriter) (v: PooledValRepr) =
        w.Write v.Typars
        writeListWith w writeArgGroup v.Groups
        writeTypeRef w v.ResultTy

    let readValRepr (r: FrozenReader) : PooledValRepr =
        let typars = r.ReadInt32()
        let groups = readListWith r readArgGroup
        let resultTy = readTypeRef r

        {
            Typars = typars
            Groups = groups
            ResultTy = resultTy
        }
