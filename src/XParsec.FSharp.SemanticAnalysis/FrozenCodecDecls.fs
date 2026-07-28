namespace XParsec.FSharp.SemanticAnalysis

open System.IO

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

open XParsec.FSharp.SemanticAnalysis.FrozenCodecPrimitives
open XParsec.FSharp.SemanticAnalysis.FrozenCodecTypes

/// The declaration shell and the scalar clusters a pool payload rides — the shapes whose
/// sub-trees are named by pool id instead of being inlined. Reads `FrozenCodecTypes` (and
/// through it `FrozenCodecPrimitives`) for every type and leaf it embeds; `FrozenCodec`'s
/// payload columns read this module, never the reverse.
module FrozenCodecDecls =

    // ── the declaration shell + the scalar clusters riding a pool payload ───
    //
    // Each writer is followed IMMEDIATELY by its reader. They have to agree field for
    // field, in order, and nothing but review makes them: putting the pair on one screen
    // is the whole of that review. (They were two groups, a couple of hundred lines
    // apart, when a `TTypeMember`'s ten-field emit order had to be checked against a
    // reader you could not see at the same time.)
    //
    // Instantiated at `<FrozenType, SyntaxToken>` — every `'ty` payload rides
    // `writeFrozenType`, every `'tok` rides `writeSyntaxToken` (both defined in the
    // leaf group above). Each writer's `match` is EXHAUSTIVE with no catch-all, so a
    // new case fails to compile here; each reader reconstructs the case / record
    // DIRECTLY (never a normalizing smart constructor) with `let`-sequenced field
    // reads that provably mirror the writer's emit order.
    //
    // There is NO recursive `TExpr`/`TPat`/`TDecl` tree codec any more: every tree in the
    // file is in the pool columns, so an expression is written as an `ExprPoolId` and a
    // pattern as a `PatPoolId` wherever one used to be inlined — a `type` declaration's
    // member bodies, an inline template's decl, a `ValRepr`'s tuple group. What remains
    // here is the declaration SHELL (which the pooled `Type` payload still carries whole)
    // plus the scalar clusters an `ExprPayload` rides (`Disposal`, `CallVia`,
    // `ForInEnumerator`, the static-opt constraints).

    /// The pool ids, written as plain `int`s — the blob is Brotli-compressed at the store
    /// seam, which absorbs their width redundancy far more cheaply than a bespoke varint
    /// would pay for in reader complexity. Defined here rather than with the column codec
    /// below because the declaration shell names its bodies by id.
    let writeExprPoolId (w: BinaryWriter) (ExprPoolId i) = w.Write i
    let readExprPoolId (r: BinaryReader) : ExprPoolId = ExprPoolId(r.ReadInt32())
    let writePatPoolId (w: BinaryWriter) (PatPoolId i) = w.Write i
    let readPatPoolId (r: BinaryReader) : PatPoolId = PatPoolId(r.ReadInt32())
    let writeDeclPoolId (w: BinaryWriter) (DeclPoolId i) = w.Write i
    let readDeclPoolId (r: BinaryReader) : DeclPoolId = DeclPoolId(r.ReadInt32())

    let rec writeDisposal (w: BinaryWriter) (d: Disposal) =
        match d with
        | Disposal.ViaCapability slot ->
            w.Write 0uy
            writeSymbolKey w slot
        | Disposal.ViaOwnMember key ->
            w.Write 1uy
            writeSymbolKey w key
        | Disposal.Unresolved -> w.Write 2uy

    and readDisposal (r: BinaryReader) : Disposal =
        match r.ReadByte() with
        | 0uy -> Disposal.ViaCapability(readSymbolKey r)
        | 1uy -> Disposal.ViaOwnMember(readSymbolKey r)
        | 2uy -> Disposal.Unresolved
        | b -> failwithf "FrozenCodec: unknown Disposal tag %d" b

    and writeCallVia (w: BinaryWriter) (v: CallVia<FrozenType>) =
        match v with
        | CallVia.Self -> w.Write 0uy
        | CallVia.Base -> w.Write 1uy
        | CallVia.Interface ifaceArgs ->
            w.Write 2uy
            writeEqArrayWith w writeFrozenType ifaceArgs

    and readCallVia (r: BinaryReader) : CallVia<FrozenType> =
        match r.ReadByte() with
        | 0uy -> CallVia.Self
        | 1uy -> CallVia.Base
        | 2uy -> CallVia.Interface(EqArray.ofArray (readArrayWith r readFrozenType))
        | b -> failwithf "FrozenCodec: unknown CallVia tag %d" b

    and writeStaticOptConstraint (w: BinaryWriter) (c: Frozen.TStaticOptConstraint) =
        match c with
        | TStaticOptConstraintG.TyconEquals(typar, required) ->
            w.Write 0uy
            writeFrozenType w typar
            writeFrozenType w required
        | TStaticOptConstraintG.IsStruct typar ->
            w.Write 1uy
            writeFrozenType w typar

    and readStaticOptConstraint (r: BinaryReader) : Frozen.TStaticOptConstraint =
        match r.ReadByte() with
        | 0uy ->
            let typar = readFrozenType r
            let required = readFrozenType r
            TStaticOptConstraintG.TyconEquals(typar, required)
        | 1uy -> TStaticOptConstraintG.IsStruct(readFrozenType r)
        | b -> failwithf "FrozenCodec: unknown TStaticOptConstraint tag %d" b

    and writeForInEnumerator (w: BinaryWriter) (e: Frozen.ForInEnumerator) =
        match e with
        | ForInEnumeratorG.Interface -> w.Write 0uy
        | ForInEnumeratorG.Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose) ->
            w.Write 1uy
            writeFrozenType w enumeratorTy
            writeForInGetEnum w getEnumerator
            writeForInEnumMembers w members
            w.Write isValueType
            w.Write dispose

    and readForInEnumerator (r: BinaryReader) : Frozen.ForInEnumerator =
        match r.ReadByte() with
        | 0uy -> ForInEnumeratorG.Interface
        | 1uy ->
            let enumeratorTy = readFrozenType r
            let getEnumerator = readForInGetEnum r
            let members = readForInEnumMembers r
            let isValueType = r.ReadBoolean()
            let dispose = r.ReadBoolean()
            ForInEnumeratorG.Pattern(enumeratorTy, getEnumerator, members, isValueType, dispose)
        | b -> failwithf "FrozenCodec: unknown ForInEnumerator tag %d" b

    and private writeForInGetEnum (w: BinaryWriter) (g: ForInGetEnumG<FrozenType>) =
        match g with
        | ForInGetEnumG.External getEnumerator ->
            w.Write 0uy
            writeSymbolKey w getEnumerator
        | ForInGetEnumG.Local -> w.Write 1uy
        | ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKey w iface
            writeEqArrayWith w writeFrozenType ifaceArgs

    and private readForInGetEnum (r: BinaryReader) : ForInGetEnumG<FrozenType> =
        match r.ReadByte() with
        | 0uy -> ForInGetEnumG.External(readSymbolKey r)
        | 1uy -> ForInGetEnumG.Local
        | 2uy ->
            let iface = readTypeKey r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readFrozenType)
            ForInGetEnumG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInGetEnum tag %d" b

    and private writeForInEnumMembers (w: BinaryWriter) (m: ForInEnumMembersG<FrozenType>) =
        match m with
        | ForInEnumMembersG.External(moveNext, current) ->
            w.Write 0uy
            writeSymbolKey w moveNext
            writeSymbolKey w current
        | ForInEnumMembersG.Local -> w.Write 1uy
        | ForInEnumMembersG.ConstrainedInterface(iface, ifaceArgs) ->
            w.Write 2uy
            writeTypeKey w iface
            writeEqArrayWith w writeFrozenType ifaceArgs

    and private readForInEnumMembers (r: BinaryReader) : ForInEnumMembersG<FrozenType> =
        match r.ReadByte() with
        | 0uy ->
            let moveNext = readSymbolKey r
            let current = readSymbolKey r
            ForInEnumMembersG.External(moveNext, current)
        | 1uy -> ForInEnumMembersG.Local
        | 2uy ->
            let iface = readTypeKey r
            let ifaceArgs = EqArray.ofArray (readArrayWith r readFrozenType)
            ForInEnumMembersG.ConstrainedInterface(iface, ifaceArgs)
        | b -> failwithf "FrozenCodec: unknown ForInEnumMembers tag %d" b

    // The `type` declaration shell — the one declaration shape a pool payload still
    // carries whole (`DeclPayload.Type`). Its member / preamble / ctor bodies are pool
    // ids, so this group bottoms out at `writeExprPoolId` where it once recursed into
    // `writeExpr`.

    and writeTypeDecl (w: BinaryWriter) (td: PooledTypeDecl) =
        w.Write td.Name
        writeTypeKey w td.TypeKey
        writeOptionWith w (fun w (s: string) -> w.Write s) td.Namespace
        writeStringArray w td.TypeParams
        w.Write td.IsRequireQualifiedAccess
        writeTypeKind w td.Kind
        writeEqualityVerdict w td.EqualitySupport
        writeComparisonVerdict w td.ComparisonSupport

    and readTypeDecl (r: BinaryReader) : PooledTypeDecl =
        let name = r.ReadString()
        let typeKey = readTypeKey r
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

    and private writeTypeKind (w: BinaryWriter) (k: TTypeKindG<FrozenType, SyntaxToken, BinderId, ExprPoolId>) =
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

    and private readTypeKind (r: BinaryReader) : TTypeKindG<FrozenType, SyntaxToken, BinderId, ExprPoolId> =
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
    // bodies — shared by the class / union / record arms.
    and private writeInterfaces
        (w: BinaryWriter)
        (interfaces: EqArray<FrozenType * EqArray<TTypeMemberG<FrozenType, BinderId, ExprPoolId>>>)
        =
        writeEqArrayWith
            w
            (fun w (ty, mems) ->
                writeFrozenType w ty
                writeEqArrayWith w writeTypeMember mems
            )
            interfaces

    and private readInterfaces
        (r: BinaryReader)
        : EqArray<FrozenType * EqArray<TTypeMemberG<FrozenType, BinderId, ExprPoolId>>> =
        EqArray.ofArray (
            readArrayWith
                r
                (fun r ->
                    let ty = readFrozenType r
                    let mems = EqArray.ofArray (readArrayWith r readTypeMember)
                    ty, mems
                )
        )

    and private writeClass (w: BinaryWriter) (c: TClassG<FrozenType, BinderId, ExprPoolId>) =
        writeEqArrayWith w writeRecordField c.Fields
        writeEqArrayWith w writeRecordField c.CtorParams
        writeEqArrayWith w writeTypeMember c.Members
        writeVOptionWith w writeFrozenType c.BaseType
        writeInterfaces w c.Interfaces
        w.Write c.IsSealed
        writeEqArrayWith w writePreambleEntry c.StaticPreamble
        writeEqArrayWith w writePreambleEntry c.InstancePreamble
        writeBinderId w c.ThisKey
        writeEqArrayWith w writeSecondaryCtor c.SecondaryCtors
        writeVOptionWith w writeBaseCtorCall c.BaseCtorCall
        writeClassValueKind w c.ValueKind
        w.Write c.HasPrimaryCtor

    and private readClass (r: BinaryReader) : TClassG<FrozenType, BinderId, ExprPoolId> =
        let fields = EqArray.ofArray (readArrayWith r readRecordField)
        let ctorParams = EqArray.ofArray (readArrayWith r readRecordField)
        let members = EqArray.ofArray (readArrayWith r readTypeMember)
        let baseType = readVOptionWith r readFrozenType
        let interfaces = readInterfaces r
        let isSealed = r.ReadBoolean()
        let staticPreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let instancePreamble = EqArray.ofArray (readArrayWith r readPreambleEntry)
        let thisKey = readBinderId r
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
            IsSealed = isSealed
            StaticPreamble = staticPreamble
            InstancePreamble = instancePreamble
            ThisKey = thisKey
            SecondaryCtors = secondaryCtors
            BaseCtorCall = baseCtorCall
            ValueKind = valueKind
            HasPrimaryCtor = hasPrimaryCtor
        }

    and private writeTypeMember (w: BinaryWriter) (m: TTypeMemberG<FrozenType, BinderId, ExprPoolId>) =
        w.Write m.Name
        w.Write m.IsStatic
        writeAccessibility w m.Accessibility
        writeTMemberKind w m.Kind
        w.Write m.IsOverride
        writeVOptionWith w writeBinderId m.ThisKey
        writeVOptionWith w writeBinderId m.BaseKey
        writeFrozenType w m.ThisTy

        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBinderId w k
                writeFrozenType w ty
            )
            m.Params

        writeExprPoolId w m.Body
        writeFrozenType w m.ReturnTy
        writeMethodTypeParams w m.MethodTypeParams

    and private readTypeMember (r: BinaryReader) : TTypeMemberG<FrozenType, BinderId, ExprPoolId> =
        let name = r.ReadString()
        let isStatic = r.ReadBoolean()
        let accessibility = readAccessibility r
        let kind = readTMemberKind r
        let isOverride = r.ReadBoolean()
        let thisKey = readVOptionWith r readBinderId
        let baseKey = readVOptionWith r readBinderId
        let thisTy = readFrozenType r

        let parameters =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readBinderId r
                        let ty = readFrozenType r
                        k, ty
                    )
            )

        let body = readExprPoolId r
        let returnTy = readFrozenType r
        let methodTypeParams = readMethodTypeParams r

        {
            Name = name
            IsStatic = isStatic
            Accessibility = accessibility
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

    and private writeClassLet (w: BinaryWriter) (l: TClassLetG<FrozenType, ExprPoolId>) =
        w.Write l.Name
        writeFrozenType w l.Type
        w.Write l.IsMutable
        writeExprPoolId w l.Init

    and private readClassLet (r: BinaryReader) : TClassLetG<FrozenType, ExprPoolId> =
        let name = r.ReadString()
        let ty = readFrozenType r
        let isMutable = r.ReadBoolean()
        let init = readExprPoolId r

        {
            Name = name
            Type = ty
            IsMutable = isMutable
            Init = init
        }

    and private writePreambleEntry (w: BinaryWriter) (p: TPreambleEntryG<FrozenType, ExprPoolId>) =
        match p with
        | TPreambleEntryG.Let l ->
            w.Write 0uy
            writeClassLet w l
        | TPreambleEntryG.Do e ->
            w.Write 1uy
            writeExprPoolId w e

    and private readPreambleEntry (r: BinaryReader) : TPreambleEntryG<FrozenType, ExprPoolId> =
        match r.ReadByte() with
        | 0uy -> TPreambleEntryG.Let(readClassLet r)
        | 1uy -> TPreambleEntryG.Do(readExprPoolId r)
        | b -> failwithf "FrozenCodec: unknown TPreambleEntry tag %d" b

    and private writeCtorLet (w: BinaryWriter) (cl: TCtorLetG<FrozenType, BinderId, ExprPoolId>) =
        writeBinderId w cl.Binder
        writeFrozenType w cl.Type
        writeExprPoolId w cl.Init

    and private readCtorLet (r: BinaryReader) : TCtorLetG<FrozenType, BinderId, ExprPoolId> =
        let binder = readBinderId r
        let ty = readFrozenType r
        let init = readExprPoolId r

        {
            Binder = binder
            Type = ty
            Init = init
        }

    and private writeCtorFieldInit (w: BinaryWriter) (fi: TCtorFieldInitG<ExprPoolId>) =
        w.Write fi.Field
        writeExprPoolId w fi.Init

    and private readCtorFieldInit (r: BinaryReader) : TCtorFieldInitG<ExprPoolId> =
        let field = r.ReadString()
        let init = readExprPoolId r
        { Field = field; Init = init }

    and private writeSecondaryCtor (w: BinaryWriter) (sc: TSecondaryCtorG<FrozenType, BinderId, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBinderId w k
                writeFrozenType w ty
            )
            sc.Params

        writeEqArrayWith w writeCtorLet sc.Lets
        writeEqArrayWith w writeExprPoolId sc.PrimaryArgs
        writeEqArrayWith w writeCtorFieldInit sc.FieldInits

    and private readSecondaryCtor (r: BinaryReader) : TSecondaryCtorG<FrozenType, BinderId, ExprPoolId> =
        let parameters =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readBinderId r
                        let ty = readFrozenType r
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

    and private writeBaseCtorCall (w: BinaryWriter) (bc: TBaseCtorCallG<FrozenType, BinderId, ExprPoolId>) =
        writeEqArrayWith
            w
            (fun w (k, ty) ->
                writeBinderId w k
                writeFrozenType w ty
            )
            bc.CtorParams

        writeEqArrayWith w writeExprPoolId bc.Args
        writeVOptionWith w writeSymbolKey bc.ChosenCtor

    and private readBaseCtorCall (r: BinaryReader) : TBaseCtorCallG<FrozenType, BinderId, ExprPoolId> =
        let ctorParams =
            EqArray.ofArray (
                readArrayWith
                    r
                    (fun r ->
                        let k = readBinderId r
                        let ty = readFrozenType r
                        k, ty
                    )
            )

        let args = EqArray.ofArray (readArrayWith r readExprPoolId)
        let chosenCtor = readVOptionWith r readSymbolKey

        {
            CtorParams = ctorParams
            Args = args
            ChosenCtor = chosenCtor
        }

    /// A published template: its identity and parameter attributes, its declaration named
    /// by pool id like any other root.
    and writeInlineTemplate (w: BinaryWriter) (v: PooledInlineValue) =
        writeSymbolKey w v.Key
        writeDeclPoolId w v.Decl
        writeArrayWith w writeParamAttrs v.ParamAttrs

    and readInlineTemplate (r: BinaryReader) : PooledInlineValue =
        let key = readSymbolKey r
        let decl = readDeclPoolId r
        let paramAttrs = readArrayWith r readParamAttrs

        {
            Key = key
            Decl = decl
            ParamAttrs = paramAttrs
        }

    and private writeArgGroup (w: BinaryWriter) (g: ArgGroupG<FrozenType, PatPoolId>) =
        match g with
        | ArgGroupG.GUnit ty ->
            w.Write 0uy
            writeFrozenType w ty
        | ArgGroupG.GSimple(slot, ty) ->
            w.Write 1uy
            writeNodeKey w slot
            writeFrozenType w ty
        | ArgGroupG.GTuple pat ->
            w.Write 2uy
            writePatPoolId w pat

    and private readArgGroup (r: BinaryReader) : ArgGroupG<FrozenType, PatPoolId> =
        match r.ReadByte() with
        | 0uy -> ArgGroupG.GUnit(readFrozenType r)
        | 1uy ->
            let slot = readNodeKey r
            let ty = readFrozenType r
            ArgGroupG.GSimple(slot, ty)
        | 2uy -> ArgGroupG.GTuple(readPatPoolId r)
        | b -> failwithf "FrozenCodec: unknown ArgGroup tag %d" b

    and writeValRepr (w: BinaryWriter) (v: PooledValRepr) =
        w.Write v.Typars
        writeListWith w writeArgGroup v.Groups
        writeFrozenType w v.ResultTy

    and readValRepr (r: BinaryReader) : PooledValRepr =
        let typars = r.ReadInt32()
        let groups = readListWith r readArgGroup
        let resultTy = readFrozenType r

        {
            Typars = typars
            Groups = groups
            ResultTy = resultTy
        }
