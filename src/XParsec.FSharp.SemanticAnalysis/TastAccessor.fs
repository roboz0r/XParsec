namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// A payload view is written as a recognizer guarding on the payload column, never on the
// shape tag; its eager form is `expect` of that recognizer.

/// The TAST-shaped accessor, qualified so `exprTy`/`patTy` do not collide with the
/// analysis-time `TastWalk` projections of the same name.
[<RequireQualifiedAccess>]
module TastAccessor =

    // Every shape an accessor below yields, re-exported so a consumer references one module for
    // both the reader and what it hands back.
    type ExprId = TastNodeViews.ExprId
    type PatId = TastNodeViews.PatId
    type DeclId = TastNodeViews.DeclId
    type TypeDecl = TastNodeViews.TypeDecl
    type TypeKind = TastNodeViews.TypeKind
    type Class = TastNodeViews.Class
    type TypeMember = TastNodeViews.TypeMember
    type ClassLet = TastNodeViews.ClassLet
    type PreambleEntry = TastNodeViews.PreambleEntry
    type CtorLet = TastNodeViews.CtorLet
    type CtorFieldInit = TastNodeViews.CtorFieldInit
    type SecondaryCtor = TastNodeViews.SecondaryCtor
    type Base = TastNodeViews.Base
    type BaseCtorCall = TastNodeViews.BaseCtorCall
    type StaticParam = TastNodeViews.StaticParam
    type ArgGroup = TastNodeViews.ArgGroup
    type ValRepr = TastNodeViews.ValRepr
    type CompiledReturn = TastNodeViews.CompiledReturn
    type CompiledForm = TastNodeViews.CompiledForm
    type ExternalMemberView = TastNodeViews.ExternalMemberView
    type ILIntrinsicView = TastNodeViews.ILIntrinsicView
    type LambdaView = TastNodeViews.LambdaView
    type LetView = TastNodeViews.LetView
    type LetMemberView = TastNodeViews.LetMemberView
    type LetGroupView = TastNodeViews.LetGroupView
    type AssignmentView = TastNodeViews.AssignmentView
    type IfThenElseView = TastNodeViews.IfThenElseView
    type AppView = TastNodeViews.AppView
    type AppliedArg = TastNodeViews.AppliedArg
    type RecordCloneView = TastNodeViews.RecordCloneView
    type FieldGetView = TastNodeViews.FieldGetView
    type FieldSetView = TastNodeViews.FieldSetView
    type NewView = TastNodeViews.NewView
    type PropertyGetView = TastNodeViews.PropertyGetView
    type MethodCallView = TastNodeViews.MethodCallView
    type StaticFieldGetView = TastNodeViews.StaticFieldGetView
    type StaticFieldSetView = TastNodeViews.StaticFieldSetView
    type InlineCallView = TastNodeViews.InlineCallView
    type Arm = TastNodeViews.Arm
    type MatchView = TastNodeViews.MatchView
    type TryWithView = TastNodeViews.TryWithView
    type TryFinallyView = TastNodeViews.TryFinallyView
    type WhileView = TastNodeViews.WhileView
    type ForToView = TastNodeViews.ForToView
    type ForInView = TastNodeViews.ForInView
    type UseView = TastNodeViews.UseView
    type FormatSink = TastNodeViews.FormatSink
    type FormatSeg = TastNodeViews.FormatSeg
    type DynFormatHole = TastNodeViews.DynFormatHole
    type FormatView = TastNodeViews.FormatView
    type EnumCasePatView = TastNodeViews.EnumCasePatView
    type DeclLetView = TastNodeViews.DeclLetView
    type DeclLetGroupView = TastNodeViews.DeclLetGroupView
    type Specialization = TastNodeViews.Specialization

    /// A sibling id in the same pool: every child edge resolves through this, so the pool
    /// propagates down a walk without any consumer referencing it.
    let inline private at (h: Handle<'a>) (id: 'b) : Handle<'b> = { Pool = h.Pool; Id = id }

    /// The EAGER form of a view that has a recognizer: project through it, or fault with
    /// `what`. `inline` so the call is still one column read and one match.
    let inline private expect (what: string) ([<InlineIfLambda>] recog: 'n -> 'v voption) (n: 'n) : 'v =
        match recog n with
        | ValueSome v -> v
        | ValueNone -> failwith what

    // ── expressions ─────────────────────────────────────────────────────────

    let exprKind (e: ExprId) : ExprShape = TastPoolBuilder.exprShape e.Pool e.Id

    let exprTy (e: ExprId) : FrozenType = TastPoolBuilder.exprTy e.Pool e.Id

    /// The node's type where the construct emits that type ITSELF: a record/union
    /// construction, a `new`, a member access's object argument.
    let exprNominalTy (e: ExprId) : FrozenNominal =
        let ty = exprTy e

        match FrozenNominal.tryOfFrozen ty with
        | ValueSome n -> n
        | ValueNone -> failwithf "a %A expression does not denote a type constructor: %A" (exprKind e) ty

    /// Where the node SITS: its token's index in the file's `Lexed`, or `Anchor.nowhere`
    /// where no source spells it (a minted node, a contract's rebuilt pattern).
    let exprTok (e: ExprId) : Anchor = TastPoolBuilder.exprTok e.Pool e.Id

    /// The immediate child *expressions*, in evaluation order. Sub-patterns are NOT among
    /// them; the sub-expressions of composite carriers (match arms, format segments,
    /// static-opt clauses) are flattened in, each appearing exactly once.
    let exprChildren (e: ExprId) : ExprId[] =
        TastPoolBuilder.exprChildren e.Pool e.Id |> Array.map (at e)

    /// The immediate child *patterns* an expression owns directly, in source order. `ForTo`'s
    /// loop variable is a `BoundVarId`, so it is not among them.
    let exprPatChildren (e: ExprId) : PatId[] =
        TastPoolBuilder.exprPatChildren e.Pool e.Id |> Array.map (at e)

    // ONE child by position, without the fresh handle array `exprChildren` builds per call.

    let exprChildCount (e: ExprId) : int =
        TastPoolBuilder.exprChildCount e.Pool e.Id

    let exprChild (e: ExprId) (i: int) : ExprId =
        at e (TastPoolBuilder.exprChild e.Pool e.Id i)

    let exprPatChild (e: ExprId) (i: int) : PatId =
        at e (TastPoolBuilder.exprPatChild e.Pool e.Id i)

    let private payload (e: ExprId) : ExprPayload = TastPoolBuilder.exprPayload e.Pool e.Id

    [<return: Struct>]
    let private (|EConst|_|) (e: ExprId) : TConstValue voption =
        match payload e with
        | ExprPayload.Const value -> ValueSome value
        | _ -> ValueNone

    let exprConstValue (e: ExprId) : TConstValue =
        expect "TastAccessor.exprConstValue: not a Const node" (|EConst|_|) e

    /// A `Var` node → the bound variable it references. Guards on the reference column rather than
    /// the payload: only a `Var` fills it.
    [<return: Struct>]
    let (|EVar|_|) (e: ExprId) : BoundVarId voption =
        TastPoolBuilder.exprVarBoundVar e.Pool e.Id

    let exprVarBoundVar (e: ExprId) : BoundVarId =
        expect "TastAccessor.exprVarBoundVar: not a Var node" (|EVar|_|) e

    [<return: Struct>]
    let private (|EVarNaming|_|) (e: ExprId) : BoundVarNaming voption =
        TastPoolBuilder.exprVarBoundVar e.Pool e.Id
        |> ValueOption.map (TastPoolBuilder.boundVarNaming e.Pool)

    /// What a backend emits the referenced bound variable's name from.
    let exprVarNaming (e: ExprId) : BoundVarNaming =
        expect "TastAccessor.exprVarNaming: not a Var node" (|EVarNaming|_|) e

    [<return: Struct>]
    let (|EExternalMember|_|) (e: ExprId) : ExternalMemberView voption =
        match payload e with
        | ExprPayload.ExternalMember p ->
            ValueSome
                {
                    ObjArg = (if p.HasObjArg then ValueSome(exprChild e 0) else ValueNone)
                    Key = p.Key
                    MemberName = p.MemberName
                    Storage = p.Storage
                    ArgGroupWidths = p.ArgGroupWidths
                }
        | _ -> ValueNone

    let exprExternalMember (e: ExprId) : ExternalMemberView =
        expect "TastAccessor.exprExternalMember: not an ExternalMember node" (|EExternalMember|_|) e

    [<return: Struct>]
    let private (|EILIntrinsic|_|) (e: ExprId) : ILIntrinsicView voption =
        match payload e with
        | ExprPayload.ILIntrinsic p ->
            ValueSome
                {
                    OpCode = p.OpCode
                    TypeOperand = p.TypeOperand
                }
        | _ -> ValueNone

    let exprILIntrinsicOpCode (e: ExprId) : string =
        (expect "TastAccessor.exprILIntrinsicOpCode: not an ILIntrinsic node" (|EILIntrinsic|_|) e).OpCode

    let exprILIntrinsicTypeOperand (e: ExprId) : FrozenType voption =
        (expect "TastAccessor.exprILIntrinsicTypeOperand: not an ILIntrinsic node" (|EILIntrinsic|_|) e).TypeOperand

    [<return: Struct>]
    let (|ELambda|_|) (e: ExprId) : LambdaView voption =
        match payload e with
        | ExprPayload.Lambda ->
            ValueSome
                {
                    Param = exprPatChild e 0
                    Body = exprChild e 0
                }
        | _ -> ValueNone

    let exprLambda (e: ExprId) : LambdaView =
        expect "TastAccessor.exprLambda: not a Lambda node" (|ELambda|_|) e

    [<return: Struct>]
    let (|ELet|_|) (e: ExprId) : LetView voption =
        match payload e with
        | ExprPayload.Let(isRec, recursion) ->
            ValueSome
                {
                    Binding =
                        {
                            Pattern = exprPatChild e 0
                            Value = exprChild e 0
                            Tok = exprTok e
                            Recursion = recursion
                        }
                    Body = exprChild e 1
                    IsRec = isRec
                }
        | _ -> ValueNone

    let exprLet (e: ExprId) : LetView =
        expect "TastAccessor.exprLet: not a Let node" (|ELet|_|) e

    let private letMembers (shape: LetGroupShape) (pat: int -> PatId) (value: int -> ExprId) : LetMemberView[] =
        shape.Members
        |> Array.mapi (fun i m ->
            {
                Pattern = pat i
                Value = value i
                Tok = m.Tok
                Recursion = m.Recursion
            }
        )

    [<return: Struct>]
    let (|ELetGroup|_|) (e: ExprId) : LetGroupView voption =
        match payload e with
        | ExprPayload.LetGroup g ->
            ValueSome
                {
                    Members = letMembers g (exprPatChild e) (exprChild e)
                    Components = g.Components
                    Body = exprChild e g.Members.Length
                }
        | _ -> ValueNone

    let exprLetGroup (e: ExprId) : LetGroupView =
        expect "TastAccessor.exprLetGroup: not a LetGroup node" (|ELetGroup|_|) e

    [<return: Struct>]
    let private (|EAssignment|_|) (e: ExprId) : AssignmentView voption =
        match payload e with
        | ExprPayload.Assignment ->
            ValueSome
                {
                    Lhs = exprChild e 0
                    Rhs = exprChild e 1
                }
        | _ -> ValueNone

    let exprAssignment (e: ExprId) : AssignmentView =
        expect "TastAccessor.exprAssignment: not an Assignment node" (|EAssignment|_|) e

    [<return: Struct>]
    let private (|EIfThenElse|_|) (e: ExprId) : IfThenElseView voption =
        match payload e with
        | ExprPayload.IfThenElse ->
            ValueSome
                {
                    Cond = exprChild e 0
                    ThenExpr = exprChild e 1
                    ElseExpr = exprChild e 2
                }
        | _ -> ValueNone

    let exprIfThenElse (e: ExprId) : IfThenElseView =
        expect "TastAccessor.exprIfThenElse: not an IfThenElse node" (|EIfThenElse|_|) e

    [<return: Struct>]
    let (|EExternal|_|) (e: ExprId) : BindingKey voption =
        match payload e with
        | ExprPayload.External key -> ValueSome key
        | _ -> ValueNone

    let exprExternalKey (e: ExprId) : BindingKey =
        expect "TastAccessor.exprExternalKey: not an External node" (|EExternal|_|) e

    [<return: Struct>]
    let (|EApp|_|) (e: ExprId) : AppView voption =
        match payload e with
        | ExprPayload.App _ ->
            ValueSome
                {
                    Fn = exprChild e 0
                    Arg = exprChild e 1
                }
        | _ -> ValueNone

    let exprApp (e: ExprId) : AppView =
        expect "TastAccessor.exprApp: not an App node" (|EApp|_|) e

    [<return: Struct>]
    let private (|ERecordCons|_|) (e: ExprId) : (string * ExprId)[] voption =
        match payload e with
        | ExprPayload.RecordCons fieldNames ->
            let values = exprChildren e

            if fieldNames.Length <> values.Length then
                failwithf
                    "TastAccessor: RecordCons node %A carries %d field names but %d value children"
                    e.Id
                    fieldNames.Length
                    values.Length

            ValueSome(Array.map2 (fun n v -> (n, v)) fieldNames values)
        | _ -> ValueNone

    /// The labels `exprChildren` drops, paired back with their value expressions.
    let exprRecordConsFields (e: ExprId) : (string * ExprId)[] =
        expect "TastAccessor.exprRecordConsFields: not a RecordCons node" (|ERecordCons|_|) e

    [<return: Struct>]
    let private (|ERecordClone|_|) (e: ExprId) : RecordCloneView voption =
        match payload e with
        | ExprPayload.RecordClone overrideNames ->
            ValueSome
                {
                    Source = exprChild e 0
                    Overrides = overrideNames |> Array.mapi (fun i n -> (n, exprChild e (i + 1)))
                }
        | _ -> ValueNone

    let exprRecordClone (e: ExprId) : RecordCloneView =
        expect "TastAccessor.exprRecordClone: not a RecordClone node" (|ERecordClone|_|) e

    [<return: Struct>]
    let (|EFieldGet|_|) (e: ExprId) : FieldGetView voption =
        match payload e with
        | ExprPayload.FieldGet fieldName ->
            ValueSome
                {
                    ObjArg = exprChild e 0
                    FieldName = fieldName
                }
        | _ -> ValueNone

    let exprFieldGet (e: ExprId) : FieldGetView =
        expect "TastAccessor.exprFieldGet: not a FieldGet node" (|EFieldGet|_|) e

    [<return: Struct>]
    let private (|EFieldSet|_|) (e: ExprId) : FieldSetView voption =
        match payload e with
        | ExprPayload.FieldSet fieldName ->
            ValueSome
                {
                    ObjArg = exprChild e 0
                    FieldName = fieldName
                    Value = exprChild e 1
                }
        | _ -> ValueNone

    let exprFieldSet (e: ExprId) : FieldSetView =
        expect "TastAccessor.exprFieldSet: not a FieldSet node" (|EFieldSet|_|) e

    [<return: Struct>]
    let private (|EUnionCons|_|) (e: ExprId) : string voption =
        match payload e with
        | ExprPayload.UnionCons caseName -> ValueSome caseName
        | _ -> ValueNone

    /// The case name a `UnionCons` constructs; its args are the node's `exprChildren`.
    let exprUnionConsCaseName (e: ExprId) : string =
        expect "TastAccessor.exprUnionConsCaseName: not a UnionCons node" (|EUnionCons|_|) e

    [<return: Struct>]
    let private (|ENew|_|) (e: ExprId) : NewView voption =
        match payload e with
        | ExprPayload.New p ->
            ValueSome
                {
                    ClassName = p.ClassName
                    ChosenCtor = p.Key
                }
        | _ -> ValueNone

    let exprNewClassName (e: ExprId) : string =
        (expect "TastAccessor.exprNewClassName: not a New node" (|ENew|_|) e).ClassName

    /// The overload identity the front end chose, `ValueNone` where it recorded none.
    let exprNewChosenCtor (e: ExprId) : SymbolKey voption =
        (expect "TastAccessor.exprNewChosenCtor: not a New node" (|ENew|_|) e).ChosenCtor

    [<return: Struct>]
    let private (|EPropertyGet|_|) (e: ExprId) : PropertyGetView voption =
        match payload e with
        | ExprPayload.PropertyGet p ->
            ValueSome
                {
                    ObjArg = exprChild e 0
                    Key = p.Key
                    Via = p.Via
                }
        | _ -> ValueNone

    let exprPropertyGet (e: ExprId) : PropertyGetView =
        expect "TastAccessor.exprPropertyGet: not a PropertyGet node" (|EPropertyGet|_|) e

    [<return: Struct>]
    let private (|EMethodCall|_|) (e: ExprId) : MethodCallView voption =
        match payload e with
        | ExprPayload.MethodCall p ->
            ValueSome
                {
                    ObjArg = exprChild e 0
                    Key = p.Key
                    Via = p.Via
                    Args = EqArray.init (exprChildCount e - 1) (fun i -> exprChild e (i + 1))
                }
        | _ -> ValueNone

    let exprMethodCall (e: ExprId) : MethodCallView =
        expect "TastAccessor.exprMethodCall: not a MethodCall node" (|EMethodCall|_|) e

    [<return: Struct>]
    let private (|EStaticPropertyGet|_|) (e: ExprId) : SymbolKey voption =
        match payload e with
        | ExprPayload.StaticPropertyGet p -> ValueSome p.Key
        | _ -> ValueNone

    let exprStaticPropertyGetKey (e: ExprId) : SymbolKey =
        expect "TastAccessor.exprStaticPropertyGetKey: not a StaticPropertyGet node" (|EStaticPropertyGet|_|) e

    /// The declaring type's instantiation at a `StaticPropertyGet` / `StaticMethodCall` site;
    /// empty for a non-generic declaring type.
    let exprStaticDeclArgs (e: ExprId) : EqArray<FrozenType> =
        match payload e with
        | ExprPayload.StaticPropertyGet p -> p.DeclArgs
        | ExprPayload.StaticMethodCall p -> p.DeclArgs
        | other -> failwithf "TastAccessor.exprStaticDeclArgs: not a static member access node: %A" other

    [<return: Struct>]
    let private (|EStaticFieldGet|_|) (e: ExprId) : StaticFieldGetView voption =
        match payload e with
        | ExprPayload.StaticFieldGet p ->
            ValueSome
                {
                    Key = p.DeclKey
                    FieldName = p.FieldName
                }
        | _ -> ValueNone

    let exprStaticFieldGet (e: ExprId) : StaticFieldGetView =
        expect "TastAccessor.exprStaticFieldGet: not a StaticFieldGet node" (|EStaticFieldGet|_|) e

    [<return: Struct>]
    let private (|EStaticFieldSet|_|) (e: ExprId) : StaticFieldSetView voption =
        match payload e with
        | ExprPayload.StaticFieldSet p ->
            ValueSome
                {
                    Key = p.DeclKey
                    FieldName = p.FieldName
                    Value = exprChild e 0
                }
        | _ -> ValueNone

    let exprStaticFieldSet (e: ExprId) : StaticFieldSetView =
        expect "TastAccessor.exprStaticFieldSet: not a StaticFieldSet node" (|EStaticFieldSet|_|) e

    [<return: Struct>]
    let private (|EStaticMethodCall|_|) (e: ExprId) : SymbolKey voption =
        match payload e with
        | ExprPayload.StaticMethodCall p -> ValueSome p.Key
        | _ -> ValueNone

    /// The resolved member key; the call's args are the node's `exprChildren`.
    let exprStaticMethodCallKey (e: ExprId) : SymbolKey =
        expect "TastAccessor.exprStaticMethodCallKey: not a StaticMethodCall node" (|EStaticMethodCall|_|) e

    [<return: Struct>]
    let private (|EInlineCall|_|) (e: ExprId) : InlineCallView voption =
        match payload e with
        | ExprPayload.InlineCall p -> ValueSome { Spec = p.Spec; Source = p.Path }
        | _ -> ValueNone

    let exprInlineCall (e: ExprId) : InlineCallView =
        expect "TastAccessor.exprInlineCall: not an InlineCall node" (|EInlineCall|_|) e

    [<return: Struct>]
    let private (|ECallerExprSource|_|) (e: ExprId) : AssemblyFilePath voption =
        match payload e with
        | ExprPayload.CallerExpr origin -> ValueSome origin
        | _ -> ValueNone

    /// The file the subtree under a `CallerExpr` is anchored in: the node marks material
    /// written at a CALL SITE and moved into an entry's body.
    let exprCallerExprSource (e: ExprId) : AssemblyFilePath =
        expect "TastAccessor.exprCallerExprSource: not a CallerExpr node" (|ECallerExprSource|_|) e

    /// The arms re-nested out of this node's child columns. `lead` is how many leading expr
    /// children belong to the node itself rather than to an arm (`Match`'s scrutinee,
    /// `TryWith`'s body).
    let private armsOf (e: ExprId) (guardPresent: bool[]) (lead: int) : Arm[] =
        ExprPayload.arms
            guardPresent
            (ExprPayload.cursor (exprPatChildren e) 0)
            (ExprPayload.cursor (exprChildren e) lead)

    [<return: Struct>]
    let (|EMatch|_|) (e: ExprId) : MatchView voption =
        match payload e with
        | ExprPayload.Match guardPresent ->
            ValueSome
                {
                    Scrutinee = exprChild e 0
                    Arms = armsOf e guardPresent 1
                }
        | _ -> ValueNone

    let exprMatch (e: ExprId) : MatchView =
        expect "TastAccessor.exprMatch: not a Match node" (|EMatch|_|) e

    [<return: Struct>]
    let (|ETryWith|_|) (e: ExprId) : TryWithView voption =
        match payload e with
        | ExprPayload.TryWith guardPresent ->
            ValueSome
                {
                    Body = exprChild e 0
                    Arms = armsOf e guardPresent 1
                }
        | _ -> ValueNone

    let exprTryWith (e: ExprId) : TryWithView =
        expect "TastAccessor.exprTryWith: not a TryWith node" (|ETryWith|_|) e

    [<return: Struct>]
    let private (|ETryFinally|_|) (e: ExprId) : TryFinallyView voption =
        match payload e with
        | ExprPayload.TryFinally ->
            ValueSome
                {
                    Body = exprChild e 0
                    Cleanup = exprChild e 1
                }
        | _ -> ValueNone

    let exprTryFinally (e: ExprId) : TryFinallyView =
        expect "TastAccessor.exprTryFinally: not a TryFinally node" (|ETryFinally|_|) e

    [<return: Struct>]
    let private (|EWhile|_|) (e: ExprId) : WhileView voption =
        match payload e with
        | ExprPayload.While ->
            ValueSome
                {
                    Cond = exprChild e 0
                    Body = exprChild e 1
                }
        | _ -> ValueNone

    let exprWhile (e: ExprId) : WhileView =
        expect "TastAccessor.exprWhile: not a While node" (|EWhile|_|) e

    [<return: Struct>]
    let (|EForTo|_|) (e: ExprId) : ForToView voption =
        match payload e with
        | ExprPayload.ForTo p ->
            ValueSome
                {
                    Var = p.Var
                    StartExpr = exprChild e 0
                    EndExpr = exprChild e 1
                    Body = exprChild e 2
                }
        | _ -> ValueNone

    let exprForTo (e: ExprId) : ForToView =
        expect "TastAccessor.exprForTo: not a ForTo node" (|EForTo|_|) e

    [<return: Struct>]
    let (|EForIn|_|) (e: ExprId) : ForInView voption =
        match payload e with
        | ExprPayload.ForIn enumerator ->
            ValueSome
                {
                    Pat = exprPatChild e 0
                    Source = exprChild e 0
                    Body = exprChild e 1
                    Enumerator = enumerator
                }
        | _ -> ValueNone

    let exprForIn (e: ExprId) : ForInView =
        expect "TastAccessor.exprForIn: not a ForIn node" (|EForIn|_|) e

    [<return: Struct>]
    let (|EUse|_|) (e: ExprId) : UseView voption =
        match payload e with
        | ExprPayload.Use dispose ->
            ValueSome
                {
                    Pattern = exprPatChild e 0
                    Value = exprChild e 0
                    Body = exprChild e 1
                    Dispose = dispose
                }
        | _ -> ValueNone

    let exprUse (e: ExprId) : UseView =
        expect "TastAccessor.exprUse: not a Use node" (|EUse|_|) e

    [<return: Struct>]
    let private (|EFormat|_|) (e: ExprId) : FormatView voption =
        match payload e with
        | ExprPayload.Format p ->
            // Cursor 0: the sink's sub-expression is the first `exprChildren` entry, the
            // node having no leading children of its own.
            let sink, segments =
                ExprPayload.format p.Sink p.Segments (ExprPayload.cursor (exprChildren e) 0)

            ValueSome { Sink = sink; Segments = segments }
        | _ -> ValueNone

    let exprFormat (e: ExprId) : FormatView =
        expect "TastAccessor.exprFormat: not a Format node" (|EFormat|_|) e

    [<return: Struct>]
    let private (|ETypeTest|_|) (e: ExprId) : FrozenType voption =
        match payload e with
        | ExprPayload.TypeTest testTy -> ValueSome testTy
        | _ -> ValueNone

    /// The tested-against type `T` of `e :? T`: the `isinst` operand, distinct from
    /// `exprTy`, which is the `bool` result. The tested `e` is the sole `exprChildren` entry.
    let exprTypeTestTestTy (e: ExprId) : FrozenType =
        expect "TastAccessor.exprTypeTestTestTy: not a TypeTest node" (|ETypeTest|_|) e

    [<return: Struct>]
    let private (|ETraitCallMemberName|_|) (e: ExprId) : string voption =
        match payload e with
        | ExprPayload.TraitCall p -> ValueSome p.MemberName
        | _ -> ValueNone

    /// The compiled member name an SRTP `TraitCall` dispatches on (`op_Addition`); its
    /// support types are `FrozenType`s and its operands are the `exprChildren`.
    let exprTraitCallMemberName (e: ExprId) : string =
        expect "TastAccessor.exprTraitCallMemberName: not a TraitCall node" (|ETraitCallMemberName|_|) e

    [<return: Struct>]
    let private (|ETraitCallSupportTys|_|) (e: ExprId) : EqArray<FrozenType> voption =
        match payload e with
        | ExprPayload.TraitCall p -> ValueSome p.SupportTys
        | _ -> ValueNone

    /// The candidate support set of an SRTP `TraitCall`, in argument order.
    let exprTraitCallSupportTys (e: ExprId) : EqArray<FrozenType> =
        expect "TastAccessor.exprTraitCallSupportTys: not a TraitCall node" (|ETraitCallSupportTys|_|) e

    [<return: Struct>]
    let private (|EStaticOptimizationDefault|_|) (e: ExprId) : ExprId voption =
        match payload e with
        | ExprPayload.StaticOptimization _ -> ValueSome(exprChild e (exprChildCount e - 1))
        | _ -> ValueNone

    /// The dynamic default of a `StaticOptimization`: the branch taken when no clause's
    /// constraints hold, and the LAST of the node's `exprChildren`.
    let exprStaticOptimizationDefault (e: ExprId) : ExprId =
        expect
            "TastAccessor.exprStaticOptimizationDefault: not a StaticOptimization node"
            (|EStaticOptimizationDefault|_|)
            e

    // ── patterns ────────────────────────────────────────────────────────────

    let patKind (p: PatId) : PatShape = TastPoolBuilder.patShape p.Pool p.Id

    let patTy (p: PatId) : FrozenType = TastPoolBuilder.patTy p.Pool p.Id

    /// A union / record pattern tests the type ITSELF, so its scrutinee denotes a type
    /// constructor.
    let patNominalTy (p: PatId) : FrozenNominal =
        let ty = patTy p

        match FrozenNominal.tryOfFrozen ty with
        | ValueSome n -> n
        | ValueNone -> failwithf "a %A pattern does not denote a type constructor: %A" (patKind p) ty

    let patTok (p: PatId) : Anchor = TastPoolBuilder.patTok p.Pool p.Id

    /// The immediate child *patterns*, in source order.
    let patChildren (p: PatId) : PatId[] =
        TastPoolBuilder.patChildren p.Pool p.Id |> Array.map (at p)

    /// The `i`-th immediate sub-pattern, without materialising its siblings.
    let patChild (p: PatId) (i: int) : PatId =
        at p (TastPoolBuilder.patChild p.Pool p.Id i)

    let private patPayload (p: PatId) : PatPayload = TastPoolBuilder.patPayload p.Pool p.Id

    /// The single bound variable a `NamedSimple` pattern introduces. `ValueNone` for a pattern that
    /// binds nothing (`Wildcard`, `Const`) or through sub-patterns (`Tuple`, `Or`).
    let patBoundVar (p: PatId) : BoundVarId voption =
        match patPayload p with
        | PatPayload.NamedSimple(boundVar, _) -> ValueSome boundVar
        | _ -> ValueNone

    [<return: Struct>]
    let (|PNamed|_|) (p: PatId) : BoundVarId voption = patBoundVar p

    /// The `PNamed` to reach for when the bound variable is about to be spelled as an emitted
    /// identifier rather than looked up.
    [<return: Struct>]
    let (|PNamedNaming|_|) (p: PatId) : BoundVarNaming voption =
        match patPayload p with
        | PatPayload.NamedSimple(boundVar, _) -> ValueSome(TastPoolBuilder.boundVarNaming p.Pool boundVar)
        | _ -> ValueNone

    [<return: Struct>]
    let private (|PConst|_|) (p: PatId) : TConstValue voption =
        match patPayload p with
        | PatPayload.Const value -> ValueSome value
        | _ -> ValueNone

    let patConstValue (p: PatId) : TConstValue =
        expect "TastAccessor.patConstValue: not a Const pattern" (|PConst|_|) p

    [<return: Struct>]
    let private (|PUnion|_|) (p: PatId) : string voption =
        match patPayload p with
        | PatPayload.Union caseName -> ValueSome caseName
        | _ -> ValueNone

    let patUnionCaseName (p: PatId) : string =
        expect "TastAccessor.patUnionCaseName: not a Union pattern" (|PUnion|_|) p

    [<return: Struct>]
    let private (|PRecord|_|) (p: PatId) : (string * PatId)[] voption =
        match patPayload p with
        | PatPayload.Record fieldNames ->
            let subs = patChildren p

            if fieldNames.Length <> subs.Length then
                failwithf
                    "TastAccessor: Record pattern %A carries %d field names but %d sub-patterns"
                    p.Id
                    fieldNames.Length
                    subs.Length

            ValueSome(Array.map2 (fun n sub -> (n, sub)) fieldNames subs)
        | _ -> ValueNone

    /// The labels `patChildren` drops, paired back with their sub-patterns.
    let patRecordFields (p: PatId) : (string * PatId)[] =
        expect "TastAccessor.patRecordFields: not a Record pattern" (|PRecord|_|) p

    [<return: Struct>]
    let private (|PEnumCase|_|) (p: PatId) : EnumCasePatView voption =
        match patPayload p with
        | PatPayload.EnumCase v ->
            ValueSome
                {
                    EnumKey = v.EnumKey
                    CaseName = v.CaseName
                }
        | _ -> ValueNone

    let patEnumCase (p: PatId) : EnumCasePatView =
        expect "TastAccessor.patEnumCase: not an EnumCase pattern" (|PEnumCase|_|) p

    [<return: Struct>]
    let private (|PTypeTestAs|_|) (p: PatId) : FrozenType voption =
        match patPayload p with
        | PatPayload.TypeTestAs testTy -> ValueSome testTy
        | _ -> ValueNone

    /// The tested-against type `T` of `:? T as x`: the `isinst` operand, distinct from
    /// `patTy`, which is the scrutinee's type. The bound `as`-name is `patChildren.[0]`.
    let patTypeTestTestTy (p: PatId) : FrozenType =
        expect "TastAccessor.patTypeTestTestTy: not a TypeTestAs pattern" (|PTypeTestAs|_|) p

    // ── declarations ────────────────────────────────────────────────────────

    let declKind (d: DeclId) : DeclShape = TastPoolBuilder.declShape d.Pool d.Id

    let private declPayload (d: DeclId) : DeclPayload = TastPoolBuilder.declPayload d.Pool d.Id

    /// The `i`-th expr / pat root of a decl.
    let private declExprChild (d: DeclId) (i: int) : ExprId =
        at d (TastPoolBuilder.declExprChild d.Pool d.Id i)

    let private declPatChild (d: DeclId) (i: int) : PatId =
        at d (TastPoolBuilder.declPatChild d.Pool d.Id i)

    [<return: Struct>]
    let private (|DType|_|) (d: DeclId) : TypeDecl voption =
        match declPayload d with
        // Only the BODIES move: the type, token and key slots already hold the shape a pooled
        // reference consumes.
        | DeclPayload.Type td ->
            ValueSome(
                TastConvert.typeDecl
                    {
                        Ty = id
                        Tok = id
                        Id = BoundVarKey.identity
                        Body = at d
                    }
                    td
            )
        | _ -> ValueNone

    /// The `type`-declaration payload, its member/preamble/ctor bodies resolved to handles.
    let declType (d: DeclId) : TypeDecl =
        expect "TastAccessor.declType: not a Type decl" (|DType|_|) d

    [<return: Struct>]
    let (|DExpression|_|) (d: DeclId) : struct (ExprId * FrozenType) voption =
        match declPayload d with
        | DeclPayload.Expression ty -> ValueSome(struct (declExprChild d 0, ty))
        | _ -> ValueNone

    let declExpression (d: DeclId) : ExprId =
        let struct (e, _) =
            expect "TastAccessor.declExpression: not an Expression decl" (|DExpression|_|) d

        e

    [<return: Struct>]
    let (|DLet|_|) (d: DeclId) : DeclLetView voption =
        match declPayload d with
        | DeclPayload.Let p ->
            ValueSome
                {
                    Binding =
                        {
                            Pattern = declPatChild d 0
                            Value = declExprChild d 0
                            Tok = p.Tok
                            Recursion = p.Recursion
                        }
                    IsInline = p.IsInline
                    IsRec = p.IsRec
                }
        | _ -> ValueNone

    let declLet (d: DeclId) : DeclLetView =
        expect "TastAccessor.declLet: not a Let decl" (|DLet|_|) d

    [<return: Struct>]
    let (|DLetGroup|_|) (d: DeclId) : DeclLetGroupView voption =
        match declPayload d with
        | DeclPayload.LetGroup g ->
            ValueSome
                {
                    Members = letMembers g (declPatChild d) (declExprChild d)
                    Components = g.Components
                }
        | _ -> ValueNone

    let declLetGroup (d: DeclId) : DeclLetGroupView =
        expect "TastAccessor.declLetGroup: not a LetGroup decl" (|DLetGroup|_|) d

    /// The file's declarations, in source order: the pool roots as handles.
    let roots (pool: PoolBuilder) : DeclId[] =
        let ids = TastPoolBuilder.roots pool
        Array.init ids.Length (fun i -> { Pool = pool; Id = ids.[i] })

    /// Every module-level binding, in source order: a `Let` root and each member of a
    /// `LetGroup` root.
    let rootBindings (pool: PoolBuilder) : LetMemberView[] =
        [|
            for d in roots pool do
                match d with
                | DLet l -> l.Binding
                | DLetGroup g -> yield! g.Members
                | _ -> ()
        |]

    /// The resolved-specialization entry an `InlineCall`'s `SpecializationId` identifies, its
    /// abstraction as a handle. Reached from the pool, not from a node: several call sites
    /// share one entry.
    let specialization (pool: PoolBuilder) (spec: SpecializationId) : Specialization =
        let entry = TastPoolBuilder.specialization pool spec

        {
            Key = entry.Key
            Path = entry.Path
            Value = { Pool = pool; Id = entry.Value }
        }

    // ── generic traversal ───────────────────────────────────────────────────

    /// Rebuild `e` with `f` applied to each immediate child expression. Its owned
    /// sub-patterns are untouched, so a rewrite that must reach them walks `exprPatChildren`.
    let mapChildren (f: ExprId -> ExprId) (e: ExprId) : ExprId =
        let kids = exprChildren e |> Array.map (fun c -> (f c).Id)
        at e (TastPoolBuilder.copyExprWith e.Pool e.Id (fun row -> { row with Children = kids }))

    /// Visit each immediate child expression. NOT `mapChildren` with the result thrown
    /// away: `mapChildren` copies a row when a child moves, and a visit must append nothing.
    let iterChildren (f: ExprId -> unit) (e: ExprId) : unit =
        for i in 0 .. exprChildCount e - 1 do
            f (exprChild e i)

    let existsChild (p: ExprId -> bool) (e: ExprId) : bool =
        let n = exprChildCount e
        let mutable i = 0
        let mutable found = false

        while not found && i < n do
            found <- p (exprChild e i)
            i <- i + 1

        found

    /// Peel a curried `App` chain into the applied function and its argument levels. The
    /// inverse of `mintAppChain`.
    let rec collectAppChain (acc: AppliedArg list) (e: ExprId) : ExprId * AppliedArg list =
        match exprKind e with
        | ExprShape.App ->
            let app = exprApp e

            let level: AppliedArg =
                {
                    Arg = app.Arg
                    StepResultTy = exprTy e
                    Tok = exprTok e
                }

            collectAppChain (level :: acc) app.Fn
        | _ -> e, acc

    /// The saturated tail self-call at `e`: the enclosing `let rec` variable it applies, and
    /// its arguments, one per lambda of that binding's value.
    [<return: Struct>]
    let (|ETailSelfCall|_|) (e: ExprId) : struct (BoundVarId * ExprId list) voption =
        match payload e with
        | ExprPayload.App AppKind.TailSelfCall ->
            match collectAppChain [] e with
            | EVar b, args -> ValueSome(struct (b, [ for a in args -> a.Arg ]))
            | _ -> failwith "TastAccessor.ETailSelfCall: the app chain does not apply a bound variable"
        | _ -> ValueNone

    /// Rewrite a curried `App` chain: `fArg` on each argument, `fFn` on the applied
    /// function. Every `App` is a row copy, so a chain whose function and arguments all
    /// stay put keeps every id it already had.
    let rec mapAppChain (fFn: ExprId -> ExprId) (fArg: ExprId -> ExprId) (e: ExprId) : ExprId =
        match exprKind e with
        | ExprShape.App ->
            let app = exprApp e
            let fn = mapAppChain fFn fArg app.Fn
            let arg = fArg app.Arg

            at
                e
                (TastPoolBuilder.copyExprWith
                    e.Pool
                    e.Id
                    (fun row ->
                        { row with
                            Children = [| fn.Id; arg.Id |]
                        }
                    ))
        | _ -> fFn e

    // ── minting ─────────────────────────────────────────────────────────────
    // A minted node is a ROW appended to the pool taken off a handle the site already holds;
    // `Anchor.nowhere` is the token for no source position.

    let private mintExpr (pool: PoolBuilder) ty (tok: Anchor) children patChildren pl : ExprId =
        {
            Pool = pool
            Id =
                TastPoolBuilder.appendExpr
                    pool
                    {
                        Ty = ty
                        Tok = tok
                        Children = children
                        PatChildren = patChildren
                        VarBoundVar = ValueNone
                        Payload = pl
                    }
        }

    /// A reference to `boundVar`, keyed by the bound variable's own dense id, so a reference
    /// minted before (or without) its defining pattern identifies the same bound variable either way.
    let mintVar (pool: PoolBuilder) (boundVar: BoundVarId) (ty: FrozenType) (tok: Anchor) : ExprId =
        {
            Pool = pool
            Id =
                TastPoolBuilder.appendExpr
                    pool
                    {
                        Ty = ty
                        Tok = tok
                        Children = [||]
                        PatChildren = [||]
                        VarBoundVar = ValueSome boundVar
                        Payload = ExprPayload.Var
                    }
        }

    /// `fn arg`, typed with the application's result type.
    let mintApp (fn: ExprId) (arg: ExprId) (ty: FrozenType) (tok: Anchor) : ExprId =
        mintExpr fn.Pool ty tok [| fn.Id; arg.Id |] [||] (ExprPayload.App AppKind.Call)

    /// Re-apply a function to its argument levels: the inverse of `collectAppChain`.
    let mintAppChain (fn: ExprId) (args: AppliedArg list) : ExprId =
        List.fold (fun acc (a: AppliedArg) -> mintApp acc a.Arg a.StepResultTy a.Tok) fn args

    /// `fun param -> body`.
    let mintLambda (param: PatId) (body: ExprId) (ty: FrozenType) (tok: Anchor) : ExprId =
        mintExpr body.Pool ty tok [| body.Id |] [| param.Id |] ExprPayload.Lambda

    /// A non-recursive `let pattern = value in body`, typed with the body's type.
    let mintLet (pattern: PatId) (value: ExprId) (body: ExprId) (ty: FrozenType) (tok: Anchor) : ExprId =
        mintExpr
            body.Pool
            ty
            tok
            [| value.Id; body.Id |]
            [| pattern.Id |]
            (ExprPayload.Let(false, Recursion.NonRecursive))

    /// `objArg.Key args` — an instance call on a project-local member.
    let mintMethodCall
        (objArg: ExprId)
        (key: SymbolKey)
        (via: CallVia<FrozenType>)
        (args: ExprId[])
        (ty: FrozenType)
        (tok: Anchor)
        : ExprId =
        mintExpr
            objArg.Pool
            ty
            tok
            (Array.append [| objArg.Id |] (args |> Array.map (fun a -> a.Id)))
            [||]
            (ExprPayload.MethodCall {| Key = key; Via = via |})

    let private mintPat (pool: PoolBuilder) ty (tok: Anchor) children pl : PatId =
        {
            Pool = pool
            Id =
                TastPoolBuilder.appendPat
                    pool
                    {
                        Ty = ty
                        Tok = tok
                        Children = children
                        Payload = pl
                    }
        }

    /// An immutable named pattern binding `boundVar`.
    let mintNamedPat (pool: PoolBuilder) (boundVar: BoundVarId) (ty: FrozenType) (tok: Anchor) : PatId =
        mintPat pool ty tok [||] (PatPayload.NamedSimple(boundVar, false))

    /// An anonymous `_` pattern.
    let mintWildcardPat (pool: PoolBuilder) (ty: FrozenType) (tok: Anchor) : PatId =
        mintPat pool ty tok [||] PatPayload.Wildcard

    let mintTuplePat (pool: PoolBuilder) (items: PatId[]) (ty: FrozenType) (tok: Anchor) : PatId =
        mintPat pool ty tok (items |> Array.map (fun i -> i.Id)) PatPayload.Tuple

    /// A top-level `let pattern = value` declaration.
    let mintLetDecl (m: LetMemberView) (isInline: bool) (isRec: bool) : DeclId =
        {
            Pool = m.Value.Pool
            Id =
                TastPoolBuilder.appendDecl
                    m.Value.Pool
                    {
                        ExprChildren = [| m.Value.Id |]
                        PatChildren = [| m.Pattern.Id |]
                        Payload =
                            DeclPayload.Let
                                {|
                                    IsInline = isInline
                                    IsRec = isRec
                                    Recursion = m.Recursion
                                    Tok = m.Tok
                                |}
                    }
        }

    /// A top-level statement declaration.
    let mintExpressionDecl (expr: ExprId) (ty: FrozenType) : DeclId =
        {
            Pool = expr.Pool
            Id =
                TastPoolBuilder.appendDecl
                    expr.Pool
                    {
                        ExprChildren = [| expr.Id |]
                        PatChildren = [||]
                        Payload = DeclPayload.Expression ty
                    }
        }

    /// Re-author `e` with a different result type, touching the type column and nothing
    /// else; returns `e` itself when the type is unchanged.
    let retype (e: ExprId) (ty: FrozenType) : ExprId =
        at e (TastPoolBuilder.copyExprWith e.Pool e.Id (fun row -> { row with Ty = ty }))

    /// Re-author `e` with different immediate children AND a different result type, so a
    /// rewrite that changes both appends ONE row rather than two.
    let retypeWithChildren (e: ExprId) (children: ExprId[]) (ty: FrozenType) : ExprId =
        let kids = children |> Array.map (fun c -> c.Id)

        at e (TastPoolBuilder.copyExprWith e.Pool e.Id (fun row -> { row with Children = kids; Ty = ty }))

    /// Re-author a decl with a different value / body expression, returning the decl itself
    /// when the expression did not move. Reaches the decl's expr-child COLUMN only, which a
    /// `type` decl has none of, so this is the identity on one.
    let mapDeclExpr (f: ExprId -> ExprId) (d: DeclId) : DeclId =
        let kids =
            TastPoolBuilder.declExprChildren d.Pool d.Id
            |> Array.map (fun c -> (f (at d c)).Id)

        at d (TastPoolBuilder.copyDeclWith d.Pool d.Id (fun row -> { row with ExprChildren = kids }))

    /// Apply `f` to EVERY expression a declaration carries, including a `type` decl's member
    /// bodies, class preambles, secondary ctors and base-ctor args, all named INSIDE the
    /// payload, so they escape a child-column rewrite.
    let mapDeclBodies (f: ExprId -> ExprId) (d: DeclId) : DeclId =
        match declPayload d with
        | DeclPayload.Type td ->
            let td' =
                TastConvert.typeDecl
                    {
                        Ty = id
                        Tok = id
                        Id = BoundVarKey.identity
                        Body = fun (b: ExprPoolId) -> (f (at d b)).Id
                    }
                    td

            at
                d
                (TastPoolBuilder.copyDeclWith
                    d.Pool
                    d.Id
                    (fun row ->
                        { row with
                            Payload = DeclPayload.Type td'
                        }
                    ))
        | DeclPayload.Let _
        | DeclPayload.LetGroup _
        | DeclPayload.Expression _ -> mapDeclExpr f d
