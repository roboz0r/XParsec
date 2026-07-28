namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The frozen-TAST access seam: ONE TAST-shaped API — `exprKind`, `exprChildren`,
// `patBinder`, … — over the id-indexable pools. Every consumer (both codegen backends,
// the shared lowering) speaks this and nothing else, so the tree representation is
// visible in exactly one file.
//
// A node is a `Handle` — a dense pool id plus the pool that resolves it — and every
// body here is a column read. That is the whole implementation: a shape tag is one
// array index, a child list is one array index, and a payload view is one array index
// plus a match on the residual scalars the columnar split left over. Nothing walks a
// tree to answer a question about a node, and no node "contains" its children, so a
// consumer can hold one without pinning a subtree.
//
// The pool being on the handle rather than a parameter is what keeps the shapes a
// consumer writes unchanged — `lv.Value`, `arm.Guard`, `exprChildren e` — and lets
// pools that are not a file's tree coexist with it (an `.fsi`-minted `ValRepr`'s
// patterns index into their own; see `TastLower.externalValRepr`).
//
// The shapes an accessor yields — the handles, the handle-carrying instantiations, the
// `…View` payload records — are `TastNodeViews.fs`, and re-exported below so a consumer
// names this module alone. The `failwith` guards are the shape contract: read a payload
// only after `exprKind`/`patKind`/`declKind` (or through the matching recognizer).
//
// EVERY view guards on the `*Payload` column, never on the `*Shape` tag — one
// convention, so a reader never has to check which of the two a given accessor used.
// Payload rather than shape because the payload is the column the view is projecting
// FROM in all but a handful of cases, so the guard and the read are one fetch. The
// shape tag is for a consumer's own TOTAL dispatch (the emit routers), where its
// closed enum is what keeps the match exhaustive.
//
// EVERY view is written ONCE, as a RECOGNIZER — the accessor in pattern position — and
// its eager form is `expect` applied to that recognizer and nothing else:
//
//     match e with
//     | EForTo ft -> ...        // ft : ForToView, bound once
//     | EVar key  -> ...
//     | _ -> ...
//
// Each is a partial single-case active pattern (`[<return: Struct>]`, so the `voption`
// is unboxed and the match allocates nothing) that succeeds on its own payload case and
// declines on every other, so an arm can never fire the wrong projection. Deriving the
// eager accessor from it keeps one guard and ONE column read per match — a recognizer
// that re-dispatched on the shape tag and then called an eager view would fetch two
// columns and match twice, on the per-node path of both backends' emit walks.
//
// No accessor is exempt. An eager form written with its own `match … | _ -> failwith`
// would restate the guard the recognizer already holds, and the two would then be free
// to name different cases; going through `expect` means an accessor cannot come to guard
// on a case other than the one it projects. A recognizer nothing outside this file
// matches on is `private` — that is the cost of the rule, and it is only surface.
//
// Where one payload case has several eager readers (`New`'s class name and chosen ctor,
// `ILIntrinsic`'s opcode and type operand), there is still ONE recognizer, returning the
// whole view, and each reader is a field of it.
//
// A *total* dispatch over every shape — the emit routers (`EmitExpr.buildExpr`,
// `EmitPattern`, the `Emit.fs` decl loop) — matches the `exprKind`/`patKind`/`declKind`
// tag instead, which the closed `ExprShape`/`PatShape`/`DeclShape` enum keeps exhaustive:
// a new case breaks those matches at compile time. `RequireQualifiedAccess` means the
// recognizers are used qualified (`TastAccessor.EForTo`).

/// The TAST-shaped accessor. `open` is disallowed so `exprTy`/`patTy` don't collide
/// with the analysis-time `TastWalk` projections of the same name.
[<RequireQualifiedAccess>]
module TastAccessor =

    // Every shape an accessor below yields, re-exported under the module that PRODUCES
    // it: the declarations sit together in `TastNodeViews`, and a consumer names one
    // module for both the reader and what it hands back. Abbreviations are compile-time,
    // so `TastAccessor.LambdaView` and `TastNodeViews.LambdaView` are the one type.
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
    type AssignmentView = TastNodeViews.AssignmentView
    type IfThenElseView = TastNodeViews.IfThenElseView
    type ExternalView = TastNodeViews.ExternalView
    type AppView = TastNodeViews.AppView
    type RecordCloneView = TastNodeViews.RecordCloneView
    type FieldGetView = TastNodeViews.FieldGetView
    type FieldSetView = TastNodeViews.FieldSetView
    type NewView = TastNodeViews.NewView
    type PropertyGetView = TastNodeViews.PropertyGetView
    type MethodCallView = TastNodeViews.MethodCallView
    type StaticFieldGetView = TastNodeViews.StaticFieldGetView
    type StaticFieldSetView = TastNodeViews.StaticFieldSetView
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

    /// A sibling id in the same pool. Every child edge resolves through this, so the
    /// pool propagates down a walk without any consumer naming it.
    let inline private at (h: Handle<'a>) (id: 'b) : Handle<'b> = { Pool = h.Pool; Id = id }

    /// The EAGER form of a view that has a recognizer: project through it, or fault.
    /// Every such accessor is exactly this applied to its own recognizer, so the guard,
    /// the read and the failure are written once instead of restated per accessor — and
    /// an eager accessor cannot come to guard on a different case than the recognizer it
    /// documents itself as. `inline` so the call is still one column read and one match.
    let inline private expect (what: string) ([<InlineIfLambda>] recog: 'n -> 'v voption) (n: 'n) : 'v =
        match recog n with
        | ValueSome v -> v
        | ValueNone -> failwith what

    // ── expressions ─────────────────────────────────────────────────────────

    /// The shape tag of an expression node.
    let exprKind (e: ExprId) : ExprShape = TastPoolBuilder.exprShape e.Pool e.Id

    /// The node's result type.
    let exprTy (e: ExprId) : FrozenType = TastPoolBuilder.exprTy e.Pool e.Id

    /// The node's source-anchor token.
    let exprTok (e: ExprId) : SyntaxToken = TastPoolBuilder.exprTok e.Pool e.Id

    /// The immediate child *expressions*, in evaluation order — the recursion spine a
    /// generic walk (free-vars, closure discovery) follows. Sub-patterns are NOT
    /// children (see `exprPatChildren`); composite carriers with no node identity of
    /// their own (match arms, format segments, static-opt clauses) were flattened in,
    /// so every reachable sub-expression appears exactly once.
    let exprChildren (e: ExprId) : ExprId[] =
        TastPoolBuilder.exprChildren e.Pool e.Id |> Array.map (at e)

    /// The immediate child *patterns* an expression owns directly, in source order —
    /// the binders (`Lambda`/`Let`/`Use`/`ForIn`) and the per-arm scrutinee patterns
    /// (`Match`/`TryWith`). They are NOT reachable through `exprChildren`, so a generic
    /// walk that must reach every sub-pattern follows this alongside it. `ForTo`'s loop
    /// variable is a `NodeKey`, not a pattern, so it is not a pat child (see `exprForTo`).
    let exprPatChildren (e: ExprId) : PatId[] =
        TastPoolBuilder.exprPatChildren e.Pool e.Id |> Array.map (at e)

    // ONE child by position, without materialising the sibling list. `exprChildren` has
    // to `Array.map` a fresh handle array — the pool column holds bare ids — so a view
    // that wants two named children (`App`'s fn/arg, `Let`'s value/body) built and threw
    // away an array per read, on the per-node path both backends' emit walks. Indexing
    // the column costs nothing. The array forms stay for the views whose payload IS a
    // list (`RecordCons`, `MethodCall`'s args, the match arms) and for a generic walk.

    /// How many immediate child expressions `e` has — the bound `exprChild` indexes into.
    let exprChildCount (e: ExprId) : int =
        (TastPoolBuilder.exprChildren e.Pool e.Id).Length

    /// The `i`-th immediate child expression, in `exprChildren` order.
    let exprChild (e: ExprId) (i: int) : ExprId =
        at e (TastPoolBuilder.exprChildren e.Pool e.Id).[i]

    /// The `i`-th owned sub-pattern, in `exprPatChildren` order.
    let exprPatChild (e: ExprId) (i: int) : PatId =
        at e (TastPoolBuilder.exprPatChildren e.Pool e.Id).[i]

    let private payload (e: ExprId) : ExprPayload = TastPoolBuilder.exprPayload e.Pool e.Id

    /// A `Const` node → the constant value it carries.
    [<return: Struct>]
    let private (|EConst|_|) (e: ExprId) : TConstValue voption =
        match payload e with
        | ExprPayload.Const value -> ValueSome value
        | _ -> ValueNone

    /// The constant value carried by a `Const` node. Guard with `exprKind` =
    /// `ExprShape.Const` first; `failwith` on any other shape.
    let exprConstValue (e: ExprId) : TConstValue =
        expect "TastAccessor.exprConstValue: not a Const node" (|EConst|_|) e

    /// A `Var` node → the binder it references, the `NodeKey` a preceding binder
    /// introduced. Declines on every other shape (only a `Var` fills the reference
    /// column).
    [<return: Struct>]
    let (|EVar|_|) (e: ExprId) : NodeKey voption =
        TastPoolBuilder.exprVarBinder e.Pool e.Id
        |> ValueOption.map (TastPoolBuilder.binderKey e.Pool)

    /// The binder a `Var` node references. Guard with `exprKind` = `ExprShape.Var` (or
    /// match `EVar`) first; `failwith` on any other shape.
    let exprVarBinding (e: ExprId) : NodeKey =
        expect "TastAccessor.exprVarBinding: not a Var node" (|EVar|_|) e

    /// A `Var` node → the naming projections of the binder it references. Guards on the
    /// reference column, like `EVar`, rather than on the payload — only a `Var` fills it.
    [<return: Struct>]
    let private (|EVarNaming|_|) (e: ExprId) : BinderNaming voption =
        TastPoolBuilder.exprVarBinder e.Pool e.Id
        |> ValueOption.map (TastPoolBuilder.binderNaming e.Pool)

    /// The naming projections of the binder a `Var` node references — what a backend
    /// emits its name from, read off the pool's naming column rather than the key's own
    /// bits. Guard with `exprKind` = `ExprShape.Var` first; `failwith` on any other shape.
    let exprVarNaming (e: ExprId) : BinderNaming =
        expect "TastAccessor.exprVarNaming: not a Var node" (|EVarNaming|_|) e

    /// An `ExternalMember` node → its `ExternalMemberView`.
    [<return: Struct>]
    let (|EExternalMember|_|) (e: ExprId) : ExternalMemberView voption =
        match payload e with
        | ExprPayload.ExternalMember p ->
            ValueSome
                {
                    Receiver =
                        (if p.HasReceiver then
                             ValueSome(exprChild e 0)
                         else
                             ValueNone)
                    Key = p.Key
                    MemberName = p.MemberName
                    Storage = p.Storage
                }
        | _ -> ValueNone

    /// The payload view of an `ExternalMember` node. Guard with `exprKind` =
    /// `ExprShape.ExternalMember` first; `failwith` on any other shape.
    let exprExternalMember (e: ExprId) : ExternalMemberView =
        expect "TastAccessor.exprExternalMember: not an ExternalMember node" (|EExternalMember|_|) e

    /// An `ILIntrinsic` node → its `ILIntrinsicView`.
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

    /// The IL opcode string of an `ILIntrinsic` node. Guard with `exprKind` =
    /// `ExprShape.ILIntrinsic` first; `failwith` on any other shape.
    let exprILIntrinsicOpCode (e: ExprId) : string =
        (expect "TastAccessor.exprILIntrinsicOpCode: not an ILIntrinsic node" (|EILIntrinsic|_|) e).OpCode

    /// The type operand `<T>` an `ILIntrinsic` node carries. Guard with `exprKind` =
    /// `ExprShape.ILIntrinsic` first; `failwith` on any other shape.
    let exprILIntrinsicTypeOperand (e: ExprId) : FrozenType voption =
        (expect "TastAccessor.exprILIntrinsicTypeOperand: not an ILIntrinsic node" (|EILIntrinsic|_|) e).TypeOperand

    /// A `Lambda` node → its `LambdaView`.
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

    /// The payload view of a `Lambda` node. Guard with `exprKind` = `ExprShape.Lambda`
    /// first; `failwith` on any other shape.
    let exprLambda (e: ExprId) : LambdaView =
        expect "TastAccessor.exprLambda: not a Lambda node" (|ELambda|_|) e

    /// A `Let` node → its `LetView`.
    [<return: Struct>]
    let (|ELet|_|) (e: ExprId) : LetView voption =
        match payload e with
        | ExprPayload.Let ->
            ValueSome
                {
                    Binding = exprPatChild e 0
                    Value = exprChild e 0
                    Body = exprChild e 1
                }
        | _ -> ValueNone

    /// The payload view of a `Let` node. Guard with `exprKind` = `ExprShape.Let` first;
    /// `failwith` on any other shape.
    let exprLet (e: ExprId) : LetView =
        expect "TastAccessor.exprLet: not a Let node" (|ELet|_|) e

    /// An `Assignment` node → its `AssignmentView`.
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

    /// The payload view of an `Assignment` node. Guard with `exprKind` =
    /// `ExprShape.Assignment` first; `failwith` on any other shape.
    let exprAssignment (e: ExprId) : AssignmentView =
        expect "TastAccessor.exprAssignment: not an Assignment node" (|EAssignment|_|) e

    /// An `IfThenElse` node → its `IfThenElseView`.
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

    /// The payload view of an `IfThenElse` node. Guard with `exprKind` =
    /// `ExprShape.IfThenElse` first; `failwith` on any other shape.
    let exprIfThenElse (e: ExprId) : IfThenElseView =
        expect "TastAccessor.exprIfThenElse: not an IfThenElse node" (|EIfThenElse|_|) e

    /// An `External` node → its `ExternalView`.
    [<return: Struct>]
    let (|EExternal|_|) (e: ExprId) : ExternalView voption =
        match payload e with
        | ExprPayload.External p ->
            ValueSome
                {
                    CompiledName = p.CompiledName
                    Key = p.Key
                }
        | _ -> ValueNone

    /// The payload view of an `External` node. Guard with `exprKind` =
    /// `ExprShape.External` first; `failwith` on any other shape.
    let exprExternal (e: ExprId) : ExternalView =
        expect "TastAccessor.exprExternal: not an External node" (|EExternal|_|) e

    /// An `App` node → its `AppView`.
    [<return: Struct>]
    let (|EApp|_|) (e: ExprId) : AppView voption =
        match payload e with
        | ExprPayload.App ->
            ValueSome
                {
                    Fn = exprChild e 0
                    Arg = exprChild e 1
                }
        | _ -> ValueNone

    /// The payload view of an `App` node. Guard with `exprKind` = `ExprShape.App` first;
    /// `failwith` on any other shape.
    let exprApp (e: ExprId) : AppView =
        expect "TastAccessor.exprApp: not an App node" (|EApp|_|) e

    /// A `RecordCons` node → its (field-name, value-expression) pairs, in source order.
    [<return: Struct>]
    let private (|ERecordCons|_|) (e: ExprId) : (string * ExprId)[] voption =
        match payload e with
        | ExprPayload.RecordCons fieldNames -> ValueSome(Array.map2 (fun n v -> (n, v)) fieldNames (exprChildren e))
        | _ -> ValueNone

    /// The (field-name, value-expression) pairs a `RecordCons` literal assigns, in source
    /// order — the labels `exprChildren` drops. Guard with `exprKind` =
    /// `ExprShape.RecordCons` first; `failwith` on any other shape.
    let exprRecordConsFields (e: ExprId) : (string * ExprId)[] =
        expect "TastAccessor.exprRecordConsFields: not a RecordCons node" (|ERecordCons|_|) e

    /// A `RecordClone` node → its `RecordCloneView`.
    [<return: Struct>]
    let private (|ERecordClone|_|) (e: ExprId) : RecordCloneView voption =
        match payload e with
        | ExprPayload.RecordClone overrideNames ->
            let es = exprChildren e

            ValueSome
                {
                    Source = es.[0]
                    Overrides = Array.map2 (fun n v -> (n, v)) overrideNames es.[1..]
                }
        | _ -> ValueNone

    /// The payload view of a `RecordClone` node. Guard with `exprKind` =
    /// `ExprShape.RecordClone` first; `failwith` on any other shape.
    let exprRecordClone (e: ExprId) : RecordCloneView =
        expect "TastAccessor.exprRecordClone: not a RecordClone node" (|ERecordClone|_|) e

    /// A `FieldGet` node → its `FieldGetView`.
    [<return: Struct>]
    let (|EFieldGet|_|) (e: ExprId) : FieldGetView voption =
        match payload e with
        | ExprPayload.FieldGet fieldName ->
            ValueSome
                {
                    Receiver = exprChild e 0
                    FieldName = fieldName
                }
        | _ -> ValueNone

    /// The payload view of a `FieldGet` node. Guard with `exprKind` =
    /// `ExprShape.FieldGet` first; `failwith` on any other shape.
    let exprFieldGet (e: ExprId) : FieldGetView =
        expect "TastAccessor.exprFieldGet: not a FieldGet node" (|EFieldGet|_|) e

    /// A `FieldSet` node → its `FieldSetView`.
    [<return: Struct>]
    let private (|EFieldSet|_|) (e: ExprId) : FieldSetView voption =
        match payload e with
        | ExprPayload.FieldSet fieldName ->
            ValueSome
                {
                    Receiver = exprChild e 0
                    FieldName = fieldName
                    Value = exprChild e 1
                }
        | _ -> ValueNone

    /// The payload view of a `FieldSet` node. Guard with `exprKind` =
    /// `ExprShape.FieldSet` first; `failwith` on any other shape.
    let exprFieldSet (e: ExprId) : FieldSetView =
        expect "TastAccessor.exprFieldSet: not a FieldSet node" (|EFieldSet|_|) e

    /// A `UnionCons` node → the union case name it constructs.
    [<return: Struct>]
    let private (|EUnionCons|_|) (e: ExprId) : string voption =
        match payload e with
        | ExprPayload.UnionCons caseName -> ValueSome caseName
        | _ -> ValueNone

    /// The union case name a `UnionCons` node constructs. Its `args` are the node's
    /// `exprChildren` and its `ty` is `exprTy`. Guard with `exprKind` =
    /// `ExprShape.UnionCons` first; `failwith` on any other shape.
    let exprUnionConsCaseName (e: ExprId) : string =
        expect "TastAccessor.exprUnionConsCaseName: not a UnionCons node" (|EUnionCons|_|) e

    /// A `New` node → its `NewView`.
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

    /// The class name a `New` node constructs. Guard with `exprKind` = `ExprShape.New`
    /// first; `failwith` on any other shape.
    let exprNewClassName (e: ExprId) : string =
        (expect "TastAccessor.exprNewClassName: not a New node" (|ENew|_|) e).ClassName

    /// The recorded overload identity a `New` node's front end chose. Guard with
    /// `exprKind` = `ExprShape.New` first; `failwith` on any other shape.
    let exprNewChosenCtor (e: ExprId) : SymbolKey voption =
        (expect "TastAccessor.exprNewChosenCtor: not a New node" (|ENew|_|) e).ChosenCtor

    /// A `PropertyGet` node → its `PropertyGetView`.
    [<return: Struct>]
    let private (|EPropertyGet|_|) (e: ExprId) : PropertyGetView voption =
        match payload e with
        | ExprPayload.PropertyGet p ->
            ValueSome
                {
                    Receiver = exprChild e 0
                    Key = p.Key
                    Via = p.Via
                }
        | _ -> ValueNone

    /// The payload view of a `PropertyGet` node. Guard with `exprKind` =
    /// `ExprShape.PropertyGet` first; `failwith` on any other shape.
    let exprPropertyGet (e: ExprId) : PropertyGetView =
        expect "TastAccessor.exprPropertyGet: not a PropertyGet node" (|EPropertyGet|_|) e

    /// A `MethodCall` node → its `MethodCallView`.
    [<return: Struct>]
    let private (|EMethodCall|_|) (e: ExprId) : MethodCallView voption =
        match payload e with
        | ExprPayload.MethodCall p ->
            let es = exprChildren e

            ValueSome
                {
                    Receiver = es.[0]
                    Key = p.Key
                    Via = p.Via
                    Args = EqArray.ofArray es.[1..]
                }
        | _ -> ValueNone

    /// The payload view of a `MethodCall` node. Guard with `exprKind` =
    /// `ExprShape.MethodCall` first; `failwith` on any other shape.
    let exprMethodCall (e: ExprId) : MethodCallView =
        expect "TastAccessor.exprMethodCall: not a MethodCall node" (|EMethodCall|_|) e

    /// A `StaticPropertyGet` node → its resolved member key.
    [<return: Struct>]
    let private (|EStaticPropertyGet|_|) (e: ExprId) : SymbolKey voption =
        match payload e with
        | ExprPayload.StaticPropertyGet key -> ValueSome key
        | _ -> ValueNone

    /// The resolved member key of a `StaticPropertyGet` node. Guard with `exprKind` =
    /// `ExprShape.StaticPropertyGet` first; `failwith` on any other shape.
    let exprStaticPropertyGetKey (e: ExprId) : SymbolKey =
        expect "TastAccessor.exprStaticPropertyGetKey: not a StaticPropertyGet node" (|EStaticPropertyGet|_|) e

    /// A `StaticFieldGet` node → its `StaticFieldGetView`.
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

    /// The payload view of a `StaticFieldGet` node. Guard with `exprKind` =
    /// `ExprShape.StaticFieldGet` first; `failwith` on any other shape.
    let exprStaticFieldGet (e: ExprId) : StaticFieldGetView =
        expect "TastAccessor.exprStaticFieldGet: not a StaticFieldGet node" (|EStaticFieldGet|_|) e

    /// A `StaticFieldSet` node → its `StaticFieldSetView`.
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

    /// The payload view of a `StaticFieldSet` node. Guard with `exprKind` =
    /// `ExprShape.StaticFieldSet` first; `failwith` on any other shape.
    let exprStaticFieldSet (e: ExprId) : StaticFieldSetView =
        expect "TastAccessor.exprStaticFieldSet: not a StaticFieldSet node" (|EStaticFieldSet|_|) e

    /// A `StaticMethodCall` node → its resolved member key.
    [<return: Struct>]
    let private (|EStaticMethodCall|_|) (e: ExprId) : SymbolKey voption =
        match payload e with
        | ExprPayload.StaticMethodCall key -> ValueSome key
        | _ -> ValueNone

    /// The resolved member key of a `StaticMethodCall` node. Its `args` are the node's
    /// `exprChildren`. Guard with `exprKind` = `ExprShape.StaticMethodCall` first;
    /// `failwith` on any other shape.
    let exprStaticMethodCallKey (e: ExprId) : SymbolKey =
        expect "TastAccessor.exprStaticMethodCallKey: not a StaticMethodCall node" (|EStaticMethodCall|_|) e

    /// The children of the arms, re-nested — `ExprPayload.arms`, the walk shared with the
    /// pool drain, driven off this node's child columns. `lead` is how many leading expr
    /// children belong to the node itself rather than an arm (`Match`'s scrutinee /
    /// `TryWith`'s body).
    let private armsOf (e: ExprId) (guardPresent: bool[]) (lead: int) : Arm[] =
        ExprPayload.arms
            guardPresent
            (ExprPayload.cursor (exprPatChildren e) 0)
            (ExprPayload.cursor (exprChildren e) lead)

    /// A `Match` node → its `MatchView`.
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

    /// The payload view of a `Match` node. Guard with `exprKind` = `ExprShape.Match` first;
    /// `failwith` on any other shape.
    let exprMatch (e: ExprId) : MatchView =
        expect "TastAccessor.exprMatch: not a Match node" (|EMatch|_|) e

    /// A `TryWith` node → its `TryWithView`.
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

    /// The payload view of a `TryWith` node. Guard with `exprKind` = `ExprShape.TryWith`
    /// first; `failwith` on any other shape.
    let exprTryWith (e: ExprId) : TryWithView =
        expect "TastAccessor.exprTryWith: not a TryWith node" (|ETryWith|_|) e

    /// A `TryFinally` node → its `TryFinallyView`.
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

    /// The payload view of a `TryFinally` node. Guard with `exprKind` = `ExprShape.TryFinally`
    /// first; `failwith` on any other shape.
    let exprTryFinally (e: ExprId) : TryFinallyView =
        expect "TastAccessor.exprTryFinally: not a TryFinally node" (|ETryFinally|_|) e

    /// A `While` node → its `WhileView`.
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

    /// The payload view of a `While` node. Guard with `exprKind` = `ExprShape.While` first;
    /// `failwith` on any other shape.
    let exprWhile (e: ExprId) : WhileView =
        expect "TastAccessor.exprWhile: not a While node" (|EWhile|_|) e

    /// A `ForTo` node → its `ForToView`.
    [<return: Struct>]
    let (|EForTo|_|) (e: ExprId) : ForToView voption =
        match payload e with
        | ExprPayload.ForTo p ->
            ValueSome
                {
                    Var = TastPoolBuilder.binderKey e.Pool p.Var
                    StartExpr = exprChild e 0
                    EndExpr = exprChild e 1
                    Body = exprChild e 2
                }
        | _ -> ValueNone

    /// The payload view of a `ForTo` node. Guard with `exprKind` = `ExprShape.ForTo`
    /// first; `failwith` on any other shape.
    let exprForTo (e: ExprId) : ForToView =
        expect "TastAccessor.exprForTo: not a ForTo node" (|EForTo|_|) e

    /// A `ForIn` node → its `ForInView`.
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

    /// The payload view of a `ForIn` node. Guard with `exprKind` = `ExprShape.ForIn`
    /// first; `failwith` on any other shape.
    let exprForIn (e: ExprId) : ForInView =
        expect "TastAccessor.exprForIn: not a ForIn node" (|EForIn|_|) e

    /// A `Use` node → its `UseView`.
    [<return: Struct>]
    let (|EUse|_|) (e: ExprId) : UseView voption =
        match payload e with
        | ExprPayload.Use dispose ->
            ValueSome
                {
                    Binding = exprPatChild e 0
                    Value = exprChild e 0
                    Body = exprChild e 1
                    Dispose = dispose
                }
        | _ -> ValueNone

    /// The payload view of a `Use` node. Guard with `exprKind` = `ExprShape.Use` first;
    /// `failwith` on any other shape.
    let exprUse (e: ExprId) : UseView =
        expect "TastAccessor.exprUse: not a Use node" (|EUse|_|) e

    /// A `Format` node → its `FormatView`, through `ExprPayload.format` — the re-nesting
    /// shared with the pool drain, driven off this node's child column.
    [<return: Struct>]
    let private (|EFormat|_|) (e: ExprId) : FormatView voption =
        match payload e with
        | ExprPayload.Format p ->
            // No leading children of its own: the sink's sub-expression is the first thing
            // `exprChildren` yields, which is where `ExprPayload.format` starts.
            let sink, segments =
                ExprPayload.format p.Sink p.Segments (ExprPayload.cursor (exprChildren e) 0)

            ValueSome { Sink = sink; Segments = segments }
        | _ -> ValueNone

    /// The payload view of a `Format` node. Guard with `exprKind` = `ExprShape.Format`
    /// first; `failwith` on any other shape.
    let exprFormat (e: ExprId) : FormatView =
        expect "TastAccessor.exprFormat: not a Format node" (|EFormat|_|) e

    /// A `TypeTest` node → its tested-against type `T`.
    [<return: Struct>]
    let private (|ETypeTest|_|) (e: ExprId) : FrozenType voption =
        match payload e with
        | ExprPayload.TypeTest testTy -> ValueSome testTy
        | _ -> ValueNone

    /// The tested-against type `T` of a `TypeTest` node (`e :? T`) — the `isinst`
    /// operand. Distinct from `exprTy`, which is always `bool` (the test's result). The
    /// tested `source` is the sole `exprChildren` entry. Guard with `exprKind` =
    /// `ExprShape.TypeTest` first; `failwith` on any other shape.
    let exprTypeTestTestTy (e: ExprId) : FrozenType =
        expect "TastAccessor.exprTypeTestTestTy: not a TypeTest node" (|ETypeTest|_|) e

    /// A `StaticOptimization` node → its fallback (dynamic) default expression — the
    /// branch F# selects when no type-specialized clause's constraints hold, and the LAST
    /// of the node's `exprChildren` (the clause bodies precede it). It is also the only
    /// branch codegen emits: reaching a backend unresolved means inline expansion never
    /// pinned an operand type.
    [<return: Struct>]
    let private (|EStaticOptimizationDefault|_|) (e: ExprId) : ExprId voption =
        match payload e with
        | ExprPayload.StaticOptimization _ -> ValueSome(exprChild e (exprChildCount e - 1))
        | _ -> ValueNone

    /// The fallback (dynamic) default expression of a `StaticOptimization` node. Guard
    /// with `exprKind` = `ExprShape.StaticOptimization` first; `failwith` on any other
    /// shape.
    let exprStaticOptimizationDefault (e: ExprId) : ExprId =
        expect
            "TastAccessor.exprStaticOptimizationDefault: not a StaticOptimization node"
            (|EStaticOptimizationDefault|_|)
            e

    // ── patterns ────────────────────────────────────────────────────────────

    /// The shape tag of a pattern node.
    let patKind (p: PatId) : PatShape = TastPoolBuilder.patShape p.Pool p.Id

    /// The pattern's type.
    let patTy (p: PatId) : FrozenType = TastPoolBuilder.patTy p.Pool p.Id

    /// The pattern's source-anchor token.
    let patTok (p: PatId) : SyntaxToken = TastPoolBuilder.patTok p.Pool p.Id

    /// The immediate child *patterns*, in source order.
    let patChildren (p: PatId) : PatId[] =
        TastPoolBuilder.patChildren p.Pool p.Id |> Array.map (at p)

    /// The `i`-th immediate sub-pattern, without materialising its siblings — see
    /// `exprChild`.
    let patChild (p: PatId) (i: int) : PatId =
        at p (TastPoolBuilder.patChildren p.Pool p.Id).[i]

    let private patPayload (p: PatId) : PatPayload = TastPoolBuilder.patPayload p.Pool p.Id

    /// The single binder a simple (`NamedSimple`) pattern introduces — the identity a
    /// `Var` references and that naming is computed from. `ValueNone` for a pattern that
    /// binds nothing (`Wildcard`, `Const`, …) or binds through nested sub-patterns
    /// (`Tuple`, `Record`, `Union`, `TypeTestAs`, `Or` — walk `patChildren` for those).
    let patBinder (p: PatId) : NodeKey voption =
        match patPayload p with
        | PatPayload.NamedSimple binder -> ValueSome(TastPoolBuilder.binderKey p.Pool binder)
        | _ -> ValueNone

    /// A `NamedSimple` pattern → the single binder it introduces (`patBinder`, which is
    /// `ValueSome` exactly for that shape).
    [<return: Struct>]
    let (|PNamed|_|) (p: PatId) : NodeKey voption = patBinder p

    /// The POSITIONAL identity of the binder a `NamedSimple` pattern introduces — the id
    /// the pool's `BinderId`-keyed side tables are keyed by, so a consumer holding the
    /// defining node looks its entry up directly. It IS the payload's own field, so a
    /// lookup keyed on this can only ever name a binder the pool interned; `ValueNone`
    /// only for a pattern that introduces no binder at all.
    let patBinderId (p: PatId) : BinderId voption =
        match patPayload p with
        | PatPayload.NamedSimple binder -> ValueSome binder
        | _ -> ValueNone

    /// A `NamedSimple` pattern → the POSITIONAL id of the binder it introduces
    /// (`patBinderId`) — the `PNamed` to reach for when the binder is about to be looked
    /// up in a `BinderId`-keyed side table.
    [<return: Struct>]
    let (|PNamedId|_|) (p: PatId) : BinderId voption = patBinderId p

    /// The naming projections of the binder a `NamedSimple` pattern introduces —
    /// `exprVarNaming`'s pattern-side twin, off the same id space and so through the same
    /// projection.
    let patBinderNaming (p: PatId) : BinderNaming voption =
        match patPayload p with
        | PatPayload.NamedSimple binder -> ValueSome(TastPoolBuilder.binderNaming p.Pool binder)
        | _ -> ValueNone

    /// A `NamedSimple` pattern → the naming projections of the binder it introduces
    /// (`patBinderNaming`) — the `PNamed` to reach for when the binder is about to be
    /// spelled as an emitted identifier rather than looked up.
    [<return: Struct>]
    let (|PNamedNaming|_|) (p: PatId) : BinderNaming voption = patBinderNaming p

    /// A `Const` pattern → the constant value it carries.
    [<return: Struct>]
    let private (|PConst|_|) (p: PatId) : TConstValue voption =
        match patPayload p with
        | PatPayload.Const value -> ValueSome value
        | _ -> ValueNone

    /// The constant value carried by a `Const` pattern. Guard with `patKind` =
    /// `PatShape.Const` first; `failwith` on any other shape.
    let patConstValue (p: PatId) : TConstValue =
        expect "TastAccessor.patConstValue: not a Const pattern" (|PConst|_|) p

    /// A `Union` pattern → the union case name it discriminates on.
    [<return: Struct>]
    let private (|PUnion|_|) (p: PatId) : string voption =
        match patPayload p with
        | PatPayload.Union caseName -> ValueSome caseName
        | _ -> ValueNone

    /// The union case name a `Union` pattern discriminates on. Guard with `patKind` =
    /// `PatShape.Union` first; `failwith` on any other shape.
    let patUnionCaseName (p: PatId) : string =
        expect "TastAccessor.patUnionCaseName: not a Union pattern" (|PUnion|_|) p

    /// A `Record` pattern → its (field-name, sub-pattern) pairs.
    [<return: Struct>]
    let private (|PRecord|_|) (p: PatId) : (string * PatId)[] voption =
        match patPayload p with
        | PatPayload.Record fieldNames -> ValueSome(Array.map2 (fun n sub -> (n, sub)) fieldNames (patChildren p))
        | _ -> ValueNone

    /// The (field-name, sub-pattern) pairs a `Record` pattern binds — the labels
    /// `patChildren` drops. Guard with `patKind` = `PatShape.Record` first; `failwith`
    /// on any other shape.
    let patRecordFields (p: PatId) : (string * PatId)[] =
        expect "TastAccessor.patRecordFields: not a Record pattern" (|PRecord|_|) p

    /// An `EnumCase` pattern → its `EnumCasePatView`.
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

    /// The payload view of an `EnumCase` pattern. Guard with `patKind` =
    /// `PatShape.EnumCase` first; `failwith` on any other shape.
    let patEnumCase (p: PatId) : EnumCasePatView =
        expect "TastAccessor.patEnumCase: not an EnumCase pattern" (|PEnumCase|_|) p

    /// A `TypeTestAs` pattern → its tested-against type `T`.
    [<return: Struct>]
    let private (|PTypeTestAs|_|) (p: PatId) : FrozenType voption =
        match patPayload p with
        | PatPayload.TypeTestAs testTy -> ValueSome testTy
        | _ -> ValueNone

    /// The tested-against type `T` of a `TypeTestAs` pattern (`:? T as x`) — the
    /// `isinst` operand. Distinct from `patTy`, which is the scrutinee's (matched)
    /// type. The bound inner sub-pattern (the `as`-name) is `patChildren.[0]`. Guard
    /// with `patKind` = `PatShape.TypeTestAs` first; `failwith` on any other shape.
    let patTypeTestTestTy (p: PatId) : FrozenType =
        expect "TastAccessor.patTypeTestTestTy: not a TypeTestAs pattern" (|PTypeTestAs|_|) p

    // ── declarations ────────────────────────────────────────────────────────

    /// The shape tag of a declaration node.
    let declKind (d: DeclId) : DeclShape = TastPoolBuilder.declShape d.Pool d.Id

    let private declPayload (d: DeclId) : DeclPayload = TastPoolBuilder.declPayload d.Pool d.Id

    /// The `i`-th expr / pat root of a decl — see `exprChild`.
    let private declExprChild (d: DeclId) (i: int) : ExprId =
        at d (TastPoolBuilder.declExprChildren d.Pool d.Id).[i]

    let private declPatChild (d: DeclId) (i: int) : PatId =
        at d (TastPoolBuilder.declPatChildren d.Pool d.Id).[i]

    /// A `Type` decl → its declaration payload, member/preamble/ctor bodies resolved to
    /// handles.
    [<return: Struct>]
    let private (|DType|_|) (d: DeclId) : TypeDecl voption =
        match declPayload d with
        // The seven body slots are enumerated by `TastConvert` — the same traversal the
        // pool build and drain run — so nothing here re-derives the declaration shape.
        | DeclPayload.Type td -> ValueSome(TastConvert.typeDecl id (at d) td)
        | _ -> ValueNone

    /// The `type`-declaration payload of a `Type` decl (its `Kind`, `Key`, `Name`, …),
    /// its member/preamble/ctor bodies resolved to handles. Guard with `declKind` =
    /// `DeclShape.Type` first; `failwith` on any other shape.
    let declType (d: DeclId) : TypeDecl =
        expect "TastAccessor.declType: not a Type decl" (|DType|_|) d

    /// An `Expression` decl → its body expression and the declared slot type it carries
    /// alongside (the type a value-producing consumer must preserve when reconstructing
    /// the decl).
    [<return: Struct>]
    let (|DExpression|_|) (d: DeclId) : struct (ExprId * FrozenType) voption =
        match declPayload d with
        | DeclPayload.Expression ty -> ValueSome(struct (declExprChild d 0, ty))
        | _ -> ValueNone

    /// The body expression of an `Expression` decl. Guard with `declKind` =
    /// `DeclShape.Expression` first; `failwith` on any other shape.
    let declExpression (d: DeclId) : ExprId =
        let struct (e, _) =
            expect "TastAccessor.declExpression: not an Expression decl" (|DExpression|_|) d

        e

    /// The declared type an `Expression` decl carries alongside its `expr`
    /// (`declExpression`). Guard with `declKind` = `DeclShape.Expression` first;
    /// `failwith` on any other shape.
    let declExpressionTy (d: DeclId) : FrozenType =
        let struct (_, ty) =
            expect "TastAccessor.declExpressionTy: not an Expression decl" (|DExpression|_|) d

        ty

    /// A `Let` decl → its `DeclLetView`.
    [<return: Struct>]
    let (|DLet|_|) (d: DeclId) : DeclLetView voption =
        match declPayload d with
        | DeclPayload.Let p ->
            ValueSome
                {
                    Binding = declPatChild d 0
                    Value = declExprChild d 0
                    IsInline = p.IsInline
                    Ty = p.Ty
                }
        | _ -> ValueNone

    /// The payload view of a `Let` decl. Guard with `declKind` = `DeclShape.Let` first;
    /// `failwith` on any other shape.
    let declLet (d: DeclId) : DeclLetView =
        expect "TastAccessor.declLet: not a Let decl" (|DLet|_|) d

    /// The file's declarations, in source order — the pool roots as handles.
    let roots (pool: PoolBuilder) : DeclId[] =
        TastPoolBuilder.roots pool |> Array.map (fun id -> { Pool = pool; Id = id })

    // ── generic traversal ───────────────────────────────────────────────────
    //
    // The single structural recursion every rewrite / discovery pass shares. In
    // columnar form a child substitution is a ROW COPY with new child ids — one row,
    // no per-case match on the node's shape — and it returns the ORIGINAL id when no
    // child moved, so a walk that touches nothing appends nothing and every id a
    // consumer already cached still names the same node.

    /// Rebuild `e` with `f` applied to each immediate child expression. Its owned
    /// sub-patterns are untouched (a rewrite that must reach them walks
    /// `exprPatChildren` itself).
    let mapChildren (f: ExprId -> ExprId) (e: ExprId) : ExprId =
        let kids = exprChildren e |> Array.map (fun c -> (f c).Id)
        at e (TastPoolBuilder.copyExprWith e.Pool e.Id (fun row -> { row with Children = kids }))

    /// Visit each immediate child expression — the one-shot discovery / free-variable
    /// pre-passes. NOT `mapChildren` with the result thrown away: `mapChildren` copies a
    /// row when a child moves, and a visit must append nothing.
    let iterChildren (f: ExprId -> unit) (e: ExprId) : unit =
        for c in exprChildren e do
            f c

    /// True when any immediate child of `e` satisfies `p` — the exists-over-children
    /// primitive the recursive search predicates build on (mirrors
    /// `FrozenType.existsChild`). `p` is not invoked on further children once one has
    /// matched, so a `p` that recurses short-circuits the descent.
    let existsChild (p: ExprId -> bool) (e: ExprId) : bool = exprChildren e |> Array.exists p

    /// Peel a curried `App` chain into its head and the arguments paired with each
    /// `App` node's *result* type and token. The inverse of `mintAppSpine`.
    let rec collectSpine
        (acc: (ExprId * FrozenType * SyntaxToken) list)
        (e: ExprId)
        : ExprId * (ExprId * FrozenType * SyntaxToken) list =
        match exprKind e with
        | ExprShape.App ->
            let app = exprApp e
            collectSpine ((app.Arg, exprTy e, exprTok e) :: acc) app.Fn
        | _ -> e, acc

    /// Rewrite a curried `App` chain IN PLACE: `fArg` on each argument, `fHead` on the
    /// spine's head. The id-preserving counterpart of `collectSpine` + `mintAppSpine`,
    /// for a rewrite that must decide something about the HEAD (is this call saturated?)
    /// yet leave the spine's own nodes alone: every `App` is a row copy, so a chain whose
    /// head and arguments all stay put keeps every id it already had. Peeling to a list
    /// and re-applying would mint a fresh node per level unconditionally.
    let rec mapSpine (fHead: ExprId -> ExprId) (fArg: ExprId -> ExprId) (e: ExprId) : ExprId =
        match exprKind e with
        | ExprShape.App ->
            let app = exprApp e
            let fn = mapSpine fHead fArg app.Fn
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
        | _ -> fHead e

    // ── minting ─────────────────────────────────────────────────────────────
    //
    // The append side of the seam: a derived node is a ROW, written straight into the
    // pool the site is already working in. Only the shapes a lowering actually
    // SYNTHESISES are here — everything else a rewrite produces is a copy of an
    // existing row (`mapChildren`, `retype`), which preserves the id when nothing moved.
    // Each takes the pool from a handle it is already holding, so no site threads a
    // builder alongside the nodes.

    // The payload IS the shape (`ExprPayload.shape`), so a mint names the node's form
    // once, in the payload it supplies — there is no second tag argument to disagree
    // with it.

    let private mintExpr (pool: PoolBuilder) ty tok children patChildren pl : ExprId =
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
                        VarBinder = ValueNone
                        Payload = pl
                    }
        }

    /// A reference to `binding`. Interning is idempotent in the key, so a reference
    /// minted before its defining pattern exists still lands on that binder's id.
    let mintVar (pool: PoolBuilder) (binding: NodeKey) (ty: FrozenType) (tok: SyntaxToken) : ExprId =
        let binder = TastPoolBuilder.internBinder pool binding

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
                        VarBinder = ValueSome binder
                        Payload = ExprPayload.Var
                    }
        }

    /// `fn arg`, typed with the application's result type.
    let mintApp (fn: ExprId) (arg: ExprId) (ty: FrozenType) (tok: SyntaxToken) : ExprId =
        mintExpr fn.Pool ty tok [| fn.Id; arg.Id |] [||] ExprPayload.App

    /// Re-apply a head to a spine of `(arg, result type, token)` levels — the inverse
    /// of `collectSpine`.
    let mintAppSpine (head: ExprId) (args: (ExprId * FrozenType * SyntaxToken) list) : ExprId =
        List.fold (fun acc (arg, resTy, tok) -> mintApp acc arg resTy tok) head args

    /// `fun param -> body`.
    let mintLambda (param: PatId) (body: ExprId) (ty: FrozenType) (tok: SyntaxToken) : ExprId =
        mintExpr body.Pool ty tok [| body.Id |] [| param.Id |] ExprPayload.Lambda

    /// `receiver.Key args` — an instance call on a project-local member.
    let mintMethodCall
        (receiver: ExprId)
        (key: SymbolKey)
        (via: CallVia<FrozenType>)
        (args: ExprId[])
        (ty: FrozenType)
        (tok: SyntaxToken)
        : ExprId =
        mintExpr
            receiver.Pool
            ty
            tok
            (Array.append [| receiver.Id |] (args |> Array.map (fun a -> a.Id)))
            [||]
            (ExprPayload.MethodCall {| Key = key; Via = via |})

    let private mintPat (pool: PoolBuilder) ty tok children pl : PatId =
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

    /// A simple binder pattern, introducing `binding`.
    let mintNamedPat (pool: PoolBuilder) (binding: NodeKey) (ty: FrozenType) (tok: SyntaxToken) : PatId =
        mintPat pool ty tok [||] (PatPayload.NamedSimple(TastPoolBuilder.internBinder pool binding))

    /// An anonymous `_` pattern.
    let mintWildcardPat (pool: PoolBuilder) (ty: FrozenType) (tok: SyntaxToken) : PatId =
        mintPat pool ty tok [||] PatPayload.Wildcard

    /// A tuple pattern over `items`.
    let mintTuplePat (pool: PoolBuilder) (items: PatId[]) (ty: FrozenType) (tok: SyntaxToken) : PatId =
        mintPat pool ty tok (items |> Array.map (fun i -> i.Id)) PatPayload.Tuple

    /// A top-level `let binding = value` declaration.
    let mintLetDecl (binding: PatId) (value: ExprId) (isInline: bool) (ty: FrozenType) : DeclId =
        {
            Pool = value.Pool
            Id =
                TastPoolBuilder.appendDecl
                    value.Pool
                    {
                        ExprChildren = [| value.Id |]
                        PatChildren = [| binding.Id |]
                        Payload = DeclPayload.Let {| IsInline = isInline; Ty = ty |}
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

    /// Re-author `e` with a different result type — a RETYPE, which touches the type
    /// column and nothing else, and returns `e` itself when the type is unchanged.
    let retype (e: ExprId) (ty: FrozenType) : ExprId =
        at e (TastPoolBuilder.copyExprWith e.Pool e.Id (fun row -> { row with Ty = ty }))

    /// Re-author `e` with different immediate children AND a different result type —
    /// the two-field form of `mapChildren` / `retype`, so a rewrite that changes both
    /// appends ONE row rather than two.
    let retypeWithChildren (e: ExprId) (children: ExprId[]) (ty: FrozenType) : ExprId =
        let kids = children |> Array.map (fun c -> c.Id)

        at e (TastPoolBuilder.copyExprWith e.Pool e.Id (fun row -> { row with Children = kids; Ty = ty }))

    /// Re-author a decl with a different value / body expression — the decl analogue of
    /// `mapChildren`, returning the decl itself when the expression did not move.
    let mapDeclExpr (f: ExprId -> ExprId) (d: DeclId) : DeclId =
        let kids =
            TastPoolBuilder.declExprChildren d.Pool d.Id
            |> Array.map (fun c -> (f (at d c)).Id)

        at d (TastPoolBuilder.copyDeclWith d.Pool d.Id (fun row -> { row with ExprChildren = kids }))
