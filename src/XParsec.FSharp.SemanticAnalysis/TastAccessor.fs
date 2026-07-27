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
// The `…View` records are the payload seam: a node's fields MINUS the child edges and
// the `ty`/`tok`, named by role, so a consumer never positions into `exprChildren` by
// hand. Each is a struct of handles and scalars — pool-agnostic, since the handles
// carry their own pool. The `failwith` guards are the shape contract: read a payload
// only after `exprKind`/`patKind`/`declKind` (or through the matching recognizer).
//
// EVERY view guards on the `*Payload` column, never on the `*Shape` tag — one
// convention, so a reader never has to check which of the two a given accessor used.
// Payload rather than shape because the payload is the column the view is projecting
// FROM in all but a handful of cases, so the guard and the read are one fetch. The
// shape tag is for a consumer's own TOTAL dispatch (the emit routers), where its
// closed enum is what keeps the match exhaustive.
//
// A view with a RECOGNIZER — the accessor in pattern position — is written ONCE, as
// the recognizer, and the eager form derives from it:
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
// A recognizer exists for exactly the shapes a consumer matches in a *partial* dispatch
// (one that ends in a `| _ ->` fall-through). A *total* dispatch over every shape — the
// emit routers (`EmitExpr.buildExpr`, `EmitPattern`, the `Emit.fs` decl loop) — matches
// the `exprKind`/`patKind`/`declKind` tag instead, which the closed
// `ExprShape`/`PatShape`/`DeclShape` enum keeps exhaustive: a new case breaks those
// matches at compile time. `RequireQualifiedAccess` means they are used qualified
// (`TastAccessor.EForTo`).

/// The TAST-shaped accessor. `open` is disallowed so `exprTy`/`patTy` don't collide
/// with the analysis-time `TastWalk` projections of the same name.
[<RequireQualifiedAccess>]
module TastAccessor =

    // The node handles: a dense id in a pool, carried with the pool.
    type ExprId = Handle<ExprPoolId>
    type PatId = Handle<PatPoolId>
    type DeclId = Handle<DeclPoolId>

    /// The `type`-declaration cluster with its member/preamble/ctor BODY slots holding
    /// handles — the shape `TTypeDeclG`'s `'body` parameter exists for. Same spine as
    /// `Frozen.TTypeDecl`, ids in the body slots.
    type TypeDecl = TTypeDeclG<FrozenType, SyntaxToken, ExprId>
    type TypeKind = TTypeKindG<FrozenType, SyntaxToken, ExprId>
    type Class = TClassG<FrozenType, ExprId>
    type TypeMember = TTypeMemberG<FrozenType, ExprId>
    type ClassLet = TClassLetG<FrozenType, ExprId>
    type PreambleEntry = TPreambleEntryG<FrozenType, ExprId>
    type CtorLet = TCtorLetG<FrozenType, ExprId>
    type CtorFieldInit = TCtorFieldInitG<ExprId>
    type SecondaryCtor = TSecondaryCtorG<FrozenType, ExprId>
    type BaseCtorCall = TBaseCtorCallG<FrozenType, ExprId>

    /// The compiled-form cluster with its tuple-group / destructuring patterns held as
    /// handles — the `'pat` instantiation every consumer reads, whether the pats came
    /// from a file's own pool (`peelValRepr` off the frozen lambda spine) or from the
    /// standalone pool an `.fsi` contract's are minted into.
    type StaticParam = StaticParamG<FrozenType, PatId>
    type ArgGroup = ArgGroupG<FrozenType, PatId>
    type ValRepr = ValReprG<FrozenType, PatId>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, PatId>

    /// A sibling id in the same pool. Every child edge resolves through this, so the
    /// pool propagates down a walk without any consumer naming it.
    let inline private at (h: Handle<'a>) (id: 'b) : Handle<'b> = { Pool = h.Pool; Id = id }

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

    /// The constant value carried by a `Const` node. Guard with `exprKind` =
    /// `ExprShape.Const` first; `failwith` on any other shape.
    let exprConstValue (e: ExprId) : TConstValue =
        match payload e with
        | ExprPayload.Const value -> value
        | _ -> failwith "TastAccessor.exprConstValue: not a Const node"

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
        match e with
        | EVar binding -> binding
        | _ -> failwith "TastAccessor.exprVarBinding: not a Var node"

    /// The naming projections of the binder a `Var` node references — what a backend
    /// emits its name from, read off the pool's naming column rather than the key's own
    /// bits. Guard with `exprKind` = `ExprShape.Var` first; `failwith` on any other shape.
    let exprVarNaming (e: ExprId) : BinderNaming =
        match TastPoolBuilder.exprVarBinder e.Pool e.Id with
        | ValueSome b -> TastPoolBuilder.binderNaming e.Pool b
        | ValueNone -> failwith "TastAccessor.exprVarNaming: not a Var node"

    /// The scalar payload of an `ExternalMember` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry. `Receiver` is a payload sub-expression named
    /// by role (the member's target), not a positional `exprChildren` entry.
    [<Struct>]
    type ExternalMemberView =
        {
            Receiver: ExprId voption
            Key: SymbolKey
            MemberName: string
            Storage: MemberStorage
        }

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
        match e with
        | EExternalMember v -> v
        | _ -> failwith "TastAccessor.exprExternalMember: not an ExternalMember node"

    /// The IL opcode string of an `ILIntrinsic` node — the `$N`-templated instruction.
    /// Its `args` are the node's `exprChildren` and its `typeOperand` is carried
    /// separately. Guard with `exprKind` = `ExprShape.ILIntrinsic` first; `failwith` on
    /// any other shape.
    let exprILIntrinsicOpCode (e: ExprId) : string =
        match payload e with
        | ExprPayload.ILIntrinsic p -> p.OpCode
        | _ -> failwith "TastAccessor.exprILIntrinsicOpCode: not an ILIntrinsic node"

    /// The type operand `<T>` an `ILIntrinsic` node carries — the element type of
    /// `newarr`/`ldelem`/`stelem`/`ldobj`, the boxed type of `box`, the zeroed type of
    /// `ilzero` — or `ValueNone` for the type-free arithmetic/`throw`/reinterpret
    /// opcodes. Carried separately from the `args` (`exprChildren`) and the result `ty`
    /// (`exprTy`). Guard with `exprKind` = `ExprShape.ILIntrinsic` first; `failwith` on
    /// any other shape.
    let exprILIntrinsicTypeOperand (e: ExprId) : FrozenType voption =
        match payload e with
        | ExprPayload.ILIntrinsic p -> p.TypeOperand
        | _ -> failwith "TastAccessor.exprILIntrinsicTypeOperand: not an ILIntrinsic node"

    /// The scalar payload of a `Lambda` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry. `Body` is the sole `exprChildren` entry; the
    /// `Param` pattern is not an expression child.
    [<Struct>]
    type LambdaView = { Param: PatId; Body: ExprId }

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
        match e with
        | ELambda v -> v
        | _ -> failwith "TastAccessor.exprLambda: not a Lambda node"

    /// The scalar payload of a `Let` node, minus the `ty`/`tok` that `exprTy`/`exprTok`
    /// already carry. `Value`/`Body` are the two `exprChildren` entries; the `Binding`
    /// pattern is not an expression child.
    [<Struct>]
    type LetView =
        {
            Binding: PatId
            Value: ExprId
            Body: ExprId
        }

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
        match e with
        | ELet v -> v
        | _ -> failwith "TastAccessor.exprLet: not a Let node"

    /// The scalar payload of an `Assignment` node (`lhs <- rhs`), minus the `ty`/`tok`
    /// that `exprTy`/`exprTok` already carry — the same two nodes `exprChildren` yields,
    /// named by role.
    [<Struct>]
    type AssignmentView = { Lhs: ExprId; Rhs: ExprId }

    /// The payload view of an `Assignment` node. Guard with `exprKind` =
    /// `ExprShape.Assignment` first; `failwith` on any other shape.
    let exprAssignment (e: ExprId) : AssignmentView =
        match payload e with
        | ExprPayload.Assignment ->
            {
                Lhs = exprChild e 0
                Rhs = exprChild e 1
            }
        | _ -> failwith "TastAccessor.exprAssignment: not an Assignment node"

    /// The scalar payload of an `IfThenElse` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry — the three branch nodes `exprChildren` yields,
    /// named by role.
    [<Struct>]
    type IfThenElseView =
        {
            Cond: ExprId
            ThenExpr: ExprId
            ElseExpr: ExprId
        }

    /// The payload view of an `IfThenElse` node. Guard with `exprKind` =
    /// `ExprShape.IfThenElse` first; `failwith` on any other shape.
    let exprIfThenElse (e: ExprId) : IfThenElseView =
        match payload e with
        | ExprPayload.IfThenElse ->
            {
                Cond = exprChild e 0
                ThenExpr = exprChild e 1
                ElseExpr = exprChild e 2
            }
        | _ -> failwith "TastAccessor.exprIfThenElse: not an IfThenElse node"

    /// The scalar payload of an `External` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry.
    [<Struct>]
    type ExternalView =
        {
            CompiledName: string
            Key: SymbolKey voption
        }

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
        match e with
        | EExternal v -> v
        | _ -> failwith "TastAccessor.exprExternal: not an External node"

    /// The scalar payload of an `App` node (`fn arg`), minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry — the same two nodes `exprChildren` yields, named
    /// by role.
    [<Struct>]
    type AppView = { Fn: ExprId; Arg: ExprId }

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
        match e with
        | EApp v -> v
        | _ -> failwith "TastAccessor.exprApp: not an App node"

    /// The (field-name, value-expression) pairs a `RecordCons` literal assigns, in source
    /// order — the labels `exprChildren` drops. Guard with `exprKind` =
    /// `ExprShape.RecordCons` first; `failwith` on any other shape.
    let exprRecordConsFields (e: ExprId) : (string * ExprId)[] =
        match payload e with
        | ExprPayload.RecordCons fieldNames -> Array.map2 (fun n v -> (n, v)) fieldNames (exprChildren e)
        | _ -> failwith "TastAccessor.exprRecordConsFields: not a RecordCons node"

    /// The scalar payload of a `RecordClone` node (`{ source with … }`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry. `Overrides` are the
    /// (field-name, replacement) pairs — the labels `exprChildren` drops.
    [<Struct>]
    type RecordCloneView =
        {
            Source: ExprId
            Overrides: (string * ExprId)[]
        }

    /// The payload view of a `RecordClone` node. Guard with `exprKind` =
    /// `ExprShape.RecordClone` first; `failwith` on any other shape.
    let exprRecordClone (e: ExprId) : RecordCloneView =
        match payload e with
        | ExprPayload.RecordClone overrideNames ->
            let es = exprChildren e

            {
                Source = es.[0]
                Overrides = Array.map2 (fun n v -> (n, v)) overrideNames es.[1..]
            }
        | _ -> failwith "TastAccessor.exprRecordClone: not a RecordClone node"

    /// The scalar payload of a `FieldGet` node (`receiver.FieldName`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry. `Receiver` is the sole
    /// `exprChildren` entry, named by role.
    [<Struct>]
    type FieldGetView = { Receiver: ExprId; FieldName: string }

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
        match e with
        | EFieldGet v -> v
        | _ -> failwith "TastAccessor.exprFieldGet: not a FieldGet node"

    /// The scalar payload of a `FieldSet` node (`receiver.FieldName <- value`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry — the two nodes `exprChildren`
    /// yields, named by role, plus the field label.
    [<Struct>]
    type FieldSetView =
        {
            Receiver: ExprId
            FieldName: string
            Value: ExprId
        }

    /// The payload view of a `FieldSet` node. Guard with `exprKind` =
    /// `ExprShape.FieldSet` first; `failwith` on any other shape.
    let exprFieldSet (e: ExprId) : FieldSetView =
        match payload e with
        | ExprPayload.FieldSet fieldName ->
            {
                Receiver = exprChild e 0
                FieldName = fieldName
                Value = exprChild e 1
            }
        | _ -> failwith "TastAccessor.exprFieldSet: not a FieldSet node"

    /// The union case name a `UnionCons` node constructs. Its `args` are the node's
    /// `exprChildren` and its `ty` is `exprTy`. Guard with `exprKind` =
    /// `ExprShape.UnionCons` first; `failwith` on any other shape.
    let exprUnionConsCaseName (e: ExprId) : string =
        match payload e with
        | ExprPayload.UnionCons caseName -> caseName
        | _ -> failwith "TastAccessor.exprUnionConsCaseName: not a UnionCons node"

    /// The class name a `New` node constructs. Its `args` are the node's `exprChildren`
    /// and its `ty` is `exprTy`. Guard with `exprKind` = `ExprShape.New` first; `failwith`
    /// on any other shape.
    let exprNewClassName (e: ExprId) : string =
        match payload e with
        | ExprPayload.New p -> p.ClassName
        | _ -> failwith "TastAccessor.exprNewClassName: not a New node"

    /// The recorded overload identity a `New` node's front end chose — the key that
    /// disambiguates a same-arity external-ctor candidate set (`ValueNone` when arity
    /// alone suffices). Guard with `exprKind` = `ExprShape.New` first; `failwith` on
    /// any other shape.
    let exprNewChosenCtor (e: ExprId) : SymbolKey voption =
        match payload e with
        | ExprPayload.New p -> p.Key
        | _ -> failwith "TastAccessor.exprNewChosenCtor: not a New node"

    /// The scalar payload of a `PropertyGet` node — the receiver, the resolved member
    /// key, and the dispatch `Via`, minus the `ty`/`tok` the node also carries.
    /// `Receiver` is the sole `exprChildren` entry, named by role. `Via` distinguishes a
    /// grounded self/base access from a `constrained.`-dispatched typar-interface one
    /// (`CallVia.Interface`) — a distinction the CLR backend dispatches on; a target that
    /// ignores it simply does not read the field.
    [<Struct>]
    type PropertyGetView =
        {
            Receiver: ExprId
            Key: SymbolKey
            Via: CallVia<FrozenType>
        }

    /// The payload view of a `PropertyGet` node. Guard with `exprKind` =
    /// `ExprShape.PropertyGet` first; `failwith` on any other shape.
    let exprPropertyGet (e: ExprId) : PropertyGetView =
        match payload e with
        | ExprPayload.PropertyGet p ->
            {
                Receiver = exprChild e 0
                Key = p.Key
                Via = p.Via
            }
        | _ -> failwith "TastAccessor.exprPropertyGet: not a PropertyGet node"

    /// The scalar payload of a `MethodCall` node — the receiver, the resolved member key,
    /// the dispatch `Via`, and the argument expressions, minus the `ty`/`tok` the node also
    /// carries. `Args` is ONLY the `args` field — NOT `exprChildren` (which merges the
    /// receiver in ahead of the args). `Via` distinguishes a grounded self/base call from a
    /// `constrained.`-dispatched typar-interface one (`CallVia.Interface`) — a distinction
    /// the CLR backend dispatches on; a target that ignores it does not read it.
    [<Struct>]
    type MethodCallView =
        {
            Receiver: ExprId
            Key: SymbolKey
            Via: CallVia<FrozenType>
            Args: EqArray<ExprId>
        }

    /// The payload view of a `MethodCall` node. Guard with `exprKind` =
    /// `ExprShape.MethodCall` first; `failwith` on any other shape.
    let exprMethodCall (e: ExprId) : MethodCallView =
        match payload e with
        | ExprPayload.MethodCall p ->
            let es = exprChildren e

            {
                Receiver = es.[0]
                Key = p.Key
                Via = p.Via
                Args = EqArray.ofArray es.[1..]
            }
        | _ -> failwith "TastAccessor.exprMethodCall: not a MethodCall node"

    /// The resolved member key of a `StaticPropertyGet` node. Guard with `exprKind` =
    /// `ExprShape.StaticPropertyGet` first; `failwith` on any other shape.
    let exprStaticPropertyGetKey (e: ExprId) : SymbolKey =
        match payload e with
        | ExprPayload.StaticPropertyGet key -> key
        | _ -> failwith "TastAccessor.exprStaticPropertyGetKey: not a StaticPropertyGet node"

    /// The scalar payload of a `StaticFieldGet` node — the declaring class key and the
    /// backing-field name, minus the `ty`/`tok` the node also carries.
    [<Struct>]
    type StaticFieldGetView = { Key: SymbolKey; FieldName: string }

    /// The payload view of a `StaticFieldGet` node. Guard with `exprKind` =
    /// `ExprShape.StaticFieldGet` first; `failwith` on any other shape.
    let exprStaticFieldGet (e: ExprId) : StaticFieldGetView =
        match payload e with
        | ExprPayload.StaticFieldGet p ->
            {
                Key = p.DeclKey
                FieldName = p.FieldName
            }
        | _ -> failwith "TastAccessor.exprStaticFieldGet: not a StaticFieldGet node"

    /// The scalar payload of a `StaticFieldSet` node — the declaring class key, the
    /// backing-field name, and the stored value, minus the `ty`/`tok` the node also
    /// carries. `Value` is the sole `exprChildren` entry, named by role.
    [<Struct>]
    type StaticFieldSetView =
        {
            Key: SymbolKey
            FieldName: string
            Value: ExprId
        }

    /// The payload view of a `StaticFieldSet` node. Guard with `exprKind` =
    /// `ExprShape.StaticFieldSet` first; `failwith` on any other shape.
    let exprStaticFieldSet (e: ExprId) : StaticFieldSetView =
        match payload e with
        | ExprPayload.StaticFieldSet p ->
            {
                Key = p.DeclKey
                FieldName = p.FieldName
                Value = exprChild e 0
            }
        | _ -> failwith "TastAccessor.exprStaticFieldSet: not a StaticFieldSet node"

    /// The resolved member key of a `StaticMethodCall` node. Its `args` are the node's
    /// `exprChildren`. Guard with `exprKind` = `ExprShape.StaticMethodCall` first;
    /// `failwith` on any other shape.
    let exprStaticMethodCallKey (e: ExprId) : SymbolKey =
        match payload e with
        | ExprPayload.StaticMethodCall key -> key
        | _ -> failwith "TastAccessor.exprStaticMethodCallKey: not a StaticMethodCall node"

    /// One arm of a `Match` / `TryWith`, its pattern and guard/body expressions held as
    /// handles — the `'pat`/`'e` instantiation of the one arm shape (`TMatchArmG`), so a
    /// consumer that scopes the arm's binders over its guard and body reads the same
    /// record whichever domain it is in.
    type Arm = TMatchArmG<PatId, ExprId>

    /// The children of the arms, re-nested — `ExprPayload.arms`, the walk shared with the
    /// pool drain, driven off this node's child columns. `lead` is how many leading expr
    /// children belong to the node itself rather than an arm (`Match`'s scrutinee /
    /// `TryWith`'s body).
    let private armsOf (e: ExprId) (guardPresent: bool[]) (lead: int) : Arm[] =
        let es = exprChildren e
        let ps = exprPatChildren e
        let mutable ei = lead
        let mutable pi = 0

        let nextE () =
            let x = es.[ei] in
            ei <- ei + 1
            x

        let nextP () =
            let x = ps.[pi] in
            pi <- pi + 1
            x

        ExprPayload.arms guardPresent nextP nextE

    /// The scalar payload of a `Match` node — the scrutinee and the arms, minus the
    /// `ty`/`tok` the node also carries.
    [<Struct>]
    type MatchView = { Scrutinee: ExprId; Arms: Arm[] }

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
        match e with
        | EMatch v -> v
        | _ -> failwith "TastAccessor.exprMatch: not a Match node"

    /// The scalar payload of a `TryWith` node — the guarded body and the handler arms,
    /// minus the `ty`/`tok` the node also carries. `Body` is the sole positional
    /// `exprChildren` head.
    [<Struct>]
    type TryWithView = { Body: ExprId; Arms: Arm[] }

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
        match e with
        | ETryWith v -> v
        | _ -> failwith "TastAccessor.exprTryWith: not a TryWith node"

    /// The scalar payload of a `TryFinally` node (`try Body finally Cleanup`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry — the two `exprChildren` entries,
    /// named by role. `Body` carries the node's `ty`; `Cleanup` is unit.
    [<Struct>]
    type TryFinallyView = { Body: ExprId; Cleanup: ExprId }

    /// The payload view of a `TryFinally` node. Guard with `exprKind` = `ExprShape.TryFinally`
    /// first; `failwith` on any other shape.
    let exprTryFinally (e: ExprId) : TryFinallyView =
        match payload e with
        | ExprPayload.TryFinally ->
            {
                Body = exprChild e 0
                Cleanup = exprChild e 1
            }
        | _ -> failwith "TastAccessor.exprTryFinally: not a TryFinally node"

    /// The scalar payload of a `While` node (`while Cond do Body`), minus the `ty`/`tok`
    /// that `exprTy`/`exprTok` already carry — the two nodes `exprChildren` yields, named
    /// by role.
    [<Struct>]
    type WhileView = { Cond: ExprId; Body: ExprId }

    /// The payload view of a `While` node. Guard with `exprKind` = `ExprShape.While` first;
    /// `failwith` on any other shape.
    let exprWhile (e: ExprId) : WhileView =
        match payload e with
        | ExprPayload.While ->
            {
                Cond = exprChild e 0
                Body = exprChild e 1
            }
        | _ -> failwith "TastAccessor.exprWhile: not a While node"

    /// The scalar payload of a `ForTo` node (`for Var = StartExpr to EndExpr do Body`),
    /// minus the `identTok`/`ty`/`tok` the node also carries. `StartExpr`/`EndExpr`/`Body`
    /// are the three `exprChildren` entries, named by role; `Var` is the loop binder,
    /// which has no pattern node behind it.
    [<Struct>]
    type ForToView =
        {
            Var: NodeKey
            StartExpr: ExprId
            EndExpr: ExprId
            Body: ExprId
        }

    /// A `ForTo` node → its `ForToView`.
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

    /// The payload view of a `ForTo` node. Guard with `exprKind` = `ExprShape.ForTo`
    /// first; `failwith` on any other shape.
    let exprForTo (e: ExprId) : ForToView =
        match e with
        | EForTo v -> v
        | _ -> failwith "TastAccessor.exprForTo: not a ForTo node"

    /// The scalar payload of a `ForIn` node (`for Pat in Source do Body`), minus the
    /// `ty`/`tok` the node also carries. `Source`/`Body` are the two `exprChildren`
    /// entries; `Pat` is a pattern (not an expression child) and `Enumerator` records
    /// how the source yields its enumerator (the front-end resolution codegen dispatches on).
    [<Struct>]
    type ForInView =
        {
            Pat: PatId
            Source: ExprId
            Body: ExprId
            Enumerator: Frozen.ForInEnumerator
        }

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
        match e with
        | EForIn v -> v
        | _ -> failwith "TastAccessor.exprForIn: not a ForIn node"

    /// The scalar payload of a `Use` node (`use Binding = Value in Body`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry. `Value`/`Body` are the two
    /// `exprChildren` entries; `Binding` is a pattern (not an expression child) and
    /// `Dispose` the resolved disposal path.
    [<Struct>]
    type UseView =
        {
            Binding: PatId
            Value: ExprId
            Body: ExprId
            Dispose: Disposal
        }

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
        match e with
        | EUse v -> v
        | _ -> failwith "TastAccessor.exprUse: not a Use node"

    /// The format cluster with its sub-expressions held as handles — the `'e`
    /// instantiation of the one sink / segment / dyn-hole shape, so a formatter replays
    /// the same records here as in the tree domain.
    type FormatSink = FormatSinkG<ExprId>
    type FormatSeg = FormatSegG<FrozenType, SyntaxToken, ExprId>
    type DynFormatHole = DynFormatHoleG<FrozenType, SyntaxToken, ExprId>

    /// The scalar payload of a `Format` node — the sink and the interleaved
    /// literal/hole segments, minus the `ty`/`tok` the node also carries.
    [<Struct>]
    type FormatView =
        {
            Sink: FormatSink
            Segments: FormatSeg[]
        }

    /// The payload view of a `Format` node — `ExprPayload.format`, the re-nesting shared
    /// with the pool drain, driven off this node's child column. Guard with `exprKind` =
    /// `ExprShape.Format` first; `failwith` on any other shape.
    let exprFormat (e: ExprId) : FormatView =
        match payload e with
        | ExprPayload.Format p ->
            let es = exprChildren e
            let mutable i = 0

            let next () =
                let x = es.[i]
                i <- i + 1
                x

            let sink, segments = ExprPayload.format p.Sink p.Segments next
            { Sink = sink; Segments = segments }
        | _ -> failwith "TastAccessor.exprFormat: not a Format node"

    /// The tested-against type `T` of a `TypeTest` node (`e :? T`) — the `isinst`
    /// operand. Distinct from `exprTy`, which is always `bool` (the test's result). The
    /// tested `source` is the sole `exprChildren` entry. Guard with `exprKind` =
    /// `ExprShape.TypeTest` first; `failwith` on any other shape.
    let exprTypeTestTestTy (e: ExprId) : FrozenType =
        match payload e with
        | ExprPayload.TypeTest testTy -> testTy
        | _ -> failwith "TastAccessor.exprTypeTestTestTy: not a TypeTest node"

    /// The fallback (dynamic) default expression of a `StaticOptimization` node — the
    /// branch F# selects when no type-specialized clause's constraints hold, and the LAST
    /// of the node's `exprChildren` (the clause bodies precede it). It is also the only
    /// branch codegen emits: reaching a backend unresolved means inline expansion never
    /// pinned an operand type. Guard with `exprKind` = `ExprShape.StaticOptimization`
    /// first; `failwith` on any other shape.
    let exprStaticOptimizationDefault (e: ExprId) : ExprId =
        match payload e with
        | ExprPayload.StaticOptimization _ -> exprChild e (exprChildCount e - 1)
        | _ -> failwith "TastAccessor.exprStaticOptimizationDefault: not a StaticOptimization node"

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
        | PatPayload.NamedSimple binding -> ValueSome binding
        | _ -> ValueNone

    /// A `NamedSimple` pattern → the single binder it introduces (`patBinder`, which is
    /// `ValueSome` exactly for that shape).
    [<return: Struct>]
    let (|PNamed|_|) (p: PatId) : NodeKey voption = patBinder p

    /// The POSITIONAL identity of the binder a `NamedSimple` pattern introduces — the id
    /// the pool's `BinderId`-keyed side tables are keyed by, so a consumer holding the
    /// defining node looks its entry up directly instead of round-tripping through the
    /// binder's `NodeKey`. `ValueNone` for a pattern that introduces no binder; also
    /// `ValueNone` — never a minted id — if the pool never interned the key, so a lookup
    /// keyed on this can only ever name a binder the tree bears.
    let patBinderId (p: PatId) : BinderId voption =
        match patPayload p with
        | PatPayload.NamedSimple binding -> TastPoolBuilder.tryBinderId p.Pool binding
        | _ -> ValueNone

    /// A `NamedSimple` pattern → the POSITIONAL id of the binder it introduces
    /// (`patBinderId`) — the `PNamed` to reach for when the binder is about to be looked
    /// up in a `BinderId`-keyed side table.
    [<return: Struct>]
    let (|PNamedId|_|) (p: PatId) : BinderId voption = patBinderId p

    /// The naming projections of the binder a `NamedSimple` pattern introduces.
    ///
    /// Straight off the key, NOT through the pool's naming column: `BinderNaming.ofKey`
    /// is that column's sole constructor, so the two agree by construction and the
    /// detour would only be a chance to MINT (`internBinder`) a binder from a read —
    /// which is exactly what `patBinderId` next door refuses to do. `exprVarNaming` is
    /// the case that genuinely needs the column: a `Var` names its binder by id and has
    /// no key to project from.
    let patBinderNaming (p: PatId) : BinderNaming voption =
        match patPayload p with
        | PatPayload.NamedSimple binding -> ValueSome(BinderNaming.ofKey binding)
        | _ -> ValueNone

    /// The constant value carried by a `Const` pattern. Guard with `patKind` =
    /// `PatShape.Const` first; `failwith` on any other shape.
    let patConstValue (p: PatId) : TConstValue =
        match patPayload p with
        | PatPayload.Const value -> value
        | _ -> failwith "TastAccessor.patConstValue: not a Const pattern"

    /// The union case name a `Union` pattern discriminates on. Guard with `patKind` =
    /// `PatShape.Union` first; `failwith` on any other shape.
    let patUnionCaseName (p: PatId) : string =
        match patPayload p with
        | PatPayload.Union caseName -> caseName
        | _ -> failwith "TastAccessor.patUnionCaseName: not a Union pattern"

    /// The (field-name, sub-pattern) pairs a `Record` pattern binds — the labels
    /// `patChildren` drops. Guard with `patKind` = `PatShape.Record` first; `failwith`
    /// on any other shape.
    let patRecordFields (p: PatId) : (string * PatId)[] =
        match patPayload p with
        | PatPayload.Record fieldNames -> Array.map2 (fun n sub -> (n, sub)) fieldNames (patChildren p)
        | _ -> failwith "TastAccessor.patRecordFields: not a Record pattern"

    /// The scalar payload of an `EnumCase` pattern — the case's `enumKey`/`caseName`
    /// identity, minus the `ty`/`tok` that `patTy`/`patTok` already carry.
    [<Struct>]
    type EnumCasePatView =
        { EnumKey: SymbolKey; CaseName: string }

    /// The payload view of an `EnumCase` pattern. Guard with `patKind` =
    /// `PatShape.EnumCase` first; `failwith` on any other shape.
    let patEnumCase (p: PatId) : EnumCasePatView =
        match patPayload p with
        | PatPayload.EnumCase v ->
            {
                EnumKey = v.EnumKey
                CaseName = v.CaseName
            }
        | _ -> failwith "TastAccessor.patEnumCase: not an EnumCase pattern"

    /// The tested-against type `T` of a `TypeTestAs` pattern (`:? T as x`) — the
    /// `isinst` operand. Distinct from `patTy`, which is the scrutinee's (matched)
    /// type. The bound inner sub-pattern (the `as`-name) is `patChildren.[0]`. Guard
    /// with `patKind` = `PatShape.TypeTestAs` first; `failwith` on any other shape.
    let patTypeTestTestTy (p: PatId) : FrozenType =
        match patPayload p with
        | PatPayload.TypeTestAs testTy -> testTy
        | _ -> failwith "TastAccessor.patTypeTestTestTy: not a TypeTestAs pattern"

    // ── declarations ────────────────────────────────────────────────────────

    /// The shape tag of a declaration node.
    let declKind (d: DeclId) : DeclShape = TastPoolBuilder.declShape d.Pool d.Id

    let private declPayload (d: DeclId) : DeclPayload = TastPoolBuilder.declPayload d.Pool d.Id

    /// The `i`-th expr / pat root of a decl — see `exprChild`.
    let private declExprChild (d: DeclId) (i: int) : ExprId =
        at d (TastPoolBuilder.declExprChildren d.Pool d.Id).[i]

    let private declPatChild (d: DeclId) (i: int) : PatId =
        at d (TastPoolBuilder.declPatChildren d.Pool d.Id).[i]

    /// The `type`-declaration payload of a `Type` decl (its `Kind`, `Key`, `Name`, …),
    /// its member/preamble/ctor bodies resolved to handles. Guard with `declKind` =
    /// `DeclShape.Type` first; `failwith` on any other shape.
    let declType (d: DeclId) : TypeDecl =
        match declPayload d with
        // The seven body slots are enumerated by `TastConvert` — the same traversal the
        // pool build and drain run — so nothing here re-derives the declaration shape.
        | DeclPayload.Type td -> TastConvert.typeDecl id (at d) td
        | _ -> failwith "TastAccessor.declType: not a Type decl"

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
        match d with
        | DExpression(e, _) -> e
        | _ -> failwith "TastAccessor.declExpression: not an Expression decl"

    /// The declared type an `Expression` decl carries alongside its `expr`
    /// (`declExpression`). Guard with `declKind` = `DeclShape.Expression` first;
    /// `failwith` on any other shape.
    let declExpressionTy (d: DeclId) : FrozenType =
        match d with
        | DExpression(_, ty) -> ty
        | _ -> failwith "TastAccessor.declExpressionTy: not an Expression decl"

    /// The payload of a `Let` decl. `Binding` is the bound pattern, `Value` its
    /// initializer, `IsInline` whether the binding expands per call site, `Ty` the
    /// binding's declared type (the slot type a value-producing consumer allocates for
    /// it — distinct from `exprTy Value` for a destructuring binding).
    [<Struct>]
    type DeclLetView =
        {
            Binding: PatId
            Value: ExprId
            IsInline: bool
            Ty: FrozenType
        }

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
        match d with
        | DLet v -> v
        | _ -> failwith "TastAccessor.declLet: not a Let decl"

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

    /// `mapChildren` with the result discarded — the one-shot discovery /
    /// free-variable pre-passes.
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
        TastPoolBuilder.internBinder pool binding |> ignore
        mintPat pool ty tok [||] (PatPayload.NamedSimple binding)

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
