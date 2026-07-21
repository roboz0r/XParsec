namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The frozen-TAST accessor seam (frozen-soa-cache-plan.md, Phase B, step B.1).
//
// A single TAST-shaped access API — `exprKind`, `exprChildren`, `patBinder`, … —
// that both the current `Frozen.TastFile` DU and the later id-indexable pools can
// back. Consumers (codegen, `FrozenSignature`) migrate onto it while it is a thin
// projection over the DU (steps B.2…B.k); the final step (B.k+5) re-points the
// bodies at the pools, so the consumers never move again.
//
// This first cut is DU-backed and *unused*: it exists so the byte-layout of the
// pools can be designed against the real access pattern, and so the migration has
// a stable seam to move onto. The gate is only that it compiles.
//
// Deliberately NOT here yet:
//   - `binderName`: the plan lists it, but the sole existing implementation lives
//     in the JS backend (`JsEmitHelpers.binderName`) and reads a `NodeKey`'s naming
//     bits directly. Re-homing it here (so it reads a pool naming integer instead)
//     is B.k+4 — doing it now would only duplicate that body. `patBinder` already
//     surfaces the binder identity naming is computed from.
//   - Decl-level field accessors: `FrozenSignature` (the decl consumer) migrates
//     late in the B.2…B.k sequence; `declShape` is provided as the entry tag, its
//     payload accessors accrete with that migration.

/// The post-freeze shape tag of a `Frozen.TExpr` node — one case per `TExprG` case.
/// This is the accessor's answer to `exprKind`. It is NOT `NodeKind`: `NodeKind` is
/// the pre-freeze CST content-address role, which freeze dissolves (plan § *Freeze
/// regime*). The case names mirror `TExprG` (documented there); a new `TExprG` case
/// makes the exhaustive matches in `TastAccessor` fail to compile, so this stays in
/// lockstep.
[<RequireQualifiedAccess>]
type ExprShape =
    | Const
    | Var
    | External
    | Lambda
    | App
    | Let
    | Use
    | IfThenElse
    | Tuple
    | Sequential
    | While
    | ForTo
    | ForIn
    | Match
    | TryWith
    | TryFinally
    | Assignment
    | Null
    | Range
    | RecordCons
    | RecordClone
    | FieldGet
    | FieldSet
    | UnionCons
    | New
    | MethodCall
    | PropertyGet
    | StaticMethodCall
    | StaticPropertyGet
    | StaticFieldGet
    | StaticFieldSet
    | ExternalMember
    | Format
    | ILIntrinsic
    | StaticOptimization
    | Upcast
    | Downcast
    | TypeTest
    | TraitCall

/// The post-freeze shape tag of a `Frozen.TPat` node — one case per `TPatG` case.
/// The accessor's answer to `patKind` (mirrors `ExprShape`'s relationship to
/// `TExprG`).
[<RequireQualifiedAccess>]
type PatShape =
    | NamedSimple
    | Wildcard
    | Tuple
    | Const
    | Record
    | Union
    | TypeTestAs
    | Null
    | EnumCase
    | Or

/// The post-freeze shape tag of a `Frozen.TDecl` node — one case per `TDeclG` case.
[<RequireQualifiedAccess>]
type DeclShape =
    | Let
    | Expression
    | Type

/// The TAST-shaped accessor. Every function is a pure projection of the frozen
/// tree; under DU backing it matches the DU, under pool backing (B.k+5) it will
/// read a column. `open` is disallowed so `exprTy`/`patTy` don't collide with the
/// analysis-time `TastWalk` projections they delegate to.
[<RequireQualifiedAccess>]
module TastAccessor =

    // The node handles. Transparent aliases over the DU today; when the backing
    // flips to pools these become dense-id handles (a struct over `int`) and the
    // accessor bodies below are what change — consumers, typed against these, do
    // not. That is the whole point of routing every access through this seam.
    type ExprId = Frozen.TExpr
    type PatId = Frozen.TPat
    type DeclId = Frozen.TDecl

    /// The shape tag of an expression node.
    let exprKind (e: ExprId) : ExprShape =
        match e with
        | TExprG.Const _ -> ExprShape.Const
        | TExprG.Var _ -> ExprShape.Var
        | TExprG.External _ -> ExprShape.External
        | TExprG.Lambda _ -> ExprShape.Lambda
        | TExprG.App _ -> ExprShape.App
        | TExprG.Let _ -> ExprShape.Let
        | TExprG.Use _ -> ExprShape.Use
        | TExprG.IfThenElse _ -> ExprShape.IfThenElse
        | TExprG.Tuple _ -> ExprShape.Tuple
        | TExprG.Sequential _ -> ExprShape.Sequential
        | TExprG.While _ -> ExprShape.While
        | TExprG.ForTo _ -> ExprShape.ForTo
        | TExprG.ForIn _ -> ExprShape.ForIn
        | TExprG.Match _ -> ExprShape.Match
        | TExprG.TryWith _ -> ExprShape.TryWith
        | TExprG.TryFinally _ -> ExprShape.TryFinally
        | TExprG.Assignment _ -> ExprShape.Assignment
        | TExprG.Null _ -> ExprShape.Null
        | TExprG.Range _ -> ExprShape.Range
        | TExprG.RecordCons _ -> ExprShape.RecordCons
        | TExprG.RecordClone _ -> ExprShape.RecordClone
        | TExprG.FieldGet _ -> ExprShape.FieldGet
        | TExprG.FieldSet _ -> ExprShape.FieldSet
        | TExprG.UnionCons _ -> ExprShape.UnionCons
        | TExprG.New _ -> ExprShape.New
        | TExprG.MethodCall _ -> ExprShape.MethodCall
        | TExprG.PropertyGet _ -> ExprShape.PropertyGet
        | TExprG.StaticMethodCall _ -> ExprShape.StaticMethodCall
        | TExprG.StaticPropertyGet _ -> ExprShape.StaticPropertyGet
        | TExprG.StaticFieldGet _ -> ExprShape.StaticFieldGet
        | TExprG.StaticFieldSet _ -> ExprShape.StaticFieldSet
        | TExprG.ExternalMember _ -> ExprShape.ExternalMember
        | TExprG.Format _ -> ExprShape.Format
        | TExprG.ILIntrinsic _ -> ExprShape.ILIntrinsic
        | TExprG.StaticOptimization _ -> ExprShape.StaticOptimization
        | TExprG.Upcast _ -> ExprShape.Upcast
        | TExprG.Downcast _ -> ExprShape.Downcast
        | TExprG.TypeTest _ -> ExprShape.TypeTest
        | TExprG.TraitCall _ -> ExprShape.TraitCall

    /// The node's result type. Delegates to the (generic) analysis-time enumeration.
    let exprTy (e: ExprId) : FrozenType = TastWalk.exprTy e

    /// The node's source-anchor token.
    let exprTok (e: ExprId) : SyntaxToken = TastWalk.exprTok e

    /// The immediate child *expressions*, in evaluation order — the recursion spine
    /// a generic walk (free-vars, closure discovery) follows. Sub-patterns are NOT
    /// children (see `patChildren`); composite carriers with no node identity of
    /// their own (match arms, format segments, static-opt clauses) are descended
    /// into so every reachable sub-expression appears exactly once.
    let exprChildren (e: ExprId) : ExprId[] =
        let acc = ResizeArray<ExprId>()

        match e with
        | TExprG.Const _
        | TExprG.Var _
        | TExprG.External _
        | TExprG.Null _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _ -> ()
        | TExprG.Lambda(body = body) -> acc.Add body
        | TExprG.App(fn = fn; arg = arg) ->
            acc.Add fn
            acc.Add arg
        | TExprG.Let(value = value; body = body) ->
            acc.Add value
            acc.Add body
        | TExprG.Use(value = value; body = body) ->
            acc.Add value
            acc.Add body
        | TExprG.IfThenElse(cond = cond; thenExpr = thenExpr; elseExpr = elseExpr) ->
            acc.Add cond
            acc.Add thenExpr
            acc.Add elseExpr
        | TExprG.Tuple(items = items)
        | TExprG.Sequential(items = items) ->
            for x in items do
                acc.Add x
        | TExprG.While(cond = cond; body = body) ->
            acc.Add cond
            acc.Add body
        | TExprG.ForTo(startExpr = startExpr; endExpr = endExpr; body = body) ->
            acc.Add startExpr
            acc.Add endExpr
            acc.Add body
        | TExprG.ForIn(source = source; body = body) ->
            acc.Add source
            acc.Add body
        | TExprG.Match(scrutinee = scrutinee; arms = arms) ->
            acc.Add scrutinee

            for arm in arms do
                match arm.Guard with
                | Some g -> acc.Add g
                | None -> ()

                acc.Add arm.Body
        | TExprG.TryWith(body = body; arms = arms) ->
            acc.Add body

            for arm in arms do
                match arm.Guard with
                | Some g -> acc.Add g
                | None -> ()

                acc.Add arm.Body
        | TExprG.TryFinally(body = body; cleanup = cleanup) ->
            acc.Add body
            acc.Add cleanup
        | TExprG.Assignment(lhs = lhs; rhs = rhs) ->
            acc.Add lhs
            acc.Add rhs
        | TExprG.Range(startExpr = startExpr; step = step; stopExpr = stopExpr) ->
            acc.Add startExpr

            match step with
            | Some s -> acc.Add s
            | None -> ()

            acc.Add stopExpr
        | TExprG.RecordCons(fields = fields) ->
            for (_, v) in fields do
                acc.Add v
        | TExprG.RecordClone(source = source; overrides = overrides) ->
            acc.Add source

            for (_, v) in overrides do
                acc.Add v
        | TExprG.FieldGet(receiver = receiver) -> acc.Add receiver
        | TExprG.FieldSet(receiver = receiver; value = value) ->
            acc.Add receiver
            acc.Add value
        | TExprG.UnionCons(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.New(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.MethodCall(receiver = receiver; args = args) ->
            acc.Add receiver

            for x in args do
                acc.Add x
        | TExprG.PropertyGet(receiver = receiver) -> acc.Add receiver
        | TExprG.StaticMethodCall(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.StaticFieldSet(value = value) -> acc.Add value
        | TExprG.ExternalMember(receiver = receiver) ->
            match receiver with
            | ValueSome r -> acc.Add r
            | ValueNone -> ()
        | TExprG.Format(sink = sink; segments = segments) ->
            match sink with
            | FormatSinkG.ToWriter(writer = writer) -> acc.Add writer
            | FormatSinkG.ToBuilder builder -> acc.Add builder
            | FormatSinkG.ToStdOut _
            | FormatSinkG.ToStdErr _
            | FormatSinkG.ToString -> ()

            for seg in segments do
                match seg with
                | FormatSegG.Lit _ -> ()
                | FormatSegG.Hole(_, value) -> acc.Add value
                | FormatSegG.DynHole hole ->
                    match hole.Width with
                    | ValueSome w -> acc.Add w
                    | ValueNone -> ()

                    match hole.Precision with
                    | ValueSome p -> acc.Add p
                    | ValueNone -> ()

                    acc.Add hole.Value
                | FormatSegG.CallbackHole(residue = residue) -> acc.Add residue
        | TExprG.ILIntrinsic(args = args) ->
            for x in args do
                acc.Add x
        | TExprG.StaticOptimization(clauses = clauses; defaultExpr = defaultExpr) ->
            for clause in clauses do
                acc.Add clause.Body

            acc.Add defaultExpr
        | TExprG.Upcast(source = source) -> acc.Add source
        | TExprG.Downcast(source = source) -> acc.Add source
        | TExprG.TypeTest(source = source) -> acc.Add source
        | TExprG.TraitCall(args = args) ->
            for x in args do
                acc.Add x

        acc.ToArray()

    /// The constant value carried by a `Const` node. Guard with `exprKind` =
    /// `ExprShape.Const` first; `failwith` on any other shape.
    let exprConstValue (e: ExprId) : TConstValue =
        match e with
        | TExprG.Const(value = value) -> value
        | _ -> failwith "TastAccessor.exprConstValue: not a Const node"

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

    /// The payload view of an `ExternalMember` node. Guard with `exprKind` =
    /// `ExprShape.ExternalMember` first; `failwith` on any other shape.
    let exprExternalMember (e: ExprId) : ExternalMemberView =
        match e with
        | TExprG.ExternalMember(receiver = receiver; key = key; memberName = memberName; storage = storage) ->
            {
                Receiver = receiver
                Key = key
                MemberName = memberName
                Storage = storage
            }
        | _ -> failwith "TastAccessor.exprExternalMember: not an ExternalMember node"

    /// The binder a `Var` node references — the `NodeKey` a preceding binder
    /// introduced. Guard with `exprKind` = `ExprShape.Var` first; `failwith` on any
    /// other shape.
    let exprVarBinding (e: ExprId) : NodeKey =
        match e with
        | TExprG.Var(binding = binding) -> binding
        | _ -> failwith "TastAccessor.exprVarBinding: not a Var node"

    /// The IL opcode string of an `ILIntrinsic` node — the `$N`-templated instruction.
    /// Its `args` are the node's `exprChildren` and its `typeOperand` is carried
    /// separately. Guard with `exprKind` = `ExprShape.ILIntrinsic` first; `failwith` on
    /// any other shape.
    let exprILIntrinsicOpCode (e: ExprId) : string =
        match e with
        | TExprG.ILIntrinsic(opCode = opCode) -> opCode
        | _ -> failwith "TastAccessor.exprILIntrinsicOpCode: not an ILIntrinsic node"

    /// The scalar payload of a `Lambda` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry. `Body` is the sole `exprChildren` entry; the
    /// `Param` pattern is not an expression child.
    [<Struct>]
    type LambdaView = { Param: PatId; Body: ExprId }

    /// The payload view of a `Lambda` node. Guard with `exprKind` = `ExprShape.Lambda`
    /// first; `failwith` on any other shape.
    let exprLambda (e: ExprId) : LambdaView =
        match e with
        | TExprG.Lambda(param = param; body = body) -> { Param = param; Body = body }
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

    /// The payload view of a `Let` node. Guard with `exprKind` = `ExprShape.Let` first;
    /// `failwith` on any other shape.
    let exprLet (e: ExprId) : LetView =
        match e with
        | TExprG.Let(binding = binding; value = value; body = body) ->
            {
                Binding = binding
                Value = value
                Body = body
            }
        | _ -> failwith "TastAccessor.exprLet: not a Let node"

    /// The scalar payload of an `Assignment` node (`lhs <- rhs`), minus the `ty`/`tok`
    /// that `exprTy`/`exprTok` already carry — the same two nodes `exprChildren` yields,
    /// named by role.
    [<Struct>]
    type AssignmentView = { Lhs: ExprId; Rhs: ExprId }

    /// The payload view of an `Assignment` node. Guard with `exprKind` =
    /// `ExprShape.Assignment` first; `failwith` on any other shape.
    let exprAssignment (e: ExprId) : AssignmentView =
        match e with
        | TExprG.Assignment(lhs = lhs; rhs = rhs) -> { Lhs = lhs; Rhs = rhs }
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
        match e with
        | TExprG.IfThenElse(cond = cond; thenExpr = thenExpr; elseExpr = elseExpr) ->
            {
                Cond = cond
                ThenExpr = thenExpr
                ElseExpr = elseExpr
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

    /// The payload view of an `External` node. Guard with `exprKind` =
    /// `ExprShape.External` first; `failwith` on any other shape.
    let exprExternal (e: ExprId) : ExternalView =
        match e with
        | TExprG.External(compiledName = compiledName; key = key) ->
            {
                CompiledName = compiledName
                Key = key
            }
        | _ -> failwith "TastAccessor.exprExternal: not an External node"

    /// The scalar payload of an `App` node (`fn arg`), minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry — the same two nodes `exprChildren` yields, named
    /// by role.
    [<Struct>]
    type AppView = { Fn: ExprId; Arg: ExprId }

    /// The payload view of an `App` node. Guard with `exprKind` = `ExprShape.App` first;
    /// `failwith` on any other shape.
    let exprApp (e: ExprId) : AppView =
        match e with
        | TExprG.App(fn = fn; arg = arg) -> { Fn = fn; Arg = arg }
        | _ -> failwith "TastAccessor.exprApp: not an App node"

    /// The (field-name, value-expression) pairs a `RecordCons` literal assigns, in source
    /// order — the labels `exprChildren` drops. Guard with `exprKind` =
    /// `ExprShape.RecordCons` first; `failwith` on any other shape.
    let exprRecordConsFields (e: ExprId) : (string * ExprId)[] =
        match e with
        | TExprG.RecordCons(fields = fields) -> EqArray.toArray fields
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
        match e with
        | TExprG.RecordClone(source = source; overrides = overrides) ->
            {
                Source = source
                Overrides = EqArray.toArray overrides
            }
        | _ -> failwith "TastAccessor.exprRecordClone: not a RecordClone node"

    /// The scalar payload of a `FieldGet` node (`receiver.FieldName`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry. `Receiver` is the sole
    /// `exprChildren` entry, named by role.
    [<Struct>]
    type FieldGetView = { Receiver: ExprId; FieldName: string }

    /// The payload view of a `FieldGet` node. Guard with `exprKind` =
    /// `ExprShape.FieldGet` first; `failwith` on any other shape.
    let exprFieldGet (e: ExprId) : FieldGetView =
        match e with
        | TExprG.FieldGet(receiver = receiver; fieldName = fieldName) ->
            {
                Receiver = receiver
                FieldName = fieldName
            }
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
        match e with
        | TExprG.FieldSet(receiver = receiver; fieldName = fieldName; value = value) ->
            {
                Receiver = receiver
                FieldName = fieldName
                Value = value
            }
        | _ -> failwith "TastAccessor.exprFieldSet: not a FieldSet node"

    /// The union case name a `UnionCons` node constructs. Its `args` are the node's
    /// `exprChildren` and its `ty` is `exprTy`. Guard with `exprKind` =
    /// `ExprShape.UnionCons` first; `failwith` on any other shape.
    let exprUnionConsCaseName (e: ExprId) : string =
        match e with
        | TExprG.UnionCons(caseName = caseName) -> caseName
        | _ -> failwith "TastAccessor.exprUnionConsCaseName: not a UnionCons node"

    /// The class name a `New` node constructs. Its `args` are the node's `exprChildren`
    /// and its `ty` is `exprTy`. Guard with `exprKind` = `ExprShape.New` first; `failwith`
    /// on any other shape.
    let exprNewClassName (e: ExprId) : string =
        match e with
        | TExprG.New(className = className) -> className
        | _ -> failwith "TastAccessor.exprNewClassName: not a New node"

    /// The scalar payload of a `PropertyGet` node — the receiver and the resolved member
    /// key, minus the `via`/`ty`/`tok` the node also carries. `Receiver` is the sole
    /// `exprChildren` entry, named by role.
    [<Struct>]
    type PropertyGetView = { Receiver: ExprId; Key: SymbolKey }

    /// The payload view of a `PropertyGet` node. Guard with `exprKind` =
    /// `ExprShape.PropertyGet` first; `failwith` on any other shape.
    let exprPropertyGet (e: ExprId) : PropertyGetView =
        match e with
        | TExprG.PropertyGet(receiver = receiver; key = key) -> { Receiver = receiver; Key = key }
        | _ -> failwith "TastAccessor.exprPropertyGet: not a PropertyGet node"

    /// The scalar payload of a `MethodCall` node — the receiver, the resolved member key,
    /// and the argument expressions, minus the `via`/`ty`/`tok` the node also carries.
    /// `Args` is ONLY the `args` field materialized — NOT `exprChildren` (which merges the
    /// receiver in ahead of the args).
    [<Struct>]
    type MethodCallView =
        {
            Receiver: ExprId
            Key: SymbolKey
            Args: ExprId[]
        }

    /// The payload view of a `MethodCall` node. Guard with `exprKind` =
    /// `ExprShape.MethodCall` first; `failwith` on any other shape.
    let exprMethodCall (e: ExprId) : MethodCallView =
        match e with
        | TExprG.MethodCall(receiver = receiver; key = key; args = args) ->
            {
                Receiver = receiver
                Key = key
                Args = EqArray.toArray args
            }
        | _ -> failwith "TastAccessor.exprMethodCall: not a MethodCall node"

    /// The resolved member key of a `StaticPropertyGet` node. Guard with `exprKind` =
    /// `ExprShape.StaticPropertyGet` first; `failwith` on any other shape.
    let exprStaticPropertyGetKey (e: ExprId) : SymbolKey =
        match e with
        | TExprG.StaticPropertyGet(key = key) -> key
        | _ -> failwith "TastAccessor.exprStaticPropertyGetKey: not a StaticPropertyGet node"

    /// The scalar payload of a `StaticFieldGet` node — the declaring class key and the
    /// backing-field name, minus the `ty`/`tok` the node also carries. Guard with
    /// `exprKind` = `ExprShape.StaticFieldGet` first; `failwith` on any other shape.
    [<Struct>]
    type StaticFieldGetView = { Key: SymbolKey; FieldName: string }

    /// The payload view of a `StaticFieldGet` node. Guard with `exprKind` =
    /// `ExprShape.StaticFieldGet` first; `failwith` on any other shape.
    let exprStaticFieldGet (e: ExprId) : StaticFieldGetView =
        match e with
        | TExprG.StaticFieldGet(declKey = declKey; fieldName = fieldName) -> { Key = declKey; FieldName = fieldName }
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
        match e with
        | TExprG.StaticFieldSet(declKey = declKey; fieldName = fieldName; value = value) ->
            {
                Key = declKey
                FieldName = fieldName
                Value = value
            }
        | _ -> failwith "TastAccessor.exprStaticFieldSet: not a StaticFieldSet node"

    /// The resolved member key of a `StaticMethodCall` node. Its `args` are the node's
    /// `exprChildren`. Guard with `exprKind` = `ExprShape.StaticMethodCall` first;
    /// `failwith` on any other shape.
    let exprStaticMethodCallKey (e: ExprId) : SymbolKey =
        match e with
        | TExprG.StaticMethodCall(key = key) -> key
        | _ -> failwith "TastAccessor.exprStaticMethodCallKey: not a StaticMethodCall node"

    /// The scalar payload of a `Match` node — the scrutinee and the arms, minus the
    /// `ty`/`tok` the node also carries. `Arms` is the `arms` field materialized (a
    /// composite carrier, not an `exprChildren` entry — `exprChildren` descends into arm
    /// bodies/guards, dropping the arm identity a consumer needs).
    [<Struct>]
    type MatchView =
        {
            Scrutinee: ExprId
            Arms: Frozen.TMatchArm[]
        }

    /// The payload view of a `Match` node. Guard with `exprKind` = `ExprShape.Match` first;
    /// `failwith` on any other shape.
    let exprMatch (e: ExprId) : MatchView =
        match e with
        | TExprG.Match(scrutinee = scrutinee; arms = arms) ->
            {
                Scrutinee = scrutinee
                Arms = EqArray.toArray arms
            }
        | _ -> failwith "TastAccessor.exprMatch: not a Match node"

    /// The scalar payload of a `While` node (`while Cond do Body`), minus the `ty`/`tok`
    /// that `exprTy`/`exprTok` already carry — the two nodes `exprChildren` yields, named
    /// by role.
    [<Struct>]
    type WhileView = { Cond: ExprId; Body: ExprId }

    /// The payload view of a `While` node. Guard with `exprKind` = `ExprShape.While` first;
    /// `failwith` on any other shape.
    let exprWhile (e: ExprId) : WhileView =
        match e with
        | TExprG.While(cond = cond; body = body) -> { Cond = cond; Body = body }
        | _ -> failwith "TastAccessor.exprWhile: not a While node"

    /// The scalar payload of a `ForTo` node (`for Var = StartExpr to EndExpr do Body`),
    /// minus the `identTok`/`ty`/`tok` the node also carries. `StartExpr`/`EndExpr`/`Body`
    /// are the three `exprChildren` entries, named by role; `Var` is the loop binder.
    [<Struct>]
    type ForToView =
        {
            Var: NodeKey
            StartExpr: ExprId
            EndExpr: ExprId
            Body: ExprId
        }

    /// The payload view of a `ForTo` node. Guard with `exprKind` = `ExprShape.ForTo`
    /// first; `failwith` on any other shape.
    let exprForTo (e: ExprId) : ForToView =
        match e with
        | TExprG.ForTo(var = var; startExpr = startExpr; endExpr = endExpr; body = body) ->
            {
                Var = var
                StartExpr = startExpr
                EndExpr = endExpr
                Body = body
            }
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

    /// The payload view of a `ForIn` node. Guard with `exprKind` = `ExprShape.ForIn`
    /// first; `failwith` on any other shape.
    let exprForIn (e: ExprId) : ForInView =
        match e with
        | TExprG.ForIn(pat = pat; source = source; body = body; enumerator = enumerator) ->
            {
                Pat = pat
                Source = source
                Body = body
                Enumerator = enumerator
            }
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

    /// The payload view of a `Use` node. Guard with `exprKind` = `ExprShape.Use` first;
    /// `failwith` on any other shape.
    let exprUse (e: ExprId) : UseView =
        match e with
        | TExprG.Use(binding = binding; value = value; body = body; dispose = dispose) ->
            {
                Binding = binding
                Value = value
                Body = body
                Dispose = dispose
            }
        | _ -> failwith "TastAccessor.exprUse: not a Use node"

    /// The scalar payload of a `Format` node — the sink and the interleaved
    /// literal/hole segments, minus the `ty`/`tok` the node also carries. Both are
    /// composite carriers whose sub-expressions `exprChildren` descends into; a consumer
    /// dispatching on the sink or replaying the segments reads them here.
    [<Struct>]
    type FormatView =
        {
            Sink: Frozen.FormatSink
            Segments: EqArray<Frozen.FormatSeg>
        }

    /// The payload view of a `Format` node. Guard with `exprKind` = `ExprShape.Format`
    /// first; `failwith` on any other shape.
    let exprFormat (e: ExprId) : FormatView =
        match e with
        | TExprG.Format(sink = sink; segments = segments) -> { Sink = sink; Segments = segments }
        | _ -> failwith "TastAccessor.exprFormat: not a Format node"

    /// The shape tag of a pattern node.
    let patKind (p: PatId) : PatShape =
        match p with
        | TPatG.NamedSimple _ -> PatShape.NamedSimple
        | TPatG.Wildcard _ -> PatShape.Wildcard
        | TPatG.Tuple _ -> PatShape.Tuple
        | TPatG.Const _ -> PatShape.Const
        | TPatG.Record _ -> PatShape.Record
        | TPatG.Union _ -> PatShape.Union
        | TPatG.TypeTestAs _ -> PatShape.TypeTestAs
        | TPatG.Null _ -> PatShape.Null
        | TPatG.EnumCase _ -> PatShape.EnumCase
        | TPatG.Or _ -> PatShape.Or

    /// The pattern's type.
    let patTy (p: PatId) : FrozenType = TastWalk.patTy p

    /// The pattern's source-anchor token.
    let patTok (p: PatId) : SyntaxToken = TastWalk.patTok p

    /// The single binder a simple (`NamedSimple`) pattern introduces — the identity
    /// a `TExpr.Var` references and that naming is computed from. `ValueNone` for a
    /// pattern that binds nothing (`Wildcard`, `Const`, …) or binds through nested
    /// sub-patterns (`Tuple`, `Record`, `Union`, `TypeTestAs`, `Or` — walk
    /// `patChildren` for those).
    let patBinder (p: PatId) : NodeKey voption =
        match p with
        | TPatG.NamedSimple(binding = binding) -> ValueSome binding
        | TPatG.Wildcard _
        | TPatG.Tuple _
        | TPatG.Const _
        | TPatG.Record _
        | TPatG.Union _
        | TPatG.TypeTestAs _
        | TPatG.Null _
        | TPatG.EnumCase _
        | TPatG.Or _ -> ValueNone

    /// The immediate child *patterns*, in source order.
    let patChildren (p: PatId) : PatId[] =
        let acc = ResizeArray<PatId>()

        match p with
        | TPatG.NamedSimple _
        | TPatG.Wildcard _
        | TPatG.Const _
        | TPatG.Null _
        | TPatG.EnumCase _ -> ()
        | TPatG.Tuple(items = items)
        | TPatG.Or(alts = items) ->
            for x in items do
                acc.Add x
        | TPatG.Record(fields = fields) ->
            for (_, sub) in fields do
                acc.Add sub
        | TPatG.Union(fields = fields) ->
            for x in fields do
                acc.Add x
        | TPatG.TypeTestAs(inner = inner) -> acc.Add inner

        acc.ToArray()

    /// The constant value carried by a `Const` pattern. Guard with `patKind` =
    /// `PatShape.Const` first; `failwith` on any other shape.
    let patConstValue (p: PatId) : TConstValue =
        match p with
        | TPatG.Const(value = value) -> value
        | _ -> failwith "TastAccessor.patConstValue: not a Const pattern"

    /// The union case name a `Union` pattern discriminates on. Guard with `patKind` =
    /// `PatShape.Union` first; `failwith` on any other shape.
    let patUnionCaseName (p: PatId) : string =
        match p with
        | TPatG.Union(caseName = caseName) -> caseName
        | _ -> failwith "TastAccessor.patUnionCaseName: not a Union pattern"

    /// The (field-name, sub-pattern) pairs a `Record` pattern binds — the labels
    /// `patChildren` drops. Guard with `patKind` = `PatShape.Record` first; `failwith`
    /// on any other shape.
    let patRecordFields (p: PatId) : (string * PatId)[] =
        match p with
        | TPatG.Record(fields = fields) -> EqArray.toArray fields
        | _ -> failwith "TastAccessor.patRecordFields: not a Record pattern"

    /// The scalar payload of an `EnumCase` pattern — the case's `enumKey`/`caseName`
    /// identity, minus the `ty`/`tok` that `patTy`/`patTok` already carry.
    [<Struct>]
    type EnumCasePatView =
        { EnumKey: SymbolKey; CaseName: string }

    /// The payload view of an `EnumCase` pattern. Guard with `patKind` =
    /// `PatShape.EnumCase` first; `failwith` on any other shape.
    let patEnumCase (p: PatId) : EnumCasePatView =
        match p with
        | TPatG.EnumCase(enumKey = enumKey; caseName = caseName) ->
            {
                EnumKey = enumKey
                CaseName = caseName
            }
        | _ -> failwith "TastAccessor.patEnumCase: not an EnumCase pattern"

    /// The shape tag of a declaration node.
    let declKind (d: DeclId) : DeclShape =
        match d with
        | TDeclG.Let _ -> DeclShape.Let
        | TDeclG.Expression _ -> DeclShape.Expression
        | TDeclG.Type _ -> DeclShape.Type

    /// The `type`-declaration payload of a `Type` decl (its `Kind`, `Key`, `Name`, …).
    /// Guard with `declKind` = `DeclShape.Type` first; `failwith` on any other shape.
    let declType (d: DeclId) : Frozen.TTypeDecl =
        match d with
        | TDeclG.Type td -> td
        | _ -> failwith "TastAccessor.declType: not a Type decl"
