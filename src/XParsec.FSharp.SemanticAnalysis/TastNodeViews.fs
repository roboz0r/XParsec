namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The SHAPES a frozen-TAST consumer receives: the node handles, the handle-carrying
// instantiation of each generic tree shape, and the `…View` payload records — a node's
// fields MINUS its child edges and its `ty`/`tok`, named by role.

module TastNodeViews =

    // The node handles: a dense id in a pool, carried with the pool.
    type ExprId = Handle<ExprPoolId>
    type PatId = Handle<PatPoolId>
    type DeclId = Handle<DeclPoolId>

    /// The `type`-declaration cluster, its member/preamble/ctor BODY slots holding handles.
    type TypeDecl = TTypeDeclG<FrozenType, Anchor, BinderId, ExprId>
    type TypeKind = TTypeKindG<FrozenType, Anchor, BinderId, ExprId>
    type Class = TClassG<FrozenType, BinderId, ExprId>
    type TypeMember = TTypeMemberG<FrozenType, BinderId, ExprId>
    type ClassLet = TClassLetG<FrozenType, ExprId>
    type PreambleEntry = TPreambleEntryG<FrozenType, ExprId>
    type CtorLet = TCtorLetG<FrozenType, BinderId, ExprId>
    type CtorFieldInit = TCtorFieldInitG<ExprId>
    type SecondaryCtor = TSecondaryCtorG<FrozenType, BinderId, ExprId>
    type BaseCtorCall = TBaseCtorCallG<FrozenType, BinderId, ExprId>

    /// One resolved-specialization table entry as a consumer of the TREE reads it. `Origin`
    /// says which file the anchors inside the entry are indices into.
    type Specialization =
        {
            Key: Frozen.SpecializationKey
            Origin: OriginFile
            /// The abstraction this entry's edges apply — the lambda chain an edge's
            /// arguments are positional against.
            Value: ExprId
        }

    /// The compiled-form cluster, its tuple-group / destructuring patterns held as handles —
    /// whether those came from a file's own pool or from the standalone pool an `.fsi`
    /// contract's patterns are minted into.
    type StaticParam = StaticParamG<FrozenType, PatId, BinderId>
    type ArgGroup = ArgGroupG<FrozenType, PatId, BinderId>
    type ValRepr = ValReprG<FrozenType, PatId, BinderId>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, PatId, BinderId>

    /// The scalar payload of an `ExternalMember` node; `Receiver` is the member's target.
    [<Struct>]
    type ExternalMemberView =
        {
            Receiver: ExprId voption
            Key: SymbolKey
            MemberName: string
            Storage: MemberStorage
        }

    /// The scalar payload of an `ILIntrinsic` node; its args are the node's `exprChildren`.
    [<Struct>]
    type ILIntrinsicView =
        {
            OpCode: string
            /// The `<T>` the opcode takes — the element type of `newarr`/`ldelem`/`stelem`,
            /// the boxed type of `box`, the zeroed type of `ilzero`. `ValueNone` for the
            /// type-free arithmetic / `throw` / reinterpret opcodes.
            TypeOperand: FrozenType voption
        }

    /// The scalar payload of a `Lambda` node. `Body` is the sole `exprChildren` entry;
    /// `Param` is a pattern child.
    [<Struct>]
    type LambdaView = { Param: PatId; Body: ExprId }

    /// The scalar payload of a `Let` node. `Value`/`Body` are the two `exprChildren`
    /// entries; `Binding` is a pattern child.
    [<Struct>]
    type LetView =
        {
            Binding: PatId
            Value: ExprId
            Body: ExprId
        }

    /// The scalar payload of an `Assignment` node (`lhs <- rhs`) — the two nodes
    /// `exprChildren` yields, named by role.
    [<Struct>]
    type AssignmentView = { Lhs: ExprId; Rhs: ExprId }

    /// The scalar payload of an `IfThenElse` node — the three nodes `exprChildren` yields,
    /// named by role.
    [<Struct>]
    type IfThenElseView =
        {
            Cond: ExprId
            ThenExpr: ExprId
            ElseExpr: ExprId
        }

    /// The scalar payload of an `External` node.
    [<Struct>]
    type ExternalView =
        {
            CompiledName: string
            Key: SymbolKey voption
        }

    /// The scalar payload of an `App` node (`fn arg`) — the two nodes `exprChildren`
    /// yields, named by role.
    [<Struct>]
    type AppView = { Fn: ExprId; Arg: ExprId }

    /// The scalar payload of a `RecordClone` node (`{ source with … }`). `Overrides` are the
    /// (field-name, replacement) pairs — the labels `exprChildren` drops.
    [<Struct>]
    type RecordCloneView =
        {
            Source: ExprId
            Overrides: (string * ExprId)[]
        }

    /// The scalar payload of a `FieldGet` node (`receiver.FieldName`); `Receiver` is the
    /// sole `exprChildren` entry.
    [<Struct>]
    type FieldGetView = { Receiver: ExprId; FieldName: string }

    /// The scalar payload of a `FieldSet` node (`receiver.FieldName <- value`) — the two
    /// nodes `exprChildren` yields, named by role, plus the field label.
    [<Struct>]
    type FieldSetView =
        {
            Receiver: ExprId
            FieldName: string
            Value: ExprId
        }

    /// The scalar payload of a `New` node; its args are the node's `exprChildren`.
    [<Struct>]
    type NewView =
        {
            ClassName: string
            /// The overload identity the front end chose — the key that disambiguates a
            /// same-arity external-ctor candidate set; `ValueNone` when arity suffices.
            ChosenCtor: SymbolKey voption
        }

    /// The scalar payload of a `PropertyGet` node; `Receiver` is the sole `exprChildren`
    /// entry. `Via` distinguishes a grounded self/base access from a
    /// `constrained.`-dispatched typar-interface one (`CallVia.Interface`).
    [<Struct>]
    type PropertyGetView =
        {
            Receiver: ExprId
            Key: SymbolKey
            Via: CallVia<FrozenType>
        }

    /// The scalar payload of a `MethodCall` node. `Args` excludes the receiver, where
    /// `exprChildren` merges it in ahead of them. `Via` distinguishes a grounded self/base
    /// call from a `constrained.`-dispatched typar-interface one (`CallVia.Interface`).
    [<Struct>]
    type MethodCallView =
        {
            Receiver: ExprId
            Key: SymbolKey
            Via: CallVia<FrozenType>
            Args: EqArray<ExprId>
        }

    /// The scalar payload of a `StaticFieldGet` node — the declaring class key and the
    /// backing-field name.
    [<Struct>]
    type StaticFieldGetView = { Key: SymbolKey; FieldName: string }

    /// The scalar payload of a `StaticFieldSet` node — the declaring class key and the
    /// backing-field name; `Value` is the sole `exprChildren` entry.
    [<Struct>]
    type StaticFieldSetView =
        {
            Key: SymbolKey
            FieldName: string
            Value: ExprId
        }

    /// One arm of a `Match` / `TryWith`, its pattern and guard/body expressions held as
    /// handles.
    type Arm = TMatchArmG<PatId, ExprId>

    /// The scalar payload of a `Match` node — the scrutinee and the arms.
    [<Struct>]
    type MatchView = { Scrutinee: ExprId; Arms: Arm[] }

    /// The scalar payload of a `TryWith` node — the guarded body and the handler arms;
    /// `Body` is the leading `exprChildren` entry.
    [<Struct>]
    type TryWithView = { Body: ExprId; Arms: Arm[] }

    /// The scalar payload of a `TryFinally` node (`try Body finally Cleanup`) — the two
    /// `exprChildren` entries. `Body` carries the node's `ty`; `Cleanup` is unit.
    [<Struct>]
    type TryFinallyView = { Body: ExprId; Cleanup: ExprId }

    /// The scalar payload of a `While` node (`while Cond do Body`) — the two nodes
    /// `exprChildren` yields, named by role.
    [<Struct>]
    type WhileView = { Cond: ExprId; Body: ExprId }

    /// The scalar payload of a `ForTo` node (`for Var = StartExpr to EndExpr do Body`).
    /// `StartExpr`/`EndExpr`/`Body` are the three `exprChildren` entries; `Var` is the loop
    /// binder, which has no pattern node behind it.
    [<Struct>]
    type ForToView =
        {
            Var: BinderId
            StartExpr: ExprId
            EndExpr: ExprId
            Body: ExprId
        }

    /// The scalar payload of a `ForIn` node (`for Pat in Source do Body`). `Source`/`Body`
    /// are the two `exprChildren` entries; `Pat` is a pattern child, and `Enumerator` records
    /// how the source yields its enumerator.
    [<Struct>]
    type ForInView =
        {
            Pat: PatId
            Source: ExprId
            Body: ExprId
            Enumerator: Frozen.ForInEnumerator
        }

    /// The scalar payload of a `Use` node (`use Binding = Value in Body`). `Value`/`Body`
    /// are the two `exprChildren` entries; `Binding` is a pattern child, and `Dispose` the
    /// resolved disposal path.
    [<Struct>]
    type UseView =
        {
            Binding: PatId
            Value: ExprId
            Body: ExprId
            Dispose: Disposal
        }

    /// The format sink / segment / dyn-hole shapes, sub-expressions held as handles.
    type FormatSink = FormatSinkG<ExprId>
    type FormatSeg = FormatSegG<FrozenType, Anchor, ExprId>
    type DynFormatHole = DynFormatHoleG<FrozenType, Anchor, ExprId>

    /// The scalar payload of a `Format` node — the sink and the interleaved literal/hole
    /// segments.
    [<Struct>]
    type FormatView =
        {
            Sink: FormatSink
            Segments: FormatSeg[]
        }

    /// The scalar payload of an `EnumCase` pattern — the case's enum-key / case-name identity.
    [<Struct>]
    type EnumCasePatView =
        { EnumKey: SymbolKey; CaseName: string }

    /// The payload of a `Let` decl. `IsInline` is whether the binding expands per call site;
    /// `Ty` is the binding's declared type, distinct from the type of `Value` for a
    /// destructuring binding.
    [<Struct>]
    type DeclLetView =
        {
            Binding: PatId
            Value: ExprId
            IsInline: bool
            Ty: FrozenType
        }
