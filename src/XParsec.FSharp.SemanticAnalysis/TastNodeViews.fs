namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The SHAPES a frozen-TAST consumer receives: the node handles, the handle-carrying
// instantiation of each generic tree shape, and the `…View` payload records: a node's
// fields MINUS its child edges and its `ty`/`tok`, named by role.

module TastNodeViews =

    // The node handles: a dense id in a pool, carried with the pool.
    type ExprId = Handle<ExprPoolId>
    type PatId = Handle<PatPoolId>
    type DeclId = Handle<DeclPoolId>

    /// The `type`-declaration cluster, its member/preamble/ctor BODY slots holding handles.
    type TypeDecl = TTypeDeclG<FrozenType, Anchor, BoundVarId, ExprId>
    type TypeKind = TTypeKindG<FrozenType, Anchor, BoundVarId, ExprId>
    type Class = TClassG<FrozenType, BoundVarId, ExprId>
    type TypeMember = TTypeMemberG<FrozenType, BoundVarId, ExprId>
    type ClassLet = TClassLetG<FrozenType, ExprId>
    type PreambleEntry = TPreambleEntryG<FrozenType, ExprId>
    type CtorLet = TCtorLetG<FrozenType, BoundVarId, ExprId>
    type CtorFieldInit = TCtorFieldInitG<ExprId>
    type SecondaryCtor = TSecondaryCtorG<FrozenType, BoundVarId, ExprId>
    type Base = TBaseG<FrozenType, BoundVarId, ExprId>
    type BaseCtorCall = TBaseCtorCallG<FrozenType, BoundVarId, ExprId>

    /// One resolved-specialization table entry as a consumer of the TREE reads it. `Source`
    /// says which file the anchors inside the entry are indices into.
    type Specialization =
        {
            Key: Frozen.SpecializationKey
            Path: AssemblyFilePath
            /// The abstraction this entry's edges apply: the lambda chain an edge's
            /// arguments are positional against.
            Value: ExprId
        }

    /// The compiled-form cluster, its tuple-group / destructuring patterns held as handles,
    /// whether those came from a file's own pool or from the standalone pool a
    /// contract's patterns are minted into.
    type StaticParam = StaticParamG<FrozenType, PatId, BoundVarId>
    type ArgGroup = ArgGroupG<FrozenType, PatId, BoundVarId>
    type ValRepr = ValReprG<FrozenType, PatId, BoundVarId>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, PatId, BoundVarId>

    /// The scalar payload of an `ExternalMember` node; `ObjArg` is the member's target.
    [<Struct>]
    type ExternalMemberView =
        {
            ObjArg: ExprId voption
            Key: SymbolKey
            MemberName: string
            Storage: MemberStorage
            ArgGroupWidths: EqArray<int>
        }

    /// The scalar payload of an `ILIntrinsic` node; its args are the node's `exprChildren`.
    [<Struct>]
    type ILIntrinsicView =
        {
            OpCode: string
            /// The `<T>` the opcode takes: the element type of `newarr`/`ldelem`/`stelem`,
            /// the boxed type of `box`, the zeroed type of `ilzero`. `ValueNone` for the
            /// type-free arithmetic / `throw` / reinterpret opcodes.
            TypeOperand: FrozenType voption
        }

    /// The scalar payload of a `Lambda` node. `Body` is the sole `exprChildren` entry;
    /// `Param` is a pattern child.
    [<Struct>]
    type LambdaView = { Param: PatId; Body: ExprId }

    /// The scalar payload of a `Let` node. `Value`/`Body` are the two `exprChildren`
    /// entries; `Pattern` is a pattern child. `IsRec` is the source `rec` keyword, distinct
    /// from the analysed `Recursion`.
    [<Struct>]
    type LetView =
        {
            Pattern: PatId
            Value: ExprId
            Body: ExprId
            IsRec: bool
            Recursion: Recursion
        }

    /// The scalar payload of an `Assignment` node (`lhs <- rhs`): the two nodes
    /// `exprChildren` yields, named by role.
    [<Struct>]
    type AssignmentView = { Lhs: ExprId; Rhs: ExprId }

    /// The scalar payload of an `IfThenElse` node: the three nodes `exprChildren` yields,
    /// named by role.
    [<Struct>]
    type IfThenElseView =
        {
            Cond: ExprId
            ThenExpr: ExprId
            ElseExpr: ExprId
        }

    /// The scalar payload of an `App` node (`fn arg`): the two nodes `exprChildren`
    /// yields, named by role.
    [<Struct>]
    type AppView = { Fn: ExprId; Arg: ExprId }

    /// One level of a curried `App` chain: in `f a b`, `a`'s `StepResultTy` is the type of
    /// `f a` — the partial application, not `a`.
    type AppliedArg =
        {
            Arg: ExprId
            StepResultTy: FrozenType
            Tok: Anchor
        }

    /// The scalar payload of a `RecordClone` node (`{ source with … }`). `Overrides` are the
    /// (field-name, replacement) pairs, carrying the labels `exprChildren` drops.
    [<Struct>]
    type RecordCloneView =
        {
            Source: ExprId
            Overrides: (string * ExprId)[]
        }

    /// The scalar payload of a `FieldGet` node (`objArg.FieldName`); `ObjArg` is the
    /// sole `exprChildren` entry.
    [<Struct>]
    type FieldGetView = { ObjArg: ExprId; FieldName: string }

    /// The scalar payload of a `FieldSet` node (`objArg.FieldName <- value`): the two
    /// nodes `exprChildren` yields, named by role, plus the field label.
    [<Struct>]
    type FieldSetView =
        {
            ObjArg: ExprId
            FieldName: string
            Value: ExprId
        }

    /// The scalar payload of a `New` node; its args are the node's `exprChildren`.
    [<Struct>]
    type NewView =
        {
            ClassName: string
            /// The overload identity the front end chose: the key that disambiguates a
            /// same-arity external-ctor candidate set; `ValueNone` when arity suffices.
            ChosenCtor: SymbolKey voption
        }

    /// The scalar payload of a `PropertyGet` node; `ObjArg` is the sole `exprChildren`
    /// entry. `Via` distinguishes a grounded self/base access from a
    /// `constrained.`-dispatched typar-interface one (`CallVia.Interface`).
    [<Struct>]
    type PropertyGetView =
        {
            ObjArg: ExprId
            Key: SymbolKey
            Via: CallVia<FrozenType>
        }

    /// The scalar payload of a `MethodCall` node. `Args` excludes the object argument, where
    /// `exprChildren` merges it in ahead of them. `Via` distinguishes a grounded self/base
    /// call from a `constrained.`-dispatched typar-interface one (`CallVia.Interface`).
    [<Struct>]
    type MethodCallView =
        {
            ObjArg: ExprId
            Key: SymbolKey
            Via: CallVia<FrozenType>
            Args: EqArray<ExprId>
        }

    /// The scalar payload of a `StaticFieldGet` node: the declaring class key and the
    /// backing-field name.
    [<Struct>]
    type StaticFieldGetView = { Key: TypeKey; FieldName: string }

    /// The scalar payload of a `StaticFieldSet` node: the declaring class key and the
    /// backing-field name; `Value` is the sole `exprChildren` entry.
    [<Struct>]
    type StaticFieldSetView =
        {
            Key: TypeKey
            FieldName: string
            Value: ExprId
        }

    /// The scalar payload of an `InlineCall` node; its args are the node's `exprChildren`.
    [<Struct>]
    type InlineCallView =
        {
            /// The specialization slot this call identifies: an index into the pools'
            /// `Specializations` root array, NOT into any column a node handle reads.
            Spec: SpecializationId
            /// The file this call's own anchor is read against, and its arguments' too, they
            /// being CALLER material. NOT the entry's: the entry states its own origin.
            Source: AssemblyFilePath
        }

    /// One arm of a `Match` / `TryWith`, its pattern and guard/body expressions held as
    /// handles.
    type Arm = TMatchArmG<PatId, ExprId>

    /// The scalar payload of a `Match` node: the scrutinee and the arms.
    [<Struct>]
    type MatchView = { Scrutinee: ExprId; Arms: Arm[] }

    /// The scalar payload of a `TryWith` node: the guarded body and the handler arms;
    /// `Body` is the leading `exprChildren` entry.
    [<Struct>]
    type TryWithView = { Body: ExprId; Arms: Arm[] }

    /// The scalar payload of a `TryFinally` node (`try Body finally Cleanup`): the two
    /// `exprChildren` entries. `Body` carries the node's `ty`; `Cleanup` is unit.
    [<Struct>]
    type TryFinallyView = { Body: ExprId; Cleanup: ExprId }

    /// The scalar payload of a `While` node (`while Cond do Body`): the two nodes
    /// `exprChildren` yields, named by role.
    [<Struct>]
    type WhileView = { Cond: ExprId; Body: ExprId }

    /// The scalar payload of a `ForTo` node (`for Var = StartExpr to EndExpr do Body`).
    /// `StartExpr`/`EndExpr`/`Body` are the three `exprChildren` entries; `Var` is the loop
    /// bound variable, which has no pattern node behind it.
    [<Struct>]
    type ForToView =
        {
            Var: BoundVarId
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

    /// The scalar payload of a `Use` node (`use Pattern = Value in Body`). `Value`/`Body`
    /// are the two `exprChildren` entries; `Pattern` is a pattern child, and `Dispose` the
    /// resolved disposal path.
    [<Struct>]
    type UseView =
        {
            Pattern: PatId
            Value: ExprId
            Body: ExprId
            Dispose: Disposal
        }

    /// The format sink / segment / dyn-hole shapes, sub-expressions held as handles.
    type FormatSink = FormatSinkG<ExprId>
    type FormatSeg = FormatSegG<FrozenType, Anchor, ExprId>
    type DynFormatHole = DynFormatHoleG<FrozenType, Anchor, ExprId>

    /// The scalar payload of a `Format` node: the sink and the interleaved literal/hole
    /// segments.
    [<Struct>]
    type FormatView =
        {
            Sink: FormatSink
            Segments: FormatSeg[]
        }

    /// The scalar payload of an `EnumCase` pattern: the case's enum-key / case-name identity.
    [<Struct>]
    type EnumCasePatView = { EnumKey: TypeKey; CaseName: string }

    /// The payload of a `Let` decl. `IsInline` is whether the binding expands per call site;
    /// `IsRec` is the source `rec` keyword, distinct from the analysed `Recursion`; `Ty` is
    /// the binding's declared type, distinct from the type of `Value` for a destructuring
    /// binding.
    [<Struct>]
    type DeclLetView =
        {
            Pattern: PatId
            Value: ExprId
            IsInline: bool
            IsRec: bool
            Recursion: Recursion
            Ty: FrozenType
        }
