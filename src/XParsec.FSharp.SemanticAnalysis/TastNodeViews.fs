namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// The SHAPES a frozen-TAST consumer receives, declared once and next to each other: the
// node handles, the handle-carrying instantiation of each generic tree shape, and the
// `…View` payload records. Nothing here reads a pool — `TastAccessor.fs` is the sole
// producer of every one of them, and re-exports each under its own name, so a consumer
// walking the tree names that one module and never this one.
//
// A node is a `Handle` — a dense pool id plus the pool that resolves it, over the columns
// of `TastPoolNodes.fs` — so every shape here is POOL-AGNOSTIC: none takes a pool
// parameter, and a view over a file's own tree has the same type as one over the
// standalone pool an `.fsi` contract's patterns are minted into.
//
// The generic shapes — `TTypeDeclG`, `ValReprG`, `TMatchArmG`, `FormatSegG` and friends —
// belong to `TastExpr.fs`/`TastDecl.fs`, parameterised over whatever holds a sub-node.
// What is here is the ONE instantiation of each whose slots are handles, so a consumer
// that scopes an arm's binders or replays a format sink reads the same record whichever
// domain it is in.
//
// The `…View` records are the payload seam: a node's fields MINUS the child edges and
// the `ty`/`tok`, named by role, so a consumer never positions into `exprChildren` by
// hand. Each is a struct of handles and scalars — pool-agnostic, since the handles
// carry their own pool.

module TastNodeViews =

    // The node handles: a dense id in a pool, carried with the pool.
    type ExprId = Handle<ExprPoolId>
    type PatId = Handle<PatPoolId>
    type DeclId = Handle<DeclPoolId>

    /// The `type`-declaration cluster with its member/preamble/ctor BODY slots holding
    /// handles — the shape `TTypeDeclG`'s `'body` parameter exists for. Same spine as
    /// `Frozen.TTypeDecl`, ids in the body slots.
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

    /// One resolved-specialization table entry with its declaration resolved to a handle —
    /// the pooled `PooledSpecialization` as a consumer of the TREE reads it. `Origin` rides
    /// across unchanged: it is a file identity, which no column addresses, and it is what
    /// says which file the anchors inside `Decl` are indices into.
    type Specialization =
        {
            Key: Frozen.SpecializationKey
            Origin: OriginFile
            Decl: DeclId
        }

    /// The compiled-form cluster with its tuple-group / destructuring patterns held as
    /// handles — the `'pat` instantiation every consumer reads, whether the pats came
    /// from a file's own pool (`peelValRepr` off the frozen lambda spine) or from the
    /// standalone pool an `.fsi` contract's are minted into.
    type StaticParam = StaticParamG<FrozenType, PatId, BinderId>
    type ArgGroup = ArgGroupG<FrozenType, PatId, BinderId>
    type ValRepr = ValReprG<FrozenType, PatId, BinderId>
    type CompiledReturn = CompiledReturnG<FrozenType>
    type CompiledForm = CompiledFormG<FrozenType, PatId, BinderId>

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

    /// The scalar payload of an `ILIntrinsic` node — the `$N`-templated instruction and
    /// its type operand, minus the `ty`/`tok` the node also carries. Its `args` are the
    /// node's `exprChildren`.
    [<Struct>]
    type ILIntrinsicView =
        {
            OpCode: string
            /// The `<T>` the opcode takes — the element type of
            /// `newarr`/`ldelem`/`stelem`/`ldobj`, the boxed type of `box`, the zeroed
            /// type of `ilzero` — `ValueNone` for the type-free arithmetic / `throw` /
            /// reinterpret opcodes. Distinct from the result `ty` (`exprTy`).
            TypeOperand: FrozenType voption
        }

    /// The scalar payload of a `Lambda` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry. `Body` is the sole `exprChildren` entry; the
    /// `Param` pattern is not an expression child.
    [<Struct>]
    type LambdaView = { Param: PatId; Body: ExprId }

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

    /// The scalar payload of an `Assignment` node (`lhs <- rhs`), minus the `ty`/`tok`
    /// that `exprTy`/`exprTok` already carry — the same two nodes `exprChildren` yields,
    /// named by role.
    [<Struct>]
    type AssignmentView = { Lhs: ExprId; Rhs: ExprId }

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

    /// The scalar payload of an `External` node, minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry.
    [<Struct>]
    type ExternalView =
        {
            CompiledName: string
            Key: SymbolKey voption
        }

    /// The scalar payload of an `App` node (`fn arg`), minus the `ty`/`tok` that
    /// `exprTy`/`exprTok` already carry — the same two nodes `exprChildren` yields, named
    /// by role.
    [<Struct>]
    type AppView = { Fn: ExprId; Arg: ExprId }

    /// The scalar payload of a `RecordClone` node (`{ source with … }`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry. `Overrides` are the
    /// (field-name, replacement) pairs — the labels `exprChildren` drops.
    [<Struct>]
    type RecordCloneView =
        {
            Source: ExprId
            Overrides: (string * ExprId)[]
        }

    /// The scalar payload of a `FieldGet` node (`receiver.FieldName`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry. `Receiver` is the sole
    /// `exprChildren` entry, named by role.
    [<Struct>]
    type FieldGetView = { Receiver: ExprId; FieldName: string }

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

    /// The scalar payload of a `New` node, minus the `ty`/`tok` the node also carries.
    /// Its `args` are the node's `exprChildren`.
    [<Struct>]
    type NewView =
        {
            ClassName: string
            /// The overload identity the front end chose — the key that disambiguates a
            /// same-arity external-ctor candidate set, `ValueNone` when arity alone
            /// suffices.
            ChosenCtor: SymbolKey voption
        }

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

    /// The scalar payload of a `StaticFieldGet` node — the declaring class key and the
    /// backing-field name, minus the `ty`/`tok` the node also carries.
    [<Struct>]
    type StaticFieldGetView = { Key: SymbolKey; FieldName: string }

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

    /// One arm of a `Match` / `TryWith`, its pattern and guard/body expressions held as
    /// handles — the `'pat`/`'e` instantiation of the one arm shape (`TMatchArmG`), so a
    /// consumer that scopes the arm's binders over its guard and body reads the same
    /// record whichever domain it is in.
    type Arm = TMatchArmG<PatId, ExprId>

    /// The scalar payload of a `Match` node — the scrutinee and the arms, minus the
    /// `ty`/`tok` the node also carries.
    [<Struct>]
    type MatchView = { Scrutinee: ExprId; Arms: Arm[] }

    /// The scalar payload of a `TryWith` node — the guarded body and the handler arms,
    /// minus the `ty`/`tok` the node also carries. `Body` is the sole positional
    /// `exprChildren` head.
    [<Struct>]
    type TryWithView = { Body: ExprId; Arms: Arm[] }

    /// The scalar payload of a `TryFinally` node (`try Body finally Cleanup`), minus the
    /// `ty`/`tok` that `exprTy`/`exprTok` already carry — the two `exprChildren` entries,
    /// named by role. `Body` carries the node's `ty`; `Cleanup` is unit.
    [<Struct>]
    type TryFinallyView = { Body: ExprId; Cleanup: ExprId }

    /// The scalar payload of a `While` node (`while Cond do Body`), minus the `ty`/`tok`
    /// that `exprTy`/`exprTok` already carry — the two nodes `exprChildren` yields, named
    /// by role.
    [<Struct>]
    type WhileView = { Cond: ExprId; Body: ExprId }

    /// The scalar payload of a `ForTo` node (`for Var = StartExpr to EndExpr do Body`),
    /// minus the `identTok`/`ty`/`tok` the node also carries. `StartExpr`/`EndExpr`/`Body`
    /// are the three `exprChildren` entries, named by role; `Var` is the loop binder,
    /// which has no pattern node behind it.
    [<Struct>]
    type ForToView =
        {
            Var: BinderId
            StartExpr: ExprId
            EndExpr: ExprId
            Body: ExprId
        }

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

    /// The format cluster with its sub-expressions held as handles — the `'e`
    /// instantiation of the one sink / segment / dyn-hole shape, so a formatter replays
    /// the same records here as in the tree domain.
    type FormatSink = FormatSinkG<ExprId>
    type FormatSeg = FormatSegG<FrozenType, Anchor, ExprId>
    type DynFormatHole = DynFormatHoleG<FrozenType, Anchor, ExprId>

    /// The scalar payload of a `Format` node — the sink and the interleaved
    /// literal/hole segments, minus the `ty`/`tok` the node also carries.
    [<Struct>]
    type FormatView =
        {
            Sink: FormatSink
            Segments: FormatSeg[]
        }

    /// The scalar payload of an `EnumCase` pattern — the case's `enumKey`/`caseName`
    /// identity, minus the `ty`/`tok` that `patTy`/`patTok` already carry.
    [<Struct>]
    type EnumCasePatView =
        { EnumKey: SymbolKey; CaseName: string }

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
