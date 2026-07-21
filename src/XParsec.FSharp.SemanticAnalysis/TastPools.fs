namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// Id-indexable pools for the frozen expr/pat/decl trees (frozen-soa-cache-plan.md,
// "Wire format"). Each frozen node is assigned a dense `int` pool id during a
// traversal of the DU; a node's child *expressions*/*patterns* are then addressable
// as their pool ids, so "node id k and its children by id" is an O(1) fetch — the
// random-access shape the projecting consumers want, which a forward-only decode
// stream could not serve.
//
// PURELY ADDITIVE and coexistent with the DU: the DU stays the working
// representation, the accessor stays DU-backed, and nothing reads the pools yet.
// The pools' correctness obligation this step is that they are INTERCONVERTIBLE with
// the DU trees — `toPools`/`ofPools` round-trip a `Frozen.TastFile` — proven over the
// corpus.
//
// Layout: the EXPRESSION pool is struct-of-arrays — parallel dense columns indexed by
// `ExprPoolId` (`ExprShapes`/`ExprTys`/`ExprToks`, the child-id columns
// `ExprChildren`/`ExprPatChildren`, the sparse `ExprVarBinder`) plus `ExprPayloads`, a
// typed side array carrying ONLY each node's residual scalars — its fields MINUS the
// `ty`/`tok`, the child expr/pat ids, and the `Var` binder id, all of which live in the
// columns. So the expr pool holds NO DU node: the whole `TExprG` subtree dissolves into
// columns, which is the point — a retained node drags its entire nested body along with
// it, so freeze could never stop materializing the DU. The pat and decl pools still
// retain their `Node` (a later slice extracts them); an expr's `ExprPatChildren` still
// reference that Node-backed pat pool, which is fine while the domains stage separately.
//
// A payload case that is a COMPOSITE carrier (`Match`/`TryWith` arms, `Format`
// segments, `StaticOptimization` clauses, `Range` step, `RecordCons`/`RecordClone`
// fields, `ExternalMember` receiver) records just enough STRUCTURE — per-arm guard
// flags, per-segment kind, per-clause constraints, presence flags — to redistribute the
// FLAT child columns back into their nested shape, since the node that once held that
// shape is gone. Both directions reuse the accessor's child enumeration
// (`TastAccessor.exprChildren`/`exprPatChildren`) and consume it in that same order —
// the coupling the round-trip test guards — rather than re-deriving the tree spine. The
// `ExprPayloads` build (`exprPayload`) and consume (`substituteExpr`) are inverse
// per-case matches, each exhaustive so a new `TExprG`/`ExprShape` case fails to compile.
//
// Identity, too, goes positional: a binder's identity after freeze IS its slot in a
// dedicated `Binders` column, not its 64-bit content key. Every distinct NodeKey a
// simple binder introduces (`NamedSimple.binding`, `ForTo.var`) is interned to a
// `BinderId`; the cross-references that named a definition by content key during
// analysis — `Var.binding` and six of the seven `Map<NodeKey,_>` side tables — name it
// by that id in the pool form. (The seventh, `FunVerdicts`, is keyed by a lambda-EXPRESSION
// key, not a binder, and takes the lambda id space — its dense id is the lambda's
// `ExprPoolId`.) `ofPools` resolves each id back through `Binders` to the retained
// NodeKey, so the round-trip exercises the remap rather than copying the keys back
// verbatim: a reference or side-table key that resolves to no interned binder faults
// here, which is the gate that keeps the enumeration honest. (Kind is not stored — its
// only role was to make a content key unique, which positional ids now do.)

/// A dense pool index into a `FrozenPools.Exprs` column.
[<Struct>]
type ExprPoolId = | ExprPoolId of int

/// A dense pool index into a `FrozenPools.Pats` column.
[<Struct>]
type PatPoolId = | PatPoolId of int

/// A dense pool index into a `FrozenPools.Decls` column.
[<Struct>]
type DeclPoolId = | DeclPoolId of int

/// A dense pool index into `FrozenPools.Binders` — the positional identity a frozen
/// binder takes on once kind dissolves. A binder is a NodeKey a `NamedSimple` pattern
/// or a `ForTo` loop variable INTRODUCES; the cross-references that named it by 64-bit
/// content key during analysis (`Var.binding`, the side-table keys) name it by this id
/// in the pool form.
[<Struct>]
type BinderId = | BinderId of int

/// The residual, EXPRESSION-FREE shape of a `Format` node's sink — its kind plus the
/// non-expr data (`ToWriter`/`ToStdOut`/`ToStdErr`'s trailing-newline flag). The sink's
/// own sub-expression (`ToWriter`'s writer, `ToBuilder`'s builder) rides `ExprChildren`
/// ahead of the segment children, so it is NOT re-listed here.
[<RequireQualifiedAccess>]
type FormatSinkShape =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of newline: bool
    | ToBuilder
    | ToString

/// The residual, EXPRESSION-FREE shape of one `Format` segment — its kind plus non-expr
/// data (a `Lit`'s text; a hole's `HoleSpec`, itself a leaf carrying no sub-expression;
/// a `DynHole`'s width/precision presence flags). Every sub-expression a segment holds
/// (a hole's value, a dyn-hole's width/precision/value, a callback's residue) rides
/// `ExprChildren` in `exprChildren` order; the presence flags re-nest it.
[<RequireQualifiedAccess>]
type FormatSegShape =
    | Lit of string
    | Hole of Frozen.HoleSpec
    | DynHole of hasWidth: bool * hasPrecision: bool * spec: Frozen.HoleSpec
    | CallbackHole of Frozen.HoleSpec

/// The residual payload of a frozen expression node — one case per `ExprShape`, carrying
/// ONLY the fields left after the columnar split drops `ty`/`tok` (the `ExprTys`/`ExprToks`
/// columns), the child expr ids (`ExprChildren`), the owned pat ids (`ExprPatChildren`),
/// and the `Var` binder id (`ExprVarBinder`). Mirrors `FrozenCodec.writeExpr` for what each
/// case carries beyond those. A composite carrier (`Match`/`TryWith`/`Range`/`RecordCons`/
/// `RecordClone`/`Format`/`StaticOptimization`/`ExternalMember`) additionally records the
/// STRUCTURE needed to redistribute the flat child columns back into their nested shape.
/// Exhaustive: a new `TExprG` case fails to compile at `exprPayload`/`substituteExpr`.
[<RequireQualifiedAccess>]
type ExprPayload =
    | Const of TConstValue
    | Var
    | External of
        {|
            CompiledName: string
            Key: SymbolKey voption
        |}
    | Lambda
    | App
    | Let
    | Use of Disposal
    | IfThenElse
    | Tuple
    | Sequential
    | While
    /// The loop binder + its `identTok` (neither the node's `tok` nor a `Var`-column
    /// binder — the `var` names its own binder, which is interned so references resolve).
    | ForTo of
        {|
            Var: NodeKey
            IdentTok: SyntaxToken
        |}
    | ForIn of Frozen.ForInEnumerator
    /// One flag per arm: whether the arm carries a guard. The scrutinee is the first
    /// child; each arm's guard (when present) and body follow in `exprChildren` order,
    /// its pat in `exprPatChildren` order. Arm count is the array length.
    | Match of guardPresent: bool[]
    /// As `Match`, but the guarded body is the first child (no scrutinee).
    | TryWith of guardPresent: bool[]
    | TryFinally
    | Assignment
    | Null
    /// Whether the optional `step` is present (start is the first child, stop the last).
    | Range of hasStep: bool
    /// The field names, in source order; the values are the child expressions.
    | RecordCons of fieldNames: string[]
    /// The override field names, in source order; the source is the first child and each
    /// override value follows.
    | RecordClone of overrideNames: string[]
    | FieldGet of fieldName: string
    | FieldSet of fieldName: string
    | UnionCons of caseName: string
    | New of
        {|
            ClassName: string
            Key: SymbolKey voption
        |}
    | MethodCall of
        {|
            Key: SymbolKey
            Via: CallVia<FrozenType>
        |}
    | PropertyGet of
        {|
            Key: SymbolKey
            Via: CallVia<FrozenType>
        |}
    | StaticMethodCall of SymbolKey
    | StaticPropertyGet of SymbolKey
    | StaticFieldGet of
        {|
            DeclKey: SymbolKey
            FieldName: string
        |}
    | StaticFieldSet of
        {|
            DeclKey: SymbolKey
            FieldName: string
        |}
    | ExternalMember of
        {|
            HasReceiver: bool
            Key: SymbolKey
            MemberName: string
            Storage: MemberStorage
        |}
    | Format of
        {|
            Sink: FormatSinkShape
            Segments: FormatSegShape[]
        |}
    | ILIntrinsic of
        {|
            OpCode: string
            TypeOperand: FrozenType voption
        |}
    /// The per-clause constraints, in source order; each clause's body is a child and the
    /// `defaultExpr` is the last child. Clause count is the array length.
    | StaticOptimization of clauseConstraints: EqArray<Frozen.TStaticOptConstraint>[]
    | Upcast
    | Downcast
    | TypeTest of testTy: FrozenType
    /// The receiver TYPE (a `FrozenType`, not a sub-expression) + member name; the args
    /// are the child expressions.
    | TraitCall of
        {|
            Receiver: FrozenType
            MemberName: string
        |}

/// One pattern pool entry. `PatChildren` are the ids of the immediate sub-patterns
/// (`TastAccessor.patChildren` order); patterns own no child expressions. `Node` is
/// the verbatim source DU node (carries the binder `NodeKey`, `ty`, `tok`).
[<Struct>]
type PatPoolEntry =
    {
        Shape: PatShape
        PatChildren: PatPoolId[]
        Node: Frozen.TPat
    }

/// The three naming projections a backend reads off a binder to emit its name WITHOUT
/// the `NodeKey` — exactly the bits `binderName` (`JsEmitHelpers.fs`) unpacks: a real
/// binder recovers its source name by slicing at `Offset`; a synthetic renders as
/// `_s<NameIndex>`. This is the naming DATA (post-freeze a binder's identity is its
/// slot, not its key) — kept separately from positional identity so it survives the
/// `NodeKey` drop once the backing flips off the DU.
[<Struct>]
type BinderNaming =
    {
        IsSynthetic: bool
        Offset: int
        NameIndex: int
    }

/// One binder pool entry — a SIMPLE name binding (`let x`, the `f`/`y` of `let f y =
/// …`, a `ForTo` loop variable, a synthetic binder). This is a DEDICATED dense column,
/// deliberately NOT an index into `Pats`: simple binders are the common case and must
/// not carry the heterogeneous payload the all-pattern-kinds `Pats` entry accommodates.
/// It retains the whole original `NodeKey` — the identity `Var.binding` and the side
/// tables resolve against, and the DU round-trip's carrier for the `Raw` bits (kind
/// included) the trees still reconstruct from, so the key cannot be dropped while the
/// backing is DU-form. Alongside it, `Naming` carries the three projections a backend
/// names the binder by, sourced at `toPools` from the SAME key (`IsSynthetic`/`Offset`/
/// `NameIndex`) so it is faithful to `binderName` by construction — the naming data that
/// outlives the `NodeKey`. The binder's TYPE splits out as its own dense field later.
[<Struct>]
type BinderPoolEntry = { Key: NodeKey; Naming: BinderNaming }

/// One declaration pool entry. `ExprChildren`/`PatChildren` are the ids of the decl's
/// immediate expr/pat roots — the `Let` binding's value + head pattern, or the
/// `Expression` body. A `Type` decl surfaces no expr/pat children (its member bodies
/// ride the retained `Node` opaquely, exactly as `TastAccessor.declType` treats them).
[<Struct>]
type DeclPoolEntry =
    {
        Shape: DeclShape
        ExprChildren: ExprPoolId[]
        PatChildren: PatPoolId[]
        Node: Frozen.TDecl
    }

/// The frozen-only companion `Freeze` produces alongside the `Frozen.TastFile` DU: the
/// expr struct-of-arrays columns plus the pat/decl node columns (indexable by the matching
/// `*PoolId`) and the decl roots the file's `Decls` pooled to, in source order. Kept OFF
/// `TastFileG` — that record is shared with the `SemType` instantiation, which has no pools.
///
/// `File` retains the source file VERBATIM as the carrier for everything the tree pools
/// do not (yet) hold — the side tables, `InlineBodies`, diagnostics. Its `Decls` are
/// re-authored from the pools by `ofPools`; every other field is the file's own. This
/// keeps `Frozen.TastFile`/`FrozenPools` both whole-file batch values while confining
/// the interconversion obligation to the decl TREES (pooling the side tables is later
/// work).
///
/// The binder pool and the dense-keyed side tables give the file's identity keys a
/// positional home: `Binders` is the distinct binder NodeKeys, indexable by `BinderId`;
/// the seven `Map<NodeKey,_>` side tables of `File` are re-expressed as `BinderId`-keyed
/// associations. `ofPools` rebuilds the maps from these (resolving each `BinderId` back
/// through `Binders`), so the round-trip proves the remap is a faithful bijection over
/// every referenced binder rather than trivially copying `File`'s maps.
type FrozenPools =
    {
        /// The expression pool as struct-of-arrays: these columns are parallel, each
        /// indexed by `ExprPoolId`. `ExprShapes` is the tag column; `ExprTys`/`ExprToks`
        /// the node's `ty`/`tok`; `ExprChildren` the immediate child-expr ids in
        /// `TastAccessor.exprChildren` order; `ExprPatChildren` the owned pat ids in
        /// `TastAccessor.exprPatChildren` order; `ExprVarBinder` the `Var` reference id
        /// (`ValueSome` only at a `Var`); `ExprPayloads` the residual per-case payload.
        /// No DU node is retained — the columns are Node-sufficient, which the round-trip
        /// gate proves.
        ExprShapes: ExprShape[]
        ExprTys: FrozenType[]
        ExprToks: SyntaxToken[]
        ExprChildren: ExprPoolId[][]
        ExprPatChildren: PatPoolId[][]
        ExprVarBinder: BinderId voption[]
        ExprPayloads: ExprPayload[]
        Pats: PatPoolEntry[]
        Decls: DeclPoolEntry[]
        /// The pool ids of `File.Decls`, in source order — the entry points for a pool
        /// walk / rebuild.
        Roots: DeclPoolId[]
        /// The distinct simple-binder entries, indexed by `BinderId` — its OWN dense
        /// array (see `BinderPoolEntry`), disjoint from `Pats`. `NamedSimple` patterns
        /// still also appear in `Pats` for the tree walk; this is the additional dense
        /// column references resolve against, not a re-pointing of `Pats`.
        Binders: BinderPoolEntry[]
        File: Frozen.TastFile
        /// Six of the seven `Map<NodeKey,_>` side tables of `File`, re-keyed by `BinderId`
        /// (a sparse association — a binder appears iff the map held it). `ofPools`
        /// rebuilds each map from its dense form.
        ModuleMembers: (BinderId * ModuleBindingInfo)[]
        TopLevelNames: (BinderId * string)[]
        ClosureReprs: (BinderId * ClosureRepr)[]
        /// The one side table keyed by a lambda-EXPRESSION `NodeKey` (a source lambda's
        /// `TastWalk.lambdaKey`, kind `ExprLambda`) rather than a binder, so it is re-keyed
        /// onto the lambda id space — a lambda's dense id IS its `ExprPoolId` (positional:
        /// every `Lambda` expr is already pooled), off the binder pool. `ofPools` inverts
        /// by recomputing that key from the lambda's `ExprToks` column (the same
        /// `NodeKey.ofToken … ExprLambda` `TastWalk.lambdaKey` computes), the Node now gone.
        FunVerdicts: (ExprPoolId * FunVerdict)[]
        GenericFnSchemes: (BinderId * FrozenConstraint list)[]
        BindingValReprs: (BinderId * Frozen.ValRepr)[]
        BindingTyparArities: (BinderId * int)[]
    }

[<RequireQualifiedAccess>]
module TastPools =

    /// The residual payload of a frozen expression node — its fields MINUS `ty`/`tok`, the
    /// child expr ids (`exprChildren`), the owned pat ids (`exprPatChildren`), and the `Var`
    /// binder id. The exact inverse of `substituteExpr`, mirroring `FrozenCodec.writeExpr`
    /// for what each case emits beyond those. Exhaustive on the DU with no catch-all, so a
    /// new `TExprG` case fails to compile here.
    let private exprPayload (e: Frozen.TExpr) : ExprPayload =
        match e with
        | TExprG.Const(value = value) -> ExprPayload.Const value
        | TExprG.Var _ -> ExprPayload.Var
        | TExprG.External(compiledName = compiledName; key = key) ->
            ExprPayload.External
                {|
                    CompiledName = compiledName
                    Key = key
                |}
        | TExprG.Lambda _ -> ExprPayload.Lambda
        | TExprG.App _ -> ExprPayload.App
        | TExprG.Let _ -> ExprPayload.Let
        | TExprG.Use(dispose = dispose) -> ExprPayload.Use dispose
        | TExprG.IfThenElse _ -> ExprPayload.IfThenElse
        | TExprG.Tuple _ -> ExprPayload.Tuple
        | TExprG.Sequential _ -> ExprPayload.Sequential
        | TExprG.While _ -> ExprPayload.While
        | TExprG.ForTo(var = var; identTok = identTok) -> ExprPayload.ForTo {| Var = var; IdentTok = identTok |}
        | TExprG.ForIn(enumerator = enumerator) -> ExprPayload.ForIn enumerator
        | TExprG.Match(arms = arms) ->
            ExprPayload.Match(arms |> EqArray.toArray |> Array.map (fun arm -> arm.Guard.IsSome))
        | TExprG.TryWith(arms = arms) ->
            ExprPayload.TryWith(arms |> EqArray.toArray |> Array.map (fun arm -> arm.Guard.IsSome))
        | TExprG.TryFinally _ -> ExprPayload.TryFinally
        | TExprG.Assignment _ -> ExprPayload.Assignment
        | TExprG.Null _ -> ExprPayload.Null
        | TExprG.Range(step = step) -> ExprPayload.Range step.IsSome
        | TExprG.RecordCons(fields = fields) -> ExprPayload.RecordCons(fields |> EqArray.toArray |> Array.map fst)
        | TExprG.RecordClone(overrides = overrides) ->
            ExprPayload.RecordClone(overrides |> EqArray.toArray |> Array.map fst)
        | TExprG.FieldGet(fieldName = fieldName) -> ExprPayload.FieldGet fieldName
        | TExprG.FieldSet(fieldName = fieldName) -> ExprPayload.FieldSet fieldName
        | TExprG.UnionCons(caseName = caseName) -> ExprPayload.UnionCons caseName
        | TExprG.New(className = className; key = key) -> ExprPayload.New {| ClassName = className; Key = key |}
        | TExprG.MethodCall(key = key; via = via) -> ExprPayload.MethodCall {| Key = key; Via = via |}
        | TExprG.PropertyGet(key = key; via = via) -> ExprPayload.PropertyGet {| Key = key; Via = via |}
        | TExprG.StaticMethodCall(key = key) -> ExprPayload.StaticMethodCall key
        | TExprG.StaticPropertyGet(key = key) -> ExprPayload.StaticPropertyGet key
        | TExprG.StaticFieldGet(declKey = declKey; fieldName = fieldName) ->
            ExprPayload.StaticFieldGet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | TExprG.StaticFieldSet(declKey = declKey; fieldName = fieldName) ->
            ExprPayload.StaticFieldSet
                {|
                    DeclKey = declKey
                    FieldName = fieldName
                |}
        | TExprG.ExternalMember(receiver = receiver; key = key; memberName = memberName; storage = storage) ->
            ExprPayload.ExternalMember
                {|
                    HasReceiver = receiver.IsSome
                    Key = key
                    MemberName = memberName
                    Storage = storage
                |}
        | TExprG.Format(sink = sink; segments = segments) ->
            let sink' =
                match sink with
                | FormatSinkG.ToStdOut newline -> FormatSinkShape.ToStdOut newline
                | FormatSinkG.ToStdErr newline -> FormatSinkShape.ToStdErr newline
                | FormatSinkG.ToWriter(newline = newline) -> FormatSinkShape.ToWriter newline
                | FormatSinkG.ToBuilder _ -> FormatSinkShape.ToBuilder
                | FormatSinkG.ToString -> FormatSinkShape.ToString

            let segments' =
                segments
                |> EqArray.toArray
                |> Array.map (fun seg ->
                    match seg with
                    | FormatSegG.Lit s -> FormatSegShape.Lit s
                    | FormatSegG.Hole(spec, _) -> FormatSegShape.Hole spec
                    | FormatSegG.DynHole hole ->
                        FormatSegShape.DynHole(hole.Width.IsSome, hole.Precision.IsSome, hole.Spec)
                    | FormatSegG.CallbackHole(spec, _) -> FormatSegShape.CallbackHole spec
                )

            ExprPayload.Format {| Sink = sink'; Segments = segments' |}
        | TExprG.ILIntrinsic(opCode = opCode; typeOperand = typeOperand) ->
            ExprPayload.ILIntrinsic
                {|
                    OpCode = opCode
                    TypeOperand = typeOperand
                |}
        | TExprG.StaticOptimization(clauses = clauses) ->
            ExprPayload.StaticOptimization(clauses |> EqArray.toArray |> Array.map (fun clause -> clause.Constraints))
        | TExprG.Upcast _ -> ExprPayload.Upcast
        | TExprG.Downcast _ -> ExprPayload.Downcast
        | TExprG.TypeTest(testTy = testTy) -> ExprPayload.TypeTest testTy
        | TExprG.TraitCall(receiver = receiver; memberName = memberName) ->
            ExprPayload.TraitCall
                {|
                    Receiver = receiver
                    MemberName = memberName
                |}

    /// Pool the frozen tree of `file.Decls`, assigning each reachable node a dense id
    /// and recording its child edges as ids. Post-order: a node's children are pooled
    /// (and so given lower ids) before the node itself is recorded, so every child id a
    /// column names already resolves. The child enumeration is the accessor's — no
    /// tree-shape knowledge is duplicated here.
    let toPools (file: Frozen.TastFile) : FrozenPools =
        // The expression pool as parallel column builders (struct-of-arrays); all are
        // appended together per node so they stay index-aligned by `ExprPoolId`.
        let exprShapes = ResizeArray<ExprShape>()
        let exprTys = ResizeArray<FrozenType>()
        let exprToks = ResizeArray<SyntaxToken>()
        let exprChildrenCol = ResizeArray<ExprPoolId[]>()
        let exprPatChildrenCol = ResizeArray<PatPoolId[]>()
        let exprPayloads = ResizeArray<ExprPayload>()

        // Each `Var`'s expr id + its binding NodeKey, captured in pass 1 and resolved to a
        // `BinderId` in pass 2 — a `Var` may name a binder pooled after it (a forward /
        // mutually-recursive reference), so the enumeration must complete first.
        let varBindings = ResizeArray<struct (int * NodeKey)>()

        let pats = ResizeArray<PatPoolEntry>()
        let decls = ResizeArray<DeclPoolEntry>()

        // The binder pool: each distinct NodeKey a `NamedSimple` pattern or `ForTo`
        // loop variable introduces, interned to a dense `BinderId` on first encounter.
        // The introducing sites are enumerated off the accessor as the tree is walked,
        // so nothing re-derives which nodes bind.
        let binders = ResizeArray<BinderPoolEntry>()
        let binderIds = System.Collections.Generic.Dictionary<NodeKey, BinderId>()

        // The lambda id space: a source lambda's dense id IS its `ExprPoolId` (positional
        // — every `Lambda` expr is already in `Exprs`). `FunVerdicts`, the one side table
        // keyed by a lambda-EXPRESSION key rather than a binder, resolves against this map;
        // it is recorded under the SAME `TastWalk.lambdaKey` codegen looks the verdict up
        // by, so the pool key space matches the DU lookup key by construction.
        let lambdaIds = System.Collections.Generic.Dictionary<NodeKey, ExprPoolId>()

        let internBinder (k: NodeKey) : unit =
            match binderIds.TryGetValue k with
            | true, _ -> ()
            | false, _ ->
                binderIds.Add(k, BinderId binders.Count)

                binders.Add
                    {
                        Key = k
                        // The naming triple IS the key's projections — the same three bits
                        // `binderName` reads — so it is faithful to emitted names by
                        // construction, and stays correct after the key itself retires.
                        Naming =
                            {
                                IsSynthetic = k.IsSynthetic
                                Offset = k.Offset
                                NameIndex = k.NameIndex
                            }
                    }

        let rec poolPat (p: Frozen.TPat) : PatPoolId =
            match TastAccessor.patBinder p with
            | ValueSome k -> internBinder k
            | ValueNone -> ()

            let kids = TastAccessor.patChildren p |> Array.map poolPat
            let id = pats.Count

            pats.Add
                {
                    Shape = TastAccessor.patKind p
                    PatChildren = kids
                    Node = p
                }

            PatPoolId id

        let rec poolExpr (e: Frozen.TExpr) : ExprPoolId =
            match TastAccessor.exprKind e with
            | ExprShape.ForTo -> internBinder (TastAccessor.exprForTo e).Var
            | _ -> ()

            let exprKids = TastAccessor.exprChildren e |> Array.map poolExpr
            let patKids = TastAccessor.exprPatChildren e |> Array.map poolPat
            let id = exprShapes.Count

            exprShapes.Add(TastAccessor.exprKind e)
            exprTys.Add(TastAccessor.exprTy e)
            exprToks.Add(TastAccessor.exprTok e)
            exprChildrenCol.Add exprKids
            exprPatChildrenCol.Add patKids
            exprPayloads.Add(exprPayload e)

            // A `Var`'s binder reference resolves in pass 2 (see `varBindings`); a lambda's
            // positional identity is this very slot, stamped so `FunVerdicts`
            // (lambda-expression-keyed) resolves onto it.
            match TastAccessor.exprKind e with
            | ExprShape.Var -> varBindings.Add(struct (id, TastAccessor.exprVarBinding e))
            | ExprShape.Lambda -> lambdaIds.[TastWalk.lambdaKey e] <- ExprPoolId id
            | _ -> ()

            ExprPoolId id

        let poolDecl (d: Frozen.TDecl) : DeclPoolId =
            let struct (exprKids, patKids) =
                match TastAccessor.declKind d with
                | DeclShape.Let ->
                    let v = TastAccessor.declLet d
                    struct ([| poolExpr v.Value |], [| poolPat v.Binding |])
                | DeclShape.Expression -> struct ([| poolExpr (TastAccessor.declExpression d) |], [||])
                | DeclShape.Type -> struct ([||], [||])

            let id = decls.Count

            decls.Add
                {
                    Shape = TastAccessor.declKind d
                    ExprChildren = exprKids
                    PatChildren = patKids
                    Node = d
                }

            DeclPoolId id

        let roots = file.Decls |> EqArray.toArray |> Array.map poolDecl

        // Resolve a reference/side-table key to the binder it names. A miss means the
        // referent was minted by no `NamedSimple`/`ForTo` node — an incomplete binder
        // enumeration, which is exactly the failure the id-resolution gate exists to
        // surface.
        let binderIdOf (k: NodeKey) : BinderId =
            match binderIds.TryGetValue k with
            | true, id -> id
            | false, _ -> failwithf "TastPools.toPools: %O references a binder no NamedSimple/ForTo node introduced" k

        // The lambda-key analogue: a `FunVerdicts` key that names no pooled lambda is the
        // honest failure a lambda-keyed entry naming no pooled lambda should be.
        let lambdaIdOf (k: NodeKey) : ExprPoolId =
            match lambdaIds.TryGetValue k with
            | true, id -> id
            | false, _ -> failwithf "TastPools.toPools: FunVerdicts key %O names no pooled lambda" k

        // Second pass: now the enumeration is complete, route each `Var`'s reference edge
        // to its binder's dense id — the sparse `ExprVarBinder` column (`ValueNone` at
        // every non-`Var` slot).
        let exprVarBinder: BinderId voption[] = Array.create exprShapes.Count ValueNone

        for (struct (id, key)) in varBindings do
            exprVarBinder.[id] <- ValueSome(binderIdOf key)

        // One generic remap over the side tables, parameterized by the key resolver: the
        // binder-keyed tables pass `binderIdOf`, `FunVerdicts` passes `lambdaIdOf`. A second
        // resolver, not a second remap, so the two id spaces share one enumeration.
        let remapSideTable (resolve: NodeKey -> 'id) (m: Map<NodeKey, 'v>) : ('id * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve k, v)

        {
            ExprShapes = exprShapes.ToArray()
            ExprTys = exprTys.ToArray()
            ExprToks = exprToks.ToArray()
            ExprChildren = exprChildrenCol.ToArray()
            ExprPatChildren = exprPatChildrenCol.ToArray()
            ExprVarBinder = exprVarBinder
            ExprPayloads = exprPayloads.ToArray()
            Pats = pats.ToArray()
            Decls = decls.ToArray()
            Roots = roots
            Binders = binders.ToArray()
            File = file
            ModuleMembers = remapSideTable binderIdOf file.ModuleMembers
            TopLevelNames = remapSideTable binderIdOf file.TopLevelNames
            ClosureReprs = remapSideTable binderIdOf file.ClosureReprs
            FunVerdicts = remapSideTable lambdaIdOf file.FunVerdicts
            GenericFnSchemes = remapSideTable binderIdOf file.GenericFnSchemes
            BindingValReprs = remapSideTable binderIdOf file.BindingValReprs
            BindingTyparArities = remapSideTable binderIdOf file.BindingTyparArities
        }

    // ── the inverse: rebuild the DU trees from the pools ────────────────────
    //
    // `substituteExpr` re-authors a node from its columns — `ty`/`tok`, the resolved
    // `Var` binder, the `ExprPayload` residual scalars/structure — and the ALREADY-REBUILT
    // child subtrees, with NO template node (the expr pool holds none). The children are
    // consumed in the exact order `TastAccessor.exprChildren`/`exprPatChildren` enumerated
    // them (`nextE`/`nextP` are order cursors) — the one coupling the round-trip gate
    // proves. The match on `ExprPayload` is exhaustive with no catch-all (the inverse of
    // `exprPayload`), so a new shape fails to compile here. `substitutePat`/`substituteDecl`
    // still take a template `Node` — the pat and decl pools retain theirs this pass.

    let private substituteExpr
        (ty: FrozenType)
        (tok: SyntaxToken)
        (varBinding: NodeKey voption)
        (payload: ExprPayload)
        (es: Frozen.TExpr[])
        (ps: Frozen.TPat[])
        : Frozen.TExpr =
        let mutable ei = 0
        let mutable pi = 0

        let nextE () =
            let x = es.[ei] in
            ei <- ei + 1
            x

        let nextP () =
            let x = ps.[pi] in
            pi <- pi + 1
            x

        match payload with
        // `binding` is supplied from the dense id, so the round-trip exercises the remap.
        | ExprPayload.Var ->
            match varBinding with
            | ValueSome k -> TExprG.Var(k, ty, tok)
            | ValueNone -> failwith "TastPools.ofPools: a Var entry carries no resolved binder id"
        | ExprPayload.Const value -> TExprG.Const(value, ty, tok)
        | ExprPayload.External p -> TExprG.External(p.CompiledName, p.Key, ty, tok)
        | ExprPayload.Null -> TExprG.Null(ty, tok)
        | ExprPayload.StaticPropertyGet key -> TExprG.StaticPropertyGet(key, ty, tok)
        | ExprPayload.StaticFieldGet p -> TExprG.StaticFieldGet(p.DeclKey, p.FieldName, ty, tok)
        | ExprPayload.Lambda ->
            let param = nextP ()
            let body = nextE ()
            TExprG.Lambda(param, body, ty, tok)
        | ExprPayload.App ->
            let fn = nextE ()
            let arg = nextE ()
            TExprG.App(fn, arg, ty, tok)
        | ExprPayload.Let ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Let(binding, value, body, ty, tok)
        | ExprPayload.Use dispose ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Use(binding, value, body, dispose, ty, tok)
        | ExprPayload.IfThenElse ->
            let cond = nextE ()
            let thenExpr = nextE ()
            let elseExpr = nextE ()
            TExprG.IfThenElse(cond, thenExpr, elseExpr, ty, tok)
        | ExprPayload.Tuple -> TExprG.Tuple(EqArray.ofArray es, ty, tok)
        | ExprPayload.Sequential -> TExprG.Sequential(EqArray.ofArray es, ty, tok)
        | ExprPayload.While ->
            let cond = nextE ()
            let body = nextE ()
            TExprG.While(cond, body, ty, tok)
        | ExprPayload.ForTo p ->
            let startExpr = nextE ()
            let endExpr = nextE ()
            let body = nextE ()
            TExprG.ForTo(p.Var, p.IdentTok, startExpr, endExpr, body, ty, tok)
        | ExprPayload.ForIn enumerator ->
            let pat = nextP ()
            let source = nextE ()
            let body = nextE ()
            TExprG.ForIn(pat, source, body, enumerator, ty, tok)
        | ExprPayload.Match guardPresent ->
            let scrutinee = nextE ()

            let arms' =
                guardPresent
                |> Array.map (fun hasGuard ->
                    let pat = nextP ()
                    let guard = if hasGuard then Some(nextE ()) else None
                    let body = nextE ()

                    {
                        Pat = pat
                        Guard = guard
                        Body = body
                    }
                )
                |> EqArray.ofArray

            TExprG.Match(scrutinee, arms', ty, tok)
        | ExprPayload.TryWith guardPresent ->
            let body = nextE ()

            let arms' =
                guardPresent
                |> Array.map (fun hasGuard ->
                    let pat = nextP ()
                    let guard = if hasGuard then Some(nextE ()) else None
                    let armBody = nextE ()

                    {
                        Pat = pat
                        Guard = guard
                        Body = armBody
                    }
                )
                |> EqArray.ofArray

            TExprG.TryWith(body, arms', ty, tok)
        | ExprPayload.TryFinally ->
            let body = nextE ()
            let cleanup = nextE ()
            TExprG.TryFinally(body, cleanup, ty, tok)
        | ExprPayload.Assignment ->
            let lhs = nextE ()
            let rhs = nextE ()
            TExprG.Assignment(lhs, rhs, ty, tok)
        | ExprPayload.Range hasStep ->
            let startExpr = nextE ()
            let step' = if hasStep then Some(nextE ()) else None
            let stopExpr = nextE ()
            TExprG.Range(startExpr, step', stopExpr, ty, tok)
        | ExprPayload.RecordCons fieldNames ->
            let fields' =
                fieldNames |> Array.map (fun name -> (name, nextE ())) |> EqArray.ofArray

            TExprG.RecordCons(fields', ty, tok)
        | ExprPayload.RecordClone overrideNames ->
            let source = nextE ()

            let overrides' =
                overrideNames |> Array.map (fun name -> (name, nextE ())) |> EqArray.ofArray

            TExprG.RecordClone(source, overrides', ty, tok)
        | ExprPayload.FieldGet fieldName ->
            let receiver = nextE ()
            TExprG.FieldGet(receiver, fieldName, ty, tok)
        | ExprPayload.FieldSet fieldName ->
            let receiver = nextE ()
            let value = nextE ()
            TExprG.FieldSet(receiver, fieldName, value, ty, tok)
        | ExprPayload.UnionCons caseName -> TExprG.UnionCons(caseName, EqArray.ofArray es, ty, tok)
        | ExprPayload.New p -> TExprG.New(p.ClassName, p.Key, EqArray.ofArray es, ty, tok)
        | ExprPayload.MethodCall p ->
            let receiver = nextE ()
            // The remaining `es` (after the receiver) are exactly the args, in order.
            let args = es.[ei..] |> EqArray.ofArray
            TExprG.MethodCall(receiver, p.Key, p.Via, args, ty, tok)
        | ExprPayload.PropertyGet p ->
            let receiver = nextE ()
            TExprG.PropertyGet(receiver, p.Key, p.Via, ty, tok)
        | ExprPayload.StaticMethodCall key -> TExprG.StaticMethodCall(key, EqArray.ofArray es, ty, tok)
        | ExprPayload.StaticFieldSet p ->
            let value = nextE ()
            TExprG.StaticFieldSet(p.DeclKey, p.FieldName, value, ty, tok)
        | ExprPayload.ExternalMember p ->
            let receiver' = if p.HasReceiver then ValueSome(nextE ()) else ValueNone
            TExprG.ExternalMember(receiver', p.Key, p.MemberName, p.Storage, ty, tok)
        | ExprPayload.Format p ->
            // The sink child (writer / builder) is consumed BEFORE the segment children —
            // the order `exprChildren` yields, which the segment loop then continues.
            let sink' =
                match p.Sink with
                | FormatSinkShape.ToWriter newline -> FormatSinkG.ToWriter(nextE (), newline)
                | FormatSinkShape.ToBuilder -> FormatSinkG.ToBuilder(nextE ())
                | FormatSinkShape.ToStdOut newline -> FormatSinkG.ToStdOut newline
                | FormatSinkShape.ToStdErr newline -> FormatSinkG.ToStdErr newline
                | FormatSinkShape.ToString -> FormatSinkG.ToString

            let segments' =
                p.Segments
                |> Array.map (fun seg ->
                    match seg with
                    | FormatSegShape.Lit s -> FormatSegG.Lit s
                    | FormatSegShape.Hole spec -> FormatSegG.Hole(spec, nextE ())
                    | FormatSegShape.DynHole(hasWidth, hasPrecision, spec) ->
                        let width = if hasWidth then ValueSome(nextE ()) else ValueNone
                        let precision = if hasPrecision then ValueSome(nextE ()) else ValueNone
                        let value = nextE ()

                        FormatSegG.DynHole
                            {
                                Width = width
                                Precision = precision
                                Spec = spec
                                Value = value
                            }
                    | FormatSegShape.CallbackHole spec -> FormatSegG.CallbackHole(spec, nextE ())
                )
                |> EqArray.ofArray

            TExprG.Format(sink', segments', ty, tok)
        | ExprPayload.ILIntrinsic p -> TExprG.ILIntrinsic(p.OpCode, p.TypeOperand, EqArray.ofArray es, ty, tok)
        | ExprPayload.StaticOptimization clauseConstraints ->
            let clauses' =
                clauseConstraints
                |> Array.map (fun constraints ->
                    {
                        Constraints = constraints
                        Body = nextE ()
                    }
                )
                |> EqArray.ofArray

            let defaultExpr = nextE ()
            TExprG.StaticOptimization(clauses', defaultExpr, ty, tok)
        | ExprPayload.Upcast -> TExprG.Upcast(nextE (), ty, tok)
        | ExprPayload.Downcast -> TExprG.Downcast(nextE (), ty, tok)
        | ExprPayload.TypeTest testTy ->
            let source = nextE ()
            TExprG.TypeTest(source, testTy, ty, tok)
        | ExprPayload.TraitCall p -> TExprG.TraitCall(p.Receiver, p.MemberName, EqArray.ofArray es, ty, tok)

    let private substitutePat (node: Frozen.TPat) (ps: Frozen.TPat[]) : Frozen.TPat =
        let mutable pi = 0

        let nextP () =
            let x = ps.[pi] in
            pi <- pi + 1
            x

        match node with
        | TPatG.NamedSimple _
        | TPatG.Wildcard _
        | TPatG.Const _
        | TPatG.Null _
        | TPatG.EnumCase _ -> node
        | TPatG.Tuple(_, ty, tok) -> TPatG.Tuple(EqArray.ofArray ps, ty, tok)
        | TPatG.Or(_, ty, tok) -> TPatG.Or(EqArray.ofArray ps, ty, tok)
        | TPatG.Record(fields, ty, tok) ->
            let fields' =
                fields
                |> EqArray.toArray
                |> Array.map (fun (name, _) -> (name, nextP ()))
                |> EqArray.ofArray

            TPatG.Record(fields', ty, tok)
        | TPatG.Union(caseName, _, ty, tok) -> TPatG.Union(caseName, EqArray.ofArray ps, ty, tok)
        | TPatG.TypeTestAs(testTy, _, ty, tok) -> TPatG.TypeTestAs(testTy, nextP (), ty, tok)

    let private substituteDecl (node: Frozen.TDecl) (es: Frozen.TExpr[]) (ps: Frozen.TPat[]) : Frozen.TDecl =
        match node with
        | TDeclG.Let(_, _, isInline, ty) -> TDeclG.Let(ps.[0], es.[0], isInline, ty)
        | TDeclG.Expression(_, ty) -> TDeclG.Expression(es.[0], ty)
        | TDeclG.Type _ -> node

    /// Rebuild the `Frozen.TastFile` DU from the pools — the inverse of `toPools`. Its
    /// `Decls` are re-authored from the pool roots (the tree interconversion under
    /// test); every other field is the retained source file's, verbatim.
    let ofPools (pools: FrozenPools) : Frozen.TastFile =
        // Resolve a dense id back to the binder NodeKey it names — the inverse of the
        // `toPools` interning. This is the resolution the reference remap and the side
        // tables both invert through.
        let binderKey (BinderId i) : NodeKey = pools.Binders.[i].Key

        // The inverse of the lambda id space: a lambda's `ExprPoolId` back to the `NodeKey`
        // codegen looks its verdict up under. With the Node gone, recompute that key from
        // the lambda's `ExprToks` column — the same `NodeKey.ofToken … ExprLambda`
        // `TastWalk.lambdaKey` computes, and the construction `toPools` keyed it by.
        let lambdaKeyOf (ExprPoolId i) : NodeKey =
            NodeKey.ofToken pools.ExprToks.[i] NodeKind.ExprLambda

        let rec fromPat (PatPoolId i) : Frozen.TPat =
            let entry = pools.Pats.[i]
            let ps = entry.PatChildren |> Array.map fromPat
            substitutePat entry.Node ps

        let rec fromExpr (ExprPoolId i) : Frozen.TExpr =
            let es = pools.ExprChildren.[i] |> Array.map fromExpr
            let ps = pools.ExprPatChildren.[i] |> Array.map fromPat
            let varBinding = pools.ExprVarBinder.[i] |> ValueOption.map binderKey
            substituteExpr pools.ExprTys.[i] pools.ExprToks.[i] varBinding pools.ExprPayloads.[i] es ps

        let fromDecl (DeclPoolId i) : Frozen.TDecl =
            let entry = pools.Decls.[i]
            let es = entry.ExprChildren |> Array.map fromExpr
            let ps = entry.PatChildren |> Array.map fromPat
            substituteDecl entry.Node es ps

        let decls = pools.Roots |> Array.map fromDecl |> EqArray.ofArray

        // Rebuild a side table from its dense form, resolving each `BinderId` back to its
        // NodeKey. Reconstructing the maps here (rather than retaining `File`'s) is what
        // makes the round-trip prove the key remap, not just the decl trees.
        // One generic rebuild, parameterized by the inverse key resolver: the binder-keyed
        // tables pass `binderKey`, `FunVerdicts` passes `lambdaKeyOf`.
        let rebuildSideTable (resolve: 'id -> NodeKey) (dense: ('id * 'v)[]) : Map<NodeKey, 'v> =
            dense |> Array.map (fun (id, v) -> resolve id, v) |> Map.ofArray

        { pools.File with
            Decls = decls
            ModuleMembers = rebuildSideTable binderKey pools.ModuleMembers
            TopLevelNames = rebuildSideTable binderKey pools.TopLevelNames
            ClosureReprs = rebuildSideTable binderKey pools.ClosureReprs
            FunVerdicts = rebuildSideTable lambdaKeyOf pools.FunVerdicts
            GenericFnSchemes = rebuildSideTable binderKey pools.GenericFnSchemes
            BindingValReprs = rebuildSideTable binderKey pools.BindingValReprs
            BindingTyparArities = rebuildSideTable binderKey pools.BindingTyparArities
        }
