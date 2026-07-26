namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The wire-shape definitions for the id-indexable frozen pools (frozen-soa-cache-plan.md,
// "Wire format"); the interconversion logic that fills and drains them lives in
// `TastPools.fs`. Each frozen node is assigned a dense `int` pool id during a traversal of
// the DU; a node's child *expressions*/*patterns* are then addressable as their pool ids, so
// "node id k and its children by id" is an O(1) fetch — the random-access shape the
// projecting consumers want, which a forward-only decode stream could not serve.
//
// The pools coexist with the DU: the DU stays the working IN-MEMORY representation and the
// accessor stays DU-backed, but the pools are the STORED form — `FrozenCodec.flatten`/`thaw`
// serialize the columns and rebuild the DU from them, so `FrozenPools` must be a
// self-contained, serializable value (nothing may ride it that only makes sense with the
// source file still in hand — see `FrozenFileResidue`). The pools' correctness obligation is
// that they are INTERCONVERTIBLE with the DU trees — `toPools`/`ofPools` round-trip a
// `Frozen.TastFile` — proven structurally over the corpus by the flatten/thaw gate.
//
// Layout: EVERY pool is struct-of-arrays — parallel dense columns indexed by the matching
// `*PoolId`. The expr columns are `ExprShapes`/`ExprTys`/`ExprToks`, the child-id columns
// `ExprChildren`/`ExprPatChildren`, the sparse `ExprVarBinder`, plus `ExprPayloads`; the pat
// columns `PatShapes`/`PatTys`/`PatToks`/`PatChildren` plus `PatPayloads`; the decl columns
// `DeclShapes`/`DeclExprChildren`/`DeclPatChildren` plus `DeclPayloads` (decls carry no
// node-level `ty`/`tok`, so the type rides the payload). Each `*Payloads` array is a typed
// side array carrying ONLY a node's residual scalars/structure — its fields MINUS the
// `ty`/`tok`, the child expr/pat ids, and (for exprs) the `Var` binder id, all of which live
// in the columns. So NO pool holds a DU node: the whole `TExprG`/`TPat`/`TDecl` subtree
// dissolves into columns, which is the point — a retained node drags its entire nested body
// along with it, so freeze could never stop materializing the DU. (A `Type` decl's member
// bodies are the one exception: they ride `DeclPayload.Type`'s `TTypeDecl` opaquely, NOT
// pooled — the same opaque treatment the retained `Node` gave them, existing behavior.)
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

/// The residual payload of a frozen pattern node — one case per `PatShape`, carrying ONLY
/// the fields left after the columnar split drops `ty`/`tok` (the `PatTys`/`PatToks`
/// columns) and the child sub-pat ids (`PatChildren`, in `TastAccessor.patChildren` order;
/// patterns own no child expressions). Mirrors `FrozenCodec.writePat` for what each case
/// carries beyond those. Exhaustive: a new `TPat`/`PatShape` case fails to compile at
/// `patPayload`/`substitutePat`.
[<RequireQualifiedAccess>]
type PatPayload =
    /// The single binder this simple name pattern INTRODUCES — a `NodeKey` kept verbatim,
    /// the pat analogue of `ExprPayload.ForTo`'s `Var`: it is interned to a `BinderId` (so
    /// `Var` references resolve) yet reconstructed from here, and it names no sub-pattern so
    /// it is not a child.
    | NamedSimple of binding: NodeKey
    | Wildcard
    | Null
    | Tuple
    | Or
    | Const of TConstValue
    /// The field names, in source order; the sub-patterns are the children.
    | Record of fieldNames: string[]
    /// The union case name; the field sub-patterns are the children.
    | Union of caseName: string
    /// The tested-against type `T` of `:? T as x` (the `isinst` operand, distinct from the
    /// scrutinee type in `PatTys`); the bound inner `as`-pattern is the sole child.
    | TypeTestAs of testTy: FrozenType
    | EnumCase of
        {|
            EnumKey: SymbolKey
            CaseName: string
        |}

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

/// The residual payload of a frozen declaration node — one case per `DeclShape`. A decl
/// carries no uniform node-level `ty`/`tok` (there are no `DeclTys`/`DeclToks` columns), so
/// each case rides whatever type/scalars it needs. Its child expr/pat roots live in the
/// `DeclExprChildren`/`DeclPatChildren` columns. Mirrors `FrozenCodec.writeDecl`. Exhaustive:
/// a new `TDecl`/`DeclShape` case fails to compile at `declPayload`/`substituteDecl`.
[<RequireQualifiedAccess>]
type DeclPayload =
    /// The binding is the sole pat child, its value the sole expr child; `IsInline`/`Ty`
    /// (the binding's declared slot type) are the residual scalars.
    | Let of {| IsInline: bool; Ty: FrozenType |}
    /// The decl's declared type; the body is the sole expr child.
    | Expression of FrozenType
    /// The whole `type` declaration, VERBATIM. Its member bodies are NOT pooled — they ride
    /// opaquely here exactly as the retained `Node` did, which is `TastAccessor.declType`'s
    /// existing, intended behavior (a `Type` decl surfaces no expr/pat children).
    | Type of Frozen.TTypeDecl

/// Everything of a `Frozen.TastFile` that has NO pooled form yet — the file MINUS its decl
/// trees (the columns) and MINUS the seven side tables (the dense `BinderId`/`ExprPoolId`
/// associations). Exactly these four fields, each for its own reason:
///
///   * `Diagnostics` — a flat list keyed by `NodeKey`, in no pooled domain (a diagnostic can
///     name a node the emittable tree does not contain, so it cannot take a pool id).
///   * `IntrinsicReprKeys` / `Accessibility` — the two `SymbolKey`-keyed dictionaries. Their
///     key space is the SYMBOL identity, not the positional node identity the pools give, so
///     they are untouched by the dense-id remap.
///   * `InlineBodies` — the inline VOCABULARY. Its bodies are `TDeclG` trees that ride the DU
///     codec opaquely (they are templates, not emittable code, so no pool walk reaches them).
///
/// Naming the residue is the point: `FrozenPools` is then a self-contained, serializable
/// value, and what remains outside the columnar form is visible in the type rather than
/// hidden inside a retained whole `TastFile` (which would also make the pools unserializable
/// without re-serializing the DU file they were built from).
type FrozenFileResidue =
    {
        // Qualified: this file `open`s `XParsec.FSharp.Parser`, which also declares a
        // `Diagnostic`; the bare name would bind to the parser's, mistyping the field —
        // the same shadowing `TastFileG.Diagnostics` guards against.
        Diagnostics: XParsec.FSharp.SemanticAnalysis.Diagnostic list
        IntrinsicReprKeys: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, IntrinsicReprInfo>
        InlineBodies: EqArray<Frozen.TInlineValue>
        Accessibility: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, Accessibility>
    }

/// The frozen-only companion produced alongside the `Frozen.TastFile` DU (by
/// `TastPools.toPools`): the expr struct-of-arrays columns plus the pat/decl node columns
/// (indexable by the matching `*PoolId`) and the decl roots the file's `Decls` pooled to, in
/// source order. Kept OFF `TastFileG` — that record is shared with the `SemType`
/// instantiation, which has no pools.
///
/// The binder pool and the dense-keyed side tables give the file's identity keys a
/// positional home: `Binders` is the distinct binder NodeKeys, indexable by `BinderId`;
/// the source file's seven `Map<NodeKey,_>` side tables are re-expressed as `BinderId`-keyed
/// associations. `ofPools` rebuilds the maps from these (resolving each `BinderId` back
/// through `Binders`), so the round-trip proves the remap is a faithful bijection over
/// every referenced binder rather than trivially copying the source maps.
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
        /// The pattern pool as struct-of-arrays: parallel columns indexed by `PatPoolId`.
        /// `PatShapes` is the tag column; `PatTys`/`PatToks` the node's `ty`/`tok`;
        /// `PatChildren` the immediate sub-pat ids in `TastAccessor.patChildren` order
        /// (patterns own no child expressions); `PatPayloads` the residual per-case payload.
        /// No DU node is retained.
        PatShapes: PatShape[]
        PatTys: FrozenType[]
        PatToks: SyntaxToken[]
        PatChildren: PatPoolId[][]
        PatPayloads: PatPayload[]
        /// The declaration pool as struct-of-arrays, indexed by `DeclPoolId`. `DeclShapes`
        /// is the tag column; `DeclExprChildren`/`DeclPatChildren` the decl's immediate
        /// expr/pat roots (the `Let` binding's value + head pattern, or the `Expression`
        /// body — a `Type` decl surfaces none); `DeclPayloads` the residual per-case payload
        /// (which also carries the decl's type, there being no node-level `ty` column). No
        /// DU node is retained.
        DeclShapes: DeclShape[]
        DeclExprChildren: ExprPoolId[][]
        DeclPatChildren: PatPoolId[][]
        DeclPayloads: DeclPayload[]
        /// The pool ids of the source file's `Decls`, in source order — the entry points
        /// for a pool walk / rebuild.
        Roots: DeclPoolId[]
        /// The distinct simple-binder entries as two parallel columns indexed by `BinderId`,
        /// its OWN dense arrays disjoint from `Pats`: `BinderKeys` retains each binder's whole
        /// original `NodeKey` — the identity `Var.binding` and the side tables resolve against,
        /// and the DU round-trip's carrier for the `Raw` bits (kind included) the trees still
        /// reconstruct from, so the key cannot be dropped while the backing is DU-form.
        /// `BinderNamings` carries the three projections a backend names the binder by, sourced
        /// at `toPools` from the SAME key (`IsSynthetic`/`Offset`/`NameIndex`) so it is faithful
        /// to `binderName` by construction — the naming data that outlives the `NodeKey`. A
        /// `NamedSimple` pattern still also appears in the pat columns for the tree walk; these
        /// are the additional dense columns references resolve against, not a re-pointing.
        BinderKeys: NodeKey[]
        BinderNamings: BinderNaming[]
        /// The not-yet-pooled remainder of the source file, carried verbatim.
        Residue: FrozenFileResidue
        /// Six of the seven source `Map<NodeKey,_>` side tables, re-keyed by `BinderId`
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
