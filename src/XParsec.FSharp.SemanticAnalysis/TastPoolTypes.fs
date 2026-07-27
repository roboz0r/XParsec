namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Parser

// The wire-shape definitions for the id-indexable frozen pools; the logic that fills them
// lives in `TastPools.fs` and the logic that drains them in `TastUnpool.fs`. Each frozen node is assigned a dense `int` pool id during a traversal of
// the DU; a node's child *expressions*/*patterns* are then addressable as their pool ids, so
// "node id k and its children by id" is an O(1) fetch — the random-access shape the
// projecting consumers want, which a forward-only decode stream could not serve.
//
// The pools are the working representation, the stored one, AND freeze's output:
// `TastAccessor` reads these columns and nothing else, `FrozenCodec.flatten`/`thaw`
// serialize them verbatim, and `Freeze.run` yields them. So `FrozenPools` must be a
// self-contained, serializable value (nothing may ride it that only makes sense with the
// source file still in hand — see `FrozenFileResidue`). The DU survives as freeze's own
// internal construction shape, as the cross-unit inline-template wire, and as a debug/test
// facility; the pools' correctness obligation is that the two are INTERCONVERTIBLE —
// `toPools`/`ofPools` round-trip a `Frozen.TastFile` — which is what proves the columns
// carry the whole tree, and is gated structurally over the corpus.
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
// along with it, so freeze could never stop materializing the DU.
//
// That now holds for EVERY tree the file bears, not just the emittable decls. The two
// carriers that used to hold trees opaquely are pooled like any other: a `Type` decl's
// member bodies (`PooledTypeDecl` — ids in the declaration shape) and the inline
// vocabulary (`InlineTemplates` — its own root array). The third, a binding's `ValRepr`
// tuple-group patterns (`PooledValRepr`), holds no tree of its own at all: it names the
// LAMBDA SPINE's own pattern nodes, being derived from that spine rather than carried
// alongside it. `FrozenFileResidue` correspondingly holds NO tree either.
//
// A payload case that is a COMPOSITE carrier (`Match`/`TryWith` arms, `Format`
// segments, `StaticOptimization` clauses, `Range` step, `RecordCons`/`RecordClone`
// fields, `ExternalMember` receiver) records just enough STRUCTURE — per-arm guard
// flags, per-segment kind, per-clause constraints, presence flags — to redistribute the
// FLAT child columns back into their nested shape, since the node that once held that
// shape is gone. Both directions reuse the pooling walk's child enumeration
// (`TastPools.exprChildren`/`exprPatChildren`) and consume it in that same order —
// the coupling the round-trip test guards — rather than re-deriving the tree spine. The
// `ExprPayloads` build (`exprPayload`) and consume (`substituteExpr`) are inverse
// per-case matches, each exhaustive so a new `TExprG`/`ExprShape` case fails to compile.
//
// Identity, too, goes positional: a binder's identity after freeze IS its slot in a
// dedicated `Binders` column, not its 64-bit content key. Every distinct NodeKey a
// definition site introduces is interned to a `BinderId` — the pattern/loop binders
// (`NamedSimple.binding`, `ForTo.var`) and the bare key slots a type declaration binds
// with no pattern node behind them (`TTypeDeclG.boundKeys`: a member's `this`/`base`
// and parameters, a ctor's parameters and locals), which its member BODIES name by
// `Var` exactly as a function body names a `let`. The cross-references that named a
// definition by content key during analysis — `Var.binding` and six of the seven
// `Map<NodeKey,_>` side tables — name it by that id in the pool form. (The seventh,
// `FunVerdicts`, is keyed by a lambda-EXPRESSION
// key, not a binder, and takes the lambda id space — its dense id is the lambda's
// `ExprPoolId`.) `ofPools` resolves each id back through `Binders` to the retained
// NodeKey, so the round-trip exercises the remap rather than copying the keys back
// verbatim: a reference or side-table key that resolves to no interned binder faults
// here, which is the gate that keeps the enumeration honest. (Kind is not stored — its
// only role was to make a content key unique, which positional ids now do.)

/// The post-freeze shape tag of an expression node — one case per `TExprG` case, and
/// the `ExprShapes` tag column's element type. This is NOT `NodeKind`: `NodeKind` is
/// the pre-freeze CST content-address role, which freeze dissolves (plan § *Freeze
/// regime*). The case names mirror `TExprG` (documented there); a new `TExprG` case
/// makes the exhaustive matches that build and drain the columns fail to compile, so
/// this stays in lockstep.
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

/// The post-freeze shape tag of a pattern node — one case per `TPatG` case (mirrors
/// `ExprShape`'s relationship to `TExprG`).
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

/// The post-freeze shape tag of a declaration node — one case per `TDeclG` case.
[<RequireQualifiedAccess>]
type DeclShape =
    | Let
    | Expression
    | Type

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
/// binder takes on once kind dissolves. A binder is a NodeKey a definition site
/// INTRODUCES — a `NamedSimple` pattern, a `ForTo` loop variable, or one of a type
/// declaration's pattern-less key slots (`TTypeDeclG.boundKeys`); the cross-references
/// that named it by 64-bit content key during analysis (`Var.binding`, the side-table
/// keys) name it by this id in the pool form.
[<Struct>]
type BinderId = | BinderId of int

/// A side table in its STORED form: a sparse association over a dense id space — an entry
/// iff the source `Map<NodeKey, _>` held that binder / lambda. An array because that is
/// what the wire and the round-trip want; it answers no question on its own, so every
/// consumer indexes it first (`DenseTable.index`).
type DenseTable<'id, 'v> = ('id * 'v)[]

[<RequireQualifiedAccess>]
module DenseTable =

    /// The stored table as the LOOKUP every consumer turns it into. One container choice,
    /// made once: a `Dictionary` over keys that are already dense ints, rather than each
    /// reader picking its own (an `F# Map` tree here, a hand-filled `Dictionary` loop
    /// there) and restating why.
    ///
    /// A repeated id FAULTS rather than letting the later entry win. The tables are built
    /// from a `Map`, so a repeat means the producer lost an entry — which `readOnlyDict`
    /// would swallow.
    let index (table: DenseTable<'id, 'v>) : System.Collections.Generic.IReadOnlyDictionary<'id, 'v> =
        let d = System.Collections.Generic.Dictionary<'id, 'v>(table.Length)

        for (id, v) in table do
            if not (d.TryAdd(id, v)) then
                failwithf "DenseTable.index: id %O appears twice" id

        d

/// A `type` declaration whose seven member/preamble/ctor BODY slots name their expression
/// by pool id instead of carrying the tree. The declaration SHAPE is unchanged — which body
/// fills which slot is structure a flat child column could not express without a re-nesting
/// record, so the ids ride the shape rather than `DeclExprChildren`. Both directions are
/// `TastConvert.typeDecl` at the matching body mapping, so nothing re-derives the shape.
type PooledTypeDecl = TTypeDeclG<FrozenType, SyntaxToken, ExprPoolId>

/// A binding's SOURCE arity with its tuple-group patterns named by pool id — the file's own
/// `ValRepr`s, whose pats ARE nodes of the pooled tree (the peel reads the pooled lambda
/// spine, so a group's pattern is the very node the spine bears, not a copy of it).
/// Distinct from `Frozen.ValRepr`, which stays at the pattern TREE because an EXTERNAL
/// symbol's pats are minted from an `.fsi` contract and index into no file's pool.
type PooledValRepr = ValReprG<FrozenType, PatPoolId>

/// The SOURCE-arity grouping rule and the curried peel that applies it — written ONCE,
/// here rather than with the rest of the compiled-form machinery (`TastLower`), because
/// `PatShape` is the pool vocabulary and the pool build is the earliest caller.
///
/// Both the rule and the LOOP are domain-agnostic: a caller supplies a reader for its own
/// representation (the raw columns at `TastPools.toPools`, node handles at
/// `TastLower.peelValRepr`) and the arity itself is not restated. Two peels that agreed
/// only by review is exactly how a binding's recorded arity came to be able to disagree
/// with the spine it was read from.
[<RequireQualifiedAccess>]
module ArgGroups =

    /// What the grouping rule reads off ONE curried parameter pattern: its shape, its
    /// type, the binder it introduces (`NamedSimple` only), and its constant value
    /// (`Const` only). Named rather than a positional tuple because each domain's reader
    /// fills it and none should have to remember an argument order.
    [<Struct>]
    type ParamPatFacts =
        {
            Shape: PatShape
            Ty: FrozenType
            Binder: NodeKey voption
            ConstValue: TConstValue voption
        }

    /// The SOURCE grouping of one curried parameter: a simple binder is a `GSimple`; a
    /// `()` parameter is a `GUnit` (the lone-erasable `let f () = …` shape); a tuple
    /// parameter is a `GTuple` carrying the WHOLE pattern, since flattening is
    /// `TastLower.compiledOf`'s job and the source grouping must survive; anything else is
    /// not a parameter group at all and stops the peel.
    let ofParam (facts: ParamPatFacts) (pat: 'p) : ArgGroupG<FrozenType, 'p> voption =
        match facts.Shape, facts.Binder, facts.ConstValue with
        | PatShape.NamedSimple, ValueSome k, _ -> ValueSome(ArgGroupG.GSimple(k, facts.Ty))
        | PatShape.Const, _, ValueSome TConstValue.Unit -> ValueSome(ArgGroupG.GUnit facts.Ty)
        | PatShape.Tuple, _, _ -> ValueSome(ArgGroupG.GTuple pat)
        | _ -> ValueNone

    /// Peel a curried lambda chain into its source groups and the residual body. The
    /// caller supplies only how to READ its representation — `unLambda` opens one lambda
    /// into its `(param, body)` and declines on anything else, `facts` reads a parameter
    /// pattern — so the walk, the grouping and the stopping condition exist once for every
    /// domain that has a spine.
    let rec peel
        (unLambda: 'e -> struct ('p * 'e) voption)
        (facts: 'p -> ParamPatFacts)
        (e: 'e)
        : ArgGroupG<FrozenType, 'p> list * 'e =
        match unLambda e with
        | ValueSome(struct (param, body)) ->
            match ofParam (facts param) param with
            | ValueSome g ->
                let gs, residual = peel unLambda facts body
                g :: gs, residual
            | ValueNone -> [], e
        | ValueNone -> [], e

/// One entry of the pooled inline VOCABULARY: a published template's identity and parameter
/// attributes, with its declaration named by pool id.
///
/// A template is a SEPARATE ROOT from the emitted function of the same name, and the two
/// trees are deliberately NOT shared: `Freeze` publishes the UNWALKED snapshot, because a
/// template's static-opt clauses and trait calls must resolve against a CALL SITE's operand
/// types rather than against the nothing that is ground at its definition. Pooling preserves
/// that split by giving the templates their own roots.
type PooledInlineValue =
    {
        Key: SymbolKey
        Decl: DeclPoolId
        ParamAttrs: ParamAttrs[]
    }

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
/// and the `Var` binder id (`ExprVarBinder`). Mirrors `FrozenCodec.writeExprPayload` for what each
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
/// columns) and the child sub-pat ids (`PatChildren`, in `TastPools.patChildren` order;
/// patterns own no child expressions). Mirrors `FrozenCodec.writePatPayload` for what each case
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

[<RequireQualifiedAccess>]
module PatPayload =

    /// Map every `FrozenType` a pattern payload EMBEDS. Only `TypeTestAs` carries one (the
    /// `isinst` operand); a node's own type is the `PatTys` column and is mapped there, so
    /// this is the residue a column-level retype (`TastPoolBuilder.copyPatTreeInto`) would
    /// otherwise miss. Exhaustive with no catch-all, so a case that grows a type field
    /// fails to compile here.
    let mapTys (f: FrozenType -> FrozenType) (p: PatPayload) : PatPayload =
        match p with
        | PatPayload.TypeTestAs testTy -> PatPayload.TypeTestAs(f testTy)
        | PatPayload.NamedSimple _
        | PatPayload.Wildcard
        | PatPayload.Null
        | PatPayload.Tuple
        | PatPayload.Or
        | PatPayload.Const _
        | PatPayload.Record _
        | PatPayload.Union _
        | PatPayload.EnumCase _ -> p

/// The three naming projections a backend reads off a binder to emit its name WITHOUT
/// the whole `NodeKey` — exactly the bits `binderName` (`JsEmitHelpers.fs`) unpacks: a
/// real binder recovers its source name by slicing at `Offset`; a synthetic renders as
/// `_s<NameIndex>`. It is the naming DATA, separated from the key so a consumer holding
/// only a `BinderId` can name what it names (`TastAccessor.exprVarNaming`).
///
/// NOT a stored column. It is a total function of the binder's retained `NodeKey`
/// (`ofKey`, its sole constructor), so storing it beside `FrozenPools.BinderKeys` would
/// put the same three bits on the wire twice; `TastPoolBuilder.binderNaming` projects it
/// on read instead. When the key itself retires this becomes the column that replaces
/// it — one representation at a time, never both.
[<Struct>]
type BinderNaming =
    {
        IsSynthetic: bool
        Offset: int
        NameIndex: int
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BinderNaming =

    /// The naming triple IS the key's own projections — the same three bits `binderName`
    /// reads — so a pooled binder names identically to its `NodeKey` by construction.
    /// Sole constructor, so no site can drift from `binderName`.
    let ofKey (k: NodeKey) : BinderNaming =
        {
            IsSynthetic = k.IsSynthetic
            Offset = k.Offset
            NameIndex = k.NameIndex
        }

/// The residual payload of a frozen declaration node — one case per `DeclShape`. A decl
/// carries no uniform node-level `ty`/`tok` (there are no `DeclTys`/`DeclToks` columns), so
/// each case rides whatever type/scalars it needs. Its child expr/pat roots live in the
/// `DeclExprChildren`/`DeclPatChildren` columns. Mirrors `FrozenCodec.writeDeclPayload`. Exhaustive:
/// a new `TDecl`/`DeclShape` case fails to compile at `declPayload`/`substituteDecl`.
[<RequireQualifiedAccess>]
type DeclPayload =
    /// The binding is the sole pat child, its value the sole expr child; `IsInline`/`Ty`
    /// (the binding's declared slot type) are the residual scalars.
    | Let of {| IsInline: bool; Ty: FrozenType |}
    /// The decl's declared type; the body is the sole expr child.
    | Expression of FrozenType
    /// The `type` declaration's spine, its seven body slots holding pool ids rather than
    /// expression trees (`PooledTypeDecl`). A `Type` decl still surfaces no
    /// `DeclExprChildren` — its bodies are named by id INSIDE the declaration shape, which
    /// is what keeps "which body fills which slot" expressed by the shape itself.
    | Type of PooledTypeDecl

/// Everything of a `Frozen.TastFile` that has NO pooled form — the file MINUS its trees (the
/// columns) and MINUS the seven side tables (the dense `BinderId`/`ExprPoolId` associations).
/// NO field here carries a tree, which is the property that matters: every expression and
/// pattern in the file is in the columns, so the residue can never drag a subtree along.
/// Exactly these three fields, each for its own reason:
///
///   * `Diagnostics` — a flat list keyed by `NodeKey`, in no pooled domain (a diagnostic can
///     name a node the emittable tree does not contain, so it cannot take a pool id).
///   * `IntrinsicReprKeys` / `Accessibility` — the two `SymbolKey`-keyed dictionaries. Their
///     key space is the SYMBOL identity, not the positional node identity the pools give, so
///     they are untouched by the dense-id remap.
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
        Accessibility: System.Collections.Generic.IReadOnlyDictionary<SymbolKey, Accessibility>
    }

/// THE frozen file: the expr struct-of-arrays columns plus the pat/decl columns (indexable
/// by the matching `*PoolId`) and the decl roots, in source order. This is what `Freeze.run`
/// yields and every consumer reads; the `Frozen.TastFile` DU it is built from
/// (`TastPools.toPools`) does not outlive the freeze. Kept OFF `TastFileG` — that record is
/// shared with the `SemType` instantiation, which has no pools.
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
        /// `TastPools.exprChildren` order; `ExprPatChildren` the owned pat ids in
        /// `TastPools.exprPatChildren` order; `ExprVarBinder` the `Var` reference id
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
        /// `PatChildren` the immediate sub-pat ids in `TastPools.patChildren` order
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
        /// The inline vocabulary's roots: one per published template, in publication order.
        /// A SECOND root array rather than entries of `Roots`, because a template is not an
        /// emittable decl and must not be walked as one — and it is a genuinely distinct
        /// tree from the emitted function of the same name (see `PooledInlineValue`).
        InlineTemplates: PooledInlineValue[]
        /// The distinct binder entries as ONE dense column indexed by `BinderId`, its own
        /// array disjoint from `Pats`: each binder's whole original `NodeKey` — the identity
        /// `Var.binding` and the side tables resolve against, and the DU round-trip's carrier
        /// for the `Raw` bits (kind included) the trees still reconstruct from, so the key
        /// cannot be dropped while the backing is DU-form. The naming a backend emits is a
        /// projection OF this column, not a second one beside it (`BinderNaming`). A
        /// `NamedSimple` pattern still also appears in the pat columns for the tree walk; this
        /// is the additional dense column references resolve against, not a re-pointing.
        BinderKeys: NodeKey[]
        /// The not-yet-pooled remainder of the source file, carried verbatim.
        Residue: FrozenFileResidue
        /// Six of the seven source `Map<NodeKey,_>` side tables, re-keyed by `BinderId`.
        /// `ofPools` rebuilds each map from its dense form; a reader indexes it with
        /// `DenseTable.index`.
        ModuleMembers: DenseTable<BinderId, ModuleBindingInfo>
        TopLevelNames: DenseTable<BinderId, string>
        ClosureReprs: DenseTable<BinderId, ClosureRepr>
        /// The one side table keyed by a lambda-EXPRESSION `NodeKey` (a source lambda's
        /// `TastWalk.lambdaKey`, kind `ExprLambda`) rather than a binder, so it is re-keyed
        /// onto the lambda id space — a lambda's dense id IS its `ExprPoolId` (positional:
        /// every `Lambda` expr is already pooled), off the binder pool. `ofPools` inverts
        /// by recomputing that key from the lambda's `ExprToks` column (the same
        /// `NodeKey.ofToken … ExprLambda` `TastWalk.lambdaKey` computes), the Node now gone.
        FunVerdicts: DenseTable<ExprPoolId, FunVerdict>
        GenericFnSchemes: DenseTable<BinderId, FrozenConstraint list>
        BindingValReprs: DenseTable<BinderId, PooledValRepr>
        BindingTyparArities: DenseTable<BinderId, int>
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FrozenPools =

    /// The zero column set — a pool that is nobody's file. Lives with the type because it
    /// is a property OF the type, not of any one consumer:
    /// `TastPoolBuilder.openEmpty` stacks an overlay on it for nodes that belong to no
    /// frozen tree at all (an EXTERNAL symbol's `.fsi`-minted `ValRepr` patterns, a
    /// provider's re-axised copies), and they are read through the same accessor as any
    /// other node.
    let empty: FrozenPools =
        {
            ExprShapes = [||]
            ExprTys = [||]
            ExprToks = [||]
            ExprChildren = [||]
            ExprPatChildren = [||]
            ExprVarBinder = [||]
            ExprPayloads = [||]
            PatShapes = [||]
            PatTys = [||]
            PatToks = [||]
            PatChildren = [||]
            PatPayloads = [||]
            DeclShapes = [||]
            DeclExprChildren = [||]
            DeclPatChildren = [||]
            DeclPayloads = [||]
            Roots = [||]
            InlineTemplates = [||]
            BinderKeys = [||]
            Residue =
                {
                    Diagnostics = []
                    IntrinsicReprKeys = readOnlyDict []
                    Accessibility = readOnlyDict []
                }
            ModuleMembers = [||]
            TopLevelNames = [||]
            ClosureReprs = [||]
            FunVerdicts = [||]
            GenericFnSchemes = [||]
            BindingValReprs = [||]
            BindingTyparArities = [||]
        }

// ── the ROW view: one node's slice across the parallel columns ──────────────
//
// A `*Row` is the TRANSPOSE of the columns at one id — every column value of a single
// node, gathered. It is what a node-at-a-time producer or rewriter speaks: the pooling
// walk hands the sink a whole row rather than a widening argument list, and a REWRITE is
// `{ row with Children = … }` / `{ row with Ty = … }` — a row copy with no per-case match
// on the node's shape, which is what makes a columnar rewrite cheaper than rebuilding a
// DU node. The columns stay the storage form; rows never accumulate anywhere the layout
// matters.

/// One expression node's slice across the `Expr*` columns, in column order.
type ExprRow =
    {
        Shape: ExprShape
        Ty: FrozenType
        Tok: SyntaxToken
        Children: ExprPoolId[]
        PatChildren: PatPoolId[]
        /// The `Var` reference edge (`ValueNone` at every other shape). A tree WALK cannot
        /// fill this — assigning binder ids is the pooling sink's business, and a `Var` may
        /// name a binder the walk has not reached yet — so a walk-produced row carries
        /// `ValueNone` here and the sink supplies the id once it can resolve one.
        VarBinder: BinderId voption
        Payload: ExprPayload
    }

/// One pattern node's slice across the `Pat*` columns, in column order.
type PatRow =
    {
        Shape: PatShape
        Ty: FrozenType
        Tok: SyntaxToken
        Children: PatPoolId[]
        Payload: PatPayload
    }

/// One declaration node's slice across the `Decl*` columns, in column order (a decl has no
/// node-level `ty`/`tok` — its type rides the payload).
type DeclRow =
    {
        Shape: DeclShape
        ExprChildren: ExprPoolId[]
        PatChildren: PatPoolId[]
        Payload: DeclPayload
    }
