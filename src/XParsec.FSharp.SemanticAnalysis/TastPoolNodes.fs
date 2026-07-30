namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

// What ONE frozen node is, once the tree dissolves: its dense id, its residual payload,
// and the row that is its slice across the columns. The FILE those columns belong to —
// `FrozenPools` and the side-table containers — is `TastPoolTypes.fs`, which reads this
// file and not the reverse; the logic that fills the columns is `TastPools.fs` and the
// logic that drains them `TastUnpool.fs`.
//
// Each frozen node is assigned a dense `int` pool id during a traversal of the DU; a
// node's child *expressions*/*patterns* are then addressable as their pool ids, so "node
// id k and its children by id" is an O(1) fetch — the random-access shape the projecting
// consumers want, which a forward-only decode stream could not serve.
//
// A `*Payload` is a node's fields MINUS everything the columnar split took: the `ty`/`tok`,
// the child expr/pat ids, and (for exprs) the `Var` binder id. So NO pool holds a DU node:
// the whole `TExprG`/`TPat`/`TDecl` subtree dissolves into columns, which is the point — a
// retained node drags its entire nested body along with it, so freeze could never stop
// materializing the DU.
//
// There is no separate SHAPE column. A node's `ExprShape`/`PatShape`/`DeclShape` — the tag
// a consumer's total dispatch matches on — is a total function of its payload
// (`ExprPayload.shape` and friends, the only constructors of one), so storing it beside the
// payload would be the same fact twice, in two places that could contradict each other.
//
// A payload case that is a COMPOSITE carrier (`Match`/`TryWith` arms, `Format`
// segments, `StaticOptimization` clauses, `Range` step, `RecordCons`/`RecordClone`
// fields, `ExternalMember` receiver) records just enough STRUCTURE — per-arm guard
// flags, per-segment kind, per-clause constraints, presence flags — to redistribute the
// FLAT child columns back into their nested shape, since the node that once held that
// shape is gone. Both directions reuse the pooling walk's child enumeration
// (`TastPoolShapes.exprChildren`/`exprPatChildren`) and consume it in that same order —
// the coupling the round-trip test guards — rather than re-deriving the tree spine. The
// `ExprPayloads` build (`exprPayload`) and consume (`substituteExpr`) are inverse
// per-case matches, each exhaustive so a new `TExprG`/`ExprShape` case fails to compile.
//
// Two carriers that used to hold trees opaquely are pooled like any other node, and their
// pooled shapes live here: a `Type` decl's member bodies (`PooledTypeDecl` — ids in the
// declaration shape) and a binding's `ValRepr` tuple-group patterns (`PooledValRepr`,
// which holds no tree of its own at all — it names the LAMBDA SPINE's own pattern nodes,
// being derived from that spine rather than carried alongside it).
//
// Identity goes positional too: a binder's identity after freeze IS its slot in the file's
// binder pool, addressed by `BinderId`. Every distinct definition site the walk reaches
// is interned to one — the pattern/loop binders (`BinderKey.ofPat`,
// `BinderKey.ofExpr`) and the bare key slots a type declaration binds with no pattern node
// behind them (`BinderKey.ofTypeDecl`: a member's `this`/`base` and parameters, a ctor's
// parameters and locals), which its member BODIES name by `Var` exactly as a function body
// names a `let`. (Kind is not stored — its only role was to make a content key unique,
// which positional ids now do.)

/// The post-freeze shape tag of an expression node — one case per `TExprG` case, and
/// the vocabulary a consumer's TOTAL dispatch matches on (`TastAccessor.exprKind`).
/// This is NOT `NodeKind`: `NodeKind` is the pre-freeze CST content-address role, which
/// freeze dissolves. The case names mirror `TExprG` (documented there).
///
/// NOT a stored column. A node's shape is a total function of its `ExprPayload`
/// (`ExprPayload.shape`, the sole way to obtain one from a node), which is why there is
/// no way for the two to disagree and nothing puts the tag on the wire twice.
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
    | InlineCall

/// The post-freeze shape tag of a pattern node — one case per `TPatG` case (mirrors
/// `ExprShape`'s relationship to `TExprG`, `PatPayload.shape` included).
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

/// A dense pool index into the parallel `FrozenPools.Expr*` columns.
[<Struct>]
type ExprPoolId = | ExprPoolId of int

/// A dense pool index into the parallel `FrozenPools.Pat*` columns.
[<Struct>]
type PatPoolId = | PatPoolId of int

/// A dense pool index into the parallel `FrozenPools.Decl*` columns.
[<Struct>]
type DeclPoolId = | DeclPoolId of int

/// The TAST at the POOLED identity: a tree whose binders are named by the dense
/// `BinderId` the columns already address them by, rather than by the `NodeKey` a
/// source-shaped tree names them by. `TastUnpool` rebuilds at these aliases, which is what
/// lets the drain be a genuine interconversion with the columns instead of a view that has
/// to consult a retained key to speak at all.
///
/// The `'ty` axis is the frozen one and `'tok` is the stored `Anchor`, which is what the
/// columns hold — so a drained tree names its positions exactly as the columns do and needs
/// no `Lexed` to be rebuilt.
module Pooled =
    type TPat = TPatG<FrozenType, Anchor, BinderId>
    type TExpr = TExprG<FrozenType, Anchor, BinderId>
    type TMatchArm = TMatchArmG<TPat, TExpr>
    type HoleSpec = HoleSpecG<FrozenType, Anchor>
    type TDecl = TDeclG<FrozenType, Anchor, BinderId>
    type TTypeDecl = TTypeDeclG<FrozenType, Anchor, BinderId, TExpr>
    type TTypeMember = TTypeMemberG<FrozenType, BinderId, TExpr>
    type TInlineValue = TInlineValueG<FrozenType, Anchor, BinderId>
    type TastFile = TastFileG<FrozenType, Anchor, BinderId>

/// The TAST as it CROSSES A UNIT BOUNDARY — a package's inline template, drained from the
/// producer's pools (`TastPoolBuilder.declTree`) for a consumer that shares neither of the
/// producer's identity spaces.
///
/// Not the producer's `BinderId`s: a slot means nothing outside the pool that issued it, so
/// the drain re-mints a `NodeKey` per binder.
///
/// The positions ARE the producer's, and arrive intact — the drain widens `Anchor` to
/// `ForeignAnchor` and changes nothing else. That marking is the whole of what distinguishes
/// this from `Pooled` on the position axis, and it is not decoration: the two hold the same
/// integers and only the DOMAIN differs, so nothing but the type stops a producer's index from
/// being read against the consumer's `Lexed`, where it lands on an unrelated token instead of
/// faulting. Reading one requires an `OriginFile` (`OriginSources.tokenAt`), which is why a
/// consumer either names the producer file the body came from or relocates the body onto a
/// position of its own (`Inline.thawBody`).
module Wire =
    type TPat = TPatG<FrozenType, ForeignAnchor, NodeKey>
    type TExpr = TExprG<FrozenType, ForeignAnchor, NodeKey>
    type TDecl = TDeclG<FrozenType, ForeignAnchor, NodeKey>
    type TInlineBody = TInlineBodyG<FrozenType, ForeignAnchor, NodeKey>
    type TInlineValue = TInlineValueG<FrozenType, ForeignAnchor, NodeKey>

/// A `type` declaration whose seven member/preamble/ctor BODY slots name their expression
/// by pool id instead of carrying the tree, and whose seven pattern-less BINDER slots name
/// their definition site by `BinderId` — the same dense identity every other pooled
/// reference uses, so a declaration's `this` / parameters / ctor locals are addressed
/// exactly as a `NamedSimple` pattern's binder is. The declaration SHAPE is unchanged —
/// which body fills which slot is structure a flat child column could not express without
/// a re-nesting record, so the ids ride the shape rather than `DeclExprChildren`. Both
/// directions are `TastConvert.typeDecl` at the matching body/identity mappings, so nothing
/// re-derives the shape.
type PooledTypeDecl = TTypeDeclG<FrozenType, Anchor, BinderId, ExprPoolId>

/// A binding's SOURCE arity with its tuple-group patterns named by pool id — the file's own
/// `ValRepr`s, whose pats ARE nodes of the pooled tree (the peel reads the pooled lambda
/// spine, so a group's pattern is the very node the spine bears, not a copy of it).
/// Distinct from `Frozen.ValRepr`, which stays at the pattern TREE because an EXTERNAL
/// symbol's pats are minted from an `.fsi` contract and index into no file's pool.
type PooledValRepr = ValReprG<FrozenType, PatPoolId, BinderId>

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
    type ParamPatFacts<'id> =
        {
            Shape: PatShape
            Ty: FrozenType
            Binder: 'id voption
            ConstValue: TConstValue voption
        }

    /// The SOURCE grouping of one curried parameter: a simple binder is a `GSimple`; a
    /// `()` parameter is a `GUnit` (the lone-erasable `let f () = …` shape); a tuple
    /// parameter is a `GTuple` carrying the WHOLE pattern, since flattening is
    /// `TastLower.compiledOf`'s job and the source grouping must survive; anything else is
    /// not a parameter group at all and stops the peel.
    let ofParam (facts: ParamPatFacts<'id>) (pat: 'p) : ArgGroupG<FrozenType, 'p, 'id> voption =
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
        (facts: 'p -> ParamPatFacts<'id>)
        (e: 'e)
        : ArgGroupG<FrozenType, 'p, 'id> list * 'e =
        match unLambda e with
        | ValueSome(struct (param, body)) ->
            match ofParam (facts param) param with
            | ValueSome g ->
                let gs, residual = peel unLambda facts body
                g :: gs, residual
            | ValueNone -> [], e
        | ValueNone -> [], e

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
    | Hole of Pooled.HoleSpec
    | DynHole of hasWidth: bool * hasPrecision: bool * spec: Pooled.HoleSpec
    | CallbackHole of Pooled.HoleSpec

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
    /// The loop binder + its `identTok`. `Var` is the binder this node INTRODUCES (not a
    /// reference, so it is not on the `ExprVarBinder` column), named by the same dense id
    /// the loop body's `Var` references resolve to — one identity, interned once.
    | ForTo of {| Var: BinderId; IdentTok: Anchor |}
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
    /// The specialization-table slot this call names; the args are the child expressions.
    /// The ENTRY is a root of its own (`FrozenPools.Specializations`) and is deliberately
    /// not a child edge — several call sites share one entry, so making it a child would
    /// turn the DAG into a tree by duplication.
    | InlineCall of spec: SpecializationId

[<RequireQualifiedAccess>]
module ExprPayload =

    /// The shape tag of a node carrying this payload — the SOLE way to obtain an
    /// `ExprShape` for a node, so a node's tag and its payload cannot disagree. There is
    /// one payload case per shape, which is why this is total and injective; the tag is
    /// therefore neither a column nor a wire field, only a projection taken on read.
    /// Exhaustive with no catch-all, so a new case fails to compile here.
    let shape (p: ExprPayload) : ExprShape =
        match p with
        | ExprPayload.Const _ -> ExprShape.Const
        | ExprPayload.Var -> ExprShape.Var
        | ExprPayload.External _ -> ExprShape.External
        | ExprPayload.Lambda -> ExprShape.Lambda
        | ExprPayload.App -> ExprShape.App
        | ExprPayload.Let -> ExprShape.Let
        | ExprPayload.Use _ -> ExprShape.Use
        | ExprPayload.IfThenElse -> ExprShape.IfThenElse
        | ExprPayload.Tuple -> ExprShape.Tuple
        | ExprPayload.Sequential -> ExprShape.Sequential
        | ExprPayload.While -> ExprShape.While
        | ExprPayload.ForTo _ -> ExprShape.ForTo
        | ExprPayload.ForIn _ -> ExprShape.ForIn
        | ExprPayload.Match _ -> ExprShape.Match
        | ExprPayload.TryWith _ -> ExprShape.TryWith
        | ExprPayload.TryFinally -> ExprShape.TryFinally
        | ExprPayload.Assignment -> ExprShape.Assignment
        | ExprPayload.Null -> ExprShape.Null
        | ExprPayload.Range _ -> ExprShape.Range
        | ExprPayload.RecordCons _ -> ExprShape.RecordCons
        | ExprPayload.RecordClone _ -> ExprShape.RecordClone
        | ExprPayload.FieldGet _ -> ExprShape.FieldGet
        | ExprPayload.FieldSet _ -> ExprShape.FieldSet
        | ExprPayload.UnionCons _ -> ExprShape.UnionCons
        | ExprPayload.New _ -> ExprShape.New
        | ExprPayload.MethodCall _ -> ExprShape.MethodCall
        | ExprPayload.PropertyGet _ -> ExprShape.PropertyGet
        | ExprPayload.StaticMethodCall _ -> ExprShape.StaticMethodCall
        | ExprPayload.StaticPropertyGet _ -> ExprShape.StaticPropertyGet
        | ExprPayload.StaticFieldGet _ -> ExprShape.StaticFieldGet
        | ExprPayload.StaticFieldSet _ -> ExprShape.StaticFieldSet
        | ExprPayload.ExternalMember _ -> ExprShape.ExternalMember
        | ExprPayload.Format _ -> ExprShape.Format
        | ExprPayload.ILIntrinsic _ -> ExprShape.ILIntrinsic
        | ExprPayload.StaticOptimization _ -> ExprShape.StaticOptimization
        | ExprPayload.Upcast -> ExprShape.Upcast
        | ExprPayload.Downcast -> ExprShape.Downcast
        | ExprPayload.TypeTest _ -> ExprShape.TypeTest
        | ExprPayload.TraitCall _ -> ExprShape.TraitCall
        | ExprPayload.InlineCall _ -> ExprShape.InlineCall

    // ── re-nesting the flat child columns ───────────────────────────────────
    //
    // A composite carrier — an arm, a format sink, a format segment — has no node
    // identity of its own, so the pool flattens its pieces into the child columns and the
    // payload keeps only the STRUCTURE needed to put them back. That structure and the
    // cursor order it implies are the same whichever domain is being rebuilt, so the walk
    // lives here, ONCE, generic over what a child is: `TastUnpool` drives it with DU
    // subtrees, `TastAccessor` with node handles, and the two cannot re-nest the same
    // columns differently.

    /// A reader that draws a node's children in column order, one per call — what the
    /// re-nesting walks below consume. `start` skips the node's own LEADING children, which
    /// belong to it rather than to a carrier (`Match`'s scrutinee, `TryWith`'s body).
    ///
    /// Here rather than at each caller: the walks take a cursor because the ORDER is the
    /// coupling they exist to hold, and three domains hand-rolling the same mutable index
    /// is three places for that order to be started from the wrong offset.
    let cursor (xs: 'a[]) (start: int) : unit -> 'a =
        let mutable i = start

        fun () ->
            let x = xs.[i]
            i <- i + 1
            x

    /// Re-nest the arm children of a `Match`/`TryWith`: each arm draws its pat, then its
    /// guard when `guardPresent` says it has one, then its body — the order
    /// `TastPoolShapes.exprChildren`/`exprPatChildren` enumerated them in. Arm count is the
    /// flag array's length.
    let arms (guardPresent: bool[]) (nextPat: unit -> 'pat) (nextExpr: unit -> 'e) : TMatchArmG<'pat, 'e>[] =
        guardPresent
        |> Array.map (fun hasGuard ->
            let pat = nextPat ()
            let guard = if hasGuard then ValueSome(nextExpr ()) else ValueNone

            {
                Pat = pat
                Guard = guard
                Body = nextExpr ()
            }
        )

    /// Re-nest the children of a `Format`: the sink's own sub-expression first (the order
    /// `TastPoolShapes.exprChildren` yields), then each segment's, with the dyn-hole presence
    /// flags saying which dimensions are there.
    ///
    /// A hole is the one leaf that carries an anchor of its own, so `widenTok` is how the
    /// stored index becomes whatever the rebuilding domain names positions by — `id` for a
    /// tree that stays in the pool's own space, the drain's widening for one that leaves it.
    let format
        (widenTok: Anchor -> 'tok)
        (sink: FormatSinkShape)
        (segments: FormatSegShape[])
        (nextExpr: unit -> 'e)
        : FormatSinkG<'e> * FormatSegG<FrozenType, 'tok, 'e>[] =
        let spec = TastConvert.hole id widenTok

        let sink' =
            match sink with
            | FormatSinkShape.ToWriter newline -> FormatSinkG.ToWriter(nextExpr (), newline)
            | FormatSinkShape.ToBuilder -> FormatSinkG.ToBuilder(nextExpr ())
            | FormatSinkShape.ToStdOut newline -> FormatSinkG.ToStdOut newline
            | FormatSinkShape.ToStdErr newline -> FormatSinkG.ToStdErr newline
            | FormatSinkShape.ToString -> FormatSinkG.ToString

        let segments' =
            segments
            |> Array.map (fun seg ->
                match seg with
                | FormatSegShape.Lit s -> FormatSegG.Lit s
                | FormatSegShape.Hole h -> FormatSegG.Hole(spec h, nextExpr ())
                | FormatSegShape.DynHole(hasWidth, hasPrecision, h) ->
                    let width = if hasWidth then ValueSome(nextExpr ()) else ValueNone
                    let precision = if hasPrecision then ValueSome(nextExpr ()) else ValueNone

                    FormatSegG.DynHole
                        {
                            Width = width
                            Precision = precision
                            Spec = spec h
                            Value = nextExpr ()
                        }
                | FormatSegShape.CallbackHole h -> FormatSegG.CallbackHole(spec h, nextExpr ())
            )

        sink', segments'

/// The residual payload of a frozen pattern node — one case per `PatShape`, carrying ONLY
/// the fields left after the columnar split drops `ty`/`tok` (the `PatTys`/`PatToks`
/// columns) and the child sub-pat ids (`PatChildren`, in `TastPoolShapes.patChildren` order;
/// patterns own no child expressions). Mirrors `FrozenCodec.writePatPayload` for what each case
/// carries beyond those. Exhaustive: a new `TPat`/`PatShape` case fails to compile at
/// `patPayload`/`substitutePat`.
[<RequireQualifiedAccess>]
type PatPayload =
    /// The single binder this simple name pattern INTRODUCES, named by the dense id its
    /// `Var` references resolve to — the pat analogue of `ExprPayload.ForTo`'s `Var`. It
    /// names no sub-pattern, so it is not a child.
    | NamedSimple of binding: BinderId
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

    /// The shape tag of a pattern carrying this payload — see `ExprPayload.shape`.
    let shape (p: PatPayload) : PatShape =
        match p with
        | PatPayload.NamedSimple _ -> PatShape.NamedSimple
        | PatPayload.Wildcard -> PatShape.Wildcard
        | PatPayload.Null -> PatShape.Null
        | PatPayload.Tuple -> PatShape.Tuple
        | PatPayload.Or -> PatShape.Or
        | PatPayload.Const _ -> PatShape.Const
        | PatPayload.Record _ -> PatShape.Record
        | PatPayload.Union _ -> PatShape.Union
        | PatPayload.TypeTestAs _ -> PatShape.TypeTestAs
        | PatPayload.EnumCase _ -> PatShape.EnumCase

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

/// How a backend SPELLS a binder — the naming column read at one slot, and the only thing
/// a backend needs to name what a `BinderId` names (`TastAccessor.exprVarNaming`).
///
/// Two cases because there are two kinds of definition site, and the difference is not a
/// fallback: a binder the SOURCE spells carries that identifier verbatim (dialect mangling
/// — JS reserved words, apostrophes — belongs to the backend that emits it, not here),
/// while a binder no identifier spells (a class's `this`/`base`, a freshened inline
/// binder) has nothing to carry and is named after its SLOT, which is unique by
/// construction because the slot is the identity.
[<RequireQualifiedAccess>]
[<Struct>]
type BinderNaming =
    /// The identifier the source spells this binder with.
    | Source of name: string
    /// No identifier spells this binder; a backend invents one from the slot.
    | Minted of slot: BinderId

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BinderNaming =

    /// The naming column read at `slot`. The EMPTY name is what "no identifier spells this
    /// binder" is stored as — it is no legal identifier, so the two cases cannot be
    /// confused — and this is the sole place that convention is decoded, so no consumer
    /// can invent a second reading of an empty slot.
    let ofColumn (name: string) (slot: BinderId) : BinderNaming =
        match name.Length with
        | 0 -> BinderNaming.Minted slot
        | _ -> BinderNaming.Source name

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

[<RequireQualifiedAccess>]
module DeclPayload =

    /// The shape tag of a declaration carrying this payload — see `ExprPayload.shape`.
    let shape (p: DeclPayload) : DeclShape =
        match p with
        | DeclPayload.Let _ -> DeclShape.Let
        | DeclPayload.Expression _ -> DeclShape.Expression
        | DeclPayload.Type _ -> DeclShape.Type

// ── the ROW view: one node's slice across the parallel columns ──────────────
//
// A `*Row` is the TRANSPOSE of the columns at one id — every column value of a single
// node, gathered. It is what a node-at-a-time producer or rewriter speaks: the pooling
// walk hands the sink a whole row rather than a widening argument list, and a REWRITE is
// `{ row with Children = … }` / `{ row with Ty = … }` — a row copy with no per-case match
// on the node's shape, which is what makes a columnar rewrite cheaper than rebuilding a
// DU node. The columns stay the storage form; rows never accumulate anywhere the layout
// matters.

/// One expression node's slice across the `Expr*` columns, in column order. No `Shape`:
/// the tag is `ExprPayload.shape Payload`, so a row cannot be minted with a tag that
/// contradicts what it carries.
type ExprRow =
    {
        Ty: FrozenType
        /// The node's anchor as the column stores it — an index, negative where no source
        /// spells the node (`Anchor`). A row is the column transpose, so it holds the
        /// column's own value; the surface that hands one out decodes it.
        Tok: Anchor
        Children: ExprPoolId[]
        PatChildren: PatPoolId[]
        /// The `Var` reference edge (`ValueNone` at every other shape). A tree WALK cannot
        /// fill this — assigning binder ids is the pooling sink's business, and a `Var` may
        /// name a binder the walk has not reached yet — so a walk-produced row carries
        /// `ValueNone` here and the sink supplies the id once it can resolve one.
        VarBinder: BinderId voption
        Payload: ExprPayload
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExprRow =

    /// Would appending `b` in place of `a` change anything? The unchanged test a row copy
    /// turns on (`TastPoolBuilder.copyExprWith`), and the same answer `a = b` gives.
    ///
    /// Written out per field rather than left to structural equality because of what the
    /// fields are. An edit REPLACES one field and carries the rest across, so the carried
    /// fields are the very same objects on both sides — but F#'s generated record equality
    /// calls each field's own `Equals` with no physical-identity check, so `a = b` walks the
    /// node's whole `FrozenType` and payload to re-discover that they never moved. On the
    /// per-node path of both backends' rewrite walks that is the dominant cost of a rewrite
    /// that changes nothing.
    ///
    /// The child columns are the one pair compared by VALUE: a child substitution mints a
    /// fresh array even when every id in it is unchanged, and that is exactly the case that
    /// must still answer "same".
    let same (a: ExprRow) (b: ExprRow) : bool =
        (obj.ReferenceEquals(a.Ty, b.Ty) || a.Ty = b.Ty)
        && a.Tok = b.Tok
        && a.Children = b.Children
        && a.PatChildren = b.PatChildren
        && a.VarBinder = b.VarBinder
        && (obj.ReferenceEquals(a.Payload, b.Payload) || a.Payload = b.Payload)

/// One pattern node's slice across the `Pat*` columns, in column order — see `ExprRow`
/// for why there is no `Shape`.
type PatRow =
    {
        Ty: FrozenType
        /// See `ExprRow.Tok`.
        Tok: Anchor
        Children: PatPoolId[]
        Payload: PatPayload
    }

/// One declaration node's slice across the `Decl*` columns, in column order (a decl has no
/// node-level `ty`/`tok` — its type rides the payload).
type DeclRow =
    {
        ExprChildren: ExprPoolId[]
        PatChildren: PatPoolId[]
        Payload: DeclPayload
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DeclRow =

    /// The unchanged test for a decl row copy — see `ExprRow.same` for why the fields are
    /// compared one at a time. A `Type` decl's payload is the whole declaration shape, so
    /// the physical-identity shortcut on `Payload` matters most here.
    let same (a: DeclRow) (b: DeclRow) : bool =
        a.ExprChildren = b.ExprChildren
        && a.PatChildren = b.PatChildren
        && (obj.ReferenceEquals(a.Payload, b.Payload) || a.Payload = b.Payload)
