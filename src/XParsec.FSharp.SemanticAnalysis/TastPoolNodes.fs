namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// The post-freeze shape tag of an expression node. Not a stored column: a node's shape is
/// derived from its `ExprPayload` on read.
[<RequireQualifiedAccess>]
type ExprShape =
    | Const
    | Var
    | External
    | Unresolved
    | Lambda
    | App
    | Let
    | LetGroup
    | Use
    | IfThenElse
    | Tuple
    | ArrayLit
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
    | CallerExpr

/// The post-freeze shape tag of a pattern node.
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

/// The post-freeze shape tag of a declaration node.
[<RequireQualifiedAccess>]
type DeclShape =
    | Let
    | LetGroup
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

/// The TAST at the POOLED identity: bound variables named by the dense `BoundVarId` the columns address
/// them by, positions by the stored `Anchor`. Rebuilding one from the columns needs no `Lexed`.
module Pooled =
    type TPat = TPatG<FrozenType, Anchor, BoundVarId>
    type TExpr = TExprG<FrozenType, Anchor, BoundVarId>
    type TMatchArm = TMatchArmG<TPat, TExpr>
    type HoleSpec = HoleSpecG<FrozenType, Anchor>
    type TDecl = TDeclG<FrozenType, Anchor, BoundVarId>
    type TTypeDecl = TTypeDeclG<FrozenType, Anchor, BoundVarId, TExpr>
    type TTypeMember = TTypeMemberG<FrozenType, BoundVarId, TExpr>
    type TInlineValue = TInlineValueG<FrozenType, Anchor, BoundVarId>
    type TastFile = TastFileG<FrozenType, Anchor, BoundVarId>

/// The TAST as it CROSSES A UNIT BOUNDARY: a package's inline template. BoundVars are
/// re-minted `NodeKey`s, a `BoundVarId` slot meaning nothing outside the pool that issued it.
module Wire =
    type TPat = TPatG<FrozenType, Anchor, NodeKey>
    type TExpr = TExprG<FrozenType, Anchor, NodeKey>
    type TDecl = TDeclG<FrozenType, Anchor, NodeKey>
    type TInlineBody = TInlineBodyG<FrozenType, Anchor, NodeKey>
    type TInlineValue = TInlineValueG<FrozenType, Anchor, NodeKey>

/// A `type` declaration whose member/preamble/ctor BODY slots identify their expression by
/// pool id instead of carrying the tree, and whose pattern-less BOUND-VARIABLE slots (a member's
/// `this`, parameters, ctor locals) identify their definition site by `BoundVarId`.
type PooledTypeDecl = TTypeDeclG<FrozenType, Anchor, BoundVarId, ExprPoolId>

/// A binding's SOURCE arity with its tuple-group patterns named by pool id: a group's
/// pattern is the very node the pooled lambda chain bears, not a copy.
type PooledValRepr = ValReprG<FrozenType, PatPoolId, BoundVarId>

/// The SOURCE-arity grouping rule and the curried peel that applies it, generic over the
/// representation peeled: a caller supplies a reader for its own (raw pool columns, node
/// handles) rather than restating the rule.
[<RequireQualifiedAccess>]
module ArgGroups =

    /// What the grouping rule reads off ONE curried parameter pattern. `BoundVar` is filled
    /// at `NamedSimple` only, `ConstValue` at `Const` only.
    [<Struct>]
    type ParamPatFacts<'id> =
        {
            Shape: PatShape
            Ty: FrozenType
            BoundVar: 'id voption
            ConstValue: TConstValue voption
        }

    /// The SOURCE grouping of one curried parameter. `GTuple` carries the WHOLE pattern:
    /// flattening it belongs to the compiled form and the source grouping must survive.
    /// Anything else is not a parameter group and stops the peel.
    let ofParam (facts: ParamPatFacts<'id>) (pat: 'p) : ArgGroupG<FrozenType, 'p, 'id> voption =
        match facts.Shape, facts.BoundVar, facts.ConstValue with
        | PatShape.NamedSimple, ValueSome k, _ -> ValueSome(ArgGroupG.GSimple(k, facts.Ty))
        | PatShape.Const, _, ValueSome TConstValue.Unit -> ValueSome(ArgGroupG.GUnit facts.Ty)
        | PatShape.Tuple, _, _ -> ValueSome(ArgGroupG.GTuple pat)
        | _ -> ValueNone

    /// Peel a curried lambda chain into its source groups and the residual body.
    /// `unLambda` opens one lambda into its `(param, body)` and declines on anything else.
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

/// The residual, EXPRESSION-FREE shape of a `Format` node's sink. Its own sub-expression
/// (`ToWriter`'s writer, `ToBuilder`'s builder) is stored in the child column ahead of the
/// segment children.
[<RequireQualifiedAccess>]
type FormatSinkShape =
    | ToStdOut of newline: bool
    | ToStdErr of newline: bool
    | ToWriter of newline: bool
    | ToBuilder
    | ToString

/// The residual, EXPRESSION-FREE shape of one `Format` segment. A segment's sub-expressions
/// are stored in the child column in walk order; the presence flags re-nest them.
[<RequireQualifiedAccess>]
type FormatSegShape =
    | Lit of string
    | Hole of Pooled.HoleSpec
    | DynHole of hasWidth: bool * hasPrecision: bool * spec: Pooled.HoleSpec
    | CallbackHole of Pooled.HoleSpec

/// Whether a `let` binding's value references the variable it binds.
[<RequireQualifiedAccess>]
type Recursion =
    | NonRecursive
    | Recursive
    /// The value is a function containing a `TailSelfCall`. A backend may lower the function
    /// to a loop.
    | TailRecursive

/// One `LetGroup` member's residual scalars. Its pattern and value are in the child columns.
type LetMemberShape =
    {
        /// The binding's declared type.
        Ty: FrozenType
        /// The binding pattern's first token.
        Tok: Anchor
        Recursion: Recursion
    }

/// A `LetGroup`'s residual payload. Member `i`'s pattern is pat child `i` and its value expr
/// child `i`; an expression group's body is the last expr child.
type LetGroupShape =
    {
        Members: LetMemberShape[]
        Components: SccPartition
    }

[<RequireQualifiedAccess>]
type AppKind =
    | Call
    /// The outermost application of a saturated tail self-call: the chain applies the
    /// enclosing `let rec` variable to one argument per lambda the value declares, in tail
    /// position of the innermost lambda's body.
    | TailSelfCall

/// The residual payload of a frozen expression node, one case per `ExprShape`: the fields
/// the columnar split left (not `ty`/`tok`, the child expr/pat ids, or a `Var`'s bound
/// variable id) plus the `Recursion` / `AppKind` the pooling walk classifies. A composite
/// carrier also records the STRUCTURE that re-nests those columns.
[<RequireQualifiedAccess>]
type ExprPayload =
    | Const of TConstValue
    | Var
    | External of key: BindingKey
    | Unresolved
    | Lambda
    | App of AppKind
    /// `isRec` is the source `rec` keyword, distinct from the analysed `recursion`.
    | Let of isRec: bool * recursion: Recursion
    | LetGroup of LetGroupShape
    | Use of Disposal
    | IfThenElse
    | Tuple
    | ArrayLit
    | Sequential
    | While
    /// `Var` is the bound variable this node INTRODUCES, named by the dense id the body's `Var`s
    /// resolve to. It is not a reference, so it is not on the bound-variable-reference column.
    | ForTo of {| Var: BoundVarId; IdentTok: Anchor |}
    | ForIn of Frozen.ForInEnumerator
    /// One flag per arm: whether the arm carries a guard. The scrutinee is the first
    /// child; each arm's guard (when present) and body follow in the expr child column,
    /// its pat in the pat child column. Arm count is the array length.
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
    | StaticMethodCall of
        {|
            Key: SymbolKey
            DeclArgs: EqArray<FrozenType>
        |}
    | StaticPropertyGet of
        {|
            Key: SymbolKey
            DeclArgs: EqArray<FrozenType>
        |}
    | StaticFieldGet of
        {|
            DeclKey: TypeKey
            FieldName: string
        |}
    | StaticFieldSet of
        {|
            DeclKey: TypeKey
            FieldName: string
        |}
    | ExternalMember of
        {|
            HasObjArg: bool
            Key: SymbolKey
            MemberName: string
            Storage: MemberStorage
            ArgGroupWidths: EqArray<int>
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
    /// The candidate support set (`FrozenType`s, not sub-expressions) + member name; the
    /// args are the child expressions.
    | TraitCall of
        {|
            SupportTys: EqArray<FrozenType>
            MemberName: string
        |}
    /// The specialization-table slot this call identifies, and the file the node's own anchor (and
    /// its args') indexes. The entry is a root of its own, not a child edge: several call
    /// sites share one.
    | InlineCall of
        {|
            Spec: SpecializationId
            Path: AssemblyFilePath
        |}
    /// The file everything under this node is anchored in.
    | CallerExpr of path: AssemblyFilePath

[<RequireQualifiedAccess>]
module ExprPayload =

    let shape (p: ExprPayload) : ExprShape =
        match p with
        | ExprPayload.Const _ -> ExprShape.Const
        | ExprPayload.Var -> ExprShape.Var
        | ExprPayload.External _ -> ExprShape.External
        | ExprPayload.Unresolved -> ExprShape.Unresolved
        | ExprPayload.Lambda -> ExprShape.Lambda
        | ExprPayload.App _ -> ExprShape.App
        | ExprPayload.Let _ -> ExprShape.Let
        | ExprPayload.LetGroup _ -> ExprShape.LetGroup
        | ExprPayload.Use _ -> ExprShape.Use
        | ExprPayload.IfThenElse -> ExprShape.IfThenElse
        | ExprPayload.Tuple -> ExprShape.Tuple
        | ExprPayload.ArrayLit -> ExprShape.ArrayLit
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
        | ExprPayload.CallerExpr _ -> ExprShape.CallerExpr

    /// Map every `Anchor` an expression payload EMBEDS: the loop variable's identifier token,
    /// each `LetGroup` member's and each format hole's. A node's own anchor lives in a column
    /// and is mapped there.
    let mapToks (f: Anchor -> Anchor) (p: ExprPayload) : ExprPayload =
        let seg (s: FormatSegShape) : FormatSegShape =
            let spec (h: Pooled.HoleSpec) : Pooled.HoleSpec = { h with Tok = f h.Tok }

            match s with
            | FormatSegShape.Lit _ -> s
            | FormatSegShape.Hole h -> FormatSegShape.Hole(spec h)
            | FormatSegShape.DynHole(hasWidth, hasPrecision, h) ->
                FormatSegShape.DynHole(hasWidth, hasPrecision, spec h)
            | FormatSegShape.CallbackHole h -> FormatSegShape.CallbackHole(spec h)

        match p with
        | ExprPayload.ForTo ft -> ExprPayload.ForTo {| ft with IdentTok = f ft.IdentTok |}
        | ExprPayload.LetGroup g ->
            ExprPayload.LetGroup
                { g with
                    Members = g.Members |> Array.map (fun m -> { m with Tok = f m.Tok })
                }
        | ExprPayload.Format fm ->
            ExprPayload.Format
                {| fm with
                    Segments = fm.Segments |> Array.map seg
                |}
        | ExprPayload.Const _
        | ExprPayload.Var
        | ExprPayload.External _
        | ExprPayload.Unresolved
        | ExprPayload.Lambda
        | ExprPayload.App _
        | ExprPayload.Let _
        | ExprPayload.Use _
        | ExprPayload.IfThenElse
        | ExprPayload.Tuple
        | ExprPayload.ArrayLit
        | ExprPayload.Sequential
        | ExprPayload.While
        | ExprPayload.ForIn _
        | ExprPayload.Match _
        | ExprPayload.TryWith _
        | ExprPayload.TryFinally
        | ExprPayload.Assignment
        | ExprPayload.Null
        | ExprPayload.Range _
        | ExprPayload.RecordCons _
        | ExprPayload.RecordClone _
        | ExprPayload.FieldGet _
        | ExprPayload.FieldSet _
        | ExprPayload.UnionCons _
        | ExprPayload.New _
        | ExprPayload.MethodCall _
        | ExprPayload.PropertyGet _
        | ExprPayload.StaticMethodCall _
        | ExprPayload.StaticPropertyGet _
        | ExprPayload.StaticFieldGet _
        | ExprPayload.StaticFieldSet _
        | ExprPayload.ExternalMember _
        | ExprPayload.ILIntrinsic _
        | ExprPayload.StaticOptimization _
        | ExprPayload.Upcast
        | ExprPayload.Downcast
        | ExprPayload.TypeTest _
        | ExprPayload.TraitCall _
        // A `Source` is a file IDENTITY and not an `Anchor`, because a MOVE that remapped it
        // would be claiming the subtree came from somewhere it did not.
        | ExprPayload.InlineCall _
        | ExprPayload.CallerExpr _ -> p

    // ── re-nesting the flat child columns ───────────────────────────────────
    // A composite carrier, such as an arm or a format segment, has no node identity of its own: its
    // pieces go in the child columns and the payload keeps the STRUCTURE that puts them back.

    /// A reader that draws a node's children in column order, one per call. `start` skips the
    /// node's own LEADING children (`Match`'s scrutinee, `TryWith`'s body).
    let cursor (xs: 'a[]) (start: int) : unit -> 'a =
        let mutable i = start

        fun () ->
            let x = xs.[i]
            i <- i + 1
            x

    /// Re-nest the arm children of a `Match`/`TryWith`: each arm draws its pat, then its
    /// guard when `guardPresent` says it has one, then its body, because that is the order the
    /// pooling walk enumerated them in. Arm count is the flag array's length.
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

    /// Re-nest the children of a `Format`: the sink's own sub-expression first, then each
    /// segment's, with the dyn-hole presence flags saying which dimensions are there.
    let format
        (sink: FormatSinkShape)
        (segments: FormatSegShape[])
        (nextExpr: unit -> 'e)
        : FormatSinkG<'e> * FormatSegG<FrozenType, Anchor, 'e>[] =
        let spec = TastConvert.hole id id

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

/// The residual payload of a frozen pattern node, one case per `PatShape`, carrying only
/// what the columnar split left: not `ty`/`tok`, not the child sub-pat ids. A pattern owns
/// no child expressions.
[<RequireQualifiedAccess>]
type PatPayload =
    /// The single bound variable this pattern INTRODUCES, named by the dense id its `Var` references
    /// resolve to. `isMutable` holds for the bound variable of a `let mutable`.
    | NamedSimple of boundVar: BoundVarId * isMutable: bool
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
    | EnumCase of {| EnumKey: TypeKey; CaseName: string |}

[<RequireQualifiedAccess>]
module PatPayload =

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
    /// `isinst` operand); a node's own type lives in a column and is mapped there.
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

/// How a backend SPELLS a bound variable. A source identifier is carried verbatim, because
/// dialect mangling (JS reserved words, apostrophes) belongs to the backend that emits it.
[<RequireQualifiedAccess>]
[<Struct>]
type BoundVarNaming =
    /// The identifier the source spells this bound variable with.
    | Source of name: string
    /// No identifier spells this bound variable (a class's `this`/`base`, a freshened inline
    /// bound variable); a backend invents one from the slot, which is unique by construction.
    | Minted of slot: BoundVarId

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module BoundVarNaming =

    /// The naming column read at `slot`. An EMPTY name stores "no identifier spells this
    /// bound variable"; no legal identifier is empty, so the two cases cannot be confused.
    let ofColumn (name: string) (slot: BoundVarId) : BoundVarNaming =
        match name.Length with
        | 0 -> BoundVarNaming.Minted slot
        | _ -> BoundVarNaming.Source name

/// The residual payload of a frozen declaration node, one case per `DeclShape`. A decl
/// has no node-level `ty`/`tok` column, so each case carries whatever type/scalars it needs;
/// its child expr/pat roots live in the decl child columns.
[<RequireQualifiedAccess>]
type DeclPayload =
    /// The binding is the sole pat child, its value the sole expr child;
    /// `IsInline`/`IsRec`/`Recursion`/`Ty` (the binding's declared slot type) are the residual
    /// scalars. `IsRec` is the source `rec` keyword, distinct from the analysed `Recursion`.
    | Let of
        {|
            IsInline: bool
            IsRec: bool
            Recursion: Recursion
            Ty: FrozenType
        |}
    /// Member `i`'s pattern is pat child `i` and its value expr child `i`.
    | LetGroup of LetGroupShape
    /// The decl's declared type; the body is the sole expr child.
    | Expression of FrozenType
    /// The `type` declaration's shape, its body slots holding pool ids rather than trees.
    /// It surfaces no decl expr children: the bodies are named by id INSIDE the shape.
    | Type of PooledTypeDecl

[<RequireQualifiedAccess>]
module DeclPayload =

    let shape (p: DeclPayload) : DeclShape =
        match p with
        | DeclPayload.Let _ -> DeclShape.Let
        | DeclPayload.LetGroup _ -> DeclShape.LetGroup
        | DeclPayload.Expression _ -> DeclShape.Expression
        | DeclPayload.Type _ -> DeclShape.Type

// ── the ROW view: one node's slice across the parallel columns ──────────────
// A `*Row` is the TRANSPOSE of the columns at one id, so a rewrite is
// `{ row with Children = … }`, with no per-case match on the node's shape.

/// One expression node's slice across the `Expr*` columns, in column order.
type ExprRow =
    {
        Ty: FrozenType
        Tok: Anchor
        Children: ExprPoolId[]
        PatChildren: PatPoolId[]
        /// The `Var` reference edge (`ValueNone` at every other shape). A walk-produced row
        /// leaves it `ValueNone`: a `Var` may reference a bound variable the walk has not reached yet.
        VarBoundVar: BoundVarId voption
        Payload: ExprPayload
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module ExprRow =

    /// Equivalent to `a = b`, but reference-checks `Ty`/`Payload` first: F# record
    /// equality has no physical-identity shortcut, so it walks a whole `FrozenType` an edit
    /// carried across unmoved. Children stay by value, because a substitution mints a fresh array.
    let same (a: ExprRow) (b: ExprRow) : bool =
        (obj.ReferenceEquals(a.Ty, b.Ty) || a.Ty = b.Ty)
        && a.Tok = b.Tok
        && a.Children = b.Children
        && a.PatChildren = b.PatChildren
        && a.VarBoundVar = b.VarBoundVar
        && (obj.ReferenceEquals(a.Payload, b.Payload) || a.Payload = b.Payload)

/// One pattern node's slice across the `Pat*` columns, in column order.
type PatRow =
    {
        Ty: FrozenType
        Tok: Anchor
        Children: PatPoolId[]
        Payload: PatPayload
    }

/// One declaration node's slice across the `Decl*` columns, in column order (a decl has no
/// node-level `ty`/`tok`, so its type is carried on the payload).
type DeclRow =
    {
        ExprChildren: ExprPoolId[]
        PatChildren: PatPoolId[]
        Payload: DeclPayload
    }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module DeclRow =

    /// The unchanged test for a decl row copy. A `Type` decl's payload is the whole
    /// declaration shape, so the physical-identity shortcut on `Payload` matters most here.
    let same (a: DeclRow) (b: DeclRow) : bool =
        a.ExprChildren = b.ExprChildren
        && a.PatChildren = b.PatChildren
        && (obj.ReferenceEquals(a.Payload, b.Payload) || a.Payload = b.Payload)
