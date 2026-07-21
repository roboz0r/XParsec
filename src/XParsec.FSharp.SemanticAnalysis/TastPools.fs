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
// Layout decision (the one load-bearing one): an entry is the *tag column* (`Shape`)
// plus the *dense child-id columns* (`ExprChildren` / `PatChildren`) plus a reference
// to the verbatim source DU node. Only the tree CHILD EDGES are turned into ids; the
// heterogeneous non-child payload (keys, strings, `FrozenType`, tokens, `NodeKey`
// references) is NOT re-listed here — it rides on the retained `Node`, exactly as the
// DU holds it. This is the "array of id-records" reading of the plan's fixed-width
// id-records: O(1) id-indexable, and it reuses the accessor's child enumeration
// (`TastAccessor.exprChildren`/`exprPatChildren`/`patChildren`) for the forward
// direction rather than re-deriving the tree spine or the ~38-case field inventory
// that `Tast.fs`/`FrozenCodec` already own. `ofPools` rebuilds each node from that
// same node as the STRUCTURAL TEMPLATE (its scalars + its composite-carrier shape —
// which arm carries a guard, which hole is starred) with the child SUBTREES supplied
// by the pool ids, so the reconstruction genuinely exercises the child columns. The
// uniform fields (`Shape`, and `ty`/`tok` reachable off `Node`) are kept out on the
// entry rather than buried, so the later columnar-SoA split can pull dense columns out
// without disturbing this shape. Child columns are filled in the accessor's
// enumeration order, and `ofPools` consumes them in that same order — the coupling the
// round-trip test guards.
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

/// One expression pool entry. `ExprChildren` are the ids of the immediate child
/// expressions in `TastAccessor.exprChildren` order; `PatChildren` the ids of the
/// patterns this node owns (`TastAccessor.exprPatChildren` order). `Node` is the
/// verbatim source DU node — the payload/`ty`/`tok`/composite-shape template that the
/// backing flip later dissolves into dense columns.
[<Struct>]
type ExprPoolEntry =
    {
        Shape: ExprShape
        ExprChildren: ExprPoolId[]
        PatChildren: PatPoolId[]
        /// For a `Var` node, the dense id of the binder its `binding` NodeKey names —
        /// the reference edge routed to positional identity. `ValueNone` for every
        /// other shape (which carry no binder reference). `ofPools` reconstructs
        /// `Var.binding` from THIS, not from the retained `Node`, so the round-trip
        /// exercises the remap rather than copying the content key back verbatim.
        VarBinder: BinderId voption
        Node: Frozen.TExpr
    }

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
/// three dense node columns (indexable by the matching `*PoolId`) and the decl roots
/// the file's `Decls` pooled to, in source order. Kept OFF `TastFileG` — that record is
/// shared with the `SemType` instantiation, which has no pools.
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
        Exprs: ExprPoolEntry[]
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
        /// through `TastWalk.lambdaKey` on the pooled `Lambda` node.
        FunVerdicts: (ExprPoolId * FunVerdict)[]
        GenericFnSchemes: (BinderId * FrozenConstraint list)[]
        BindingValReprs: (BinderId * Frozen.ValRepr)[]
        BindingTyparArities: (BinderId * int)[]
    }

[<RequireQualifiedAccess>]
module TastPools =

    /// Pool the frozen tree of `file.Decls`, assigning each reachable node a dense id
    /// and recording its child edges as ids. Post-order: a node's children are pooled
    /// (and so given lower ids) before the node itself is recorded, so every child id a
    /// column names already resolves. The child enumeration is the accessor's — no
    /// tree-shape knowledge is duplicated here.
    let toPools (file: Frozen.TastFile) : FrozenPools =
        let exprs = ResizeArray<ExprPoolEntry>()
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
            let id = exprs.Count

            exprs.Add
                {
                    Shape = TastAccessor.exprKind e
                    ExprChildren = exprKids
                    PatChildren = patKids
                    // Resolved in a second pass — a `Var` may name a binder pooled after
                    // it (a forward/mutually-recursive reference), so the enumeration must
                    // be complete before any reference resolves.
                    VarBinder = ValueNone
                    Node = e
                }

            // A lambda's positional identity is this very slot; stamp the map so
            // `FunVerdicts` (lambda-expression-keyed) resolves onto it.
            match TastAccessor.exprKind e with
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
        // to its binder's dense id.
        let exprArr =
            exprs.ToArray()
            |> Array.map (fun entry ->
                match entry.Shape with
                | ExprShape.Var ->
                    { entry with
                        VarBinder = ValueSome(binderIdOf (TastAccessor.exprVarBinding entry.Node))
                    }
                | _ -> entry
            )

        // One generic remap over the side tables, parameterized by the key resolver: the
        // binder-keyed tables pass `binderIdOf`, `FunVerdicts` passes `lambdaIdOf`. A second
        // resolver, not a second remap, so the two id spaces share one enumeration.
        let remapSideTable (resolve: NodeKey -> 'id) (m: Map<NodeKey, 'v>) : ('id * 'v)[] =
            m |> Map.toArray |> Array.map (fun (k, v) -> resolve k, v)

        {
            Exprs = exprArr
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
    // Each `substitute*` takes the pool entry's `Node` as the structural template and
    // the ALREADY-REBUILT child subtrees, and re-authors the node with the template's
    // scalars and the supplied children. The children are consumed in the exact order
    // `TastAccessor.exprChildren`/`exprPatChildren`/`patChildren` enumerated them
    // (`nextE`/`nextP` are order cursors) — the one coupling the round-trip gate proves.
    // Each match is exhaustive with no catch-all, so a new `TExprG`/`TPatG`/`TDeclG`
    // case fails to compile here.

    let private substituteExpr
        (node: Frozen.TExpr)
        (varBinding: NodeKey voption)
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

        match node with
        // `binding` is re-supplied from the dense id, NOT read off the template `node`:
        // that is what makes the round-trip exercise the reference remap.
        | TExprG.Var(ty = ty; tok = tok) ->
            match varBinding with
            | ValueSome k -> TExprG.Var(k, ty, tok)
            | ValueNone -> failwith "TastPools.ofPools: a Var entry carries no resolved binder id"
        | TExprG.Const _
        | TExprG.External _
        | TExprG.Null _
        | TExprG.StaticPropertyGet _
        | TExprG.StaticFieldGet _ -> node
        | TExprG.Lambda(_, _, ty, tok) ->
            let param = nextP ()
            let body = nextE ()
            TExprG.Lambda(param, body, ty, tok)
        | TExprG.App(_, _, ty, tok) ->
            let fn = nextE ()
            let arg = nextE ()
            TExprG.App(fn, arg, ty, tok)
        | TExprG.Let(_, _, _, ty, tok) ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Let(binding, value, body, ty, tok)
        | TExprG.Use(_, _, _, dispose, ty, tok) ->
            let binding = nextP ()
            let value = nextE ()
            let body = nextE ()
            TExprG.Use(binding, value, body, dispose, ty, tok)
        | TExprG.IfThenElse(_, _, _, ty, tok) ->
            let cond = nextE ()
            let thenExpr = nextE ()
            let elseExpr = nextE ()
            TExprG.IfThenElse(cond, thenExpr, elseExpr, ty, tok)
        | TExprG.Tuple(_, ty, tok) -> TExprG.Tuple(EqArray.ofArray es, ty, tok)
        | TExprG.Sequential(_, ty, tok) -> TExprG.Sequential(EqArray.ofArray es, ty, tok)
        | TExprG.While(_, _, ty, tok) ->
            let cond = nextE ()
            let body = nextE ()
            TExprG.While(cond, body, ty, tok)
        | TExprG.ForTo(var, identTok, _, _, _, ty, tok) ->
            let startExpr = nextE ()
            let endExpr = nextE ()
            let body = nextE ()
            TExprG.ForTo(var, identTok, startExpr, endExpr, body, ty, tok)
        | TExprG.ForIn(_, _, _, enumerator, ty, tok) ->
            let pat = nextP ()
            let source = nextE ()
            let body = nextE ()
            TExprG.ForIn(pat, source, body, enumerator, ty, tok)
        | TExprG.Match(_, arms, ty, tok) ->
            let scrutinee = nextE ()

            let arms' =
                arms
                |> EqArray.toArray
                |> Array.map (fun arm ->
                    let pat = nextP ()

                    let guard =
                        match arm.Guard with
                        | Some _ -> Some(nextE ())
                        | None -> None

                    let body = nextE ()

                    {
                        Pat = pat
                        Guard = guard
                        Body = body
                    }
                )
                |> EqArray.ofArray

            TExprG.Match(scrutinee, arms', ty, tok)
        | TExprG.TryWith(_, arms, ty, tok) ->
            let body = nextE ()

            let arms' =
                arms
                |> EqArray.toArray
                |> Array.map (fun arm ->
                    let pat = nextP ()

                    let guard =
                        match arm.Guard with
                        | Some _ -> Some(nextE ())
                        | None -> None

                    let armBody = nextE ()

                    {
                        Pat = pat
                        Guard = guard
                        Body = armBody
                    }
                )
                |> EqArray.ofArray

            TExprG.TryWith(body, arms', ty, tok)
        | TExprG.TryFinally(_, _, ty, tok) ->
            let body = nextE ()
            let cleanup = nextE ()
            TExprG.TryFinally(body, cleanup, ty, tok)
        | TExprG.Assignment(_, _, ty, tok) ->
            let lhs = nextE ()
            let rhs = nextE ()
            TExprG.Assignment(lhs, rhs, ty, tok)
        | TExprG.Range(_, step, _, ty, tok) ->
            let startExpr = nextE ()

            let step' =
                match step with
                | Some _ -> Some(nextE ())
                | None -> None

            let stopExpr = nextE ()
            TExprG.Range(startExpr, step', stopExpr, ty, tok)
        | TExprG.RecordCons(fields, ty, tok) ->
            let fields' =
                fields
                |> EqArray.toArray
                |> Array.map (fun (name, _) -> (name, nextE ()))
                |> EqArray.ofArray

            TExprG.RecordCons(fields', ty, tok)
        | TExprG.RecordClone(_, overrides, ty, tok) ->
            let source = nextE ()

            let overrides' =
                overrides
                |> EqArray.toArray
                |> Array.map (fun (name, _) -> (name, nextE ()))
                |> EqArray.ofArray

            TExprG.RecordClone(source, overrides', ty, tok)
        | TExprG.FieldGet(_, fieldName, ty, tok) ->
            let receiver = nextE ()
            TExprG.FieldGet(receiver, fieldName, ty, tok)
        | TExprG.FieldSet(_, fieldName, _, ty, tok) ->
            let receiver = nextE ()
            let value = nextE ()
            TExprG.FieldSet(receiver, fieldName, value, ty, tok)
        | TExprG.UnionCons(caseName, _, ty, tok) -> TExprG.UnionCons(caseName, EqArray.ofArray es, ty, tok)
        | TExprG.New(className, key, _, ty, tok) -> TExprG.New(className, key, EqArray.ofArray es, ty, tok)
        | TExprG.MethodCall(_, key, via, _, ty, tok) ->
            let receiver = nextE ()
            // The remaining `es` (after the receiver) are exactly the args, in order.
            let args = es.[ei..] |> EqArray.ofArray
            TExprG.MethodCall(receiver, key, via, args, ty, tok)
        | TExprG.PropertyGet(_, key, via, ty, tok) ->
            let receiver = nextE ()
            TExprG.PropertyGet(receiver, key, via, ty, tok)
        | TExprG.StaticMethodCall(key, _, ty, tok) -> TExprG.StaticMethodCall(key, EqArray.ofArray es, ty, tok)
        | TExprG.StaticFieldSet(declKey, fieldName, _, ty, tok) ->
            let value = nextE ()
            TExprG.StaticFieldSet(declKey, fieldName, value, ty, tok)
        | TExprG.ExternalMember(receiver, key, memberName, storage, ty, tok) ->
            let receiver' =
                match receiver with
                | ValueSome _ -> ValueSome(nextE ())
                | ValueNone -> ValueNone

            TExprG.ExternalMember(receiver', key, memberName, storage, ty, tok)
        | TExprG.Format(sink, segments, ty, tok) ->
            let sink' =
                match sink with
                | FormatSinkG.ToWriter(_, newline) -> FormatSinkG.ToWriter(nextE (), newline)
                | FormatSinkG.ToBuilder _ -> FormatSinkG.ToBuilder(nextE ())
                | FormatSinkG.ToStdOut _
                | FormatSinkG.ToStdErr _
                | FormatSinkG.ToString -> sink

            let segments' =
                segments
                |> EqArray.toArray
                |> Array.map (fun seg ->
                    match seg with
                    | FormatSegG.Lit _ -> seg
                    | FormatSegG.Hole(spec, _) -> FormatSegG.Hole(spec, nextE ())
                    | FormatSegG.DynHole hole ->
                        let width =
                            match hole.Width with
                            | ValueSome _ -> ValueSome(nextE ())
                            | ValueNone -> ValueNone

                        let precision =
                            match hole.Precision with
                            | ValueSome _ -> ValueSome(nextE ())
                            | ValueNone -> ValueNone

                        let value = nextE ()

                        FormatSegG.DynHole
                            { hole with
                                Width = width
                                Precision = precision
                                Value = value
                            }
                    | FormatSegG.CallbackHole(spec, _) -> FormatSegG.CallbackHole(spec, nextE ())
                )
                |> EqArray.ofArray

            TExprG.Format(sink', segments', ty, tok)
        | TExprG.ILIntrinsic(opCode, typeOperand, _, ty, tok) ->
            TExprG.ILIntrinsic(opCode, typeOperand, EqArray.ofArray es, ty, tok)
        | TExprG.StaticOptimization(clauses, _, ty, tok) ->
            let clauses' =
                clauses
                |> EqArray.toArray
                |> Array.map (fun clause -> { clause with Body = nextE () })
                |> EqArray.ofArray

            let defaultExpr = nextE ()
            TExprG.StaticOptimization(clauses', defaultExpr, ty, tok)
        | TExprG.Upcast(_, ty, tok) -> TExprG.Upcast(nextE (), ty, tok)
        | TExprG.Downcast(_, ty, tok) -> TExprG.Downcast(nextE (), ty, tok)
        | TExprG.TypeTest(_, testTy, ty, tok) ->
            let source = nextE ()
            TExprG.TypeTest(source, testTy, ty, tok)
        | TExprG.TraitCall(receiver, memberName, _, ty, tok) ->
            TExprG.TraitCall(receiver, memberName, EqArray.ofArray es, ty, tok)

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
        // codegen looks its verdict up under — `TastWalk.lambdaKey` on the pooled `Lambda`
        // node, the same construction `toPools` keyed it by.
        let lambdaKeyOf (ExprPoolId i) : NodeKey = TastWalk.lambdaKey pools.Exprs.[i].Node

        let rec fromPat (PatPoolId i) : Frozen.TPat =
            let entry = pools.Pats.[i]
            let ps = entry.PatChildren |> Array.map fromPat
            substitutePat entry.Node ps

        let rec fromExpr (ExprPoolId i) : Frozen.TExpr =
            let entry = pools.Exprs.[i]
            let es = entry.ExprChildren |> Array.map fromExpr
            let ps = entry.PatChildren |> Array.map fromPat
            let varBinding = entry.VarBinder |> ValueOption.map binderKey
            substituteExpr entry.Node varBinding es ps

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
