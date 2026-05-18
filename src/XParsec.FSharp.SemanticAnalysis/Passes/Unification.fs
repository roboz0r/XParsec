namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Pre:  ctx.Desugared and ctx.Binding populated.
// Post: ctx.TypeVar populated; every TypeVar's Link reaches its solved type
//       via UnionFind.find. ctx.Scheme populated for every generalisable
//       `let`-bound name (single-name headPats — see `shouldGeneralise`).
//
// Algorithm J + Rémy's levels: fresh TypeVar per AST node, constraints
// generated and unified on the fly. Generalisation happens at the close of
// each binding group; instantiation at every use of a scheme-bearing name.
//
// Tiny-subset omissions (TODO):
//   - Value restriction is split: the *generalisation gate* on
//     `mutableToken` lives here (`shouldGeneralise`), but the *diagnostic*
//     for a mutable binding whose resolved type still has free TyVars at
//     end of analysis lives in Validation — by then every use site has
//     had a chance to pin them via unification. See docs/mutable-plan.md.
//   - `ref` cells / refs-as-values still generalise without a check; lands
//     when the `Ref<'a>` provider entry does (see mutable-plan §Open questions).
//   - SRTP / IWSAM bound resolution. The on-unified callbacks per
//     docs/typevar.md aren't wired yet.
//   - Binding-level return-type annotations (`let f x : int = ...`). Only
//     Expr.TypeAnnotation (`(e : t)`) is handled today.

module Unification =

    /// Walk TyVar links to the equivalence-class representative; if the rep
    /// has a Link, return its target (one level deep — call recursively for
    /// full resolution). Stops at a measure-bearing root so the measure
    /// stays attached: `unify` and `unitsOf` need the TyVar wrapper to
    /// see Units, and following Link straight through to the bare carrier
    /// would drop them.
    let private resolveStep (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' when root.Units.IsNone -> t'
            | _ -> TyVar root
        | _ -> t

    /// Fully resolve a SemType: walk all TyVar chains AND recurse into TyFun
    /// arms. Used by Freeze (and tests) to materialise the final inferred
    /// type for a node. A measure-bearing TyVar (`Units` set on its root)
    /// is preserved as a TyVar rather than collapsed into its carrier —
    /// the measure rides on the root, so downstream consumers can read it
    /// off the returned `TyVar` (already a root).
    let rec zonk (t: SemType) : SemType =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome t' when root.Units.IsNone -> zonk t'
            | _ -> TyVar root
        | TyConst _ -> t
        | TyFun(a, r) -> TyFun(zonk a, zonk r)
        | TyTuple items -> TyTuple(List.map zonk items)
        | TyRecord _ -> t

    /// Move pending deferred-constraint state from `source` onto `target`.
    /// Called whenever a TyVar is no longer the equivalence-class
    /// representative (either after union-find collapse, or when its Link is
    /// set). Bounds attached to a non-representative would otherwise never
    /// fire their on-unified callbacks. Lists are empty in the current
    /// subset, so this is a no-op at runtime — but the contract has to be
    /// honoured before SRTPs / IWSAMs come online.
    let private migrateBounds (target: TypeVar) (source: TypeVar) : unit =
        if not (System.Object.ReferenceEquals(target, source)) then
            if not (List.isEmpty source.IfaceBounds) then
                target.IfaceBounds <- source.IfaceBounds @ target.IfaceBounds
                source.IfaceBounds <- []

            if not (List.isEmpty source.SrtpBounds) then
                target.SrtpBounds <- source.SrtpBounds @ target.SrtpBounds
                source.SrtpBounds <- []

            if not (List.isEmpty source.PendingFieldAccess) then
                target.PendingFieldAccess <- source.PendingFieldAccess @ target.PendingFieldAccess
                source.PendingFieldAccess <- []
    // TODO: fire on-unified callbacks for newly-stable bounds once
    // the SRTP / IWSAM resolution machinery exists. Until then,
    // appending is enough to preserve them through unification.

    /// Two passes folded into one walk:
    /// (a) **Occurs check** — does `target` (already a union-find root) appear
    ///     anywhere inside `t`? Stops the `let rec f x = f` / `let rec g = g g`
    ///     family from cycling Link pointers and making zonk loop.
    /// (b) **Level adjustment** — when `target` is about to be linked to `t`,
    ///     every TyVar reachable from `t` becomes co-scoped with `target`.
    ///     Lower any reachable level above `target.Level` down to it so
    ///     generalisation at the enclosing scope sees the right "free" set.
    /// Resolves through Links and recurses into compound shapes. The `||`
    /// short-circuit on occurs-fail leaves some reachable TyVars unadjusted,
    /// but a failed unification produces a diagnostic and there's nothing
    /// to generalise after; adjusting them would be wasted work.
    let rec private occursAndAdjust (target: TypeVar) (t: SemType) : bool =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if System.Object.ReferenceEquals(root, target) then
                true
            else
                if root.Level > target.Level then
                    root.Level <- target.Level

                false
        | TyConst _ -> false
        | TyFun(a, r) -> occursAndAdjust target a || occursAndAdjust target r
        | TyTuple items -> List.exists (occursAndAdjust target) items
        | TyRecord _ -> false

    /// Merge the Units field of two union-find roots after they've been
    /// joined into `newRoot`. Two non-equal measures emit a diagnostic; one
    /// of them is kept on the survivor so further unifications against it
    /// stay coherent. ValueNone on one side propagates from the other.
    let private mergeUnits
        (ctx: PassContext)
        (key: NodeKey)
        (newRoot: TypeVar)
        (unitsA: MeasureTerm voption)
        (unitsB: MeasureTerm voption)
        : unit =
        match unitsA, unitsB with
        | ValueNone, ValueNone -> ()
        | ValueSome m, ValueNone
        | ValueNone, ValueSome m -> newRoot.Units <- ValueSome m
        | ValueSome m1, ValueSome m2 when m1.Equals(m2) -> newRoot.Units <- ValueSome m1
        | ValueSome m1, ValueSome m2 ->
            newRoot.Units <- ValueSome m1

            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                    Severity = Error
                }

    /// Walk a `SemType` through TyVar Links to surface a `TyRecord _` if
    /// the type has resolved to one. Returns ValueNone for free TyVars and
    /// non-record concrete types. Inlined here so `drainPendingFieldAccess`
    /// can recognise records pinned through a chain.
    let rec private tryResolveRecord (t: SemType) : string voption =
        match t with
        | TyRecord n -> ValueSome n
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome target -> tryResolveRecord target
            | ValueNone -> ValueNone
        | _ -> ValueNone

    let rec private unify (ctx: PassContext) (key: NodeKey) (a: SemType) (b: SemType) =
        let a = resolveStep a
        let b = resolveStep b

        match a, b with
        | TyConst n1, TyConst n2 when n1 = n2 -> ()
        | TyRecord n1, TyRecord n2 when n1 = n2 -> ()
        | TyFun(a1, r1), TyFun(a2, r2) ->
            unify ctx key a1 a2
            unify ctx key r1 r2
        | TyTuple xs, TyTuple ys when xs.Length = ys.Length -> List.iter2 (unify ctx key) xs ys
        | TyVar tv1, TyVar tv2 when System.Object.ReferenceEquals(tv1, tv2) -> ()
        | TyVar tv1, TyVar tv2 ->
            let r1 = UnionFind.find tv1
            let r2 = UnionFind.find tv2
            let unitsA = r1.Units
            let unitsB = r2.Units
            let linkA = r1.Link
            let linkB = r2.Link
            UnionFind.union r1 r2
            // After union, exactly one of r1/r2 still has Parent = ValueNone.
            let newRoot = UnionFind.find r1

            let merged =
                if System.Object.ReferenceEquals(newRoot, r1) then
                    r2
                else
                    r1

            migrateBounds newRoot merged
            mergeUnits ctx key newRoot unitsA unitsB
            // Migrate Link: if one side carried a concrete target and the
            // survivor doesn't, copy it across. If both sides carried links,
            // unify them so the carrier types agree.
            match linkA, linkB with
            | ValueNone, ValueNone -> ()
            | ValueSome _, ValueNone ->
                newRoot.Link <- linkA

                match linkA with
                | ValueSome t -> drainPendingFieldAccess ctx newRoot t
                | ValueNone -> ()
            | ValueNone, ValueSome _ ->
                newRoot.Link <- linkB

                match linkB with
                | ValueSome t -> drainPendingFieldAccess ctx newRoot t
                | ValueNone -> ()
            | ValueSome a, ValueSome b ->
                newRoot.Link <- linkA
                unify ctx key a b

                match linkA with
                | ValueSome t -> drainPendingFieldAccess ctx newRoot t
                | ValueNone -> ()
        | TyVar tv, other
        | other, TyVar tv ->
            let root = UnionFind.find tv

            if occursAndAdjust root other then
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message =
                            sprintf
                                "Occurs check: cannot construct infinite type %A = %A"
                                (zonk (TyVar root))
                                (zonk other)
                        Severity = Error
                    }
            else
                // Linking to a plain TyConst (a dimensionless carrier) when
                // the variable is already known to be measured is a
                // dimensionless-vs-measured mismatch.
                match root.Units, other with
                | ValueSome m, TyConst _ when not m.IsDimensionless ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Dimensionless %A used where <%O> expected" other m
                            Severity = Error
                        }
                | _ -> ()

                root.Link <- ValueSome other
                drainPendingFieldAccess ctx root other
        // No bound migration needed here: `root` keeps its bounds, and
        // setting Link is the trigger for on-unified callbacks to fire
        // once they exist.
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = sprintf "Type mismatch: %A vs %A" (zonk a) (zonk b)
                    Severity = Error
                }

    /// When a TyVar's Link is set to (or resolves to) a `TyRecord T`,
    /// resolve any field-access constraints that were parked on it. Each
    /// entry unifies the access expression's result TyVar with the field's
    /// declared type; a missing field produces a diagnostic.
    and private drainPendingFieldAccess (ctx: PassContext) (root: TypeVar) (linkTarget: SemType) : unit =
        if List.isEmpty root.PendingFieldAccess then
            ()
        else
            match tryResolveRecord linkTarget with
            | ValueNone -> ()
            | ValueSome recName ->
                let pending = root.PendingFieldAccess
                root.PendingFieldAccess <- []

                match ctx.RecordTypes.TryGetValue recName with
                | true, info ->
                    for (fieldName, useKey, resultTv) in pending do
                        match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                        | Some field -> unify ctx useKey (TyVar resultTv) field.Type
                        | None ->
                            ctx.Diagnostics.Add
                                {
                                    Key = useKey
                                    Message = sprintf "Type '%s' has no field '%s'" recName fieldName
                                    Severity = Error
                                }
                | false, _ ->
                    for (_, useKey, _) in pending do
                        ctx.Diagnostics.Add
                            {
                                Key = useKey
                                Message = sprintf "Unknown record type '%s'" recName
                                Severity = Error
                            }

    let private enterLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel + 1

    let private exitLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel - 1

    /// Allocate a fresh, unkeyed TypeVar at the current let-depth. Used for
    /// intermediate "result" TyVars in apps, ifs, matches — anything not
    /// directly tied to a CST node's NodeKey.
    let private freshTyVar (ctx: PassContext) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        tv

    /// Allocate a fresh TypeVar for `key`, stamped at the current level, and
    /// store it in ctx.TypeVar. Overwrites any prior entry — callers that
    /// need "get or allocate" (e.g. for forward-referenced let-rec siblings)
    /// must go through `tvOf`.
    let private freshTv (ctx: PassContext) (key: NodeKey) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        ctx.TypeVar.Set(key, tv)
        tv

    /// Look up the TypeVar previously stored for a node. Fresh-allocates if
    /// missing — happens for binding-site patterns that haven't been visited
    /// yet by inferPat, including forward references inside `let rec` groups.
    let private tvOf (ctx: PassContext) (key: NodeKey) : TypeVar =
        match ctx.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Mint fresh TyVars at the current level for every quantifier of
    /// `scheme`, then walk `scheme.Body` rewriting each quantified TyVar to
    /// its fresh counterpart. Mirrors `ExternalSymbol.Instantiate` for the
    /// finite set of `'a`s captured by a user-written `let`. Non-quantified
    /// TyVars are left alone — they're free with respect to the surrounding
    /// scope and must keep their identity. `scheme.Body` is already zonked
    /// by `generalise`, so we don't follow Links here.
    let private instantiate (ctx: PassContext) (scheme: TypeScheme) : SemType =
        let subst = Dictionary<TypeVar, TypeVar>(HashIdentity.Reference)

        for q in scheme.Quantified do
            let qRoot = UnionFind.find q
            let fresh = TypeVar()
            fresh.Level <- ctx.CurrentLevel
            subst.[qRoot] <- fresh

        let rec walk (t: SemType) : SemType =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match subst.TryGetValue root with
                | true, fresh -> TyVar fresh
                | false, _ -> TyVar root
            | TyConst _ -> t
            | TyFun(a, r) -> TyFun(walk a, walk r)
            | TyTuple xs -> TyTuple [ for x in xs -> walk x ]
            | TyRecord _ -> t

        walk scheme.Body

    /// Walk `zonkedTy` and collect every union-find root whose Level strictly
    /// exceeds `outerLevel` — those are the TyVars to quantify. Dedupes by
    /// reference identity (a single TyVar can appear in multiple positions).
    /// Quantified TyVars stay live in the union-find graph; the scheme just
    /// captures their identities so `instantiate` can swap them per use.
    ///
    /// A TyVar with a concrete `Link` is not free — it has been pinned to a
    /// specific target. We skip those even though they survive zonking when
    /// they carry `Units` (measure-bearing TyVars). Treating them as ground
    /// matches v1 "no measure polymorphism" — each `let f (x : float<m>) …`
    /// has the measure baked in.
    /// True if `t` contains a TyVar whose root carries a deferred
    /// `PendingFieldAccess` constraint. Such a binding cannot be safely
    /// generalised in v1 — quantifying a TyVar with pending field accesses
    /// would freeze the constraint into the scheme, and a use site that
    /// pins the receiver would only resolve a fresh instantiation, leaving
    /// the original (still-quantified) constraint dangling. Keeping the
    /// binding monomorphic lets the first use site unify directly with the
    /// pre-instantiation TyVar, which drains the constraint normally.
    let rec private hasPendingFieldAccess (t: SemType) : bool =
        match t with
        | TyVar tv ->
            let root = UnionFind.find tv

            if not (List.isEmpty root.PendingFieldAccess) then
                true
            else
                match root.Link with
                | ValueSome target -> hasPendingFieldAccess target
                | ValueNone -> false
        | TyConst _ -> false
        | TyFun(a, r) -> hasPendingFieldAccess a || hasPendingFieldAccess r
        | TyTuple xs -> List.exists hasPendingFieldAccess xs
        | TyRecord _ -> false

    let private generalise (zonkedTy: SemType) (outerLevel: int) : TypeScheme =
        let quantified = ResizeArray<TypeVar>()
        let seen = HashSet<TypeVar>(HashIdentity.Reference)

        let rec walk (t: SemType) : unit =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                if root.Level > outerLevel && root.Link.IsNone && seen.Add(root) then
                    quantified.Add(root)
            | TyConst _ -> ()
            | TyFun(a, r) ->
                walk a
                walk r
            | TyTuple xs -> List.iter walk xs
            | TyRecord _ -> ()

        walk zonkedTy
        TypeScheme(List.ofSeq quantified, zonkedTy)

    /// Single-name `let` generalises unless the binding is `mutable`.
    /// Mutable bindings stay monomorphic: every use of the name unifies
    /// against the binding's own TyVar (no instantiation), so a free TyVar
    /// in a mutable binding's type can be pinned later by any use or
    /// assignment — but the binding is never made polymorphic at the
    /// scheme level, which would re-introduce the classic value-
    /// restriction soundness hole. Compound destructuring heads and
    /// bindings whose head is something other than `Pat.NamedSimple`
    /// don't get schemes either — they bind values, not function
    /// abstractions, and the scheme table is keyed by a single NodeKey.
    let private shouldGeneralise (b: Binding<SyntaxToken>) : bool =
        if b.mutableToken.IsSome then
            false
        else
            match b.headPat with
            | Pat.NamedSimple _ -> true
            | _ -> false

    /// SemType of the carrier of a single literal token (`int`, `float`, …).
    /// Pulled out of `inferConst` so the measured-literal arm can stamp this
    /// onto a TyVar's `Link` while the measure rides on `Units`.
    let private literalCarrier (t: SyntaxToken) : SemType =
        match t.Token with
        | Token.KWTrue
        | Token.KWFalse -> MockBuiltins.tyBool
        | Token.NumIEEE64
        | Token.NumIEEE64Hex
        | Token.NumIEEE64Octal
        | Token.NumIEEE64Binary -> MockBuiltins.tyFloat
        | Token.NumInt64
        | Token.NumInt64Hex
        | Token.NumInt64Octal
        | Token.NumInt64Binary -> MockBuiltins.tyInt64
        | Token.NumByte
        | Token.NumByteHex
        | Token.NumByteOctal
        | Token.NumByteBinary -> MockBuiltins.tyByte
        | _ -> MockBuiltins.tyInt

    /// Walk a `Measure<SyntaxToken>` CST and produce a canonical
    /// `MeasureTerm`. Multi-segment qualified unit names (`Microsoft.FSharp.SI.kg`)
    /// and measure typars (`'u`) are v2 — they produce an empty term plus a
    /// diagnostic so the rest of inference continues without measure noise.
    let rec private translateMeasure (ctx: PassContext) (diagKey: NodeKey) (m: Measure<SyntaxToken>) : MeasureTerm =
        match m with
        | Measure.One _ -> MeasureTerm.empty
        | Measure.Named li when li.Idents.Length = 1 -> MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], Rational.One ]
        | Measure.Power(inner, _, neg, expTok) ->
            let n = System.Numerics.BigInteger.Parse(ctx.NameOf expTok)
            let signed = if neg.IsSome then -n else n

            MeasureTerm.pow
                (translateMeasure ctx diagKey inner)
                (Rational.create (signed, System.Numerics.BigInteger.One))
        | Measure.Product(l, _, r) -> MeasureTerm.mul (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Quotient(l, _, r) -> MeasureTerm.div (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx diagKey inner)
        | Measure.Paren(_, inner, _) -> translateMeasure ctx diagKey inner
        | Measure.Juxtaposition(elems, _) ->
            (MeasureTerm.empty, elems)
            ||> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx diagKey m))
        | Measure.Anonymous _
        | Measure.Typar _
        | Measure.Named _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = "Measure typars / wildcards / qualified unit names not yet supported"
                    Severity = Error
                }

            MeasureTerm.empty

    let private inferConst (ctx: PassContext) (c: Constant<SyntaxToken>) : SemType =
        // Dispatch covers the numeric / boolean tokens that lex into a
        // Constant.Literal. Anything we don't recognise still types as int
        // (matches the parser's most common case) — extend as new literal
        // kinds become reachable.
        match c with
        | Constant.Literal t -> literalCarrier t
        | Constant.MeasuredLiteral(value = t; measure = m) ->
            let carrier = literalCarrier t
            let diagKey = NodeKey.ofToken t NodeKind.ExprConst
            let mt = translateMeasure ctx diagKey m
            let tv = freshTyVar ctx
            tv.Link <- ValueSome carrier
            tv.Units <- ValueSome mt
            TyVar tv

    /// Built-in numeric type names that can carry a measure annotation
    /// (`float<m>`, `int<kg>`, etc.). User-defined `[<Measure>]`-aware types
    /// land when records / DUs do.
    let private isNumericCarrier (name: string) : bool =
        match name with
        | "int"
        | "int64"
        | "byte"
        | "float"
        | "float32"
        | "decimal"
        | "single"
        | "double" -> true
        | _ -> false

    /// Translate a syntactic `Type<SyntaxToken>` into a `SemType`. The tiny
    /// subset only recognises the primitive built-ins (`int`, `bool`,
    /// `unit`) by name; anything else turns into a `TyConst <name>` whose
    /// unification will succeed only against an identical `TyConst`. Typars
    /// (`'a`) and generic types are TODO — they need typar-scoping plumbing
    /// we don't have yet.
    let rec private translateType (ctx: PassContext) (t: Type<SyntaxToken>) : SemType =
        match t with
        | Type.ParenType(typ = inner) -> translateType ctx inner
        | Type.NamedType li when li.Idents.Length = 1 ->
            let name = ctx.NameOf li.Idents.[0]

            match name with
            | "int" -> MockBuiltins.tyInt
            | "bool" -> MockBuiltins.tyBool
            | "unit" -> MockBuiltins.tyUnit
            | "float" -> MockBuiltins.tyFloat
            | "string" -> MockBuiltins.tyString
            | "int64" -> MockBuiltins.tyInt64
            | "byte" -> MockBuiltins.tyByte
            | _ ->
                if ctx.RecordTypes.ContainsKey name then
                    TyRecord name
                else
                    TyConst name
        | Type.GenericType(longIdent = li; typeArgs = args) when
            li.Idents.Length = 1
            && args.Length = 1
            && isNumericCarrier (ctx.NameOf li.Idents.[0])
            ->
            // `float<m>` / `int<kg>` — recognise the measure-shaped generic
            // and stamp the measure onto a fresh TyVar whose Link carries
            // the carrier. Anything else (real generics) still falls through
            // to the wildcard arm.
            //
            // The parser only tags an arg as `TypeArg.Measure` when the
            // measure grammar is unambiguous; for bare `float<m>` it lands
            // as `TypeArg.Type (Type.NamedType "m")` because the type
            // grammar can't tell unit names apart from type-arg type names.
            // Both shapes resolve here.
            let carrierTok = li.Idents.[0]
            let diagKey = NodeKey.ofToken carrierTok NodeKind.TypeGeneric

            let measureFromTypeArg =
                match args.[0] with
                | TypeArg.Measure m -> ValueSome m
                | TypeArg.Type(Type.NamedType nameLi) ->
                    // Reinterpret a single-segment named type as a measure
                    // atom; multi-segment qualifiers stay a real type.
                    if nameLi.Idents.Length = 1 then
                        ValueSome(Measure.Named nameLi)
                    else
                        ValueNone
                | _ -> ValueNone

            match measureFromTypeArg with
            | ValueSome m ->
                let mt = translateMeasure ctx diagKey m
                let tv = freshTyVar ctx
                tv.Link <- ValueSome(translateType ctx (Type.NamedType li))
                tv.Units <- ValueSome mt
                TyVar tv
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple [ for t in types -> translateType ctx t ]
        | _ ->
            // TODO: VarType (typars), GenericType (non-measure), etc. Free
            // variable until we model them properly — unification will pin
            // it via context.
            TyVar(freshTyVar ctx)

    /// Measure carried on `t`'s union-find root, if any. Reads `Units`
    /// straight off the root — does NOT use `resolveStep`, since that
    /// would follow a measured TyVar through its `Link` to the bare
    /// carrier and drop the measure. Returns ValueNone for plain
    /// `TyConst` (dimensionless), function types, tuples, and free
    /// variables.
    let private unitsOf (t: SemType) : MeasureTerm voption =
        match t with
        | TyVar tv -> (UnionFind.find tv).Units
        | _ -> ValueNone

    /// Underlying numeric carrier of a (possibly measure-wrapped) type.
    /// For a `TyVar` whose Link points at `TyConst "float"`, returns
    /// `TyConst "float"`. For a plain TyConst, returns itself. For a free
    /// variable (no Link), returns the variable so a later unification can
    /// pin it.
    let private carrierOf (t: SemType) : SemType =
        match resolveStep t with
        | TyVar tv ->
            let root = UnionFind.find tv

            match root.Link with
            | ValueSome link -> link
            | ValueNone -> TyVar root
        | other -> other

    /// Allocate a fresh TyVar at the current level pre-stamped with a
    /// carrier link and (optionally) a measure. The dispatcher uses this
    /// for the result type of a measured arithmetic operation.
    let private freshTyVarWith (ctx: PassContext) (carrier: SemType) (units: MeasureTerm voption) : TypeVar =
        let tv = freshTyVar ctx
        tv.Link <- ValueSome carrier
        tv.Units <- units
        tv

    /// Compiled names for comparison operators that return `bool` regardless
    /// of operand measure (provided the measures match).
    let private isComparisonOp (name: string) : bool =
        match name with
        | "op_Equality"
        | "op_Inequality"
        | "op_LessThan"
        | "op_GreaterThan"
        | "op_LessThanOrEqual"
        | "op_GreaterThanOrEqual" -> true
        | _ -> false

    /// Measure-aware arithmetic dispatcher. Fires before the provider
    /// lookup in `inferInfix` so measured `+`, `-`, `*`, `/`, and comparison
    /// operators get measure-correct result types and surface a dedicated
    /// "Measure mismatch" diagnostic rather than a generic carrier-type
    /// mismatch. Returns `None` for the all-dimensionless case (or for
    /// operators we don't dispatch); the caller falls through to the
    /// existing provider path.
    let private tryMeasuredArith
        (ctx: PassContext)
        (key: NodeKey)
        (name: string)
        (leftTy: SemType)
        (rightTy: SemType)
        : SemType option =
        let leftUnits = unitsOf leftTy
        let rightUnits = unitsOf rightTy

        match leftUnits, rightUnits with
        | ValueNone, ValueNone -> None
        | _ ->
            let carrier = carrierOf leftTy
            // Carriers must agree even between measured operands (no
            // `float<m> + int<m>`). Surface that as a normal type mismatch.
            unify ctx key carrier (carrierOf rightTy)

            match name, leftUnits, rightUnits with
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 when m1.Equals m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m1, ValueSome m2 ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Severity = Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m1)))
            | ("op_Addition" | "op_Subtraction"), ValueSome m, ValueNone
            | ("op_Addition" | "op_Subtraction"), ValueNone, ValueSome m ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Severity = Error
                    }

                Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Multiply", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.mul m1 m2))))
            | "op_Multiply", ValueSome m, ValueNone
            | "op_Multiply", ValueNone, ValueSome m -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueSome m1, ValueSome m2 ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.div m1 m2))))
            | "op_Division", ValueSome m, ValueNone -> Some(TyVar(freshTyVarWith ctx carrier (ValueSome m)))
            | "op_Division", ValueNone, ValueSome m ->
                Some(TyVar(freshTyVarWith ctx carrier (ValueSome(MeasureTerm.inv m))))
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name && m1.Equals m2 -> Some MockBuiltins.tyBool
            | name, ValueSome m1, ValueSome m2 when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: <%O> vs <%O>" m1 m2
                        Severity = Error
                    }

                Some MockBuiltins.tyBool
            | name, ValueSome m, ValueNone
            | name, ValueNone, ValueSome m when isComparisonOp name ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Measure mismatch: dimensionless vs <%O>" m
                        Severity = Error
                    }

                Some MockBuiltins.tyBool
            | _ -> None

    /// Field-initializer / field-pattern long-ident inspection. v1 only
    /// supports single-segment (`X`) and two-segment qualified (`R.X`)
    /// forms. Multi-segment qualifiers (`A.B.X`) fall through as ValueNone
    /// for the qualifier and the last segment for the field name.
    let private fieldNameAndQualifier (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string voption * string =
        let idents = li.Idents
        let last = ctx.NameOf idents.[idents.Length - 1]

        if idents.Length = 1 then
            ValueNone, last
        elif idents.Length = 2 then
            ValueSome(ctx.NameOf idents.[0]), last
        else
            ValueNone, last

    /// Try to find the unique record type whose declared field set equals
    /// `names` (order-insensitive, duplicates rejected). Returns ValueNone
    /// when zero matches or multiple match — the caller emits the
    /// appropriate diagnostic.
    let private findUniqueRecordByFieldSet (ctx: PassContext) (names: string list) : RecordTypeInfo voption * int =
        // Returns (info, candidateCount). candidateCount disambiguates the
        // "no match" vs "ambiguous" diagnostic paths.
        match names with
        | [] -> ValueNone, 0
        | first :: _ ->
            match ctx.FieldIndex.TryGetValue first with
            | false, _ -> ValueNone, 0
            | true, candidates ->
                let nameSet = Set.ofList names

                let matches =
                    candidates
                    |> List.filter (fun info ->
                        let declared = info.Fields |> Array.map (fun f -> f.Name) |> Set.ofArray
                        declared = nameSet
                    )

                match matches with
                | [ info ] -> ValueSome info, 1
                | [] -> ValueNone, 0
                | many -> ValueNone, List.length many

    let rec private inferPat (ctx: PassContext) (p: Pat<SyntaxToken>) : SemType =
        // Each pattern node gets its own TypeVar keyed on its NodeKey. For
        // compound patterns (Tuple, EnclosedBlock, As) the outer TypeVar is
        // linked to the underlying shape so a single lookup against any
        // pattern node returns the right type.
        let key = CstKeys.ofPat p

        match p with
        | Pat.NamedSimple _ ->
            // Use tvOf so a let-rec sibling whose TyVar was already lazy-minted
            // by a forward reference (or pre-allocated by inferBindingGroup)
            // is reused, not overwritten.
            TyVar(tvOf ctx key)
        | Pat.Wildcard _ -> TyVar(freshTv ctx key)
        | Pat.EnclosedBlock(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Tuple(patterns = pats) ->
            let elemTys = [ for p in pats -> inferPat ctx p ]
            let tupleTy = TyTuple elemTys
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome tupleTy
            tupleTy
        | Pat.Const c ->
            let constTy = inferConst ctx c
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome constTy
            constTy
        | Pat.As(pat = inner) ->
            let innerTy = inferPat ctx inner
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome innerTy
            innerTy
        | Pat.Typed(pat = inner; typ = t) ->
            let innerTy = inferPat ctx inner
            let annTy = translateType ctx t
            unify ctx key innerTy annTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome annTy
            annTy
        | Pat.EmptyBlock _ ->
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome MockBuiltins.tyUnit
            MockBuiltins.tyUnit
        | Pat.Or(left = leftPat; right = rightPat) ->
            // F# requires both sides to bind the same set of names with
            // matching types. Validation will check the name set; here we
            // unify the patterns' overall types so the scrutinee constraint
            // is consistent.
            let leftTy = inferPat ctx leftPat
            let rightTy = inferPat ctx rightPat
            unify ctx key leftTy rightTy
            let nodeTv = freshTv ctx key
            nodeTv.Link <- ValueSome leftTy
            leftTy
        | Pat.Record(fieldPats = fieldPats) ->
            let pairs =
                [
                    for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                        let q, n = fieldNameAndQualifier ctx li
                        q, n, sub
                ]

            let qualifier =
                pairs
                |> List.tryPick (fun (q, _, _) ->
                    match q with
                    | ValueSome q -> Some q
                    | _ -> None
                )

            let names = pairs |> List.map (fun (_, n, _) -> n)

            let candidate =
                match qualifier with
                | Some typeName ->
                    match ctx.RecordTypes.TryGetValue typeName with
                    | true, info -> ValueSome info
                    | false, _ ->
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "Unknown record type qualifier: %s" typeName
                                Severity = Error
                            }

                        ValueNone
                | None ->
                    let cand, count = findUniqueRecordByFieldSet ctx names

                    match cand with
                    | ValueSome _ -> cand
                    | ValueNone ->
                        if count = 0 then
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                    Severity = Error
                                }
                        else
                            ctx.Diagnostics.Add
                                {
                                    Key = key
                                    Message =
                                        sprintf
                                            "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                            count
                                    Severity = Error
                                }

                        ValueNone

            match candidate with
            | ValueNone ->
                // Still walk sub-patterns so binders are inferred — they
                // attach as free TyVars without unification noise.
                for _, _, sub in pairs do
                    inferPat ctx sub |> ignore

                let nodeTv = freshTv ctx key
                TyVar nodeTv
            | ValueSome info ->
                for _, fieldName, sub in pairs do
                    let subTy = inferPat ctx sub

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofPat sub) subTy field.Type
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofPat sub
                                Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                                Severity = Error
                            }

                let recTy = TyRecord info.Name
                let nodeTv = freshTv ctx key
                nodeTv.Link <- ValueSome recTy
                recTy
        | _ ->
            // TODO: Named (DU ctor) / Cons patterns — they need
            // provider lookups or recursive shape unification.
            TyVar(freshTv ctx key)

    let rec private infer (ctx: PassContext) (e: Expr<SyntaxToken>) : SemType =
        let key = CstKeys.ofExpr e
        let nodeTv = freshTv ctx key

        let inferredTy =
            match e with
            | Expr.Const c -> inferConst ctx c
            | Expr.Ident _ -> inferIdent ctx e key
            | Expr.LongIdentOrOp _ -> inferIdent ctx e key
            | Expr.App(fn, args) -> inferApp ctx key fn args
            | Expr.HighPrecedenceApp(funcExpr = fn; argExpr = arg) -> inferHighPrecApp ctx key fn arg
            | Expr.InfixApp(left, _, right) -> inferInfix ctx key left right
            | Expr.PrefixApp(_, operand) -> inferPrefix ctx key operand
            | Expr.Fun(argumentPats = argPats; expr = body) -> inferFun ctx argPats body
            | Expr.LetOrUse(bindings = bindings; body = body) -> inferLet ctx key bindings body
            | Expr.EnclosedBlock(expr = inner) -> infer ctx inner
            | Expr.IfThenElse(condition = cond; thenExpr = thenE; elifBranches = elifs; elseBranch = elseB) ->
                inferIfThenElse ctx key cond thenE elifs elseB
            | Expr.Tuple(exprs = items) -> inferTuple ctx items
            | Expr.Sequential(exprs = items) -> inferSequential ctx key items
            | Expr.TypeAnnotation(expr = inner; typ = t) -> inferTypeAnnotation ctx key inner t
            | Expr.EmptyBlock _ -> MockBuiltins.tyUnit
            | Expr.While(condition = cond; body = body) -> inferWhile ctx key cond body
            | Expr.ForTo(ident = ident; startExpr = startE; endExpr = endE; body = body) ->
                inferForTo ctx key ident startE endE body
            | Expr.ForIn(pat = pat; enumerableExpr = src; body = body) -> inferForIn ctx key pat src body
            | Expr.String(parts = parts) -> inferString ctx key parts
            | Expr.Match(matchExpr = scrutinee; rules = Rules(rules = rules)) -> inferMatch ctx key scrutinee rules
            | Expr.Function(rules = Rules(rules = rules)) -> inferFunction ctx key rules
            | Expr.TryWith(expr = body; rules = Rules(rules = rules)) -> inferTryWith ctx key body rules
            | Expr.TryFinally(tryExpr = body; finallyExpr = finallyE) -> inferTryFinally ctx key body finallyE
            | Expr.Assignment(leftExpr = left; rightExpr = right) -> inferAssignment ctx key left right
            | Expr.Range(fromExpr = a; toExpr = b) -> inferRange ctx key a ValueNone b
            | Expr.SteppedRange(fromExpr = a; stepExpr = s; toExpr = b) -> inferRange ctx key a (ValueSome s) b
            | Expr.Null _ ->
                // `null` lacks a constraint in the tiny subset (no
                // reference-type bound yet). Hand back a free TypeVar so
                // surrounding context can pin it.
                TyVar(freshTyVar ctx)
            | Expr.Record(fieldInitializers = inits) -> inferRecord ctx key inits
            | Expr.RecordClone(expr = src; fieldInitializers = inits) -> inferRecordClone ctx key src inits
            | Expr.DotLookup(expr = r; longIdentOrOp = LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                inferFieldAccess ctx key r li.Idents.[0]
            | _ ->
                // TODO: other expression kinds.
                TyVar(freshTyVar ctx)

        nodeTv.Link <- ValueSome inferredTy
        inferredTy

    and private inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        // First: a multi-segment LongIdent whose head segment resolved as
        // a local binding is a record-field access chain (`r.X`, `r.X.Y`),
        // not a qualified name. The parser doesn't emit `Expr.DotLookup`
        // for these — they ride inside a single `Expr.LongIdentOrOp`.
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx key li
        | _ ->
            match ctx.Binding.TryGetValue key with
            | ValueSome rb ->
                // Local binding — the BindingSite is the headPat NodeKey. If the
                // binding has been generalised already, instantiate the scheme so
                // independent use-sites get independent variables (mirrors the
                // external-symbol path). Otherwise fall back to the monomorphic
                // TyVar minted by inferPat — this includes uses inside a sibling's
                // RHS within the same `let rec` group, which is exactly what
                // forbids polymorphic recursion.
                match ctx.Scheme.TryGetValue rb.BindingSite with
                | ValueSome scheme -> instantiate ctx scheme
                | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
            | ValueNone ->
                // External symbol (or unresolved — NameRes will already have
                // emitted a diagnostic in that case). Re-query the provider.
                let name = qualifiedNameOf ctx e

                match ctx.Provider.TryLookup name with
                | ValueSome sym ->
                    // Polymorphic external symbols allocate fresh TypeVars per
                    // call; we pass the current level so those vars are stamped
                    // at the use-site's depth.
                    sym.Instantiate ctx.CurrentLevel
                | ValueNone -> TyVar(freshTyVar ctx)

    /// Source-level rendering of an ident/qualified-name expression. For
    /// single-segment idents this is just the token text; for multi-segment
    /// `LongIdent.LongIdent` it joins segments with `.` so the provider can
    /// look up dotted names like `Math.PI` directly.
    and private qualifiedNameOf (ctx: PassContext) (e: Expr<SyntaxToken>) : string =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
        | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

    and private inferApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        : SemType =
        let mutable currTy = infer ctx fn

        for a in args do
            let argTy = infer ctx a
            let resultTy = TyVar(freshTyVar ctx)
            unify ctx key currTy (TyFun(argTy, resultTy))
            currTy <- resultTy

        currTy

    and private inferHighPrecApp
        (ctx: PassContext)
        (key: NodeKey)
        (fn: Expr<SyntaxToken>)
        (arg: Expr<SyntaxToken>)
        : SemType =
        // `f(x)` — high-precedence one-arg application. Same shape as
        // `Expr.App fn [|arg|]`, just a separate CST case for the parser.
        let fnTy = infer ctx fn
        let argTy = infer ctx arg
        let resultTy = TyVar(freshTyVar ctx)
        unify ctx key fnTy (TyFun(argTy, resultTy))
        resultTy

    and private inferRange
        (ctx: PassContext)
        (key: NodeKey)
        (fromE: Expr<SyntaxToken>)
        (stepE: Expr<SyntaxToken> voption)
        (toE: Expr<SyntaxToken>)
        : SemType =
        // `a..b` / `a..step..b` — endpoints (and step) constrained to int
        // in the tiny subset; result is the `seq<int>` placeholder. Real F#
        // is generic over any type with the `..` operator overload.
        let fromTy = infer ctx fromE
        unify ctx key fromTy MockBuiltins.tyInt

        match stepE with
        | ValueSome s ->
            let stepTy = infer ctx s
            unify ctx key stepTy MockBuiltins.tyInt
        | ValueNone -> ()

        let toTy = infer ctx toE
        unify ctx key toTy MockBuiltins.tyInt
        MockBuiltins.tySeqInt

    and private inferInfix
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        let leftTy = infer ctx left
        let rightTy = infer ctx right

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match tryMeasuredArith ctx key name leftTy rightTy with
            | Some resultTy -> resultTy
            | None ->
                match ctx.Provider.TryLookup name with
                | ValueSome sym ->
                    let resultTy = TyVar(freshTyVar ctx)
                    unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(leftTy, TyFun(rightTy, resultTy)))
                    resultTy
                | ValueNone ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Unknown operator symbol: %s" name
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
        | ValueNone ->
            // Desugar didn't recognise the operator token; leave the result
            // as a free TypeVar (the unknown operator is a deficiency in
            // Desugar's lookup table, not a user error here).
            TyVar(freshTyVar ctx)

    and private inferPrefix (ctx: PassContext) (key: NodeKey) (operand: Expr<SyntaxToken>) : SemType =
        let operandTy = infer ctx operand

        match ctx.Desugared.TryGetValue key with
        | ValueSome(DesugaredForm.OpName name) ->
            match ctx.Provider.TryLookup name with
            | ValueSome sym ->
                let resultTy = TyVar(freshTyVar ctx)
                unify ctx key (sym.Instantiate ctx.CurrentLevel) (TyFun(operandTy, resultTy))
                resultTy
            | ValueNone ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown prefix operator: %s" name
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | ValueNone -> TyVar(freshTyVar ctx)

    and private inferIfThenElse
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (thenE: Expr<SyntaxToken>)
        (elifs: ImmutableArray<ElifBranch<SyntaxToken>>)
        (elseB: ElseBranch<SyntaxToken> voption)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy MockBuiltins.tyBool

        let thenTy = infer ctx thenE

        for elif_ in elifs do
            let elifCond, elifExpr =
                match elif_ with
                | ElifBranch.Elif(condition = c; expr = e)
                | ElifBranch.ElseIf(condition = c; expr = e) -> c, e

            let elifCondTy = infer ctx elifCond
            unify ctx key elifCondTy MockBuiltins.tyBool
            let elifTy = infer ctx elifExpr
            unify ctx key thenTy elifTy

        match elseB with
        | ValueSome(ElseBranch(expr = elseExpr)) ->
            let elseTy = infer ctx elseExpr
            unify ctx key thenTy elseTy
            thenTy
        | ValueNone ->
            // `if c then e` (no else) requires e : unit. Tiny subset
            // doesn't have unit yet — surface as a diagnostic.
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "if-then without else not yet supported"
                    Severity = Error
                }

            thenTy

    and private inferFun
        (ctx: PassContext)
        (argPats: ImmutableArray<Pat<SyntaxToken>>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let argTypes = [ for p in argPats -> inferPat ctx p ]
        let bodyTy = infer ctx body
        List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

    and private inferTuple (ctx: PassContext) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        TyTuple [ for e in items -> infer ctx e ]

    and private inferSequential (ctx: PassContext) (key: NodeKey) (items: ImmutableArray<Expr<SyntaxToken>>) : SemType =
        // `e1; e2; …; en` — all but the last must be unit, result is the
        // last's type. A `Sequential` with fewer than two items shouldn't
        // come from the parser, but if it does, fall through harmlessly.
        if items.Length = 0 then
            MockBuiltins.tyUnit
        else
            for i = 0 to items.Length - 2 do
                let ty = infer ctx items.[i]
                unify ctx key ty MockBuiltins.tyUnit

            infer ctx items.[items.Length - 1]

    and private inferWhile
        (ctx: PassContext)
        (key: NodeKey)
        (cond: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let condTy = infer ctx cond
        unify ctx key condTy MockBuiltins.tyBool
        let bodyTy = infer ctx body
        unify ctx key bodyTy MockBuiltins.tyUnit
        MockBuiltins.tyUnit

    and private inferForTo
        (ctx: PassContext)
        (key: NodeKey)
        (ident: SyntaxToken)
        (startE: Expr<SyntaxToken>)
        (endE: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        let startTy = infer ctx startE
        unify ctx key startTy MockBuiltins.tyInt
        let endTy = infer ctx endE
        unify ctx key endTy MockBuiltins.tyInt
        // Bind the loop variable as int via a TypeVar keyed on its NodeKey.
        let varKey = CstKeys.ofForToVar ident
        let varTv = freshTv ctx varKey
        varTv.Link <- ValueSome MockBuiltins.tyInt
        let bodyTy = infer ctx body
        unify ctx key bodyTy MockBuiltins.tyUnit
        MockBuiltins.tyUnit

    and private inferForIn
        (ctx: PassContext)
        (key: NodeKey)
        (pat: Pat<SyntaxToken>)
        (src: Expr<SyntaxToken>)
        (body: Expr<SyntaxToken>)
        : SemType =
        // Special-cased: when the source is an int range we know the
        // element type and can bind the pattern to int. For everything
        // else there's no `seq<T>` machinery yet, so the pattern stays
        // unconstrained and we emit an Info to flag the gap.
        let srcTy = infer ctx src
        let patTy = inferPat ctx pat

        let isRangeSource =
            match src with
            | Expr.Range _
            | Expr.SteppedRange _ -> true
            | Expr.EnclosedBlock(expr = Expr.Range _)
            | Expr.EnclosedBlock(expr = Expr.SteppedRange _) -> true
            | _ -> false

        if isRangeSource then
            unify ctx key srcTy MockBuiltins.tySeqInt
            unify ctx key patTy MockBuiltins.tyInt
        else
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "for-in: enumerable / element-type checking not yet implemented"
                    Severity = Info
                }

        let bodyTy = infer ctx body
        unify ctx key bodyTy MockBuiltins.tyUnit
        MockBuiltins.tyUnit

    and private inferRules
        (ctx: PassContext)
        (key: NodeKey)
        (scrutineeTy: SemType)
        (resultTy: SemType)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : unit =
        for r in rules do
            match r with
            | Rule.Rule(pat = pat; guard = guard; expr = body) ->
                let patTy = inferPat ctx pat
                unify ctx key patTy scrutineeTy

                match guard with
                | ValueSome(PatternGuard(expr = g)) ->
                    let gTy = infer ctx g
                    unify ctx key gTy MockBuiltins.tyBool
                | ValueNone -> ()

                let bodyTy = infer ctx body
                unify ctx key bodyTy resultTy
            | _ -> ()

    and private inferMatch
        (ctx: PassContext)
        (key: NodeKey)
        (scrutinee: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        let scrutineeTy = infer ctx scrutinee
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key scrutineeTy resultTy rules
        resultTy

    and private inferFunction (ctx: PassContext) (key: NodeKey) (rules: ImmutableArray<Rule<SyntaxToken>>) : SemType =
        // `function p1 -> e1 | p2 -> e2` ~ `fun x -> match x with p1 -> e1 | p2 -> e2`.
        // The synthesised parameter's TypeVar IS the scrutinee's TypeVar —
        // every arm's pattern unifies with it.
        let paramTy = TyVar(freshTyVar ctx)
        let resultTy = TyVar(freshTyVar ctx)
        inferRules ctx key paramTy resultTy rules
        TyFun(paramTy, resultTy)

    and private inferTryWith
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (rules: ImmutableArray<Rule<SyntaxToken>>)
        : SemType =
        // `try body with | pat -> arm`: body and every arm share the result
        // type. The patterns are matched against an exception value — until
        // a real `exn` type is modelled, leave them as fresh TypeVars and
        // only unify the result side. Arm patterns are still inferred so
        // any names they bind have a stable TypeVar.
        let resultTy = infer ctx body
        let exnTy = TyVar(freshTyVar ctx)
        inferRules ctx key exnTy resultTy rules
        resultTy

    and private inferTryFinally
        (ctx: PassContext)
        (key: NodeKey)
        (body: Expr<SyntaxToken>)
        (finallyE: Expr<SyntaxToken>)
        : SemType =
        // `try body finally cleanup` — body's type is the result; cleanup
        // must be unit.
        let resultTy = infer ctx body
        let finallyTy = infer ctx finallyE
        unify ctx key finallyTy MockBuiltins.tyUnit
        resultTy

    and private inferAssignment
        (ctx: PassContext)
        (key: NodeKey)
        (left: Expr<SyntaxToken>)
        (right: Expr<SyntaxToken>)
        : SemType =
        // `lhs <- rhs` — the assignment expression has type unit. The LHS
        // and RHS must agree in type. (Mutability of the LHS binding is a
        // Validation concern; here we only typecheck.)
        let leftTy = infer ctx left
        let rightTy = infer ctx right
        unify ctx key leftTy rightTy
        MockBuiltins.tyUnit

    and private inferRecord
        (ctx: PassContext)
        (key: NodeKey)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let pairs =
            [
                for FieldInitializer(longIdent = li; expr = e) in inits ->
                    let q, n = fieldNameAndQualifier ctx li
                    q, n, e
            ]

        let qualifier =
            pairs
            |> List.tryPick (fun (q, _, _) ->
                match q with
                | ValueSome q -> Some q
                | _ -> None
            )

        let names = pairs |> List.map (fun (_, n, _) -> n)

        let candidate =
            match qualifier with
            | Some typeName ->
                match ctx.RecordTypes.TryGetValue typeName with
                | true, info -> ValueSome info
                | false, _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = key
                            Message = sprintf "Unknown record type qualifier: %s" typeName
                            Severity = Error
                        }

                    ValueNone
            | None ->
                let cand, count = findUniqueRecordByFieldSet ctx names

                match cand with
                | ValueSome _ -> cand
                | ValueNone ->
                    if count = 0 then
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message = sprintf "No record type matches the field set: %s" (String.concat ", " names)
                                Severity = Error
                            }
                    else
                        ctx.Diagnostics.Add
                            {
                                Key = key
                                Message =
                                    sprintf
                                        "Field set is ambiguous (%d candidate record types); add a qualifier or annotation"
                                        count
                                Severity = Error
                            }

                    ValueNone

        match candidate with
        | ValueNone ->
            for _, _, e in pairs do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)
        | ValueSome info ->
            for _, fieldName, e in pairs do
                let eTy = infer ctx e

                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                | Some field -> unify ctx (CstKeys.ofExpr e) eTy field.Type
                | None ->
                    ctx.Diagnostics.Add
                        {
                            Key = CstKeys.ofExpr e
                            Message = sprintf "Type '%s' has no field '%s'" info.Name fieldName
                            Severity = Error
                        }

            TyRecord info.Name

    and private inferRecordClone
        (ctx: PassContext)
        (key: NodeKey)
        (src: Expr<SyntaxToken>)
        (inits: ImmutableArray<FieldInitializer<SyntaxToken>>)
        : SemType =
        let srcTy = infer ctx src

        match resolveStep srcTy with
        | TyRecord recName ->
            match ctx.RecordTypes.TryGetValue recName with
            | true, info ->
                for FieldInitializer(longIdent = li; expr = e) in inits do
                    let _, fieldName = fieldNameAndQualifier ctx li
                    let eTy = infer ctx e

                    match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                    | Some field -> unify ctx (CstKeys.ofExpr e) eTy field.Type
                    | None ->
                        ctx.Diagnostics.Add
                            {
                                Key = CstKeys.ofExpr e
                                Message = sprintf "Type '%s' has no field '%s'" recName fieldName
                                Severity = Error
                            }

                TyRecord recName
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = key
                        Message = sprintf "Unknown record type '%s'" recName
                        Severity = Error
                    }

                for FieldInitializer(expr = e) in inits do
                    infer ctx e |> ignore

                TyRecord recName
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = key
                    Message = "Record clone requires the source expression to be a record"
                    Severity = Error
                }

            for FieldInitializer(expr = e) in inits do
                infer ctx e |> ignore

            TyVar(freshTyVar ctx)

    /// One step of field resolution: given the receiver's resolved type
    /// and the access expression's diagnostic NodeKey, produce the
    /// access's type. Deferred when the receiver is a free TyVar.
    and private resolveFieldStep (ctx: PassContext) (diagKey: NodeKey) (rTy: SemType) (fieldName: string) : SemType =
        match resolveStep rTy with
        | TyRecord recName ->
            match ctx.RecordTypes.TryGetValue recName with
            | true, info ->
                match info.Fields |> Array.tryFind (fun f -> f.Name = fieldName) with
                | Some field -> field.Type
                | None ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message = sprintf "Type '%s' has no field '%s'" recName fieldName
                            Severity = Error
                        }

                    TyVar(freshTyVar ctx)
            | false, _ ->
                ctx.Diagnostics.Add
                    {
                        Key = diagKey
                        Message = sprintf "Unknown record type '%s'" recName
                        Severity = Error
                    }

                TyVar(freshTyVar ctx)
        | TyVar tv ->
            let root = UnionFind.find tv
            let resultTv = freshTyVar ctx
            root.PendingFieldAccess <- (fieldName, diagKey, resultTv) :: root.PendingFieldAccess
            TyVar resultTv
        | _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = sprintf "Cannot read field '%s' from non-record type" fieldName
                    Severity = Error
                }

            TyVar(freshTyVar ctx)

    and private inferFieldAccess
        (ctx: PassContext)
        (key: NodeKey)
        (receiver: Expr<SyntaxToken>)
        (fieldTok: SyntaxToken)
        : SemType =
        let fieldName = ctx.NameOf fieldTok
        let rTy = infer ctx receiver
        resolveFieldStep ctx key rTy fieldName

    /// `r.X.Y…` parsed as a single multi-segment `Expr.LongIdentOrOp`.
    /// The head segment was resolved by NameResolution as a local binding
    /// — type it through `ctx.Binding`/`ctx.Scheme` (same path as
    /// `inferIdent` for a single-segment ident), then walk the remaining
    /// segments as a field-access chain.
    and private inferLongIdentFieldChain (ctx: PassContext) (key: NodeKey) (li: LongIdent<SyntaxToken>) : SemType =
        let head = li.Idents.[0]
        let headKey = NodeKey.ofToken head NodeKind.ExprIdent

        let headTy =
            match ctx.Binding.TryGetValue headKey with
            | ValueSome rb ->
                match ctx.Scheme.TryGetValue rb.BindingSite with
                | ValueSome scheme -> instantiate ctx scheme
                | ValueNone -> TyVar(tvOf ctx rb.BindingSite)
            | ValueNone -> TyVar(freshTyVar ctx)

        let mutable currTy = headTy

        for i = 1 to li.Idents.Length - 1 do
            let seg = li.Idents.[i]
            let segName = ctx.NameOf seg
            // Diagnose against the LongIdent's overall key — there's no
            // separate sub-expression NodeKey for an intermediate segment.
            currTy <- resolveFieldStep ctx key currTy segName

        currTy

    and private inferString
        (ctx: PassContext)
        (_key: NodeKey)
        (parts: ImmutableArray<StringPart<SyntaxToken>>)
        : SemType =
        // Non-interpolated strings type as `string`. Interpolated strings
        // also type as string (the holes are typed via printf-format
        // checking, which we don't model yet — recurse into the hole exprs
        // so their types still get computed, but don't constrain them).
        for part in parts do
            match part with
            | StringPart.Expr(expr = e) -> infer ctx e |> ignore
            | _ -> ()

        MockBuiltins.tyString

    and private inferTypeAnnotation
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let innerTy = infer ctx inner
        let annTy = translateType ctx t
        unify ctx key innerTy annTy
        annTy

    and private inferLet
        (ctx: PassContext)
        (key: NodeKey)
        (bindings: ImmutableArray<Binding<SyntaxToken>>)
        (body: Expr<SyntaxToken> voption)
        : SemType =
        inferBindingGroup ctx bindings

        match body with
        | ValueSome bodyExpr -> infer ctx bodyExpr
        | ValueNone ->
            // `Expr.LetOrUse(body = ValueNone)` is `use fixed` (module-level
            // lets are ModuleElems, not Expr.LetOrUse). Tiny subset doesn't
            // support pinning — surface loudly so Freeze doesn't see a
            // best-effort type for an unsupported construct.
            failwith "Unification: Expr.LetOrUse with no body (UseFixed) not supported"

    and private inferBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        let patTy = inferPat ctx b.headPat

        let rhsTy =
            if b.argumentPats.IsEmpty then
                infer ctx b.expr
            else
                // `let f x y = body` is `let f = fun x y -> body`.
                let argTypes = [ for p in b.argumentPats -> inferPat ctx p ]
                let bodyTy = infer ctx b.expr
                List.foldBack (fun a r -> TyFun(a, r)) argTypes bodyTy

        unify ctx (CstKeys.ofBinding b) patTy rhsTy

    /// Type a `let` / `let rec` group with Rémy-level discipline:
    ///   1. Snapshot the outer level and push one level for the group.
    ///   2. Pre-allocate single-name sibling headPat TyVars so forward
    ///      references from inside one RHS (or any nested let within) find
    ///      the sibling's TyVar at this group's level rather than lazy-mint
    ///      at a deeper one — which would let a nested let generalise a
    ///      var that actually belongs to an un-typed outer sibling.
    ///   3. Type every binding's RHS at the pushed level (sibling lookups
    ///      stay monomorphic — no scheme is written until step 5).
    ///   4. Pop back to the outer level.
    ///   5. Generalise each `shouldGeneralise` binding against the outer
    ///      level and write its scheme to `ctx.Scheme`.
    and private inferBindingGroup (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        let outerLevel = ctx.CurrentLevel
        enterLevel ctx

        for b in bindings do
            match b.headPat with
            | Pat.NamedSimple _ -> tvOf ctx (CstKeys.ofPat b.headPat) |> ignore
            | _ -> ()

        for b in bindings do
            inferBinding ctx b

        exitLevel ctx

        for b in bindings do
            if shouldGeneralise b then
                let key = CstKeys.ofPat b.headPat
                let headTv = tvOf ctx key
                let zonked = zonk (TyVar headTv)

                if not (hasPendingFieldAccess zonked) then
                    let scheme = generalise zonked outerLevel
                    ctx.Scheme.Set(key, scheme)

    let private walkModuleElem (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            inferBindingGroup ctx bindings
        | ModuleElem.Expression e -> infer ctx e |> ignore
        | _ -> ()

    /// After NameResolution stamps placeholder TyVars for every record
    /// field, walk the file's `TypeDefn.Record`s again and Link each
    /// placeholder to the real translated CST type. Done as a pre-pass so
    /// a record's field type can reference another record declared
    /// elsewhere in the same file — at this point every record name is
    /// already in `ctx.RecordTypes`, so `translateType`'s lookup succeeds.
    let private fillRecordFieldTypes (ctx: PassContext) (m: ModuleElem<SyntaxToken>) =
        match m with
        | ModuleElem.Type defs ->
            for td in defs do
                match td with
                | TypeDefn.Record(typeName = TypeName(ident = nameLi); fields = fields) when nameLi.Idents.Length = 1 ->
                    let name = ctx.NameOf nameLi.Idents.[0]

                    match ctx.RecordTypes.TryGetValue name with
                    | true, info ->
                        let n = min info.Fields.Length fields.Length

                        for i = 0 to n - 1 do
                            let (RecordField(ident = id; typ = t)) = fields.[i]
                            let translated = translateType ctx t

                            match info.Fields.[i].Type with
                            | TyVar tv ->
                                let root = UnionFind.find tv
                                root.Link <- ValueSome translated
                            | _ -> ()

                            ignore id
                    | false, _ -> ()
                | _ -> ()
        | _ -> ()

    let private walkElems (ctx: PassContext) (elems: ModuleElems<SyntaxToken>) =
        for m in elems do
            fillRecordFieldTypes ctx m

        for m in elems do
            walkModuleElem ctx m

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : unit =
        match file with
        | ImplementationFile.AnonymousModule elems -> walkElems ctx elems
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) -> walkElems ctx elems
        | ImplementationFile.Namespaces _ -> ()
