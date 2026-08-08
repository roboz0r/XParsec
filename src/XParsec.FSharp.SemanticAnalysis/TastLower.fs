namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Platform-neutral TAST lowering, shared by both codegen backends (CLR and JS), which
/// must not reference each other. Nothing here may traffic in more than node handles,
/// `FrozenType` and `NodeKey`.
module TastLower =

    type StaticParam = TastAccessor.StaticParam

    let private unexpanded (what: string) : 'a =
        failwithf
            "Emit: %s reached the emitter — the specialization table is expanded before emission, so this node should not exist here"
            what

    /// `TExpr.InlineCall` is an EDGE into the file's specialization table, so a backend that
    /// has not expanded the table has no body to emit.
    let inlineCallUnexpanded (spec: SpecializationId) : 'a =
        let (SpecializationId i) = spec
        unexpanded (sprintf "an InlineCall on specialization %d" i)

    /// `TExpr.CallerExpr` marks an anchor-domain boundary INSIDE a specialization entry, so
    /// it can only be reached through an edge the same expansion would have consumed.
    let callerExprUnexpanded () : 'a = unexpanded "a CallerExpr"

    /// The fault an emit router raises on a `TExpr.TraitCall`. An SRTP constraint has no
    /// compiled signature on ANY target, so this is not a per-backend gap: inline
    /// expansion grounds what it can, and `lower` drops the rest as template-only.
    let traitCallUnresolved (memberName: string) : 'a =
        failwithf
            "Emit: a TraitCall to '%s' reached the emitter — inline expansion grounds every trait call it can and reports the rest, so this node should not exist here"
            memberName

    let inline receiverShape (ty: FrozenType) : (TypeKey * FrozenType list) voption =
        match ty with
        | FTUnion(n, args)
        | FTRecord(n, args)
        | FTClass(n, args) -> ValueSome(n, EqArray.toList args)
        | _ -> ValueNone

    /// As `matchInstantiation`, but leaves a `ValueNone` hole for a typar no
    /// parameter/result mentions — a phantom constraint typar (`fold`'s enumerator `'E`)
    /// is unrecoverable by param-matching and must be solved from its bounds.
    let matchInstantiationPartial
        (typarCount: int)
        (defTys: FrozenType list)
        (actualTys: FrozenType list)
        : FrozenType voption[] =
        let result = Array.create typarCount ValueNone

        let rec go (defT: FrozenType) (actT: FrozenType) =
            match defT, actT with
            // `act` may itself be an `FTTypar(TyparAxis.Method, j)` — the enclosing
            // context's own typar.
            | FTTypar(TyparAxis.Method, i), act ->
                if i >= 0 && i < typarCount && result.[i].IsNone then
                    result.[i] <- ValueSome act
            | FTFun(a1, r1), FTFun(a2, r2) ->
                go a1 a2
                go r1 r2
            | FTTuple xs, FTTuple ys when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | FTRecord(_, xs), FTRecord(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | FTUnion(_, xs), FTUnion(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | FTClass(_, xs), FTClass(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            // A generic intrinsic (notably `'T[]`, whose element is its one arg) carries
            // its element structurally; recurse so the element typar is recovered.
            | FTConst(_, xs), FTConst(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | _ -> ()

        List.iter2 go defTys actualTys
        result

    /// Recover a generic static method's per-typar instantiation at a call site by
    /// structurally matching each declared parameter type against the actual argument
    /// type; first occurrence wins. Strict — an unrecovered typar throws.
    let matchInstantiation (typarCount: int) (defTys: FrozenType list) (actualTys: FrozenType list) : FrozenType list =
        let result = matchInstantiationPartial typarCount defTys actualTys

        [
            for i in 0 .. typarCount - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone -> failwithf "Emit: could not infer instantiation for static-method type parameter %d" i
        ]

    /// Fill `instArr`'s remaining holes from the CONSTRAINTS: for a `Coercion(ci, target)`
    /// whose `ci` is already solved, `tryWitness` reads that type's actual impl of
    /// `target`'s interface, and `target`'s typars are recovered from the witness.
    let solvePhantomTypars
        (typarCount: int)
        (constraints: FrozenConstraint list)
        (tryWitness: FrozenType -> TypeKey -> EqArray<FrozenType> voption)
        (instArr: FrozenType voption[])
        : unit =
        if not (List.isEmpty constraints) then
            // A witness OVERRIDES an already-recovered value at these indices: a typar in
            // both a bound and the result can be stale in the result occurrence, while the
            // witness reads it off the concrete impl.
            let boundMentioned = System.Collections.Generic.HashSet<int>()

            let rec mention (t: FrozenType) =
                match t with
                | FTTypar(TyparAxis.Method, i) -> boundMentioned.Add i |> ignore
                | t -> FrozenType.iterChildren mention t

            for c in constraints do
                match c with
                | FrozenConstraint.Coercion(_, target) -> mention target

            let mutable changed = true

            while changed do
                changed <- false

                for c in constraints do
                    match c with
                    | FrozenConstraint.Coercion(ci, target) ->
                        if ci >= 0 && ci < instArr.Length then
                            match instArr.[ci], target with
                            | ValueSome receiver, FTClass(ifaceKey, _) ->
                                match tryWitness receiver ifaceKey with
                                | ValueSome witnessArgs ->
                                    let holes =
                                        matchInstantiationPartial
                                            typarCount
                                            [ target ]
                                            [ FTClass(ifaceKey, witnessArgs) ]

                                    for j in 0 .. instArr.Length - 1 do
                                        match holes.[j] with
                                        | ValueSome t when
                                            instArr.[j] <> ValueSome t
                                            && (instArr.[j] = ValueNone || boundMentioned.Contains j)
                                            ->
                                            instArr.[j] <- ValueSome t
                                            changed <- true
                                        | _ -> ()
                                | ValueNone -> ()
                            | _ -> ()

    type ArgGroup = TastAccessor.ArgGroup
    type ValRepr = TastAccessor.ValRepr
    type CompiledReturn = TastAccessor.CompiledReturn
    type CompiledForm = TastAccessor.CompiledForm

    let private isUnitFrozen (t: FrozenType) : bool =
        match t with
        | FTUnit -> true
        | _ -> false

    /// Peel up to `n` top-level `->` off a frozen type (all of them when `n < 0`),
    /// returning each as a `(domain, codomain)` pair in order.
    let rec peelFuns (n: int) (t: FrozenType) : (FrozenType * FrozenType) list =
        if n = 0 then
            []
        else
            match t with
            | FTFun(a, b) -> (a, b) :: peelFuns (if n < 0 then -1 else n - 1) b
            | _ -> []

    /// Rebuild a frozen type by transforming its top-level type-argument vector: every
    /// nominal-with-args shape plus the tuple's element vector. Anything else — including
    /// `FTFun`, whose domain/codomain are not an arg vector — passes through unchanged.
    let mapFrozenArgs (f: EqArray<FrozenType> -> EqArray<FrozenType>) (t: FrozenType) : FrozenType =
        match t with
        | FTClass(key, args) -> FTClass(key, f args)
        | FTRecord(key, args) -> FTRecord(key, f args)
        | FTUnion(key, args) -> FTUnion(key, f args)
        | FTConst(key, args) -> FTConst(key, f args)
        | FTTuple items -> FTTuple(f items)
        | _ -> t

    /// `peelFuns` projected to `(parameter types, residual result)`; the residual is the
    /// type after the last peeled `->`.
    let peelFunDomains (n: int) (t: FrozenType) : FrozenType list * FrozenType =
        match peelFuns n t with
        | [] -> [], t
        | levels -> List.map fst levels, snd (List.last levels)

    /// The lone-`()` group shape: the only arity that erases to zero compiled params
    /// (F#'s `[[]]` rule, read off the ARITY, not the type).
    let isLoneUnitGroup (groups: ArgGroup list) : bool =
        match groups with
        | [ ArgGroupG.GUnit _ ] -> true
        | _ -> false

    /// Every source group is a plain single bound variable — the shape whose flat params map
    /// one-to-one onto the source applications.
    let allSimpleGroups (groups: ArgGroup list) : bool =
        groups
        |> List.forall (
            function
            | ArgGroupG.GSimple _ -> true
            | _ -> false
        )

    /// Does a value-use of a function with these source groups need a curried
    /// adapter (its flat call shape differs from the curried one)? Only for arity ≥ 2
    /// or a tuple group; a single `GSimple` / lone `GUnit` is flat-==-curried.
    let needsCurryAdapter (groups: ArgGroup list) : bool =
        List.length groups >= 2
        || groups
           |> List.exists (
               function
               | ArgGroupG.GTuple _ -> true
               | _ -> false
           )

    /// The flatten rule over TYPES, given each group's parameter type: a lone `()` group
    /// erases to nothing, a tuple group expands to its `FTTuple` elements (one level
    /// only), every other group contributes its one type.
    let flattenGroupShape (groups: ArgGroup list) (groupParamTys: FrozenType list) : FrozenType list =
        if isLoneUnitGroup groups then
            []
        else
            List.zip groups groupParamTys
            |> List.collect (fun (g, pt) ->
                match g with
                | ArgGroupG.GUnit _
                | ArgGroupG.GSimple _ -> [ pt ]
                | ArgGroupG.GTuple _ ->
                    match pt with
                    | FTTuple xs -> EqArray.toList xs
                    | _ -> [ pt ]
            )

    /// Peel a curried `Lambda` chain into its source `ArgGroup`s and the residual body.
    /// Only the two node-handle READERS are supplied here; the grouping rule is shared.
    let peelValRepr (e: TastAccessor.ExprId) : ArgGroup list * TastAccessor.ExprId =
        let unLambda (e: TastAccessor.ExprId) =
            match e with
            | TastAccessor.ELambda lam -> ValueSome(struct (lam.Param, lam.Body))
            | _ -> ValueNone

        let facts (p: TastAccessor.PatId) : ArgGroups.ParamPatFacts<BoundVarId> =
            let shape = TastAccessor.patKind p

            {
                Shape = shape
                Ty = TastAccessor.patTy p
                BoundVar = TastAccessor.patBoundVar p
                ConstValue =
                    match shape with
                    | PatShape.Const -> ValueSome(TastAccessor.patConstValue p)
                    | _ -> ValueNone
            }

        ArgGroups.peel unLambda facts e

    /// `peelValRepr` projected to one flat parameter per SOURCE group. A unit group gets a
    /// PLACEHOLDER bound variable so a slot is still allocated for the unit value the caller
    /// pushes; both placeholders are minted into the lambda chain's own pool.
    let peelLambda (e: TastAccessor.ExprId) : StaticParam list * TastAccessor.ExprId =
        let groups, body = peelValRepr e

        let paramOf (g: ArgGroup) : StaticParam =
            match g with
            | ArgGroupG.GSimple(k, ty) -> { Slot = k; Ty = ty; Pat = None }
            | ArgGroupG.GUnit ty ->
                {
                    Slot = TastPoolBuilder.mintBoundVar e.Pool
                    Ty = ty
                    Pat = None
                }
            | ArgGroupG.GTuple pat ->
                {
                    Slot = TastPoolBuilder.mintBoundVar e.Pool
                    Ty = TastAccessor.patTy pat
                    Pat = Some pat
                }

        List.map paramOf groups, body

    /// Build the SOURCE `ValRepr` for a function value, returning the residual body the
    /// backend emits. `ResultTy` is that body's type, before unit→void normalisation.
    let valReprOf (typars: int) (e: TastAccessor.ExprId) : ValRepr * TastAccessor.ExprId =
        let groups, body = peelValRepr e

        {
            Typars = typars
            Groups = groups
            ResultTy = TastAccessor.exprTy body
        },
        body

    /// One level only: a NESTED tuple stays one param carrying its pattern.
    let private flattenTupleItem (p: TastAccessor.PatId) : StaticParam =
        let ty = TastAccessor.patTy p

        match TastAccessor.patKind p, TastAccessor.patBoundVar p with
        | PatShape.NamedSimple, ValueSome k -> { Slot = k; Ty = ty; Pat = None }
        | PatShape.Wildcard, _ ->
            {
                Slot = TastPoolBuilder.mintBoundVar p.Pool
                Ty = ty
                Pat = None
            }
        | _ ->
            {
                Slot = TastPoolBuilder.mintBoundVar p.Pool
                Ty = ty
                Pat = Some p
            }

    /// Derive the flat `CompiledForm` from a source `ValRepr`: tuple flattening one level;
    /// a LONE unit group erases to zero params (one among others stays a param); a unit
    /// result becomes `RVoid`. `pool` is where a `GUnit`'s placeholder slot is minted.
    let compiledOf (pool: PoolBuilder) (vr: ValRepr) : CompiledForm =
        let flattenGroup (g: ArgGroup) : StaticParam list =
            match g with
            | ArgGroupG.GUnit ty ->
                [
                    {
                        Slot = TastPoolBuilder.mintBoundVar pool
                        Ty = ty
                        Pat = None
                    }
                ]
            | ArgGroupG.GSimple(k, ty) -> [ { Slot = k; Ty = ty; Pat = None } ]
            | ArgGroupG.GTuple pat ->
                match TastAccessor.patKind pat with
                | PatShape.Tuple -> [ for it in TastAccessor.patChildren pat -> flattenTupleItem it ]
                | other -> failwithf "peelValRepr: GTuple must carry a tuple pattern, not %A" other

        let ps =
            if isLoneUnitGroup vr.Groups then
                []
            else
                vr.Groups |> List.collect flattenGroup

        {
            Params = ps
            Return =
                (if isUnitFrozen vr.ResultTy then
                     CompiledReturnG.RVoid
                 else
                     CompiledReturnG.RValue vr.ResultTy)
        }

    /// Build the SOURCE `ValRepr` for an EXTERNAL (contract-extracted) function, which has
    /// no lambda tree to peel: each `(arity, paramTy)` pair reconstructs a group, arity
    /// being the `*`-separated width within it. A contract's parameter names are dropped.
    let externalValRepr (typars: int) (groups: (int * FrozenType) list) (resultTy: FrozenType) : ValRepr =
        // A contract-minted pattern belongs to no file's tree, so it indexes into no
        // file's pool — it gets a standalone one, owned by this `ValRepr` and reachable
        // only through the handles it hands out. No token spells any of them.
        let contractPats = TastPoolBuilder.openEmpty ()

        let groupOf (arity: int, pty: FrozenType) : ArgGroup =
            if arity >= 2 then
                match pty with
                | FTTuple elems ->
                    let items =
                        elems
                        |> EqArray.toArray
                        |> Array.map (fun e -> TastAccessor.mintWildcardPat contractPats e Anchor.nowhere)

                    ArgGroupG.GTuple(TastAccessor.mintTuplePat contractPats items pty Anchor.nowhere)
                | _ -> ArgGroupG.GSimple(TastPoolBuilder.mintBoundVar contractPats, pty)
            elif isUnitFrozen pty then
                ArgGroupG.GUnit pty
            else
                ArgGroupG.GSimple(TastPoolBuilder.mintBoundVar contractPats, pty)

        {
            Typars = typars
            Groups = groups |> List.map groupOf
            ResultTy = resultTy
        }

    /// Flatten a decl list for emission, rewriting no expression: drop `type` decls
    /// (emitted as metadata), drop an `inline` binding whose body still carries a trait
    /// call, and split a folded top-level statement chain into standalone decls.
    let lower (decls: TastAccessor.DeclId list) : TastAccessor.DeclId list =
        // Consecutive top-level statements/lets arrive as ONE `TDecl.Expression` over a
        // `Sequential` / `let … in …` chain; peeling it makes each trailing `let` its own
        // decl.
        let rec flattenTopLevel (e: TastAccessor.ExprId) : TastAccessor.DeclId list =
            match TastAccessor.exprKind e with
            | ExprShape.Sequential ->
                [
                    for it in TastAccessor.exprChildren e do
                        yield! flattenTopLevel it
                ]
            | ExprShape.Let ->
                let l = TastAccessor.exprLet e

                TastAccessor.mintLetDecl l.Pattern l.Value false (TastAccessor.exprTy l.Value)
                :: flattenTopLevel l.Body
            | _ -> [ TastAccessor.mintExpressionDecl e (TastAccessor.exprTy e) ]

        let result = ResizeArray<TastAccessor.DeclId>()

        // A body still carrying a trait call is TEMPLATE-ONLY: "the type `^T` has this
        // member" is not encodable on a generic parameter, so there is no signature to
        // emit under. Narrower than "is `inline`" — most `inline` bodies do emit.
        let rec hasTraitCall (e: TastAccessor.ExprId) : bool =
            match TastAccessor.exprKind e with
            | ExprShape.TraitCall -> true
            | _ -> TastAccessor.existsChild hasTraitCall e

        let lowerOne (d: TastAccessor.DeclId) =
            match TastAccessor.declKind d with
            | DeclShape.Let ->
                let lv = TastAccessor.declLet d

                if not (lv.IsInline && hasTraitCall lv.Value) then
                    result.Add d
            | DeclShape.Expression -> result.Add d
            | DeclShape.Type -> ()

        for d in decls do
            match TastAccessor.declKind d with
            | DeclShape.Expression ->
                for fd in flattenTopLevel (TastAccessor.declExpression d) do
                    lowerOne fd
            | _ -> lowerOne d

        List.ofSeq result
