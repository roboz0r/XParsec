namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser

/// Platform-neutral TAST lowering, shared by both codegen backends (CLR and JS).
/// The two codegen projects must not reference each other, so the lowering logic
/// that traffics only in node handles / `FrozenType` / `NodeKey` lives here, in
/// `SemanticAnalysis`, where both backends already depend.
///
/// Lowering is backend-uniform: no operator reaches it needing a per-backend finish.
/// `Passes.InlineExpansion` splices every operator's contract body by `SymbolKey`
/// pre-freeze, so what arrives here is already the body — an `ILIntrinsic` clause, a
/// `StaticMethodCall`, or a diagnostic that stopped the compile.
module TastLower =

    /// One flattened parameter of a top-level function lowered to a static method.
    /// A simple binder's `Slot` key is referenced directly by the body (it resolves
    /// to the parameter's slot); a destructuring tuple parameter (`fun (a, b) -> …`)
    /// carries `Pat = Some …` and a synthetic `Slot`, whose value the backend
    /// spills to a local and `bindPattern`s into the leaf bindings. Platform-neutral
    /// (pattern handle / `FrozenType` / `NodeKey` only); the CLR-shaped `StaticFn`
    /// that carries it stays in `Codegen.Clr`. The shape lives in `Tast.fs`
    /// (`StaticParamG`, generic over `'ty`/`'pat`); this is the pooled instantiation.
    type StaticParam = TastAccessor.StaticParam

    /// Resolve a nominal receiver type to its `(SymbolKey, type-args)` pair
    /// (was a projected string name). The
    /// project-local emitted-type tables key by this `SymbolKey` directly; the
    /// external provider lookups derive the qualified compiled name from it via
    /// `SymbolKeyOps.qualifiedName`. Returns `ValueNone` if the type isn't a
    /// user-defined or external nominal type (e.g. a `TyVar` that should have been
    /// zonked away by now).
    let inline receiverShape (ty: FrozenType) : (TypeKey * FrozenType list) voption =
        match ty with
        | FTUnion(n, args)
        | FTRecord(n, args)
        | FTClass(n, args) -> ValueSome(n, EqArray.toList args)
        | _ -> ValueNone

    /// As `matchInstantiation`, but returns the recovery array WITH `ValueNone`
    /// holes for typars that no parameter/result mentions — a phantom constraint
    /// typar (e.g. `fold`'s enumerator `'E`) is unrecoverable by param-matching and
    /// must be solved separately from its bounds at the call site (EmitCall).
    let matchInstantiationPartial
        (typarCount: int)
        (defTys: FrozenType list)
        (actualTys: FrozenType list)
        : FrozenType voption[] =
        let result = Array.create typarCount ValueNone

        let rec go (defT: FrozenType) (actT: FrozenType) =
            match defT, actT with
            // A freeze-quantified method typar: the index is on
            // the node, so the recovered instantiation is index-keyed. `act` may
            // itself be a `FTTypar(Method, j)` — the enclosing generic context's
            // typar — which the `MethodSpec` then encodes verbatim.
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
            // A generic intrinsic (notably the array `[]<!!i>`) carries its element
            // structurally; recurse so the element typar is recovered.
            | FTConst(_, xs), FTConst(_, ys) when xs.Length = ys.Length ->
                for i in 0 .. xs.Length - 1 do
                    go xs.[i] ys.[i]
            | _ -> ()

        List.iter2 go defTys actualTys
        result

    /// Recover a generic static method's per-typar instantiation at a call site:
    /// structurally match each declared parameter type (`defTys`, carrying
    /// the method's typar `TypeVar`s) against the actual argument type. First
    /// occurrence wins. A recursive self-call yields the method's own typars
    /// (encoded `!!i`); an external call yields concrete types. Strict: every
    /// typar must be recovered by param-matching (the `EmitExpr` caller has no
    /// phantom typars).
    let matchInstantiation (typarCount: int) (defTys: FrozenType list) (actualTys: FrozenType list) : FrozenType list =
        let result = matchInstantiationPartial typarCount defTys actualTys

        [
            for i in 0 .. typarCount - 1 ->
                match result.[i] with
                | ValueSome t -> t
                | ValueNone -> failwithf "Emit: could not infer instantiation for static-method type parameter %d" i
        ]

    /// The call-site PHANTOM-typar solve, shared by the
    /// project-local static-fn call (`EmitCall.buildAppCall`) and the EXTERNAL
    /// module-fn call (`ClrRecipes.emitExternalCall`); `tryWitness` is the only
    /// head-specific seam (project-local `env.Classes` vs the external provider's
    /// `FrozenInterfaces`), so the solve itself is head-agnostic.
    ///
    /// `instArr` already carries every signature-reachable method typar (recovered by
    /// `matchInstantiationPartial` / the encoder's `recoverOpenTypars`); a phantom
    /// constraint typar (`fold`'s enumerator `'E`, present only in a
    /// `'S :> IStructSeq<'T,'E>` bound, in no param/result) is still `ValueNone`. For
    /// each `Coercion(ci, target)` whose constrained typar `ci` is already resolved
    /// (e.g. `'S := instArr.[idx_S]`, the node-key-rewritten `<closure>$`-bearing arg
    /// type), `tryWitness` walks the constrained type's interface impl for `target`'s
    /// interface and the typars `target` mentions (incl. `'E`) are structurally
    /// recovered from the concrete witness. Iterate to a fixpoint — one bound's target
    /// may mention a typar another bound just solved.
    ///
    /// The witness is AUTHORITATIVE for a bound-mentioned typar even when the signature
    /// already recovered it: a combinator carrying `'E` in BOTH its bound AND its result
    /// can have a stale arrow in the result occurrence (a chained source's `'TFunc`
    /// buried in its frozen enumerator type), whereas the witness reads it from the
    /// source's ACTUAL `<closure>$`-bearing seq impl — so every bound-mentioned index is
    /// overridden with the witness value. Mutates `instArr` in place.
    let solvePhantomTypars
        (typarCount: int)
        (constraints: FrozenConstraint list)
        (tryWitness: FrozenType -> TypeKey -> EqArray<FrozenType> voption)
        (instArr: FrozenType voption[])
        : unit =
        if not (List.isEmpty constraints) then
            // Indices the bounds can recover — those the witness may overwrite.
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

    // ----------------------------------------------------------------------
    // Compiled-form representation
    //
    // Two preserved artifacts for a function / method:
    //   * `ValRepr`      — the SOURCE arity (the `ValReprInfo` analogue): the
    //                      curried groups, each group's tuple structure, the typar
    //                      arity, and the SOURCE (non-erased) result type. A caller
    //                      reconciles its application spine against this.
    //   * `CompiledForm` — the flat CLR/JS signature DERIVED from a `ValRepr`:
    //                      tuple groups flattened (full F#, one level), a lone unit
    //                      group erased, a unit result mapped to `void`.
    //
    // `peelValRepr` is `peelLambda` recast as the `ValRepr` builder (it walks the
    // same curried lambda + tuple-pattern structure); `compiledOf` is the
    // `GetValReprTypeInCompiledForm` analogue. `ValRepr` is the load-bearing
    // artifact (the in-assembly `gather` and the cross-assembly `ExternalSymbol`
    // both carry it); `CompiledForm` is derived from it on demand by `compiledOf`,
    // never stored — it is fully determined by the `ValRepr`.
    // ----------------------------------------------------------------------

    // The source-arity / compiled-form types live in `Tast.fs` (`ArgGroupG`,
    // `ValReprG`, `CompiledReturnG`, `CompiledFormG`, generic over `'ty`/`'pat`/`'id`);
    // these are the pooled instantiations the builders below produce.
    type ArgGroup = TastAccessor.ArgGroup
    type ValRepr = TastAccessor.ValRepr
    type CompiledReturn = TastAccessor.CompiledReturn
    type CompiledForm = TastAccessor.CompiledForm

    let private isUnitFrozen (t: FrozenType) : bool =
        match t with
        | FTUnit -> true
        | _ -> false

    /// Peel up to `n` top-level `->` arrows off a frozen type (all of them when
    /// `n < 0`), returning each as a `(domain, codomain)` pair in order. One home for
    /// the `peelN` / `arrows` / `decurryFrozen` walks that were copied across
    /// `VesperLib`, `ClrRecipes`, and `EmitClosures`.
    let rec peelArrows (n: int) (t: FrozenType) : (FrozenType * FrozenType) list =
        if n = 0 then
            []
        else
            match t with
            | FTFun(a, b) -> (a, b) :: peelArrows (if n < 0 then -1 else n - 1) b
            | _ -> []

    /// Rebuild a frozen type by transforming its top-level type-argument vector.
    /// Covers every nominal-with-args shape (`FTConst`/`FTRecord`/`FTUnion`/`FTClass`
    /// and the tuple's element vector); leaves (`FTFun`/`FTOr`/`FTTypar`/`FTUnknown`)
    /// pass through unchanged. The one combinator the codegen verdict rewrites share
    /// for "replace some of a nominal's args" — the per-arg transform `f` decides what
    /// (position match, recursion, etc.).
    let mapFrozenArgs (f: EqArray<FrozenType> -> EqArray<FrozenType>) (t: FrozenType) : FrozenType =
        match t with
        | FTClass(key, args) -> FTClass(key, f args)
        | FTRecord(key, args) -> FTRecord(key, f args)
        | FTUnion(key, args) -> FTUnion(key, f args)
        | FTConst(key, args) -> FTConst(key, f args)
        | FTTuple items -> FTTuple(f items)
        | _ -> t

    /// `peelArrows` projected to the F#-form `(parameter types, residual result)`:
    /// `decurryFrozen`'s shape (`n < 0`, peel all) and the contract peelers (`n`
    /// groups). The residual is the type after the peeled arrows.
    let peelArrowDomains (n: int) (t: FrozenType) : FrozenType list * FrozenType =
        match peelArrows n t with
        | [] -> [], t
        | levels -> List.map fst levels, snd (List.last levels)

    /// The lone-`()` group shape (`[GUnit]`): the only arity that erases to zero
    /// compiled params (F#'s `[[]]` rule, read off the arity not the type). The one
    /// definition of the test the flatten / call-site / adapter paths share.
    let isLoneUnitGroup (groups: ArgGroup list) : bool =
        match groups with
        | [ ArgGroupG.GUnit _ ] -> true
        | _ -> false

    /// Every source group is a plain single binder — the only shape whose flat
    /// params map one-to-one onto the source applications (a self-tail-call can
    /// trampoline; flat == curried, so a value-use needs no adapter beyond aliasing).
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

    /// The flat compiled parameter-TYPE vector of a source group list, given each
    /// group's (already type-correct) parameter type — the
    /// `GetValReprTypeInCompiledForm` flatten rule expressed over types: a lone `()`
    /// group erases to nothing; a tuple group expands to its `FTTuple` elements (full
    /// F#, one level); every other group contributes its one type. `compiledOf`
    /// follows the identical shape over `StaticParam`s; the cross-assembly member-ref
    /// encoder (`ClrRecipes.emitExternalCall`) routes its open-template-peeled types
    /// through here, so the erase/flatten rule is written once.
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

    /// Peel a curried `Lambda` chain into its source `ArgGroup`s and the residual body —
    /// `ArgGroups.peel` over node handles. Only the two READERS are here; the walk, the
    /// grouping and the stopping condition are the shared rule, which the pool build runs
    /// over its raw columns.
    let peelValRepr (e: TastAccessor.ExprId) : ArgGroup list * TastAccessor.ExprId =
        let unLambda (e: TastAccessor.ExprId) =
            match e with
            | TastAccessor.ELambda lam -> ValueSome(struct (lam.Param, lam.Body))
            | _ -> ValueNone

        let facts (p: TastAccessor.PatId) : ArgGroups.ParamPatFacts<BinderId> =
            let shape = TastAccessor.patKind p

            {
                Shape = shape
                Ty = TastAccessor.patTy p
                Binder = TastAccessor.patBinder p
                ConstValue =
                    match shape with
                    | PatShape.Const -> ValueSome(TastAccessor.patConstValue p)
                    | _ -> ValueNone
            }

        ArgGroups.peel unLambda facts e

    /// `peelValRepr` projected to one flat parameter per SOURCE group — the shape a
    /// static-method emission binds its arg slots from. A unit group gets a PLACEHOLDER
    /// binder (`fun () -> …` — the body never references it) so the emission still
    /// allocates a slot for the unit value the caller pushes; a tuple group likewise gets
    /// one and carries its `Pat` so the emission `bindPattern`s the leaf bindings out of
    /// the value. Both are minted into the spine's OWN pool, so every slot a lowering
    /// hands the emission is addressed in the one id space its `Var`s are.
    let peelLambda (e: TastAccessor.ExprId) : StaticParam list * TastAccessor.ExprId =
        let groups, body = peelValRepr e

        let paramOf (g: ArgGroup) : StaticParam =
            match g with
            | ArgGroupG.GSimple(k, ty) -> { Slot = k; Ty = ty; Pat = None }
            | ArgGroupG.GUnit ty ->
                {
                    Slot = TastPoolBuilder.mintBinder e.Pool
                    Ty = ty
                    Pat = None
                }
            | ArgGroupG.GTuple pat ->
                {
                    Slot = TastPoolBuilder.mintBinder e.Pool
                    Ty = TastAccessor.patTy pat
                    Pat = Some pat
                }

        List.map paramOf groups, body

    /// Build the SOURCE `ValRepr` for a function value (`typars` = its generic
    /// arity), returning the residual body the backend emits. `ResultTy` is the
    /// residual body's type — the source result, before any unit→void normalisation.
    let valReprOf (typars: int) (e: TastAccessor.ExprId) : ValRepr * TastAccessor.ExprId =
        let groups, body = peelValRepr e

        {
            Typars = typars
            Groups = groups
            ResultTy = TastAccessor.exprTy body
        },
        body

    /// Flatten one source tuple element to a compiled parameter (one level): a
    /// simple binder becomes a direct arg slot the body references; a wildcard a
    /// slotted-but-unnamed arg; anything else keeps its pattern for the backend to
    /// destructure (a nested tuple element stays one `ValueTuple` param).
    let private flattenTupleItem (p: TastAccessor.PatId) : StaticParam =
        let ty = TastAccessor.patTy p

        match TastAccessor.patKind p, TastAccessor.patBinder p with
        | PatShape.NamedSimple, ValueSome k -> { Slot = k; Ty = ty; Pat = None }
        | PatShape.Wildcard, _ ->
            {
                Slot = TastPoolBuilder.mintBinder p.Pool
                Ty = ty
                Pat = None
            }
        | _ ->
            {
                Slot = TastPoolBuilder.mintBinder p.Pool
                Ty = ty
                Pat = Some p
            }

    /// Derive the flat `CompiledForm` from a source `ValRepr` — the
    /// `GetValReprTypeInCompiledForm` analogue. Full F# tuple flattening (one
    /// level); a LONE unit group (`[GUnit]`) erases to zero params (a unit group
    /// among others stays a `ValueTuple` param); a unit result becomes `RVoid`.
    ///
    /// `pool` is where a placeholder slot is minted — the pool the `ValRepr`'s own nodes
    /// belong to, so the flattened params are addressed in one id space. A `GUnit` group
    /// carries no pattern to take it from, which is why it is a parameter.
    let compiledOf (pool: PoolBuilder) (vr: ValRepr) : CompiledForm =
        let flattenGroup (g: ArgGroup) : StaticParam list =
            match g with
            | ArgGroupG.GUnit ty ->
                [
                    {
                        Slot = TastPoolBuilder.mintBinder pool
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
                [] // lone unit group erased (F#'s `[[]]` rule, off the arity)
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

    /// Build the SOURCE `ValRepr` for an EXTERNAL (contract-extracted) function. A
    /// `.fsi` `val` gives each curried group's arity (`CurriedSig`/`ArgsSpec`) but
    /// has no lambda tree to `peelValRepr`, so the groups are reconstructed from
    /// `(arity, paramTy)` pairs (arity = the `*`-separated width within the group):
    /// arity ≥ 2 → `GTuple` over the group's `FTTuple` param, its elements rebuilt
    /// as anonymous `Wildcard` leaves (a contract names them, but the compiled form
    /// does not); arity-1 `unit` → `GUnit` (the `let f () = …` lone-erasable shape);
    /// arity-1 other → `GSimple`. Feeding the result to `compiledOf` keeps the
    /// flatten / lone-unit-erase / unit→void rule single-sourced — the cross-assembly
    /// consumer derives the same compiled form an in-assembly function does from its
    /// own `gather`ed `ValRepr`.
    let externalValRepr (typars: int) (groups: (int * FrozenType) list) (resultTy: FrozenType) : ValRepr =
        // A contract-minted pattern belongs to no file's tree, so it indexes into no
        // file's pool — it gets a standalone one, owned by this `ValRepr` and reachable
        // only through the handles it hands out. That is what the pool-carrying handle
        // buys: the consumer reads these pats through the same accessor as any other,
        // and never learns they came from somewhere else. Nor does any of them sit
        // anywhere: a `.fsi` `val` has no lambda tree, so the reconstructed pattern
        // is spelled by no token at all.
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
                | _ ->
                    // A ≥2-width group is always an `FTTuple` (translateArgsSpec); keep a
                    // single param defensively rather than fabricate one.
                    ArgGroupG.GSimple(TastPoolBuilder.mintBinder contractPats, pty)
            elif isUnitFrozen pty then
                ArgGroupG.GUnit pty
            else
                ArgGroupG.GSimple(TastPoolBuilder.mintBinder contractPats, pty)

        {
            Typars = typars
            Groups = groups |> List.map groupOf
            ResultTy = resultTy
        }

    /// Lower a decl list into a closure-bearing tree. Every `TExprG.Lambda` is a
    /// function value and every `External` is a call head or has non-function type —
    /// but that is now an INPUT invariant, not something this establishes.
    ///
    /// Nothing operator- or inline-shaped survives to here. `Passes.InlineExpansion`
    /// walks every decl — `inline` bindings included — splicing inline call heads
    /// (local and cross-package) by `SymbolKey`, beta-reducing them, and eta-reifying
    /// every `External` used as a VALUE, including inside member bodies, which this
    /// lowering never walks. So the frozen decls arriving here carry no inline call
    /// heads and no `External` function values.
    ///
    /// `StaticOptimization` nodes DO survive on an `inline` binding, and are emittable:
    /// clause selection is a compile-time choice keyed on the operand type, and the node
    /// carries the `defaultExpr` fallback for exactly the case where no type is pinned —
    /// which is what the ordinary (non-spliced) function IS. Each backend emits that
    /// default.
    ///
    /// What is left is the flattening: drop `type` decls (emitted as metadata), drop the
    /// one binding shape that has no IL form at all (`traitCallOnly`), and split a nested
    /// `let` chain into a flat top-level decl list. It rewrites no EXPRESSION: what the
    /// expression walk used to do — descend rebuilding every node, leaving an `External`
    /// call head alone — is the identity, an `External` node having no children to
    /// descend into in the first place.
    let lower (decls: TastAccessor.DeclId list) : TastAccessor.DeclId list =
        // Split a folded top-level statement sequence back into standalone decls
        // in source order. The parser folds
        // consecutive top-level statements/lets into ONE `TDecl.Expression` whose
        // expr is a `Sequential` / `let … in …` chain, so a value *after* a
        // statement (a *trailing* value) arrives nested as a `TExpr.Let` the
        // Program-value collector never sees and stays a `Main` local. Peeling the
        // chain here lets each trailing `let` become its own `TDecl.Let`, which
        // flows into the already-built `ProgramMainValues` / `stsfld` path.
        //
        // Only the outermost statement *spine* is peeled — `Sequential` items and
        // the continuation (`body`) of a top-level `let … in …`. Sub-expressions
        // (application args, lambda bodies, match arms) are NOT descended into, so
        // a genuinely-local `let` nested inside an expression is left intact.
        let rec flattenTopLevel (e: TastAccessor.ExprId) : TastAccessor.DeclId list =
            match TastAccessor.exprKind e with
            | ExprShape.Sequential ->
                [
                    for it in TastAccessor.exprChildren e do
                        yield! flattenTopLevel it
                ]
            | ExprShape.Let ->
                // The bound value is itself an expression (not a statement spine) —
                // keep it whole; only the `body` continuation is more top-level decls.
                let l = TastAccessor.exprLet e

                TastAccessor.mintLetDecl l.Binding l.Value false (TastAccessor.exprTy l.Value)
                :: flattenTopLevel l.Body
            | _ -> [ TastAccessor.mintExpressionDecl e (TastAccessor.exprTy e) ]

        let result = ResizeArray<TastAccessor.DeclId>()

        // A body that still carries a `TExprG.TraitCall` is TEMPLATE-ONLY: it has no
        // compiled form on any target, and never will without witness passing.
        //
        // A trait call is "the type `^T` has this member" — a constraint the CLR cannot
        // encode on a generic parameter, so there is no signature to emit the function
        // under. `Inline.substMapper` discharges the node by rewriting it to a
        // `StaticMethodCall` once a SPLICE grounds `^T` to a nominal that carries the
        // member; at the definition site nothing is ground, so the node stands. This is
        // narrower than "is `inline`": a `let inline` whose body has no trait call — the
        // overwhelming majority, `StaticOptimization`-bearing ones included — is emitted
        // as an ordinary module function like any other binding, and only its use sites
        // decide whether they splice it or call it.
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
            // Type declarations are emitted as metadata, not through the expr stream.
            | DeclShape.Type -> ()

        for d in decls do
            match TastAccessor.declKind d with
            // A top-level statement decl may be a folded sequence — split it first,
            // so a trailing `let` reaches the collector as a standalone decl.
            | DeclShape.Expression ->
                for fd in flattenTopLevel (TastAccessor.declExpression d) do
                    lowerOne fd
            | _ -> lowerOne d

        List.ofSeq result
