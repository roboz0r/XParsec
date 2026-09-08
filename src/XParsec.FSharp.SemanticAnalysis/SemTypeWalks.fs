namespace XParsec.FSharp.SemanticAnalysis

open Vesper

/// The variance a type position carries: `Co` a value read / result, `Contra` a
/// parameter, `Inv` a generic type ARGUMENT, whose slot admits both reads and writes.
[<RequireQualifiedAccess>]
type Variance =
    | Co
    | Contra
    | Inv

    member this.Flip =
        match this with
        | Variance.Co -> Variance.Contra
        | Variance.Contra -> Variance.Co
        | Variance.Inv -> Variance.Inv

/// One-level structural walks over `FrozenType`'s DIRECT children. Only for walks where
/// recursing into every child-carrying arm is definitionally correct; a walk with per-arm
/// semantics (encoders, renderers, freezing) stays an explicit exhaustive match.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FrozenType =
    /// Rebuild with `f` applied to each DIRECT child; a leaf returns unchanged.
    /// `FTOr` rebuilds through `MkUnion`: a mapped disjunct set can collapse or splice.
    let mapChildren (f: FrozenType -> FrozenType) (t: FrozenType) : FrozenType =
        match t with
        | FTConst(key, args) -> FTConst(key, Block.map f args)
        | FTFun(arg, result) -> FTFun(f arg, f result)
        | FTTuple items -> FTTuple(Block.map f items)
        | FTRecord(key, args) -> FTRecord(key, Block.map f args)
        | FTUnion(key, args) -> FTUnion(key, Block.map f args)
        | FTClass(key, args) -> FTClass(key, Block.map f args)
        | FTOr disjuncts -> disjuncts.Map f
        | FTKeyOf ty -> FTKeyOf(f ty)
        | FTIndexedAccess(objTy, index) -> FTIndexedAccess(f objTy, f index)
        | FTConditional c ->
            FTConditional
                {
                    Check = f c.Check
                    Extends = f c.Extends
                    WhenTrue = f c.WhenTrue
                    WhenFalse = f c.WhenFalse
                }
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTUnknown _
        | FTMeasure _ -> t

    /// Every `FTTypar` leaf replaced by `f scope index`.
    let rec mapTypars (f: TyparScope -> int<typeSlot> -> FrozenType) (t: FrozenType) : FrozenType =
        match t with
        | FTTypar(scope, index) -> f scope index
        | t -> mapChildren (mapTypars f) t

    /// Variance-tracking rebuild. `tryReplace v node` is consulted FIRST at every node,
    /// interior ones included: `ValueSome replacement` replaces `node` at variance `v` and
    /// STOPS the recursion; `ValueNone` recurses under the variance rule the arms spell out.
    let rec mapVariant
        (tryReplace: Variance -> FrozenType -> FrozenType voption)
        (v: Variance)
        (t: FrozenType)
        : FrozenType =
        match tryReplace v t with
        | ValueSome replaced -> replaced
        | ValueNone ->
            match t with
            | FTFun(a, b) -> FTFun(mapVariant tryReplace v.Flip a, mapVariant tryReplace v b)
            | FTConst(key, args) -> FTConst(key, args |> Block.map (mapVariant tryReplace Variance.Inv))
            | FTClass(k, args) -> FTClass(k, args |> Block.map (mapVariant tryReplace Variance.Inv))
            | FTRecord(k, args) -> FTRecord(k, args |> Block.map (mapVariant tryReplace Variance.Inv))
            | FTUnion(k, args) -> FTUnion(k, args |> Block.map (mapVariant tryReplace Variance.Inv))
            | FTTuple _
            | FTOr _
            | FTKeyOf _
            | FTIndexedAccess _
            | FTConditional _ -> mapChildren (mapVariant tryReplace v) t
            | FTEnum _
            | FTLiteral _
            | FTTypar _
            | FTUnknown _
            | FTMeasure _ -> t

    let iterChildren (f: FrozenType -> unit) (t: FrozenType) : unit =
        match t with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> Block.iter f args
        | FTFun(arg, result) ->
            f arg
            f result
        | FTTuple items -> Block.iter f items
        | FTOr disjuncts -> EqSet.iter f disjuncts.Disjuncts
        | FTKeyOf ty -> f ty
        | FTIndexedAccess(objTy, index) ->
            f objTy
            f index
        | FTConditional c ->
            f c.Check
            f c.Extends
            f c.WhenTrue
            f c.WhenFalse
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTUnknown _
        | FTMeasure _ -> ()

    /// `p` holds for EVERY direct child (vacuously true at a leaf). Short-circuits.
    let forallChildren (p: FrozenType -> bool) (t: FrozenType) : bool =
        match t with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> Block.forall p args
        | FTFun(arg, result) -> p arg && p result
        | FTTuple items -> Block.forall p items
        | FTOr disjuncts -> EqSet.forall p disjuncts.Disjuncts
        | FTKeyOf ty -> p ty
        | FTIndexedAccess(objTy, index) -> p objTy && p index
        | FTConditional c -> p c.Check && p c.Extends && p c.WhenTrue && p c.WhenFalse
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTUnknown _
        | FTMeasure _ -> true

    /// `p` holds for SOME direct child (vacuously false at a leaf). Short-circuits.
    let existsChild (p: FrozenType -> bool) (t: FrozenType) : bool =
        not (forallChildren (fun c -> not (p c)) t)

    let private isMeasureArg (arg: FrozenType) : bool =
        match arg with
        | FTMeasure _ -> true
        | _ -> false

    /// The type-slot arguments of a nominal's signature-order argument list: the arguments
    /// a type-kinded typar leaf indexes.
    let typeSlotArgs (args: Block<FrozenType>) : BlockM<FrozenType, typeSlot> =
        args
        |> Block.toArray
        |> Array.filter (isMeasureArg >> not)
        |> Block.unsafeOfArray

    type MeasuredClaim =
        {
            Key: TypeKey
            TypeArgs: BlockM<FrozenType, typeSlot>
            Units: MeasureTerm
        }

    /// A measured nominal: a claim applied to exactly one measure (`float<m>` is
    /// `FTConst(Vesper.float`1, [FTMeasure m])`).
    let (|MeasuredNominal|_|) (t: FrozenType) : MeasuredClaim voption =
        match t with
        | FTConst(key, args) ->
            let measures, types = args |> Block.toArray |> Array.partition isMeasureArg

            match measures with
            | [||] -> ValueNone
            | [| FTMeasure units |] ->
                ValueSome
                    {
                        Key = key
                        TypeArgs = Block.unsafeOfArray types
                        Units = units
                    }
            | _ -> failwithf "FrozenType.MeasuredNominal: a nominal applied to several measures: %A" t
        | _ -> ValueNone

    /// True when `a` and `b` share the same outermost type constructor: same case, and for a
    /// nominal the same `key`; child structure is ignored. A type's, member's or module
    /// function's typar is a WILDCARD matching anything; a local typar matches only itself.
    let private sameTyCtor (a: FrozenType) (b: FrozenType) : bool =
        match a, b with
        | FTTypar(TyparScope.LocalFunction s1, i1), FTTypar(TyparScope.LocalFunction s2, i2) -> s1 = s2 && i1 = i2
        | FTTypar(scope, _), _
        | _, FTTypar(scope, _) -> not scope.IsLocal
        | FTConst(k1, _), FTConst(k2, _) -> k1 = k2
        | FTRecord(k1, _), FTRecord(k2, _)
        | FTUnion(k1, _), FTUnion(k2, _)
        | FTClass(k1, _), FTClass(k2, _) -> k1 = k2
        | FTEnum k1, FTEnum k2 -> k1 = k2
        | FTFun _, FTFun _ -> true
        | FTTuple _, FTTuple _ -> true
        | FTOr _, FTOr _ -> true
        | FTLiteral v1, FTLiteral v2 -> v1 = v2
        | FTKeyOf _, FTKeyOf _ -> true
        | FTIndexedAccess _, FTIndexedAccess _ -> true
        | FTConditional _, FTConditional _ -> true
        | FTUnknown r1, FTUnknown r2 -> r1 = r2
        | FTMeasure m1, FTMeasure m2 -> m1 = m2
        | _ -> false

    /// PAIRWISE descent: `f` on each corresponding child of `a` and `b`. A case, type
    /// constructor or length mismatch is a silent no-op, because the caller decides what
    /// it means. Nominal KEYS are not compared: this serves open-template vs instantiated
    /// matching.
    let iterChildren2 (f: FrozenType -> FrozenType -> unit) (a: FrozenType) (b: FrozenType) : unit =
        let pairwise (xs: Block<FrozenType>) (ys: Block<FrozenType>) =
            if xs.Length = ys.Length then
                for i in 0 .. xs.Length - 1 do
                    f xs.[i] ys.[i]

        match a, b with
        | FTFun(a1, r1), FTFun(a2, r2) ->
            f a1 a2
            f r1 r2
        | FTTuple xs, FTTuple ys
        | FTConst(_, xs), FTConst(_, ys)
        | FTRecord(_, xs), FTRecord(_, ys)
        | FTUnion(_, xs), FTUnion(_, ys)
        | FTClass(_, xs), FTClass(_, ys) -> pairwise xs ys
        | FTOr xds, FTOr yds when xds.Disjuncts.Length = yds.Disjuncts.Length ->
            let xs = xds.Disjuncts
            let ys = yds.Disjuncts
            let n = xs.Length
            let mutable positionalOk = true

            for i in 0 .. n - 1 do
                positionalOk <- positionalOk && sameTyCtor xs.[i] ys.[i]

            if positionalOk then
                for i in 0 .. n - 1 do
                    f xs.[i] ys.[i]
            else
                // Instantiation can reorder the disjunct set: pair by type constructor instead.
                let used = Array.zeroCreate<bool> n
                let wildcards = ResizeArray<FrozenType>()

                for i in 0 .. n - 1 do
                    match xs.[i] with
                    | FTTypar(scope, _) when not scope.IsLocal -> wildcards.Add xs.[i]
                    | x ->
                        let candidates =
                            [
                                for j in 0 .. n - 1 do
                                    if not used.[j] && sameTyCtor x ys.[j] then
                                        yield j
                            ]

                        match candidates with
                        | [ j ] ->
                            used.[j] <- true
                            f x ys.[j]
                        | [] -> ()
                        | _ ->
                            failwithf
                                "FrozenType.iterChildren2: ambiguous FTOr pairing, because open disjunct %A matches multiple instantiated disjuncts in %A"
                                x
                                ys

                // Leftover instantiated disjuncts go to the wildcard open ones, in index
                // order.
                let mutable wi = 0

                for j in 0 .. n - 1 do
                    if not used.[j] && wi < wildcards.Count then
                        f wildcards.[wi] ys.[j]
                        wi <- wi + 1
        | FTKeyOf x1, FTKeyOf x2 -> f x1 x2
        | FTIndexedAccess(o1, i1), FTIndexedAccess(o2, i2) ->
            f o1 o2
            f i1 i2
        | FTConditional c1, FTConditional c2 ->
            f c1.Check c2.Check
            f c1.Extends c2.Extends
            f c1.WhenTrue c2.WhenTrue
            f c1.WhenFalse c2.WhenFalse
        | _ -> ()

[<AutoOpen>]
module SemTypePatterns =

    /// A member-bearing nominal (class, union or record) as `(declaring key, its type
    /// args)`; NOT `TyEnum` (niladic) or `TyConst` (an intrinsic). An arm where the
    /// kind forks must precede this one. Does NOT zonk: match an already-resolved type.
    [<return: Struct>]
    let (|TyNominal|_|) (ty: SemType) : struct (TypeKey * Block<SemType>) voption =
        match ty with
        | TyClass(key, args)
        | TyUnion(key, args)
        | TyRecord(key, args) -> ValueSome(struct (key, args))
        | _ -> ValueNone

    /// A type declared under a `TypeKey`, as `(key, its type args)`: a nominal, an enum (no
    /// args) or an intrinsic. Does NOT zonk: match an already-resolved type.
    [<return: Struct>]
    let (|TyKeyed|_|) (ty: SemType) : struct (TypeKey * Block<SemType>) voption =
        match ty with
        | TyClass(key, args)
        | TyUnion(key, args)
        | TyRecord(key, args)
        | TyConst(key, args) -> ValueSome(struct (key, args))
        | TyEnum key -> ValueSome(struct (key, Block.empty))
        | _ -> ValueNone

    /// `TyKeyed` over `FrozenType`.
    [<return: Struct>]
    let (|FTKeyed|_|) (ty: FrozenType) : struct (TypeKey * Block<FrozenType>) voption =
        match ty with
        | FTClass(key, args)
        | FTUnion(key, args)
        | FTRecord(key, args)
        | FTConst(key, args) -> ValueSome(struct (key, args))
        | FTEnum key -> ValueSome(struct (key, Block.empty))
        | _ -> ValueNone

    /// A type-level node still carried unevaluated: `keyof T`, `T[K]` or a conditional. Does
    /// NOT zonk: match an already-resolved type. An exhaustive match over `SemType` spells the
    /// three cases out instead, so the compiler reports a new carrier form there.
    [<return: Struct>]
    let (|TyCarrier|_|) (ty: SemType) : unit voption =
        match ty with
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> ValueSome()
        | _ -> ValueNone

/// One-level structural walks over `SemType`'s DIRECT children. PURELY structural:
/// nothing here resolves or zonks, so a `TyVar` is a leaf. A walk that needs the
/// resolved view takes it first, then delegates the child-carrying remainder here.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SemType =
    /// Rebuild with `f` applied to each DIRECT child; a leaf (incl. `TyVar`) returns
    /// unchanged, and so does a node whose children all come back reference-equal; a ground
    /// subtree therefore walks allocation-free.
    let mapChildren (f: SemType -> SemType) (t: SemType) : SemType =
        match t with
        | TyConst(name, args) ->
            match Block.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyConst(name, args')
        | TyFun(arg, result) ->
            let arg' = f arg
            let result' = f result

            if refEq arg' arg && refEq result' result then
                t
            else
                TyFun(arg', result')
        | TyTuple items ->
            match Block.mapPreserve f items with
            | ValueNone -> t
            | ValueSome items' -> TyTuple items'
        | TyRecord(key, args) ->
            match Block.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyRecord(key, args')
        | TyUnion(key, args) ->
            match Block.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyUnion(key, args')
        | TyClass(key, args) ->
            match Block.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyClass(key, args')
        | TyOr disjuncts ->
            match disjuncts.MapPreserve f with
            | ValueNone -> t
            | ValueSome t' -> t'
        | TyKeyOf ty ->
            let ty' = f ty
            if refEq ty' ty then t else TyKeyOf ty'
        | TyIndexedAccess(objTy, index) ->
            let objTy' = f objTy
            let index' = f index

            if refEq objTy' objTy && refEq index' index then
                t
            else
                TyIndexedAccess(objTy', index')
        | TyConditional c ->
            let check = f c.Check
            let extends = f c.Extends
            let whenTrue = f c.WhenTrue
            let whenFalse = f c.WhenFalse

            if
                refEq check c.Check
                && refEq extends c.Extends
                && refEq whenTrue c.WhenTrue
                && refEq whenFalse c.WhenFalse
            then
                t
            else
                TyConditional
                    {
                        Check = check
                        Extends = extends
                        WhenTrue = whenTrue
                        WhenFalse = whenFalse
                    }
        | TyVar _
        | TyEnum _
        | TyLiteral _
        | TyTypar _
        | TyUnknown _ -> t

    let iterChildren (f: SemType -> unit) (t: SemType) : unit =
        match t with
        | TyConst(_, args)
        | TyRecord(_, args)
        | TyUnion(_, args)
        | TyClass(_, args) -> Block.iter f args
        | TyFun(arg, result) ->
            f arg
            f result
        | TyTuple items -> Block.iter f items
        | TyOr disjuncts -> EqSet.iter f disjuncts.Disjuncts
        | TyKeyOf ty -> f ty
        | TyIndexedAccess(objTy, index) ->
            f objTy
            f index
        | TyConditional c ->
            f c.Check
            f c.Extends
            f c.WhenTrue
            f c.WhenFalse
        | TyVar _
        | TyEnum _
        | TyLiteral _
        | TyTypar _
        | TyUnknown _ -> ()

    /// `p` holds for EVERY direct child (vacuously true at a leaf). Short-circuits.
    let forallChildren (p: SemType -> bool) (t: SemType) : bool =
        match t with
        | TyConst(_, args)
        | TyRecord(_, args)
        | TyUnion(_, args)
        | TyClass(_, args) -> Block.forall p args
        | TyFun(arg, result) -> p arg && p result
        | TyTuple items -> Block.forall p items
        | TyOr disjuncts -> EqSet.forall p disjuncts.Disjuncts
        | TyKeyOf ty -> p ty
        | TyIndexedAccess(objTy, index) -> p objTy && p index
        | TyConditional c -> p c.Check && p c.Extends && p c.WhenTrue && p c.WhenFalse
        | TyVar _
        | TyEnum _
        | TyLiteral _
        | TyTypar _
        | TyUnknown _ -> true

    /// `p` holds for SOME direct child (vacuously false at a leaf). Short-circuits.
    let existsChild (p: SemType -> bool) (t: SemType) : bool =
        not (forallChildren (fun c -> not (p c)) t)

/// `∀ Quantified . Body`. Instantiating mints a fresh TyVar at the current level for
/// every entry in `Quantified` and substitutes it through `Body`, so independent use
/// sites get independent variables. A quantified TyVar stays live in the union-find graph.
[<Sealed>]
type TypeScheme(quantified: TyVarId list, body: SemType, constraints: (TyVarId * SemanticConstraint) list) =
    new(quantified: TyVarId list, body: SemType) = TypeScheme(quantified, body, [])
    member _.Quantified = quantified
    member _.Body = body
    /// Captured at generalisation time: each entry pairs the constraint with the
    /// QUANTIFIED TyVar it constrained, and instantiation re-stamps it onto that TyVar's
    /// fresh instance.
    member _.Constraints = constraints

/// One resolved `when ^T : …` constraint of a library-only static-optimization clause.
/// Generic over the type domain: at `'ty = SemType` the typar is a `TyVar` inline
/// expansion substitutes; frozen it carries no union-find cell and can cross assemblies.
[<RequireQualifiedAccess>]
type TStaticOptConstraintG<'ty> =
    /// `when ^T : SomeType` — holds when the type substituted for `typar` equals
    /// `required`. The catch-all `when ^T : ^T` is `required` = `typar`, which after
    /// substitution always matches.
    | TyconEquals of typar: 'ty * required: 'ty
    /// `when ^T : struct` — holds when the substituted `typar` is a value type.
    | IsStruct of typar: 'ty

type TStaticOptConstraint = TStaticOptConstraintG<SemType>

/// `BindingSite` is where the name was INTRODUCED, not the use site.
type ResolvedBinding =
    {
        BindingSite: NodeKey
        IsInline: bool
        IsMutable: bool
    }
