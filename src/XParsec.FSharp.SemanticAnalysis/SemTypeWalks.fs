namespace XParsec.FSharp.SemanticAnalysis

/// The variance a `FrozenType` position carries, threaded by `FrozenType.mapVariant`:
/// COVARIANT (a value read / result), CONTRAVARIANT (a parameter), INVARIANT (a
/// generic type ARGUMENT — a slot that admits both reads and writes, so neither the
/// covariant nor the contravariant face alone is sound for it). A general type-system
/// concept, not a backend one — the walk names no concrete type; a caller's leaf owns
/// any policy.
[<RequireQualifiedAccess>]
type Variance =
    | Co
    | Contra
    | Inv

    /// Flip co/contra; invariant is self-dual. Applied at each `FTFun` DOMAIN — a
    /// parameter position inverts the enclosing variance.
    member this.Flip =
        match this with
        | Variance.Co -> Variance.Contra
        | Variance.Contra -> Variance.Co
        | Variance.Inv -> Variance.Inv

/// One-level structural walks over `FrozenType`'s DIRECT children — THE answer to
/// the "every new constructor fans out into N hand-written walker arms" tax: a
/// generic walk keeps only its leaf-specific arms and delegates every
/// child-carrying case here, so the next constructor addition touches this module
/// instead of twenty walk sites. Walks with per-arm SEMANTICS (encoders,
/// renderers, `freeze`) stay explicit exhaustive matches by design — these
/// skeletons are only for walks where child recursion is definitionally correct
/// for any child-carrying arm.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module FrozenType =
    /// Rebuild with `f` applied to each DIRECT child; a leaf returns unchanged.
    /// `FTOr` rebuilds through `MkUnion` — a mapped member set can collapse or
    /// splice — so every mapping walk inherits the canonical-form invariant
    /// STRUCTURALLY instead of by per-site convention.
    let mapChildren (f: FrozenType -> FrozenType) (t: FrozenType) : FrozenType =
        match t with
        | FTConst(key, args) -> FTConst(key, EqArray.map f args)
        | FTFun(arg, result) -> FTFun(f arg, f result)
        | FTTuple items -> FTTuple(EqArray.map f items)
        | FTRecord(key, args) -> FTRecord(key, EqArray.map f args)
        | FTUnion(key, args) -> FTUnion(key, EqArray.map f args)
        | FTClass(key, args) -> FTClass(key, EqArray.map f args)
        | FTOr members -> FrozenType.MkUnion(seq { for m in members -> f m })
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
        | FTLocalTypar _
        | FTUnknown _ -> t

    /// Variance-tracking rebuild — the reusable skeleton for any walk whose per-arm
    /// action depends on POSITION (a covariant value read vs a contravariant parameter
    /// vs an invariant generic slot). `leaf v node` is consulted FIRST at every node:
    /// `ValueSome replacement` replaces `node` at variance `v` and STOPS the recursion
    /// (the leaf owns that subtree); `ValueNone` recurses under the structural variance
    /// rule. That rule is the type system's own and is FIXED here so no variance-sensitive
    /// walk re-derives it: variance FLIPS at each `FTFun` domain (a parameter is
    /// contravariant) and is KEPT for the result; a nominal / applied-constructor type
    /// ARGUMENT drops to INVARIANT (a generic slot admits both reads and writes);
    /// structural operators (tuple, anonymous union, keyof, indexed access, conditional)
    /// CARRY the enclosing variance into their children. Unlike `mapChildren`, the arms
    /// carry SEMANTICS (the variance decision), so — apart from the structural-operator
    /// arm, whose children are unconditionally same-variance — this is an EXHAUSTIVE match
    /// with no `mapChildren` catch-all: a new child-carrying constructor must force a
    /// variance decision here rather than silently inherit the enclosing one.
    let rec mapVariant (leaf: Variance -> FrozenType -> FrozenType voption) (v: Variance) (t: FrozenType) : FrozenType =
        match leaf v t with
        | ValueSome replaced -> replaced
        | ValueNone ->
            match t with
            // A parameter is contravariant: flip for the domain, keep variance for the result.
            | FTFun(a, b) -> FTFun(mapVariant leaf v.Flip a, mapVariant leaf v b)
            // Nominal / applied-constructor type ARGUMENTS are invariant slots.
            | FTConst(key, args) -> FTConst(key, args |> EqArray.map (mapVariant leaf Variance.Inv))
            | FTClass(k, args) -> FTClass(k, args |> EqArray.map (mapVariant leaf Variance.Inv))
            | FTRecord(k, args) -> FTRecord(k, args |> EqArray.map (mapVariant leaf Variance.Inv))
            | FTUnion(k, args) -> FTUnion(k, args |> EqArray.map (mapVariant leaf Variance.Inv))
            // Structural operators carry the ENCLOSING variance into their children.
            | FTTuple _
            | FTOr _
            | FTKeyOf _
            | FTIndexedAccess _
            | FTConditional _ -> mapChildren (mapVariant leaf v) t
            // Childless leaves.
            | FTEnum _
            | FTLiteral _
            | FTTypar _
            | FTLocalTypar _
            | FTUnknown _ -> t

    let iterChildren (f: FrozenType -> unit) (t: FrozenType) : unit =
        match t with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> EqArray.iter f args
        | FTFun(arg, result) ->
            f arg
            f result
        | FTTuple items -> EqArray.iter f items
        | FTOr members -> EqSet.iter f members
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
        | FTLocalTypar _
        | FTUnknown _ -> ()

    /// `p` holds for EVERY direct child (vacuously true at a leaf). Short-circuits.
    let forallChildren (p: FrozenType -> bool) (t: FrozenType) : bool =
        match t with
        | FTConst(_, args)
        | FTRecord(_, args)
        | FTUnion(_, args)
        | FTClass(_, args) -> EqArray.forall p args
        | FTFun(arg, result) -> p arg && p result
        | FTTuple items -> EqArray.forall p items
        | FTOr members -> EqSet.forall p members
        | FTKeyOf ty -> p ty
        | FTIndexedAccess(objTy, index) -> p objTy && p index
        | FTConditional c -> p c.Check && p c.Extends && p c.WhenTrue && p c.WhenFalse
        | FTEnum _
        | FTLiteral _
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> true

    /// `p` holds for SOME direct child (vacuously false at a leaf). Short-circuits.
    let existsChild (p: FrozenType -> bool) (t: FrozenType) : bool =
        not (forallChildren (fun c -> not (p c)) t)

    /// True when `a` and `b` present the SAME head — same case, and for a nominal
    /// the same `key`; child structure is ignored (that is what the pairwise descent
    /// recovers). An `FTTypar` is a WILDCARD that heads-matches anything: an open
    /// template slot accepts any instantiated shape. Used to test whether an `FTOr`'s
    /// members line up POSITIONALLY, and to head-key the fallback pairing when they do
    /// not.
    let private sameHead (a: FrozenType) (b: FrozenType) : bool =
        match a, b with
        | FTTypar _, _
        | _, FTTypar _ -> true
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
        | FTUnknown n1, FTUnknown n2 -> n1 = n2
        // NOT a wildcard (unlike `FTTypar`): a local typar is an identity-bearing
        // leaf that no argument vector instantiates, so it only heads-matches the
        // same `(binder, index)` PAIR — the leaf-identity rule `FTUnknown`/`FTLiteral`
        // follow. Never equate two local typars by index alone.
        | FTLocalTypar(b1, i1), FTLocalTypar(b2, i2) -> b1 = b2 && i1 = i2
        | _ -> false

    /// PAIRWISE descent: when `a` and `b` share the same head (same case, same
    /// child count — nominal KEYS are deliberately not compared, mirroring the
    /// open-vs-instantiated template matching this serves), invoke `f` on each
    /// corresponding child pair; any head mismatch is a silent no-op (the caller
    /// decides what a mismatch means). `FTOr` members are a SET (`EqSet`), so their
    /// storage order is NOT a semantic invariant across instantiation. When the
    /// members line up positionally (the common case — instantiation maps in order),
    /// pair by position; otherwise pair each open member to the instantiated member
    /// sharing its HEAD KEY (a wildcard `FTTypar` open member takes any leftover). A
    /// concrete open member whose head matches TWO unused instantiated members is
    /// genuinely ambiguous — fail loudly rather than guess; no match declines
    /// silently (like a head mismatch). A length mismatch declines wholesale.
    let iterChildren2 (f: FrozenType -> FrozenType -> unit) (a: FrozenType) (b: FrozenType) : unit =
        let pairwise (xs: EqArray<FrozenType>) (ys: EqArray<FrozenType>) =
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
        | FTOr xs, FTOr ys when xs.Length = ys.Length ->
            let n = xs.Length
            let mutable positionalOk = true

            for i in 0 .. n - 1 do
                positionalOk <- positionalOk && sameHead xs.[i] ys.[i]

            if positionalOk then
                for i in 0 .. n - 1 do
                    f xs.[i] ys.[i]
            else
                // Members were reordered (or freshly re-set-ified) by instantiation:
                // recover the pairing by head key instead of trusting position.
                let used = Array.zeroCreate<bool> n
                let wildcards = ResizeArray<FrozenType>()

                for i in 0 .. n - 1 do
                    match xs.[i] with
                    | FTTypar _ -> wildcards.Add xs.[i]
                    | x ->
                        let candidates =
                            [
                                for j in 0 .. n - 1 do
                                    if not used.[j] && sameHead x ys.[j] then
                                        yield j
                            ]

                        match candidates with
                        | [ j ] ->
                            used.[j] <- true
                            f x ys.[j]
                        | [] -> () // no partner: decline, mirroring a head mismatch
                        | _ ->
                            failwithf
                                "FrozenType.iterChildren2: ambiguous FTOr member pairing — open member %A matches multiple instantiated members in %A"
                                x
                                ys

                // Leftover instantiated members go to the wildcard open members. With a
                // SINGLE wildcard (the only shape any producer reaches today) this is
                // exact. With TWO+ wildcards the pairing is index-order ARBITRARY — head
                // keys can't disambiguate one bare typar from another — so if a future
                // reachable producer can emit a reordered `FTOr` with multiple bare-typar
                // members, this needs a real assignment, not first-come.
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

/// SemType-level active patterns that read naturally in `match` arms, auto-opened
/// with the rest of `SemanticInfo`.
[<AutoOpen>]
module SemTypePatterns =

    /// A NOMINAL registry type that carries type arguments and can hold instance
    /// members — class, union, OR record. NOT `TyEnum` (niladic, no `args`) and NOT
    /// `TyConst` (an intrinsic head, not a member-bearing registry type). Yields the
    /// declaring `TypeKey` and the receiver's type arguments.
    ///
    /// This is the single unification vehicle for kind-blind instance-member
    /// dispatch: a `match` arm on `TyNominal(key, args)` treats the three kinds
    /// identically (the declaring-key lookup, the member-key registry read, the
    /// `MethodCall`/`PropertyGet` lowering), and every such site is greppable. The
    /// three cases stay DISTINCT in the representation — this pattern is the only
    /// sanctioned way to say "these three, identically", and it keeps the arms
    /// source-compatible with a future real `TyNominal` DU case (only construction
    /// sites would change). A site where a kind adds behaviour ON TOP of member
    /// dispatch — a record's field-by-name access, or a construction / tag / field
    /// site where the kind genuinely forks — keeps its explicit
    /// `TyRecord`/`TyUnion`/`TyClass` arm, ordered BEFORE this one so the
    /// kind-specific behaviour wins. Does NOT zonk — match on an already-resolved
    /// type (`Unification.zonk` first where the receiver may be a link).
    [<return: Struct>]
    let (|TyNominal|_|) (ty: SemType) : struct (TypeKey * EqArray<SemType>) voption =
        match ty with
        | TyClass(key, args)
        | TyUnion(key, args)
        | TyRecord(key, args) -> ValueSome(struct (key, args))
        | _ -> ValueNone

/// `SemType` sibling of the `FrozenType` child-walk module above — the same
/// one-level skeletons, PURELY structural: no `resolveStep`/`zonk` here (a walk
/// dispatches on its own resolved view first, then delegates the child-carrying
/// remainder). `TyVar` is a leaf from this module's viewpoint.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module SemType =
    /// Rebuild with `f` applied to each DIRECT child; a leaf (incl. `TyVar`) returns
    /// unchanged. `TyOr` rebuilds through `UnionMembers.Map`/`MkUnion`, so every mapping
    /// walk inherits the canonical-form invariant structurally.
    ///
    /// SHARING-PRESERVING: when `f` leaves every child reference-unchanged — the common
    /// case when a walk (`zonk`/`substitute`/`resolveStep`) hits an already-ground subtree
    /// — the SAME node is returned with no allocation, so the sharing propagates up the
    /// tree and a ground type walks allocation-free. Sound because `SemType` is immutable
    /// and value-compared (identity is never observed); it mints nothing, so a `TyVar`
    /// leaf's store-relative `TyVarId` is never fabricated or shared across stores.
    let mapChildren (f: SemType -> SemType) (t: SemType) : SemType =
        match t with
        | TyConst(name, args) ->
            match EqArray.mapPreserve f args with
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
            match EqArray.mapPreserve f items with
            | ValueNone -> t
            | ValueSome items' -> TyTuple items'
        | TyRecord(key, args) ->
            match EqArray.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyRecord(key, args')
        | TyUnion(key, args) ->
            match EqArray.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyUnion(key, args')
        | TyClass(key, args) ->
            match EqArray.mapPreserve f args with
            | ValueNone -> t
            | ValueSome args' -> TyClass(key, args')
        | TyOr members -> members.Map f
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
        | TyClass(_, args) -> EqArray.iter f args
        | TyFun(arg, result) ->
            f arg
            f result
        | TyTuple items -> EqArray.iter f items
        | TyOr members -> EqSet.iter f members.Members
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
        | TyClass(_, args) -> EqArray.forall p args
        | TyFun(arg, result) -> p arg && p result
        | TyTuple items -> EqArray.forall p items
        | TyOr members -> EqSet.forall p members.Members
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

/// `∀ Quantified . Body`. Built by `Unification.generalise` and stored in
/// `PassContext.Bindings.Scheme` keyed by the binding's headPat NodeKey. Each
/// `inferIdent` of a generalised binding instantiates the scheme — mints a
/// fresh TyVar at the current level for every entry in `Quantified` and
/// walks `Body` substituting them, so independent use sites get independent
/// variables. Mirrors `ExternalSymbols.instantiateSymbol` for the finitely many
/// `'a`s that come out of a user-written `let`. Quantified TyVars stay live
/// in the union-find graph; they are simply no longer "free" with respect
/// to the outer scope.
[<Sealed>]
type TypeScheme(quantified: TyVarId list, body: SemType, constraints: (TyVarId * SemanticConstraint) list) =
    new(quantified: TyVarId list, body: SemType) = TypeScheme(quantified, body, [])
    member _.Quantified = quantified
    member _.Body = body
    /// Constraints captured at generalisation time. Each entry pairs the
    /// constraint with the *quantified* TyVar it constrained at that
    /// point; `instantiate` swaps the TyVar through the substitution
    /// before re-stamping. Empty for the overwhelming majority of
    /// schemes — only `let f<'a when 'a : C> ...` populates this list.
    member _.Constraints = constraints

/// One resolved `when ^T : …` constraint of an F# library-only static
/// optimization clause. Lives here (not in `Tast.fs`) because the side table
/// that carries it is declared before `Tast.fs` in the compile order.
///
/// GENERIC over the type domain, exactly like the clause (`TStaticOptClauseG`)
/// that carries it, so it rides `TastConvert`'s freeze/thaw conversions rather
/// than being copied verbatim across them. On the producer side (`'ty = SemType`,
/// the `PassContext.StaticOpt` side table and the pre-freeze tree) the typar is a
/// `TyVar` over the inline binding's quantified root, so `Inline.inlineExpand`'s
/// typar substitution turns it into the call site's concrete type before the
/// clause is tested. In the frozen domain (`'ty = FrozenType`) it is the same
/// constraint with that root quantified — the whole point being that a frozen
/// clause carries NO `UnionFind` cell, so it can cross an assembly boundary.
[<RequireQualifiedAccess>]
type TStaticOptConstraintG<'ty> =
    /// `when ^T : SomeType` — holds when the type substituted for `typar` equals
    /// `required`. The catch-all `when ^T : ^T` is this case with `required`
    /// equal to `typar`, so after substitution both sides are the same concrete
    /// type and it matches unconditionally.
    | TyconEquals of typar: 'ty * required: 'ty
    /// `when ^T : struct` — holds when the substituted `typar` is a value type.
    | IsStruct of typar: 'ty

/// The producer-domain (inference-side) static-optimization constraint — what the
/// `PassContext.StaticOpt` side table and the pre-freeze TAST carry.
type TStaticOptConstraint = TStaticOptConstraintG<SemType>

/// BindingSite is the NodeKey of the LetBinding / lambda parameter /
/// TypeMember that introduced the name — NOT the use site.
type ResolvedBinding =
    {
        BindingSite: NodeKey
        IsInline: bool
        IsMutable: bool
    }

/// A thin view, not a rewritten tree: Desugar attaches this without ever
/// mutating CST shape.
[<RequireQualifiedAccess>]
type DesugaredForm =
    /// On an InfixApp / PrefixApp node, the operator's compiled name
    /// ("op_Addition", "op_Subtraction", "op_PipeRight", …). Unification
    /// looks the name up via the provider and types the application as if
    /// it were a normal function call. Polymorphic operators (`|>`, `>>`)
    /// are resolved this way too — the provider returns a fresh
    /// instantiation of the polymorphic scheme on each lookup.
    | OpName of compiledName: string
    /// On an `Expr.EnclosedBlock(ParenKind.List, …)` /
    /// `Expr.EmptyBlock(ParenKind.List, …)` node — `[1; 2; 3]` or `[]`.
    /// Unification types as `Microsoft.FSharp.Collections.list<'elem>`
    /// (single element-TyVar shared by every item); Elaborate projects the
    /// chain into nested `TExpr.UnionCons("Cons", [hd; tl])` /
    /// `UnionCons("Nil", [])` nodes.
    | ListLiteral
    /// On an `Expr.EnclosedBlock(ParenKind.Array, …)` /
    /// `Expr.EmptyBlock(ParenKind.Array, …)` node — `[|1; 2; 3|]` or
    /// `[||]`. Same element-typing rule as `ListLiteral`; Elaborate wraps
    /// the lowered list chain in an `Array.ofList` external call so
    /// the same nested `UnionCons` shape feeds both literal forms.
    | ArrayLiteral
    /// On an `Expr.InfixApp(_, ::, _)` node — cons construction `h :: t`. The
    /// `::` operator is not a provider-resolved function (unlike `+`/`|>`); it
    /// builds the list union directly. Unification types `h :: t` as the list
    /// type carrying `h`'s element type (`tail` unified to the same list);
    /// Elaborate projects it to `TExpr.UnionCons("Cons", [hd; tl])` against the
    /// resolved list union — the same shape `ListLiteral` lowers to.
    | ConsExpr
