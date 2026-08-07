namespace XParsec.FSharp.SemanticAnalysis

/// `SemType` ↔ `FrozenType`. `TyVar` is the sole `SemType` case with no frozen
/// counterpart — that split is the point of `FrozenType`.
[<AutoOpen>]
module FrozenTypeBridge =
    let rec toFrozenWith (onVar: SemType -> FrozenType) (ty: SemType) : FrozenType =
        let go = toFrozenWith onVar

        match ty with
        | TyConst(key, args) -> FTConst(key, EqArray.map go args)
        | TyFun(arg, result) -> FTFun(go arg, go result)
        | TyTuple items -> FTTuple(EqArray.map go items)
        | TyRecord(key, args) -> FTRecord(key, EqArray.map go args)
        | TyUnion(key, args) -> FTUnion(key, EqArray.map go args)
        | TyClass(key, args) -> FTClass(key, EqArray.map go args)
        | TyEnum key -> FTEnum key
        // Freezing members can collapse the set (two distinct members freezing
        // equal), so rebuild through `MkUnion` rather than mapping in place.
        | TyOr members -> FrozenType.MkUnion(seq { for m in members.Members -> go m })
        | TyLiteral v -> FTLiteral v
        // The type-level computations are carried inert — never evaluated here.
        | TyKeyOf t -> FTKeyOf(go t)
        | TyIndexedAccess(objTy, index) -> FTIndexedAccess(go objTy, go index)
        | TyConditional c ->
            FTConditional
                {
                    Check = go c.Check
                    Extends = go c.Extends
                    WhenTrue = go c.WhenTrue
                    WhenFalse = go c.WhenFalse
                }
        | TyTypar(axis, index) -> FTTypar(axis, index)
        | TyUnknown name -> FTUnknown name
        | TyVar _ -> onVar ty

    let toFrozen (ty: SemType) : FrozenType =
        toFrozenWith (fun v -> failwithf "FrozenType.toFrozen: cannot freeze SemType: %A" v) ty

    /// Two templates of ONE signature must share a `methodVar` memo to agree on `j`.
    let rec instantiateWith
        (declaring: int -> SemType)
        (methodVar: int -> SemType)
        (localTypar: SchemeId -> int -> SemType)
        (template: FrozenType)
        : SemType =
        let go = instantiateWith declaring methodVar localTypar

        match template with
        | FTConst(key, args) -> TyConst(key, EqArray.map go args)
        | FTFun(arg, result) -> TyFun(go arg, go result)
        | FTTuple items -> TyTuple(EqArray.map go items)
        | FTRecord(key, args) -> TyRecord(key, EqArray.map go args)
        | FTUnion(key, args) -> TyUnion(key, EqArray.map go args)
        | FTClass(key, args) -> TyClass(key, EqArray.map go args)
        | FTEnum key -> TyEnum key
        // Realising members can collapse the set (a typar member instantiating to
        // another member), so rebuild through `MkUnion`, not a raw `TyOr`.
        | FTOr members -> SemType.MkUnion(seq { for m in members -> go m })
        | FTLiteral v -> TyLiteral v
        // The type-level computations realise their children but are NOT evaluated.
        | FTKeyOf t -> TyKeyOf(go t)
        | FTIndexedAccess(objTy, index) -> TyIndexedAccess(go objTy, go index)
        | FTConditional c ->
            TyConditional
                {
                    Check = go c.Check
                    Extends = go c.Extends
                    WhenTrue = go c.WhenTrue
                    WhenFalse = go c.WhenFalse
                }
        | FTTypar(TyparAxis.Declaring, i) -> declaring i
        | FTTypar(TyparAxis.Method, j) -> methodVar j
        | FTLocalTypar(scheme, k) -> localTypar scheme k
        | FTUnknown name -> TyUnknown name

    /// The identity realisation: each DECLARED placeholder maps back to its own
    /// `TyTypar` marker. `FTLocalTypar` has no marker to map to, so it MINTS a fresh
    /// `TyVar`, memoised per `(scheme, k)` so repeated occurrences share one cell.
    let ofFrozen (store: TypeStore) (ft: FrozenType) : SemType =
        let localCache =
            System.Collections.Generic.Dictionary<struct (SchemeId * int), SemType>()

        instantiateWith
            (fun i -> TyTypar(TyparAxis.Declaring, i))
            (fun j -> TyTypar(TyparAxis.Method, j))
            (fun scheme k ->
                let key = struct (scheme, k)

                match localCache.TryGetValue key with
                | true, v -> v
                | _ ->
                    let v = TyVar(store.NewTypeVar())
                    localCache.[key] <- v
                    v
            )
            ft

    // A template is an external descriptor's body with its open typars baked as
    // `FTTypar(Declaring,i)` / `FTTypar(Method,j)`. It carries type shape only —
    // never constraints.

    /// Stands in for a body that can't be built at extraction time — it may
    /// forward-reference a type registered later in the same package.
    let deferredTemplate: FrozenType = FTUnknown "<deferred>"

    let methodFreshener
        (store: TypeStore)
        (cache: System.Collections.Generic.Dictionary<int, SemType>)
        (level: int)
        (j: int)
        : SemType =
        match cache.TryGetValue j with
        | true, v -> v
        | _ ->
            let tv = store.NewTypeVar()
            store.SetLevel(UnionFind.find store tv, level)
            let v = TyVar tv
            cache.[j] <- v
            v

    /// An `FTLocalTypar` arises only inside a decl's BODY (a body-local `let`'s own
    /// generalized scheme), so one in a SIGNATURE / type-shape template is a producer bug.
    let localTyparInTemplate (site: string) (scheme: SchemeId) (k: int) : SemType =
        failwithf "%s: unexpected body-local typar %d of scheme %O in a signature template" site k scheme

    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`, for a type-shape descriptor —
    /// record field, union-case field, interface arg, base type, abbreviation body.
    /// An index past `declaringArgs` degrades to `TyUnknown "<arity-mismatch>"`.
    let instantiateDeclaring (template: FrozenType) (declaringArgs: SemType[]) : SemType =
        instantiateWith
            (fun i ->
                if i < declaringArgs.Length then
                    declaringArgs.[i]
                else
                    TyUnknown "<arity-mismatch>"
            )
            (fun j ->
                failwithf
                    "FrozenTypeBridge.instantiateDeclaring: unexpected method typar %d in a type-shape template"
                    j
            )
            (localTyparInTemplate "FrozenTypeBridge.instantiateDeclaring")
            template

    /// Largest declaring-typar index the template references, `-1` if none — the
    /// declaring arity a receiver must supply to realise it.
    let rec maxDeclaringIndex (template: FrozenType) : int =
        match template with
        | FTTypar(TyparAxis.Declaring, i) -> i
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.maxDeclaringIndex: unexpected method typar %d in a type-shape template" j
        | t ->
            let mutable m = -1
            FrozenType.iterChildren (fun c -> m <- max m (maxDeclaringIndex c)) t
            m

    /// Contract extraction bakes EVERY typar on the `Declaring` axis, numbering the
    /// declaring type's own first, so a typar the member INTRODUCES (`<'a>`, or an
    /// implicit `'T`) lands at `i >= declaringTyparArity`: re-axis those to `Method`.
    let rec reaxisMethodTypars (declaringTyparArity: int) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) when i >= declaringTyparArity ->
            FTTypar(TyparAxis.Method, i - declaringTyparArity)
        | t -> FrozenType.mapChildren (reaxisMethodTypars declaringTyparArity) t

    /// Fully ground: no open typar on either axis, no body-local typar, and no
    /// `FTUnknown` (a leaked metavar the front end never resolved).
    let rec ftIsGround (t: FrozenType) : bool =
        match t with
        | FTTypar _
        | FTLocalTypar _
        | FTUnknown _ -> false
        | t -> FrozenType.forallChildren ftIsGround t

    /// `FTTypar(Declaring,i)` → `declaringArgs.[i]`, staying in `FrozenType` and
    /// touching no inference state — how an abbreviation body is expanded against
    /// use-site args. An under-applied generic abbrev is tolerated, not a crash.
    let rec substituteDeclaring (declaringArgs: FrozenType[]) (template: FrozenType) : FrozenType =
        match template with
        | FTTypar(TyparAxis.Declaring, i) ->
            if i < declaringArgs.Length then
                declaringArgs.[i]
            else
                FTUnknown "<abbrev-arity-mismatch>"
        | FTTypar(TyparAxis.Method, j) ->
            failwithf "FrozenTypeBridge.substituteDeclaring: unexpected method typar %d in a type-shape template" j
        | t -> FrozenType.mapChildren (substituteDeclaring declaringArgs) t

    /// The impl in `ifaces` (each `(compiled name, args over the declaring typars)`)
    /// whose name is `target`, with its args realised at THIS receiver —
    /// `FTTypar(Declaring,i) := declArgs.[i]`.
    let pickInterfaceWitness
        (target: string)
        (declArgs: FrozenType[])
        (ifaces: (string * FrozenType[]) seq)
        : EqArray<FrozenType> voption =
        match
            ifaces
            |> Seq.tryPick (fun (iname, ifaceArgs) ->
                if iname = target then
                    Some(ifaceArgs |> Array.map (substituteDeclaring declArgs) |> EqArray.ofArray)
                else
                    None
            )
        with
        | Some ia -> ValueSome ia
        | None -> ValueNone
