namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore

/// Which type in a trait's support set declares a member of the trait's name and
/// shape. The unify-time discharge and the inline-expansion dispatcher share this
/// search; only the verdict policy differs per caller (the discharge propagates,
/// expansion reports).
module UnificationTraitMembers =

    /// The declaration a resolved trait member came from, carrying what the key mint needs.
    [<RequireQualifiedAccess>]
    type TraitMemberSource =
        /// A project-local nominal or intrinsic-abbrev host.
        | User of TypeRegistry.NominalMember
        /// A reference-assembly member.
        | External of ExternalMember

    /// One support type's resolved trait member.
    type TraitCandidate =
        {
            /// The support type that supplied the member.
            HostTy: SemType
            /// The member's type, declaring typars instantiated at the host's args.
            Ty: SemType
            /// The host's nominal type arguments, as a `StaticMethodCall` carries them;
            /// empty for an intrinsic host.
            DeclArgs: SemType[]
            Source: TraitMemberSource
        }

    [<RequireQualifiedAccess>]
    type TraitPick =
        /// Exactly one support type declares an applicable member.
        | Resolved of TraitCandidate
        /// Several support types each declare an applicable member; `rest` holds the
        /// further winners in support-set order and is non-empty.
        | Ambiguous of first: TraitCandidate * rest: TraitCandidate list
        /// Members of the name exist but no declared signature admits the trait shape
        /// (`1 + 1L` against `int`'s `int * int` and `int64`'s `int64 * int64`).
        /// `first` is the first such member in support-set order; `unsupported` is the
        /// support types with no member of the name at all.
        | NameOnly of first: TraitCandidate * unsupported: SemType list
        /// No support type declares a member of the name.
        | NoSupport
        /// A support type is unpinned or unreadable, so a later pin may still decide.
        | Incomplete

    /// Read-only structural agreement for choosing a trait host: ground positions must
    /// agree by shape and key, and a free var on either side matches anything. Choosing a
    /// host must not pin operands as a side effect; the winner's real `unify` does that.
    let rec looseMatch (store: TypeStore) (a: SemType) (b: SemType) : bool =
        let loose = looseMatch store

        match resolveStep store a, resolveStep store b with
        | TyVar _, _
        | _, TyVar _ -> true
        | TyConst(k1, xs), TyConst(k2, ys)
        | TyClass(k1, xs), TyClass(k2, ys)
        | TyUnion(k1, xs), TyUnion(k2, ys)
        | TyRecord(k1, xs), TyRecord(k2, ys) -> k1 = k2 && xs.Length = ys.Length && EqArray.forall2 loose xs ys
        | TyFun(a1, r1), TyFun(a2, r2) -> loose a1 a2 && loose r1 r2
        | TyTuple xs, TyTuple ys -> xs.Length = ys.Length && EqArray.forall2 loose xs ys
        | _ -> false

    /// The trait signature in the shape `candTy` is compared (and unified) against: F#
    /// accepts both `static member (+)(a, b)` and `static member (+) a b` for a trait
    /// declared `^T * ^T -> ^T`, so a tupled candidate gets the tupled form and a curried
    /// one the curried form.
    let expectedShape (store: TypeStore) (argTys: EqArray<SemType>) (retTy: SemType) (candTy: SemType) : SemType =
        let tupled =
            match argTys.Length with
            | 0 -> retTy
            | 1 -> TyFun(argTys.[0], retTy)
            | _ -> TyFun(TyTuple argTys, retTy)

        match resolveStep store candTy with
        | TyFun(TyTuple _, _) -> tupled
        | _ when argTys.Length >= 2 -> EqArray.foldBack (fun a r -> TyFun(a, r)) argTys retTy
        | _ -> tupled

    /// What one support type contributes to the search.
    [<RequireQualifiedAccess>]
    type private HostVerdict =
        /// An applicable member.
        | Candidate of TraitCandidate
        /// Members of the name exist but none admits the trait shape; the first, in
        /// declaration order, is kept for the mismatch report.
        | NameOnly of TraitCandidate
        /// A readable host with no member of the name.
        | NoMember
        /// A shape the search cannot read: an unknown class, a function, an unpinned var.
        | Opaque

    let private hostVerdict
        (ctx: PassContext)
        (memberName: string)
        (argTys: EqArray<SemType>)
        (retTy: SemType)
        (hostTy: SemType)
        : HostVerdict =
        let applicable (candTy: SemType) : bool =
            looseMatch ctx.Store candTy (expectedShape ctx.Store argTys retTy candTy)

        // The first applicable member wins within one host; declaration order is the
        // tiebreak, mirroring the per-host order the provider serves.
        let pickApplicable (candidates: TraitCandidate[]) : HostVerdict =
            match candidates with
            | [||] -> HostVerdict.NoMember
            | cs ->
                match cs |> Array.tryFind (fun c -> applicable c.Ty) with
                | Some c -> HostVerdict.Candidate c
                | None -> HostVerdict.NameOnly cs.[0]

        let fromDecl (declArgs: SemType[]) (args: EqArray<SemType>) (decl: TypeRegistry.NominalDecl) : HostVerdict =
            decl.Members
            |> Array.filter (fun m -> m.IsStatic && m.Name = memberName)
            |> Array.map (fun m ->
                {
                    HostTy = hostTy
                    Ty = instantiateMember ctx.Store (decl.TypeParams, args) m.Type
                    DeclArgs = declArgs
                    Source = TraitMemberSource.User { Decl = decl; Member = m }
                }
            )
            |> pickApplicable

        let fromProvider (declArgs: SemType[]) (key: TypeKey) (args: EqArray<SemType>) : HostVerdict voption =
            let members = EqArray.toArray (ctx.Provider.TryLookupMembers(key, memberName))

            match members |> Array.filter (fun m -> m.IsStatic) with
            | [||] -> ValueNone
            | statics ->
                statics
                |> Array.map (fun m ->
                    {
                        HostTy = hostTy
                        Ty = ExternalSymbols.openSignature m (EqArray.toArray args)
                        DeclArgs = declArgs
                        Source = TraitMemberSource.External m
                    }
                )
                |> pickApplicable
                |> ValueSome

        match resolveStep ctx.Store hostTy with
        | TyConst(key, args) ->
            // An intrinsic's members are fully readable, so a miss in both its abbrev
            // host and its provider contract is `NoMember`, not `Opaque`.
            match TypeRegistry.tryIntrinsicAbbrevByKey ctx.Types key with
            | ValueSome decl -> fromDecl [||] args decl
            | ValueNone ->
                match fromProvider [||] key args with
                | ValueSome v -> v
                | ValueNone -> HostVerdict.NoMember
        | TyClass(key, args)
        | TyUnion(key, args)
        | TyRecord(key, args) ->
            let declArgs = EqArray.toArray args

            match TypeRegistry.tryNominalByKey ctx.Types key with
            | ValueSome decl -> fromDecl declArgs args decl
            | ValueNone ->
                // Not project-local: dispatching `s + t` on an `.fsi`-imported type
                // resolves through the provider. A class the provider cannot see
                // stays `Opaque`.
                match fromProvider declArgs key args with
                | ValueSome v -> v
                | ValueNone -> HostVerdict.Opaque
        | _ -> HostVerdict.Opaque

    /// Search the distinct support types for `memberName` with the trait shape
    /// `argTys -> retTy`. Without `force`, an unpinned or unreadable support type
    /// defers the whole search; `force` searches past them over the readable pinned
    /// subset.
    let pick
        (ctx: PassContext)
        (memberName: string)
        (argTys: EqArray<SemType>)
        (retTy: SemType)
        (supportTys: SemType[])
        (force: bool)
        : TraitPick =
        let hosts =
            supportTys
            |> Array.map (zonk ctx.Store)
            |> Array.distinct
            |> Array.map (fun t ->
                let v =
                    match resolveStep ctx.Store t with
                    | TyVar _ -> HostVerdict.Opaque
                    | _ -> hostVerdict ctx memberName argTys retTy t

                t, v
            )

        let complete =
            hosts
            |> Array.forall (fun (_, h) ->
                match h with
                | HostVerdict.Opaque -> false
                | _ -> true
            )

        if not (complete || force) then
            TraitPick.Incomplete
        else
            let candidates =
                hosts
                |> Array.choose (fun (_, h) ->
                    match h with
                    | HostVerdict.Candidate c -> Some c
                    | _ -> None
                )
                |> List.ofArray

            match candidates with
            | [ one ] -> TraitPick.Resolved one
            | first :: rest -> TraitPick.Ambiguous(first, rest)
            | [] ->
                let firstNamed =
                    hosts
                    |> Array.tryPick (fun (_, h) ->
                        match h with
                        | HostVerdict.NameOnly c -> Some c
                        | _ -> None
                    )

                match firstNamed with
                | Some c ->
                    let unsupported =
                        hosts
                        |> Array.choose (fun (t, h) ->
                            match h with
                            | HostVerdict.NoMember -> Some t
                            | _ -> None
                        )

                    TraitPick.NameOnly(c, List.ofArray unsupported)
                | None ->
                    if complete then
                        TraitPick.NoSupport
                    else
                        TraitPick.Incomplete
