namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// The single home for minting a RESOLVED member's TOTAL `SymbolKey.MemberKey`. Both the
// Elaborate method-call sites (`ElaborateResolve.mk*MethodCall`) and the inline
// trait-call dispatcher (`Inline.substMapper`) mint through here, so the
// local-vs-external branch and the declaring-typar freeze live in ONE place — a lossy
// placeholder method key is unrepresentable by construction.
//
// Public with no `InternalsVisibleTo` (this project has none, cf. `RecordFieldClassifier`)
// so the external-arm overload-discrimination wiring is exercised directly by its unit tests.

module LocalMemberKeys =

    /// A local nominal member resolved by key, carrying the axes `frozenUserMemberKey`
    /// freezes into: the declaring type's canonical `DeclKey`, its own typar binders
    /// (`DeclTypars`), and the member itself.
    [<Struct>]
    type NominalMember =
        {
            DeclKey: TypeKey
            DeclTypars: EqArray<string * TyVarId>
            Member: TypeMemberInfo
        }

    /// The nominal member-by-key walk that ALSO surfaces the declaring type's own typar
    /// binders — the declaring axis `frozenUserMemberKey` freezes the value signature
    /// into. Resolves the declaring class / union / record by its arity-qualified
    /// `TypeKey` (the same three-registry read `tryNominalMemberByKey` projects, which
    /// delegates here so the walk is spelled once). `ValueNone` when no local nominal by
    /// that key carries the member.
    let tryNominalMemberWithTypars (ctx: PassContext) (typeKey: TypeKey) (memberName: string) : NominalMember voption =
        let pick (key: TypeKey) (typeParams: EqArray<string * TyVarId>) (members: TypeMemberInfo[]) =
            match members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m ->
                ValueSome
                    {
                        DeclKey = key
                        DeclTypars = typeParams
                        Member = m
                    }
            | None -> ValueNone

        match TypeRegistry.tryClassByKey ctx.Types typeKey with
        | ValueSome info -> pick info.TypeKey info.TypeParams info.Members
        | ValueNone ->
            match TypeRegistry.tryUnionByKey ctx.Types typeKey with
            | ValueSome info -> pick info.TypeKey info.TypeParams info.Members
            | ValueNone ->
                match TypeRegistry.tryRecordByKey ctx.Types typeKey with
                | ValueSome info -> pick info.TypeKey info.TypeParams info.Members
                | ValueNone -> ValueNone

    /// The ground operand types a genuinely-overloaded EXTERNAL member set is
    /// discriminated by — the external branch's analogue of the local arm's unique
    /// name. `DeclArgs` are the declaring type's type arguments (they substitute the
    /// candidate signature's declaring typars, exactly as `memberParamTypes` reads them
    /// at the application-site pick); `ArgElems` are the call's actual argument-element
    /// types, one per (flattened) parameter position. BOTH are ground by construction
    /// (`externalOperands` refuses to build one otherwise): a non-ground operand makes
    /// the picker's `matchTypes` bind the free leaf and admit every candidate at that
    /// position, so it could no longer tell two same-arity overloads apart.
    [<Struct>]
    type ExternalOperands =
        {
            DeclArgs: SemType[]
            ArgElems: SemType list
        }

    /// Fully ground (no free var / unsubstituted typar / unresolved type-level
    /// computation) — the precondition for operand-type overload discrimination.
    let rec private isGround (store: TypeStore) (t: SemType) : bool =
        match Unification.zonk store t with
        | TyVar _
        | TyUnknown _
        | TyTypar _ -> false
        | t -> SemType.forallChildren (isGround store) t

    /// The type arguments of a (zonked) nominal head — the declaring-type args a member
    /// signature's declaring typars substitute from. Empty for a non-nominal (a
    /// non-generic static declarer, an intrinsic, an unpinned var).
    let nominalArgs (store: TypeStore) (t: SemType) : SemType[] =
        match Unification.zonk store t with
        | TyNominal(_, args) -> EqArray.toArray args
        | _ -> [||]

    /// Build the operand-type discriminator for `totalMemberKey`'s EXTERNAL arm. `declArgs`
    /// are the declaring type's type arguments (from `nominalArgs` of the receiver, the
    /// interface's args, or empty for a non-generic static declarer); `argElems` are the
    /// call's argument-element types. `ValueNone` when any operand type is NOT ground: the
    /// picker cannot then discriminate, so the mint keeps its best-by-arity single
    /// (behaviour-identical to the pre-picker mint) rather than force a wrong pick.
    let externalOperands (store: TypeStore) (declArgs: SemType[]) (argElems: SemType list) : ExternalOperands voption =
        let declArgs = declArgs |> Array.map (Unification.zonk store)
        let argElems = argElems |> List.map (Unification.zonk store)

        if List.forall (isGround store) argElems && Array.forall (isGround store) declArgs then
            ValueSome
                {
                    DeclArgs = declArgs
                    ArgElems = argElems
                }
        else
            ValueNone

    /// THE total `MemberKey` mint for a resolved member `memberName` on declaring type
    /// `declKey`. A resolved member is either LOCAL — frozen with the declaring type's
    /// open typars and the member's real method-typar arity, so two same-name overloads
    /// mint DISTINCT keys — or EXTERNAL. The external arm resolves overload-precisely: a
    /// bare member NAME collapses an overload set, so when the declaring type carries a
    /// GENUINE same-arity overload set (≥2 mapped overloads) AND the caller supplies ground
    /// `operands`, it discriminates by operand type through the SAME application-site picker
    /// (`pickBestOverload`) inference used — otherwise the sole / best-by-arity single is
    /// already the right member and is taken verbatim, byte-identical to the former
    /// `TryLookupMember` mint. `ValueNone` iff the member resolves in NEITHER registry, OR
    /// the picker finds no applicable / no unique-best overload: not a placeholder and not a
    /// crash — the caller turns it into the appropriate diagnostic (a user-facing "type does
    /// not support this member" for a trait dispatch, an internal-error for a post-inference
    /// Elaborate site where the member was already committed).
    let totalMemberKey
        (ctx: PassContext)
        (declKey: TypeKey)
        (memberName: string)
        (operands: ExternalOperands voption)
        : SymbolKey voption =
        // The singular best-by-arity channel — the behaviour-preserving fallback for the
        // 0/1-candidate case and for a site that cannot supply ground operands.
        let singular () =
            match ctx.Provider.TryLookupMember(SymbolKey.Type declKey, memberName) with
            | ValueSome em -> ValueSome(SymbolKey.Member em.Key)
            | ValueNone -> ValueNone

        match tryNominalMemberWithTypars ctx declKey memberName with
        | ValueSome nm ->
            ValueSome(UnificationInferOverload.frozenUserMemberKey ctx.Store nm.DeclKey nm.DeclTypars nm.Member)
        | ValueNone ->
            match ctx.Provider.TryLookupMembers(SymbolKey.Type declKey, memberName) with
            // A provider that models members only singularly (or none): the plural channel
            // is empty, so the singular pick is the sole behaviour-preserving answer.
            | [||] -> singular ()
            // Exactly one overload: the pick is forced and equals `TryLookupMember`'s — the
            // overwhelming majority, including every currently-green external call. Operands
            // are not even consulted, so a non-ground call site is unaffected here.
            | [| only |] -> ValueSome(SymbolKey.Member only.Key)
            // ≥2 overloads sharing this name: a genuine set. Discriminate by operand type
            // when the site supplied ground operands; a `ValueNone` pick (none-applicable /
            // ambiguous) flows to the caller's diagnostic, never a wrong key. Without ground
            // operands, keep the best-by-arity single (the pre-picker behaviour).
            | members ->
                match operands with
                | ValueSome ops ->
                    match UnificationInferOverload.pickBestOverload ctx ops.DeclArgs members ops.ArgElems with
                    | ValueSome em -> ValueSome(SymbolKey.Member em.Key)
                    | ValueNone -> ValueNone
                | ValueNone -> singular ()
