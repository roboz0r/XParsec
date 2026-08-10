namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// Minting a resolved member's `SymbolKey.MemberKey`: the local-vs-external branch and the
// declaring-typar freeze, for the Elaborate method-call sites and the inline trait-call
// dispatcher.

module LocalMemberKeys =

    [<Struct>]
    type NominalMember =
        {
            DeclKey: TypeKey
            DeclTypars: EqArray<string * TyVarId>
            Member: TypeMemberInfo
        }

    /// The declaring type's own typar bound variables come back alongside the member: they are the
    /// declaring axis a value signature freezes against.
    let tryNominalMemberWithTypars (ctx: PassContext) (typeKey: TypeKey) (memberName: string) : NominalMember voption =
        match TypeRegistry.tryNominalByKey ctx.Types typeKey with
        | ValueSome decl ->
            match decl.Members |> Array.tryFind (fun m -> m.Name = memberName) with
            | Some m ->
                ValueSome
                    {
                        DeclKey = decl.TypeKey
                        DeclTypars = decl.TypeParams
                        Member = m
                    }
            | None -> ValueNone
        | ValueNone -> ValueNone

    /// The ground operand types a genuinely-overloaded EXTERNAL member set is discriminated
    /// by. `DeclArgs` substitute the candidate signature's declaring typars; `ArgElems` are
    /// the call's argument-element types, one per (flattened) parameter position.
    [<Struct>]
    type ExternalOperands =
        {
            DeclArgs: SemType[]
            ArgElems: SemType list
        }

    /// Empty for a non-nominal: a non-generic static declarer, an intrinsic, an unpinned var.
    let nominalArgs (store: TypeStore) (t: SemType) : SemType[] =
        match Unification.zonk store t with
        | TyNominal(_, args) -> EqArray.toArray args
        | _ -> [||]

    /// `ValueNone` when any operand type is NOT ground: the picker cannot then discriminate,
    /// so the mint keeps its best-by-arity single rather than force a wrong pick.
    let externalOperands (store: TypeStore) (declArgs: SemType[]) (argElems: SemType list) : ExternalOperands voption =
        let declArgs = declArgs |> Array.map (Unification.zonk store)
        let argElems = argElems |> List.map (Unification.zonk store)

        // Ground is the precondition for operand-type overload discrimination: a free leaf
        // binds against every candidate, so it tells no two same-arity overloads apart.
        if
            List.forall (SemTypeQuery.isGround store) argElems
            && Array.forall (SemTypeQuery.isGround store) declArgs
        then
            ValueSome
                {
                    DeclArgs = declArgs
                    ArgElems = argElems
                }
        else
            ValueNone

    /// A LOCAL member freezes with the declaring type's open typars and its own method-typar
    /// arity, so two same-name overloads mint DISTINCT keys; an EXTERNAL one is picked by
    /// operand type. `ValueNone` (unresolved / no unique best) is the caller's diagnostic.
    let totalMemberKey
        (ctx: PassContext)
        (declKey: TypeKey)
        (memberName: string)
        (operands: ExternalOperands voption)
        : SymbolKey voption =
        // Best-by-arity single: the fallback for a site that cannot supply ground operands.
        let singular () =
            match ctx.Provider.TryLookupMember(SymbolKey.Type declKey, memberName) with
            | ValueSome em -> ValueSome(SymbolKey.Member em.Key)
            | ValueNone -> ValueNone

        match tryNominalMemberWithTypars ctx declKey memberName with
        | ValueSome nm ->
            ValueSome(UnificationInferOverload.frozenUserMemberKey ctx.Store nm.DeclKey nm.DeclTypars nm.Member)
        | ValueNone ->
            match ctx.Provider.TryLookupMembers(SymbolKey.Type declKey, memberName) with
            // A provider that models this member only singularly (or not at all).
            | EqEmpty -> singular ()
            | EqOne only -> ValueSome(SymbolKey.Member only.Key)
            // ≥2 overloads sharing this name: a genuine set. A `ValueNone` pick
            // (none-applicable / ambiguous) is left for the caller to diagnose, never a wrong key.
            | members ->
                match operands with
                | ValueSome ops ->
                    match UnificationInferOverload.pickBestOverload ctx ops.DeclArgs members ops.ArgElems with
                    | ValueSome em -> ValueSome(SymbolKey.Member em.Key)
                    | ValueNone -> ValueNone
                | ValueNone -> singular ()
