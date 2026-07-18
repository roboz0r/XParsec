namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.SemanticAnalysis.Passes

// The single home for minting a RESOLVED member's TOTAL `SymbolKey.MemberKey`. Both the
// Elaborate method-call sites (`ElaborateResolve.mk*MethodCall`) and the inline
// trait-call dispatcher (`Inline.resolveTraitCall`) mint through here, so the
// local-vs-external branch and the declaring-typar freeze live in ONE place — a lossy
// placeholder method key is unrepresentable by construction.

module internal LocalMemberKeys =

    /// A local nominal member resolved by key, carrying the axes `frozenUserMemberKey`
    /// freezes into: the declaring type's canonical `DeclKey`, its own typar binders
    /// (`DeclTypars`), and the member itself.
    [<Struct>]
    type NominalMember =
        {
            DeclKey: TypeKey
            DeclTypars: EqArray<string * TypeVar>
            Member: TypeMemberInfo
        }

    /// The nominal member-by-key walk that ALSO surfaces the declaring type's own typar
    /// binders — the declaring axis `frozenUserMemberKey` freezes the value signature
    /// into. Resolves the declaring class / union / record by its arity-qualified
    /// `TypeKey` (the same three-registry read `tryNominalMemberByKey` projects, which
    /// delegates here so the walk is spelled once). `ValueNone` when no local nominal by
    /// that key carries the member.
    let tryNominalMemberWithTypars (ctx: PassContext) (typeKey: TypeKey) (memberName: string) : NominalMember voption =
        let pick (key: TypeKey) (typeParams: EqArray<string * TypeVar>) (members: TypeMemberInfo[]) =
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

    /// THE total `MemberKey` mint for a resolved member `memberName` on declaring type
    /// `declKey`. A resolved member is either LOCAL — frozen with the declaring type's
    /// open typars and the member's real method-typar arity, so two same-name overloads
    /// mint DISTINCT keys — or EXTERNAL, whose provider entry already carries a total
    /// `MemberKey`. `ValueNone` iff the member resolves in NEITHER registry: not a
    /// placeholder and not a crash — the caller turns it into the appropriate diagnostic
    /// (a user-facing "type does not support this member" for a trait dispatch, an
    /// internal-error for a post-inference Elaborate site where the member was already
    /// committed).
    let totalMemberKey (ctx: PassContext) (declKey: TypeKey) (memberName: string) : SymbolKey voption =
        match tryNominalMemberWithTypars ctx declKey memberName with
        | ValueSome nm -> ValueSome(UnificationInferOverload.frozenUserMemberKey nm.DeclKey nm.DeclTypars nm.Member)
        | ValueNone ->
            match ctx.Provider.TryLookupMember(SymbolKey.Type declKey, memberName) with
            | ValueSome em -> ValueSome(SymbolKey.Member em.Key)
            | ValueNone -> ValueNone
