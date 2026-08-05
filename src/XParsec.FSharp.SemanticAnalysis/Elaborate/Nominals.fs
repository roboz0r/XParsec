namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Nominal-type probes for the Elaborate pass: a node's zonked type, the registry reads
// behind the `Local*` active patterns, and the member lookup every nominal dispatch
// routes through. Everything here reads a type or the registry; nothing builds a TAST
// node.

module internal ElaborateNominals =

    // Public surface for the companion `Elaborate` (type-declaration) module: the
    // entry points it projects member bodies / ctor args / field types from.
    let typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> Unification.zonk ctx.Store (TyVar tv)
        | ValueNone -> TyVar(ctx.NewTypeVar())

    /// The enum `TypeKey` a node's type carries, if it is an enum. Both the
    /// project-local and the external (TS-manifest) `E.C1` arms type their node
    /// `TyEnum key`, so this is the single signal the expression / pattern freeze
    /// arms read to reuse the same `StaticFieldGet` / `TPat.EnumCase` carrier — the
    /// key is the enum's identity whether the cases are emitted locally (object map)
    /// or imported from a TS module.
    let enumKeyOfTy (store: TypeStore) (ty: SemType) : TypeKey voption =
        match Unification.zonk store ty with
        | TyEnum key -> ValueSome key
        | _ -> ValueNone

    [<return: Struct>]
    let (|LocalClass|_|) (ctx: PassContext) (ty: SemType) : ClassTypeInfo voption =
        match Unification.zonk ctx.Store ty with
        | TyClass(key, _) -> TypeRegistry.tryClassByKey ctx.Types key
        | _ -> ValueNone

    [<return: Struct>]
    let (|LocalRecord|_|) (ctx: PassContext) (ty: SemType) : RecordTypeInfo voption =
        match Unification.zonk ctx.Store ty with
        | TyRecord(key, _) -> TypeRegistry.tryRecordByKey ctx.Types key
        | _ -> ValueNone

    [<return: Struct>]
    let (|LocalUnion|_|) (ctx: PassContext) (ty: SemType) : UnionTypeInfo voption =
        match Unification.zonk ctx.Store ty with
        | TyUnion(key, _) -> TypeRegistry.tryUnionByKey ctx.Types key
        | _ -> ValueNone

    /// The enum key for a two-segment `E.C1` access/pattern, binding it once and
    /// collapsing the formerly-separate local-registry and type-signal arms into a
    /// single freeze arm (used by both `ElaborateExpr`'s `StaticFieldGet` and
    /// `ElaboratePatterns`' `TPat.EnumCase`). `enumKeyOfTy` is the canonical signal —
    /// Unification types BOTH project-local and external `E.C1` as `TyEnum key`, so
    /// it alone resolves the valid case; the local registry is consulted only as the
    /// error-path fallback, where an invalid case (`E.BadCase`, already diagnosed
    /// upstream) left the node's type un-pinned. This preserves the prior arms'
    /// behaviour exactly while removing the duplicate arm and the guard re-lookups.
    [<return: Struct>]
    let (|EnumCaseAccess|_|) (ctx: PassContext) (ty: SemType) (li: LongIdent<SyntaxToken>) : TypeKey voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            match enumKeyOfTy ctx.Store ty with
            | ValueSome key -> ValueSome key
            | ValueNone ->
                // The written enum name's own token is the use site — the same position
                // Unification resolved the head at, so the fallback cannot see an enum the
                // canonical `TyEnum` signal could not.
                let useSite =
                    ctx.UseSiteAt(NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprIdent)

                match TypeRegistry.tryEnum ctx.Types useSite (ctx.NameOf li.Idents.[0]) with
                | ValueSome info -> ValueSome info.TypeKey
                | ValueNone -> ValueNone

    /// The declaring nominal `TypeKey` of a class/union/record receiver type — the
    /// `Decl` slot of the `MemberKey` minted for an instance member access. Only
    /// called where the receiver is already known to be nominal (the active patterns /
    /// `InstanceMethodCall` guard on `TyNominal`), so a non-nominal type is an
    /// Elaborate invariant break.
    let nominalDeclKey (store: TypeStore) (ty: SemType) : TypeKey =
        match Unification.zonk store ty with
        | TyNominal(key, _) -> key
        | other -> failwithf "Elaborate: expected a class/union/record receiver for a member access, got %A" other

    /// Resolve the declaring class / union / record by its arity-qualified `SymbolKey`
    /// (`tryClassByKey`/`tryUnionByKey`/`tryRecordByKey`, which read the key's
    /// ``Name`arity`` verbatim), not the bare simple name. An arity-overloaded receiver
    /// (`Fun`2`/`Fun`3`) does not resolve by bare name, so a `simpleName`-keyed lookup
    /// would miss and the call would mis-lower to a `Vesper.Fun::Invoke` function
    /// application. This is the single member-key registry read every nominal member
    /// dispatch routes through — a record's members resolve here on the same path as a
    /// class's or union's. Callers holding the receiver's nominal key must route through
    /// here.
    let tryNominalMemberByKey
        (ctx: PassContext)
        (typeKey: TypeKey)
        (memberName: string)
        : (TypeKey * TypeMemberInfo) voption =
        // The walk (and the declaring-typar surface `LocalMemberKeys.totalMemberKey`
        // needs) is spelled once in `LocalMemberKeys`; this projection drops the typars
        // for the callers here that only need the member and its declaring key.
        match LocalMemberKeys.tryNominalMemberWithTypars ctx typeKey memberName with
        | ValueSome nm -> ValueSome(nm.DeclKey, nm.Member)
        | ValueNone -> ValueNone
