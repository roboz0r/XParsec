namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes

// Nominal-type probes for the Elaborate pass: a node's zonked type, the registry reads
// behind the `Local*` active patterns, and nominal member lookup. Everything here reads a
// type or the registry; nothing builds a TAST node.

module internal ElaborateNominals =

    let typeOfKey (ctx: PassContext) (key: NodeKey) : SemType =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> Unification.zonk ctx.Store (TyVar tv)
        | ValueNone -> TyVar(ctx.NewTypeVar())

    /// The enum `TypeKey` a node's type carries, if it is an enum. Both a project-local and
    /// an external (TS-manifest) `E.C1` type their node `TyEnum key`, so one signal answers
    /// for both.
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

    /// A provider-published union: its case shapes and the type arguments `ty` is
    /// instantiated at.
    [<return: Struct>]
    let (|ExternalUnion|_|)
        (ctx: PassContext)
        (ty: SemType)
        : struct (EqArray<ExternalCaseShape> * EqArray<SemType>) voption =
        match Unification.zonk ctx.Store ty with
        | TyUnion(key, args) ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Union(cases = cases)) -> ValueSome(struct (cases, args))
            | _ -> ValueNone
        | _ -> ValueNone

    /// The enum key for a two-segment `E.C1` access/pattern. The node's own `TyEnum` type
    /// resolves every valid case; the local registry is the error-path fallback only, where
    /// an invalid case (`E.BadCase`, diagnosed upstream) left the node's type un-pinned.
    [<return: Struct>]
    let (|EnumCaseAccess|_|) (ctx: PassContext) (ty: SemType) (li: LongIdent<SyntaxToken>) : TypeKey voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            match enumKeyOfTy ctx.Store ty with
            | ValueSome key -> ValueSome key
            | ValueNone ->
                // The written enum name's own token is the use site.
                let useSite =
                    ctx.UseSiteAt(NodeKey.ofToken (CstKeys.firstTokenOfLongIdent li) NodeKind.ExprIdent)

                match TypeRegistry.tryEnum ctx.Types useSite (ctx.NameOf li.Idents.[0]) with
                | ValueSome info -> ValueSome info.TypeKey
                | ValueNone -> ValueNone

    /// The declaring nominal `TypeKey` of a class/union/record object-argument type, the
    /// `Decl` slot of the `MemberKey` minted for an instance member access. Only called where
    /// `ty` is already guarded on `TyNominal`, so a non-nominal one `failwith`s below.
    let nominalDeclKey (store: TypeStore) (ty: SemType) : TypeKey =
        match Unification.zonk store ty with
        | TyNominal(key, _) -> key
        | other -> failwithf "Elaborate: expected a class/union/record type for a member access, got %A" other
