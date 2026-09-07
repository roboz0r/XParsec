namespace XParsec.FSharp.SemanticAnalysis

open Vesper
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
    /// an external (TS-manifest) `E.C1` type their node `TyEnum key`, so a single check covers
    /// both.
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
        : struct (Block<ExternalCaseShape> * Block<SemType>) voption =
        match Unification.zonk ctx.Store ty with
        | TyUnion(key, args) ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Union { Cases = cases }) -> ValueSome(struct (cases, args))
            | _ -> ValueNone
        | _ -> ValueNone

    /// The enum owning the case access or pattern at `key`. The node's own `TyEnum` type
    /// resolves every valid case; the stamp is the error-path fallback, where an invalid case
    /// (`E.BadCase`, diagnosed upstream) left the node's type un-pinned.
    let enumKeyAt (ctx: PassContext) (key: NodeKey) (ty: SemType) : TypeKey voption =
        match enumKeyOfTy ctx.Store ty with
        | ValueSome k -> ValueSome k
        | ValueNone -> ResolvedStamps.tryEnumQualifier ctx.Resolution.Resolved key

    /// An enum-case access `E.C1` or `E<int>.C1`: the enum key and the case name.
    [<return: Struct>]
    let (|EnumCaseAccess|_|)
        (ctx: PassContext)
        (key: NodeKey)
        (ty: SemType)
        (e: Expr<SyntaxToken>)
        : struct (TypeKey * string) voption =
        let caseTok =
            match e with
            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 2 -> ValueSome li.Idents.[1]
            | Expr.DotLookup(expr = Expr.TypeApp _; longIdentOrOp = LongIdentOrOp.LongIdent li) when
                li.Idents.Length = 1
                ->
                ValueSome li.Idents.[0]
            | _ -> ValueNone

        match caseTok with
        | ValueSome tok ->
            match enumKeyAt ctx key ty with
            | ValueSome k -> ValueSome(struct (k, ctx.NameOf tok))
            | ValueNone -> ValueNone
        | ValueNone -> ValueNone

    /// An enum-case pattern `| E.C1`: the enum key and the case name.
    [<return: Struct>]
    let (|EnumCasePattern|_|)
        (ctx: PassContext)
        (key: NodeKey)
        (ty: SemType)
        (li: LongIdent<SyntaxToken>)
        : struct (TypeKey * string) voption =
        if li.Idents.Length <> 2 then
            ValueNone
        else
            match enumKeyAt ctx key ty with
            | ValueSome k -> ValueSome(struct (k, ctx.NameOf li.Idents.[1]))
            | ValueNone -> ValueNone

    /// The declaring nominal `TypeKey` of a class/union/record object-argument type, the
    /// `Decl` slot of the `MemberKey` minted for an instance member access. Only called where
    /// `ty` is already guarded on `TyNominal`, so a non-nominal one `failwith`s below.
    let nominalDeclKey (store: TypeStore) (ty: SemType) : TypeKey =
        match Unification.zonk store ty with
        | TyNominal(key, _) -> key
        | other -> failwithf "Elaborate: expected a class/union/record type for a member access, got %A" other
