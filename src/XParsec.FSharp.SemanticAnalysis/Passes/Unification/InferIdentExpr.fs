namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch
open UnificationInferRecordAccess

module internal UnificationInferIdentExpr =

    let rec inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        // A multi-segment LongIdent whose head is a local binding is a
        // record-field access chain (`r.X.Y`), not a qualified name — the
        // parser rides these inside a single `Expr.LongIdentOrOp` rather
        // than emitting `Expr.DotLookup`.
        match e with
        // `(+)` used as a value: resolve the operator's compiled name through
        // the provider, instantiating its scheme like any external symbol.
        // Freeze projects this to `External("op_Addition", …)`.
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name ->
                // The ambient prelude leg resolves a contract's `[<AutoOpen>]`
                // operator module.
                match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
                | ValueSome sym ->
                    let ty = sym.Instantiate ctx.CurrentLevel
                    // The provider hands back the *built-in* operator scheme. If the
                    // operands turn out to be a project-local nominal with its own
                    // `static member (+)`, F# binds the value to that member instead —
                    // a type-directed decision we can't make until every operand is
                    // ground, so enqueue the node for `resolveOperatorValues` to settle
                    // post-walk (it scans all operands; `ty`'s TyVars zonk to the
                    // operand types once the consuming context has unified them).
                    ctx.OperatorValueSites.Add { Node = key; Name = name; Ty = ty }
                    ty
                | ValueNone -> errorTy ctx key (sprintf "Operator '%s' is not available from the symbol provider" name)
            | ValueNone -> TyVar(freshTyVar ctx)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx key li
        // Two-segment qualified reference whose head is *not* a local binding:
        // `Math.Pi` / `Lst.Empty` / `Result2.Ok`.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2 && not (ctx.Bindings.Binding.ContainsKey key)
            ->
            let headName = ctx.NameOf li.Idents.[0]
            let tailName = ctx.NameOf li.Idents.[1]

            let tryStaticMember (typeParams: EqArray<string * TypeVar>) (members: TypeMemberInfo[]) =
                match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = tailName) with
                | Some m ->
                    let _, subst = freshNamedInstance ctx typeParams
                    ValueSome(substituteWith subst m.Type)
                | None -> ValueNone

            // Class static member takes priority over union static member which
            // takes priority over a union ctor — preserves the original cascade
            // order so a static member shadows the not-a-case diagnostic.
            let classHit =
                match ctx.Types.Class.TryGetValue headName with
                | true, info -> tryStaticMember info.TypeParams info.Members
                | false, _ -> ValueNone

            match classHit with
            | ValueSome ty -> ty
            | ValueNone ->
                match ctx.Types.Union.TryGetValue headName with
                | true, info ->
                    match tryStaticMember info.TypeParams info.Members with
                    | ValueSome ty -> ty
                    | ValueNone ->
                        // Qualified ctor reference `Result2.Ok` — via the union
                        // registry, bypassing the CtorIndex ambiguity check.
                        match resolveQualifiedCtor ctx headName tailName with
                        | ValueSome info -> ctorType ctx info
                        | ValueNone -> errorTy ctx key (sprintf "Union '%s' has no case '%s'" headName tailName)
                | false, _ ->
                    // Qualified external union case (`Option.Some`) — the head is
                    // an external union, not a local one (Gap 2 Layer B).
                    match tryExternalCtorType ctx (ValueSome headName) tailName with
                    | ValueSome t -> t
                    | ValueNone -> inferIdentDefault ctx e key
        | _ -> inferIdentDefault ctx e key

    /// Resolution order: local binding map, then provider, then `Class`-name
    /// and `Union`-case registries (the latter two only for single-segment names).
    and inferIdentDefault (ctx: PassContext) (e: Expr<SyntaxToken>) (key: NodeKey) : SemType =
        match ctx.Bindings.Binding.TryGetValue key with
        | ValueSome rb -> instantiateBinding ctx rb
        | ValueNone ->
            // Provider first — provider hits beat ctor-name resolution
            // when both exist (a let-bound `Ok` would have a Binding entry
            // and never reach here). Bare single-segment idents absent
            // from the provider fall to the ctor registry.
            let name = qualifiedNameOf ctx e

            match OpenScope.tryResolve ctx.Resolution.OpenScope ctx.Provider.TryLookup name with
            | ValueSome sym -> sym.Instantiate ctx.CurrentLevel
            | ValueNone ->

                match tryExternalStaticLongIdent ctx key e with
                | ValueSome ty -> ty
                | ValueNone ->
                    let singleSegName =
                        match e with
                        | Expr.Ident t -> ValueSome(ctx.NameOf t)
                        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                            ValueSome(ctx.NameOf li.Idents.[0])
                        | _ -> ValueNone

                    match singleSegName with
                    | ValueSome n ->
                        let info, count = resolveCtorName ctx n

                        match info with
                        | ValueSome i -> ctorType ctx i
                        | ValueNone when count >= 2 ->
                            errorTy
                                ctx
                                key
                                (sprintf
                                    "Ambiguous constructor '%s'; declared in %d union types — add a qualifier or annotation"
                                    n
                                    count)
                        | ValueNone ->
                            // External union case ctor (`Some` / `None` from a
                            // referenced package, in scope via `open`): typed as
                            // `field… → TyUnion(union, …)` so `inferApp` flows the
                            // application through the normal function arm and the
                            // bare nullary form (`None`) lands as the union value.
                            match tryExternalCtorType ctx ValueNone n with
                            | ValueSome t -> t
                            | ValueNone ->
                                // Class-name-as-function: `Point(3, 4)` parses as
                                // `Expr.App (Expr.Ident "Point", ...)`. Return the
                                // ctor as a function value so `inferApp` types the
                                // call through the normal function arm.
                                classCtorAsFunction ctx n
                    | ValueNone ->
                        // A multi-segment qualified name that resolved to nothing.
                        // If its qualifier names a known external union/record, the
                        // tail is a missing member (`Result.Nope` / `Option.Nope`):
                        // diagnose it rather than minting a fresh TyVar that unifies
                        // with anything and hides the typo deep in codegen — the
                        // symmetric front-end miss to `resolveFieldStep`'s instance-
                        // member arm.
                        match tryQualifiedExternalMemberMiss ctx e with
                        | ValueSome(qual, memberName) ->
                            errorTy ctx key (sprintf "Type '%s' has no value or member '%s'" qual memberName)
                        | ValueNone -> TyVar(freshTyVar ctx)

    and qualifiedNameOf (ctx: PassContext) (e: Expr<SyntaxToken>) : string =
        match e with
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> li.Idents |> Seq.map ctx.NameOf |> String.concat "."
        // `A.B.(+)` — the qualified operator form NameResolution resolved through
        // the provider; rebuild the same compiled name
        // (`A.B.op_Addition`) so the provider round-trip here matches its key.
        | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
            match OperatorNames.qualifiedOpName ctx.NameOf li idOp with
            | ValueSome n -> n
            | ValueNone -> ctx.NameOf(CstKeys.firstTokenOfExpr e)
        | _ -> ctx.NameOf(CstKeys.firstTokenOfExpr e)

    /// `ClassName<'args>.Member` where the receiver is an *explicitly* instantiated
    /// **local** class/union (`Set<'T>.Empty`, `Box<'T>.Tag`). The bare folded
    /// `ClassName.Member` form resolves its static member in `inferIdent`, but the
    /// `<'args>`-bearing form parses as `DotLookup(TypeApp(ClassName, <'args>),
    /// .Member)`; inferring the `TypeApp` receiver as a value yields the ctor
    /// function type (→ a spurious "non-class" member-read error). Resolve the
    /// static member directly here, mirroring `inferIdent`'s `tryStaticMember`: a
    /// fresh type-param instance + substitution; the surrounding context (the
    /// member's annotated return type) pins the instantiation, so the explicit
    /// `<'args>` aren't separately unified (matching the folded form, which has
    /// none). The applied static-*method* form (`ClassName<'args>.M args`) is
    /// handled separately by the App arm.
    and tryLocalTypeAppStaticMember
        (ctx: PassContext)
        (recv: Expr<SyntaxToken>)
        (memberTok: SyntaxToken)
        : SemType voption =
        match recv with
        | Expr.TypeApp(expr = classExpr) ->
            let classNameOpt =
                match classExpr with
                | Expr.Ident t -> ValueSome(ctx.NameOf t)
                | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
                    ValueSome(ctx.NameOf li.Idents.[0])
                | _ -> ValueNone

            match classNameOpt with
            | ValueNone -> ValueNone
            | ValueSome className ->
                let memberName = ctx.NameOf memberTok

                let resolve (typeParams: EqArray<string * TypeVar>) (members: TypeMemberInfo[]) =
                    match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = memberName) with
                    | Some m ->
                        let _, subst = freshNamedInstance ctx typeParams
                        ValueSome(substituteWith subst m.Type)
                    | None -> ValueNone

                match ctx.Types.Class.TryGetValue className with
                | true, info -> resolve info.TypeParams info.Members
                | false, _ ->
                    match ctx.Types.Union.TryGetValue className with
                    | true, info -> resolve info.TypeParams info.Members
                    | false, _ -> ValueNone
        | _ -> ValueNone
