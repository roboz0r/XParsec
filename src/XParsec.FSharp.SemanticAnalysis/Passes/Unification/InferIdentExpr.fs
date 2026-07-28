namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
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

    let rec inferIdent (ctx: PassContext) (e: Expr<SyntaxToken>) (node: NodeSite) : SemType =
        // A multi-segment LongIdent whose head is a local binding is a
        // record-field access chain (`r.X.Y`), not a qualified name — the
        // parser rides these inside a single `Expr.LongIdentOrOp` rather
        // than emitting `Expr.DotLookup`.
        match e with
        // `(+)` used as a value: resolve the operator's compiled name through the
        // provider and instantiate its scheme like any other external symbol. Elaborate
        // projects this to `External("op_Addition", …)`, and `Passes.InlineExpansion`
        // eta-reifies that into `fun a b -> (+) a b` and splices the operator's contract
        // body at the call head it minted — which is what binds the value to a
        // project-local nominal's OWN `static member (+)` when that is what the operands
        // are: the body's static-opt base is an SRTP trait call, and a nominal receiver
        // dispatches to its member. Nothing type-directed is needed HERE; the trait call
        // in the contract body already IS the type-directed decision, made once the
        // operands are ground.
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            match Desugar.symbolicOpCompiledName op.Token with
            | ValueSome name ->
                // NameResolution resolved the operator's compiled name (opens-aware,
                // ambient-prelude leg included) and stamped its `ExternalSymbol` here;
                // instantiate the scheme by key rather than re-resolving.
                match ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key with
                | ValueSome sym -> ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel
                | ValueNone ->
                    errorTy ctx node.Tok (sprintf "Operator '%s' is not available from the symbol provider" name)
            | ValueNone -> TyVar(freshTyVar ctx)
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length > 1
            && ctx.Bindings.Binding.ContainsKey(NodeKey.ofToken li.Idents.[0] NodeKind.ExprIdent)
            ->
            inferLongIdentFieldChain ctx node li
        // An EXTERNAL enum-case access `E.C1` — the head names a TS-manifest
        // (provider) enum, not a project-local one (which the next arm's
        // `ctx.Types.Enum` lookup handles). Types as the nominal `TyEnum key`, the
        // external analogue of the local-enum arm below; the key is shared with an
        // `(x: E)` annotation (`Translate.tryResolveExternalType`), so the two unify.
        // Guarded ahead of the general two-segment cascade so an external enum head
        // never falls through to the class/union static path.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) & Stamped ctx.Resolution.ExternalEnumCaseStamp node.Key enumKey when
            not (ctx.Bindings.Binding.ContainsKey node.Key)
            ->
            TyEnum enumKey
        // A project-local enum-case access `E.C1`: the head names a project-local
        // enum (a separate registry, so no class/union collision). Types as the enum
        // nominal `TyEnum Key`, NOT its underlying int/string; an unknown case is a
        // resolution error (the enum analogue of "Union 'U' has no case 'C'"). Sibling
        // of the external-enum arm above and of `InferPat`'s enum-pattern arm, kept
        // ahead of the general cascade so an enum head never falls into it.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2
            && not (ctx.Bindings.Binding.ContainsKey node.Key)
            && (TypeRegistry.tryEnum ctx.Types (ctx.UseSiteAt node.Key) (ctx.NameOf li.Idents.[0])).IsSome
            ->
            let einfo =
                (TypeRegistry.tryEnum ctx.Types (ctx.UseSiteAt node.Key) (ctx.NameOf li.Idents.[0])).Value

            let caseName = ctx.NameOf li.Idents.[1]

            if einfo.HasCase caseName then
                TyEnum einfo.TypeKey
            else
                errorTy ctx node.Tok (sprintf "Enum '%s' has no case '%s'" einfo.Name caseName)
        // Two-segment qualified reference whose head is *not* a local binding:
        // `Math.Pi` / `Lst.Empty` / `Result2.Ok`.
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 2 && not (ctx.Bindings.Binding.ContainsKey node.Key)
            ->
            let headName = ctx.NameOf li.Idents.[0]
            let tailName = ctx.NameOf li.Idents.[1]

            let tryStaticMember (typeParams: EqArray<string * TyVarId>) (members: TypeMemberInfo[]) =
                match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = tailName) with
                | Some m ->
                    let _, subst = freshNamedInstance ctx typeParams
                    ValueSome(substituteWith ctx.Store subst m.Type)
                | None -> ValueNone

            // Class static member takes priority over union static member which
            // takes priority over a union ctor — preserves the original cascade
            // order so a static member shadows the not-a-case diagnostic.
            // The qualifier resolves AS SEEN FROM this node: a class / union declared below
            // it does not answer for the name, so `Foo.Bar` above `type Foo` falls through
            // to the external cascade and lands unresolved — the same miss NameResolution
            // already diagnosed on the qualifier.
            let classHit =
                match TypeRegistry.tryClass ctx.Types (ctx.UseSiteAt node.Key) headName with
                | ValueSome info -> tryStaticMember info.TypeParams info.Members
                | ValueNone -> ValueNone

            match classHit with
            | ValueSome ty -> ty
            | ValueNone ->
                match TypeRegistry.tryUnionBare ctx.Types (ctx.UseSiteAt node.Key) headName with
                | ValueSome info ->
                    match tryStaticMember info.TypeParams info.Members with
                    | ValueSome ty -> ty
                    | ValueNone ->
                        // Qualified ctor reference `Result2.Ok` — via the union
                        // registry, bypassing the CtorIndex ambiguity check.
                        match resolveQualifiedCtor ctx (ctx.UseSiteAt node.Key) headName tailName with
                        | ValueSome info -> ctorType ctx info
                        | ValueNone -> errorTy ctx node.Tok (sprintf "Union '%s' has no case '%s'" headName tailName)
                | ValueNone ->
                    // Qualified external union case (`Option.Some`) — the head is
                    // an external union, not a local one. NameResolution stamped
                    // the resolved case at this node's key.
                    match tryExternalCtorType ctx node.Key with
                    | ValueSome t -> t
                    | ValueNone -> inferIdentDefault ctx e node
        | _ -> inferIdentDefault ctx e node

    /// Resolution order: local binding map, then provider, then `Class`-name
    /// and `Union`-case registries (the latter two only for single-segment names).
    and inferIdentDefault (ctx: PassContext) (e: Expr<SyntaxToken>) (node: NodeSite) : SemType =

        match ctx.Bindings.Binding.TryGetValue node.Key with
        | ValueSome rb -> instantiateBinding ctx rb
        | ValueNone ->
            // Provider first — provider hits beat ctor-name resolution
            // when both exist (a let-bound `Ok` would have a Binding entry
            // and never reach here). Bare single-segment idents absent
            // from the provider fall to the ctor registry. NameResolution
            // resolved this spelling (opens-aware) and stamped its
            // `ExternalSymbol`; instantiate the scheme by key.
            match ctx.Resolution.ExternalSymbolStamp.TryGetValue node.Key with
            | ValueSome sym -> ExternalSymbols.instantiateSymbol ctx.Store sym ctx.CurrentLevel
            | ValueNone ->

                match tryExternalStaticLongIdent ctx node.Key e with
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
                        let info, count = resolveCtorName ctx (ctx.UseSiteAt node.Key) n

                        match info with
                        | ValueSome i -> ctorType ctx i
                        | ValueNone when count >= 2 ->
                            errorTy
                                ctx
                                node.Tok
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
                            match tryExternalCtorType ctx node.Key with
                            | ValueSome t -> t
                            | ValueNone ->
                                // Class-name-as-function: `Point(3, 4)` parses as
                                // `Expr.App (Expr.Ident "Point", ...)`. Return the
                                // ctor as a function value so `inferApp` types the
                                // call through the normal function arm.
                                classCtorAsFunction ctx (ctx.UseSiteAt node.Key) n
                    | ValueNone ->
                        // A qualified name. `A.Point(3, 4)` — a class named through the module
                        // holding it — is a ctor reference exactly as the bare `Point(3, 4)`
                        // above is: the head names a TYPE, so it resolves through the type
                        // registry, not as a value.
                        let localCtor =
                            match e with
                            | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
                                tryWrittenClassCtorAsFunction ctx (ctx.UseSiteAt node.Key) (ctx.WrittenTypeNameOf li)
                            | _ -> ValueNone

                        match localCtor with
                        | ValueSome ty -> ty
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
                                errorTy ctx node.Tok (sprintf "Type '%s' has no value or member '%s'" qual memberName)
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

                let resolve (typeParams: EqArray<string * TyVarId>) (members: TypeMemberInfo[]) =
                    match members |> Array.tryFind (fun m -> m.IsStatic && m.Name = memberName) with
                    | Some m ->
                        let _, subst = freshNamedInstance ctx typeParams
                        ValueSome(substituteWith ctx.Store subst m.Type)
                    | None -> ValueNone

                let useSite = ctx.UseSiteAt(CstKeys.ofExpr recv)

                match TypeRegistry.tryClass ctx.Types useSite className with
                | ValueSome info -> resolve info.TypeParams info.Members
                | ValueNone ->
                    match TypeRegistry.tryUnionBare ctx.Types useSite className with
                    | ValueSome info -> resolve info.TypeParams info.Members
                    | ValueNone -> ValueNone
        | _ -> ValueNone
