namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionContainers
open NameResolutionLongIdent
open NameResolutionTypeRefStamp

// Scope tracking and ident-use resolution for NameResolution. A name denoting a binding of
// this file is bound in `ctx.Bindings.Binding` at its use-site key; any other name resolves
// through `NameResolutionLongIdent` and stamps a `ctx.Resolution` table for what it denotes.

module NameResolutionScope =

    /// A name the walk has bound: an expression or type-body binding. A module-level `let` is
    /// never here; it resolves through the ranked environment (`LocalModuleMembers`).
    [<Struct; NoComparison>]
    type ScopeBinding = { Site: NodeKey; IsMutable: bool }

    type Scope = Map<string, ScopeBinding>

    let scopeBinding (site: NodeKey) (isMutable: bool) : ScopeBinding = { Site = site; IsMutable = isMutable }

    /// Bind a use-site key to the binding it references, with `IsInline` false: only a
    /// declaration writes it.
    let private bindUseSite (ctx: PassContext) (key: NodeKey) (bindingSite: NodeKey) (isMutable: bool) : unit =
        ctx.Bindings.Binding.Set(
            key,
            {
                BindingSite = bindingSite
                IsInline = false
                IsMutable = isMutable
            }
        )

    /// The stamps a resolution feeds beside `Resolved`. A local module value keyed at `key`
    /// binds it as a plain local ident is bound.
    let private stampItem (ctx: PassContext) (key: NodeKey) (item: ResolvedItem) : unit =
        match item with
        | ResolvedItem.Value(ResolvedValue.External sym) ->
            // Both channels or neither: with only the symbol the ref freezes to a KEYLESS
            // `External`, and inline bodies are spliced by key, so the body is lost.
            ctx.Resolution.ExternalValue.Set(key, sym.Key)
            ctx.Resolution.ExternalSymbolStamp.Set(key, sym)
        | ResolvedItem.Value(ResolvedValue.Local m) -> bindUseSite ctx key m.BindingSite m.IsMutable
        // A referenced class in expression position is a ctor-sugar application
        // (`InvalidOperationException "x"`, `System.Exception "x"`); a generic one is
        // stamped at its exact arity by the enclosing `TypeApp` visit instead.
        | ResolvedItem.Ctor(ResolvedTypeRef.External(typeKey, _)) -> ctx.Resolution.ResolvedType.Set(key, typeKey)
        | ResolvedItem.Ctor(ResolvedTypeRef.Local _)
        | ResolvedItem.Value _
        | ResolvedItem.UnionCase _
        | ResolvedItem.EnumCase _
        | ResolvedItem.Type _
        | ResolvedItem.StaticMember _
        | ResolvedItem.ModuleOrNamespace _
        | ResolvedItem.AmbiguousCase _
        | ResolvedItem.AmbiguousTypeArity _
        | ResolvedItem.Unresolved _ -> ()

    /// A type whose member misses are Unification's to report, with the precise `NoCase` /
    /// `NoMember`, so no report is owed here.
    let private missReportedDownstream (owner: ResolvedTypeRef) : bool =
        match owner with
        | ResolvedTypeRef.Local claim ->
            match claim.Kind with
            | TypeDeclKind.Union
            | TypeDeclKind.Record
            | TypeDeclKind.Enum -> true
            | TypeDeclKind.Class
            | TypeDeclKind.Abbreviation
            | TypeDeclKind.IntrinsicBinding -> false
        | ResolvedTypeRef.External(_, shape) ->
            match shape with
            | ExternalTypeShape.Union _
            | ExternalTypeShape.Record _
            | ExternalTypeShape.Enum _ -> true
            | ExternalTypeShape.Class _
            | ExternalTypeShape.Intrinsic _
            | ExternalTypeShape.IntrinsicInterface _
            | ExternalTypeShape.Abbrev _
            | ExternalTypeShape.Unmodelled _ -> false

    /// The diagnostic a resolution in expression position owes. A `Ctor` is a value; any
    /// other type name is not, except a generic the enclosing `TypeApp` applied.
    let private reportExpr (ctx: PassContext) (e: Expr<SyntaxToken>) (names: string[]) (r: Resolution) : unit =
        let tok = CstKeys.firstTokenOfExpr e
        let key = CstKeys.ofExpr e
        let written = String.concat "." names

        let unresolved () =
            match names.Length with
            | 1 -> ctx.Report(tok, Kind.Message(sprintf "Unresolved identifier: %s" written))
            | _ -> ctx.Report(tok, Kind.UnresolvedQualifiedName written)

        match r.Item with
        | ResolvedItem.Unresolved {
                                      Segment = segment
                                      Within = ResolutionScope.Type owner
                                  } ->
            if not (missReportedDownstream owner) then
                ctx.Report(tok, Kind.NoMember(owner.Key.Name, MemberNoun.ValueOrMember, segment))
        | ResolvedItem.Unresolved _ ->
            // The printf family is a front-end intrinsic, typed from its format string
            // rather than a provider symbol, so no contract declares it.
            if (PrintfSpec.tryFamily written).IsNone then
                unresolved ()
        | ResolvedItem.UnionCase(case, true) ->
            ctx.Report(tok, Kind.RequireQualifiedAccessCase(case.UnionName, case.CaseName))
        | ResolvedItem.AmbiguousCase(name, claims) -> ctx.Report(tok, Kind.AmbiguousConstructor(name, claims.Length))
        | ResolvedItem.AmbiguousTypeArity(name, arities) -> ctx.Report(tok, Kind.AmbiguousTypeArity(name, arities))
        | ResolvedItem.ModuleOrNamespace _ -> unresolved ()
        | ResolvedItem.Ctor _ -> ()
        | ResolvedItem.Type(ResolvedTypeRef.Local _) -> unresolved ()
        | ResolvedItem.Type(ResolvedTypeRef.External(_, shape)) when shape.TyparArity <> 0 -> unresolved ()
        // A member chain on a resolved item is resolved by no later pass yet, except the field
        // chain on a local module value, which is anchored like a lexical binding.
        | ResolvedItem.Value(ResolvedValue.Local _) -> ()
        | _ when r.Rest < names.Length -> unresolved ()
        | _ -> ()

    /// Resolve a name in expression position that is not lexically bound, stamp what it
    /// denotes at `e`'s key, and report what it owes.
    let private resolveExprNames
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (idents: ImmutableArray<SyntaxToken>)
        : unit =
        let key = CstKeys.ofExpr e
        let names = [| for id in idents -> ctx.NameOf id |]
        let r = resolveExpr ctx (ctx.UseSiteAt key) names
        ctx.Resolution.Resolved.Set(key, r.Item)

        match r.Item with
        | ResolvedItem.Value(ResolvedValue.Local m) when r.Rest < names.Length ->
            // A module-level value of this file anchoring a field chain (`v.X.Y`): keyed on the
            // anchor token, as a lexical anchor is, so the chain reads the anchor's type there.
            bindUseSite ctx (NodeKey.ofToken idents.[0] NodeKind.ExprIdent) m.BindingSite m.IsMutable
        | item -> stampItem ctx key item

        reportExpr ctx e names r

    /// The written type name an `Expr.TypeApp`'s applied expression spells, when it could
    /// denote a type; `ValueNone` for shapes that never can (e.g. an applied expression).
    let private typeAppTypeName (ctx: PassContext) (expr: Expr<SyntaxToken>) : WrittenTypeName voption =
        match expr with
        | Expr.Ident tok -> ValueSome(WrittenTypeName.bare (ctx.NameOf tok))
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) -> ValueSome(ctx.WrittenTypeNameOf li)
        | _ -> ValueNone

    let rec private lookupLexical (scope: Scope list) (name: string) : ScopeBinding voption =
        match scope with
        | [] -> ValueNone
        | innermost :: enclosing ->
            match Map.tryFind name innermost with
            | Some bs -> ValueSome bs
            | None -> lookupLexical enclosing name

    let private resolveIdent (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) (tok: SyntaxToken) =
        match lookupLexical scope (ctx.NameOf tok) with
        | ValueSome b -> bindUseSite ctx (CstKeys.ofExpr e) b.Site b.IsMutable
        | ValueNone -> resolveExprNames ctx e (ImmutableArray.Create tok)

    /// True if `name` is a ctor reference in pattern position: uppercase-leading (per
    /// the F# spec) AND a case-registry hit, so an unrelated uppercase bound variable still
    /// binds. Empty strings (virtual tokens) never match.
    let private isCtorName (ctx: PassContext) (useSite: UseSite) (name: string) : bool =
        name.Length > 0
        && System.Char.IsUpper name.[0]
        && (TypeRegistry.isCaseName ctx.Types useSite name
            || not (externalCasesInScope ctx useSite name).IsEmpty)

    /// Every (name, NodeKey) pair introduced by a pattern; [] for patterns that
    /// bind nothing (Wildcard, Const, nullary ctors).
    let rec bindingsOfPat (ctx: PassContext) (p: Pat<SyntaxToken>) : (string * NodeKey) list =
        match p with
        | Pat.NamedSimple t when isCtorName ctx (ctx.UseSiteAt(CstKeys.ofPat p)) (ctx.NameOf t) ->
            // A known nullary ctor pattern; binds nothing.
            []
        | Pat.NamedSimple t -> [ ctx.NameOf t, CstKeys.ofPat p ]
        | Pat.Wildcard _
        | Pat.Const _
        | Pat.EmptyBlock _ -> []
        | Pat.EnclosedBlock(pat = inner) -> bindingsOfPat ctx inner
        | Pat.Tuple(patterns = pats) -> [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Typed(pat = inner) -> bindingsOfPat ctx inner
        | Pat.Attributed(pat = inner) -> bindingsOfPat ctx inner
        | Pat.As(pat = inner; ident = ident) -> (ctx.NameOf ident, CstKeys.ofPat p) :: bindingsOfPat ctx inner
        | Pat.TypeTestAs(pat = inner) ->
            // `:? T as x` — the inner pattern is the bound variable; the test type is not.
            bindingsOfPat ctx inner
        | Pat.Record(fieldPats = fieldPats) ->
            [ for FieldPat(pat = sub) in fieldPats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Named(argumentPats = args) ->
            // `Circle r`, `Color.Red`: the head is a discriminator, never a bound variable, because
            // a bound variable is a lone ident, which parses as `Pat.NamedSimple`. So the sub-patterns
            // bind whether or not the head resolves; Unification reports one that does not resolve.
            [
                for sub in args do
                    yield! bindingsOfPat ctx sub
            ]
        | Pat.Cons(head = h; tail = t) ->
            // `h :: t`: the `::` ctor binds nothing; both sub-patterns introduce bound variables.
            bindingsOfPat ctx h @ bindingsOfPat ctx t
        // `|` spells the same names on both sides, `&` binds both. Lowering rejects an
        // or-pattern that binds, so nothing has to choose between the two occurrences.
        | Pat.Or(left = l; right = r)
        | Pat.And(left = l; right = r) -> bindingsOfPat ctx l @ bindingsOfPat ctx r
        | Pat.Elems(pats = pats) ->
            // `[a; b; c]` list-literal pattern (the multi-element form, wrapped in
            // `EnclosedBlock(List, …)`): each element introduces bound variables.
            [ for sub in pats -> bindingsOfPat ctx sub ] |> List.concat
        | Pat.Op io ->
            // Operator-named binding (`let (=) x y = …`) binds the compiled name
            // `op_Equality`; use sites resolve through the desugared form instead.
            match OperatorNames.ofPatOp ctx.NameOf io with
            | ValueSome n -> [ n, CstKeys.ofPat p ]
            | ValueNone -> []
        | Pat.OpNamed _
        | Pat.StructTuple _
        | Pat.NamedFieldPats _
        | Pat.Optional _
        | Pat.TypeTest _
        | Pat.Null _
        | Pat.String _
        | Pat.Expr _
        | Pat.Missing
        | Pat.SkipsTokens _ -> []

    /// The visitor for every type NAME written at a DECLARING position. Classify each name
    /// (a claim in scope wins, else the external universe) and diagnose a SINGLE-SEGMENT one
    /// that resolves to neither (FS0039); a DOTTED name is judged where its path's scope is resolved.
    let classifyingTypeIter (ctx: PassContext) : CstTypeWalk.TypeIter =
        { CstTypeWalk.identityTypeIter with
            VisitType =
                fun _ t ->
                    if isMeasuredCarrier ctx t then
                        false
                    else
                        match CstKeys.ofTypeRef t with
                        | ValueSome typeRef ->
                            match classifyTypeRef ctx typeRef with
                            | TypeRefVerdict.UnknownType when
                                typeRef.LongIdent.Idents.Length = 1
                                // A target-optional primitive name is language-known: it
                                // resolves to its key with no contract behind it, and
                                // `PlatformTypes` reports the mention instead.
                                && (RuntimeNames.tryTargetOptionalPrimitiveKey (ctx.NameOf typeRef.Site.Tok)).IsNone
                                ->
                                ctx.UndefinedType(
                                    Site.ofTokenOr (Site.ofLongIdent typeRef.LongIdent) typeRef.Site.Tok,
                                    ctx.NameOf typeRef.Site.Tok
                                )
                            | TypeRefVerdict.UnknownType
                            | TypeRefVerdict.LocalType _
                            | TypeRefVerdict.LocalTypeAtOtherArity _
                            | TypeRefVerdict.ExternalType _ -> ()
                        | ValueNone -> ()

                        true
        }

    /// Resolve a discriminator in pattern position and stamp what it denotes at the pattern's
    /// key. A single ident that denotes nothing is a bound variable, which has no stamp.
    let private resolvePatNames
        (ctx: PassContext)
        (pat: Pat<SyntaxToken>)
        (idents: ImmutableArray<SyntaxToken>)
        : unit =
        let key = CstKeys.ofPat pat
        let names = [| for id in idents -> ctx.NameOf id |]
        let r = resolvePattern ctx (ctx.UseSiteAt key) names

        match r.Item, names.Length with
        | ResolvedItem.Unresolved _, 1 -> ()
        | item, _ ->
            ctx.Resolution.Resolved.Set(key, item)
            stampItem ctx key item

            match item with
            | ResolvedItem.UnionCase(case, true) ->
                ctx.Report(CstKeys.firstTokenOfPat pat, Kind.RequireQualifiedAccessCase(case.UnionName, case.CaseName))
            | ResolvedItem.AmbiguousCase(name, claims) ->
                ctx.Report(CstKeys.firstTokenOfPat pat, Kind.AmbiguousConstructor(name, claims.Length))
            | _ -> ()

    /// Stamp every union-case and enum-case discriminator in `p`, including the alternatives
    /// and sub-patterns that introduce no bound variable.
    let private stampPatCasesWith (ctx: PassContext) (typeIter: CstTypeWalk.TypeIter) (p: Pat<SyntaxToken>) : unit =
        let visit (pat: Pat<SyntaxToken>) : unit =
            match pat with
            | Pat.NamedSimple t -> resolvePatNames ctx pat (ImmutableArray.Create t)
            | Pat.Named(longIdent = li) -> resolvePatNames ctx pat li.Idents
            | Pat.Typed(typ = t)
            | Pat.TypeTestAs(typ = t)
            | Pat.TypeTest(typ = t) -> CstTypeWalk.iterType typeIter t
            | _ -> ()

        CstWalk.iterPat
            {
                VisitPat =
                    fun _ pat ->
                        visit pat
                        true
            }
            p

    /// The body / value-position form: an embedded type name that resolves to nothing is
    /// stamped and left alone.
    let stampPatCases (ctx: PassContext) (p: Pat<SyntaxToken>) : unit =
        stampPatCasesWith ctx (stampTypeIter ctx) p

    /// The DECLARING-position form: an embedded type name that resolves to nothing is
    /// diagnosed (FS0039).
    let stampPatCasesDeclaring (ctx: PassContext) (p: Pat<SyntaxToken>) : unit =
        stampPatCasesWith ctx (classifyingTypeIter ctx) p

    /// Lambda args / for-in / match-arm patterns can't carry `mutable`, so every
    /// bound variable they introduce is immutable.
    let extendScope (ctx: PassContext) (pats: ImmutableArray<Pat<SyntaxToken>>) (acc: Scope) : Scope =
        let mutable s = acc

        for p in pats do
            stampPatCases ctx p

            for n, k in bindingsOfPat ctx p do
                s <- Map.add n (scopeBinding k false) s

        s

    /// Build the scope additions for a let-group, writing a self-entry
    /// (`BindingSite = key`) for every bound variable.
    let bindingsToScope (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : Scope =
        let mutable s = Map.empty

        for b in bindings do
            let isMut = b.mutableToken.IsSome
            stampPatCases ctx b.pattern

            for n, k in bindingsOfPat ctx b.pattern do
                s <- Map.add n (scopeBinding k isMut) s

                ctx.Bindings.Binding.Set(
                    k,
                    {
                        BindingSite = k
                        IsInline = b.inlineToken.IsSome
                        IsMutable = isMut
                    }
                )

        s

    /// Resolve an operator/value spelling against the referenced surfaces and, on a hit, stamp
    /// the full symbol so its scheme is instantiated by key later.
    let private stampExternalSymbol (ctx: PassContext) (key: NodeKey) (qualifier: Qualifier) (name: string) : unit =
        match NameResolutionLongIdent.externalValueInScope ctx (ctx.UseSiteAt key) qualifier name with
        | ValueSome sym -> ctx.Resolution.ExternalSymbolStamp.Set(key, sym)
        | ValueNone -> ()

    /// Resolve an external VALUE reference by its compiled spelling and stamp both channels.
    let private tryStampExternalValue (ctx: PassContext) (key: NodeKey) (qualifier: Qualifier) (name: string) : bool =
        match NameResolutionLongIdent.externalValueInScope ctx (ctx.UseSiteAt key) qualifier name with
        | ValueSome sym ->
            stampItem ctx key (ResolvedItem.Value(ResolvedValue.External sym))
            true
        | ValueNone -> false

    /// Bind an operator's compiled name to a binding of this file: one the walk bound (a nested
    /// `let (>=>)`), else a module-level one the ranked environment yields. A hit shadows every
    /// provider symbol, so a mono `let (+)` retypes every `+` below its definition.
    let private tryStampBoundOperator (ctx: PassContext) (scope: Scope list) (key: NodeKey) (name: string) : bool =
        match lookupLexical scope name with
        | ValueSome b ->
            bindUseSite ctx key b.Site b.IsMutable
            true
        | ValueNone ->
            match NameResolutionLongIdent.valueInEnv ctx (ctx.UseSiteAt key) name with
            | ValueSome(ResolvedValue.Local m) ->
                stampItem ctx key (ResolvedItem.Value(ResolvedValue.Local m))
                true
            // Returns false so the caller's external-symbol path stamps it.
            | ValueSome(ResolvedValue.External _)
            | ValueNone -> false

    /// Bind a qualified value naming one of this file's own modules (`M.(>=>)`), as a plain
    /// local ident is bound. False on an external or partial resolution.
    let private tryStampLocalQualifiedValue (ctx: PassContext) (key: NodeKey) (names: string[]) : bool =
        let r = resolveExpr ctx (ctx.UseSiteAt key) names

        match r.Item with
        | ResolvedItem.Value(ResolvedValue.Local _) when r.Rest = names.Length ->
            stampItem ctx key r.Item
            true
        | _ -> false

    /// Resolve an operator's compiled name as a bare value ident resolves: a binding in
    /// scope, then the referenced surfaces.
    let private stampOperatorName (ctx: PassContext) (scope: Scope list) (key: NodeKey) (name: string) : unit =
        if not (tryStampBoundOperator ctx scope key name) then
            stampExternalSymbol ctx key Qualifier.Bare name

    /// An `InfixApp` / `PrefixApp` operator: stamp what its compiled name denotes.
    /// `::` has no compiled name and `op_AddressOf` has no provider symbol, so
    /// neither stamps.
    let private stampOperator (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        let name =
            match e with
            | Expr.InfixApp(_, op, _) -> OperatorNames.ofSymbolic (ctx.NameOf op) op
            | Expr.PrefixApp(op, _) -> OperatorNames.ofPrefix (ctx.NameOf op) op
            | _ -> ValueNone

        match name with
        | ValueSome name -> stampOperatorName ctx scope (CstKeys.ofExpr e) name
        | ValueNone -> ()

    /// The enclosing `TypeApp` visit resolved this applied name at its exact arity.
    let private resolvedByTypeApp (ctx: PassContext) (e: Expr<SyntaxToken>) : bool =
        ctx.Resolution.Resolved.ContainsKey(CstKeys.ofExpr e)

    /// Resolve the applied name of an `Expr.TypeApp` at `types.Length` and stamp it at the
    /// applied expression. Returns this file's claim on the name, at the written arity else at
    /// the nearest one; Unification checks the written count against the claim's arity.
    let private resolveTypeApp
        (ctx: PassContext)
        (applied: Expr<SyntaxToken>)
        (types: ImmutableArray<Type<SyntaxToken>>)
        : TypeIdentity voption =
        match typeAppTypeName ctx applied with
        | ValueSome written ->
            let key = CstKeys.ofExpr applied

            match resolveType ctx (ctx.UseSiteAt key) written types.Length with
            | TypeNameResolution.Type(ResolvedTypeRef.Local claim as t) ->
                ctx.Resolution.Resolved.Set(key, ResolvedItem.Type t)
                ValueSome claim
            | TypeNameResolution.LocalAtOtherArity claim ->
                ctx.Resolution.Resolved.Set(key, ResolvedItem.Type(ResolvedTypeRef.Local claim))
                ValueSome claim
            | TypeNameResolution.Type(ResolvedTypeRef.External(typeKey, _) as t) ->
                ctx.Resolution.Resolved.Set(key, ResolvedItem.Type t)
                ctx.Resolution.ResolvedType.Set(key, typeKey)
                ValueNone
            | TypeNameResolution.Unresolved _ -> ValueNone
        | ValueNone -> ValueNone

    /// `U<int>.Case`, `E<int>.A`, `C<int>.M`: the member inside `claim`, stamped at the
    /// `DotLookup` node as the folded `U.Case` is stamped at its `LongIdent`. A class member
    /// miss is reported here; a union, record or enum miss is Unification's `NoCase`/`NoMember`.
    let private resolveTypeAppMember
        (ctx: PassContext)
        (e: Expr<SyntaxToken>)
        (claim: TypeIdentity)
        (memberTok: SyntaxToken)
        : unit =
        let item = memberOfLocalType ctx claim (ctx.NameOf memberTok)
        ctx.Resolution.Resolved.Set(CstKeys.ofExpr e, item)

        match item with
        | ResolvedItem.Unresolved {
                                      Segment = segment
                                      Within = ResolutionScope.Type owner
                                  } when not (missReportedDownstream owner) ->
            ctx.Report(memberTok, Kind.NoMember(owner.Key.Name, MemberNoun.ValueOrMember, segment))
        | _ -> ()

    let private visit (ctx: PassContext) (scope: Scope list) (e: Expr<SyntaxToken>) : unit =
        // Stamp every external type name embedded in this node's annotations. Recursion
        // into child expressions is the walker's, so each name is stamped once.
        stampExprEmbeddedTypes ctx e

        match e with
        | Expr.Ident _
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent _) when resolvedByTypeApp ctx e -> ()
        | Expr.Ident tok -> resolveIdent ctx scope e tok
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) when li.Idents.Length = 1 ->
            resolveIdent ctx scope e li.Idents.[0]
        | Expr.LongIdentOrOp(LongIdentOrOp.LongIdent li) ->
            // Multi-segment: a chained field access (`r.X.Y`, anchor local) or a qualified
            // name (`Math.PI`, provider). Field resolution waits for the anchor's type.
            let anchorIdent = li.Idents.[0]

            match lookupLexical scope (ctx.NameOf anchorIdent) with
            | ValueSome b ->
                // Key the anchor's binding entry under ExprIdent on the anchor token
                // so later passes look up the anchor's type by the same key.
                bindUseSite ctx (NodeKey.ofToken anchorIdent NodeKind.ExprIdent) b.Site b.IsMutable
            | ValueNone -> resolveExprNames ctx e li.Idents
        | Expr.LongIdentOrOp(LongIdentOrOp.Op(IdentOrOp.ParenOp(opName = OpName.SymbolicOp op))) ->
            // `(+)` used as a value resolves as its infix form does: a binding in scope,
            // then the providers. A miss is not diagnosed here.
            match OperatorNames.ofSymbolic (ctx.NameOf op) op with
            | ValueSome name ->
                let key = CstKeys.ofExpr e

                if not (tryStampBoundOperator ctx scope key name) then
                    tryStampExternalValue ctx key Qualifier.Bare name |> ignore
            | ValueNone ->
                // A symbolic spelling with no compiled `op_` name (`(::)` as a value):
                // surface the gap as the non-symbolic catch-all does.
                ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName(ctx.NameOf op))
        | Expr.LongIdentOrOp(LongIdentOrOp.QualifiedOp(longIdent = li; op = idOp)) ->
            // `A.B.(+)` — translate the operator segment to its compiled name
            // (`op_Addition`) and route it through the value long-ident machinery under the
            // container `A.B` denotes. Only the qualified form needs this; bare ops come from
            // the prelude.
            match OperatorNames.qualifiedOpParts ctx.NameOf li idOp with
            | ValueSome(struct (segments, opName)) ->
                let key = CstKeys.ofExpr e

                if
                    not (tryStampLocalQualifiedValue ctx key (Array.append segments [| opName |]))
                    && not (tryStampExternalValue ctx key (Qualifier.ofSegments segments) opName)
                then
                    ctx.Report(
                        CstKeys.firstTokenOfExpr e,
                        Kind.UnresolvedQualifiedName(SymbolKeyOps.qualify (String.concat "." segments) opName)
                    )
            | ValueNone ->
                // A non-symbolic op segment (active-pattern / nil / range) has no
                // `op_` member to qualify, so keep surfacing the gap.
                ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName(ctx.NameOf li.Idents.[0]))
        | Expr.LongIdentOrOp lio ->
            // TODO: a bare non-symbolic `LongIdentOrOp.Op` (an active-pattern or nil
            // op-name used as a value) needs its own resolution story. Surface the gap
            // rather than silently skipping.
            let firstTok = CstKeys.firstTokenOfLongIdentOrOp lio
            let displayName = ctx.NameOf firstTok

            ctx.Report(CstKeys.firstTokenOfExpr e, Kind.OperatorFormQualifiedName displayName)
        | Expr.DotLookup(expr = Expr.TypeApp(expr = applied; types = types); longIdentOrOp = LongIdentOrOp.LongIdent li) when
            li.Idents.Length = 1
            ->
            match resolveTypeApp ctx applied types with
            | ValueSome claim -> resolveTypeAppMember ctx e claim li.Idents.[0]
            | ValueNone -> ()
        // The walker reaches a qualifier `TypeApp` again as its own node, already stamped by
        // the arm above.
        | Expr.TypeApp(expr = applied; types = types) when not (resolvedByTypeApp ctx applied) ->
            resolveTypeApp ctx applied types |> ignore
        | Expr.InfixApp _
        | Expr.PrefixApp _ -> stampOperator ctx scope e
        // `x?name` — stamp `op_Dynamic`. The SET form (`x?name <- v`) parses as
        // `Assignment(DynamicLookup, v)`, whose inner `DynamicLookup` is visited and
        // stamped too, but the setter reads the enclosing node, so that stamp is inert.
        | Expr.DynamicLookup _ -> stampExternalSymbol ctx (CstKeys.ofExpr e) Qualifier.Bare OperatorData.OpDynamic
        // `x?name <- value` — stamp `op_DynamicAssignment` on the enclosing
        // `Assignment`, the node the setter is typed at.
        | Expr.Assignment(leftExpr = Expr.DynamicLookup _) ->
            stampExternalSymbol ctx (CstKeys.ofExpr e) Qualifier.Bare OperatorData.OpDynamicAssignment
        | _ -> ()

    let mkWalker (ctx: PassContext) : CstWalk.ExprWalker<Scope list> =
        {
            Visit = visit ctx
            EnterFun = fun scope argPats -> extendScope ctx argPats Map.empty :: scope
            EnterBindingRhs =
                fun scope isRec siblings b ->
                    // `let rec`: sibling names, including this binding's own, are in
                    // scope for the RHS. Function-form pushes parameter names on top.
                    let mutable s = scope

                    if isRec then
                        s <- bindingsToScope ctx siblings :: s

                    if not b.argumentPats.IsEmpty then
                        s <- extendScope ctx b.argumentPats Map.empty :: s

                    s
            EnterLetBody = fun scope bindings -> bindingsToScope ctx bindings :: scope
            EnterForTo =
                fun scope ident ->
                    let name = ctx.NameOf ident
                    Map.ofList [ name, scopeBinding (CstKeys.ofForToVar ident) false ] :: scope
            EnterForIn =
                fun scope pat ->
                    stampPatCases ctx pat

                    let scopeMap =
                        bindingsOfPat ctx pat
                        |> List.map (fun (n, k) -> n, scopeBinding k false)
                        |> Map.ofList

                    scopeMap :: scope
            EnterMatchArm =
                fun scope pat ->
                    stampPatCases ctx pat

                    let scopeMap =
                        bindingsOfPat ctx pat
                        |> List.map (fun (n, k) -> n, scopeBinding k false)
                        |> Map.ofList

                    scopeMap :: scope
        }
