namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateLiterals
open XParsec.FSharp.SemanticAnalysis.ElaborateNominals

// Pattern projection for the Elaborate pass: the recursive CST `Pat` -> `TPat`
// translation and the list-case-name resolution it shares with the expression
// projection.

module internal ElaboratePatterns =

    /// The `(cons, empty)` case names of the list type `ty`. A program-declared list union
    /// declares its own BY ARITY (binary = cons, nullary = empty); anything else is the
    /// cons-list, `("Cons", "Empty")`.
    let listCaseNames (ctx: PassContext) (ty: SemType) : string * string =
        let consList = RuntimeNames.consCaseName, RuntimeNames.emptyCaseName

        match Unification.zonk ctx.Store ty with
        | LocalUnion ctx info ->
            let emptyCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
            let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

            match emptyCase, consCase with
            | Some e, Some c -> c.Name, e.Name
            | _ -> consList
        | _ -> consList

    /// A `NamedSimple` node, recording HOW THE SOURCE WRITES its bound variable as the node is
    /// built, because that is the last moment the key and the token that produced it are
    /// together. Without that spelling the freeze has no name for the bound variable.
    let private namedSimple (ctx: PassContext) (key: NodeKey) (ty: SemType) (tok: SyntaxToken) : TPat =
        let node = TPat.NamedSimple(key, ty, tok)

        BoundVarKey.ofPat node
        |> ValueOption.iter (fun b -> ctx.SetBoundVarName(b, tok))

        node

    let rec translatePat (ctx: PassContext) (p: Pat<SyntaxToken>) : TPat =
        let key = CstKeys.ofPat p
        let ty = typeOfKey ctx key
        // Source-map anchor for this binding site; the synthetic `Cons`/`Empty` chain a list
        // pattern desugars to reuses the whole pattern's token.
        let tok = CstKeys.firstTokenOfPat p

        match p with
        | Pat.NamedSimple t when
            ctx.Resolution.ExternalUnionCaseStamp.ContainsKey key
            || (let n = ctx.NameOf t

                n.Length > 0
                && System.Char.IsUpper n.[0]
                && TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt key) n)
            ->
            // Nullary ctor in pattern position — a local union, or an external one (`None`)
            // stamped upstream and read by key. Both lower to the same `TPat.Union`: the
            // node's type carries the `TyUnion` a backend routes local vs external off.
            TPat.Union(ctx.NameOf t, EqArray.empty, ty, tok)
        | Pat.NamedSimple _ -> namedSimple ctx key ty tok
        | Pat.Wildcard _ -> TPat.Wildcard(ty, tok)
        | Pat.EnclosedBlock(lParen = ParenKind.List _; pat = inner) ->
            // `[a; b; c]` → `Cons(a, Cons(b, Cons(c, Empty)))`. Every node carries the WHOLE
            // list type, because a tail of a `'T list` is the same `'T list`. A single-element
            // `[a]` arrives unwrapped; `[]` is `Pat.EmptyBlock`.
            let consName, emptyName = listCaseNames ctx ty

            let elems =
                match inner with
                | Pat.Elems(pats = pats) -> List.ofSeq pats
                | single -> [ single ]

            let empty = TPat.Union(emptyName, EqArray.empty, ty, tok)

            List.foldBack
                (fun el acc -> TPat.Union(consName, EqArray.ofList [ translatePat ctx el; acc ], ty, tok))
                elems
                empty
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) ->
            TPat.Tuple(EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub }), ty, tok)
        | Pat.EmptyBlock(lParen = ParenKind.List _) ->
            // `[]` pattern → the list union's nullary (empty) case, by arity.
            let _, emptyName = listCaseNames ctx ty
            TPat.Union(emptyName, EqArray.empty, ty, tok)
        | Pat.Cons(head = headPat; tail = tailPat) ->
            // `h :: t` → the list union's binary (cons) case.
            let consName, _ = listCaseNames ctx ty
            TPat.Union(consName, EqArray.ofList [ translatePat ctx headPat; translatePat ctx tailPat ], ty, tok)
        | Pat.Const c -> TPat.Const(parseConst ctx c, ty, tok)
        | Pat.As(pat = inner) ->
            // The `as`-name isn't surfaced in TPat yet, but downstream Var lookups find the
            // alias via the CST + side tables.
            translatePat ctx inner
        | Pat.Typed(pat = inner) ->
            // Annotation is consumed by Unification; runtime shape is the inner.
            translatePat ctx inner
        | Pat.Attributed(pat = inner) ->
            // Parameter attributes are decoded elsewhere; the runtime shape is the wrapped
            // pattern.
            translatePat ctx inner
        | Pat.Or _ ->
            // The parser builds a left-nested `Or(Or(p1, p2), p3)`; flatten it to one level
            // so a backend tests a flat alternative list, first match wins.
            let rec flatten (acc: Pat<SyntaxToken> list) (pat: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
                match pat with
                | Pat.Or(left = l; right = r) -> flatten (flatten acc l) r
                | other -> other :: acc

            let leaves = flatten [] p |> List.rev

            // A bound variable would lower to an irrefutable test that eats the disjunction.
            // One report for the whole chain, however many alternatives bind.
            match NameResolutionScope.bindingsOfPat ctx p with
            | [] -> ()
            | _ -> ctx.Report(tok, Kind.NotYetSupported "or-patterns that bind names (e.g. `(1, x) | (2, x)`)")

            let alts = leaves |> List.map (translatePat ctx)
            TPat.Or(EqArray.ofList alts, ty, tok)
        | Pat.EmptyBlock _ -> TPat.Const(TConstValue.Unit, ty, tok)
        | Pat.Record(fieldPats = fieldPats) ->
            let fields =
                EqArray.ofSeq (
                    seq {
                        for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translatePat ctx sub
                    }
                )

            TPat.Record(fields, ty, tok)
        // `| E.C1` enum-case pattern, project-local or external. The case's underlying
        // literal is NOT copied onto the node: codegen looks it up by key + name in the
        // frozen enum case table and compares, exactly like a `Const` pattern.
        | Pat.Named(longIdent = li & EnumCaseAccess ctx ty enumKey) ->
            TPat.EnumCase(enumKey, ctx.NameOf li.Idents.[1], ty, tok)
        | Pat.Named(longIdent = li; argumentPats = args) when
            (ResolvedStamps.tryUnionCase ctx.Resolution.Resolved key).IsSome
            || ctx.Resolution.ExternalUnionCaseStamp.ContainsKey key
            || (li.Idents.Length >= 1
                && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                    last.Length > 0
                    && System.Char.IsUpper last.[0]
                    && (li.Idents.Length = 1
                        && TypeRegistry.isCaseName ctx.Types (ctx.UseSiteAt key) last
                        || li.Idents.Length = 2
                           && TypeRegistry.localQualifiedCase
                               ctx.Types
                               (ctx.UseSiteAt key)
                               (ctx.NameOf li.Idents.[0])
                               last)))
            ->
            let caseName = ctx.NameOf li.Idents.[li.Idents.Length - 1]

            let subPats =
                if args.Length = 1 then
                    match args.[0] with
                    | Pat.EnclosedBlock(pat = Pat.Tuple(patterns = pats)) ->
                        EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub })
                    | Pat.EnclosedBlock(pat = inner) -> EqArray.singleton (translatePat ctx inner)
                    | Pat.Tuple(patterns = pats) -> EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub })
                    | sub -> EqArray.singleton (translatePat ctx sub)
                else
                    EqArray.ofSeq (seq { for sub in args -> translatePat ctx sub })

            TPat.Union(caseName, subPats, ty, tok)
        | Pat.TypeTestAs(pat = inner) ->
            // `:? T as x` — Unification stashed `T` in `TypeTestTargets`, keyed on this
            // node. Codegen lowers the whole thing to an `isinst` + null check + bind.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Elaborate.translatePat: no TypeTestTargets entry for type-test pattern %A" p

            TPat.TypeTestAs(testTy, translatePat ctx inner, ty, tok)
        | Pat.TypeTest _ ->
            // Bare `:? T` — same lowering as `:? T as x`, with a synthesised wildcard inner
            // so the cast-down value is discarded.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Elaborate.translatePat: no TypeTestTargets entry for type-test pattern %A" p

            TPat.TypeTestAs(testTy, TPat.Wildcard(testTy, tok), ty, tok)
        | Pat.Null _ ->
            // `null` literal pattern; the node's type is the scrutinee's reference type.
            TPat.Null(ty, tok)
        | Pat.Op _ ->
            // Operator-named binding (`let (=) x y = …`): a single bound variable shaped like a
            // `Pat.NamedSimple`, compiled under `op_Equality`. Being a `NamedSimple` is what
            // gives it a frozen identity, and so an entry the inline-body loader can find.
            namedSimple ctx key ty tok
        | _ -> failwithf "Elaborate.translatePat: TODO %A" p
