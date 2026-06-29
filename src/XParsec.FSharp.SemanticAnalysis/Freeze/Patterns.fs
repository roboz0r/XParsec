namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeLiterals
open XParsec.FSharp.SemanticAnalysis.FreezeResolve

// Pattern projection for the Freeze pass: the recursive CST `Pat` -> `TPat`
// translation and the list-case-name resolution it shares with the expression
// projection (`FreezeExpr`).

module internal FreezePatterns =

    /// Does `caseName` (optionally written with `qualifier`) name a case of an
    /// *external* (referenced-package) union the provider knows? Mirrors the
    /// Unification recogniser (`tryExternalCasePattern`) for the Freeze pattern
    /// path, so a cross-package `match o with Some x -> …` lowers to `TPat.Union`
    /// exactly as the local-union arm does.
    /// The lowering is identical to the local case — only the recognition differs.
    let private isExternalUnionCase (ctx: PassContext) (qualifier: string voption) (caseName: string) : bool =
        // Mirror `tryExternalCasePattern`: a bare reference to an RQA union's case
        // is not recognised, so it lowers as a binder, not `TPat.Union`
        // (bare RQA case). The qualified form still resolves.
        ctx.Provider.TryLookupUnionCase caseName
        |> ValueOption.exists (fun uc -> uc.ResolvesWith qualifier)

    /// The `(consName, nilName)` case names of the list union a `[…]` literal,
    /// `[]`/`h :: t` pattern, or `::` construction targets. Mirrors the
    /// case-by-arity resolution in `translateListLikeLiteral`: a program-declared
    /// list union (its nullary case = the empty terminator, its single binary case
    /// = cons) drives its own factories. For the self-host `list.fs` and the
    /// external Vesper list (whose `[]`/`::` cases register / compile as
    /// `Empty`/`Cons`) this returns `("Cons", "Empty")`; the FSharp.Core fallback
    /// keeps `("Cons", "Nil")`.
    let listCaseNames (ctx: PassContext) (ty: SemType) : string * string =
        match Unification.zonk ty with
        | TyUnion(unionKey, _) when (TypeRegistry.tryUnionByKey ctx.Types unionKey).IsSome ->
            let info = (TypeRegistry.tryUnionByKey ctx.Types unionKey).Value
            let nilCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 0)
            let consCase = info.Cases |> Array.tryFind (fun c -> c.Fields.Length = 2)

            match nilCase, consCase with
            | Some n, Some c -> c.Name, n.Name
            | _ -> "Cons", "Empty"
        | TyUnion(listKey, _) when RuntimeNames.isVesperListKey listKey -> "Cons", "Empty"
        | _ -> "Cons", "Nil"

    /// Patterns Unification doesn't understand yet fall through loudly so the
    /// gap surfaces at translation time.
    let rec translatePat (ctx: PassContext) (p: Pat<SyntaxToken>) : TPat =
        let key = CstKeys.ofPat p
        let ty = typeOfKey ctx key
        // Source-map anchor for this binding site; synthetic sub-nodes (list
        // desugaring's `Cons`/`Empty` chain) reuse the whole pattern's token.
        let tok = CstKeys.firstTokenOfPat p

        match p with
        | Pat.NamedSimple t when
            let n = ctx.NameOf t

            n.Length > 0
            && System.Char.IsUpper n.[0]
            && (ctx.Types.CtorIndex.ContainsKey n || isExternalUnionCase ctx ValueNone n)
            ->
            // Nullary ctor in pattern position — a local union or an external
            // referenced-package one (`None`). Must precede the plain NamedSimple
            // arm. Both lower to the same `TPat.Union`; the node's type
            // (`typeOfKey`) already carries the right `TyUnion`, so the backend
            // routes local vs external off that.
            TPat.Union(ctx.NameOf t, EqArray.empty, ty, tok)
        | Pat.NamedSimple _ -> TPat.NamedSimple(key, ty, tok)
        | Pat.Wildcard _ -> TPat.Wildcard(ty, tok)
        | Pat.EnclosedBlock(lParen = ParenKind.List _; pat = inner) ->
            // `[a; b; c]` list-literal pattern → nested cons:
            // `Cons(a, Cons(b, Cons(c, Empty)))`. Each cons/nil node carries the
            // whole list type (`ty`) — a tail of a `'T list` is the same `'T list`
            // — so `listCaseNames` resolves the same factory at every level. A
            // single-element `[a]` arrives unwrapped; `[]` is `Pat.EmptyBlock`.
            let consName, nilName = listCaseNames ctx ty

            let elems =
                match inner with
                | Pat.Elems(pats = pats) -> List.ofSeq pats
                | single -> [ single ]

            let nil = TPat.Union(nilName, EqArray.empty, ty, tok)

            List.foldBack
                (fun el acc -> TPat.Union(consName, EqArray.ofList [ translatePat ctx el; acc ], ty, tok))
                elems
                nil
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) ->
            TPat.Tuple(EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub }), ty, tok)
        | Pat.EmptyBlock(lParen = ParenKind.List _) ->
            // `[]` pattern → the list union's nullary (empty) case, by arity.
            let _, nilName = listCaseNames ctx ty
            TPat.Union(nilName, EqArray.empty, ty, tok)
        | Pat.Cons(head = headPat; tail = tailPat) ->
            // `h :: t` → the list union's binary (cons) case. The node's type
            // (`typeOfKey`) is the list `TyUnion` Unification resolved; the backend
            // routes local vs external off it, exactly like a named-ctor pattern.
            let consName, _ = listCaseNames ctx ty
            TPat.Union(consName, EqArray.ofList [ translatePat ctx headPat; translatePat ctx tailPat ], ty, tok)
        | Pat.Const c -> TPat.Const(parseConst ctx c, ty, tok)
        | Pat.As(pat = inner) ->
            // The `as`-name isn't surfaced in TPat yet — downstream Var lookups
            // find the alias via the CST + side tables.
            translatePat ctx inner
        | Pat.Typed(pat = inner) ->
            // Annotation is consumed by Unification; runtime shape is the inner.
            translatePat ctx inner
        | Pat.Attributed(pat = inner) ->
            // Parameter attributes (`[<CallAtMostOnce>]`) are decoded in
            // `Elaborate`; the runtime shape is the wrapped pattern.
            translatePat ctx inner
        | Pat.Or(left = leftPat) ->
            // Both sides must bind the same names (Validation's job). Until or-
            // patterns are first-class in TPat, pick the left arm for shape.
            translatePat ctx leftPat
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
        // `| E.C1` enum-case pattern (project-local OR external TS-manifest enum) →
        // `TPat.EnumCase(enumKey, caseName, …)`, mirroring the `E.C1` expression
        // lowering (`StaticFieldGet`, same carrier). v1 = equality only: codegen
        // resolves the case's underlying literal off the frozen enum case table by
        // key + name and compares, exactly like a `Const` pattern — the literal is
        // NOT duplicated onto the node. `EnumCaseAccess` resolves the key from the
        // pattern's `TyEnum` type (set by Unification for both local and external
        // heads) or the local enum registry on the error path — exclusive with the
        // union / ctor heads below.
        | Pat.Named(longIdent = li & EnumCaseAccess ctx ty enumKey) ->
            TPat.EnumCase(enumKey, ctx.NameOf li.Idents.[1], ty, tok)
        | Pat.Named(longIdent = li; argumentPats = args) when
            li.Idents.Length >= 1
            && (let last = ctx.NameOf li.Idents.[li.Idents.Length - 1]

                last.Length > 0
                && System.Char.IsUpper last.[0]
                && (li.Idents.Length = 1
                    && (ctx.Types.CtorIndex.ContainsKey last || isExternalUnionCase ctx ValueNone last)
                    || li.Idents.Length = 2
                       && (TypeRegistry.localQualifiedCase ctx.Types (ctx.NameOf li.Idents.[0]) last
                           || isExternalUnionCase ctx (ValueSome(ctx.NameOf li.Idents.[0])) last)))
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
            // `:? T as x` — Unification stashed the tested type in
            // `TypeTestTargets` (keyed on this node, like the `:?` expression
            // form). The inner pattern (the `as`-name) is translated against it;
            // codegen lowers the whole thing to an `isinst` + null check + bind.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Freeze.translatePat: no TypeTestTargets entry for type-test pattern %A" p

            TPat.TypeTestAs(testTy, translatePat ctx inner, ty, tok)
        | Pat.TypeTest _ ->
            // Bare `:? T` — same lowering as `:? T as x` but with a synthesised
            // wildcard inner (binds nothing). Codegen's `isinst` + null-check arm
            // discards the cast-down value.
            let testTy =
                match ctx.Resolution.TypeTestTargets.TryGetValue key with
                | ValueSome t -> t
                | ValueNone -> failwithf "Freeze.translatePat: no TypeTestTargets entry for type-test pattern %A" p

            TPat.TypeTestAs(testTy, TPat.Wildcard(testTy, tok), ty, tok)
        | Pat.Null _ ->
            // `null` literal pattern → `TPat.Null`; codegen lowers it to a
            // non-null test (`ldloc; brtrue nextLabel`). The node's type is the
            // scrutinee's reference type (pinned by Unification).
            TPat.Null(ty, tok)
        | Pat.Op _ ->
            // Operator-named binding head (`let (=) x y = …`): a single binder,
            // shaped like a `Pat.NamedSimple`. Its source name is the operator's
            // compiled name (`memberNameOfBinding` → `op_Equality`); the key
            // matches `CstKeys.ofBinding`, so the binding's `ModuleMembers` entry
            // (and thus the cross-package inline-body loader) finds it.
            TPat.NamedSimple(key, ty, tok)
        | _ -> failwithf "Freeze.translatePat: TODO %A" p
