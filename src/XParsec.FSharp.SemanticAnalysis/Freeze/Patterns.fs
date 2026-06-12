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
    /// exactly as the local-union arm does (vesper-lib-test-plan Gap 2 Layer C).
    /// The lowering is identical to the local case — only the recognition differs.
    let private isExternalUnionCase (ctx: PassContext) (qualifier: string voption) (caseName: string) : bool =
        match ctx.Provider.TryLookupUnionCase caseName with
        | ValueSome uc ->
            match qualifier with
            | ValueNone -> true
            | ValueSome q -> SymbolKeyOps.shortName uc.UnionName = q
        | ValueNone -> false

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
            TPat.Union(ctx.NameOf t, EqArray.empty, ty)
        | Pat.NamedSimple _ -> TPat.NamedSimple(key, ty)
        | Pat.Wildcard _ -> TPat.Wildcard ty
        | Pat.EnclosedBlock(pat = inner) -> translatePat ctx inner
        | Pat.Tuple(patterns = pats) -> TPat.Tuple(EqArray.ofSeq (seq { for sub in pats -> translatePat ctx sub }), ty)
        | Pat.EmptyBlock(lParen = ParenKind.List _) ->
            // `[]` pattern → the list union's nullary (empty) case, by arity.
            let _, nilName = listCaseNames ctx ty
            TPat.Union(nilName, EqArray.empty, ty)
        | Pat.Cons(head = headPat; tail = tailPat) ->
            // `h :: t` → the list union's binary (cons) case. The node's type
            // (`typeOfKey`) is the list `TyUnion` Unification resolved; the backend
            // routes local vs external off it, exactly like a named-ctor pattern.
            let consName, _ = listCaseNames ctx ty
            TPat.Union(consName, EqArray.ofList [ translatePat ctx headPat; translatePat ctx tailPat ], ty)
        | Pat.Const c -> TPat.Const(parseConst ctx c, ty)
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
        | Pat.EmptyBlock _ -> TPat.Const(TConstValue.Unit, ty)
        | Pat.Record(fieldPats = fieldPats) ->
            let fields =
                EqArray.ofSeq (
                    seq {
                        for FieldPat(longIdent = li; pat = sub) in fieldPats ->
                            let idents = li.Idents
                            ctx.NameOf idents.[idents.Length - 1], translatePat ctx sub
                    }
                )

            TPat.Record(fields, ty)
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

            TPat.Union(caseName, subPats, ty)
        | Pat.Op _ ->
            // Operator-named binding head (`let (=) x y = …`): a single binder,
            // shaped like a `Pat.NamedSimple`. Its source name is the operator's
            // compiled name (`memberNameOfBinding` → `op_Equality`); the key
            // matches `CstKeys.ofBinding`, so the binding's `ModuleMembers` entry
            // (and thus the cross-package inline-body loader) finds it.
            TPat.NamedSimple(key, ty)
        | _ -> failwithf "Freeze.translatePat: TODO %A" p
