namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload
open UnificationInferForwardSchemes
open UnificationInferDispatch

module internal UnificationInferTypeOps =

    /// What an explicit type application (`f<int>`) pins its arguments onto. `Name` is the
    /// spelling a wrong-arity report blames.
    [<RequireQualifiedAccess>]
    type private TypeAppTarget =
        /// A generalised binding, whose quantified roots the arguments pin in `Order`.
        | Scheme of name: string * scheme: TypeScheme * order: TyVarId list
        /// A nominal result (`ResizeArray<int>()`), whose own type arguments the explicit
        /// arguments pin.
        | Nominal of name: string * args: EqArray<SemType>
        /// The arguments pin nothing.
        | Untargeted

    /// Explicit type application on a value or ctor: the arguments pin the applied thing's
    /// type parameters, and a wrong count is reported against it.
    let rec inferTypeApp
        (infer: Infer)
        (ctx: PassContext)
        (tok: SyntaxToken)
        (inner: Expr<SyntaxToken>)
        (typeArgs: ImmutableArray<Type<SyntaxToken>>)
        : SemType =
        let innerTy = infer ctx inner
        let explicit = [ for t in typeArgs -> translateType ctx t ]

        let rec resultOf t =
            match resolveStep ctx.Store t with
            | TyFun(_, r) -> resultOf r
            | other -> other

        // A generalised binding wins over the nominal result, so `let mk<'a> () : Map<'a,'a>`
        // takes one argument rather than `Map`'s two. A scheme quantifying nothing pins
        // nothing, and falls through.
        let target =
            let bindingSite =
                ctx.Bindings.Binding.TryGetValue(CstKeys.ofExpr inner)
                |> ValueOption.map (fun rb -> rb.BindingSite)

            let schemeTarget =
                match bindingSite |> ValueOption.bind ctx.Bindings.Scheme.TryGetValue with
                | ValueNone -> ValueNone
                | ValueSome scheme ->
                    let declared =
                        match bindingSite |> ValueOption.bind ctx.Bindings.DeclaredTypars.TryGetValue with
                        | ValueSome ds -> ds
                        | ValueNone -> []

                    match explicitTyparOrder ctx declared scheme with
                    | [] -> ValueNone
                    | order ->
                        let shown = ctx.NameOf(CstKeys.firstTokenOfExpr inner)
                        ValueSome(TypeAppTarget.Scheme(shown, scheme, order))

            match schemeTarget with
            | ValueSome t -> t
            | ValueNone ->
                match resultOf innerTy with
                | TyClass(key, args)
                | TyUnion(key, args)
                | TyRecord(key, args) ->
                    let (DisplayName shown) = SymbolKeyOps.typeSimpleName key
                    TypeAppTarget.Nominal(shown, args)
                | _ -> TypeAppTarget.Untargeted

        let reportArity (name: string) (expected: int) =
            ctx.Report(tok, Kind.TypeArgArity(name, expected, List.length explicit))

        match target with
        | TypeAppTarget.Untargeted -> ()
        | TypeAppTarget.Scheme(name, scheme, order) ->
            if List.length order <> List.length explicit then
                reportArity name (List.length order)
            else
                // A SECOND instantiation of the scheme, its fresh vars pinned to the explicit
                // arguments and the whole then unified with the occurrence `infer` minted.
                let inst = instantiateOpen ctx scheme

                List.iter2
                    (fun root ex ->
                        match inst.FreshOf.TryGetValue root with
                        | true, fresh -> unify ctx tok (TyVar fresh) ex
                        | _ -> ()
                    )
                    order
                    explicit

                unify ctx tok innerTy inst.Body
        | TypeAppTarget.Nominal(name, args) ->
            if args.Length <> List.length explicit then
                reportArity name args.Length
            else
                List.iter2 (fun fresh ex -> unify ctx tok fresh ex) (EqArray.toList args) explicit

        innerTy

    /// Value-level inline IL `(# "op" args : retTy #)`. The instruction string is opaque
    /// to the type-checker; each operand is typed only so its own subtree is solved, and
    /// the node's type comes from the declared result annotation (absent → `unit`).
    and inferILIntrinsic
        (infer: Infer)
        (ctx: PassContext)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (returnType: ReturnType<SyntaxToken> voption)
        : SemType =
        let argTys = [| for a in args -> infer ctx a |]

        // A bare `null` operand mints an unpinned fresh TypeVar; in the
        // `(# "ceq" value null : bool #)` shape of `isNull` it never links, and an `inline`
        // body rides it into callers unresolved. Pin it to the first non-`null` operand.
        let isNullOperand (e: Expr<SyntaxToken>) =
            match e with
            | Expr.Null _ -> true
            | _ -> false

        match Seq.tryFindIndex (isNullOperand >> not) args with
        | Some anchor ->
            for i in 0 .. args.Length - 1 do
                if isNullOperand args.[i] then
                    unify ctx (CstKeys.firstTokenOfExpr args.[i]) argTys.[i] argTys.[anchor]
        | None -> ()

        match returnType with
        | ValueSome(ReturnType(typ = t)) -> translateType ctx t
        | ValueNone -> ctx.Intrinsics.Unit

    /// `defaultExpr when ^T : Type = optimizedExpr` — a library-only static optimization.
    /// The default's type is the node's; each clause body is typed only to solve its own
    /// subtrees, never cross-unified (clause results differ: `byte`/`int16` for `(+)`).
    and inferLibraryOnlyStaticOptimization
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (defaultE: Expr<SyntaxToken>)
        (clauses: ImmutableArray<StaticOptimizationClause<SyntaxToken>>)
        : SemType =
        let defaultTy = infer ctx defaultE

        // `when ^T : Type` is a compile-time dispatch, not a unification constraint: the
        // typar is recorded, never unified, and the verdict is resolved at the call site.
        let resolveConstraint c =
            match c with
            | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar = tp; rhsType = rhs) ->
                TStaticOptConstraint.TyconEquals(translateType ctx (Type.VarType tp), translateType ctx rhs)
            | StaticOptimizationConstraint.WhenTyparIsStruct(typar = tp) ->
                TStaticOptConstraint.IsStruct(translateType ctx (Type.VarType tp))

        // One entry per clause, in the node's clause order, because the consumer pairs by index.
        let resolved = ResizeArray(clauses.Length)

        for clause in clauses do
            infer ctx clause.OptimizedExpr |> ignore
            resolved.Add(EqArray.ofSeq (Seq.map resolveConstraint clause.Constraints))

        ctx.StaticOpt.Set(key, EqArray.ofSeq resolved)
        defaultTy

    /// `((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))` — an SRTP
    /// member-trait call, resolved at inline expansion. So type only the argument tuple
    /// and yield the declared return `^T3`: for `Vec2 * float -> Vec2`, neither operand.
    and inferStaticMemberInvocation
        (infer: Infer)
        (ctx: PassContext)
        (msig: MemberSig<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        infer ctx argExpr |> ignore

        // The trait's typars resolve through the enclosing `let inline`'s
        // `ctx.Resolution.TyparScope`, so `^T3` here is the binding's own result typar.
        match msig with
        | MemberSig.MethodOrPropSig(sign = CurriedSig(returnType = ret))
        | MemberSig.PropSig(sign = CurriedSig(returnType = ret)) -> translateType ctx ret

    and inferTypeAnnotation
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let annTy = translateType ctx t

        ctx.MarkTypeDeclared(node.Key, annTy)

        // A format literal ascribed to a `PrintfFormat` family (`("%d" : StringFormat<_>)`)
        // types AS the format, not `string`: stamp the annotation's format type onto the literal
        // node and skip `infer`, because inferring would pin the node's TyVar to `string`.
        match tryTypeFormatLiteral ctx node.Tok inner annTy with
        | ValueSome fmt ->
            ctx.Store.SetLink(UnionFind.find ctx.Store (freshTv ctx (CstKeys.ofExpr inner)), ValueSome fmt)
            annTy
        | ValueNone ->
            let innerTy = infer ctx inner
            // The SAME admission a binding annotation gets: `("abc" : seq<char>)` and
            // `let s : seq<char> = "abc"` are one written annotation in two positions.
            unifyAnnotation ctx node.Tok innerTy annTy

            // An ascription DIRECTLY on a `?` expression (`(d?foo : int)`) is an explicit
            // assertion, so it suppresses the implicit dynamic-escape warning. An annotation
            // on the binding (`let n : int = d?foo`) is not on the `?` node and still warns.
            match inner with
            | Expr.DynamicLookup _ -> ctx.SuppressDynamicEscape(CstKeys.ofExpr inner)
            | _ -> ()

            annTy

    /// `obj` is the top of every reference hierarchy but `subsumes` does not model it
    /// (`System.Object` is not a registered class), so the coercion arms special-case it:
    /// a downcast or type-test from `obj` is statically admissible, resolved at runtime.
    and isObjTy (store: TypeStore) (t: SemType) : bool =
        match resolveStep store t with
        | TyObj -> true
        | _ -> false

    /// `e :> T` — explicit upcast. `src` must instantiate `T`'s nominal: itself (a
    /// redundant but legal upcast), a base, or a declared interface. The witness's type
    /// args are unified against `T`'s, so a free var in the target (`this :> seq<_>`) pins.
    and inferStaticUpcast
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        if not (tryCoerceUpcast ctx node.Tok srcTy tgtTy) then
            ctx.Report(node.Tok, Kind.UpcastUnrelated(shown ctx.Store srcTy, shown ctx.Store tgtTy))

        ctx.MarkTypeDeclared(node.Key, tgtTy)
        tgtTy

    /// `e :? T` — type test. The static types must be related in either direction; an
    /// unrelated test is statically always-false.
    and inferDynamicTypeTest
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t
        // The node's own type is `bool`, so stash the tested-against type. Nothing else
        // records what the emitted `isinst` tests against.
        ctx.Resolution.TypeTestTargets.Set(node.Key, tgtTy)

        // A nullable-reference source tests exactly as its non-null part does, the way a
        // downcast reads it. Diagnostics still show the original `srcTy`.
        let checkSrc = stripReferenceNull ctx.Store srcTy

        let related =
            isObjTy ctx.Store checkSrc
            || subsumes ctx checkSrc tgtTy <> SubsumeOutcome.Unrelated
            || subsumes ctx tgtTy checkSrc <> SubsumeOutcome.Unrelated

        if not related then
            ctx.Report(node.Tok, Kind.UnrelatedTypeTest(shown ctx.Store srcTy, shown ctx.Store tgtTy))

        ctx.Intrinsics.Bool

    /// `e :?> T` — explicit downcast. The target must be a strict descendant of the
    /// source; an equal static type warns as redundant, an unrelated one errors. A
    /// downcast from `obj` is always admissible, checked at runtime.
    and inferDynamicDowncast
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        // A nullable-reference source downcasts exactly as its non-null part does:
        // `obj | null :?> C` is fine because `obj` has proper subtypes, `string | null :?> C`
        // is FS0016 because `string` is sealed. Diagnostics still show the original `srcTy`.
        let checkSrc = stripReferenceNull ctx.Store srcTy

        // A still-unresolved source TyVar is admitted, runtime-checked like `obj`: an
        // override's unannotated param (`that` in `IStructuralEquatable.Equals`) is pinned
        // to `obj` only by the conformance unify that runs after the body.
        let isUnresolvedVar =
            match resolveStep ctx.Store checkSrc with
            | TyVar _ -> true
            | _ -> false

        if not (isObjTy ctx.Store checkSrc) && not isUnresolvedVar then
            match subsumes ctx tgtTy checkSrc with
            | SubsumeOutcome.Subtype -> ()
            | SubsumeOutcome.Equal -> ctx.Report(node.Tok, Kind.RedundantDowncast(shown ctx.Store srcTy))
            | SubsumeOutcome.Unrelated ->
                ctx.Report(node.Tok, Kind.DowncastUnrelated(shown ctx.Store srcTy, shown ctx.Store tgtTy))

        ctx.MarkTypeDeclared(node.Key, tgtTy)
        tgtTy
