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

    /// Explicit type application on a value/constructor head: `Set<'T>(args)`
    /// (`set.fs` construction sites), `Box<int>(x)`, etc. The CST shape is
    /// `HighPrecedenceApp(TypeApp(head, [tyArgs]), valueArgs)`, so this types the
    /// `TypeApp` node to the head's curried ctor / function type — the enclosing
    /// App then unifies the value args as usual. The supplied type arguments are
    /// unified pairwise against the head's *nominal result* type arguments so the
    /// instantiation is pinned even when the value args alone wouldn't determine
    /// it (e.g. `ResizeArray<int>()`). A non-nominal result (a bare generic
    /// *function*, `id<int>`) carries its typars scattered through the function
    /// type rather than in a single nominal result; v1 leaves those to value-arg
    /// inference — the explicit args are a no-op there, matching eliding `<…>`.
    /// External generic-static *member* receivers (`EqualityComparer<int>.Default`)
    /// never reach here — they are a `DotLookup` over the `TypeApp`, handled by
    /// `tryExternalTypeReceiver` upstream.
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

        match resultOf innerTy with
        | TyClass(_, freshArgs)
        | TyUnion(_, freshArgs)
        | TyRecord(_, freshArgs) when freshArgs.Length = List.length explicit ->
            List.iter2 (fun fresh ex -> unify ctx tok fresh ex) (EqArray.toList freshArgs) explicit
        | _ -> ()

        innerTy

    /// Value-level inline IL `(# "op" args : retTy #)`. The instruction string is
    /// opaque to the type-checker (the IL contract is the platform author's
    /// responsibility); we only type each operand so its own subtree is solved,
    /// and take the node's type from the declared result annotation (no annotation
    /// → `unit`). Value-level analogue of the type-level `Type.ILIntrinsic`.
    and inferILIntrinsic
        (infer: Infer)
        (ctx: PassContext)
        (args: ImmutableArray<Expr<SyntaxToken>>)
        (returnType: ReturnType<SyntaxToken> voption)
        : SemType =
        let argTys = [| for a in args -> infer ctx a |]

        // A bare `null` operand mints its own fresh TypeVar (`Expr.Null`) with no
        // pinning context. In a binary compare against a typed operand — the
        // `(# "ceq" value null : bool #)` shape of `isNull` — that var never links,
        // and because `isNull` is `inline` it rides the spliced body into every
        // caller, surfacing as a spurious `ResolvedTypes: unresolved TyVar`. Pin
        // each `null` operand to the first non-`null` operand's type (the
        // type-checker still treats the *instruction* as opaque; this only solves
        // the otherwise-context-free `null` leaf).
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

    /// `expr when ^T : Type [and ^U : Type]* = optimizedExpr` — one clause of an
    /// F# library-only static optimization. Type the default `baseE` (its type is
    /// the node's type — the operator's declared result, e.g. `bool` for the
    /// equality family, `^T3` for `(+)`) and type this clause's `optimizedExpr` so
    /// its own subtree (operands, nested inline IL) is solved.
    ///
    /// The clause body is **NOT** cross-unified with the base. F#'s static-opt
    /// rule is per-clause — "assume the constraint, then check the body against the
    /// return type": under `when ^T1 : int …` the body's `int` matches the (then-also
    /// -`int`) declared result `^T3`. The earlier blanket `unify baseTy optTy` only
    /// happens to work when every clause shares one concrete type (the equality
    /// family's `bool`); it wrongly fuses the distinct clause results of an
    /// `^T3`-returning op — `byte`/`int16`/`^T3` for `(+)` — and fails to unify them.
    /// We omit that check (a fully sound version would speculatively unify under
    /// the assumed constraint and undo — out of scope, by the
    /// no-speculative-unification stop); soundness rides on the clause being
    /// selected (and its body substituted) at expansion, where `^T` is concrete.
    ///
    /// The `when ^T : Type` constraints are a *compile-time dispatch*, NOT
    /// unification constraints, so the typar is **not** unified with its required
    /// type; it is translated only to record the verdict for `Inline.inlineExpand`
    /// to resolve at the call site. The typar resolves through `ctx.Resolution.TyparScope` —
    /// already seeded by the enclosing binding's parameters (`(x: ^T)`) — so the
    /// recorded `SemType` carries the binding's quantified root.
    and inferLibraryOnlyStaticOptimization
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (baseE: Expr<SyntaxToken>)
        (constraints: ImmutableArray<StaticOptimizationConstraint<SyntaxToken>>)
        (optimizedExpr: Expr<SyntaxToken>)
        : SemType =
        let baseTy = infer ctx baseE
        infer ctx optimizedExpr |> ignore

        let resolved =
            EqArray.ofSeq (
                seq {
                    for c in constraints do
                        match c with
                        | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon(typar = tp; rhsType = rhs) ->
                            TStaticOptConstraint.TyconEquals(translateType ctx (Type.VarType tp), translateType ctx rhs)
                        | StaticOptimizationConstraint.WhenTyparIsStruct(typar = tp) ->
                            TStaticOptConstraint.IsStruct(translateType ctx (Type.VarType tp))
                }
            )

        ctx.StaticOpt.Set(key, resolved)
        baseTy

    /// `((^T1 or ^T2): (static member (+) : ^T1 * ^T2 -> ^T3) (x, y))` — an SRTP
    /// member-trait call, only ever the static-opt BASE of a `let inline` arithmetic
    /// operator (`ops-platform.fs`). The member is resolved at inline
    /// expansion (the typars are abstract here), so inference only types the argument
    /// tuple — so its operand subtrees are solved — and yields the member signature's
    /// declared RETURN type. That is `^T3`, which for a heterogeneous operator
    /// (`Vec2 * float -> Vec2`) is neither operand's type; reading it off the first
    /// argument instead would type the node as `^T1` and Elaborate would stamp that
    /// wrong type onto the `TExpr.TraitCall` it lowers to.
    and inferStaticMemberInvocation
        (infer: Infer)
        (ctx: PassContext)
        (msig: MemberSig<SyntaxToken>)
        (argExpr: Expr<SyntaxToken>)
        : SemType =
        infer ctx argExpr |> ignore

        // The trait's typars resolve through the enclosing `let inline`'s
        // `ctx.Resolution.TyparScope`, so `^T3` here IS the binding's declared result
        // typar — the same root the operator's `: ^T3` return annotation carries.
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

        // Type provenance: `(e : T)` writes the node's type explicitly.
        ctx.MarkTypeDeclared(node.Key, annTy)

        // E1(a): a format-string literal ascribed to a `PrintfFormat` family
        // (`("%d" : Printf.StringFormat<_>)`, and the `let fmt = (… : Fmt)` form that
        // desugars to it) types AS the format, not `string`. Skip `infer` on the
        // literal (it would type it `string` and pin the node's TyVar); the helper
        // unifies the specifiers' printer into the annotation (pinning a `<_>` wildcard
        // printer), and we stamp the annotation's format type onto the literal node.
        // Otherwise the ordinary annotation reconciliation.
        match tryTypeFormatLiteral ctx node.Tok inner annTy with
        | ValueSome fmt ->
            ctx.Store.SetLink(UnionFind.find ctx.Store (freshTv ctx (CstKeys.ofExpr inner)), ValueSome fmt)
            annTy
        | ValueNone ->
            let innerTy = infer ctx inner
            unify ctx node.Tok innerTy annTy

            // "Name the type at the escape point": an ascription DIRECTLY on a `?` expression
            // (`(d?foo : int)`) is an explicit assertion, so it suppresses the implicit-escape
            // warning `DynamicEscape.run` would otherwise raise. An annotation on the binding
            // (`let n : int = d?foo`) is NOT on the `?` node and still warns.
            match inner with
            | Expr.DynamicLookup _ -> ctx.DynamicEscapeSuppressed.Add(CstKeys.ofExpr inner) |> ignore
            | _ -> ()

            annTy

    /// `obj` is the top of every reference hierarchy. `subsumes` doesn't model
    /// it (the BCL `System.Object` class isn't in `ctx.Types.Class`), so the
    /// coercion arms special-case it: a downcast / type-test from `obj` to any
    /// known type is statically admissible and resolved at runtime. The
    /// `set.fs:988` `(that :?> Set<'T>).Tree` site relies on this.
    and isObjTy (store: TypeStore) (t: SemType) : bool =
        match resolveStep store t with
        | TyObj -> true
        | _ -> false

    /// `e :> T` — explicit upcast. `src` must instantiate `T`'s nominal (itself
    /// — a redundant but legal upcast — a base, or a declared interface);
    /// `tryCoerceUpcast` both verifies that and unifies the witness's type args
    /// against `T`'s, so a free var in the target (`this :> seq<_>`) is pinned.
    /// The result type is the target.
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

        // Type provenance: `e :> T` writes the node's (target) type explicitly.
        ctx.MarkTypeDeclared(node.Key, tgtTy)
        tgtTy

    /// `e :? T` — type test. v1 requires the static types to be related in
    /// either direction (an unrelated test is statically always-false); the
    /// result is always `bool`.
    and inferDynamicTypeTest
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t
        // The node's own type is `bool`; stash the tested-against type so Elaborate
        // can carry it into `TExpr.TypeTest.testTy` for the `isinst` operand.
        ctx.Resolution.TypeTestTargets.Set(node.Key, tgtTy)

        let related =
            isObjTy ctx.Store srcTy
            || subsumes ctx srcTy tgtTy <> SubsumeOutcome.Unrelated
            || subsumes ctx tgtTy srcTy <> SubsumeOutcome.Unrelated

        if not related then
            ctx.Report(node.Tok, Kind.UnrelatedTypeTest(shown ctx.Store srcTy, shown ctx.Store tgtTy))

        ctx.Intrinsics.Bool

    /// `e :?> T` — explicit downcast. The target must be a strict descendant of
    /// the source (`subsumes tgt src = Subtype`); an equal static type warns
    /// (redundant), an unrelated one errors. A downcast from `obj` is always
    /// admissible (checked at runtime).
    and inferDynamicDowncast
        (infer: Infer)
        (ctx: PassContext)
        (node: NodeSite)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        // A nullable-reference source `T | null` downcasts EXACTLY as its non-null part
        // `T` does — F# governs the coercion by the non-null type's proper-subtype
        // structure (`obj | null :?> C` is fine because `obj` has proper subtypes;
        // `string | null :?> C` is FS0016 because `string` is sealed). So erase the
        // `null` member and run the ordinary downcast check on the remainder. (The
        // diagnostics still show the original `srcTy` so the user sees `string | null`.)
        let checkSrc = stripReferenceNull ctx.Store srcTy

        // A still-unresolved source TyVar is admitted (runtime-checked, like `obj`):
        // an interface/override member's unannotated param (`that` in
        // `IStructuralEquatable.Equals`) is pinned to `obj` only by the *conformance*
        // unify that runs after the body — so the operand is a free var here. We
        // can't prove unrelatedness of an unknown type, so no static error (G21).
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

        // Type provenance: `e :?> T` writes the node's (target) type explicitly.
        ctx.MarkTypeDeclared(node.Key, tgtTy)
        tgtTy
