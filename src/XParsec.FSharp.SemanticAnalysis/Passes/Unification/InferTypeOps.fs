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
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (typeArgs: ImmutableArray<Type<SyntaxToken>>)
        : SemType =
        let innerTy = infer ctx inner
        let explicit = [ for t in typeArgs -> translateType ctx t ]

        let rec resultOf t =
            match resolveStep t with
            | TyFun(_, r) -> resultOf r
            | other -> other

        match resultOf innerTy with
        | TyClass(_, freshArgs)
        | TyUnion(_, freshArgs)
        | TyRecord(_, freshArgs) when freshArgs.Length = List.length explicit ->
            List.iter2 (fun fresh ex -> unify ctx key fresh ex) (EqArray.toList freshArgs) explicit
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
                    unify ctx (CstKeys.ofExpr args.[i]) argTys.[i] argTys.[anchor]
        | None -> ()

        match returnType with
        | ValueSome(ReturnType(typ = t)) -> translateType ctx t
        | ValueNone -> BuiltinTypes.tyUnit

    /// `expr when ^T : Type [and ^U : Type]* = optimizedExpr` — one clause of an
    /// F# library-only static optimization. Type the default `baseE` (its type is
    /// the node's type — the operator's declared result, e.g. `bool` for the
    /// equality family, `^T` for `(+)`) and type this clause's `optimizedExpr` so
    /// its own subtree (operands, nested inline IL) is solved.
    ///
    /// The clause body is **NOT** cross-unified with the base. F#'s static-opt
    /// rule is per-clause — "assume the constraint, then check the body against the
    /// return type": under `when ^T : int` the body's `int` matches the (then-also
    /// -`int`) declared result `^T`. The earlier blanket `unify baseTy optTy` only
    /// happens to work when every clause shares one concrete type (the equality
    /// family's `bool`); it wrongly fuses the distinct clause results of an
    /// `^T`-returning op — `byte`/`int16`/`^T` for `(+)` — and fails to unify them.
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
    /// recorded `SemType` carries the binding's quantified root. See
    /// docs/operators-plan.md (the arithmetic/bitwise/unary task).
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

    and inferTypeAnnotation
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let innerTy = infer ctx inner
        let annTy = translateType ctx t
        unify ctx key innerTy annTy
        annTy

    /// `obj` is the top of every reference hierarchy. `subsumes` doesn't model
    /// it (the BCL `System.Object` class isn't in `ctx.Types.Class`), so the
    /// coercion arms special-case it: a downcast / type-test from `obj` to any
    /// known type is statically admissible and resolved at runtime. The
    /// `set.fs:988` `(that :?> Set<'T>).Tree` site relies on this.
    and isObjTy (t: SemType) : bool =
        match resolveStep t with
        | TyConst("obj", _) -> true
        | _ -> false

    /// `e :> T` — explicit upcast. `src` must instantiate `T`'s nominal (itself
    /// — a redundant but legal upcast — a base, or a declared interface);
    /// `tryCoerceUpcast` both verifies that and unifies the witness's type args
    /// against `T`'s, so a free var in the target (`this :> seq<_>`) is pinned.
    /// The result type is the target.
    and inferStaticUpcast
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        if not (tryCoerceUpcast ctx key srcTy tgtTy) then
            ctx.Error(
                key,
                sprintf "Cannot upcast type '%A' to '%A' — no inheritance relationship" (zonk srcTy) (zonk tgtTy)
            )

        tgtTy

    /// `e :? T` — type test. v1 requires the static types to be related in
    /// either direction (an unrelated test is statically always-false); the
    /// result is always `bool`.
    and inferDynamicTypeTest
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t
        // The node's own type is `bool`; stash the tested-against type so Freeze
        // can carry it into `TExpr.TypeTest.testTy` for the `isinst` operand.
        ctx.Resolution.TypeTestTargets.Set(key, tgtTy)

        let related =
            isObjTy srcTy
            || subsumes ctx srcTy tgtTy <> SubsumeOutcome.Unrelated
            || subsumes ctx tgtTy srcTy <> SubsumeOutcome.Unrelated

        if not related then
            ctx.Warn(
                key,
                sprintf "Type test of '%A' against unrelated type '%A' is always false" (zonk srcTy) (zonk tgtTy)
            )

        BuiltinTypes.tyBool

    /// `e :?> T` — explicit downcast. The target must be a strict descendant of
    /// the source (`subsumes tgt src = Subtype`); an equal static type warns
    /// (redundant), an unrelated one errors. A downcast from `obj` is always
    /// admissible (checked at runtime).
    and inferDynamicDowncast
        (infer: Infer)
        (ctx: PassContext)
        (key: NodeKey)
        (inner: Expr<SyntaxToken>)
        (t: Type<SyntaxToken>)
        : SemType =
        let srcTy = infer ctx inner
        let tgtTy = translateType ctx t

        // A still-unresolved source TyVar is admitted (runtime-checked, like `obj`):
        // an interface/override member's unannotated param (`that` in
        // `IStructuralEquatable.Equals`) is pinned to `obj` only by the *conformance*
        // unify that runs after the body — so the operand is a free var here. We
        // can't prove unrelatedness of an unknown type, so no static error (G21).
        let isUnresolvedVar =
            match resolveStep srcTy with
            | TyVar _ -> true
            | _ -> false

        if not (isObjTy srcTy) && not isUnresolvedVar then
            match subsumes ctx tgtTy srcTy with
            | SubsumeOutcome.Subtype -> ()
            | SubsumeOutcome.Equal ->
                ctx.Warn(key, sprintf "Downcast is redundant — the static type '%A' already matches" (zonk srcTy))
            | SubsumeOutcome.Unrelated ->
                ctx.Error(key, sprintf "Cannot downcast type '%A' to unrelated type '%A'" (zonk srcTy) (zonk tgtTy))

        tgtTy
