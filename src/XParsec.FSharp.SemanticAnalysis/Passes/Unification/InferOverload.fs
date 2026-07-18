namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate

module UnificationInferOverload =

    /// A union whose every member is a structural literal — kept by-VALUE in overload
    /// filtering (its `argSigOf` spelling is sharp), unlike a union with a function /
    /// carried-node member, which is applicability-opaque.
    let isPureLiteralUnion (ms: UnionMembers) : bool =
        ms.Members
        |> EqSet.forall (fun m ->
            match zonk m with
            | TyLiteral _ -> true
            | _ -> false
        )

    /// Does `t` carry a not-yet-ground type-level computation anywhere inside it?
    /// Only such a type is genuinely applicability-OPAQUE; a plain nominal /
    /// primitive union must NOT act as a filtering wildcard (it would perturb BCL
    /// overload sets — any union-typed argument would match every same-arity
    /// parameter of every overloaded external method).
    let rec hasCarriedNode (t: SemType) : bool =
        match zonk t with
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> true
        | t -> SemType.existsChild hasCarriedNode t

    /// NOT an equality: the structural-match relation overload FILTERING uses.
    /// Wildcard arms (open method typars, carried type-level nodes, opaque unions)
    /// deliberately return `true` for anything — "indistinguishable during
    /// filtering", with the real admission at the `unifyAppliedSig` commit seam.
    /// `canon` normalises a nominal key to its capability-canonical face (identity for
    /// every non-capability key) so a `seq` argument admits a BCL `IEnumerable\`1`
    /// parameter of an overloaded external method (`Enumerable.Take`) — the overload-
    /// filter mirror of the `unify` / `subsumes` capability reconciliation. The
    /// `SemType`-free codegen path passes `id` (frozen overloads carry no capability
    /// gap; a mismatch there just falls back to the first arity match).
    let rec applicabilityMatches (canon: SymbolKey -> SymbolKey) (a: SemType) (b: SemType) : bool =
        match zonk a, zonk b with
        // A generic method's own typar (`Take<TSource>` ⇒ `TyTypar(Method, _)`,
        // kept as a wildcard in the open signature by `ExternalSymbols.openSignature`)
        // is unconstrained — it matches any argument during applicability filtering,
        // so a generic external method resolves against concrete call-site types.
        // The eventual instantiation is recovered by
        // `tryInferExternalStaticMethodCall` (front end) / `recoverTypeArgs`
        // (codegen); here it is a wildcard at any structural depth.
        | TyTypar(TyparAxis.Method, _), _
        | _, TyTypar(TyparAxis.Method, _) -> true
        // A CARRIED TS type-level computation (`keyof`/`T[K]`/conditional) is
        // applicability-OPAQUE — its structural identity can't be decided until a call
        // site grounds it (R4a step 3), so during overload FILTERING it matches any
        // argument, exactly like an open method typar. The real solution happens at the
        // `unifyAppliedSig` commit seam (where the fold fires). External-vocabulary only,
        // so this never perturbs a BCL overload set (none carry these nodes).
        | (TyKeyOf _ | TyIndexedAccess _ | TyConditional _), _
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> true
        // A non-literal UNION parameter (off's optional `Handler<Events[Key]> | undefined`)
        // is applicability-OPAQUE during filtering, same as the carried-node / open-typar
        // wildcards above: its members can carry a not-yet-ground `T[K]` and the real
        // admission (member subsumption + carried-node fold) happens at the
        // `unifyAppliedSig` commit seam. A PURE literal union stays by-VALUE below so
        // literal-union overload specificity (the sharp `argSigOf` spelling) is preserved.
        // A `TyOr` only ever arises from TS vocabulary, so a param-side wildcard cannot
        // perturb a BCL overload set.
        | _, TyOr ms when not (isPureLiteralUnion ms) -> true
        // The ARGUMENT side is a wildcard ONLY when the union genuinely carries a
        // not-yet-ground node — the actual motivation. A plain nominal/primitive union
        // argument (`string | MyClass` by annotation) must fall through to structural
        // comparison, not match every same-arity parameter.
        | TyOr ms, _ when EqSet.exists hasCarriedNode ms.Members -> true
        // Two structural literals are equal by VALUE (so `on("*", …)` prefers the
        // literal-`'*'` overload over a same-position typar); a literal vs a non-literal
        // falls through to `false` (a plain `string` is not a specific literal).
        | TyLiteral v1, TyLiteral v2 -> v1 = v2
        | TyConst(k1, xs), TyConst(k2, ys) -> k1 = k2 && EqArray.forall2 (applicabilityMatches canon) xs ys
        | TyVar x, TyVar y -> System.Object.ReferenceEquals(UnionFind.find x, UnionFind.find y)
        | TyFun(a1, r1), TyFun(a2, r2) -> applicabilityMatches canon a1 a2 && applicabilityMatches canon r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 (applicabilityMatches canon) xs ys
        // The `canon n1 = canon n2` fallback fires only for a capability interface (its two
        // faces canonicalise equal); `canon` is identity for records/unions and every
        // non-capability class, so the common `n1 = n2` short-circuits unchanged.
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) ->
            (n1 = n2 || canon (SymbolKey.Type n1) = canon (SymbolKey.Type n2))
            && EqArray.forall2 (applicabilityMatches canon) xs ys
        | _ -> false

    /// `object`/`obj` is the only supertype we model — no other reference
    /// hierarchy, so a non-`object` param only matches an arg it equals.
    and isObjectTy (t: SemType) : bool =
        match zonk t with
        | TyObj -> true
        | _ -> false

    and argAssignable (canon: SymbolKey -> SymbolKey) (argTy: SemType) (paramTy: SemType) : bool =
        applicabilityMatches canon argTy paramTy || isObjectTy paramTy

    and asSpecificOrEq (canon: SymbolKey -> SymbolKey) (aTy: SemType) (bTy: SemType) : bool =
        applicabilityMatches canon aTy bTy || isObjectTy bTy

    /// Flattens the tupled signature back to N parameters. The `argSig` length — the
    /// member's own identity — distinguishes a flattened N-param method from a genuine
    /// single tuple param; the signature alone cannot.
    and memberParamTypes (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
        let n = m.Key.ArgSig.Length

        match zonk (ExternalSymbols.openSignature m typeArgs) with
        | TyFun(TyTuple elems, _) when n >= 2 && elems.Length = n -> EqArray.toList elems
        | TyFun(TyUnit, _) when n = 0 -> []
        | TyFun(p, _) -> [ p ]
        | _ -> []

    /// `ValueNone` = none applicable, or no unique best (ambiguous — the caller diagnoses).
    /// Static/instance/ctor agnostic: pure arity + `argAssignable` + specificity ranking
    /// over any `ExternalMember[]` candidate set (callers pre-filter by static-ness).
    and pickBestOverload
        (canon: SymbolKey -> SymbolKey)
        (typeArgs: SemType[])
        (candidates: ExternalMember[])
        (argElems: SemType list)
        : ExternalMember voption =
        let arity = List.length argElems

        let applicable =
            candidates
            |> Array.filter (fun m ->
                m.Key.ArgSig.Length = arity
                && (let ps = memberParamTypes typeArgs m
                    List.length ps = arity && List.forall2 (argAssignable canon) argElems ps)
            )

        match applicable with
        | [||] -> ValueNone
        | [| only |] -> ValueSome only
        | many ->
            let betterThan (a: ExternalMember) (b: ExternalMember) =
                let pa = memberParamTypes typeArgs a
                let pb = memberParamTypes typeArgs b

                List.forall2 (asSpecificOrEq canon) pa pb
                && List.exists2 (fun x y -> not (applicabilityMatches canon x y)) pa pb

            let best =
                many
                |> Array.filter (fun a ->
                    many
                    |> Array.forall (fun b -> System.Object.ReferenceEquals(a, b) || betterThan a b)
                )

            match best with
            | [| unique |] -> ValueSome unique
            | _ -> ValueNone
