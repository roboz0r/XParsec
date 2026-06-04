namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine
open UnificationTranslate

module UnificationInferOverload =

    let rec semTypeEq (a: SemType) (b: SemType) : bool =
        match zonk a, zonk b with
        // A generic method's own typar (`Take<TSource>` ⇒ `TempTypar(Method, _)`,
        // baked into the open signature by `BuildSignature`) is unconstrained — it
        // matches any argument during applicability filtering, so a generic
        // external method resolves against concrete call-site types
        // (frozen-type-plan 2C). The eventual instantiation is recovered by
        // `tryInferExternalStaticMethodCall` (front end) / `recoverTypeArgs`
        // (codegen); here it is a wildcard at any structural depth.
        | TempTypar(TyparAxis.Method, _), _
        | _, TempTypar(TyparAxis.Method, _) -> true
        | TyConst(n1, xs), TyConst(n2, ys) -> n1 = n2 && EqArray.forall2 semTypeEq xs ys
        | TyVar x, TyVar y -> System.Object.ReferenceEquals(UnionFind.find x, UnionFind.find y)
        | TyFun(a1, r1), TyFun(a2, r2) -> semTypeEq a1 a2 && semTypeEq r1 r2
        | TyTuple xs, TyTuple ys -> EqArray.forall2 semTypeEq xs ys
        | TyRecord(n1, xs), TyRecord(n2, ys)
        | TyUnion(n1, xs), TyUnion(n2, ys)
        | TyClass(n1, xs), TyClass(n2, ys) -> n1 = n2 && EqArray.forall2 semTypeEq xs ys
        | _ -> false

    /// `object`/`obj` is the only supertype we model — no other reference
    /// hierarchy, so a non-`object` param only matches an arg it equals.
    and isObjectTy (t: SemType) : bool =
        match zonk t with
        | TyClass(n, args) when args.IsEmpty && RuntimeNames.isSystemObjectKey n -> true
        | TyConst("obj", _) -> true
        | _ -> false

    and argAssignable (argTy: SemType) (paramTy: SemType) : bool =
        semTypeEq argTy paramTy || isObjectTy paramTy

    and asSpecificOrEq (aTy: SemType) (bTy: SemType) : bool = semTypeEq aTy bTy || isObjectTy bTy

    /// `argSig` length distinguishes a flattened N-param method from a genuine
    /// single tuple param.
    and memberParamCount (m: ExternalMember) : int =
        match m.Key with
        | SymbolKey.MemberKey(_, _, argSig, _) -> argSig.Length
        | _ -> 0

    /// Flattens the tupled signature back to N parameters.
    and memberParamTypes (typeArgs: SemType[]) (m: ExternalMember) : SemType list =
        let n = memberParamCount m

        match zonk (ExternalSymbols.openSignature m typeArgs) with
        | TyFun(TyTuple elems, _) when n >= 2 && elems.Length = n -> EqArray.toList elems
        | TyFun(TyConst("unit", _), _) when n = 0 -> []
        | TyFun(p, _) -> [ p ]
        | _ -> []

    /// `ValueNone` = none applicable, or no unique best (ambiguous — the caller diagnoses).
    and pickStaticOverload
        (typeArgs: SemType[])
        (candidates: ExternalMember[])
        (argElems: SemType list)
        : ExternalMember voption =
        let arity = List.length argElems

        let applicable =
            candidates
            |> Array.filter (fun m ->
                memberParamCount m = arity
                && (let ps = memberParamTypes typeArgs m
                    List.length ps = arity && List.forall2 argAssignable argElems ps)
            )

        match applicable with
        | [||] -> ValueNone
        | [| only |] -> ValueSome only
        | many ->
            let betterThan (a: ExternalMember) (b: ExternalMember) =
                let pa = memberParamTypes typeArgs a
                let pb = memberParamTypes typeArgs b

                List.forall2 asSpecificOrEq pa pb
                && List.exists2 (fun x y -> not (semTypeEq x y)) pa pb

            let best =
                many
                |> Array.filter (fun a ->
                    many
                    |> Array.forall (fun b -> System.Object.ReferenceEquals(a, b) || betterThan a b)
                )

            match best with
            | [| unique |] -> ValueSome unique
            | _ -> ValueNone

    /// `FrozenType`-typed entry point for codegen (external-signature-plan step 4):
    /// codegen's `externalCtor` re-runs the same-arity ctor pick on a `SemType`-free
    /// (`FrozenType`) basis. Overload resolution is inference, so it stays here and
    /// names `SemType` internally; the use-site type arguments / call-site arg types
    /// arrive ground (frozen) and `ofFrozen` recovers the ground `SemType` the picker
    /// compares. Identity (which `ExternalMember`) is what's returned, so no `SemType`
    /// crosses back to the emission side.
    let pickStaticOverloadFrozen
        (typeArgs: FrozenType[])
        (candidates: ExternalMember[])
        (argElems: FrozenType list)
        : ExternalMember voption =
        pickStaticOverload (Array.map ofFrozen typeArgs) candidates (List.map ofFrozen argElems)
