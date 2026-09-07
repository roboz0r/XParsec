namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open EmitLower

/// Eta-bridging: every NON-saturated reference to a function emitted as a flat static method
/// becomes a closure over a saturated call, leaving the flat method the function's only form.
module EmitBridges =
    /// Eta-expand every NON-saturated reference to a function in `arity` to its source arity,
    /// in every expression a declaration carries, member bodies included:
    ///   `f` → `fun a0 … a(n-1) -> f a0 … a(n-1)`, and `f x` → that lambda applied to `x`.
    let private bridgeReferences
        (arity: Dictionary<BoundVarId, int>)
        (decls: TastAccessor.DeclId list)
        : TastAccessor.DeclId list =
        if arity.Count = 0 then
            decls
        else
            // `fun a0 … a(n-1) -> f a0 … a(n-1)`, typed from the reference's own curried
            // type. For a tuple or unit source group the single fresh param carries the
            // whole group's domain and is passed as one argument.
            let buildEta (fVar: TastAccessor.ExprId) (n: int) : TastAccessor.ExprId =
                let tok = TastAccessor.exprTok fVar

                // Each peeled `->` as `(domain, codomain)`: the fresh param's type and the
                // intermediate `App` result type.
                let levels = TastLower.peelFuns n (typeOfExpr fVar)

                if List.length levels <> n then
                    failwithf
                        "bridgeStaticFnEscapes: function type has fewer than %d parameters: %A"
                        n
                        (typeOfExpr fVar)

                let keys = levels |> List.map (fun _ -> TastPoolBuilder.mintBoundVar fVar.Pool)

                // The bridge's nodes append to the pool `fVar` already lives in, so the
                // spliced `fVar` keeps its own id.
                let pool = fVar.Pool

                let appliedArgs: TastAccessor.AppliedArg list =
                    List.map2
                        (fun k (dom, cod) ->
                            {
                                Arg = TastAccessor.mintVar pool k dom tok
                                StepResultTy = cod
                                Tok = tok
                            }
                        )
                        keys
                        levels

                let body = TastAccessor.mintAppChain fVar appliedArgs

                List.foldBack2
                    (fun k (dom, cod) acc ->
                        TastAccessor.mintLambda (TastAccessor.mintNamedPat pool k dom tok) acc (FTFun(dom, cod)) tok
                    )
                    keys
                    levels
                    body

            let rec rw (e: TastAccessor.ExprId) : TastAccessor.ExprId =
                match e with
                | TastAccessor.EVar k when arity.ContainsKey k -> buildEta e arity.[k]
                | TastAccessor.EApp _ ->
                    let fn, args = TastAccessor.collectAppChain [] e

                    match fn with
                    | TastAccessor.EVar k when arity.ContainsKey k && List.length args < arity.[k] ->
                        // Under-application: partially apply the eta closure.
                        TastAccessor.mintAppChain
                            (buildEta fn arity.[k])
                            (args |> List.map (fun a -> { a with Arg = rw a.Arg }))
                    | _ ->
                        // A saturated (or over-applied) eligible function stays a direct
                        // `call`: the head is left alone and only the arguments are rewritten.
                        TastAccessor.mapAppChain
                            (fun h ->
                                match h with
                                | TastAccessor.EVar k when arity.ContainsKey k -> h
                                | _ -> rw h
                            )
                            rw
                            e
                | _ -> TastAccessor.mapChildren rw e

            decls |> List.map (TastAccessor.mapDeclBodies rw)

    /// `bridgeReferences` over the `eligible` static-method functions, keeping the flat
    /// static method a cross-assembly consumer `call`s.
    let bridgeStaticFnEscapes
        (eligible: HashSet<BoundVarId>)
        (fns: CompiledFns.CompiledFn list)
        (decls: TastAccessor.DeclId list)
        : TastAccessor.DeclId list =
        // Each eligible function's source arity: the parameters the eta-expansion peels, and
        // the argument count at or above which a reference is a saturated direct `call`.
        let arity = Dictionary<BoundVarId, int>()

        for f in fns do
            if eligible.Contains f.Key then
                arity.[f.Key] <- f.Params.GroupCount

        bridgeReferences arity decls

    /// `let name = value in body` where `name` is a generalised local with a positive typar
    /// count: the shape `discoverClosures` lifts to a generic static method.
    [<return: Struct>]
    let (|LetBoundGenericLocal|_|)
        (localSchemes: IReadOnlyDictionary<BoundVarId, LocalScheme>)
        (e: TastAccessor.ExprId)
        : struct (BoundVarId * LocalScheme * TastAccessor.LetView) voption =
        match e with
        | TastAccessor.ELet letv ->
            match TastAccessor.patBoundVar letv.Binding.Pattern with
            | ValueSome k ->
                match localSchemes.TryGetValue k with
                | true, scheme -> ValueSome(struct (k, scheme, letv))
                | false, _ -> ValueNone
            | ValueNone -> ValueNone
        | _ -> ValueNone

    /// `bridgeReferences` over every generalised local of the file, at the source arity of the
    /// local's lambda chain. A local bound to a non-lambda value (`let g = id`) has arity `0`
    /// and stays unbridged: a bare reference `call`s it as a generic module value is called.
    let bridgeLiftedLocalEscapes
        (localSchemes: IReadOnlyDictionary<BoundVarId, LocalScheme>)
        (decls: TastAccessor.DeclId list)
        : TastAccessor.DeclId list =
        let arity = Dictionary<BoundVarId, int>()

        let rec collect (e: TastAccessor.ExprId) =
            match e with
            | LetBoundGenericLocal localSchemes (k, _, letv) ->
                match TastLower.peelValRepr letv.Binding.Value with
                | (_ :: _ as groups), _ -> arity.[k] <- List.length groups
                | [], _ -> ()
            | _ -> ()

            iterChildren collect e

        for d in decls do
            TastAccessor.iterDeclBodies collect d

        bridgeReferences arity decls
