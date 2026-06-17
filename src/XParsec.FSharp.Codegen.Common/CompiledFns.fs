namespace XParsec.FSharp.Codegen.Common

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// Backend-agnostic compiled-form analysis of a file's top-level module functions —
/// the facts both the CLR and JS backends need to lower a `let f … = …`. Two pieces:
///
///   * `gather` — each function's flat compiled signature (`CompiledForm`): the
///     SOURCE arity groups (how many spine applications a saturated call collapses),
///     the tuple-expanded / lone-unit-erased flat params, and the `void`-vs-value
///     return. Recomputed via the same `TastLower.peelValRepr` + `compiledOf` builders
///     Freeze ran on the `LetFn` node, so it equals the in-assembly `LetFn.compiled`.
///   * `escaping` — which of those functions are *used as a value or under-applied*
///     somewhere, so a curried view of them is needed in addition to the flat form.
///
/// The flat form is a function's ABI: a publicly reachable function ALWAYS exports it
/// (function-method-compiled-form-plan.md §D). `escaping` is therefore ADDITIVE — it
/// signals a curried bridge is *also* required, never that the flat form is suppressed.
/// Each backend layers its own policy on these facts: the JS backend emits every
/// function flat and a curried bridge for the escapers; the CLR backend currently still
/// *demotes* an escaping function entirely to a closure (its capture fixpoint stays
/// CLR-private), a known latent gap the plan tracks.
module CompiledFns =

    /// One top-level module-function binding's compiled form. `Groups.Length` is the
    /// number of source applications a saturated call consumes; `Params` is the flat
    /// CLR/JS parameter vector (a tupled group expands to N flat params, a lone unit
    /// group erases to none); `Body` is the residual after the groups are peeled.
    type CompiledFn =
        {
            Key: NodeKey
            Groups: Frozen.ArgGroup list
            Params: TastLower.StaticParam list
            Body: Frozen.TExpr
            ResultTy: FrozenType
            /// `true` when the source result is `unit` — CLR `void` / JS no return value.
            ReturnsVoid: bool
        }

    /// Gather every top-level `let f … = …` whose value peels to ≥ 1 source group
    /// (a function, not a zero-param value), in declaration order. Must be called on
    /// already-`lower`ed decls — a `LetFn` (the un-normalised frozen node) is a bug.
    let gather (decls: Frozen.TDecl list) : CompiledFn list =
        [
            for d in decls do
                match d with
                | TDeclG.Let(TPatG.NamedSimple(k, _, _), value, _, _) ->
                    match TastLower.peelValRepr value with
                    | (_ :: _ as groups), body ->
                        let resultTy = TastLower.typeOfExpr body

                        let vr: TastLower.ValRepr =
                            {
                                Typars = 0 // unused by `compiledOf`; the real count is a backend concern
                                Groups = groups
                                ResultTy = resultTy
                            }

                        let cf = TastLower.compiledOf vr

                        {
                            Key = k
                            Groups = groups
                            Params = cf.Params
                            Body = body
                            ResultTy = resultTy
                            ReturnsVoid =
                                match cf.Return with
                                | CompiledReturnG.RVoid -> true
                                | CompiledReturnG.RValue _ -> false
                        }
                    | [], _ -> ()
                | TDeclG.LetFn _ -> failwith "CompiledFns.gather: LetFn must be normalised to Let by lower"
                | _ -> ()
        ]

    /// Which gathered functions ESCAPE: referenced as a value, or applied through a
    /// spine shorter than their source arity (a partial application that needs a
    /// curried view). A reference that heads a spine of ≥ `Groups.Length` arguments is
    /// a direct flat call and does NOT escape. Mirrors the CLR escape walk exactly so
    /// both backends agree on the boundary.
    let escaping (fns: CompiledFn list) (decls: Frozen.TDecl list) : HashSet<NodeKey> =
        let arity = Dictionary<NodeKey, int>()

        for f in fns do
            arity.[f.Key] <- List.length f.Groups

        let escapes = HashSet<NodeKey>()

        let rec walk (e: Frozen.TExpr) =
            match e with
            | TExprG.Var(k, _, _) when arity.ContainsKey k -> escapes.Add k |> ignore
            | TExprG.App _ ->
                let head, args = TastWalk.collectSpine [] e

                match head with
                | TExprG.Var(k, _, _) when arity.ContainsKey k ->
                    if List.length args < arity.[k] then
                        escapes.Add k |> ignore

                    for (a, _, _) in args do
                        walk a
                | _ ->
                    walk head

                    for (a, _, _) in args do
                        walk a
            | _ -> TastLower.iterChildren walk e

        for d in decls do
            match d with
            | TDeclG.Let(_, value, _, _) -> walk value
            | TDeclG.Expression(e, _) -> walk e
            | TDeclG.LetFn _ -> failwith "CompiledFns.escaping: LetFn must be normalised to Let by lower"
            | TDeclG.Type _ -> ()

        escapes
