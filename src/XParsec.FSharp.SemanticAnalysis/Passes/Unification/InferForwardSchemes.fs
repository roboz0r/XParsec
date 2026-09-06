namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationEngine
open UnificationTranslate
open UnificationInferGeneralize
open UnificationInferLiterals
open UnificationInferResolve
open UnificationInferPat
open UnificationInferOverload

/// The annotation-derived forward-scheme pre-pass: schemes for module-level functions
/// synthesised from the binding's annotations alone, before any body is typed. Nothing here
/// calls `infer`, so it lives ahead of it in the pass order.
module internal UnificationInferForwardSchemes =

    /// Seed a binding's explicit `<'a>` typar defns into the current `TyparScope` at
    /// `CurrentLevel`, so later implicit `'a` mentions share the same var. Reuses the
    /// member's prototype from `BindingTyparSeed` on a name match. Constraints are NOT seeded.
    let seedBindingTypars (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.typarDefns with
        | ValueSome(TyparDefns(defns = ds)) ->
            for TyparDefn(typar = t) in ds do
                match t with
                | Typar.Named(ident = id)
                | Typar.Static(ident = id) ->
                    let n = ctx.NameOf id

                    if not (ctx.Resolution.TyparScope.ContainsKey n) then
                        let tv =
                            match ctx.Resolution.BindingTyparSeed with
                            | ValueSome seed ->
                                match seed.TryGetValue n with
                                | true, proto -> proto
                                | _ -> ctx.FreshTyVar()
                            | ValueNone -> ctx.FreshTyVar()

                        ctx.Resolution.TyparScope.[n] <- tv
                | Typar.Anon _ -> ()
        | ValueNone -> ()

    /// The declared type annotation of a curried argument pattern, peeling the `( … )` /
    /// `as` / attribute wrappers the parser leaves around `(x: int)`. A tuple or otherwise
    /// compound arg carries no single annotation at this level and returns `ValueNone`.
    let rec private tryArgAnnotation (p: Pat<SyntaxToken>) : Type<SyntaxToken> voption =
        match p with
        | Pat.Typed(typ = t) -> ValueSome t
        | Pat.EnclosedBlock(pat = inner) -> tryArgAnnotation inner
        | Pat.As(pat = inner) -> tryArgAnnotation inner
        | Pat.Attributed(pat = inner) -> tryArgAnnotation inner
        | _ -> ValueNone

    /// Pre-seed an annotation-derived scheme for each generalisable module function, so a
    /// class member forward-referencing one instantiates fresh rather than pinning the
    /// function's binding-site TyVar. Unannotated arg / return slots get a fresh typar.
    let prebindModuleFunctionSchemes (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        for b in bindings do
            if shouldGeneralise b && not b.argumentPats.IsEmpty then
                let key = CstKeys.ofPat b.pattern

                if (ctx.Bindings.Scheme.TryGetValue key).IsNone then
                    use _ =
                        ctx.PushTyparScope(Dictionary<string, TyVarId>(System.StringComparer.Ordinal), false)

                    let outerLevel = ctx.CurrentLevel

                    seedBindingTypars ctx b

                    enterLevel ctx

                    let argTypes =
                        [
                            for p in b.argumentPats ->
                                match tryArgAnnotation p with
                                | ValueSome t -> translateType ctx t
                                | ValueNone -> TyVar(ctx.FreshTyVar())
                        ]

                    let retTy =
                        match b.returnType with
                        | ValueSome(ReturnType(typ = t)) -> translateType ctx t
                        | ValueNone -> TyVar(ctx.FreshTyVar())

                    let fnTy = List.foldBack (fun a r -> TyFun(a, r)) argTypes retTy
                    exitLevel ctx
                    let scheme = generalise ctx.Store (zonk ctx.Store fnTy) outerLevel
                    ctx.Bindings.Scheme.Set(key, scheme)
