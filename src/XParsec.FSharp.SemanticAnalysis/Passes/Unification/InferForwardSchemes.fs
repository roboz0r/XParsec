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

/// The annotation-derived forward-scheme pre-pass (G19 residue). `walkElems`
/// types class member bodies before module-level `let`s, so a class member that
/// forward-references a sibling-module function must see a *scheme* (not the
/// function's monomorphic binding-site TyVar) for the argument-coercion upcast
/// to fire. These helpers synthesise that scheme from the binding's annotations
/// alone — they never call `infer`, so they live ahead of it in the pass order.
module UnificationInferForwardSchemes =

    /// Seed a binding's explicit `<'a>` typar defns into the current `TyparScope`
    /// as `CurrentLevel` `TypeVar`s, so later implicit `'a` mentions in the
    /// binding's annotations / body share the same var. Reuses the member's
    /// prototype typar from `BindingTyparSeed` when the name matches (B-12, so the
    /// inferred signature shares roots with `TypeMemberInfo.MethodTypeParams`);
    /// otherwise mints fresh. Does *not* translate constraints — that stays with
    /// the body-typing caller (`inferBinding`); the annotation-only forward
    /// pre-pass (`prebindModuleFunctionSchemes`) needs only the var bindings.
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
                                | _ ->
                                    let tv = TypeVar()
                                    tv.Level <- ctx.CurrentLevel
                                    tv
                            | ValueNone ->
                                let tv = TypeVar()
                                tv.Level <- ctx.CurrentLevel
                                tv

                        ctx.Resolution.TyparScope.[n] <- tv
                | Typar.Anon _ -> ()
        | ValueNone -> ()

    /// The declared type annotation of a curried argument pattern, if it carries
    /// one — peeling the `(…)`/`as` wrappers the parser leaves around
    /// `(comparer: IComparer<'T>)`. A tuple/compound arg (no single annotation at
    /// this level) returns `ValueNone`; `prebindModuleFunctionSchemes` then mints
    /// a fresh quantified typar for that slot rather than a declared type.
    let rec private tryArgAnnotation (p: Pat<SyntaxToken>) : Type<SyntaxToken> voption =
        match p with
        | Pat.Typed(typ = t) -> ValueSome t
        | Pat.EnclosedBlock(pat = inner) -> tryArgAnnotation inner
        | Pat.As(pat = inner) -> tryArgAnnotation inner
        | Pat.Attributed(pat = inner) -> tryArgAnnotation inner
        | _ -> ValueNone

    /// Forward-reference pre-pass (G19 residue). `walkElems` types class member
    /// bodies (`fillClassMembers`) *before* it walks module-level `let`s, so a
    /// class member that calls a sibling-module function (`SetTree.add`) sees no
    /// scheme yet — `instantiateBinding` falls back to the function's monomorphic
    /// binding-site TyVar, and a subtype argument (`Comparer<'T>` flowing into the
    /// `comparer: IComparer<'T>` param) pins that shared TyVar to the subtype,
    /// which then clashes with the function's own `IComparer` annotation once its
    /// body is finally typed. Pre-seeding an annotation-derived scheme for each
    /// generalisable module function makes the forward reference instantiate
    /// fresh, so the argument-coercion site (`unifyArg`) inserts the upcast just
    /// as it would for an already-generalised callee. Unannotated arg/return slots
    /// get a fresh quantified typar — no worse than the binding's own pre-body
    /// state. The real scheme replaces this one when `inferBindingGroup` walks the
    /// binding (it first clears any forward scheme so its body still types with
    /// monomorphic self/sibling references — no polymorphic recursion).
    let prebindModuleFunctionSchemes (ctx: PassContext) (bindings: ImmutableArray<Binding<SyntaxToken>>) : unit =
        for b in bindings do
            if shouldGeneralise b && not b.argumentPats.IsEmpty then
                let key = CstKeys.ofPat b.headPat

                if (ctx.Bindings.Scheme.TryGetValue key).IsNone then
                    let savedScope = ctx.Resolution.TyparScope
                    let savedStrict = ctx.Resolution.TyparScopeStrict
                    ctx.Resolution.TyparScope <- Dictionary<string, TypeVar>(System.StringComparer.Ordinal)
                    ctx.Resolution.TyparScopeStrict <- false
                    let outerLevel = ctx.CurrentLevel

                    try
                        // Seed explicit `<'a>` typars first so later implicit `'a`
                        // mentions in the annotations share the same TyVar.
                        seedBindingTypars ctx b

                        enterLevel ctx

                        let argTypes =
                            [
                                for p in b.argumentPats ->
                                    match tryArgAnnotation p with
                                    | ValueSome t -> translateType ctx t
                                    | ValueNone -> TyVar(freshTyVar ctx)
                            ]

                        let retTy =
                            match b.returnType with
                            | ValueSome(ReturnType(typ = t)) -> translateType ctx t
                            | ValueNone -> TyVar(freshTyVar ctx)

                        let fnTy = List.foldBack (fun a r -> TyFun(a, r)) argTypes retTy
                        exitLevel ctx
                        let scheme = generalise (zonk fnTy) outerLevel
                        ctx.Bindings.Scheme.Set(key, scheme)
                    finally
                        ctx.Resolution.TyparScope <- savedScope
                        ctx.Resolution.TyparScopeStrict <- savedStrict
