namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaboratePatterns
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr

// Type-declaration surfacing + the top-level `Elaborate.run` entry point: CST →
// `TastFileG<SemType>`, inline-expanded, open typars quantified to `TyTypar` —
// all still `SemType`. This is NOT the `SemType → FrozenType` freeze (that is the
// `Freeze` module, the final pipeline step); renamed from `Freeze` to
// retire that naming bug. The expression / pattern
// projection lives in ElaborateExpr (opened above).
//
// Invariant: side tables can be discarded after this returns. The TAST is
// sharable; the CST + side tables are scoped to one compilation.

module Elaborate =
    // A declaring-type typar becomes a `TyConst "'A"` marker the backend's
    // typar encoder maps to a generic-parameter index.

    /// The `i`-th curried parameter of an elaborated `let`-body (a nest of
    /// `Lambda`s): its binder `NodeKey` and the lambda's body (the parameter's
    /// scope). `ValueNone` if the body has fewer than `i+1` lambdas, or the
    /// target parameter is not a simple name (a tuple-destructured parameter
    /// can't carry `[<CallAtMostOnce>]`).
    let rec private nthLambdaParam (body: TExpr) (i: int) : (NodeKey * TExpr) voption =
        match body with
        | TExpr.Lambda(p, inner, _, _) ->
            if i = 0 then
                match p with
                | TPat.NamedSimple(k, _, _) -> ValueSome(k, inner)
                | _ -> ValueNone
            else
                nthLambdaParam inner (i - 1)
        | _ -> ValueNone

    /// The binder `NodeKey` `translatePat` mints for an argument pattern — the
    /// innermost `NamedSimple` after peeling the inert wrappers (`[<…>] p`, `(p)`,
    /// `p : t`, `p as x`). `ValueNone` for a non-simple parameter (a tuple &c.).
    /// Used only to assert the positional alignment between an inline's
    /// `argumentPats` and its elaborated curried-lambda nest (`recordInlineParamAttrs`).
    let rec private argPatBinderKey (p: Pat<SyntaxToken>) : NodeKey voption =
        match p with
        | Pat.NamedSimple _ -> ValueSome(CstKeys.ofPat p)
        | Pat.Attributed(pat = inner)
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner)
        | Pat.As(pat = inner) -> argPatBinderKey inner
        | _ -> ValueNone

    /// The `[<CallAtMostOnce>]` linearity contract: `k` is referenced AT MOST
    /// ONCE in `scope`, and (if once) that use is not under a lambda or loop — so
    /// substituting the argument at the use evaluates it at most once. Conditional
    /// branches / match arms are fine (they only *skip* the use, never repeat it),
    /// so they are not special-cased; `While`/`ForTo`/`ForIn` bodies (and a
    /// `While` condition) repeat, so a use there is rejected. `TastWalk.usesOf` is
    /// the shared depth-tracking walk: `[]` (unused) or `[0]` (one straight-line
    /// use) satisfies the contract.
    let private paramUsedAtMostOnce (k: NodeKey) (scope: TExpr) : bool =
        match TastWalk.usesOf k scope with
        | [] -> true
        | [ depth ] -> depth = 0
        | _ -> false

    /// Decode + validate the compiler attributes on an `inline` binding's
    /// parameters, recording them in `ctx.InlineParamAttrs` (keyed by the
    /// function-binder `NodeKey`) for `Passes.InlineExpansion`. Errors a
    /// `[<CallAtMostOnce>]` on a non-`inline` binding, a non-simple parameter, or
    /// one that violates the linearity contract. A no-op when no parameter carries
    /// a recognised attribute.
    let private recordInlineParamAttrs
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (binderKey: NodeKey)
        (valT: TExpr)
        : unit =
        if not b.argumentPats.IsEmpty then
            let attrs = [| for p in b.argumentPats -> Attributes.paramAttrsOfArgPat ctx p |]

            if attrs |> Array.exists (fun a -> not a.IsDefault) then
                if not b.inlineToken.IsSome then
                    ctx.Error(
                        CstKeys.ofBinding b,
                        "A parameter attribute such as [<CallAtMostOnce>] is only valid on a parameter of an 'inline' function"
                    )
                else
                    attrs
                    |> Array.iteri (fun i a ->
                        if a.CallAtMostOnce then
                            // The flag at position `i` (decoded from `argumentPats.[i]`)
                            // must validate against — and later be honoured at — the
                            // `i`-th curried lambda. The inliner's `peel` re-derives the
                            // same `i` from the lambda nest, so this attrs array and the
                            // nest must stay positionally aligned; assert the binder keys
                            // agree so a future reordering of either fails loudly here
                            // rather than silently mis-marking a parameter as lazy.
                            match nthLambdaParam valT i with
                            | ValueSome(pk, _) when ValueSome pk <> argPatBinderKey b.argumentPats.[i] ->
                                failwithf
                                    "Elaborate.recordInlineParamAttrs: parameter %d binder key %A does not match its argument pattern (alignment invariant broken)"
                                    i
                                    pk
                            | ValueSome(pk, scope) when paramUsedAtMostOnce pk scope -> ()
                            | ValueSome _ ->
                                ctx.Error(
                                    CstKeys.ofPat b.argumentPats.[i],
                                    "A [<CallAtMostOnce>] parameter must be used at most once in the body, and not under a lambda or loop"
                                )
                            | ValueNone ->
                                ctx.Error(
                                    CstKeys.ofPat b.argumentPats.[i],
                                    "[<CallAtMostOnce>] is not supported on this parameter shape (it must be a single named parameter)"
                                )
                    )

                    ctx.InlineParamAttrs.[binderKey] <- attrs

    let private typeNameSimple (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.IsEmpty then
            ""
        else
            ctx.NameOf li.Idents.[li.Idents.Length - 1]

    /// The decl-site `NodeKey` for a single-segment `TypeName` — the same key
    /// `NameResolution` mints (`NodeKey.ofToken <first ident> DeclType`) and stamps
    /// into `Resolution.ResolvedType`. `ValueNone`
    /// for a multi-segment name, which is never a project-local type and so never
    /// registered. Used to recover an arity-overloaded union (`Choice\`2`…`Choice\`7`)
    /// by its stamped `SymbolKey` instead of re-deriving the `(name, arity)` key.
    let private typeNameDeclKey (ctx: PassContext) (tn: TypeName<SyntaxToken>) : NodeKey voption =
        let (TypeName(ident = li)) = tn

        if li.Idents.Length = 1 then
            ValueSome(NodeKey.ofToken li.Idents.[0] NodeKind.DeclType)
        else
            ValueNone

    /// Rewrite open typars (free `TyVar`s, by zonked root) to their frozen
    /// `TyTypar` nodes: `env` pairs each typar's zonked root
    /// with its target `TyTypar(axis, index)`. Anything else passes through
    /// unchanged — a leftover inference `TyVar` not in `env` stays a `TyVar`, which
    /// the backend rejects loudly (an unresolved-typar bug).
    let private remapDeclTypars (env: (TypeVar * SemType) list) (t: SemType) : SemType =
        let rec go t =
            match t with
            | TyVar tv ->
                match
                    env
                    |> List.tryPick (fun (r, target) -> if Object.ReferenceEquals(r, tv) then Some target else None)
                with
                | Some target -> target
                | None -> t
            | t -> SemType.mapChildren go t

        go (Unification.zonk t)

    /// Rewrite every `SemType` embedded in a member body via `f`. Used to push a
    /// generic union's declaring-typar remap (`remapDeclTypars`) through the whole
    /// member body, so a typar-typed local / scrutinee / bound variable carries the
    /// `TyConst "'T"` marker the backend's generic-member encoder consumes — just as
    /// the case-field types do (generalised to member bodies).
    let private mapExprTypes (f: SemType -> SemType) (e: TExpr) : TExpr =
        TastWalk.mapExpr
            { TastWalk.identityMapper with
                MapType = f
            }
            e

    /// Pair each declared typar's *zonked* root TyVar with the frozen `TyTypar`
    /// it remaps to: `axis` selects declaring (`!i`) vs method (`!!i`), and the
    /// index is the typar's position in its declaration list — the same index the
    /// backend's `GenericParam` rows use. Pinned typars (collapsed to a non-`TyVar`)
    /// are dropped (nothing to remap), but the loop index still tracks declaration
    /// position so a surviving typar keeps its correct slot. Shared by every
    /// `try*Type` surfacer and the interface / abstract-method projections.
    let private mkTyparEnv (axis: TyparAxis) (typeParams: EqArray<string * TypeVar>) : (TypeVar * SemType) list =
        [
            for i in 0 .. typeParams.Length - 1 do
                let (_, ptv) = typeParams.[i]

                match Unification.zonk (TyVar ptv) with
                | TyVar root -> yield (root, TyTypar(axis, i))
                | _ -> ()
        ]

    let private mkDeclTyparEnv (typeParams: EqArray<string * TypeVar>) =
        mkTyparEnv TyparAxis.Declaring typeParams

    let private mkMethodTyparEnv (typeParams: EqArray<string * TypeVar>) = mkTyparEnv TyparAxis.Method typeParams

    /// Quantify a module-`let`'s free type parameters into `TyTypar(Method, i)` in
    /// CANONICAL order via the one shared `GeneralizedTypars.canonical` — the F#
    /// rule: explicitly-declared `<'b,'a>` typars first in source order (`declared`,
    /// threaded from the `TDecl.Let` site), then the remaining inferred roots by
    /// first-left-to-right appearance (params left-to-right, then return). A *linked*
    /// root (pinned to a concrete type, or a measure carrier whose `Link` points at
    /// its carrier) is followed, not collected, so measures and pinned vars stay out
    /// of the typar list. After the canonical order, the dependent-typar fixpoint
    /// (constraint-only `Coercion` targets, absent from the type) is preserved and
    /// appended, mirroring `InferGeneralize.generalise`. The resulting env feeds
    /// `remapDeclTypars`, exactly like the declaring-typar env in 2A. Caller
    /// restricts this to function bindings (a non-function value's free var is a
    /// value-restriction case, not a method typar).
    let private mkMethodQuantEnv (declared: (string * TypeVar) list) (declTy: SemType) : (TypeVar * SemType) list =
        // The canonical F# order — declared typars first in source order, then the
        // remaining free roots by first-left-to-right-appearance — is computed by the
        // ONE shared `GeneralizedTypars.canonical`. Free functions have no enclosing
        // class typars, so `fixedRoots` is empty. A declared typar that inference
        // pinned to a concrete type (its root is `Link`ed) is NOT a real method typar;
        // drop it so this stays identical to the old appearance-only walk for the
        // no-typar / pinned-declared cases (only the genuinely-reordered case changes).
        let declaredFree =
            declared |> List.filter (fun (_, tv) -> (UnionFind.find tv).Link.IsNone)

        let zonked = Unification.zonk declTy

        // Free-fn inferred typars have no source names, so an empty `knownNames`
        // preserves today's all-`M%d` synthesis for the appearance tail.
        let knownNames =
            System.Collections.Generic.Dictionary<TypeVar, string>(HashIdentity.Reference)
            :> System.Collections.Generic.IReadOnlyDictionary<_, _>

        let gt =
            GeneralizedTypars.canonical
                declaredFree
                (System.Collections.Generic.HashSet<TypeVar>(HashIdentity.Reference))
                knownNames
                zonked

        // The canonical roots, in ABI order, become the seed of the dependent-typar
        // worklist below.
        let acc = ResizeArray<TypeVar>(GeneralizedTypars.toArray gt |> Array.map snd)
        let seen = System.Collections.Generic.HashSet<TypeVar>(HashIdentity.Reference)

        for r in acc do
            seen.Add r |> ignore

        // Dependent typars (mirrors `InferGeneralize.generalise`): a collected typar's
        // `Coercion` bound may name further typars absent from the declared (curried)
        // type — `let f (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator>)`
        // has `'E` in no parameter/return position. F# generalises these phantom
        // parameters too, so they are genuine method typars; fold each collected
        // typar's `Coercion` targets in to a fixpoint (a bound may itself reference a
        // typar with bounds), `ResizeArray` growth driving the worklist. Without this a
        // constrained `for … in` over `'S` leaks `'E` as `?free-typar` at the freeze cut.
        let mutable depIdx = 0

        while depIdx < acc.Count do
            for c in acc.[depIdx].Constraints do
                match c.Kind with
                | SemanticConstraintKind.Coercion target ->
                    // Append the first-appearance roots of the coercion-bound target
                    // (link-following, deduped against the seed). The shared collector.
                    SemTypeWalk.collectLinkedRoots acc seen (Unification.zonk target)
                | _ -> ()

            depIdx <- depIdx + 1

        [ for i in 0 .. acc.Count - 1 -> acc.[i], TyTypar(TyparAxis.Method, i) ]

    /// Did the generaliser quantify this binding into a (non-empty) scheme? A
    /// *value* binding's free typars are only genuine method typars when the
    /// generaliser actually quantified them — i.e. an annotated generic value
    /// like `let empty: SetTree<'T> = null` (non-expansive, so generalised) — as
    /// opposed to a bare value-restricted `let n = null` (no scheme: its free var
    /// stays a metavar). Mirrors `inferBindingGroup`'s `shouldGeneralise` cut via
    /// the scheme it left in `ctx.Bindings.Scheme`.
    let private bindingWasGeneralised (ctx: PassContext) (b: Binding<SyntaxToken>) : bool =
        match ctx.Bindings.Scheme.TryGetValue(CstKeys.ofBinding b) with
        | ValueSome scheme -> not (List.isEmpty scheme.Quantified)
        | ValueNone -> false

    /// For a generalised binding, record its frozen typar
    /// BOUNDS, keyed by the binding's `NodeKey`, onto `ctx.GenericFnSchemes`. Each
    /// `Coercion` bound is frozen as a `FrozenConstraint.Coercion(idx, target)`
    /// template over the METHOD typars: the target is remapped through the SAME
    /// `quantEnv` the body freezes with (so its typar leaves get the identical
    /// `FTTypar(Method, idx)` indices) and then `toFrozen`-converted. The constrained
    /// typar's `idx` is its position in `quantEnv`. A constraint whose typar or
    /// target is not (yet) a `quantEnv` method typar is dropped — only method-axis
    /// bounds are carried. Read by the codegen call-site phantom-typar solve.
    /// (A binding with no recorded scheme has no bounds and records nothing — its
    /// absence from the table is equivalent to an empty list; the emitted typar
    /// arity comes independently from `staticFnTypars`' body sweep.)
    let private recordGenericFnScheme
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (quantEnv: (TypeVar * SemType) list)
        : unit =
        if not (List.isEmpty quantEnv) then
            match ctx.Bindings.Scheme.TryGetValue(CstKeys.ofBinding b) with
            | ValueNone -> ()
            | ValueSome scheme ->
                // Index of `tv`'s zonked root in `quantEnv` (its `TyTypar(Method, i)`).
                let methodIndexOf (tv: TypeVar) : int option =
                    match Unification.zonk (TyVar tv) with
                    | TyVar root ->
                        quantEnv
                        |> List.tryPick (fun (r, target) ->
                            match target with
                            | TyTypar(TyparAxis.Method, i) when Object.ReferenceEquals(r, root) -> Some i
                            | _ -> None
                        )
                    | _ -> None

                let constraints =
                    [
                        for (tv, sc) in scheme.Constraints do
                            match sc.Kind with
                            | SemanticConstraintKind.Coercion target ->
                                match methodIndexOf tv with
                                | Some idx ->
                                    // Freeze the target with the SAME typar env the body
                                    // uses, so its leaves carry matching method indices.
                                    let frozenTarget = toFrozen (remapDeclTypars quantEnv target)
                                    FrozenConstraint.Coercion(idx, frozenTarget)
                                | None -> ()
                            | _ -> ()
                    ]

                ctx.GenericFnSchemes.Set(CstKeys.ofBinding b, constraints)

    /// The declaring-type typars as `SemType` args, for a member's `ThisTy` and
    /// the body's synthesised `this` self-type: each declared typar zonked to its
    /// root `TyVar`. `elaborate` keeps these in `TyVar` form (not `TyTypar`) so
    /// the whole tree stays metavar-shaped until the `freezeTypars` cut, which
    /// remaps each root to `TyTypar(Declaring, i)`. The
    /// index `i` is the typar's declaration position — the same index
    /// `mkDeclTyparEnv` pairs the root with — so the round-trip is faithful.
    let private declTyparArgs (typeParams: EqArray<string * TypeVar>) : EqArray<SemType> =
        EqArray.ofSeq (seq { for (_, ptv) in typeParams -> Unification.zonk (TyVar ptv) })

    /// Elaborate one type member: stamp its `ThisTy` with the `TyVar`-rooted
    /// `selfTy` and surface its *method-axis* typar roots so the caller folds them
    /// into the decl's freeze env. The signature / body / return types stay
    /// verbatim — the `TyVar → TyTypar` cut is deferred to `freezeTypars`. Shared
    /// by the union / class member surfacers (they differ only in `selfTy`'s
    /// `TyUnion` vs `TyClass` head). `MethodTypeParams` is untouched (its roots feed
    /// the `GenericParam` rows and the header arity).
    let private elaborateMember (selfTy: SemType) (m: TTypeMember) : TTypeMember * (TypeVar * SemType) list =
        let methodMarkers =
            if GeneralizedTypars.count m.MethodTypeParams = 0 then
                []
            else
                GeneralizedTypars.methodEnv m.MethodTypeParams

        { m with ThisTy = selfTy }, methodMarkers

    /// The per-member elaborator every host surfacer (union / record / class /
    /// intrinsic-abbrev) folds over its members — they differ only in `selfTy`'s
    /// head. A monomorphic host (`declTypars` empty) keeps
    /// `translateNominalMember`'s self-type byte-identical; a generic host remaps
    /// each member's self-type to declaring-typar roots via `elaborateMember` and
    /// accumulates the surfaced method markers into the decl's freeze `env`.
    let private mkMemberElaborator
        (selfTy: SemType)
        (declTypars: string list)
        (env: ResizeArray<TypeVar * SemType>)
        : TTypeMember -> TTypeMember =
        fun m ->
            if List.isEmpty declTypars then
                m
            else
                let m, methodMarkers = elaborateMember selfTy m
                env.AddRange methodMarkers
                m

    /// freezeTypars (member): apply the typar cut `f` (= `remapDeclTypars env`) to
    /// every `SemType` embedded in a member — the deferred half of the old
    /// `remapMemberTypes`. `MethodTypeParams` (whose `TypeVar` roots feed the
    /// `GenericParam` rows) is left untouched.
    let private freezeMember (f: SemType -> SemType) (m: TTypeMember) : TTypeMember =
        { m with
            ThisTy = f m.ThisTy
            Params = m.Params |> EqArray.map (fun (k, ty) -> k, f ty)
            Body = mapExprTypes f m.Body
            ReturnTy = f m.ReturnTy
        }

    /// freezeTypars (type kind): push `f` through every `SemType` a type
    /// declaration's body carries — case / record fields, ctor params, member
    /// bodies, base type, interface impls, static / secondary ctors.
    let private freezeKind (f: SemType -> SemType) (k: TTypeKind) : TTypeKind =
        let field (fld: TRecordField) = { fld with Type = f fld.Type }

        match k with
        | TTypeKind.Interface methods ->
            TTypeKind.Interface(methods |> EqArray.map (fun am -> { am with Signature = f am.Signature }))
        | TTypeKind.Union(cases, members, interfaces) ->
            let cases =
                cases
                |> EqArray.map (fun c ->
                    { c with
                        Fields = c.Fields |> EqArray.map (fun (n, ty) -> n, f ty)
                    }
                )

            TTypeKind.Union(
                cases,
                members |> EqArray.map (freezeMember f),
                interfaces
                |> EqArray.map (fun (ity, ms) -> f ity, ms |> EqArray.map (freezeMember f))
            )
        | TTypeKind.Record(fields, members, interfaces) ->
            TTypeKind.Record(
                fields |> EqArray.map field,
                members |> EqArray.map (freezeMember f),
                interfaces
                |> EqArray.map (fun (ity, ms) -> f ity, ms |> EqArray.map (freezeMember f))
            )
        // Enum cases carry no `SemType` (the value is a resolved literal, not a
        // typed term), so the typar remap is a no-op.
        | TTypeKind.Enum cases -> TTypeKind.Enum cases
        | TTypeKind.Class c ->
            let preambleEntry (entry: TPreambleEntry) =
                match entry with
                | TPreambleEntry.Let l ->
                    TPreambleEntry.Let
                        { l with
                            Type = f l.Type
                            Init = mapExprTypes f l.Init
                        }
                | TPreambleEntry.Do e -> TPreambleEntry.Do(mapExprTypes f e)

            let ctorLet (cl: TCtorLet) =
                { cl with
                    Type = f cl.Type
                    Init = mapExprTypes f cl.Init
                }

            let secondary (sc: TSecondaryCtor) =
                { sc with
                    Params = sc.Params |> EqArray.map (fun (k, ty) -> k, f ty)
                    Lets = sc.Lets |> EqArray.map ctorLet
                    PrimaryArgs = sc.PrimaryArgs |> EqArray.map (mapExprTypes f)
                    FieldInits =
                        sc.FieldInits
                        |> EqArray.map (fun fi ->
                            { fi with
                                Init = mapExprTypes f fi.Init
                            }
                        )
                }

            let baseCtor (bc: TBaseCtorCall) =
                { bc with
                    CtorParams = bc.CtorParams |> EqArray.map (fun (k, ty) -> k, f ty)
                    Args = bc.Args |> EqArray.map (mapExprTypes f)
                }

            TTypeKind.Class
                {
                    Fields = c.Fields |> EqArray.map field
                    CtorParams = c.CtorParams |> EqArray.map field
                    Members = c.Members |> EqArray.map (freezeMember f)
                    BaseType = c.BaseType |> ValueOption.map f
                    Interfaces =
                        c.Interfaces
                        |> EqArray.map (fun (ity, ms) -> f ity, ms |> EqArray.map (freezeMember f))
                    IsSealed = c.IsSealed
                    StaticPreamble = c.StaticPreamble |> EqArray.map preambleEntry
                    InstancePreamble = c.InstancePreamble |> EqArray.map preambleEntry
                    ThisKey = c.ThisKey
                    SecondaryCtors = c.SecondaryCtors |> EqArray.map secondary
                    BaseCtorCall = c.BaseCtorCall |> ValueOption.map baseCtor
                    ValueKind = c.ValueKind
                    HasPrimaryCtor = c.HasPrimaryCtor
                }

    /// The deferred typar cut. Walk every `SemType` in a
    /// decl through `remapDeclTypars env`, rewriting the decl's open `TyVar` typars
    /// to their `TyTypar(axis, index)` nodes. `env` is the decl's own quantified
    /// typar roots, collected by `elaborate` (the single index-minting point).
    /// `remapDeclTypars` zonks as it recurses, so an empty `env` is a pure
    /// zonk-rebuild — exactly the old monomorphic `remapDeclTypars []` path every
    /// surfacer applied inline.
    let private freezeTypars (env: (TypeVar * SemType) list) (d: TDecl) : TDecl =
        let f = remapDeclTypars env

        match d with
        | TDecl.Let(binding, value, isInline, ty) ->
            let binding =
                TastWalk.mapPat
                    { TastWalk.identityMapper with
                        MapType = f
                    }
                    binding

            TDecl.Let(binding, mapExprTypes f value, isInline, f ty)
        | TDecl.Expression(e, ty) -> TDecl.Expression(mapExprTypes f e, f ty)
        | TDecl.Type td -> TDecl.Type { td with Kind = freezeKind f td.Kind }

    /// Classify an object-model body as an interface — every element an abstract
    /// method signature, no base type, no `let`/`do` preamble — and build its
    /// methods from the *resolved* member signatures in `ctx.Types.Class` (an
    /// `Anon`/`Interface` registers as a class). None for a concrete
    /// member/field/inherit (a class or other later construct) or a never-registered type.
    let private tryInterfaceMethods
        (ctx: PassContext)
        (name: string)
        (arity: int)
        (body: ObjectModelBody<SyntaxToken>)
        : (EqArray<string> * EqArray<TAbstractMethod> * (TypeVar * SemType) list) option =
        let allAbstractMethods =
            not body.elements.IsEmpty
            && body.elements
               |> Seq.forall (fun el ->
                   match el with
                   | TypeDefnElement.Member(MemberDefn.Member(
                       defn = MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig _))) -> true
                   | _ -> false
               )

        if body.inherits.IsSome || not body.classPreamble.IsEmpty || not allAbstractMethods then
            None
        else
            // The key of the type being LOWERED, minted from the module the walk is in —
            // not a by-name read. This is the declaration itself, so there is nothing to
            // resolve: a sibling module's same-named interface is a different type, and an
            // arity-overloaded `Fun\`2`/`Fun\`3` does not resolve by bare name at all.
            match TypeRegistry.tryClassByKey ctx.Types (ctx.DeclaredTypeKey(name, arity)) with
            | ValueNone -> None
            | ValueSome info ->
                // The member signatures share these prototype TyVars (Unification
                // typed them under the class's typar scope), so the remap reaches
                // every typar.
                let markers = mkDeclTyparEnv info.TypeParams
                // Accumulate the decl's freeze env: the declaring typars plus every
                // generic method's own typars. `freezeTypars` later applies this to
                // each `Signature` (left verbatim here) — the deferred typar cut.
                let env = ResizeArray markers

                let methods =
                    EqArray.ofSeq (
                        seq {
                            // An interface body is all-abstract; both abstract
                            // methods and abstract *properties* become slots. A
                            // property (`abstract member Current : int`) emits as a
                            // `get_<Name>` getter so a property impl binds to it.
                            for m in info.Members do
                                // A generic method's own typars join the env so
                                // the backend routes them to `GenericMethodParameter`
                                // (declaring typars stay `GenericTypeParameter`).
                                if GeneralizedTypars.count m.Generalized > 0 then
                                    env.AddRange(GeneralizedTypars.methodEnv m.Generalized)

                                yield
                                    {
                                        Name = m.Name
                                        MethodTypeParams = EqArray.ofArray (GeneralizedTypars.names m.Generalized)
                                        Signature = m.Type
                                        IsProperty = (m.Kind = ClassMemberKind.Property)
                                    }
                        }
                    )

                Some(EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }), methods, List.ofSeq env)

    /// Member name from a member binding's `headPat` (`member this.M …` parses
    /// the member name as the head pattern's ident).
    let private memberNameOfBinding (ctx: PassContext) (b: Binding<SyntaxToken>) : string voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple id -> ValueSome(ctx.NameOf id)
            // Operator-named binding head: surface the operator's compiled name
            // (`(=)` → `op_Equality`) so the member is addressable from a use
            // site's desugared `External(op_Equality)` head.
            | Pat.Op io -> Desugar.opPatCompiledName ctx.NameOf io
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    /// The member's declaration `NodeKey` — `CstKeys.ofPat` of the same name-head
    /// pattern `MemberRegistration.memberNameOf` keys the `TypeMemberInfo.DeclKey`
    /// from. Unique per declared member (it carries the member's source offset), so
    /// it disambiguates *same-name overloads* that share a name + kind + static-ness
    /// — which a name-only `Array.tryFind` cannot. Used to recover the *right*
    /// overload's `MethodTypeParams` (without it every `Fmt` overload took
    /// the first one's typars, so the others' own `'T` was never generalised and
    /// froze ungrounded).
    let private memberKeyOfBinding (b: Binding<SyntaxToken>) : NodeKey voption =
        let rec walk (p: Pat<SyntaxToken>) =
            match p with
            | Pat.NamedSimple _
            | Pat.Op _ -> ValueSome(CstKeys.ofPat p)
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.Attributed(pat = inner) -> walk inner
            | _ -> ValueNone

        walk b.headPat

    /// Member parameter list as `(bindingKey, ty)` pairs in declaration order
    /// (`this` is separate). The binding key is the same one `translatePat` mints,
    /// so a `Var` reference in the body resolves to it.
    ///
    /// A tupled member (`M(a, b)`) is *one* `argumentPats` entry that translates to
    /// a `TPat.Tuple`; F# compiles it to a .NET method with one parameter per tuple
    /// component (not an actual `Tuple<_,_>`), so we flatten the tuple to one
    /// `(key, ty)` per component. The sequential order lines up with both
    /// `Emit.buildMember`'s `args.[k] <- baseIdx + i` slots and the emitted method
    /// signature. Curried members (`M a b`) appear as multiple `argumentPats`
    /// entries and compose with the flatten. Non-simple components (wildcards,
    /// nested destructuring) bind nothing and are dropped.
    let private memberParams (ctx: PassContext) (b: Binding<SyntaxToken>) : EqArray<NodeKey * SemType> =
        let rec flatten (tp: TPat) =
            seq {
                match tp with
                | TPat.NamedSimple(k, ty, _) -> yield (k, ty)
                | TPat.Tuple(items, _, _) ->
                    for it in items do
                        yield! flatten it
                | _ -> ()
            }

        EqArray.ofSeq (
            seq {
                for p in b.argumentPats do
                    yield! flatten (translatePat ctx p)
            }
        )

    /// `override`/`default` ⇒ the member overrides a base virtual slot (Object's
    /// `Equals`/`GetHashCode`/`ToString` for an `inherit`-less class); `member`/
    /// `abstract` do not. Drives virtual emission + the skip-generalise / Object-slot
    /// conformance passes via `TTypeMember.IsOverride`.
    let private isOverrideKeyword (kw: MemberKeyword<SyntaxToken>) : bool =
        match kw with
        | MemberKeyword.Override _
        | MemberKeyword.Default _ -> true
        | MemberKeyword.Member _
        | MemberKeyword.Abstract _ -> false

    /// Translate one union/record augmentation member element into a `TTypeMember`.
    /// Instance members reference `this` via `host.ThisKey`; `ThisTy` is the host's
    /// own monomorphic Self (`TyUnion`/`TyRecord` via `MkSelfType`), remapped to
    /// declaring typars later by the caller's `elaborateOne`. Neither unions nor
    /// records carry primary-ctor params, so (unlike `translateClassMember`) no
    /// ctor-param → `FieldGet` rewrite is needed — a field reference is already an
    /// explicit `this.N`. Generic methods on such augmentations are out of scope
    /// (class-only), so `MethodTypeParams` is always empty here.
    let private translateNominalMember
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        // Unions/records are not inheritable — `base` never in scope.
        let selfTy = host.MkSelfType EqArray.empty

        match el with
        | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; keyword = kw; defn = d)) ->
            let isStatic = s.IsSome
            let isOverride = isOverrideKeyword kw

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Kind = kind
                            IsOverride = isOverride
                            ThisKey = (if isStatic then ValueNone else ValueSome host.ThisKey)
                            BaseKey = ValueNone
                            ThisTy = selfTy
                            Params = memberParams ctx b
                            Body = translateExpr ctx b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                            // Generic methods on union augmentations are out of
                            // B-12 scope (class-only); always non-generic here.
                            MethodTypeParams = GeneralizedTypars.empty
                        }
                | ValueNone -> ValueNone

            match d with
            | MethodOrPropDefn.Method(defn = b) -> build TMemberKind.Method b
            | MethodOrPropDefn.Property(defn = b) -> build TMemberKind.Property b
            | MethodOrPropDefn.AutoProperty(ident = id; expr = e) ->
                ValueSome
                    {
                        Name = ctx.NameOf id
                        IsStatic = isStatic
                        Kind = TMemberKind.Property
                        IsOverride = isOverride
                        ThisKey = (if isStatic then ValueNone else ValueSome host.ThisKey)
                        BaseKey = ValueNone
                        ThisTy = selfTy
                        Params = EqArray.empty
                        Body = translateExpr ctx e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        MethodTypeParams = GeneralizedTypars.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Surface a union/record host's augmentation members and resolved `interface …
    /// with` impl bodies as the `(members, interfaces)` pair carried by `TTypeKind`.
    /// Each member/impl-body is translated through `translateNominalMember` then run
    /// through `elaborateOne` (the caller's generic self-type remapper). Impls whose
    /// interface failed to resolve are dropped (that diagnostic already fired).
    let private elaborateHostMembers
        (ctx: PassContext)
        (host: IInterfaceImplHost)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        (elaborateOne: TTypeMember -> TTypeMember)
        : EqArray<TTypeMember> * EqArray<SemType * EqArray<TTypeMember>> =
        let translate (els: TypeDefnElements<SyntaxToken>) : EqArray<TTypeMember> =
            EqArray.ofSeq (
                seq {
                    for el in els do
                        match translateNominalMember ctx host el with
                        | ValueSome m -> yield elaborateOne m
                        | ValueNone -> ()
                }
            )

        let members =
            match ext with
            | ValueNone -> EqArray.empty
            | ValueSome(TypeExtensionElements(elements = elems)) -> translate elems

        let interfaces =
            EqArray.ofSeq (
                seq {
                    for impl in host.InterfaceImpls do
                        match impl.Resolved with
                        | ValueSome ifaceTy -> yield (ifaceTy, translate impl.Elements)
                        | ValueNone -> ()
                }
            )

        members, interfaces

    /// The class binders that are not locals but FIELDS: a primary-ctor param and an
    /// instance-`let` binder (instance fields), a `static let` binder (a static field).
    /// Every reference to one — in a member body, in a `.cctor` initialiser, in a later
    /// preamble entry — must be rewritten to a field access, so codegen never sees the
    /// binder's `NodeKey`. `MkSet` rewrites the WRITE side with the read side: a
    /// `let mutable` binder IS the field, so a `c <- c + 1` must store to it (a `TExpr.Let`
    /// binder would instead be promoted to a ref cell and fork the storage). `MkSet` is
    /// `ValueNone` where the TAST has no store node for the target: there is no
    /// `TExpr.StaticFieldSet`, which is exactly why `static let mutable` is rejected at
    /// registration — nothing writable can reach the static rewrite.
    [<NoEquality; NoComparison>]
    type private FieldRewrite =
        {
            /// Field name, by binder `NodeKey`.
            Names: Map<NodeKey, string>
            MkGet: string -> SemType -> SyntaxToken -> TExpr
            MkSet: (string -> TExpr -> SemType -> SyntaxToken -> TExpr) voption
        }

    let private rewriteFieldRefs (r: FieldRewrite) (body: TExpr) : TExpr =
        if Map.isEmpty r.Names then
            body
        else
            TastWalk.mapExpr
                { TastWalk.identityMapper with
                    OverrideExpr =
                        fun m e ->
                            match e with
                            | TExpr.Assignment(TExpr.Var(k, _, _), rhs, ty, tok) ->
                                match r.MkSet, Map.tryFind k r.Names with
                                | ValueSome mkSet, Some name -> ValueSome(mkSet name (TastWalk.mapExpr m rhs) ty tok)
                                | _ -> ValueNone
                            | TExpr.Var(k, ty, tok) ->
                                match Map.tryFind k r.Names with
                                | Some name -> ValueSome(r.MkGet name ty tok)
                                | None -> ValueNone
                            | _ -> ValueNone
                }
                body

    /// `static let` binders → `TExpr.StaticFieldGet` on the declaring class.
    let private staticFieldRewrite (info: ClassTypeInfo) : FieldRewrite =
        {
            Names =
                ClassPreamble.lets info.StaticPreamble
                |> Array.map (fun l -> l.DeclKey, l.Name)
                |> Map.ofArray
            MkGet = fun name ty tok -> TExpr.StaticFieldGet(info.Key, name, ty, tok)
            MkSet = ValueNone
        }

    /// Primary-ctor params AND instance-`let` binders → `TExpr.FieldGet`/`FieldSet` on
    /// `this`. ONE map, because they are one kind of thing: an instance `let` is a ctor
    /// param whose value comes from an initialiser rather than an argument. The map is
    /// keyed by `NodeKey`, so it stays exact even though the two families share a name
    /// space — which `NameResolution` separately requires to be collision-free, since a
    /// field is emitted under its SOURCE name.
    let private instanceFieldRewrite (info: ClassTypeInfo) (classTy: SemType) : FieldRewrite =
        let names =
            (Map.empty, info.CtorParams)
            ||> Array.fold (fun acc p -> Map.add p.DeclKey p.Name acc)

        let names =
            (names, ClassPreamble.lets info.InstancePreamble)
            ||> Array.fold (fun acc l -> Map.add l.DeclKey l.Name acc)

        {
            Names = names
            MkGet = fun name ty tok -> TExpr.FieldGet(TExpr.Var(info.ThisKey, classTy, tok), name, ty, tok)
            MkSet =
                ValueSome(fun name rhs ty tok ->
                    TExpr.FieldSet(TExpr.Var(info.ThisKey, classTy, tok), name, rhs, ty, tok)
                )
        }

    /// Translate one class member element into a `TTypeMember`. Parallel to
    /// `translateUnionMember` — only differs in the `ThisTy` shape
    /// (`TyClass(info.Name, …)` vs `TyUnion`) and in the field rewrites: a
    /// reference to a ctor param or an instance-`let` binder in an *instance* body becomes
    /// a `FieldGet`/`FieldSet` on `this`, and one to a `static let` binder becomes a
    /// `StaticFieldGet` — so the back end resolves them through the same field mechanism
    /// every other nominal type uses (codegen never sees the binder's NodeKey). Static
    /// members see neither `this` nor the instance binders (front-end's `staticScope`), so
    /// only the static rewrite applies there.
    let private translateClassMember
        (ctx: PassContext)
        (info: ClassTypeInfo)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        // The instantiated self-type the synthesised `this` Var carries. Empty
        // typar list for a monomorphic class; the declaring typars ride as
        // `TyVar` roots (not `TyTypar`), which `freezeTypars` cuts over the
        // whole member body.
        let classTy = TyClass(info.Key, declTyparArgs info.TypeParams)

        // `base` is in scope only when the class has an `inherit` clause; an
        // instance member then carries the shared `BaseKey` so codegen maps a
        // `base.M(...)` receiver to `ldarg.0` (CallVia.Base drives the
        // non-virtual opcode — see `viaOfReceiver`).
        let baseKey =
            if info.BaseType.IsSome then
                ValueSome info.BaseKey
            else
                ValueNone

        let staticRewrite = staticFieldRewrite info
        let instanceRewrite = instanceFieldRewrite info classTy

        match el with
        | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; keyword = kw; defn = d)) ->
            let isStatic = s.IsSome
            let isOverride = isOverrideKeyword kw

            let lowerBody (e: Expr<SyntaxToken>) : TExpr =
                let body = translateExpr ctx e |> rewriteFieldRefs staticRewrite

                if isStatic then
                    body
                else
                    rewriteFieldRefs instanceRewrite body

            // The member's own generic parameters (B-12), recovered from the
            // registered `TypeMemberInfo`'s canonical `Generalized` order. The order
            // flows through UNCHANGED (the carrier mints no new order); each entry's
            // root is refreshed to its current union-find / link representative and
            // any that pinned to a concrete type since generalise is DROPPED — both
            // ORDER-PRESERVING, so the ABI index is untouched. Codegen installs these
            // roots as ambient method typars so they encode to `!!i`.
            let methodTypeParams (n: string) (kind: TMemberKind) (declKey: NodeKey voption) : GeneralizedTypars =
                let kindMatches (mi: TypeMemberInfo) =
                    match mi.Kind, kind with
                    | ClassMemberKind.Method, TMemberKind.Method
                    | ClassMemberKind.Property, TMemberKind.Property -> true
                    | _ -> false

                // Match the *exact* overload by its registration `DeclKey` first —
                // same-name overloads share `Name`/`Kind`/`IsStatic`, so a name-only
                // `tryFind` would return the first overload's typars for every one,
                // dropping the others' own `'T`. Fall back to the name match
                // for any member whose binding key didn't resolve (operator heads,
                // auto-properties — none of which overload generically).
                let byKey =
                    match declKey with
                    | ValueSome k -> info.Members |> Array.tryFind (fun mi -> mi.DeclKey = k)
                    | ValueNone -> None

                match
                    byKey
                    |> Option.orElseWith (fun () ->
                        info.Members
                        |> Array.tryFind (fun mi -> mi.Name = n && mi.IsStatic = isStatic && kindMatches mi)
                    )
                with
                | Some mi ->
                    // Refresh each canonical root to its CURRENT union-find / link
                    // representative and DROP any that pinned to a concrete type since
                    // generalise — ORDER-PRESERVING, so the ABI index is untouched.
                    // Mirrors the pre-split helper's per-entry `zonk`+drop: a root
                    // unioned away keys the body's frozen `TyTypar(Method, i)` markers on
                    // its survivor, and a root linked to a concrete type is no longer a
                    // real typar (keeping it would inflate the GenericParam arity).
                    mi.Generalized
                    |> GeneralizedTypars.refreshRoots (fun tv ->
                        match Unification.zonk (TyVar tv) with
                        | TyVar r -> ValueSome r
                        | _ -> ValueNone
                    )
                | None -> GeneralizedTypars.empty

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Kind = kind
                            IsOverride = isOverride
                            ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                            BaseKey = (if isStatic then ValueNone else baseKey)
                            ThisTy = TyClass(info.Key, EqArray.empty)
                            Params = memberParams ctx b
                            Body = lowerBody b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                            MethodTypeParams = methodTypeParams n kind (memberKeyOfBinding b)
                        }
                | ValueNone -> ValueNone

            match d with
            | MethodOrPropDefn.Method(defn = b) -> build TMemberKind.Method b
            | MethodOrPropDefn.Property(defn = b) -> build TMemberKind.Property b
            | MethodOrPropDefn.AutoProperty(ident = id; expr = e) ->
                ValueSome
                    {
                        Name = ctx.NameOf id
                        IsStatic = isStatic
                        Kind = TMemberKind.Property
                        IsOverride = isOverride
                        ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                        BaseKey = (if isStatic then ValueNone else baseKey)
                        ThisTy = TyClass(info.Key, EqArray.empty)
                        Params = EqArray.empty
                        Body = lowerBody e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        // Auto-properties never carry their own generic params.
                        MethodTypeParams = GeneralizedTypars.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Translate one secondary constructor into a `TSecondaryCtor`. The
    /// params / preamble / chain-call args are translated verbatim; each
    /// `let`-preamble binding becomes a `TCtorLet`, the final chain call's
    /// arguments become `PrimaryArgs`. A generic class's declaring typars ride as
    /// `TyVar` roots and are cut over the whole decl by `freezeTypars` (the
    /// declaring env `tryClassType` collects), so no per-ctor remap is needed here.
    /// v1 supports a `let` preamble followed by the chain call; sequencing /
    /// conditional preambles recurse to the chain and drop intervening statements.
    let private translateSecondaryCtor (ctx: PassContext) (sc: ClassSecondaryCtorInfo) : TSecondaryCtor =
        let parms =
            EqArray.ofSeq (seq { for p in sc.Params -> (p.DeclKey, Unification.zonk p.Type) })

        // Binder NodeKey for a `let`-preamble head (simple names only in v1); the
        // key matches `bindingsOfPat` (the innermost `NamedSimple`'s own key).
        let binderKeyOf (b: Binding<SyntaxToken>) : NodeKey voption =
            let rec walk (p: Pat<SyntaxToken>) =
                match p with
                | Pat.NamedSimple _ -> ValueSome(CstKeys.ofPat p)
                | Pat.EnclosedBlock(pat = inner)
                | Pat.Typed(pat = inner)
                | Pat.Attributed(pat = inner) -> walk inner
                | _ -> ValueNone

            walk b.headPat

        let chainArgs (e: Expr<SyntaxToken>) : EqArray<TExpr> =
            let raw =
                match e with
                | Expr.HighPrecedenceApp(argExpr = arg) -> peelOneArg (translateExpr ctx) arg
                | Expr.App(argExprs = args) -> peelCtorArgs (translateExpr ctx) args
                | _ -> EqArray.empty

            raw

        let lets = ResizeArray<TCtorLet>()
        let mutable primaryArgs = EqArray.empty
        let fieldInits = ResizeArray<TCtorFieldInit>()

        // The explicit field-init form `new(args) = { f = e; … }`:
        // each `FieldInitializer` stores into a declared
        // instance field. The `LongIdent` is a single field-name segment (the
        // last segment names the field); there is no primary-ctor chain.
        let fieldInitsOf (inits: ImmutableArray<FieldInitializer<SyntaxToken>>) =
            for FieldInitializer(longIdent = li; expr = e) in inits do
                if not li.Idents.IsEmpty then
                    fieldInits.Add
                        {
                            Field = ctx.NameOf li.Idents.[li.Idents.Length - 1]
                            Init = translateExpr ctx e
                        }

        let rec go (ace: AdditionalConstrExpr<SyntaxToken>) =
            match ace with
            | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
                match binderKeyOf b with
                | ValueSome k ->
                    lets.Add
                        {
                            Binder = k
                            Type = typeOfKey ctx k
                            Init = translateExpr ctx b.expr
                        }
                | ValueNone -> ()

                go body
            | AdditionalConstrExpr.SequenceAfter(rest = rest) -> go rest
            | AdditionalConstrExpr.SequenceBefore(before = before) -> go before
            | AdditionalConstrExpr.Conditional(thenBranch = t) -> go t
            | AdditionalConstrExpr.Init initExpr ->
                match initExpr with
                | AdditionalConstrInitExpr.Expression e
                | AdditionalConstrInitExpr.Delegated(expr = e) -> primaryArgs <- chainArgs e
                | AdditionalConstrInitExpr.Explicit(initializers = inits) -> fieldInitsOf inits

        go sc.Body

        {
            Params = parms
            Lets = EqArray.ofSeq lets
            PrimaryArgs = primaryArgs
            FieldInits = EqArray.ofSeq fieldInits
        }

    /// Build the `TDecl.Type` wrapper shared by record / union / interface
    /// (and the upcoming class) surfacers — same five-field shape, only `Kind`
    /// differs. `typars` is the already-projected typar-name list (`info` /
    /// `tryInterfaceMethods` projections both flow through here unchanged).
    let private mkTypeDecl
        (name: string)
        (key: SymbolKey)
        (ns: string option)
        (typars: EqArray<string>)
        (kind: TTypeKind)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : TDecl =
        TDecl.Type
            {
                Name = name
                Key = key
                Namespace = ns
                TypeParams = typars
                Kind = kind
                EqualitySupport = eq
                ComparisonSupport = cmp
            }

    /// Surface a `TypeDefn.Union` as a `TDecl.Type` from the resolved
    /// `UnionTypeInfo`. Any declaring-type typar is remapped to a `TyConst "'A"`
    /// marker (a no-op for a monomorphic union — `TypeParams` empty). Augmentation
    /// members (`ext`) are surfaced as `TTypeMember`s.
    let private tryUnionType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (declKey: NodeKey voption)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TypeVar * SemType) list) option =
        // Resolve the union by the `SymbolKey`
        // `NameResolution` stamped at the decl site, rather than re-deriving the
        // `(name, arity)` key here. The stamp is co-populated with `ctx.Types.Union`
        // (same registration branch), so this is exactly as total as the former
        // `TypeRegistry.tryUnion name arity`.
        let resolved =
            match declKey with
            | ValueSome k ->
                match ctx.Resolution.ResolvedType.TryGetValue k with
                | ValueSome key -> TypeRegistry.tryUnionByKey ctx.Types key
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone

        match resolved with
        | ValueNone -> None
        | ValueSome info ->
            let markers = mkDeclTyparEnv info.TypeParams
            // The decl's freeze env (declaring typars + any member method typars),
            // collected here at the single index-minting point; `freezeTypars`
            // applies it to the whole decl, performing the deferred `TyVar` cut.
            let env = ResizeArray markers

            let cases =
                EqArray.ofSeq (
                    seq {
                        for c in info.Cases ->
                            let fields =
                                EqArray.ofSeq (
                                    seq {
                                        for i in 0 .. c.Fields.Length - 1 ->
                                            let nm =
                                                if i < c.FieldNames.Length then
                                                    c.FieldNames.[i]
                                                else
                                                    ValueNone

                                            nm, c.Fields.[i]
                                    }
                                )

                            { Name = c.Name; Fields = fields }
                    }
                )

            // A generic union's members carry the declaring typars as `TyVar` roots
            // in the self-type; `freezeTypars` later cuts them to `TyTypar`
            // (`!0`), exactly like the case fields. Monomorphic unions
            // (`declTypars` empty) keep `translateNominalMember`'s `TyUnion(key, [])`
            // self-type untouched, so the path stays byte-identical.
            let declTypars = [ for (n, _) in info.TypeParams -> n ]

            let selfTy = TyUnion(info.Key, declTyparArgs info.TypeParams)
            let elaborateOne = mkMemberElaborator selfTy declTypars env

            let members, interfaces =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            Some(
                mkTypeDecl
                    name
                    info.Key
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Union(cases, members, interfaces))
                    info.EqualitySupport
                    info.ComparisonSupport,
                List.ofSeq env
            )

    /// Resolve one enum case's value `Expr` to a `TEnumLiteral` via the canonical
    /// literal readers (`ElaborateLiterals.parseConst` for a numeric / bool / char
    /// constant, `foldStringParts` for a string), classifying it as `Int` or
    /// `String`. A non-literal expression, an interpolated string, or a
    /// non-int-non-string constant (bool / char / float / decimal) is a hard
    /// error (reported at the case identifier `idTok`) and yields `ValueNone` —
    /// the only heterogeneity admitted is int + string *across* cases (the mixed
    /// warning, raised once per enum below), never within a single case value.
    let rec private resolveEnumCaseValue
        (ctx: PassContext)
        (idTok: SyntaxToken)
        (v: Expr<SyntaxToken>)
        : TEnumLiteral voption =
        match v with
        // A value-grouping paren around the literal (`| C = (1)`) is not itself
        // the constant; peel it and resolve the inner expression.
        | Expr.EnclosedBlock(expr = inner) -> resolveEnumCaseValue ctx idTok inner
        | Expr.Const c ->
            // The lexer merges `-<numeric>` into a single negative literal token
            // (`tryMergeNegativeLiteral`) ONLY when the `-` follows an opening
            // bracket/brace/paren or trivia (`allowsNegativeLiteral`). After the `=`
            // of an enum case a *bare* `| A = -1` is NOT merged — it parses as a
            // unary-minus `PrefixApp` (the arm below). A negative integral literal
            // reaches THIS arm via the parenthesised form `| A = (-1)`: the `(`
            // admits the merge, then the `EnclosedBlock` arm peels it to a negative
            // `Const`. A negative *signed* literal projects cleanly (`Int -1`); a
            // negative *unsigned* literal (`(-1uy)`/`(-1u)`) has no representation —
            // `tryParseConst` reports it as an `Error` (total; it no longer throws),
            // surfaced here as the hard error.
            match ElaborateLiterals.tryParseConst ctx c with
            // Any integral width a CLR enum may be based on — `int` doubles as the
            // unsuffixed default, and the rest preserve the authored width for step 2.
            // `isEnumBase` excludes exactly the pointer pair; they fall to the error below.
            | Ok(TConstValue.Integral(w, _) as iv) when IntWidth.isEnumBase w -> ValueSome(TEnumLiteral.Int iv)
            // The literal is no primitive constant at all, and the two reasons are
            // different things to tell the user — `52I` is not an out-of-range magnitude.
            | Error ConstRejection.OutOfRange ->
                ctx.Error(
                    NodeKey.ofToken idTok NodeKind.DeclType,
                    "An enum case value is not representable at its authored width (a negative value has no unsigned representation)"
                )

                ValueNone
            | Error ConstRejection.CustomLiteral ->
                ctx.Error(
                    NodeKey.ofToken idTok NodeKind.DeclType,
                    "An enum case value must be a primitive integer literal; a custom numeric literal ('52I') is a call to a NumericLiteral module, not a constant"
                )

                ValueNone
            | Ok other ->
                ctx.Error(
                    NodeKey.ofToken idTok NodeKind.DeclType,
                    sprintf
                        "An enum case value must be an integer or string literal; '%A' is not a valid enum constant"
                        other
                )

                ValueNone
        | Expr.String _ ->
            // Plain / verbatim / triple-quoted string literals are constants (the
            // shared projection folds them); an interpolated string ($"…") is the
            // only String kind it declines — reject that as non-literal.
            match StringLiterals.tryEnumCaseStringLiteral ctx v with
            | ValueSome s -> ValueSome(TEnumLiteral.String s)
            | ValueNone ->
                ctx.Error(
                    NodeKey.ofToken idTok NodeKind.DeclType,
                    "An enum case value must be a literal string; an interpolated string is not a constant"
                )

                ValueNone
        // A unary minus on an integer literal (`| A = -1`) parses as a PrefixApp
        // (`-` → op_UnaryNegation), not an `Expr.Const`, yet negative integral enum
        // members are legal and common (`None = -1`). Admit *only* a single unary
        // minus directly on an integral literal (recursing peels an enclosing paren
        // so `-(1)` works); the recursion stays bounded to the literal forms above,
        // so general arithmetic (`1 + 1`, `-(1 + 1)`) still falls through to the
        // expression error. Negating an unsigned width (`-1uy`/`-1u`) has no
        // representation and is a hard error; a unary minus on a string (or any
        // non-int constant, handled by the inner resolution) likewise stays an error.
        | Expr.PrefixApp(op, operand) when op.Token = Token.OpSubtraction ->
            match resolveEnumCaseValue ctx idTok operand with
            // Negation is defined on the signed widths and no other. It wraps AT THE WIDTH
            // (`IntWidth.negate`), so `-(-128y)` stays `-128y`.
            | ValueSome(TEnumLiteral.Int(TConstValue.Integral(w, bits))) when IntWidth.isSigned w ->
                ValueSome(TEnumLiteral.Int(TConstValue.Integral(w, IntWidth.negate w bits)))
            | ValueSome(TEnumLiteral.Int(TConstValue.Integral _)) ->
                ctx.Error(
                    NodeKey.ofToken idTok NodeKind.DeclType,
                    "A negative enum case value has no unsigned representation; use a signed integer width"
                )

                ValueNone
            // `-"abc"` or a deeper non-int form: the inner resolution produced a
            // non-negatable shape (the `Int _` arm is unreachable — handled above —
            // but kept for exhaustiveness). Reject.
            | ValueSome(TEnumLiteral.String _)
            | ValueSome(TEnumLiteral.Int _) ->
                ctx.Error(
                    NodeKey.ofToken idTok NodeKind.DeclType,
                    "An enum case value must be a literal integer or string constant, not an expression"
                )

                ValueNone
            | ValueNone -> ValueNone
        | _ ->
            ctx.Error(
                NodeKey.ofToken idTok NodeKind.DeclType,
                "An enum case value must be a literal integer or string constant, not an expression"
            )

            ValueNone

    /// Surface a `TypeDefn.Enum` as a `TDecl.Type`. Each case's constant-value
    /// `Expr` is resolved to a `TEnumLiteral` (`resolveEnumCaseValue`) and the
    /// ordered case→literal table recorded on the node; the numeric / string /
    /// mixed variant is left *derivable* (`TEnumCases.classify`) rather than
    /// stored. A mix of int and string case values is accepted with a **warning**
    /// (heterogeneous enums are legal but discouraged; the repr is a later
    /// freeze/backend concern). An enum has no type parameters and no augmentation
    /// members, so the decl is a flat case list with the canonical arity-0 type
    /// key minted directly (mirroring the interface fallback's
    /// `LocalSymbolKey.ofType`).
    let private tryEnumType
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (name: string)
        (cases: EnumTypeCases<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) option =
        let ns = DeclContainment.namespaceOpt c
        // The key of the type being LOWERED, minted from the module the walk is in — the
        // SAME key `NameResolution.registerEnumTypeDefn` minted, so the surfaced decl, the
        // `(x: E)` annotation and the `E.C1` access all share one identity. It stands on its
        // own when the registry has no entry (a duplicate enum the registrar rejected), which
        // is why the mint comes first and the lookup second.
        let key = ctx.DeclaredTypeKey(name, 0)

        let tcases =
            EqArray.ofSeq (
                seq {
                    for EnumTypeCase(ident = id; constValue = v) in cases ->
                        {
                            Name = ctx.NameOf id
                            Value = resolveEnumCaseValue ctx id v
                            Tok = id
                        }
                }
            )

        // A mixed (int + string) enum is accepted but warned; pin the warning to
        // the first case's token (cases are `sepBy1`, so always non-empty).
        match TEnumCases.classify tcases with
        | ValueSome TEnumVariant.Mixed ->
            let (EnumTypeCase(ident = firstId)) = cases.[0]

            ctx.Warn(
                NodeKey.ofToken firstId NodeKind.DeclType,
                sprintf
                    "Enum '%s' mixes integer and string case values; heterogeneous enums are legal but discouraged"
                    name
            )
        | _ -> ()

        // CLR uniform-width invariant: a `System.Enum` has exactly one underlying
        // integral type, so explicitly-suffixed cases of differing width
        // (`| A = 1uy | B = 2L`) are a hard error. Unsuffixed `Int` cases are
        // width-flexible (they adopt the single explicit width present) and never
        // conflict; string / mixed enums carry no integral width. Reported at the
        // offending case's token, via the same `ctx.Error` channel.
        match TEnumCases.firstWidthConflict tcases with
        | ValueSome(tok, w0, w1) ->
            ctx.Error(
                NodeKey.ofToken tok NodeKind.DeclType,
                sprintf
                    "Enum '%s' mixes integral widths '%s' and '%s'; a CLR enum has a single underlying type"
                    name
                    w0
                    w1
            )
        | ValueNone -> ()

        Some(
            mkTypeDecl
                name
                key
                ns
                (EqArray.ofList [])
                (TTypeKind.Enum tcases)
                // An enum synthesises no equality triple / comparison pair here;
                // the verdict fields keep the decl record total and stay unread.
                EqualityVerdict.Structural
                ComparisonVerdict.NoComparison,
            []
        )

    /// Surface a `TypeDefn.Record` as a `TDecl.Type` from the resolved
    /// `RecordTypeInfo`. Field types are remapped through the declaring-type
    /// typars (a no-op for a monomorphic record — `TypeParams` empty — but the
    /// right shape for the generic record path, exactly like `tryUnionType`).
    /// Augmentation members and `interface … with` impls are surfaced from `ext`
    /// (the registered `info.Members` / `info.InterfaceImpls`), mirroring
    /// `tryUnionType`.
    let private tryRecordType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (declKey: NodeKey voption)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TypeVar * SemType) list) option =
        // Resolve the record by the `SymbolKey` `NameResolution` stamped at the decl
        // site (`tryRecordByKey`), not the bare name — an arity-overloaded record
        // (`Point`2`/`Point`3`) does not resolve by bare name. Mirrors `tryUnionType`.
        let resolved =
            match declKey with
            | ValueSome k ->
                match ctx.Resolution.ResolvedType.TryGetValue k with
                | ValueSome key -> TypeRegistry.tryRecordByKey ctx.Types key
                | ValueNone -> ValueNone
            | ValueNone -> ValueNone

        match resolved with
        | ValueNone -> None
        | ValueSome info ->
            let markers = mkDeclTyparEnv info.TypeParams
            // The decl's freeze env (declaring typars + any member method typars),
            // collected here at the single index-minting point; mirrors `tryUnionType`.
            let env = ResizeArray markers

            let fields =
                EqArray.ofSeq (
                    seq {
                        for f in info.Fields ->
                            {
                                Name = f.Name
                                Type = f.Type
                                IsMutable = f.IsMutable
                            }
                    }
                )

            let declTypars = [ for (n, _) in info.TypeParams -> n ]
            let selfTy = TyRecord(info.Key, declTyparArgs info.TypeParams)
            let elaborateOne = mkMemberElaborator selfTy declTypars env

            let members, interfaces =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            Some(
                mkTypeDecl
                    name
                    info.Key
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Record(fields, members, interfaces))
                    info.EqualitySupport
                    info.ComparisonSupport,
                List.ofSeq env
            )

    /// Surface a `TypeDefn.Class` (or class-shaped `TypeDefn.Anon`) as a
    /// `TDecl.Type` from the resolved `ClassTypeInfo`. Ctor params and member
    /// signatures are remapped through the declaring-type typars (the same
    /// `mkTypeMarkers` + `remapDeclTypars` pipeline records / unions use).
    /// An early slice left `fields` empty (no mutable instance fields yet) and
    /// `baseType` `ValueNone` (codegen defaults to `Object`); a later slice fills the
    /// base type and another projects `info.InterfaceImpls` onto
    /// `interfaces`.
    let private tryClassType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (arity: int)
        (elements: TypeDefnElements<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) option =
        // The key of the type being LOWERED, minted from the module the walk is in — not a
        // by-name read. This is the declaration itself: a sibling module's same-named class
        // is a different type, and an arity-overloaded `Box\`1`/`Box\`2` does not resolve by
        // bare name at all.
        match TypeRegistry.tryClassByKey ctx.Types (ctx.DeclaredTypeKey(name, arity)) with
        | ValueNone -> None
        | ValueSome info ->
            let markers = mkDeclTyparEnv info.TypeParams
            // The decl's freeze env: declaring typars plus every generic member's
            // method typars, accumulated as members are surfaced. `freezeTypars`
            // applies it to the whole class decl, cutting `TyVar → TyTypar`.
            let env = ResizeArray markers

            let ctorParams =
                EqArray.ofSeq (
                    seq {
                        for p in info.CtorParams ->
                            {
                                Name = p.Name
                                Type = p.Type
                                IsMutable = false
                            }
                    }
                )

            // Explicit `val [mutable] x: T` instance fields.
            // Their linked placeholder TyVars are zonked + cut to declaring typars by
            // the later `freezeTypars`/`field` mapper, exactly as `ctorParams`.
            let instanceFields =
                EqArray.ofSeq (
                    seq {
                        for fld in info.InstanceFields ->
                            {
                                Name = fld.Name
                                Type = fld.Type
                                IsMutable = fld.IsMutable
                            }
                    }
                )

            let declTypars = [ for (n, _) in info.TypeParams -> n ]

            let selfTy = TyClass(info.Key, declTyparArgs info.TypeParams)

            // Surface a member when the declaring type is generic (declaring axis)
            // *or* the member itself is generic (method axis): stamp its
            // self-type and fold its method typars into the decl env, so
            // `freezeTypars` later flips both axes. A generic method on a
            // *monomorphic* class still needs its `'C` cut to `TyTypar(Method, i)`,
            // so it can't be skipped. For a mono type with a
            // mono member, `selfTy = TyClass(key, [])` equals the member's existing
            // `ThisTy`, so leaving it verbatim is byte-identical.
            let needsRemap (m: TTypeMember) =
                not (List.isEmpty declTypars) || GeneralizedTypars.count m.MethodTypeParams > 0

            let elaborateOne (m: TTypeMember) : TTypeMember =
                if needsRemap m then
                    let m, methodMarkers = elaborateMember selfTy m
                    env.AddRange methodMarkers
                    m
                else
                    m

            let members =
                EqArray.ofSeq (
                    seq {
                        for el in elements do
                            match translateClassMember ctx info el with
                            | ValueSome m -> yield elaborateOne m
                            | ValueNone -> ()
                    }
                )

            // The preambles. `translateBinding` — not `translateExpr` on the initialiser —
            // is what makes `let f x = …` the function value it is. A reference to an
            // earlier binder is rewritten to the field it lowers to, exactly as in a member
            // body; the instance sequence additionally sees the ctor params (it runs inside
            // the primary ctor). Both sequences ride the class's declaring typars as `TyVar`
            // roots, which the decl-wide `freezeTypars` cuts — so a generic class's
            // preamble needs no special case.
            let staticRewrite = staticFieldRewrite info
            let instanceRewrite = instanceFieldRewrite info selfTy

            let translatePreambleEntry (rewrite: TExpr -> TExpr) (entry: ClassPreambleEntry) : TPreambleEntry =
                match entry with
                | ClassPreambleEntry.Let l ->
                    TPreambleEntry.Let
                        {
                            Name = l.Name
                            Type = Unification.zonk l.Type
                            IsMutable = l.IsMutable
                            Init = translateBinding ctx l.Binding |> rewrite
                        }
                | ClassPreambleEntry.Do e -> TPreambleEntry.Do(translateExpr ctx e |> rewrite)

            let staticPreamble =
                EqArray.ofSeq (
                    seq {
                        for entry in info.StaticPreamble ->
                            translatePreambleEntry (rewriteFieldRefs staticRewrite) entry
                    }
                )

            let instancePreamble =
                EqArray.ofSeq (
                    seq {
                        for entry in info.InstancePreamble ->
                            translatePreambleEntry
                                (rewriteFieldRefs staticRewrite >> rewriteFieldRefs instanceRewrite)
                                entry
                    }
                )

            // Secondary constructors. Each `new(...)` overload becomes a
            // `TSecondaryCtor`; codegen emits a `.ctor` overload chaining to the
            // primary ctor. Empty unless the class declares any.
            let secondaryCtors =
                EqArray.ofSeq (seq { for sc in info.SecondaryCtors -> translateSecondaryCtor ctx sc })

            // Inheritance. `baseType` is the parent's resolved
            // `TyClass`, carried with this class's declaring typars as `TyVar` roots
            // so `freezeTypars` encodes a generic parent (`SetTree\`1<!0>`) against
            // this class's own generic parameters; codegen reads it for the IL
            // `TypeDefinition.BaseType`. `baseCtorCall` carries the `inherit
            // Base(args)` invocation: the derived class's primary-ctor params (the
            // `ldarg` mapping the args reference, since `this` isn't constructed yet)
            // and the translated arg expressions.
            let baseType = info.BaseType

            // Interface implementations.
            // Each registered `interface IFace with member …` block becomes an
            // `(ifaceTy, members)` entry: the resolved interface `TyClass` (carrying
            // this class's declaring typars as roots so a generic arg like
            // `IEnumerable<'T>` encodes against this class's typars after the cut)
            // paired with its already-typed member bodies. The bodies translate
            // through the *class* `info` exactly like the class's own members —
            // `this` and ctor-param references rewrite identically — but read their
            // elements from the impl's own `Elements`. Impls whose interface failed
            // to resolve (`Resolved = ValueNone`, the diagnostic already fired)
            // are dropped.
            let interfaces =
                EqArray.ofSeq (
                    seq {
                        for impl in info.InterfaceImpls do
                            match impl.Resolved with
                            | ValueSome ifaceTy ->
                                let implMembers =
                                    EqArray.ofSeq (
                                        seq {
                                            for el in impl.Elements do
                                                match translateClassMember ctx info el with
                                                | ValueSome m -> yield elaborateOne m
                                                | ValueNone -> ()
                                        }
                                    )

                                yield (ifaceTy, implMembers)
                            | ValueNone -> ()
                    }
                )

            let baseCtorCall =
                match info.BaseType, info.BaseCtorArgs with
                | ValueSome _, ValueSome argExpr ->
                    let ctorParamKeys =
                        EqArray.ofSeq (seq { for p in info.CtorParams -> (p.DeclKey, Unification.zonk p.Type) })

                    // The base-ctor args run before `this` exists (they are `ldarg`-only), so the
                    // INSTANCE rewrite must not apply — but the `.cctor` has already run, so a
                    // `static let` is in scope here (NameResolution scopes it in) and is a FIELD:
                    // without the static rewrite its binder `NodeKey` would survive as a bare
                    // `TExpr.Var` into the base-ctor args, where codegen has no slot for it.
                    let args = peelOneArg (translateExpr ctx >> rewriteFieldRefs staticRewrite) argExpr

                    ValueSome
                        {
                            CtorParams = ctorParamKeys
                            Args = args
                        }
                | _ -> ValueNone

            Some(
                mkTypeDecl
                    name
                    info.Key
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Class
                        {
                            Fields = instanceFields
                            CtorParams = ctorParams
                            Members = members
                            BaseType = baseType
                            Interfaces = interfaces
                            IsSealed = info.IsSealed
                            StaticPreamble = staticPreamble
                            InstancePreamble = instancePreamble
                            ThisKey = info.ThisKey
                            SecondaryCtors = secondaryCtors
                            BaseCtorCall = baseCtorCall
                            // The mutable `ClassTypeInfo` bool pair collapses into the
                            // invariant-enforcing tri-state here (a ref struct is
                            // necessarily a value type, so `IsByRefLike` wins).
                            ValueKind =
                                if info.IsByRefLike then ClassValueKind.RefStruct
                                elif info.IsValueType then ClassValueKind.Struct
                                else ClassValueKind.RefType
                            HasPrimaryCtor = info.HasPrimaryCtor
                        })
                    // Classes are reference-equal by default;
                    // [<CustomEquality>] / [<NoEquality>] lift this in a later sprint.
                    EqualityVerdict.Reference
                    ComparisonVerdict.NoComparison,
                List.ofSeq env
            )

    /// Surface an inline intrinsic-abbrev host (`type X = (# … #) with member …`) as a
    /// `TDecl.Type` of kind `Class` from its `IntrinsicAbbrevInfo`. This decl is an
    /// INTERNAL artifact consumed only by the member-inline harvest (a concrete
    /// `(# … #)`-bodied member becomes a `this`-first inline body); it is NEVER emitted,
    /// and the abbrev keeps its `TyConst` identity (it stays in `IntrinsicReprTypes`).
    /// Members surface through the shared host-member path (`elaborateHostMembers` →
    /// `translateNominalMember`), whose `MkSelfType` yields the abbrev's `TyConst` type,
    /// so each member's `ThisTy` is the intrinsic type — NOT a `TyClass`. Every non-member
    /// `Class` facet is empty (no ctor / fields / base / static-lets / impls). `Class` is
    /// the container kind because it is the one PROVEN INERT through the non-frozen passes
    /// this decl still traverses (`Regions` / `RefCellPromotion` / `ResolvedTypes` /
    /// `PlatformTypes` run before emit): an empty-cases `Union` / empty-fields `Record`
    /// would route its members through those passes' union/record-specific branches
    /// (e.g. `PlatformTypes`' `Record | Union` arm) for no gain. The harvest itself is
    /// kind-agnostic (`TTypeKindG.members`), so the choice is purely which container is
    /// safest to carry inert.
    let private tryIntrinsicAbbrevType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TypeVar * SemType) list) option =
        match ctx.Types.IntrinsicAbbrevHost.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers = mkDeclTyparEnv info.TypeParams
            let env = ResizeArray markers
            let declTypars = [ for (n, _) in info.TypeParams -> n ]
            let selfTy = TyConst(info.SelfKey, declTyparArgs info.TypeParams)
            let elaborateOne = mkMemberElaborator selfTy declTypars env

            let members, _ =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            let clsG: TClassG<SemType, SyntaxToken> =
                {
                    Fields = EqArray.empty
                    CtorParams = EqArray.empty
                    Members = members
                    BaseType = ValueNone
                    Interfaces = EqArray.empty
                    IsSealed = false
                    StaticPreamble = EqArray.empty
                    InstancePreamble = EqArray.empty
                    ThisKey = info.ThisKey
                    SecondaryCtors = EqArray.empty
                    BaseCtorCall = ValueNone
                    ValueKind = ClassValueKind.RefType
                    HasPrimaryCtor = false
                }

            Some(
                mkTypeDecl
                    name
                    info.Key
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Class clsG)
                    EqualityVerdict.Reference
                    ComparisonVerdict.NoComparison,
                List.ofSeq env
            )

    /// Surface an interface-shaped, union, record, or class `TypeDefn` as a
    /// `TDecl.Type`. A plain abbreviation surfaces nothing; an inline intrinsic-abbrev
    /// carrying a `with member …` augmentation surfaces its members (harvest-only).
    let private tryTypeDecl
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (td: TypeDefn<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) option =
        let ns = DeclContainment.namespaceOpt c

        let classify tn (body: ObjectModelBody<SyntaxToken>) =
            let name = typeNameSimple ctx tn

            let arity = NameResolutionTypeRegistration.arityOfTypeName ctx tn

            match tryInterfaceMethods ctx name arity body with
            | Some(typars, methods, env) ->
                // Interfaces aren't in the codegen emitted-type tables (their own
                // `interfaceDecls` path), but `TTypeDecl.Key` is total — mint the identity
                // registration would, from the SAME containment-derived holder
                // (`localTypeHolder`), so a reference to the interface compares equal to
                // this decl's key wherever the interface is declared.
                let key = ctx.DeclaredTypeKey(name, typars.Length)

                Some(
                    mkTypeDecl
                        name
                        key
                        ns
                        typars
                        (TTypeKind.Interface methods)
                        // Interfaces never synthesise an equality triple or
                        // comparison pair — the verdict fields are filled to
                        // keep the record shape total and the values are
                        // unread for this kind.
                        EqualityVerdict.Structural
                        ComparisonVerdict.NoComparison,
                    env
                )
            // Not all-abstract ⇒ class shape (`type C(x) = member …`).
            | None -> tryClassType ctx ns name arity body.elements

        match td with
        | TypeDefn.Anon(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Interface(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Class(typeName = tn; body = body) ->
            tryClassType
                ctx
                ns
                (typeNameSimple ctx tn)
                (NameResolutionTypeRegistration.arityOfTypeName ctx tn)
                body.elements
        | TypeDefn.Union(typeName = tn; extensions = ext) ->
            tryUnionType ctx ns (typeNameSimple ctx tn) (typeNameDeclKey ctx tn) ext
        | TypeDefn.Record(typeName = tn; extensions = ext) ->
            tryRecordType ctx ns (typeNameSimple ctx tn) (typeNameDeclKey ctx tn) ext
        | TypeDefn.Enum(typeName = tn; cases = cases) -> tryEnumType ctx c (typeNameSimple ctx tn) cases
        // A plain abbrev has no host in `IntrinsicAbbrevHost` and surfaces `None`; an
        // inline intrinsic-abbrev with `with member …` surfaces its members (harvest-only).
        | TypeDefn.Abbrev(typeName = tn; extensions = ext) -> tryIntrinsicAbbrevType ctx ns (typeNameSimple ctx tn) ext
        | _ -> None

    /// `c` is the element's declaring containment — the `namespace` group plus the
    /// `module`s it is nested in. Its innermost module is the compiled holder type a `let`
    /// binding lands on: such a binding records its `NodeKey` → `ModuleMemberInfo` so the
    /// backend emits it as a named public static method on that holder (e.g.
    /// `ListModule::fold`) rather than on the anonymous "Program" holder. The holder is
    /// `ModuleRules.holderChain` — the SAME chain builder the type-key mint reads
    /// (`localTypeHolder`), so a binding and a type declared in one module are held by the
    /// same module key, nesting included.
    let private translateModuleElem
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (m: ModuleElem<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) list =
        let holder =
            match ctx.CurrentHolder with
            | ModuleHolder.InModule mk -> Some mk
            | ModuleHolder.InNamespace _ -> None

        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings do
                    let tpat = translatePat ctx b.headPat

                    // Inside a named module: record where this binding's static
                    // method belongs. The emitted method takes its `[<CompiledName>]`
                    // (the IL boundary name, e.g. `Set.empty` ⇒ `SetModule.Empty`),
                    // falling back to the source name — matching the contract
                    // extractor's `compiledNameForVal`, so a separately-compiled
                    // consumer resolving `Set.empty` to `SetModule.Empty` finds the
                    // method this emits. Both the producer-internal call resolver
                    // (`SymbolProviders`) and codegen key off this same `Name`.
                    match holder with
                    | Some h ->
                        match memberNameOfBinding ctx b with
                        | ValueSome nm ->
                            let compiledNm =
                                match VesperLibTypeTranslate.tryCompiledName ctx.Lexed ctx.Input b.attributes with
                                | ValueSome cn -> cn
                                | ValueNone -> nm

                            // The holder is the containment chain itself, so the binding's
                            // `SymbolKey` is a direct construction downstream
                            // (`ModuleMemberInfo.Key`), never a dotted-string re-parse.
                            ctx.Bindings.ModuleMembers.[CstKeys.ofBinding b] <- { Holder = h; Name = compiledNm }
                        | ValueNone -> ()
                    // A top-level (implicit-Program-module) binding records no
                    // `ModuleMemberInfo`; stash its source name so the backend can
                    // name a top-level value's Program-holder static field. Recorded for every top-level
                    // binding (function or value); only the value collector reads it,
                    // so a top-level function's `fn$<off>` path is untouched.
                    | None ->
                        match memberNameOfBinding ctx b with
                        | ValueSome nm -> ctx.Bindings.TopLevelNames.[CstKeys.ofBinding b] <- nm
                        | ValueNone -> ()

                    let valT = translateBinding ctx b
                    let declTy = typeOfKey ctx (CstKeys.ofBinding b)

                    // Decode + validate compiler parameter attributes
                    // (`[<CallAtMostOnce>]`) for an inline binding, recording them
                    // for `Passes.InlineExpansion`. Keyed by the function binder.
                    match tpat with
                    | TPat.NamedSimple(binderKey, _, _) -> recordInlineParamAttrs ctx b binderKey valT
                    | _ -> ()

                    // A module-`let` compiled as a generic
                    // static method (or generic closure) carries its free typars as
                    // `TyTypar(Method, i)`. The index order is minted once here
                    // (Edge A order) as `quantEnv`, but the cut itself is deferred to
                    // `freezeTypars` — `elaborate` leaves the head pattern, value
                    // body, and declared type in `TyVar` form and just pairs the decl
                    // with its `quantEnv`. A *function* binding (`TyFun` declared
                    // type) always quantifies. A *value* binding quantifies only when
                    // (a) the generaliser left it a non-empty scheme and (b) its free
                    // typars sit *inside a type constructor* (`let empty: SetTree<'T> =
                    // null` ⇒ `TyClass(SetTree, ['T])`, lowered to a generic method
                    // returning `ldnull : SetTree<!!0>`, which verifies). A *bare* free
                    // var — `let n = null` (`TyVar`) or `let x: 'T = …` — stays a
                    // value-restriction metavar: generalising it would emit `ldnull :
                    // !!0` over an unconstrained typar (no `class` constraint ⇒
                    // unverifiable), so it keeps its `TyVar` representation.
                    // The binding's explicit `<'b,'a>` typars, in source order with
                    // their inference-seeded roots (recorded by `inferBinding` while
                    // the transient `TyparScope` was live). `canonical` orders these
                    // first, the F# rule.
                    let declaredTypars =
                        match ctx.Bindings.DeclaredTypars.TryGetValue(CstKeys.ofBinding b) with
                        | ValueSome ds -> ds
                        | ValueNone -> []

                    let quantEnv =
                        // An INLINE binding quantifies unconditionally — every shape
                        // gate below is about EMISSION, and an inline binding is never
                        // emitted. It is a TEMPLATE: `Freeze` publishes it as vocabulary
                        // and a consumer thaws + substitutes it per call site, so its
                        // free typars are its template parameters and must be named on a
                        // self-describing axis (`TyTypar(Method, i)` ⇒ `FTTypar`), not
                        // left as roots only this compilation's `UnionFind` can explain.
                        //
                        // The value-restriction arm is exactly where that bites:
                        // `let inline defaultof<'T> : 'T = (# "ilzero" … #)` has a declTy
                        // that zonks to a bare `TyVar`, so it would fall into `| TyVar _
                        // -> []` and reach freeze with an unmapped root — which is not a
                        // metavar leak (its scheme DID quantify it) but has no binder
                        // freeze can honestly name, so it would degrade to `FTUnknown`.
                        // Its verifiability rationale does not apply either: no `ldnull :
                        // !!0` is ever emitted for a template.
                        if b.inlineToken.IsSome then
                            mkMethodQuantEnv declaredTypars declTy
                        else
                            match Unification.zonk declTy with
                            | TyFun _ -> mkMethodQuantEnv declaredTypars declTy
                            // A bare free var is value-restricted — never a method typar.
                            | TyVar _
                            | TyTypar _ -> []
                            | _ when bindingWasGeneralised ctx b -> mkMethodQuantEnv declaredTypars declTy
                            | _ -> []

                    // Record the binding's frozen typar
                    // bounds using THIS `quantEnv` (the same env `freezeTypars` freezes
                    // the body with, so the bounds' typar indices line up). Read by the
                    // call-site phantom-typar solve (`EmitCall`).
                    recordGenericFnScheme ctx b quantEnv

                    // Drop an E1 format-literal alias binding (`let fmt : Format<…> =
                    // "%d"`): its value froze to a `New PrintfFormat` that is dead —
                    // every use const-propagates the literal (`PrintfFormatLiterals`),
                    // and the self-host contract has no cold runtime for a format value,
                    // so nothing reads it. (A genuinely dynamic read is E2, rejected
                    // upstream.) Eliding it here keeps `New PrintfFormat` off codegen.
                    if not (ctx.PrintfFormatLiterals.ContainsKey(CstKeys.ofPat b.headPat)) then
                        yield TDecl.Let(tpat, valT, b.inlineToken.IsSome, declTy), quantEnv
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)), [] ]
        | ModuleElem.Type defs -> defs |> Seq.choose (tryTypeDecl ctx c) |> List.ofSeq
        | _ -> []

    /// The first half of the split Elaborate pass: translate
    /// the CST to a `TExpr` tree whose `.ty` fields are zonk'd `SemType`, still
    /// `TyVar`-carrying (no `TyTypar`). Each decl is paired with the typar `env`
    /// it quantifies — the declaring / method / static-fn typar roots, collected at
    /// this single index-minting point. `freezeTypars` consumes that `env` to make
    /// the `TyVar → TyTypar` cut. (A later change will slot the inline-expansion pass
    /// between `elaborate` and the freeze cut, where `zonk` / union-find are native;
    /// today nothing runs between them and the output is byte-identical to the old
    /// fused pass.)
    let elaborate (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : (TDecl * (TypeVar * SemType) list) list =
        // The SAME flattened walk NameResolution and Unification take — a nested
        // `module Foo = …` surfaces its body flat, in source order, with the containment
        // extended (a binding at any depth is held by the whole chain of modules it is
        // written in). Sharing the walk is what makes `EnterElement` reach here: the by-name
        // reads lowering still makes (a class reference, a union-case head, an enum case)
        // must speak from the module AND the `open`s they are written under, and only the
        // walk knows those.
        CstWalk.walkModuleTreeWith ctx.NameOf ctx.Resolution.AmbientOpenScope (fun _ _ -> ()) file
        |> List.collect (fun w ->
            ctx.EnterElement w
            translateModuleElem ctx w.Containment w.Elem
        )

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        // Split pass: `elaborate` produces the
        // `TyVar`-carrying tree + per-decl typar envs; the `InlineExpansion` pass
        // then expands module-level inline call sites *before* the cut (where
        // `zonk` / union-find are native); `freezeTypars` makes the
        // `TyVar → TyTypar` cut on each.
        //
        // Cross-unit inline bodies ride `ctx.Provider` directly: a published body sits ON
        // the resolved entry (`ExternalSymbol.InlineBody` / `ExternalMember.InlineBody`),
        // reached by the same `SymbolKey` the use-site node carries. Frozen, so
        // `InlineExpansion` thaws it into this unit's own cells before splicing.
        let elaborateDecls () =
            elaborate ctx file
            |> InlineExpansion.run ctx
            |> List.map (fun (d, env) -> freezeTypars env d)

        // Elaborate assumes well-typed input: it asserts its invariants with `failwith`
        // (it never diagnoses). Under an already-diagnosed type error — malformed
        // source (`let fmt : Format<int -> string> = "%d %s"`, an arity/type mismatch) —
        // an invariant may not hold, and a raw `failwith` would abort the whole
        // compilation. Malformed source is expected input, not a reason to throw: when
        // inference has ALREADY recorded an error the program will not be code-generated,
        // so degrade elaboration to diagnostics-only (drop the decls, keep the errors)
        // rather than crash. With NO prior error, elaboration runs unguarded, so a
        // `failwith` on well-formed input still surfaces loudly as the compiler bug it is.
        let hasErrors = ctx.Diagnostics |> Seq.exists (fun d -> d.Severity = Severity.Error)

        let decls =
            if hasErrors then
                try
                    elaborateDecls ()
                with _ ->
                    []
            else
                elaborateDecls ()

        {
            Decls = EqArray.ofList decls
            // The inline vocabulary is `Freeze`'s to publish: it is the pass that
            // partitions the templates out of `Decls`, and it is where they become
            // `FrozenType`. Pre-freeze they are still IN `Decls`, where the same-unit
            // splice (`Passes.InlineExpansion`) reads them.
            InlineBodies = EqArray.empty
            Diagnostics = List.ofSeq ctx.Diagnostics
            // Snapshot so the backend can key the emitted IL type off the representation
            // string without the PassContext. The KEY-addressed table, not its by-name
            // twin: the backend holds a resolved canon key, and a display name cannot say
            // which type it names.
            IntrinsicReprKeys = System.Collections.Generic.Dictionary(ctx.Types.IntrinsicReprKeys)
            // Snapshot the named-module placements: the backend keys
            // off a binding's `NodeKey.Raw` to emit it on its holder type.
            ModuleMembers = ctx.Bindings.ModuleMembers |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            // Snapshot the top-level (implicit-Program-module) binding names so the
            // backend can name a top-level value's static field.
            TopLevelNames = ctx.Bindings.TopLevelNames |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            // The closure stack/heap verdict is filled in by the Pipeline after
            // `Regions.run` — escape analysis hasn't run at elaboration time.
            ClosureReprs = Map.empty
            // The value-struct closure verdicts — snapshotted by the
            // Pipeline from `ctx.FunVerdicts` alongside `ClosureReprs`.
            FunVerdicts = Map.empty
            // The per-binding frozen typar bounds, filled by
            // `recordGenericFnScheme` during `elaborate` (above) at the index-minting
            // point — snapshot here, not in the Pipeline, because the indices are
            // minted in this pass. Read by the call-site phantom-typar solve.
            GenericFnSchemes =
                ctx.GenericFnSchemes.AsDictionary()
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
        }
