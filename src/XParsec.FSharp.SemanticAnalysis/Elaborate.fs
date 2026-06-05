namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeExpr

// Type-declaration surfacing + the top-level `Elaborate.run` entry point: CST →
// `TastFileG<SemType>`, inline-expanded, open typars quantified to `TyTypar` —
// all still `SemType`. This is NOT the `SemType → FrozenType` freeze (that is the
// `Freeze` module, the final pipeline step); renamed from `Freeze` to
// retire that naming bug. The expression / pattern
// projection lives in FreezeExpr (opened above).
//
// Invariant: side tables can be discarded after this returns. The TAST is
// sharable; the CST + side tables are scoped to one compilation.

module Elaborate =
    // A declaring-type typar becomes a `TyConst "'A"` marker the backend's
    // typar encoder maps to a generic-parameter index.

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
            | TyConst(n, args) -> TyConst(n, EqArray.map go args)
            | TyFun(a, b) -> TyFun(go a, go b)
            | TyTuple ts -> TyTuple(EqArray.map go ts)
            | TyRecord(n, args) -> TyRecord(n, EqArray.map go args)
            | TyUnion(n, args) -> TyUnion(n, EqArray.map go args)
            | TyClass(n, args) -> TyClass(n, EqArray.map go args)
            | TyUnknown _ -> t
            // Already-frozen leaf (task #2 will make this remap produce it).
            | TyTypar _ -> t

        go (Unification.zonk t)

    /// Rewrite every `SemType` embedded in a member body via `f`. Used to push a
    /// generic union's declaring-typar remap (`remapDeclTypars`) through the whole
    /// member body, so a typar-typed local / scrutinee / bound variable carries the
    /// `TyConst "'T"` marker the backend's generic-member encoder consumes — just as
    /// the case-field types do (P3d.4 generalised to member bodies for R2).
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

    /// Quantify a module-`let`'s free type parameters into `TyTypar(Method, i)`
    /// : walk the declared (curried) type collecting
    /// each genuine free typar root in first-appearance pre-order — params left-to-
    /// right, then return — and pair it with its method-axis index. Mirrors
    /// `Inline.quantifiedTypars` / `EmitClosures.staticFnTypars`: a *linked* root
    /// (pinned to a concrete type, or a measure carrier whose `Link` points at its
    /// carrier) is followed, not collected, so measures and pinned vars stay out of
    /// the typar list. The resulting env feeds `remapDeclTypars`, exactly like the
    /// declaring-typar env in 2A. Caller restricts this to function bindings (a
    /// non-function value's free var is a value-restriction case, not a method typar).
    let private mkMethodQuantEnv (declTy: SemType) : (TypeVar * SemType) list =
        let acc = ResizeArray<TypeVar>()

        let rec go t =
            match t with
            | TyVar tv ->
                let root = UnionFind.find tv

                match root.Link with
                | ValueSome target -> go target
                | ValueNone ->
                    if not (acc |> Seq.exists (fun r -> Object.ReferenceEquals(r, root))) then
                        acc.Add root
            | TyConst(_, args)
            | TyTuple args
            | TyRecord(_, args)
            | TyUnion(_, args)
            | TyClass(_, args) ->
                for a in args do
                    go a
            | TyFun(a, b) ->
                go a
                go b
            | TyUnknown _
            | TyTypar _ -> ()

        go declTy
        [ for i in 0 .. acc.Count - 1 -> acc.[i], TyTypar(TyparAxis.Method, i) ]

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
            if m.MethodTypeParams.IsEmpty then
                []
            else
                mkMethodTyparEnv m.MethodTypeParams

        { m with ThisTy = selfTy }, methodMarkers

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
        | TTypeKind.Union(cases, members) ->
            let cases =
                cases
                |> EqArray.map (fun c ->
                    { c with
                        Fields = c.Fields |> EqArray.map (fun (n, ty) -> n, f ty)
                    }
                )

            TTypeKind.Union(cases, members |> EqArray.map (freezeMember f))
        | TTypeKind.Record(fields, members) ->
            TTypeKind.Record(fields |> EqArray.map field, members |> EqArray.map (freezeMember f))
        | TTypeKind.Class(fields,
                          ctorParams,
                          members,
                          baseType,
                          interfaces,
                          isSealed,
                          staticLets,
                          secondaryCtors,
                          baseCtorCall,
                          isStruct) ->
            let staticLet (sl: TStaticLet) =
                { sl with
                    Type = f sl.Type
                    Init = mapExprTypes f sl.Init
                }

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

            TTypeKind.Class(
                fields |> EqArray.map field,
                ctorParams |> EqArray.map field,
                members |> EqArray.map (freezeMember f),
                baseType |> ValueOption.map f,
                interfaces
                |> EqArray.map (fun (ity, ms) -> f ity, ms |> EqArray.map (freezeMember f)),
                isSealed,
                staticLets |> EqArray.map staticLet,
                secondaryCtors |> EqArray.map secondary,
                baseCtorCall |> ValueOption.map baseCtor,
                isStruct
            )

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
    /// member/field/inherit (a class or later rung) or a never-registered type.
    let private tryInterfaceMethods
        (ctx: PassContext)
        (name: string)
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
            match ctx.Types.Class.TryGetValue name with
            | false, _ -> None
            | true, info ->
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
                            for m in info.Members do
                                if m.Kind = ClassMemberKind.Method then
                                    // A generic method's own typars join the env so
                                    // the backend routes them to `GenericMethodParameter`
                                    // (declaring typars stay `GenericTypeParameter`).
                                    if not m.MethodTypeParams.IsEmpty then
                                        env.AddRange(mkMethodTyparEnv m.MethodTypeParams)

                                    yield
                                        {
                                            Name = m.Name
                                            MethodTypeParams =
                                                EqArray.ofSeq (seq { for (n, _) in m.MethodTypeParams -> n })
                                            Signature = m.Type
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
            | Pat.Typed(pat = inner) -> walk inner
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
                | TPat.NamedSimple(k, ty) -> yield (k, ty)
                | TPat.Tuple(items, _) ->
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

    /// Translate one union augmentation member element into a `TTypeMember`.
    /// Instance members reference `this` via `info.ThisKey`.
    let private translateUnionMember
        (ctx: PassContext)
        (info: UnionTypeInfo)
        (el: TypeDefnElement<SyntaxToken>)
        : TTypeMember voption =
        match el with
        | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
            let isStatic = s.IsSome

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Kind = kind
                            ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                            // Unions are not inheritable — `base` never in scope.
                            BaseKey = ValueNone
                            ThisTy = TyUnion(info.Key, EqArray.empty)
                            Params = memberParams ctx b
                            Body = translateExpr ctx b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                            // Generic methods on union augmentations are out of
                            // B-12 scope (class-only); always non-generic here.
                            MethodTypeParams = EqArray.empty
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
                        ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                        BaseKey = ValueNone
                        ThisTy = TyUnion(info.Key, EqArray.empty)
                        Params = EqArray.empty
                        Body = translateExpr ctx e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        MethodTypeParams = EqArray.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Rewrite each `static let`-bound name reference (`TExpr.Var(staticLetKey)`)
    /// in a member body or a `.cctor` initialiser to `TExpr.StaticFieldGet(class,
    /// name)` (vesper-set-sprint-plan §1.8 / B-10) — the static analogue of the
    /// primary-ctor-param → `FieldGet` rewrite. Applies to instance and static
    /// member bodies alike (a `static let` is in scope for both).
    let private rewriteStaticLetRefs (staticLetByKey: Map<NodeKey, string>) (declKey: SymbolKey) (body: TExpr) : TExpr =
        if Map.isEmpty staticLetByKey then
            body
        else
            TastWalk.mapExpr
                { TastWalk.identityMapper with
                    OverrideExpr =
                        fun _ e ->
                            match e with
                            | TExpr.Var(k, ty) ->
                                match Map.tryFind k staticLetByKey with
                                | Some name -> ValueSome(TExpr.StaticFieldGet(declKey, name, ty))
                                | None -> ValueNone
                            | _ -> ValueNone
                }
                body

    /// Translate one class member element into a `TTypeMember`. Parallel to
    /// `translateUnionMember` — only differs in the `ThisTy` shape
    /// (`TyClass(info.Name, …)` vs `TyUnion`) and in one extra rewrite step:
    /// each `TExpr.Var(ctorParamKey)` in an *instance* member body becomes
    /// `TExpr.FieldGet(this, paramName)`, so the back end resolves a primary-
    /// ctor argument through the same field-access mechanism every other
    /// nominal type uses (codegen never sees the ctor-param NodeKey). Static
    /// members don't see ctor params (front-end's `staticScope` is empty), so
    /// the rewrite is a no-op there. Phase 2 (B-4) will extend the dispatch
    /// path to consult `info.BaseType` for `base.M` resolution.
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

        let ctorParamByKey =
            info.CtorParams |> Array.map (fun p -> p.DeclKey, p.Name) |> Map.ofArray

        let staticLetByKey =
            info.StaticLets |> Array.map (fun sl -> sl.DeclKey, sl.Name) |> Map.ofArray

        let rewriteCtorParamRefs (body: TExpr) : TExpr =
            if Map.isEmpty ctorParamByKey then
                body
            else
                TastWalk.mapExpr
                    { TastWalk.identityMapper with
                        OverrideExpr =
                            fun _ e ->
                                match e with
                                | TExpr.Var(k, ty) ->
                                    match Map.tryFind k ctorParamByKey with
                                    | Some name -> ValueSome(TExpr.FieldGet(TExpr.Var(info.ThisKey, classTy), name, ty))
                                    | None -> ValueNone
                                | _ -> ValueNone
                    }
                    body

        match el with
        | TypeDefnElement.Member(MemberDefn.Member(staticToken = s; defn = d)) ->
            let isStatic = s.IsSome

            let lowerBody (e: Expr<SyntaxToken>) : TExpr =
                let body = translateExpr ctx e |> rewriteStaticLetRefs staticLetByKey info.Key
                if isStatic then body else rewriteCtorParamRefs body

            // The member's own generic parameters (B-12), recovered from the
            // registered `TypeMemberInfo`. Each prototype TyVar is zonked to the
            // union-find root the member's signature / body actually references
            // (mirrors the abstract-method path); entries that unified away to a
            // concrete type are dropped. Codegen installs these roots as ambient
            // method typars so they encode to `!!i`.
            let methodTypeParams (n: string) (kind: TMemberKind) : EqArray<string * TypeVar> =
                let kindMatches (mi: TypeMemberInfo) =
                    match mi.Kind, kind with
                    | ClassMemberKind.Method, TMemberKind.Method
                    | ClassMemberKind.Property, TMemberKind.Property -> true
                    | _ -> false

                match
                    info.Members
                    |> Array.tryFind (fun mi -> mi.Name = n && mi.IsStatic = isStatic && kindMatches mi)
                with
                | Some mi ->
                    EqArray.ofSeq (
                        seq {
                            for (nm, ptv) in mi.MethodTypeParams do
                                match Unification.zonk (TyVar ptv) with
                                | TyVar root -> yield (nm, root)
                                | _ -> ()
                        }
                    )
                | None -> EqArray.empty

            let build (kind: TMemberKind) (b: Binding<SyntaxToken>) : TTypeMember voption =
                match memberNameOfBinding ctx b with
                | ValueSome n ->
                    ValueSome
                        {
                            Name = n
                            IsStatic = isStatic
                            Kind = kind
                            ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                            BaseKey = (if isStatic then ValueNone else baseKey)
                            ThisTy = TyClass(info.Key, EqArray.empty)
                            Params = memberParams ctx b
                            Body = lowerBody b.expr
                            ReturnTy = typeOfKey ctx (CstKeys.ofExpr b.expr)
                            MethodTypeParams = methodTypeParams n kind
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
                        ThisKey = (if isStatic then ValueNone else ValueSome info.ThisKey)
                        BaseKey = (if isStatic then ValueNone else baseKey)
                        ThisTy = TyClass(info.Key, EqArray.empty)
                        Params = EqArray.empty
                        Body = lowerBody e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        // Auto-properties never carry their own generic params.
                        MethodTypeParams = EqArray.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Translate one secondary constructor (B-11) into a `TSecondaryCtor`. The
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
                | Pat.Typed(pat = inner) -> walk inner
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

        // The explicit field-init form `new(args) = { f = e; … }`
        // (structs-handoff #2): each `FieldInitializer` stores into a declared
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
            // (`declTypars` empty) keep `translateUnionMember`'s `TyUnion(key, [])`
            // self-type untouched, so the path stays byte-identical.
            let declTypars = [ for (n, _) in info.TypeParams -> n ]

            let selfTy = TyUnion(info.Key, declTyparArgs info.TypeParams)

            let members =
                match ext with
                | ValueNone -> EqArray.empty
                | ValueSome(TypeExtensionElements(elements = elems)) ->
                    EqArray.ofSeq (
                        seq {
                            for el in elems do
                                match translateUnionMember ctx info el with
                                | ValueSome m ->
                                    if List.isEmpty declTypars then
                                        yield m
                                    else
                                        let m, methodMarkers = elaborateMember selfTy m
                                        env.AddRange methodMarkers
                                        yield m
                                | ValueNone -> ()
                        }
                    )

            Some(
                mkTypeDecl
                    name
                    info.Key
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Union(cases, members))
                    info.EqualitySupport
                    info.ComparisonSupport,
                List.ofSeq env
            )

    /// Surface a `TypeDefn.Record` as a `TDecl.Type` from the resolved
    /// `RecordTypeInfo`. Field types are remapped through the declaring-type
    /// typars (a no-op for a monomorphic record — `TypeParams` empty — but the
    /// right shape for the generic record path, exactly like `tryUnionType`).
    /// Augmentation members are out of scope for v1 — the
    /// member list stays empty; the front end never registers them under a record
    /// today.
    let private tryRecordType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        : (TDecl * (TypeVar * SemType) list) option =
        match ctx.Types.Record.TryGetValue name with
        | false, _ -> None
        | true, info ->
            // Records carry no members (v1), so the decl's freeze env is just the
            // declaring typars; field types ride as `TyVar` roots until the cut.
            let markers = mkDeclTyparEnv info.TypeParams

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

            Some(
                mkTypeDecl
                    name
                    info.Key
                    ns
                    (EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }))
                    (TTypeKind.Record(fields, EqArray.empty))
                    info.EqualitySupport
                    info.ComparisonSupport,
                markers
            )

    /// Surface a `TypeDefn.Class` (or class-shaped `TypeDefn.Anon`) as a
    /// `TDecl.Type` from the resolved `ClassTypeInfo`. Ctor params and member
    /// signatures are remapped through the declaring-type typars (the same
    /// `mkTypeMarkers` + `remapDeclTypars` pipeline records / unions use).
    /// Phase 1 (B-1) leaves `fields` empty (no mutable instance fields yet) and
    /// `baseType` `ValueNone` (codegen defaults to `Object`); Phase 2 fills the
    /// base type and Phase 5 (§5.3) projects `info.InterfaceImpls` onto
    /// `interfaces`.
    let private tryClassType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (elements: TypeDefnElements<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) option =
        match ctx.Types.Class.TryGetValue name with
        | false, _ -> None
        | true, info ->
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

            // Explicit `val [mutable] x: T` instance fields (vesper-set-sprint-phase-6).
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
            // *or* the member itself is generic (method axis, B-12): stamp its
            // self-type and fold its method typars into the decl env, so
            // `freezeTypars` later flips both axes. A generic method on a
            // *monomorphic* class still needs its `'C` cut to `TyTypar(Method, i)`,
            // so it can't be skipped. For a mono type with a
            // mono member, `selfTy = TyClass(key, [])` equals the member's existing
            // `ThisTy`, so leaving it verbatim is byte-identical.
            let needsRemap (m: TTypeMember) =
                not (List.isEmpty declTypars) || not m.MethodTypeParams.IsEmpty

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

            // `static let` fields + `.cctor` initialisers (B-10). The front-end
            // rejects `static let` on a generic class, so `info.StaticLets` is
            // only ever non-empty for a monomorphic class — no typar remap needed.
            // A later static-let initialiser referencing an earlier one is rewritten
            // through `rewriteStaticLetRefs`, matching the member-body lowering.
            let staticLetByKey =
                info.StaticLets |> Array.map (fun sl -> sl.DeclKey, sl.Name) |> Map.ofArray

            let staticLets =
                EqArray.ofSeq (
                    seq {
                        for sl in info.StaticLets ->
                            {
                                Name = sl.Name
                                Type = Unification.zonk sl.Type
                                Init = translateExpr ctx sl.Init |> rewriteStaticLetRefs staticLetByKey info.Key
                            }
                    }
                )

            // Secondary constructors (B-11). Each `new(...)` overload becomes a
            // `TSecondaryCtor`; codegen emits a `.ctor` overload chaining to the
            // primary ctor. Empty unless the class declares any.
            let secondaryCtors =
                EqArray.ofSeq (seq { for sc in info.SecondaryCtors -> translateSecondaryCtor ctx sc })

            // Inheritance (B-4 Step 2.5). `baseType` is the parent's resolved
            // `TyClass`, carried with this class's declaring typars as `TyVar` roots
            // so `freezeTypars` encodes a generic parent (`SetTree\`1<!0>`) against
            // this class's own generic parameters; codegen reads it for the IL
            // `TypeDefinition.BaseType`. `baseCtorCall` carries the `inherit
            // Base(args)` invocation: the derived class's primary-ctor params (the
            // `ldarg` mapping the args reference, since `this` isn't constructed yet)
            // and the translated arg expressions.
            let baseType = info.BaseType

            // Interface implementations (B-2, vesper-set-sprint-phase-5 §5.3).
            // Each registered `interface IFace with member …` block becomes an
            // `(ifaceTy, members)` entry: the resolved interface `TyClass` (carrying
            // this class's declaring typars as roots so a generic arg like
            // `IEnumerable<'T>` encodes against this class's typars after the cut)
            // paired with its already-typed member bodies. The bodies translate
            // through the *class* `info` exactly like the class's own members —
            // `this` and ctor-param references rewrite identically — but read their
            // elements from the impl's own `Elements`. Impls whose interface failed
            // to resolve (`Resolved = ValueNone`, the §5.1 diagnostic already fired)
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

                    let args = peelOneArg (translateExpr ctx) argExpr

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
                    (TTypeKind.Class(
                        instanceFields,
                        ctorParams,
                        members,
                        baseType,
                        interfaces,
                        info.IsSealed,
                        staticLets,
                        secondaryCtors,
                        baseCtorCall,
                        info.IsValueType
                    ))
                    // Classes are reference-equal by default ([[project_c_attr_pr_a]]);
                    // [<CustomEquality>] / [<NoEquality>] lift this in a later sprint.
                    EqualityVerdict.Reference
                    ComparisonVerdict.NoComparison,
                List.ofSeq env
            )

    /// Surface an interface-shaped, union, record, or class `TypeDefn` as a
    /// `TDecl.Type`. Abbreviations surface nothing.
    let private tryTypeDecl
        (ctx: PassContext)
        (ns: string option)
        (td: TypeDefn<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) option =
        let classify tn (body: ObjectModelBody<SyntaxToken>) =
            let name = typeNameSimple ctx tn

            match tryInterfaceMethods ctx name body with
            | Some(typars, methods, env) ->
                // Interfaces aren't in the codegen emitted-type tables (their own
                // `interfaceDecls` path), but `TTypeDecl.Key` is total — mint the
                // same `(asm, ns, name\`arity)` identity registration would, so a
                // reference to the interface compares equal to this decl's key.
                let key =
                    LocalSymbolKey.ofType (SymbolKeyOps.asmOf ctx.AssemblyName) (defaultArg ns "") name typars.Length

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
            | None -> tryClassType ctx ns name body.elements

        match td with
        | TypeDefn.Anon(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Interface(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Class(typeName = tn; body = body) -> tryClassType ctx ns (typeNameSimple ctx tn) body.elements
        | TypeDefn.Union(typeName = tn; extensions = ext) ->
            tryUnionType ctx ns (typeNameSimple ctx tn) (typeNameDeclKey ctx tn) ext
        | TypeDefn.Record(typeName = tn) -> tryRecordType ctx ns (typeNameSimple ctx tn)
        | _ -> None

    let private longIdentText (ctx: PassContext) (li: LongIdent<SyntaxToken>) : string =
        li.Idents |> Seq.map ctx.NameOf |> String.concat "."

    /// `holder` is the enclosing named module's compiled holder-type name (R3
    /// deferred): `Some` for elements inside a `module Foo = …`, `None` at the
    /// namespace/file top level. A `let` binding under a holder records its
    /// `NodeKey` → `ModuleMemberInfo` so the backend emits it as a named public
    /// static method on that holder (e.g. `ListModule::fold`) rather than on the
    /// anonymous "Program" holder.
    let rec private translateModuleElem
        (ctx: PassContext)
        (ns: string option)
        (holder: string option)
        (m: ModuleElem<SyntaxToken>)
        : (TDecl * (TypeVar * SemType) list) list =
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            [
                for b in bindings ->
                    let tpat = translatePat ctx b.headPat

                    // Inside a named module: record where this binding's static
                    // method belongs (its source name on the holder type).
                    match holder with
                    | Some h ->
                        match memberNameOfBinding ctx b with
                        | ValueSome nm ->
                            ctx.Bindings.ModuleMembers.[(CstKeys.ofBinding b).Raw] <-
                                {
                                    Namespace = ns
                                    Holder = h
                                    Name = nm
                                }
                        | ValueNone -> ()
                    | None -> ()

                    let valT = translateBinding ctx b
                    let declTy = typeOfKey ctx (CstKeys.ofBinding b)

                    // A module-`let` compiled as a generic
                    // static method (or generic closure) carries its free typars as
                    // `TyTypar(Method, i)`. The index order is minted once here
                    // (Edge A order) as `quantEnv`, but the cut itself is deferred to
                    // `freezeTypars` — `elaborate` leaves the head pattern, value
                    // body, and declared type in `TyVar` form and just pairs the decl
                    // with its `quantEnv`. Restricted to *function* bindings (a
                    // `TyFun` declared type): a non-function value's free var is a
                    // value-restriction case, not a method typar (`let n = null` stays
                    // `TyVar`). Inline bindings are exempt — their bodies are expanded
                    // + substituted to concrete types at each call site, never emitted
                    // as a generic method, so `quantEnv` is empty and they keep the
                    // `TyVar` representation.
                    let quantEnv =
                        if b.inlineToken.IsSome then
                            []
                        else
                            match Unification.zonk declTy with
                            | TyFun _ -> mkMethodQuantEnv declTy
                            | _ -> []

                    TDecl.Let(tpat, valT, b.inlineToken.IsSome, declTy), quantEnv
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)), [] ]
        | ModuleElem.Type defs -> defs |> Seq.choose (tryTypeDecl ctx ns) |> List.ofSeq
        // A nested `module Foo = …` surfaces its body flat at the enclosing
        // namespace (v1 has no module-scoped *types*), mirroring the analysis
        // passes' `CstWalk.implFileElems` flattening — but its *functions* carry
        // the holder name `Foo`, suffixed `FooModule` when a type of the same name
        // shares the namespace (the F# rule that mandates
        // `[<CompilationRepresentation(ModuleSuffix)>]`), so they emit onto a real
        // holder type. Deeper nesting takes the innermost module's name.
        | ModuleElem.Module(ModuleDefn.ModuleDefn(
            attributes = attrs; ident = ident; body = ModuleDefnBody(elements = inner))) ->
            match inner with
            | ValueSome innerElems ->
                let moduleName = ctx.NameOf ident

                let holderName =
                    // The `…Module` suffix the compiled holder takes when it would
                    // otherwise clash with a same-named type — either a *project*
                    // type in this namespace, or one the author pinned with
                    // `[<CompilationRepresentation(ModuleSuffix)>]` (e.g.
                    // `Vesper.Array`'s `Array` module over the intrinsic `'T[]`,
                    // which has no project type to collide with but must still
                    // compile to `ArrayModule` to match its contract + FSharp.Core).
                    if
                        ctx.Types.Union.ContainsKey moduleName
                        || ctx.Types.Record.ContainsKey moduleName
                        || ctx.Types.Class.ContainsKey moduleName
                        || VesperLibTypeTranslate.hasModuleSuffix ctx.Lexed ctx.Input attrs
                    then
                        moduleName + "Module"
                    else
                        moduleName

                innerElems
                |> Seq.collect (translateModuleElem ctx ns (Some holderName))
                |> List.ofSeq
            | ValueNone -> []
        | _ -> []

    /// The first half of the split Freeze pass: translate
    /// the CST to a `TExpr` tree whose `.ty` fields are zonk'd `SemType`, still
    /// `TyVar`-carrying (no `TyTypar`). Each decl is paired with the typar `env`
    /// it quantifies — the declaring / method / static-fn typar roots, collected at
    /// this single index-minting point. `freezeTypars` consumes that `env` to make
    /// the `TyVar → TyTypar` cut. (Step 3A-1 will slot the inline-expansion pass
    /// between `elaborate` and the freeze cut, where `zonk` / union-find are native;
    /// today nothing runs between them and the output is byte-identical to the old
    /// fused pass.)
    let elaborate (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : (TDecl * (TypeVar * SemType) list) list =
        match file with
        | ImplementationFile.AnonymousModule elems ->
            elems |> Seq.collect (translateModuleElem ctx None None) |> List.ofSeq
        | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) ->
            elems |> Seq.collect (translateModuleElem ctx None None) |> List.ofSeq
        | ImplementationFile.Namespaces groups ->
            [
                for g in groups do
                    let nsName, elems =
                        match g with
                        | NamespaceDeclGroup.Named(longIdent = li; elements = elems) ->
                            Some(longIdentText ctx li), elems
                        | NamespaceDeclGroup.Global(elements = elems) -> None, elems

                    yield! elems |> Seq.collect (translateModuleElem ctx nsName None)
            ]

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        // Split pass: `elaborate` produces the
        // `TyVar`-carrying tree + per-decl typar envs; the `InlineExpansion` pass
        // then expands module-level inline call sites *before* the cut (where
        // `zonk` / union-find are native); `freezeTypars` makes the
        // `TyVar → TyTypar` cut on each.
        //
        // Cross-package inline bodies ride `ctx.Provider` directly: its
        // `TryLookupInlineBody` / `…ByName` members (keyed by the resolved
        // `SymbolKey`) are part of `IExternalSymbolProvider`, served by the
        // contract-stack wrapper `SymbolProviders.buildContract` builds. No cast.
        let decls =
            elaborate ctx file
            |> InlineExpansion.run ctx.Provider
            |> List.map (fun (d, env) -> freezeTypars env d)

        {
            Decls = EqArray.ofList decls
            Diagnostics = List.ofSeq ctx.Diagnostics
            // Snapshot so the backend can key the emitted IL type off the
            // representation string (G7) without the PassContext.
            IntrinsicReprTypes =
                ctx.Types.IntrinsicReprTypes
                |> Seq.map (fun kv -> kv.Key, kv.Value)
                |> Map.ofSeq
            // Snapshot the named-module placements (R3 deferred): the backend keys
            // off a binding's `NodeKey.Raw` to emit it on its holder type.
            ModuleMembers = ctx.Bindings.ModuleMembers |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
        }
