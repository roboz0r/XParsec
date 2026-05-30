namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.FreezeExpr

// Type-declaration surfacing + the top-level Freeze.run entry point. The
// expression / pattern projection lives in FreezeExpr (opened above).
//
// Invariant: side tables can be discarded after this returns. The TAST is
// sharable; the CST + side tables are scoped to one compilation.

module Freeze =
    // A declaring-type typar becomes a `TyConst "'A"` marker the backend's
    // typar encoder maps to a generic-parameter index.

    let private typeNameSimple (ctx: PassContext) (tn: TypeName<SyntaxToken>) : string =
        let (TypeName(ident = li)) = tn

        if li.Idents.IsEmpty then
            ""
        else
            ctx.NameOf li.Idents.[li.Idents.Length - 1]

    /// Rewrite declaring-type typars (free `TyVar`s, by zonked root) to the
    /// `TyConst "'A"` markers the backend's typar encoder consumes. Anything else
    /// passes through unchanged — a leftover inference var stays a `TyVar`, which
    /// the backend rejects loudly.
    let private remapDeclTypars (markers: (TypeVar * string) list) (t: SemType) : SemType =
        let rec go t =
            match t with
            | TyVar tv ->
                match
                    markers
                    |> List.tryPick (fun (r, n) -> if Object.ReferenceEquals(r, tv) then Some n else None)
                with
                | Some n -> TyConst n
                | None -> t
            | TyConst _ -> t
            | TyFun(a, b) -> TyFun(go a, go b)
            | TyTuple ts -> TyTuple(EqArray.map go ts)
            | TyRecord(n, args) -> TyRecord(n, EqArray.map go args)
            | TyUnion(n, args) -> TyUnion(n, EqArray.map go args)
            | TyClass(n, args) -> TyClass(n, EqArray.map go args)

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

    /// Pair each declared typar's *zonked* root TyVar with its marker name, so
    /// `remapDeclTypars` can rewrite free occurrences back to `TyConst "'A"`.
    /// Pinned typars (anything that's already collapsed to a non-`TyVar`) are
    /// dropped — there's nothing left to remap. Shared by every `try*Type`
    /// surfacer and the interface / abstract-method projections.
    let private mkTypeMarkers (typeParams: EqArray<string * TypeVar>) : (TypeVar * string) list =
        [
            for (n, ptv) in typeParams do
                match Unification.zonk (TyVar ptv) with
                | TyVar root -> yield (root, n)
                | _ -> ()
        ]

    /// Push a declaring-typar remap through a member's signature + body, retyping
    /// `this` to `selfTy`. Shared by the union / class member surfacers, which
    /// differ only in the `ThisTy` constructor (`TyUnion` vs `TyClass`).
    let private remapMemberTypes (selfTy: SemType) (markers: (TypeVar * string) list) (m: TTypeMember) : TTypeMember =
        let f = remapDeclTypars markers

        { m with
            ThisTy = selfTy
            Params = m.Params |> EqArray.map (fun (k, ty) -> k, f ty)
            Body = mapExprTypes f m.Body
            ReturnTy = f m.ReturnTy
        }

    /// Remap every embedded type in a member / ctor body through the declaring-type
    /// typars, or pass it through untouched when there are no markers (a monomorphic
    /// type — the body's types are already correct).
    let private remapBodyTypes (markers: (TypeVar * string) list) (e: TExpr) : TExpr =
        if List.isEmpty markers then
            e
        else
            mapExprTypes (remapDeclTypars markers) e

    /// Classify an object-model body as an interface — every element an abstract
    /// method signature, no base type, no `let`/`do` preamble — and build its
    /// methods from the *resolved* member signatures in `ctx.Types.Class` (an
    /// `Anon`/`Interface` registers as a class). None for a concrete
    /// member/field/inherit (a class or later rung) or a never-registered type.
    let private tryInterfaceMethods
        (ctx: PassContext)
        (name: string)
        (body: ObjectModelBody<SyntaxToken>)
        : (EqArray<string> * EqArray<TAbstractMethod>) option =
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
                let markers = mkTypeMarkers info.TypeParams

                let methods =
                    EqArray.ofSeq (
                        seq {
                            for m in info.Members do
                                if m.Kind = ClassMemberKind.Method then
                                    // A generic method's own typars get markers too so
                                    // the backend routes them to `GenericMethodParameter`
                                    // (declaring typars stay `GenericTypeParameter`); the
                                    // `TyConst "name"` picks the table.
                                    let methodMarkers = markers @ mkTypeMarkers m.MethodTypeParams

                                    yield
                                        {
                                            Name = m.Name
                                            MethodTypeParams =
                                                EqArray.ofSeq (seq { for (n, _) in m.MethodTypeParams -> n })
                                            Signature = remapDeclTypars methodMarkers m.Type
                                        }
                        }
                    )

                Some(EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }), methods)

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
    /// so a `Var` reference in the body resolves to it. Only simple parameters (a
    /// single ident per arg group) are surfaced (v1).
    let private memberParams (ctx: PassContext) (b: Binding<SyntaxToken>) : EqArray<NodeKey * SemType> =
        EqArray.ofSeq (
            seq {
                for p in b.argumentPats do
                    match translatePat ctx p with
                    | TPat.NamedSimple(k, ty) -> yield (k, ty)
                    | _ -> ()
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
                            ThisTy = TyUnion(info.Name, EqArray.empty)
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
                        ThisTy = TyUnion(info.Name, EqArray.empty)
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
    let private rewriteStaticLetRefs (staticLetByKey: Map<NodeKey, string>) (className: string) (body: TExpr) : TExpr =
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
                                | Some name -> ValueSome(TExpr.StaticFieldGet(className, name, ty))
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
        // typar list for a monomorphic class; the `tryClassType` remap leaves
        // the markers in place (it rewrites prototype `TyVar` roots, not
        // `TyConst` markers — see `remapDeclTypars`).
        let classTy =
            TyClass(info.Name, EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> TyConst n }))

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
                let body = translateExpr ctx e |> rewriteStaticLetRefs staticLetByKey info.Name
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
                            ThisTy = TyClass(info.Name, EqArray.empty)
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
                        ThisTy = TyClass(info.Name, EqArray.empty)
                        Params = EqArray.empty
                        Body = lowerBody e
                        ReturnTy = typeOfKey ctx (CstKeys.ofExpr e)
                        // Auto-properties never carry their own generic params.
                        MethodTypeParams = EqArray.empty
                    }
            | _ -> ValueNone
        | _ -> ValueNone

    /// Translate one secondary constructor (B-11) into a `TSecondaryCtor`. The
    /// params carry the declaring-type typar markers (like the primary ctor's
    /// params); each `let`-preamble binding becomes a `TCtorLet`; the final chain
    /// call's arguments become `PrimaryArgs`. Generic-class bodies are remapped
    /// through `markers` exactly like instance members. v1 supports a `let`
    /// preamble followed by the chain call; sequencing / conditional preambles
    /// recurse to the chain and drop intervening statements.
    let private translateSecondaryCtor
        (ctx: PassContext)
        (markers: (TypeVar * string) list)
        (sc: ClassSecondaryCtorInfo)
        : TSecondaryCtor =
        let remapTy = remapDeclTypars markers
        let remapBody = remapBodyTypes markers

        let parms =
            EqArray.ofSeq (seq { for p in sc.Params -> (p.DeclKey, remapTy (Unification.zonk p.Type)) })

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

            raw |> EqArray.map remapBody

        let lets = ResizeArray<TCtorLet>()
        let mutable primaryArgs = EqArray.empty

        let rec go (ace: AdditionalConstrExpr<SyntaxToken>) =
            match ace with
            | AdditionalConstrExpr.LetIn(binding = b; body = body) ->
                match binderKeyOf b with
                | ValueSome k ->
                    lets.Add
                        {
                            Binder = k
                            Type = remapTy (typeOfKey ctx k)
                            Init = translateExpr ctx b.expr |> remapBody
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
                | AdditionalConstrInitExpr.Explicit _ -> ()

        go sc.Body

        {
            Params = parms
            Lets = EqArray.ofSeq lets
            PrimaryArgs = primaryArgs
        }

    /// Build the `TDecl.Type` wrapper shared by record / union / interface
    /// (and the upcoming class) surfacers — same five-field shape, only `Kind`
    /// differs. `typars` is the already-projected typar-name list (`info` /
    /// `tryInterfaceMethods` projections both flow through here unchanged).
    let private mkTypeDecl
        (name: string)
        (ns: string option)
        (typars: EqArray<string>)
        (kind: TTypeKind)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : TDecl =
        TDecl.Type
            {
                Name = name
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
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : TDecl option =
        match ctx.Types.Union.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers = mkTypeMarkers info.TypeParams

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

                                            nm, remapDeclTypars markers c.Fields.[i]
                                    }
                                )

                            { Name = c.Name; Fields = fields }
                    }
                )

            // A generic union's members must carry the declaring-typar markers the
            // backend's generic-member encoder consumes (`!0`), exactly like the
            // case fields above: remap the member signature (`ThisTy` / `Params` /
            // `ReturnTy`) *and* the body's embedded types. Monomorphic unions
            // (`markers` empty) keep the bodies untouched — `translateUnionMember`'s
            // `TyUnion(name, [])` is already correct, so the path stays byte-identical.
            let declTypars = [ for (n, _) in info.TypeParams -> n ]

            let remapMember =
                let selfTy =
                    TyUnion(info.Name, EqArray.ofSeq (seq { for n in declTypars -> TyConst n }))

                remapMemberTypes selfTy markers

            let members =
                match ext with
                | ValueNone -> EqArray.empty
                | ValueSome(TypeExtensionElements(elements = elems)) ->
                    EqArray.ofSeq (
                        seq {
                            for el in elems do
                                match translateUnionMember ctx info el with
                                | ValueSome m -> yield (if List.isEmpty declTypars then m else remapMember m)
                                | ValueNone -> ()
                        }
                    )

            Some(
                mkTypeDecl
                    name
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Union(cases, members))
                    info.EqualitySupport
                    info.ComparisonSupport
            )

    /// Surface a `TypeDefn.Record` as a `TDecl.Type` from the resolved
    /// `RecordTypeInfo`. Field types are remapped through the declaring-type
    /// typars (a no-op for a monomorphic record — `TypeParams` empty — but the
    /// right shape for the generic record path, exactly like `tryUnionType`).
    /// Augmentation members are out of scope for v1 (records-plan §B1) — the
    /// member list stays empty; the front end never registers them under a record
    /// today.
    let private tryRecordType (ctx: PassContext) (ns: string option) (name: string) : TDecl option =
        match ctx.Types.Record.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers = mkTypeMarkers info.TypeParams

            let fields =
                EqArray.ofSeq (
                    seq {
                        for f in info.Fields ->
                            {
                                Name = f.Name
                                Type = remapDeclTypars markers f.Type
                                IsMutable = f.IsMutable
                            }
                    }
                )

            Some(
                mkTypeDecl
                    name
                    ns
                    (EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }))
                    (TTypeKind.Record(fields, EqArray.empty))
                    info.EqualitySupport
                    info.ComparisonSupport
            )

    /// Surface a `TypeDefn.Class` (or class-shaped `TypeDefn.Anon`) as a
    /// `TDecl.Type` from the resolved `ClassTypeInfo`. Ctor params and member
    /// signatures are remapped through the declaring-type typars (the same
    /// `mkTypeMarkers` + `remapDeclTypars` pipeline records / unions use).
    /// Phase 1 (B-1) leaves `fields` empty (no mutable instance fields yet),
    /// `baseType` `ValueNone` (codegen defaults to `Object`), and `interfaces`
    /// empty — Phases 2 and 5 fill those slots.
    let private tryClassType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (elements: TypeDefnElements<SyntaxToken>)
        : TDecl option =
        match ctx.Types.Class.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers = mkTypeMarkers info.TypeParams

            let ctorParams =
                EqArray.ofSeq (
                    seq {
                        for p in info.CtorParams ->
                            {
                                Name = p.Name
                                Type = remapDeclTypars markers p.Type
                                IsMutable = false
                            }
                    }
                )

            let declTypars = [ for (n, _) in info.TypeParams -> n ]

            let remapMember =
                let selfTy =
                    TyClass(info.Name, EqArray.ofSeq (seq { for n in declTypars -> TyConst n }))

                remapMemberTypes selfTy markers

            let members =
                EqArray.ofSeq (
                    seq {
                        for el in elements do
                            match translateClassMember ctx info el with
                            | ValueSome m -> yield (if List.isEmpty declTypars then m else remapMember m)
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
                                Init = translateExpr ctx sl.Init |> rewriteStaticLetRefs staticLetByKey info.Name
                            }
                    }
                )

            // Secondary constructors (B-11). Each `new(...)` overload becomes a
            // `TSecondaryCtor`; codegen emits a `.ctor` overload chaining to the
            // primary ctor. Empty unless the class declares any.
            let secondaryCtors =
                EqArray.ofSeq (seq { for sc in info.SecondaryCtors -> translateSecondaryCtor ctx markers sc })

            // Inheritance (B-4 Step 2.5). `baseType` is the parent's resolved
            // `TyClass`, remapped onto the declaring-type typar markers so codegen
            // encodes a generic parent (`SetTree\`1<!0>`) against this class's own
            // generic parameters; codegen reads it for the IL `TypeDefinition.BaseType`.
            // `baseCtorCall` carries the `inherit Base(args)` invocation: the derived
            // class's primary-ctor params (the `ldarg` mapping the args reference,
            // since `this` isn't constructed yet) and the translated arg expressions.
            let baseType = info.BaseType |> ValueOption.map (remapDeclTypars markers)

            let baseCtorCall =
                match info.BaseType, info.BaseCtorArgs with
                | ValueSome _, ValueSome argExpr ->
                    let remapBody = remapBodyTypes markers

                    let ctorParamKeys =
                        EqArray.ofSeq (
                            seq {
                                for p in info.CtorParams ->
                                    (p.DeclKey, remapDeclTypars markers (Unification.zonk p.Type))
                            }
                        )

                    let args = peelOneArg (translateExpr ctx) argExpr |> EqArray.map remapBody

                    ValueSome
                        {
                            CtorParams = ctorParamKeys
                            Args = args
                        }
                | _ -> ValueNone

            Some(
                mkTypeDecl
                    name
                    ns
                    (EqArray.ofList declTypars)
                    (TTypeKind.Class(
                        EqArray.empty,
                        ctorParams,
                        members,
                        baseType,
                        EqArray.empty,
                        info.IsSealed,
                        staticLets,
                        secondaryCtors,
                        baseCtorCall
                    ))
                    // Classes are reference-equal by default ([[project_c_attr_pr_a]]);
                    // [<CustomEquality>] / [<NoEquality>] lift this in a later sprint.
                    EqualityVerdict.Reference
                    ComparisonVerdict.NoComparison
            )

    /// Surface an interface-shaped, union, record, or class `TypeDefn` as a
    /// `TDecl.Type`. Abbreviations surface nothing.
    let private tryTypeDecl (ctx: PassContext) (ns: string option) (td: TypeDefn<SyntaxToken>) : TDecl option =
        let classify tn (body: ObjectModelBody<SyntaxToken>) =
            let name = typeNameSimple ctx tn

            match tryInterfaceMethods ctx name body with
            | Some(typars, methods) ->
                Some(
                    mkTypeDecl
                        name
                        ns
                        typars
                        (TTypeKind.Interface methods)
                        // Interfaces never synthesise an equality triple or
                        // comparison pair — the verdict fields are filled to
                        // keep the record shape total and the values are
                        // unread for this kind.
                        EqualityVerdict.Structural
                        ComparisonVerdict.NoComparison
                )
            // Not all-abstract ⇒ class shape (`type C(x) = member …`).
            | None -> tryClassType ctx ns name body.elements

        match td with
        | TypeDefn.Anon(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Interface(typeName = tn; body = body) -> classify tn body
        | TypeDefn.Class(typeName = tn; body = body) -> tryClassType ctx ns (typeNameSimple ctx tn) body.elements
        | TypeDefn.Union(typeName = tn; extensions = ext) -> tryUnionType ctx ns (typeNameSimple ctx tn) ext
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
        : TDecl list =
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
                    TDecl.Let(tpat, valT, b.inlineToken.IsSome, typeOfKey ctx (CstKeys.ofBinding b))
            ]
        | ModuleElem.Expression e ->
            let eT = translateExpr ctx e
            [ TDecl.Expression(eT, typeOfKey ctx (CstKeys.ofExpr e)) ]
        | ModuleElem.Type defs -> defs |> Seq.choose (tryTypeDecl ctx ns) |> List.ofSeq
        // A nested `module Foo = …` surfaces its body flat at the enclosing
        // namespace (v1 has no module-scoped *types*), mirroring the analysis
        // passes' `CstWalk.implFileElems` flattening — but its *functions* carry
        // the holder name `Foo`, suffixed `FooModule` when a type of the same name
        // shares the namespace (the F# rule that mandates
        // `[<CompilationRepresentation(ModuleSuffix)>]`), so they emit onto a real
        // holder type. Deeper nesting takes the innermost module's name.
        | ModuleElem.Module(ModuleDefn.ModuleDefn(ident = ident; body = ModuleDefnBody(elements = inner))) ->
            match inner with
            | ValueSome innerElems ->
                let moduleName = ctx.NameOf ident

                let holderName =
                    if
                        ctx.Types.Union.ContainsKey moduleName
                        || ctx.Types.Record.ContainsKey moduleName
                        || ctx.Types.Class.ContainsKey moduleName
                    then
                        moduleName + "Module"
                    else
                        moduleName

                innerElems
                |> Seq.collect (translateModuleElem ctx ns (Some holderName))
                |> List.ofSeq
            | ValueNone -> []
        | _ -> []

    let run (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) : TastFile =
        let decls =
            match file with
            | ImplementationFile.AnonymousModule elems -> elems |> Seq.collect (translateModuleElem ctx None None)
            | ImplementationFile.NamedModule(NamedModule.NamedModule(elements = elems)) ->
                elems |> Seq.collect (translateModuleElem ctx None None)
            | ImplementationFile.Namespaces groups ->
                seq {
                    for g in groups do
                        let nsName, elems =
                            match g with
                            | NamespaceDeclGroup.Named(longIdent = li; elements = elems) ->
                                Some(longIdentText ctx li), elems
                            | NamespaceDeclGroup.Global(elements = elems) -> None, elems

                        yield! elems |> Seq.collect (translateModuleElem ctx nsName None)
                }

        {
            Decls = EqArray.ofSeq decls
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
