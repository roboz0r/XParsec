namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionTypeRegistration
open UnificationTranslate

// What the `.fsi` front end resolves AGAINST, and the two things it must get right before any
// declaration can be published: which typars are in scope over a signature, and which AXIS
// each one freezes onto.

module SignatureResolutionContext =

    /// What one `.fsi` is resolved against, beside the provider its `PassContext` carries.
    [<NoEquality; NoComparison>]
    type SignatureInputs =
        {
            /// The assembly this signature's published keys are homed in; `""` for a
            /// front-end-only run that emits nothing.
            Assembly: string
            /// The target being compiled, named on an `extern` this target binds no repr for.
            Target: string
            /// Short type name -> the platform repr the paired implementation binds. A
            /// signature's `extern` declares that the platform supplies the type and never
            /// how it spells it, so the answer comes from the `.fs`.
            Reprs: IReadOnlyDictionary<string, string>
        }

    [<NoEquality; NoComparison>]
    type SigCtx =
        {
            Pass: PassContext
            Surface: PublishedSurfaceBuilder
            Inputs: SignatureInputs
        }

    // --- freezing -------------------------------------------------------------------

    /// A `SemType` the registry resolved, cut to the template a consumer instantiates. A
    /// `TyVar` that no typar env names is a hole the signature left open and no consumer can
    /// fill, so it degrades to an unresolved-typar marker rather than faulting the file.
    let freezeOver (ctx: PassContext) (env: (TyVarId * SemType) list) (ty: SemType) : FrozenType =
        FrozenTypeBridge.toFrozenWith
            (fun _ -> FTUnknown UnknownReason.UnresolvedTypar)
            (ElaborateTypars.remapDeclTypars ctx.Store env ty)

    /// The declaring-axis env of a type's own typars: `'T` written anywhere in its structure
    /// freezes to `FTTypar(Declaring, i)` at its declared position.
    let private declaringEnv (ctx: PassContext) (typeParams: EqArray<string * TyVarId>) =
        ElaborateTypars.mkDeclTyparEnv ctx.Store typeParams

    /// The method-axis env of a signature's own typars, `i` its declared position.
    let private methodEnv (ctx: PassContext) (typeParams: EqArray<string * TyVarId>) : (TyVarId * SemType) list =
        [
            for i in 0 .. typeParams.Length - 1 do
                let (_, ptv) = typeParams.[i]

                match Unification.zonk ctx.Store (TyVar ptv) with
                | TyVar root -> yield (root, TyTypar(TyparAxis.Method, i))
                | _ -> ()
        ]

    /// WHAT is being frozen, which is what decides the AXIS each typar lands on.
    [<RequireQualifiedAccess>]
    type TyparOwner =
        /// A TYPE and everything its declared structure writes: fields, cases, base,
        /// interfaces, an abbreviation's body.
        | Type of typars: EqArray<string * TyVarId>
        /// A MEMBER: its declaring type already owns the declaring axis, so the member's own
        /// typars take the method axis.
        | Member of declaring: EqArray<string * TyVarId> * own: EqArray<string * TyVarId>
        /// A VALUE quantifies its own typars on the DECLARING axis: it has no declaring type
        /// for the other axis to belong to, and instantiating a symbol substitutes there.
        | Value of typars: EqArray<string * TyVarId>

    let typarEnv (ctx: PassContext) (owner: TyparOwner) : (TyVarId * SemType) list =
        match owner with
        | TyparOwner.Type typars
        | TyparOwner.Value typars -> declaringEnv ctx typars
        | TyparOwner.Member(declaring, own) -> declaringEnv ctx declaring @ methodEnv ctx own

    // --- typar scopes ---------------------------------------------------------------

    /// Every `when` clause a signature writes: the header's, plus each one hanging off a
    /// `Type.WhenConstrainedType` inside it.
    let collectWhenClauses
        (header: TyparDefns<SyntaxToken> voption)
        (walk: CstTypeWalk.TypeIter -> unit)
        : TyparConstraints<SyntaxToken> list =
        let acc = ResizeArray<TyparConstraints<SyntaxToken>>()

        match header with
        | ValueSome(TyparDefns(constraints = ValueSome cs)) -> acc.Add cs
        | _ -> ()

        walk
            { CstTypeWalk.identityTypeIter with
                VisitType =
                    fun _ t ->
                        match t with
                        | Type.WhenConstrainedType(constraints = cs) -> acc.Add cs
                        | _ -> ()

                        true
            }

        List.ofSeq acc

    /// The typar names a signature writes and its enclosing scope does not declare, in the
    /// order F# quantifies them: the argument/result SHAPE first, in first-appearance order,
    /// then the ones only a `when` clause's TARGET mentions (`'E` in `'S :> IStructSeq<'T,'E>`).
    let implicitTyparNames
        (ctx: PassContext)
        (known: seq<string>)
        (header: TyparDefns<SyntaxToken> voption)
        (walk: CstTypeWalk.TypeIter -> unit)
        : string list =
        let declared = HashSet<string>(known, System.StringComparer.Ordinal)
        let seen = HashSet<string>(System.StringComparer.Ordinal)
        let acc = ResizeArray<string>()

        let addTypar (t: Typar<SyntaxToken>) =
            match typarName ctx t with
            | ValueSome n ->
                if not (declared.Contains n) && seen.Add n then
                    acc.Add n
            | ValueNone -> ()

        let collecting (descendWhen: bool) : CstTypeWalk.TypeIter =
            { CstTypeWalk.identityTypeIter with
                VisitType =
                    fun it t ->
                        match t with
                        | Type.VarType tp ->
                            addTypar tp
                            true
                        | Type.SubtypeConstraint(typar = tp) ->
                            addTypar tp
                            true
                        | Type.WhenConstrainedType(typ = inner) when not descendWhen ->
                            CstTypeWalk.iterType it inner
                            false
                        | _ -> true
            }

        walk (collecting false)

        for clause in collectWhenClauses header walk do
            CstTypeWalk.iterTypeConstraints (collecting true) clause

        List.ofSeq acc

    /// The explicit `<'a, 'b>` a signature declares, in source order.
    let explicitTyparNames (ctx: PassContext) (tds: TyparDefns<SyntaxToken> voption) : string list =
        match tds with
        | ValueNone -> []
        | ValueSome(TyparDefns(defns = ds)) ->
            [
                for TyparDefn(typar = t) in ds do
                    match typarName ctx t with
                    | ValueSome n -> yield n
                    | ValueNone -> ()
            ]

    /// Run `f` under a typar scope holding exactly `outer` then `own`, STRICT: every typar the
    /// signature writes was collected before entry, so one that still misses is undeclared.
    let underTypars
        (ctx: PassContext)
        (outer: EqArray<string * TyVarId>)
        (own: EqArray<string * TyVarId>)
        (f: unit -> 'a)
        : 'a =
        let savedScope = ctx.Resolution.TyparScope
        let savedStrict = ctx.Resolution.TyparScopeStrict
        let scope = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

        for (n, tv) in outer do
            scope.[n] <- tv

        // The signature's own `<'a>` shadows an enclosing typar of the same name.
        for (n, tv) in own do
            scope.[n] <- tv

        ctx.Resolution.TyparScope <- scope
        ctx.Resolution.TyparScopeStrict <- true

        try
            f ()
        finally
            ctx.Resolution.TyparScope <- savedScope
            ctx.Resolution.TyparScopeStrict <- savedStrict

    // --- curried signatures ---------------------------------------------------------

    /// One `->`-separated group's .NET-tupled domain, and the return type. `a * b -> r` is one
    /// group of width 2; `(a * b) -> r` is one group of width 1.
    let translateSigGroups (ctx: PassContext) (CurriedSig(args = argGroups; returnType = ret)) =
        let domains =
            [
                for struct (ArgsSpec.ArgsSpec(args = specs), _) in argGroups ->
                    EqArray.ofSeq (seq { for ArgSpec(typ = t) in specs -> translateType ctx t })
            ]

        domains, translateType ctx ret

    /// The whole signature as one function type, which is what a VALUE's scheme is.
    let curriedFunTy (domains: EqArray<SemType> list) (ret: SemType) : SemType =
        let tupled (d: EqArray<SemType>) =
            match d.Length with
            | 1 -> d.[0]
            | _ -> TyTuple d

        List.foldBack (fun d acc -> TyFun(tupled d, acc)) domains ret

    let freezeDomains (ctx: PassContext) env (domains: EqArray<SemType> list) : FrozenType list =
        domains
        |> List.map (fun d -> ExternalSignature.tupledParams (d |> EqArray.map (freezeOver ctx env)))

    // --- `when` clauses --------------------------------------------------------------

    /// The published form of a signature's `when` clauses, over its own typars by INDEX. An
    /// entry referencing a typar the signature does not declare is dropped: a consumer
    /// instantiates by index and has nothing to attach it to.
    let publishedConstraints
        (ctx: PassContext)
        (env: (TyVarId * SemType) list)
        (typeParams: EqArray<string * TyVarId>)
        (clauses: TyparConstraints<SyntaxToken> list)
        : ExternalConstraint list =
        let indexOf (t: Typar<SyntaxToken>) : int voption =
            match typarName ctx t with
            | ValueNone -> ValueNone
            | ValueSome n -> typeParams |> EqArray.tryFindIndex (fun (name, _) -> name = n)

        let target (t: Type<SyntaxToken>) : FrozenType =
            freezeOver ctx env (translateType ctx t)

        let trait' (t: Typar<SyntaxToken>) (kind: SemanticConstraintKind) =
            match indexOf t with
            | ValueSome i -> [ ExternalConstraint.Trait(i, kind) ]
            | ValueNone -> []

        [
            for clause in clauses do
                for c in clause.Constraints do
                    match c with
                    | Constraint.Equality(typar = t) -> yield! trait' t SemanticConstraintKind.Equality
                    | Constraint.Comparison(typar = t) -> yield! trait' t SemanticConstraintKind.Comparison
                    | Constraint.Struct(typar = t) -> yield! trait' t SemanticConstraintKind.Struct
                    | Constraint.ReferenceType(typar = t) -> yield! trait' t SemanticConstraintKind.ReferenceType
                    | Constraint.Nullness(typar = t) -> yield! trait' t SemanticConstraintKind.Nullness
                    | Constraint.NotNull(typar = t) -> yield! trait' t SemanticConstraintKind.NotNull
                    | Constraint.Coercion(typar = t; typ = tgt) ->
                        match indexOf t with
                        | ValueSome i -> yield ExternalConstraint.Coercion(i, target tgt)
                        | ValueNone -> ()
                    | Constraint.Default(typar = t; typ = tgt) ->
                        match indexOf t with
                        | ValueSome i -> yield ExternalConstraint.Default(i, target tgt)
                        | ValueNone -> ()
                    | Constraint.MemberTrait(staticTypars = sts; membersign = ms) ->
                        let indices =
                            EqArray.ofList
                                [
                                    match sts with
                                    | StaticTypars.Single t ->
                                        match indexOf t with
                                        | ValueSome i -> yield i
                                        | ValueNone -> ()
                                    | StaticTypars.OrList(typars = ts) ->
                                        for t in ts do
                                            match indexOf t with
                                            | ValueSome i -> yield i
                                            | ValueNone -> ()
                                ]

                        let ident, CurriedSig(args = argGroups; returnType = retTy) =
                            match ms with
                            | MemberSig.MethodOrPropSig(ident = i; sign = s)
                            | MemberSig.PropSig(ident = i; sign = s) -> i, s

                        match OperatorNames.ofDeclaredName ctx.NameOf ident with
                        | ValueSome memberName when not indices.IsEmpty ->
                            // A trait signature is tupled by convention (`^T * ^T -> ^T`),
                            // parsing as one group of N args; flatten it to the arg list.
                            let argFts =
                                EqArray.ofList
                                    [
                                        for struct (ArgsSpec.ArgsSpec(args = specs), _) in argGroups do
                                            for ArgSpec(typ = t) in specs -> target t
                                    ]

                            yield ExternalConstraint.MemberTrait(indices, memberName, argFts, target retTy)
                        | _ -> ()
                    // Each has its own resolution phase, or no published form.
                    | Constraint.DefaultConstructor _
                    | Constraint.Enum _
                    | Constraint.Unmanaged _
                    | Constraint.Delegate _ -> ()
        ]
