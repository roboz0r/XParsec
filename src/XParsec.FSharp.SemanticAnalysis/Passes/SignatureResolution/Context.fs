namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open Vesper
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
            /// The assembly this signature's published keys are homed in.
            Assembly: AssemblyName
            /// The target being compiled, named on an `extern` this target binds no type for.
            Target: string
            /// Short type name -> the platform type id the paired implementation binds. A
            /// signature's `extern` declares that the platform supplies the type, not how it
            /// spells it, so the type id is read from the `.fs`.
            Bindings: IReadOnlyDictionary<string, PlatformTypeId>
        }

    [<NoEquality; NoComparison>]
    type SigCtx =
        {
            Pass: PassContext
            Surface: PublishedSurfaceBuilder
            Inputs: SignatureInputs
            /// Keys of the current `type … and …` group's declarations that will publish as
            /// interfaces, so a forward reference within the group resolves before its shape
            /// is published. `registerSigGroup` rebinds a fresh set per group.
            GroupInterfaceKeys: EqSet<TypeKey>
        }

    // --- freezing -------------------------------------------------------------------

    /// A `SemType` the registry resolved, cut to the template a consumer instantiates. A
    /// `TyVar` that no typar env names is a hole the signature left open and no consumer can
    /// fill, so it degrades to an unresolved-typar marker rather than faulting the file.
    let freezeOver (ctx: PassContext) (env: (TyVarId * SemType) list) (ty: SemType) : FrozenType =
        FrozenTypeBridge.freezeWith
            ctx.Store
            (fun _ -> FTUnknown UnknownReason.UnresolvedTypar)
            (ElaborateTypars.remapDeclTypars ctx.Store env ty)

    /// The env of a declaration's own typars under `scope`: a typar written anywhere in the
    /// declaration's structure freezes to `FTTypar(scope, i)` at its declared position.
    let scopedEnv (ctx: PassContext) (scope: TyparScope) (typars: Block<DeclaredTypar>) : (TyVarId * SemType) list =
        ElaborateTypars.mkDeclTyparEnv ctx.Store scope typars

    /// A member's env: the owner's `declaring` typars under the type's scope, then the
    /// member's `own` under the member's.
    let memberEnv
        (ctx: PassContext)
        (owner: TypeKey)
        (declaring: Block<DeclaredTypar>)
        (own: Block<DeclaredTypar>)
        : (TyVarId * SemType) list =
        [
            yield! scopedEnv ctx (TyparScope.Type owner) declaring
            yield! scopedEnv ctx (TyparScope.Member owner) own
        ]

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

    /// The explicit `<'a, [<Measure>] 'u>` a signature declares, named and kinded, in source
    /// order.
    let explicitTypars (ctx: PassContext) (tds: TyparDefns<SyntaxToken> voption) : (string * TyparKind) list =
        match tds with
        | ValueNone -> []
        | ValueSome(TyparDefns(defns = ds)) ->
            [
                for TyparDefn(attributes = attrs; typar = t) in ds do
                    match typarName ctx t with
                    | ValueSome n -> yield n, kindOfSlot ctx attrs
                    | ValueNone -> ()
            ]

    /// Run `f` under a typar scope holding exactly `groups`, outer-to-inner: the signature's
    /// own `<'a>` shadows an enclosing typar of the same name. STRICT: every typar the
    /// signature writes was collected before entry, so one that still misses is undeclared.
    let underTypars (ctx: PassContext) (groups: (TyparScope * Block<DeclaredTypar>) list) (f: unit -> 'a) : 'a =
        use _ = ctx.PushTyparScope(groups, true)
        f ()

    // --- curried signatures ---------------------------------------------------------

    /// One `->`-separated group's .NET-tupled domain, and the return type. `a * b -> r` is one
    /// group of width 2; `(a * b) -> r` is one group of width 1.
    let translateSigGroups (ctx: PassContext) (CurriedSig(args = argGroups; returnType = ret)) =
        let domains =
            [
                for struct (ArgsSpec.ArgsSpec(args = specs), _) in argGroups ->
                    Block.ofSeq (seq { for ArgSpec(typ = t) in specs -> translateType ctx t })
            ]

        domains, translateType ctx ret

    /// The whole signature as one function type, which is what a VALUE's scheme is.
    let curriedFunTy (domains: Block<SemType> list) (ret: SemType) : SemType =
        let tupled (d: Block<SemType>) =
            match d.Length with
            | 1 -> d.[0]
            | _ -> TyTuple d

        List.foldBack (fun d acc -> TyFun(tupled d, acc)) domains ret

    let freezeDomains (ctx: PassContext) env (domains: Block<SemType> list) : FrozenType list =
        domains
        |> List.map (fun d -> ExternalSignature.tupledParams (d |> Block.map (freezeOver ctx env)))

    // --- `when` clauses --------------------------------------------------------------

    /// The trait a `when (^T or ^U) : (static member M : …)` clause declares, over the `Types`
    /// indices `indexOf` assigns and the frozen types `target` builds. `ValueNone` for a member
    /// with no compiled name, or a support set naming no declared type-kinded typar.
    let private memberTraitOf
        (ctx: PassContext)
        (indexOf: Typar<SyntaxToken> -> int<typeSlot> voption)
        (target: Type<SyntaxToken> -> FrozenType)
        sts
        (ms: MemberSig<SyntaxToken>)
        : MemberTrait voption =
        let indices =
            Block.ofList
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
            // A trait signature is tupled by convention (`^T * ^T -> ^T`), parsing as one
            // group of N args; flatten it to the arg list.
            let argFts =
                Block.ofList
                    [
                        for struct (ArgsSpec.ArgsSpec(args = specs), _) in argGroups do
                            for ArgSpec(typ = t) in specs -> target t
                    ]

            ValueSome
                {
                    TyparIndices = indices
                    MemberName = memberName
                    ArgTypes = argFts
                    ReturnType = target retTy
                }
        | _ -> ValueNone

    /// The published scheme of a signature over its own typars: each type-kinded typar's `when`
    /// clauses, and the member traits. A clause referencing a typar the signature does not
    /// declare is dropped; one naming a measure-kinded typar is reported.
    let publishedScheme
        (ctx: PassContext)
        (env: (TyVarId * SemType) list)
        (typeParams: Block<DeclaredTypar>)
        (clauses: TyparConstraints<SyntaxToken> list)
        : FunctionScheme =
        let shape: TyparList = TyparList.unconstrained typeParams

        /// The `Types` index of the declared type parameter `t` names. A measure-kinded
        /// parameter is reported.
        let typeSlotOf (t: Typar<SyntaxToken>) : int<typeSlot> voption =
            match typarName ctx t with
            | ValueNone -> ValueNone
            | ValueSome n ->
                match typeParams |> Block.tryFindIndex (fun tp -> tp.Name = n) with
                | ValueNone -> ValueNone
                | ValueSome pos ->
                    match TyparList.typeSlotOf shape (TyparIndex.sigSlot pos) with
                    | ValueSome i -> ValueSome i
                    | ValueNone ->
                        match CstKeys.typarToken t with
                        | ValueSome tok -> ctx.Report(tok, Kind.TypeParameterExpectedNotMeasure)
                        | ValueNone -> ()

                        ValueNone

        let target (t: Type<SyntaxToken>) : FrozenType =
            freezeOver ctx env (translateType ctx t)

        let constraints =
            Block.init shape.TypeArity (fun _ -> ResizeArray<TyparConstraintKindG<FrozenType>>())

        let defaults = Block.init shape.TypeArity (fun _ -> ResizeArray<FrozenType>())
        let traits = ResizeArray<MemberTrait>()

        let constraintOn (t: Typar<SyntaxToken>) (kind: TyparConstraintKindG<FrozenType>) =
            match typeSlotOf t with
            | ValueSome i -> constraints.[i].Add kind
            | ValueNone -> ()

        for clause in clauses do
            for c in clause.Constraints do
                match c with
                | Constraint.Equality(typar = t) -> constraintOn t TyparConstraintKindG.Equality
                | Constraint.Comparison(typar = t) -> constraintOn t TyparConstraintKindG.Comparison
                | Constraint.Struct(typar = t) -> constraintOn t TyparConstraintKindG.Struct
                | Constraint.ReferenceType(typar = t) -> constraintOn t TyparConstraintKindG.ReferenceType
                | Constraint.Nullness(typar = t) -> constraintOn t TyparConstraintKindG.Nullness
                | Constraint.NotNull(typar = t) -> constraintOn t TyparConstraintKindG.NotNull
                | Constraint.Coercion(typar = t; typ = tgt) ->
                    constraintOn t (TyparConstraintKindG.Coercion(target tgt))
                | Constraint.DefaultConstructor(typar = t) -> constraintOn t TyparConstraintKindG.DefaultConstructor
                | Constraint.Unmanaged(typar = t) -> constraintOn t TyparConstraintKindG.Unmanaged
                | Constraint.Enum(typar = t; typ = underlying) ->
                    constraintOn t (TyparConstraintKindG.Enum(target underlying))
                | Constraint.Delegate(typar = t; type1 = args; type2 = ret) ->
                    constraintOn t (TyparConstraintKindG.Delegate(target args, target ret))
                | Constraint.Default(typar = t; typ = tgt) ->
                    match typeSlotOf t with
                    | ValueSome i -> defaults.[i].Add(target tgt)
                    | ValueNone -> ()
                | Constraint.MemberTrait(staticTypars = sts; membersign = ms) ->
                    match memberTraitOf ctx typeSlotOf target sts ms with
                    | ValueSome trait_ -> traits.Add trait_
                    | ValueNone -> ()

        let typars =
            { shape with
                Types =
                    shape.Types
                    |> Block.mapi (fun i t ->
                        { t with
                            Constraints =
                                {
                                    Kinds = EqSet.ofSeq constraints.[i]
                                    Defaults = Block.ofResizeArray defaults.[i]
                                }
                        }
                    )
            }

        FunctionScheme.create typars (Block.ofResizeArray traits)
