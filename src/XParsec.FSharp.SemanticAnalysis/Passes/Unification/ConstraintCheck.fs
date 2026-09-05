namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationSubsume

/// The READ-ONLY constraint verdicts: whether a ground type satisfies a typar bound, and
/// the propagation of a still-undecided bound onto a compound's free arguments.
module UnificationConstraintCheck =

    /// `Defer` is the not-yet verdict: the target is still free (or
    /// compound-with-free-args). It stays on the TyVar and re-fires on the next `Link`.
    type ConstraintOutcome =
        | Satisfied
        | Violated
        | Defer

    /// `int` is equatable because `prim-types-min.fsi` declares `interface equatable<int>`.
    let private primitiveDeclares
        (ctx: PassContext)
        (cap: RuntimeNames.CapabilityIdentity voption)
        (key: TypeKey)
        : bool =
        match ctx.Provider.TryLookupType key with
        | ValueSome(ExternalTypeShape.Intrinsic { Class = ValueSome surface }) ->
            RuntimeNames.declaresCapability cap surface.Interfaces
        | _ -> false

    let private constraintKindName (ctx: PassContext) (k: SemanticConstraintKind) : string =
        match k with
        | SemanticConstraintKind.Equality -> "equality"
        | SemanticConstraintKind.Comparison -> "comparison"
        | SemanticConstraintKind.Struct -> "struct"
        | SemanticConstraintKind.ReferenceType -> "not struct"
        | SemanticConstraintKind.Nullness -> "null"
        | SemanticConstraintKind.NotNull -> "not null"
        | SemanticConstraintKind.Coercion target -> sprintf "subtype of %s" (shown ctx.Store target)
        | SemanticConstraintKind.DefaultConstructor -> "new"
        | SemanticConstraintKind.Unmanaged -> "unmanaged"
        | SemanticConstraintKind.Enum underlying -> sprintf "enum<%s>" (shown ctx.Store underlying)
        | SemanticConstraintKind.Delegate(args, ret) ->
            sprintf "delegate<%s, %s>" (shown ctx.Store args) (shown ctx.Store ret)
        | SemanticConstraintKind.OneOf choices ->
            "one of " + String.concat ", " [ for k in choices.Underlying -> k.Name ]

    /// `ValueSome true` = constraint holds; `ValueSome false` = violation;
    /// `ValueNone` = undecided, fall through to structural / deferred handling.
    let private primitiveSupports (ctx: PassContext) (kind: SemanticConstraintKind) (key: TypeKey) : bool voption =
        // By KEY, not by name: a user type merely spelled `int` in its own namespace reaches
        // no contract shape, so it declares no capability.
        match kind with
        // An undeclared capability defers rather than refusing: `decimal` on JS has no
        // contract to reach, so it has said nothing, not "no".
        | SemanticConstraintKind.Equality ->
            if primitiveDeclares ctx ctx.CapabilityIds.Equatable key then
                ValueSome true
            else
                ValueNone
        | SemanticConstraintKind.Comparison ->
            if primitiveDeclares ctx ctx.CapabilityIds.Comparable key then
                ValueSome true
            else
                ValueNone
        // Nullness is decided structurally and value-ness by the target, for primitives as
        // much as for anything else, so neither reaches this table.
        | SemanticConstraintKind.Struct
        | SemanticConstraintKind.ReferenceType
        | SemanticConstraintKind.Nullness
        | SemanticConstraintKind.NotNull
        | SemanticConstraintKind.Coercion _
        | SemanticConstraintKind.DefaultConstructor
        | SemanticConstraintKind.Unmanaged
        | SemanticConstraintKind.Enum _
        | SemanticConstraintKind.Delegate _
        // Membership is decided against the choice list by `checkConstraint`, which never
        // reaches this table.
        | SemanticConstraintKind.OneOf _ -> ValueNone

    /// `Violated` is sticky (once any element fails, the whole compound fails);
    /// `Defer` propagates when no element failed but at least one is still pending.
    let private reduceOutcome (check: SemType -> ConstraintOutcome) (items: seq<SemType>) : ConstraintOutcome =
        let mutable result = Satisfied

        for item in items do
            match result, check item with
            | Violated, _ -> ()
            | _, Violated -> result <- Violated
            | Defer, _
            | _, Defer -> result <- Defer
            | Satisfied, Satisfied -> ()

        result

    /// The dual of `reduceOutcome`: `Satisfied` is sticky (one element proving it proves the
    /// whole); `Defer` propagates when none proved it but at least one is still pending.
    let private reduceAny (check: SemType -> ConstraintOutcome) (items: seq<SemType>) : ConstraintOutcome =
        let mutable result = Violated

        for item in items do
            match result, check item with
            | Satisfied, _ -> ()
            | _, Satisfied -> result <- Satisfied
            | Defer, _
            | _, Defer -> result <- Defer
            | Violated, Violated -> ()

        result

    let private negate (outcome: ConstraintOutcome) : ConstraintOutcome =
        match outcome with
        | Satisfied -> Violated
        | Violated -> Satisfied
        | Defer -> Defer

    /// Does `null` inhabit this type? `null` is a union MEMBER, not a property of a type:
    /// `objnull` is `obj | null` and admits it where bare `obj` does not. The CLR's
    /// reference-null is erased at the ABI seam, so the verdict is the same on every target.
    let rec private admitsNull (ctx: PassContext) (t: SemType) : ConstraintOutcome =
        match resolveStep ctx.Store t with
        // Not ground yet, so it states nothing either way: the next `Link` re-fires the check.
        | TyVar _
        | TyUnknown _
        | TyTypar _
        | TyCarrier -> Defer
        | TyNull -> Satisfied
        // ANY disjunct carrying `null` admits it, so one `null` disjunct decides the union and
        // an ungrounded one defers. `never` has no disjunct to carry `null`.
        | TyOr ds -> reduceAny (admitsNull ctx) (ds.Disjuncts.Underlying :> seq<SemType>)
        // `[<AllowNullLiteral>]` is the class's own statement that `null` inhabits it, which is
        // what makes `let empty: T = null` legal on such a class.
        | TyClass(classKey, _) ->
            match NominalDecl.tryOfKey ctx classKey with
            | ValueSome decl ->
                if NominalDecl.allowsNullLiteral decl then
                    Satisfied
                else
                    Violated
            // No class shape in hand, so refusing a legal `isNull` here would be a guess.
            | ValueNone -> Defer
        // Every other ground shape (primitives, tuples, functions, records, unions, enums)
        // carries no `null` member.
        | _ -> Violated

    /// The layout query as a constraint verdict. An unsettled layout is a `Defer`, never a
    /// refusal: a compile composing no platform states nothing about either polarity.
    let private valueLayoutOutcome (ctx: PassContext) (t: SemType) : ConstraintOutcome =
        match TypeLayout.ofSemType ctx t with
        | TypeLayout.Value -> Satisfied
        | TypeLayout.Reference -> Violated
        | TypeLayout.Unsettled -> Defer

    /// `when 'a : unmanaged`: a fixed-width scalar, an enum, or a non-generic value type whose
    /// every field is unmanaged. A generic nominal is refused whatever its arguments, as F#
    /// refuses it. A published value class is accepted on its layout alone.
    let rec private unmanagedOutcome (ctx: PassContext) (t: SemType) : ConstraintOutcome =
        match resolveStep ctx.Store t with
        | TyVar _
        | TyUnknown _
        | TyTypar _
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> Defer
        | TyLiteral v -> unmanagedOutcome ctx (TyConst(RuntimeNames.literalBaseKey v, EqArray.empty))
        | TyConst(key, args) ->
            if args.IsEmpty && RuntimeNames.isUnmanagedPrimitiveKey key then
                Satisfied
            else
                Violated
        | TyEnum _ -> Satisfied
        | TyTuple items as ty ->
            match valueLayoutOutcome ctx ty with
            | Satisfied -> reduceOutcome (unmanagedOutcome ctx) (items.Underlying :> seq<SemType>)
            | other -> other
        | (TyRecord(key, args) | TyUnion(key, args) | TyClass(key, args)) as ty ->
            if not args.IsEmpty then
                Violated
            else
                match valueLayoutOutcome ctx ty with
                | Satisfied ->
                    match
                        NominalDecl.tryOfKey ctx key
                        |> ValueOption.bind (fun d -> NominalDecl.fieldTypes ctx.Store d args)
                    with
                    | ValueSome fields -> reduceOutcome (unmanagedOutcome ctx) fields
                    | ValueNone -> Satisfied
                | other -> other
        | TyFun _
        | TyOr _ -> Violated

    /// `when 'a : (new : unit -> 'a)`. Every value type has a default constructor; a
    /// reference type needs a declared parameterless `.ctor` of any accessibility and must
    /// be neither abstract nor an interface. `obj` and `exn` are the reference primitives with one.
    let rec private defaultCtorOutcome (ctx: PassContext) (t: SemType) : ConstraintOutcome =
        match resolveStep ctx.Store t with
        | TyVar _
        | TyUnknown _
        | TyTypar _
        | TyKeyOf _
        | TyIndexedAccess _
        | TyConditional _ -> Defer
        | TyEnum _ -> Satisfied
        | TyLiteral v -> defaultCtorOutcome ctx (TyConst(RuntimeNames.literalBaseKey v, EqArray.empty))
        | TyConst(key, _) when key = RuntimeNames.objKey || key = RuntimeNames.exnKey -> Satisfied
        | (TyConst _ | TyTuple _ | TyRecord _ | TyUnion _) as ty -> valueLayoutOutcome ctx ty
        | TyClass(key, _) as ty ->
            match valueLayoutOutcome ctx ty with
            | Satisfied -> Satisfied
            | Defer -> Defer
            | Violated ->
                match NominalDecl.tryOfKey ctx key with
                | ValueSome decl ->
                    if NominalDecl.isInterface decl || NominalDecl.isAbstract decl then
                        Violated
                    elif NominalDecl.hasParameterlessCtor decl then
                        Satisfied
                    else
                        Violated
                // A class with no published shape has said nothing about its ctors.
                | ValueNone -> Defer
        | TyFun _
        | TyOr _ -> Violated

    /// The underlying primitive of an enum, local or imported.
    let enumUnderlyingType (ctx: PassContext) (key: TypeKey) : SemType voption =
        let ofKey (k: TypeKey) = TyConst(k, EqArray.empty)

        match TypeRegistry.tryEnumByKey ctx.Types key with
        | ValueSome info -> TEnumCases.underlyingTypeKey info.Cases |> ValueOption.map ofKey
        | ValueNone ->
            match ctx.Provider.TryLookupType key with
            | ValueSome(ExternalTypeShape.Enum(underlying = underlying)) -> ValueSome(ofKey underlying)
            | _ -> ValueNone

    /// Free TyVars return `Defer` so the next `Link` assignment re-fires the check; nested
    /// compounds recurse compositionally.
    let rec checkConstraint (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : ConstraintOutcome =
        // The type's stamped equality / comparison verdict overrides the field-walk: a
        // `Custom` type is `Satisfied` by its own members, and its fields may individually
        // lack equality.
        let verdictOutcome
            (eq: EqualityVerdict)
            (cmp: ComparisonVerdict)
            (fieldsOf: unit -> seq<SemType>)
            : ConstraintOutcome =
            match c.Kind, eq, cmp with
            | SemanticConstraintKind.Equality, EqualityVerdict.NoEquality, _ -> Violated
            | SemanticConstraintKind.Equality, (EqualityVerdict.Reference | EqualityVerdict.Custom), _ -> Satisfied
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.NoComparison -> Violated
            | SemanticConstraintKind.Comparison, _, ComparisonVerdict.Custom -> Satisfied
            | _ -> reduceOutcome (checkConstraint ctx c) (fieldsOf ())

        match c.Kind, resolveStep ctx.Store t with
        | _, TyVar _ -> Defer
        // An unresolved contract type supports no constraint, but the mismatch was
        // already reported where it unified, so a second error would be a duplicate.
        | _, TyUnknown _ -> Defer
        | _, TyTypar _ -> Defer
        // A carried type-level computation can decide no constraint until it grounds.
        | _, (TyKeyOf _ | TyIndexedAccess _ | TyConditional _) -> Defer
        | SemanticConstraintKind.Nullness, ty -> admitsNull ctx ty
        | SemanticConstraintKind.NotNull, ty -> negate (admitsNull ctx ty)
        // One query and its negation, over every shape a layout is decided for. A LITERAL is
        // excluded so it widens to its base primitive first.
        | SemanticConstraintKind.Struct,
          ((TyConst _ | TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _ | TyOr _ | TyEnum _) as ty) ->
            valueLayoutOutcome ctx ty
        | SemanticConstraintKind.ReferenceType,
          ((TyConst _ | TyTuple _ | TyFun _ | TyRecord _ | TyUnion _ | TyClass _ | TyOr _ | TyEnum _) as ty) ->
            negate (valueLayoutOutcome ctx ty)
        | SemanticConstraintKind.Unmanaged, ty -> unmanagedOutcome ctx ty
        | SemanticConstraintKind.DefaultConstructor, ty -> defaultCtorOutcome ctx ty
        // The underlying type is unified by `dischargeConstraints` once the typar grounds to
        // an enum, so a width mismatch reports as a type mismatch, as F# reports it.
        | SemanticConstraintKind.Enum _, TyEnum _ -> Satisfied
        | SemanticConstraintKind.Enum _, _ -> Violated
        // No modelled shape is a delegate, and `Translate` refuses the clause, so this arm
        // only meets an imported bound.
        | SemanticConstraintKind.Delegate _, _ -> Violated
        // Equality on an enum is universal and comparison on one is out of scope, so
        // neither is ever proved or refused here.
        | _, TyEnum _ -> Defer
        // A structural literal erases to its base primitive, so re-entering with it judges
        // every kind, `Coercion` included, exactly as the base primitive would be.
        | _, TyLiteral v -> checkConstraint ctx c (TyConst(RuntimeNames.literalBaseKey v, EqArray.empty))
        | SemanticConstraintKind.OneOf choices, ty ->
            match ty with
            | TyConst(k, targs) when targs.IsEmpty && EqArray.exists (fun c -> c = k) choices -> Satisfied
            | _ -> Violated
        | SemanticConstraintKind.Coercion target, _ ->
            // `'e :> exn`: `subsumes` walks user and BCL `inherit` chains, so a thrown
            // `InvalidOperationException` reaches `exn`. Past the `TyVar _` guard above,
            // `Unrelated` is a real violation, not "unknown yet".
            match subsumes ctx t target with
            | SubsumeOutcome.Equal
            | SubsumeOutcome.Subtype -> Satisfied
            | SubsumeOutcome.Unrelated -> Violated
        | k, TyConst(nameKey, _) ->
            match primitiveSupports ctx k nameKey with
            | ValueSome true -> Satisfied
            | ValueSome false -> Violated
            | ValueNone -> Defer
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyFun _ -> Violated
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison), TyTuple items ->
            reduceOutcome (checkConstraint ctx c) (items.Underlying :> seq<SemType>)
        // A published shape carries no stamped verdict, so it decides neither capability.
        | (SemanticConstraintKind.Equality | SemanticConstraintKind.Comparison),
          (TyRecord(key, args) | TyUnion(key, args) | TyClass(key, args)) ->
            match NominalDecl.tryOfKey ctx key with
            | ValueSome(NominalDecl.Local decl) ->
                verdictOutcome
                    (LocalNominal.equalitySupport decl)
                    (LocalNominal.comparisonSupport decl)
                    (fun () -> LocalNominal.fieldTypes ctx.Store decl args :> seq<SemType>)
            | ValueSome(NominalDecl.Published _)
            | ValueNone -> Defer
        | SemanticConstraintKind.Equality, TyOr ds ->
            // EQUALITY iff EVERY disjunct has it: generic equality is total on the union's
            // boxed repr, a cross-disjunct `=` returning `false` rather than throwing.
            reduceOutcome (checkConstraint ctx c) (ds.Disjuncts.Underlying :> seq<SemType>)
        | SemanticConstraintKind.Comparison, TyOr ds ->
            // COMPARISON does NOT reduce disjunct-wise: `(1).CompareTo("a")` throws, so a
            // heterogeneous union is non-comparable even when each disjunct is comparable.
            if ds.Disjuncts.IsEmpty then Satisfied else Violated

    let reportConstraintViolation
        (ctx: PassContext)
        (tok: SyntaxToken)
        (c: SemanticConstraint)
        (target: SemType)
        : unit =
        ctx.Report(tok, Kind.ConstraintNotSupported(shown ctx.Store target, constraintKindName ctx c.Kind))

    /// When a compound shape is partially resolved, the parent constraint is
    /// satisfied iff every component supports it, so a still-free component
    /// carries the same constraint forward.
    let propagateToFreeArgs (ctx: PassContext) (c: SemanticConstraint) (t: SemType) : unit =
        let rec walk t =
            match resolveStep ctx.Store t with
            | TyVar tv ->
                let root = UnionFind.find ctx.Store tv
                addConstraintByKind ctx.Store root.Id c
            | t -> SemType.iterChildren walk t

        walk t
