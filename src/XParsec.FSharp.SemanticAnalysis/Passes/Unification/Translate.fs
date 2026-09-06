namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngineCore
open UnificationConstraintCheck
open UnificationEngine

module internal UnificationTranslate =

    let enterLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel + 1

    let exitLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel - 1

    /// Overwrites any prior entry; get-or-allocate callers must use `tvOf`.
    let freshTv (ctx: PassContext) (key: NodeKey) : TyVarId =
        let tv = ctx.FreshTyVar()
        ctx.Bindings.TypeVar.Set(key, tv)
        tv

    /// Get-or-allocate, because a missing key is a binding-site pattern not yet walked,
    /// including a forward reference inside a `let rec` group.
    let tvOf (ctx: PassContext) (key: NodeKey) : TyVarId =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Recover with a fresh TyVar, so a broken subtree still yields a type.
    let errorTy (ctx: PassContext) (tok: SyntaxToken) (kind: Kind) : SemType =
        ctx.Report(tok, kind)
        TyVar(ctx.FreshTyVar())

    /// The last resort for a WRITTEN reference no claim of this file holds and no external shape built.
    /// The spelling either resolves to an external type the contract registered without a body,
    /// which is a gap to report, or resolved to nothing at all.
    let private unresolvedRefTy (ctx: PassContext) (site: NodeSite) (name: string) : SemType =
        let unmodelled =
            match ctx.Resolution.TypeRefVerdicts.TryGetValue site.Key with
            | ValueSome(TypeRefVerdict.ExternalType(_, ExternalTypeShape.Unmodelled(reason = r))) -> ValueSome r
            | _ -> ValueNone

        match unmodelled with
        | ValueSome(UnmodelledReason.ExtractionFailed reason) ->
            errorTy
                ctx
                site.Tok
                (Kind.Message(
                    sprintf "A referenced package declares '%s', but its body did not extract (%s)" name reason
                ))
        | ValueSome r -> errorTy ctx site.Tok (Kind.NotYetSupported(sprintf "'%s' is %s" name r.Description))
        | ValueNone ->
            // Reported at the name's first token alone; the long-ident span is not
            // available here.
            ctx.UndefinedType(Site.ofToken site.Tok, name)
            TyUnknown(UnknownReason.UndefinedName name)

    /// `decl`'s body, `ValueNone` once it is `Broken`. Idempotent: `decl` ends in a terminal
    /// state. Re-entry while `translate` runs is a cycle, reported once at the declaration
    /// and ending `Broken`.
    let private fill (ctx: PassContext) (decl: FillableDecl<'Body>) (translate: unit -> 'Body) : 'Body voption =
        match decl.State with
        | FillState.Filled body -> ValueSome body
        | FillState.Broken -> ValueNone
        | FillState.InProgress ->
            ctx.Report(decl.DeclSite.Tok, Kind.CyclicType(decl.Name, TypeCycle.Abbreviation))
            decl.State <- FillState.Broken
            ValueNone
        | FillState.NotFilled ->
            decl.State <- FillState.InProgress

            try
                let body = translate ()

                match decl.State with
                | FillState.InProgress ->
                    decl.State <- FillState.Filled body
                    ValueSome body
                | _ -> ValueNone
            finally
                match decl.State with
                | FillState.InProgress -> decl.State <- FillState.Broken
                | _ -> ()

    /// `info`'s term over base-measure atoms, forced on first call: a base measure is its own
    /// atom, an abbreviation is its translated body. `ValueNone` once a cycle among
    /// abbreviations broke it; the cycle is reported once, at the declaration.
    let rec forceMeasureFill (ctx: PassContext) (info: MeasureInfo) : MeasureTerm voption =
        fill
            ctx
            info
            (fun () ->
                match info.RhsCst with
                | ValueNone -> MeasureTerm.atom info.TypeKey
                | ValueSome rhs -> translateMeasure ctx info.DeclSite.Tok rhs
            )

    /// The term a local measure claim contributes; a broken claim contributes the empty term.
    and private measureOfClaim (ctx: PassContext) (claim: TypeIdentity) : MeasureTerm =
        match forceMeasureFill ctx (TypeRegistry.measureOfClaim ctx.Types claim) with
        | ValueSome term -> term
        | ValueNone -> MeasureTerm.empty

    /// The term ONE measure atom contributes, read off the verdict NameResolution stamped at
    /// its name. A claim of another kind is FS0705 and an undefined name is FS0039; either
    /// way the atom contributes the empty term.
    and private measureAtom (ctx: PassContext) (li: LongIdent<SyntaxToken>) : MeasureTerm =
        let typeRef = CstKeys.namedTypeRef li
        let tok = typeRef.Site.Tok

        match NameResolutionTypeRefStamp.classifyTypeRef ctx typeRef with
        | TypeRefVerdict.LocalType claim when claim.Kind = TypeDeclKind.Measure -> measureOfClaim ctx claim
        | TypeRefVerdict.ExternalType(_, ExternalTypeShape.Measure term) -> term
        | TypeRefVerdict.LocalType _
        | TypeRefVerdict.ExternalType _ ->
            ctx.Report(tok, Kind.MeasureExpected)
            MeasureTerm.empty
        // FS0033 was reported as the verdict was stamped.
        | TypeRefVerdict.LocalTypeAtOtherArity _
        | TypeRefVerdict.ExternalTypeAtOtherArity _ -> MeasureTerm.empty
        | TypeRefVerdict.UnknownType ->
            ctx.UndefinedType(Site.ofTokenOr (Site.ofLongIdent li) tok, ctx.NameOf tok)
            MeasureTerm.empty

    /// Measure typars (`'u`) and wildcards yield an empty term plus a diagnostic, so the rest
    /// of inference continues without measure noise.
    and translateMeasure (ctx: PassContext) (measureTok: SyntaxToken) (m: Measure<SyntaxToken>) : MeasureTerm =
        match m with
        | Measure.One _ -> MeasureTerm.empty
        | Measure.Named li -> measureAtom ctx li
        | Measure.Power(inner, _, neg, expTok) ->
            let n = System.Numerics.BigInteger.Parse(ctx.NameOf expTok)
            let signed = if neg.IsSome then -n else n

            MeasureTerm.pow
                (translateMeasure ctx measureTok inner)
                (Rational.create (signed, System.Numerics.BigInteger.One))
        | Measure.Product(l, _, r) ->
            MeasureTerm.mul (translateMeasure ctx measureTok l) (translateMeasure ctx measureTok r)
        | Measure.Quotient(l, _, r) ->
            MeasureTerm.div (translateMeasure ctx measureTok l) (translateMeasure ctx measureTok r)
        | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx measureTok inner)
        | Measure.Paren(_, inner, _) -> translateMeasure ctx measureTok inner
        | Measure.Juxtaposition(elems, _) ->
            (MeasureTerm.empty, elems)
            ||> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx measureTok m))
        | Measure.Anonymous _
        | Measure.Typar _ ->
            ctx.Report(measureTok, Kind.NotYetSupported "measure typars / wildcards")

            MeasureTerm.empty

    /// A resolved `Class` whose metadata name a canon is declared on is an intrinsic's
    /// platform type id (`System.Exception` → `exn`) and resolves to the canon `TyConst`,
    /// so no raw BCL nominal enters the unifier. Capability interfaces declare no canon.
    let externalClassTy (ctx: PassContext) (key: TypeKey) (args: EqArray<SemType>) : SemType =
        // Built on the resolved `key` directly: re-minting an identity from the flattened
        // metadata name loses the container and gives an unequal key. The probe keys on that
        // NAME, which is sound because its entries are all bare-IL, where name and key agree.
        match IntrinsicTypeMap.tryCanon (PlatformTypeId(SymbolKeyOps.typeMetaName key)) ctx.IntrinsicTypeMap.Value with
        | ValueSome canon -> TyConst(canon, args)
        | ValueNone -> TyClass(key, args)

    /// DEBUG-only, for a WRITTEN reference about to be reported unresolved: every written
    /// reference carries a verdict, so NO verdict is a stamping walk that missed this syntax
    /// position, and an EXTERNAL verdict's key must be servable by the store view.
    let private assertVerdictServable (ctx: PassContext) (site: NodeSite) (name: string) : unit =
#if DEBUG
        match ctx.Resolution.TypeRefVerdicts.TryGetValue site.Key with
        | ValueNone ->
            failwithf
                "NameResolution stamping gap: type reference '%s' carries no verdict, so a stamping walk missed this syntax position"
                name
        // An external shape that declined to build (no modelled body, or an arity the shape
        // does not carry) reports at the use site.
        | ValueSome(TypeRefVerdict.ExternalType _)
        | ValueSome(TypeRefVerdict.LocalType _)
        | ValueSome(TypeRefVerdict.LocalTypeAtOtherArity _)
        | ValueSome(TypeRefVerdict.ExternalTypeAtOtherArity _)
        | ValueSome TypeRefVerdict.UnknownType -> ()
#else
        ignore ctx
        ignore site
        ignore name
#endif

    /// Build the annotation `SemType` from a resolved external shape addressed by the RESOLVED
    /// identity `symKey`, shared by the stamped and by-name paths. `ValueNone` for an
    /// unmodelled body, which has no kind a type annotation can take.
    let private buildExternalTy
        (ctx: PassContext)
        (symKey: TypeKey)
        (shape: ExternalTypeShape)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        match shape with
        // A referenced intrinsic (`exn = (# "System.Exception" #)`) is NON-transparent: its
        // identity is the shape's canon `TyConst` (`Vesper.exn`) whatever base/ctor surface it
        // carries, which is what keeps `exn.Message` routing to the platform type's members.
        | ExternalTypeShape.Intrinsic s -> ValueSome(TyConst(s.Id.Canon, translatedArgs))
        | ExternalTypeShape.Class _ -> ValueSome(externalClassTy ctx symKey translatedArgs)
        // A capability interface (`disposable`) is a `TyClass` CONSTRAINT; the axis holds no
        // interface canons, so its identity is the resolved key directly.
        | ExternalTypeShape.IntrinsicInterface _ -> ValueSome(TyClass(symKey, translatedArgs))
        | ExternalTypeShape.Record _ -> ValueSome(TyRecord(symKey, translatedArgs))
        | ExternalTypeShape.Union _ -> ValueSome(TyUnion(symKey, translatedArgs))
        // An external enum annotation `(x: E)` → `TyEnum key` (never generic), keyed off the
        // SAME identity an `E.Ci` use site mints, so the two unify. The enum is a DISTINCT
        // nominal, not its underlying int/string.
        | ExternalTypeShape.Enum _ -> ValueSome(TyEnum(symKey))
        // A transparent abbreviation dealiases to its body: `int32 = int` resolves to the
        // `int` key the IL encoder encodes, never a nominal `int32`. The frozen RHS is
        // already kind-correct; the type args substitute into it.
        | ExternalTypeShape.Abbrev { Body = frozen } ->
            ValueSome(FrozenTypeBridge.instantiateDeclaring ctx frozen (translatedArgs.AsSpan().ToArray()))
        // No modelled body, so no kind a *type annotation* can resolve to. Declining routes
        // the reference to `unresolvedRefTy`, which records the gap.
        | ExternalTypeShape.Unmodelled _ -> ValueNone
        // A measure is not a type; `translateTypeRef` reports FS0704 at the reference.
        | ExternalTypeShape.Measure _ -> ValueNone

    /// Build from a resolved external identity and its shape. An arity mismatch is rejected:
    /// it is not this type.
    let private tryExternalTypeOfShape
        (ctx: PassContext)
        (symKey: TypeKey)
        (shape: ExternalTypeShape)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        if shape.TyparArity = translatedArgs.Length then
            buildExternalTy ctx symKey shape translatedArgs
        else
            ValueNone

    /// Fetch + build from an already-resolved external type identity.
    let tryExternalTypeOfKey (ctx: PassContext) (symKey: TypeKey) (translatedArgs: EqArray<SemType>) : SemType voption =
        match ctx.Provider.TryLookupType symKey with
        | ValueSome shape -> tryExternalTypeOfShape ctx symKey shape translatedArgs
        | ValueNone -> ValueNone

    /// One written type argument read by the kind of the parameter it fills.
    [<RequireQualifiedAccess; NoEquality; NoComparison>]
    type private TypeArgRead =
        | Type of SemType
        | Measure of MeasureTerm

    /// `body` is the abbreviation's forced body; `ValueNone` (`Broken`) yields a fresh TyVar
    /// rather than cascading. Prototype-typar constraints are checked against the supplied args
    /// here, because an abbreviation has no fresh-instance step; a Defer propagates to free arg TyVars.
    let expandAbbreviation
        (ctx: PassContext)
        (tok: SyntaxToken)
        (info: AbbreviationInfo)
        (body: SemType voption)
        (args: EqArray<SemType>)
        : SemType =
        let n = min (info.TypeParams.Length) args.Length

        for i = 0 to n - 1 do
            let arg = args.[i]
            let protoRoot = UnionFind.find ctx.Store info.TypeParams.[i].TyVar

            for c in ctx.Store.Constraints.Items protoRoot do
                match checkConstraint ctx c arg with
                | Satisfied -> ()
                | Violated -> reportConstraintViolation ctx tok c arg
                | Defer -> propagateToFreeArgs ctx c arg

        match body with
        | ValueSome body -> instantiateMember ctx.Store (info.TypeParams, args) body
        | ValueNone -> TyVar(ctx.FreshTyVar())

    /// Resolves `'a` through `ctx.Resolution.TyparScope`; callers open a fresh scope per
    /// signature (binding or type defn) before walking. A generic named type written
    /// without its args back-fills the arg list with fresh TyVars for unification to pin.
    let rec translateType (ctx: PassContext) (t: Type<SyntaxToken>) : SemType =
        match t with
        | Type.ParenType(typ = inner) -> translateType ctx inner
        | Type.VarType(Typar.Named(ident = id))
        | Type.VarType(Typar.Static(ident = id)) ->
            let name = ctx.NameOf id

            match ctx.Resolution.TyparScope.TryGetValue name with
            | true, tv -> TyVar tv
            | false, _ ->
                // An implicit typar is minted at the current level, for generalisation at
                // binding-group exit, and memoised so later occurrences share the TyVar. A
                // strict scope (a type defn) forbids one, and reports it first.
                if ctx.Resolution.TyparScopeStrict then
                    ctx.Report(
                        id,
                        Kind.Message(
                            sprintf
                                "Free type parameter %s is not declared in the enclosing type's type-parameter list"
                                name
                        )
                    )

                let tv = ctx.FreshTyVar()
                ctx.Resolution.TyparScope.[name] <- tv
                TyVar tv
        | Type.VarType(Typar.Anon _) ->
            // `_` typar — always fresh, never stored; distinct per occurrence.
            let tv = ctx.FreshTyVar()
            // `_` is the one INFERRED position inside a written type (`Box<_>`): mark it so a
            // consumer of the annotation tells the hole from written structure or a named `'a`.
            ctx.MarkInferenceHole tv
            TyVar tv
        | Type.NamedType li ->
            translateTypeRef ctx (CstKeys.typeRefSite t) (ctx.WrittenTypeNameOf li).Written ImmutableArray.Empty
        | Type.GenericType(longIdent = li; typeArgs = args) ->
            translateTypeRef ctx (CstKeys.typeRefSite t) (ctx.WrittenTypeNameOf li).Written args
        | Type.SuffixedType(baseType = baseTy; longIdent = li) when li.Idents.Length = 1 ->
            // Postfix generic syntax: `'T list` ≡ `list<'T>`. A multi-arg postfix form
            // (`(int, string) Map`) parses its base as a tuple and falls to the arity diagnostic.
            let site = CstKeys.typeRefSite t
            translateTypeRef ctx site (ctx.NameOf site.Tok) (ImmutableArray.Create(TypeArg.Type baseTy))
        | Type.SuffixedType(longIdent = li) ->
            // Postfix application through a QUALIFIED name (`int A.T`). F# accepts it; this
            // compiler has no model for the shape, which the diagnostic says rather than
            // reporting the name as unresolved.
            let site = CstKeys.typeRefSite t
            let written = (ctx.WrittenTypeNameOf li).Written

            errorTy ctx site.Tok (Kind.NotYetSupported(sprintf "the postfix type application '%s'" written))
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple(EqArray.ofSeq (seq { for t in types -> translateType ctx t }))
        | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
            let inner = translateType ctx inner
            translateConstraints ctx cs
            inner
        | Type.ArrayType(baseType = baseTy; commas = commas) ->
            // `'T[]` / `'T[,]` → `TyConst(arrayKey rank, [elem])`, rank = comma count + 1.
            // Array literals build the same repr.
            let rank = commas.Length + 1

            TyConst(RuntimeNames.arrayKey rank, EqArray.singleton (translateType ctx baseTy))
        | Type.Null _ ->
            // The `null` type — a *member* of an anonymous union (`T | null`), not a nominal.
            // Resolves to the cross-backend `nullKey` intrinsic.
            TyConst(RuntimeNames.nullKey, EqArray.empty)
        | Type.UnionType(left = l; right = r) ->
            // TypeScript-style anonymous structural union (`X | Y`); the CST is left-nested
            // for `a | b | c`. `mkUnion` flattens and dedups, collapses a singleton to its
            // one member, and leaves a set that compares equal in any member order.
            mkUnion [ translateType ctx l; translateType ctx r ]
        | Type.StructTupleType(structToken = tok) -> errorTy ctx tok (Kind.NotYetSupported "a struct tuple type")
        | Type.AnonRecordType(lBraceBar = tok) -> errorTy ctx tok (Kind.NotYetSupported "an anonymous record type")
        | Type.DottedType(dot = tok) -> errorTy ctx tok (Kind.NotYetSupported "a dotted type application")
        | Type.AnonymousSubtype(hash = tok) -> errorTy ctx tok (Kind.NotYetSupported "a flexible type '#T'")
        | Type.SubtypeConstraint(colonGreaterThan = tok) ->
            errorTy ctx tok (Kind.NotYetSupported "a subtype constraint written as a type")
        | Type.ILIntrinsic(lHashParen = tok) ->
            // `(# "…" #)` is a type abbreviation's RHS, recognised by `TypeRegistration` and
            // `IntrinsicBindings` before translation. Reaching here means it was written in an
            // ordinary type position.
            errorTy ctx tok (Kind.NotYetSupported "an inline-IL type outside a type abbreviation")
        | Type.MeasureType _ ->
            // A measure reaches a type through `float<kg>`, which arrives as `TypeArg.Measure`
            // under `GenericType`. A bare `MeasureType` carries no token to report at.
            TyVar(ctx.FreshTyVar())
        | Type.Missing
        | Type.SkipsTokens _ ->
            // Parse-error recovery. The parse reported at this span already, so inference
            // continues on a fresh TyVar rather than reporting a second time.
            TyVar(ctx.FreshTyVar())

    /// The `SemType` of the project-local type `claim` identifies, applied to `args`. A nominal is
    /// built from the claim's `TypeKey` alone, so `type A = { x: B } and B = { y: A }`
    /// resolves before either detail registers.
    and private resolveClaimedType
        (ctx: PassContext)
        (site: NodeSite)
        (claim: TypeIdentity)
        (args: EqArray<SemType>)
        : SemType voption =
        let key = claim.Key

        match claim.Kind with
        // Primitive binding (`type int = (# "System.Int32" #)`): a nominal intrinsic, NOT a
        // transparent abbreviation. Its canon key is contract-sourced, minted at claim time.
        | TypeDeclKind.IntrinsicBinding -> ValueSome(TyConst(TypeRegistry.intrinsicKeyOf ctx.Types claim.Name, args))
        | TypeDeclKind.Abbreviation ->
            match TypeRegistry.tryAbbrevByKey ctx.Types key with
            | ValueSome info ->
                // Eager expansion: force the body, then substitute the use-site args.
                ValueSome(expandAbbreviation ctx site.Tok info (forceFill ctx info) args)
            | ValueNone -> ValueNone
        | TypeDeclKind.Record -> ValueSome(TyRecord(key, args))
        | TypeDeclKind.Union ->
            ctx.Resolution.ResolvedType.Set(site.Key, key)
            ValueSome(TyUnion(key, args))
        | TypeDeclKind.Enum ->
            // An enum is niladic (no type args), so the reference is just `TyEnum key`.
            ctx.Resolution.ResolvedType.Set(site.Key, key)
            ValueSome(TyEnum key)
        | TypeDeclKind.Class -> ValueSome(TyClass(key, args))
        // A measure is not a type, so a reference in TYPE position is FS0704; the recovery
        // type keeps the rest of inference off it.
        | TypeDeclKind.Measure -> ValueSome(errorTy ctx site.Tok Kind.TypeExpectedNotMeasure)

    /// The `SemType` a WRITTEN type reference translates to: the verdict NameResolution stamped
    /// at `site`, applied to the written `args`. A claim at another arity (FS0033, reported when
    /// stamped) or an argument of the wrong kind recovers as a fresh type.
    and private translateTypeRef
        (ctx: PassContext)
        (site: NodeSite)
        (name: string)
        (args: ImmutableArray<TypeArg<SyntaxToken>>)
        : SemType =
        let unresolved () =
            assertVerdictServable ctx site name
            unresolvedRefTy ctx site name

        /// `build` applied to the type-kinded arguments, measured by the measure-kinded one.
        let apply (kinds: EqArray<TyparKind>) (build: EqArray<SemType> -> SemType) : SemType =
            match readTypeArgs ctx site.Tok kinds args with
            | ValueNone -> TyVar(ctx.FreshTyVar())
            | ValueSome reads ->
                // A measure-kinded position holds a free placeholder; the measure lives on the
                // wrapping measured TyVar.
                let typeArgs =
                    reads
                    |> EqArray.map (fun read ->
                        match read with
                        | TypeArgRead.Type ty -> ty
                        | TypeArgRead.Measure _ -> TyVar(ctx.FreshTyVar())
                    )

                let units =
                    reads
                    |> EqArray.toArray
                    |> Array.choose (fun read ->
                        match read with
                        | TypeArgRead.Measure term -> Some term
                        | TypeArgRead.Type _ -> None
                    )

                match units with
                | [||] -> build typeArgs
                | [| term |] -> ctx.MeasuredTy(build typeArgs, term)
                | _ -> errorTy ctx site.Tok (Kind.NotYetSupported "a type with several measure parameters")

        match ctx.Resolution.TypeRefVerdicts.TryGetValue site.Key with
        | ValueSome(TypeRefVerdict.LocalType claim) ->
            apply
                claim.TyparKinds
                (fun typeArgs ->
                    match resolveClaimedType ctx site claim typeArgs with
                    | ValueSome ty -> ty
                    | ValueNone -> unresolved ()
                )
        | ValueSome(TypeRefVerdict.LocalTypeAtOtherArity _)
        | ValueSome(TypeRefVerdict.ExternalTypeAtOtherArity _) -> TyVar(ctx.FreshTyVar())
        // A measure is not a type, so a reference in TYPE position is FS0704.
        | ValueSome(TypeRefVerdict.ExternalType(_, ExternalTypeShape.Measure _)) ->
            errorTy ctx site.Tok Kind.TypeExpectedNotMeasure
        | ValueSome(TypeRefVerdict.ExternalType(key, shape)) ->
            apply
                shape.TyparKinds
                (fun typeArgs ->
                    match tryExternalTypeOfShape ctx key shape typeArgs with
                    | ValueSome ty -> ty
                    | ValueNone -> unresolved ()
                )
        | ValueSome TypeRefVerdict.UnknownType
        | ValueNone ->
            match RuntimeNames.tryTargetOptionalPrimitiveKey name with
            | ValueNone -> apply (TyparKinds.typeOnly args.Length) (fun _ -> unresolved ())
            // A target-optional primitive (`nativeint`, `decimal`, `undefined`, …) resolves to
            // its language-known key on a stack that declares no contract for it; `PlatformTypes`
            // then reports each mention as unsupported on the compiling target.
            | ValueSome key ->
                let known = RuntimeNames.targetOptionalPrimitiveKinds key
                let bare = TyConst(key, EqArray.empty)

                match known |> List.tryFind (fun kinds -> kinds.Length = args.Length) with
                | Some kinds -> apply kinds (fun _ -> bare)
                | None ->
                    let nearest = known |> List.minBy (fun kinds -> abs (kinds.Length - args.Length))
                    ctx.Report(site.Tok, Kind.TypeArgArity(name, nearest.Length, args.Length))
                    bare

    /// The written `args` read by the kind of the parameter each fills, one of `kinds` per
    /// argument. `ValueNone` after FS0704 (a measure filling a type parameter) or FS0705 (a
    /// type filling a measure parameter), reported at the argument.
    and private readTypeArgs
        (ctx: PassContext)
        (nameTok: SyntaxToken)
        (kinds: EqArray<TyparKind>)
        (args: ImmutableArray<TypeArg<SyntaxToken>>)
        : EqArray<TypeArgRead> voption =
        let readArg (kind: TyparKind) (arg: TypeArg<SyntaxToken>) : TypeArgRead voption =
            match kind, arg with
            | TyparKind.Type, TypeArg.Type argTy -> ValueSome(TypeArgRead.Type(translateType ctx argTy))
            | TyparKind.Type, TypeArg.Measure m ->
                ctx.Report(CstKeys.firstTokenOfMeasure m, Kind.TypeExpectedNotMeasure)
                ValueNone
            | TyparKind.Measure, TypeArg.Measure m -> ValueSome(TypeArgRead.Measure(translateMeasure ctx nameTok m))
            | TyparKind.Measure, TypeArg.Type argTy ->
                // The parser spells a lone name in measure position as a `NamedType`; only that
                // shape has a measure reading.
                match CstKeys.measureOfType argTy with
                | ValueSome m -> ValueSome(TypeArgRead.Measure(translateMeasure ctx nameTok m))
                | ValueNone ->
                    let tok =
                        match CstKeys.ofTypeRef argTy with
                        | ValueSome typeRef -> typeRef.Site.Tok
                        | ValueNone -> nameTok

                    ctx.Report(tok, Kind.MeasureExpected)
                    ValueNone

        if kinds.Length <> args.Length then
            failwithf
                "type reference '%s' resolved to %d parameters for %d written arguments"
                (ctx.NameOf nameTok)
                kinds.Length
                args.Length

        let reads = Array.init args.Length (fun i -> readArg kinds.[i] args.[i])

        if reads |> Array.exists ValueOption.isNone then
            ValueNone
        else
            ValueSome(EqArray.ofSeq (Seq.map ValueOption.get reads))

    /// Attach to the constrained typar's TyVar through the current
    /// `ctx.Resolution.TyparScope`.
    and private translateConstraint (ctx: PassContext) (c: Constraint<SyntaxToken>) : unit =
        let typarTokenOf (t: Typar<SyntaxToken>) : SyntaxToken voption =
            match t with
            | Typar.Named(ident = id)
            | Typar.Static(ident = id) -> ValueSome id
            | Typar.Anon _ -> ValueNone

        /// The root of a declared typar in the live scope; `ValueNone` for an anonymous or
        /// undeclared one.
        let rootOf (typar: Typar<SyntaxToken>) : Rep voption =
            match typarTokenOf typar with
            | ValueNone -> ValueNone
            | ValueSome id ->
                match ctx.Resolution.TyparScope.TryGetValue(ctx.NameOf id) with
                | true, tv -> ValueSome(UnionFind.find ctx.Store tv)
                | false, _ -> ValueNone

        let attach (typar: Typar<SyntaxToken>) (kind: SemanticConstraintKind) (declTok: SyntaxToken) : unit =
            match typarTokenOf typar with
            | ValueNone -> ()
            | ValueSome id ->
                let name = ctx.NameOf id

                match ctx.Resolution.TyparScope.TryGetValue name with
                | true, tv ->
                    let root = UnionFind.find ctx.Store tv

                    let sc =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofToken declTok NodeKind.TypeVarRef
                        }

                    if not (ctx.Store.Constraints.Items root |> List.exists (fun e -> e.Kind = sc.Kind)) then
                        ctx.Store.Constraints.Prepend(root, sc)
                | false, _ ->
                    ctx.Report(
                        id,
                        Kind.Message(
                            sprintf
                                "Type parameter '%s' in constraint clause is not declared in the enclosing scope"
                                name
                        )
                    )

        match c with
        | Constraint.Equality(typar = tp; equalityToken = tok) -> attach tp SemanticConstraintKind.Equality tok
        | Constraint.Comparison(typar = tp; comparisonToken = tok) -> attach tp SemanticConstraintKind.Comparison tok
        | Constraint.Struct(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.Struct tok
        | Constraint.ReferenceType(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.ReferenceType tok
        | Constraint.Nullness(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.Nullness tok
        | Constraint.NotNull(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.NotNull tok
        | Constraint.Coercion(typar = tp; colonGreaterThan = tok; typ = target) ->
            // `'a :> SomeType`: resolve the target now, while the typar scope is live, and
            // stamp a Coercion constraint on the typar's TyVar. Checked later by subsumption.
            attach tp (SemanticConstraintKind.Coercion(translateType ctx target)) tok
        | Constraint.DefaultConstructor(typar = tp; newToken = tok; resultTypar = result) ->
            // The constructed type is the constrained typar itself; `attach` reports an
            // undeclared one.
            match rootOf tp, rootOf result with
            | ValueSome constrained, ValueSome constructed when constrained.Id <> constructed.Id ->
                ctx.Report(tok, Kind.NewConstraintResultType)
            | ValueSome _, ValueNone -> ctx.Report(tok, Kind.NewConstraintResultType)
            | _ -> attach tp SemanticConstraintKind.DefaultConstructor tok
        | Constraint.Unmanaged(typar = tp; unmanagedToken = tok) -> attach tp SemanticConstraintKind.Unmanaged tok
        | Constraint.Enum(typar = tp; enumToken = tok; typ = underlying) ->
            attach tp (SemanticConstraintKind.Enum(translateType ctx underlying)) tok
        | Constraint.Delegate(delegateToken = tok) -> ctx.Report(tok, Kind.NotYetSupported "a 'delegate' constraint")
        | Constraint.MemberTrait _
        | Constraint.Default _ ->
            // A member trait is solved through the SRTP channel. A `default` clause reaches
            // `store.Defaults` only through an imported signature; a local one is dropped.
            ()

    /// The scope must already contain the constrained typars; callers seed it first.
    and translateConstraints (ctx: PassContext) (tcs: TyparConstraints<SyntaxToken>) : unit =
        for c in tcs.Constraints do
            translateConstraint ctx c

    /// `fill` over the abbreviation's RHS, with its typars and constraints in scope.
    and forceFill (ctx: PassContext) (info: AbbreviationInfo) : SemType voption =
        fill
            ctx
            info
            (fun () ->
                let scope = Dictionary<string, TyVarId>(System.StringComparer.Ordinal)

                for tp in info.TypeParams do
                    if not (scope.ContainsKey tp.Name) then
                        scope.[tp.Name] <- tp.TyVar

                use _ = ctx.PushTyparScope(scope, true)

                match info.TyparConstraints with
                | ValueSome cs -> translateConstraints ctx cs
                | ValueNone -> ()

                translateType ctx info.RhsCst
            )

    /// Group close: force every alias and measure body among `claims` into a terminal state,
    /// so an unreferenced body is filled and a cycle among them is reported.
    let forceGroupBodies (ctx: PassContext) (claims: TypeIdentity seq) : unit =
        for claim in claims do
            match claim.Kind with
            | TypeDeclKind.Abbreviation -> forceFill ctx (TypeRegistry.abbrevOfClaim ctx.Types claim) |> ignore
            | TypeDeclKind.Measure -> forceMeasureFill ctx (TypeRegistry.measureOfClaim ctx.Types claim) |> ignore
            | TypeDeclKind.Record
            | TypeDeclKind.Union
            | TypeDeclKind.Enum
            | TypeDeclKind.Class
            | TypeDeclKind.IntrinsicBinding -> ()
