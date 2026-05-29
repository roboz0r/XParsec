namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Generic
open System.Collections.Immutable
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationEngine

module UnificationTranslate =

    let enterLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel + 1

    let exitLevel (ctx: PassContext) : unit =
        ctx.CurrentLevel <- ctx.CurrentLevel - 1

    /// Fresh unkeyed TypeVar — for intermediate "result" TyVars not tied to
    /// a CST node's NodeKey.
    let freshTyVar (ctx: PassContext) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        tv

    /// Overwrites any prior entry — callers that need "get or allocate"
    /// (e.g. forward-referenced let-rec siblings) must go through `tvOf`.
    let freshTv (ctx: PassContext) (key: NodeKey) : TypeVar =
        let tv = TypeVar()
        tv.Level <- ctx.CurrentLevel
        ctx.Bindings.TypeVar.Set(key, tv)
        tv

    /// Get-or-allocate: fresh-allocates if missing — happens for binding-site
    /// patterns not yet visited by inferPat, including forward references
    /// inside `let rec` groups.
    let tvOf (ctx: PassContext) (key: NodeKey) : TypeVar =
        match ctx.Bindings.TypeVar.TryGetValue key with
        | ValueSome tv -> tv
        | ValueNone -> freshTv ctx key

    /// Report an error at `key` and recover with a fresh TyVar — the pervasive
    /// "diagnose and keep going" shape, so a broken subtree still yields a type
    /// rather than aborting the walk.
    let errorTy (ctx: PassContext) (key: NodeKey) (msg: string) : SemType =
        ctx.Error(key, msg)
        TyVar(freshTyVar ctx)

    /// Multi-segment qualified unit names (`Microsoft.FSharp.SI.kg`) and
    /// measure typars (`'u`) are v2 — they produce an empty term plus a
    /// diagnostic so the rest of inference continues without measure noise.
    let rec translateMeasure (ctx: PassContext) (diagKey: NodeKey) (m: Measure<SyntaxToken>) : MeasureTerm =
        match m with
        | Measure.One _ -> MeasureTerm.empty
        | Measure.Named li when li.Idents.Length = 1 -> MeasureTerm.ofList [ ctx.NameOf li.Idents.[0], Rational.One ]
        | Measure.Power(inner, _, neg, expTok) ->
            let n = System.Numerics.BigInteger.Parse(ctx.NameOf expTok)
            let signed = if neg.IsSome then -n else n

            MeasureTerm.pow
                (translateMeasure ctx diagKey inner)
                (Rational.create (signed, System.Numerics.BigInteger.One))
        | Measure.Product(l, _, r) -> MeasureTerm.mul (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Quotient(l, _, r) -> MeasureTerm.div (translateMeasure ctx diagKey l) (translateMeasure ctx diagKey r)
        | Measure.Reciprocal(_, inner) -> MeasureTerm.inv (translateMeasure ctx diagKey inner)
        | Measure.Paren(_, inner, _) -> translateMeasure ctx diagKey inner
        | Measure.Juxtaposition(elems, _) ->
            (MeasureTerm.empty, elems)
            ||> Seq.fold (fun acc m -> MeasureTerm.mul acc (translateMeasure ctx diagKey m))
        | Measure.Anonymous _
        | Measure.Typar _
        | Measure.Named _ ->
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = "Measure typars / wildcards / qualified unit names not yet supported"
                    Code = ""
                    Severity = Error
                }

            MeasureTerm.empty

    /// Built-in numeric type names that can carry a measure annotation
    /// (`float<m>`, `int<kg>`). User-defined `[<Measure>]`-aware types land
    /// when records / DUs do.
    let private isNumericCarrier (name: string) : bool =
        match name with
        | "int"
        | "int64"
        | "byte"
        | "float"
        | "float32"
        | "decimal"
        | "single"
        | "double" -> true
        | _ -> false

    /// Reads `ctx.Resolution.TyparScope` for `'a` typar resolution; callers open a
    /// fresh scope per signature (binding or type defn) before walking.
    /// Bare references to generic named types back-fill the arg list with
    /// fresh TyVars so unification can pin them.
    let rec translateType (ctx: PassContext) (t: Type<SyntaxToken>) : SemType =
        match t with
        | Type.ParenType(typ = inner) -> translateType ctx inner
        | Type.VarType(Typar.Named(ident = id))
        | Type.VarType(Typar.Static(ident = id)) ->
            let name = ctx.NameOf id

            match ctx.Resolution.TyparScope.TryGetValue name with
            | true, tv -> TyVar tv
            | false, _ ->
                if ctx.Resolution.TyparScopeStrict then
                    // Strict (type-defn fill-in): implicit free typars aren't
                    // legal F#. Diagnose, but still mint and memoise so later
                    // occurrences share the TyVar and don't cascade.
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken id NodeKind.TypeVarRef
                            Message =
                                sprintf
                                    "Free type parameter %s is not declared in the enclosing type's type-parameter list"
                                    name
                            Code = ""
                            Severity = Error
                        }

                    let tv = TypeVar()
                    tv.Level <- ctx.CurrentLevel
                    ctx.Resolution.TyparScope.[name] <- tv
                    TyVar tv
                else
                    // Implicit typar: mint at the binding's current level so
                    // generalisation at binding-group exit picks it up;
                    // memoise so later occurrences share identity.
                    let tv = TypeVar()
                    tv.Level <- ctx.CurrentLevel
                    ctx.Resolution.TyparScope.[name] <- tv
                    TyVar tv
        | Type.VarType(Typar.Anon _) ->
            // `_` typar — always fresh, never stored. Distinct per
            // occurrence, same as `Pat.Wildcard`.
            TyVar(freshTyVar ctx)
        | Type.NamedType li when li.Idents.Length = 1 ->
            let name = ctx.NameOf li.Idents.[0]

            match name with
            | "int" -> BuiltinTypes.tyInt
            | "bool" -> BuiltinTypes.tyBool
            | "unit" -> BuiltinTypes.tyUnit
            | "float" -> BuiltinTypes.tyFloat
            | "string" -> BuiltinTypes.tyString
            | "int64" -> BuiltinTypes.tyInt64
            | "byte" -> BuiltinTypes.tyByte
            | _ when ctx.Types.IntrinsicReprTypes.ContainsKey name ->
                // Primitive binding (`type int = (# "System.Int32" #)`): a
                // nominal intrinsic, NOT a transparent abbreviation. Resolve to
                // `TyConst name`; the representation string is consumed later by
                // the codegen `encodeType` rekey. See docs/self-host-rung1-plan.md.
                TyConst name
            | _ ->
                match ctx.Types.Abbreviation.TryGetValue name with
                | true, info ->
                    // Eager expansion: force the body, then substitute fresh
                    // TyVars for every declared typar.
                    forceFill ctx info

                    let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))

                    let diagKey = NodeKey.ofToken li.Idents.[0] NodeKind.TypeNamed
                    expandAbbreviation ctx diagKey info args
                | false, _ ->
                    match ctx.Types.Record.TryGetValue name with
                    | true, info ->
                        // Back-fill generic args with fresh TyVars at the
                        // current level — unpinned at the declaration site,
                        // fixed by surrounding unification (e.g. `r : Box`
                        // unifies the args with whatever `r`'s usage pins).
                        let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))

                        TyRecord(name, args)
                    | false, _ ->
                        match ctx.Types.Union.TryGetValue name with
                        | true, info ->
                            let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))

                            TyUnion(name, args)
                        | false, _ ->
                            match ctx.Types.Class.TryGetValue name with
                            | true, info ->
                                let args = EqArray.init (info.TypeParams.Length) (fun _ -> TyVar(freshTyVar ctx))

                                TyClass(name, args)
                            | false, _ ->
                                // Not project-local: probe the external provider
                                // (a short BCL name under its `open`) before the
                                // opaque fallback. See `tryResolveExternalType`.
                                match tryResolveExternalType ctx name EqArray.empty with
                                | ValueSome ty -> ty
                                | ValueNone -> TyConst name
        | Type.NamedType li ->
            // Multi-segment named type (`System.Text.StringBuilder`). Project-local
            // types are single-segment, so a dotted name is either external or
            // unknown; probe the provider before the catch-all TyVar.
            let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

            match tryResolveExternalType ctx qualName EqArray.empty with
            | ValueSome ty -> ty
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.GenericType(longIdent = li; typeArgs = args) when
            li.Idents.Length = 1
            && args.Length = 1
            && isNumericCarrier (ctx.NameOf li.Idents.[0])
            ->
            // `float<m>` / `int<kg>` — stamp the measure onto a fresh TyVar
            // whose Link carries the carrier.
            //
            // The parser only tags an arg as `TypeArg.Measure` when the
            // measure grammar is unambiguous; for bare `float<m>` it lands
            // as `TypeArg.Type (Type.NamedType "m")` because the type
            // grammar can't tell unit names apart from type-arg type names.
            // Both shapes resolve here.
            let carrierTok = li.Idents.[0]
            let diagKey = NodeKey.ofToken carrierTok NodeKind.TypeGeneric

            let measureFromTypeArg =
                match args.[0] with
                | TypeArg.Measure m -> ValueSome m
                | TypeArg.Type(Type.NamedType nameLi) ->
                    // Reinterpret a single-segment named type as a measure
                    // atom; multi-segment qualifiers stay a real type.
                    if nameLi.Idents.Length = 1 then
                        ValueSome(Measure.Named nameLi)
                    else
                        ValueNone
                | _ -> ValueNone

            match measureFromTypeArg with
            | ValueSome m ->
                let mt = translateMeasure ctx diagKey m
                let tv = freshTyVar ctx
                tv.Link <- ValueSome(translateType ctx (Type.NamedType li))
                tv.Units <- ValueSome mt
                TyVar tv
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.GenericType(longIdent = li; typeArgs = args) when li.Idents.Length = 1 ->
            let nameTok = li.Idents.[0]
            let name = ctx.NameOf nameTok
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeGeneric

            let translatedArgs =
                EqArray.ofSeq (
                    seq {
                        for a in args ->
                            match a with
                            | TypeArg.Type t -> translateType ctx t
                            // A measure-shaped arg landing on a non-numeric
                            // carrier shouldn't happen in well-formed code,
                            // but stay total — emit a free TyVar.
                            | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                    }
                )

            resolveNamedGeneric ctx diagKey name translatedArgs
        | Type.GenericType(longIdent = li; typeArgs = args) ->
            // Multi-segment generic type
            // (`System.Collections.Generic.EqualityComparer<int>`); the
            // single-segment forms are handled above.
            let qualName = li.Idents |> Seq.map ctx.NameOf |> String.concat "."

            let translatedArgs =
                EqArray.ofSeq (
                    seq {
                        for a in args ->
                            match a with
                            | TypeArg.Type t -> translateType ctx t
                            | TypeArg.Measure _ -> TyVar(freshTyVar ctx)
                    }
                )

            match tryResolveExternalType ctx qualName translatedArgs with
            | ValueSome ty -> ty
            | ValueNone -> TyVar(freshTyVar ctx)
        | Type.SuffixedType(baseType = baseTy; longIdent = li) when li.Idents.Length = 1 ->
            // Postfix generic syntax: `'T list` ≡ `list<'T>`. Multi-arg
            // postfix forms (`(int, string) Map`) parse the base as a tuple
            // and fall to the single-arg arity diagnostic — out of scope for v1.
            let nameTok = li.Idents.[0]
            let name = ctx.NameOf nameTok
            let diagKey = NodeKey.ofToken nameTok NodeKind.TypeGeneric
            resolveNamedGeneric ctx diagKey name (EqArray.singleton (translateType ctx baseTy))
        | Type.FunctionType(fromType = from; toType = into) -> TyFun(translateType ctx from, translateType ctx into)
        | Type.TupleType(types = types) -> TyTuple(EqArray.ofSeq (seq { for t in types -> translateType ctx t }))
        | Type.WhenConstrainedType(typ = inner; constraints = cs) ->
            let inner = translateType ctx inner
            translateConstraints ctx cs
            inner
        | _ ->
            // Multi-segment named/generic types and other shapes (arrays,
            // anonymous records, etc.) aren't modelled yet. Hand back a free
            // TyVar so unification can pin it via context.
            TyVar(freshTyVar ctx)

    /// Resolve a single-segment generic type reference against the type
    /// registries, in the same precedence the bare-name arm uses: intrinsic
    /// binding → transparent abbreviation → record → union → class → opaque
    /// `TyConst`. An arity mismatch diagnoses but still produces a
    /// best-effort shape.
    and private resolveNamedGeneric
        (ctx: PassContext)
        (diagKey: NodeKey)
        (name: string)
        (translatedArgs: EqArray<SemType>)
        : SemType =
        let argCount = translatedArgs.Length

        let diagnoseArity (expected: int) : unit =
            ctx.Diagnostics.Add
                {
                    Key = diagKey
                    Message = sprintf "Type '%s' expects %d type argument(s) but got %d" name expected argCount
                    Code = ""
                    Severity = Error
                }

        let checkArity (expected: int) : unit =
            if expected <> argCount then
                diagnoseArity expected

        if ctx.Types.IntrinsicReprTypes.ContainsKey name then
            // Generic primitive binding: nominal, not transparent.
            TyConst name
        else
            match ctx.Types.Abbreviation.TryGetValue name with
            | true, info ->
                forceFill ctx info
                checkArity (info.TypeParams.Length)
                expandAbbreviation ctx diagKey info translatedArgs
            | false, _ ->
                match ctx.Types.Record.TryGetValue name with
                | true, info ->
                    checkArity (info.TypeParams.Length)
                    TyRecord(name, translatedArgs)
                | false, _ ->
                    match ctx.Types.Union.TryGetValue name with
                    | true, info ->
                        checkArity (info.TypeParams.Length)
                        TyUnion(name, translatedArgs)
                    | false, _ ->
                        match ctx.Types.Class.TryGetValue name with
                        | true, info ->
                            checkArity (info.TypeParams.Length)
                            TyClass(name, translatedArgs)
                        | false, _ ->
                            match tryResolveExternalType ctx name translatedArgs with
                            | ValueSome ty -> ty
                            | ValueNone ->
                                // Unknown name with type args — opaque TyConst,
                                // args ignored (matches the bare-name arm).
                                TyConst name

    /// Resolve a named/generic type reference that missed every project-local
    /// registry against the external provider — the type-annotation analogue of
    /// `tryExternalTypeReceiver` (which only typed static-member *receivers*, so a
    /// `(c : EqualityComparer<int>)` annotation used to land as an opaque
    /// `TyConst`). A short name resolves through `OpenScope` exactly like that
    /// sibling, so `EqualityComparer<int>` under `open System.Collections.Generic`
    /// reaches the qualified metadata name. The resolved provider key *is* the
    /// canonical SemType name — the same name member signatures and list literals
    /// carry — so the annotation unifies with the resolved receiver type. Two
    /// keying conventions coexist: the metadata (BCL) layer keys generic types by
    /// their arity-suffixed name (`` EqualityComparer`1 ``), the contract layer by
    /// the bare compiled name, so both forms are probed and the hit's key becomes
    /// the SemType name. An arity-mismatched hit is rejected (a generic type
    /// referenced at the wrong arity isn't this type, and guards the abbrev/record
    /// builders against a wrong-length arg array). Abbreviations are left to the
    /// caller's opaque fallback rather than expanded here — expanding would discard
    /// the abbrev name the extractor convention pins (symbol-resolution-handoff.md).
    and private tryResolveExternalType
        (ctx: PassContext)
        (qualName: string)
        (translatedArgs: EqArray<SemType>)
        : SemType voption =
        let arity = translatedArgs.Length

        // Metadata keys generic types `Name`arity`; the contract layer keys them
        // bare. Probe the suffixed form first so it wins when both could match.
        let keysFor (n: string) : string list =
            if arity = 0 then [ n ] else [ sprintf "%s`%d" n arity; n ]

        let shapeArity (shape: ExternalTypeShape) : int =
            match shape with
            | ExternalTypeShape.Class info -> info.Arity
            | ExternalTypeShape.Record(arity = a)
            | ExternalTypeShape.Union(arity = a)
            | ExternalTypeShape.Abbrev(arity = a) -> a

        let lookup (candidate: string) : SemType voption =
            let picked =
                keysFor candidate
                |> List.tryPick (fun key ->
                    match ctx.Provider.TryLookupType key with
                    | ValueSome shape when shapeArity shape = arity ->
                        match shape with
                        | ExternalTypeShape.Class _ -> Some(TyClass(key, translatedArgs))
                        | ExternalTypeShape.Record _ -> Some(TyRecord(key, translatedArgs))
                        | ExternalTypeShape.Union _ -> Some(TyUnion(key, translatedArgs))
                        // A transparent abbreviation dealiases to its body: `int32 =
                        // int` (`int = (# "System.Int32" #)`) resolves to `TyConst
                        // "int"`, the form codegen actually encodes — without this an
                        // abbrev name (`int32`) leaked through as a nominal `TyConst
                        // "int32"` the IL encoder doesn't key. Mirrors the *local*
                        // abbrev expansion (`expandAbbreviation`); the `build` closure
                        // substitutes the type args into the (already-translated) RHS.
                        | ExternalTypeShape.Abbrev(_, build) -> Some(build (translatedArgs.AsSpan().ToArray()))
                    | _ -> None
                )

            match picked with
            | Some ty -> ValueSome ty
            | None -> ValueNone

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup qualName

    /// Attach to the constrained typar's TyVar through the current
    /// `ctx.Resolution.TyparScope`. Unsupported kinds (Coercion, MemberTrait, etc.) are
    /// skipped — they belong to their own resolution phases.
    and private translateConstraint (ctx: PassContext) (c: Constraint<SyntaxToken>) : unit =
        let typarTokenOf (t: Typar<SyntaxToken>) : SyntaxToken voption =
            match t with
            | Typar.Named(ident = id)
            | Typar.Static(ident = id) -> ValueSome id
            | Typar.Anon _ -> ValueNone

        let attach (typar: Typar<SyntaxToken>) (kind: SemanticConstraintKind) (declTok: SyntaxToken) : unit =
            match typarTokenOf typar with
            | ValueNone -> ()
            | ValueSome id ->
                let name = ctx.NameOf id

                match ctx.Resolution.TyparScope.TryGetValue name with
                | true, tv ->
                    let root = UnionFind.find tv

                    let sc =
                        {
                            Kind = kind
                            DeclKey = NodeKey.ofToken declTok NodeKind.TypeVarRef
                        }

                    if not (root.Constraints |> List.exists (fun e -> e.Kind = sc.Kind)) then
                        root.Constraints <- sc :: root.Constraints
                | false, _ ->
                    ctx.Diagnostics.Add
                        {
                            Key = NodeKey.ofToken id NodeKind.TypeVarRef
                            Message =
                                sprintf
                                    "Type parameter '%s' in constraint clause is not declared in the enclosing scope"
                                    name
                            Code = ""
                            Severity = Error
                        }

        match c with
        | Constraint.Equality(typar = tp; equalityToken = tok) -> attach tp SemanticConstraintKind.Equality tok
        | Constraint.Comparison(typar = tp; comparisonToken = tok) -> attach tp SemanticConstraintKind.Comparison tok
        | Constraint.Struct(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.Struct tok
        | Constraint.ReferenceType(typar = tp; structToken = tok) -> attach tp SemanticConstraintKind.ReferenceType tok
        | Constraint.Nullness(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.Nullness tok
        | Constraint.NotNull(typar = tp; nullToken = tok) -> attach tp SemanticConstraintKind.NotNull tok
        | Constraint.Coercion _
        | Constraint.MemberTrait _
        | Constraint.DefaultConstructor _
        | Constraint.Enum _
        | Constraint.Unmanaged _
        | Constraint.Delegate _
        | Constraint.Default _ ->
            // v1 skips these — each has its own resolution phase (SRTPs /
            // IWSAMs / attribute pass). Silent skip, not a diagnostic.
            ()

    /// The scope must already contain the constrained typars — callers
    /// (binding-level, type-defn fill-in, inline `WhenConstrainedType`)
    /// seed it first.
    and translateConstraints (ctx: PassContext) (tcs: TyparConstraints<SyntaxToken>) : unit =
        let (TyparConstraints(constraints = cs)) = tcs

        for c in cs do
            translateConstraint ctx c

    /// Idempotent — already-`Filled` entries short-circuit. Re-entry through
    /// a recursive abbreviation reference detects the cycle (`InProgress`),
    /// emits a diagnostic, and freezes `Status` to `Filled` without setting
    /// `Body`. The outer call notices `Status` flipped mid-walk and skips
    /// assigning `Body`, leaving `ValueNone` so the expansion arm
    /// substitutes a fresh TyVar per use site instead of a stale one.
    and forceFill (ctx: PassContext) (info: AbbreviationInfo) : unit =
        match info.Status with
        | AbbreviationStatus.Filled -> ()
        | AbbreviationStatus.InProgress ->
            ctx.Diagnostics.Add
                {
                    Key = info.DeclKey
                    Message = sprintf "Type abbreviation '%s' is cyclic" info.Name
                    Code = ""
                    Severity = Error
                }

            info.Status <- AbbreviationStatus.Filled
        | AbbreviationStatus.NotFilled ->
            info.Status <- AbbreviationStatus.InProgress
            let savedScope = ctx.Resolution.TyparScope
            let savedStrict = ctx.Resolution.TyparScopeStrict
            let scope = Dictionary<string, TypeVar>(System.StringComparer.Ordinal)

            for (n, tv) in info.TypeParams do
                if not (scope.ContainsKey n) then
                    scope.[n] <- tv

            ctx.Resolution.TyparScope <- scope
            ctx.Resolution.TyparScopeStrict <- true

            try
                match info.TyparConstraints with
                | ValueSome cs -> translateConstraints ctx cs
                | ValueNone -> ()

                let body = translateType ctx info.RhsCst

                if info.Status = AbbreviationStatus.InProgress then
                    info.Body <- ValueSome body
            finally
                ctx.Resolution.TyparScope <- savedScope
                ctx.Resolution.TyparScopeStrict <- savedStrict
                info.Status <- AbbreviationStatus.Filled

    /// Returns a fresh TyVar if `Body = ValueNone` (cycle detected, or
    /// fill-in not yet run) — best-effort rather than cascading.
    ///
    /// Constraints on the prototype typars are evaluated against the supplied
    /// args here: unlike records / unions, an abbreviation has no
    /// fresh-instance step that would let `drainConstraints` fire on its own.
    /// A Defer outcome propagates the constraint to any free TyVar inside the
    /// supplied arg so a later unification re-fires the check.
    and expandAbbreviation
        (ctx: PassContext)
        (diagKey: NodeKey)
        (info: AbbreviationInfo)
        (args: EqArray<SemType>)
        : SemType =
        let n = min (info.TypeParams.Length) args.Length

        for i = 0 to n - 1 do
            let (_, protoTv) = info.TypeParams.[i]
            let arg = args.[i]
            let protoRoot = UnionFind.find protoTv

            for c in protoRoot.Constraints do
                match checkConstraint ctx c arg with
                | Satisfied -> ()
                | Violated ->
                    ctx.Diagnostics.Add
                        {
                            Key = diagKey
                            Message =
                                sprintf
                                    "The type '%A' does not support the '%s' constraint"
                                    (zonk arg)
                                    (constraintKindName c.Kind)
                            Code = ""
                            Severity = Error
                        }
                | Defer -> propagateToFreeArgs ctx c arg

        match info.Body with
        | ValueSome body -> instantiateMember (info.TypeParams, args) body
        | ValueNone -> TyVar(freshTyVar ctx)
