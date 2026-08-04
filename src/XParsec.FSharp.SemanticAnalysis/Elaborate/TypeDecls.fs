namespace XParsec.FSharp.SemanticAnalysis

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.ElaborateResolve
open XParsec.FSharp.SemanticAnalysis.ElaborateExprArgs
open XParsec.FSharp.SemanticAnalysis.ElaborateExpr
open XParsec.FSharp.SemanticAnalysis.ElaborateTypars
open XParsec.FSharp.SemanticAnalysis.ElaborateMembers
open XParsec.FSharp.SemanticAnalysis.ElaborateClassMembers

// Type-declaration surfacing for the Elaborate pass: one `try*Type` per host kind,
// each resolving its registered `*TypeInfo` and projecting it onto a `TDecl.Type`,
// paired with the typar env the decl quantifies. Every one leaves its types
// `TyVar`-shaped; the caller makes the cut with that env.

module internal ElaborateTypeDecls =

    /// The declared `access` keyword token of a `type` definition, off its
    /// `TypeName`. Every `TypeName`-headed `TypeDefn` variant carries it; the
    /// remainder (a bare delegate/exception form) reports absence (⇒ `Public`).
    let typeDefnAccessToken (td: TypeDefn<SyntaxToken>) : SyntaxToken voption =
        let ofTn (TypeName(_, access, _, _, _, _)) = access

        match td with
        | TypeDefn.Anon(typeName = tn)
        | TypeDefn.Interface(typeName = tn)
        | TypeDefn.Class(typeName = tn)
        | TypeDefn.Struct(typeName = tn)
        | TypeDefn.Union(typeName = tn)
        | TypeDefn.Record(typeName = tn)
        | TypeDefn.Enum(typeName = tn)
        | TypeDefn.Abbrev(typeName = tn) -> ofTn tn
        | _ -> ValueNone

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
        : (EqArray<string> * EqArray<TAbstractMethod> * (TyVarId * SemType) list) option =
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
                let markers = mkDeclTyparEnv ctx.Store info.TypeParams
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
                                if GeneralizedTypars.count m.CanonicalTypars > 0 then
                                    env.AddRange(GeneralizedTypars.methodEnv m.CanonicalTypars)

                                yield
                                    {
                                        Name = m.Name
                                        MethodTypeParams = EqArray.ofArray (GeneralizedTypars.names m.CanonicalTypars)
                                        Signature = m.Type
                                        IsProperty = (m.Kind = ClassMemberKind.Property)
                                    }
                        }
                    )

                Some(EqArray.ofSeq (seq { for (n, _) in info.TypeParams -> n }), methods, List.ofSeq env)

    /// Build the `TDecl.Type` wrapper shared by record / union / interface
    /// (and the upcoming class) surfacers — same five-field shape, only `Kind`
    /// differs. `typars` is the already-projected typar-name list (`info` /
    /// `tryInterfaceMethods` projections both flow through here unchanged).
    let private mkTypeDecl
        (name: string)
        (key: TypeKey)
        (ns: string option)
        (typars: EqArray<string>)
        (rqa: bool)
        (kind: TTypeKind)
        (eq: EqualityVerdict)
        (cmp: ComparisonVerdict)
        : TDecl =
        TDecl.Type
            {
                Name = name
                TypeKey = key
                Namespace = ns
                TypeParams = typars
                IsRequireQualifiedAccess = rqa
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
        : (TDecl * (TyVarId * SemType) list) option =
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
            let markers = mkDeclTyparEnv ctx.Store info.TypeParams
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

            let selfTy = TyUnion(info.TypeKey, declTyparArgs ctx.Store info.TypeParams)
            let elaborateOne = mkMemberElaborator selfTy declTypars env

            let members, interfaces =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            Some(
                mkTypeDecl
                    name
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    info.IsRequireQualifiedAccess
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
                ctx.Report(
                    idTok,
                    Kind.Message
                        "An enum case value is not representable at its authored width (a negative value has no unsigned representation)"
                )

                ValueNone
            | Error ConstRejection.CustomLiteral ->
                ctx.Report(
                    idTok,
                    Kind.Message
                        "An enum case value must be a primitive integer literal; a custom numeric literal ('52I') is a call to a NumericLiteral module, not a constant"
                )

                ValueNone
            | Ok other ->
                ctx.Report(
                    idTok,
                    Kind.Message(
                        sprintf
                            "An enum case value must be an integer or string literal; '%A' is not a valid enum constant"
                            other
                    )
                )

                ValueNone
        | Expr.String _ ->
            // Plain / verbatim / triple-quoted string literals are constants (the
            // shared projection folds them); an interpolated string ($"…") is the
            // only String kind it declines — reject that as non-literal.
            match StringLiterals.tryEnumCaseStringLiteral ctx v with
            | ValueSome s -> ValueSome(TEnumLiteral.String s)
            | ValueNone ->
                ctx.Report(
                    idTok,
                    Kind.Message "An enum case value must be a literal string; an interpolated string is not a constant"
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
                ctx.Report(
                    idTok,
                    Kind.Message "A negative enum case value has no unsigned representation; use a signed integer width"
                )

                ValueNone
            // `-"abc"` or a deeper non-int form: the inner resolution produced a
            // non-negatable shape (the `Int _` arm is unreachable — handled above —
            // but kept for exhaustiveness). Reject.
            | ValueSome(TEnumLiteral.String _)
            | ValueSome(TEnumLiteral.Int _) ->
                ctx.Report(idTok, Kind.EnumCaseNotConstant)

                ValueNone
            | ValueNone -> ValueNone
        | _ ->
            ctx.Report(idTok, Kind.EnumCaseNotConstant)

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
        : (TDecl * (TyVarId * SemType) list) option =
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

            ctx.Report(firstId, Kind.HeterogeneousEnum name)
        | _ -> ()

        // CLR uniform-width invariant: a `System.Enum` has exactly one underlying
        // integral type, so explicitly-suffixed cases of differing width
        // (`| A = 1uy | B = 2L`) are a hard error. Unsuffixed `Int` cases are
        // width-flexible (they adopt the single explicit width present) and never
        // conflict; string / mixed enums carry no integral width. Reported at the
        // offending case's token, via the same diagnostic channel.
        match TEnumCases.firstWidthConflict tcases with
        | ValueSome(tok, w0, w1) ->
            ctx.Report(
                tok,
                Kind.Message(
                    sprintf
                        "Enum '%s' mixes integral widths '%s' and '%s'; a CLR enum has a single underlying type"
                        name
                        w0
                        w1
                )
            )
        | ValueNone -> ()

        Some(
            mkTypeDecl
                name
                key
                ns
                (EqArray.ofList [])
                // An enum's cases are always qualified (`E.C1`); RQA adds nothing,
                // so the flag is `false` and unread for this kind.
                false
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
        : (TDecl * (TyVarId * SemType) list) option =
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
            let markers = mkDeclTyparEnv ctx.Store info.TypeParams
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
            let selfTy = TyRecord(info.TypeKey, declTyparArgs ctx.Store info.TypeParams)
            let elaborateOne = mkMemberElaborator selfTy declTypars env

            let members, interfaces =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            // `[<Struct>]` record ⇒ value-type emission; a record is never
            // byref-like, so the only two verdicts are `Struct` / `RefType`.
            let valueKind =
                if info.IsValueType then
                    ClassValueKind.Struct
                else
                    ClassValueKind.RefType

            Some(
                mkTypeDecl
                    name
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    info.IsRequireQualifiedAccess
                    (TTypeKind.Record(fields, members, interfaces, valueKind))
                    info.EqualitySupport
                    info.ComparisonSupport,
                List.ofSeq env
            )

    /// Surface a `TypeDefn.Class` (or class-shaped `TypeDefn.Anon`) as a
    /// `TDecl.Type` from the resolved `ClassTypeInfo`. Ctor params and member
    /// signatures are remapped through the declaring-type typars (the same
    /// `mkDeclTyparEnv` + `remapDeclTypars` pipeline records / unions use).
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
        : (TDecl * (TyVarId * SemType) list) option =
        // The key of the type being LOWERED, minted from the module the walk is in — not a
        // by-name read. This is the declaration itself: a sibling module's same-named class
        // is a different type, and an arity-overloaded `Box\`1`/`Box\`2` does not resolve by
        // bare name at all.
        match TypeRegistry.tryClassByKey ctx.Types (ctx.DeclaredTypeKey(name, arity)) with
        | ValueNone -> None
        | ValueSome info ->
            let markers = mkDeclTyparEnv ctx.Store info.TypeParams
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

            let selfTy = TyClass(info.TypeKey, declTyparArgs ctx.Store info.TypeParams)

            // Surface a member when the declaring type is generic (declaring axis)
            // *or* the member itself is generic (method axis): stamp its
            // self-type and fold its method typars into the decl env, so
            // `freezeTypars` later flips both axes. A generic method on a
            // *monomorphic* class still needs its `'C` cut to `TyTypar(Method, i)`,
            // so it can't be skipped. For a mono type with a
            // mono member, `selfTy = TyClass(key, [])` equals the member's existing
            // `ThisTy`, so leaving it verbatim is byte-identical.
            let needsRemap (m: TTypeMember) =
                not (List.isEmpty declTypars) || m.MethodTypeParams.Length > 0

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

            let staticRewrite = staticFieldRewrite info
            let instanceRewrite = instanceFieldRewrite info selfTy

            let translatePreambleEntry (rewrite: TExpr -> TExpr) (entry: ClassPreambleEntry) : TPreambleEntry =
                match entry with
                | ClassPreambleEntry.Let l ->
                    TPreambleEntry.Let
                        {
                            Name = l.Name
                            Type = Unification.zonk ctx.Store l.Type
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
                        EqArray.ofSeq (
                            seq { for p in info.CtorParams -> (p.DeclSite.Binder, Unification.zonk ctx.Store p.Type) }
                        )

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
                            // The base ctor's chosen identity, recorded by
                            // `Unification.fillBaseCtorCall` under the args expr (an external
                            // base like `inherit exn(msg)`); `ValueNone` for a project-local base.
                            ChosenCtor = ctx.Resolution.ExternalCtor.TryGetValue(CstKeys.ofExpr argExpr)
                        }
                | _ -> ValueNone

            Some(
                mkTypeDecl
                    name
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    // RQA on a class gates only its unqualified module-member access,
                    // which is not modelled here; a class is never bare-constructed by
                    // field set, so the flag is unread for this kind.
                    false
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
    /// INTERNAL artifact consumed only by the member-inline lifting (a concrete
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
    /// (e.g. `PlatformTypes`' `Record | Union` arm) for no gain. The lifting itself is
    /// kind-agnostic (`TTypeKindG.members`), so the choice is purely which container is
    /// safest to carry inert.
    let private tryIntrinsicAbbrevType
        (ctx: PassContext)
        (ns: string option)
        (name: string)
        (ext: TypeExtensionElements<SyntaxToken> voption)
        : (TDecl * (TyVarId * SemType) list) option =
        match ctx.Types.IntrinsicAbbrevHost.TryGetValue name with
        | false, _ -> None
        | true, info ->
            let markers = mkDeclTyparEnv ctx.Store info.TypeParams
            let env = ResizeArray markers
            let declTypars = [ for (n, _) in info.TypeParams -> n ]
            let selfTy = TyConst(info.SelfKey, declTyparArgs ctx.Store info.TypeParams)
            let elaborateOne = mkMemberElaborator selfTy declTypars env

            let members, _ =
                elaborateHostMembers ctx (info :> IInterfaceImplHost) ext elaborateOne

            let clsG: TClass =
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
                    info.TypeKey
                    ns
                    (EqArray.ofList declTypars)
                    false
                    (TTypeKind.Class clsG)
                    EqualityVerdict.Reference
                    ComparisonVerdict.NoComparison,
                List.ofSeq env
            )

    /// Surface an interface-shaped, union, record, or class `TypeDefn` as a
    /// `TDecl.Type`. A plain abbreviation surfaces nothing; an inline intrinsic-abbrev
    /// carrying a `with member …` augmentation surfaces its members (lift-only).
    let tryTypeDecl
        (ctx: PassContext)
        (c: DeclContainment<SyntaxToken>)
        (td: TypeDefn<SyntaxToken>)
        : (TDecl * (TyVarId * SemType) list) option =
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
                        // An interface is not bare-constructed by field set; RQA is
                        // unread for this kind.
                        false
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
        // inline intrinsic-abbrev with `with member …` surfaces its members (lift-only).
        | TypeDefn.Abbrev(typeName = tn; extensions = ext) -> tryIntrinsicAbbrevType ctx ns (typeNameSimple ctx tn) ext
        | _ -> None
