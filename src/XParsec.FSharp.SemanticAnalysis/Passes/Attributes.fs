namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Each attribute's long-ident is RESOLVED as a type and compared by `TypeKey`, so a user type
// named `ReferenceEqualityAttribute` elsewhere keeps its own meaning, and an ABBREVIATION
// of a marker resolves to the alias' OWN identity, so it is ignored.

module Attributes =

    /// The axis the attribute-legality matrices key on: one case per shape a declaration can
    /// take, because a record may carry `[<ReferenceEquality>]` where a class cannot.
    [<RequireQualifiedAccess>]
    type TypeDefnKind =
        | Record
        | Union
        | Enum
        | Abbrev
        | Struct
        | RefClass
        | Interface

    /// What a declaration's attributes decided, each already checked legal for its kind: an
    /// attribute this kind may not carry supplies no verdict, only its complaint.
    type TypeDefnAttrVerdict =
        {
            /// `ValueNone` where no legal attribute spoke, leaving the caller's per-kind default.
            Equality: EqualityVerdict voption
            Comparison: ComparisonVerdict voption
            AllowNullLiteral: bool
        }

    type private EqCompAttr<'Verdict> =
        {
            Key: TypeKey
            Verdict: 'Verdict
            LegalKinds: TypeDefnKind list
            OnWrongKind: Kind
        }

    let private anyKind =
        [
            TypeDefnKind.Record
            TypeDefnKind.Union
            TypeDefnKind.Enum
            TypeDefnKind.Abbrev
            TypeDefnKind.Struct
            TypeDefnKind.RefClass
            TypeDefnKind.Interface
        ]

    /// A structural posture states what the FIELDS decide, so only the kinds that have them.
    let private structuralKinds =
        [ TypeDefnKind.Record; TypeDefnKind.Union; TypeDefnKind.Struct ]

    /// Reference identity additionally bars a struct, which has none.
    let private referenceKinds = [ TypeDefnKind.Record; TypeDefnKind.Union ]

    /// A custom posture needs members to carry it, which an interface cannot declare.
    let private customKinds = anyKind |> List.except [ TypeDefnKind.Interface ]

    /// FS0934. `[<AllowNullLiteral>]` states that `null` inhabits the type, which only a
    /// reference can hold. A record or union reaches `null` through `| null` instead, a struct
    /// (an enum included) has no reference to hold it, and an abbreviation states nothing.
    let private allowNullLiteralKinds =
        [ TypeDefnKind.RefClass; TypeDefnKind.Interface ]

    /// The equality axis. Table ORDER is the within-axis verdict priority.
    let private equalityAttrs: EqCompAttr<EqualityVerdict> list =
        [
            {
                Key = RuntimeNames.structuralEqualityAttributeKey
                Verdict = EqualityVerdict.Structural
                LegalKinds = structuralKinds
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.referenceEqualityAttributeKey
                Verdict = EqualityVerdict.Reference
                LegalKinds = referenceKinds
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.noEqualityAttributeKey
                Verdict = EqualityVerdict.NoEquality
                LegalKinds = anyKind
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.customEqualityAttributeKey
                Verdict = EqualityVerdict.Custom
                LegalKinds = customKinds
                OnWrongKind = Kind.CustomEqualityAttributeOnInterface
            }
        ]

    /// Likewise in priority order. Comparison is OPT-IN where equality is not:
    /// `[<StructuralComparison>]` is what buys a record the synthesised pair.
    let private comparisonAttrs: EqCompAttr<ComparisonVerdict> list =
        [
            {
                Key = RuntimeNames.structuralComparisonAttributeKey
                Verdict = ComparisonVerdict.Structural
                LegalKinds = structuralKinds
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.noComparisonAttributeKey
                Verdict = ComparisonVerdict.NoComparison
                LegalKinds = anyKind
                OnWrongKind = Kind.StructuralEqualityAttributeOnWrongKind
            }
            {
                Key = RuntimeNames.customComparisonAttributeKey
                Verdict = ComparisonVerdict.Custom
                LegalKinds = customKinds
                OnWrongKind = Kind.CustomEqualityAttributeOnInterface
            }
        ]

    /// No first-wins short-circuit, so a contradictory mix stays visible to the FS0377 check.
    let private presentAttrs
        (a: NameResolutionTypeRefStamp.ResolvedAttributes)
        (rows: EqCompAttr<'Verdict> list)
        : EqCompAttr<'Verdict> list =
        rows |> List.filter (fun r -> a.Has r.Key)

    let private isLegalOn (kind: TypeDefnKind) (r: EqCompAttr<'Verdict>) : bool = List.contains kind r.LegalKinds

    let private legalOn (kind: TypeDefnKind) (rows: EqCompAttr<'Verdict> list) : EqCompAttr<'Verdict> list =
        rows |> List.filter (isLegalOn kind)

    let private wrongKindDiagnostics (kind: TypeDefnKind) (rows: EqCompAttr<'Verdict> list) : Kind list =
        rows
        |> List.filter (fun r -> not (isLegalOn kind r))
        |> List.map (fun r -> r.OnWrongKind)

    /// Every attribute-against-kind check a type declaration gets: FS0382 legality + FS0377
    /// invalid-mix on the equality / comparison axes, and FS0934 on `[<AllowNullLiteral>]`,
    /// all reported at `declTok`.
    let validateTypeDefnAttributes
        (ctx: PassContext)
        (kind: TypeDefnKind)
        (declTok: SyntaxToken)
        (attrs: Attributes<SyntaxToken> voption)
        : TypeDefnAttrVerdict =
        let a = NameResolutionTypeRefStamp.resolveAttributes ctx attrs
        let eq = presentAttrs a equalityAttrs
        let cmp = presentAttrs a comparisonAttrs

        // Once per distinct complaint: two attributes illegal the same way are one mistake.
        for d in List.distinct (wrongKindDiagnostics kind eq @ wrongKindDiagnostics kind cmp) do
            ctx.Report(declTok, d)

        // ABSENCE of an equality attribute is no contradiction: every kind surviving FS0382
        // defaults to structural equality, so `[<StructuralComparison>]` alone is valid.
        let structuralCmp =
            cmp |> List.exists (fun r -> r.Verdict = ComparisonVerdict.Structural)

        let nonStructuralEq =
            eq |> List.exists (fun r -> r.Verdict <> EqualityVerdict.Structural)

        if eq.Length > 1 || cmp.Length > 1 || (structuralCmp && nonStructuralEq) then
            ctx.Report(declTok, Kind.InvalidEqualityAttributeMix)

        // Off the LEGAL rows only: a posture just refused for this kind must not go on to
        // stamp the verdict it asked for, or the report is advice the compiler ignored.
        let firstVerdict (rows: EqCompAttr<'Verdict> list) =
            match legalOn kind rows with
            | r :: _ -> ValueSome r.Verdict
            | [] -> ValueNone

        let allowNullLiteral = a.Has RuntimeNames.allowNullLiteralAttributeKey
        let legalNullLiteral = allowNullLiteral && List.contains kind allowNullLiteralKinds

        if allowNullLiteral && not legalNullLiteral then
            ctx.Report(declTok, Kind.AllowNullLiteralOnWrongKind)

        {
            Equality = firstVerdict eq
            Comparison = firstVerdict cmp
            AllowNullLiteral = legalNullLiteral
        }

    let private mergeParamAttrSets (ctx: PassContext) (acc: ParamAttrs) (sets: Attributes<SyntaxToken>) : ParamAttrs =
        let a = NameResolutionTypeRefStamp.resolveAttributes ctx (ValueSome sets)

        if a.Has RuntimeNames.callAtMostOnceAttributeKey then
            { acc with CallAtMostOnce = true }
        else
            acc

    /// Unwraps the inert pattern wrappers (`(p)`, `p : t`, `p as x`, `?p`), so
    /// `([<CallAtMostOnce>] e2 : bool)` is recognised regardless of nesting.
    let paramAttrsOfArgPat (ctx: PassContext) (p: Pat<SyntaxToken>) : ParamAttrs =
        let rec go (acc: ParamAttrs) (p: Pat<SyntaxToken>) : ParamAttrs =
            match p with
            | Pat.Attributed(attributes = sets; pat = inner) -> go (mergeParamAttrSets ctx acc sets) inner
            | Pat.EnclosedBlock(pat = inner)
            | Pat.Typed(pat = inner)
            | Pat.As(pat = inner)
            | Pat.Optional(pat = inner) -> go acc inner
            | _ -> acc

        go ParamAttrs.Default p

    /// `TypeDefn.Record` / `TypeDefn.Union` carry these on their `typeName: TypeName`.
    let attributesOfTypeName (tn: TypeName<SyntaxToken>) : Attributes<SyntaxToken> voption =
        let (TypeName(attributes = a)) = tn
        a

    /// `[<Global>]`: the value IS a target global, so no definition is emitted. Checked BOTH
    /// ways because marking a body that is not a bare intrinsic template silently deletes real code,
    /// and an unmarked restatement emits `const undefined = undefined`, which cannot run.
    let declareGlobalBinding
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (emittedName: string voption)
        (exportedKey: SymbolKey voption)
        (valT: TExpr)
        : unit =
        let isGlobal =
            (NameResolutionTypeRefStamp.resolveAttributes ctx b.attributes).Has RuntimeNames.globalAttributeKey

        let site = (CstKeys.siteOfBinding b).Tok

        let named =
            match emittedName with
            | ValueSome n -> sprintf "'%s'" n
            | ValueNone -> "this binding"

        let report (message: string) = ctx.Report(site, Kind.Message message)

        match TExprG.nullaryIntrinsicText valT, isGlobal with
        | ValueNone, true ->
            report (
                sprintf
                    "[<Global>] declares %s to BE a target global, so its body must be exactly one zero-operand intrinsic naming that global, because no definition is emitted for it"
                    named
            )
        | ValueSome text, false when emittedName = ValueSome text ->
            report (
                sprintf
                    "The binding %s restates the target global '%s': its definition would initialise from itself and could not run. Mark it [<Global>], which emits no definition and references the global by its bare name."
                    named
                    text
            )
        | ValueSome _, true ->
            match exportedKey with
            | ValueSome k -> ctx.Bindings.GlobalValueKeys.Add k |> ignore
            // A pattern with no single bound name has no identity to file the declaration under.
            | ValueNone ->
                report
                    "[<Global>] declares the VALUE a binding introduces to be a target global, but this binding has no single name, so give it one, or drop the attribute"
        | _ -> ()
