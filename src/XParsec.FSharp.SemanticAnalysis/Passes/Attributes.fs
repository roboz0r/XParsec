namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Each attribute's long-ident is RESOLVED as a type and compared by `TypeKey`, so a user type
// named `ReferenceEqualityAttribute` elsewhere keeps its own meaning — and an ABBREVIATION
// naming a marker resolves to the alias' OWN identity, so it is ignored.

module Attributes =

    /// The axis the FS0382 legality matrix keys on. Records, unions and exceptions are
    /// separate cases because a record may carry `[<ReferenceEquality>]` where a class cannot.
    [<RequireQualifiedAccess>]
    type EqCompTargetKind =
        | Record
        | Union
        | Exception
        | Struct
        | RefClass
        | Interface

    /// One recognised attribute. Honouring one more is one more row.
    type private EqCompAttr<'Verdict> =
        {
            Key: TypeKey
            Verdict: 'Verdict
            LegalKinds: EqCompTargetKind list
            OnWrongKind: Kind
        }

    let private anyKind =
        [
            EqCompTargetKind.Record
            EqCompTargetKind.Union
            EqCompTargetKind.Exception
            EqCompTargetKind.Struct
            EqCompTargetKind.RefClass
            EqCompTargetKind.Interface
        ]

    /// A structural posture states what the FIELDS decide, so only the kinds that have them.
    let private structuralKinds =
        [
            EqCompTargetKind.Record
            EqCompTargetKind.Union
            EqCompTargetKind.Exception
            EqCompTargetKind.Struct
        ]

    /// Reference identity additionally bars a struct, which has none.
    let private referenceKinds =
        [ EqCompTargetKind.Record; EqCompTargetKind.Union; EqCompTargetKind.Exception ]

    /// A custom posture needs members to carry it, which an interface cannot declare.
    let private customKinds = anyKind |> List.except [ EqCompTargetKind.Interface ]

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
        (a: NameResolutionTypeHeadStamp.ResolvedAttributes)
        (rows: EqCompAttr<'Verdict> list)
        : EqCompAttr<'Verdict> list =
        rows |> List.filter (fun r -> a.Has r.Key)

    let private wrongKindDiagnostics (kind: EqCompTargetKind) (rows: EqCompAttr<'Verdict> list) : Kind list =
        rows
        |> List.filter (fun r -> not (List.contains kind r.LegalKinds))
        |> List.map (fun r -> r.OnWrongKind)

    /// FS0382 kind-legality + FS0377 invalid-mix at `declTok`, returning the verdict per axis.
    /// The single attribute-validation entry point for every type-registration site.
    let validateEqCompAttributes
        (ctx: PassContext)
        (kind: EqCompTargetKind)
        (declTok: SyntaxToken)
        (attrs: Attributes<SyntaxToken> voption)
        : EqualityVerdict voption * ComparisonVerdict voption =
        let a = NameResolutionTypeHeadStamp.resolveAttributes ctx attrs
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

        // More than one row is already FS0377, so priority exists only to make this total.
        let firstVerdict (rows: EqCompAttr<'Verdict> list) =
            match rows with
            | r :: _ -> ValueSome r.Verdict
            | [] -> ValueNone

        firstVerdict eq, firstVerdict cmp

    /// Extend as more parameter attributes are honoured: one type, one key, one flag.
    let private mergeParamAttrSets (ctx: PassContext) (acc: ParamAttrs) (sets: Attributes<SyntaxToken>) : ParamAttrs =
        let a = NameResolutionTypeHeadStamp.resolveAttributes ctx (ValueSome sets)

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

    /// The `.fsi` contract extractor drives the same decoder with its own resolver, so the
    /// canonical short names live in one place.
    let decodeClassAttributes (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) =
        AttributeDecode.decodeClassAttributes ctx.NameOf attrs

    /// `[<Global>]`: the value IS a target global, so no definition is emitted. Checked BOTH
    /// ways — marking a body that is not a bare intrinsic template silently deletes real code,
    /// and an unmarked restatement emits `const undefined = undefined`, which cannot run.
    let declareGlobalBinding
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (emittedName: string voption)
        (exportedKey: SymbolKey voption)
        (valT: TExpr)
        : unit =
        let isGlobal =
            (NameResolutionTypeHeadStamp.resolveAttributes ctx b.attributes).Has RuntimeNames.globalAttributeKey

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
                    "[<Global>] declares %s to BE a target global, so its body must be exactly one zero-operand intrinsic naming that global — no definition is emitted for it"
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
            // A head naming no single value has no identity to file the declaration under.
            | ValueNone ->
                report
                    "[<Global>] declares the VALUE a binding names to be a target global, but this binding names none — give it a single name, or drop the attribute"
        | _ -> ()
