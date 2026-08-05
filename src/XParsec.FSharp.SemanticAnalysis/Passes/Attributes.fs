namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Decode the compiler-recognised attributes and validate what each one is written on: a
// record / union's equality AND comparison postures, an inline parameter's calling
// convention, and `[<Global>]` on a module binding. Each attribute's long-ident is
// RESOLVED as a type — through the same local-claim-then-external engine every
// other written type head goes through — and the resolved `TypeKey` is compared
// against the `Vesper.Core` attribute identities in `RuntimeNames`. So a user type
// named `ReferenceEqualityAttribute` in another namespace keeps its own meaning,
// and a qualified path is honoured because it resolves, not because its leaf
// happens to read right.
//
// A TYPE ABBREVIATION naming a marker (`type R = Vesper.ReferenceEqualityAttribute`;
// `[<R>]`) resolves to the alias' OWN identity, so it is ignored — F# dealiases it.
// Closing that needs the abbreviation body, which is not filled while a type
// registers.
//
// Equality and comparison are independent axes: `[<StructuralEquality;
// NoComparison>]` is a valid combination. One verdict per axis comes back, and
// `NameResolution` writes both onto the matching `*Info` mutable.

module Attributes =

    /// The type-kind axis the equality / comparison attribute legality matrix
    /// (FS0382) keys on. `Struct` is any value type (`[<Struct>]`, `struct … end`,
    /// `[<IsByRefLike>]`); `RefClass` is a plain reference class. Records, unions
    /// and exceptions are separate cases because their legality differs from
    /// classes' (a record may carry `[<ReferenceEquality>]`; a class may not).
    [<RequireQualifiedAccess>]
    type EqCompTargetKind =
        | Record
        | Union
        | Exception
        | Struct
        | RefClass
        | Interface

    /// One recognised equality / comparison attribute: the identity it must RESOLVE to,
    /// the verdict it decodes to, the kinds it may be written on, and what to say where
    /// it may not. One more honoured attribute is one more row.
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

    /// A structural posture states what the FIELDS decide, so only the kinds that have
    /// fields of their own.
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

    /// The comparison axis, likewise in priority order. Comparison is OPT-IN where
    /// equality is not: `[<StructuralComparison>]` is what buys a record / union the
    /// synthesised pair, `[<NoComparison>]` is explicit refusal, and
    /// `[<CustomComparison>]` leaves the user's `CompareTo` / `IComparable<Self>`
    /// authoritative.
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

    /// The rows PRESENT on the declaration, in table order. No first-wins
    /// short-circuit, so a contradictory mix stays visible to the FS0377 check.
    let private presentAttrs
        (a: NameResolutionTypeHeadStamp.ResolvedAttributes)
        (rows: EqCompAttr<'Verdict> list)
        : EqCompAttr<'Verdict> list =
        rows |> List.filter (fun r -> a.Has r.Key)

    let private wrongKindDiagnostics (kind: EqCompTargetKind) (rows: EqCompAttr<'Verdict> list) : Kind list =
        rows
        |> List.filter (fun r -> not (List.contains kind r.LegalKinds))
        |> List.map (fun r -> r.OnWrongKind)

    /// Validate the equality / comparison attributes against the type kind
    /// (FS0382 kind-legality + FS0377 invalid-mix), emitting diagnostics at the
    /// type's declaration site, and return the resolved `(EqualityVerdict voption,
    /// ComparisonVerdict voption)` for the caller to default + stamp. `ValueNone`
    /// on either axis ⇒ no relevant attribute present (caller applies its
    /// kind-aware default). This is the single attribute-validation entry point
    /// for every type-registration site (record / union / class / interface).
    ///
    /// `declTok` is the diagnostic site — the `TypeIdentity.DeclSite` token a registrar is
    /// handed, so no registration site re-derives the name token to place a diagnostic.
    let validateEqCompAttributes
        (ctx: PassContext)
        (kind: EqCompTargetKind)
        (declTok: SyntaxToken)
        (attrs: Attributes<SyntaxToken> voption)
        : EqualityVerdict voption * ComparisonVerdict voption =
        let a = NameResolutionTypeHeadStamp.resolveAttributes ctx attrs
        let eq = presentAttrs a equalityAttrs
        let cmp = presentAttrs a comparisonAttrs

        // FS0382, once per distinct complaint: two attributes illegal the same way on one
        // declaration are one mistake.
        for d in List.distinct (wrongKindDiagnostics kind eq @ wrongKindDiagnostics kind cmp) do
            ctx.Report(declTok, d)

        // FS0377 — more than one attribute on an axis, or `[<StructuralComparison>]`
        // against an EXPLICIT non-structural equality. ABSENCE of an equality attribute is
        // not a contradiction: every kind that survives the FS0382 above defaults to
        // structural equality, so `[<StructuralComparison>]` alone is valid — and usual.
        let structuralCmp =
            cmp |> List.exists (fun r -> r.Verdict = ComparisonVerdict.Structural)

        let nonStructuralEq =
            eq |> List.exists (fun r -> r.Verdict <> EqualityVerdict.Structural)

        if eq.Length > 1 || cmp.Length > 1 || (structuralCmp && nonStructuralEq) then
            ctx.Report(declTok, Kind.InvalidEqualityAttributeMix)

        // A set with more than one row is already FS0377 above, so which of them wins is
        // moot — the priority exists only to make the verdict total.
        let firstVerdict (rows: EqCompAttr<'Verdict> list) =
            match rows with
            | r :: _ -> ValueSome r.Verdict
            | [] -> ValueNone

        firstVerdict eq, firstVerdict cmp

    /// Fold one parameter's `[<…>]` sets into `acc`, flipping each recognised
    /// flag. `[<CallAtMostOnce>]` marks an inline parameter for
    /// call-by-name-at-its-single-use splicing; unresolved attributes are silently
    /// ignored. Extend as more parameter attributes are honoured (F# declares many
    /// — `InlineIfLambda`, `CallerMemberName`, …): one declared type, one key, one
    /// `ParamAttrs` flag.
    let private mergeParamAttrSets (ctx: PassContext) (acc: ParamAttrs) (sets: Attributes<SyntaxToken>) : ParamAttrs =
        let a = NameResolutionTypeHeadStamp.resolveAttributes ctx (ValueSome sets)

        if a.Has RuntimeNames.callAtMostOnceAttributeKey then
            { acc with CallAtMostOnce = true }
        else
            acc

    /// Decode the compiler-recognised attributes on a single argument pattern.
    /// Unwraps the inert pattern wrappers (`(p)`, `p : t`, `p as x`, `?p`)
    /// accumulating every `[<…>]` set, so `([<CallAtMostOnce>] e2 : bool)` is
    /// recognised regardless of paren / annotation nesting. `ParamAttrs.Default`
    /// when the parameter carries no recognised attribute.
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

    /// Pull the attributes off a `TypeName` (`TypeDefn.Record` /
    /// `TypeDefn.Union` carry these on their `typeName: TypeName`).
    let attributesOfTypeName (tn: TypeName<SyntaxToken>) : Attributes<SyntaxToken> voption =
        let (TypeName(attributes = a)) = tn
        a

    /// Decode the class-shaping attributes (`[<Sealed>]`, `[<AllowNullLiteral>]`,
    /// `[<Struct>]`, `[<IsByRefLike>]`) off a type's CST sets, binding the pass's
    /// `NameOf` resolver to the shared `AttributeDecode` decoder (the `.fsi`
    /// contract extractor uses the same decoder with its own resolver, so the
    /// canonical short names live in exactly one place).
    let decodeClassAttributes (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) =
        AttributeDecode.decodeClassAttributes ctx.NameOf attrs

    /// `[<Global>]` on a module-level binding: the value IS a target global (JS
    /// `undefined`), so the declaring file emits no definition for it and a reference emits
    /// its bare name, from any file, with no import. Checks the declaration and, when it
    /// holds, files it under `exportedKey` — the value's own identity, which is what the
    /// declaration is ABOUT. Recording here rather than at the call site is the point: a
    /// checked declaration cannot then be dropped on the floor.
    ///
    /// The declaration is checked against the body it is written on BOTH ways. Marking a
    /// body that is not a bare intrinsic template would silently delete real code; leaving
    /// a binding that restates its own target global unmarked emits
    /// `const undefined = undefined`, which cannot initialise and kills the module at load.
    /// `emittedName` is the name the binding is emitted under, which is what a restatement
    /// is a restatement OF.
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
            // A head naming no single value (`let _ = …`) has no identity to file the
            // declaration under, so honouring it silently would emit the definition anyway.
            | ValueNone ->
                report
                    "[<Global>] declares the VALUE a binding names to be a target global, but this binding names none — give it a single name, or drop the attribute"
        | _ -> ()
