namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Decode the small set of attributes that govern a record / union's equality AND comparison postures,
// off the type's `TypeName.attributes` CST node. The decoder is intentionally
// syntactic — F# attributes resolve by short name (with the `Attribute` suffix
// optional) and a fully qualified path collapses to the same leaf, so matching
// on the long-ident's last segment is what F# itself does for these BCL
// attributes.
//
// Equality and comparison are independent axes: `[<StructuralEquality;
// NoComparison>]` is a valid combination. Each `decode*Attributes` decoder
// returns its own verdict, and `NameResolution` writes both onto the matching
// `*Info` mutable.

module Attributes =

    /// Canonical equality-relevant short names. `Attribute` is the F# suffix
    /// rule (`StructuralEqualityAttribute` ≡ `StructuralEquality`), accepted on
    /// either form.
    let private structuralEqualityNames =
        [ "StructuralEquality"; "StructuralEqualityAttribute" ]

    let private referenceEqualityNames =
        [ "ReferenceEquality"; "ReferenceEqualityAttribute" ]

    let private noEqualityNames = [ "NoEquality"; "NoEqualityAttribute" ]

    let private customEqualityNames = [ "CustomEquality"; "CustomEqualityAttribute" ]

    /// Canonical comparison-relevant short names. `[<StructuralComparison>]`
    /// opts a record / union INTO structural comparison (per
    /// brainstorm-comparison §9 the default is opt-in); `[<NoComparison>]` is
    /// explicit refusal. `[<CustomComparison>]` decodes to
    /// `ComparisonVerdict.Custom` — no pair is synthesised; the user's
    /// `CompareTo`/`IComparable<Self>` members are authoritative.
    let private structuralComparisonNames =
        [ "StructuralComparison"; "StructuralComparisonAttribute" ]

    let private noComparisonNames = [ "NoComparison"; "NoComparisonAttribute" ]

    let private customComparisonNames =
        [ "CustomComparison"; "CustomComparisonAttribute" ]

    /// The attribute "class" lives inside `ObjectConstruction.typ` as the
    /// long-ident the user wrote; yield the last segment so we match the F#
    /// resolution rule on short name. Pass-binding `PassContext.NameOf` to the
    /// shared `AttributeDecode` resolver.
    let private attributeShortName (ctx: PassContext) (typ: Type<SyntaxToken>) : string voption =
        AttributeDecode.attributeShortName ctx.NameOf typ

    // Per-axis verdict resolution + the FS0382 / FS0377 attribute validation
    // lives in `validateEqCompAttributes` below (the single entry point for every
    // type-registration site). The old first-wins `decode*Attributes` decoders
    // were folded into it.

    /// The set of equality / comparison attributes PRESENT on a type, collected
    /// without first-wins short-circuiting so the mix validator (FS0377) can see
    /// every contributing attribute. Each flag is `true` iff the corresponding
    /// attribute short name appears in the type's `[<…>]` sets.
    [<Struct>]
    type private EqCompAttrSet =
        {
            StructuralEq: bool
            ReferenceEq: bool
            NoEq: bool
            CustomEq: bool
            StructuralCmp: bool
            NoCmp: bool
            CustomCmp: bool
        }

        static member Empty =
            {
                StructuralEq = false
                ReferenceEq = false
                NoEq = false
                CustomEq = false
                StructuralCmp = false
                NoCmp = false
                CustomCmp = false
            }

    /// Collect the FULL set of present equality / comparison attributes off a
    /// type's `[<…>]` sets — unlike `decode*Attributes`, no first-wins
    /// short-circuit, so a contradictory mix (`[<ReferenceEquality;
    /// StructuralEquality>]`) is visible to the FS0377 validator.
    let private collectEqCompAttrs (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) : EqCompAttrSet =
        match attrs with
        | ValueNone -> EqCompAttrSet.Empty
        | ValueSome sets ->
            let mutable r = EqCompAttrSet.Empty

            for AttributeSet(attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    let attrTy =
                        match construction with
                        | ObjectConstruction(typ = t) -> t
                        | InterfaceConstruction(typ = t) -> t

                    match attributeShortName ctx attrTy with
                    | ValueSome n when List.contains n structuralEqualityNames -> r <- { r with StructuralEq = true }
                    | ValueSome n when List.contains n referenceEqualityNames -> r <- { r with ReferenceEq = true }
                    | ValueSome n when List.contains n noEqualityNames -> r <- { r with NoEq = true }
                    | ValueSome n when List.contains n customEqualityNames -> r <- { r with CustomEq = true }
                    | ValueSome n when List.contains n structuralComparisonNames -> r <- { r with StructuralCmp = true }
                    | ValueSome n when List.contains n noComparisonNames -> r <- { r with NoCmp = true }
                    | ValueSome n when List.contains n customComparisonNames -> r <- { r with CustomCmp = true }
                    | _ -> ()

            r

    /// The type-kind axis the equality / comparison attribute legality matrix
    /// (FS0382) keys on. `Struct` is any value type (`[<Struct>]`, `struct … end`,
    /// `[<IsByRefLike>]`); `RefClass` is a plain reference class. Records, unions
    /// and exceptions each have their own arm because their legality differs from
    /// classes' (a record may carry `[<ReferenceEquality>]`; a class may not).
    [<RequireQualifiedAccess>]
    type EqCompTargetKind =
        | Record
        | Union
        | Exception
        | Struct
        | RefClass
        | Interface

    let private addDiag (ctx: PassContext) (declTok: SyntaxToken) (code: string) (message: string) : unit =
        ctx.Error(declTok, code, message)

    /// Validate the equality / comparison attributes against the type kind
    /// (FS0382 kind-legality + FS0377 invalid-mix), emitting diagnostics at the
    /// type's declaration site, and return the resolved `(EqualityVerdict voption,
    /// ComparisonVerdict voption)` for the caller to default + stamp. `ValueNone`
    /// on either axis ⇒ no relevant attribute present (caller applies its
    /// kind-aware default). This is the single attribute-validation entry point
    /// for every type-registration site (record / union / class / interface);
    /// the within-axis "first wins" of `decode*Attributes` is preserved here for
    /// the verdict, while the mix check sees the whole set.
    ///
    /// `declTok` is the diagnostic site — the `TypeIdentity.DeclSite` token a registrar is
    /// handed, so no registration site re-derives the name token to place a diagnostic.
    let validateEqCompAttributes
        (ctx: PassContext)
        (kind: EqCompTargetKind)
        (declTok: SyntaxToken)
        (attrs: Attributes<SyntaxToken> voption)
        : EqualityVerdict voption * ComparisonVerdict voption =
        let s = collectEqCompAttrs ctx attrs

        // FS0382 — kind legality. StructuralEquality / StructuralComparison are
        // legal only on record / union / exception / struct; ReferenceEquality
        // additionally bars struct; Custom* bar only interface; No* are legal
        // everywhere.
        let structuralLegal =
            match kind with
            | EqCompTargetKind.Record
            | EqCompTargetKind.Union
            | EqCompTargetKind.Exception
            | EqCompTargetKind.Struct -> true
            | EqCompTargetKind.RefClass
            | EqCompTargetKind.Interface -> false

        let referenceLegal =
            match kind with
            | EqCompTargetKind.Record
            | EqCompTargetKind.Union
            | EqCompTargetKind.Exception -> true
            | EqCompTargetKind.Struct
            | EqCompTargetKind.RefClass
            | EqCompTargetKind.Interface -> false

        let customLegal =
            match kind with
            | EqCompTargetKind.Interface -> false
            | _ -> true

        let structMsg =
            "Only record, union, exception and struct types may be augmented with the 'ReferenceEquality', 'StructuralEquality' and 'StructuralComparison' attributes."

        if (s.StructuralEq || s.StructuralCmp) && not structuralLegal then
            addDiag ctx declTok "FS0382" structMsg

        if s.ReferenceEq && not referenceLegal then
            addDiag ctx declTok "FS0382" structMsg

        if (s.CustomEq || s.CustomCmp) && not customLegal then
            addDiag
                ctx
                declTok
                "FS0382"
                "The 'CustomEquality' and 'CustomComparison' attributes are not valid on an interface type."

        // FS0377 — invalid mix. Count attributes per axis; more than one is a
        // mix. The cross-axis rules forbid structural comparison without
        // structural equality, and reference / no-equality alongside structural
        // comparison.
        let eqCount =
            (if s.StructuralEq then 1 else 0)
            + (if s.ReferenceEq then 1 else 0)
            + (if s.NoEq then 1 else 0)
            + (if s.CustomEq then 1 else 0)

        let cmpCount =
            (if s.StructuralCmp then 1 else 0)
            + (if s.NoCmp then 1 else 0)
            + (if s.CustomCmp then 1 else 0)

        // `[<StructuralComparison>]` contradicts a non-structural equality
        // POSTURE — but absence of an equality attribute is NOT a contradiction:
        // a record / union / struct defaults to structural equality, so
        // `[<StructuralComparison>]` on its own is valid (and is the common case).
        // Only an explicit Reference / No / Custom equality attribute conflicts.
        // (StructuralComparison is already FS0382 on a reference class, whose
        // default equality is Reference, so the kinds that reach here always have
        // a structural default.)
        let invalidMix =
            eqCount > 1
            || cmpCount > 1
            || (s.StructuralCmp && (s.ReferenceEq || s.NoEq || s.CustomEq))

        if invalidMix then
            addDiag
                ctx
                declTok
                "FS0377"
                "This type uses an invalid mix of the attributes 'NoEquality', 'ReferenceEquality', 'StructuralEquality', 'NoComparison' and 'StructuralComparison'."

        // Resolved verdicts use a fixed within-axis priority (Structural > Reference >
        // No > Custom). For any non-contradictory attribute set this is exactly the
        // verdict the old source-order `decode*Attributes` first-wins decoders stamped;
        // the two differ only for a contradictory mix (e.g. `[<ReferenceEquality;
        // StructuralEquality>]`), which is now an FS0377 error, so the stamped verdict is
        // moot. Net: observationally unchanged; only the diagnostics are new.
        let eqVerdict =
            if s.StructuralEq then ValueSome EqualityVerdict.Structural
            elif s.ReferenceEq then ValueSome EqualityVerdict.Reference
            elif s.NoEq then ValueSome EqualityVerdict.NoEquality
            elif s.CustomEq then ValueSome EqualityVerdict.Custom
            else ValueNone

        let cmpVerdict =
            if s.StructuralCmp then
                ValueSome ComparisonVerdict.Structural
            elif s.NoCmp then
                ValueSome ComparisonVerdict.NoComparison
            elif s.CustomCmp then
                ValueSome ComparisonVerdict.Custom
            else
                ValueNone

        eqVerdict, cmpVerdict

    /// Canonical parameter-attribute short names. `[<CallAtMostOnce>]` marks an
    /// inline parameter for call-by-name-at-its-single-use splicing (see
    /// `ParamAttrs.CallAtMostOnce`). Extend this section as more special
    /// parameter attributes are honoured (F# declares many — `InlineIfLambda`,
    /// `CallerMemberName`, …): one name list + one decoder arm + one `ParamAttrs`
    /// flag.
    let private callAtMostOnceNames = [ "CallAtMostOnce"; "CallAtMostOnceAttribute" ]

    /// Fold one parameter's `[<…>]` sets into `acc`, flipping each recognised
    /// flag. Mirrors `decodeClassAttributes`; unrecognised attributes are
    /// silently ignored.
    let private mergeParamAttrSets (ctx: PassContext) (acc: ParamAttrs) (sets: Attributes<SyntaxToken>) : ParamAttrs =
        let mutable r = acc

        for AttributeSet(attributes = entries) in sets do
            for Attribute(construction = construction), _sep in entries do
                let attrTy =
                    match construction with
                    | ObjectConstruction(typ = t) -> t
                    | InterfaceConstruction(typ = t) -> t

                match attributeShortName ctx attrTy with
                | ValueSome n when List.contains n callAtMostOnceNames -> r <- { r with CallAtMostOnce = true }
                | _ -> ()

        r

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
