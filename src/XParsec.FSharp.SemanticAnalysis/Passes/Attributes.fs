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

    /// The set of equality / comparison attributes PRESENT on a type, collected
    /// without first-wins short-circuiting so the mix validator (FS0377) can see
    /// every contributing attribute. Each flag is `true` iff the type's `[<…>]`
    /// sets carry an attribute RESOLVING to the corresponding `Vesper.Core` type.
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

    /// Collect the FULL set of present equality / comparison attributes off a
    /// type's `[<…>]` sets. No first-wins short-circuit, so a contradictory mix
    /// (`[<ReferenceEquality; StructuralEquality>]`) is visible to the FS0377
    /// validator.
    ///
    /// `[<StructuralComparison>]` opts a record / union INTO structural comparison
    /// (the default is opt-in); `[<NoComparison>]` is explicit refusal.
    /// `[<CustomComparison>]` decodes to `ComparisonVerdict.Custom` — no pair is
    /// synthesised; the user's `CompareTo` / `IComparable<Self>` members are
    /// authoritative.
    let private collectEqCompAttrs (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) : EqCompAttrSet =
        let a = NameResolutionTypeHeadStamp.resolveAttributes ctx attrs

        {
            StructuralEq = a.Has RuntimeNames.structuralEqualityAttributeKey
            ReferenceEq = a.Has RuntimeNames.referenceEqualityAttributeKey
            NoEq = a.Has RuntimeNames.noEqualityAttributeKey
            CustomEq = a.Has RuntimeNames.customEqualityAttributeKey
            StructuralCmp = a.Has RuntimeNames.structuralComparisonAttributeKey
            NoCmp = a.Has RuntimeNames.noComparisonAttributeKey
            CustomCmp = a.Has RuntimeNames.customComparisonAttributeKey
        }

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

        if (s.StructuralEq || s.StructuralCmp) && not structuralLegal then
            ctx.Report(declTok, Kind.StructuralEqualityAttributeOnWrongKind)

        if s.ReferenceEq && not referenceLegal then
            ctx.Report(declTok, Kind.StructuralEqualityAttributeOnWrongKind)

        if (s.CustomEq || s.CustomCmp) && not customLegal then
            ctx.Report(declTok, Kind.CustomEqualityAttributeOnInterface)

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
            ctx.Report(declTok, Kind.InvalidEqualityAttributeMix)

        // Fixed within-axis priority (Structural > Reference > No > Custom). A set with
        // more than one is already an FS0377 error above, so which of them this picks is
        // moot — the priority exists only to make the verdict total.
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
