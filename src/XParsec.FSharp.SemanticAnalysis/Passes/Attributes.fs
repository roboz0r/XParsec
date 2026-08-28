namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// Each attribute's long-ident is RESOLVED as a type and compared by `TypeKey`, so a user type
// named `ReferenceEqualityAttribute` elsewhere keeps its own meaning, and an ABBREVIATION
// of a marker resolves to the alias' OWN identity, so it is ignored.

module Attributes =

    let private wrongKindDiagnostics (kind: TypeDefnKind) (rows: EqCompAttr<'Verdict> list) : Kind list =
        rows
        |> List.filter (fun r -> not (AttributeVerdicts.isLegalOn kind r))
        |> List.map (AttributeVerdicts.wrongKindDiag kind)

    /// Every attribute-against-kind check a type declaration gets: FS0382 legality + FS0377
    /// invalid-mix on the equality / comparison axes, and FS0934 on `[<AllowNullLiteral>]`,
    /// all reported at `declTok`. Verdicts come from `AttributeVerdicts` over the same
    /// `attrs`.
    let validateTypeDefnAttributes
        (ctx: PassContext)
        (kind: TypeDefnKind)
        (declTok: SyntaxToken)
        (attrs: TAttributes)
        : unit =
        let eq = AttributeVerdicts.presentRows attrs AttributeVerdicts.equalityAttrs
        let cmp = AttributeVerdicts.presentRows attrs AttributeVerdicts.comparisonAttrs

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

        if
            AttributeVerdicts.has attrs RuntimeNames.allowNullLiteralAttributeKey
            && not (AttributeVerdicts.allowNullLiteral kind attrs)
        then
            ctx.Report(declTok, Kind.AllowNullLiteralOnWrongKind)

    /// The element classification fsc enforces `[<AttributeUsage>]` against for a type
    /// declaration of `kind`: every value-type kind is a `Struct` element.
    let private attrTargetOfKind (kind: TypeDefnKind) : AttrTarget =
        match kind with
        | TypeDefnKind.Interface -> AttrTarget.Interface
        | TypeDefnKind.Enum -> AttrTarget.Enum
        | TypeDefnKind.Abbrev -> AttrTarget.Abbreviation
        | TypeDefnKind.StructRecord
        | TypeDefnKind.StructUnion
        | TypeDefnKind.StructClass -> AttrTarget.Struct
        | TypeDefnKind.Record
        | TypeDefnKind.Union
        | TypeDefnKind.RefClass -> AttrTarget.Class

    /// Fold a type declaration's attributes under its element classification.
    let foldTypeDefn (ctx: PassContext) (kind: TypeDefnKind) (attrs: ResolvedAttributes) : TAttributes =
        AttributeFold.build ctx (attrTargetOfKind kind) attrs

    let foldAndValidateTypeDefn
        (ctx: PassContext)
        (kind: TypeDefnKind)
        (declTok: SyntaxToken)
        (attrs: ResolvedAttributes)
        : TAttributes =
        let tattrs = foldTypeDefn ctx kind attrs
        validateTypeDefnAttributes ctx kind declTok tattrs
        tattrs

    let private mergeParamAttrSets (ctx: PassContext) (acc: ParamAttrs) (sets: Attributes<SyntaxToken>) : ParamAttrs =
        let a = ctx.ResolveAttributes(ValueSome sets)

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
            (ctx.ResolveAttributes b.attributes).Has RuntimeNames.globalAttributeKey

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

    /// `[<Import>]`: the binding's implementation IS the named export of a committed runtime
    /// asset. Checked BOTH ways because a real body beside the attribute would be silently
    /// discarded, and a `jsNative` body with no attribute emits the throwing template as the
    /// definition. The selector must equal the emitted name, because a reference imports that.
    ///
    /// The BODY is read off the CST, ahead of the inline expansion that turns `jsNative` into
    /// the template it stands for.
    let declareImportBinding (ctx: PassContext) (b: Binding<SyntaxToken>) (emittedName: string voption) : unit =
        let name =
            match MemberNames.ofBinding ctx b with
            | ValueSome m -> m.Name
            | ValueNone -> ""

        let emitted =
            match emittedName with
            | ValueSome n -> n
            | ValueNone -> name

        let report (e: Conformance.ConformanceError) =
            ctx.Report((CstKeys.siteOfBinding b).Tok, Kind.Message(Conformance.describe e))

        let isJsNative = Conformance.isJsNativeBody ctx.NameOf b.expr

        match AttributeDecode.tryImport ctx.NameOf (ctx.ResolveAttributes b.attributes) with
        | ImportDecl.NoImport ->
            if isJsNative then
                report (Conformance.ConformanceError.JsNativeWithoutImport name)
        | ImportDecl.Malformed -> report (Conformance.ConformanceError.ImportMalformed name)
        | ImportDecl.Import r ->
            if not isJsNative then
                report (Conformance.ConformanceError.ImportBodyNotJsNative name)

            if r.Selector <> emitted then
                report (Conformance.ConformanceError.ImportSelectorMismatch(name, r.Selector))
