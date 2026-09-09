namespace XParsec.FSharp.SemanticAnalysis.Passes

open Vesper
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
        (keys: AttributeKeys)
        : unit =
        let eq = AttributeVerdicts.presentRows keys AttributeVerdicts.equalityAttrs
        let cmp = AttributeVerdicts.presentRows keys AttributeVerdicts.comparisonAttrs

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
            AttributeVerdicts.has keys RuntimeNames.allowNullLiteralAttributeKey
            && not (AttributeVerdicts.allowNullLiteral kind keys)
        then
            ctx.Report(declTok, Kind.AllowNullLiteralOnWrongKind)

    /// The element classification fsc enforces `[<AttributeUsage>]` against for a type
    /// declaration of `kind`: every value-type kind is a `Struct` element.
    let attrTargetOfKind (kind: TypeDefnKind) : AttrTarget =
        match kind with
        | TypeDefnKind.Interface -> AttrTarget.Interface
        | TypeDefnKind.Enum -> AttrTarget.Enum
        // fsc admits an attribute on a measure at every type kind, as on an alias.
        | TypeDefnKind.Abbrev
        | TypeDefnKind.Measure -> AttrTarget.Abbreviation
        | TypeDefnKind.StructRecord
        | TypeDefnKind.StructUnion
        | TypeDefnKind.StructClass -> AttrTarget.Struct
        | TypeDefnKind.Record
        | TypeDefnKind.Union
        | TypeDefnKind.RefClass -> AttrTarget.Class

    /// Declare a type declaration's attributes under its element classification, and judge
    /// them against its kind.
    let declareTypeDefn
        (ctx: PassContext)
        (kind: TypeDefnKind)
        (declTok: SyntaxToken)
        (attrs: ResolvedAttributes)
        : unit =
        ctx.DeclareAttributes(AttributeSite.ofToken declTok, attrTargetOfKind kind, attrs)
        validateTypeDefnAttributes ctx kind declTok attrs.Keys

    /// The classes this file declares, read off the registry after inference has settled
    /// their member types: a constructor's parameters at their declared annotations, a
    /// property at its inferred type, a `val` field at its declared type.
    let private localClasses (ctx: PassContext) : IAttributeClassSource =
        let freeze (ty: SemType) : FrozenType =
            FrozenTypeBridge.freezeWith ctx.Store (fun _ -> FTUnknown UnknownReason.UnresolvedTypar) ty

        let ctorOf (classKey: TypeKey) (ps: ClassCtorParamInfo[]) : AttributeCtor =
            let parameters =
                Block.ofArray (
                    ps
                    |> Array.map (fun p ->
                        {
                            Name = ValueSome p.Name
                            Declared = p.Declared |> ValueOption.map freeze
                        }
                    )
                )

            let identity =
                match ps |> Array.tryFind (fun p -> p.Declared.IsNone) with
                | Some p -> AttributeCtorIdentity.Unannotated p.Name
                | None ->
                    AttributeCtorIdentity.Key(
                        SymbolKeyOps.ctorKeyOf classKey (parameters |> Block.map (fun p -> p.Declared.Value)) 0<typeSlot>
                    )

            {
                Identity = identity
                Params = parameters
            }

        /// The type of the property `m` accesses: a parameterless getter's own, a `with get ()`
        /// accessor's result, a setter's argument.
        let propertyType (m: TypeMemberInfo) : SemType voption =
            match m.Kind, UnionFind.zonk ctx.Store m.Type with
            | TMemberKind.Property, ty -> ValueSome ty
            | TMemberKind.Accessor(_, TAccessorRole.Getter), TyFun(_, result) -> ValueSome result
            | TMemberKind.Accessor(_, TAccessorRole.Setter), TyFun(arg, _) -> ValueSome arg
            | _ -> ValueNone

        { new IAttributeClassSource with
            member _.Ctors attrKey =
                match TypeRegistry.tryClassByKey ctx.Types attrKey with
                | ValueSome info ->
                    Block.ofList
                        [
                            if info.HasPrimaryCtor then
                                yield ctorOf attrKey info.CtorParams

                            for secondary in info.Body.SecondaryCtors do
                                yield ctorOf attrKey secondary.Params
                        ]
                | ValueNone -> Block.empty

            member _.TrySettable(attrKey, name) =
                match TypeRegistry.tryClassByKey ctx.Types attrKey with
                | ValueSome info ->
                    let property =
                        info.Body.Members
                        |> Array.tryPick (fun m ->
                            match TMemberKind.propertyOf m.Name m.Kind, propertyType m with
                            | ValueSome(prop, _), ValueSome ty when prop = name ->
                                Some(TAttributeMember.Property(name, freeze ty))
                            | _ -> None
                        )

                    let field =
                        info.Body.InstanceFields
                        |> Array.tryFind (fun f -> f.Name = name)
                        |> Option.map (fun f -> TAttributeMember.Field(name, freeze f.Type))

                    match property, field with
                    | Some target, _
                    | None, Some target -> ValueSome target
                    | None, None -> ValueNone
                | ValueNone -> ValueNone
        }

    /// Check every attribute position filed during name resolution, against the classes this
    /// file declares and those its references publish, and check each later position at its
    /// declaration. Runs after inference, which settles a local class's member types.
    let openChecks (ctx: PassContext) : unit =
        ctx.OpenAttributeChecks(
            AttributeClasses.firstDeclaring (localClasses ctx) (AttributeClasses.ofStore ctx.Provider)
        )

    /// Enforce every declared position's `[<AttributeUsage>]` target, in SOURCE order. Seals
    /// the position table, so every declaration must precede this pass.
    let run (ctx: PassContext) : unit =
        AttributeUsageCheck.enforceAll ctx (ctx.Resolution.AttributePositions.Seal())

    let private mergeParamAttrSets (ctx: PassContext) (acc: ParamAttrs) (sets: Attributes<SyntaxToken>) : ParamAttrs =
        let a = ctx.ResolveAttributes(ValueSome sets)

        if a.Has RuntimeNames.callAtMostOnceAttributeKey then
            { acc with CallAtMostOnce = true }
        else
            acc

    /// The keyword token a member element is anchored on: the key its attribute position is
    /// filed under.
    let memberKeywordToken (kw: MemberKeyword<SyntaxToken>) : SyntaxToken =
        match kw with
        | MemberKeyword.Member t
        | MemberKeyword.Override t
        | MemberKeyword.Default t
        | MemberKeyword.Abstract(abstractToken = t) -> t

    /// The element a member declaration occupies: a property declaration, an auto-property,
    /// a get/set pair and an argument-less abstract signature are all properties; every other
    /// form is a method.
    let private memberTarget (d: MethodOrPropDefn<SyntaxToken>) : AttrTarget =
        match d with
        | MethodOrPropDefn.Method _ -> AttrTarget.Method
        | MethodOrPropDefn.Property _
        | MethodOrPropDefn.PropertyWithGetSet _
        | MethodOrPropDefn.AutoProperty _ -> AttrTarget.Property
        | MethodOrPropDefn.AbstractSignature(MemberSig.PropSig _) -> AttrTarget.Property
        | MethodOrPropDefn.AbstractSignature(MemberSig.MethodOrPropSig(sign = CurriedSig(args = sigArgs))) ->
            if sigArgs.IsEmpty then
                AttrTarget.Property
            else
                AttrTarget.Method

    /// Declare a member element's attributes under the element it occupies.
    let declareMemberAttributes
        (ctx: PassContext)
        (attrs: Attributes<SyntaxToken> voption)
        (kw: MemberKeyword<SyntaxToken>)
        (d: MethodOrPropDefn<SyntaxToken>)
        : unit =
        ctx.DeclareAttributes(AttributeSite.ofToken (memberKeywordToken kw), memberTarget d, attrs)

    /// Declare the attributes written on a parameter, unwrapping the inert pattern wrappers
    /// (`(p)`, `p : t`, `p as x`, `?p`) an attribute set may sit under.
    let rec declareParamAttributes (ctx: PassContext) (p: Pat<SyntaxToken>) : unit =
        match p with
        | Pat.Attributed(attributes = sets; pat = inner) ->
            ctx.DeclareAttributeSets(ValueSome sets, AttrTarget.Parameter)
            declareParamAttributes ctx inner
        | Pat.EnclosedBlock(pat = inner)
        | Pat.Typed(pat = inner)
        | Pat.As(pat = inner)
        | Pat.Optional(pat = inner) -> declareParamAttributes ctx inner
        | _ -> ()

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

    /// The body is `nativeOnly`, read off the TRANSLATED tree by resolved identity: the
    /// binding's own key, or the sentinel template a reference to it splices to. A shadowing
    /// declaration of the same spelling does not satisfy the check.
    let rec private isNativeOnlyBody (e: TExpr) : bool =
        match e with
        | TExpr.Lambda(_, body, _, _) -> isNativeOnlyBody body
        | TExpr.External(key, _, _) -> key = RuntimeNames.nativeOnlyBindingKey
        | _ -> TExprG.nullaryIntrinsicText e = ValueSome RuntimeNames.importSentinelText

    /// `[<Import>]`: the binding's implementation IS the named export of a committed runtime
    /// asset. Checked BOTH ways, because a real body beside the attribute is silently
    /// discarded and a bare `nativeOnly` body reaches a backend that refuses it.
    let declareImportBinding
        (ctx: PassContext)
        (b: Binding<SyntaxToken>)
        (emittedName: string voption)
        (exportedKey: SymbolKey voption)
        (valT: TExpr)
        : unit =
        let name =
            match MemberNames.ofBinding ctx b with
            | ValueSome m -> m.Name
            | ValueNone -> ""

        let emitted =
            match emittedName with
            | ValueSome n -> n
            | ValueNone -> name

        let report (e: Conformance.ConformanceError) =
            ctx.Report((CstKeys.siteOfBinding b).Tok, Kind.ConformanceFinding e)

        let isNativeOnly = isNativeOnlyBody valT

        // `nativeOnly`'s own declaration is the sentinel's source, not a binding it serves.
        let isSentinelDeclaration = exportedKey = ValueSome RuntimeNames.nativeOnlyKey

        match AttributeDecode.tryImport ctx.NameOf (ctx.ResolveAttributes b.attributes) with
        | ImportDecl.NoImport ->
            if isNativeOnly && not isSentinelDeclaration then
                report (Conformance.ConformanceError.NativeOnlyWithoutImport name)
        | ImportDecl.Malformed -> report (Conformance.ConformanceError.ImportMalformed name)
        | ImportDecl.Import r ->
            if not isNativeOnly then
                report (Conformance.ConformanceError.ImportBodyNotNativeOnly name)

            if r.Selector <> emitted then
                report (Conformance.ConformanceError.ImportSelectorMismatch(name, r.Selector))

            // The manifest half of the check is the assembly gate's: it resolves `Path`
            // through the target's module system and reports through `Site`.
            ctx.Bindings.Imports.Add
                {
                    Binding = name
                    Selector = r.Selector
                    Path = r.Path
                    Site = (CstKeys.siteOfBinding b).Tok
                }
