namespace XParsec.FSharp.SemanticAnalysis.Passes

open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open UnificationTranslate
open NameResolutionTypeRegistration
open NameResolutionDeclRegistration

// The registry stamping for a union definition: its case table, each case's field names
// and types, and the FS3176 check on those names.

module NameResolutionUnionRegistration =

    /// A union case's ctor name: `([])` → `Empty`, `(::)` → `Cons`, an ordinary case its
    /// own text, and `""` for a case with no name (`(*)`, range / active-pattern ops).
    let private unionCaseName (ctx: PassContext) (ident: IdentOrOp<SyntaxToken>) : string =
        match OperatorNames.unionCaseCtorName ctx.NameOf ident with
        | ValueSome n -> n
        | ValueNone -> ""

    /// FS3176 on one case's fields, `ValueNone` for an anonymous field. At most one report
    /// per case, as fsc: `Declared` at the first occurrence of a repeated name, else
    /// `AnonymousSpelling` at the first declared name equal to an anonymous field's spelling.
    let private checkCaseFieldNames (ctx: PassContext) (fields: (string * SyntaxToken) voption[]) : unit =
        let declared =
            [
                for f in fields do
                    match f with
                    | ValueSome d -> yield d
                    | ValueNone -> ()
            ]

        let repeated =
            declared
            |> List.groupBy fst
            |> List.tryPick (fun (name, occurrences) ->
                match occurrences with
                | (_, tok) :: _ :: _ -> Some(name, tok)
                | _ -> None
            )

        match repeated with
        | Some(name, tok) -> ctx.Report(tok, Kind.UnionCaseFieldNameClash(name, UnionFieldNameClash.Declared))
        | None ->
            let anonymousSpellings =
                UnionCaseFieldName.fsharpNames [ for f in fields -> ValueOption.map fst f ]
                |> Seq.zip fields
                |> Seq.choose (fun (f, name) -> if ValueOption.isNone f then Some name else None)
                |> Set.ofSeq

            declared
            |> List.tryFind (fun (name, _) -> anonymousSpellings.Contains name)
            |> Option.iter (fun (name, tok) ->
                ctx.Report(tok, Kind.UnionCaseFieldNameClash(name, UnionFieldNameClash.AnonymousSpelling))
            )

    /// One union case's registrable shape: its ctor name plus, positionally, each field's source
    /// name (`ValueNone` when unnamed) and written type. GADT-SYNTAX cases (FSharp.Core's list)
    /// decompose here too, their return type read as the declaring union; true GADTs do not.
    [<NoEquality; NoComparison>]
    type private UnionCaseShape =
        {
            Name: string
            FieldNames: string voption[]
            FieldTypes: Type<SyntaxToken>[]
        }

    /// A case's shape, with its field names checked under FS3176. `ValueNone` drops a case
    /// with no ctor name.
    let private inspectCaseData (ctx: PassContext) (data: UnionTypeCaseData<SyntaxToken>) : UnionCaseShape voption =
        let named (name: string) (fieldNames: (string * SyntaxToken) voption[]) (fieldTypes: Type<SyntaxToken>[]) =
            if name.Length = 0 then
                ValueNone
            else
                checkCaseFieldNames ctx fieldNames

                ValueSome
                    {
                        Name = name
                        FieldNames = fieldNames |> Array.map (ValueOption.map fst)
                        FieldTypes = fieldTypes
                    }

        match data with
        | UnionTypeCaseData.Nullary(name = ident)
        | UnionTypeCaseData.GadtNullary(name = ident) -> named (unionCaseName ctx ident) [||] [||]
        | UnionTypeCaseData.Nary(name = ident; fields = fields) ->
            named
                (unionCaseName ctx ident)
                [|
                    for f in fields ->
                        match f with
                        | UnionTypeField.Named(ident = id) -> ValueSome(ctx.NameOf id, id)
                        | UnionTypeField.Unnamed _ -> ValueNone
                |]
                [|
                    for f in fields ->
                        match f with
                        | UnionTypeField.Named(typ = t)
                        | UnionTypeField.Unnamed(typ = t) -> t
                |]
        | UnionTypeCaseData.GadtNary(name = ident; sign = UncurriedSig(args = ArgsSpec(args = specs))) ->
            named
                (unionCaseName ctx ident)
                [|
                    for ArgSpec(name = nm) in specs ->
                        match nm with
                        | ValueSome(ArgNameSpec(ident = id)) -> ValueSome(ctx.NameOf id, id)
                        | ValueNone -> ValueNone
                |]
                [| for ArgSpec(typ = t) in specs -> t |]

    let registerUnionDecl
        (ctx: PassContext)
        (id: TypeIdentity)
        (tn: TypeName<SyntaxToken>)
        (cases: UnionTypeCases<SyntaxToken>)
        : unit =
        let name = id.Name
        let declSite = id.DeclSite
        let typeParams = declaredTyparsOfTypeName ctx tn
        let typarConstraints = typarConstraintsOfTypeName tn
        let caseInfos = ResizeArray<UnionCaseInfo>(cases.Length)

        underTyparScope
            ctx
            typeParams
            (fun () ->
                match typarConstraints with
                | ValueSome cs -> translateConstraints ctx cs
                | ValueNone -> ()

                for UnionTypeCase(attributes = caseAttrs; data = data) in cases do
                    match inspectCaseData ctx data with
                    | ValueSome shape ->
                        let fieldTys = shape.FieldTypes |> Array.map (translateType ctx)

                        // The case carries its union's own claim KEY, so "which union
                        // declares this case" never re-resolves a name.
                        caseInfos.Add(
                            UnionCaseInfo(
                                shape.Name,
                                name,
                                id.Key,
                                fieldTys,
                                shape.FieldNames,
                                declSite.Key,
                                AttributeFold.resolveAndBuild ctx AttrTarget.UnionCase caseAttrs
                            )
                        )
                    | ValueNone -> ()
            )

        let caseInfos = caseInfos.ToArray()

        let info =
            UnionTypeInfo(name, typeParams, caseInfos, id.DeclSite, typarConstraints, id.Key)

        // `[<Struct>]` union ⇒ value type. Unions have no `struct … end` form, so the
        // attribute is the whole verdict.
        info.IsValueType <- isStructAttributed ctx tn

        info.Attributes <-
            Attributes.foldAndValidateTypeDefn
                ctx
                info.DefnKind
                declSite.Tok
                (ctx.ResolveAttributes(Attributes.attributesOfTypeName tn))

        rejectCustomOnDataType ctx declSite.Tok info.EqualitySupport info.ComparisonSupport

        TypeRegistry.registerUnion ctx.Types info

        // Record the decl-site identity so the type-decl emitter recovers the union by its
        // arity-qualified key rather than re-deriving `(name, arity)`.
        ctx.Resolution.ResolvedType.Set(declSite.Key, info.TypeKey)

        for c in caseInfos do
            prependToIndex ctx.Types.CtorIndex c.Name c
