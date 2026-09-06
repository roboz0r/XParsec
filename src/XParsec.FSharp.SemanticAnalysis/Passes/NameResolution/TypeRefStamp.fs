namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open NameResolutionLongIdent

// The `stamp*` helpers record a type reference's verdict on the same `CstKeys.ofTypeRef`
// derivation the read side keys on.

module NameResolutionTypeRefStamp =

    /// A local claim WINS at the arity written, including one qualified by a module of this
    /// file. A bare `T` is arity 0; a name claimed only at another arity is still that type,
    /// and reports FS0033. Claims apply only where VISIBLE AT THE USE SITE (file-order shadowing).
    let classifyTypeRef (ctx: PassContext) (typeRef: CstKeys.TypeRef) : TypeRefVerdict =
        match ctx.Resolution.TypeRefVerdicts.TryGetValue typeRef.Site.Key with
        | ValueSome verdict -> verdict
        | ValueNone ->
            let written = ctx.WrittenTypeNameOf typeRef.LongIdent

            let verdict =
                match resolveType ctx (ctx.UseSiteAt typeRef.Site.Key) written typeRef.TyparArity with
                | TypeNameResolution.Type(ResolvedTypeRef.Local claim) -> TypeRefVerdict.LocalType claim
                | TypeNameResolution.Type(ResolvedTypeRef.External(key, shape)) ->
                    TypeRefVerdict.ExternalType(key, shape)
                | TypeNameResolution.LocalAtOtherArity claim ->
                    ctx.Report(
                        typeRef.Site.Tok,
                        Kind.TypeArgArity(written.Written, claim.TyparArity, typeRef.TyparArity)
                    )

                    TypeRefVerdict.LocalTypeAtOtherArity claim
                | TypeNameResolution.ExternalAtOtherArity(key, shape) ->
                    ctx.Report(
                        typeRef.Site.Tok,
                        Kind.TypeArgArity(written.Written, shape.TyparArity, typeRef.TyparArity)
                    )

                    TypeRefVerdict.ExternalTypeAtOtherArity(key, shape)
                | TypeNameResolution.Unresolved _ -> TypeRefVerdict.UnknownType

            ctx.Resolution.TypeRefVerdicts.Set(typeRef.Site.Key, verdict)
            verdict

    /// A structural shape applies no type name, so it records no verdict.
    let stampTypeIter (ctx: PassContext) : CstTypeWalk.TypeIter =
        let visitType _ (t: Type<SyntaxToken>) =
            match CstKeys.ofTypeRef t with
            | ValueSome typeRef -> classifyTypeRef ctx typeRef |> ignore
            | ValueNone -> ()

            true

        {
            VisitType = visitType
            VisitMeasureName = fun li -> classifyTypeRef ctx (CstKeys.namedTypeRef li) |> ignore
        }

    let stampTypeRefs (ctx: PassContext) (ty: Type<SyntaxToken>) : unit =
        CstTypeWalk.iterType (stampTypeIter ctx) ty

    let stampMemberSig (ctx: PassContext) (ms: MemberSig<SyntaxToken>) : unit =
        CstTypeWalk.iterTypeMemberSig (stampTypeIter ctx) ms

    /// A type header's trailing constraints hang off `TypeName`, reached by no other stamper,
    /// so a coercion bound there must be stamped here or codegen cannot lower its `.Invoke`.
    let stampTyparConstraints (ctx: PassContext) (cs: TyparConstraints<SyntaxToken>) : unit =
        CstTypeWalk.iterTypeConstraints (stampTypeIter ctx) cs

    let stampUncurriedSig (ctx: PassContext) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            stampTypeRefs ctx t

        stampTypeRefs ctx ret

    /// A binding's return-type annotation only; its pattern annotations are stamped at
    /// every pattern-scope site instead.
    let stampBindingSigTypes (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.returnType with
        | ValueSome(ReturnType(typ = t)) -> stampTypeRefs ctx t
        | ValueNone -> ()

    /// Only what hangs off `e` itself; recursing into children is the walker's job.
    let stampExprEmbeddedTypes (ctx: PassContext) (e: Expr<SyntaxToken>) : unit =
        CstWalk.iterExprEmbeddedTypes (stampTypeIter ctx) e
