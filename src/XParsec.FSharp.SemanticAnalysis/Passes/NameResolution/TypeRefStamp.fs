namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// The `stamp*` helpers record a type reference's verdict on the same `CstKeys.ofTypeRef`
// derivation the read side keys on.

module NameResolutionTypeRefStamp =

    open ExternalTypeProbe

    /// A qualifier written without type args: its arity is not recoverable at the use site,
    /// so probe bare, then `` `1 ``..`` `4 ``.
    let qualifierProbes (candidate: string) : struct (string * int) list =
        [
            for a in 0..4 ->
                struct ((if a = 0 then
                             candidate
                         else
                             SymbolKeyOps.arityName candidate a),
                        a)
        ]

    /// The unfiltered `tryPickExternalType`: the first hit, whatever it is.
    let tryClassifyExternalType
        (ctx: PassContext)
        (probes: string -> struct (string * int) list)
        (name: string)
        : ExternalTypeHit voption =
        tryPickExternalType ctx probes ValueSome name

    /// An enum-case access `E.C1` resolved to the enum's nominal key, at arity 0.
    let tryExternalEnumCaseKey (ctx: PassContext) (anchorName: string) (caseName: string) : TypeKey voption =
        tryPickExternalType
            ctx
            (arityProbes 0)
            (fun hit ->
                match hit.Shape with
                | ExternalTypeShape.Enum(cases = cases) when
                    cases |> EqArray.exists (fun (c: ExternalEnumCaseShape) -> c.Name = caseName)
                    ->
                    ValueSome hit.UseSiteKey
                | _ -> ValueNone
            )
            anchorName

    /// A local claim WINS, including one qualified by a module of this file. The claims
    /// consulted are those VISIBLE AT THE USE SITE, so one written ABOVE a same-named local
    /// declaration sees none: F#'s file-order shadowing.
    let classifyTypeRef (ctx: PassContext) (typeRef: CstKeys.TypeRef) : TypeRefVerdict =
        let written = ctx.WrittenTypeNameOf typeRef.LongIdent

        let verdict =
            if TypeRegistry.isWrittenTypeNameInScope ctx.Types (ctx.UseSiteAt typeRef.Site.Key) written then
                TypeRefVerdict.LocalType
            else
                match tryResolveExternalTypeKey ctx written.Written typeRef.TyparArity with
                | ValueSome sym -> TypeRefVerdict.ExternalType sym
                | ValueNone -> TypeRefVerdict.UnknownType

        ctx.Resolution.TypeRefVerdicts.Set(typeRef.Site.Key, verdict)
        verdict


    /// A structural shape applies no type name, so it records no verdict.
    let stampTypeIter (ctx: PassContext) : CstTypeWalk.TypeIter =
        { CstTypeWalk.identityTypeIter with
            VisitType =
                fun _ t ->
                    match CstKeys.ofTypeRef t with
                    | ValueSome typeRef -> classifyTypeRef ctx typeRef |> ignore
                    | ValueNone -> ()

                    true
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
        CstWalk.iterExprEmbeddedTypes (stampTypeRefs ctx) (stampMemberSig ctx) e
