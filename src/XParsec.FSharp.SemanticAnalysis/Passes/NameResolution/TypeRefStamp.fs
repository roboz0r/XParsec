namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// The `stamp*` helpers record a type reference's verdict on the same `CstKeys.ofTypeRef`
// derivation the read side keys on.

module NameResolutionTypeRefStamp =

    [<Struct>]
    type ExternalTypeHit =
        {
            /// The identity the ANSWERING provider registered, never re-cut from `Compiled`.
            Key: TypeKey
            /// The probed name that hit: open-prefix qualified, arity-suffixed if the probe was.
            Compiled: string
            /// The arity the PROBE asked for, not necessarily the shape's own: a bare-keyed
            /// generic (`Vesper.Option`, arity 1) hits the bare probe.
            ProbedTyparArity: int
            Shape: ExternalTypeShape
        }

    /// Per qualified candidate, probe every pair `probes` yields and take the first hit
    /// `pick` admits.
    let tryPickExternalType
        (ctx: PassContext)
        (probes: string -> struct (string * int) list)
        (pick: ExternalTypeHit -> 'T voption)
        (name: string)
        : 'T voption =
        let lookup (candidate: string) : 'T voption =
            let rec go (remaining: struct (string * int) list) =
                match remaining with
                | [] -> ValueNone
                | struct (probe, arity) :: rest ->
                    match ctx.Resolver.TryLookupType probe with
                    | ValueSome(struct (key, shape)) ->
                        let hit =
                            {
                                Key = key
                                Compiled = probe
                                ProbedTyparArity = arity
                                Shape = shape
                            }

                        match pick hit with
                        | ValueSome v -> ValueSome v
                        | ValueNone -> go rest
                    | ValueNone -> go rest

            go (probes candidate)

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup name

    /// Metadata keys a generic type `` Name`arity `` while the contract layer keys it bare,
    /// so the arity-suffixed name is probed first and wins when both could match.
    let arityProbes (arity: int) (candidate: string) : struct (string * int) list =
        if arity = 0 then
            [ struct (candidate, 0) ]
        else
            [
                struct (SymbolKeyOps.arityName candidate arity, arity)
                struct (candidate, arity)
            ]

    /// A NOMINAL type constructor takes the producer's REGISTERED key: only that preserves an `InModule`
    /// containment chain, a re-cut from the dotted spelling flattening the module segment into the
    /// namespace. The rest key off the compiled name, an abbrev dealiasing on read.
    let useSiteTypeKey (hit: ExternalTypeHit) : TypeKey =
        match hit.Shape with
        | ExternalTypeShape.Class _
        | ExternalTypeShape.IntrinsicInterface _
        | ExternalTypeShape.Record _
        | ExternalTypeShape.Union _
        | ExternalTypeShape.Enum _ -> hit.Key
        | ExternalTypeShape.Abbrev _
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.Opaque _ -> SymbolKeyOps.qualifiedTypeKeyOf hit.Compiled hit.ProbedTyparArity

    /// At exactly `arity`: a shape whose own typar count differs is not a hit.
    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : TypeKey voption =
        tryPickExternalType
            ctx
            (arityProbes arity)
            (fun hit ->
                if hit.Shape.TyparArity = hit.ProbedTyparArity then
                    ValueSome(useSiteTypeKey hit)
                else
                    ValueNone
            )
            name

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
                    cases |> Array.exists (fun (c: ExternalEnumCaseShape) -> c.Name = caseName)
                    ->
                    ValueSome(useSiteTypeKey hit)
                | _ -> ValueNone
            )
            anchorName

    /// A local claim WINS — including one qualified by a module of this file. The claims
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

    /// An attribute resolving to NOTHING is simply absent: most of F#'s vocabulary is
    /// undeclared in the Vesper contract, so rejecting unresolved names rejects every file.
    [<Struct; NoEquality; NoComparison>]
    type ResolvedAttributes =
        {
            Keys: System.Collections.Generic.HashSet<TypeKey>
        }

        member this.Has(k: TypeKey) : bool = this.Keys.Contains k

        static member None: ResolvedAttributes =
            {
                Keys = System.Collections.Generic.HashSet()
            }

    /// F#'s suffix rule: `Attribute`-suffixed FIRST, then as written — `[<Foo>]` binds
    /// `FooAttribute` even where a non-attribute `Foo` is in scope. Deliberately unstamped.
    let tryResolveAttributeTypeKey (ctx: PassContext) (typ: Type<SyntaxToken>) : TypeKey voption =
        match CstKeys.ofTypeRef typ with
        | ValueNone -> ValueNone
        | ValueSome typeRef ->
            let useSite = ctx.UseSiteAt typeRef.Site.Key
            let written = ctx.WrittenTypeNameOf typeRef.LongIdent

            let tryName (name: string) : TypeKey voption =
                let w = { written with Name = name }

                match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite w typeRef.TyparArity with
                | ValueSome claim -> ValueSome claim.Key
                | ValueNone -> tryResolveExternalTypeKey ctx w.Written typeRef.TyparArity

            match tryName (written.Name + "Attribute") with
            | ValueSome k -> ValueSome k
            | ValueNone -> tryName written.Name

    /// Each declared marker name and, per F#'s optional-suffix rule, the same name without
    /// `Attribute`.
    let private compilerMarkerLeaves: System.Collections.Generic.HashSet<string> =
        System.Collections.Generic.HashSet<string>(
            seq {
                for k in RuntimeNames.compilerAttributeKeys do
                    yield k.Name
                    yield k.Name.Substring(0, k.Name.Length - RuntimeNames.AttributeSuffix.Length)
            }
        )

    /// An unresolved name that SPELLS a compiler marker: the author asked for a meaning the
    /// compiler has and got none, and silence ships a record with the posture it refused.
    let private reportUnresolvedMarker (ctx: PassContext) (typ: Type<SyntaxToken>) : unit =
        match CstKeys.ofTypeRef typ with
        | ValueNone -> ()
        | ValueSome typeRef ->
            let written = ctx.WrittenTypeNameOf typeRef.LongIdent

            if compilerMarkerLeaves.Contains written.Name then
                ctx.Report(
                    typeRef.Site.Tok,
                    Kind.Message(
                        sprintf
                            "'%s' names no type here, so this attribute is not the compiler marker it spells and would have no effect. Reference the contract that declares it, or qualify the path to the type meant."
                            written.Written
                    )
                )

    let resolveAttributes (ctx: PassContext) (attrs: Attributes<SyntaxToken> voption) : ResolvedAttributes =
        match attrs with
        | ValueNone -> ResolvedAttributes.None
        | ValueSome sets ->
            let keys = System.Collections.Generic.HashSet()

            for AttributeSet(attributes = entries) in sets do
                for Attribute(construction = construction), _sep in entries do
                    let attrTy =
                        match construction with
                        | ObjectConstruction(typ = t) -> t
                        | InterfaceConstruction(typ = t) -> t

                    match tryResolveAttributeTypeKey ctx attrTy with
                    | ValueSome k -> keys.Add k |> ignore
                    | ValueNone -> reportUnresolvedMarker ctx attrTy

            { Keys = keys }

    /// A structural shape applies no type name, so it records no verdict.
    let stampTypeIter (ctx: PassContext) : CstWalk.TypeIter =
        { CstWalk.identityTypeIter with
            VisitType =
                fun _ t ->
                    match CstKeys.ofTypeRef t with
                    | ValueSome typeRef -> classifyTypeRef ctx typeRef |> ignore
                    | ValueNone -> ()

                    true
        }

    let stampTypeRefs (ctx: PassContext) (ty: Type<SyntaxToken>) : unit = CstWalk.iterType (stampTypeIter ctx) ty

    let stampMemberSig (ctx: PassContext) (ms: MemberSig<SyntaxToken>) : unit =
        CstWalk.iterTypeMemberSig (stampTypeIter ctx) ms

    /// A type header's trailing constraints hang off `TypeName`, reached by no other stamper —
    /// so a coercion bound there must be stamped here or codegen cannot lower its `.Invoke`.
    let stampTyparConstraints (ctx: PassContext) (cs: TyparConstraints<SyntaxToken>) : unit =
        CstWalk.iterTypeConstraints (stampTypeIter ctx) cs

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
