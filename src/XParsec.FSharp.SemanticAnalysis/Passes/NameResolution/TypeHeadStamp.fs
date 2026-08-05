namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// The type-annotation half of NameResolution's resolve-once boundary: `tryPickExternalType`
// is the ONE opens-aware spelling→identity engine, and the `stamp*` helpers record its
// verdict on the same `CstKeys.ofTypeHead` derivation the read side keys on.

module NameResolutionTypeHeadStamp =

    /// The FIRST `TryLookupType` hit, whatever its shape. Every verdict about a name derives
    /// from this one hit, not from several filtered scans that could disagree.
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

    /// Per qualified candidate — bare first, then each `open` prefix — probe every pair
    /// `probes` yields and take the first hit `pick` admits, falling through on `ValueNone`.
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

    /// A NOMINAL head takes the producer's REGISTERED key: only that preserves an `InModule`
    /// holder chain, a re-cut from the dotted spelling flattening the module segment into the
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

    /// At exactly `arity` — the receiver's type-arg count, from the enclosing `Expr.TypeApp`.
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

    /// An enum-case access `E.C1` resolved to the enum's nominal key, at arity 0. Recognised
    /// HERE, so the Unification enum arms read the stamp rather than the spelling.
    let tryExternalEnumCaseKey (ctx: PassContext) (headName: string) (caseName: string) : TypeKey voption =
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
            headName

    /// What a written type head NAMES. The three outcomes are exhaustive and mutually
    /// exclusive, which is what makes this the ONE local/external precedence rule.
    [<Struct>]
    type TypeHeadVerdict =
        /// Deliberately left UNSTAMPED: `translateType` reads it off the registry.
        | LocalType
        /// Resolved at the head's syntactic arity, and stamped into `ResolvedTypeHead`.
        | ExternalType
        /// No claim in scope, no reachable external type.
        | UnknownType

    /// A local claim WINS — including one qualified by a module of this file — so a stamped
    /// head is external for good. The claims consulted are those VISIBLE AT THE HEAD, so one
    /// written ABOVE a same-named local declaration sees none: F#'s file-order shadowing.
    let classifyTypeHead (ctx: PassContext) (head: CstKeys.TypeHead) : TypeHeadVerdict =
        let written = ctx.WrittenTypeNameOf head.LongIdent

        if TypeRegistry.isWrittenTypeNameInScope ctx.Types (ctx.UseSiteAt head.Site.Key) written then
            LocalType
        else
            match tryResolveExternalTypeKey ctx written.Written head.TyparArity with
            | ValueSome sym ->
                ctx.Resolution.ResolvedTypeHead.Set(head.Site.Key, sym)
                ExternalType
            | ValueNone -> UnknownType

    /// An attribute resolving to NOTHING is simply absent: most of F#'s vocabulary is
    /// undeclared in the Vesper contract, so rejecting unresolved heads rejects every file.
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
        match CstKeys.ofTypeHead typ with
        | ValueNone -> ValueNone
        | ValueSome head ->
            let useSite = ctx.UseSiteAt head.Site.Key
            let written = ctx.WrittenTypeNameOf head.LongIdent

            let tryName (name: string) : TypeKey voption =
                let w = { written with Name = name }

                match TypeRegistry.tryWrittenTypeClaim ctx.Types useSite w head.TyparArity with
                | ValueSome claim -> ValueSome claim.Key
                | ValueNone -> tryResolveExternalTypeKey ctx w.Written head.TyparArity

            match tryName (written.Name + "Attribute") with
            | ValueSome k -> ValueSome k
            | ValueNone -> tryName written.Name

    /// Each declared marker name and, per F#'s optional-suffix rule, the same name without
    /// `Attribute`. Derived from the KEYS, so spellings cannot drift from the identities.
    let private compilerMarkerLeaves: System.Collections.Generic.HashSet<string> =
        System.Collections.Generic.HashSet<string>(
            seq {
                for k in RuntimeNames.compilerAttributeKeys do
                    yield k.Name
                    yield k.Name.Substring(0, k.Name.Length - RuntimeNames.AttributeSuffix.Length)
            }
        )

    /// An unresolved head that SPELLS a compiler marker: the author asked for a meaning the
    /// compiler has and got none, and silence ships a record with the posture it refused.
    let private reportUnresolvedMarker (ctx: PassContext) (typ: Type<SyntaxToken>) : unit =
        match CstKeys.ofTypeHead typ with
        | ValueNone -> ()
        | ValueSome head ->
            let written = ctx.WrittenTypeNameOf head.LongIdent

            if compilerMarkerLeaves.Contains written.Name then
                ctx.Report(
                    head.Site.Tok,
                    Kind.Message(
                        sprintf
                            "'%s' names no type here, so this attribute is not the compiler marker it spells and would have no effect. Reference the contract that declares it, or qualify the path to the type meant."
                            written.Written
                    )
                )

    /// Resolve every attribute in a declaration's `[<…>]` sets to the type it names.
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

    /// `iterType`'s recursion mirrors `translateType`'s, so the two walks agree node-for-node.
    /// A local / bare-typar / unknown head stays unstamped; an abbrev stamps its OWN key.
    let stampTypeIter (ctx: PassContext) : CstWalk.TypeIter =
        { CstWalk.identityTypeIter with
            VisitType =
                fun _ t ->
                    match CstKeys.ofTypeHead t with
                    | ValueSome head -> classifyTypeHead ctx head |> ignore
                    | ValueNone -> ()

                    true
        }

    let stampTypeHeads (ctx: PassContext) (ty: Type<SyntaxToken>) : unit = CstWalk.iterType (stampTypeIter ctx) ty

    /// Every arg and return type in a member signature's curried shape.
    let stampMemberSig (ctx: PassContext) (ms: MemberSig<SyntaxToken>) : unit =
        CstWalk.iterTypeMemberSig (stampTypeIter ctx) ms

    /// A type header's trailing constraints hang off `TypeName`, reached by no other stamper —
    /// so a coercion bound there must be stamped here or codegen cannot lower its `.Invoke`.
    let stampTyparConstraints (ctx: PassContext) (cs: TyparConstraints<SyntaxToken>) : unit =
        CstWalk.iterTypeConstraints (stampTypeIter ctx) cs

    /// Every arg type and the return type of an uncurried signature.
    let stampUncurriedSig (ctx: PassContext) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            stampTypeHeads ctx t

        stampTypeHeads ctx ret

    /// A binding's return-type annotation. Its pattern annotations are stamped by
    /// `stampPatCases`, which already runs at every pattern-scope site.
    let stampBindingSigTypes (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.returnType with
        | ValueSome(ReturnType(typ = t)) -> stampTypeHeads ctx t
        | ValueNone -> ()

    /// Only what hangs off `e` itself; recursing into children is the walker's job. The
    /// position enumeration sits beside `iterExpr`'s, so a new parser case fails there.
    let stampExprEmbeddedTypes (ctx: PassContext) (e: Expr<SyntaxToken>) : unit =
        CstWalk.iterExprEmbeddedTypes (stampTypeHeads ctx) (stampMemberSig ctx) e
