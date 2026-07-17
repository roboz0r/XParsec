namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// External type resolution + the type-annotation half of NameResolution's
// resolve-once boundary. `tryPickExternalType` is the ONE opens-aware
// spelling→identity engine; the `tryResolve*Key` family are its shape-filtered
// instantiations, and the `stamp*` helpers walk each declared-signature /
// annotation position and record the resolved key in `ResolvedTypeHead` so
// Unification's `Translate` reads the key-addressed store face instead of
// re-resolving the spelling. Split out of `NameResolutionScope` (which owns
// value/ident resolution) because it is a self-contained unit keyed on the same
// `CstKeys.ofTypeHead` derivation the read side uses.

module NameResolutionTypeHeadStamp =

    /// One committed classification of a written name against the external universe: the
    /// FIRST `TryLookupType` hit across the probe list (under each open prefix in candidate
    /// order), whatever its shape; consumers filter by shape/arity. This is the
    /// resolve-once discipline applied to classification itself — a written name IS one
    /// thing, so every verdict about it (its identity, its stamp, diagnostic suppression)
    /// derives from the one hit rather than from several differently-filtered scans that
    /// could disagree on which hit they see.
    [<Struct>]
    type ExternalTypeHit =
        {
            /// The identity the ANSWERING provider registered for this type — never re-cut
            /// from `Compiled`, and never fetched by a second by-name scan that a composite
            /// could answer from a different provider than the one that resolved `Shape`.
            Key: TypeKey
            /// The probed compiled name that hit (open-prefix qualified,
            /// arity-suffixed where the hitting probe was).
            Compiled: string
            /// The arity the hitting probe asked for — NOT necessarily the shape's
            /// own (`Shape.TyparArity`): a bare-keyed generic (`Vesper.Option`, arity 1)
            /// hits the bare probe (`ProbedTyparArity` 0).
            ProbedTyparArity: int
            Shape: ExternalTypeShape
        }

    /// The single opens-aware spelling→identity engine behind every external-type
    /// resolution. Applies the in-scope `open` prefixes (`tryResolve`'s candidate
    /// order: bare/abbrev-expanded then each prefix); per qualified candidate it
    /// probes `ctx.Resolver.TryLookupType` with each `(compiled key, arity)` pair
    /// `probes` yields and accepts the first hit `pick` admits. `pick` returning
    /// `ValueNone` (wrong shape / wrong arity) falls through to the next probe,
    /// then the next `open` prefix.
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

    /// Probe candidates for a type written at exactly `arity`: metadata keys a
    /// generic type `` Name`arity `` while the contract layer keys it bare, so the
    /// arity-suffixed compiled name is probed first and wins when both could match.
    let arityProbes (arity: int) (candidate: string) : struct (string * int) list =
        if arity = 0 then
            [ struct (candidate, 0) ]
        else
            [
                struct (SymbolKeyOps.arityName candidate arity, arity)
                struct (candidate, arity)
            ]

    /// Mint the use-site `TypeKey` for a resolved external type head.
    ///
    /// A NOMINAL head (Class/IntrinsicInterface/Record/Union/Enum) takes the producer's
    /// REGISTERED key — the identity that came back WITH the shape, from the one provider
    /// that answered — rather than a key re-cut from the spelling. Only the registered key
    /// preserves an `InModule` holder chain: a module-held cross-unit type written by its
    /// dotted source name (`Test.A.M.R`) has canonical key `{InModule M in Test.A, R}`, but
    /// a re-cut would flatten the module segment into the namespace (`{InNamespace
    /// Test.A.M, R}`) — an unequal identity that mismatches the one construction pins via
    /// `ExternalRecordCandidate.TypeKey`.
    ///
    /// The non-nominal shapes are keyed off the compiled name instead, and NOT off the
    /// registered key: an Abbrev dealiases on read, and an intrinsic's identity is the canon
    /// keyed off its compiled name — identical by construction to the canon the extractor
    /// stamped (`SymbolKeyOps.intrinsicCanonKey`), which is what makes the stamp and the
    /// shape agree. That is the same key for a name-indexed producer, but a key-indexed one
    /// registers intrinsics too (`FrozenSignature`'s `IntrinsicReprKeys` projection), and
    /// its registered key is the one thing here that is not the canon.
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

    /// Resolve `name` (possibly dotted) as an external *type* at exactly `arity` —
    /// the receiver's type-arg count, supplied by the enclosing `Expr.TypeApp`
    /// (0 for a non-generic static-access receiver like `System.Console`). Any
    /// shape at the matching arity; returns the use-site `TypeKey`.
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

    /// Probe candidates for a qualifier written without type args — its arity is
    /// not recoverable at the use site, so probe bare, then `` `1 ``..`` `4 ``.
    let qualifierProbes (candidate: string) : struct (string * int) list =
        [
            for a in 0..4 ->
                struct ((if a = 0 then
                             candidate
                         else
                             SymbolKeyOps.arityName candidate a),
                        a)
        ]

    /// The unfiltered `tryPickExternalType`: take the first hit whatever it is, and let the
    /// caller decide. See `ExternalTypeHit`.
    let tryClassifyExternalType
        (ctx: PassContext)
        (probes: string -> struct (string * int) list)
        (name: string)
        : ExternalTypeHit voption =
        tryPickExternalType ctx probes ValueSome name

    /// Resolve an external enum-case access `E.C1` (`headName` = `E`, `caseName` = `C1`)
    /// to the enum's nominal `SymbolKey`: `E` qualified — opens-aware — through the active
    /// `open`s to an external `ExternalTypeShape.Enum` that declares `caseName`, arity-0
    /// (enums are never generic). The key matches the annotation mint for an `(x: E)`
    /// annotation, so the access/pattern unifies with the annotation. `ValueNone` when no
    /// reachable external enum named `headName` declares `caseName`. NameResolution — the
    /// resolve-once layer — recognises the case HERE and stamps the key
    /// (`ExternalEnumCaseStamp`); Unification's `InferIdentExpr` / `InferPat` enum arms READ
    /// the stamp rather than re-recognising the spelling through the resolver face.
    let tryExternalEnumCaseKey (ctx: PassContext) (headName: string) (caseName: string) : TypeKey voption =
        tryPickExternalType
            ctx
            (arityProbes 0)
            (fun hit ->
                match hit.Shape with
                | ExternalTypeShape.Enum(cases = cases) when
                    cases |> Array.exists (fun (c: ExternalEnumCaseShape) -> c.Name = caseName)
                    ->
                    // THE annotation mint, not a re-cut of it: "matches the `(x: E)` key" is
                    // a property this must HAVE, so it is taken from the one function that
                    // decides it rather than restated here and kept in step by hand.
                    ValueSome(useSiteTypeKey hit)
                | _ -> ValueNone
            )
            headName

    /// What a written type head NAMES. The three outcomes are exhaustive and mutually
    /// exclusive, which is what makes this the ONE local/external precedence rule.
    [<Struct>]
    type TypeHeadVerdict =
        /// A name a project-local claim already holds — bare (`T`), or qualified by the
        /// module that holds it (`A.T`, `N.A.T`). Deliberately left UNSTAMPED:
        /// `translateType` reads it off the registry.
        | LocalType
        /// Resolved — opens-aware, at the head's syntactic arity — through the external
        /// universe, and stamped into `ResolvedTypeHead`.
        | ExternalType
        /// Names nothing: no claim in scope, no reachable external type.
        | UnknownType

    /// Classify ONE written type head and, when it is external, stamp its `SymbolKey` into
    /// `ResolvedTypeHead` so `Translate` fetches the shape through the key-addressed store
    /// face instead of re-resolving the spelling. Each head is decomposed ONCE through
    /// `CstKeys.ofTypeHead` (key + long-ident + syntactic arity) — the SAME derivation the
    /// read side keys on, so write and read agree by construction.
    ///
    /// THE precedence rule, and the reason it is a classification rather than two
    /// independent probes: a local claim WINS, so a head whose name is claimed is never
    /// stamped, and a head that IS stamped is therefore external for good — the read side
    /// prefers the stamp over the registry. The claims consulted are those VISIBLE AT THE
    /// HEAD — `head.Key` is where it is written — so a head written ABOVE a same-named local
    /// declaration sees no claim, stamps external, and keeps resolving to the external type
    /// even once the local one is registered. That is F#'s file-order shadowing rule
    /// (`open System` + a `type Uri` declared below a use of `Uri` binds `System.Uri`), and
    /// it falls out of the head's POSITION alone, not out of when the walk reaches it.
    ///
    /// A head is LOCAL by the claims in scope where it is written, whether it is written bare
    /// (`T`) or qualified by the module holding it (`A.T`, `N.A.T`) — the qualifier names a
    /// scope of this unit, so the type it selects there is as local as a bare one, and beats
    /// an external type of the same dotted spelling.
    let classifyTypeHead (ctx: PassContext) (head: CstKeys.TypeHead) : TypeHeadVerdict =
        let written = ctx.WrittenTypeNameOf head.LongIdent

        if TypeRegistry.isWrittenTypeNameInScope ctx.Types (ctx.UseSiteAt head.Key) written then
            LocalType
        else
            match tryResolveExternalTypeKey ctx written.Written head.TyparArity with
            | ValueSome sym ->
                ctx.Resolution.ResolvedTypeHead.Set(head.Key, sym)
                ExternalType
            | ValueNone -> UnknownType

    /// The `CstWalk.iterType` visitor that classifies + stamps every written type head
    /// reachable from a `Type`. `iterType`'s recursion reaches every nested head (generic
    /// args, function/tuple members, `when`-constraint types), so a single call over a
    /// top-level annotation covers the whole tree — mirroring `translateType`'s own
    /// recursion, so the two faces agree node-for-node.
    ///
    /// A local / bare-typar / unknown head stays unstamped; `translateType` then takes its
    /// local-registry / opaque / `TyVar` paths. An abbrev head stamps its OWN key (the
    /// resolver's `keyOf` returns it); `translateType` dealiases on read.
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

    /// Stamp the type heads of a member signature (`abstract M : T -> U`, an SRTP
    /// trait sig) — every arg and return type in its curried signature. Reuses
    /// `CstWalk`'s sig recursion with the same head-stamping visitor.
    let stampMemberSig (ctx: PassContext) (ms: MemberSig<SyntaxToken>) : unit =
        CstWalk.iterTypeMemberSig (stampTypeIter ctx) ms

    /// Stamp the type heads inside a `when`-constraint block. A `Type` position
    /// recurses its OWN inline `when` clause through `iterType`
    /// (`Type.WhenConstrainedType`), but a *type header*'s trailing typar-definition
    /// constraints (`type M<'F when 'F :> Fun<'T,'U>>`) hang off `TypeName`, reached
    /// by neither the field/member/param stampers nor `iterType` — so a coercion bound
    /// there (`Fun<'T,'U>`) must be stamped here for the constraint-resolution phase to
    /// read the store face rather than re-resolve the spelling (without it a
    /// struct-function typar keeps a bare-typar `.Invoke` that codegen cannot lower).
    let stampTyparConstraints (ctx: PassContext) (cs: TyparConstraints<SyntaxToken>) : unit =
        CstWalk.iterTypeConstraints (stampTypeIter ctx) cs

    /// Stamp the type heads of an uncurried signature (`DelegateSig`, a GADT case's
    /// `Name : arg -> ret`) — every arg type and the return type.
    let stampUncurriedSig (ctx: PassContext) (sign: UncurriedSig<SyntaxToken>) : unit =
        let (UncurriedSig(args = ArgsSpec.ArgsSpec(args = args); returnType = ret)) = sign

        for (ArgSpec(typ = t)) in args do
            stampTypeHeads ctx t

        stampTypeHeads ctx ret

    /// Stamp the type heads of a binding's *signature* — its return-type annotation.
    /// The binding's pattern annotations (`headPat` / `argumentPats`, `(x: T)`) are
    /// stamped by `stampPatCases`, which already runs at every pattern-scope site;
    /// only the `returnType` is not a pattern, so it is stamped here. Called wherever
    /// a binding is processed for name resolution (module lets, member defns, nested
    /// lets, secondary ctors).
    let stampBindingSigTypes (ctx: PassContext) (b: Binding<SyntaxToken>) : unit =
        match b.returnType with
        | ValueSome(ReturnType(typ = t)) -> stampTypeHeads ctx t
        | ValueNone -> ()

    /// Stamp the type heads embedded *directly* in one expression node. The
    /// position enumeration lives in `CstWalk.iterExprEmbeddedTypes` — exhaustive
    /// over `Expr`, beside `iterExpr`'s own enumeration, so a new parser case
    /// fails the incomplete-match check there instead of silently going unstamped
    /// here (the read side has no by-name fallback). Recursion into child
    /// *expressions* is the walker's job — this stamps only what hangs off `e`
    /// itself, so calling it once per visited node (the walker visits every node)
    /// reaches every expression-embedded type exactly once. The pattern annotations
    /// inside `fun`/`match`/`for` are stamped by `stampPatCases` at the scope hooks.
    let stampExprEmbeddedTypes (ctx: PassContext) (e: Expr<SyntaxToken>) : unit =
        CstWalk.iterExprEmbeddedTypes (stampTypeHeads ctx) (stampMemberSig ctx) e
