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

    /// The single opens-aware spelling→shape engine behind every external-type
    /// resolution. Applies the in-scope `open` prefixes (`tryResolve`'s candidate
    /// order: bare/abbrev-expanded then each prefix); per qualified candidate it
    /// probes `ctx.Resolver.TryLookupType` with each `(compiled key, arity)` pair
    /// `probes` yields and accepts the first hit `pick` admits. `pick` returning
    /// `ValueNone` (wrong shape / wrong arity) falls through to the next probe,
    /// then the next `open` prefix.
    let tryPickExternalType
        (ctx: PassContext)
        (probes: string -> struct (string * int) list)
        (pick: string -> int -> ExternalTypeShape -> 'T voption)
        (name: string)
        : 'T voption =
        let lookup (candidate: string) : 'T voption =
            let rec go (remaining: struct (string * int) list) =
                match remaining with
                | [] -> ValueNone
                | struct (key, arity) :: rest ->
                    match ctx.Resolver.TryLookupType key with
                    | ValueSome shape ->
                        match pick key arity shape with
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

    /// Mint the use-site `SymbolKey` from the matched shape's origin + compiled name.
    /// Class/Union/Record/Enum carry the home assembly + namespace; the origin-less
    /// shapes fall back to splitting the qualified compiled name. Mirrors
    /// `Translate`'s nominal mint, so the stamped key round-trips through the
    /// key-addressed store face.
    let useSiteTypeKey (compiled: string) (arity: int) (shape: ExternalTypeShape) : SymbolKey =
        match shape with
        | ExternalTypeShape.Class info -> SymbolKeyOps.externalTypeKey info.Origin compiled arity
        // A capability interface's VALUE identity key is origin-homed (asm-qualified),
        // exactly as a `Class`'s — NOT the asm-blind canon the `Intrinsic` arm uses.
        | ExternalTypeShape.IntrinsicInterface s -> SymbolKeyOps.externalTypeKey s.Origin compiled arity
        | ExternalTypeShape.Record(origin = o)
        | ExternalTypeShape.Union(origin = o)
        | ExternalTypeShape.Enum(origin = o) -> SymbolKeyOps.externalTypeKey o compiled arity
        | ExternalTypeShape.Abbrev _
        // An intrinsic's identity is the canon (asm-blind), keyed off the compiled
        // name — the optional base/ctor surface does not change the key.
        | ExternalTypeShape.Intrinsic _
        | ExternalTypeShape.Opaque _ -> SymbolKeyOps.qualifiedTypeKey compiled arity

    /// Resolve `name` (possibly dotted) as an external *type* at exactly `arity` —
    /// the receiver's type-arg count, supplied by the enclosing `Expr.TypeApp`
    /// (0 for a non-generic static-access receiver like `System.Console`). Any
    /// shape at the matching arity; returns the use-site `SymbolKey`.
    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : SymbolKey voption =
        tryPickExternalType
            ctx
            (arityProbes arity)
            (fun key a shape ->
                if shape.Arity = a then
                    ValueSome(useSiteTypeKey key a shape)
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

    /// One committed classification of a written name against the external
    /// universe: the FIRST `TryLookupType` hit across the probe list (under each
    /// open prefix in candidate order), whatever its shape; consumers filter by
    /// shape/arity. This is the resolve-once discipline applied to classification
    /// itself — a written name IS one thing, so every verdict about it (stamp,
    /// diagnostic suppression) derives from the one hit rather than from several
    /// differently-filtered scans that could disagree on which hit they see.
    [<Struct>]
    type ExternalTypeHit =
        {
            /// The probed compiled name that hit (open-prefix qualified,
            /// arity-suffixed where the hitting probe was).
            Compiled: string
            /// The arity the hitting probe asked for — NOT necessarily the shape's
            /// own (`Shape.Arity`): a bare-keyed generic (`Vesper.Option`, arity 1)
            /// hits the bare probe (`ProbedArity` 0).
            ProbedArity: int
            Shape: ExternalTypeShape
        }

    let tryClassifyExternalType
        (ctx: PassContext)
        (probes: string -> struct (string * int) list)
        (name: string)
        : ExternalTypeHit voption =
        tryPickExternalType
            ctx
            probes
            (fun key a shape ->
                ValueSome
                    {
                        Compiled = key
                        ProbedArity = a
                        Shape = shape
                    }
            )
            name

    /// Resolve an external enum-case access `E.C1` (`headName` = `E`, `caseName` = `C1`)
    /// to the enum's nominal `SymbolKey`: `E` qualified — opens-aware — through the active
    /// `open`s to an external `ExternalTypeShape.Enum` that declares `caseName`, arity-0
    /// (enums are never generic). The key matches the annotation mint for an `(x: E)`
    /// annotation, so the access/pattern unifies with the annotation. `ValueNone` when no
    /// reachable external enum named `headName` declares `caseName`. NameResolution — the
    /// resolve-once layer — recognises the case HERE and stamps the key
    /// (`ExternalEnumCaseStamp`); Unification's `InferIdentExpr` / `InferPat` enum arms READ
    /// the stamp rather than re-recognising the spelling through the resolver face.
    let tryExternalEnumCaseKey (ctx: PassContext) (headName: string) (caseName: string) : SymbolKey voption =
        tryPickExternalType
            ctx
            (arityProbes 0)
            (fun key _ shape ->
                match shape with
                | ExternalTypeShape.Enum(cases, origin) when
                    cases |> Array.exists (fun (c: ExternalEnumCaseShape) -> c.Name = caseName)
                    ->
                    ValueSome(SymbolKeyOps.externalTypeKey origin key 0)
                | _ -> ValueNone
            )
            headName

    /// The `CstWalk.iterType` visitor that stamps every written external *type head*
    /// reachable from a `Type` into `ResolvedTypeHead`. Each head is decomposed ONCE
    /// through `CstKeys.ofTypeHead` (key + long-ident + syntactic arity), resolved —
    /// opens-aware, at that arity — through `tryResolveExternalTypeKey`, and its
    /// `SymbolKey` recorded under the head's `NodeKey`. `Translate.tryResolveExternalType`
    /// then reads the stamp and fetches the shape through the key-addressed store face
    /// instead of re-resolving the spelling. `iterType`'s recursion reaches every nested
    /// head (generic args, function/tuple members, `when`-constraint types), so a single
    /// call over a top-level annotation stamps the whole tree — mirroring `translateType`'s
    /// own recursion, so the two faces agree node-for-node.
    ///
    /// A project-local / bare-typar / unreachable head misses the resolver and stays
    /// unstamped; `translateType` then takes its local-registry / opaque / `TyVar`
    /// paths. An abbrev head stamps its OWN key (the resolver's `keyOf` returns it);
    /// `translateType` dealiases on read.
    let private stampTypeIter (ctx: PassContext) : CstWalk.TypeIter =
        { CstWalk.identityTypeIter with
            VisitType =
                fun _ t ->
                    match CstKeys.ofTypeHead t with
                    | ValueSome head ->
                        let name = head.LongIdent.Idents |> Seq.map ctx.NameOf |> String.concat "."

                        match tryResolveExternalTypeKey ctx name head.Arity with
                        | ValueSome sym -> ctx.Resolution.ResolvedTypeHead.Set(head.Key, sym)
                        | ValueNone -> ()
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
