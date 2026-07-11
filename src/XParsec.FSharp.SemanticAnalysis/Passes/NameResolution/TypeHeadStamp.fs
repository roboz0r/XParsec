namespace XParsec.FSharp.SemanticAnalysis.Passes

open System.Collections.Immutable
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis

// External type-head resolution + the type-annotation half of NameResolution's
// resolve-once boundary. `tryResolveExternalTypeKey` resolves a written type head's
// spelling to its external `SymbolKey` (opens-aware, at an exact arity); the `stamp*`
// helpers walk each declared-signature / annotation position and record that key in
// `ResolvedTypeHead` so Unification's `Translate` reads the key-addressed store face
// instead of re-resolving the spelling. Split out of `NameResolutionScope` (which owns
// value/ident resolution) because it is a self-contained unit keyed on the same
// `CstKeys.ofTypeHead` derivation the read side uses.

module NameResolutionTypeHeadStamp =

    /// Resolve `name` (possibly dotted) as an external *type* at exactly `arity` —
    /// the receiver's type-arg count, supplied by the enclosing `Expr.TypeApp`
    /// (0 for a non-generic static-access receiver like `System.Console`). Applies
    /// the in-scope `open` prefixes (`tryResolve`'s candidate order: bare/abbrev-
    /// expanded then each prefix); per qualified candidate it probes the arity-
    /// suffixed compiled name first (metadata keys a generic `Name`arity`; the
    /// contract layer keys it bare). Returns the use-site `SymbolKey` minted from
    /// the matched shape's origin + compiled name. This replaces the former bounded
    /// `[1;2;3;4]` arity scan: the arity is now exact because the TypeApp visit
    /// resolves receiver+arity together.
    let tryResolveExternalTypeKey (ctx: PassContext) (name: string) (arity: int) : SymbolKey voption =
        let keysFor (n: string) =
            if arity = 0 then
                [ n ]
            else
                [ SymbolKeyOps.arityName n arity; n ]

        let shapeArity (shape: ExternalTypeShape) =
            match shape with
            | ExternalTypeShape.Class info -> info.Arity
            | ExternalTypeShape.Intrinsic s -> s.Id.Arity
            | ExternalTypeShape.IntrinsicInterface s -> s.Arity
            | ExternalTypeShape.Enum _ -> 0 // enums are never generic
            | ExternalTypeShape.Record(arity = a)
            | ExternalTypeShape.Union(arity = a)
            | ExternalTypeShape.Abbrev(arity = a)
            | ExternalTypeShape.Opaque(arity = a) -> a

        // Mint from the matched shape's origin where one exists (Class/Union/Record
        // carry the home assembly + namespace); the origin-less shapes fall back to
        // splitting the qualified compiled name. Mirrors `Translate`'s nominal mint.
        let keyOf (compiled: string) (shape: ExternalTypeShape) =
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

        let lookup (candidate: string) : SymbolKey voption =
            let rec go (keys: string list) =
                match keys with
                | [] -> ValueNone
                | key :: rest ->
                    match ctx.Resolver.TryLookupType key with
                    | ValueSome shape when shapeArity shape = arity -> ValueSome(keyOf key shape)
                    | _ -> go rest

            go (keysFor candidate)

        OpenScope.tryResolve ctx.Resolution.OpenScope lookup name

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

    /// Stamp the type heads embedded *directly* in one expression node — the
    /// positions `translateType` reaches from expression inference: `new T(…)`, the
    /// explicit type args of `f<T>` / `T<T>`, the annotation / upcast / dynamic-test
    /// target types, a nested `let x : T = …` return type, an inline-IL body's
    /// result annotation (`(# "…" : T #)`, the sole external head an intrinsic-abbrev
    /// host member carries that neither its arg-pattern nor its return-type sig
    /// already stamps), and an object
    /// expression's construction + interface-impl types. Recursion into child
    /// *expressions* is the walker's job — this stamps only what hangs off `e`
    /// itself, so calling it once per visited node (the walker visits every node)
    /// reaches every expression-embedded type exactly once. The pattern annotations
    /// inside `fun`/`match`/`for` are stamped by `stampPatCases` at the scope hooks.
    let stampExprEmbeddedTypes (ctx: PassContext) (e: Expr<SyntaxToken>) : unit =
        match e with
        | Expr.New(typ = t)
        | Expr.TypeAnnotation(typ = t)
        | Expr.StaticUpcast(typ = t)
        | Expr.DynamicTypeTest(typ = t)
        | Expr.DynamicDowncast(typ = t) -> stampTypeHeads ctx t
        | Expr.ILIntrinsic(returnType = ValueSome(ReturnType(typ = t))) -> stampTypeHeads ctx t
        | Expr.TypeApp(types = types) ->
            for t in types do
                stampTypeHeads ctx t
        | Expr.LetOrUse(bindings = bindings) ->
            for b in bindings do
                stampBindingSigTypes ctx b
        | Expr.Object(baseCall = baseCall; interfaceImpls = impls) ->
            let ctorTy =
                match baseCall with
                | BaseCall.AnonBaseCall c
                | BaseCall.NamedBaseCall(construction = c) ->
                    match c with
                    | ObjectConstruction.ObjectConstruction(typ = t)
                    | ObjectConstruction.InterfaceConstruction(typ = t) -> t

            stampTypeHeads ctx ctorTy

            for InterfaceImpl.InterfaceImpl(typ = t) in impls do
                stampTypeHeads ctx t
        | _ -> ()
