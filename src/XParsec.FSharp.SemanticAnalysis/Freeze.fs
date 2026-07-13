namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Generic

open XParsec.FSharp.SemanticAnalysis.Passes

// The genuine freeze: the single
// `SemType → FrozenType` rebuild, run as the FINAL SemanticAnalysis pipeline step
// on the typar-quantified, SemType-domain-settled tree that `Elaborate.run` (+
// `Regions` / `RefCellPromotion` / `ResolvedTypes`) produced. After this the
// assembly's output tree (`Pipeline.analyse`'s result) holds no `SemType.TyVar`:
// an open typar is the self-describing `FrozenType.FTTypar`; a metavar is
// unrepresentable by construction, in codegen's input AND in the assembly output.
//
// One O(n) cross-type structural map (`TastConvert.file toFrozen`). `toFrozen` is
// total on the post-freeze subset and a hard error on a stray `TyVar` — an
// inference/elaboration bug that `ResolvedTypes` (which runs upstream, on the
// SemType tree) would already have surfaced as a graceful per-decl diagnostic.

[<RequireQualifiedAccess>]
module Freeze =

    /// Attribute each typar root to the BINDER whose generalized scheme quantified
    /// it, and to its index within that scheme.
    ///
    /// The residue reaching freeze is not *free* — it is BOUND, by a binder that is
    /// not the enclosing method (see `FrozenType.FTLocalTypar`). A body-local
    /// `let g = fun x -> x` is its own declaration with its own generalized scheme,
    /// and `Elaborate.mkMethodQuantEnv` — which derives the `TyVar -> TyTypar` remap
    /// by walking the ENCLOSING decl's type — never sees `g`'s own root, because
    /// every use of `g` instantiates away from it. So the root survives as a `TyVar`
    /// on `g`'s own nodes.
    ///
    /// `ctx.Bindings.Scheme` is the AUTHORITATIVE record of which roots a binder
    /// quantified — it is what `Unification.generalise` wrote and what
    /// `ResolvedTypes` (running immediately before the freeze) checks against. Read
    /// it rather than re-deriving the set from the binder's type: the two agree on a
    /// generalized binding, but a NON-generalized one (the value restriction —
    /// `Infer` *removes* the scheme, `Infer.fs:501`) still has residual roots in its
    /// type, and re-derivation would attribute those to a scheme that does not exist.
    /// Reading the table instead makes freeze's notion of "bound" identical to
    /// `ResolvedTypes`' by construction: a root absent here is exactly a root
    /// `ResolvedTypes` already raised an error-severity diagnostic for.
    ///
    /// One root belongs to at most one scheme (an inner binding cannot quantify a
    /// root that is free in its environment), so the map needs no precedence rule and
    /// does not depend on enumeration order.
    let private schemeBinders (ctx: PassContext) : Dictionary<TypeVar, struct (NodeKey * int)> =
        let map = Dictionary<TypeVar, struct (NodeKey * int)>(HashIdentity.Reference)

        for KeyValue(binder, scheme) in ctx.Bindings.Scheme.AsDictionary() do
            scheme.Quantified
            |> Seq.iteri (fun i tv -> map.[UnionFind.find tv] <- struct (binder, i))

        map

    /// `TastFileG<SemType> → TastFileG<FrozenType>`. The cut point where `SemType`
    /// stops being the currency and `FrozenType` takes over for codegen.
    ///
    /// Each `.ty` is deep-`zonk`ed before conversion (= the encoder's old per-slot
    /// `frozen = toFrozen ∘ zonk`, hoisted to one tree-wide pass): `elaborate` does
    /// not deep-zonk every embedded `.ty`, so a field can hold a `TyVar root` linked
    /// to a concrete type. `zonk` resolves the link; the ground shape is then frozen.
    ///
    /// A residual (unlinked) `TyVar` is **tolerated**, and maps to the
    /// identity-bearing `FTLocalTypar`. It is a typar bound by a *local* `let`'s own
    /// scheme: it is instantiated afresh at every use site, so it never occurs in the
    /// ENCLOSING decl's type — and `Elaborate.mkMethodQuantEnv`, which derives the
    /// `TyVar -> TyTypar(Method, i)` remap by walking exactly that type, therefore
    /// never maps it. The local binding's own nodes keep the unmapped root.
    ///
    ///     let f () = let g = fun x -> x in (g, g)
    ///     // F#:  val f: unit -> ('a -> 'a) * ('b -> 'b)
    ///
    /// There are THREE roots here, and the printed signature is the evidence. `'a` and
    /// `'b` are the two USE-SITE instantiations, one per occurrence of `g`: they ARE in
    /// `f`'s type, so they map to the method axis like any other typar, and they are
    /// the ones F# prints. `'x` — `g`'s OWN locally-quantified root, what its lambda
    /// node is typed at — is what every use instantiates AWAY from, so it occurs in
    /// neither `'a` nor `'b`, hence nowhere in `f`'s type, hence never in the remap.
    /// That root is the residue, and it is what reaches this policy. (Two distinct
    /// typars in the signature is precisely the fingerprint of a local scheme having
    /// been generalised: a monomorphic `g` gives `('a -> 'a) * ('a -> 'a)` and leaves
    /// no residue. Contrast `let mkConst x = fun () -> x`: `x`'s typar is free in the
    /// environment, so the local `let` cannot quantify it — it IS in the enclosing
    /// type, and it maps.)
    ///
    /// The residue is IDENTITY-PRESERVING, not a phantom collapsed to a name: each
    /// root is attributed to the local scheme that BINDS it (`localBinders`), so it
    /// freezes to `FTLocalTypar(binder, index)`. Structural equality is by that pair,
    /// so two body-local typars stay two typars across the round-trip — the
    /// predecessor `FTUnknown "?free-typar"` gave every root the SAME name and
    /// `FTUnknown` equality is by name, so they conflated into one leaf. That was
    /// harmless only for a decl headed straight to codegen (the typar is phantom there
    /// — no value of it is ever constructed, a closure over it is `Vesper.Fun`-boxed);
    /// it is NOT harmless for an inline TEMPLATE, which is re-substituted, SRTP-re-
    /// resolved and static-opt-evaluated at every splice site.
    ///
    /// The leaf carries no `UnionFind` cell either way, so freezing still closes the
    /// backward-flow hole. `toFrozen` itself stays strict (its `TyVar` hard-error is
    /// unchanged), and a genuine unresolved-metavar inference bug — a root NO local
    /// scheme binds — is caught upstream by `ResolvedTypes` (a graceful per-decl
    /// diagnostic) and degrades HERE to `FTUnknown` rather than fabricating a binder:
    /// `FTLocalTypar` is for a typar a local scheme legitimately quantified, never a
    /// catch-all for "a `TyVar` I couldn't explain".
    ///
    /// Emptying `FTLocalTypar` of population — so that it could become a hard error
    /// everywhere — is NOT a matter of projecting these roots onto the enclosing
    /// method's typar axis. Real F# pointedly does not do that: it would change `f`'s
    /// ABI (callers would have to pass a type argument for a typar `f`'s signature
    /// never mentions). F# instead gives the local scheme its OWN axis — it compiles
    /// the example above to `f<'a,'b>` plus a *generic closure class* `g@2T<'c>`
    /// carrying `g`'s own root. So the real (out-of-scope) fix is GENERIC CLOSURES:
    /// lift a locally-generalized binding to its own typar axis. Not needed for the
    /// identity fix above.
    ///
    /// The binder attribution is a property of the SCHEME TABLE, not of tree
    /// position, so it is built once per file and the freeze stays the pure per-type
    /// map it has always been.
    let private freezeTy (binders: Dictionary<TypeVar, struct (NodeKey * int)>) (t: SemType) : FrozenType =
        let onVar (v: SemType) : FrozenType =
            match v with
            | TyVar tv ->
                // Key on the union-find ROOT: two `TyVar` nodes in the same class are
                // the same typar and must land on the same leaf.
                match binders.TryGetValue(UnionFind.find tv) with
                | true, struct (binder, index) -> FTLocalTypar(binder, index)
                // No scheme quantified it ⇒ a genuine metavar leak, already an
                // error-severity `ResolvedTypes` diagnostic on this decl. Degrade
                // rather than crash (the decl is not going to be emitted) — and do
                // NOT invent a binder for it.
                | _ -> FTUnknown "?unresolved-typar"
            | _ -> failwithf "Freeze.freezeTy: `toFrozenWith` invoked the TyVar policy on a non-TyVar: %A" v

        // `toFrozenWith` is the one structural fold; only the `TyVar` POLICY differs
        // here. Each `.ty` is deep-`zonk`ed first, so only a genuinely UNLINKED root
        // reaches `onVar`.
        Unification.zonk t |> FrozenTypeBridge.toFrozenWith onVar

    /// Is this decl a splice TEMPLATE — a member of the unit's inline vocabulary?
    ///
    /// Two shapes, one meaning ("a use of this is spliced, never called"): an explicit
    /// `let inline`, and a `let` value whose body is a single zero-operand intrinsic
    /// (`let undefined = (# "undefined" #)`), which is a compile-time ALIAS for its
    /// intrinsic — it has no `inline` keyword but every reference splices the body
    /// (`Inline.nullaryIntrinsicValueBody`), so no `const undefined = undefined`
    /// definition or import is emitted.
    ///
    /// EMITTABILITY is a separate question and is NOT this predicate: only the
    /// `let inline` shape is un-emittable (no backend can lower a template). The
    /// nullary alias stays in `Decls`, so a vocabulary member is not automatically
    /// dropped from them — that is `emittable` below.
    let private isInlineVocabulary (d: TDecl) : bool =
        match d with
        | TDecl.Let(TPat.NamedSimple _, _, true, _) -> true
        | TDecl.Let(TPat.NamedSimple _, _, false, _) -> (Inline.nullaryIntrinsicValueBody d).IsSome
        | _ -> false

    /// An inline TEMPLATE is not emittable: no backend has a lowering for one, and both
    /// drop it independently (`TastLower.lower`, and `Passes.InlineExpansion` leaves it
    /// unwalked). Dropping it here is what makes that structural rather than repeated.
    let private emittable (d: TDecl) : bool =
        match d with
        | TDecl.Let(isInline = true) -> false
        | _ -> true

    /// Rewrite a template's references to its SIBLING templates from `Var` to
    /// `External`, carrying the sibling's published `SymbolKey`.
    ///
    /// A `Var` names a binder that exists only in THIS unit's tree; a consumer splicing
    /// the body has no such binder in scope. `External` + key is the cross-unit form,
    /// and it must be baked into the PUBLISHED body — the consumer resolves it through
    /// the by-key inline channel, hitting the same identity `Freeze` minted for the
    /// sibling. (The simple `name` it also carries does NOT resolve at the consumer: the
    /// provider index is qualified-name keyed and the holder is not auto-opened. That is
    /// exactly why the key channel exists.)
    let private rewriteSiblingRefs (siblings: Dictionary<NodeKey, ModuleMemberInfo>) (d: TDecl) : TDecl =
        let mapper: TastWalk.Mapper =
            { TastWalk.identityMapper with
                OverrideExpr =
                    fun _ e ->
                        match e with
                        | TExpr.Var(k, ty, tok) ->
                            match siblings.TryGetValue k with
                            | true, info -> ValueSome(TExpr.External(info.Name, ValueSome info.Key, ty, tok))
                            | _ -> ValueNone
                        | _ -> ValueNone
            }

        match d with
        | TDecl.Let(pat, value, isInline, ty) -> TDecl.Let(pat, TastWalk.mapExpr mapper value, isInline, ty)
        | other -> other

    let run (ctx: PassContext) (tast: TastFile) : Frozen.TastFile =
        // The vocabulary's members, by binder key — both the set the sibling rewrite
        // rewires against and the source of each published identity. A template with no
        // `ModuleMemberInfo` (a top-level `let inline` outside any module) has no home
        // module and so no exportable identity: it is spliced within its own unit and
        // published nowhere.
        let vocabulary = Dictionary<NodeKey, ModuleMemberInfo>()

        for d in tast.Decls do
            match d with
            | TDecl.Let(TPat.NamedSimple(k, _, _), _, _, _) when isInlineVocabulary d ->
                match Map.tryFind k tast.ModuleMembers with
                | Some info -> vocabulary.[k] <- info
                | None -> ()
            | _ -> ()

        let inlineBodies =
            [
                for d in tast.Decls do
                    match d with
                    | TDecl.Let(TPat.NamedSimple(k, _, _), _, _, _) ->
                        match vocabulary.TryGetValue k with
                        | true, info ->
                            yield
                                {
                                    // Minted, not recovered. Every OTHER symbol's identity is
                                    // a side effect of emitting it; an inline value is never
                                    // emitted, so its identity must be minted deliberately —
                                    // here, from the holder chain its declaration already knows.
                                    TInlineValue.Key = info.Key
                                    Body =
                                        {
                                            Decl = rewriteSiblingRefs vocabulary d
                                            ParamAttrs =
                                                match ctx.InlineParamAttrs.TryGetValue k with
                                                | true, a -> a
                                                | _ -> [||]
                                        }
                                }
                        | _ -> ()
                    | _ -> ()
            ]

        let frozen =
            { tast with
                Decls = tast.Decls |> EqArray.toList |> List.filter emittable |> EqArray.ofList
                InlineBodies = EqArray.ofList inlineBodies
            }

        TastConvert.file (freezeTy (schemeBinders ctx)) frozen
