namespace XParsec.FSharp.SemanticAnalysis

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
    /// `TastFileG<SemType> → TastFileG<FrozenType>`. The cut point where `SemType`
    /// stops being the currency and `FrozenType` takes over for codegen.
    ///
    /// Inline TEMPLATES (`TDecl.Let(isInline = true)`) are dropped first: they
    /// retain `TyVar` (the 2B exemption — a `let inline` survives elaboration
    /// unexpanded, its uses already spliced in) and are NOT emittable (codegen's
    /// `EmitLower.lower` drops them too). They are unrepresentable in `FrozenType`
    /// and reach the backend nowhere; the cross-package publish path reads them off
    /// the *pre-freeze* `SemType` tree (`Pipeline.analyseSem*` →
    /// `SymbolProviders.collectInlineBodies`), never `analyse`'s frozen output. So
    /// dropping them here is the freeze's first act, leaving `toFrozen` total over
    /// what remains (the residual cross-package `TyVar`
    /// source is 3B-5/PF6).
    ///
    /// Each `.ty` is deep-`zonk`ed before conversion (= the encoder's old per-slot
    /// `frozen = toFrozen ∘ zonk`, hoisted to one tree-wide pass): `elaborate` does
    /// not deep-zonk every embedded `.ty`, so a field can hold a `TyVar root` linked
    /// to a concrete type. `zonk` resolves the link; the ground shape is then frozen.
    ///
    /// A residual free (unlinked) `TyVar` is **tolerated** and mapped to `FTUnknown`.
    /// The source is a typar quantified by a *local* `let`'s own scheme: it is
    /// instantiated afresh at every use site, so it never occurs in the ENCLOSING
    /// decl's type — and `Elaborate.mkMethodQuantEnv`, which derives the
    /// `TyVar -> TyTypar(Method, i)` remap by walking exactly that type, therefore
    /// never maps it. The local binding's own nodes keep the unmapped root.
    ///
    ///     let f () = let g = fun x -> x in (g, g)
    ///
    /// `g` is generalised locally over `'x`; the two `(g, g)` occurrences instantiate
    /// it at fresh roots, so `f`'s type names those and not `'x`. `g`'s lambda node
    /// still carries `'x`, which reaches here free. (Contrast `let mkConst x = fun () -> x`:
    /// `x`'s typar is free in the environment, so the local `let` cannot quantify it —
    /// it IS in the enclosing type, and it maps.)
    ///
    /// The typar is phantom in the emitted code (no value of it is ever constructed —
    /// a closure over it is `Vesper.Fun`-boxed), so `FTUnknown` is sound here and
    /// round-trips harmlessly through `ofFrozen`. `toFrozen` itself stays strict (its
    /// `TyVar` hard-error is unchanged), and genuine unresolved-`TyVar` inference bugs
    /// are still caught upstream by `ResolvedTypes` (a graceful per-decl diagnostic),
    /// which runs before the freeze.
    ///
    /// Making this strict means teaching local generalisation to project its own
    /// quantified roots onto the enclosing method's typar axis — a real change to the
    /// typar ABI, not a cleanup.
    let private freezeTy (t: SemType) : FrozenType =
        // `toFrozenWith` is the one structural fold; only the `TyVar` POLICY differs
        // here: the residue (see the doc comment) maps to a fixed placeholder name for
        // determinism, since no emitted type ever depends on it.
        Unification.zonk t
        |> FrozenTypeBridge.toFrozenWith (fun _ -> FTUnknown "?free-typar")

    let run (tast: TastFile) : Frozen.TastFile =
        let emittable =
            tast.Decls
            |> EqArray.toList
            |> List.filter (fun d ->
                match d with
                | TDecl.Let(isInline = true) -> false
                | _ -> true
            )
            |> EqArray.ofList

        TastConvert.file freezeTy { tast with Decls = emittable }
