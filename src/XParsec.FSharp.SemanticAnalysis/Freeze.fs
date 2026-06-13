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
    /// One residual free (unlinked) `TyVar` is **tolerated** and mapped to
    /// `FTUnknown`: the head type of an *un-ground built-in operator* (`13 &&& 11`)
    /// whose `External` head keeps a generic function type because the operator rides
    /// codegen's surviving `expandBuiltinOps` fallback (it was not inline-expanded).
    /// `expandBuiltinOps` runs POST-freeze and dispatches on the operator *name* +
    /// the (ground) operand/result types, discarding this head type entirely — so it
    /// is semantically irrelevant and `FTUnknown` round-trips harmlessly through
    /// `ofFrozen`. `toFrozen` itself stays strict (its `TyVar` hard-error + the 3B-1
    /// round-trip oracle are unchanged); the strict-everywhere invariant is restored
    /// in the codegen-flip follow-up, which moves `expandBuiltinOps` pre-freeze so no
    /// un-ground operator reaches `freezeTy` (then this lenient arm + the placeholder
    /// can be deleted). Genuine unresolved-`TyVar` inference bugs are still caught
    /// upstream by `ResolvedTypes` (a graceful per-decl diagnostic), which runs before
    /// the freeze.
    let private freezeTy (t: SemType) : FrozenType =
        let rec go (ty: SemType) : FrozenType =
            match ty with
            | TyConst(n, args) -> FTConst(n, EqArray.map go args)
            | TyFun(a, b) -> FTFun(go a, go b)
            | TyTuple xs -> FTTuple(EqArray.map go xs)
            | TyRecord(k, args) -> FTRecord(k, EqArray.map go args)
            | TyUnion(k, args) -> FTUnion(k, EqArray.map go args)
            | TyClass(k, args) -> FTClass(k, EqArray.map go args)
            | TyOr members -> FTOr(EqArray.map go members)
            | TyTypar(axis, i) -> FTTypar(axis, i)
            | TyUnknown n -> FTUnknown n
            // The un-ground-operator residue (see the doc comment); placeholder name
            // is fixed for determinism since the node is discarded post-freeze.
            | TyVar _ -> FTUnknown "?ungrounded-operator"

        go (Unification.zonk t)

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
