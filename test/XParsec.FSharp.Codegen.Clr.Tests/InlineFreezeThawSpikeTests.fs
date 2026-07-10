module XParsec.FSharp.Codegen.Clr.Tests.InlineFreezeThawSpikeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// SPIKE for docs/inline-body-freeze-thaw-plan.md — the gate the plan stakes itself
// on: can an inline SRTP body (`when ^T : …`) be frozen to a `Frozen.TDecl`
// *losslessly*, i.e. with NO `SemType` (and no live inference cell) surviving into
// the frozen tree?
//
// Answer (this test): the `StaticOptimization` EXPRESSION half is representable
// frozen — clauses survive, bodies become `FrozenType`, open typars become
// `FTTypar` leaves — so "unrepresentable in FrozenType" is a POLICY (freeze drops
// inline decls), not a fundamental limit, exactly as the plan hoped.
//
// BUT the clause CONSTRAINT half is not: `TStaticOptClauseG.Constraints` is
// `EqArray<TStaticOptConstraint>`, deliberately NOT `'ty`-typed (Tast.fs:457,
// TastConvert.fs:19-21), and `TStaticOptConstraint` embeds raw `SemType`
// (SemanticInfo.fs:1496-1503) whose typar is a LIVE `TyVar` over the inline
// binding's quantified root (SemanticInfo.fs:1492). The functor copies it VERBATIM,
// so a naively-frozen inline decl still carries `SemType` — and the very
// `UnionFind` cell — the plan exists to keep off the provider boundary.
//
// ⇒ Not "fundamental unrepresentability" (the frozen form IS expressible:
//   `TyconEquals of FrozenType * FrozenType`), but a REQUIRED, currently-unplanned
//   production change: make `TStaticOptConstraint` `'ty`-generic and route it
//   through `TastConvert.clause`. The plan needs that addition before freeze-inline
//   is sound.

/// Every `TStaticOptConstraint` typar root reachable in a clause list, zonked —
/// works on both the SemType and the frozen clause list because `Constraints` is the
/// SAME monomorphic `TStaticOptConstraint` in both (that is the whole point).
let private constraintRoots (clauses: TStaticOptClauseG<'ty, _> list) : TypeVar list =
    [
        for c in clauses do
            for k in EqArray.toList c.Constraints do
                match k with
                | TStaticOptConstraint.TyconEquals(TyVar a, _) -> yield UnionFind.find a
                | TStaticOptConstraint.TyconEquals(_, TyVar b) -> yield UnionFind.find b
                | TStaticOptConstraint.IsStruct(TyVar a) -> yield UnionFind.find a
                | _ -> ()
    ]

[<Tests>]
let tests =
    testList
        "InlineFreezeThawSpike"
        [
            test "freeze-inline: StaticOptimization body freezes, but clause Constraints retain the live SemType cell (plan gap)" {
                // The known-good SRTP inline shape (identical to StaticOptimizationTests):
                // a `when ^T : …` cascade that elaborates to a `TExpr.StaticOptimization`.
                let src =
                    String.concat
                        "\n"
                        [
                            "let inline kindOf (x: ^T) : int ="
                            "    -1"
                            "    when ^T : int   = 1"
                            "    when ^T : float = 2"
                            "    when ^T : ^T    = 0"
                        ]

                let tast = analyse src
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                // Pre-freeze SemType inline decl (retained — freeze has not run).
                let inlineDecl =
                    tast.Decls
                    |> EqArray.toList
                    |> List.tryPick (fun d ->
                        match d with
                        | TDeclG.Let(_, _, true, _) -> Some d
                        | _ -> None)
                    |> Option.defaultWith (fun () -> failtest "no inline decl in tast.Decls")

                let semClauses =
                    match inlineDecl with
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.StaticOptimization(cls, _, _, _), _, _), _, _) ->
                        EqArray.toList cls
                    | _ -> failtestf "expected a static-opt inline binding, got %A" inlineDecl

                // (1) PREMISE (re-anchoring StaticOptimizationTests): pre-freeze, the clause
                //     constraints hold a LIVE TyVar root — the cell the plan must not leak.
                let semRoots = constraintRoots semClauses
                Expect.isNonEmpty semRoots "pre-freeze: a clause constraint carries a live TyVar root"

                // (2) Freeze the whole decl through the functor. `onVar` stands in for the
                //     real quantEnv (`Elaborate.mkMethodQuantEnv`): the spike tests
                //     REPRESENTABILITY, not the typar index order, so a single placeholder
                //     leaf for the lone `^T` is faithful enough. A strict `toFrozen` would
                //     throw here on the un-quantified `^T`; that it does not is the point —
                //     with quantification, the body is total.
                let onVar (_: SemType) : FrozenType = FTTypar(TyparAxis.Method, 0)

                let frozenDecl: Frozen.TDecl =
                    TastConvert.decl (FrozenTypeBridge.toFrozenWith onVar) inlineDecl

                // (2a) POSITIVE — the StaticOptimization EXPRESSION is representable frozen:
                //      the shape survives, the result type froze to a real FrozenType, and
                //      the open typar became an `FTTypar` leaf (policy, not fundamental).
                let frozenClauses, frozenResultTy =
                    match frozenDecl with
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.StaticOptimization(cls, _, resultTy, _), _, _), _, _) ->
                        EqArray.toList cls, resultTy
                    | _ -> failtestf "freeze lost the static-opt shape: %A" frozenDecl

                Expect.equal frozenClauses.Length semClauses.Length "all when-clauses survive freeze"

                match frozenResultTy with
                | FTConst _ -> () // `: int`
                | other -> failtestf "expected a frozen result type, got %A" other

                // (2b) GAP — each frozen clause's `Constraints` is STILL a
                //      `TStaticOptConstraint` carrying `SemType`, and it is the SAME
                //      `UnionFind` cell as the pre-freeze tree: the functor copied it
                //      verbatim. `SemType` (and a mutable inference cell) has silently
                //      survived into `Frozen.TDecl`.
                let frozenRoots = constraintRoots frozenClauses
                Expect.isNonEmpty frozenRoots "GAP: a frozen clause still exposes a SemType TyVar constraint"

                let shared =
                    frozenRoots
                    |> List.exists (fun fr -> semRoots |> List.exists (fun sr -> System.Object.ReferenceEquals(sr, fr)))

                Expect.isTrue
                    shared
                    "GAP: the frozen clause shares the pre-freeze SemType cell — the backward-flow hazard survives freeze, so `TStaticOptConstraint` must become `'ty`-generic"
            }
        ]
