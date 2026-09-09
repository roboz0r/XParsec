namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Vesper
open XParsec.FSharp.SemanticAnalysis
open EmitTypes
open EmitLower
open EmitResolve
open EmitPattern
open EmitDispatch

/// The control-flow joins: `match`, `if/then/else`, statement sequencing. The builder's
/// linear depth tracker follows one arm only, so both branching forms `SetDepth` back to
/// the pre-branch base before the join.
module EmitMatch =

    let buildMatch (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprMatch e
        let scrutinee = view.Scrutinee
        let arms = view.Arms
        // The scrutinee is evaluated once into a local; a mismatching arm or a failing
        // guard branches to the next arm's test.
        let scrutSlot = b.Local(typeOfExpr scrutinee)
        recur ExprPos.Value env b scrutinee
        b.Add(ILInstr.Stloc scrutSlot)
        let baseDepth = b.Depth
        let endLabel = b.Label()

        for arm in arms do
            let nextLabel = b.Label()
            buildMatchTest env b scrutSlot nextLabel arm.Pat

            match arm.Guard with
            | ValueSome g ->
                recur ExprPos.Value env b g
                b.Add(ILInstr.Brfalse nextLabel)
            | ValueNone -> ()

            recur pos env b arm.Body
            b.Add(ILInstr.Br endLabel)
            b.SetDepth baseDepth
            b.Add(ILInstr.Mark nextLabel)

        buildMatchFailure env b
        // Every arm carries the match's own position, so all reach `endLabel` at this depth.
        b.SetDepth(baseDepth + pos.Pushes)
        b.Add(ILInstr.Mark endLabel)

    let buildIfThenElse (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprIfThenElse e
        let cond = view.Cond
        let thenExpr = view.ThenExpr
        let elseExpr = view.ElseExpr
        // `<cond>; brfalse else; <then>; br end; else: <else>; end:`.
        let elseLabel = b.Label()
        let endLabel = b.Label()
        recur ExprPos.Value env b cond
        b.Add(ILInstr.Brfalse elseLabel)
        let baseDepth = b.Depth
        recur pos env b thenExpr
        b.Add(ILInstr.Br endLabel)
        b.SetDepth baseDepth
        b.Add(ILInstr.Mark elseLabel)
        recur pos env b elseExpr
        b.Add(ILInstr.Mark endLabel)

    let buildSequential (recur: RecurAt) (pos: ExprPos) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let items = TastAccessor.exprChildren e
        let n = items.Length

        items
        |> Block.iteri (fun i it ->
            if i = n - 1 then
                recur pos env b it
            else
                recur ExprPos.Statement env b it
        )
