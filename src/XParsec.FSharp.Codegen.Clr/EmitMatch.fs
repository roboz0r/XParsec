namespace XParsec.FSharp.Codegen.Clr

open System.Collections.Generic
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
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

    let buildMatch (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprMatch e
        let scrutinee = view.Scrutinee
        let arms = view.Arms
        // The scrutinee is evaluated once into a local; a mismatching arm or a failing
        // guard branches to the next arm's test.
        let scrutSlot = b.Local(typeOfExpr scrutinee)
        recur env b scrutinee
        b.Add(ILInstr.Stloc scrutSlot)
        let baseDepth = b.Depth
        let endLabel = b.Label()

        for arm in arms do
            let nextLabel = b.Label()
            buildMatchTest env b scrutSlot nextLabel arm.Pat

            match arm.Guard with
            | ValueSome g ->
                recur env b g
                b.Add(ILInstr.Brfalse nextLabel)
            | ValueNone -> ()

            recur env b arm.Body
            b.Add(ILInstr.Br endLabel)
            b.SetDepth baseDepth
            b.Add(ILInstr.Mark nextLabel)

        buildMatchFailure env b
        b.SetDepth(baseDepth + 1)
        b.Add(ILInstr.Mark endLabel)

    let buildIfThenElse (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        let view = TastAccessor.exprIfThenElse e
        let cond = view.Cond
        let thenExpr = view.ThenExpr
        let elseExpr = view.ElseExpr
        // `<cond>; brfalse else; <then>; br end; else: <else>; end:`.
        let elseLabel = b.Label()
        let endLabel = b.Label()
        recur env b cond
        b.Add(ILInstr.Brfalse elseLabel)
        let baseDepth = b.Depth
        recur env b thenExpr
        b.Add(ILInstr.Br endLabel)
        b.SetDepth baseDepth
        b.Add(ILInstr.Mark elseLabel)
        recur env b elseExpr
        b.Add(ILInstr.Mark endLabel)

    let buildSequential (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: TastAccessor.ExprId) : unit =
        // Every item but the last is a statement: pop whatever it leaves, back to the
        // pre-item depth. The last item leaves the sequence's result.
        let items = TastAccessor.exprChildren e
        let n = items.Length

        items
        |> Array.iteri (fun i it ->
            if i = n - 1 then
                recur env b it
            else
                let baseDepth = b.Depth
                recur env b it

                while b.Depth > baseDepth do
                    b.Add ILInstr.Pop
        )
