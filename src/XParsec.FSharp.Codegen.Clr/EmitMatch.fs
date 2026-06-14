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

/// The control-flow joins — `match`, `if/then/else`, and statement sequencing.
/// Each leaves exactly one value; the builder's linear depth tracker is reset to
/// the post-branch base before every join so later statement-discards stay
/// correct (`IlIr.analyze` re-derives the buffer's true merge depths).
module EmitMatch =

    let buildMatch (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Match(scrutinee, arms, _, _) ->
            // Evaluate the scrutinee once into a local, then test each arm in
            // order: on a mismatch branch to the next arm; on a match (and a
            // passing guard) emit the body and branch to the shared end. The
            // builder's depth tracker is reset to the post-scrutinee base before
            // each arm and before the end label (every body leaves one result);
            // `IlIr.analyze` re-derives the buffer's merge depths.
            let scrutSlot = b.Local(typeOfExpr scrutinee)
            recur env b scrutinee
            b.Add(ILInstr.Stloc scrutSlot)
            let baseDepth = b.Depth
            let endLabel = b.Label()

            for arm in arms do
                let nextLabel = b.Label()
                buildMatchTest env b scrutSlot nextLabel arm.Pat

                match arm.Guard with
                | Some g ->
                    recur env b g
                    b.Add(ILInstr.Brfalse nextLabel)
                | None -> ()

                recur env b arm.Body
                b.Add(ILInstr.Br endLabel)
                b.SetDepth baseDepth
                b.Add(ILInstr.Mark nextLabel)

            buildMatchFailure env b
            b.SetDepth(baseDepth + 1)
            b.Add(ILInstr.Mark endLabel)
        | _ -> failwith "EmitMatch.buildMatch: unreachable"

    let buildIfThenElse (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.IfThenElse(cond, thenExpr, elseExpr, _, _) ->
            // `<cond>; brfalse else; <then>; br end; else: <else>; end:`. Both
            // arms leave one value; the builder's linear depth tracker (which
            // follows only the then-arm) is reset to the post-`brfalse` base
            // before the else-arm so subsequent statement-discards stay correct —
            // the *buffer's* merge depths are re-derived by `IlIr.analyze`.
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
        | _ -> failwith "EmitMatch.buildIfThenElse: unreachable"

    let buildSequential (recur: Recur) (env: EmitEnv) (b: IlBuilder) (e: Frozen.TExpr) : unit =
        match e with
        | TExprG.Sequential(items, _, _) ->
            // Every item but the last is a unit-typed statement: emit it and
            // discard whatever value it leaves (popping back to the pre-item
            // depth); the last item leaves the sequence's result.
            let n = items.Length

            items
            |> EqArray.iteri (fun i it ->
                if i = n - 1 then
                    recur env b it
                else
                    let baseDepth = b.Depth
                    recur env b it

                    while b.Depth > baseDepth do
                        b.Add ILInstr.Pop
            )
        | _ -> failwith "EmitMatch.buildSequential: unreachable"
