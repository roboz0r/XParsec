module XParsec.FSharp.Codegen.Js.Tests.NominalEmitSiteTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// A record/union construction emits the type ITSELF, so no source program hands one an
// intrinsic; these retype the node in the pooled file to reach it. An intrinsic key is as
// nominal as a record's, so the failure comes from the table that missed and names the key.

/// The first intrinsic (`FTConst`) row of the file's own type table, and the key it names.
let private intrinsicRow (pools: FrozenPools) : TypeId * TypeKey =
    pools.Types.Rows.Types
    |> Seq.indexed
    |> Seq.pick (fun (i, r) ->
        match r with
        | TypeRow.Const(keyId, _) -> Some(TypeId i, pools.Types.[keyId])
        | _ -> None
    )

/// Point every node `isTarget` picks at that row, leaving every other node's type alone: the
/// binding around the retyped node keeps its own nominal type.
let private emitRetypedToIntrinsic (name: string) (src: string) (isTarget: ExprPayload -> bool) (table: string) =
    let pools = frozenOfJs src
    let row, key = intrinsicRow pools
    let tys = Array.copy pools.ExprTys
    let mutable hits = 0

    for i in 0 .. pools.ExprPayloads.Length - 1 do
        if isTarget pools.ExprPayloads.[i] then
            tys.[i] <- row
            hits <- hits + 1

    if hits = 0 then
        failwith "the program pools no node of the retyped shape"

    Expect.throwsC
        (fun () -> emitFrozenJs name src { pools with ExprTys = tys } |> ignore)
        (fun ex ->
            Expect.stringContains
                ex.Message
                (sprintf "on %s with no emitted type (key %A)" table key)
                "the failure names the table that missed and the key it looked up"
        )

[<Tests>]
let tests =
    testList
        "Codegen.Js nominal emit sites"
        [
            test "a RecordCons on an intrinsic type fails naming the key it looked up" {
                emitRetypedToIntrinsic
                    "RecordConsIntrinsic"
                    "type R = { X: int }\nlet r = { X = 1 }"
                    (fun p ->
                        match p with
                        | ExprPayload.RecordCons _ -> true
                        | _ -> false
                    )
                    "record"
            }

            test "a UnionCons on an intrinsic type fails naming the key it looked up" {
                emitRetypedToIntrinsic
                    "UnionConsIntrinsic"
                    "type U = | A of int | B\nlet u = A 1"
                    (fun p ->
                        match p with
                        | ExprPayload.UnionCons _ -> true
                        | _ -> false
                    )
                    "union"
            }
        ]
