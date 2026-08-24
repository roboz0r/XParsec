module XParsec.FSharp.SemanticAnalysis.Tests.FrozenCodecColumnTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `thaw` gates every column against the pool it is indexed by, so a blob whose parallel column
// is one slot short is refused at decode rather than at whichever node first indexes past the
// end.

let private source =
    "let add x y = x + y\nlet twice f x = f (f x)\nlet answer = twice (add 1) 40\n"

/// The named column of `source`'s frozen pools, dropped by its last slot and re-flattened.
type private TruncatedColumn =
    {
        Name: string
        Truncate: FrozenPools -> FrozenPools
    }

let private columns =
    [
        {
            Name = "ExprTys"
            Truncate =
                fun p ->
                    { p with
                        ExprTys = p.ExprTys.[.. p.ExprTys.Length - 2]
                    }
        }
        {
            Name = "ExprToks"
            Truncate =
                fun p ->
                    { p with
                        ExprToks = p.ExprToks.[.. p.ExprToks.Length - 2]
                    }
        }
        {
            Name = "ExprVarBoundVar"
            Truncate =
                fun p ->
                    { p with
                        ExprVarBoundVar = p.ExprVarBoundVar.[.. p.ExprVarBoundVar.Length - 2]
                    }
        }
        {
            Name = "PatTys"
            Truncate =
                fun p ->
                    { p with
                        PatTys = p.PatTys.[.. p.PatTys.Length - 2]
                    }
        }
        {
            Name = "PatToks"
            Truncate =
                fun p ->
                    { p with
                        PatToks = p.PatToks.[.. p.PatToks.Length - 2]
                    }
        }
        {
            Name = "BoundVarToks"
            Truncate =
                fun p ->
                    { p with
                        BoundVarToks = p.BoundVarToks.[.. p.BoundVarToks.Length - 2]
                    }
        }
    ]

[<Tests>]
let tests =
    testList
        "Frozen codec parallel columns"
        [
            test "an intact blob round-trips" {
                let pools = freezeFor source
                let thawed = FrozenCodec.thaw (FrozenCodec.flatten pools)

                Expect.equal
                    thawed.ExprTys.Length
                    pools.ExprTys.Length
                    "the expression type column survives the round trip"
            }

            for c in columns do
                test (sprintf "a truncated %s is refused" c.Name) {
                    let corrupt = c.Truncate(freezeFor source)
                    let blob = FrozenCodec.flatten corrupt

                    Expect.throwsC
                        (fun () -> FrozenCodec.thaw blob |> ignore)
                        (fun ex -> Expect.stringContains ex.Message c.Name "the fault names the column that is short")
                }
        ]
