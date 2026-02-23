try
    let x = riskyOp ()
    x + 1
with
| :? System.Exception as ex ->
    let msg = ex.Message
    -1
| :? System.InvalidOperationException ->
    -2
