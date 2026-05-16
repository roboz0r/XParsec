// Active pattern invoked with an untyped quotation `<@@ ... @@>` as a curried
// argument in match-arm pattern position. Same as 385 but exercising the
// `OpQuotationUntypedLeft` dispatch arm.
let classify e =
    match e with
    | (|Const|_|) <@@ 1 + 1 @@> x -> Some x
    | _ -> None
