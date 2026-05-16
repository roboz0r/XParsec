// Active pattern by-name invoked with a quotation argument (no parens around
// the active-pattern name). Combines named active-pattern dispatch with the
// quotation atomic-arg arm, plus a trailing identifier-bound result.
let test e =
    match e with
    | MyPattern <@ x + 1 @> bound -> Some bound
    | OtherPattern <@@ y * 2 @@> v -> Some v
    | _ -> None
