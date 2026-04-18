// Parenthesized tuple where one element is a multi-line `match` expression
// whose `|` bars sit at a column less than the tuple's other elements.
//
// From ilwrite.fs:1820-1825 — exception-clause adjustment where the last
// tuple field is a `match kind with | ... | ... -> ...`. The match bars
// undent from the inner tuple offside line but the whole thing is a single
// tuple inside a paren expression.

let adjustExnClauses origExnClauses adjuster =
    origExnClauses |> List.map (fun (st1, sz1, st2, sz2, kind) ->
        (adjuster st1, (adjuster (st1 + sz1) - adjuster st1),
         adjuster st2, (adjuster (st2 + sz2) - adjuster st2),
         (match kind with
         | FinallyClause | FaultClause | TypeFilterClause _ -> kind
         | FilterClause n -> FilterClause (adjuster n))))
