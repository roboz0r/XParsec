// Last argument of a function application is a tuple whose elements are on
// separate lines at an indent deeper than the function's own column. The
// tuple elements mix a simple function call and a multi-line lambda.
//
// Pattern used throughout TypedTreeOps.fs (lines ~7974, 7989, 8000) inside
// a `match`/`when`-guarded arm, which is part of why the offside bug bites.

let rewrite expr g access rvs =
    match expr with
    | Expr.Op (TOp.Tuple tupInfo, argTys, args, m) when not (evalTupInfoIsStruct tupInfo) ->
      args |> List.iteri (fun n ->
          IterateRecursiveFixups g None rvs
            (mkTupleFieldGet g (tupInfo, access, argTys, n, m),
            (fun e ->
              errorR (Error (FSComp.SR.tastRecursiveValuesMayNotBeInConstructionOfTuple (), m))
              e)))
    | _ -> ()
