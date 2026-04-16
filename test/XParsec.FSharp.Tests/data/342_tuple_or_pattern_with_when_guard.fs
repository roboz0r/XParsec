// 5-tuple pattern containing a parenthesized OR-pattern of string constants
// and a list pattern, with a multi-line `when` guard that starts with an
// end-of-line comment then a parenthesized `&&`/`||` expression broken
// across several lines with visual alignment spaces.
// Affects: ConstraintSolver.fs
let isInt x = x > 0
let isBin x y = x + y > 0
let permitWeakResolution = true

let test nm minfos a b =
    match "_", "_", false, nm, [a; b] with
    | _, _, false, ("op_Addition" | "op_Subtraction" | "op_Modulus"), [argTy1; argTy2]
        when // Ignore any explicit +/- overloads from any basic integral types
            (minfos |> List.forall (fun (_, minfo) -> isInt minfo) &&
                (   isBin argTy1 argTy2 && isBin argTy2 argTy1
                ||  isBin argTy1 argTy1 && isBin argTy2 argTy2)) ->
        1
    | _, _, false, ("op_LessThan" | "op_Equality" | "op_Inequality"), [argTy1; argTy2]
        when (minfos |> List.forall (fun (_, minfo) -> isInt minfo) &&
                (   isBin argTy1 argTy2 && isBin argTy2 argTy1
                ||  isBin argTy1 argTy1 && isBin argTy2 argTy2)) ->
        2
    | _ -> 0
