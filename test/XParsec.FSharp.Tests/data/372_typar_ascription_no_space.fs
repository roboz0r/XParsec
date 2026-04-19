// Type-ascribed statically-resolved typar pattern without a space after the colon,
// as used throughout FSharp.Core/prim-types.fs (e.g. NonStructuralComparison.(<)):
//   `let inline (+) (x:^T) (y:^U) = ...`
//
// Previously the lexer greedy-fused `:^` into a single `KWReservedOperator` token,
// breaking type ascription. `:` is only valid as the first char of the predefined
// colon-starting operators (`:`, `::`, `:?`, `:?>`, `:>`, `:=`).

let inline (+) (x:^T) (y:^U) =
    AdditionDynamic<(^T),(^U),(^T)> x y
    when ^T : int32 = (# "add" x y : int32 #)
