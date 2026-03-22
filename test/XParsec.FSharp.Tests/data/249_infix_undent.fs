module InfixUndentation

// ============================================================
// INFIX OPERATOR UNDENTATION (F# spec 15.1.9)
// An infix operator may undent from the SeqBlock context by
// (operator_length + 1) columns. This allows vertical alignment
// of operands when the operator is placed at the start of the
// continuation line.
// ============================================================

module PipeOps =

    let pipeForward =
        1
     |> string           // col 5: `1` at col 8, |> len 2, 8-(2+1)=5

    let pipeBackward =
        string
     <| 1                // col 5: `string` at col 8, <| len 2, 8-(2+1)=5

    let boolOr =
        true
     || false            // col 5: `true` at col 8, || len 2, 8-(2+1)=5

    let boolAnd =
        true
     && false            // col 5: `true` at col 8, && len 2, 8-(2+1)=5

    let triplePipe f x =
     x
|||> f                   // col 0: `x` at col 5, |||> len 4, 5-(4+1)=0

// ============================================================
// NON-PIPE INFIX OPERATORS
// Same rule applies: undent by (operator_length + 1).
// For single-char ops like `+`, that means undent by 2.
// ============================================================

module ArithOps =

    let add =
     1
   + 2                   // col 3: `1` at col 5, + len 1, 5-(1+1)=3

    let subtract =
        3
      - 1                // col 6: `3` at col 8, - len 1, 8-(1+1)=6

    let multiply =
        2
      * 3                // col 6: `2` at col 8, * len 1, 8-(1+1)=6


module Nonsense =

     let nonsense a b c =
       a
     + b
    |> c 
