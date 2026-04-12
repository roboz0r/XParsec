module Test

// Type annotation suffix on begin...end block body
// F# grammar: BEGIN typedSeqExprBlock END

let a = begin 1 + 2 : int end

let b =
    begin
        1 + 2 : int
    end
