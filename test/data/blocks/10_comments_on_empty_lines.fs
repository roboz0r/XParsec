// Test 10: Comments on otherwise empty lines
let start =
    let intermediate = 1
    // This is a comment on its own line
        // This is an indented comment
    intermediate + 1
(*
   This is a block comment
   on multiple lines
*)
let finish = 2