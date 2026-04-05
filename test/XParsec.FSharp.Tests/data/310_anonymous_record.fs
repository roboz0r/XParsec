module Test

// Anonymous record expression and type
// From FSharpRequestContext.fs line 117

let makeToken (line: int) (col: int) (len: int) =
    {| startLine = line - 1
       startCol = col
       length = len
       tokType = 0
       tokMods = 0 |}

let readToken (tok: {| startLine: int; startCol: int; length: int |}) =
    tok.startLine + tok.startCol + tok.length
