module Test

type Rec = { X: int; Y: int }

let a = { X = 1; Y = 2 }

let b = { a with X = 3; Y = 4 }

let f r = { r with X = 10 }
