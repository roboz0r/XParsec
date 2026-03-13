module Slices

let fullList = [ 1 .. 100 ]
let smallSlice = fullList.[1..2]
let unboundedBeginning = fullList.[..3]
let unboundedEnd = fullList.[4..]
let smallSlice2 = fullList[5..6]
let unboundedBeginning2 = fullList[..7]
let unboundedEnd2 = fullList[8..]
