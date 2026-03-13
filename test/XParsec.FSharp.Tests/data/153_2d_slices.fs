module Slices2D

let A = array2D [[1;2;3];[4;5;6];[7;8;9]]
let row0 = A[0,*]
let col0 = A[*,0]
let subA = A[*,0..1]
let subA' = A[0..1,*]
let twoByTwo = A[0..1,0..1]
