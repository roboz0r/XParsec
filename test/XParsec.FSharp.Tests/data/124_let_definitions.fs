module M

let f x = x (try 1 with _ -> 2)
let g x = x (if true then 1 else 2)
let h x = x (match 1 with 1 -> 1 | _ -> 2)
let y = ref 1
let i x = x !y
let j x = x begin () end
let k x = x struct (1, 2)
let l x = x (new System.Object())
let m x = x <@ 1 @>
let n x = x <@@ 1 @@>
let o x = x begin end
let p = begin end = ()
