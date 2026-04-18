module Repro

let ParseUInt32 (s: string) =
    let l = s.Length
    let mutable p = 0
    if p >= l then failwith "fmt" else
    match s with
    | "x" -> 1u
    | "b" -> 2u
    | _ -> 3u in

let inline next (x: uint32) = x + 1u
