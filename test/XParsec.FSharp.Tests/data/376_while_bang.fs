module TestWhileBang

// F# 7+ `while!` form inside computation expressions. Body and condition both
// produce a wrapped value (e.g. a Task<bool>). The parser shape is identical
// to `while ... do ... done`; the keyword token slot carries the distinction.

let getCond () = task { return true }

let runTask =
    task {
        while! getCond () do
            printfn "loop"
    }

let runAsync =
    async {
        while! async { return false } do
            return ()
    }

// while! with a single-line body
let oneLiner =
    task {
        while! getCond () do
            ()
    }

// Plain `while` still works (regression guard)
let plain =
    let mutable i = 0

    while i < 3 do
        i <- i + 1

    i
