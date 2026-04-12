module Test

// Type annotation suffix on computation expression body elements
// Both `yield` and `return` bodies accept a trailing `: typ`

let xs = seq { yield 1 + 2 : int }

let a = async { return 1 + 2 : int }

let ys =
    seq {
        yield 1 + 2 : int
        yield 3 + 4 : int
    }
