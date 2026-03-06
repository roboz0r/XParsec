module M

// return with simple value
let a =
    async {
        return 42
    }

// return with infix expression
let b =
    async {
        return x + 1
    }

// return! with simple application
let c =
    async {
        return! fetchData url
    }

// return! with fun argument (the pattern that exposed the pSepVirt bug)
let d =
    async {
        return! find (fun x -> x > 0) xs
    }

// return! with multi-arg application including fun
let e =
    async {
        return! nextFn (fun t -> t.Token = Token.EOF) "eof"
    }

// yield with simple value
let f =
    seq {
        yield 1
    }

// yield with expression
let g =
    seq {
        yield x * 2
    }

// yield! with application
let h =
    seq {
        yield! getItems ()
    }

// yield! with fun argument
let i =
    seq {
        yield! filter (fun x -> x > 0) xs
    }
