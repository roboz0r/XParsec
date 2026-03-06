module M

// try/with in CE - with arms use regular expressions
let a =
    async {
        try
            let! result = dangerousOp ()
            return result
        with
        | :? System.Exception as ex -> defaultValue
    }

// try/finally in CE
let b =
    async {
        try
            return! mainOp ()
        finally
            cleanup ()
    }

// try/with with fun in the body
let c =
    async {
        try
            let! result = run (fun () -> compute ())
            return result
        with
        | ex -> handleEx ex
    }

// match in CE (non-bang, over a plain expression) - arms use regular expressions
let d =
    async {
        match getValue () with
        | Some v -> process v
        | None -> defaultValue
    }

// match! (CE-level match) - arms use regular expressions
let e =
    async {
        match! fetchAsync () with
        | Ok v -> v
        | Error msg -> recover msg
    }

// Nested CEs
let f =
    seq {
        for x in outer do
            yield! async {
                let! y = transform x
                return y
            }
    }

// CE as argument to a function
let g =
    let run comp = Async.RunSynchronously comp
    run (async {
        let! x = getX ()
        return x + 1
    })
