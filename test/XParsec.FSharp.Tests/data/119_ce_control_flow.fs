module M

// for-in with yield
let a =
    seq {
        for x in xs do
            yield x * x
    }

// for-in with yield!
let b =
    seq {
        for x in xs do
            yield! expand x
    }

// for-to with yield
let c =
    seq {
        for i = 0 to 9 do
            yield i
    }

// for-in with do! then return
let d =
    async {
        for item in items do
            do! processItem item
        return ()
    }

// if/then/else in CE
let e =
    async {
        if condition then
            return! trueCase ()
        else
            return! falseCase ()
    }

// if/then without else (void branch)
let f =
    async {
        if shouldLog then
            do! logAction ()
        return ()
    }

// if/then with complex condition including fun
let g =
    async {
        if check (fun t -> t.IsValid) token then
            return! processToken token
        else
            return defaultValue
    }

// if/then with let! binding in both branches
let h =
    async {
        if flag then
            let! x = computeA ()
            return x
        else
            let! y = computeB ()
            return y
    }

// Nested: for-in containing if/then
let i =
    seq {
        for x in xs do
            if x > 0 then
                yield x
    }

// for-in with complex body
let j =
    async {
        for item in items do
            let result = transform item
            do! save result
        return ()
    }
