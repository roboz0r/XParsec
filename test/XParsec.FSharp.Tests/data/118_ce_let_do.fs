module M

// let! followed by return!
let a =
    async {
        let! x = getX ()
        return! transform x
    }

// Multiple let! bindings then return
let b =
    async {
        let! x = getX ()
        let! y = getY x
        return x + y
    }

// let (non-bang) binding then return
let c =
    async {
        let x = compute ()
        return x * 2
    }

// Mixed let!/let then return!
let d =
    async {
        let! raw = fetchRaw ()
        let parsed = parse raw
        return! wrap parsed
    }

// do! then return!
let e =
    async {
        do! initialize ()
        return! getResult ()
    }

// do! then do! then return
let f =
    async {
        do! step1 ()
        do! step2 ()
        return ()
    }

// do (non-bang) then return
let g =
    async {
        do printfn "starting"
        return 0
    }

// use! then return!
let h =
    async {
        use! resource = acquire ()
        return! resource.DoWork ()
    }

// use (non-bang) then return
let i =
    async {
        use conn = openConnection ()
        return conn.Query ()
    }

// let! with fun-arg expression
let j =
    async {
        let! result = run (fun () -> computeExpensive ())
        return result
    }
