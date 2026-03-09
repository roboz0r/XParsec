module CE_Collection_Undentations

let a = 
    Class.Method(seq {
        yield 1
    })

let b = 
    Class.Method(Array.ofSeq (seq {
        yield 2
    }))

let c = 
    Class.Method(arg1=expr1, arg2=expr2, seq {
        yield 3
    })

let d = 
    Class.Method [
        4
    ]

let e = 
    Class.Method([|
        5
    |])

let f = 
    Class.Method(Array.ofList [
        6
    ])

let g = 
    Class.Method(arg1=expr1, arg2=expr2, [
        7
    ])

// Nested CEs
let h =
    seq {
        for x in outer do
            yield! async {
                let! y = transform x
                return y
            }
    }

// CE as argument to a function
let i =
    let run comp = Async.RunSynchronously comp
    run (async {
        let! x = getX ()
        return x + 1
    })
