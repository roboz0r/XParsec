module Test

// Class with 'as this' self identifier
type MyClass(x: int) as this =
    let mutable value = x
    do this.Print()
    member _.Print() = printfn "%d" value
    member _.Value = value
