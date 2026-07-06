// A value-type flat 2-arg closure. A fieldless `[<Struct>]`
// whose body is ONLY an interface impl trips parse recovery
// ("Skipped tokens at module level"); a `val`/`new` preamble
// (the `Add1` shape) parses, so carry a dummy field.
[<Struct>]
type Add2 =
    val Z : int
    new(z: int) = { Z = z }
    interface Fun<int, int, int> with
        member this.Invoke(a: int, b: int) : int = a + b + this.Z
// a genuinely curried value: AddB captures `a`, returns b -> a+b
type AddB(a: int) =
    interface Fun<int, int> with
        member this.Invoke(b: int) : int = a + b
type AddCurried() =
    interface Fun<int, Fun<int, int>> with
        member this.Invoke(a: int) : Fun<int, int> = AddB(a) :> Fun<int, int>
let flat = (Add2(0) :> Fun<int, int, int>).Invoke(20, 22)
let curried = ((curryFun (Add2(0) :> Fun<int, int, int>) 20) :> Fun<int, int>).Invoke(22)
let flattened = (flatten (AddCurried() :> Fun<int, Fun<int, int>>)).Invoke(20, 22)
printfn "%d %d %d" flat curried flattened
