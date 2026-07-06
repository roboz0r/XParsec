[<Struct>]
type Add1 =
    val N : int
    new(n: int) = { N = n }
    interface Fun<int, int> with
        member this.Invoke(x: int) : int = x + this.N
[<Struct>]
type Applier<'TFunc, 'T, 'U when 'TFunc :> Fun<'T, 'U>> =
    val F : 'TFunc
    new(f: 'TFunc) = { F = f }
    member this.Apply(x: 'T) : 'U = this.F.Invoke(x)
let a = Applier<Add1, int, int>(Add1 1)
printfn "%d" (a.Apply 41)
