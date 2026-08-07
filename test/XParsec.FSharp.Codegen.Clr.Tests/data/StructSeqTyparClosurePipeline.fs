//#include _struct-seq-types.fs
// hand-written struct closures (each carries a `val`/`new`
// so the [<Struct>] parses — a fieldless impl-only struct
// trips parse recovery).
[<Struct>]
type AddN =
    val N : int
    new(n: int) = { N = n }
    interface Fun<int, int> with
        member this.Invoke(x: int) : int = x + this.N
[<Struct>]
type SumAcc =
    val Z : int
    new(z: int) = { Z = z }
    interface Fun<int, int, int> with
        member this.Invoke(state: int, y: int) : int = state + y + this.Z
//#include _struct-seq-combinators.fs
let xs = [| 1; 2; 3; 4 |]
let s0 = ofArray xs
let s1 = map (AddN 1) s0
let total = fold (SumAcc 0) 0 s1
printfn "%d" total
