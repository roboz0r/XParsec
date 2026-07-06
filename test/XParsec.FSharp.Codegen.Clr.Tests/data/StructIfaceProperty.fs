type IStructEnumerator =
    abstract member MoveNext : unit -> bool
    abstract member Current : int
[<Struct>]
type ArrayEnumerator =
    val Arr : int[]
    val mutable Idx : int
    new(arr: int[]) = { Arr = arr; Idx = -1 }
    interface IStructEnumerator with
        member this.MoveNext() : bool =
            this.Idx <- this.Idx + 1
            this.Idx < this.Arr.Length
        member this.Current : int = this.Arr.[this.Idx]
let e = ArrayEnumerator([| 10; 20 |])
let i = (e :> IStructEnumerator)
i.MoveNext() |> ignore
printfn "%d" i.Current
