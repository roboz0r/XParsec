type IStructEnumerator =
    abstract member MoveNext : unit -> bool
    abstract member Current : int
type IStructSeq<'E when 'E :> IStructEnumerator> =
    abstract member GetEnumerator : unit -> 'E
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
[<Struct>]
type ArraySeq =
    val Arr : int[]
    new(arr: int[]) = { Arr = arr }
    interface IStructSeq<ArrayEnumerator> with
        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)
let fold (f: 'State -> int -> 'State) (seed: 'State) (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : 'State =
    let mutable state = seed
    for y in s do
        state <- f state y
    state
let xs = [| 1; 2; 3; 4 |]
printfn "%d" (fold (fun acc x -> acc + x) 0 (ArraySeq(xs)))
