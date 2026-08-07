type IStructEnumerator<'T> =
    abstract member MoveNext : unit -> bool
    abstract member Current : 'T
type IStructSeq<'T, 'E when 'E :> IStructEnumerator<'T>> =
    abstract member GetEnumerator : unit -> 'E
[<Struct>]
type ArrayEnumerator<'T> =
    val Arr : 'T[]
    val mutable Idx : int
    new(arr: 'T[]) = { Arr = arr; Idx = -1 }
    interface IStructEnumerator<'T> with
        member this.MoveNext() : bool =
            this.Idx <- this.Idx + 1
            this.Idx < this.Arr.Length
        member this.Current : 'T = this.Arr.[this.Idx]
[<Struct>]
type ArraySeq<'T> =
    val Arr : 'T[]
    new(arr: 'T[]) = { Arr = arr }
    interface IStructSeq<'T, ArrayEnumerator<'T>> with
        member this.GetEnumerator() : ArrayEnumerator<'T> = ArrayEnumerator<'T>(this.Arr)
let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)
let fold (f: 'TFunc when 'TFunc :> Fun<'State, 'T, 'State>) (seed: 'State) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : 'State =
    let mutable state = seed
    for y in source do
        state <- f.Invoke(state, y)
    state
let total = fold (fun acc x -> acc + x) 0 (ofArray [| 1; 2; 3; 4 |])
printfn "%d" total
