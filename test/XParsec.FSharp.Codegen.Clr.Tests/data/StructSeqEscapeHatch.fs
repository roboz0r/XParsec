open System.Collections.Generic
open System.Collections
[<Struct>]
type ArrayEnumerator<'T> =
    val Arr : 'T[]
    val mutable Idx : int
    new(arr: 'T[]) = { Arr = arr; Idx = -1 }
    interface IEnumerator<'T> with
        member this.Current : 'T = this.Arr.[this.Idx]
    interface IEnumerator with
        member this.MoveNext() : bool =
            this.Idx <- this.Idx + 1
            this.Idx < this.Arr.Length
        member this.Current : obj = box (this.Arr.[this.Idx])
        member this.Reset() : unit = ()
    interface System.IDisposable with
        member this.Dispose() : unit = ()
[<Struct>]
type ArraySeq<'T> =
    val Arr : 'T[]
    new(arr: 'T[]) = { Arr = arr }
    interface IEnumerable<'T> with
        member this.GetEnumerator() : IEnumerator<'T> = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator<'T>)
    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator = (ArrayEnumerator<'T>(this.Arr) :> IEnumerator)
let sum3 (xs: IEnumerable<int>) : int =
    let e = xs.GetEnumerator()
    let mutable total = 0
    while e.MoveNext() do
        total <- total + e.Current
    total
let s = ArraySeq<int>([| 1; 2; 3 |])
printfn "%d" (sum3 (s :> IEnumerable<int>))
