[<Struct>]
type ArrayEnumerator =
    val Arr : int[]
    val mutable Idx : int
    new(arr: int[]) = { Arr = arr; Idx = -1 }
    member this.MoveNext() : bool =
        this.Idx <- this.Idx + 1
        this.Idx < this.Arr.Length
    member this.Current : int = this.Arr.[this.Idx]
type IStructSeq<'E> =
    abstract member GetEnumerator : unit -> 'E
[<Struct>]
type ArraySeq =
    val Arr : int[]
    new(arr: int[]) = { Arr = arr }
    interface IStructSeq<ArrayEnumerator> with
        member this.GetEnumerator() : ArrayEnumerator = ArrayEnumerator(this.Arr)
[<Struct>]
type MapEnumerator =
    val mutable Source : ArrayEnumerator
    val F : int -> int
    new(source: ArrayEnumerator, f: int -> int) = { Source = source; F = f }
    member this.MoveNext() : bool = this.Source.MoveNext()
    member this.Current : int = this.F (this.Source.Current)
[<Struct>]
type MapSeq<'S when 'S :> IStructSeq<ArrayEnumerator>> =
    val Source : 'S
    val F : int -> int
    new(source: 'S, f: int -> int) = { Source = source; F = f }
    member this.GetEnumerator() : MapEnumerator = MapEnumerator(this.Source.GetEnumerator(), this.F)
let xs = [| 1; 2; 3 |]
let s = MapSeq<ArraySeq>(ArraySeq(xs), fun x -> x * 2)
for y in s do
    printfn "%d" y
printfn "done"
