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
[<Struct>]
type MapEnumerator<'E when 'E :> IStructEnumerator> =
    val mutable Source : 'E
    val F : int -> int
    new(source: 'E, f: int -> int) = { Source = source; F = f }
    interface IStructEnumerator with
        member this.MoveNext() : bool = this.Source.MoveNext()
        member this.Current : int = this.F (this.Source.Current)
[<Struct>]
type MapSeq<'S, 'E when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator> =
    val Source : 'S
    val F : int -> int
    new(source: 'S, f: int -> int) = { Source = source; F = f }
    interface IStructSeq<MapEnumerator<'E>> with
        member this.GetEnumerator() : MapEnumerator<'E> = MapEnumerator<'E>(this.Source.GetEnumerator(), this.F)
let sumSeq (s: 'S when 'S :> IStructSeq<'E> and 'E :> IStructEnumerator) : int =
    let mutable total = 0
    for y in s do
        total <- total + y
    total
let xs = [| 1; 2; 3 |]
let s = MapSeq<ArraySeq, ArrayEnumerator>(ArraySeq(xs), fun x -> x * 2)
printfn "%d" (sumSeq s)
