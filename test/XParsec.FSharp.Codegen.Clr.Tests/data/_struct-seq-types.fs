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
[<Struct>]
type MapEnumerator<'E, 'TFunc, 'T, 'U when 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> =
    val mutable Source : 'E
    val F : 'TFunc
    new(source: 'E, f: 'TFunc) = { Source = source; F = f }
    interface IStructEnumerator<'U> with
        member this.MoveNext() : bool = this.Source.MoveNext()
        member this.Current : 'U = this.F.Invoke(this.Source.Current)
[<Struct>]
type MapSeq<'S, 'E, 'TFunc, 'T, 'U when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T> and 'TFunc :> Fun<'T, 'U>> =
    val Source : 'S
    val F : 'TFunc
    new(source: 'S, f: 'TFunc) = { Source = source; F = f }
    interface IStructSeq<'U, MapEnumerator<'E, 'TFunc, 'T, 'U>> with
        member this.GetEnumerator() : MapEnumerator<'E, 'TFunc, 'T, 'U> = MapEnumerator<'E, 'TFunc, 'T, 'U>(this.Source.GetEnumerator(), this.F)
