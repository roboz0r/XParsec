namespace XParsec.FSharp

open System.Collections.Immutable
open XParsec

type ImArr<'T> = ImmutableArray<'T>

[<Struct>]
type ImmutableArrayM<'T, [<Measure>] 'M>(arr: ImmutableArray<'T>) =
    member this.Length = arr.Length
    member this.LengthM = LanguagePrimitives.Int32WithMeasure<'M> arr.Length

    member this.Item
        with get (i: int<'M>) = arr[int i]

    member this.AsImmutableArray() = arr

    interface System.Collections.Generic.IReadOnlyList<'T> with
        member this.Count = arr.Length

        member this.Item
            with get (i: int) =
                if i < 0 || i >= arr.Length then
                    invalidArg (nameof i) "Index out of range"

                arr[i]

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
            (arr :> System.Collections.IEnumerable).GetEnumerator()

    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator() =
            (arr :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator()


/// Measure-typed wrapper over a ReadableArray slice. Mirrors ImmutableArrayM
/// but uses a partial-view backing so Lexed output can avoid the copy the
/// ImmutableArray.Builder.ToImmutable finaliser would otherwise perform.
[<Struct>]
type ReadableArrayM<'T, [<Measure>] 'M>(arr: ReadableArray<'T>) =
    member _.Length = arr.Length
    member _.LengthM = LanguagePrimitives.Int32WithMeasure<'M> arr.Length

    member _.Item
        with get (i: int<'M>) = arr.[int i]

    member _.AsReadableArray() = arr

    interface System.Collections.Generic.IReadOnlyList<'T> with
        member _.Count = arr.Length

        member _.Item
            with get (i: int) =
                if i < 0 || i >= arr.Length then
                    invalidArg (nameof i) "Index out of range"

                arr.[i]

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
            (this :> System.Collections.Generic.IEnumerable<'T>).GetEnumerator() :> System.Collections.IEnumerator

    interface System.Collections.Generic.IEnumerable<'T> with
        member _.GetEnumerator() =
            let mutable i = 0
            let length = arr.Length

            { new System.Collections.Generic.IEnumerator<'T> with
                member _.Current = arr.[i - 1]
              interface System.Collections.IEnumerator with
                  member _.Current = box arr.[i - 1]

                  member _.MoveNext() =
                      if i < length then
                          i <- i + 1
                          true
                      else
                          false

                  member _.Reset() = i <- 0
              interface System.IDisposable with
                  member _.Dispose() = ()
            }
