namespace XParsec.FSharp.Lexer

open System.Collections.Immutable

[<Struct>]
type ImmutableArrayM<'T, [<Measure>] 'M>(arr: ImmutableArray<'T>) =
    member this.Length = arr.Length
    member this.LengthM = LanguagePrimitives.Int32WithMeasure<'M> arr.Length

    member this.Item
        with get (i: int<'M>) = arr[int i]

    member this.ToImmutableArray() = arr

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
