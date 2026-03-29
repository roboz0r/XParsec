module Test

open System.Collections.Generic

type Foo() =
    interface IEnumerable<int> with
        member _.GetEnumerator() =
            let elems = List<int>()
            elems.GetEnumerator()

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() =
            (this :> IEnumerable<int>).GetEnumerator() :> System.Collections.IEnumerator
