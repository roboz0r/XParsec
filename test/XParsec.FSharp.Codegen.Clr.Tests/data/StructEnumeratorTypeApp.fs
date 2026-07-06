[<Struct>]
type OnceEnum<'T> =
    val mutable Item: 'T
    val mutable Started: bool
    new(x: 'T) = { Item = x; Started = false }

    interface System.Collections.Generic.IEnumerator<'T> with
        member this.Current = this.Item

    interface System.Collections.IEnumerator with
        member this.Current = box this.Item

        member this.MoveNext() =
            if this.Started then
                false
            else
                this.Started <- true
                true

        member this.Reset() = ()

    interface System.IDisposable with
        member this.Dispose() = ()

type OnceSeq<'T>(x: 'T) =
    interface System.Collections.Generic.IEnumerable<'T> with
        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> =
            OnceEnum<'T>(x) :> System.Collections.Generic.IEnumerator<'T>

    interface System.Collections.IEnumerable with
        member this.GetEnumerator() : System.Collections.IEnumerator =
            OnceEnum<'T>(x) :> System.Collections.IEnumerator
