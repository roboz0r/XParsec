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
