module M

type Foo() =
    interface System.IDisposable with
        member this.Dispose() = ()
    interface System.IEquatable<Foo> with
        member this.Equals(other: Foo) = false
