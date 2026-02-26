module MyModule

type MyClass =
    class
        interface System.IDisposable with
            member this.Dispose() = ()
    end
