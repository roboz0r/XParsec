module MyModule

type MyClass() =
    class
        member val Name = "default"
        static member val Count = 0 with get, set
        member val internal ReadOnly = 42 with get
    end
