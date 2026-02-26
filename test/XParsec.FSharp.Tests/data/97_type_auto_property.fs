module MyModule

type MyClass =
    class
        member val Name = "default"
        member val Count = 0 with get, set
        member val ReadOnly = 42 with get
    end
