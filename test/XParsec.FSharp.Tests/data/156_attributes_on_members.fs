module Test156

type MyClass() =
    [<DefaultValue>]
    val mutable Value : int

    [<System.Obsolete("Use NewMethod instead")>]
    member _.OldMethod() = 42

    [<System.Obsolete>]
    member _.AnotherOld() = 0

    [<System.Obsolete>]
    override _.ToString() = "MyClass"
