module Test

type Base() =
    abstract member Greet: string -> string
    default _.Greet(name) = "Hello, " + name

type Derived() =
    inherit Base()

    override _.Greet(name) =
        let baseResult = base.Greet(name)
        baseResult + "!"
