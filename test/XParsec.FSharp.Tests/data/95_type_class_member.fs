module MyModule

type Counter =
    class
        val mutable count: int
        member this.Increment() = this.count <- this.count + 1
        member this.Value = this.count
    end
