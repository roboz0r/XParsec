module MyModule

type Counter =
    class
        val mutable count: int
        member this.Count
            with get() = this.count
            and set(v) = this.count <- v
    end
