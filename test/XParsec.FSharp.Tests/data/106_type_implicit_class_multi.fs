module MyModule

type Counter(initialCount: int) =
    let mutable count = initialCount
    member this.Increment() = count <- count + 1
    member this.Value = count

type Greeter(name: string) =
    member this.Greet() = name
