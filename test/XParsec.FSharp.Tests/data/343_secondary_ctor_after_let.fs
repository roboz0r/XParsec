// Implicit class with a `let` field binding followed by a secondary
// constructor `new(...) =`. The first secondary constructor on line N
// fails with `MissingExpression` — the parser appears not to accept `new`
// as a valid class-body element after a preceding `let` binding.
// Affects: DependencyProvider.fs
type DependencyProvider
    internal (a: int, b: int, c: bool)
    =

    let cache = System.Collections.Generic.Dictionary<int, int>()

    new(a: int, b: int) = new DependencyProvider(a, b, true)

    new() = new DependencyProvider(0, 0, true)
