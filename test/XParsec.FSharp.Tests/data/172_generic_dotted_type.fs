type Foo =
    {
        Items: System.Collections.Immutable.ImmutableArray<int>.Builder
        More: Result<int, string>.Ok
    }

let x: ImmutableArray<int>.Builder = failwith ""
