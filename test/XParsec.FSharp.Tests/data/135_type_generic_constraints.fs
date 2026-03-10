type Foo<'Input, 'InputSlice
    when 'Input :> IReadable<int, 'InputSlice>
    and 'InputSlice :> IReadable<int, 'InputSlice>>() =
    static let x = 1
