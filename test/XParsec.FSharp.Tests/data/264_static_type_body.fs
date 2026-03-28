module TestStaticTypeBody

[<Sealed>]
type Cancellable =
    static let tokenHolder = AsyncLocal<CancellationToken voption>()
    static let guard = true
    static member Token = tokenHolder.Value
    static member Set v = tokenHolder.Value <- ValueSome v

type StaticOnly =
    static member X = 1
    static member Y = 2
