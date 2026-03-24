module Test

// Attributes between 'type' keyword and type name
type
    [<NoEquality; NoComparison>]
    MyRecord = { X: int; Y: string }

// Single attribute inline
type [<Struct>] Point = { X: float; Y: float }
