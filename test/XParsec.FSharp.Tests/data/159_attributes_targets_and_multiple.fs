module Test159

[<Struct; System.Serializable>]
type Pair = { First: int; Second: int }

[<RequireQualifiedAccess; System.Obsolete>]
type OldEnum =
    | A
    | B

[<AbstractClass>]
[<Sealed>]
type StaticHelper =
    static member DoWork() = ()
