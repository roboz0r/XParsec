module Test155

[<Struct>]
type Point = { X: float; Y: float }

[<RequireQualifiedAccess>]
type Color =
    | Red
    | Green
    | Blue

[<AbstractClass>]
type Shape() =
    abstract member Area : float

[<Sealed>]
type Circle(radius: float) =
    inherit Shape()
    override _.Area = System.Math.PI * radius * radius

[<Interface>]
type IDrawable =
    abstract member Draw : unit -> unit
