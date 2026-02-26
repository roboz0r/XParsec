module MyModule

type Shape =
    | Circle of radius: float
    | Rectangle of width: float * height: float
    | Point
