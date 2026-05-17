module Shapes

type Point = { X: float; Y: float }

type Shape =
    | Circle of radius: float
    | Square of float
    | Rectangle of width: float * height: float
