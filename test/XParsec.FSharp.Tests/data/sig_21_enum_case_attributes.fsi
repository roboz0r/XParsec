module MyModule

open System

type Color =
    | [<Obsolete>] Red = 0
    | [<Obsolete>] [<CLSCompliant(true)>] Green = 1
    | [<Obsolete; CLSCompliant(true)>] Blue = 2
    | Plain = 3
