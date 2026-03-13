module Test157

type Shape =
    | [<System.Obsolete>] Circle of radius: float
    | Rectangle of width: float * height: float
    | [<System.Obsolete>] Point

type Config =
    {
        [<System.Obsolete>]
        Host: string
        [<System.Obsolete>]
        Port: int
        [<System.Obsolete>]
        mutable Delay: int
    }
