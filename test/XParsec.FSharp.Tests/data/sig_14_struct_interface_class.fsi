module ExplicitBodies

[<Struct>]
type Box =
    struct
        val mutable Value: int
        new: v: int -> Box
        member Doubled: int
    end

type IBag =
    interface
        abstract member Add: item: obj -> unit
        abstract member Count: int with get
    end

type Pool<'T> =
    class
        new: capacity: int -> Pool<'T>
        member Get: unit -> 'T
        member Put: item: 'T -> unit
        abstract member Capacity: int
    end
