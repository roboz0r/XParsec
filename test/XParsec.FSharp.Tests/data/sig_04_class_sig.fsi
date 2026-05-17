module Containers

type Container<'T> =
    new: capacity: int -> Container<'T>
    member Count: int with get
    member Add: item: 'T -> unit
    abstract member Resize: newCapacity: int -> unit
