namespace Containers

type Container<'T> =
    inherit System.Object

    new: capacity: int -> Container<'T>
    internal new: capacity: int * eager: bool -> Container<'T>

    val mutable internal items: 'T array
    val readonly: 'T

    abstract member Count: int with get, set

    interface System.Collections.IEnumerable
