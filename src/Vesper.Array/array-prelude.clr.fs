namespace Vesper.Collections

module ArrayPrelude =

    let inline NewArray (count: int) : 'T[] = (# "newarr !0" type ('T) count : 'T[] #)
