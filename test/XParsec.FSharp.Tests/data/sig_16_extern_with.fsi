module Primitives

type Plain = extern

type Iterable<'T> = extern with
    interface System.Collections.Generic.IEnumerable<'T>
    member GetEnumerator: unit -> System.Collections.Generic.IEnumerator<'T>
