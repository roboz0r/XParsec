namespace Vesper

#nowarn "42"

type disposable = (# "System.IDisposable" #)
type equatable<'T> = (# "System.IEquatable`1" #)
type comparable<'T> = (# "System.IComparable`1" #)

namespace Vesper.Collections

type enumerator<'T> = (# "System.Collections.Generic.IEnumerator`1" #)
type seq<'T> = (# "System.Collections.Generic.IEnumerable`1" #)
