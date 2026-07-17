namespace System

type IDisposable = Vesper.disposable

type IEquatable<'T> = Vesper.equatable<'T>

type IComparable<'T> = Vesper.comparable<'T>

namespace System.Collections.Generic

type IEnumerable<'T> = Vesper.Collections.seq<'T>

type IEnumerator<'T> = Vesper.Collections.enumerator<'T>
