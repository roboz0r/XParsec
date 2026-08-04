namespace Vesper.Collections

// The primitive `int` ordering subset, for the cursor's `taken >= limit` guard. NOT the
// polymorphic family in Vesper.Comparison: both operands are `int`, so this resolves
// without a `structuralCompare` detour.
open Vesper.IntComparison

/// `Seq.truncate`'s result — a lazily-truncating view of `source`.
///
/// Hand-rolled rather than a sequence expression because `seq { }` does not exist, in this
/// front end or in either backend. It is not standing in for one: this IS the state machine
/// a compiler would emit for `seq { for x in source do … }`, written out, and `list.fs`'s
/// `ListEnumerator` already proves the shape end to end on both targets.
type TruncateSeq<'T> =
    val source: seq<'T>
    val limit: int

    new(source: seq<'T>, limit: int) = { source = source; limit = limit }

    interface seq<'T> with
        member this.GetEnumerator() =
            (new TruncateEnumerator<'T>(this.source.GetEnumerator(), this.limit) :> enumerator<'T>)

and TruncateEnumerator<'T> =
    val inner: enumerator<'T>
    val limit: int
    val mutable taken: int

    new(inner: enumerator<'T>, limit: int) =
        {
            inner = inner
            limit = limit
            taken = 0
        }

    interface enumerator<'T> with
        member this.Current = this.inner.Current

        /// Stops AT the limit without pulling the element past it — an infinite or
        /// side-effecting source must not be advanced further than was asked for.
        member this.MoveNext() =
            if this.taken >= this.limit then false
            elif this.inner.MoveNext() then
                this.taken <- this.taken + 1
                true
            else
                false

    interface Vesper.disposable with
        /// The inner cursor may hold a resource; truncating a sequence does not release the
        /// caller from finishing with it.
        member this.Dispose() = this.inner.Dispose()

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =

    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (source: seq<'T>) : 'State =
        let mutable acc = state

        for x in source do
            acc <- folder acc x

        acc

    let reduce (reduction: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
        let mutable acc: 'T = Unchecked.defaultof<'T>
        let mutable seen = false

        for x in source do
            acc <- if seen then reduction acc x else x
            seen <- true

        if not seen then
            invalidArg "source" "The input sequence was empty."

        acc

    let truncate (count: int) (source: seq<'T>) : seq<'T> =
        (new TruncateSeq<'T>(source, count) :> seq<'T>)

    let toArray (source: seq<'T>) : 'T[] =
        // A doubling buffer, then one exactly-sized copy. The source is enumerated ONCE
        // and its length is never asked for: a seq may be one-shot, and may not know it.
        let mutable buffer: 'T[] = Array.zeroCreate 4
        let mutable count = 0

        for x in source do
            if count = buffer.Length then
                let grown: 'T[] = Array.zeroCreate (buffer.Length * 2)

                for i = 0 to count - 1 do
                    grown.[i] <- buffer.[i]

                buffer <- grown

            buffer.[count] <- x
            count <- count + 1

        let result: 'T[] = Array.zeroCreate count

        for i = 0 to count - 1 do
            result.[i] <- buffer.[i]

        result
