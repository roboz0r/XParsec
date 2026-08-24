namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// An immutable collection with SET-semantic equality: equal iff the same members in any
/// order, hashed by a commutative combine. Construction DEDUPES and otherwise preserves
/// insertion order, so `A | B | A` reads back as the declared `A | B`.
///
/// `'T` must hash consistently with its equality: construction hashes above
/// `EqSet.ScanLimit` members.
[<Struct; IsReadOnly; CustomEquality; NoComparison>]
type EqSet<'T> =
    val private items: ImmutableArray<'T>

    /// A member count up to which duplicate detection compares rather than hashes. A union
    /// type reaches two or three members, where a `HashSet` allocation and a structural hash
    /// of every member cost more than the comparisons they replace.
    static member ScanLimit: int = 8

    /// `Equals`/`GetHashCode` below assume the members are distinct. An already-distinct
    /// input keeps its original array uncopied and allocates nothing.
    new(items: ImmutableArray<'T>) =
        let cmp = EqualityComparer<'T>.Default

        let deduped =
            if items.Length <= EqSet<'T>.ScanLimit then
                // The index of the first member some earlier one repeats, or `-1`. Reaching
                // `-1` costs only comparisons, which is what keeps a distinct input free.
                let mutable firstDuplicate = -1
                let mutable i = 1

                while firstDuplicate < 0 && i < items.Length do
                    let mutable j = 0

                    while firstDuplicate < 0 && j < i do
                        if cmp.Equals(items.[j], items.[i]) then
                            firstDuplicate <- i

                        j <- j + 1

                    i <- i + 1

                if firstDuplicate < 0 then
                    items
                else
                    // The prefix below `firstDuplicate` is already distinct; only the rest is
                    // re-tested, against the accumulator.
                    let acc = ImmutableArray.CreateBuilder<'T>(items.Length - 1)
                    acc.AddRange(items, firstDuplicate)

                    for i in firstDuplicate + 1 .. items.Length - 1 do
                        let mutable dup = false
                        let mutable j = 0

                        while not dup && j < acc.Count do
                            if cmp.Equals(acc.[j], items.[i]) then
                                dup <- true

                            j <- j + 1

                        if not dup then
                            acc.Add items.[i]

                    acc.ToImmutable()
            else
                let seen = HashSet<'T>(items.Length, cmp)
                let acc = ImmutableArray.CreateBuilder<'T>(items.Length)

                for i in 0 .. items.Length - 1 do
                    if seen.Add items.[i] then
                        acc.Add items.[i]

                if acc.Count = items.Length then
                    items
                else
                    acc.ToImmutable()

        { items = deduped }

    /// Normalised so an uninitialised `EqSet` reads as empty rather than throwing.
    member this.Underlying: ImmutableArray<'T> =
        if this.items.IsDefault then
            ImmutableArray<'T>.Empty
        else
            this.items

    member this.Length: int = if this.items.IsDefault then 0 else this.items.Length

    member this.IsEmpty: bool = this.items.IsDefaultOrEmpty

    member this.Item
        with get (index: int): 'T = this.Underlying.[index]

    member this.GetEnumerator() = this.Underlying.GetEnumerator()

    interface IEquatable<EqSet<'T>> with
        member this.Equals(other: EqSet<'T>) =
            let a = this.Underlying
            let b = other.Underlying

            if a.Length <> b.Length then
                false
            else
                let cmp = EqualityComparer<'T>.Default
                let mutable i = 0
                let mutable ok = true

                while ok && i < a.Length do
                    let mutable found = false
                    let mutable j = 0

                    while not found && j < b.Length do
                        if cmp.Equals(a.[i], b.[j]) then
                            found <- true

                        j <- j + 1

                    if not found then
                        ok <- false

                    i <- i + 1

                ok

    override this.Equals(o: obj) =
        match o with
        | :? EqSet<'T> as other -> (this :> IEquatable<EqSet<'T>>).Equals other
        | _ -> false

    override this.GetHashCode() =
        // Unordered sum, so a permutation of the same members hashes identically.
        let xs = this.Underlying
        let cmp = EqualityComparer<'T>.Default
        let mutable h = 0

        for i in 0 .. xs.Length - 1 do
            h <- h + cmp.GetHashCode xs.[i]

        h

[<RequireQualifiedAccess>]
module EqSet =
    [<GeneralizableValue>]
    let empty<'T> : EqSet<'T> = EqSet<'T>(ImmutableArray<'T>.Empty)

    let ofSeq (xs: 'T seq) : EqSet<'T> =
        EqSet<'T>(ImmutableArray.CreateRange xs)

    let private asArray (xs: EqSet<'T>) : EqArray<'T> = EqArray.ofImmutable xs.Underlying

    let toList (xs: EqSet<'T>) : 'T list = EqArray.toList (asArray xs)

    let iter (action: 'T -> unit) (xs: EqSet<'T>) : unit = EqArray.iter action (asArray xs)

    let exists (predicate: 'T -> bool) (xs: EqSet<'T>) : bool = EqArray.exists predicate (asArray xs)

    let forall (predicate: 'T -> bool) (xs: EqSet<'T>) : bool = EqArray.forall predicate (asArray xs)

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: EqSet<'T>) : 'State =
        EqArray.fold folder state (asArray xs)
