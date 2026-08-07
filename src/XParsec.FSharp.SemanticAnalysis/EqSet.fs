namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// An immutable collection with SET-semantic equality: equal iff the same members in any
/// order, hashed by a commutative combine. Construction DEDUPES and otherwise preserves
/// insertion order, so `A | B | A` reads back as the declared `A | B`.
[<Struct; IsReadOnly; CustomEquality; NoComparison>]
type EqSet<'T> =
    val private items: ImmutableArray<'T>

    /// `Equals`/`GetHashCode` below assume the members are distinct. The scan is quadratic —
    /// member counts are tiny — and an already-distinct input keeps its original array uncopied.
    new(items: ImmutableArray<'T>) =
        let cmp = EqualityComparer<'T>.Default
        let acc = ImmutableArray.CreateBuilder<'T>(items.Length)

        for i in 0 .. items.Length - 1 do
            let x = items.[i]
            let mutable dup = false
            let mutable j = 0

            while not dup && j < acc.Count do
                if cmp.Equals(acc.[j], x) then
                    dup <- true

                j <- j + 1

            if not dup then
                acc.Add x

        {
            items =
                if acc.Count = items.Length then
                    items
                else
                    acc.ToImmutable()
        }

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
