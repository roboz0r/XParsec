namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// An insertion-ordered immutable collection with **SET-semantic equality**: two
/// `EqSet`s are equal iff they hold the same MEMBER SET (order-insensitive), and
/// the hash is a commutative (order-independent) combine of the member hashes.
/// Construction DEDUPES, keeping the first occurrence — so iteration order is the
/// declared order minus later duplicates.
///
/// This is the sibling of `EqArray` (which is order-SENSITIVE, for positional
/// data — tuples, args, typar vectors). `EqSet` exists solely for anonymous-union
/// (`FTOr`/`TyOr`) MEMBER storage, per the codegen-js-symbol-provider design
/// (§"Literal types stay structural … EqSet sub-decision"): a total order on
/// `FrozenType` does not exist (so canonical-SORT normalisation was rejected), but
/// set equality reuses `'T`'s own `Equals`/`GetHashCode`, and insertion order lets
/// the DECLARED `.d.ts` union order survive into diagnostics / the manifest golden.
/// `default`/uninitialised reads as empty.
///
/// `NoComparison` is deliberate — a union member set has no natural order and
/// nothing keys on one (do NOT add `CustomComparison` without a concrete need).
[<Struct; IsReadOnly; CustomEquality; NoComparison>]
type EqSet<'T> =
    val private items: ImmutableArray<'T>

    /// PRIVATE-in-spirit: the raw array is assumed already deduped (the `EqSet`
    /// module functions are the sanctioned producers and dedupe on the way in).
    new(items: ImmutableArray<'T>) = { items = items }

    /// The backing array, normalised so a `default(EqSet)` reads as empty.
    member this.Underlying: ImmutableArray<'T> =
        if this.items.IsDefault then
            ImmutableArray<'T>.Empty
        else
            this.items

    member this.Length: int = if this.items.IsDefault then 0 else this.items.Length

    member this.IsEmpty: bool = this.items.IsDefaultOrEmpty

    member this.Item
        with get (index: int): 'T = this.Underlying.[index]

    /// Contiguous read-only view (insertion order), for hot traversals.
    member this.AsSpan() : ReadOnlySpan<'T> = this.Underlying.AsSpan()

    /// Allocation-free struct enumerator; enables `for x in eqSet do …` in
    /// insertion order.
    member this.GetEnumerator() = this.Underlying.GetEnumerator()

    interface IEquatable<EqSet<'T>> with
        member this.Equals(other: EqSet<'T>) =
            let a = this.Underlying
            let b = other.Underlying
            // Set equality: same cardinality (both are deduped, so cardinality =
            // length) AND every member of `a` occurs in `b`. `EqualityComparer<'T>`
            // dispatches to `'T`'s own `IEquatable<'T>`, so members recurse
            // structurally exactly like `EqArray`.
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
        // Commutative (order-independent) combine — the unordered sum of member
        // hashes — so a permutation of the SAME members hashes identically,
        // consistent with the set-semantic equality above.
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

    /// The one normaliser: dedupe an arbitrary sequence, keeping the FIRST
    /// occurrence of each distinct member (so insertion order is preserved).
    let ofSeq (xs: 'T seq) : EqSet<'T> =
        let acc = ResizeArray<'T>()

        for x in xs do
            if not (acc.Contains x) then
                acc.Add x

        EqSet<'T>(ImmutableArray.CreateRange acc)

    let ofList (xs: 'T list) : EqSet<'T> = ofSeq xs

    let singleton (x: 'T) : EqSet<'T> = EqSet<'T>(ImmutableArray.Create(x))

    let toList (xs: EqSet<'T>) : 'T list = List.ofSeq xs.Underlying

    /// Map each member then RE-DEDUPE: a mapping can collapse two distinct members
    /// onto one (`'a | string` with `'a := string`), so the result routes back
    /// through `ofSeq` and stays a canonical set.
    let map (mapping: 'T -> 'U) (xs: EqSet<'T>) : EqSet<'U> =
        let src = xs.Underlying
        let acc = ResizeArray<'U>(src.Length)

        for i in 0 .. src.Length - 1 do
            let y = mapping src.[i]

            if not (acc.Contains y) then
                acc.Add y

        EqSet<'U>(ImmutableArray.CreateRange acc)

    let iter (action: 'T -> unit) (xs: EqSet<'T>) : unit =
        let src = xs.Underlying

        for i in 0 .. src.Length - 1 do
            action src.[i]

    let exists (predicate: 'T -> bool) (xs: EqSet<'T>) : bool =
        let src = xs.Underlying
        let mutable i = 0
        let mutable hit = false

        while not hit && i < src.Length do
            if predicate src.[i] then
                hit <- true

            i <- i + 1

        hit

    let forall (predicate: 'T -> bool) (xs: EqSet<'T>) : bool =
        let src = xs.Underlying
        let mutable i = 0
        let mutable ok = true

        while ok && i < src.Length do
            if not (predicate src.[i]) then
                ok <- false

            i <- i + 1

        ok

    let contains (x: 'T) (xs: EqSet<'T>) : bool =
        let cmp = EqualityComparer<'T>.Default
        exists (fun y -> cmp.Equals(x, y)) xs

    let tryFind (predicate: 'T -> bool) (xs: EqSet<'T>) : 'T voption =
        let src = xs.Underlying
        let mutable i = 0
        let mutable found = ValueNone

        while found.IsNone && i < src.Length do
            if predicate src.[i] then
                found <- ValueSome src.[i]

            i <- i + 1

        found

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: EqSet<'T>) : 'State =
        let src = xs.Underlying
        let mutable acc = state

        for i in 0 .. src.Length - 1 do
            acc <- folder acc src.[i]

        acc
