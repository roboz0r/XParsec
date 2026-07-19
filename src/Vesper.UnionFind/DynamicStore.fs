namespace Vesper.UnionFind

open System.Collections.Generic

/// A dense, monotone, measure-typed id arena: interns each distinct `'T` and hands back a
/// stable `int<'M>` id assigned in first-seen order.
///
/// Grow-only by design — ids are never reused. That is exactly the property a semi-persistent
/// consumer needs (see `SemiPersistentUnionFind`): a live id can never come to mean two
/// different elements across a rollback, so the store can be a single mutable object shared by
/// every version without breaking observational persistence. Reuse/removal would reintroduce
/// aliasing and is deliberately not offered.
///
/// The phantom measure `'M` tags the id space, so the compiler rejects passing one arena's
/// ids to a consumer parameterized over another (e.g. a type-var arena vs a region arena).
[<Sealed>]
type DynamicStore<'T, [<Measure>] 'M when 'T: equality>(capacity: int) =
    let data = ResizeArray<'T>(max 0 capacity)
    let index = Dictionary<'T, int>(max 0 capacity)

    new() = DynamicStore(0)

    /// The number of distinct elements interned so far — also the next id that will be minted.
    member _.Count = data.Count

    /// The element behind an id. The id must have been minted by this store.
    member _.Item
        with get (i: int<'M>): 'T = data[int i]

    /// The id of `x`, minting a fresh monotone id if `x` has not been seen before.
    member _.GetOrAdd(x: 'T) : int<'M> =
        match index.TryGetValue x with
        | true, i -> LanguagePrimitives.Int32WithMeasure<'M> i
        | _ ->
            let id = data.Count
            data.Add x
            index[x] <- id
            LanguagePrimitives.Int32WithMeasure<'M> id

    /// The id of `x` if it has been interned.
    member _.TryId(x: 'T) : int<'M> voption =
        match index.TryGetValue x with
        | true, i -> ValueSome(LanguagePrimitives.Int32WithMeasure<'M> i)
        | _ -> ValueNone

    /// Whether `x` has been interned.
    member _.Contains(x: 'T) : bool = index.ContainsKey x
