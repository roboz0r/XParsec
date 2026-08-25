namespace XParsec.FSharp.SemanticAnalysis

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

/// An immutable key → value map with structural (value) equality: equal iff the same keys, each
/// bound to an equal value, hashed by a commutative combine. An uninitialised value reads as
/// empty.
[<Struct; IsReadOnly; CustomEquality; NoComparison>]
type EqDict<'K, 'V> =
    val private items: ImmutableDictionary<'K, 'V>

    new(items: ImmutableDictionary<'K, 'V>) = { items = items }

    member this.Underlying: ImmutableDictionary<'K, 'V> =
        if isNull this.items then
            ImmutableDictionary<'K, 'V>.Empty
        else
            this.items

    member this.Count: int = if isNull this.items then 0 else this.items.Count

    member this.IsEmpty: bool = isNull this.items || this.items.IsEmpty

    member this.Item
        with get (key: 'K): 'V = this.Underlying.[key]

    member this.Keys: 'K seq = this.Underlying.Keys

    member this.Values: 'V seq = this.Underlying.Values

    member this.ContainsKey(key: 'K) : bool = this.Underlying.ContainsKey key

    member this.GetEnumerator() = this.Underlying.GetEnumerator()

    interface IEnumerable<KeyValuePair<'K, 'V>> with
        member this.GetEnumerator() : IEnumerator<KeyValuePair<'K, 'V>> =
            (this.Underlying :> IEnumerable<KeyValuePair<'K, 'V>>).GetEnumerator()

    interface IEnumerable with
        member this.GetEnumerator() : IEnumerator =
            (this.Underlying :> IEnumerable).GetEnumerator()

    interface IEquatable<EqDict<'K, 'V>> with
        member this.Equals(other: EqDict<'K, 'V>) =
            let a = this.Underlying
            let b = other.Underlying

            if a.Count <> b.Count then
                false
            else
                let cmp = EqualityComparer<'V>.Default
                let mutable e = a.GetEnumerator()
                let mutable ok = true

                while ok && e.MoveNext() do
                    let kv = e.Current

                    match b.TryGetValue kv.Key with
                    | true, v -> ok <- cmp.Equals(kv.Value, v)
                    | _ -> ok <- false

                ok

    override this.Equals(o: obj) =
        match o with
        | :? EqDict<'K, 'V> as other -> (this :> IEquatable<EqDict<'K, 'V>>).Equals other
        | _ -> false

    override this.GetHashCode() =
        // Unordered sum, so entry order does not reach the hash.
        let cmpK = EqualityComparer<'K>.Default
        let cmpV = EqualityComparer<'V>.Default
        let mutable e = this.Underlying.GetEnumerator()
        let mutable h = 0

        while e.MoveNext() do
            let kv = e.Current
            h <- h + ((cmpK.GetHashCode kv.Key * 397) ^^^ cmpV.GetHashCode kv.Value)

        h

[<RequireQualifiedAccess>]
module EqDict =
    [<GeneralizableValue>]
    let empty<'K, 'V> : EqDict<'K, 'V> =
        EqDict<'K, 'V>(ImmutableDictionary<'K, 'V>.Empty)

    /// Throws on a key repeated with a different value.
    let ofSeq (entries: KeyValuePair<'K, 'V> seq) : EqDict<'K, 'V> =
        EqDict<'K, 'V>(ImmutableDictionary.CreateRange entries)

    let tryFind (key: 'K) (d: EqDict<'K, 'V>) : 'V voption =
        match d.Underlying.TryGetValue key with
        | true, v -> ValueSome v
        | _ -> ValueNone
