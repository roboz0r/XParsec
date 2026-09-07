namespace XParsec.FSharp.SemanticAnalysis

open System.Collections.Immutable
open System.Runtime.InteropServices

open Vesper

[<AutoOpen>]
module internal RefEquality =
    let inline refEq (x: 'T) (y: 'T) = System.Object.ReferenceEquals(x, y)

/// Transitional alias for `Vesper.Block`. Call sites move to `Block` directly, and this file
/// goes with the last of them.
type EqArray<'T> = Block<'T>

[<RequireQualifiedAccess>]
module EqArray =
    [<GeneralizableValue>]
    let empty<'T> : EqArray<'T> = Block.empty

    /// Wraps the immutable array's own buffer, so nothing is copied.
    let ofImmutable (xs: ImmutableArray<'T>) : EqArray<'T> =
        match ImmutableCollectionsMarshal.AsArray xs with
        | null -> Block.empty
        | arr -> Block.unsafeOfArray arr

    let singleton (x: 'T) : EqArray<'T> = Block.singleton x

    let ofSeq (xs: 'T seq) : EqArray<'T> = Block.ofSeq xs

    let ofList (xs: 'T list) : EqArray<'T> = Block.ofList xs

    /// Copies, so the caller may keep mutating `xs`.
    let ofArray (xs: 'T[]) : EqArray<'T> = Block.ofArray xs

    /// Copies, so the caller may keep mutating `xs`.
    let ofResizeArray (xs: ResizeArray<'T>) : EqArray<'T> = Block.ofResizeArray xs

    let toList (xs: EqArray<'T>) : 'T list = Block.toList xs

    /// Copies into a fresh mutable array.
    let toArray (xs: EqArray<'T>) : 'T[] = Block.toArray xs

    let init (n: int) (f: int -> 'T) : EqArray<'T> = Block.init n f

    let map (mapping: 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> = Block.map mapping xs

    /// Reference-preserving map: `ValueNone`, allocating nothing, when `mapping` returns a
    /// reference-equal result for EVERY element.
    let mapPreserve<'T when 'T: not struct> (mapping: 'T -> 'T) (xs: EqArray<'T>) : EqArray<'T> voption =
        Block.mapPreserve mapping xs

    let mapi (mapping: int -> 'T -> 'U) (xs: EqArray<'T>) : EqArray<'U> = Block.mapi mapping xs

    let iter (action: 'T -> unit) (xs: EqArray<'T>) : unit = Block.iter action xs

    let iteri (action: int -> 'T -> unit) (xs: EqArray<'T>) : unit = Block.iteri action xs

    let exists (predicate: 'T -> bool) (xs: EqArray<'T>) : bool = Block.exists predicate xs

    let forall (predicate: 'T -> bool) (xs: EqArray<'T>) : bool = Block.forall predicate xs

    /// Returns `false` on a length mismatch; it does NOT throw, so a caller that must
    /// distinguish "mismatched" from "unequal" has to compare `.Length` itself.
    let forall2 (predicate: 'T -> 'U -> bool) (xs: EqArray<'T>) (ys: EqArray<'U>) : bool = Block.forall2 predicate xs ys

    /// Raises `ArgumentException` on a length mismatch.
    let map2 (mapping: 'T -> 'U -> 'V) (xs: EqArray<'T>) (ys: EqArray<'U>) : EqArray<'V> = Block.map2 mapping xs ys

    let tryFind (predicate: 'T -> bool) (xs: EqArray<'T>) : 'T voption = Block.tryFind predicate xs

    let tryFindIndex (predicate: 'T -> bool) (xs: EqArray<'T>) : int voption = Block.tryFindIndex predicate xs

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (xs: EqArray<'T>) : 'State = Block.fold folder state xs

    /// Visits elements last-to-first.
    let foldBack (folder: 'T -> 'State -> 'State) (xs: EqArray<'T>) (state: 'State) : 'State =
        Block.foldBack folder xs state

    /// `ValueNone` for an out-of-range index, in place of an indexer's throw.
    let tryItem (index: int) (xs: EqArray<'T>) : 'T voption = Block.tryItem index xs

    let tryLast (xs: EqArray<'T>) : 'T voption = Block.tryLast xs

    let last (xs: EqArray<'T>) : 'T = Block.last xs

    let filter (predicate: 'T -> bool) (xs: EqArray<'T>) : EqArray<'T> = Block.filter predicate xs

    /// Ascending by `Comparer<'T>.Default`, so callers need no `'T: comparison` constraint.
    let sort (xs: EqArray<'T>) : EqArray<'T> = Block.sort xs

    let append (xs: EqArray<'T>) (ys: EqArray<'T>) : EqArray<'T> = Block.append xs ys

    /// First occurrence of each element wins, so the surviving order is the input's. Uses
    /// `EqualityComparer<'T>.Default`, so callers need no `'T: equality` constraint.
    let distinct (xs: EqArray<'T>) : EqArray<'T> = Block.distinct xs

    /// Uses `EqualityComparer<'T>.Default`, so callers need no `'T: equality` constraint.
    let contains (value: 'T) (xs: EqArray<'T>) : bool = Block.contains value xs

    /// Returns the first `count` elements (all of them when `count` exceeds the
    /// length; empty when `count <= 0`). Returns the SAME array when nothing is dropped.
    let truncate (count: int) (xs: EqArray<'T>) : EqArray<'T> = Block.truncate count xs

/// Fixed-arity deconstruction patterns: allocation-free arity checks that bind elements by
/// index, in place of `match EqArray.toList xs with [ … ]`.
[<AutoOpen>]
module EqArrayPatterns =
    [<return: Struct>]
    let (|EqEmpty|_|) (xs: EqArray<'T>) : unit voption =
        match xs with
        | BlockEmpty -> ValueSome()
        | _ -> ValueNone

    [<return: Struct>]
    let (|EqOne|_|) (xs: EqArray<'T>) : 'T voption =
        match xs with
        | BlockOne x -> ValueSome x
        | _ -> ValueNone

    [<return: Struct>]
    let (|EqTwo|_|) (xs: EqArray<'T>) : struct ('T * 'T) voption =
        match xs with
        | BlockTwo(a, b) -> ValueSome(struct (a, b))
        | _ -> ValueNone

    [<return: Struct>]
    let (|EqThree|_|) (xs: EqArray<'T>) : struct ('T * 'T * 'T) voption =
        match xs with
        | BlockThree(a, b, c) -> ValueSome(struct (a, b, c))
        | _ -> ValueNone
