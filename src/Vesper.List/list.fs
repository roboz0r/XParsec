namespace Vesper.Collections

open System
open System.Collections
open System.Collections.Generic

// Vesper's cons-list. BCL-only; `List.fold` is compiled into this DLL so it
// carries a `Vesper.Core` `AssemblyRef`.
//
// NOT Fantomas-formatted (this dir is in `.fantomasignore`): Fantomas strips the
// `[]` / `::` operator-union-case payloads.

type List<'T> =
    | ([]): 'T list
    | (::): Head: 'T * Tail: 'T list -> 'T list

    member this.IsEmpty =
        match this with
        | [] -> true
        | _ :: _ -> false

    member this.Head =
        match this with
        | [] -> failwith "The input list was empty."
        | h :: _ -> h

    member this.Tail =
        match this with
        | [] -> failwith "The input list was empty."
        | _ :: t -> t

    // The cons-list IS a `seq<'T>`: it implements `IEnumerable<'T>` directly,
    // walking its cells through the `ListEnumerator` cursor (mutual recursion
    // `and ListEnumerator`). This retires the old `ListSeq` wrapper — `for x in xs`
    // over a bare list now drives `GetEnumerator` on the list itself.
    interface IEnumerable<'T> with
        member this.GetEnumerator() = (new ListEnumerator<'T>(this) :> IEnumerator<'T>)

    interface IEnumerable with
        member this.GetEnumerator() = (new ListEnumerator<'T>(this) :> IEnumerator)

// Struct enumerator: advance/read logic is inlined in interface members (a struct
// member calling another on `this` copies `this`, losing the mutation). The `'T
// list` abbreviation stays LAST in the rec group (mirroring the original
// declaration order); `ListEnumerator` sits between `List` and the abbreviation.
and [<NoEquality; NoComparison; Struct>] ListEnumerator<'T> =
    val mutable cursor: List<'T>
    val mutable started: bool
    val source: List<'T>

    new(s: List<'T>) =
        {
            cursor = s
            started = false
            source = s
        }

    interface IEnumerator<'T> with
        member this.Current = this.cursor.Head

    interface IEnumerator with
        member this.Current = box this.cursor.Head

        member this.MoveNext() =
            if this.started then
                match this.cursor with
                | [] -> false
                | _ :: t ->
                    this.cursor <- t
                    not this.cursor.IsEmpty
            else
                this.started <- true // The first call to MoveNext "starts" the enumeration.
                not this.cursor.IsEmpty

        member this.Reset() =
            this.cursor <- this.source
            this.started <- false

    interface IDisposable with
        member this.Dispose() = ()

and 'T list = List<'T>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =

    let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (list: 'T list) : 'State =
        match list with
        | [] -> state
        | h :: t -> fold folder (folder state h) t

    let isEmpty (list: 'T list) : bool =
        match list with
        | [] -> true
        | _ :: _ -> false

    // `length` / `rev` recurse directly rather than via `fold` + a lambda: a
    // closure that captures the function's own generic typar `'T` (the folder's
    // element parameter, or a cons cell it builds) is not yet encodable by the
    // backend ("cannot encode SemType: TyVar"). Direct structural recursion keeps
    // every `'T` in static-method scope where it encodes fine.
    let rec length (list: 'T list) : int =
        match list with
        | [] -> 0
        | _ :: t -> 1 + length t

    let head (list: 'T list) : 'T =
        match list with
        | [] -> failwith "The input list was empty."
        | h :: _ -> h

    let tail (list: 'T list) : 'T list =
        match list with
        | [] -> failwith "The input list was empty."
        | _ :: t -> t

    let rec map (mapping: 'T -> 'U) (list: 'T list) : 'U list =
        match list with
        | [] -> []
        | h :: t -> mapping h :: map mapping t

    let rec filter (predicate: 'T -> bool) (list: 'T list) : 'T list =
        match list with
        | [] -> []
        | h :: t ->
            if predicate h then
                h :: filter predicate t
            else
                filter predicate t

    let rec append (list1: 'T list) (list2: 'T list) : 'T list =
        match list1 with
        | [] -> list2
        | h :: t -> h :: append t list2

    // `rev` recurses structurally (`append (rev t) [h]`) rather than folding a
    // cons accumulator through a closure — same typar-encoding reason as `length`.
    // O(n²), acceptable for the minimal lib; a tail-recursive accumulator version
    // waits on generic-closure codegen.
    let rec rev (list: 'T list) : 'T list =
        match list with
        | [] -> []
        | h :: t -> append (rev t) (h :: [])

    // `toSeq` upcasts the list directly — the cons-list IS a `seq<'T>` now that
    // `List<'T>` implements `IEnumerable<'T>` (the `ListSeq` wrapper is retired).
    // `ofSeq` stays contract-only (`for x in IEnumerable`).
    let toSeq (list: 'T list) : IEnumerable<'T> = (list :> IEnumerable<'T>)
