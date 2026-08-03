namespace Vesper.Collections

type List<'T> =
    | ([]): 'T list
    | (::): Head: 'T * Tail: 'T list -> 'T list

    // The cons-list IS a `seq<'T>`: its iteration-capability impl lands on the base
    // class `List` (a JS union value is a case-subclass instance, so the
    // `*[Symbol.iterator]()` adapter must be inherited by every case), driving the
    // `ListEnumerator` cursor. `for x in xs` over a bare list now drives this — no
    // `:> seq` upcast. Because the JS union emits NO `.Head`/`.Tail` accessors, the
    // enumerator walks the cells with cons-pattern `match`, not member access.
    interface seq<'T> with
        member this.GetEnumerator() : enumerator<'T> = (new ListEnumerator<'T>(this) :> enumerator<'T>)

// JS-target cons enumerator: a `val mutable` cursor walked by `MoveNext`/`Current`
// (the duck-typed protocol the `*[Symbol.iterator]()` generator adapter drives).
// `started` makes the first `MoveNext` "start" the walk (cursor stays at the head);
// each later call advances to the tail. Mirrors `list.clr.fs`'s `ListEnumerator` but
// pattern-matches the cons cells (no `.Head`/`.IsEmpty` members on the JS union).
and ListEnumerator<'T> =
    val mutable cursor: 'T list
    val mutable started: bool

    new(s: 'T list) = { cursor = s; started = false }

    interface enumerator<'T> with
        member this.MoveNext() : bool =
            // Advance the cursor (the first call only "starts" it, leaving the head),
            // then report non-empty ONCE — the started/first-call branches differ only
            // in how they move the cursor, not in the post-move emptiness test.
            if this.started then
                match this.cursor with
                | [] -> ()
                | _ :: t -> this.cursor <- t
            else
                this.started <- true

            match this.cursor with
            | [] -> false
            | _ :: _ -> true

        member this.Current : 'T =
            match this.cursor with
            | [] -> failwith "The input list was empty."
            | h :: _ -> h

// The `'T list` abbreviation stays LAST in the rec group (mirroring `list.clr.fs` and the
// original declaration order); `ListEnumerator` sits between `List` and the abbreviation.
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

    let rec rev (list: 'T list) : 'T list =
        match list with
        | [] -> []
        | h :: t -> append (rev t) (h :: [])
