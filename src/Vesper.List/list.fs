namespace Vesper.Collections

// Vesper's cons-list. `List.fold` is compiled into this DLL so it carries a
// `Vesper.Core` `AssemblyRef`.
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

    // The cons-list IS a `seq<'T>`: it implements the ITERATION CAPABILITY directly,
    // walking its cells through the `ListEnumerator` cursor (mutual recursion
    // `and ListEnumerator`). The BCL faces (`IEnumerable<'T>` and the non-generic
    // `IEnumerable`) are NOT authored here — the CLR backend synthesizes them as
    // forwarding co-slots during capability reconciliation, so BCL interop is
    // unchanged while the source stays platform-agnostic.
    interface seq<'T> with
        member this.GetEnumerator() = (new ListEnumerator<'T>(this) :> enumerator<'T>)

// Struct enumerator: advance/read logic is inlined in interface members (a struct
// member calling another on `this` copies `this`, losing the mutation). The `'T
// list` abbreviation stays LAST in the rec group (mirroring the original
// declaration order); `ListEnumerator` sits between `List` and the abbreviation.
//
// `Reset` and the non-generic `IEnumerator` co-slots are likewise synthesized, not
// authored: the capability declares only the pull protocol. Disposal is the SEPARATE
// `disposable` capability (which `enumerator` inherits) — a no-op for an in-memory cursor.
and [<NoEquality; NoComparison; Struct>] ListEnumerator<'T> =
    val mutable cursor: List<'T>
    val mutable started: bool

    new(s: List<'T>) = { cursor = s; started = false }

    interface enumerator<'T> with
        member this.Current = this.cursor.Head

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

    interface Vesper.disposable with
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
    // `List<'T>` implements the iteration capability (the `ListSeq` wrapper is retired).
    let toSeq (list: 'T list) : seq<'T> = (list :> seq<'T>)

    // `ofSeq` iterates the source with `for x in source`, consing each element to the
    // front and reversing at the end. The `for .. in` form (not a manual
    // `GetEnumerator`/`MoveNext` walk) lets each backend lower the enumeration its own
    // way, so it ports cleanly to JS; the `while`/mutable accumulator keeps the
    // `'T`-consing loop closure-free, sidestepping the generic-closure codegen that kept
    // this forward-declared. `Set.ofSeq`/`set.fs` name-resolve it.
    let ofSeq (source: seq<'T>) : 'T list =
        let mutable acc = []

        for x in source do
            acc <- x :: acc

        rev acc
