namespace Vesper.Collections

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

    interface seq<'T> with
        member this.GetEnumerator() = (new ListEnumerator<'T>(this) :> enumerator<'T>)

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

    let toSeq (list: 'T list) : seq<'T> = (list :> seq<'T>)

    let ofSeq (source: seq<'T>) : 'T list =
        let mutable acc = []

        for x in source do
            acc <- x :: acc

        rev acc
