namespace Vesper.Collections

// Runtime implementation target for this repo's own backend — rung 2 of the
// self-hosting ladder (union type + recursion + `match`). Mirrors FSharp.Core's
// `FSharpList`. The type/`List.fold` contract lives in `core-types.fsi`.
//
// NOT fsc-buildable as authored: the `[]` / `::` union-case-operator syntax and
// the null-as-empty representation require `--compiling-fslib` (the same wall
// XParsec.FSharp.Lib hit — see its compiler-clr-project.md). fsc cannot build it
// outside fslib; our backend compiles it once rung 2 lands. Until then this is
// the authored target source. See minimal-core-lib-plan.md §"Self-hosting
// capability ladder".

open System

type List<'T> =
    | ([]): 'T list
    | (::): Head: 'T * Tail: 'T list -> 'T list

    static member Empty: 'T list = []

    member this.IsEmpty =
        match this with
        | [] -> true
        | _ -> false

    member this.Head =
        match this with
        | [] -> raise (InvalidOperationException "The input list was empty.")
        | h :: _ -> h

    member this.Tail =
        match this with
        | [] -> raise (InvalidOperationException "The input list was empty.")
        | _ :: t -> t

    member this.Length =
        let rec loop n (xs: 'T list) =
            match xs with
            | [] -> n
            | _ :: t -> loop (n + 1) t

        loop 0 this

    static member Cons(head: 'T, tail: 'T list) : 'T list = head :: tail

and 'T list = List<'T>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =

    // `folder state h` is function-application sugar; the backend lowers each
    // application to `callvirt Fun::Invoke`.
    let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (list: 'T list) : 'State =
        match list with
        | [] -> state
        | h :: t -> fold folder (folder state h) t

    // `ofSeq` / `toSeq` bridge the cons-list and the BCL `IEnumerable<'T>` seam.
    // Both ride Phase 4's `for x in IEnumerable`: `ofSeq` accumulates the
    // enumeration into a list comprehension (resolves to `[]` / `::` by arity);
    // `toSeq` re-publishes the list as a `seq` computation expression.
    let ofSeq (source: seq<'T>) : 'T list = [ for x in source -> x ]

    let toSeq (list: 'T list) : seq<'T> = seq { for x in list -> x }
