namespace Vesper.Collections

// Vesper's cons-list — the compiled runtime impl of the `Vesper.List` package,
// in the verbatim FSharp.Core `[]` / `::` operator-case form. This is the
// *cutover* target `list-min.fs`'s `Nil` / `Cons` deviation was a placeholder
// for (minimal-core-lib-plan D6): it is now buildable by our own backend because
// the front end lowers cons patterns (`h :: t`), the empty-list pattern (`[]`),
// and cons construction (`x :: xs`) through the same `TPat.Union` / `TExpr.UnionCons`
// machinery the named cases used. The `[]` / `::` declarations compile to
// FSharpList's exact shape: `[]` → a static `Empty` factory, `(::)` → a static
// `Cons` factory + `Cons_0` / `Cons_1` payload fields, with a `_tag` discriminator
// (so `match`/construction resolve the case names by arity, the `Empty`/`Cons`
// pair `TypeRegistration.unionCaseName` mints for the operator heads).
//
// BCL-only, no FSharp.Core: `Head` / `Tail` raise via `failwith` (the backend
// lowers it to a plain `System.Exception`, so the list type references no
// `FSharp.Core`). `List.fold` is compiled *into* this DLL — its folder parameter
// is a `Vesper.Fun`, so the DLL carries a `Vesper.Core` `AssemblyRef`; each
// folder application lowers to `callvirt Fun::Invoke`.
//
// The module surface is the proven "grow" set (vesper-lib-test-plan Phase 2):
// `fold`/`isEmpty`/`length`/`head`/`tail`/`map`/`filter`/`append`/`rev`, each
// built only from cons patterns, `[]` / `::` construction, `Vesper.Fun`
// application, `if`, and recursion. `ofSeq` / `toSeq` stay contract-only in
// `list.fsi` — they ride Phase 4's `for x in IEnumerable` + seq comprehensions,
// not yet compilable.
//
// NOT Fantomas-formatted (this dir is in `.fantomasignore`): Fantomas strips the
// `[]` / `::` operator-union-case payloads. Authored to a fixed shape; parser
// coverage is the golden `list.fs.parsed` snapshot.

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

and 'T list = List<'T>

// Compiles to the `Vesper.Collections.ListModule` static class (the
// `ModuleSuffix` representation gives the module the holder name `ListModule`
// because the type `List` shares its name in this namespace). The folder's arrow
// desugars to `Vesper.Fun`; each application lowers to `callvirt Fun::Invoke`.
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
