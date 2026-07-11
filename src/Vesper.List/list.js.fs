namespace Vesper.Collections

// Vesper.List — JS-target `impl`, selected
// through the manifest `impl-js` override (the mechanism Vesper.Core uses for
// `inline-bodies-js`) and compiled by the JS backend in *library* mode into the
// committed `Vesper.List.mjs` runtime asset — retiring the hand-authored `.mjs`.
//
// This is NOT the full `list.fs`: a faithful `list.fs` compile needs two things
// the JS backend has no surface for (union/record member methods; the
// class/interface/`[<Struct>]` machinery behind `ListSeq`/`ListEnumerator`/`toSeq`).
// Instead this exposes only the `.mjs` subset, authored in backend-friendly idiom:
//
//   * The cons-list *type* is declared here with its `[]`/`::` cases but **no member
//     methods** (the `IsEmpty`/`Head`/`Tail`/`Length`/… the `.fsi` advertises) — so
//     the JS backend emits just the base + `List_Empty`/`List_Cons` classes
//     (`collectTypes`), no member emission. Like `list.fs`, the impl resolves its own
//     type locally (the provider is `depends-on` only — `Vesper.Core`), so the
//     module functions' `[]`/`::` construct/match the in-file union.
//   * `head`/`tail` raise the empty-list error with `failwith`, whose JS inline body
//     (`ops-platform.js.fs`) is the expression-position IIFE
//     `(() => { throw new Error($0); })()` — riding the `ILIntrinsic` → `JsExpr.Raw`
//     path with no new backend arms (cf. `ops-platform.js.fs`'s `$N` operator
//     templates).
//   * The set is `fold`/`isEmpty`/`length`/`head`/`tail`/`map`/`filter`/`append`/`rev`
//     (the `list.fs` "grow" set minus `toSeq`/`ofSeq`, which need the
//     class/interface surface). `fold` is tail-recursive → trampolined; the rest
//     recurse structurally (JS-stack, fine for the MVP runtime).
//
// The generic functions need NO accumulator gymnastics (unlike `list.fs`, which
// dodges the CLR "cannot encode SemType: TyVar" closure gap): JS erases types, so
// `map`/`rev` are plain recursive arrows. The interop invariant Step 6 locked holds —
// the match compiler and structural runtime read `.tag` + own keys, never
// `instanceof`, so a `List_Cons` this module builds and one a consumer builds (with
// its own separately-emitted class) are interchangeable.
//
// NOT Fantomas-formatted (this dir is in `.fantomasignore`): Fantomas strips the
// `[]`/`::` operator-union-case payloads.

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
// each later call advances to the tail. Mirrors `list.fs`'s `ListEnumerator` but
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

// The `'T list` abbreviation stays LAST in the rec group (mirroring `list.fs` and the
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
