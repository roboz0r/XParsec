namespace Vesper.Collections

// Vesper's cons-list — the runtime type the R3 cutover resolves a bare program's
// `[…]` literals and `List.fold` onto (handoff R3). Compiled by *our own* backend
// into `Vesper.Core.dll`, BCL-only.
//
// Mirrors FSharpList's shape (a nullary terminator + a binary cons), but uses the
// explicit `Nil` / `Cons` case names rather than the `[]` / `::` operator forms —
// a sanctioned deviation (minimal-core-lib-plan D6) that keeps the core inside the
// proven front-end subset and unambiguous. `[1; 2; 3]` still resolves here: a
// list literal binds to the union by arity (nullary terminator + binary cons),
// not by case name.
//
// `List.fold` is **not** compiled here yet. R3 chose to emit it *inline* over this
// type in the consuming program (the backend `emitFold` walks
// `IsEmpty` / `Head` / `Tail` + `Vesper.Fun::Invoke`); compiling `fold` *into*
// this DLL needs public module-function compilation (a self-reference to `Fun`
// and a real `Vesper.Collections.ListModule` holder) — the deferred gap in
// selfhost-handoff.md. `Head` / `Tail` use `failwith` (lowered to a BCL
// `System.Exception`) so this DLL carries no `FSharp.Core` reference.

type List<'T> =
    | Nil
    | Cons of 'T * List<'T>

    member this.IsEmpty =
        match this with
        | Nil -> true
        | Cons(_, _) -> false

    member this.Head =
        match this with
        | Nil -> failwith "The input list was empty."
        | Cons(h, _) -> h

    member this.Tail =
        match this with
        | Nil -> failwith "The input list was empty."
        | Cons(_, t) -> t
