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
// `List.fold` is compiled *into* this DLL now (R3 deferred — public
// module-function compilation): the `module List` below emits as a real
// `Vesper.Collections.ListModule` static class, so a consuming program calls
// `ListModule::fold<'State,'T>` via a `MethodSpec` rather than inlining the loop.
// Because `fold`'s folder parameter is a `Vesper.Fun`, this DLL now carries a
// `Vesper.Core` `AssemblyRef` (it had none while only the minimal list shipped).
// `Head` / `Tail` use `failwith` (lowered to a BCL `System.Exception`) so the
// list type itself still references no `FSharp.Core`.

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

// Compiles to the `Vesper.Collections.ListModule` static class (the
// `ModuleSuffix` representation gives the module the holder name `ListModule`
// because the type `List` shares its name in this namespace). `fold` is the only
// operation R3 needs; the rest of the module is additive. The folder's arrows
// desugar to `Vesper.Fun`; each application lowers to `callvirt Fun::Invoke`.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module List =

    let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (list: List<'T>) : 'State =
        match list with
        | Nil -> state
        | Cons(h, t) -> fold folder (folder state h) t

    // `ofSeq` / `toSeq` bridge the cons-list and the BCL `IEnumerable<'T>` seam,
    // riding Phase 4's `for x in IEnumerable`. The list comprehension resolves to
    // the `Nil` / `Cons` union by arity (the same arity binding `[1; 2; 3]` uses);
    // `toSeq` re-publishes the list as a `seq` computation expression.
    let ofSeq (source: seq<'T>) : List<'T> = [ for x in source -> x ]

    let toSeq (list: List<'T>) : seq<'T> = seq { for x in list -> x }
