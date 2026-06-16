namespace Vesper

// Vesper.Option — JS-target `impl` (codegen-js-steps.md Step 5b Phase 3), selected
// through the manifest `impl-js` override and compiled by the JS backend in library
// mode into the committed `Vesper.Option.mjs` runtime asset. Mirrors `list.js.fs`.
//
// Deviations from `option.fs`, all to stay inside the JS backend's surface:
//   * **No `[<Struct>]`** — JS has no value types; the union erases to the same
//     `{ tag, Value }` shape either way, so the attribute is dropped (the consumer's
//     inline `Option_Some`/`Option_None` and this module's emitted ones interoperate
//     via `.tag`, never `instanceof` — the Step-6 invariant).
//   * **No type member methods** (`Value`/`IsSome`/`IsNone`) — the JS backend emits
//     no member methods; the `Option` module functions cover the same ground.
//   * **`get` raises via an FFI `throw` template** (an expression-position IIFE,
//     riding `ILIntrinsic` → `JsExpr.Raw`) rather than `raise (InvalidOperationException …)`,
//     which would freeze to a `System.Exception` construction the JS backend cannot
//     emit. (`defaultWith`/`orElseWith`/`exists`/… need no such treatment.)
//
// The full `option.fs` module is portable as-is (every function is a `match` +
// `Vesper.Fun` application + `if`), so this is the whole `Option` module, not a
// subset.
//
// NOT Fantomas-formatted (this dir is in `.fantomasignore`): consistency with the
// other Vesper operator-/union-case impls.

type Option<'T> =
    | None
    | Some of Value: 'T

and 'T option = Option<'T>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =

    let isSome (option: 'T option) =
        match option with
        | None -> false
        | Some _ -> true

    let isNone (option: 'T option) =
        match option with
        | None -> true
        | Some _ -> false

    let defaultValue (value: 'T) (option: 'T option) =
        match option with
        | None -> value
        | Some v -> v

    let defaultWith (defThunk: unit -> 'T) (option: 'T option) =
        match option with
        | None -> defThunk ()
        | Some v -> v

    let orElse (ifNone: 'T option) (option: 'T option) =
        match option with
        | None -> ifNone
        | Some _ -> option

    let orElseWith (ifNoneThunk: unit -> 'T option) (option: 'T option) =
        match option with
        | None -> ifNoneThunk ()
        | Some _ -> option

    let get (option: 'T option) =
        match option with
        | None -> (# "(() => { throw new Error($0); })()" "Option.get: the option value was None" : 'T #)
        | Some v -> v

    let count (option: 'T option) =
        match option with
        | None -> 0
        | Some _ -> 1

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (option: 'T option) : 'State =
        match option with
        | None -> state
        | Some x -> folder state x

    let exists (predicate: 'T -> bool) (option: 'T option) =
        match option with
        | None -> false
        | Some x -> predicate x

    let forall (predicate: 'T -> bool) (option: 'T option) =
        match option with
        | None -> true
        | Some x -> predicate x

    let iter (action: 'T -> unit) (option: 'T option) =
        match option with
        | None -> ()
        | Some x -> action x

    let map (mapping: 'T -> 'U) (option: 'T option) =
        match option with
        | None -> None
        | Some x -> Some(mapping x)

    let bind (binder: 'T -> 'U option) (option: 'T option) =
        match option with
        | None -> None
        | Some x -> binder x

    let flatten (option: 'T option option) =
        match option with
        | None -> None
        | Some x -> x

    let filter (predicate: 'T -> bool) (option: 'T option) =
        match option with
        | None -> None
        | Some x -> if predicate x then Some x else None
