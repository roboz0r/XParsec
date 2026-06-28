namespace Vesper

// Runtime implementation target for this repo's own backend. The type leg is a
// value-typed (struct) union with two value-carrying cases — `Ok` of `'T`,
// `Error` of `'TError`; neither case allocates on the heap. The module leg rides
// R1 (the `Fun`-not-`FSharpFunc` cutover, since the combinators are higher-order).
// The type / `Result` module contract lives in `result.fsi`.
//
// NOT fsc-buildable as authored: redefining the intrinsic `Result`/`Ok`/`Error`
// (which the F# compiler treats specially) requires `--compiling-fslib` — the
// same wall List.fs hit. Our backend compiles it once struct-union emit lands
// (the rung-2 union path currently emits reference classes). Until then this is
// the authored target source, a growing subset of the `result.fsi` contract (the
// array/list/option conversions and `contains` are not implemented here yet —
// see result.fsi).

[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpResult`2")>]
[<Struct>]
type Result<'T, 'TError> =
    | Ok of ResultValue: 'T
    | Error of ErrorValue: 'TError

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

    // `folder state x`, `predicate x`, etc. are function-application sugar; the
    // backend lowers each application to `callvirt Fun::Invoke`.

    let isOk (result: Result<'T, 'TError>) =
        match result with
        | Ok _ -> true
        | Error _ -> false

    let isError (result: Result<'T, 'TError>) =
        match result with
        | Ok _ -> false
        | Error _ -> true

    let defaultValue (value: 'T) (result: Result<'T, 'TError>) =
        match result with
        | Ok v -> v
        | Error _ -> value

    let defaultWith (defThunk: 'TError -> 'T) (result: Result<'T, 'TError>) =
        match result with
        | Ok v -> v
        | Error e -> defThunk e

    let count (result: Result<'T, 'TError>) =
        match result with
        | Ok _ -> 1
        | Error _ -> 0

    let fold<'T, 'TError, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (result: Result<'T, 'TError>) : 'State =
        match result with
        | Ok x -> folder state x
        | Error _ -> state

    let foldBack<'T, 'TError, 'State> (folder: 'T -> 'State -> 'State) (result: Result<'T, 'TError>) (state: 'State) : 'State =
        match result with
        | Ok x -> folder x state
        | Error _ -> state

    let exists (predicate: 'T -> bool) (result: Result<'T, 'TError>) =
        match result with
        | Ok x -> predicate x
        | Error _ -> false

    let forall (predicate: 'T -> bool) (result: Result<'T, 'TError>) =
        match result with
        | Ok x -> predicate x
        | Error _ -> true

    let iter (action: 'T -> unit) (result: Result<'T, 'TError>) =
        match result with
        | Ok x -> action x
        | Error _ -> ()

    let map (mapping: 'T -> 'U) (result: Result<'T, 'TError>) =
        match result with
        | Ok x -> Ok(mapping x)
        | Error e -> Error e

    let mapError (mapping: 'TError -> 'U) (result: Result<'T, 'TError>) =
        match result with
        | Ok x -> Ok x
        | Error e -> Error(mapping e)

    let bind (binder: 'T -> Result<'U, 'TError>) (result: Result<'T, 'TError>) =
        match result with
        | Ok x -> binder x
        | Error e -> Error e
