namespace Vesper

// Runtime implementation target for this repo's own backend. The type leg is a
// value-typed (struct) union — `None` is the zero-initialized struct, `Some`
// carries its `Value` field; neither case allocates. The struct shape mirrors
// FSharp.Core's `ValueOption` (`ValueNone` / `ValueSome of 'T`), carried under the
// `Option` name. The module leg rides R1 (the `Fun`-not-`FSharpFunc` cutover,
// since the combinators are higher-order). The type / `Option` module contract
// lives in `option.fsi`.
//
// FSharp.Core's `static member None / Some / op_Implicit` are deliberately NOT
// mirrored: they exist there for C# / null-representation interop, which Vesper
// has no consumers for yet. The idiomatic instance members (`Value` / `IsSome` /
// `IsNone`) are kept. This is a growing subset of the `option.fsi` contract — the
// conversions and `map2`/`map3` are not implemented here yet (see option.fsi).
//
// NOT fsc-buildable as authored: redefining the intrinsic `Option`/`Some`/`None`
// (which the F# compiler treats specially) requires `--compiling-fslib`. Our
// backend compiles it directly.

open System

[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpOption`1")>]
[<Struct>]
type Option<'T> =
    | None
    | Some of Value: 'T

    member this.Value =
        match this with
        | Some v -> v
        | None -> raise (InvalidOperationException "Option.Value: the option value was None")

    member this.IsSome =
        match this with
        | Some _ -> true
        | None -> false

    member this.IsNone =
        match this with
        | None -> true
        | Some _ -> false

and 'T option = Option<'T>

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =

    // `folder state x`, `predicate x`, etc. are function-application sugar; the
    // backend lowers each application to `callvirt Fun::Invoke`.

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
        | None -> raise (InvalidOperationException "Option.get: the option value was None")
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
