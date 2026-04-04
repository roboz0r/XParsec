module Test

// Match guard with 'let' binding inside 'when'
// From QueryExtensions.fs lines 130-135

open System.Reflection

let (|NewAnonymousObject|_|) (e: obj) =
    match e with
    | :? MethodInfo as ctor when
        let dty = ctor.DeclaringType
        dty <> null && dty.IsGenericType
        ->
        Some ctor
    | _ ->
        None
