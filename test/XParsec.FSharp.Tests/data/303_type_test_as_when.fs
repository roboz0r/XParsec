module Test

// :? type test pattern with 'as' alias and 'when' guard
// From SymbolPatterns.fs lines 18-26

let classify (arg: obj) =
    match arg with
    | :? int32 as arg when arg = int System.Int32.MaxValue ->
        Some "max int"
    | :? System.Enum as arg when arg.GetType().IsPublic ->
        Some "public enum"
    | _ ->
        None
