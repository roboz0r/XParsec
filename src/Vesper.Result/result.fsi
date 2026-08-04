namespace Vesper

open System

/// <summary>Helper type for error handling without exceptions.</summary>
///
/// <remarks>A <b>struct</b> union with two cases: <c>Ok</c> carries a success
/// value of <c>'T</c>, <c>Error</c> carries a failure value of <c>'TError</c>.
/// Use the constructors <c>Ok</c> and <c>Error</c> to create values of this type,
/// or pattern match against the values directly. Use the values in the
/// <c>Result</c> module to manipulate values of this type.</remarks>
///
/// <category>Choices and Results</category>
// Data, not State: structural equality (unconditional) + opt-in structural
// comparison, comparable iff its args are (the
// `Comparison.Structural ⇒ Equality.Structural` invariant holds).
[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpResult`2")>]
[<Struct>]
type Result<'T, 'TError> =

    /// Represents an OK or a Successful result. The code succeeded with a value of 'T.
    | Ok of ResultValue: 'T

    /// Represents an Error or a Failure. The code failed with a value of 'TError representing what went wrong.
    | Error of ErrorValue: 'TError

/// Operations over `Result<'T, 'TError>`. A focused starter set (the
/// bread-and-butter operations whose signatures touch only `Fun` / `bool` / `int`
/// / `result` itself); the array/list/option conversions (`toArray` / `toList` /
/// `toOption`) and `contains` (which needs `'T: equality`) are additive later, as
/// the language and the cross-package surface grow — the same "grow the module
/// additively" stance as Vesper.Option and Vesper.Core's `List`. Most members are
/// higher-order, so this module leg rides R1 (the `Fun`-not-`FSharpFunc`
/// cutover). The `ModuleSuffix` representation lets the module share the `Result`
/// name with the type (compiled name `ResultModule`). Each functional argument's
/// function type desugars to `Vesper.Fun`.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =

    /// `isOk inp` evaluates to `match inp with Ok _ -> true | Error _ -> false`.
    val isOk: result: Result<'T, 'TError> -> bool

    /// `isError inp` evaluates to `match inp with Ok _ -> false | Error _ -> true`.
    val isError: result: Result<'T, 'TError> -> bool

    /// Returns the success value if `Ok`, otherwise the supplied default.
    val defaultValue: value: 'T -> result: Result<'T, 'TError> -> 'T

    /// Returns the success value if `Ok`, otherwise the result of `defThunk` applied to the error value.
    val defaultWith: defThunk: ('TError -> 'T) -> result: Result<'T, 'TError> -> 'T

    /// `count inp` evaluates to `match inp with Ok _ -> 1 | Error _ -> 0`.
    val count: result: Result<'T, 'TError> -> int

    /// `fold f s inp` evaluates to `match inp with Ok x -> f s x | Error _ -> s`.
    val fold<'T, 'TError, 'State> :
        folder: ('State -> 'T -> 'State) -> state: 'State -> result: Result<'T, 'TError> -> 'State

    /// `foldBack f inp s` evaluates to `match inp with Ok x -> f x s | Error _ -> s`.
    val foldBack<'T, 'TError, 'State> :
        folder: ('T -> 'State -> 'State) -> result: Result<'T, 'TError> -> state: 'State -> 'State

    /// `exists p inp` evaluates to `match inp with Ok x -> p x | Error _ -> false`.
    val exists: predicate: ('T -> bool) -> result: Result<'T, 'TError> -> bool

    /// `forall p inp` evaluates to `match inp with Ok x -> p x | Error _ -> true`.
    val forall: predicate: ('T -> bool) -> result: Result<'T, 'TError> -> bool

    /// `iter f inp` executes `match inp with Ok x -> f x | Error _ -> ()`.
    val iter: action: ('T -> unit) -> result: Result<'T, 'TError> -> unit

    /// `map f inp` evaluates to `match inp with Ok x -> Ok (f x) | Error e -> Error e`.
    val map: mapping: ('T -> 'U) -> result: Result<'T, 'TError> -> Result<'U, 'TError>

    /// `mapError f inp` evaluates to `match inp with Ok x -> Ok x | Error e -> Error (f e)`.
    val mapError: mapping: ('TError -> 'U) -> result: Result<'T, 'TError> -> Result<'T, 'U>

    /// `bind f inp` evaluates to `match inp with Ok x -> f x | Error e -> Error e`.
    val bind: binder: ('T -> Result<'U, 'TError>) -> result: Result<'T, 'TError> -> Result<'U, 'TError>
