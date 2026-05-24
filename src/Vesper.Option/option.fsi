namespace Vesper

// Vesper.Option contract — the option type + the `Option` module, carved out of
// Vesper.Core's `core-types.fsi` (package-split-plan PS1: one package per type).
// Like the rest of the Vesper tree this is the front-end symbol contract: parsed
// by XParsec.FSharp and walked into an IExternalSymbolProvider. The runtime impl
// is `option.fs` (→ Vesper.Option.dll, BCL-only, our own backend). Depends on
// Vesper.Core (`Fun`, `unit`, `bool`, `int`).
//
// Per package-split-plan PS5 the package is named `Vesper.Option` but it
// contributes type `Option` into namespace `Vesper`, not `Vesper.Option`.
//
// `ValueOption`/`voption` stay in Vesper.Core's `core-types.fsi` for now.

open System

/// <summary>The type of optional values, represented as a <b>struct</b>.</summary>
///
/// <remarks>Use the constructors <c>Some</c> and <c>None</c> to create values of this type,
/// or pattern match against the values directly. Use the values in the <c>Option</c>
/// module to manipulate values of this type.
///
/// Unlike F#'s reference-typed option, this is a value type: <c>None</c> is the
/// zero-initialized struct, so a default field or a freshly-allocated array of
/// options is already all <c>None</c>, and neither <c>None</c> nor <c>Some</c>
/// allocates on the heap. The exact runtime layout is a backend decision; the
/// contract fixes only that <c>Option</c> is a struct whose default is
/// <c>None</c>.</remarks>
///
/// <category>Options</category>
[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpOption`1")>]
[<Struct>]
type Option<'T> =

    /// <summary>The representation of "No value" — the zero-initialized struct.</summary>
    | None

    /// <summary>The representation of "Value of type 'T"</summary>
    ///
    /// <param name="Value">The input value.</param>
    ///
    /// <returns>An option representing the value.</returns>
    | Some

    /// <summary>Create an option value that is a 'None' value.</summary>
    /// <exclude />
    static member None: 'T option

    /// <summary>Create an option value that is a 'Some' value.</summary>
    ///
    /// <param name="value">The input value</param>
    ///
    /// <returns>An option representing the value.</returns>
    /// <exclude />
    static member Some: value: 'T -> 'T option

    /// <summary>Implicitly converts a value into an optional that is a 'Some' value.</summary>
    ///
    /// <param name="value">The input value</param>
    ///
    /// <returns>An option representing the value.</returns>
    /// <exclude />
    static member op_Implicit: value: 'T -> 'T option

    /// <summary>Get the value of a 'Some' option. An InvalidOperationException is raised if the option is 'None'.</summary>
    member Value: 'T

    /// <summary>Return 'true' if the option is a 'Some' value.</summary>
    member IsSome: bool

    /// <summary>Return 'true' if the option is a 'None' value.</summary>
    member IsNone: bool

/// <summary>The type of optional values, represented as a struct.
/// See <see cref="T:Vesper.Option`1"/>.</summary>
///
/// <category index="3">Options</category>
and 'T option = Option<'T>

/// Operations over `'T option`. A focused starter set (the bread-and-butter
/// operations whose signatures touch only `Fun` / `bool` / `int` / `option`
/// itself); the array/list/Nullable/obj conversions and `map2`/`map3` are
/// additive later, as the language and the cross-package surface grow — the same
/// "grow the module additively" stance as Vesper.Core's `List`. Most members are
/// higher-order, so this module leg rides R1 (the `Fun`-not-`FSharpFunc`
/// cutover). The `ModuleSuffix` representation lets the module share the `Option`
/// name with the type (compiled name `OptionModule`). Each functional argument's
/// arrow desugars to `Vesper.Fun`.
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Option =

    /// `isSome inp` evaluates to `match inp with None -> false | Some _ -> true`.
    val isSome: option: 'T option -> bool

    /// `isNone inp` evaluates to `match inp with None -> true | Some _ -> false`.
    val isNone: option: 'T option -> bool

    /// Returns the value if `Some`, otherwise the supplied default.
    val defaultValue: value: 'T -> option: 'T option -> 'T

    /// Returns the value if `Some`, otherwise the result of `defThunk ()`.
    val defaultWith: defThunk: (unit -> 'T) -> option: 'T option -> 'T

    /// Returns `option` if it is `Some`, otherwise `ifNone`.
    val orElse: ifNone: 'T option -> option: 'T option -> 'T option

    /// Returns `option` if it is `Some`, otherwise the result of `ifNoneThunk ()`.
    val orElseWith: ifNoneThunk: (unit -> 'T option) -> option: 'T option -> 'T option

    /// Gets the value of a `Some`. Raises if the option is `None`.
    val get: option: 'T option -> 'T

    /// `count inp` evaluates to `match inp with None -> 0 | Some _ -> 1`.
    val count: option: 'T option -> int

    /// `fold f s inp` evaluates to `match inp with None -> s | Some x -> f s x`.
    val fold: folder: ('State -> 'T -> 'State) -> state: 'State -> option: 'T option -> 'State

    /// `exists p inp` evaluates to `match inp with None -> false | Some x -> p x`.
    val exists: predicate: ('T -> bool) -> option: 'T option -> bool

    /// `forall p inp` evaluates to `match inp with None -> true | Some x -> p x`.
    val forall: predicate: ('T -> bool) -> option: 'T option -> bool

    /// `iter f inp` executes `match inp with None -> () | Some x -> f x`.
    val iter: action: ('T -> unit) -> option: 'T option -> unit

    /// `map f inp` evaluates to `match inp with None -> None | Some x -> Some (f x)`.
    val map: mapping: ('T -> 'U) -> option: 'T option -> 'U option

    /// `bind f inp` evaluates to `match inp with None -> None | Some x -> f x`.
    val bind: binder: ('T -> 'U option) -> option: 'T option -> 'U option

    /// `flatten inp` evaluates to `match inp with None -> None | Some x -> x`.
    val flatten: option: 'T option option -> 'T option

    /// `filter p inp` keeps the value only when `p` holds; otherwise `None`.
    val filter: predicate: ('T -> bool) -> option: 'T option -> 'T option
