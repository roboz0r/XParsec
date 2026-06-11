namespace Vesper

// ops-std.fs — the runtime *implementation* of `ops-std.fsi` (the `.fsi` is the
// target-agnostic contract, this `.fs` is the binding). Like `ops-platform.fs`,
// each `let inline` body here is read across the package boundary by the
// inline-body loader (`SymbolProviders.inlineBodies`) and spliced at each use
// site by the pre-freeze `Passes.InlineExpansion` pass — so the logical /
// composition / pipe operators flow through the same cross-package-inline
// machinery as the arithmetic / equality families, with NO operator hard-coded
// into the compiler.
//
// `&&` / `||` SHORT-CIRCUIT: the right operand is marked `[<CallAtMostOnce>]`, so
// the inliner splices it at its single use (call-by-name for that one validated-
// linear use) inside the `if`-body rather than eager `let`-binding it — the right
// operand is then evaluated at most once and only on demand, exactly as the `.fsi`
// documents. The laziness is DECLARED here (the attribute), not inferred from the
// body shape and not special-cased in the compiler: these are plain `val inline`
// bindings, the same machinery any library lazy combinator would use. (F# instead
// special-cases the `&&`/`||` intrinsic vals by identity; we keep the knowledge in
// the library contract.)
//
// NOT Fantomas-formatted (this dir is in `.fantomasignore`): authored to a fixed
// shape, parser coverage is the golden `.parsed` snapshot.

[<AutoOpen>]
module LogicalOperators =
    let inline (&&) (e1: bool) ([<CallAtMostOnce>] e2: bool) : bool = if e1 then e2 else false
    let inline (||) (e1: bool) ([<CallAtMostOnce>] e2: bool) : bool = if e1 then true else e2


[<AutoOpen>]
module CompositionOperators =
    let inline (>>) (func1: 'T1 -> 'T2) (func2: 'T2 -> 'T3) : 'T1 -> 'T3 = fun x -> func2 (func1 x)

    let inline (<<) (func2: 'T2 -> 'T3) (func1: 'T1 -> 'T2) : 'T1 -> 'T3 = fun x -> func2 (func1 x)

    let inline (|>) (arg: 'T1) (func: 'T1 -> 'U) : 'U = func arg

    let inline (||>) (arg1: 'T1, arg2: 'T2) (func: 'T1 -> 'T2 -> 'U) : 'U = func arg1 arg2

    let inline (|||>) (arg1: 'T1, arg2: 'T2, arg3: 'T3) (func: 'T1 -> 'T2 -> 'T3 -> 'U) : 'U = func arg1 arg2 arg3

    let inline (<|) (func: 'T -> 'U) (arg1: 'T) : 'U = func arg1

    let inline (<||) (func: 'T1 -> 'T2 -> 'U) (arg1: 'T1, arg2: 'T2) : 'U = func arg1 arg2

    let inline (<|||) (func: 'T1 -> 'T2 -> 'T3 -> 'U) (arg1: 'T1, arg2: 'T2, arg3: 'T3) : 'U = func arg1 arg2 arg3
