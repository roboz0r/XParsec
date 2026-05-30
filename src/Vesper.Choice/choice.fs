namespace Vesper

// Runtime implementation target for this repo's own backend. A value-typed
// (struct) union with two value-carrying cases — `Choice1Of2` of `'T1`,
// `Choice2Of2` of `'T2`; neither case allocates on the heap. The same shape as
// Vesper.Result (`Ok` / `Error`), so the backend's two-case heterogeneous
// struct-DU emit is already proven. The type contract lives in `choice.fsi`.
//
// NOT fsc-buildable as authored: redefining the intrinsic `Choice`/`Choice1Of2`/
// `Choice2Of2` (which the F# compiler treats specially) requires
// `--compiling-fslib` — the same wall Result.fs / List.fs hit. Our backend
// compiles it via the struct-union emit path. There is no `Choice` module today
// (the sole consumer `set.fs` uses only the constructors + pattern matching);
// combinators are additive later, the same "grow the module additively" stance
// as Vesper.Result.

[<StructuralEquality; StructuralComparison>]
[<CompiledName("FSharpChoice`2")>]
[<Struct>]
type Choice<'T1, 'T2> =
    | Choice1Of2 of Choice1Of2: 'T1
    | Choice2Of2 of Choice2Of2: 'T2
