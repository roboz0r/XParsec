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
[<Struct>]
type Choice<'T1, 'T2> =
    | Choice1Of2 of Choice1Of2: 'T1
    | Choice2Of2 of Choice2Of2: 'T2

// Higher-arity variants up through `Choice<'T1, …, 'T7>` (the full FSharp.Core
// surface, `FSharpChoice`2`…`FSharpChoice`7`). Same struct + named-field posture
// as the 2-arity case: each case carries one payload, the field is named after its
// case so the struct union's field names stay distinct, and the backend's N-case
// heterogeneous struct-DU emit (`_tag` + one payload field + factory per case)
// generalises the proven 2-case path. The type contract lives in `choice.fsi`.

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3> =
    | Choice1Of3 of Choice1Of3: 'T1
    | Choice2Of3 of Choice2Of3: 'T2
    | Choice3Of3 of Choice3Of3: 'T3

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4> =
    | Choice1Of4 of Choice1Of4: 'T1
    | Choice2Of4 of Choice2Of4: 'T2
    | Choice3Of4 of Choice3Of4: 'T3
    | Choice4Of4 of Choice4Of4: 'T4

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5> =
    | Choice1Of5 of Choice1Of5: 'T1
    | Choice2Of5 of Choice2Of5: 'T2
    | Choice3Of5 of Choice3Of5: 'T3
    | Choice4Of5 of Choice4Of5: 'T4
    | Choice5Of5 of Choice5Of5: 'T5

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> =
    | Choice1Of6 of Choice1Of6: 'T1
    | Choice2Of6 of Choice2Of6: 'T2
    | Choice3Of6 of Choice3Of6: 'T3
    | Choice4Of6 of Choice4Of6: 'T4
    | Choice5Of6 of Choice5Of6: 'T5
    | Choice6Of6 of Choice6Of6: 'T6

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> =
    | Choice1Of7 of Choice1Of7: 'T1
    | Choice2Of7 of Choice2Of7: 'T2
    | Choice3Of7 of Choice3Of7: 'T3
    | Choice4Of7 of Choice4Of7: 'T4
    | Choice5Of7 of Choice5Of7: 'T5
    | Choice6Of7 of Choice6Of7: 'T6
    | Choice7Of7 of Choice7Of7: 'T7
