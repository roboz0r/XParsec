namespace Vesper

// Vesper.Choice contract — the `Choice<'T1, 'T2>` type, a standalone package
// (package-split-plan PS1: one package per type) mirroring Vesper.Result and
// Vesper.Option. Like the rest of the Vesper tree this is the front-end symbol
// contract: parsed by XParsec.FSharp and walked into an IExternalSymbolProvider.
// The runtime impl is `choice.fs` (→ Vesper.Choice.dll, BCL-only, our own
// backend). Depends on Vesper.Core (`unit`, `bool`, `int`).
//
// Per package-split-plan PS5 the package is named `Vesper.Choice` but it
// contributes type `Choice` into namespace `Vesper`, not `Vesper.Choice`.
//
// Sole consumer today is `set.fs` (lines 440/441/446/773/1064): `partition1With`
// births a `Choice` from `partitioner k` and consumes it in the very next
// `match`. It never escapes and runs once per set element — which is why this is
// a [<Struct>] (zero heap allocations per element), the same posture and proven
// layout as Vesper.Result. See vesper-set-sprint-phase-8.md §8.1 for the
// struct-not-class rationale and the higher-arity / SROA follow-ups.

open System

/// <summary>Helper type for two-way disjoint union results.</summary>
///
/// <remarks>A <b>struct</b> union with two cases: <c>Choice1Of2</c> carries a
/// value of <c>'T1</c>, <c>Choice2Of2</c> carries a value of <c>'T2</c>. Use the
/// constructors <c>Choice1Of2</c> and <c>Choice2Of2</c> to create values of this
/// type, or pattern match against the values directly.</remarks>
///
/// <category>Choices and Results</category>
// Data, not State: structural equality (unconditional) + opt-in structural
// comparison, comparable iff its args are (operators-plan.md O10; the O7
// `Comparison.Structural ⇒ Equality.Structural` invariant holds), exactly as
// Vesper.Result.
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2> =

    /// Choice 1 of 2 choices.
    | Choice1Of2 of Choice1Of2: 'T1

    /// Choice 2 of 2 choices.
    | Choice2Of2 of Choice2Of2: 'T2

// Higher-arity variants up through `Choice<'T1, …, 'T7>`, matching the FSharp.Core
// surface (`prim-types.fsi` `FSharpChoice`2`…`FSharpChoice`7`) — FSharp.Core stops
// at arity 7, so this is the whole family, not a 16-wide tower. They keep the same
// posture as the 2-arity case (vesper-set-sprint-phase-8.md §8.1 follow-up):
// `[<Struct>]` + named-field cases (a struct union requires *distinct* field names
// across all cases, so each field is named after its case), structural equality +
// opt-in structural comparison. Each added arm widens the struct by one payload;
// none is consumed by `set.fs` yet (which uses only the 2-arity form), but they
// round out the active-pattern helper surface. SROA / slot-sharing for the wider
// arms stays the deferred codegen follow-up.

/// <summary>Helper type for active patterns with 3 choices.</summary>
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3> =

    /// Choice 1 of 3 choices.
    | Choice1Of3 of Choice1Of3: 'T1

    /// Choice 2 of 3 choices.
    | Choice2Of3 of Choice2Of3: 'T2

    /// Choice 3 of 3 choices.
    | Choice3Of3 of Choice3Of3: 'T3

/// <summary>Helper type for active patterns with 4 choices.</summary>
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4> =

    /// Choice 1 of 4 choices.
    | Choice1Of4 of Choice1Of4: 'T1

    /// Choice 2 of 4 choices.
    | Choice2Of4 of Choice2Of4: 'T2

    /// Choice 3 of 4 choices.
    | Choice3Of4 of Choice3Of4: 'T3

    /// Choice 4 of 4 choices.
    | Choice4Of4 of Choice4Of4: 'T4

/// <summary>Helper type for active patterns with 5 choices.</summary>
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5> =

    /// Choice 1 of 5 choices.
    | Choice1Of5 of Choice1Of5: 'T1

    /// Choice 2 of 5 choices.
    | Choice2Of5 of Choice2Of5: 'T2

    /// Choice 3 of 5 choices.
    | Choice3Of5 of Choice3Of5: 'T3

    /// Choice 4 of 5 choices.
    | Choice4Of5 of Choice4Of5: 'T4

    /// Choice 5 of 5 choices.
    | Choice5Of5 of Choice5Of5: 'T5

/// <summary>Helper type for active patterns with 6 choices.</summary>
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6> =

    /// Choice 1 of 6 choices.
    | Choice1Of6 of Choice1Of6: 'T1

    /// Choice 2 of 6 choices.
    | Choice2Of6 of Choice2Of6: 'T2

    /// Choice 3 of 6 choices.
    | Choice3Of6 of Choice3Of6: 'T3

    /// Choice 4 of 6 choices.
    | Choice4Of6 of Choice4Of6: 'T4

    /// Choice 5 of 6 choices.
    | Choice5Of6 of Choice5Of6: 'T5

    /// Choice 6 of 6 choices.
    | Choice6Of6 of Choice6Of6: 'T6

/// <summary>Helper type for active patterns with 7 choices.</summary>
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2, 'T3, 'T4, 'T5, 'T6, 'T7> =

    /// Choice 1 of 7 choices.
    | Choice1Of7 of Choice1Of7: 'T1

    /// Choice 2 of 7 choices.
    | Choice2Of7 of Choice2Of7: 'T2

    /// Choice 3 of 7 choices.
    | Choice3Of7 of Choice3Of7: 'T3

    /// Choice 4 of 7 choices.
    | Choice4Of7 of Choice4Of7: 'T4

    /// Choice 5 of 7 choices.
    | Choice5Of7 of Choice5Of7: 'T5

    /// Choice 6 of 7 choices.
    | Choice6Of7 of Choice6Of7: 'T6

    /// Choice 7 of 7 choices.
    | Choice7Of7 of Choice7Of7: 'T7
