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
[<CompiledName("FSharpChoice`2")>]
[<Struct>]
type Choice<'T1, 'T2> =

    /// Choice 1 of 2 choices.
    | Choice1Of2 of Choice1Of2: 'T1

    /// Choice 2 of 2 choices.
    | Choice2Of2 of Choice2Of2: 'T2
