namespace Vesper

open System

/// <summary>Helper type for two-way disjoint union results.</summary>
///
/// <remarks>A <b>struct</b> union with two cases: <c>Choice1Of2</c> carries a
/// value of <c>'T1</c>, <c>Choice2Of2</c> carries a value of <c>'T2</c>. Use the
/// constructors <c>Choice1Of2</c> and <c>Choice2Of2</c> to create values of this
/// type, or pattern match against the values directly.</remarks>
///
/// <category>Choices and Results</category>
[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2> =

    /// Choice 1 of 2 choices.
    | Choice1Of2 of Choice1Of2: 'T1

    /// Choice 2 of 2 choices.
    | Choice2Of2 of Choice2Of2: 'T2

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
