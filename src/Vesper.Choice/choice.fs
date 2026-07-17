namespace Vesper

[<StructuralEquality; StructuralComparison>]
[<Struct>]
type Choice<'T1, 'T2> =
    | Choice1Of2 of Choice1Of2: 'T1
    | Choice2Of2 of Choice2Of2: 'T2

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
