module Test

open System.Collections.Generic

// 'when' constraint on type declaration
type MySet<'T, 'Tag> when 'Tag :> IComparer<'T>(comparer: IComparer<'T>) =
    member _.Comparer = comparer

// Inline let with generic constraint on value
let inline Structural<'T when 'T: equality> : IEqualityComparer<'T> =
    EqualityComparer<'T>.Default
