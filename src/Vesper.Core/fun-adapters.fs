namespace Vesper

type Curried<'A, 'B, 'C>(f: Fun<'A, 'B, 'C>, a: 'A) =
    interface Fun<'B, 'C> with
        member _.Invoke(b) = f.Invoke(a, b)

type Flattened<'A, 'B, 'C>(f: Fun<'A, Fun<'B, 'C>>) =
    interface Fun<'A, 'B, 'C> with
        member _.Invoke(a: 'A, b: 'B) : 'C = f.Invoke(a).Invoke(b)

[<AutoOpen>]
module FunAdapters =

    let curryFun (f: Fun<'A, 'B, 'C>) (a: 'A) : Fun<'B, 'C> = Curried(f, a) :> Fun<'B, 'C>

    let flatten (f: Fun<'A, Fun<'B, 'C>>) : Fun<'A, 'B, 'C> = Flattened(f) :> Fun<'A, 'B, 'C>
