let ofArray (arr: 'T[]) : ArraySeq<'T> = ArraySeq<'T>(arr)
let map (f: 'TFunc when 'TFunc :> Fun<'T, 'U>) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : MapSeq<'S, 'E, 'TFunc, 'T, 'U> =
    MapSeq<'S, 'E, 'TFunc, 'T, 'U>(source, f)
let fold (f: 'TFunc when 'TFunc :> Fun<'State, 'T, 'State>) (seed: 'State) (source: 'S when 'S :> IStructSeq<'T, 'E> and 'E :> IStructEnumerator<'T>) : 'State =
    let mutable state = seed
    for y in source do
        state <- f.Invoke(state, y)
    state
