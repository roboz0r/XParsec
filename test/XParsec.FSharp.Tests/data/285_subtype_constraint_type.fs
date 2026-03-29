module Test

let f (x: 'T :> System.IDisposable) = x

let g (resource: 'T :> System.IDisposable | null) = resource

let h (source: 'T -> #seq<'U>) = source
