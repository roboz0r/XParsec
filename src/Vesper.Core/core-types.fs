namespace Vesper

#nowarn "42"

// Runtime body of the cell type whose contract sits in `core-types.fsi`.
// Captured-mutable promotion rewrites escaping `let mutable` bindings into `Ref<'T>`,
// so the type must be a real `TypeDefinition` in `Vesper.Core.dll`.

[<ReferenceEquality>]
[<NoComparison>]
type Ref<'T> = { mutable contents: 'T }

type Curried<'A, 'B, 'C>(f: Fun2<'A, 'B, 'C>, a: 'A) =
    interface Fun<'B, 'C> with
        member _.Invoke(b) = f.Invoke(a, b)

type Flattened<'A, 'B, 'C>(f: Fun<'A, Fun<'B, 'C>>) =
    interface Fun2<'A, 'B, 'C> with
        member _.Invoke(a: 'A, b: 'B) : 'C = f.Invoke(a).Invoke(b)

[<AutoOpen>]
module FunAdapters =

    let curryFun (f: Fun2<'A, 'B, 'C>) (a: 'A) : Fun<'B, 'C> = Curried(f, a) :> Fun<'B, 'C>

    let flatten (f: Fun<'A, Fun<'B, 'C>>) : Fun2<'A, 'B, 'C> = Flattened(f) :> Fun2<'A, 'B, 'C>
