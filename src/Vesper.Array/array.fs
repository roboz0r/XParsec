namespace Vesper.Collections

// Runtime implementation target for this repo's own backend. The `Array` module
// over the intrinsic `'T[]` type; the contract lives in `array.fsi`. Compiles to
// the `Vesper.Collections.ArrayModule` static class (the `ModuleSuffix`
// representation gives the module the `ArrayModule` holder name, matching the
// FSharp.Core surface). BCL-only — no `FSharp.Core`.
//
// `zeroCreate` is open-coded over the `newarr` IL intrinsic (the same
// `(# "..." #)` surface the arithmetic / comparison operators use), so it carries
// no dependency beyond the intrinsic. `fold` is a counted index loop over the
// array — `folder` is a `Vesper.Fun`, so each application lowers to
// `callvirt Fun::Invoke`. A focused starter surface (just what `set.fs` consumes);
// the rest of the FSharp.Core `Array` surface is additive later.

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Array =

    let zeroCreate (count: int) : 'T[] = (# "newarr !0" type ('T) count : 'T[] #)

    let fold (folder: 'State -> 'T -> 'State) (state: 'State) (array: 'T[]) : 'State =
        let mutable acc = state

        for i = 0 to array.Length - 1 do
            acc <- folder acc array.[i]

        acc
