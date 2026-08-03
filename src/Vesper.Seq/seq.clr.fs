namespace Vesper.Collections

open System.Collections.Generic
open System.Linq

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Seq =

    let fold<'T, 'State> (folder: 'State -> 'T -> 'State) (state: 'State) (source: seq<'T>) : 'State =
        let mutable acc = state

        for x in source do
            acc <- folder acc x

        acc

    let reduce (reduction: 'T -> 'T -> 'T) (source: seq<'T>) : 'T =
        let mutable acc: 'T = Unchecked.defaultof<'T>
        let mutable seen = false

        for x in source do
            acc <- if seen then reduction acc x else x
            seen <- true

        if not seen then
            invalidArg "source" "The input sequence was empty."

        acc

    let truncate (count: int) (source: seq<'T>) : seq<'T> =
        Enumerable.Take(source, count)

    let toArray (source: seq<'T>) : 'T[] =
        let res = ResizeArray<'T>()

        for x in source do
            res.Add(x)

        res.ToArray()
