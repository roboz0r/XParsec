namespace XParsec.FSharp.Codegen.Js

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// The char-offset → (line, column) index over a file's text, and the declaring files one
/// emission attributed a node to: the entries after slot 0 of the emitted map's `"sources"`.
module JsMapSources =

    /// `Starts.[n]` is the char offset line `n` begins at, so `"a\nbc"` gives `[| 0; 2 |]`.
    /// `Length` is that text's length, which bounds a resolved offset.
    type LineIndex = { Starts: int[]; Length: int }

    module LineIndex =
        let build (source: string) : LineIndex =
            let starts = ResizeArray<int>()
            starts.Add 0

            for i in 0 .. source.Length - 1 do
                if source.[i] = '\n' then
                    starts.Add(i + 1)

            {
                Starts = starts.ToArray()
                Length = source.Length
            }

        /// `source` is the emitted map's `"sources"` slot holding the text `offset` indexes.
        /// A past-end offset clamps to the end of the last line.
        let resolve (idx: LineIndex) (source: int) (offset: int) : JsLoc =
            let offset = max 0 (min offset idx.Length)
            let starts = idx.Starts
            // Binary search for the greatest line start <= offset.
            let mutable lo = 0
            let mutable hi = starts.Length - 1

            while lo < hi do
                let mid = (lo + hi + 1) / 2

                if starts.[mid] <= offset then lo <- mid else hi <- mid - 1

            {
                Source = source
                Line = lo
                Column = offset - starts.[lo]
            }

    /// `Slot` is this file's position in the emitted map's `"sources"`; `Lines` is over `Published.Content`.
    type DeclaringSource =
        {
            Slot: int
            Published: JsMapSource
            Lines: LineIndex
        }

    type MapSources =
        private
            {
                Ordered: ResizeArray<DeclaringSource>
                ByPath: Dictionary<AssemblyFilePath, DeclaringSource>
            }

    module MapSources =

        let create () : MapSources =
            {
                Ordered = ResizeArray()
                ByPath = Dictionary()
            }

        /// The emitted map's `"sources"` opens with the file being compiled, so the first
        /// `publish` here takes slot 1: `["app.fs", "Vesper.Core/math/z.fs", …]`. Paths are
        /// package-qualified because two packages may each ship a `math/z.fs`.
        let publish (file: LexedFile) (m: MapSources) : unit =
            let entry =
                {
                    Slot = m.Ordered.Count + 1
                    Published =
                        {
                            Path =
                                AssemblyName.toStored file.Path.Assembly
                                + "/"
                                + AssemblyFileId.toStored file.Path.Relative
                            Content = file.Input
                        }
                    Lines = LineIndex.build file.Input
                }

            m.Ordered.Add entry
            m.ByPath.[file.Path] <- entry

        /// `ValueNone` is a broken invariant, not a position to fall back from: the caller
        /// resolving a node's map position throws rather than reading the compiling file's text.
        let tryFind (file: AssemblyFilePath) (m: MapSources) : DeclaringSource voption =
            match m.ByPath.TryGetValue file with
            | true, entry -> ValueSome entry
            | _ -> ValueNone

        let published (m: MapSources) : JsMapSource list =
            [ for entry in m.Ordered -> entry.Published ]
