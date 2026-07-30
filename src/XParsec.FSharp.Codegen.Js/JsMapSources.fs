namespace XParsec.FSharp.Codegen.Js

open System.Collections.Generic
open XParsec.FSharp.SemanticAnalysis

/// What the emission accumulates FOR THE MAP, and nothing else: the char-offset → (line,
/// column) index a V3 map's coordinates are, and the set of producer files one emission
/// attributed a node to. It reads no tree and holds no walk state, which is why it sits here
/// rather than in `EmitJsContext` — the walker's context merely CARRIES a `MapSources` from
/// `EmitJs.buildProgram`, which discovers the files, to the driver, which publishes them
/// (`JsSourceMap.build`).
module JsMapSources =

    /// Maps a source char offset to 0-based (line, column) — V3 source-map
    /// coordinates. `Starts.[n]` is the char offset at which line `n` begins.
    /// Columns count UTF-16 code units, as V3 maps require.
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

        /// Resolve a char offset within the text of `source` — an index into the map's
        /// `sources[]` — to a `JsLoc`. Clamps past-end offsets to the last line.
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

    /// A producer file this emission attributed at least one node to: its slot in the finished
    /// `sources[]`, what the map publishes for it, and the line starts that turn a token offset
    /// in that text into V3 coordinates.
    type ProducerSource =
        {
            Slot: int
            Published: JsMapSource
            Lines: LineIndex
        }

    /// The producer files ONE emission reached, in publication order.
    ///
    /// Mutable and shared with the driver for the same reason `WalkCtx.Pool` is: WHICH producers
    /// a program reaches is settled by splicing the specialization graph, which happens inside
    /// `EmitJs.buildProgram`, while the map that must publish them is built by whoever called it.
    ///
    /// Index 0 of the finished `sources[]` is the CONSUMING file and is not held here, so the
    /// first `publish` takes slot 1 — which is why a build that reaches no retained producer
    /// publishes exactly the single-element array it always did.
    type MapSources =
        private
            {
                Ordered: ResizeArray<ProducerSource>
                ByPath: Dictionary<OriginPath, ProducerSource>
            }

    module MapSources =

        let create () : MapSources =
            {
                Ordered = ResizeArray()
                ByPath = Dictionary()
            }

        /// Give `src` the next slot. THE mutator, so `Ordered`'s position and `Slot` cannot
        /// come apart — a mapping whose source index disagreed with the array would resolve
        /// into the wrong file's text and still decode cleanly.
        ///
        /// The published PATH is `<package>/<relative>`. Not `Absolute`: a build-machine path
        /// leaks the layout of the machine that compiled and resolves nowhere in a browser.
        /// Not the bare `Relative` either: two packages may each publish an `ops.fs`, and a
        /// `sources[]` with two identical names is unusable. The package-qualified form is
        /// unique across the manifest set, machine-independent, and the shape a bundler's
        /// `sourceRoot` (or a debugger's path mapping) is built to prefix.
        let publish (src: OriginSource) (m: MapSources) : unit =
            let entry =
                {
                    Slot = m.Ordered.Count + 1
                    Published =
                        {
                            Path = src.File.Path.BucketName + "/" + src.File.Path.Relative
                            Content = src.Input
                        }
                    Lines = LineIndex.build src.Input
                }

            m.Ordered.Add entry
            m.ByPath.[src.File.Path] <- entry

        /// The slot `file` was published at, or `ValueNone` when it was not published — a file
        /// the emission reached but the compilation never retained. Its nodes then keep the
        /// call-site position, which is the answer a single-source map has always given them;
        /// `EmitJsContext.locOf` names the one configuration that still produces such a file.
        let tryFind (file: OriginPath) (m: MapSources) : ProducerSource voption =
            match m.ByPath.TryGetValue file with
            | true, entry -> ValueSome entry
            | _ -> ValueNone

        /// The producers as the map publishes them, in slot order — appended AFTER the
        /// consuming file, which owns index 0.
        let published (m: MapSources) : JsMapSource list =
            [ for entry in m.Ordered -> entry.Published ]
