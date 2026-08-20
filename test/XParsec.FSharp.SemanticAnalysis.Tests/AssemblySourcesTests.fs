module XParsec.FSharp.SemanticAnalysis.Tests.AssemblySourcesTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.Codegen.Common.Tests

// What the front end is handed: the assembly a run emits into, paired with the units it
// compiles. The pairing is the point — a manifest's name and target ride with the files that
// manifest lists, and a synthetic run states both.

let private impl (id: string) (text: string) : SourceUnit =
    SourceUnit.ofImplementation (SourceFile.ofText id text)

/// The implementation half of every unit that parsed, by file name, in order.
let private parsedNames (sources: AssemblySources) : string list =
    [
        for u in sources.Units do
            match u with
            | Ok u -> u.Implementation.Id.Name
            | Error e -> failtestf "unit %s yielded no tree: %A" e.Id.Name e.Failure.Diagnostics
    ]

/// A body inside `#if FOO` that the parser has to recover from, so the branch taken shows in
/// the unit's recovery findings.
let private guardedSource =
    """module M
#if FOO
let broken = (1 + 2
#endif
let y = 1
"""

let private recoveryDiagnostics (sources: AssemblySources) : Diagnostic list =
    match sources.Units with
    | [ Ok u ] -> u.Implementation.Parsed.Diagnostics
    | other -> failtestf "expected one parsed unit, got %i" (List.length other)

[<Tests>]
let tests =
    testList
        "AssemblySources"
        [
            test "synthetic compiles under the name and target it is given" {
                let sources =
                    AssemblySources.synthetic "Synth" "clr" Set.empty [ impl "a.fs" "module A" ]

                Expect.equal sources.Assembly.Name (AssemblyName "Synth") "the caller's name"
                Expect.equal sources.Assembly.Target "clr" "the caller's target"
            }

            test "synthetic keeps its units in the order they were handed over" {
                let sources =
                    AssemblySources.synthetic
                        "Synth"
                        "clr"
                        Set.empty
                        [ impl "b.fs" "module B"; impl "a.fs" "module A"; impl "c.fs" "module C" ]

                Expect.equal (parsedNames sources) [ "b.fs"; "a.fs"; "c.fs" ] "the caller's order"
            }

            test "synthetic parses each unit under the compilation defines" {
                let undefined =
                    AssemblySources.synthetic "Synth" "clr" Set.empty [ impl "guarded.fs" guardedSource ]

                let defined =
                    AssemblySources.synthetic "Synth" "clr" (Set.ofList [ "FOO" ]) [ impl "guarded.fs" guardedSource ]

                Expect.isEmpty (recoveryDiagnostics undefined) "the inactive branch is not parsed"

                Expect.isNonEmpty (recoveryDiagnostics defined) "the active branch is parsed, and recovery reports it"
            }

            test "ofManifest takes the assembly off the manifest it read" {
                let sources =
                    AssemblySources.ofManifest (srcManifest "clr" "Vesper.Core")
                    |> PackageFaults.okOrFail "Vesper.Core clr sources"

                Expect.equal sources.Assembly.Name (AssemblyName "Vesper.Core") "the manifest's package name"
                Expect.equal sources.Assembly.Target "clr" "the target it was resolved under"
                Expect.isNonEmpty (parsedNames sources) "the manifest's files came through"
            }

            test "ofManifest reads the same package under each target it publishes for" {
                let js =
                    AssemblySources.ofManifest (srcManifest "js" "Vesper.Core")
                    |> PackageFaults.okOrFail "Vesper.Core js sources"

                Expect.equal js.Assembly.Name (AssemblyName "Vesper.Core") "the manifest's package name"
                Expect.equal js.Assembly.Target "js" "the target it was resolved under"
            }
        ]
