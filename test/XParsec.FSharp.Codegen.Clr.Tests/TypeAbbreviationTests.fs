module XParsec.FSharp.Codegen.Clr.Tests.TypeAbbreviationTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Common.Tests

// Every use site of a type abbreviation expands to the underlying type, so the emitted PE
// carries no TypeDef and no TypeRef for the name, while a program written through the name
// compiles, loads and runs.

let private isRow (alias: string) (row: string) =
    row = alias || row.EndsWith("." + alias) || row.StartsWith(alias + "`")

/// No TypeDef and no TypeRef in `bytes` is one of the aliases.
let private expectNoArtifactForAliases (bytes: byte[]) : unit =
    let defs = peTypeDefNames bytes
    let refs = peTypeRefNames bytes

    for alias in TypeAbbreviationFixtures.aliasNames do
        Expect.isFalse
            (defs |> List.exists (isRow alias))
            (sprintf "no TypeDef is emitted for the abbreviation `%s`; defs = %A" alias defs)

        Expect.isFalse
            (refs |> List.exists (isRow alias))
            (sprintf "no TypeRef binds to the abbreviation `%s`; refs = %A" alias refs)

let private runSource (asmName: string) (src: string) : string =
    let bytes = compileSource asmName src |> Codegen.toBytes
    expectNoArtifactForAliases bytes
    let exitCode, output = runEntryPoint bytes
    let actual = output.Replace("\r", "").Trim()
    Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
    actual

/// A multi-file assembly through the production driver, as `CrossFileTests` compiles one.
let private compileUnits (asmName: string) (units: SourceUnit list) : byte[] =
    let external = ClrSymbolProviders.buildContract defaultPackages
    let project = withCore (ProjectInfo.defaults asmName)

    match ClrDriver.compileWith [] external project (ClrDriver.sourcesFor project Set.empty units) with
    | Ok artifact -> Codegen.toBytes artifact
    | Error diags -> failtestf "cross-file compile failed:\n%s" (AnchoredDiagnostic.renderAll diags)

let private runUnits (asmName: string) (units: SourceUnit list) : string =
    let bytes = compileUnits asmName units
    expectNoArtifactForAliases bytes

    let refs = peAssemblyRefs bytes

    Expect.isFalse
        (refs |> List.contains asmName)
        (sprintf "the emitted PE must not reference its own assembly '%s'; refs = %A" asmName refs)

    let exitCode, output = runEntryPoint bytes
    let actual = output.Replace("\r", "").Trim()
    Expect.equal exitCode 0 (sprintf "expected exit 0; stdout was %A" actual)
    actual

let private declaringFile =
    TypeAbbreviationFixtures.declaringFile "namespace Abbrev" ""

[<Tests>]
let tests =
    testList
        "TypeAbbreviation (erased name, expanded use)"
        [
            for p in TypeAbbreviationFixtures.programs do
                test p.Description {
                    Expect.equal
                        (runSource p.Name p.Source)
                        p.Expected
                        "the program written through the abbreviations runs"
                }

            test "two files run: file 2 writes file 1's abbreviations qualified and bare (prints 7 / 25 / 7 / 30)" {
                let consumingFile =
                    "\
open Abbrev

let sum (p: Abbrev.PointAlias) : myint = p.X + p.Y

let area (s: ShapeAlias) : int =
    match s with
    | ShapeAlias.Circle r -> 3 * r * r
    | Abbrev.ShapeAlias.Square w -> w * w

let both (p: intpair) : int =
    let (a, b) = p
    a + b

let seven = CounterAlias(7)

let zero: CounterAlias = Abbrev.CounterAlias.Zero

printfn \"%d\" (sum { X = 3; Y = 4 })
printfn \"%d\" (area (ShapeAlias.Square 5))
printfn \"%d\" (seven.Value + zero.Value)
printfn \"%d\" (both (10, 20))
"

                let actual =
                    runUnits
                        "AbbrevCrossFile"
                        [
                            SourceUnit.ofImplementation (SourceFile.ofText "file1.fs" declaringFile)
                            SourceUnit.ofImplementation (SourceFile.ofText "file2.fs" consumingFile)
                        ]

                Expect.equal actual "7\n25\n7\n30" "every published abbreviation expands in file 2"
            }

            test "two files run: file 1's `.fsi` publishes the abbreviations file 2 writes (prints 7 / 30)" {
                let declaringSig =
                    "\
namespace Abbrev

type Point = { X: int; Y: int }

type PointAlias = Point

type Shape =
    | Circle of int
    | Square of int

type ShapeAlias = Shape

type Counter =
    new: start: int -> Counter
    member Value: int
    static member Zero: Counter

type CounterAlias = Counter

type myint = int

type intpair = int * int
"

                let consumingFile =
                    "\
open Abbrev

let sum (p: PointAlias) : myint = p.X + p.Y

let both (p: intpair) : int =
    let (a, b) = p
    a + b

printfn \"%d\" (sum { X = 3; Y = 4 })
printfn \"%d\" (both (10, 20))
"

                let actual =
                    runUnits
                        "AbbrevCrossFileSigned"
                        [
                            SourceUnit.paired
                                (SourceFile.ofText "file1.fsi" declaringSig)
                                (SourceFile.ofText "file1.fs" declaringFile)
                            SourceUnit.ofImplementation (SourceFile.ofText "file2.fs" consumingFile)
                        ]

                Expect.equal actual "7\n30" "the signature-published abbreviations expand in file 2"
            }
        ]
