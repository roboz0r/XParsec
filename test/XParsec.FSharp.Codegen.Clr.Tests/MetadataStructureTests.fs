module XParsec.FSharp.Codegen.Clr.Tests.MetadataStructureTests

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

/// A program with one of everything the emitted `TypeDef` order groups by: a union, a
/// record, a class, a closure (`adder`'s lambda captures `k`), a module class holding a
/// NESTED type, the anonymous `Program` class's top-level value fields, and `Main`.
let private representative =
    String.concat
        "\n"
        [
            "type Shape ="
            "    | Dot"
            "    | Line of int"
            "type Point = { X: int; Y: int }"
            "type Counter(start: int) ="
            "    member c.Start = start"
            "    member c.Next() = c.Start + 1"
            "module M ="
            "    type Tally = { Hits: int }"
            "    let twice (x: int) = x + x"
            "    let adder (k: int) = fun (x: int) -> x + k"
            // `Tally` and its field labels reach the top level only through `open M`.
            "open M"
            "let bump = M.adder 3"
            "let p = { X = 1; Y = 2 }"
            "let c = Counter(4)"
            "let s = Line 5"
            "let t = { Hits = 7 }"
            "let n = M.twice (bump (p.X + p.Y + c.Next() + t.Hits))"
            "printfn \"%d\" n"
            "printfn \"%b\" (s = Line 5)"
        ]

let private representativeBytes: Lazy<byte[]> =
    lazy (compileSource "MetaStructRepresentative" representative |> Codegen.toBytes)

/// The same declarations as a LIBRARY (no `Main`, no `Program` slot at all).
let private libraryBytes: Lazy<byte[]> =
    lazy
        (let src =
            String.concat
                "\n"
                [
                    "namespace L"
                    ""
                    "type Shape ="
                    "    | Dot"
                    "    | Line of int"
                    ""
                    "type Point = { X: int; Y: int }"
                    ""
                    "type Counter(start: int) ="
                    "    member c.Start = start"
                ]

         compileSourceTo (ProjectInfo.library "MetaStructLibrary") src |> Codegen.toBytes)

// ---- The teeth: a hand-built, deliberately mis-ordered metadata image ---------
// The emitter cannot be made to emit a bad PE, so the corrupt image is built here with
// `MetadataBuilder` and fed to the same assertions.

/// `<Module>`, `N.A` (fields `a1`, `a2`) and `N.B` (field `b1`). The `FieldList` columns
/// always prefix-sum the TYPES, so handing `fieldRows` in another order gives a
/// counts-preserving mis-order; `firstField = 2` instead of 1 skews the prefix sum.
let private misorderedImage (fieldRows: string list) (firstField: int) : ImmutableArray<byte> =
    let mb = MetadataBuilder()

    mb.AddModule(0, mb.GetOrAddString "MisOrdered.dll", mb.GetOrAddGuid(Guid.NewGuid()), GuidHandle(), GuidHandle())
    |> ignore

    let fieldSig =
        let b = BlobBuilder()
        BlobEncoder(b).FieldSignature().Int32()
        mb.GetOrAddBlob b

    let methodSig =
        let b = BlobBuilder()

        BlobEncoder(b).MethodSignature().Parameters(0, (fun r -> r.Void()), (fun _ -> ()))

        mb.GetOrAddBlob b

    let typeRow (ns: string) (name: string) (field: int) (method: int) =
        mb.AddTypeDefinition(
            TypeAttributes.Public,
            mb.GetOrAddString ns,
            mb.GetOrAddString name,
            EntityHandle(),
            MetadataTokens.FieldDefinitionHandle field,
            MetadataTokens.MethodDefinitionHandle method
        )
        |> ignore

    // TypeDef rows first, since their FieldList/MethodList columns ARE the prefix sums.
    // `firstField` skews `<Module>` too: a mid-table skew is invisible, since a range's
    // END is the next row's start, so only a range not starting at 1 unclaims a row.
    typeRow "" "<Module>" firstField 1
    typeRow "N" "A" firstField 1
    typeRow "N" "B" (firstField + 2) 2

    // Then the row tables, in whatever order the caller asks for.
    for name in fieldRows do
        mb.AddFieldDefinition(FieldAttributes.Public, mb.GetOrAddString name, fieldSig)
        |> ignore

    for name in [ "aMethod"; "bMethod" ] do
        mb.AddMethodDefinition(
            MethodAttributes.Public ||| MethodAttributes.Static,
            MethodImplAttributes.IL,
            mb.GetOrAddString name,
            methodSig,
            -1,
            MetadataTokens.ParameterHandle 1
        )
        |> ignore

    let root = MetadataRootBuilder(mb, null, true)
    let output = BlobBuilder()
    root.Serialize(output, 0, 0)
    output.ToImmutableArray()

let private readImage (image: ImmutableArray<byte>) =
    MetadataReaderProvider.FromMetadataImage image

[<Tests>]
let tests =
    testList
        "MetadataStructure"
        [
            test "the representative exe's metadata is well-formed" {
                assertWellFormed "representative" representativeBytes.Value
            }

            test "the library's metadata is well-formed and does not carry an entry point" {
                assertWellFormed "library" libraryBytes.Value
                assertNoEntryPoint "library" libraryBytes.Value
            }

            test "the entry point lies inside the Program type's method range" {
                assertEntryPointOwner "representative" representativeBytes.Value "Program" "Main"
            }

            // The pin that makes the structural checks non-vacuous: the ACTUAL rows of
            // every type in a known assembly.
            test "each type's field and method ranges hold ITS OWN rows" {
                assertTypeMembers
                    "representative"
                    representativeBytes.Value
                    [
                        {
                            Type = "<Module>"
                            Fields = []
                            Methods = []
                        }
                        {
                            Type = "Shape"
                            Fields = [ "_tag"; "Line_0" ]
                            Methods = [ ".ctor"; "Dot"; "Line"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        {
                            Type = "Point"
                            Fields = [ "X"; "Y" ]
                            Methods = [ ".ctor"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        {
                            Type = "Counter"
                            Fields = [ "start" ]
                            Methods = [ ".ctor"; "get_Start"; "Next" ]
                        }
                        // The lambda's capture class, then the singleton closure
                        // (`instance` field + `.cctor`) `adder` itself reifies to.
                        {
                            Type = "<closure>$0"
                            Fields = [ "capture0" ]
                            Methods = [ ".ctor"; "Invoke" ]
                        }
                        {
                            Type = "<closure>$1"
                            Fields = [ "instance" ]
                            Methods = [ ".ctor"; "Invoke"; ".cctor" ]
                        }
                        // The module's compiled module class: static methods, no fields,
                        // and immediately followed (pre-order) by the type it holds.
                        {
                            Type = "M"
                            Fields = []
                            Methods = [ "twice"; "adder" ]
                        }
                        {
                            Type = "M+Tally"
                            Fields = [ "Hits" ]
                            Methods = [ ".ctor"; "GetHashCode"; "Equals"; "Equals"; "Format" ]
                        }
                        // The anonymous module class, last, so `Main` (the final method row)
                        // falls inside its range. A top-level value's field carries its
                        // SOURCE name; only a shadowed binding gets a slot-suffixed mint.
                        {
                            Type = "Program"
                            Fields = [ "p"; "c"; "s"; "t"; "n" ]
                            Methods = [ "Main" ]
                        }
                    ]
            }

            // Each Vesper package is a library full of modules, unions, records and closures.
            for package in
                [
                    "Vesper.Core"
                    "Vesper.List"
                    "Vesper.Comparison"
                    "Vesper.Printf"
                    "Vesper.Set"
                ] do
                test (sprintf "the emitted %s package is well-formed" package) {
                    match ((buildPackage package).Value |> snd).OutputPath with
                    | Some path -> assertWellFormedFile package path
                    | None -> failwithf "buildPackage %s produced no OutputPath" package
                }

            // ---- Teeth ----------------------------------------------------------

            test "a counts-preserving field mis-order is caught" {
                // The rows are written b1, a1, a2 while the FieldList columns still say
                // A claims the first two and B the third. Structurally flawless.
                let bad = misorderedImage [ "b1"; "a1"; "a2" ] 1
                use provider = readImage bad
                let md = provider.GetMetadataReader()

                assertWellFormedMetadata "mis-ordered" md

                let expected =
                    [
                        {
                            Type = "N.A"
                            Fields = [ "a1"; "a2" ]
                            Methods = [ "aMethod" ]
                        }
                        {
                            Type = "N.B"
                            Fields = [ "b1" ]
                            Methods = [ "bMethod" ]
                        }
                    ]

                Expect.throws
                    (fun () -> assertTypeMembersMetadata "mis-ordered" md expected)
                    "a permuted Field table must be caught by the member assertion"

                // …and the same image with the rows in the right order passes.
                let good = misorderedImage [ "a1"; "a2"; "b1" ] 1
                use goodProvider = readImage good
                assertTypeMembersMetadata "ordered" (goodProvider.GetMetadataReader()) expected
            }

            test "a skewed prefix sum leaves a Field row unclaimed and is caught" {
                // Every FieldList column is off by one, so field row 1 belongs to no
                // type and the ranges no longer cover the table.
                let bad = misorderedImage [ "a1"; "a2"; "b1" ] 2
                use provider = readImage bad

                Expect.throws
                    (fun () -> assertWellFormedMetadata "skewed" (provider.GetMetadataReader()))
                    "a Field row claimed by no TypeDef must break the range partition"
            }
        ]
