module XParsec.FSharp.Codegen.Clr.Tests.FieldVisibilitySweepTests

open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Field-attribute invariants of the backend, checked over the whole CLR conformance corpus.

/// One `Field` row of one corpus program.
type private CorpusField =
    {
        Program: string
        Type: string
        Field: string
        Attrs: FieldAttributes
    }

let private describe (f: CorpusField) : string =
    sprintf "%s: %s::%s (%O)" f.Program f.Type f.Field f.Attrs

/// Every CLR-compiled corpus program's field rows, compiled once and shared by both sweeps.
let private corpusFields =
    lazy
        [
            for p in compiledBy Target.Clr do
                let bytes =
                    Codegen.toBytes (compileSource (conformanceAssemblyName p.Name) p.Source)

                for (ty, fields) in MetadataStructure.allFieldAttrs bytes do
                    for (name, attrs) in fields do
                        yield
                            {
                                Program = p.Name
                                Type = ty
                                Field = name
                                Attrs = attrs
                            }
        ]

let private visibilityOf (attrs: FieldAttributes) : FieldAttributes =
    attrs &&& FieldAttributes.FieldAccessMask

[<Tests>]
let tests =
    testList
        "CLR field visibility sweep"
        [
            test "a minted name reaches no public field" {
                // `$` marks a backend-minted field name: splice residue (`value$8`) or a
                // shadowed top-level binding (`x$0`).
                let exposed =
                    corpusFields.Value
                    |> List.filter (fun f -> f.Field.Contains "$" && visibilityOf f.Attrs = FieldAttributes.Public)
                    |> List.map describe

                Expect.isEmpty exposed "a minted field name is assembly storage, never public"
            }

            test "a closure capture is private initonly" {
                let captures =
                    corpusFields.Value
                    |> List.filter (fun f -> f.Type.StartsWith "<closure>$" && f.Field.StartsWith "capture")

                Expect.isNonEmpty
                    captures
                    "the corpus emits closures with captures, so this sweep has something to check"

                let wrong =
                    captures
                    |> List.filter (fun f -> f.Attrs <> (FieldAttributes.Private ||| FieldAttributes.InitOnly))
                    |> List.map describe

                Expect.isEmpty
                    wrong
                    "a capture is written by its closure's .ctor and read by its Invoke, both on the closure"
            }

            // No corpus program shadows a top-level binding, so the residue mint is pinned
            // here, once per `FieldWrites` kind.
            test "a shadowed top-level binding's residue storage is assembly-visible" {
                let src =
                    """
let x = 1
let x = 2
printfn "%d" x
"""

                let bytes = Codegen.toBytes (compileSource "ResidueVisibility" src)

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "Program")
                    [
                        "x$0", FieldAttributes.Assembly ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                        "x", FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                    ]
                    "the rebinding takes the source identity and is public; the shadowed one is residue"

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "2" "the rebinding is what the reference reads"
            }

            test "residue storage a top-level do makes Main-written is assembly-visible" {
                // A binding after a top-level `do` is filled by `Main`, so its field is
                // writable rather than `initonly`; shadowing still makes the first residue.
                let src =
                    """
printfn "start"
let z = 1
let z = 2
printfn "%d" z
"""

                let bytes = Codegen.toBytes (compileSource "ResidueVisibilityMain" src)

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "Program")
                    [
                        "z$0", FieldAttributes.Assembly ||| FieldAttributes.Static
                        "z", FieldAttributes.Public ||| FieldAttributes.Static
                    ]
                    "a Main-written residue field is assembly-visible on the same terms"

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "start\n2" "the program still runs in source order"
            }
        ]
