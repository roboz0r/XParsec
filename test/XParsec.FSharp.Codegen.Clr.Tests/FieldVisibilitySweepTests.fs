module XParsec.FSharp.Codegen.Clr.Tests.FieldVisibilitySweepTests

open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Common.Tests.Conformance
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Field- and method-attribute invariants of the backend, checked over the whole CLR conformance
// corpus and pinned per shadowing shape.

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
                // `$` marks a backend-minted field name: a splice temporary (`value$8`) or a
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

            // No corpus program shadows a top-level binding, so the minted member is pinned
            // here, per `FieldWrites` kind, home class and member kind.
            test "a shadowed top-level binding's minted field is assembly-visible" {
                let src =
                    """
let x = 1
let x = 2
printfn "%d" x
"""

                let bytes = Codegen.toBytes (compileSource "MintedVisibility" src)

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "Program")
                    [
                        "x$0", FieldAttributes.Assembly ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                        "x", FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                    ]
                    "the rebinding takes the source identity and is public; the shadowed one is minted"

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "2" "the rebinding is what the reference reads"
            }

            test "a minted field written by Main is assembly-visible" {
                // A binding after a top-level `do` is filled by `Main`, so its field is
                // writable rather than `initonly`; shadowing still mints the first.
                let src =
                    """
printfn "start"
let z = 1
let z = 2
printfn "%d" z
"""

                let bytes = Codegen.toBytes (compileSource "MintedVisibilityMain" src)

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "Program")
                    [
                        "z$0", FieldAttributes.Assembly ||| FieldAttributes.Static
                        "z", FieldAttributes.Public ||| FieldAttributes.Static
                    ]
                    "a Main-written minted field is assembly-visible on the same terms"

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "start\n2" "the program still runs in source order"
            }

            test "a shadowed named-module binding's minted field is on the module class" {
                let src =
                    """
module M =
    let x = 1
    let x = 2
printfn "%d" M.x
"""

                let bytes = Codegen.toBytes (compileSource "MintedVisibilityModule" src)

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "M")
                    [
                        "x$0", FieldAttributes.Assembly ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                        "x", FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                    ]
                    "both fields sit on the declaring module's class"

                Expect.equal (MetadataStructure.fieldAttrsOf bytes "Program") [] "the Program class has no field"

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "2" "the rebinding is what the reference reads"
            }

            test "an expression-local let lowered out of the entry chain is minted on Program" {
                // Written inside `M`'s statement chain, but every top-level statement runs in
                // `Main`, so the peeled `let` has no module identity and stores on Program.
                let src =
                    """
module M =
    let x = 1
    (let t = List.length [ 1; 2 ] in printfn "%d" t)
printfn "end"
"""

                let bytes = Codegen.toBytes (compileSource "MintedEntryChain" src)

                Expect.equal
                    (MetadataStructure.fieldAttrsOf bytes "M")
                    [
                        "x", FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.InitOnly
                    ]
                    "the module class holds only its declared value"

                match MetadataStructure.fieldAttrsOf bytes "Program" with
                | [ name, attrs ] ->
                    Expect.stringStarts name "t$" "the peeled let is minted"

                    Expect.equal
                        attrs
                        (FieldAttributes.Assembly ||| FieldAttributes.Static ||| FieldAttributes.InitOnly)
                        "assembly-visible, initialised in the Program .cctor"
                | other -> failtestf "unexpected Program fields: %A" other

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "2\nend" "the program runs in source order"
            }

            test "a shadowed named-module function's minted method is assembly-visible" {
                let src =
                    """
module M =
    let f () = 1
    let f () = 2
printfn "%d" (M.f ())
"""

                let bytes = Codegen.toBytes (compileSource "MintedVisibilityFn" src)

                let visibilityOf (attrs: MethodAttributes) =
                    attrs &&& MethodAttributes.MemberAccessMask

                Expect.equal
                    (MetadataStructure.methodAttrsOf bytes "M"
                     |> List.map (fun (n, a) -> n, visibilityOf a))
                    [ "f$0", MethodAttributes.Assembly; "f", MethodAttributes.Public ]
                    "the rebinding is the public method; the shadowed one is minted on the same class"

                let _, output = runEntryPoint bytes
                Expect.equal (output.Replace("\r", "").Trim()) "2" "the rebinding is what the call reaches"
            }
        ]
