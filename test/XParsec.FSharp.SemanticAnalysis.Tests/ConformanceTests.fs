module XParsec.FSharp.SemanticAnalysis.Tests.ConformanceTests

// selfhost-handoff P4 — sig/impl conformance. Proves the Vesper.Core contract
// (`.fsi`) `extern` capability set coincides with the implementation (`.fs`)
// `(# … #)` intrinsic representation set, and exercises each drift the check
// catches. A source-level check, so it is not gated on the self-hosting rungs.

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Path to a `src/Vesper.Core/<fileName>` source file (mirrors
/// VesperCoreContractTests' resolution from the test project root).
let private vesperCorePath (fileName: string) =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Core", fileName)

let private readNormalised (path: string) =
    (File.ReadAllText path).Replace("\r\n", "\n")

/// Run the conformance check over a `.fsi` / `.fs` source pair.
let private conform (sigSrc: string) (implSrc: string) : Conformance.ConformanceError list =
    let sigLexed, sigFile = parseSigFile sigSrc
    let implLexed, implFile = parseFile implSrc
    Conformance.checkPair sigLexed sigSrc sigFile implLexed implSrc implFile

let private externNames (decls: Conformance.SigDecl list) =
    decls
    |> List.choose (fun d ->
        match d.Shape with
        | Conformance.SigShape.Extern -> Some d.Name
        | _ -> None
    )
    |> Set.ofList

let private intrinsicNames (decls: Conformance.ImplDecl list) =
    decls
    |> List.choose (fun d ->
        match d.Shape with
        | Conformance.ImplShape.Intrinsic _ -> Some d.Name
        | _ -> None
    )
    |> Set.ofList

[<Tests>]
let tests =
    testList
        "Conformance"
        [
            // ---- The real contract/impl pair: the load-bearing P4 assertion ----

            test "prim-types-min.fsi conforms to prim-types-min.fs (no drift)" {
                let sigSrc = readNormalised (vesperCorePath "prim-types-min.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-min.fs")
                let errors = conform sigSrc implSrc
                Expect.isEmpty errors "prim-types-min should conform with no errors"
            }

            test "prim-types-min: extern capability set equals the intrinsic set" {
                let sigSrc = readNormalised (vesperCorePath "prim-types-min.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-min.fs")

                let sigLexed, sigFile = parseSigFile sigSrc
                let implLexed, implFile = parseFile implSrc

                let externs = externNames (Conformance.summariseSig sigLexed sigSrc sigFile)

                let intrinsics =
                    intrinsicNames (Conformance.summariseImpl implLexed implSrc implFile)

                Expect.equal externs intrinsics "extern set == intrinsic set"
                Expect.equal (Set.count externs) 4 "four primitives are extern"
                Expect.isTrue (externs.Contains "int") "int is extern"
                Expect.isTrue (externs.Contains "bool") "bool is extern"
                Expect.isTrue (externs.Contains "unit") "unit is extern"
                Expect.isTrue (externs.Contains "``[]``") "the array type constructor is extern"
            }

            // ---- Negative cases: each drift the check is meant to catch ----

            test "extern in .fsi with a non-intrinsic impl → ExternWithoutIntrinsic" {
                let errors =
                    conform "namespace V\n\ntype foo = extern" "namespace V\n\ntype foo = int"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.ExternWithoutIntrinsic "foo" ]
                    "extern without intrinsic"
            }

            test "intrinsic in .fs with a non-extern sig → IntrinsicWithoutExtern" {
                let errors =
                    conform "namespace V\n\ntype foo = int" "namespace V\n\ntype foo = (# \"System.Int32\" #)"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.IntrinsicWithoutExtern "foo" ]
                    "intrinsic without extern"
            }

            test "type declared in .fsi but absent from .fs → MissingInImpl" {
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern\n\ntype bar = int"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)"

                Expect.equal errors [ Conformance.ConformanceError.MissingInImpl "bar" ] "bar missing in impl"
            }

            test "type defined in .fs but absent from .fsi → MissingInSig" {
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)\n\ntype baz = int"

                Expect.equal errors [ Conformance.ConformanceError.MissingInSig "baz" ] "baz missing in sig"
            }

            test "a matching extern↔intrinsic + shared abbrev conform with no errors" {
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern\n\ntype myabbrev = int"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)\n\ntype myabbrev = int"

                Expect.isEmpty errors "an extern paired with its intrinsic, plus a matching abbrev, conforms"
            }

            test "multiple drifts surface together" {
                let errors =
                    conform
                        "namespace V\n\ntype a = extern\n\ntype b = extern"
                        "namespace V\n\ntype a = int\n\ntype c = (# \"X\" #)"

                Expect.equal
                    errors
                    [
                        Conformance.ConformanceError.ExternWithoutIntrinsic "a"
                        Conformance.ConformanceError.MissingInImpl "b"
                        Conformance.ConformanceError.MissingInSig "c"
                    ]
                    "all three drifts, sig-order then impl-only"
            }
        ]
