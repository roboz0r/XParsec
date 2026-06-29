module XParsec.FSharp.SemanticAnalysis.Tests.ConformanceTests

// Sig/impl conformance. Proves the Vesper.Core contract
// (`.fsi`) `extern` capability set coincides with the implementation (`.fs`)
// `(# … #)` intrinsic representation set, and exercises each drift the check
// catches. A source-level check, so it is not gated on the self-hosting rungs.

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Path to a `src/<package>/<fileName>` source file (mirrors
/// VesperCoreContractTests' resolution from the test project root).
let private vesperPath (package: string) (fileName: string) =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", package, fileName)

let private vesperCorePath (fileName: string) = vesperPath "Vesper.Core" fileName

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

            test "extern class ↔ (# class repr #) conforms (heritable external base)" {
                let errors =
                    conform
                        "namespace V\n\ntype Attribute = extern class"
                        "namespace V\n\ntype Attribute = (# class \"System.Attribute\" #)"

                Expect.isEmpty errors "a heritable extern class paired with its tagged intrinsic conforms"
            }

            test "extern class with an untagged (# repr #) impl → HeritabilityMismatch" {
                let errors =
                    conform "namespace V\n\ntype foo = extern class" "namespace V\n\ntype foo = (# \"System.Foo\" #)"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.HeritabilityMismatch "foo" ]
                    "sig is heritable, impl is an opaque value repr"
            }

            test "bare extern with a (# class repr #) impl → HeritabilityMismatch" {
                let errors =
                    conform "namespace V\n\ntype foo = extern" "namespace V\n\ntype foo = (# class \"System.Foo\" #)"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.HeritabilityMismatch "foo" ]
                    "sig is opaque, impl is a heritable external base"
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

// ---- Conformance over every package -------
//
// The conformance check is codegen-independent (CST-level, not rung-gated), so it
// runs on all the Vesper.* packages — Set included — and is the cheapest way to
// catch `.fsi`/`.fs` drift the parser alone can't see. Each row conforms one
// `.fsi` against its same-base `.fs` companion; the two tables together *are* the
// per-package drift map. Signature-only `.fsi` files with no `.fs` companion
// (`compiler-attributes.fsi`, `ops-std.fsi`) are omitted — there is nothing to
// conform against. A green row = the contract and its implementation declare the
// same types with matching extern↔intrinsic pairing; a known-drift row asserts an
// *expected* divergence with the exact `ConformanceError`s pinned (`Expect.equal`,
// not skipped), so the divergence is locked in rather than silently tolerated and
// goes red the moment the contract or impl shape changes.
//
// Signature-only `.fsi` files with NO `.fs` impl at all are NOT omitted silently:
// the legitimate impl-free contracts are catalogued in `implFreeExemptions` below
// (front-end-intrinsic / per-target / FSharp.Core-interop / type-abbreviation),
// each tested to actually lack a companion `.fs` so the list cannot rot. The
// operator `.fsi` (`ops-std.fsi`, `int-comparison.fsi`) declare no `type`s, so they
// are trivially-conforming (empty type sets) and need no row.

/// Conform one `.fsi`/`.fs` pair from a package directory.
let private conformPair (package: string) (sigName: string) (implName: string) =
    let sigSrc = readNormalised (vesperPath package sigName)
    let implSrc = readNormalised (vesperPath package implName)
    conform sigSrc implSrc

/// (`.fsi`, `.fs`) pairs that conform with zero drift — the green rows. One per
/// package base file; signature-only `.fsi` (no `.fs`) omitted.
let private conformingPairs: (string * (string * string) list) list =
    [
        "Vesper.Core",
        [
            "prim-types-min.fsi", "prim-types-min.fs"
            "prim-types-int.fsi", "prim-types-int.fs"
            "prim-types-float.fsi", "prim-types-float.fs"
            "prim-types-string.fsi", "prim-types-string.fs"
            "prim-types-object.fsi", "prim-types-object.fs"
            "prim-types-exn.fsi", "prim-types-exn.fs"
            "prim-types-decimal.fsi", "prim-types-decimal.fs"
            "prim-types-nativeint.fsi", "prim-types-nativeint.fs"
            "prim-types-nd-array.fsi", "prim-types-nd-array.fs"
            "prim-types-attr.fsi", "prim-types-attr.fs"
            // The compiler-recognised equality/comparison attributes now have a
            // paired impl (`compiler-attributes.fs`: sealed classes inheriting
            // `Attribute`), so this is a real conformance row, no longer omitted.
            "compiler-attributes.fsi", "compiler-attributes.fs"
            "ops-platform.fsi", "ops-platform.fs"
        ]
        "Vesper.Option", [ "option.fsi", "option.fs" ]
        "Vesper.Result", [ "result.fsi", "result.fs" ]
        "Vesper.Choice", [ "choice.fsi", "choice.fs" ]
        "Vesper.Comparison", [ "comparison.fsi", "comparison.fs" ]
        "Vesper.Array", [ "array.fsi", "array.fs" ]
        "Vesper.Seq", [ "seq.fsi", "seq.fs" ]
    ]

/// (`.fsi`, `.fs`) pairs with a known, *expected* divergence — the contract and
/// impl deliberately declare different type sets, so the strict 1:1 conformance
/// check reports it. Each row pins the exact `ConformanceError`s; if the
/// divergence ever changes, the row goes red and must be re-justified. These are
/// abbreviation aliases (a lowercase `.fsi` alias / BCL alias with no companion
/// impl type) and private implementation types (not surfaced in the contract) —
/// not sig/impl mistakes.
let private knownDriftPairs: (string * string * string * string * Conformance.ConformanceError list) list =
    [
        // `ref` is a signature-only lowercase abbreviation alias for `Ref<'T>`
        // (core-types.fsi:19, "Same backing record as Ref<'T>"); the impl defines
        // only the `Ref<'T>` record it aliases, so the alias has no companion type.
        "Vesper.Core",
        "core-types.fsi",
        "core-types.fs",
        "ref is a sig-only alias of Ref<'T>",
        [ Conformance.ConformanceError.MissingInImpl "ref" ]

        // `ResizeArray<'T>` is an abbreviation of the BCL type
        // (`System.Collections.Generic.List<'T>`, list.fsi:126); it aliases the runtime
        // BCL type directly, so there is no companion type in `list.fs` (which defines
        // only the cons-list `List<'T>`). (`seq<'T>` was also here, but moved to
        // `Vesper.Core/capabilities.fsi` to break the enumerable-capability resolution
        // circularity — so it no longer drifts against `list.fs`.)
        //
        // `ListEnumerator` is the private `[<Struct>]` cursor `List<'T>`'s
        // `IEnumerable<'T>` impl walks — deliberately not in the public `list.fsi`
        // contract. (The old `ListSeq` wrapper class is retired now that `List<'T>`
        // implements `IEnumerable<'T>` directly — the cons-list IS a `seq<'T>`.)
        "Vesper.List",
        "list.fsi",
        "list.fs",
        "ResizeArray is a BCL abbreviation; ListEnumerator is the private enumerator cursor",
        [
            Conformance.ConformanceError.MissingInImpl "ResizeArray"
            Conformance.ConformanceError.MissingInSig "ListEnumerator"
        ]

        // `SetTree` / `SetTreeNode` / `SetIterator` are the private AVL-tree
        // implementation types in `set.fs`, deliberately not surfaced in the public
        // `set.fsi` contract (which exposes only `Set<'T>` and the `Set` module).
        "Vesper.Set",
        "set.fsi",
        "set.fs",
        "SetTree/SetTreeNode/SetIterator are private impl types",
        [
            Conformance.ConformanceError.MissingInSig "SetTree"
            Conformance.ConformanceError.MissingInSig "SetTreeNode"
            Conformance.ConformanceError.MissingInSig "SetIterator"
        ]

        // `Doc` / `FrameKind` / `Frame` are the private layout internals of the
        // `%A` engine (the Wadler document tree + frame stack). The new
        // `structural-printer.fsi` contract deliberately encapsulates them, publishing
        // only the cross-package surface (`RuntimeFormatState : IFormatSink` +
        // `StructuralPrinter.Print`) — the same private-impl-type drift as Set's tree.
        "Vesper.Printf",
        "structural-printer.fsi",
        "structural-printer.fs",
        "Doc/FrameKind/Frame are private %A layout internals",
        [
            Conformance.ConformanceError.MissingInSig "Doc"
            Conformance.ConformanceError.MissingInSig "FrameKind"
            Conformance.ConformanceError.MissingInSig "Frame"
        ]
    ]

/// Legitimately impl-free contract `.fsi` files: a signature with NO `.fs`
/// implementation anywhere, for a recorded reason. This is the explicit, tested
/// successor to silently omitting them — Step 5 of the T8 plan flips
/// `MissingInImpl` to a hard error, and these are the rows that must NOT trip it.
/// Each entry is `(package, sigName, category, why)`; the test asserts the `.fsi`
/// exists and the same-stem `.fs` does NOT, so adding an impl forces a move to
/// `conformingPairs` (the list cannot silently rot).
let private implFreeExemptions: (string * string * string * string) list =
    [
        // Front-end intrinsic: printf/printfn/sprintf are lowered inline to
        // `Formatter`/`Format` on the happy path (like the operators), so there is
        // no `.fs` body to emit.
        "Vesper.Printf", "printf.fsi", "front-end-intrinsic", "printf family lowered inline to Formatter/Format"

        // FSharp.Core-interop: the cold path instantiates `PrintfFormat` as
        // FSharp.Core's `PrintfFormat`4` (ClrRecipes); self-hosting it + retargeting
        // the recipe is a sequenced dependency on the vesper-printf cold path.
        "Vesper.Printf",
        "printf-format.fsi",
        "fsharp-core-interop",
        "PrintfFormat cold path still FSharp.Core; retarget pending"

        // Per-target: impl-free on CLR (the exception mechanism is BCL-resolved);
        // the JS representation comes via `prim-types-exn`, not a base `.fs`.
        "Vesper.Exceptions", "exceptions.fsi", "per-target", "BCL-resolved on CLR; JS repr via prim-types-exn"

        // Pure type abbreviations: the JS-only capability-compat shim maps BCL
        // interface spellings onto the canonical capabilities — abbrevs, no bodies.
        "Vesper.Core",
        "capabilities-compat.js.fsi",
        "type-abbreviation",
        "JS capability spelling shim; abbreviations only"
    ]

[<Tests>]
let packageConformanceTests =
    testList
        "PackageConformance"
        [
            for package, pairs in conformingPairs do
                for sigName, implName in pairs do
                    test $"{package}: {sigName} conforms to {implName}" {
                        let errors = conformPair package sigName implName

                        Expect.isEmpty errors (sprintf "%s/%s should conform to %s: %A" package sigName implName errors)
                    }

            for package, sigName, implName, why, expected in knownDriftPairs do
                test $"{package}: {sigName} vs {implName} — known drift ({why})" {
                    let errors = conformPair package sigName implName

                    Expect.equal
                        errors
                        expected
                        (sprintf
                            "%s/%s known drift against %s should be exactly the pinned set"
                            package
                            sigName
                            implName)
                }
        ]

[<Tests>]
let implFreeExemptionTests =
    testList
        "ImplFreeExemptions"
        [
            for package, sigName, category, why in implFreeExemptions do
                test $"{package}: {sigName} is impl-free ({category}: {why})" {
                    let sigPath = vesperPath package sigName
                    // Companion impl by the stem rule: foo.fsi ↔ foo.fs.
                    let implName = sigName.Substring(0, sigName.Length - 4) + ".fs"
                    let implPath = vesperPath package implName

                    Expect.isTrue (File.Exists sigPath) (sprintf "%s/%s should exist" package sigName)

                    Expect.isFalse
                        (File.Exists implPath)
                        (sprintf
                            "%s/%s is catalogued impl-free (%s) but a companion %s now exists — move it to conformingPairs"
                            package
                            sigName
                            category
                            implName)
                }
        ]
