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

// ---- Manifest-driven conformance over every package (T8 Step 3) -------
//
// The pairing is no longer a hand-maintained file list: `ConformancePass.checkManifest`
// reads each `Vesper.*/manifest.toml` and derives the `.fsi`↔`.fs` pairs from it
// (stem rule + the `resolveImpl ∪ resolveInlineBodies` impl set), so a newly-added
// `.fsi`/`.fs` is conformance-checked automatically and can no longer be silently
// dropped from a curated list. The packages themselves are discovered from the
// source tree for the same reason.
//
// Each package asserts THREE things against a small declared expectation:
//   1. The kernel conformance errors equal `acceptedFindings` for that package —
//      the abbreviation aliases (sig-only `MissingInImpl`) and private impl types
//      (`MissingInSig`) that are deliberate, not sig/impl mistakes. Anything else
//      goes red. (Step 5 flips an *un*-accepted `MissingInImpl` to a hard FS0240.)
//   2. The impl-free (`SigOnly`) `.fsi` contracts equal `implFreeExemptions` — the
//      legitimate signatures with no `.fs` on the CLR target (front-end-intrinsic /
//      per-target / FSharp.Core-interop). An impl appearing in the manifest moves a
//      `.fsi` out of `SigOnly`, forcing it out of this set.
//   3. No module-decl-guard violations and no contract-less `.fs` (`ImplOnly`) — a
//      mismatch here means the stem rule paired two unrelated files, always a bug.
//
// The conformance check is codegen-independent (CST-level, not rung-gated), so it
// runs on all the Vesper.* packages — Set included — and is the cheapest way to
// catch `.fsi`/`.fs` drift the parser alone can't see. The CLR pairing is `None`;
// JS-only contracts (`capabilities-compat.js.fsi`, appended via `files-js`) are not
// in the CLR file set, so they need no CLR exemption.

let private vesperSrcDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src")

/// Every `Vesper.*` package, discovered from the source tree → (dir name, manifest
/// path). Sorted for stable test ordering.
let private packageManifests: (string * string) list =
    Directory.GetDirectories(vesperSrcDir, "Vesper.*")
    |> Array.map (fun d -> Path.GetFileName d, Path.Combine(d, "manifest.toml"))
    |> Array.filter (fun (_, m) -> File.Exists m)
    |> Array.sortBy fst
    |> List.ofArray

/// Conformance findings each package is EXPECTED to emit — abbreviation aliases and
/// private implementation types, not sig/impl mistakes. Keyed by package dir name;
/// each entry is `(sigFile, error)`. A package with no entry must emit none.
/// Compared as a sorted multiset, so file/source order is irrelevant.
let private acceptedFindings: Map<string, (string * Conformance.ConformanceError) list> =
    Map.ofList
        [
            // `ref` is a signature-only lowercase abbreviation alias for `Ref<'T>`
            // (core-types.fsi, "Same backing record as Ref<'T>"); the impl defines only
            // the `Ref<'T>` record it aliases, so the alias has no companion type.
            // `seq` is the enumerable-capability abbreviation (`capabilities.fsi`, moved
            // there to break the enumerable-capability resolution circularity) — likewise
            // a sig-only abbreviation aliasing the capability interface, no companion type.
            "Vesper.Core",
            [
                "core-types.fsi", Conformance.ConformanceError.MissingInImpl "ref"
                "capabilities.fsi", Conformance.ConformanceError.MissingInImpl "seq"
            ]

            // `ResizeArray<'T>` is an abbreviation of the BCL `List<'T>` (list.fsi); it
            // aliases the runtime BCL type directly, so there is no companion type in
            // `list.fs` (which defines only the cons-list `List<'T>`). `ListEnumerator`
            // is the private `[<Struct>]` cursor `List<'T>`'s `IEnumerable<'T>` impl
            // walks — deliberately not in the public `list.fsi` contract.
            "Vesper.List",
            [
                "list.fsi", Conformance.ConformanceError.MissingInImpl "ResizeArray"
                "list.fsi", Conformance.ConformanceError.MissingInSig "ListEnumerator"
            ]

            // `SetTree` / `SetTreeNode` / `SetIterator` are the private AVL-tree impl
            // types in `set.fs`, deliberately not surfaced in the public `set.fsi`
            // contract (which exposes only `Set<'T>` and the `Set` module).
            "Vesper.Set",
            [
                "set.fsi", Conformance.ConformanceError.MissingInSig "SetTree"
                "set.fsi", Conformance.ConformanceError.MissingInSig "SetTreeNode"
                "set.fsi", Conformance.ConformanceError.MissingInSig "SetIterator"
            ]

            // `Doc` / `FrameKind` / `Frame` are the private layout internals of the
            // `%A` engine (the Wadler document tree + frame stack). The
            // `structural-printer.fsi` contract deliberately encapsulates them,
            // publishing only `RuntimeFormatState : IFormatSink` + `StructuralPrinter.Print`.
            "Vesper.Printf",
            [
                "structural-printer.fsi", Conformance.ConformanceError.MissingInSig "Doc"
                "structural-printer.fsi", Conformance.ConformanceError.MissingInSig "FrameKind"
                "structural-printer.fsi", Conformance.ConformanceError.MissingInSig "Frame"
            ]
        ]

/// Legitimately impl-free (`SigOnly`) contract `.fsi` files on the CLR target: a
/// signature the manifest pairs with no `.fs`, for a recorded reason. The explicit,
/// tested successor to silently omitting them — Step 5 flips an un-listed `SigOnly`
/// to a hard FS0240-style error. Keyed by package dir name → set of `.fsi` files.
let private implFreeExemptions: Map<string, Set<string>> =
    Map.ofList
        [
            "Vesper.Printf",
            Set.ofList
                [
                    // printf/printfn/sprintf are lowered inline to `Formatter`/`Format`
                    // on the happy path (like the operators), so there is no `.fs` body.
                    "printf.fsi"
                    // `PrintfFormat` cold path still instantiates FSharp.Core's
                    // `PrintfFormat`4`; self-hosting it + retargeting the recipe is a
                    // sequenced dependency on the vesper-printf cold path.
                    "printf-format.fsi"
                ]

            // Impl-free on CLR (the exception mechanism is BCL-resolved); the JS
            // representation comes via `prim-types-exn`, not a base `.fs`.
            "Vesper.Exceptions", Set.ofList [ "exceptions.fsi" ]
        ]

/// Run the manifest-driven pass for a package, failing the test on a manifest /
/// parse error (the pass returns `Error`).
let private outcomeFor (manifestPath: string) : ConformancePass.PackageOutcome =
    match ConformancePass.checkManifest None manifestPath with
    | Ok o -> o
    | Error e ->
        failtestf "checkManifest failed for %s: %s" manifestPath e
        Unchecked.defaultof<_>

[<Tests>]
let packageConformanceTests =
    testList
        "PackageConformance"
        [
            for package, manifestPath in packageManifests do
                test $"{package}: manifest-driven conformance" {
                    let outcome = outcomeFor manifestPath

                    // 1. Kernel findings equal the accepted multiset (sorted: order-free).
                    let actual = ConformancePass.pairErrors outcome |> List.sort

                    let expected =
                        acceptedFindings |> Map.tryFind package |> Option.defaultValue [] |> List.sort

                    Expect.equal
                        actual
                        expected
                        (sprintf "%s: conformance findings should be exactly the accepted set" package)

                    // 2. Impl-free `.fsi` equal the recorded exemptions for this package.
                    let actualSigOnly = ConformancePass.sigOnlyFiles outcome |> Set.ofList

                    let expectedSigOnly =
                        implFreeExemptions |> Map.tryFind package |> Option.defaultValue Set.empty

                    Expect.equal
                        actualSigOnly
                        expectedSigOnly
                        (sprintf "%s: impl-free .fsi should be exactly the recorded exemptions" package)

                    // 3. The stem rule must never pair unrelated files, and every
                    //    compiled `.fs` must have a contract.
                    Expect.isEmpty
                        (ConformancePass.moduleMismatches outcome)
                        (sprintf "%s: no module-decl-guard violations" package)

                    Expect.isEmpty outcome.ImplOnly (sprintf "%s: every compiled .fs has a .fsi contract" package)
                }
        ]
