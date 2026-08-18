module XParsec.FSharp.SemanticAnalysis.Tests.ConformanceTests

// Sig/impl conformance: the `.fsi` `extern` set coincides with the `.fs`
// `(# … #)` intrinsic set. A source-level check, so codegen plays no part.

open System.IO

open Expecto

open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private vesperPath (package: string) (fileName: string) =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", package, fileName)

let private vesperCorePath (fileName: string) = vesperPath "Vesper.Core" fileName

let private readNormalised (path: string) =
    (File.ReadAllText path).Replace("\r\n", "\n")

let private conform (sigSrc: string) (implSrc: string) : Conformance.ConformanceError list =
    let sigLexed, sigFile = parseSigFile sigSrc
    let implLexed, implFile = parseFile implSrc
    (Conformance.checkUnit sigLexed sigFile implLexed implFile).Errors

let private externNames (decls: Conformance.SigDecl list) =
    decls
    |> List.choose (fun d ->
        // Not `DemandsIntrinsic`: an `extern class` pairs with the TAGGED
        // `(# class … #)`, which `intrinsicNames` counts separately.
        if d.Shape.DemandsIntrinsic && not d.Shape.IsHeritable then
            Some d.Name
        else
            None
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
            // ---- The real contract/impl pair ----

            test "prim-types-min.fsi conforms to prim-types-min.clr.fs (no drift)" {
                let sigSrc = readNormalised (vesperCorePath "prim-types-min.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-min.clr.fs")
                let errors = conform sigSrc implSrc
                Expect.isEmpty errors "prim-types-min should conform with no errors"
            }

            test "prim-types-min: extern set equals the intrinsic set, primitives and anchors alike" {
                let sigSrc = readNormalised (vesperCorePath "prim-types-min.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-min.clr.fs")

                let sigLexed, sigFile = parseSigFile sigSrc
                let implLexed, implFile = parseFile implSrc

                let externs = externNames (Conformance.summariseSig sigLexed sigFile)

                let intrinsics = intrinsicNames (Conformance.summariseImpl implLexed implFile)

                Expect.equal externs intrinsics "extern set == intrinsic set"
                Expect.equal (Set.count externs) 6 "three primitives and three capability anchors are extern"
                Expect.isTrue (externs.Contains "int") "int is extern"
                Expect.isTrue (externs.Contains "bool") "bool is extern"
                Expect.isTrue (externs.Contains "unit") "unit is extern"
                Expect.isTrue (externs.Contains "equatable") "equatable is extern"
                Expect.isTrue (externs.Contains "comparable") "comparable is extern"
                Expect.isTrue (externs.Contains "disposable") "disposable is extern"
            }

            test "prim-types-array.fsi conforms to prim-types-array.fs (no drift)" {
                // The array is declared apart from the other primitives because it references the
                // `seq` capability, which must already be in scope. Its impl is target-neutral,
                // so one `.fs` serves both targets.
                let sigSrc = readNormalised (vesperCorePath "prim-types-array.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-array.fs")
                let errors = conform sigSrc implSrc
                Expect.isEmpty errors "prim-types-array should conform with no errors"

                let sigLexed, sigFile = parseSigFile sigSrc
                let implLexed, implFile = parseFile implSrc
                let externs = externNames (Conformance.summariseSig sigLexed sigFile)

                Expect.equal
                    externs
                    (intrinsicNames (Conformance.summariseImpl implLexed implFile))
                    "extern set == intrinsic set"

                Expect.isTrue (externs.Contains "[]") "the array type constructor is extern"
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

            test "concrete type declared in .fsi but absent from .fs → MissingInImpl" {
                // `bar` is a CONCRETE type (a union), so it requires an implementation and
                // its absence is real drift.
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern\n\ntype bar = | BarCase"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)"

                Expect.equal errors [ Conformance.ConformanceError.MissingInImpl "bar" ] "bar missing in impl"
            }

            test "plain type defined in .fs but absent from .fsi → no error (HiddenTycon)" {
                // F# hides an impl type the signature omits (a private impl detail like
                // Set's AVL-tree nodes); it is not drift, so the check stays silent.
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)\n\ntype baz = int"

                Expect.isEmpty errors "a plain impl-only type is a HiddenTycon, not drift"
            }

            test "intrinsic defined in .fs but absent from .fsi → IntrinsicWithoutExtern" {
                // An impl `(# … #)` repr with no `extern` IS reported — a primitive repr
                // the contract never declares (distinct from a plain HiddenTycon).
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)\n\ntype baz = (# \"System.Int64\" #)"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.IntrinsicWithoutExtern "baz" ]
                    "an impl-only intrinsic with no extern"
            }

            test "abbreviation declared in .fsi but absent from .fs → MissingInImpl" {
                // Transparency buys the abbreviation nothing: fsc requires the `.fs` to
                // restate it, so its absence is drift like any other.
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern\n\ntype myalias = int"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)"

                Expect.equal errors [ Conformance.ConformanceError.MissingInImpl "myalias" ] "myalias missing in impl"
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
                        // a: extern in sig, plain `int` impl → repr promised, none given.
                        Conformance.ConformanceError.ExternWithoutIntrinsic "a"
                        // b: extern in sig, no impl at all.
                        Conformance.ConformanceError.MissingInImpl "b"
                        // c: impl intrinsic with no extern in sig.
                        Conformance.ConformanceError.IntrinsicWithoutExtern "c"
                    ]
                    "all three drifts, sig-order then impl-only"
            }

            // ---- Value-binding presence ----

            test "val in .fsi with no let in .fs → ValueMissingInImpl" {
                let errors =
                    conform "namespace V\n\nval foo: int -> int" "namespace V\n\nlet bar (x: int) = x"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.ValueMissingInImpl "foo" ]
                    "a val with no matching let"
            }

            test "let in .fs with no val in .fsi → no error (HiddenVal)" {
                // F# silently allows an implementation value absent from the signature,
                // so a private helper is not drift — the converse is NOT reported.
                let errors =
                    conform
                        "namespace V\n\nval foo: int -> int"
                        "namespace V\n\nlet foo (x: int) = x\n\nlet helper (y: int) = y"

                Expect.isEmpty errors "an extra impl let is a HiddenVal, not drift"
            }

            test "matching val/let (incl operator) conform with no value errors" {
                let errors =
                    conform
                        "namespace V\n\nval foo: int -> int\n\nval inline (+++): int -> int -> int"
                        "namespace V\n\nlet foo (x: int) = x\n\nlet inline (+++) (a: int) (b: int) = a"

                Expect.isEmpty errors "a val paired with its let — plain and operator — conforms"
            }

            test "module-nested val with no let → ValueMissingInImpl (flattened)" {
                // Nested modules are flattened, so a `val` inside `module M` pairs with
                // a `let` inside `module M` on the impl side.
                let errors =
                    conform
                        "namespace V\n\nmodule M =\n\n    val gone: int -> int"
                        "namespace V\n\nmodule M =\n\n    let other (x: int) = x"

                Expect.equal
                    errors
                    [ Conformance.ConformanceError.ValueMissingInImpl "gone" ]
                    "a nested-module val with no matching let"
            }
        ]

// ---- Manifest-driven conformance over every package (CLR target) -------
// The `.fsi`↔`.fs` pairs are read from each `Vesper.*/manifest.clr.toml`; every
// discrepancy becomes a hard `Severity.Error`.

let private vesperSrcDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src")

/// Every `Vesper.*` package that builds for `target` → (dir name, its resolved manifest). A
/// package that publishes no `manifest.<target>.toml` does not build for it and is absent here.
let private packageManifests (target: string) : (string * ReferencedProject.ManifestPath) list =
    Directory.GetDirectories(vesperSrcDir, "Vesper.*")
    |> Array.choose (fun d ->
        match ReferencedProject.resolveManifest target d with
        | Ok mp -> Some(Path.GetFileName d, mp)
        | Error _ -> None
    )
    |> Array.sortBy fst
    |> List.ofArray

/// The manifest-driven pass's outcome; a manifest or parse error fails the test.
let private outcomeFor (mp: ReferencedProject.ManifestPath) : ConformancePass.PackageOutcome =
    match ConformancePass.checkManifest mp with
    | Ok o -> o
    | Error e ->
        failtestf "checkManifest failed for %s: %s" mp.Path e
        Unchecked.defaultof<_>

[<Tests>]
let packageConformanceTests =
    testList
        "PackageConformance"
        [
            for package, manifestPath in packageManifests "clr" do
                test $"{package}: manifest-driven conformance is enforced (no hard errors)" {
                    let outcome = outcomeFor manifestPath

                    let errors = ConformancePass.enforce outcome

                    Expect.isEmpty
                        errors
                        (sprintf
                            "%s: conformance must produce no hard errors; got:\n%s"
                            package
                            (errors |> List.map (fun d -> d.Message) |> String.concat "\n"))
                }
        ]

// ---- The SAME pass, run for JS --------------------------------------------
// A type JS has no representation for is ABSENT from the js manifest, so its contract is
// never a pair here; a companion-less signature is a hard error. A runtime-asset-served
// value is a PAIR whose `.fs` binding declares `[<Import>]`.

let private manifestOf (target: string) (package: string) : ReferencedProject.ManifestPath =
    packageManifests target
    |> List.tryFind (fun (p, _) -> p = package)
    |> Option.map snd
    |> Option.defaultWith (fun () -> failtestf "%s %s manifest not found" package target)

/// Materialise a throwaway package from `files` (file name → content, `manifest.js.toml`
/// among them) and run the js pass over it.
let private syntheticOutcome (files: (string * string) list) : ConformancePass.PackageOutcome =
    let dir =
        Path.Combine(Path.GetTempPath(), "vesper.synthetic." + System.Guid.NewGuid().ToString("N"))

    Directory.CreateDirectory dir |> ignore

    try
        for name, content in files do
            File.WriteAllText(Path.Combine(dir, name), content)

        match ReferencedProject.resolveManifest "js" dir with
        | Ok mp -> outcomeFor mp
        | Error e -> failtestf "resolveManifest: %s" (PackageSetFault.describe e)
    finally
        Directory.Delete(dir, true)

/// A one-contract package whose `.fs` binds `served` by `[<Import>]` against a runtime
/// asset exporting `exportedAs` — the export name is the only variable.
let private runtimeAssetOutcome (exportedAs: string) : ConformancePass.PackageOutcome =
    syntheticOutcome
        [
            "manifest.js.toml",
            "[core]\nfiles = [\"served.fsi\"]\nimpl = [\"served.js.fs\"]\nruntime = [\"Asset.mjs\"]\n"
            "served.fsi", "module V\n\nval served: x: int -> int\n"
            "served.js.fs",
            "module V\n\n[<Import(\"served\", \"./Asset.mjs\")>]\nlet served (x: int) : int = jsNative\n"
            "Asset.mjs", sprintf "export const %s = (x) => x;\n" exportedAs
        ]

/// `runtimeAssetOutcome` with the whole `.fs` under the caller's control.
let private importImplOutcome (implSource: string) : ConformancePass.PackageOutcome =
    syntheticOutcome
        [
            "manifest.js.toml",
            "[core]\nfiles = [\"served.fsi\"]\nimpl = [\"served.js.fs\"]\nruntime = [\"Asset.mjs\"]\n"
            "served.fsi", "module V\n\nval served: x: int -> int\n"
            "served.js.fs", implSource
            "Asset.mjs", "export const served = (x) => x;\n"
        ]

let private enforcedMessages (outcome: ConformancePass.PackageOutcome) : string list =
    ConformancePass.enforce outcome |> List.map (fun d -> d.Message)

[<Tests>]
let jsPackageConformanceTests =
    testList
        "PackageConformanceJs"
        [
            test "js: an all-extern, val-less signature with no body is a hard error, not accepted" {
                // The former `Unrepresentable` route: a type the target has no representation
                // for is OMITTED from the manifest, so a declared-and-unimplemented contract
                // owes a body like any other.
                let outcome =
                    syntheticOutcome
                        [
                            "manifest.js.toml", "[core]\nfiles = [\"widths.fsi\"]\n"
                            "widths.fsi", "namespace V\n\ntype myint = extern\n"
                        ]

                let errors = ConformancePass.enforce outcome |> List.map (fun d -> d.Message)

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.stringContains errors.Head "widths.fsi" "the FS0240-style error names the signature file"
            }

            test "js: a contract whose declarations need a real body stays a hard error, not `unsupported`" {
                // A record needs a real `.fs`: absence is missing work, not a statement that
                // JS cannot represent it. The asset exports the type's NAME, and an asset
                // export still answers no companion-less signature.
                let outcome =
                    syntheticOutcome
                        [
                            "manifest.js.toml", "[core]\nfiles = [\"cell.fsi\"]\nruntime = [\"Asset.mjs\"]\n"
                            "cell.fsi", "namespace V\n\ntype Cell = { N: int }\n"
                            "Asset.mjs", "export const Cell = 1;\n"
                        ]

                let errors = ConformancePass.enforce outcome |> List.map (fun d -> d.Message)

                Expect.equal (List.length errors) 1 "one hard error"

                Expect.stringContains
                    errors.Head
                    "cell.fsi"
                    "the FS0240-style error names the signature file owing a body"
            }

            test "js: the hard-error set is exactly the un-ported library surface" {
                // EMPTY: every in-scope contract has a JS body, is unrepresentable, or is
                // declared impl-free. Pinned as a list, so a new entry identifies the contract
                // that lost its body.
                let expected: string list = []

                let actual =
                    [
                        for _, manifestPath in packageManifests "js" do
                            for d in ConformancePass.enforce (outcomeFor manifestPath) -> d.Message
                    ]

                Expect.equal actual expected "the js hard-error set"
            }

            test "js: the runtime contracts PAIR, their bindings `[<Import>]`-served by the committed asset" {
                // The bodies live in the committed `.mjs`; the `.fs` declares that per
                // binding, so the pair conforms like any other.
                let pairedWith (package: string) (sigFile: string) (implFile: string) =
                    let paired =
                        [
                            for p in (outcomeFor (manifestOf "js" package)).Pairs do
                                match p with
                                | ConformancePass.PairOutcome.Paired r when r.SigFile = sigFile -> yield r
                                | _ -> ()
                        ]

                    match paired with
                    | [ r ] ->
                        Expect.equal r.ImplFile implFile (sprintf "%s pairs with its `[<Import>]` body" sigFile)
                        Expect.isEmpty r.Errors (sprintf "%s: every import is declared and exported" sigFile)
                    | _ -> failtestf "%s must pair with %s" sigFile implFile

                pairedWith "Vesper.Core" "ops-platform-runtime.js.fsi" "ops-platform-runtime.js.fs"
                pairedWith "Vesper.Comparison" "comparison-runtime.js.fsi" "comparison-runtime.js.fs"
            }

            test "js: rename the asset's export and the `[<Import>]` binding is a hard error" {
                // The whole difference between the two runs is one identifier in the `.mjs`:
                // the verdict is checked against the asset, not read off the attribute.
                Expect.isEmpty (ConformancePass.enforce (runtimeAssetOutcome "served")) "the declared export exists"

                let errors = enforcedMessages (runtimeAssetOutcome "servedRenamed")

                Expect.equal (List.length errors) 1 "the pair is a hard error"
                Expect.stringContains errors.Head "'served'" "naming the import whose export vanished"
                Expect.stringContains errors.Head "does not export" "as a missing export"
            }

            test "js: an `[<Import>]` body other than `jsNative` is a hard error" {
                // The attribute is the implementation; a real body beside it would be
                // silently discarded.
                let errors =
                    importImplOutcome
                        "module V\n\n[<Import(\"served\", \"./Asset.mjs\")>]\nlet served (x: int) : int = x\n"
                    |> enforcedMessages

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.stringContains errors.Head "jsNative" "demanding the jsNative body"
            }

            test "js: a `jsNative` body without `[<Import>]` is a hard error" {
                let errors =
                    importImplOutcome "module V\n\nlet served (x: int) : int = jsNative\n"
                    |> enforcedMessages

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.stringContains errors.Head "[<Import>]" "demanding the declaration that serves it"
            }

            test "js: an `[<Import>]` selector that is not the binding's name is a hard error" {
                // Two findings: the mismatch itself, and the declared selector is also
                // absent from the asset's exports.
                let errors =
                    importImplOutcome
                        "module V\n\n[<Import(\"other\", \"./Asset.mjs\")>]\nlet served (x: int) : int = jsNative\n"
                    |> enforcedMessages

                Expect.equal (List.length errors) 2 "the mismatch and the missing export"
                Expect.all errors (fun e -> e.Contains "'other'") "each names the mismatched selector"
            }

            test "js: an `[<Import>]` path outside `./` + the manifest's runtime list is a hard error" {
                // A bare specifier is ESM's npm resolution, a different feature, and is
                // refused; so is a `./` path the manifest's `runtime` key does not list.
                let bare =
                    importImplOutcome
                        "module V\n\n[<Import(\"served\", \"Asset.mjs\")>]\nlet served (x: int) : int = jsNative\n"
                    |> enforcedMessages

                Expect.equal (List.length bare) 1 "a bare specifier is refused"
                Expect.stringContains bare.Head "'Asset.mjs'" "naming the path"

                let unlisted =
                    importImplOutcome
                        "module V\n\n[<Import(\"served\", \"./Other.mjs\")>]\nlet served (x: int) : int = jsNative\n"
                    |> enforcedMessages

                Expect.equal (List.length unlisted) 1 "an unlisted asset is refused"
                Expect.stringContains unlisted.Head "'./Other.mjs'" "naming the path"
            }

            test "js: an `[<Import>]` whose arguments are not two strings is a hard error" {
                let errors =
                    importImplOutcome "module V\n\n[<Import(\"served\")>]\nlet served (x: int) : int = jsNative\n"
                    |> enforcedMessages

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.stringContains errors.Head "two non-empty string literals" "naming the malformed shape"
            }

            test "js: capabilities-compat.js.fsi PAIRS, transparent abbreviations and all" {
                // Every declaration in it is an abbreviation of a capability, and each is
                // restated by the body: transparency exempts none of them.
                let paired =
                    [
                        for p in (outcomeFor (manifestOf "js" "Vesper.Core")).Pairs do
                            match p with
                            | ConformancePass.PairOutcome.Paired r when r.SigFile = "capabilities-compat.js.fsi" ->
                                yield r
                            | _ -> ()
                    ]

                match paired with
                | [ r ] ->
                    Expect.equal r.ImplFile "capabilities-compat.js.fs" "paired with its body"
                    Expect.isEmpty r.Errors "the BCL spellings and the capabilities they abbreviate conform"
                | _ -> failtest "capabilities-compat.js.fsi must pair with capabilities-compat.js.fs"
            }

            test "js: prim-types-array.fsi PAIRS with its body rather than being waved through" {
                // The manifest lists one pair and it conforms: `extern` ↔ `(# "!0[]" #)`.
                // Neither half is waved through — the `.fsi` as owing no body, or the
                // `.fs` as contract-less.
                let paired =
                    [
                        for p in (outcomeFor (manifestOf "js" "Vesper.Core")).Pairs do
                            match p with
                            | ConformancePass.PairOutcome.Paired r when r.SigFile = "prim-types-array.fsi" -> yield r
                            | _ -> ()
                    ]

                match paired with
                | [ r ] ->
                    Expect.equal r.ImplFile "prim-types-array.fs" "paired with its body"
                    Expect.isEmpty r.Errors "the array's extern and its intrinsic repr conform"
                | _ -> failtest "prim-types-array.fsi must pair with prim-types-array.fs"
            }

            test "js: a body that answers no contract raises nothing" {
                // `structural-printer.js.fs` is a standalone `%A` engine whose published
                // surface IS its contract. It pairs with nothing, and owes nothing: only a
                // `.fsi` demands a companion.
                let outcome = outcomeFor (manifestOf "js" "Vesper.Printf")

                let paired =
                    [
                        for p in outcome.Pairs do
                            match p with
                            | ConformancePass.PairOutcome.Paired r -> yield r.ImplFile
                            | _ -> ()
                    ]

                Expect.isFalse (List.contains "structural-printer.js.fs" paired) "the engine answers no contract"
                Expect.isEmpty (ConformancePass.enforce outcome) "Vesper.Printf conforms on js"
            }

        ]

// ---- conformance findings are HARD errors ----------------------------
// `enforce` turns a finding into an FS0240-style error that fails the build. Pinned
// on a synthetic `PackageOutcome`, so no manifest round-trip is involved.

let private mkOutcome (pairs: ConformancePass.PairOutcome list) : ConformancePass.PackageOutcome =
    { Package = "Test"; Pairs = pairs }

[<Tests>]
let enforcementTests =
    testList
        "ConformanceEnforcement"
        [
            test "a SigOnly .fsi (a deleted impl) → hard FS0240-style error, unconditionally" {
                let outcome = mkOutcome [ ConformancePass.PairOutcome.SigOnly "deleted-impl.fsi" ]

                let errors = ConformancePass.enforce outcome

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.equal errors.Head.Severity Severity.Error "error severity"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V240") "the FS0240 family"
                Expect.stringContains errors.Head.Message "deleted-impl.fsi" "names the orphaned .fsi"
            }

            test "a MissingInImpl kernel finding on a paired contract → hard FS0240-style error" {
                let paired =
                    ConformancePass.PairOutcome.Paired
                        {
                            SigFile = "x.fsi"
                            ImplFile = "x.fs"
                            ModuleMismatch = ValueNone
                            Errors = [ Conformance.ConformanceError.MissingInImpl "bar" ]
                        }

                let errors = ConformancePass.enforce (mkOutcome [ paired ])

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.equal errors.Head.Severity Severity.Error "error severity"
                Expect.stringContains errors.Head.Message "bar" "names the missing type"
            }

            test "a parse failure is a per-contract V244 error, not an abort that masks the rest" {
                // A parse failure is collected as a `ParseFailed` verdict rather than
                // aborting the package, so a sibling contract's drift still surfaces.
                let outcome =
                    mkOutcome
                        [
                            ConformancePass.PairOutcome.ParseFailed("broken.fsi", "unexpected token")
                            ConformancePass.PairOutcome.SigOnly "deleted-impl.fsi"
                        ]

                let errors = ConformancePass.enforce outcome

                Expect.equal (List.length errors) 2 "the parse failure does not mask the orphaned .fsi"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V244") "the parse-failure family"
                Expect.stringContains errors.Head.Message "broken.fsi" "names the unparseable signature file"
                Expect.equal errors.[1].Code (DiagCode.Vesper "V240") "the sibling drift still surfaces"
            }
        ]

// ---- Semantic typar-order conformance ---------------------------------
// The `.fs`-inferred scheme (`FTTypar(Method, i)`) vs the `.fsi`-declared one
// (`FTTypar(Declaring, i)`): positional, so `=` fails on a typar-ORDER difference.

/// A contract provider that publishes exactly `entries` (name → declared scheme) and
/// nothing else — the `.fsi` side of one `checkFile` run.
let private contractProvider (entries: (string * ExternalSymbol) list) : IExternalSymbolProvider =
    let m = Map.ofList entries

    ExternalSymbolProviders.ofNamedChannels
        { ExternalSymbolProviders.NamedChannels.empty with
            TryLookup =
                fun name ->
                    match Map.tryFind name m with
                    | Some s -> ValueSome s
                    | None -> ValueNone
        }

/// Run the `.fs` through the real frozen self-host pipeline, so a generic binding's
/// typar order is inference's own rather than a hand-built `FrozenType`.
let private frozenOf (src: string) : FrozenPools =
    let lexed, file = parseFile src
    Pipeline.analyseFor { Name = "M"; Target = "clr" } realProvider.Value (Hashing.originSourceOfText lexed) file

/// `val f: 'a -> 'b -> 'b` — the `.fsi` appearance-order scheme (`'a` = index 0).
let private fScheme: FrozenType =
    FTFun(FTTypar(TyparAxis.Declaring, 0), FTFun(FTTypar(TyparAxis.Declaring, 1), FTTypar(TyparAxis.Declaring, 1)))

[<Tests>]
let typarConformanceTests =
    testList
        "TyparConformance"
        [
            // ---- Kernel: axis-normalized structural equality ----

            test "schemesAgree: same order across Declaring/Method axes → agree" {
                let declared =
                    FTFun(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Declaring, 1))

                let inferred = FTFun(FTTypar(TyparAxis.Method, 0), FTTypar(TyparAxis.Method, 1))
                Expect.isTrue (ConformanceTypars.schemesAgree declared inferred) "axis differs, order agrees"
            }

            test "schemesAgree: swapped typar order → disagree" {
                let declared =
                    FTFun(FTTypar(TyparAxis.Declaring, 0), FTTypar(TyparAxis.Declaring, 1))

                let inferred = FTFun(FTTypar(TyparAxis.Method, 1), FTTypar(TyparAxis.Method, 0))
                Expect.isFalse (ConformanceTypars.schemesAgree declared inferred) "reversed order disagrees"
            }

            // ---- Driver over the real frozen pipeline ----

            test "declared `<'b,'a>` reorder vs `.fsi` appearance order → TyparMismatch" {
                // `.fs` declares `<'b,'a>`, so `'b` = Method 0, `'a` = Method 1 ⇒ the
                // inferred scheme is `'a -> 'b -> 'b` = `M1 -> M0 -> M0`, the REVERSE
                // positional skeleton of the `.fsi`'s `'a -> 'b -> 'b` = `D0 -> D1 -> D1`.
                let contract =
                    contractProvider [ "f", ExternalSymbols.scheme (SymbolKeyOps.inNamespace "") "f" fScheme 2 [] ]

                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "one typar-order mismatch"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }

            test "appearance-order impl conforms to `.fsi` appearance order → no mismatch" {
                // No explicit `<…>`: the canonical order IS appearance order, matching the
                // `.fsi`. The very same binding+contract that fails above now conforms.
                let contract =
                    contractProvider [ "f", ExternalSymbols.scheme (SymbolKeyOps.inNamespace "") "f" fScheme 2 [] ]

                let tast = frozenOf "let f (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                Expect.isEmpty (ConformanceTypars.checkFile contract tast) "appearance-order impl conforms"
            }

            test "a binding the contract does not publish is skipped (presence is a separate check)" {
                // An empty contract: a private/unpublished binding has no declared scheme to
                // compare — typar-order is not the presence check's job.
                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty (ConformanceTypars.checkFile (contractProvider []) tast) "unpublished binding skipped"
            }

            // ---- `inline`: the declared typars ARE the splice's substitution slots ----

            test "an inline binding's typar order is checked: `<'b,'a>` reorder → TyparMismatch" {
                let contract =
                    contractProvider [ "f", ExternalSymbols.scheme (SymbolKeyOps.inNamespace "") "f" fScheme 2 [] ]

                let tast = frozenOf "let inline f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "one typar-order mismatch"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }

            test "an inline binding in appearance order conforms → no mismatch" {
                let contract =
                    contractProvider [ "f", ExternalSymbols.scheme (SymbolKeyOps.inNamespace "") "f" fScheme 2 [] ]

                let tast = frozenOf "let inline f (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                Expect.isEmpty (ConformanceTypars.checkFile contract tast) "appearance-order inline impl conforms"
            }

            test "an inline body folding two declared typars into one → TyparMismatch" {
                // The miscompile species: the contract's `'a -> 'b -> 'a` is TWO substitution
                // slots, and a homogeneous body offers one, so a call site would bind the
                // second argument at the first's type. Nothing else catches this.
                let contract =
                    contractProvider
                        [
                            "f",
                            ExternalSymbols.scheme
                                (SymbolKeyOps.inNamespace "")
                                "f"
                                (FTFun(
                                    FTTypar(TyparAxis.Declaring, 0),
                                    FTFun(FTTypar(TyparAxis.Declaring, 1), FTTypar(TyparAxis.Declaring, 0))
                                ))
                                2
                                []
                        ]

                let tast = frozenOf "let inline f (x: 'a) (y: 'a) : 'a = x"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "the folded body disagrees with the contract"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }
        ]

// ---- Semantic typar-order conformance for type MEMBERS ----------------------
// A generic `.fs` member (`member M<'a,'b>(x,y) = …`) vs the published overload set.
// Both sides write `FTTypar(Declaring,_)`/`FTTypar(Method,_)`, so `=` needs no collapse.

let private mAxis (i: int) : FrozenType = FTTypar(TyparAxis.Method, i)

/// The `.fsi`-published overload the stub serves: an instance method with
/// `methodTyparArity` own typars over already method-axised `parameters` / `ret`.
let private mkMember
    (name: string)
    (methodTyparArity: int)
    (parameters: FrozenType)
    (ret: FrozenType)
    : ExternalMember =
    { ExternalMember.OfKey(
          SymbolKeyOps.memberKeyOf (SymbolKeyOps.qualifiedTypeKeyOf "C" 0) name EqArray.empty 0 MemberKind.Method
      ) with
        Signature = mkSignature 0 methodTyparArity parameters ret
        MethodTyparArity = methodTyparArity
    }

/// A contract provider publishing exactly `overloads`, keyed by member name only:
/// the stub serves whatever qualified name the `.fs` type resolves under.
let private memberContractProvider (overloads: ExternalMember list) : IExternalSymbolProvider =
    ExternalSymbolProviders.ofNamedChannels
        { ExternalSymbolProviders.NamedChannels.empty with
            TryLookupMembers = fun q -> overloads |> List.filter (fun m -> m.Name = q.Name) |> EqArray.ofList
        }

[<Tests>]
let memberTyparConformanceTests =
    testList
        "MemberTyparConformance"
        [
            test "generic member conforming to its published overload → no mismatch" {
                // `member this.M<'a>(x: 'a) = x` — one method typar, signature `'a -> 'a`
                // (`M0 -> M0`). The published overload says the same, so it conforms.
                let tast = frozenOf "type C() =\n    member this.M<'a>(x: 'a) = x"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let contract = memberContractProvider [ mkMember "M" 1 (mAxis 0) (mAxis 0) ]
                Expect.isEmpty (ConformanceTypars.checkMembers contract tast) "identity generic member conforms"
            }

            test "published `<'b,'a>` reorder vs `.fs` `<'a,'b>` → MemberMismatch" {
                // `.fs` declares `<'a,'b>`: `x:'a` = Method 0, `y:'b` = Method 1, so the
                // inferred signature is `(M0 * M1) -> M0`. The published overload is the
                // REVERSED `<'b,'a>` numbering, `(M1 * M0) -> M1`.
                let tast = frozenOf "type C() =\n    member this.M<'a,'b>(x: 'a, y: 'b) = x"
                Expect.isEmpty tast.Residue.Diagnostics "no diagnostics"

                let swapped =
                    mkMember "M" 2 (FTTuple(EqArray.ofList [ mAxis 1; mAxis 0 ])) (mAxis 1)

                let mismatches =
                    ConformanceTypars.checkMembers (memberContractProvider [ swapped ]) tast

                Expect.equal (List.length mismatches) 1 "one member typar-order mismatch"
                Expect.equal mismatches.Head.MemberName "M" "the mismatch names M"
                Expect.equal mismatches.Head.MethodTyparArity 2 "carries the method arity"
            }

            test "a member the contract does not publish is skipped (presence is a separate check)" {
                // No published overload of matching arity → no typar-order verdict to make.
                let tast = frozenOf "type C() =\n    member this.M<'a,'b>(x: 'a, y: 'b) = x"

                Expect.isEmpty
                    (ConformanceTypars.checkMembers (memberContractProvider []) tast)
                    "unpublished member skipped"
            }
        ]

// ---- The units a package COMPILES ------------------------------------------
// Pairing is what carries the CST and typar conformance checks INTO the assembly pipeline:
// a unit list built off `impl` alone analyses every body as though it published its own
// surface, and reaches neither check.

/// Each unit as `(signature, implementation)` relative paths, the signature `""` when the
/// body carries none.
let private unitPaths
    (units: Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list)
    : (string * string) list =
    [
        for u in units do
            match u with
            | Ok u ->
                yield
                    (match u.Signature with
                     | ValueSome s -> s.Id.Name
                     | ValueNone -> ""),
                    u.Implementation.Id.Name
            | Error e -> failtestf "the package read did not deliver %s" e.Id.Name
    ]

let private unitsOf
    (mp: ReferencedProject.ManifestPath)
    : Result<AssemblyFiles.ParsedUnit, AssemblyFiles.UnparsedFile> list =
    match PackageUnits.ofManifest mp with
    | Ok units -> units
    | Error e ->
        failtestf "PackageUnits.ofManifest failed for %s: %s" mp.Path e
        []

[<Tests>]
let packageUnitsTests =
    testList
        "PackageUnits"
        [
            for target in [ "clr"; "js" ] do
                for package, manifestPath in packageManifests target do
                    test $"{package} ({target}): every body compiles under the contract it answers" {
                        let manifest =
                            match ReferencedProject.loadManifest manifestPath with
                            | Ok m -> m
                            | Error e -> failtestf "%s: %s" package e

                        let units = unitsOf manifestPath

                        Expect.equal
                            (unitPaths units |> List.map snd)
                            manifest.Impl
                            "one unit per `impl` entry, in manifest order"

                        let paired = unitPaths units |> List.filter (fst >> (<>) "") |> List.sort

                        let expected =
                            [
                                for p in (outcomeFor manifestPath).Pairs do
                                    match p with
                                    | ConformancePass.PairOutcome.Paired r -> yield r.SigFile, r.ImplFile
                                    | ConformancePass.PairOutcome.SigOnly _
                                    | ConformancePass.PairOutcome.ParseFailed _ -> ()
                            ]
                            |> List.sort

                        Expect.equal paired expected "the conformance pass's pairing, and no other"
                    }

            // The loop above passes vacuously on a package whose bodies pair with nothing, so
            // one real corpus pair is named here: `.fsi` first, at the `.fs`'s own position.
            test "Vesper.Core (clr): prim-types-min is compiled as one two-halved unit" {
                let paths = unitPaths (unitsOf (manifestOf "clr" "Vesper.Core"))

                Expect.equal
                    (List.head paths)
                    ("prim-types-min.fsi", "prim-types-min.clr.fs")
                    "the first body of the package carries its contract"

                Expect.isEmpty (paths |> List.filter (fun (s, _) -> s = "")) "every Vesper.Core body answers a contract"
            }
        ]
