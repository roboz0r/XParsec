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

            test "prim-types-min.fsi conforms to prim-types-min.clr.fs (no drift)" {
                let sigSrc = readNormalised (vesperCorePath "prim-types-min.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-min.clr.fs")
                let errors = conform sigSrc implSrc
                Expect.isEmpty errors "prim-types-min should conform with no errors"
            }

            test "prim-types-min: extern capability set equals the intrinsic set" {
                let sigSrc = readNormalised (vesperCorePath "prim-types-min.fsi")
                let implSrc = readNormalised (vesperCorePath "prim-types-min.clr.fs")

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

            test "concrete type declared in .fsi but absent from .fs → MissingInImpl" {
                // `bar` is a CONCRETE type (a union) — it requires an implementation, so its
                // absence is real drift. (A transparent abbreviation would be exempt; see the
                // dedicated abbreviation test below.)
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

            test "sig-only abbreviation needs no impl companion → no error" {
                // `type myalias = int` in the .fsi resolves transitively to `int`; F# needs
                // no `.fs` companion for a transparent abbreviation (the `ref`/`ResizeArray`/
                // `seq` pattern), so the check does not flag it MissingInImpl.
                let errors =
                    conform
                        "namespace V\n\ntype foo = extern\n\ntype myalias = int"
                        "namespace V\n\ntype foo = (# \"System.Int32\" #)"

                Expect.isEmpty errors "a sig-only abbreviation is conformant"
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

            // ---- Value-binding presence (Step 4.1) ----

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
                // `CstWalk` flattens nested modules, so a `val` inside `module M` pairs
                // with a `let` inside `module M` on the impl side.
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

// ---- Manifest-driven conformance over every package (T8 Step 3) -------
//
// The pairing is no longer a hand-maintained file list: `ConformancePass.checkManifest`
// reads each `Vesper.*/manifest.toml` and derives the `.fsi`↔`.fs` pairs from it
// (the manifest's own pairing stem over the resolved `impl` set), so a newly-added
// `.fsi`/`.fs` is conformance-checked automatically and can no longer be silently
// dropped from a curated list. The packages themselves are discovered from the
// source tree for the same reason.
//
// Each package is driven through `ConformancePass.enforce` (T8 Step 5), which
// promotes every discrepancy to a hard `Severity.Error` diagnostic — the FS0240
// family for a contract binding with no implementation (`MissingInImpl` /
// `ValueMissingInImpl`), extern/intrinsic drift, an un-exempted impl-free `SigOnly`
// `.fsi`, a leading-module-decl mismatch, or a contract-less `.fs`. The exemption
// list is no longer a test-side constant: it is the manifest's `[core] sig-only`
// (front-end-intrinsic `printf.fsi`, FSharp.Core-interop `printf-format.fsi`,
// per-target `exceptions.fsi`), so a `.fsi` whose `.fs` was deleted — and which is
// not declared impl-free — is a hard error by construction, NOT a pinned golden.
//
// The conformance check is codegen-independent (CST-level, not rung-gated), so it
// runs on all the Vesper.* packages — Set included — and is the cheapest way to
// catch `.fsi`/`.fs` drift the parser alone can't see. Only the CLR target is driven;
// JS-only contracts (`[targets.js] files`) are not in the CLR file set, so they need
// no CLR exemption.

let private vesperSrcDir = Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src")

/// Every `Vesper.*` package, discovered from the source tree → (dir name, manifest
/// path). Sorted for stable test ordering.
let private packageManifests: (string * string) list =
    Directory.GetDirectories(vesperSrcDir, "Vesper.*")
    |> Array.map (fun d -> Path.GetFileName d, Path.Combine(d, "manifest.toml"))
    |> Array.filter (fun (_, m) -> File.Exists m)
    |> Array.sortBy fst
    |> List.ofArray

/// Run the manifest-driven pass for a package, failing the test on a manifest /
/// parse error (the pass returns `Error`).
let private outcomeFor (target: string) (manifestPath: string) : ConformancePass.PackageOutcome =
    match ConformancePass.checkManifest target manifestPath with
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
                test $"{package}: manifest-driven conformance is enforced (no hard errors)" {
                    let outcome = outcomeFor "clr" manifestPath

                    // `enforce` subsumes every drift species — the FS0240 family
                    // (`MissingInImpl`/`ValueMissingInImpl`), extern/intrinsic drift, an
                    // un-exempted impl-free `SigOnly` (`sig-only` is now the manifest's,
                    // not a test constant), a module-decl mismatch, and a contract-less
                    // `.fs`. Every package must produce zero hard errors.
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
//
// The pass now runs for a second target, which it could not before: a contract JS binds
// no representation for is `Unrepresentable` — derived from the file's own content, not
// from a key someone remembered to add — so no JS exemption list has to be guessed. What
// remains on JS is not exemptions but MISSING WORK: most Vesper packages ship no JS
// bodies at all yet, and each of those is a hard error the run is right to keep making.
// So the JS assertions below are about the axis this outcome closes — every contract JS
// cannot represent is accepted, and no manifest anywhere carries a JS-specific
// `sig-only` key — rather than a blanket "no hard errors" the unported library cannot
// satisfy.

/// The contracts a target accepts as declared-but-unrepresentable, with the `extern`
/// types each names.
let private unrepresentableOf (outcome: ConformancePass.PackageOutcome) : (string * string list) list =
    [
        for p in outcome.Pairs do
            match p with
            | ConformancePass.PairOutcome.Unrepresentable(sigFile, types) -> yield sigFile, types
            | ConformancePass.PairOutcome.Paired _
            | ConformancePass.PairOutcome.SigOnly _
            | ConformancePass.PairOutcome.RuntimeServed _
            | ConformancePass.PairOutcome.ParseFailed _ -> ()
    ]

/// The contracts a target accepts because the committed runtime asset exports every value
/// they declare, with the asset and the values named.
let private runtimeServedOf (outcome: ConformancePass.PackageOutcome) : (string * string * string list) list =
    [
        for p in outcome.Pairs do
            match p with
            | ConformancePass.PairOutcome.RuntimeServed(sigFile, asset, values) -> yield sigFile, asset, values
            | ConformancePass.PairOutcome.Paired _
            | ConformancePass.PairOutcome.SigOnly _
            | ConformancePass.PairOutcome.Unrepresentable _
            | ConformancePass.PairOutcome.ParseFailed _ -> ()
    ]

let private manifestOf (package: string) : string =
    packageManifests
    |> List.tryFind (fun (p, _) -> p = package)
    |> Option.map snd
    |> Option.defaultWith (fun () -> failtestf "%s manifest not found" package)

[<Tests>]
let jsPackageConformanceTests =
    testList
        "PackageConformanceJs"
        [
            test "js: prim-types-nativeint.fsi is accepted as unrepresentable, naming all five types" {
                match
                    unrepresentableOf (outcomeFor "js" (manifestOf "Vesper.Core"))
                    |> List.tryFind (fun (f, _) -> f.Contains "nativeint")
                with
                | None -> failtest "prim-types-nativeint.fsi must be Unrepresentable on js"
                | Some(_, types) ->
                    Expect.equal
                        (List.sort types)
                        [ "ilsigptr"; "nativeint"; "nativeptr"; "unativeint"; "voidptr" ]
                        "every type the contract declares is named"
            }

            test "js: a contract whose declarations need a real body stays a hard error, not `unsupported`" {
                // `core-types.fsi` declares records/unions and ships no JS body yet. Absence
                // there is missing work, not a statement that JS cannot represent them —
                // exactly the split that keeps a forgotten `.fs` from reading as polite.
                let outcome = outcomeFor "js" (manifestOf "Vesper.Core")

                Expect.isFalse
                    (unrepresentableOf outcome |> List.exists (fun (f, _) -> f = "core-types.fsi"))
                    "a body-bearing contract is never accepted as unrepresentable"

                Expect.isFalse
                    (runtimeServedOf outcome |> List.exists (fun (f, _, _) -> f = "core-types.fsi"))
                    "nor as served by the runtime asset — an asset export is a value, never a type"

                Expect.stringContains
                    (ConformancePass.enforce outcome
                     |> List.map (fun d -> d.Message)
                     |> String.concat "\n")
                    "core-types.fsi"
                    "its absent body is still the FS0240-style hard error"
            }

            test "js: an unrepresentable contract raises no hard error, and needs no exemption to" {
                for package, manifestPath in packageManifests do
                    let outcome = outcomeFor "js" manifestPath
                    let errors = ConformancePass.enforce outcome |> List.map (fun d -> d.Message)

                    for sigFile, _ in unrepresentableOf outcome do
                        Expect.isFalse
                            (errors |> List.exists (fun m -> m.Contains sigFile))
                            (sprintf
                                "%s: %s is unrepresentable on js, so nothing may be enforced about it"
                                package
                                sigFile)

                        Expect.isFalse
                            (outcome.SigOnlyExemptions.Contains sigFile)
                            (sprintf "%s: %s is accepted by DERIVATION, not by a `sig-only` key" package sigFile)
            }

            test "js: the hard-error set is exactly the un-ported library surface" {
                // The JS port's remaining work, enumerated. Every entry is a `.fs` that has
                // not been written (or a codegen feature that is not there); nothing here is
                // a machinery artifact. Shrinking this list IS the port, so it is pinned
                // rather than counted — an entry that vanishes without the corresponding
                // source appearing means the pass stopped asking, and a NEW entry means a
                // contract lost its body.
                let expected =
                    [
                        "Vesper.Array: the signature file 'array.fsi' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                        "Vesper.Core: prim-types-min.fsi: type '``[]``' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Core: prim-types-min.fsi: type 'Fun' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Core: prim-types-object.fsi: type 'obj' disagrees on heritability across the pair: one side marks it a heritable external base ('extern class' / '(# class … #)'), the other an opaque value repr"
                        "Vesper.Core: the signature file 'compiler-attributes.fsi' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                        "Vesper.Core: the signature file 'core-types.fsi' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                        "Vesper.Core: the signature file 'structural-format.fsi' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                        "Vesper.Core: ops-platform.fsi: value 'ignore' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Core: ops-platform.fsi: value 'isNull' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Core: ops-platform.fsi: value 'box' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Core: ops-platform.fsi: value 'invalidArg' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Core: the signature file 'int-comparison.fsi' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                        "Vesper.List: list.fsi: value 'ofSeq' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.List: list.fsi: value 'toSeq' is declared in the signature (.fsi) but not defined in the implementation (.fs)"
                        "Vesper.Seq: the signature file 'seq.fsi' has no corresponding implementation file and is not declared `sig-only` in the manifest"
                    ]

                let actual =
                    [
                        for _, manifestPath in packageManifests do
                            for d in ConformancePass.enforce (outcomeFor "js" manifestPath) -> d.Message
                    ]

                Expect.equal actual expected "the js hard-error set"
            }

            test "js: a contract is runtime-served only when the asset exports every val it declares" {
                // The bodies of these two contracts live in the committed `.mjs`, not in a
                // `.fs`, so no `.fs` is owed. The verdict is CHECKED against the asset: the
                // negative control is `int-comparison.fsi`, which is equally all-`val` in a
                // package that equally ships an asset, and stays a hard error purely because
                // `Vesper.Core.mjs` exports no `<`/`>`/`<=`/`>=`.
                Expect.equal
                    (runtimeServedOf (outcomeFor "js" (manifestOf "Vesper.Core")))
                    [
                        "ops-platform-runtime.js.fsi",
                        "Vesper.Core.mjs",
                        [ "structuralEquals"; "structuralHash"; "checkedDivisor" ]
                    ]
                    "Vesper.Core: the equality/divisor runtime, and not int-comparison.fsi"

                Expect.equal
                    (runtimeServedOf (outcomeFor "js" (manifestOf "Vesper.Comparison")))
                    [
                        "comparison-runtime.js.fsi", "Vesper.Comparison.mjs", [ "structuralCompare" ]
                    ]
                    "Vesper.Comparison: the ordering runtime"
            }

            test "js: capabilities-compat.js.fsi is accepted as pure abbreviation, naming no extern" {
                // Five transparent abbreviations and nothing else. F# needs no `.fs` for an
                // abbreviation, so the contract owes no body — and it says so with an EMPTY
                // extern list, which is what distinguishes it from the nativeint family.
                match
                    unrepresentableOf (outcomeFor "js" (manifestOf "Vesper.Core"))
                    |> List.tryFind (fun (f, _) -> f = "capabilities-compat.js.fsi")
                with
                | None -> failtest "capabilities-compat.js.fsi must owe no `.fs` on js"
                | Some(_, types) -> Expect.isEmpty types "it declares no extern — every declaration is an abbreviation"
            }

            test "js: array-index.js.fsi PAIRS with its body rather than being waved through" {
                // Both halves of the old two-way bug: the `.fsi` was accepted as owing no
                // body (though its body exists) while the body was reported as contract-less.
                // The stem now names one pair, and it conforms — `extern` ↔ `(# "!0[]" #)`.
                let paired =
                    [
                        for p in (outcomeFor "js" (manifestOf "Vesper.Core")).Pairs do
                            match p with
                            | ConformancePass.PairOutcome.Paired r when r.SigFile = "array-index.js.fsi" -> yield r
                            | _ -> ()
                    ]

                match paired with
                | [ r ] ->
                    Expect.equal r.ImplFile "array-index.js.fs" "paired with its body"
                    Expect.isEmpty r.Errors "the array's extern and its intrinsic repr conform"
                | _ -> failtest "array-index.js.fsi must pair with array-index.js.fs"
            }

            test "js: a contract-less body is declared, not inferred, and raises nothing" {
                // `structural-printer.js.fs` is a standalone `%A` engine whose published
                // surface IS its contract. Declared `impl-only`, so it neither pairs with the
                // CLR `structural-printer.fsi` nor counts as an orphaned body.
                let outcome = outcomeFor "js" (manifestOf "Vesper.Printf")

                Expect.equal
                    (List.ofSeq outcome.ImplOnlyDeclarations)
                    [ "structural-printer.js.fs" ]
                    "the one body that implements no contract"

                Expect.isEmpty (ConformancePass.enforce outcome) "Vesper.Printf conforms on js"
            }

            test "no manifest carries a target-specific `sig-only` list" {
                // The exemption list that would otherwise have to be guessed per target.
                // Its absence everywhere is what "list-free" means concretely.
                for package, manifestPath in packageManifests do
                    match ReferencedProject.loadManifest manifestPath with
                    | Error e -> failtestf "%s: %s" package e
                    | Ok m ->
                        for KeyValue(target, lists) in m.Targets do
                            Expect.isEmpty
                                lists.SigOnly
                                (sprintf "%s: [targets.%s] declares a sig-only exemption list" package target)
            }
        ]

// ---- Step 5: conformance findings are HARD errors --------------------
//
// `enforce` is the flip from "a finding a test inspects" to "an FS0240-style hard
// error that fails the build". These pin the promotion directly on a synthetic
// `PackageOutcome` (no manifest round-trip): a `.fsi` with no `.fs` and no `sig-only`
// exemption, a kernel `MissingInImpl`, and the conforming/exempt controls.

let private mkOutcome
    (pairs: ConformancePass.PairOutcome list)
    (sigOnly: Set<string>)
    : ConformancePass.PackageOutcome =
    {
        Package = "Test"
        Pairs = pairs
        ImplOnly = []
        ImplOnlyDeclarations = Set.empty
        SigOnlyExemptions = sigOnly
    }

[<Tests>]
let enforcementTests =
    testList
        "ConformanceEnforcement"
        [
            test "an un-exempted SigOnly .fsi (a deleted impl) → hard FS0240-style error" {
                let outcome =
                    mkOutcome [ ConformancePass.PairOutcome.SigOnly "deleted-impl.fsi" ] Set.empty

                let errors = ConformancePass.enforce outcome

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.equal errors.Head.Severity Severity.Error "error severity"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V240") "the FS0240 family"
                Expect.stringContains errors.Head.Message "deleted-impl.fsi" "names the orphaned .fsi"
            }

            test "an Unrepresentable .fsi → no error, with no exemption declared" {
                // The third verdict: declared, unrepresentable, accepted. The reject is
                // owed at the use site, so nothing is enforced here — and unlike the
                // `SigOnly` above, the empty exemption set is what it is accepted against.
                let outcome =
                    mkOutcome
                        [
                            ConformancePass.PairOutcome.Unrepresentable("prim-types-nativeint.fsi", [ "nativeint" ])
                        ]
                        Set.empty

                Expect.isEmpty (ConformancePass.enforce outcome) "an unrepresentable contract conforms"
            }

            test "a SigOnly .fsi declared `sig-only` in the manifest → no error (exempt)" {
                let outcome =
                    mkOutcome [ ConformancePass.PairOutcome.SigOnly "printf.fsi" ] (Set.ofList [ "printf.fsi" ])

                Expect.isEmpty (ConformancePass.enforce outcome) "a recorded impl-free exemption conforms"
            }

            test "a MissingInImpl kernel finding on a paired contract → hard FS0240-style error" {
                let paired =
                    ConformancePass.PairOutcome.Paired
                        {
                            SigFile = "x.fsi"
                            ImplFile = "x.fs"
                            ModuleMismatch = None
                            Errors = [ Conformance.ConformanceError.MissingInImpl "bar" ]
                        }

                let errors = ConformancePass.enforce (mkOutcome [ paired ] Set.empty)

                Expect.equal (List.length errors) 1 "one hard error"
                Expect.equal errors.Head.Severity Severity.Error "error severity"
                Expect.stringContains errors.Head.Message "bar" "names the missing type"
            }

            test "a stale `sig-only` exemption (companion .fs exists) → V243 hygiene error" {
                let paired =
                    ConformancePass.PairOutcome.Paired
                        {
                            SigFile = "paired.fsi"
                            ImplFile = "paired.fs"
                            ModuleMismatch = None
                            Errors = []
                        }

                let errors =
                    ConformancePass.enforce (mkOutcome [ paired ] (Set.ofList [ "paired.fsi" ]))

                Expect.equal (List.length errors) 1 "one hygiene error"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V243") "stale exemption"
            }

            test "a contract-less .fs → V242, unless the manifest declares it `impl-only`" {
                // F# requires no `.fsi`, but a Vesper package publishes a contract surface —
                // so an undeclared body with none is the hard error, and the declaration is
                // what turns it into a statement.
                let orphaned =
                    { mkOutcome [] Set.empty with
                        ImplOnly = [ "engine.js.fs" ]
                    }

                let errors = ConformancePass.enforce orphaned
                Expect.equal (List.length errors) 1 "one hard error"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V242") "the contract-less-body family"

                Expect.isEmpty
                    (ConformancePass.enforce
                        { orphaned with
                            ImplOnlyDeclarations = Set.ofList [ "engine.js.fs" ]
                        })
                    "a declared contract-less body conforms"
            }

            test "an `impl-only` naming a body the target does not compile → V243 hygiene error" {
                // The mirror of a stale `sig-only`: the declaration outlived the file, or the
                // `.fsi` it disclaims came back (in which case that contract's own V240 fires
                // alongside).
                let errors =
                    ConformancePass.enforce
                        { mkOutcome [] Set.empty with
                            ImplOnlyDeclarations = Set.ofList [ "gone.js.fs" ]
                        }

                Expect.equal (List.length errors) 1 "one hygiene error"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V243") "stale/unknown declaration"
                Expect.stringContains errors.Head.Message "gone.js.fs" "names the dangling declaration"
            }

            test "a RuntimeServed .fsi → no error, with no exemption declared" {
                // The fourth verdict: the bodies are the committed runtime asset's exports,
                // checked against the asset, so the absent `.fs` is correct rather than
                // waived.
                let outcome =
                    mkOutcome
                        [
                            ConformancePass.PairOutcome.RuntimeServed(
                                "ops-platform-runtime.js.fsi",
                                "Vesper.Core.mjs",
                                [ "structuralEquals" ]
                            )
                        ]
                        Set.empty

                Expect.isEmpty (ConformancePass.enforce outcome) "a runtime-served contract conforms"
            }

            test "a parse failure is a per-contract V244 error, not an abort that masks the rest" {
                // `checkManifest` collects a parse failure as a `ParseFailed` verdict
                // rather than returning `Error`, so a sibling contract's drift on the
                // same package is still reported — both errors surface, in order.
                let outcome =
                    mkOutcome
                        [
                            ConformancePass.PairOutcome.ParseFailed("broken.fsi", "unexpected token")
                            ConformancePass.PairOutcome.SigOnly "deleted-impl.fsi"
                        ]
                        Set.empty

                let errors = ConformancePass.enforce outcome

                Expect.equal (List.length errors) 2 "the parse failure does not mask the orphaned .fsi"
                Expect.equal errors.Head.Code (DiagCode.Vesper "V244") "the parse-failure family"
                Expect.stringContains errors.Head.Message "broken.fsi" "names the unparseable contract"
                Expect.equal errors.[1].Code (DiagCode.Vesper "V240") "the sibling drift still surfaces"
            }
        ]

// ---- Semantic typar-order conformance (T8 Step 4.2) -------------------
//
// `ConformanceTypars.checkFile` is the SEMANTIC half: it compares a `.fs`-inferred
// generic module binding's frozen scheme (typars `FTTypar(Method, i)`, in
// `GeneralizedTypars.canonical` order) against the `.fsi`-declared scheme an
// `IExternalSymbolProvider` publishes (typars `FTTypar(Declaring, i)`, in
// `translateCurriedSig` appearance order). Because `FTTypar` is positional, a
// structural `FrozenType` equality after axis normalization IS α-equivalence-WITH-
// ORDER: it fails exactly when the two sides number their typars differently.
//
// The contract side is a stub provider so the test pins the exact declared order
// without a manifest round-trip; the impl side runs the REAL frozen pipeline
// (`Pipeline.analyseForSelfHost`), so the inferred order is genuinely inference's, not
// a hand-built `FrozenType`. The canonical case is the plan's `<'b,'a>`-reorder: a
// `.fs` that declares its typars in a different order than the `.fsi`'s appearance
// order is the one species of drift this catches (see `ElaborateTests`' "free function
// honours declared `<'b,'a>` typar order over appearance").

/// A contract provider that publishes exactly `entries` (name → declared scheme) and
/// nothing else — the `.fsi` side of one `checkFile` run.
let private contractProvider (entries: (string * ExternalSymbol) list) : IExternalSymbolProvider =
    let m = Map.ofList entries

    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookup =
                fun name ->
                    match Map.tryFind name m with
                    | Some s -> ValueSome s
                    | None -> ValueNone
        }

/// Run the `.fs` through the real frozen self-host pipeline (so a generic binding's
/// typar order is inference's own). The snippets reference no external symbols, so
/// the provider only matters for its absence of interference — the real contract
/// resolves them identically.
let private frozenOf (src: string) : FrozenPools =
    let lexed, file = parseFile src
    Pipeline.analyseForSelfHost "M" realProvider.Value (Hashing.originSourceOfText src lexed) file

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

            test "a binding the contract does not publish is skipped (presence is Step 4.1)" {
                // An empty contract: a private/unpublished binding has no declared scheme to
                // compare — typar-order is not the presence check's job.
                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty (ConformanceTypars.checkFile (contractProvider []) tast) "unpublished binding skipped"
            }
        ]

// ---- Semantic typar-order conformance for type MEMBERS (T8 Step 6) ----------
//
// `ConformanceTypars.checkMembers` is the member-level twin of `checkFile`: a
// generic `.fs` type member (`member M<'a,'b>(x,y) = …`) is compared against the
// `.fsi`-published overload set (`TryLookupMembers`). A member carries two typar
// axes, so the comparison is a DIRECT structural equality of the two frozen member
// signatures (no axis collapse): both sides write the declaring type's typars on
// `FTTypar(Declaring,_)` and the method's own on `FTTypar(Method,_)`, each in
// canonical order, so `=` is α-equivalence-with-order across both axes. A member
// with no matching-arity published overload is skipped (presence is Step 4.1's job).
//
// The real `formatter.clr.fs ↔ formatter.fsi` end-to-end check lives in
// `Codegen.Clr.Tests/ConformanceTyparsTests.fs` (it needs `ClrSymbolProviders` to
// EXTRACT the contract); here the contract side is a stub publishing an exact member
// overload set, so the drift case is pinned without a manifest round-trip.

/// A method-axis typar marker (`FTTypar(Method, i)`).
let private mAxis (i: int) : FrozenType = FTTypar(TyparAxis.Method, i)

/// A non-property, non-static external member named `name` with `methodTyparArity` own
/// typars and the given (already method-axised) tupled `parameters` / `ret` — the
/// `.fsi`-published overload the stub serves.
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

/// A contract provider publishing exactly `overloads` as the member set of every
/// type (keyed by member name; the declaring-type name is ignored, so the stub
/// serves whatever qualified name the `.fs` type resolves under).
let private memberContractProvider (overloads: ExternalMember list) : IExternalSymbolProvider =
    // The declaring-type name of each member channel is ignored, so the stub serves its
    // overload set to whatever qualified name the `.fs` type resolves under.
    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookupMember =
                fun (_, name) ->
                    match overloads |> List.tryFind (fun m -> m.Name = name) with
                    | Some m -> ValueSome m
                    | None -> ValueNone
            TryLookupMembers = fun (_, name) -> overloads |> List.filter (fun m -> m.Name = name) |> List.toArray
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
                // REVERSED `<'b,'a>` numbering — `(M1 * M0) -> M1` — the member-level twin
                // of `checkFile`'s `<'b,'a>` drift, caught by the same positional equality.
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

            test "a member the contract does not publish is skipped (presence is Step 4.1)" {
                // No published overload of matching arity → no typar-order verdict to make.
                let tast = frozenOf "type C() =\n    member this.M<'a,'b>(x: 'a, y: 'b) = x"

                Expect.isEmpty
                    (ConformanceTypars.checkMembers (memberContractProvider []) tast)
                    "unpublished member skipped"
            }
        ]
