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

/// Conformance findings each package is EXPECTED to emit. The check is now accurate
/// BY CONSTRUCTION — a transparent abbreviation (`type X = Y`) needs no companion, an
/// impl-only type is a HiddenTycon (F# hides it, not drift), and value presence
/// matches F#'s FS0240 — so the false positives that used to be pinned here are gone.
/// Any finding the pass still produces is therefore a REAL `.fsi`/`.fs` discrepancy to
/// be FIXED AT THE SOURCE, never accepted here. The map is empty: every package
/// conforms (its drift fixed at the source).
let private acceptedFindings: Map<string, (string * Conformance.ConformanceError) list> =
    Map.empty

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
// order is the one species of drift this catches (see `FreezeTests`' "free function
// honours declared `<'b,'a>` typar order over appearance").

/// A contract provider that publishes exactly `entries` (name → declared scheme) and
/// nothing else — the `.fsi` side of one `checkFile` run.
let private contractProvider (entries: (string * ExternalSymbol) list) : IExternalSymbolProvider =
    let m = Map.ofList entries

    { new IExternalSymbolProvider with
        member _.TryLookup name =
            match Map.tryFind name m with
            | Some s -> ValueSome s
            | None -> ValueNone

        member _.TryLookupType _ = ValueNone
        member _.TryLookupMember(_, _) = ValueNone
        member _.TryLookupMembers(_, _) = [||]
        member _.TryLookupUnionCase _ = ValueNone
        member _.AmbientOpenPrefixes = []
        member _.TryLookupInlineBody _ = ValueNone
        member _.TryLookupInlineBodyByName _ = ValueNone
        member _.IntrinsicReverseCanon = Map.empty
        member _.IntrinsicForwardRepr = Map.empty
    }

/// Run the `.fs` through the real frozen self-host pipeline (so a generic binding's
/// typar order is inference's own). The snippets reference no external symbols, so
/// the provider only matters for its absence of interference — the real contract
/// resolves them identically.
let private frozenOf (src: string) : Frozen.TastFile =
    let lexed, file = parseFile src
    Pipeline.analyseForSelfHost "M" realProvider.Value src lexed file

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
                let contract = contractProvider [ "f", ExternalSymbols.scheme "f" fScheme 2 [] ]
                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let mismatches = ConformanceTypars.checkFile contract tast
                Expect.equal (List.length mismatches) 1 "one typar-order mismatch"
                Expect.equal mismatches.Head.Name "f" "the mismatch names f"
            }

            test "appearance-order impl conforms to `.fsi` appearance order → no mismatch" {
                // No explicit `<…>`: the canonical order IS appearance order, matching the
                // `.fsi`. The very same binding+contract that fails above now conforms.
                let contract = contractProvider [ "f", ExternalSymbols.scheme "f" fScheme 2 [] ]
                let tast = frozenOf "let f (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                Expect.isEmpty (ConformanceTypars.checkFile contract tast) "appearance-order impl conforms"
            }

            test "a binding the contract does not publish is skipped (presence is Step 4.1)" {
                // An empty contract: a private/unpublished binding has no declared scheme to
                // compare — typar-order is not the presence check's job.
                let tast = frozenOf "let f<'b,'a> (x: 'a) (y: 'b) : 'b = y"
                Expect.isEmpty (ConformanceTypars.checkFile (contractProvider []) tast) "unpublished binding skipped"
            }
        ]
