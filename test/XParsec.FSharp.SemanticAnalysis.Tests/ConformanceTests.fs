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
                test $"{package}: manifest-driven conformance is enforced (no hard errors)" {
                    let outcome = outcomeFor manifestPath

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
                Expect.equal errors.Head.Code "V240" "the FS0240 family"
                Expect.stringContains errors.Head.Message "deleted-impl.fsi" "names the orphaned .fsi"
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
                Expect.equal errors.Head.Code "V243" "stale exemption"
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
                Expect.equal errors.Head.Code "V244" "the parse-failure family"
                Expect.stringContains errors.Head.Message "broken.fsi" "names the unparseable contract"
                Expect.equal errors.[1].Code "V240" "the sibling drift still surfaces"
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
        member _.TryLookupIndexSignature _ = []
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
// The real `formatter.fs ↔ formatter.fsi` end-to-end check lives in
// `Codegen.Clr.Tests/ConformanceTyparsTests.fs` (it needs `ClrSymbolProviders` to
// EXTRACT the contract); here the contract side is a stub publishing an exact member
// overload set, so the drift case is pinned without a manifest round-trip.

/// A method-axis typar marker (`FTTypar(Method, i)`).
let private mAxis (i: int) : FrozenType = FTTypar(TyparAxis.Method, i)

/// A non-property, non-static external member named `name` with `methodArity` own
/// typars and the given (already method-axised) tupled `parameters` / `ret` — the
/// `.fsi`-published overload the stub serves.
let private mkMember (name: string) (methodArity: int) (parameters: FrozenType) (ret: FrozenType) : ExternalMember =
    {
        Name = name
        IsStatic = false
        Storage = MemberStorage.Method
        Signature = mkSignature 0 methodArity parameters ret
        MethodArity = methodArity
        Origin = SymbolOrigin.Empty
        Key = SymbolKey.MemberKey(SymbolKeyOps.qualifiedTypeKeyOf None "C" 0, name, EqArray.empty, MemberKind.Method)
        OptionalDefaults = []
        IsOptional = false
    }

/// A contract provider publishing exactly `overloads` as the member set of every
/// type (keyed by member name; the declaring-type name is ignored, so the stub
/// serves whatever qualified name the `.fs` type resolves under).
let private memberContractProvider (overloads: ExternalMember list) : IExternalSymbolProvider =
    { new IExternalSymbolProvider with
        member _.TryLookup _ = ValueNone
        member _.TryLookupType _ = ValueNone

        member _.TryLookupMember(_, name) =
            match overloads |> List.tryFind (fun m -> m.Name = name) with
            | Some m -> ValueSome m
            | None -> ValueNone

        member _.TryLookupMembers(_, name) =
            overloads |> List.filter (fun m -> m.Name = name) |> List.toArray

        member _.TryLookupIndexSignature _ = []
        member _.TryLookupUnionCase _ = ValueNone
        member _.AmbientOpenPrefixes = []
        member _.TryLookupInlineBody _ = ValueNone
        member _.TryLookupInlineBodyByName _ = ValueNone
        member _.IntrinsicReverseCanon = Map.empty
        member _.IntrinsicForwardRepr = Map.empty
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
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let contract = memberContractProvider [ mkMember "M" 1 (mAxis 0) (mAxis 0) ]
                Expect.isEmpty (ConformanceTypars.checkMembers contract tast) "identity generic member conforms"
            }

            test "published `<'b,'a>` reorder vs `.fs` `<'a,'b>` → MemberMismatch" {
                // `.fs` declares `<'a,'b>`: `x:'a` = Method 0, `y:'b` = Method 1, so the
                // inferred signature is `(M0 * M1) -> M0`. The published overload is the
                // REVERSED `<'b,'a>` numbering — `(M1 * M0) -> M1` — the member-level twin
                // of `checkFile`'s `<'b,'a>` drift, caught by the same positional equality.
                let tast = frozenOf "type C() =\n    member this.M<'a,'b>(x: 'a, y: 'b) = x"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let swapped =
                    mkMember "M" 2 (FTTuple(EqArray.ofList [ mAxis 1; mAxis 0 ])) (mAxis 1)

                let mismatches =
                    ConformanceTypars.checkMembers (memberContractProvider [ swapped ]) tast

                Expect.equal (List.length mismatches) 1 "one member typar-order mismatch"
                Expect.equal mismatches.Head.MemberName "M" "the mismatch names M"
                Expect.equal mismatches.Head.MethodArity 2 "carries the method arity"
            }

            test "a member the contract does not publish is skipped (presence is Step 4.1)" {
                // No published overload of matching arity → no typar-order verdict to make.
                let tast = frozenOf "type C() =\n    member this.M<'a,'b>(x: 'a, y: 'b) = x"

                Expect.isEmpty
                    (ConformanceTypars.checkMembers (memberContractProvider []) tast)
                    "unpublished member skipped"
            }
        ]
