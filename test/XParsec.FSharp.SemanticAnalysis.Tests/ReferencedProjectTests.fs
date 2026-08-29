module XParsec.FSharp.SemanticAnalysis.Tests.ReferencedProjectTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.Codegen.Common.Tests

// `Vesper.Core` stood up from its real manifests: what its contract resolves, and with
// which identities.

/// `src/Vesper.Core`, found by walking up from the test assembly.
let private vesperCorePackage =
    let testDir = Path.GetDirectoryName(typeof<ParsedManifest>.Assembly.Location)

    let mutable dir = DirectoryInfo testDir
    let mutable found = None

    while not (isNull dir) && found.IsNone do
        let candidate = Path.Combine(dir.FullName, "src", "Vesper.Core")

        if File.Exists(Path.Combine(candidate, "manifest.clr.toml")) then
            found <- Some candidate

        dir <- dir.Parent

    match found with
    | Some p -> p
    | None -> failwithf "Could not locate src/Vesper.Core/manifest.clr.toml from %s" testDir

/// The `src/` directory holding the real packages — the parent of `Vesper.Core`.
let private srcDir = Path.GetDirectoryName vesperCorePackage

/// A sibling package that `depends-on` Vesper.Core — the dependency-ordering fixture.
let private vesperListPackage = Path.Combine(srcDir, "Vesper.List")

/// Resolve a package for `target`, failing the test if it does not build for it.
let private resolveOrFail (target: string) (packageDir: string) : ReferencedProject.ManifestPath =
    ReferencedProject.resolveManifest target packageDir
    |> PackageFaults.okOrFail "resolveManifest"

/// Parse a resolved manifest, failing the test if it is malformed.
let private loadOrFail (mp: ReferencedProject.ManifestPath) : ReferencedProject.Manifest =
    ReferencedProject.loadManifest mp |> PackageFaults.okOrFail "loadManifest"

let private vesperCoreManifest = resolveOrFail "clr" vesperCorePackage
let private vesperCoreJsManifest = resolveOrFail "js" vesperCorePackage
let private vesperListManifest = resolveOrFail "clr" vesperListPackage

/// A throwaway `src/`-shaped tree under the repo `./tmp`: a `depends-on` name resolves to
/// a SIBLING package directory, so synthetic manifests must live side by side as
/// `tmpSrc/<name>/manifest.<target>.toml`.
let private tmpSrc =
    let repoRoot = Path.GetDirectoryName srcDir
    let d = Path.Combine(repoRoot, "tmp", "buildClosure-tests", "src")
    Directory.CreateDirectory d |> ignore
    d

/// Writes `tmpSrc/<name>/manifest.<target>.toml` with `body`, and resolves it.
let private writeManifestFor (target: string) (name: string) (body: string) : ReferencedProject.ManifestPath =
    let dir = Path.Combine(tmpSrc, name)
    Directory.CreateDirectory dir |> ignore
    File.WriteAllText(Path.Combine(dir, "manifest." + target + ".toml"), body)
    resolveOrFail target dir

/// `writeManifestFor` at a tag no backend claims, for the fixtures that exercise
/// resolution and closure shape rather than anything a target decides.
let private writeManifest (name: string) (body: string) : ReferencedProject.ManifestPath =
    writeManifestFor "none" name body

/// A synthetic package with no sources: `files = []` keeps it parse-valid with no `.fsi`.
let private writeSyntheticManifest (name: string) (dependsOn: string list) : ReferencedProject.ManifestPath =
    // These fixtures are siblings under one synthetic `src/`, so each lists `../<package>`.
    let deps = dependsOn |> List.map (sprintf "\"../%s\"") |> String.concat ", "
    writeManifest name (sprintf "[core]\nname = \"%s\"\ndepends-on = [%s]\nfiles = []\n" name deps)

/// Writes a synthetic package declaring `body` under `ns` — for the duplicate-type tests,
/// which need two packages declaring one name. `body` is a union, so the `.fsi` and its
/// companion `.fs` spell it identically and the pair conforms.
let private writeSyntheticPackageWithType (name: string) (ns: string) (body: string) : ReferencedProject.ManifestPath =
    let dir = Path.Combine(tmpSrc, name)
    Directory.CreateDirectory dir |> ignore
    let source = sprintf "namespace %s\n%s\n" ns body
    File.WriteAllText(Path.Combine(dir, "contract.fsi"), source)
    File.WriteAllText(Path.Combine(dir, "contract.fs"), source)

    writeManifest name (sprintf "[core]\nname = \"%s\"\nfiles = [\"contract.fsi\", \"contract.fs\"]\n" name)

let private builtProvider =
    lazy
        (PackageProviders.buildProvider vesperCoreManifest
         |> PackageFaults.okOrFail "buildProvider")

/// The same package's JS contract: no `.js.fs` capability reprs, plus the
/// `capabilities-compat.js.fsi` shim `manifest.js.toml` names and the clr one does not.
let private builtProviderJs =
    lazy
        (let bp =
            PackageProviders.buildProviderWith
                ExternalSymbolProviders.nullProvider
                (ParsedManifest.ofManifest (loadOrFail vesperCoreJsManifest))

         bp.Provider, bp.Diagnostics)

[<Tests>]
let tests =
    testList
        "ReferencedProject"
        [
            test "manifest parses: name and files in compile order" {
                let m = loadOrFail vesperCoreManifest

                // `Vesper.Core`'s `[core]` carries no `name`, so it falls back to the directory.
                Expect.equal m.Name "Vesper.Core" "assembly name from dir"
                Expect.equal m.Target "clr" "target from the file name"
                Expect.isNonEmpty m.Files "files listed"

                Expect.equal
                    (List.head m.Files)
                    {
                        ReferencedProject.Relative = "prim-types-min.fsi"
                        ReferencedProject.Kind = SourceFileKind.Signature
                    }
                    "compile order: prim-types-min's contract first"
            }

            // Every signature file goes through the same front end an in-assembly `.fsi` does,
            // so anything it could not resolve is reported rather than silently unpublished.
            test "every listed .fsi resolves clean" {
                let _, diags = builtProvider.Value

                let rendered = [ for d in diags -> AssemblyFiles.AnchoredDiagnostic.render d ]

                Expect.isEmpty rendered (sprintf "expected a clean contract, got: %A" rendered)
            }

            test "int resolves (qualified) as an Intrinsic shape carrying its `.fs` repr" {
                let provider, _ = builtProvider.Value

                // `int` is an `extern` paired with its sibling `.fs` `(# "System.Int32" #)`
                // binding, so it surfaces as an `Intrinsic`: `canon` is the `.fsi` name `int`,
                // `platform` the CLI repr codegen consumes. An intrinsic carries no `Origin`.
                match ExternalSymbols.tryReprType provider "Vesper.int" with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Canon = canon
                                                                     Platform = IntrinsicPlatform.Repr platform
                                                                 }
                                                        }) ->
                    Expect.equal canon (RuntimeNames.intKey) "int's canon identity is the `.fsi` name"

                    Expect.equal
                        platform
                        "System.Int32"
                        "int's platform name is its prim-types-min `.fs` CLI representation"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }

            // Discharging an `op_Addition` bound on `int` asks the provider under the key the
            // `TyConst` payload carries, and nothing else. Published under any other key the
            // lookup misses silently: operator-name synthesis yields the same name for `int`.
            test "the key a `TyConst int` carries resolves its declared op_Addition through the contract" {
                let provider, _ = builtProvider.Value

                match provider.TryLookupMember(RuntimeNames.intKey, "op_Addition") with
                | ValueSome m -> Expect.isTrue m.IsStatic "the declared operator witness is static"
                | ValueNone -> failtest "Vesper.int declares `static member (+)`, but not under its TyConst key"
            }

            test "ctx.Intrinsics resolves each primitive from the contract to the static BuiltinTypes key" {
                // Each `ctx.Intrinsics.*` resolves its name through the provider's ambient
                // `open Vesper` (no hardcoded namespace) and must equal its `BuiltinTypes.ty*`.
                let provider, _ = builtProvider.Value
                let lexed = XParsec.FSharp.Lexer.Lexing.lexString ""
                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                Expect.equal ctx.Intrinsics.Int BuiltinTypes.tyInt "int"
                Expect.equal ctx.Intrinsics.Int64 BuiltinTypes.tyInt64 "int64"
                Expect.equal ctx.Intrinsics.Byte BuiltinTypes.tyByte "byte"
                Expect.equal ctx.Intrinsics.Float BuiltinTypes.tyFloat "float"
                Expect.equal ctx.Intrinsics.Float32 BuiltinTypes.tyFloat32 "float32"
                Expect.equal ctx.Intrinsics.Bool BuiltinTypes.tyBool "bool"
                Expect.equal ctx.Intrinsics.Char BuiltinTypes.tyChar "char"
                Expect.equal ctx.Intrinsics.Unit BuiltinTypes.tyUnit "unit"
                Expect.equal ctx.Intrinsics.String BuiltinTypes.tyString "string"
            }

            test "the language-capability anchors resolve as two-name IntrinsicInterfaces carrying their CLR repr" {
                // A capability pairs `extern with abstract member …` in `capabilities.fsi` with a
                // `capabilities.clr.fs` `(# "<BCL interface>" #)` repr, surfacing as one two-name
                // `IntrinsicInterface`; a generic carries its arity in both `Canon` and `Platform`.
                let provider, _ = builtProvider.Value

                let expectCapability (lookup: string) (canonKey: TypeKey) (platformExpected: string) =
                    match ExternalSymbols.tryReprType provider lookup with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                        Expect.equal
                            iface.Canon
                            canonKey
                            (sprintf "%s canon is the identity of its `.fsi` type, arity included" lookup)

                        Expect.equal
                            iface.Platform
                            platformExpected
                            (sprintf "%s platform name is its `.fs` CLR repr" lookup)

                        // A capability interface is ABSENT from the intrinsic axis:
                        // reconciliation goes through the platform name above, not the axis.
                        match IntrinsicTypeMap.canonsOf platformExpected provider.IntrinsicTypeMap with
                        | EqEmpty -> ()
                        | canons ->
                            failtestf
                                "capability interface %s must NOT enter the intrinsic axis; found %A"
                                lookup
                                canons
                    | other -> failtestf "expected %s as an IntrinsicInterface shape, got %A" lookup other

                expectCapability "Vesper.disposable" (RuntimeNames.primitiveKey "disposable") "System.IDisposable"

                expectCapability "Vesper.equatable`1" (RuntimeNames.primitiveKey "equatable`1") "System.IEquatable`1"

                expectCapability "Vesper.comparable`1" (RuntimeNames.primitiveKey "comparable`1") "System.IComparable`1"

                expectCapability
                    "Vesper.Collections.enumerator`1"
                    (SymbolKeyOps.typeKeyOf "Vesper.Collections" "enumerator`1")
                    "System.Collections.Generic.IEnumerator`1"

                expectCapability
                    "Vesper.Collections.seq`1"
                    (SymbolKeyOps.typeKeyOf "Vesper.Collections" "seq`1")
                    "System.Collections.Generic.IEnumerable`1"

                // `enumerator` inherits `disposable`, so the `use` / for-in disposability scan
                // needs that inherited interface on the capability shape.
                match ExternalSymbols.tryReprType provider "Vesper.Collections.enumerator`1" with
                | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                    let ifaceNames =
                        iface.Interfaces |> EqArray.map (fun i -> SymbolKeyOps.typeMetaName i.Key)

                    Expect.isTrue
                        (ifaceNames |> EqArray.exists (fun n -> n.Contains "disposable"))
                        (sprintf "enumerator inherits disposable; Interfaces = %A" ifaceNames)
                | other -> failtestf "expected enumerator as IntrinsicInterface, got %A" other
            }

            test "JS build: capabilities are canon-only; BCL spellings resolve through the compat shim" {
                // The JS bodies bind sentinel reprs and the `capabilities-compat.js.fsi` shim
                // is appended, so the BCL spelling is quarantined to the shim and reaches the
                // canonical by abbreviation.
                let provider, _ = builtProviderJs.Value

                let expectSentinelRepr (lookup: string) (sentinel: string) =
                    match ExternalSymbols.tryReprType provider lookup with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                        Expect.equal
                            iface.Platform
                            sentinel
                            (sprintf "%s binds the sentinel %s, which names no JS global" lookup sentinel)
                    | other -> failtestf "expected %s as an IntrinsicInterface on JS, got %A" lookup other

                expectSentinelRepr "Vesper.disposable" "!Vesper.disposable"
                expectSentinelRepr "Vesper.equatable`1" "!Vesper.equatable"
                expectSentinelRepr "Vesper.comparable`1" "!Vesper.comparable"

                // The shim abbreviates each BCL spelling to the canonical capability, so
                // `interface System.IDisposable` records the canonical key on JS.
                let expectShimAbbrev (bcl: string) (canonQualified: string) =
                    match ExternalSymbols.tryReprType provider bcl with
                    | ValueSome(ExternalTypeShape.Abbrev(_, FTClass(key, _))) ->
                        Expect.equal
                            (SymbolKeyOps.typeMetaName key)
                            canonQualified
                            (sprintf "%s shim-abbreviates to the canonical %s" bcl canonQualified)
                    | other -> failtestf "expected %s as a compat-shim Abbrev to %s, got %A" bcl canonQualified other

                expectShimAbbrev "System.IDisposable" "Vesper.disposable"
                expectShimAbbrev "System.IEquatable`1" "Vesper.equatable`1"
                expectShimAbbrev "System.IComparable`1" "Vesper.comparable`1"
            }

            test "JS build: no capability keys by a BCL spelling; every one keys by a sentinel" {
                // A BCL spelling reaches JS only through the compat shim above, which is an
                // `Abbrev` that expands at the use. So no capability keys by one, and nothing
                // downstream may tag a JS type with one.
                let provider, _ = builtProviderJs.Value
                let caps = ExternalSymbols.resolveCapabilities provider

                // A capability's spelling is a compiled NAME in the contract, cut to the key
                // the identity compares on. Arity 0: the `` `1 `` suffix parses.
                let matchesSpelling (id: RuntimeNames.CapabilityIdentity) (compiled: string) =
                    id.Matches(SymbolKeyOps.qualifiedTypeKeyOf compiled 0)

                let expectMatches
                    (name: string)
                    (cap: RuntimeNames.CapabilityIdentity voption)
                    (bcl: string)
                    (canon: string)
                    : RuntimeNames.CapabilityIdentity =
                    match cap with
                    | ValueSome id ->
                        Expect.isTrue (matchesSpelling id canon) (sprintf "%s matches its canonical key %s" name canon)
                        Expect.isFalse (matchesSpelling id bcl) (sprintf "%s's BCL spelling %s is not a name" name bcl)
                        id
                    | ValueNone -> failtestf "%s resolved to ValueNone on JS" name

                // Each JS body binds a `!`-prefixed repr, so the anchor keys by that sentinel
                // WITH the canon beside it — a spelling no JS global can collide with.
                let expectSentinel name cap bcl canon sentinel =
                    let id = expectMatches name cap bcl canon

                    Expect.equal
                        id.CanonKey
                        (ValueSome(SymbolKeyOps.qualifiedTypeKeyOf canon 0))
                        (sprintf "%s carries its canon beside the platform key" name)

                    Expect.isTrue (matchesSpelling id sentinel) (sprintf "%s keys by the sentinel %s" name sentinel)

                expectSentinel
                    "Enumerable"
                    caps.Enumerable
                    "System.Collections.Generic.IEnumerable`1"
                    "Vesper.Collections.seq`1"
                    "!Vesper.Collections.seq"

                expectSentinel
                    "Enumerator"
                    caps.Enumerator
                    "System.Collections.Generic.IEnumerator`1"
                    "Vesper.Collections.enumerator`1"
                    "!Vesper.Collections.enumerator"

                expectSentinel
                    "Disposable"
                    caps.Disposable
                    "System.IDisposable"
                    "Vesper.disposable"
                    "!Vesper.disposable"

                expectSentinel "Equatable" caps.Equatable "System.IEquatable`1" "Vesper.equatable`1" "!Vesper.equatable"

                expectSentinel
                    "Comparable"
                    caps.Comparable
                    "System.IComparable`1"
                    "Vesper.comparable`1"
                    "!Vesper.comparable"
            }

            test "Fun resolves (qualified) as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                // `type Fun<'A,'B>` parses as an anonymous (`= begin … end`) type, so the
                // extractor records a non-interface `Class` shape. Generic compiled names are
                // arity-suffixed (`Fun`2`), matching the emitted metadata name.
                match ExternalSymbols.tryReprTypeAt provider "Vesper.Fun`2" 0 with
                | ValueSome(struct (key, ExternalTypeShape.Class info)) ->
                    Expect.equal info.TyparArity 2 "Fun has two typars"

                    // The HOME is what the package wrapper stamps; the NAMESPACE is carried on
                    // the registered key, from the `.fsi`'s own header — no manifest declares one.
                    Expect.equal
                        info.Origin.Home.AssemblyOption
                        (ValueSome(AssemblyName "Vesper.Core"))
                        "origin assembly = Vesper.Core"

                    Expect.equal key.Namespace.Dotted "Vesper" "key namespace = Vesper, from the file header"
                | other -> failtestf "expected Vesper.Fun as Class shape, got %A" other
            }

            test "short names no longer resolve through the provider directly (O3)" {
                // The namespace retry lives in the ambient open scope, probed by the pipeline
                // behind explicit opens, so a bare `int` is a miss at the provider surface.
                let provider, _ = builtProvider.Value

                Expect.isTrue
                    (ExternalSymbols.tryReprType provider "int" |> ValueOption.isNone)
                    "bare int is a provider miss"
            }

            test "the contract surfaces its [<AutoOpen>] modules + the language prelude as the ambient prefix set" {
                let provider, _ = builtProvider.Value
                let prefixes = provider.AmbientOpenPrefixes
                // `ops-platform.fsi`'s `[<AutoOpen>]` modules, then the fixed prelude. The
                // prelude is a language constant: a package declaring no `Vesper.Collections`
                // type still carries the prefix, and it resolves nothing.
                Expect.isTrue
                    (List.contains "Vesper.ArithmeticOperators" prefixes)
                    "ArithmeticOperators auto-open surfaced"

                Expect.isTrue (List.contains "Vesper.Operators" prefixes) "Operators (hash) auto-open surfaced"

                Expect.equal
                    (prefixes |> List.skip (prefixes.Length - RuntimeNames.preludeNamespaces.Length))
                    RuntimeNames.preludeNamespaces
                    "the prelude comes LAST (probed after the AutoOpen modules)"
            }

            test "an unknown type misses" {
                let provider, _ = builtProvider.Value

                Expect.isTrue
                    (ExternalSymbols.tryReprType provider "NoSuchType" |> ValueOption.isNone)
                    "unknown type miss"
            }

            // A program resolves `+` and `hash` purely through the contract's `[<AutoOpen>]`
            // operator modules via the ambient open scope — no MockBuiltins, no explicit `open`.
            test "operators + hash resolve from the Vesper.Core contract, no MockBuiltins" {
                let provider, _ = builtProvider.Value

                let input = "let r = 1 + 2\nlet h = hash 5"
                let lexed, file = parseFile input

                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                // `op_Addition` and `hash` each found a home in the auto-opened operator modules.
                let failures =
                    ctx.Diagnostics
                    |> Diagnostic.errors
                    |> Seq.map (fun d -> d.Message)
                    |> List.ofSeq

                Expect.isEmpty failures (sprintf "expected clean resolution against the contract, got: %A" failures)

                // `r : int` via the SRTP `(+)`'s `default ^T : int` chain, typed from the contract.
                let rIdx = input.IndexOf "let r" + 4

                match ctx.Bindings.TypeVar.TryGetValue(NodeKey.ofSource rIdx NodeKind.PatIdent) with
                | ValueSome tv ->
                    match Unification.zonk ctx.Store (TyVar tv) with
                    | TyConst(k, _) when SymbolKeyOps.typeSimpleName k = DisplayName "int" -> ()
                    | other -> failtestf "Expected r : int, got %A" other
                | ValueNone -> failtest "no TypeVar for r"
            }

            // A `use` bound variable must implement `disposable`, matching F#. This
            // contract-only provider surfaces no BCL `System.IDisposable` interface, so only
            // the REJECTED direction is testable here; the accepted one needs a full provider.
            let analyseErrors (provider: IExternalSymbolProvider) (input: string) =
                let lexed, file = parseFile input

                let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

                Desugar.run ctx file
                NameResolution.run ctx file
                Unification.run ctx file

                ctx.Diagnostics
                |> Diagnostic.errors
                |> Seq.map (fun d -> d.Message)
                |> List.ofSeq

            let analyseDiagnostics (provider: IExternalSymbolProvider) (input: string) =
                analyseErrors provider input
                |> List.exists (fun m -> m.Contains "implement 'disposable'")

            test "`use` over a type with a `Dispose` method but no `disposable` impl is rejected (interface-required)" {
                let provider, _ = builtProvider.Value

                let input =
                    String.concat
                        "\n"
                        [
                            "type Res() ="
                            "    member this.Dispose () = ()"
                            "let run () ="
                            "    use r = Res()"
                            "    ()"
                            "run ()"
                        ]

                Expect.isTrue
                    (analyseDiagnostics provider input)
                    "a duck-typed `Dispose` (no `System.IDisposable`) no longer qualifies for `use`"
            }

            // `IsByRefLikeAttribute` is a BCL declaration with no Vesper counterpart, and this
            // provider carries no platform metadata, so the attribute is an unresolved-attribute
            // error, as on the JS target. The accepted direction (`use` calling
            // the pattern `Dispose` on a byref-like) runs against real BCL metadata in
            // `Codegen.Clr.Tests/StructTests.fs`.
            test "`[<IsByRefLike>]` with no platform metadata is an unresolved-attribute error" {
                let provider, _ = builtProvider.Value

                let input =
                    String.concat
                        "\n"
                        [
                            "[<Struct; System.Runtime.CompilerServices.IsByRefLike>]"
                            "type Res ="
                            "    member this.Dispose () = ()"
                            "let run () ="
                            "    use r = Res()"
                            "    ()"
                            "run ()"
                        ]

                Expect.isTrue
                    (analyseDiagnostics provider input)
                    "a BCL-only attribute does not resolve without the platform metadata that declares it"
            }

            // `buildClosure` closes a root manifest set over `depends-on` and orders it dependencies-first.
            testList
                "buildClosure"
                [
                    test "pulls a transitive dependency into the closure (List ⇒ + Core)" {
                        // `Vesper.List` lists `Vesper.Core` only via `depends-on`; the closure
                        // resolves that to the sibling directory.
                        let manifests =
                            ReferencedProject.buildClosure [ vesperListManifest ]
                            |> PackageFaults.okOrFail "buildClosure"

                        let ordered = manifests |> List.map (fun m -> m.Path)
                        let coreFull = vesperCoreManifest
                        let listFull = vesperListManifest
                        Expect.contains ordered coreFull "Core pulled into the closure"
                        Expect.contains ordered listFull "List itself present"

                        Expect.isLessThan
                            (List.findIndex ((=) coreFull) ordered)
                            (List.findIndex ((=) listFull) ordered)
                            "Core ordered before List"
                    }

                    test "reorders a dependent-first input dependencies-first" {
                        // Post-order topo sort, so the input order does not survive.
                        let manifests =
                            ReferencedProject.buildClosure [ vesperListManifest; vesperCoreManifest ]
                            |> PackageFaults.okOrFail "buildClosure"

                        let ordered = manifests |> List.map (fun m -> m.Path)
                        let coreFull = vesperCoreManifest
                        let listFull = vesperListManifest

                        Expect.isLessThan
                            (List.findIndex ((=) coreFull) ordered)
                            (List.findIndex ((=) listFull) ordered)
                            "Core ordered before List despite being listed second"
                    }

                    test "empty root set closes to nothing" {
                        match ReferencedProject.buildClosure [] with
                        | Result.Ok [] -> ()
                        | other -> failtestf "expected Ok [], got %A" other
                    }

                    test "a depends-on cycle is a hard error" {
                        let a = writeSyntheticManifest "CycleA" [ "CycleB" ]
                        writeSyntheticManifest "CycleB" [ "CycleA" ] |> ignore

                        match ReferencedProject.buildClosure [ a ] with
                        | Result.Ok ordered -> failtestf "expected a cycle error, got Ok %A" ordered
                        | Result.Error(PackageSetFault.UnresolvedDependency detail) ->
                            Expect.stringContains detail "cycle" "the fault names the cycle"
                        | Result.Error e -> failtestf "expected UnresolvedDependency, got %A" e
                    }

                    test "a missing dependency manifest is a hard error" {
                        let p = writeSyntheticManifest "NeedsGhost" [ "NoSuchPackage" ]

                        match ReferencedProject.buildClosure [ p ] with
                        | Result.Ok ordered -> failtestf "expected a missing-dependency error, got Ok %A" ordered
                        | Result.Error e ->
                            Expect.equal
                                e
                                (PackageSetFault.NoManifestForTarget("NoSuchPackage", "none"))
                                "the fault names the absent package and the target"
                    }

                    // Each package's TRANSITIVE `depends-on` closure, so composition can scope a
                    // package's ambient to its declared dependencies rather than to every
                    // topological predecessor.
                    test "buildClosureWithDeps reports the transitive depends-on closure" {
                        // A → B → C: A's closure must include C even though A never references it.
                        writeSyntheticManifest "ClosureC" [] |> ignore
                        writeSyntheticManifest "ClosureB" [ "ClosureC" ] |> ignore
                        let a = writeSyntheticManifest "ClosureA" [ "ClosureB" ]

                        let _, transitiveDeps =
                            ReferencedProject.buildClosureWithDeps [ a ]
                            |> PackageFaults.okOrFail "buildClosureWithDeps"

                        let pathOf name =
                            resolveOrFail "none" (Path.Combine(tmpSrc, name))

                        let depsA = transitiveDeps (pathOf "ClosureA")
                        Expect.contains depsA (pathOf "ClosureB") "A's direct dependency B"
                        Expect.contains depsA (pathOf "ClosureC") "A's transitive dependency C"

                        Expect.equal (transitiveDeps (pathOf "ClosureB")) [ pathOf "ClosureC" ] "B depends on C only"

                        Expect.equal (transitiveDeps (pathOf "ClosureC")) [] "C is dependency-free"
                    }

                    test "buildClosureWithDeps excludes a non-dependency that merely sorts earlier" {
                        writeSyntheticManifest "IndepD" [] |> ignore
                        let e = writeSyntheticManifest "IndepE" []

                        let _, transitiveDeps =
                            ReferencedProject.buildClosureWithDeps
                                [ resolveOrFail "none" (Path.Combine(tmpSrc, "IndepD")); e ]
                            |> PackageFaults.okOrFail "buildClosureWithDeps"

                        let pathOf name =
                            resolveOrFail "none" (Path.Combine(tmpSrc, name))

                        Expect.equal
                            (transitiveDeps (pathOf "IndepE"))
                            []
                            "E declares no dependency on D despite D sorting earlier"
                    }

                    // A qualified type name owned by two peer packages would resolve as a silent
                    // first-hit shadow, the loser's type minted but unreachable, so composition
                    // refuses it — a CS0433-equivalent citing both homes.
                    test "composition rejects the same type declared by two peer packages" {
                        let a =
                            writeSyntheticPackageWithType "DupPkgA" "Dup" "type Thing =\n    | A\n    | B"

                        let b =
                            writeSyntheticPackageWithType "DupPkgB" "Dup" "type Thing =\n    | C\n    | D"

                        let composed =
                            PackageProviders.composeContract PackageProviders.noPlatformMetadata [ a; b ]

                        match
                            composed.Diagnostics
                            |> List.tryPick (fun d ->
                                match d.Diagnostic.Kind with
                                | Kind.PackageSet(PackageSetFault.DuplicateType _) -> Some d.Diagnostic.Message
                                | _ -> None
                            )
                        with
                        | None ->
                            failtestf
                                "expected a duplicate-type diagnostic, got: %A"
                                (composed.Diagnostics |> List.map (fun d -> d.Diagnostic.Message))
                        | Some m ->
                            Expect.stringContains m "Dup.Thing" "names the clashing type"
                            Expect.stringContains m "DupPkgA" "names the first home assembly"
                            Expect.stringContains m "DupPkgB" "names the second home assembly"
                    }

                    // The duplicate check keys on the home assembly, so one package's own type
                    // is not a clash.
                    test "composition accepts one package's own type (no false positive)" {
                        let a =
                            writeSyntheticPackageWithType "SoloPkg" "Solo" "type Thing =\n    | A\n    | B"

                        let composed =
                            PackageProviders.composeContract PackageProviders.noPlatformMetadata [ a ]

                        Expect.isEmpty
                            (composed.Diagnostics
                             |> List.filter (fun d ->
                                 match d.Diagnostic.Kind with
                                 | Kind.PackageSet _ -> true
                                 | _ -> false
                             ))
                            "one package's own type is not a clash"
                    }

                    // The directory name IS the package identity, and the assembly name it emits
                    // under, so a diverging `[core] name` is rejected at parse time.
                    test "a [core] name diverging from the directory name is rejected" {
                        // `writeSyntheticManifest` always matches name to directory, so this one
                        // is hand-written to diverge.
                        let path = writeManifest "DivergeDir" "[core]\nname = \"Mismatch\"\nfiles = []\n"

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> failtestf "expected a name/dir mismatch error, got Ok %A" m
                        | Result.Error e ->
                            let e = PackageSetFault.describe e
                            Expect.stringContains e "Mismatch" "error names the declared name"
                            Expect.stringContains e "DivergeDir" "error names the directory"
                    }

                    test "an omitted [core] name falls back to the directory name (no divergence)" {
                        match ReferencedProject.loadManifest (writeManifest "NoName" "[core]\nfiles = []\n") with
                        | Result.Ok m -> Expect.equal m.Name "NoName" "name falls back to directory name"
                        | Result.Error e -> failtestf "expected Ok, got Error %s" (PackageSetFault.describe e)
                    }

                    // A package participates in a target by publishing `manifest.<target>.toml`
                    // and in no other way, so a bare `manifest.toml` does not declare a target and is
                    // not resolved for any. `Vesper.Set` is the real instance: clr only.
                    test "a package that publishes no manifest for the target does not resolve" {
                        let dir = Path.Combine(tmpSrc, "ClrOnlyPkg")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "manifest.clr.toml"), "[core]\nfiles = []\n")
                        File.WriteAllText(Path.Combine(dir, "manifest.toml"), "[core]\nfiles = []\n")

                        Expect.isOk (ReferencedProject.resolveManifest "clr" dir) "clr is published"

                        match ReferencedProject.resolveManifest "js" dir with
                        | Result.Ok mp -> failtestf "expected no js manifest, got %A" mp.Path
                        // The CASE, not its English: the fault is a value, so a test asserts on
                        // what it says rather than on how it is worded.
                        | Result.Error e ->
                            Expect.equal
                                e
                                (PackageSetFault.NoManifestForTarget("ClrOnlyPkg", "js"))
                                "the fault names the package and the target"
                    }
                ]

            // One flat `[core]` per target: each list is the compile order, written once.
            testList
                "the flat per-target manifest"
                [
                    let jsManifest =
                        loadOrFail (
                            writeManifestFor
                                "js"
                                "FlatLists"
                                "[core]\n\
                                 files = [\"contract.fsi\", \"contract.js.fs\", \"shim.js.fsi\", \"shim.js.fs\", \"ops.fs\"]\n\
                                 runtime = [\"runtime.mjs\"]\n"
                        )

                    let sig' rel =
                        {
                            ReferencedProject.Relative = rel
                            ReferencedProject.Kind = SourceFileKind.Signature
                        }

                    let impl rel =
                        {
                            ReferencedProject.Relative = rel
                            ReferencedProject.Kind = SourceFileKind.Implementation
                        }

                    test "the file list is paired into units, each entry classified by extension" {
                        Expect.equal jsManifest.Target "js" "the file name states the target"

                        Expect.equal
                            jsManifest.Units
                            [
                                {
                                    Signature = ValueSome(sig' "contract.fsi")
                                    Implementation = impl "contract.js.fs"
                                }
                                {
                                    Signature = ValueSome(sig' "shim.js.fsi")
                                    Implementation = impl "shim.js.fs"
                                }
                                {
                                    Signature = ValueNone
                                    Implementation = impl "ops.fs"
                                }
                            ]
                            "units, in declared order, each `.fsi` over the companion it precedes"

                        Expect.equal
                            jsManifest.Files
                            [
                                sig' "contract.fsi"
                                impl "contract.js.fs"
                                sig' "shim.js.fsi"
                                impl "shim.js.fs"
                                impl "ops.fs"
                            ]
                            "flattened back to the order the manifest spells"

                        Expect.equal
                            (ReferencedProject.signatureFiles jsManifest)
                            [ "contract.fsi"; "shim.js.fsi" ]
                            "the signature half"

                        Expect.equal
                            (ReferencedProject.implementationFiles jsManifest)
                            [ "contract.js.fs"; "shim.js.fs"; "ops.fs" ]
                            "the implementation half"

                        Expect.equal jsManifest.Runtime [ "runtime.mjs" ] "runtime"
                    }

                    // A `.fsi` alone declares a surface this target compiles no body for, which
                    // is a stale manifest rather than a package that ships contracts only.
                    test "a .fsi with no companion .fs is rejected" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "OrphanSig" "[core]\nfiles = [\"deleted-impl.fsi\", \"ops.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an orphan-signature rejection, got Ok %A" m
                        | Result.Error e ->
                            let e = PackageSetFault.describe e
                            Expect.stringContains e "`deleted-impl.fsi`" "the error names the orphaned signature file"
                            Expect.stringContains e "deleted-impl.fs" "and the companion it expected"
                    }

                    // The typed list is total over `.fsi`/`.fs`; anything else read as silence
                    // would resolve a stale manifest to a plausible wrong file set.
                    test "a files entry that is neither .fsi nor .fs is rejected" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "StrayEntry" "[core]\nfiles = [\"notes.txt\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected a rejection of notes.txt, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains
                                (PackageSetFault.describe e)
                                "notes.txt"
                                "the error names the offending entry"
                    }

                    // One file read twice would compile twice; the parse refuses the repeat
                    // rather than deduplicating it.
                    test "a files entry listed twice is rejected" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "TwiceEntry" "[core]\nfiles = [\"ops.fs\", \"ops.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected a rejection of the repeat, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains (PackageSetFault.describe e) "twice" "the error names the repeat"
                    }

                    // The layout guarantee `Manifest.Files` states: a `.fsi` sits immediately
                    // ahead of its companion `.fs`, so key pairing and list layout agree.
                    test "a .fsi parted from its companion .fs is rejected" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "PartedPair" "[core]\nfiles = [\"x.fsi\", \"other.fs\", \"x.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an adjacency rejection, got Ok %A" m
                        | Result.Error e ->
                            let e = PackageSetFault.describe e
                            Expect.stringContains e "`x.fsi`" "the error names the signature file"
                            Expect.stringContains e "`x.fs`" "and the companion it must precede"
                    }

                    // Both `x.fs` and `x.js.fs` key on `x` under the js target, so one of them
                    // could never sit beside the `.fsi` — the parse refuses the pair outright.
                    test "a .fsi two .fs entries pair with is rejected" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifestFor
                                    "js"
                                    "TwoClaimants"
                                    "[core]\nfiles = [\"x.fsi\", \"x.fs\", \"x.js.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected a two-companion rejection, got Ok %A" m
                        | Result.Error e ->
                            let e = PackageSetFault.describe e
                            Expect.stringContains e "`x.fs`" "the error names the first claimant"
                            Expect.stringContains e "`x.js.fs`" "and the second"
                    }

                    // Extension off, then this manifest's own target suffix — how a `.fsi` finds
                    // its `.fs` body.
                    test "pairingKey strips the extension and this manifest's target suffix" {
                        Expect.equal (ReferencedProject.pairingKey "js" "ops.js.fs") "ops" "target suffix off"

                        Expect.equal (ReferencedProject.pairingKey "js" "ops.fs") "ops" "bare body"

                        Expect.equal
                            (ReferencedProject.pairingKey "js" "shim.js.fsi")
                            "shim"
                            "a `.js.fsi` signature file keys the same as its `.js.fs` body"

                        Expect.equal
                            (ReferencedProject.pairingKey "js" "ops.clr.fs")
                            "ops.clr"
                            "ANOTHER target's suffix is part of the key"
                    }

                    // The asset contents, read off disk and keyed by package name — what the JS
                    // backend imports and materialises into the package's own directory. Every
                    // listed file is read, in manifest order: a package ships as many as it names.
                    test "runtimeModules reads the asset contents keyed by package name" {
                        let dir = Path.Combine(tmpSrc, "RuntimeAsset")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "asset.mjs"), "export const k = 1;\n")
                        File.WriteAllText(Path.Combine(dir, "extra.mjs"), "export const j = 2;\n")

                        let jsPath =
                            writeManifestFor
                                "js"
                                "RuntimeAsset"
                                "[core]\nfiles = []\nruntime = [\"asset.mjs\", \"extra.mjs\"]\n"

                        let resolved = ReferencedProject.runtimeModules [ loadOrFail jsPath ]

                        Expect.equal
                            (resolved.ByPackage |> Map.tryFind "RuntimeAsset")
                            (Some
                                [
                                    {
                                        FileName = "asset.mjs"
                                        Source = "export const k = 1;\n"
                                    }
                                    {
                                        FileName = "extra.mjs"
                                        Source = "export const j = 2;\n"
                                    }
                                ])
                            "package RuntimeAsset → its assets read from disk, in manifest order"

                        Expect.isEmpty resolved.Missing "every declared asset was on disk"

                        let clrPath = writeManifestFor "clr" "RuntimeAsset" "[core]\nfiles = []\n"

                        Expect.isEmpty
                            (ReferencedProject.runtimeModules [ loadOrFail clrPath ]).ByPackage
                            "the same package's clr manifest lists no asset, so it resolves to an empty map"
                    }

                    // A declared asset absent from disk would otherwise leave the backend
                    // importing a `.mjs` nothing materialises.
                    test "runtimeModules faults on a declared asset that is not on disk" {
                        let dir = Path.Combine(tmpSrc, "MissingRuntimeAsset")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "present.mjs"), "export const k = 1;\n")

                        let jsPath =
                            writeManifestFor
                                "js"
                                "MissingRuntimeAsset"
                                "[core]\nfiles = []\nruntime = [\"present.mjs\", \"absent.mjs\"]\n"

                        let resolved = ReferencedProject.runtimeModules [ loadOrFail jsPath ]

                        Expect.equal
                            resolved.Missing
                            [ PackageSetFault.FileMissing("MissingRuntimeAsset", "absent.mjs") ]
                            "the absent asset is reported, and only it"

                        Expect.isFalse
                            (resolved.ByPackage.ContainsKey "MissingRuntimeAsset")
                            "the incomplete package contributes no assets, so `present.mjs` is not promoted to its entry"
                    }

                    // One package's absent asset must not take another package's assets with it.
                    test "runtimeModules keeps a sound package's assets beside a faulted one" {
                        let goodDir = Path.Combine(tmpSrc, "SoundRuntimeAsset")
                        Directory.CreateDirectory goodDir |> ignore
                        File.WriteAllText(Path.Combine(goodDir, "sound.mjs"), "export const s = 1;\n")

                        let goodPath =
                            writeManifestFor "js" "SoundRuntimeAsset" "[core]\nfiles = []\nruntime = [\"sound.mjs\"]\n"

                        let badDir = Path.Combine(tmpSrc, "FaultedRuntimeAsset")
                        Directory.CreateDirectory badDir |> ignore

                        let badPath =
                            writeManifestFor "js" "FaultedRuntimeAsset" "[core]\nfiles = []\nruntime = [\"gone.mjs\"]\n"

                        let resolved =
                            ReferencedProject.runtimeModules [ loadOrFail badPath; loadOrFail goodPath ]

                        Expect.equal
                            (resolved.ByPackage |> Map.tryFind "SoundRuntimeAsset")
                            (Some
                                [
                                    {
                                        FileName = "sound.mjs"
                                        Source = "export const s = 1;\n"
                                    }
                                ])
                            "the sound package resolves independently of the faulted one"

                        Expect.equal
                            resolved.Missing
                            [ PackageSetFault.FileMissing("FaultedRuntimeAsset", "gone.mjs") ]
                            "the faulted package's absent asset is still reported"
                    }

                    // `depends-on` is taken for THIS manifest's target, so a package may depend on
                    // a peer on one target and not on another.
                    test "a dependency resolves to the sibling package's manifest for the same target" {
                        writeManifestFor "js" "DepOnlyJs" "[core]\nname = \"DepOnlyJs\"\nfiles = []\n"
                        |> ignore

                        let dependent =
                            writeManifestFor
                                "js"
                                "NeedsJsDep"
                                "[core]\nname = \"NeedsJsDep\"\ndepends-on = [\"../DepOnlyJs\"]\nfiles = []\n"

                        // The clr collection of the same package declares the SAME dependency, but
                        // the dependency ships no clr manifest, so a clr closure over it must fail.
                        let clrDependent =
                            writeManifestFor
                                "clr"
                                "NeedsJsDep"
                                "[core]\nname = \"NeedsJsDep\"\ndepends-on = [\"../DepOnlyJs\"]\nfiles = []\n"

                        let ordered =
                            ReferencedProject.buildClosure [ dependent ]
                            |> PackageFaults.okOrFail "js closure"

                        Expect.contains
                            (ordered |> List.map (fun m -> m.Path))
                            (resolveOrFail "js" (Path.Combine(tmpSrc, "DepOnlyJs")))
                            "the js dependency is the js manifest beside it"

                        match ReferencedProject.buildClosure [ clrDependent ] with
                        | Result.Ok ordered -> failtestf "expected a missing clr dependency, got Ok %A" ordered
                        | Result.Error e ->
                            Expect.stringContains
                                (PackageSetFault.describe e)
                                "DepOnlyJs"
                                "a package absent for a target is named, not silent"
                    }

                    // An undefined key is a parse ERROR, not silence: a dashed-suffix spelling
                    // like `impl-js` would otherwise resolve to a plausible wrong file set.
                    test "an unknown [core] key is rejected" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "StaleCore" "[core]\nfiles = []\nimpl-js = [\"ops.js.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an unknown-key error, got Ok %A" m
                        | Result.Error e ->
                            let e = PackageSetFault.describe e
                            Expect.stringContains e "impl-js" "the error names the offending key"
                            Expect.stringContains e "core" "and the table it was found in"
                    }

                    // The `.fs` entries are listed in `files` now, so a manifest still declaring
                    // the retired second list must error: read as silence it resolves to a
                    // package missing every body and splice source that list held.
                    test "the retired `impl` key is rejected, not ignored" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "RetiredCore" "[core]\nfiles = []\nimpl = [\"ops.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an unknown-key error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains (PackageSetFault.describe e) "impl" "the error names the retired key"
                    }

                    // A `.fs` owes no signature file, so there is nothing to exempt. The key erroring
                    // rather than being ignored is what tells a manifest still carrying it that
                    // the rule it waived no longer exists.
                    test "the retired `impl-only` key is rejected, not ignored" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "RetiredImplOnly" "[core]\nfiles = []\nimpl-only = [\"ops.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an unknown-key error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains
                                (PackageSetFault.describe e)
                                "impl-only"
                                "the error names the retired key"
                    }

                    // A `[targets.<t>]` table is the retired two-tier shape; read as silence it
                    // would resolve a stale manifest to a file set missing everything it held.
                    test "the retired [targets.<t>] table is rejected, not ignored" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest
                                    "RetiredTargets"
                                    "[core]\nfiles = []\n\n[targets.js]\nimpl = [\"ops.js.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected a rejection of [targets.js], got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains
                                (PackageSetFault.describe e)
                                "targets"
                                "the error names the retired table"
                    }
                ]

            // `sourceInputs` reports the sources a provider build reads, which is what a list
            // NAMES and no runtime asset. A caller deriving a package's inputs from it and the
            // build disagreeing means the caller misses a file the build resolves through.
            testList
                "sourceInputs covers what the provider build reads"
                [
                    test "sourceInputs returns every file-list path, and no runtime asset" {
                        let m =
                            loadOrFail (
                                writeManifestFor
                                    "js"
                                    "AllLists"
                                    "[core]\n\
                                     files = [\"contract.fsi\", \"contract.js.fs\", \"shim.js.fsi\", \"shim.js.fs\", \"ops.fs\"]\n\
                                     runtime = [\"x.mjs\"]\n"
                            )

                        let inputs = ReferencedProject.sourceInputs m

                        Expect.equal
                            (List.sort inputs)
                            (List.sort [ "contract.fsi"; "contract.js.fs"; "shim.js.fsi"; "shim.js.fs"; "ops.fs" ])
                            "every file-list entry"

                        Expect.isFalse (List.contains "x.mjs" inputs) "a runtime asset is not a parsed source"
                    }

                    test "every real package's listed sources are folded" {
                        // Across the shipped manifests: every `.fs` and `.fsi` the target compiles.
                        for manifest in [ vesperCoreManifest; vesperCoreJsManifest; vesperListManifest ] do
                            let m = loadOrFail manifest
                            let inputs = ReferencedProject.sourceInputs m

                            for rel in ReferencedProject.implementationFiles m @ ReferencedProject.signatureFiles m do
                                Expect.contains inputs rel (sprintf "%s (%s): %s is folded" m.Name m.Target rel)
                    }
                ]

            // A `ManifestPath` is minted only by `resolveManifest`, which takes the target, so
            // the file and the target it is read under are one value. There is no mixed-target
            // set to reject: the closure resolves each dependency at its dependent's target.
            testList
                "a manifest carries the target it was resolved for"
                [
                    test "resolving one package for two targets gives two distinct manifests" {
                        Expect.equal vesperCoreManifest.Target "clr" "the clr resolution"
                        Expect.equal vesperCoreJsManifest.Target "js" "the js resolution"

                        Expect.notEqual vesperCoreManifest.Path vesperCoreJsManifest.Path "and they are different files"

                        Expect.equal vesperCoreManifest.PackageDir vesperCoreJsManifest.PackageDir "of the same package"
                    }

                    test "a closure carries its roots' target throughout" {
                        let ordered =
                            ReferencedProject.buildClosure [ vesperCoreJsManifest ]
                            |> PackageFaults.okOrFail "buildClosure"

                        Expect.isNonEmpty ordered "the closure is not empty"

                        for m in ordered do
                            Expect.equal m.Target "js" (sprintf "%s resolved for js" m.Path.Path)
                    }
                ]

            // A package that builds for two targets writes each shared name twice, and nothing
            // in the format ties the two files together. `name` cannot drift (it is checked
            // against the directory name); every list can.
            testList
                "the per-target manifests of one package agree wherever neither names a target"
                [
                    // An entry whose pairing key is its own name carries no `.<target>` segment,
                    // so it is a name BOTH manifests have to spell. One that does carry a segment
                    // is that target's own file, and differing is what it is for.
                    let neutral (m: ReferencedProject.Manifest) (entries: string list) =
                        entries
                        |> List.filter (fun rel ->
                            ReferencedProject.pairingKey m.Target rel = Path.ChangeExtension(rel, null)
                        )

                    let sharedLists (m: ReferencedProject.Manifest) =
                        [
                            "depends-on", m.DependsOn
                            "files", neutral m (ReferencedProject.sourceInputs m)
                        ]

                    test "every divergence is a declared one" {
                        // A name one target carries and the other does not can be a real
                        // statement about the target rather than drift, so these are PINNED,
                        // not banned. Each below is explained by the manifest that declares it; a
                        // NEW entry is two manifests that drifted.
                        let expected =
                            [
                                // decimal / nativeint / nd-array are types JS does not have, so
                                // their contracts are ABSENT from the js manifest; a js program
                                // writing one is rejected off the language-known key. And a CLR
                                // function value is a nominal `Fun` interface, so adapting
                                // flat<->curried needs a reified object; JS applies directly.
                                {|
                                    Package = "Vesper.Core"
                                    List = "files"
                                    ClrOnly =
                                        [
                                            "prim-types-decimal.fsi"
                                            "prim-types-nativeint.fsi"
                                            "prim-types-nd-array.fsi"
                                            "fun-adapters.fsi"
                                            "fun-adapters.fs"
                                        ]
                                    JsOnly = []
                                |}
                                // The CLR `%A` engine builds its `Doc` child lists on the
                                // cons-list; the JS one is a free function over its own frames.
                                {|
                                    Package = "Vesper.Printf"
                                    List = "depends-on"
                                    ClrOnly = [ "../Vesper.List" ]
                                    JsOnly = []
                                |}
                                // `formatter.fsi` references `TextWriter`/`StringBuilder`/`IsByRefLike`
                                // and `structural-printer.fsi` an interface-dispatching state.
                                {|
                                    Package = "Vesper.Printf"
                                    List = "files"
                                    ClrOnly = [ "structural-printer.fsi"; "formatter.fsi" ]
                                    JsOnly = []
                                |}
                                // It references the NON-generic `IEnumerable`/`IEnumerator`, which the
                                // JS capability shim does not map.
                                {|
                                    Package = "Vesper.Seq"
                                    List = "files"
                                    ClrOnly = [ "struct-seq.fsi" ]
                                    JsOnly = []
                                |}
                            ]

                        let actual =
                            [
                                for dir in Directory.GetDirectories(srcDir, "Vesper.*") |> Array.sort do
                                    match
                                        ReferencedProject.resolveManifest "clr" dir,
                                        ReferencedProject.resolveManifest "js" dir
                                    with
                                    | Result.Ok clr, Result.Ok js ->
                                        for (list, clrEntries), (_, jsEntries) in
                                            List.zip (sharedLists (loadOrFail clr)) (sharedLists (loadOrFail js)) do
                                            if clrEntries <> jsEntries then
                                                {|
                                                    Package = Path.GetFileName dir
                                                    List = list
                                                    ClrOnly = clrEntries |> List.except jsEntries
                                                    JsOnly = jsEntries |> List.except clrEntries
                                                |}
                                    // A package that builds for one target has no divergence.
                                    | _ -> ()
                            ]

                        Expect.equal actual expected "the per-target divergences over target-neutral names"
                    }
                ]
        ]
