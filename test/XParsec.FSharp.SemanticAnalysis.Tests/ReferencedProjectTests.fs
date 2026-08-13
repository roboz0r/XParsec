module XParsec.FSharp.SemanticAnalysis.Tests.ReferencedProjectTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `Vesper.Core` stood up from its real manifests: what its contract resolves, and with
// which identities.

/// `src/Vesper.Core`, found by walking up from the test assembly.
let private vesperCorePackage =
    let testDir = Path.GetDirectoryName(typeof<VesperLib.LibFile>.Assembly.Location)
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
    match ReferencedProject.resolveManifest target packageDir with
    | Result.Ok mp -> mp
    | Result.Error e -> failwithf "resolveManifest: %s" e

/// Parse a resolved manifest, failing the test if it is malformed.
let private loadOrFail (mp: ReferencedProject.ManifestPath) : ReferencedProject.Manifest =
    match ReferencedProject.loadManifest mp with
    | Result.Ok m -> m
    | Result.Error e -> failwithf "loadManifest: %s" e

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

/// `writeManifestFor` at clr, the target most of these fixtures are indifferent to.
let private writeManifest (name: string) (body: string) : ReferencedProject.ManifestPath =
    writeManifestFor "clr" name body

/// A synthetic package with no sources: `files = []` keeps it parse-valid with no `.fsi`.
let private writeSyntheticManifest (name: string) (dependsOn: string list) : ReferencedProject.ManifestPath =
    let deps = dependsOn |> List.map (sprintf "\"%s\"") |> String.concat ", "
    writeManifest name (sprintf "[core]\nname = \"%s\"\ndepends-on = [%s]\nfiles = []\n" name deps)

/// Writes a synthetic package whose `contract.fsi` declares `fsiBody` under `ns` — for the
/// duplicate-type tests, which need two packages declaring one name.
let private writeSyntheticPackageWithType
    (name: string)
    (ns: string)
    (fsiBody: string)
    : ReferencedProject.ManifestPath =
    let dir = Path.Combine(tmpSrc, name)
    Directory.CreateDirectory dir |> ignore
    File.WriteAllText(Path.Combine(dir, "contract.fsi"), sprintf "namespace %s\n%s\n" ns fsiBody)
    writeManifest name (sprintf "[core]\nname = \"%s\"\nfiles = [\"contract.fsi\"]\n" name)

let private builtProvider =
    lazy
        (match ReferencedProject.buildProvider vesperCoreManifest with
         | Result.Error e -> failwithf "buildProvider failed: %s" e
         | Result.Ok(provider, diags) -> provider, diags)

/// The same package's JS contract: no `.js.fs` capability reprs, plus the
/// `capabilities-compat.js.fsi` shim `manifest.js.toml` names and the clr one does not.
let private builtProviderJs =
    lazy
        (let bp =
            ReferencedProject.buildProviderWith (fun _ -> ValueNone) [] (loadOrFail vesperCoreJsManifest)

         bp.Provider, bp.Diagnostics)

[<Tests>]
let tests =
    testList
        "ReferencedProject"
        [
            test "manifest parses: name and files in compile order" {
                match ReferencedProject.loadManifest vesperCoreManifest with
                | Result.Error e -> failtestf "loadManifest failed: %s" e
                | Result.Ok m ->
                    // `Vesper.Core`'s `[core]` carries no `name`, so it falls back to the directory.
                    Expect.equal m.Name "Vesper.Core" "assembly name from dir"
                    Expect.equal m.Target "clr" "target from the file name"
                    Expect.isNonEmpty m.Files "files listed"
                    Expect.equal (List.head m.Files) "prim-types-min.fsi" "compile order: prim-types-min first"
            }

            test "every listed .fsi parses (no file-level diagnostics)" {
                let _, diags = builtProvider.Value
                Expect.isEmpty diags (sprintf "expected clean parse, got: %A" diags)
            }

            test "int resolves (qualified) as an Intrinsic shape carrying its `.fs` repr" {
                let provider, _ = builtProvider.Value

                // `int` is an `extern` paired with its sibling `.fs` `(# "System.Int32" #)`
                // binding, so it surfaces as an `Intrinsic`: `canon` is the `.fsi` name `int`,
                // `platform` the CLI repr codegen consumes. An intrinsic carries no `Origin`.
                match provider.TryLookupType "Vesper.int" |> ExternalSymbols.typeShapeOf with
                | ValueSome(ExternalTypeShape.Intrinsic {
                                                            Id = {
                                                                     Canon = canon
                                                                     Platform = IntrinsicPlatform.Repr platform
                                                                 }
                                                        }) ->
                    Expect.equal (SymbolKey.Type canon) (RuntimeNames.intKey) "int's canon identity is the `.fsi` name"

                    Expect.equal
                        platform
                        "System.Int32"
                        "int's platform name is its prim-types-min `.fs` CLI representation"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }

            // Discharging an `op_Addition` bound on `int` asks the provider under the key the
            // `TyConst` payload carries, and nothing else. Published under any other key the
            // lookup misses silently: operator-name synthesis answers identically for `int`.
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
                let input = ""

                let lexed =
                    match XParsec.FSharp.Lexer.Lexing.lexString input with
                    | Result.Error e -> failtestf "lex failed: %A" e
                    | Result.Ok lexed -> lexed

                let ctx = PassContext(provider, Hashing.originSourceOfText lexed)

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

                let expectCapability (lookup: string) (canonKey: SymbolKey) (platformExpected: string) =
                    match provider.TryLookupType lookup |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                        Expect.equal
                            (SymbolKey.Type iface.Canon)
                            canonKey
                            (sprintf "%s canon is the identity of its `.fsi` type, arity included" lookup)

                        Expect.equal
                            iface.Platform
                            platformExpected
                            (sprintf "%s platform name is its `.fs` CLR repr" lookup)

                        // A capability interface is ABSENT from the reverse-canon map:
                        // reconciliation rides the platform name above, not this map.
                        match Map.tryFind platformExpected provider.IntrinsicReverseCanon with
                        | None -> ()
                        | Some canons ->
                            failtestf
                                "capability interface %s must NOT enter the reverse-canon map; found %A"
                                lookup
                                canons
                    | other -> failtestf "expected %s as an IntrinsicInterface shape, got %A" lookup other

                expectCapability "Vesper.disposable" (RuntimeNames.primitiveKey "disposable") "System.IDisposable"

                expectCapability "Vesper.equatable`1" (RuntimeNames.primitiveKey "equatable`1") "System.IEquatable`1"

                expectCapability "Vesper.comparable`1" (RuntimeNames.primitiveKey "comparable`1") "System.IComparable`1"

                expectCapability
                    "Vesper.Collections.enumerator`1"
                    (SymbolKeyOps.typeKey "Vesper.Collections" ("enumerator`1"))
                    "System.Collections.Generic.IEnumerator`1"

                expectCapability
                    "Vesper.Collections.seq`1"
                    (SymbolKeyOps.typeKey "Vesper.Collections" ("seq`1"))
                    "System.Collections.Generic.IEnumerable`1"

                // `enumerator` inherits `disposable`, so the `use` / for-in disposability scan
                // needs that inherited interface on the capability shape.
                match
                    provider.TryLookupType "Vesper.Collections.enumerator`1"
                    |> ExternalSymbols.typeShapeOf
                with
                | ValueSome(ExternalTypeShape.IntrinsicInterface iface) ->
                    let ifaceNames =
                        iface.Interfaces |> EqArray.map (fun i -> SymbolKeyOps.qualifiedName i.Key)

                    Expect.isTrue
                        (ifaceNames |> EqArray.exists (fun n -> n.Contains "disposable"))
                        (sprintf "enumerator inherits disposable; Interfaces = %A" ifaceNames)
                | other -> failtestf "expected enumerator as IntrinsicInterface, got %A" other
            }

            test "JS build: capabilities are canon-only; BCL spellings resolve through the compat shim" {
                // The JS build omits the `.js.fs` capability reprs and appends the
                // `capabilities-compat.js.fsi` shim, so a capability is CANON-ONLY and the BCL
                // spelling is quarantined to the shim, reaching the canonical by abbreviation.
                let provider, _ = builtProviderJs.Value

                let expectCanonOnly (lookup: string) =
                    // No `(# … #)` repr binds a platform name here, so it is a plain interface
                    // `Class`, not the CLR `IntrinsicInterface`.
                    match provider.TryLookupType lookup |> ExternalSymbols.typeShapeOf with
                    | ValueSome(ExternalTypeShape.Class shape) ->
                        Expect.isTrue shape.IsInterface (sprintf "%s is a canon-only interface Class on JS" lookup)
                    | other -> failtestf "expected %s as a canon-only Class on JS, got %A" lookup other

                expectCanonOnly "Vesper.disposable"
                expectCanonOnly "Vesper.equatable`1"
                expectCanonOnly "Vesper.comparable`1"

                // The shim abbreviates each BCL spelling to the canonical capability, so
                // `interface System.IDisposable` records the canonical key on JS.
                let expectShimAbbrev (bcl: string) (canonQualified: string) =
                    match provider.TryLookupType bcl |> ExternalSymbols.typeShapeOf with
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

            test "JS build: EVERY capability is canon-only; a BCL spelling is not a name" {
                // A BCL spelling reaches JS only through the compat shim above, which is an
                // `Abbrev` that expands at the use. So no capability keys by one, and nothing
                // downstream may tag a JS type with one.
                let provider, _ = builtProviderJs.Value
                let caps = ExternalSymbols.resolveCapabilities provider

                // A capability's spelling is a compiled NAME in the contract, cut to the key
                // the identity compares on. Arity 0: the `` `1 `` suffix parses.
                let matchesSpelling (id: RuntimeNames.CapabilityIdentity) (compiled: string) =
                    id.Matches(SymbolKeyOps.qualifiedTypeKeyOf compiled 0)

                let expectCanonOnly
                    (name: string)
                    (cap: RuntimeNames.CapabilityIdentity voption)
                    (bcl: string)
                    (canon: string)
                    =
                    match cap with
                    | ValueSome id ->
                        Expect.equal id.CanonKey ValueNone (sprintf "%s is canon-only on JS" name)
                        Expect.isTrue (matchesSpelling id canon) (sprintf "%s matches its canonical key %s" name canon)
                        Expect.isFalse (matchesSpelling id bcl) (sprintf "%s's BCL spelling %s is not a name" name bcl)
                    | ValueNone -> failtestf "%s resolved to ValueNone on JS" name

                expectCanonOnly
                    "Enumerable"
                    caps.Enumerable
                    "System.Collections.Generic.IEnumerable`1"
                    "Vesper.Collections.seq`1"

                expectCanonOnly
                    "Enumerator"
                    caps.Enumerator
                    "System.Collections.Generic.IEnumerator`1"
                    "Vesper.Collections.enumerator`1"

                expectCanonOnly "Disposable" caps.Disposable "System.IDisposable" "Vesper.disposable"
                expectCanonOnly "Equatable" caps.Equatable "System.IEquatable`1" "Vesper.equatable`1"
                expectCanonOnly "Comparable" caps.Comparable "System.IComparable`1" "Vesper.comparable`1"
            }

            test "Fun resolves (qualified) as a Class shape with a non-empty Origin" {
                let provider, _ = builtProvider.Value

                // `type Fun<'A,'B>` parses as an anonymous (`= begin … end`) type, so the
                // extractor records a non-interface `Class` shape. Generic compiled names are
                // arity-suffixed (`Fun`2`), matching the emitted metadata name.
                match provider.TryLookupType "Vesper.Fun`2" with
                | ValueSome(struct (key, ExternalTypeShape.Class info)) ->
                    Expect.equal info.TyparArity 2 "Fun has two typars"

                    // The HOME is what the package wrapper stamps; the NAMESPACE rides the
                    // registered key, from the `.fsi`'s own header — no manifest declares one.
                    Expect.equal
                        info.Origin.Home.AssemblyOption
                        (ValueSome "Vesper.Core")
                        "origin assembly = Vesper.Core"

                    Expect.equal key.Namespace.Dotted "Vesper" "key namespace = Vesper, from the file header"
                | other -> failtestf "expected Vesper.Fun as Class shape, got %A" other
            }

            test "short names no longer resolve through the provider directly (O3)" {
                // The namespace retry lives in the ambient open scope, probed by the pipeline
                // behind explicit opens, so a bare `int` is a miss at the provider surface.
                let provider, _ = builtProvider.Value
                Expect.isTrue (provider.TryLookupType "int" |> ValueOption.isNone) "bare int is a provider miss"
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
                    "the prelude is the tail (probed after the AutoOpen modules)"
            }

            test "an unknown type misses" {
                let provider, _ = builtProvider.Value
                Expect.isTrue (provider.TryLookupType "NoSuchType" |> ValueOption.isNone) "unknown type miss"
            }

            // A program resolves `+` and `hash` purely through the contract's `[<AutoOpen>]`
            // operator modules via the ambient open scope — no MockBuiltins, no explicit `open`.
            test "operators + hash resolve from the Vesper.Core contract, no MockBuiltins" {
                let provider, _ = builtProvider.Value

                let input = "let r = 1 + 2\nlet h = hash 5"
                let lexed, file = parseFile input
                let ctx = PassContext(provider, Hashing.originSourceOfText lexed)
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
                    | TyConst(k, _) when SymbolKeyOps.simpleName k = DisplayName "int" -> ()
                    | other -> failtestf "Expected r : int, got %A" other
                | ValueNone -> failtest "no TypeVar for r"
            }

            // A `use` bound variable must implement `disposable`, matching F#. This
            // contract-only provider surfaces no BCL `System.IDisposable` interface, so only
            // the REJECTED direction is testable here; the accepted one needs a full provider.
            let analyseErrors (provider: IExternalSymbolProvider) (input: string) =
                let lexed, file = parseFile input
                let ctx = PassContext(provider, Hashing.originSourceOfText lexed)
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

            // A `[<IsByRefLike>]` type can't be boxed to `IDisposable`, so a duck-typed pattern
            // `Dispose()` is accepted (C#8 pattern-`using` parity) and called directly. That
            // path needs no capability resolution, so it is the accepted direction testable here.
            test "`use` over a `[<IsByRefLike>]` ref struct with a pattern `Dispose` is accepted (carve-out)" {
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

                Expect.isFalse
                    (analyseDiagnostics provider input)
                    "a ref struct exposing a pattern `Dispose` qualifies for `use` (the carve-out)"
            }

            // `buildClosure` closes a root manifest set over `depends-on` and orders it dependencies-first.
            testList
                "buildClosure"
                [
                    test "pulls a transitive dependency into the closure (List ⇒ + Core)" {
                        // `Vesper.List` names `Vesper.Core` only via `depends-on`; the closure
                        // resolves that to the sibling directory.
                        match ReferencedProject.buildClosure [ vesperListManifest ] with
                        | Result.Error e -> failtestf "buildClosure failed: %s" e
                        | Result.Ok manifests ->
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
                        match ReferencedProject.buildClosure [ vesperListManifest; vesperCoreManifest ] with
                        | Result.Error e -> failtestf "buildClosure failed: %s" e
                        | Result.Ok manifests ->
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
                        | Result.Error e -> Expect.stringContains e "cycle" "error names the cycle"
                    }

                    test "a missing dependency manifest is a hard error" {
                        let p = writeSyntheticManifest "NeedsGhost" [ "NoSuchPackage" ]

                        match ReferencedProject.buildClosure [ p ] with
                        | Result.Ok ordered -> failtestf "expected a missing-dependency error, got Ok %A" ordered
                        | Result.Error e -> Expect.stringContains e "buildClosure" "error is surfaced from buildClosure"
                    }

                    // Each package's TRANSITIVE `depends-on` closure, so composition can scope a
                    // package's ambient to its declared dependencies rather than to every
                    // topological predecessor.
                    test "buildClosureWithDeps reports the transitive depends-on closure" {
                        // A → B → C: A's closure must include C even though A never names it.
                        writeSyntheticManifest "ClosureC" [] |> ignore
                        writeSyntheticManifest "ClosureB" [ "ClosureC" ] |> ignore
                        let a = writeSyntheticManifest "ClosureA" [ "ClosureB" ]

                        match ReferencedProject.buildClosureWithDeps [ a ] with
                        | Result.Error e -> failtestf "buildClosureWithDeps failed: %s" e
                        | Result.Ok(_, transitiveDeps) ->
                            let pathOf name =
                                resolveOrFail "clr" (Path.Combine(tmpSrc, name))

                            let depsA = transitiveDeps (pathOf "ClosureA")
                            Expect.contains depsA (pathOf "ClosureB") "A's direct dependency B"
                            Expect.contains depsA (pathOf "ClosureC") "A's transitive dependency C"

                            Expect.equal
                                (transitiveDeps (pathOf "ClosureB"))
                                [ pathOf "ClosureC" ]
                                "B depends on C only"

                            Expect.equal (transitiveDeps (pathOf "ClosureC")) [] "C is dependency-free"
                    }

                    test "buildClosureWithDeps excludes a non-dependency that merely sorts earlier" {
                        writeSyntheticManifest "IndepD" [] |> ignore
                        let e = writeSyntheticManifest "IndepE" []

                        match
                            ReferencedProject.buildClosureWithDeps
                                [ resolveOrFail "clr" (Path.Combine(tmpSrc, "IndepD")); e ]
                        with
                        | Result.Error err -> failtestf "buildClosureWithDeps failed: %s" err
                        | Result.Ok(_, transitiveDeps) ->
                            let pathOf name =
                                resolveOrFail "clr" (Path.Combine(tmpSrc, name))

                            Expect.equal
                                (transitiveDeps (pathOf "IndepE"))
                                []
                                "E declares no dependency on D despite D sorting earlier"
                    }

                    // A qualified type name owned by two peer packages would resolve as a silent
                    // first-hit shadow, the loser's type minted but unreachable, so composition
                    // refuses it — a CS0433-equivalent naming both homes.
                    test "composition rejects the same type declared by two peer packages" {
                        let a =
                            writeSyntheticPackageWithType "DupPkgA" "Dup" "type Thing =\n    | A\n    | B"

                        let b =
                            writeSyntheticPackageWithType "DupPkgB" "Dup" "type Thing =\n    | C\n    | D"

                        let caught =
                            try
                                ReferencedProject.composeContract ReferencedProject.noMetaTail [ a; b ]
                                |> ignore

                                None
                            with ex ->
                                Some ex.Message

                        match caught with
                        | None -> failtest "expected a duplicate-type composition error, got none"
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

                        ReferencedProject.composeContract ReferencedProject.noMetaTail [ a ] |> ignore
                    }

                    // The directory name IS the package identity — `depends-on` resolves against
                    // it — so a diverging `[core] name` is rejected at parse time.
                    test "a [core] name diverging from the directory name is rejected" {
                        // `writeSyntheticManifest` always matches name to directory, so this one
                        // is hand-written to diverge.
                        let path = writeManifest "DivergeDir" "[core]\nname = \"Mismatch\"\nfiles = []\n"

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> failtestf "expected a name/dir mismatch error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains e "Mismatch" "error names the declared name"
                            Expect.stringContains e "DivergeDir" "error names the directory"
                    }

                    test "an omitted [core] name falls back to the directory name (no divergence)" {
                        match ReferencedProject.loadManifest (writeManifest "NoName" "[core]\nfiles = []\n") with
                        | Result.Ok m -> Expect.equal m.Name "NoName" "name falls back to directory name"
                        | Result.Error e -> failtestf "expected Ok, got Error %s" e
                    }

                    // A package participates in a target by publishing `manifest.<target>.toml`
                    // and in no other way, so a bare `manifest.toml` does not name a target and is
                    // not resolved for any. `Vesper.Set` is the real instance: clr only.
                    test "a package that publishes no manifest for the target does not resolve" {
                        let dir = Path.Combine(tmpSrc, "ClrOnlyPkg")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "manifest.clr.toml"), "[core]\nfiles = []\n")
                        File.WriteAllText(Path.Combine(dir, "manifest.toml"), "[core]\nfiles = []\n")

                        Expect.isOk (ReferencedProject.resolveManifest "clr" dir) "clr is published"

                        match ReferencedProject.resolveManifest "js" dir with
                        | Result.Ok mp -> failtestf "expected no js manifest, got %A" mp.Path
                        | Result.Error e ->
                            Expect.stringContains e "does not build for target" "the error says so plainly"
                            Expect.stringContains e "ClrOnlyPkg" "and names the package"
                    }
                ]

            // One flat `[core]` per target: each list is the compile order, written once.
            testList
                "the flat per-target manifest"
                [
                    let jsManifest =
                        let path =
                            writeManifestFor
                                "js"
                                "FlatLists"
                                "[core]\n\
                                 files = [\"contract.fsi\", \"shim.js.fsi\"]\n\
                                 impl = [\"ops.fs\", \"ops.js.fs\"]\n\
                                 sig-only = [\"shim.js.fsi\"]\n\
                                 impl-only = [\"ops.js.fs\"]\n\
                                 runtime = [\"runtime.mjs\"]\n"

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> m
                        | Result.Error e -> failwithf "loadManifest failed: %s" e

                    test "every list is read verbatim, in the declared order" {
                        Expect.equal jsManifest.Target "js" "the file name states the target"
                        Expect.equal jsManifest.Files [ "contract.fsi"; "shim.js.fsi" ] "files"
                        Expect.equal jsManifest.Impl [ "ops.fs"; "ops.js.fs" ] "impl"
                        Expect.equal jsManifest.SigOnly [ "shim.js.fsi" ] "sig-only"
                        Expect.equal jsManifest.ImplOnly [ "ops.js.fs" ] "impl-only"
                        Expect.equal jsManifest.Runtime [ "runtime.mjs" ] "runtime"
                    }

                    // Extension off, then this manifest's own target suffix — how a `.fsi` finds
                    // its `.fs` body.
                    test "pairingKey strips the extension and this manifest's target suffix" {
                        Expect.equal (ReferencedProject.pairingKey jsManifest "ops.js.fs") "ops" "target suffix off"

                        Expect.equal (ReferencedProject.pairingKey jsManifest "ops.fs") "ops" "bare body"

                        Expect.equal
                            (ReferencedProject.pairingKey jsManifest "shim.js.fsi")
                            "shim"
                            "a `.js.fsi` contract keys the same as its `.js.fs` body"

                        Expect.equal
                            (ReferencedProject.pairingKey jsManifest "ops.clr.fs")
                            "ops.clr"
                            "ANOTHER target's suffix is part of the key"
                    }

                    // The asset's contents, read off disk and keyed by package name — what the
                    // JS backend imports and materialises beside its output.
                    test "runtimeModules reads the asset contents keyed by package name" {
                        let dir = Path.Combine(tmpSrc, "RuntimeAsset")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "asset.mjs"), "export const k = 1;\n")

                        let jsPath =
                            writeManifestFor "js" "RuntimeAsset" "[core]\nfiles = []\nruntime = [\"asset.mjs\"]\n"

                        Expect.equal
                            (ReferencedProject.runtimeModules [ loadOrFail jsPath ]
                             |> Map.tryFind "RuntimeAsset")
                            (Some("asset.mjs", "export const k = 1;\n"))
                            "package RuntimeAsset → (fileName, source) read from disk"

                        let clrPath = writeManifestFor "clr" "RuntimeAsset" "[core]\nfiles = []\n"

                        Expect.isEmpty
                            (ReferencedProject.runtimeModules [ loadOrFail clrPath ])
                            "the same package's clr manifest lists no asset, so it resolves to an empty map"
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
                                "[core]\nname = \"NeedsJsDep\"\ndepends-on = [\"DepOnlyJs\"]\nfiles = []\n"

                        // The clr collection of the same package declares the SAME dependency, but
                        // the dependency ships no clr manifest, so a clr closure over it must fail.
                        let clrDependent =
                            writeManifestFor
                                "clr"
                                "NeedsJsDep"
                                "[core]\nname = \"NeedsJsDep\"\ndepends-on = [\"DepOnlyJs\"]\nfiles = []\n"

                        match ReferencedProject.buildClosure [ dependent ] with
                        | Result.Error e -> failtestf "js closure failed: %s" e
                        | Result.Ok ordered ->
                            Expect.contains
                                (ordered |> List.map (fun m -> m.Path))
                                (resolveOrFail "js" (Path.Combine(tmpSrc, "DepOnlyJs")))
                                "the js dependency is the js manifest beside it"

                        match ReferencedProject.buildClosure [ clrDependent ] with
                        | Result.Ok ordered -> failtestf "expected a missing clr dependency, got Ok %A" ordered
                        | Result.Error e ->
                            Expect.stringContains e "DepOnlyJs" "a package absent for a target is named, not silent"
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
                            Expect.stringContains e "impl-js" "the error names the offending key"
                            Expect.stringContains e "core" "and the table it was found in"
                    }

                    // `impl` is both compiled and spliced, so a manifest still naming the retired
                    // second list must error: read as silence it resolves to an impl missing
                    // every splice source that list named.
                    test "the retired `inline-bodies` key is rejected, not ignored" {
                        match
                            ReferencedProject.loadManifest (
                                writeManifest "RetiredCore" "[core]\nfiles = []\ninline-bodies = [\"ops.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an unknown-key error, got Ok %A" m
                        | Result.Error e -> Expect.stringContains e "inline-bodies" "the error names the retired key"
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
                        | Result.Error e -> Expect.stringContains e "targets" "the error names the retired table"
                    }
                ]

            // The compile cache key folds `sourceInputs`; the provider build reads only what a
            // list NAMES. A file in the second set and not the first is a stale cache hit.
            testList
                "sourceInputs covers what the provider build reads"
                [
                    test "sourceInputs names every path any list holds, and no runtime asset" {
                        let m =
                            loadOrFail (
                                writeManifestFor
                                    "js"
                                    "AllLists"
                                    "[core]\n\
                                     files = [\"contract.fsi\", \"shim.js.fsi\"]\n\
                                     impl = [\"ops.fs\", \"ops.js.fs\"]\n\
                                     sig-only = [\"contract.fsi\"]\n\
                                     runtime = [\"x.mjs\"]\n"
                            )

                        let inputs = ReferencedProject.sourceInputs m

                        Expect.equal
                            (List.sort inputs)
                            (List.sort [ "contract.fsi"; "shim.js.fsi"; "ops.fs"; "ops.js.fs" ])
                            "every list, deduplicated"

                        Expect.isFalse (List.contains "x.mjs" inputs) "a runtime asset is not a parsed source"
                    }

                    test "every real package's listed sources are folded" {
                        // Across the shipped manifests: every `.fs` and `.fsi` the target compiles.
                        for manifest in [ vesperCoreManifest; vesperCoreJsManifest; vesperListManifest ] do
                            let m = loadOrFail manifest
                            let inputs = ReferencedProject.sourceInputs m

                            for rel in m.Impl @ m.Files do
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
                        match ReferencedProject.buildClosure [ vesperCoreJsManifest ] with
                        | Result.Error e -> failtestf "buildClosure failed: %s" e
                        | Result.Ok ordered ->
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
                        |> List.filter (fun rel -> ReferencedProject.pairingKey m rel = Path.ChangeExtension(rel, null))

                    let sharedLists (m: ReferencedProject.Manifest) =
                        [
                            "depends-on", m.DependsOn
                            "files", neutral m m.Files
                            "impl", neutral m m.Impl
                            "sig-only", neutral m m.SigOnly
                            "impl-only", neutral m m.ImplOnly
                        ]

                    test "every divergence is a declared one" {
                        // A name one target carries and the other does not can be a real
                        // statement about the target rather than drift, so these are PINNED,
                        // not banned. Each below is explained by the manifest that names it; a
                        // NEW entry is two manifests that drifted.
                        let expected =
                            [
                                // A CLR function value is a nominal `Fun` interface, so adapting
                                // flat<->curried needs a reified object; JS applies directly.
                                {|
                                    Package = "Vesper.Core"
                                    List = "files"
                                    ClrOnly = [ "fun-adapters.fsi" ]
                                    JsOnly = []
                                |}
                                // `fun-adapters` follows its `.fsi` above; the attribute types
                                // are fully erased on JS, so they get a body on CLR alone.
                                {|
                                    Package = "Vesper.Core"
                                    List = "impl"
                                    ClrOnly = [ "compiler-attributes.fs"; "fun-adapters.fs" ]
                                    JsOnly = []
                                |}
                                // The same erasure, stated on the JS side: bodiless there, so it
                                // must be exempted from the pairing rule there.
                                {|
                                    Package = "Vesper.Core"
                                    List = "sig-only"
                                    ClrOnly = []
                                    JsOnly = [ "compiler-attributes.fsi" ]
                                |}
                                // The CLR `%A` engine builds its `Doc` child lists on the
                                // cons-list; the JS one is a free function over its own frames.
                                {|
                                    Package = "Vesper.Printf"
                                    List = "depends-on"
                                    ClrOnly = [ "Vesper.List" ]
                                    JsOnly = []
                                |}
                                // `formatter.fsi` names `TextWriter`/`StringBuilder`/`IsByRefLike`
                                // and `structural-printer.fsi` an interface-dispatching state.
                                {|
                                    Package = "Vesper.Printf"
                                    List = "files"
                                    ClrOnly = [ "structural-printer.fsi"; "formatter.fsi" ]
                                    JsOnly = []
                                |}
                                // It names the NON-generic `IEnumerable`/`IEnumerator`, which the
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
