module XParsec.FSharp.SemanticAnalysis.Tests.ReferencedProjectTests

open System.IO
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// `Vesper.Core` stood up from its real `manifest.toml`: what its contract resolves, and
// with which identities.

/// `src/Vesper.Core/manifest.toml`, found by walking up from the test assembly.
let private vesperCoreManifest =
    let testDir = Path.GetDirectoryName(typeof<VesperLib.LibFile>.Assembly.Location)
    let mutable dir = DirectoryInfo testDir
    let mutable found = None

    while not (isNull dir) && found.IsNone do
        let candidate = Path.Combine(dir.FullName, "src", "Vesper.Core", "manifest.toml")

        if File.Exists candidate then
            found <- Some candidate

        dir <- dir.Parent

    match found with
    | Some p -> p
    | None -> failwithf "Could not locate src/Vesper.Core/manifest.toml from %s" testDir

/// The `src/` directory holding the real package manifests — the parent of `Vesper.Core`.
let private srcDir = Path.GetDirectoryName(Path.GetDirectoryName vesperCoreManifest)

/// A sibling package that `depends-on` Vesper.Core — the dependency-ordering fixture.
let private vesperListManifest =
    Path.Combine(srcDir, "Vesper.List", "manifest.toml")

/// A throwaway `src/`-shaped tree under the repo `./tmp`: a `depends-on` name resolves to
/// a SIBLING package directory, so synthetic manifests must live side by side as
/// `tmpSrc/<name>/manifest.toml`.
let private tmpSrc =
    let repoRoot = Path.GetDirectoryName srcDir
    let d = Path.Combine(repoRoot, "tmp", "buildClosure-tests", "src")
    Directory.CreateDirectory d |> ignore
    d

/// Writes `tmpSrc/<name>/manifest.toml`; `files = []` keeps it parse-valid with no `.fsi`.
let private writeSyntheticManifest (name: string) (dependsOn: string list) : string =
    let dir = Path.Combine(tmpSrc, name)
    Directory.CreateDirectory dir |> ignore
    let deps = dependsOn |> List.map (sprintf "\"%s\"") |> String.concat ", "
    let path = Path.Combine(dir, "manifest.toml")

    File.WriteAllText(path, sprintf "[core]\nname = \"%s\"\ndepends-on = [%s]\nfiles = []\n" name deps)

    path

/// Writes `tmpSrc/<name>/{manifest.toml,contract.fsi}`, the `.fsi` declaring `fsiBody`
/// under `ns` — for the duplicate-type tests, which need two packages declaring one name.
let private writeSyntheticPackageWithType (name: string) (ns: string) (fsiBody: string) : string =
    let dir = Path.Combine(tmpSrc, name)
    Directory.CreateDirectory dir |> ignore
    File.WriteAllText(Path.Combine(dir, "contract.fsi"), sprintf "namespace %s\n%s\n" ns fsiBody)
    let path = Path.Combine(dir, "manifest.toml")

    File.WriteAllText(path, sprintf "[core]\nname = \"%s\"\nfiles = [\"contract.fsi\"]\n" name)

    path

let private builtProvider =
    lazy
        (match ReferencedProject.buildProvider "clr" vesperCoreManifest with
         | Result.Error e -> failwithf "buildProvider failed: %s" e
         | Result.Ok(provider, diags) -> provider, diags)

/// The same contract built for JS: no `.js.fs` capability reprs, plus the
/// `capabilities-compat.js.fsi` shim appended from `[targets.js] files`.
let private builtProviderJs =
    lazy
        (match ReferencedProject.buildProviderWith "js" (fun _ -> ValueNone) [] vesperCoreManifest with
         | Result.Error e -> failwithf "buildProviderWith js failed: %s" e
         | Result.Ok bp -> bp.Provider, bp.Diagnostics)

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
                    Expect.isNonEmpty m.Shared.Files "files listed"
                    Expect.equal (List.head m.Shared.Files) "prim-types-min.fsi" "compile order: prim-types-min first"
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
                        | Result.Ok ordered ->
                            let coreFull = Path.GetFullPath vesperCoreManifest
                            let listFull = Path.GetFullPath vesperListManifest
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
                        | Result.Ok ordered ->
                            let coreFull = Path.GetFullPath vesperCoreManifest
                            let listFull = Path.GetFullPath vesperListManifest

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
                                Path.GetFullPath(Path.Combine(tmpSrc, name, "manifest.toml"))

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
                                [ Path.Combine(tmpSrc, "IndepD", "manifest.toml"); e ]
                        with
                        | Result.Error err -> failtestf "buildClosureWithDeps failed: %s" err
                        | Result.Ok(_, transitiveDeps) ->
                            let pathOf name =
                                Path.GetFullPath(Path.Combine(tmpSrc, name, "manifest.toml"))

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
                                ReferencedProject.composeContract ReferencedProject.noMetaTail "clr" [ a; b ]
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

                        ReferencedProject.composeContract ReferencedProject.noMetaTail "clr" [ a ]
                        |> ignore
                    }

                    // The directory name IS the package identity — `depends-on` resolves against
                    // it — so a diverging `[core] name` is rejected at parse time.
                    test "a [core] name diverging from the directory name is rejected" {
                        // `writeSyntheticManifest` always matches name to directory, so this one
                        // is hand-written to diverge.
                        let dir = Path.Combine(tmpSrc, "DivergeDir")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, "[core]\nname = \"Mismatch\"\nfiles = []\n")

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> failtestf "expected a name/dir mismatch error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains e "Mismatch" "error names the declared name"
                            Expect.stringContains e "DivergeDir" "error names the directory"
                    }

                    test "an omitted [core] name falls back to the directory name (no divergence)" {
                        let dir = Path.Combine(tmpSrc, "NoName")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, "[core]\nfiles = []\n")

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> Expect.equal m.Name "NoName" "name falls back to directory name"
                        | Result.Error e -> failtestf "expected Ok, got Error %s" e
                    }
                ]

            // Every `[targets.<t>]` list INHERITS its `[core]` peer and APPENDS to it.
            testList
                "per-target lists"
                [
                    let withTargets =
                        let dir = Path.Combine(tmpSrc, "TargetLists")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")

                        File.WriteAllText(
                            path,
                            "[core]\n\
                             files = [\"contract.fsi\"]\n\
                             impl = [\"ops.fs\"]\n\
                             sig-only = [\"contract.fsi\"]\n\
                             impl-only = [\"ops.fs\"]\n\
                             \n\
                             [targets.js]\n\
                             files = [\"shim.js.fsi\"]\n\
                             impl = [\"ops.js.fs\"]\n\
                             sig-only = [\"shim.js.fsi\"]\n\
                             impl-only = [\"ops.js.fs\"]\n\
                             runtime = [\"runtime.mjs\"]\n"
                        )

                        match ReferencedProject.loadManifest path with
                        | Result.Ok m -> m
                        | Result.Error e -> failwithf "loadManifest failed: %s" e

                    test "a [targets.<t>] table is captured under its bare name" {
                        Expect.equal (withTargets.Targets |> Map.toList |> List.map fst) [ "js" ] "one declared target"

                        Expect.equal
                            (withTargets.Targets.["js"])
                            {
                                Files = [ "shim.js.fsi" ]
                                Impl = [ "ops.js.fs" ]
                                SigOnly = [ "shim.js.fsi" ]
                                ImplOnly = [ "ops.js.fs" ]
                                Runtime = [ "runtime.mjs" ]
                            }
                            "every [targets.js] list captured"
                    }

                    test "every resolver inherits the shared list and appends the target's" {
                        Expect.equal
                            (ReferencedProject.resolveFiles "js" withTargets)
                            [ "contract.fsi"; "shim.js.fsi" ]
                            "shared contract first, then the target's extras"

                        Expect.equal
                            (ReferencedProject.resolveImpl "js" withTargets)
                            [ "ops.fs"; "ops.js.fs" ]
                            "shared impl first, then the target's"

                        Expect.equal
                            (ReferencedProject.resolveSigOnly "js" withTargets)
                            [ "contract.fsi"; "shim.js.fsi" ]
                            "shared exemptions first, then the target's"

                        Expect.equal
                            (ReferencedProject.resolveImplOnly "js" withTargets)
                            [ "ops.fs"; "ops.js.fs" ]
                            "shared contract-less bodies first, then the target's"
                    }

                    test "an undeclared target resolves to the shared lists alone" {
                        Expect.equal
                            (ReferencedProject.resolveImpl "wasm" withTargets)
                            [ "ops.fs" ]
                            "no [targets.wasm] ⇒ shared impl only"

                        Expect.equal
                            (ReferencedProject.resolveFiles "wasm" withTargets)
                            [ "contract.fsi" ]
                            "no [targets.wasm] ⇒ shared contract only"
                    }

                    // `runtime` has no `[core]` peer to inherit: a runtime asset is inherently
                    // target-specific.
                    test "resolveRuntime selects the target asset, empty for an unknown target" {
                        Expect.equal
                            (ReferencedProject.resolveRuntime "js" withTargets)
                            [ "runtime.mjs" ]
                            "js runtime asset selected"

                        Expect.isEmpty
                            (ReferencedProject.resolveRuntime "wasm" withTargets)
                            "unknown target ⇒ no runtime (there is no shared one to inherit)"
                    }

                    // Extension off, then a declared target's suffix — how a `.fsi` finds its
                    // `.fs` body.
                    test "pairingKey strips the extension and a declared target suffix" {
                        Expect.equal (ReferencedProject.pairingKey withTargets "ops.js.fs") "ops" "target suffix off"

                        Expect.equal (ReferencedProject.pairingKey withTargets "ops.fs") "ops" "bare body"

                        Expect.equal
                            (ReferencedProject.pairingKey withTargets "shim.js.fsi")
                            "shim"
                            "a `.js.fsi` contract keys the same as its `.js.fs` body"

                        Expect.equal
                            (ReferencedProject.pairingKey withTargets "ops.wasm.fs")
                            "ops.wasm"
                            "an UNdeclared suffix is part of the key"
                    }

                    // The asset's contents, read off disk and keyed by package name — what the
                    // JS backend imports and materialises beside its output.
                    test "runtimeModules reads the asset contents keyed by package name" {
                        let dir = Path.Combine(tmpSrc, "RuntimeAsset")
                        Directory.CreateDirectory dir |> ignore
                        File.WriteAllText(Path.Combine(dir, "asset.mjs"), "export const k = 1;\n")
                        let path = Path.Combine(dir, "manifest.toml")

                        File.WriteAllText(
                            path,
                            "[core]\n\
                             files = []\n\
                             \n\
                             [targets.js]\n\
                             runtime = [\"asset.mjs\"]\n"
                        )

                        let resolved = ReferencedProject.runtimeModules "js" [ path ]

                        Expect.equal
                            (resolved |> Map.tryFind "RuntimeAsset")
                            (Some("asset.mjs", "export const k = 1;\n"))
                            "package RuntimeAsset → (fileName, source) read from disk"

                        Expect.isEmpty
                            (ReferencedProject.runtimeModules "wasm" [ path ])
                            "a target with no runtime asset resolves to an empty map"
                    }

                    test "a manifest with no [targets] table declares no target and resolves to shared" {
                        let dir = Path.Combine(tmpSrc, "NoTargets")
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, "[core]\nfiles = []\nimpl = [\"ops.fs\"]\n")

                        match ReferencedProject.loadManifest path with
                        | Result.Error e -> failtestf "loadManifest failed: %s" e
                        | Result.Ok m ->
                            Expect.isEmpty m.Targets "no [targets.<t>] table ⇒ no declared target"
                            Expect.equal (ReferencedProject.resolveImpl "js" m) [ "ops.fs" ] "js ⇒ shared impl"

                    }

                    // An undefined key is a parse ERROR, not silence: a dashed-suffix spelling
                    // like `impl-js` would otherwise resolve to a plausible wrong file set.
                    test "an unknown key is rejected, in [core] and in a target table" {
                        let write (name: string) (body: string) =
                            let dir = Path.Combine(tmpSrc, name)
                            Directory.CreateDirectory dir |> ignore
                            let path = Path.Combine(dir, "manifest.toml")
                            File.WriteAllText(path, body)
                            path

                        match
                            ReferencedProject.loadManifest (
                                write "StaleCore" "[core]\nfiles = []\nimpl-js = [\"ops.js.fs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an unknown-key error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains e "impl-js" "the error names the offending key"
                            Expect.stringContains e "core" "and the table it was found in"

                        match
                            ReferencedProject.loadManifest (
                                write "StaleTarget" "[core]\nfiles = []\n\n[targets.js]\nruntime-js = [\"x.mjs\"]\n"
                            )
                        with
                        | Result.Ok m -> failtestf "expected an unknown-key error, got Ok %A" m
                        | Result.Error e ->
                            Expect.stringContains e "runtime-js" "the error names the offending key"
                            Expect.stringContains e "targets.js" "and the target table it was found in"
                    }

                    // `impl` is both compiled and spliced, so a manifest still naming the retired
                    // second list must error: read as silence it resolves to an impl missing
                    // every splice source that list named.
                    test "the retired `inline-bodies` key is rejected, not ignored" {
                        let write (name: string) (body: string) =
                            let dir = Path.Combine(tmpSrc, name)
                            Directory.CreateDirectory dir |> ignore
                            let path = Path.Combine(dir, "manifest.toml")
                            File.WriteAllText(path, body)
                            path

                        for name, body in
                            [
                                "RetiredCore", "[core]\nfiles = []\ninline-bodies = [\"ops.fs\"]\n"
                                "RetiredTarget", "[core]\nfiles = []\n\n[targets.js]\ninline-bodies = [\"ops.js.fs\"]\n"
                            ] do
                            match ReferencedProject.loadManifest (write name body) with
                            | Result.Ok m -> failtestf "%s: expected an unknown-key error, got Ok %A" name m
                            | Result.Error e ->
                                Expect.stringContains e "inline-bodies" "the error names the retired key"
                    }
                ]

            // The compile cache key folds `sourceInputs`; the provider build reads only what a
            // list NAMES. A file in the second set and not the first is a stale cache hit.
            testList
                "sourceInputs covers what the provider build reads"
                [
                    let loadOrFail (path: string) =
                        match ReferencedProject.loadManifest path with
                        | Result.Error e -> failtestf "loadManifest failed: %s" e
                        | Result.Ok m -> m

                    let writeManifest (name: string) (body: string) : string =
                        let dir = Path.Combine(tmpSrc, name)
                        Directory.CreateDirectory dir |> ignore
                        let path = Path.Combine(dir, "manifest.toml")
                        File.WriteAllText(path, body)
                        path

                    test "sourceInputs names every path any list holds, and no runtime asset" {
                        let m =
                            loadOrFail (
                                writeManifest
                                    "AllLists"
                                    "[core]\n\
                                     files = [\"contract.fsi\"]\n\
                                     impl = [\"ops.fs\"]\n\
                                     sig-only = [\"contract.fsi\"]\n\
                                     \n\
                                     [targets.clr]\n\
                                     impl = [\"ops.clr.fs\"]\n\
                                     \n\
                                     [targets.js]\n\
                                     files = [\"shim.js.fsi\"]\n\
                                     impl = [\"ops.js.fs\"]\n\
                                     runtime = [\"x.mjs\"]\n"
                            )

                        let inputs = ReferencedProject.sourceInputs m

                        Expect.equal
                            (List.sort inputs)
                            (List.sort [ "contract.fsi"; "ops.fs"; "ops.clr.fs"; "shim.js.fsi"; "ops.js.fs" ])
                            "every target's lists unioned, deduplicated"

                        Expect.isFalse (List.contains "x.mjs" inputs) "a runtime asset is not a parsed source"
                    }

                    test "a manifest naming no target yields nothing target-shaped" {
                        let m =
                            loadOrFail (writeManifest "TargetBlind" "[core]\nfiles = [\"contract.fsi\"]\n")

                        Expect.isEmpty m.Targets "no [targets.<t>] table"

                        Expect.equal
                            (ReferencedProject.sourceInputs m)
                            [ "contract.fsi" ]
                            "only what the shared lists name"
                    }

                    test "every real package's listed bodies are folded" {
                        // Across the shipped manifests: every `.fs` a target compiles is covered.
                        for manifest in [ vesperCoreManifest; vesperListManifest ] do
                            let m = loadOrFail manifest
                            let inputs = ReferencedProject.sourceInputs m

                            for t in m.Targets |> Map.toList |> List.map fst do
                                for rel in ReferencedProject.resolveImpl t m @ ReferencedProject.resolveFiles t m do
                                    Expect.contains inputs rel (sprintf "%s: %s's %s is folded" m.Name t rel)
                    }
                ]
        ]
