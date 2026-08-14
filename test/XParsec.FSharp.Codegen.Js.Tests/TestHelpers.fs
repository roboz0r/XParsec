module XParsec.FSharp.Codegen.Js.Tests.TestHelpers

open System
open System.Diagnostics
open Expecto
open Vesper.Ts.Manifest
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js

/// Lex + parse a source string; a script fragment wraps as `AnonymousModule`. A source that
/// parses only because RECOVERY patched it raises here. `Result.Error` is qualified because
/// `open …SemanticAnalysis` brings `Severity.Error` into scope, shadowing it.
let parseFile (input: string) : Lexed * ImplementationFile<SyntaxToken> =
    match Pipeline.parseUnrecovered input with
    | Result.Error ds -> failwithf "parse failed: %A" (ds |> List.map (fun d -> d.Message))
    | Result.Ok parsed -> parsed.Lexed, parsed.File

/// `src/<pkg>` — the package DIRECTORY. The JS backend resolves it to `manifest.js.toml`;
/// this suite never names a manifest file, so it cannot name another target's.
let srcPackage (pkg: string) : string =
    IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", pkg)

/// `src/Vesper.Core`.
let vesperCorePackage: string = srcPackage "Vesper.Core"

/// A file inside a package directory (e.g. `srcFile "Vesper.List" "list.fs"`).
let srcFile (pkg: string) (file: string) : string = IO.Path.Combine(srcPackage pkg, file)

/// `src/Vesper.Printf`.
let vesperPrintfPackage: string = srcPackage "Vesper.Printf"

/// `<repo-root>/tmp/<name>`, created on demand.
let tmpDir (name: string) : string =
    let rec up (dir: string) =
        if isNull dir then
            failwith "repo root not found"
        elif IO.File.Exists(IO.Path.Combine(dir, "claude_tools.cmd")) then
            dir
        else
            up (IO.Path.GetDirectoryName dir)

    let d = IO.Path.Combine(up AppContext.BaseDirectory, "tmp", name)
    IO.Directory.CreateDirectory d |> ignore
    d

/// Run a `.mjs` file under Node. Returns `None` when `node` is absent (exec tests skip).
let runNode (jsPath: string) : (int * string) option =
    let psi = ProcessStartInfo "node"
    psi.ArgumentList.Add jsPath
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    psi.UseShellExecute <- false

    try
        use p = Process.Start psi
        let out = p.StandardOutput.ReadToEnd()
        let err = p.StandardError.ReadToEnd()
        p.WaitForExit()
        Some(p.ExitCode, (if p.ExitCode = 0 then out else out + err))
    with :? System.ComponentModel.Win32Exception ->
        None

/// The packages the JS-target program resolves against.
let jsPackages: string list =
    [
        vesperCorePackage
        srcPackage "Vesper.Comparison"
        vesperPrintfPackage
        srcPackage "Vesper.Option"
        srcPackage "Vesper.List"
        srcPackage "Vesper.Array"
    ]

/// The JS-target contract for `jsPackages`, BCL-free: the provider a program is analysed
/// against, the producer files its served inline bodies are anchored in, and the manifest set
/// backing its runtime imports.
let jsContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContract jsPackages

/// `jsContract`'s provider, for the front-end helpers: analysis resolves symbols and reads no
/// position.
let jsProvider: Lazy<IExternalSymbolProvider> = lazy jsContract.Value.Provider

/// A member declared in a `.fsi` and bodied in an `impl` file is keyed twice, and the
/// inline-body store is keyed by the WHOLE member key. Assert the two halves agree for each of
/// `members`, so an `ArgSig` divergence is named here instead of surfacing as an absent body.
let expectMemberKeyHalvesAgree
    (contract: SymbolProviders.Contract)
    (implPackages: string list)
    (declKey: SymbolKey)
    (members: string list)
    : unit =
    // The named packages alone, NOT their `depends-on` closure: the assertion is about the
    // halves one package's own `impl` publishes.
    let manifests =
        ReferencedProject.resolveAll Target.Js implPackages
        |> List.map (fun mp ->
            match ReferencedProject.loadManifest mp with
            | Result.Ok m -> m
            | Result.Error e -> failtestf "loadManifest: %s" e
        )

    let implBodies = (SymbolProviders.inlineBodies contract.Provider manifests).Members

    for memberName in members do
        let contractKey =
            match contract.Provider.TryLookupMember(declKey, memberName) with
            | ValueSome m -> SymbolKey.Member m.Key
            | ValueNone -> failtestf "the contract of %A publishes no `%s`" declKey memberName

        // Scoped to `declKey`: several intrinsics declare an `Item` accessor, so the name
        // alone names more than one impl-side body.
        match
            implBodies
            |> List.filter (fun mb ->
                match mb.Key with
                | SymbolKey.Member mk -> SymbolKey.Type mk.Decl = declKey && mk.Name = memberName
                | _ -> false
            )
        with
        | [ mb ] ->
            Expect.equal
                mb.Key
                contractKey
                (sprintf "`%s`: the contract half and the impl half key differently" memberName)
        | found ->
            failtestf
                "expected exactly one impl-side `%s` body, got %A"
                memberName
                (found |> List.map (fun mb -> mb.Key))

/// Front-end a program to its frozen `FrozenPools`, through `jsProvider`. Fails on any error
/// diagnostic.
let frozenOf (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemWithContext jsProvider.Value (Hashing.originSourceOfText lexed) file

    let errors = tast.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run ctx tast

/// Compile `input` to JS source text (in-memory).
let emit (input: string) : string =
    Codegen.compile (JsProjectInfo.defaults "Test") (frozenOf input)
    |> Codegen.toSource

/// Front-end a program through the JS-target provider. Fails on any error diagnostic.
let frozenOfJs (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemWithContext jsProvider.Value (Hashing.originSourceOfText lexed) file

    let errors = tast.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errors) then
        failwithf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run ctx tast

/// The map's view of a source: its text plus the tokens a frozen node's anchor indexes into.
/// A test that only has the text re-lexes here; production hands over the front end's `Lexed`.
let jsSource (path: string) (input: string) : JsSource =
    match Lexing.lexString input with
    | Result.Error e -> failwithf "lex failed: %A" e
    | Result.Ok lexed ->
        {
            Path = path
            Content = input
            Lexed = lexed
        }

/// Emit a named program's JS from an ALREADY-frozen tree, through the project shape the
/// byte-identity gates pin (assembly `name`, `name + ".fsx"` source), stripping the trailing
/// `//# sourceMappingURL` line.
let emitFrozenJs (name: string) (src: string) (frozen: FrozenPools) : string =
    let project =
        { JsProjectInfo.defaults name with
            Source = Some(jsSource (name + ".fsx") src)
        }

    let source = Codegen.compileWith jsContract.Value project frozen |> Codegen.toSource

    let idx = source.IndexOf "//# sourceMappingURL"
    if idx >= 0 then source.Substring(0, idx) else source

/// Compile a JS-target `input` to JS source text (strips `//# sourceMappingURL`).
let emitJs (input: string) : string =
    let project =
        { JsProjectInfo.defaults "Test" with
            Source = Some(jsSource "test.fsx" input)
        }

    let src =
        Codegen.compileWith jsContract.Value project (frozenOfJs input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Like `emitJs` but in library mode (top-level `let` → `export const`).
let emitJsLibrary (input: string) : string =
    let project =
        { JsProjectInfo.defaults "Test" with
            Source = Some(jsSource "test.fsx" input)
            Kind = Library
        }

    let src =
        Codegen.compileWith jsContract.Value project (frozenOfJs input)
        |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Deps-only JS contract for compiling a package impl. The package's own contract is absent
/// because `compileLibrary` carries no home assembly, so its declarations would be a second
/// claimant of the types the impl declares; `compileOwnLibrary` carries one and takes them both.
let coreDepsJsContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContract [ vesperCorePackage ]

/// As `coreDepsJsContract`, plus `Vesper.Array`'s OWN manifest, because `array.fs` splices
/// `NewArray` out of the per-target `array-prelude.js.fs`. Safe only because `Vesper.Array`
/// declares no in-file types; one that does (`Vesper.List`) takes the deps-only contract above.
let arrayDepsJsContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContract [ vesperCorePackage; srcPackage "Vesper.Array" ]

/// Contract for `Vesper.Seq`'s impl: `Vesper.Array` for `toArray`'s buffer, and the
/// package's own manifest for the `SeqPrelude.truncate` its `seq.fs` forwards to.
let seqDepsJsContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContract [ vesperCorePackage; srcPackage "Vesper.Array"; srcPackage "Vesper.Seq" ]

/// Front-end + freeze a JS-target package impl. The provider carries only the package's
/// dependencies, so the impl's own in-file types are the resolution authority.
let frozenImplJs (provider: IExternalSymbolProvider) (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let ctx, tast =
        Pipeline.analyseSemWithContext provider (Hashing.originSourceOfText lexed) file

    let errors = tast.Diagnostics |> Diagnostic.errors

    if not (List.isEmpty errors) then
        failwithf "impl analysis errors: %A" (errors |> List.map (fun d -> d.Message))

    Freeze.run ctx tast

/// Front-end + freeze a JS-target package impl compiled AS `assemblyName`. Naming the home
/// assembly puts the package's own contract in scope: a type shape homed there is the file
/// seeing what it declares, not a second claimant of the name.
let frozenOwnImplJs (assemblyName: string) (provider: IExternalSymbolProvider) (input: string) : FrozenPools =
    let lexed, file = parseFile input

    let frozen =
        Pipeline.analyseFor assemblyName provider (Hashing.originSourceOfText lexed) file

    match frozen.Residue.Diagnostics |> Diagnostic.errors with
    | [] -> frozen
    | errors -> failwithf "impl analysis errors: %A" (errors |> List.map (fun d -> d.Message))

/// Emit an already-frozen package impl as runtime-module source text (strips
/// sourceMappingURL). `sourceFile` is the Vesper source basename (`list.fs`), recorded
/// both in the source map and in the emitted `// Generated from …` header.
let private emitLibrarySource
    (contract: SymbolProviders.Contract)
    (moduleName: string)
    (sourceFile: string)
    (input: string)
    (frozen: FrozenPools)
    : string =
    let project =
        { JsProjectInfo.defaults moduleName with
            Source = Some(jsSource sourceFile input)
            Kind = Library
            GeneratedFrom = Some sourceFile
        }

    let src = Codegen.compileWith contract project frozen |> Codegen.toSource

    let idx = src.IndexOf "//# sourceMappingURL"
    if idx >= 0 then src.Substring(0, idx) else src

/// Compile a package impl in library mode, against a contract carrying only its
/// DEPENDENCIES. The impl is analysed against the same contract it is emitted through, so
/// a body spliced out of a dependency resolves against the file it was written in.
let compileLibrary
    (contract: SymbolProviders.Contract)
    (moduleName: string)
    (sourceFile: string)
    (input: string)
    : string =
    emitLibrarySource contract moduleName sourceFile input (frozenImplJs contract.Provider input)

/// `compileLibrary` for an impl compiled as its OWN package, so `moduleName` is both the
/// emitted module and the home assembly the contract's own declarations are attributed to.
let compileOwnLibrary
    (contract: SymbolProviders.Contract)
    (moduleName: string)
    (sourceFile: string)
    (input: string)
    : string =
    emitLibrarySource contract moduleName sourceFile input (frozenOwnImplJs moduleName contract.Provider input)

/// Write `files` to a tmp dir and run the first as entry point under Node.
let runNodeFiles (name: string) (files: (string * string) list) : (int * string) option =
    let dir = tmpDir name

    for (fileName, source) in files do
        IO.File.WriteAllText(IO.Path.Combine(dir, fileName), source)

    match files with
    | (entry, _) :: _ ->
        runNode (IO.Path.Combine(dir, entry))
        |> Option.map (fun (code, out) -> code, out.Replace("\r", "").Trim())
    | [] -> failwith "runNodeFiles: no files"

/// Compile `input` and run under Node. Returns `None` when `node` is absent (test skips).
let runJs (name: string) (input: string) : (int * string) option =
    let outDir = tmpDir name
    let jsPath = IO.Path.Combine(outDir, name + ".mjs")

    let project =
        { JsProjectInfo.defaults name with
            OutputPath = Some jsPath
            Source = Some(jsSource (name + ".fsx") input)
        }

    Codegen.compileWith jsContract.Value project (frozenOfJs input)
    |> Codegen.materialise

    runNode jsPath
    |> Option.map (fun (code, out) -> code, out.Replace("\r", "").Trim())

// ─── TS-provider test scaffolding (shared by the provider tests) ─────────

/// Composes `sources` (no home assembly), then the covariant `number → float` resolution and
/// the per-lookup cache, in production's order.
let stackJs (ambient: string list) (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
    ExternalSymbolProviders.stack ValueNone ambient sources
    |> NumberCovariance.wrap
    |> ExternalSymbolProviders.memoize

/// `stackJs` with the ambient prefix set AGGREGATED from the sources, as production does.
/// `stackJs []` over a Vesper contract source drops the `Vesper` open-prefix the intrinsic
/// resolver needs to find `Vesper.unit`/`Vesper.int`/`Vesper.undefined` beneath a TS manifest.
let stackWithAmbient (sources: IExternalSymbolProvider list) : IExternalSymbolProvider =
    let ambient = sources |> List.collect (fun s -> s.AmbientOpenPrefixes)
    stackJs ambient sources

/// A TS-manifest provider layered over the JS-native provider, so the manifest's
/// primitive/`int`/`string` argument types still resolve.
let stackTs (manifest: Schema.PackageManifest) : IExternalSymbolProvider =
    stackWithAmbient [ TsManifestProvider.providerOfManifest manifest; jsProvider.Value ]

/// Like `stackTs` but layers SEVERAL TS-manifest providers (order preserved) over the
/// JS-native provider, for a program driving more than one external package.
let stackTsMany (manifests: Schema.PackageManifest list) : IExternalSymbolProvider =
    stackWithAmbient
        [
            yield! manifests |> List.map TsManifestProvider.providerOfManifest
            jsProvider.Value
        ]

/// The EMIT contract of a `stackTsMany` stack: that stack as the provider, re-seated in
/// `jsContract`, so it carries the JS-native stubs' retention as its anchor domain. A TS
/// manifest serves no inline body, so those stubs are the only layer a served body comes from.
let contractTsMany (manifests: Schema.PackageManifest list) : SymbolProviders.Contract =
    { jsContract.Value with
        Provider = stackTsMany manifests
    }

/// `contractTsMany` for one manifest: the emit contract behind `stackTs`.
let contractTs (manifest: Schema.PackageManifest) : SymbolProviders.Contract = contractTsMany [ manifest ]

/// Analyse `input` through the self-host front end; returns only the ERROR diagnostics.
let analyseWith (provider: IExternalSymbolProvider) (input: string) : Diagnostic list =
    let lexed, file = parseFile input

    let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText lexed) file

    tast.Diagnostics |> Diagnostic.errors

/// The newline-joined messages of `ds`, for `stringContains` assertions.
let errorText (ds: Diagnostic list) : string =
    ds |> List.map (fun d -> d.Message) |> String.concat "\n"

/// A `WalkCtx` matching production wiring: a real resolver over the source (its token table
/// AND its line starts, since an anchor is an index into the former), and all lowering tables
/// empty for `buildProgram` to fill. `exportTopLevel` selects script (`false`) vs library.
let private jsWalkCtx
    (contract: SymbolProviders.Contract)
    (runtime: Map<string, JsRuntimeModule>)
    (exportTopLevel: bool)
    (input: string)
    (frozen: FrozenPools)
    : EmitJsContext.WalkCtx =
    let resolver: EmitJsContext.Resolver =
        match Lexing.lexString input with
        | Result.Ok lexed ->
            ValueSome
                {
                    Lexed = lexed
                    Lines = JsMapSources.LineIndex.build input
                    Origins = contract.Origins
                }
        | Result.Error _ -> ValueNone

    EmitJsContext.WalkCtx.create
        resolver
        (TastPoolBuilder.openOver frozen)
        contract.Provider
        (JsImports.create runtime)
        exportTopLevel

/// Front-end + freeze `input` through `contract`, then emit JS with the injected `runtime`
/// modules. Analysed and emitted through the SAME contract, so a body spliced out of a
/// dependency resolves its position against the file it was written in.
let emitWith
    (contract: SymbolProviders.Contract)
    (runtime: Map<string, JsRuntimeModule>)
    (exportTopLevel: bool)
    (input: string)
    : string =
    let frozen = frozenImplJs contract.Provider input
    let ctx = jsWalkCtx contract runtime exportTopLevel input frozen
    (JsPrint.print (EmitJs.buildProgram ctx)).Source

/// Write `files` to a tmp dir, run the first under Node, and require the trimmed stdout to
/// EXACTLY equal `expected`. When `node` is absent the exec check is SKIPPED; the caller's
/// emit-time assertions already ran.
let expectNodeOutput (name: string) (files: (string * string) list) (expected: string) : unit =
    match runNodeFiles name files with
    | None -> ()
    | Some(code, out) ->
        Expect.equal code 0 (sprintf "node exited non-zero:\n%s" out)
        Expect.equal out expected (sprintf "round-trip output, got:\n%s" out)

/// The vendored es2015 ref pack (`../ts-fixtures/es2015/es2015.manifest.json`): the GLOBAL
/// lib manifest mounted under `Js`, so it emits no import. Committed golden; the Node
/// extractor is never run.
let es2015Manifest: Schema.PackageManifest =
    let path =
        IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "ts-fixtures", "es2015", "es2015.manifest.json")

    match Codec.deserialize (IO.File.ReadAllText path) with
    | Error e -> failwithf "es2015 manifest does not parse: %s" e
    | Ok man -> man

/// The mitt TS fixture: golden manifest + vendored runtime under `../ts-fixtures/mitt`.
/// Reads ONLY committed files; the Node extractor is never run.
module MittFixture =

    let dir = IO.Path.Combine(__SOURCE_DIRECTORY__, "..", "ts-fixtures", "mitt")

    /// The golden `mitt.manifest.json`, deserialised.
    let manifest: Schema.PackageManifest =
        match Codec.deserialize (IO.File.ReadAllText(IO.Path.Combine(dir, "mitt.manifest.json"))) with
        | Error e -> failwithf "mitt manifest does not parse: %s" e
        | Ok man -> man

    /// The vendored `dist/mitt.mjs` runtime source.
    let runtimeSource: string =
        IO.File.ReadAllText(IO.Path.Combine(dir, "dist", "mitt.mjs"))

    /// mitt STACKED OVER es2015, so mitt's `all: Map<…>` homed ref resolves as a real
    /// `Js.Map` and its members can be called, then over the JS-native provider.
    let provider: IExternalSymbolProvider = stackTsMany [ manifest; es2015Manifest ]
