namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// One of the assets a package's `manifest.js.toml` `runtime` key lists: a committed
/// `.mjs` shipped inside that package's own output directory.
type JsRuntimeModule =
    {
        /// `Vesper.Seq/Vesper.Seq.mjs` — where it is written, and what an importer references.
        Path: JsModulePath
        Source: string
        /// The modules `Source`'s own `import` lines reference: `Vesper.Seq.mjs` imports
        /// `Vesper.Array`'s barrel and `Vesper.Core`'s runtime file. Shipping this one means
        /// shipping those too.
        Imports: JsModulePath list
    }

module JsRuntimeModule =

    /// The module specifier of an `import`/`export … from` STATEMENT: the `from "<spec>"`
    /// clause, or the bare `import "<spec>"` a side-effect import is. Line-anchored, so a
    /// specifier-shaped string in an expression is not one.
    let private statementSpecifier =
        System.Text.RegularExpressions.Regex(
            "^[ \\t]*(?:import|export)\\b(?:[^;\\n]*?\\bfrom)?[ \\t]*[\"']([^\"']*)[\"']",
            System.Text.RegularExpressions.RegexOptions.Multiline
            ||| System.Text.RegularExpressions.RegexOptions.Compiled
        )

    /// A committed file at `path`, reading its `Imports` off its own text — resolved from the
    /// directory it sits in, which is what its own specifiers were written against.
    let ofSource (path: JsModulePath) (source: string) : JsRuntimeModule =
        {
            Path = path
            Source = source
            Imports =
                [
                    for m in statementSpecifier.Matches source do
                        match JsModulePath.tryOfSpecifier path.Package m.Groups.[1].Value with
                        | ValueSome target -> target
                        | ValueNone -> ()
                ]
        }

    let ofAsset (package: string) (asset: RuntimeAsset) : JsRuntimeModule =
        ofSource (JsModulePath.asset package asset.FileName) asset.Source

/// A referenced package's committed JS output, as a consumer links it.
[<RequireQualifiedAccess>]
type JsPackageOutput =
    /// A package this build lays out: a directory of committed files, entered by the generated
    /// `<Package>/index.mjs` barrel. `files` is in manifest order, and the FIRST is the
    /// package's runtime entry, where a backend-synthesised import resolves.
    | Directory of barrel: JsRuntimeModule * files: JsRuntimeModule list
    /// A module this build does NOT lay out — an npm package, whose stub sits at the output
    /// root under its own name and is entered by that file.
    | RootModule of JsRuntimeModule

module JsPackageOutput =

    /// A laid-out package, its barrel `export * from "./<file>";` per committed file.
    let ofAssets (package: string) (assets: RuntimeAsset list) : JsPackageOutput =
        let files = [ for a in assets -> JsRuntimeModule.ofAsset package a ]

        let barrelSource =
            files
            |> List.map (fun f -> sprintf "export * from \"./%s\";\n" f.Path.FileName)
            |> String.concat ""

        JsPackageOutput.Directory(JsRuntimeModule.ofSource (JsModulePath.barrel package) barrelSource, files)

    /// An npm package's stub at the output root.
    let rootModule (fileName: string) (source: string) : JsPackageOutput =
        JsPackageOutput.RootModule(JsRuntimeModule.ofSource (JsModulePath.atRoot fileName) source)

    /// Every committed file, barrel included.
    let modules (output: JsPackageOutput) : JsRuntimeModule list =
        match output with
        | JsPackageOutput.Directory(barrel, files) -> barrel :: files
        | JsPackageOutput.RootModule rt -> [ rt ]

/// The accumulating import for one MODULE: its named bindings plus at most one default and
/// one namespace binding. A TS default export cannot be imported by name, so it is stored in
/// its own slot; the two print as `import D, { a as $x } from "<spec>"`.
type private ImportEntry =
    {
        Path: JsModulePath
        /// The committed asset to materialise beside the output. `ValueNone` for a module
        /// this build EMITS, because that compilation writes the sibling file itself.
        Asset: JsRuntimeModule voption
        Named: System.Collections.Generic.HashSet<JsNamedImport>
        mutable Default: string option
        /// The `import * as <binding>` local. Its own statement: a namespace clause cannot
        /// combine with the `{ named }` clause, so it is emitted beside the module's other import.
        mutable Namespace: string option
    }

/// An external VALUE to import: what is referenced, the module it lives in, and how that
/// module exports it. The home comes from the provider's resolved symbol, so an unkeyed
/// node or a provider miss leaves it empty and `addRef` throws rather than importing.
type JsValueRef =
    {
        Key: SymbolKey voption
        Home: JsHome voption
        Form: ImportForm
    }

/// Per-compilation accumulator for the module imports a program needs. Each imported
/// module is recorded once, on first reference by the expression walker.
type JsImports =
    private
        {
            /// Package/assembly name → its committed output; only the referenced subset ships.
            Runtime: Map<string, JsPackageOutput>
            /// The package directory the module being emitted sits in, the base every
            /// specifier is rendered from. `ValueNone` for a program at the output root.
            SelfPackage: string voption
            Entries: System.Collections.Generic.Dictionary<JsModulePath, ImportEntry>
        }

module JsImports =

    let createIn (selfPackage: string voption) (runtime: Map<string, JsPackageOutput>) : JsImports =
        {
            Runtime = runtime
            SelfPackage = selfPackage
            Entries = System.Collections.Generic.Dictionary()
        }

    /// A program emitted at the output root.
    let create (runtime: Map<string, JsPackageOutput>) : JsImports = createIn ValueNone runtime

    /// The module `home` is imported from: a DECLARING FILE resolves to that file's own module,
    /// which the compilation writing it also writes; a whole package, its barrel; a synthesised
    /// runtime entry, the package's runtime file, so that a package compiling ITSELF references
    /// that file directly rather than cycling through its own barrel.
    let private moduleOf (imports: JsImports) (home: JsHome) (what: string) : JsModulePath * JsRuntimeModule voption =
        let noModule () =
            failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what home.Assembly

        let committed (pick: JsRuntimeModule * JsRuntimeModule list -> JsRuntimeModule) =
            let rt =
                match imports.Runtime |> Map.tryFind home.Assembly with
                | Some(JsPackageOutput.Directory(barrel, files)) -> pick (barrel, files)
                | Some(JsPackageOutput.RootModule rt) -> rt
                | None -> noModule ()

            rt.Path, ValueSome rt

        match home.Where with
        | JsHomeWhere.InFile f -> JsModulePath.ofSource (AssemblyName.toStored f.Assembly) f.Relative.Name, ValueNone
        | JsHomeWhere.Package -> committed fst
        | JsHomeWhere.RuntimeAsset ->
            committed (fun (_, files) ->
                match files with
                | rt :: _ -> rt
                | [] -> noModule ()
            )

    /// The entry for `home`'s module, recording it on first lookup.
    let private entryFor (imports: JsImports) (home: JsHome) (what: string) : ImportEntry =
        let path, asset = moduleOf imports home what

        match imports.Entries.TryGetValue path with
        | true, entry -> entry
        | _ ->
            let entry =
                {
                    Path = path
                    Asset = asset
                    Named = System.Collections.Generic.HashSet<JsNamedImport>()
                    Default = None
                    Namespace = None
                }

            imports.Entries.[path] <- entry
            entry

    /// The local identifier for an external value, recording the import `ref.Form` picks:
    /// `Named` → `import { f as $_f }` → `$_f`; `Default`/`CommonJs` → `import $_f` → `$_f`
    /// (`export =` binds `module.exports` to the default slot); `Namespace` → `$ns_m.f`.
    let addRef (imports: JsImports) (compiledName: string) (ref: JsValueRef) : string =
        // No module to import from: an unkeyed node, a type/member key, or a provider miss.
        let unsupported () =
            failwithf "JS codegen: unsupported external value '%s' (key %A)" compiledName ref.Key

        let b =
            match ref.Key with
            | ValueSome(SymbolKey.Binding b) -> b
            | _ -> unsupported ()

        match ref.Home with
        | ValueNone -> unsupported ()
        // A global pack's export is a JS-runtime intrinsic: bare name, NO import recorded.
        | ValueSome home when TsGlobalHomes.isGlobalHome home.Assembly -> b.Name
        | ValueSome home ->
            let asm = home.Assembly
            let name = b.Name
            let entry = entryFor imports home (sprintf "external value '%s'" compiledName)

            // `$<container>_<name>`, every `.` underscored. An UNQUALIFIED binding's container is
            // the global namespace (`""`), so the join's leading `.` survives as `_`:
            // `makeBox` → `$_makeBox`.
            let alias =
                "$" + (SymbolKeyOps.containerFullName b.Decl + "." + name).Replace('.', '_')

            // At-most-one module slot, ENFORCED: a second, different local would silently
            // clobber the first in the emitted `import` line; the SAME local is the no-op.
            let bindOnce (current: string option) (set: string -> unit) (slotDesc: string) (local: string) : unit =
                match current with
                | Some prev when prev <> local ->
                    failwithf
                        "JS codegen: module '%s' already binds its %s as '%s'; cannot re-bind it as '%s'"
                        asm
                        slotDesc
                        prev
                        local
                | _ -> set local

            match ref.Form with
            | ImportForm.Default
            | ImportForm.CommonJs ->
                bindOnce entry.Default (fun v -> entry.Default <- Some v) "default export" alias
                alias
            | ImportForm.Named ->
                entry.Named.Add { Export = name; Local = alias } |> ignore
                alias
            | ImportForm.Namespace ->
                // The local derives from the MODULE, not the export, so every `Namespace`
                // ref to `asm` shares one `import * as $ns_m` and reads `$ns_m.name` off it.
                let nsLocal = "$ns_" + asm.Replace('.', '_').Replace('/', '_')
                bindOnce entry.Namespace (fun v -> entry.Namespace <- Some v) "namespace import" nsLocal
                nsLocal + "." + name

    /// The local identifier for an external class, importing `className` from `home`'s
    /// module as `$<asm>_<className>`, since the assembly disambiguates same-named classes.
    let addTypeRef (imports: JsImports) (home: JsHome) (className: string) : string =
        let entry = entryFor imports home (sprintf "external type '%s'" className)
        let alias = "$" + home.Assembly.Replace('.', '_') + "_" + className
        entry.Named.Add { Export = className; Local = alias } |> ignore
        alias

    /// The local identifier for an external member, importing the export from `home`'s
    /// module as `$<exportName>`.
    let addMemberRef (imports: JsImports) (home: JsHome) (exportName: string) : string =
        let entry = entryFor imports home (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        entry.Named.Add { Export = exportName; Local = alias } |> ignore
        alias

    /// The leading `import … from "<spec>"` block: one statement per imported module,
    /// modules and specifiers sorted so the emitted text is deterministic.
    let importStatements (imports: JsImports) : JsStatement list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                let entry = kv.Value
                let source = JsModulePath.specifierFrom imports.SelfPackage entry.Path

                if entry.Default.IsSome || entry.Named.Count > 0 then
                    JsStatement.Import(entry.Default, entry.Named |> List.ofSeq |> List.sort, source)

                match entry.Namespace with
                | Some binding -> JsStatement.ImportNamespace(binding, source)
                | None -> ()
        ]

    /// The modules the emitted `import` block lists, sorted as it emits them.
    let importedModules (imports: JsImports) : JsModulePath list =
        [ for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> kv.Key ]

    /// The committed runtime ASSETS this program needs: those referenced during the walk,
    /// closed over asset→asset imports, sorted by file name. An asset importing a module no
    /// package in the manifest closure ships is a dangling ESM specifier, and throws here
    /// rather than at the point Node loads the written output.
    let assets (imports: JsImports) : JsRuntimeModule list =
        let byPath = System.Collections.Generic.Dictionary<JsModulePath, JsRuntimeModule>()

        for KeyValue(_, output) in imports.Runtime do
            for rt in JsPackageOutput.modules output do
                byPath.[rt.Path] <- rt

        let selected =
            System.Collections.Generic.Dictionary<JsModulePath, JsRuntimeModule>()

        let pending = System.Collections.Generic.Stack<JsRuntimeModule>()

        for kv in imports.Entries do
            match kv.Value.Asset with
            | ValueSome asset -> pending.Push asset
            | ValueNone -> ()

        while pending.Count > 0 do
            let asset = pending.Pop()

            if not (selected.ContainsKey asset.Path) then
                selected.[asset.Path] <- asset

                for target in asset.Imports do
                    match byPath.TryGetValue target with
                    | true, dep -> pending.Push dep
                    | _ ->
                        failwithf
                            "JS codegen: runtime asset '%s' imports '%s', which no referenced package ships"
                            (JsModulePath.specifierFrom ValueNone asset.Path)
                            (JsModulePath.specifierFrom asset.Path.Package target)

        selected.Values
        |> Seq.sortBy (fun asset -> JsModulePath.specifierFrom ValueNone asset.Path)
        |> List.ofSeq
