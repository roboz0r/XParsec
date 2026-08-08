namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// A committed `.mjs` shipped beside the compiled output — one of the assets a package
/// manifest's `[targets.js] runtime` key names.
type JsRuntimeModule =
    {
        /// `Vesper.List.mjs` — written beside the output, named in `import … from "./<FileName>"`.
        FileName: string
        Source: string
    }

/// The accumulating import for one MODULE: `(exportName, alias)` named bindings plus at
/// most one default and one namespace binding. A TS default export cannot be imported by
/// name, so it rides its own slot; the two print as `import D, { a as $x } from "<spec>"`.
type private ImportEntry =
    {
        Path: JsModulePath
        /// The committed asset to materialise beside the output. `ValueNone` for a module
        /// this build EMITS — a sibling file of the package being compiled writes itself.
        Asset: JsRuntimeModule voption
        Named: System.Collections.Generic.HashSet<string * string>
        mutable Default: string option
        /// The `import * as <binding>` local. Its own statement: a namespace clause cannot
        /// ride the `{ named }` braces, so it is emitted beside the module's other import.
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

/// Per-compilation accumulator for the module imports a program needs — each imported
/// module recorded once, on first reference by the expression walker.
type JsImports =
    private
        {
            /// Package/assembly name → its committed runtime asset; only the referenced
            /// subset ships.
            Runtime: Map<string, JsRuntimeModule>
            /// The package directory the module being emitted sits in — every specifier
            /// is rendered from it. `ValueNone` for a program at the output root.
            SelfPackage: string voption
            Entries: System.Collections.Generic.Dictionary<JsModulePath, ImportEntry>
        }

module JsImports =

    let createIn (selfPackage: string voption) (runtime: Map<string, JsRuntimeModule>) : JsImports =
        {
            Runtime = runtime
            SelfPackage = selfPackage
            Entries = System.Collections.Generic.Dictionary()
        }

    /// A program emitted at the output root.
    let create (runtime: Map<string, JsRuntimeModule>) : JsImports = createIn ValueNone runtime

    /// The module `home` is imported from: a home refined to its DECLARING FILE names that
    /// file's module in its package directory; an assembly-only home, the committed asset.
    let private moduleOf (imports: JsImports) (home: JsHome) (what: string) : JsModulePath * JsRuntimeModule voption =
        match home.DeclaringFile with
        | ValueSome f -> JsModulePath.ofSource f.BucketName f.Relative, ValueNone
        | ValueNone ->
            match imports.Runtime |> Map.tryFind home.Assembly with
            | Some rt -> JsModulePath.asset rt.FileName, ValueSome rt
            | None -> failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what home.Assembly

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
                    Named = System.Collections.Generic.HashSet<string * string>()
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
                entry.Named.Add((name, alias)) |> ignore
                alias
            | ImportForm.Namespace ->
                // The local derives from the MODULE, not the export, so every `Namespace`
                // ref to `asm` shares one `import * as $ns_m` and reads `$ns_m.name` off it.
                let nsLocal = "$ns_" + asm.Replace('.', '_').Replace('/', '_')
                bindOnce entry.Namespace (fun v -> entry.Namespace <- Some v) "namespace import" nsLocal
                nsLocal + "." + name

    /// The local identifier for an external class, importing `className` from `home`'s
    /// module as `$<asm>_<className>` — the assembly disambiguates same-named classes.
    let addTypeRef (imports: JsImports) (home: JsHome) (className: string) : string =
        let entry = entryFor imports home (sprintf "external type '%s'" className)
        let alias = "$" + home.Assembly.Replace('.', '_') + "_" + className
        entry.Named.Add((className, alias)) |> ignore
        alias

    /// The local identifier for an external member, importing the export from `home`'s
    /// module as `$<exportName>`.
    let addMemberRef (imports: JsImports) (home: JsHome) (exportName: string) : string =
        let entry = entryFor imports home (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        entry.Named.Add((exportName, alias)) |> ignore
        alias

    /// The leading `import … from "<spec>"` block — one statement per imported module,
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

    /// The modules the emitted `import` block names, sorted as it emits them.
    let importedModules (imports: JsImports) : JsModulePath list =
        [ for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> kv.Key ]

    /// The committed runtime ASSETS referenced during the walk, sorted by module. Only the
    /// directly referenced ones: an asset that imports another asset is not closed over.
    /// A per-file module of the package under compilation is not here — that build writes it.
    let assets (imports: JsImports) : JsRuntimeModule list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                match kv.Value.Asset with
                | ValueSome asset -> yield asset
                | ValueNone -> ()
        ]
