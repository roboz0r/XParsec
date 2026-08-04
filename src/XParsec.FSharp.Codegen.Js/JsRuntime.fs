namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// A hand-authored JS runtime module a compiled program imports and ships beside its
/// output. One ESM module per Vesper package whose functions a program reaches at
/// run time. The source is a committed `.mjs` asset declared by the package manifest's
/// `runtime-js` key; the backend resolves the set and materialises the referenced ones.
type JsRuntimeModule =
    {
        /// The emitted file name (`Vesper.List.mjs`), written beside the output and
        /// named in the `import … from "./<FileName>"` specifier.
        FileName: string
        /// The verbatim ESM source (read from the committed `.mjs` asset).
        Source: string
    }

/// Where a `.mjs` sits in the emitted output tree, relative to the output ROOT: the
/// package directory it belongs to (`ValueNone` = the root itself) and its file name.
///
/// A specifier is rendered BETWEEN two of these, never as a fixed prefix: a package
/// member reaching a sibling, a root program reaching that same module, and a package
/// member reaching a root-level asset are three different spellings of one relation.
[<Struct>]
type JsModulePath =
    {
        Package: string voption
        FileName: string
    }

module JsModulePath =

    /// A committed runtime ASSET: referenced and copied to the output root, one per
    /// package — the analogue of an app's `.dll` beside its executable.
    let asset (fileName: string) : JsModulePath =
        {
            Package = ValueNone
            FileName = fileName
        }

    /// The module STEM of a source file: its name without extensions, minus a trailing
    /// `.js` target segment (`ops-platform.js.fs` → `ops-platform`). That is the stem the
    /// manifest pairs a body with its `.fsi` contract under, so a module is named for the
    /// declarations it implements rather than for which target's body it happens to be.
    let stem (relative: string) : string =
        let noExt = System.IO.Path.GetFileNameWithoutExtension relative

        if noExt.EndsWith(".js", System.StringComparison.Ordinal) then
            noExt.Substring(0, noExt.Length - 3)
        else
            noExt

    /// One emitting source file of `package`, inside that package's own directory.
    let ofSource (package: string) (relative: string) : JsModulePath =
        {
            Package = ValueSome package
            FileName = stem relative + ".mjs"
        }

    /// The specifier a module emitted in `fromPackage` (`ValueNone` = the output root)
    /// names `target` by. The hop out of a package directory is COMPUTED, so nothing here
    /// assumes the importer is a program at the root.
    let specifierFrom (fromPackage: string voption) (target: JsModulePath) : string =
        let toRoot =
            match fromPackage with
            | ValueNone -> "./"
            | ValueSome _ -> "../"

        match target.Package with
        | ValueSome p when fromPackage = ValueSome p -> "./" + target.FileName
        | ValueSome p -> toRoot + p + "/" + target.FileName
        | ValueNone -> toRoot + target.FileName

/// The accumulating import specifiers for one imported MODULE: where it sits, the asset
/// to ship for it (if it is one), the set of NAMED `(exportName, alias)` bindings, and
/// the at-most-one DEFAULT binding (`$alias` for an `import $alias from '<spec>'`). A TS
/// default export cannot be imported by name, so it rides its own slot; the printer
/// combines them into `import D, { a, b }`.
type private ImportEntry =
    {
        Path: JsModulePath
        /// The committed asset to materialise beside the output. `ValueNone` for a module
        /// this build EMITS — a sibling file of the package being compiled writes itself.
        Asset: JsRuntimeModule voption
        /// `(exportName, alias)` pairs — structure, not pre-rendered `name as alias`
        /// strings; `JsPrint` owns the `as` spelling. A `HashSet` because re-recording
        /// a repeat reference must be a no-op; `importStatements` sorts on read.
        Named: System.Collections.Generic.HashSet<string * string>
        mutable Default: string option
        /// The at-most-one `import * as <binding>` namespace local for this module
        /// (`Schema.ImportShape.Namespace`). Its own statement — a namespace clause
        /// cannot ride the `{ named }` braces — so it is emitted beside any default/
        /// named import for the same source.
        mutable Namespace: string option
    }

/// An external VALUE reference, as the import machinery needs it: WHAT is referenced
/// (the key — a nominal identity, which is all a key is), WHERE it lives, and HOW its
/// home module exports it. The home and the form are facts of the RESOLVED SHAPE, never
/// of the key, so a provider-resolved value pairs its key with the `SymbolOrigin.Home`
/// the provider stamped (`EmitJsContext.externalValueRef`), and a codegen-synthesised
/// runtime entry — no front-end symbol resolves to it — names its own runtime module.
type JsValueRef =
    {
        /// `ValueNone` for a node the front end left unkeyed: `addRef` has nothing to
        /// import and fails loudly.
        Key: SymbolKey voption
        /// The home module the export is imported from. `Origin.Unstamped` is unimportable.
        Home: Origin
        Form: ImportForm
    }

/// Per-compilation accumulator for the module imports a program needs. `addRef` /
/// `addMemberRef` are called by the expression walker; each imported module is recorded
/// once, on first reference. `importStatements` yields the leading `import` block;
/// `assets` yields the committed modules to materialise beside the output.
type JsImports =
    private
        {
            /// Package/assembly name → its committed runtime asset. A subset is actually
            /// referenced.
            Runtime: Map<string, JsRuntimeModule>
            /// The package directory the module being emitted sits in — every specifier
            /// is rendered from it. `ValueNone` for a program at the output root.
            SelfPackage: string voption
            /// Imported module → its accumulating import entry.
            Entries: System.Collections.Generic.Dictionary<JsModulePath, ImportEntry>
        }

module JsImports =

    /// `selfPackage` is the package directory the module under emission sits in — one
    /// member of a package build. `ValueNone` is a program at the output root.
    let createIn (selfPackage: string voption) (runtime: Map<string, JsRuntimeModule>) : JsImports =
        {
            Runtime = runtime
            SelfPackage = selfPackage
            Entries = System.Collections.Generic.Dictionary()
        }

    /// A program emitted at the output root.
    let create (runtime: Map<string, JsRuntimeModule>) : JsImports = createIn ValueNone runtime

    /// The module a resolved `home` is imported from. A home refined to its DECLARING FILE
    /// names one module of that package's directory — the per-file artifact this backend
    /// emits; an assembly-only home (a `.fsi` contract view, a TS manifest, a
    /// codegen-synthesised runtime entry) is served by the package's committed asset.
    let private moduleOf (imports: JsImports) (home: Origin) (what: string) : JsModulePath * JsRuntimeModule voption =
        match home.DeclaringFile with
        | ValueSome f -> JsModulePath.ofSource f.BucketName f.Relative, ValueNone
        | ValueNone ->
            match home.AssemblyOption with
            | ValueNone -> failwithf "JS codegen: %s carries no home assembly" what
            | ValueSome assembly ->
                match imports.Runtime |> Map.tryFind assembly with
                | Some rt -> JsModulePath.asset rt.FileName, ValueSome rt
                | None -> failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what assembly

    /// The entry for `home`'s module, recording it on first lookup. Fails loudly when the
    /// home names no module to import from.
    let private entryFor (imports: JsImports) (home: Origin) (what: string) : ImportEntry =
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

    /// Resolve an `External` value node to its local import identifier, recording the
    /// import. `ref.Form` is the resolved symbol's `ExternalSymbol.ImportForm` (read off
    /// the provider seam by the caller), selecting the import-statement shape:
    ///   • `Named`            → a named specifier `{ name as $<ns>_<name> }`, returns
    ///     the alias;
    ///   • `Default`/`CommonJs` → a DEFAULT binding (the alias binds the module's
    ///     default export / `module.exports`, no named specifier), returns the alias.
    ///     `export =` binds `module.exports` to the same default slot under the
    ///     esModuleInterop lowering, so `CommonJs` rides the `Default` path;
    ///   • `Namespace`        → an `import * as <nsLocal>` binding, returns the
    ///     MEMBER path `<nsLocal>.<name>` (the export is read off the namespace
    ///     object).
    /// The alias/namespace-local is `$`-prefixed (`$` is illegal in F#, so
    /// collision-free). Fails loudly for a package with no authored runtime module.
    let addRef (imports: JsImports) (compiledName: string) (ref: JsValueRef) : string =
        // An external value is a BINDING whose resolved shape names a home module. A
        // project-local binding (`Origin.Unstamped`) has no module to import from, so it is
        // unsupported here — the same error a non-binding key gets, stated once.
        let unsupported () =
            failwithf "JS codegen: unsupported external value '%s' (key %A)" compiledName ref.Key

        let b =
            match ref.Key with
            | ValueSome(SymbolKey.Binding b) -> b
            | _ -> unsupported ()

        match ref.Home.AssemblyOption with
        | ValueNone -> unsupported ()
        // A GLOBAL pack's export (its home is a `TsGlobalHomes.isGlobalHome`) is
        // provided by the JS runtime intrinsically: emit its BARE export name, record
        // NO import. Global rides the HOME, so this is decided by the resolved shape's
        // home — the SAME single-source fact the provider mounted the pack under `Js` by.
        // (A Global class's construction bypasses `addRef` entirely via the
        // external-new arm; this covers a Global pack's free-function / variable
        // exports.) A node module MOUNTS under a namespace too (`node/fs → Node.Fs`)
        // but is NOT a global home, so it falls through to a real import below.
        | ValueSome asm when TsGlobalHomes.isGlobalHome asm -> b.Name
        | ValueSome asm ->
            let name = b.Name
            let entry = entryFor imports ref.Home (sprintf "external value '%s'" compiledName)

            // The alias is `$<holder>_<name>` with every `.` underscored. An UNQUALIFIED
            // binding's holder is the global namespace (`""`), so the leading `.` of the
            // join survives as a leading `_` (`$_f`) — the shape the emitted import lines
            // (and their tests) expect. Joining here rather than through `qualifiedName`
            // (which drops the empty holder) keeps that spelling exact.
            let alias =
                "$" + (SymbolKeyOps.holderFullName b.Decl + "." + name).Replace('.', '_')

            // Bind an at-most-one module slot (`Default`/`Namespace`), ENFORCED: a
            // second, different local would silently clobber the first in the emitted
            // `import` line; re-recording the SAME local is the normal repeat-reference
            // no-op. `slotDesc` names the slot for the diagnostic.
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
                // One namespace local per module; a member is read off it (`ns.name`).
                // The local derives from the module (not the export), so every Namespace
                // ref to `asm` shares it.
                let nsLocal = "$ns_" + asm.Replace('.', '_').Replace('/', '_')
                bindOnce entry.Namespace (fun v -> entry.Namespace <- Some v) "namespace import" nsLocal
                nsLocal + "." + name

    /// The home's assembly, which an alias is disambiguated by so a same-named class from
    /// another package cannot collide. Fails loudly on an unstamped home.
    let private homeAssembly (home: Origin) (what: string) : string =
        match home.AssemblyOption with
        | ValueSome a -> a
        | ValueNone -> failwithf "JS codegen: %s carries no home assembly" what

    /// Resolve an external union's case class to its local import identifier, importing
    /// the class export `className` from `home`'s module aliased as `$<asm>_<className>`.
    /// Used by a `UnionCons` on an external union, whose case classes are imported rather
    /// than re-emitted. Fails loudly for a home that names no module.
    let addTypeRef (imports: JsImports) (home: Origin) (className: string) : string =
        let what = sprintf "external type '%s'" className
        let entry = entryFor imports home what
        let alias = "$" + (homeAssembly home what).Replace('.', '_') + "_" + className
        entry.Named.Add((className, alias)) |> ignore
        alias

    /// Resolve an external member reference to its local import identifier, aliasing
    /// the export from `home`'s module as `$<exportName>`. Member analogue of `addRef`.
    let addMemberRef (imports: JsImports) (home: Origin) (exportName: string) : string =
        let entry = entryFor imports home (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        entry.Named.Add((exportName, alias)) |> ignore
        alias

    /// The leading `import … from "<spec>"` block — one statement per imported module,
    /// specifiers and modules sorted (deterministic). `List.sort` on the
    /// `(exportName, alias)` pairs is ordinal (F# structural string comparison),
    /// matching the emitted-text order the former pre-rendered `name as alias`
    /// `SortedSet` produced.
    let importStatements (imports: JsImports) : JsStatement list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                let entry = kv.Value
                let source = JsModulePath.specifierFrom imports.SelfPackage entry.Path
                // A default/named import statement, only when the entry has such a
                // binding (a pure-namespace module has none).
                if entry.Default.IsSome || entry.Named.Count > 0 then
                    JsStatement.Import(entry.Default, entry.Named |> List.ofSeq |> List.sort, source)
                // A `import * as ns` statement rides its own line beside the above.
                match entry.Namespace with
                | Some binding -> JsStatement.ImportNamespace(binding, source)
                | None -> ()
        ]

    /// The modules the emitted `import` block names, sorted as it emits them. An entry is
    /// only ever created by a reference that then binds something, so this is exactly the
    /// set of specifier targets — what a package build checks resolves to a module it
    /// writes, without reading back the generated text.
    let importedModules (imports: JsImports) : JsModulePath list =
        [ for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> kv.Key ]

    /// The committed runtime ASSETS referenced during the walk, sorted by module. Each is
    /// a self-contained leaf (no asset `.mjs` imports another), so the referenced set is
    /// exactly the set to materialise — no transitive closure needed. A per-file module of
    /// a package under compilation is not here: that build writes it.
    let assets (imports: JsImports) : JsRuntimeModule list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                match kv.Value.Asset with
                | ValueSome asset -> yield asset
                | ValueNone -> ()
        ]
