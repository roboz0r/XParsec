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

/// The accumulating import specifiers for one home assembly: the runtime module, the
/// set of NAMED `(exportName, alias)` bindings, and the at-most-one DEFAULT binding
/// (`$alias` for an `import $alias from '<spec>'`). A TS default export cannot be
/// imported by name, so it rides its own slot; the printer combines them into
/// `import D, { a, b }`.
type private ImportEntry =
    {
        Module: JsRuntimeModule
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

/// Per-compilation accumulator for the runtime-module imports a program needs.
/// `addRef` / `addMemberRef` are called by the expression walker; each home assembly's
/// module is recorded once, on first reference. `importStatements` yields the leading
/// `import` block; `modules` yields the modules to materialise beside the output.
type JsImports =
    private
        {
            /// Package/assembly name → its runtime module. A subset is actually referenced.
            Runtime: Map<string, JsRuntimeModule>
            /// Home assembly → its accumulating import entry.
            Entries: System.Collections.Generic.Dictionary<string, ImportEntry>
        }

module JsImports =

    let create (runtime: Map<string, JsRuntimeModule>) : JsImports =
        {
            Runtime = runtime
            Entries = System.Collections.Generic.Dictionary()
        }

    /// The entry for `assembly`, recording it on first lookup. Fails loudly when the
    /// assembly has no runtime module in scope.
    let private entryFor (imports: JsImports) (assembly: string) (what: string) : ImportEntry =
        match imports.Entries.TryGetValue assembly with
        | true, entry -> entry
        | _ ->
            match imports.Runtime |> Map.tryFind assembly with
            | Some rt ->
                let entry =
                    {
                        Module = rt
                        Named = System.Collections.Generic.HashSet<string * string>()
                        Default = None
                        Namespace = None
                    }

                imports.Entries.[assembly] <- entry
                entry
            | None -> failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what assembly

    /// Resolve an `External` value node to its local import identifier, recording the
    /// import. `form` is the resolved symbol's `ExternalSymbol.ImportForm` (read off
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
    let addRef (imports: JsImports) (compiledName: string) (key: SymbolKey voption) (form: ImportForm) : string =
        match key with
        // A GLOBAL pack's export (its home is a `TsGlobalHomes.isGlobalHome`) is
        // provided by the JS runtime intrinsically: emit its BARE export name, record
        // NO import. Global rides the HOME, so this is decided by the key's home
        // assembly — the SAME single-source fact the provider mounted the pack under
        // `Js` by. (A Global class's construction bypasses `addRef` entirely via the
        // external-new arm; this covers a Global pack's free-function / variable
        // exports.) A node module MOUNTS under a namespace too (`node/fs → Node.Fs`)
        // but is NOT a global home, so it falls through to a real import below.
        | ValueSome(SymbolKey.ValueKey(Some asm, _, name)) when TsGlobalHomes.isGlobalHome asm -> name
        | ValueSome(SymbolKey.ValueKey(Some asm, ns, name)) ->
            let entry = entryFor imports asm (sprintf "external value '%s'" compiledName)
            let alias = "$" + (ns + "." + name).Replace('.', '_')

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

            match form with
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
        | _ -> failwithf "JS codegen (Step 5b): unsupported external value '%s' (key %A)" compiledName key

    /// Resolve an external union's case class to its local import identifier, importing
    /// the class export `className` from `asm`'s module aliased as `$<asm>_<className>`
    /// (`asm` disambiguates a same-named class from another package). Used by a
    /// `UnionCons` on an external union, whose case classes are imported rather than
    /// re-emitted. Fails loudly for a package with no authored runtime module.
    let addTypeRef (imports: JsImports) (asm: string) (className: string) : string =
        let entry = entryFor imports asm (sprintf "external type '%s'" className)
        let alias = "$" + asm.Replace('.', '_') + "_" + className
        entry.Named.Add((className, alias)) |> ignore
        alias

    /// Resolve an external member reference to its local import identifier, aliasing
    /// the export from `asm`'s module as `$<exportName>`. Member analogue of `addRef`.
    let addMemberRef (imports: JsImports) (asm: string) (exportName: string) : string =
        let entry = entryFor imports asm (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        entry.Named.Add((exportName, alias)) |> ignore
        alias

    /// The leading `import … from "./<file>"` block — one statement per home assembly,
    /// specifiers and assemblies sorted (deterministic). `List.sort` on the
    /// `(exportName, alias)` pairs is ordinal (F# structural string comparison),
    /// matching the emitted-text order the former pre-rendered `name as alias`
    /// `SortedSet` produced.
    let importStatements (imports: JsImports) : JsStatement list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                let entry = kv.Value
                let source = "./" + entry.Module.FileName
                // A default/named import statement, only when the entry has such a
                // binding (a pure-namespace module has none).
                if entry.Default.IsSome || entry.Named.Count > 0 then
                    JsStatement.Import(entry.Default, entry.Named |> List.ofSeq |> List.sort, source)
                // A `import * as ns` statement rides its own line beside the above.
                match entry.Namespace with
                | Some binding -> JsStatement.ImportNamespace(binding, source)
                | None -> ()
        ]

    /// The runtime modules referenced during the walk, sorted by assembly. Each emitted
    /// runtime asset is a self-contained leaf (no `.mjs` imports another), so the
    /// referenced set is exactly the set to materialise — no transitive closure needed.
    let modules (imports: JsImports) : JsRuntimeModule list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> kv.Value.Module
        ]
