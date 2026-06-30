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

/// Per-compilation accumulator for the runtime-module imports a program needs.
/// `addRef` / `addMemberRef` are called by the expression walker; each home assembly's
/// module is recorded once, on first reference. `importStatements` yields the leading
/// `import` block; `modules` yields the modules to materialise beside the output.
/// The accumulating import specifiers for one home assembly: the runtime module, the
/// sorted set of NAMED specifiers (`name as $alias`), and the at-most-one DEFAULT binding
/// (`$alias` for an `import $alias from '<spec>'`). A TS default export cannot be imported
/// by name, so it rides its own slot; the printer combines them into `import D, { a, b }`.
type private ImportEntry =
    {
        Module: JsRuntimeModule
        Named: System.Collections.Generic.SortedSet<string>
        mutable Default: string option
    }

type JsImports =
    private
        {
            /// Package/assembly name → its runtime module. A subset is actually referenced.
            Runtime: Map<string, JsRuntimeModule>
            /// The `(asm, ns, name)` identities whose `ImportShape` is `Default` (from
            /// `TsManifestProvider.defaultValueKeys`): `addRef` lowers these to a default
            /// import. Import shape cannot ride the node/`SymbolKey`, so the backend seeds
            /// it here. Empty ⇒ every value is a named import (the prior behaviour).
            DefaultKeys: Set<string * string * string>
            /// Home assembly → its accumulating import entry.
            Entries: System.Collections.Generic.Dictionary<string, ImportEntry>
        }

module JsImports =

    /// Build with a set of default-exported `ValueKey`s (the channel import shape rides,
    /// since it cannot ride the node). `create` is the empty-set special case.
    let createWithDefaults
        (runtime: Map<string, JsRuntimeModule>)
        (defaultKeys: Set<string * string * string>)
        : JsImports =
        {
            Runtime = runtime
            DefaultKeys = defaultKeys
            Entries = System.Collections.Generic.Dictionary()
        }

    let create (runtime: Map<string, JsRuntimeModule>) : JsImports = createWithDefaults runtime Set.empty

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
                        Named = System.Collections.Generic.SortedSet<string>(System.StringComparer.Ordinal)
                        Default = None
                    }

                imports.Entries.[assembly] <- entry
                entry
            | None -> failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what assembly

    /// Resolve an `External` value node to its local import identifier, recording the
    /// import. The export is aliased to `$<ns>_<name>` (`$` is illegal in F#, so
    /// collision-free). A `DefaultKeys` hit lowers to a DEFAULT import (the alias binds
    /// the module's default export, no named specifier); otherwise a named specifier.
    /// Fails loudly for a package with no authored runtime module.
    let addRef (imports: JsImports) (compiledName: string) (key: SymbolKey voption) : string =
        match key with
        | ValueSome(SymbolKey.ValueKey(Some asm, ns, name)) ->
            let entry = entryFor imports asm (sprintf "external value '%s'" compiledName)
            let alias = "$" + (ns + "." + name).Replace('.', '_')

            if imports.DefaultKeys.Contains((asm, ns, name)) then
                entry.Default <- Some alias
            else
                entry.Named.Add(sprintf "%s as %s" name alias) |> ignore

            alias
        | _ -> failwithf "JS codegen (Step 5b): unsupported external value '%s' (key %A)" compiledName key

    /// Resolve an external union's case class to its local import identifier, importing
    /// the class export `className` from `asm`'s module aliased as `$<asm>_<className>`
    /// (`asm` disambiguates a same-named class from another package). Used by a
    /// `UnionCons` on an external union, whose case classes are imported rather than
    /// re-emitted. Fails loudly for a package with no authored runtime module.
    let addTypeRef (imports: JsImports) (asm: string) (className: string) : string =
        let entry = entryFor imports asm (sprintf "external type '%s'" className)
        let alias = "$" + asm.Replace('.', '_') + "_" + className
        entry.Named.Add(sprintf "%s as %s" className alias) |> ignore
        alias

    /// Resolve an external member reference to its local import identifier, aliasing
    /// the export from `asm`'s module as `$<exportName>`. Member analogue of `addRef`.
    let addMemberRef (imports: JsImports) (asm: string) (exportName: string) : string =
        let entry = entryFor imports asm (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        entry.Named.Add(sprintf "%s as %s" exportName alias) |> ignore
        alias

    /// The leading `import … from "./<file>"` block — one statement per home assembly,
    /// specifiers and assemblies sorted (deterministic).
    let importStatements (imports: JsImports) : JsStatement list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                let entry = kv.Value
                JsStatement.Import(entry.Default, List.ofSeq entry.Named, "./" + entry.Module.FileName)
        ]

    /// The runtime modules referenced during the walk, sorted by assembly. Each emitted
    /// runtime asset is a self-contained leaf (no `.mjs` imports another), so the
    /// referenced set is exactly the set to materialise — no transitive closure needed.
    let modules (imports: JsImports) : JsRuntimeModule list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> kv.Value.Module
        ]
