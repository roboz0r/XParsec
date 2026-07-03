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
                    }

                imports.Entries.[assembly] <- entry
                entry
            | None -> failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what assembly

    /// Resolve an `External` value node to its local import identifier, recording the
    /// import. The export is aliased to `$<ns>_<name>` (`$` is illegal in F#, so
    /// collision-free). `form` is the resolved symbol's `ExternalSymbol.ImportForm`
    /// (read off the provider seam by the caller): `Default` lowers to a DEFAULT
    /// import (the alias binds the module's default export, no named specifier);
    /// `Named` to a named specifier. Fails loudly for a package with no authored
    /// runtime module.
    let addRef (imports: JsImports) (compiledName: string) (key: SymbolKey voption) (form: ImportForm) : string =
        match key with
        // A GLOBAL pack's export (its home is in `globalLibHomes`) is provided by the
        // JS runtime intrinsically: emit its BARE export name, record NO import. Global
        // rides the HOME, so this is decided by the key's home assembly — the SAME
        // single-source fact the provider mounted the pack under `Js` by. (A Global
        // class's construction bypasses `addRef` entirely via the external-new arm;
        // this covers a Global pack's free-function / variable exports.)
        | ValueSome(SymbolKey.ValueKey(Some asm, _, name)) when TsGlobalHomes.globalLibHomes.ContainsKey asm -> name
        | ValueSome(SymbolKey.ValueKey(Some asm, ns, name)) ->
            let entry = entryFor imports asm (sprintf "external value '%s'" compiledName)
            let alias = "$" + (ns + "." + name).Replace('.', '_')

            match form with
            | ImportForm.Default ->
                // At-most-one default binding per module, ENFORCED: a second, different
                // alias would silently clobber the first in the emitted `import` line.
                // Re-recording the same alias is the normal repeat-reference no-op.
                match entry.Default with
                | Some prev when prev <> alias ->
                    failwithf
                        "JS codegen: module '%s' already binds its default export as '%s'; cannot re-bind it as '%s'"
                        asm
                        prev
                        alias
                | _ -> entry.Default <- Some alias
            | ImportForm.Named -> entry.Named.Add((name, alias)) |> ignore

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
                JsStatement.Import(entry.Default, entry.Named |> List.ofSeq |> List.sort, "./" + entry.Module.FileName)
        ]

    /// The runtime modules referenced during the walk, sorted by assembly. Each emitted
    /// runtime asset is a self-contained leaf (no `.mjs` imports another), so the
    /// referenced set is exactly the set to materialise — no transitive closure needed.
    let modules (imports: JsImports) : JsRuntimeModule list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> kv.Value.Module
        ]
