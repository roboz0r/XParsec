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
type JsImports =
    private
        {
            /// Package/assembly name → its runtime module. A subset is actually referenced.
            Runtime: Map<string, JsRuntimeModule>
            /// Home assembly → its runtime module + sorted import specifiers referenced so far.
            Entries:
                System.Collections.Generic.Dictionary<
                    string,
                    JsRuntimeModule * System.Collections.Generic.SortedSet<string>
                 >
        }

module JsImports =

    let create (runtime: Map<string, JsRuntimeModule>) : JsImports =
        {
            Runtime = runtime
            Entries = System.Collections.Generic.Dictionary()
        }

    /// The `(module, specifier-set)` entry for `assembly`, recording it on first lookup.
    /// Fails loudly when the assembly has no runtime module in scope.
    let private entryFor
        (imports: JsImports)
        (assembly: string)
        (what: string)
        : JsRuntimeModule * System.Collections.Generic.SortedSet<string> =
        match imports.Entries.TryGetValue assembly with
        | true, entry -> entry
        | _ ->
            match imports.Runtime |> Map.tryFind assembly with
            | Some rt ->
                let entry =
                    rt, System.Collections.Generic.SortedSet<string>(System.StringComparer.Ordinal)

                imports.Entries.[assembly] <- entry
                entry
            | None -> failwithf "JS codegen: %s from assembly '%s' has no JS runtime module" what assembly

    /// Resolve an `External` value node to its local import identifier, recording the
    /// import. The export is aliased to `$<ns>_<name>` (`$` is illegal in F#, so
    /// collision-free). Fails loudly for a package with no authored runtime module.
    let addRef (imports: JsImports) (compiledName: string) (key: SymbolKey voption) : string =
        match key with
        | ValueSome(SymbolKey.ValueKey(Some asm, ns, name)) ->
            let _, specs = entryFor imports asm (sprintf "external value '%s'" compiledName)
            let alias = "$" + (ns + "." + name).Replace('.', '_')
            specs.Add(sprintf "%s as %s" name alias) |> ignore
            alias
        | _ -> failwithf "JS codegen (Step 5b): unsupported external value '%s' (key %A)" compiledName key

    /// Resolve an external member reference to its local import identifier, aliasing
    /// the export from `asm`'s module as `$<exportName>`. Member analogue of `addRef`.
    let addMemberRef (imports: JsImports) (asm: string) (exportName: string) : string =
        let _, specs = entryFor imports asm (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        specs.Add(sprintf "%s as %s" exportName alias) |> ignore
        alias

    /// The leading `import … from "./<file>"` block — one statement per home assembly,
    /// specifiers and assemblies sorted (deterministic).
    let importStatements (imports: JsImports) : JsStatement list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                let rt, specs = kv.Value
                JsStatement.Import(List.ofSeq specs, "./" + rt.FileName)
        ]

    /// The runtime modules referenced during the walk, sorted by assembly.
    let modules (imports: JsImports) : JsRuntimeModule list =
        [ for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> fst kv.Value ]
