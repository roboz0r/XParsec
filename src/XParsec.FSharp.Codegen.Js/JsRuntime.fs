namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// A hand-authored JS runtime module a compiled program imports and ships beside
/// its output — the JS analogue of a platform-support binary (cf. Vesper.Printf's
/// committed CLR DLL). One ESM module per Vesper *package* whose functions /
/// structural core a program reaches at run time (`Vesper.List`'s cons-list module
/// functions; `Vesper.Core`'s structural `equals` / `structuralHash`).
///
/// The source is **not embedded in the compiler**: each module is a committed
/// `.mjs` asset living with its package (`src/<pkg>/<pkg>.mjs`), declared by the
/// package manifest's `runtime-js` key and read through
/// `ReferencedProject.runtimeModules`. The backend resolves the set for its target,
/// threads it in (`Codegen.compileWith`), and materialises the *referenced* ones
/// beside the output. The modules are authored to the *same value shape the backend
/// emits inline* for their package's types, so a value built by the consumer and
/// one built in the runtime interoperate (Step 5b's plain `{ tag, Head, Tail }`
/// cons cells; Step 6's shape-keyed `equals`). This is the seam Route B (a
/// backend-*compiled* runtime, the `--compiling-fslib` bootstrap) later slots into —
/// it generates the same module, leaving import + materialise unchanged.
type JsRuntimeModule =
    {
        /// The emitted file name (`Vesper.List.mjs`), written beside the output and
        /// named in the `import … from "./<FileName>"` specifier.
        FileName: string
        /// The verbatim ESM source (read from the committed `.mjs` asset).
        Source: string
    }

/// Per-compilation accumulator for the runtime-module imports a program needs.
/// Built over a resolved `Runtime` registry (package/assembly name →
/// `JsRuntimeModule`, read from the manifests' `runtime-js` assets by
/// `ReferencedProject.runtimeModules`). The expression walker calls `addRef` for
/// every `External` value node and `ensureCoreImport` when it emits a
/// structural-core template; `buildProgram` reads `importStatements` (the leading
/// `import …` block) and `Codegen` reads `modules` (the `JsRuntimeModule`s to
/// materialise). Each home assembly's module is recorded **once**, on its first
/// reference — the one place a missing runtime fails loudly — so the import
/// specifier and the materialised file are the same `JsRuntimeModule` by
/// construction.
type JsImports =
    private
        {
            /// Resolved package/assembly name → its runtime module: the fixed
            /// registry the program draws imports from. A subset is actually
            /// referenced during the walk and lands in `Entries`.
            Runtime: Map<string, JsRuntimeModule>
            /// Home assembly (`Vesper.List`) → its runtime module + the sorted
            /// import specifiers referenced so far (`name as $alias` for module
            /// functions; the bare export name for the structural core).
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

    /// The `(module, specifier-set)` entry for `assembly`, recording it as
    /// referenced on first lookup. Fails loudly when the assembly has no runtime
    /// module in scope — the F-step "just-in-time, not front-loaded" rule: a
    /// package's `runtime-js` asset arrives with the step that first needs it.
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

    /// Resolve an `External` value node (a Vesper module function — `List.length`)
    /// to the local identifier of its runtime import, recording the import. The home
    /// assembly (`ValueKey`'s assembly, `Vesper.List`) selects the runtime module
    /// (from the resolved `Runtime` registry, recorded once on first reference); the
    /// import path is `./<FileName>`, derived from that module — so this is
    /// target-general, not list-specific. The export is aliased to a `$`-prefixed
    /// name (illegal in F#, so collision-free against user / synthesised
    /// identifiers) built from the fully-qualified value name, deterministic across
    /// modules. A call into a package with no authored runtime fails loudly.
    let addRef (imports: JsImports) (compiledName: string) (key: SymbolKey voption) : string =
        match key with
        | ValueSome(SymbolKey.ValueKey(Some asm, ns, name)) ->
            let _, specs = entryFor imports asm (sprintf "external value '%s'" compiledName)
            let alias = "$" + (ns + "." + name).Replace('.', '_')
            specs.Add(sprintf "%s as %s" name alias) |> ignore
            alias
        | _ -> failwithf "JS codegen (Step 5b): unsupported external value '%s' (key %A)" compiledName key

    /// Resolve an external *member* reference (Step 7 — a consumer's `o.IsSome` on an
    /// imported `Option`) to the local identifier of its runtime import. The member is
    /// emitted by its home package in library mode under the shared mangled name
    /// (`Option__get_IsSome`), so that is the runtime module's export; this records the
    /// import of that export from `asm`'s module and aliases it to `$<exportName>` — the
    /// `$` prefix (illegal in F#) keeps it collision-free against the bare local mangled
    /// names a self-compile emits, and the mangled name carries no `.` so it needs no
    /// further sanitising. The member analogue of `addRef`; a member on a package with
    /// no authored runtime fails loudly the same way.
    let addMemberRef (imports: JsImports) (asm: string) (exportName: string) : string =
        let _, specs = entryFor imports asm (sprintf "external member '%s'" exportName)
        let alias = "$" + exportName
        specs.Add(sprintf "%s as %s" exportName alias) |> ignore
        alias

    /// Ensure the structural-core runtime (`Vesper.Core.mjs`) is imported under the
    /// *unaliased* export `name` (`equals` / `structuralHash`). Module functions
    /// alias to `$Ns_name` (`addRef`) to dodge collisions, but these are referenced
    /// verbatim from the `ops-platform` equality / hash `$N` templates
    /// (`equals($0, $1)`, `structuralHash($0)`), so the export name must reach the
    /// call site unchanged. Registered in the same `Entries` map as the aliased
    /// runtimes, so it orders and materialises through the existing
    /// `importStatements` / `modules` paths. Driven by the JS backend on emitting a
    /// template that references one of these helpers (Step 6), so a program that
    /// never compares / hashes an aggregate pulls in no core import.
    let ensureCoreImport (imports: JsImports) (name: string) : unit =
        let _, specs = entryFor imports "Vesper.Core" "the structural core"
        specs.Add name |> ignore

    /// The leading `import … from "./<file>"` block — one statement per home
    /// assembly, specifiers sorted and assemblies sorted (deterministic). Imports
    /// must precede every reference, the emitted classes included.
    let importStatements (imports: JsImports) : JsStatement list =
        [
            for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) do
                let rt, specs = kv.Value
                JsStatement.Import(List.ofSeq specs, "./" + rt.FileName)
        ]

    /// The runtime modules referenced during the walk, sorted by assembly
    /// (deterministic) — materialised beside the output by `Codegen.materialise`.
    let modules (imports: JsImports) : JsRuntimeModule list =
        [ for kv in imports.Entries |> Seq.sortBy (fun kv -> kv.Key) -> fst kv.Value ]
