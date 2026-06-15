namespace XParsec.FSharp.Codegen.Js

open XParsec.FSharp.SemanticAnalysis

/// The hand-authored JS runtime modules a compiled program imports — Step 5b's
/// "first real runtime import". One ESM module per Vesper *package* whose module
/// functions a program calls (today only `Vesper.List`). This is a deliberate
/// stopgap: Step 6 (the structural-runtime bootstrap) replaces these hand-authored
/// sources with modules the JS backend compiles from the package's own `.fs`,
/// mirroring `--compiling-fslib`. Until then, the bodies are authored by hand to
/// the *same value shape the backend emits inline* for the package's types, so a
/// value built by the consumer and one built in the runtime interoperate.
type JsRuntimeModule =
    {
        /// The emitted file name (`Vesper.List.mjs`), written beside the output and
        /// named in the `import … from "./<FileName>"` specifier.
        FileName: string
        /// The verbatim ESM source.
        Source: string
    }

/// The registry mapping a *home assembly* (an `External` value node's
/// `SymbolKey.ValueKey` assembly — `Vesper.List`) to the runtime module that
/// supplies its module functions at run time. The import path is derived from the
/// assembly name (`./<asm>.mjs`), so the emission is target-general: any package
/// with a registered runtime is importable; a call into one *without* a runtime
/// fails loudly at emit (the F-step "just-in-time, not front-loaded" rule).
module JsRuntime =

    /// `Vesper.List` — the cons-list module functions. Operates *structurally* on
    /// cons-list values: `{ tag: 0 }` = `Empty`, `{ tag: 1, Head, Tail }` = `Cons`
    /// — the exact own-property shape the JS backend emits inline for the
    /// `Vesper.Collections.List` union (`List_Empty` / `List_Cons` instances). The
    /// JS `match` compiler tests `.tag` and reads `.Head`/`.Tail` (never
    /// `instanceof`), so a list the consumer built with the inline classes and one
    /// these functions build (plain `cons` cells) are fully interchangeable. Curried
    /// to match the backend's unary-arrow calling convention (`map(f)(xs)`).
    let private vesperListSource =
        "// Vesper.List — JS runtime (Step 5b, hand-authored). Step 6 replaces this\n"
        + "// with a backend-compiled module. Cons-list shape: { tag: 0 } = Empty,\n"
        + "// { tag: 1, Head, Tail } = Cons — the shape the backend emits inline.\n"
        + "const empty = { tag: 0 };\n"
        + "const cons = (h, t) => ({ tag: 1, Head: h, Tail: t });\n"
        + "\n"
        + "export const length = (xs) => {\n"
        + "  let n = 0;\n"
        + "  while (xs.tag === 1) { n = n + 1; xs = xs.Tail; }\n"
        + "  return n;\n"
        + "};\n"
        + "\n"
        + "export const isEmpty = (xs) => xs.tag === 0;\n"
        + "\n"
        + "export const head = (xs) => {\n"
        + "  if (xs.tag === 0) throw new Error(\"The input list was empty.\");\n"
        + "  return xs.Head;\n"
        + "};\n"
        + "\n"
        + "export const tail = (xs) => {\n"
        + "  if (xs.tag === 0) throw new Error(\"The input list was empty.\");\n"
        + "  return xs.Tail;\n"
        + "};\n"
        + "\n"
        + "export const rev = (xs) => {\n"
        + "  let acc = empty;\n"
        + "  while (xs.tag === 1) { acc = cons(xs.Head, acc); xs = xs.Tail; }\n"
        + "  return acc;\n"
        + "};\n"
        + "\n"
        + "export const map = (f) => (xs) => {\n"
        + "  let acc = empty;\n"
        + "  while (xs.tag === 1) { acc = cons(f(xs.Head), acc); xs = xs.Tail; }\n"
        + "  return rev(acc);\n"
        + "};\n"

    /// The runtime module for a home assembly, or `ValueNone` when none is authored
    /// yet (the call site then fails loudly — the runtime arrives with the F-step
    /// that first exercises it).
    let tryForAssembly (assembly: string) : JsRuntimeModule voption =
        match assembly with
        | "Vesper.List" ->
            ValueSome
                {
                    FileName = "Vesper.List.mjs"
                    Source = vesperListSource
                }
        | _ -> ValueNone

/// Per-compilation accumulator for the runtime-module imports a program needs
/// (Step 5b). The expression walker calls `addRef` for every `External` value
/// node; `buildProgram` reads `importStatements` (the leading `import …` block)
/// and `Codegen` reads `modules` (the `JsRuntimeModule`s to materialise beside
/// the output). Each home assembly's runtime module is resolved **once**, on its
/// first reference — the one place a missing runtime fails loudly — so the import
/// specifier and the materialised file are the same `JsRuntimeModule` by
/// construction, and neither downstream consumer re-resolves it.
type JsImports =
    private
        {
            /// Home assembly (`Vesper.List`) → its resolved runtime module + the
            /// sorted `name as $alias` import specifiers referenced so far.
            Entries:
                System.Collections.Generic.Dictionary<
                    string,
                    JsRuntimeModule * System.Collections.Generic.SortedSet<string>
                 >
        }

module JsImports =

    let create () : JsImports =
        {
            Entries = System.Collections.Generic.Dictionary()
        }

    /// Resolve an `External` value node (a Vesper module function — `List.length`)
    /// to the local identifier of its runtime import, recording the import. The
    /// home assembly (`ValueKey`'s assembly, `Vesper.List`) selects the runtime
    /// module (`JsRuntime.tryForAssembly`, resolved once on first reference); the
    /// import path is `./<FileName>.mjs`, derived from that module — so this is
    /// target-general, not list-specific. The export is aliased to a `$`-prefixed
    /// name (illegal in F#, so collision-free against user / synthesised
    /// identifiers) built from the fully-qualified value name, deterministic across
    /// modules. A call into a package with no authored runtime fails loudly (its
    /// runtime arrives with the F-step that first needs it).
    let addRef (imports: JsImports) (compiledName: string) (key: SymbolKey voption) : string =
        match key with
        | ValueSome(SymbolKey.ValueKey(Some asm, ns, name)) ->
            let _, specs =
                match imports.Entries.TryGetValue asm with
                | true, entry -> entry
                | _ ->
                    match JsRuntime.tryForAssembly asm with
                    | ValueSome rt ->
                        let entry =
                            rt, System.Collections.Generic.SortedSet<string>(System.StringComparer.Ordinal)

                        imports.Entries.[asm] <- entry
                        entry
                    | ValueNone ->
                        failwithf
                            "JS codegen (Step 5b): external value '%s' from assembly '%s' has no JS runtime module"
                            compiledName
                            asm

            let alias = "$" + (ns + "." + name).Replace('.', '_')
            specs.Add(sprintf "%s as %s" name alias) |> ignore
            alias
        | _ -> failwithf "JS codegen (Step 5b): unsupported external value '%s' (key %A)" compiledName key

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
