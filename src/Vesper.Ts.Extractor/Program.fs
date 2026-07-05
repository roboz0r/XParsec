module Vesper.Ts.Extractor.Program

open Fable.Core

// argv[2..] under node (argv[0]=node, argv[1]=script).
[<Emit("process.argv")>]
let private argv: string[] = jsNative

[<EntryPoint>]
let main _ =
    let args = argv |> Array.skip 2

    // Two forms. The single-file form is UNCHANGED (the existing golden harness
    // invokes exactly `<dtsPath> <packageName> <outPath>`). The package form is
    // gated behind a leading `--package` flag so it can never collide with a local
    // `.d.ts` path, and pulls a package's cross-file `.d.ts` closure (item 18).
    match args with
    | [| "--package"; specifier; resolveFromDir; packageName; outPath |] ->
        Extractor.runPackage specifier resolveFromDir packageName outPath
    // Ambient-global entry: `--globals <packageName> <outPath> <dts…>` (variadic — the
    // merge fixture spans multiple sibling `.d.ts`, all fed to one program so the
    // checker merges cross-file declarations). Gated behind the flag so it never
    // collides with the single-file `<dts> <pkg> <out>` form.
    | _ when args.Length >= 4 && args.[0] = "--globals" ->
        Extractor.runGlobals (args.[3..] |> List.ofArray) args.[1] args.[2]
    // Real-scale lib extraction (Step 3): `--lib-globals <packageName> <outPath> <dts…>`
    // — same variadic shape as `--globals`, but sets `noLib` so the passed `lib.es*.d.ts`
    // files extract AS CONTENT (see `libOptions`). Used to vendor the `es2015` ref pack.
    | _ when args.Length >= 4 && args.[0] = "--lib-globals" ->
        Extractor.runLibGlobals (args.[3..] |> List.ofArray) args.[1] args.[2]
    // Ambient-module entry (decision A): `--ambient-modules <packageName> <outDir>
    // <dts…>` — same variadic shape as `--globals`, but the third arg is an output
    // DIRECTORY: this entry emits ONE manifest per quoted `declare module "…"` the
    // program declares (`@types/node`'s many modules), not a single artifact.
    | _ when args.Length >= 4 && args.[0] = "--ambient-modules" ->
        Extractor.runAmbientModules (args.[3..] |> List.ofArray) args.[1] args.[2]
    | [| dtsPath; packageName; outPath |] -> Extractor.run dtsPath packageName outPath
    | _ ->
        eprintfn "usage: extractor <dtsPath> <packageName> <outPath>"
        eprintfn "       extractor --package <specifier> <resolveFromDir> <packageName> <outPath>"
        eprintfn "       extractor --globals <packageName> <outPath> <dtsPath...>"
        eprintfn "       extractor --lib-globals <packageName> <outPath> <dtsPath...>"
        eprintfn "       extractor --ambient-modules <packageName> <outDir> <dtsPath...>"

    0
