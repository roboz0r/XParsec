module Vesper.Ts.Extractor.Program

open Fable.Core

// argv[2..] under node (argv[0]=node, argv[1]=script).
[<Emit("process.argv")>]
let private argv: string[] = jsNative

[<EntryPoint>]
let main _ =
    let args = argv |> Array.skip 2

    // A leading flag distinguishes each mode from the bare `<dts> <pkg> <out>` form.
    // `--package` resolves a package specifier and pulls its cross-file `.d.ts` closure.
    match args with
    | [| "--package"; specifier; resolveFromDir; packageName; outPath |] ->
        Extractor.runPackage specifier resolveFromDir packageName outPath
    // Variadic: the sibling `.d.ts` all go into ONE program, so the checker merges
    // their cross-file declarations before the walk.
    | _ when args.Length >= 4 && args.[0] = "--globals" ->
        Extractor.runGlobals (args.[3..] |> List.ofArray) args.[1] args.[2]
    // As `--globals`, but with `noLib`, so the passed `lib.es*.d.ts` extract AS CONTENT
    // instead of being filtered out as the default lib.
    | _ when args.Length >= 4 && args.[0] = "--lib-globals" ->
        Extractor.runLibGlobals (args.[3..] |> List.ofArray) args.[1] args.[2]
    // Third arg is an output DIRECTORY, not a file: one manifest per quoted
    // `declare module "…"` the program declares (`@types/node` has many).
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
