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
    | [| dtsPath; packageName; outPath |] -> Extractor.run dtsPath packageName outPath
    | _ ->
        eprintfn "usage: extractor <dtsPath> <packageName> <outPath>"
        eprintfn "       extractor --package <specifier> <resolveFromDir> <packageName> <outPath>"

    0
