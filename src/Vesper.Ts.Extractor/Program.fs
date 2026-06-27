module Vesper.Ts.Extractor.Program

open Fable.Core

// argv[2..] under node (argv[0]=node, argv[1]=script).
[<Emit("process.argv")>]
let private argv: string[] = jsNative

[<EntryPoint>]
let main _ =
    let args = argv |> Array.skip 2

    match args with
    | [| dtsPath; packageName; outPath |] -> Extractor.run dtsPath packageName outPath
    | _ -> eprintfn "usage: extractor <dtsPath> <packageName> <outPath>"

    0
