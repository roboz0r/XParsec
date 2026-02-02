module XParsec.CLArgs.Interactive

open System

open XParsec
open XParsec.Parsers
open XParsec.CLArgs

type Args =
    | DumpFile of string
    | AllText of bool

let options =
    [
        CLSetting("dump-file", DumpFile, None, true)
        CLBoolSetting("all-text", AllText, None, true)
    ]

[<EntryPoint>]
let main argv =
    // printfn "Interactive test for XParsec.CLArgs.\n    %s" (String.Join("; ", argv))
    Console.WriteLine("Interactive test for XParsec.CLArgs.\n    {0}", (String.Join("; ", argv)))

    let parser = options |> CLParser.ofOptions

    let reader = Reader.ofArray argv ()

    match parser reader with
    | Ok result ->
        let args = result

        if args.Length = 0 then
            // printfn "No arguments."
            Console.WriteLine("No arguments.")
        else
            for arg in args do
                match arg with
                | DumpFile file ->
                    // printfn "Dump file '%s'..." file
                    Console.WriteLine("Dump file '{0}'...", file)

                    if System.IO.File.Exists file then
                        // printfn "File exists."
                        Console.WriteLine("File exists.")

                        if args.Contains(AllText true) then
                            // printfn "%s" (System.IO.File.ReadAllText file)
                            Console.WriteLine("{0}", (System.IO.File.ReadAllText file))
                    else
                        // printfn "File does not exist."
                        Console.WriteLine("File does not exist.")
                | AllText allText -> ()
    | Error e -> failwithf "%A" e

    0
