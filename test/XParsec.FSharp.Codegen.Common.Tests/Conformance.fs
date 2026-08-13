/// The backend conformance corpus (`test/Codegen.Conformance/`): programs in our own F#
/// dialect, on disk as data no fsproj compiles, turned into Expecto tests parameterised over
/// a `Backend` and judged against the manifest, never against another backend's output.
module XParsec.FSharp.Codegen.Common.Tests.Conformance

open System
open System.IO
open Expecto
open XParsec.Toml

/// What became of a conformance program that was compiled and run. A fault has no portable
/// exit code: the CLR harness invokes the entry point in-process by reflection, where an
/// uncaught exception never becomes one, while Node runs out of process and yields a real one.
[<RequireQualifiedAccess>]
type RunOutcome =
    /// The program ran to completion, with this exit code and stdout.
    | Completed of exitCode: int * stdout: string
    /// The program FAULTED: an uncaught runtime error. `description` is whatever the
    /// target can say about it (the exception type + message on the CLR; stdout plus
    /// stderr from Node). A fault is therefore matched by SUBSTRING, never by code.
    | Faulted of description: string

/// One compilation target under test.
type Backend =
    {
        /// The name the manifest keys this backend by (`"clr"`, `"js"`).
        Name: string
        /// Compile + run a conformance program (given its name and source). `None` ⇒ the
        /// RUNTIME is unavailable (no Node on PATH), which SKIPS the row; a run that FAILS
        /// is `Faulted`, and a program that will not COMPILE raises.
        CompileAndRun: string -> string -> RunOutcome option
        /// Compile only (front end); the error-severity diagnostic messages.
        Diagnostics: string -> string list
    }

/// What one backend owes one program.
[<RequireQualifiedAccess>]
type Obligation =
    /// Run it, exit 0, and match this golden: the `.expected` file beside the program,
    /// normalised.
    | Run of golden: string
    /// Run it and FAULT, with a runtime error mentioning this (case-insensitively).
    | Fault of message: string
    /// Reject it at COMPILE time with an error containing this substring.
    | Diagnose of fragment: string
    /// Compile it clean, and assert nothing further. For a subject no stdout can observe:
    /// `when 'a : struct` holding at `int` admits the call and yields no value to print.
    | Accept

/// What one program pins about the (width × operator) → backend arithmetic-support matrix:
/// the manifest's `width` / `operators` keys. A program about no single primitive
/// (`arith-unsigned-div.fs` spans several) carries none and contributes nothing.
type WidthCoverage =
    {
        /// The primitive this program is about (`byte`, `string`).
        Width: string
        /// The arithmetic operators this program's obligations are about: the pairs it
        /// contributes are `Width` × these, and `None` ⇒ all of them. Narrowing it says a
        /// width supports some and not others — `string` has only `+`.
        Operators: Set<string> option
    }

type ConformanceProgram =
    {
        /// Corpus-relative path, as written in the manifest (`ops/arith-byte.fs`).
        Path: string
        /// The file's base name (`arith-byte`) — the test name, and the assembly /
        /// module name a backend derives its output from.
        Name: string
        Source: string
        /// Backend name -> what that backend owes. A backend absent from the map owes
        /// nothing for this program.
        Obligations: Map<string, Obligation>
        /// The width this program contributes to the arithmetic-support matrix, if any.
        Covers: WidthCoverage option
    }

/// `test/Codegen.Conformance/`, resolved from this file's location: the corpus is read as
/// text from the SOURCE tree, never copied to the output directory.
let corpusDir: string =
    Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", "Codegen.Conformance"))

/// Strip CR and trim, so a golden authored with either line ending compares equal to
/// output captured on either platform.
let private normalise (s: string) : string = s.Replace("\r", "").Trim()

let private asString (v: TomlValue) : string option =
    match v with
    | TomlValue.String s -> Some s
    | _ -> None

let private strings (t: TomlTable) (key: string) : string list =
    match Map.tryFind key t with
    | Some(TomlValue.Array xs) -> xs |> List.choose asString
    | _ -> []

/// The `diagnose = { js = "…" }` inline table: backend name -> required message
/// fragment.
let private diagnoseMap (t: TomlTable) : Map<string, string> =
    match Map.tryFind "diagnose" t with
    | Some(TomlValue.InlineTable d)
    | Some(TomlValue.Table d) ->
        d
        |> Map.toSeq
        |> Seq.choose (fun (k, v) -> asString v |> Option.map (fun s -> k, s))
        |> Map.ofSeq
    | _ -> Map.empty

let private programOf (entry: TomlTable) : ConformanceProgram =
    let path =
        match Map.tryFind "path" entry |> Option.bind asString with
        | Some p -> p
        | None -> failwith "conformance manifest: a [[program]] entry has no `path`"

    let sourcePath = Path.Combine(corpusDir, path)
    let goldenPath = Path.ChangeExtension(sourcePath, ".expected")

    if not (File.Exists sourcePath) then
        failwithf "conformance manifest: %s does not exist" sourcePath

    let faults = Map.tryFind "faults" entry |> Option.bind asString
    let runners = strings entry "run"
    let accepters = strings entry "accept"

    // Lazy so the missing-golden failure fires only when some backend is listed in `run`.
    let ran =
        lazy
            (match faults with
             | Some message -> Obligation.Fault message
             | None when File.Exists goldenPath -> Obligation.Run(normalise (File.ReadAllText goldenPath))
             | None -> failwithf "conformance program %s has no `.expected` golden beside it" path)

    let obligations =
        Map.ofList
            [
                for b in runners -> b, ran.Value
                for b in accepters -> b, Obligation.Accept
                for KeyValue(b, fragment) in diagnoseMap entry -> b, Obligation.Diagnose fragment
            ]

    // A golden beside a program no backend runs to completion is stale: nothing compares it.
    if File.Exists goldenPath && (faults.IsSome || runners.IsEmpty) then
        failwithf "conformance program %s carries an `.expected` golden that no backend runs it against" path

    let covers =
        Map.tryFind "width" entry
        |> Option.bind asString
        |> Option.map (fun width ->
            {
                Width = width
                Operators =
                    match Map.tryFind "operators" entry with
                    | Some _ -> Some(Set.ofList (strings entry "operators"))
                    | None -> None
            }
        )

    {
        Path = path
        Name = Path.GetFileNameWithoutExtension path
        Source = File.ReadAllText sourcePath
        Obligations = obligations
        Covers = covers
    }

/// The corpus, in manifest order. Parsed with this repo's own TOML reader.
let programs: ConformanceProgram list =
    let manifestPath = Path.Combine(corpusDir, "manifest.toml")

    let doc =
        match Toml.parse (File.ReadAllText manifestPath) with
        | Error e -> failwithf "conformance manifest does not parse (%s): %s" manifestPath e
        | Ok doc -> doc

    let parsed =
        match Map.tryFind "program" doc with
        | Some(TomlValue.Array entries) ->
            entries
            |> List.map (
                function
                | TomlValue.Table t -> programOf t
                | v -> failwithf "conformance manifest: [[program]] entry is not a table: %A" v
            )
        | _ -> failwithf "conformance manifest %s declares no [[program]] entries" manifestPath

    // `Name` is the base name and paths are nested (`ops/x.fs`, `constraints/x.fs`), so
    // two directories sharing one would put both programs on one golden (`goldens/<Name>.js`)
    // and surface as a content diff rather than a clash.
    match parsed |> List.countBy (fun p -> p.Name) |> List.filter (fun (_, n) -> n > 1) with
    | [] -> parsed
    | dups -> failwithf "conformance manifest: program names must be unique across directories, but %A repeat" dups

/// The corpus programs `backend` COMPILES: everything but a `Diagnose` row, which the front
/// end rejects, so it emits nothing to inspect. A `Fault` program compiles — it throws at
/// RUNTIME — and so does an `Accept` one.
let compiledBy (backend: string) : ConformanceProgram list =
    programs
    |> List.filter (fun p ->
        match Map.tryFind backend p.Obligations with
        | Some(Obligation.Run _)
        | Some(Obligation.Fault _)
        | Some Obligation.Accept -> true
        | Some(Obligation.Diagnose _)
        | None -> false
    )

/// A fault is identified by a message, not a code, and the same message must match on every
/// target: the BCL says "Attempted to divide by zero." and a JS target throws an `Error`
/// carrying the same words, so the comparison is case-insensitive substring.
let private mentions (fragment: string) (description: string) : bool =
    description.IndexOf(fragment, StringComparison.OrdinalIgnoreCase) >= 0

/// A backend's obligations over the whole corpus, as an Expecto test list. Each program
/// contributes at most one test.
let conformanceTests (backend: Backend) : Test =
    testList
        (sprintf "Codegen conformance (%s)" backend.Name)
        [
            for p in programs do
                let run () = backend.CompileAndRun p.Name p.Source

                match Map.tryFind backend.Name p.Obligations with
                | None -> ()

                | Some(Obligation.Run golden) ->
                    test p.Name {
                        match run () with
                        | None -> skiptest (sprintf "%s runtime unavailable" backend.Name)
                        | Some(RunOutcome.Faulted description) ->
                            failtestf
                                "%s: %s must run to completion, but it FAULTED:\n%s"
                                backend.Name
                                p.Path
                                description
                        | Some(RunOutcome.Completed(exitCode, output)) ->
                            let actual = normalise output

                            Expect.equal
                                exitCode
                                0
                                (sprintf "%s exits 0 for %s\n--- stdout ---\n%s" backend.Name p.Path actual)

                            Expect.equal actual golden (sprintf "%s conforms to %s.expected" backend.Name p.Name)
                    }

                | Some Obligation.Accept ->
                    test (sprintf "%s (accepted)" p.Name) {
                        match backend.Diagnostics p.Source with
                        | [] -> ()
                        | messages ->
                            failtestf
                                "%s must ACCEPT %s — it pins no output, only that the front end does not refuse it — but reported %A"
                                backend.Name
                                p.Path
                                messages
                    }

                | Some(Obligation.Fault message) ->
                    test (sprintf "%s (faults)" p.Name) {
                        match run () with
                        | None -> skiptest (sprintf "%s runtime unavailable" backend.Name)
                        | Some(RunOutcome.Completed(exitCode, output)) ->
                            failtestf
                                "%s: %s must FAULT at runtime with %A, but it ran to completion (exit %d):\n%s"
                                backend.Name
                                p.Path
                                message
                                exitCode
                                (normalise output)
                        | Some(RunOutcome.Faulted description) ->
                            Expect.isTrue
                                (mentions message description)
                                (sprintf
                                    "%s: %s must fault with a runtime error mentioning %A, but it reported:\n%s"
                                    backend.Name
                                    p.Path
                                    message
                                    description)
                    }

                | Some(Obligation.Diagnose fragment) ->
                    test (sprintf "%s (rejected)" p.Name) {
                        match backend.Diagnostics p.Source with
                        | [] ->
                            failtestf
                                "%s must REJECT %s with an error containing %A, but it compiled clean"
                                backend.Name
                                p.Path
                                fragment
                        | messages ->
                            Expect.isTrue
                                (messages |> List.exists (fun m -> m.Contains fragment))
                                (sprintf
                                    "%s must reject %s with an error containing %A, but reported %A"
                                    backend.Name
                                    p.Path
                                    fragment
                                    messages)
                    }
        ]
