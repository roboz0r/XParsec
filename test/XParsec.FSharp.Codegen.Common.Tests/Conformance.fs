/// The backend conformance corpus, and the assertions every backend owes it.
///
/// The corpus (`test/Codegen.Conformance/`) is data on disk: programs in our own F#
/// dialect that no fsproj compiles, each with a golden stdout. This module loads it
/// and turns it into Expecto tests, parameterised over the ONE thing that differs
/// between targets — a `Backend`. A backend that does not exist yet joins by writing
/// a three-field record; it inherits the whole corpus for free.
///
/// Deliberately NOT a differential test (compile with both, diff the two outputs):
/// comparing A to B can only say they disagree, never which is wrong, and it goes
/// green the moment both are wrong the same way. Each program is judged against a
/// golden no backend can influence.
module XParsec.FSharp.Codegen.Common.Tests.Conformance

open System
open System.IO
open Expecto
open XParsec.Toml

/// What became of a conformance program that was compiled and run.
///
/// This is NOT `int * string`. A faulting program (integer division by zero) has no
/// portable exit code: the CLR harness invokes the entry point IN-PROCESS by
/// reflection, so an uncaught exception never reaches a process exit code — it comes
/// back as a thrown description — while Node runs out of process and yields a real
/// non-zero exit. Each backend maps its own surface into these two cases, and the
/// corpus never has to know which one it is talking to.
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
        /// Compile + run a conformance program (given its name and source). `None` ⇒
        /// the RUNTIME is unavailable (no Node on PATH), which SKIPS the row; it never
        /// means "the program failed" — a run that fails is `Faulted`, and a program
        /// that will not COMPILE raises (a compile failure is not a conformance state,
        /// it is a broken backend).
        CompileAndRun: string -> string -> RunOutcome option
        /// Compile only (front end); the error-severity diagnostic messages.
        Diagnostics: string -> string list
    }

/// What one backend owes one program. These are the only legal states — there is no
/// state in which a clause exists but quietly computes the wrong answer.
[<RequireQualifiedAccess>]
type Obligation =
    /// Run it, exit 0, and match the golden.
    | Run
    /// Run it and FAULT, with a runtime error mentioning this (case-insensitively).
    | Fault of message: string
    /// Reject it at COMPILE time with an error containing this substring.
    | Diagnose of fragment: string

/// What one program pins about the width→backend arithmetic-support matrix — the
/// manifest's `width` / `operators` keys. A program that is about no single primitive
/// (`arith-unsigned-div.fs` spans several; `arith-div-by-zero.fs` is about faulting)
/// carries NO coverage and contributes nothing to the matrix; the option is what makes
/// that honestly sayable rather than forcing a width on it.
type WidthCoverage =
    {
        /// The primitive this program is about (`byte`, `string`).
        Width: string
        /// The arithmetic operators this WIDTH supports. `None` ⇒ unspecified, which
        /// the parity guard reads as all of them; only `string` narrows it.
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
        /// The expected stdout, normalised (CRLF stripped, trimmed). `None` for a
        /// program that is expected to fault: there is no golden to hold it to.
        Golden: string option
        /// Backend name -> what that backend owes. A backend absent from the map owes
        /// nothing for this program.
        Obligations: Map<string, Obligation>
        /// The width this program contributes to the arithmetic-support matrix, if any.
        Covers: WidthCoverage option
    }

/// `test/Codegen.Conformance/`, resolved from this file's location — the corpus is
/// read as text from the SOURCE tree (it is never copied to the output directory,
/// because it is never an msbuild item that anything compiles).
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

    let ran =
        match faults with
        | Some message -> Obligation.Fault message
        | None -> Obligation.Run

    let obligations =
        Map.ofList
            [
                for b in runners -> b, ran
                for KeyValue(b, fragment) in diagnoseMap entry -> b, Obligation.Diagnose fragment
            ]

    // A golden is owed by exactly those programs some backend RUNS TO COMPLETION. A
    // program every backend REJECTS (`decimal`) never produces stdout at all, and one
    // that must FAULT produces none worth trusting — demanding a golden of either would
    // be asserting on a value that is not there. Requiring it in the other direction too
    // keeps a golden from lingering beside a program nothing judges it against.
    let golden =
        let owed = faults.IsNone && not runners.IsEmpty

        match owed, File.Exists goldenPath with
        | false, true ->
            failwithf "conformance program %s carries an `.expected` golden that no backend runs it against" path
        | false, false -> None
        | true, false -> failwithf "conformance program %s has no `.expected` golden beside it" path
        | true, true -> Some(normalise (File.ReadAllText goldenPath))

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
        Golden = golden
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

    match Map.tryFind "program" doc with
    | Some(TomlValue.Array entries) ->
        entries
        |> List.map (
            function
            | TomlValue.Table t -> programOf t
            | v -> failwithf "conformance manifest: [[program]] entry is not a table: %A" v
        )
    | _ -> failwithf "conformance manifest %s declares no [[program]] entries" manifestPath

/// A fault is identified by a message, not a code — and the same message must match
/// on every target (the BCL says "Attempted to divide by zero."; a JS target throws an
/// `Error` carrying the same words), so the comparison is case-insensitive substring.
let private mentions (fragment: string) (description: string) : bool =
    description.IndexOf(fragment, StringComparison.OrdinalIgnoreCase) >= 0

/// A backend's obligations over the whole corpus, as an Expecto test list. Each
/// program contributes at most one test: the golden match, the fault, or the
/// rejection.
let conformanceTests (backend: Backend) : Test =
    testList
        (sprintf "Codegen conformance (%s)" backend.Name)
        [
            for p in programs do
                /// The one place a run is dispatched; `None` skips the row.
                let run () = backend.CompileAndRun p.Name p.Source

                match Map.tryFind backend.Name p.Obligations with
                | None -> ()

                | Some Obligation.Run ->
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

                            Expect.equal
                                actual
                                (Option.defaultValue "" p.Golden)
                                (sprintf "%s conforms to %s.expected" backend.Name p.Name)
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
                                "%s must REJECT %s (no representation for this width) with an error containing %A, but it compiled clean"
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
