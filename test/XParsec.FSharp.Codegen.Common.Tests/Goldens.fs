/// Byte-identity golden read/write, shared by the JS and CLR conformance gates so the
/// `UPDATE_SNAPSHOTS` / `CI` snapshot convention (the one every other snapshot suite
/// uses) is single-sourced rather than copy-pasted into each backend test project. The
/// per-backend gates supply only the golden path and the artifact string.
module XParsec.FSharp.Codegen.Common.Tests.Goldens

open System
open System.IO
open Expecto

/// `UPDATE_SNAPSHOTS` set (the `-UpdateSnapshots` wrapper flag) ⇒ overwrite goldens
/// rather than compare — how `./claude_tools.cmd … -UpdateSnapshots` regenerates them.
let private updateSnapshots =
    Environment.GetEnvironmentVariable "UPDATE_SNAPSHOTS" |> isNull |> not

/// `CI` set ⇒ a missing golden skips rather than fails, so a newly added gate is green
/// on its first push until a developer captures + commits the golden locally.
let private isCi = Environment.GetEnvironmentVariable "CI" |> isNull |> not

/// CR is stripped both sides so a golden committed under `core.autocrlf` (CRLF in the
/// working tree) still compares equal to the emitters' native `\n` output — neither the
/// emitted JS nor a hex digest ever carries a meaningful bare CR of its own.
let private normalise (s: string) = s.Replace("\r", "")

/// Compare `actual` against the golden at `path`; under `UPDATE_SNAPSHOTS` (or when the
/// golden is absent) write it instead. `label` names the artifact in the failure.
let check (path: string) (label: string) (actual: string) : unit =
    if updateSnapshots || not (File.Exists path) then
        Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
        File.WriteAllText(path, actual)

        if not updateSnapshots then
            if isCi then
                skiptest $"Golden created at {Path.GetFileName path}; commit it to enable this gate in CI"
            else
                failtestf "Created golden at %s, verify it is correct" path
    else
        Expect.equal
            (normalise actual)
            (normalise (File.ReadAllText path))
            (sprintf "byte-identity golden holds: %s" label)

/// Fail on a committed golden matching `pattern` that `pinned` no longer names: a program the
/// manifest stops compiling leaves its golden behind, and the per-program gate only ever reads
/// the ones it still covers.
let checkNoOrphans (dir: string) (pattern: string) (pinned: string seq) : unit =
    let pinned = Set.ofSeq pinned

    let committed =
        set [ for f in Directory.GetFiles(dir, pattern) -> Path.GetFileName f ]

    Expect.isEmpty (Set.difference committed pinned) "goldens with no program left to pin"
