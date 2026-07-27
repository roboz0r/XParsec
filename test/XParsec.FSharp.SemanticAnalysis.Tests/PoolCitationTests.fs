module XParsec.FSharp.SemanticAnalysis.Tests.PoolCitationTests

open System.IO
open System.Text.RegularExpressions
open Expecto
open XParsec.FSharp.SemanticAnalysis

// The pool columns are cited BY NAME throughout the SemanticAnalysis and codegen sources —
// the design rationale in `TastPoolTypes.fs`, the ordering coupling in `TastPools.fs`, the
// layer arithmetic in `TastPoolBuilder.fs`. A citation is prose, so removing a column does
// not break it: `TastPoolBuilder`'s read-surface header went on naming `FrozenPools.ExprShapes`
// for the whole life of the branch that deleted that column.
//
// This is the guard for that one bug class, and deliberately only that one: a citation of
// `FrozenPools.X` must name a field of the record or a member of its module. It is cheap
// because the valid set is reflected off the type rather than restated, so a renamed or
// deleted column fails here with no allowlist to update.
[<Tests>]
let tests =
    testList
        "PoolCitations"
        [
            test "every `FrozenPools.X` citation names a member that exists" {
                let repoRoot = Path.GetFullPath(Path.Combine(__SOURCE_DIRECTORY__, "..", ".."))
                let srcRoot = Path.Combine(repoRoot, "src")

                // The record's own columns, plus the `FrozenPools` module's values and
                // functions (`empty`, `typarArity`) — cited in the same `FrozenPools.` form.
                let members =
                    let recordFields =
                        Reflection.FSharpType.GetRecordFields typeof<FrozenPools>
                        |> Array.map (fun p -> p.Name)

                    let moduleMembers =
                        match
                            typeof<FrozenPools>.Assembly.GetType "XParsec.FSharp.SemanticAnalysis.FrozenPoolsModule"
                        with
                        | null -> [||]
                        | t ->
                            Array.append
                                (t.GetMethods() |> Array.map (fun m -> m.Name))
                                (t.GetProperties() |> Array.map (fun p -> p.Name))

                    Set.ofArray (Array.append recordFields moduleMembers)

                // Only BACKTICKED citations: an unquoted `FrozenPools.` is real code, which
                // the compiler already checks. The trailing look-ahead lets prose name a
                // column FAMILY (`FrozenPools.Expr*`) without the wildcard being read as a
                // member name — that names the parallel columns, not one of them.
                let citation = Regex @"`FrozenPools\.([A-Za-z_][A-Za-z0-9_]*)(?![A-Za-z0-9_*])"

                let dangling =
                    Directory.EnumerateFiles(srcRoot, "*.fs", SearchOption.AllDirectories)
                    |> Seq.filter (fun f ->
                        let rel = Path.GetRelativePath(srcRoot, f).Replace('\\', '/')
                        not (rel.Contains "/obj/" || rel.Contains "/bin/")
                    )
                    |> Seq.collect (fun f ->
                        File.ReadAllLines f
                        |> Array.mapi (fun i line -> (i + 1, line))
                        |> Array.collect (fun (lineNo, line) ->
                            citation.Matches line
                            |> Seq.map (fun m -> m.Groups.[1].Value)
                            |> Seq.filter (fun name -> not (Set.contains name members))
                            |> Seq.map (fun name ->
                                sprintf
                                    "%s:%d cites `FrozenPools.%s`"
                                    (Path.GetRelativePath(repoRoot, f))
                                    lineNo
                                    name
                            )
                            |> Seq.toArray
                        )
                    )
                    |> List.ofSeq

                // The offenders are IN the message: a citation is prose, so the only way to
                // act on this failure is to be told which line to read.
                Expect.isEmpty
                    dangling
                    (sprintf
                        "citation(s) of a `FrozenPools` member that does not exist — the column was \
                         renamed or deleted and the prose that explains it was left behind:\n%s"
                        (String.concat "\n" dangling))
            }
        ]
