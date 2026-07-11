module XParsec.FSharp.SemanticAnalysis.Tests.ResolverAllowlistTests

open System.IO
open Expecto

// `PassContext.Resolver` — the narrow spelling→identity face — has an
// enumerated reader set. The other half of the resolve-once boundary is
// compiler-enforced (`ctx.Provider` is the store face and cannot resolve a
// spelling); the resolver face, however, must stay a reachable `ctx` member
// (Translate's by-name hatch needs it deep in Unification with only `ctx` in
// hand), so nothing STRUCTURAL stops a new consumer-pass string lookup. This
// test enforces that half: a new `ctx.Resolver` reader inside the
// SemanticAnalysis passes must add itself here and justify its reach.
[<Tests>]
let tests =
    testList
        "ResolverAllowlist"
        [
            test "ctx.Resolver readers are exactly the sanctioned set" {
                let srcRoot =
                    Path.GetFullPath(
                        Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "XParsec.FSharp.SemanticAnalysis")
                    )

                let sanctioned =
                    Set.ofList
                        [
                            // The member definition (and its doc-comment).
                            "PassContext.fs"
                            // The resolve-once engine + the type-head stamp writers.
                            "Passes/NameResolution/TypeHeadStamp.fs"
                            // Ident/value resolution and the expression-position stamp writers.
                            "Passes/NameResolution/Scope.fs"
                            // Registration-time external-type resolution.
                            "Passes/NameResolution/MemberRegistration.fs"
                            // The sanctioned by-name hatch (the `float<m>` measure carrier —
                            // the one head with no `Type` node to carry a stamp) and the
                            // DEBUG-only stamping-gap witness.
                            "Passes/Unification/Translate.fs"
                        ]

                let readers =
                    Directory.EnumerateFiles(srcRoot, "*.fs", SearchOption.AllDirectories)
                    |> Seq.filter (fun f ->
                        let rel = Path.GetRelativePath(srcRoot, f).Replace('\\', '/')
                        not (rel.StartsWith "obj/" || rel.StartsWith "bin/")
                    )
                    |> Seq.filter (fun f -> (File.ReadAllText f).Contains "ctx.Resolver")
                    |> Seq.map (fun f -> Path.GetRelativePath(srcRoot, f).Replace('\\', '/'))
                    |> Set.ofSeq

                let unsanctioned = Set.difference readers sanctioned
                let stale = Set.difference sanctioned readers

                Expect.isEmpty
                    unsanctioned
                    "new `ctx.Resolver` reader(s) outside the sanctioned set — a consumer pass \
                     must read the store face (`ctx.Provider`); if this reach is genuinely a \
                     spelling with no stampable node, add it here with its justification"

                Expect.isEmpty stale "sanctioned reader(s) no longer read ctx.Resolver — prune the allowlist"
            }
        ]
