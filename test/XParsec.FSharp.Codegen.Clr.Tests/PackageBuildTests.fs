module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTests

open Expecto
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Pre-1 (vesper-lib-test-plan.md): the manifest-driven `buildPackage` harness.
// These anchors exercise it on the two *proven* packages only (Core + List) —
// they assert the harness compiles each package's `impl` `.fs` to a BCL-only DLL
// (empty `FSharpCoreDependencies`) and loads it with its expected public types,
// resolving the `depends-on` graph through the harness's own load context. The
// Candidate/At-risk triage table (Option/Result/Choice/Comparison/…) is Pre-2.

[<Tests>]
let tests =
    testList
        "PackageBuild"
        [
            // Vesper.Core has no dependencies; its `impl` is the prim-types/`Ref`
            // compile target (the operator inline bodies live in `inline-bodies`,
            // not the DLL). Built through `buildContract []` — no `MockBuiltins`.
            test "buildPackage Vesper.Core builds a BCL-only DLL with Fun`2 + Ref`1" {
                let asm, artifact = (buildPackage "Vesper.Core").Value

                Expect.isEmpty artifact.FSharpCoreDependencies "Vesper.Core.dll is BCL-only (no FSharp.Core)"

                Expect.isNotNull (asm.GetType "Vesper.Fun`2") "the DLL contains Vesper.Fun`2"
                Expect.isNotNull (asm.GetType "Vesper.Ref`1") "the DLL contains Vesper.Ref`1"

                // Flat arity-2 fn type + its flat<->curried adapters (Fun2 wall).
                Expect.isNotNull (asm.GetType "Vesper.Fun2`3") "the DLL contains Vesper.Fun2`3"
                Expect.isNotNull (asm.GetType "Vesper.Curried`3") "the DLL contains Vesper.Curried`3"
                Expect.isNotNull (asm.GetType "Vesper.Flattened`3") "the DLL contains Vesper.Flattened`3"
            }

            // Vesper.List depends on Vesper.Core: the harness builds + loads Core
            // first, references its DLL, and resolves `Vesper.Fun` against the
            // harness's own Core (not the Default-context one) at load time.
            test "buildPackage Vesper.List builds a BCL-only DLL with List`1 over its Core dep" {
                let asm, artifact = (buildPackage "Vesper.List").Value

                Expect.isEmpty artifact.FSharpCoreDependencies "Vesper.List.dll is BCL-only (no FSharp.Core)"

                Expect.isNotNull (asm.GetType "Vesper.Collections.List`1") "the DLL contains Vesper.Collections.List`1"
            }

            // Vesper.Seq's `struct-seq.fs` is the graduation of the rung-3 inline
            // struct-`Seq` slice (brainstorm-seq-module.md) into a real,
            // generic-over-`'T` library: the `IStructEnumerator`/`IStructSeq` marker
            // interfaces, the `ArrayEnumerator`/`ArraySeq` + `MapEnumerator`/`MapSeq`
            // struct pairs, and the `ofArray`/`map`/`fold` module. Built via the
            // STRICTER package path (`buildPackage` fails on any error diagnostic),
            // so this surfaces front-end gaps the inline `compileSource` fixtures
            // (which tolerate errors) never hit.
            test "buildPackage Vesper.Seq builds a BCL-only DLL with the generic struct-seq surface" {
                let asm, artifact = (buildPackage "Vesper.Seq").Value

                Expect.isEmpty artifact.FSharpCoreDependencies "Vesper.Seq.dll is BCL-only (no FSharp.Core)"

                Expect.isNotNull
                    (asm.GetType "Vesper.Collections.ArraySeq`1")
                    "the DLL contains Vesper.Collections.ArraySeq`1"

                Expect.isNotNull
                    (asm.GetType "Vesper.Collections.MapSeq`5")
                    "the DLL contains Vesper.Collections.MapSeq`5"
            }
        ]
