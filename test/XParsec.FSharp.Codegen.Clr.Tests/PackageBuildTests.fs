module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTests

open Expecto
open XParsec.FSharp.Codegen.Clr
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
            // Vesper.Core has no dependencies; its `impl` is the whole compile target.
            // Built through `buildContract []` — no `MockBuiltins`.
            test "buildPackage Vesper.Core builds a BCL-only DLL with Fun`2 + Ref`1" {
                let asm, artifact = (buildPackage "Vesper.Core").Value

                Expect.isEmpty artifact.FSharpCoreDependencies "Vesper.Core.dll is BCL-only (no FSharp.Core)"

                Expect.isNotNull (asm.GetType "Vesper.Fun`2") "the DLL contains Vesper.Fun`2"
                Expect.isNotNull (asm.GetType "Vesper.Ref`1") "the DLL contains Vesper.Ref`1"

                // Flat arity-2 fn type + its flat<->curried adapters (Fun wall).
                Expect.isNotNull (asm.GetType "Vesper.Fun`3") "the DLL contains Vesper.Fun`3"
                Expect.isNotNull (asm.GetType "Vesper.Curried`3") "the DLL contains Vesper.Curried`3"
                Expect.isNotNull (asm.GetType "Vesper.Flattened`3") "the DLL contains Vesper.Flattened`3"

                // The compiler-recognised attribute classes (compiler-attributes.fs)
                // inherit the heritable external base `Attribute = (# class
                // "System.Attribute" #)`. This exercises the `extends`-to-BCL column +
                // the synthesised primary `.ctor` chaining to `System.Attribute::.ctor()`:
                // a malformed base-ctor call faults at construction, not load.
                let attrTy = asm.GetType "Vesper.StructuralEqualityAttribute"
                Expect.isNotNull attrTy "the DLL contains Vesper.StructuralEqualityAttribute"
                Expect.equal attrTy.BaseType typeof<System.Attribute> "the attribute inherits System.Attribute"

                let instance = System.Activator.CreateInstance attrTy
                Expect.isTrue (instance :? System.Attribute) "an instance is a System.Attribute (base ctor ran)"
            }

            // The `%A` interfaces are resolved like any other nominal
            // (`ClrEnv.coreInterfaceEntity`): Core's own records bind their OWN `TypeDef`
            // rather than an `AssemblyRef` back to Core. That self-reference — which
            // `refRequired` rejects — was the ONLY reason the backend ever asked "am I
            // Core?", so nothing is special-cased now and Core's records carry `%A` like
            // everyone else's. Both halves are pinned here: the record really implements the
            // interface, AND the PE really has no self-reference (the failure the old gate
            // was avoiding). Neither was covered before: no test asserted Core's records
            // either way, so the whole suite was green under both policies.
            test "Vesper.Core's own records implement its OWN IStructuralFormattable (no self-AssemblyRef)" {
                let asm, artifact = (buildPackage "Vesper.Core").Value

                let refTy = asm.GetType "Vesper.Ref`1"
                let formattable = asm.GetType "Vesper.IStructuralFormattable"

                Expect.isNotNull formattable "the DLL contains Vesper.IStructuralFormattable"

                // Reference equality against the type loaded from THIS assembly: the
                // interface `Ref`1` implements is Core's own, not a same-named import.
                Expect.isTrue
                    (refTy.GetInterfaces() |> Array.exists (fun i -> i = formattable))
                    "Ref`1 implements the IStructuralFormattable declared in this same assembly"

                // Core is `structural-format`'s home, so the interface must resolve to a
                // local TypeDef — an `AssemblyRef` to itself would fault at load.
                let refs = peAssemblyRefs (Codegen.toBytes artifact)

                Expect.isFalse
                    (refs |> List.contains "Vesper.Core")
                    (sprintf "Vesper.Core.dll must not reference its own assembly; refs = %A" refs)
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
