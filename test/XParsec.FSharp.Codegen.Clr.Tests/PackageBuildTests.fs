module XParsec.FSharp.Codegen.Clr.Tests.PackageBuildTests

open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// The manifest-driven `buildPackage` harness: each anchor compiles a package's `impl`
// `.fs` to a BCL-only DLL and loads it with its expected public types, resolving the
// `depends-on` graph through the harness's own load context.

[<Tests>]
let tests =
    testList
        "PackageBuild"
        [
            // Vesper.Core has no dependencies; its `impl` is the whole compile target.
            test "buildPackage Vesper.Core builds a BCL-only DLL with Fun`2 + Ref`1" {
                let asm, artifact = (buildPackage "Vesper.Core").Value

                expectNoFSharpCore artifact "Vesper.Core.dll is BCL-only"

                Expect.isNotNull (asm.GetType "Vesper.Fun`2") "the DLL contains Vesper.Fun`2"
                Expect.isNotNull (asm.GetType "Vesper.Ref`1") "the DLL contains Vesper.Ref`1"

                // The flat arity-2 function type and its flat <-> curried adapters.
                Expect.isNotNull (asm.GetType "Vesper.Fun`3") "the DLL contains Vesper.Fun`3"
                Expect.isNotNull (asm.GetType "Vesper.Curried`3") "the DLL contains Vesper.Curried`3"
                Expect.isNotNull (asm.GetType "Vesper.Flattened`3") "the DLL contains Vesper.Flattened`3"

                // The attribute classes inherit an external base, `Attribute = (# class
                // "System.Attribute" #)`, so this exercises the `extends`-to-BCL column and
                // the synthesised `.ctor` chain, which faults at construction, not at load.
                let attrTy = asm.GetType "Vesper.StructuralEqualityAttribute"
                Expect.isNotNull attrTy "the DLL contains Vesper.StructuralEqualityAttribute"
                Expect.equal attrTy.BaseType typeof<System.Attribute> "the attribute inherits System.Attribute"

                let instance = System.Activator.CreateInstance attrTy
                Expect.isTrue (instance :? System.Attribute) "an instance is a System.Attribute (base ctor ran)"
            }

            // The `%A` interface resolves like any other nominal, so Core's own records bind
            // their OWN `TypeDef` rather than an `AssemblyRef` back to Core. Both halves are
            // pinned: the record implements the interface, and the PE has no self-reference.
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

                // An `AssemblyRef` to itself would fault at load.
                let refs = peAssemblyRefs (Codegen.toBytes artifact)

                Expect.isFalse
                    (refs |> List.contains "Vesper.Core")
                    (sprintf "Vesper.Core.dll must not reference its own assembly; refs = %A" refs)
            }

            // Vesper.List depends on Vesper.Core: the harness builds and loads Core first,
            // and resolves `Vesper.Fun` against THAT one, not the default context's.
            test "buildPackage Vesper.List builds a BCL-only DLL with List`1 over its Core dep" {
                let asm, artifact = (buildPackage "Vesper.List").Value

                expectNoFSharpCore artifact "Vesper.List.dll is BCL-only"

                Expect.isNotNull (asm.GetType "Vesper.Collections.List`1") "the DLL contains Vesper.Collections.List`1"
            }

            // Vesper.Seq is a real generic-over-`'T` library of struct enumerator / sequence
            // pairs. `buildPackage` fails on ANY error diagnostic, so it surfaces front-end
            // gaps the inline `compileSource` fixtures, which tolerate errors, never hit.
            test "buildPackage Vesper.Seq builds a BCL-only DLL with the generic struct-seq surface" {
                let asm, artifact = (buildPackage "Vesper.Seq").Value

                expectNoFSharpCore artifact "Vesper.Seq.dll is BCL-only"

                Expect.isNotNull
                    (asm.GetType "Vesper.Collections.ArraySeq`1")
                    "the DLL contains Vesper.Collections.ArraySeq`1"

                Expect.isNotNull
                    (asm.GetType "Vesper.Collections.MapSeq`5")
                    "the DLL contains Vesper.Collections.MapSeq`5"
            }
        ]
