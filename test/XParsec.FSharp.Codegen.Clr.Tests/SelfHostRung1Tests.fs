module XParsec.FSharp.Codegen.Clr.Tests.SelfHostRung1Tests

open System.IO
open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Self-hosting rung 1 (docs/self-host-rung1-plan.md): compile the real
// `src/Vesper.Core/prim-types-min.fs` with our own backend into a *library* —
// `Vesper.Core.dll` — that emits the `Fun<'A,'B>` interface and references no
// `FSharp.Core`. The intrinsic abbrevs (`type int = (# "System.Int32" #)` …)
// surface nothing (their effect is the Part-A registry); the only emitted type
// is the interface. This is the first time the backend emits a declared nominal
// type and produces an entry-point-free assembly.

/// `src/Vesper.Core/<fileName>`, relative to this test file (mirrors the
/// VesperCoreContractTests / parser-golden resolution).
let private vesperCorePath (fileName: string) =
    Path.Combine(__SOURCE_DIRECTORY__, "..", "..", "src", "Vesper.Core", fileName)

[<Tests>]
let tests =
    testList
        "SelfHostRung1"
        [
            test "compiles prim-types-min.fs to a Vesper.Core.dll with the Fun`2 interface and no FSharp.Core" {
                let src = File.ReadAllText(vesperCorePath "prim-types-min.fs")
                let project = ProjectInfo.library "Vesper.Core"
                let artifact = compileSourceTo project src

                // The library path is provider-free; a typar-only interface pins
                // no FSharp.Core construct.
                Expect.isEmpty artifact.FSharpCoreDependencies "the Fun interface references no FSharp.Core construct"

                // Load the emitted PE and reflect over it (the conformance smoke
                // test — the metadata round-trips to a real, loadable interface).
                let asm = Assembly.Load(Codegen.toBytes artifact)

                let funTy = asm.GetType("Vesper.Fun`2")
                Expect.isNotNull funTy "the DLL contains Vesper.Fun`2"
                Expect.isTrue funTy.IsInterface "Fun`2 is an interface"
                Expect.isTrue funTy.IsGenericTypeDefinition "Fun`2 is a generic type definition"

                let typars = funTy.GetGenericArguments()
                Expect.equal typars.Length 2 "Fun`2 has two type parameters"
                Expect.equal typars.[0].Name "A" "first type parameter is 'A"
                Expect.equal typars.[1].Name "B" "second type parameter is 'B"

                let invoke = funTy.GetMethod("Invoke")
                Expect.isNotNull invoke "Fun`2 has an Invoke method"
                Expect.isTrue invoke.IsAbstract "Invoke is abstract"
                Expect.isTrue invoke.IsVirtual "Invoke is virtual"

                // `Invoke('A) : 'B` — the parameter is the type's first generic
                // parameter, the return its second.
                let ps = invoke.GetParameters()
                Expect.equal ps.Length 1 "Invoke takes one argument"
                Expect.equal ps.[0].ParameterType typars.[0] "the argument is 'A"
                Expect.equal invoke.ReturnType typars.[1] "the return is 'B"

                // No FSharp.Core (nor any other) dependency in the metadata.
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "Vesper.Core.dll must not reference FSharp.Core (refs: %A)" refs)
            }
        ]
