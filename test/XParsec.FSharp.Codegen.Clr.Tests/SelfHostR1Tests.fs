module XParsec.FSharp.Codegen.Clr.Tests.SelfHostR1Tests

open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// R1 (docs/selfhost-handoff.md): a function value is a `Vesper.Fun`, not an
// `FSharpFunc`. A program's synthesised closures derive from `System.Object` and
// *implement* the `Vesper.Fun\`2` interface read from a compiled `Vesper.Core.dll`;
// application is `callvirt Vesper.Fun::Invoke`. The emitted PE carries a
// `Vesper.Core` `AssemblyRef` and no `Microsoft.FSharp.*` reference.
//
// The two-stage flow the cutover rests on:
//   1. compile `src/Vesper.Core/prim-types-min.fs` → `Vesper.Core.dll` (its only
//      emitted type is the `Vesper.Fun\`2` interface — self-host rung 1);
//   2. compile a function-value-bearing user program *referencing* that DLL.
// `TestHelpers.vesperCoreDll` runs (1) once and loads the result into the Default
// `AssemblyLoadContext` so an in-process run resolves `Fun`; `compileSource`
// runs (2) by injecting the core path (`withCore`).

[<Tests>]
let tests =
    testList
        "SelfHostR1"
        [
            test "stage 1: the Vesper.Core.dll the cutover references is on disk and named Vesper.Core" {
                // Forcing the lazy *is* stage 1 — it compiled prim-types-min.fs to
                // the on-disk DLL and loaded it for in-process resolution.
                let corePath = vesperCoreDll.Value
                Expect.isTrue (System.IO.File.Exists corePath) "Vesper.Core.dll written to disk"

                let coreAsm = AssemblyName.GetAssemblyName corePath
                Expect.equal coreAsm.Name "Vesper.Core" "the library is named Vesper.Core"
            }

            test "stage 2: a closure program references Vesper.Core (for Fun), not FSharp.Core, and runs" {
                // A *capturing* lambda is synthesised as a closure (a non-capturing
                // `let f = fun x -> …` lowers to a static method — no function value).
                let _, artifact =
                    compileSource "R1Closure" "let n = 1\nlet f = fun x -> x + n\nprintfn \"%d\" (f 41)"

                // The function value is `Vesper.Fun` now — no FSharp.Core construct.
                Expect.isEmpty artifact.FSharpCoreDependencies "a plain closure pins no FSharp.Core construct"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "no FSharp.Core AssemblyRef (refs: %A)" refs)

                Expect.contains refs "Vesper.Core" "the PE references Vesper.Core (where Fun lives)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "42" "the closure ran via callvirt Vesper.Fun::Invoke"
            }

            test "a capturing closure (object base + Fun interface) runs and prints 51" {
                let _, artifact =
                    compileSource "R1Capturing" "let n = 10\nlet g = fun x -> x + n\nprintfn \"%d\" (g 41)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "51" "the captured n is read back via ldfld in Invoke"
            }

            test "the synthesised closure derives from System.Object and implements Vesper.Fun`2" {
                let _, artifact =
                    compileSource "R1ClosureShape" "let n = 1\nlet f = fun x -> x + n\nprintfn \"%d\" (f 41)"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let implementsFun (t: System.Type) =
                    t.GetInterfaces()
                    |> Array.exists (fun i -> i.IsGenericType && i.GetGenericTypeDefinition().FullName = "Vesper.Fun`2")

                match asm.GetTypes() |> Array.tryFind implementsFun with
                | None -> failtest "no emitted type implements Vesper.Fun`2"
                | Some t ->
                    Expect.equal
                        t.BaseType
                        typeof<System.Object>
                        "the closure derives from System.Object (no FSharpFunc base)"

                    Expect.isNotNull
                        (t.GetMethod("Invoke"))
                        "the closure has an Invoke method implementing the Fun slot"
            }

            test "an eta-reified curried operator value (`let add = (+)`) runs through nested Fun closures" {
                let _, artifact = compileSource "R1Eta" "let add = (+)\nprintfn \"%d\" (add 40 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "42"
                    "outer Fun closure newobjs the inner; Invoke().Invoke() runs op_Addition"
            }
        ]
