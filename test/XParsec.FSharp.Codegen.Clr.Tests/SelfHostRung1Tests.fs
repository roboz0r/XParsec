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

            // G4 item 5 (docs/selfhost-handoff.md): an abstract method may declare
            // its *own* generic parameters. The backend emits them as method-owned
            // `GenericParam` rows and routes the signature's `'B` to a
            // `GenericMethodParameter` (vs the type's `'A` ⇒ `GenericTypeParameter`).
            // The rows must be globally sorted by `CodedIndex.TypeOrMethodDef` — the
            // method's `'B` (a MethodDef owner) sorts *before* the type's `'A` (a
            // later TypeDef owner), so a naive type-then-method emit would produce an
            // unsorted table and SRM would reject it on serialize. A clean reflect
            // here proves the ordering is right.
            test "compiles a generic interface method to a generic MethodDef with interleaved GenericParam rows" {
                let src =
                    "namespace Vesper\n\ntype Mapper<'A> =\n    abstract member Map<'B> : arg: 'A -> 'B"

                let project = ProjectInfo.library "Vesper.Mapper"
                let artifact = compileSourceTo project src

                Expect.isEmpty artifact.FSharpCoreDependencies "a typar-only signature pins no FSharp.Core"

                let asm = Assembly.Load(Codegen.toBytes artifact)

                let mapperTy = asm.GetType("Vesper.Mapper`1")
                Expect.isNotNull mapperTy "the DLL contains Vesper.Mapper`1"
                Expect.isTrue mapperTy.IsInterface "Mapper`1 is an interface"

                let typeArgs = mapperTy.GetGenericArguments()
                Expect.equal typeArgs.Length 1 "Mapper`1 has one type parameter"
                Expect.equal typeArgs.[0].Name "A" "the type parameter is 'A"

                let map = mapperTy.GetMethod("Map")
                Expect.isNotNull map "Mapper`1 has a Map method"
                Expect.isTrue map.IsAbstract "Map is abstract"
                Expect.isTrue map.IsGenericMethodDefinition "Map is a generic method definition"

                let methodArgs = map.GetGenericArguments()
                Expect.equal methodArgs.Length 1 "Map has one method type parameter"
                Expect.equal methodArgs.[0].Name "B" "the method type parameter is 'B"

                // `Map('A) : 'B` — the parameter is the declaring type's 'A, the
                // return the method's own 'B.
                let ps = map.GetParameters()
                Expect.equal ps.Length 1 "Map takes one argument"
                Expect.equal ps.[0].ParameterType typeArgs.[0] "the argument is the type's 'A"
                Expect.equal map.ReturnType methodArgs.[0] "the return is the method's 'B"
            }

            // G7 (docs/selfhost-handoff.md): the backend keys a primitive's
            // emitted IL type off its *representation string* — the value a
            // `type x = (# "..." #)` intrinsic binds (carried on
            // `TastFile.IntrinsicReprTypes`, overlaid on the built-in defaults) —
            // not a hard-coded Vesper name. So a custom `type myint = (# … #)` in
            // the source drives what `myint` emits as, and retargeting it is a
            // one-line `.fs` edit. Compile the same interface twice with different
            // bindings and watch the parameter / return type follow the string —
            // the Part-A registry finally earning its keep.
            test "an interface method's custom intrinsic primitive is emitted from its representation string (G7)" {
                let unwrapOf (asmSuffix: string) (repr: string) : System.Reflection.MethodInfo =
                    let src =
                        sprintf
                            "namespace Vesper\n\ntype myint = (# \"%s\" #)\n\ntype IBox =\n    abstract member Unwrap : x: myint -> myint"
                            repr

                    // Distinct assembly names so two same-shaped PEs don't collide
                    // on identity when both are `Assembly.Load`ed in this process.
                    let project = ProjectInfo.library (sprintf "Vesper.G7Box.%s" asmSuffix)
                    let artifact = compileSourceTo project src

                    Expect.isEmpty
                        artifact.FSharpCoreDependencies
                        "a primitive-only interface pins no FSharp.Core construct"

                    let asm = Assembly.Load(Codegen.toBytes artifact)
                    let boxTy = asm.GetType("Vesper.IBox")
                    Expect.isNotNull boxTy "the DLL contains Vesper.IBox"
                    let unwrap = boxTy.GetMethod("Unwrap")
                    Expect.isNotNull unwrap "IBox has an Unwrap method"
                    unwrap

                // `myint = System.Int32` ⇒ the parameter and return emit as `int32`.
                let asInt = unwrapOf "I32" "System.Int32"
                Expect.equal (asInt.GetParameters().[0].ParameterType) typeof<int> "System.Int32 ⇒ int32 parameter"
                Expect.equal asInt.ReturnType typeof<int> "System.Int32 ⇒ int32 return"

                // Retarget the one `.fs` line to `System.Int64`: the emitted type
                // follows the representation string, with no codegen change.
                let asInt64 = unwrapOf "I64" "System.Int64"

                Expect.equal
                    (asInt64.GetParameters().[0].ParameterType)
                    typeof<int64>
                    "retargeted to System.Int64 ⇒ int64 parameter"

                Expect.equal asInt64.ReturnType typeof<int64> "retargeted to System.Int64 ⇒ int64 return"
            }

            // The same rekey on the executable path (`ClrProvider.encodeType`): a
            // program that declares its own intrinsic and uses it in a function
            // value's type drives the synthesised closure's `Invoke` / local
            // signatures off the representation string. Before G7, `encodeType`
            // had no arm for a custom `TyConst "myint"` and would `failwith`; now
            // the file's binding (overlaid on the defaults) resolves it. The
            // closure is built but never invoked — the program just proves the
            // signature encoded and the assembly runs.
            test "a program's custom intrinsic resolves through encodeType on the executable path (G7)" {
                let src =
                    "type myint = (# \"System.Int32\" #)\nlet boxId : myint -> myint = fun x -> x\nprintfn \"ok\""

                let _, artifact = compileSource "G7ExeOverlay" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "ok"
                    "the custom-intrinsic-typed closure encoded via the repr rekey and the app ran"
            }

            // G5 / P2 (docs/selfhost-handoff.md): the provider's refs are now
            // `lazy` (G6), so `assembleLibrary` constructs a `ClrProvider` and
            // reuses its `encodeType` instead of the old provider-free encoder.
            // An abstract method may now reference a *concrete* type — here a
            // nested function `('A -> 'B)` — that the provider-free path would
            // `failwith` on. The typar leaves stay positional generic parameters
            // (intercepted at every depth, not just the top decurried params); the
            // function wrapper becomes an `FSharpFunc\`2`, which (correctly) pins
            // FSharp.Core — now surfaced on the library path's dependency set,
            // which was hard-coded empty before P2.
            test "an interface method referencing a function type compiles via the provider (G5)" {
                let src =
                    "namespace Vesper\n\ntype Applier<'A, 'B> =\n    abstract member Apply : f: ('A -> 'B) -> x: 'A -> 'B"

                let project = ProjectInfo.library "Vesper.Applier"
                let artifact = compileSourceTo project src

                Expect.contains
                    artifact.FSharpCoreDependencies
                    "Microsoft.FSharp.Core.FSharpFunc`2"
                    "the function-typed parameter pins FSharpFunc`2 (now reported on the library path)"

                let asm = Assembly.Load(Codegen.toBytes artifact)

                // The lazy FSharp.Core ref was forced by the FSharpFunc encoding, so
                // the metadata genuinely references FSharp.Core now (vs the typar-only
                // Fun interface, which references nothing).
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)
                Expect.contains refs "FSharp.Core" "the DLL references FSharp.Core (via FSharpFunc)"

                let applierTy = asm.GetType("Vesper.Applier`2")
                Expect.isNotNull applierTy "the DLL contains Vesper.Applier`2"
                Expect.isTrue applierTy.IsInterface "Applier`2 is an interface"

                let typeArgs = applierTy.GetGenericArguments()
                Expect.equal typeArgs.Length 2 "Applier`2 has two type parameters"

                let apply = applierTy.GetMethod("Apply")
                Expect.isNotNull apply "Applier`2 has an Apply method"

                // `Apply(('A -> 'B), 'A) : 'B` — param0 is `FSharpFunc<'A,'B>`, param1
                // the type's 'A, the return the type's 'B.
                let ps = apply.GetParameters()
                Expect.equal ps.Length 2 "Apply takes two arguments"

                let funcParam = ps.[0].ParameterType
                Expect.isTrue funcParam.IsGenericType "the first parameter is a generic type"

                Expect.equal
                    (funcParam.GetGenericTypeDefinition())
                    typedefof<Microsoft.FSharp.Core.FSharpFunc<_, _>>
                    "the first parameter is FSharpFunc`2"

                let funcArgs = funcParam.GetGenericArguments()
                Expect.equal funcArgs.[0] typeArgs.[0] "FSharpFunc's domain is the type's 'A"
                Expect.equal funcArgs.[1] typeArgs.[1] "FSharpFunc's range is the type's 'B"
                Expect.equal ps.[1].ParameterType typeArgs.[0] "the second parameter is the type's 'A"
                Expect.equal apply.ReturnType typeArgs.[1] "the return is the type's 'B"
            }
        ]
