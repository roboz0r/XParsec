module XParsec.FSharp.Codegen.Clr.Tests.SelfHostRung1Tests

open System.IO
open System.Reflection
open System.Runtime.Loader
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Compile the real
// `src/Vesper.Core/prim-types-min.fs` into a `Vesper.Core.dll` library that
// emits the `Fun<'A,'B>` interface and references no `FSharp.Core`. The
// intrinsic abbrevs (`type int = (# "System.Int32" #)` …) surface nothing —
// their effect is the Part-A registry — so the only emitted type is the interface.

/// `src/Vesper.Core/<fileName>`, relative to this test file.
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

                Expect.isEmpty artifact.FSharpCoreDependencies "the Fun interface references no FSharp.Core construct"

                let asm = loadAssembly (Codegen.toBytes artifact)

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

                let ps = invoke.GetParameters()
                Expect.equal ps.Length 1 "Invoke takes one argument"
                Expect.equal ps.[0].ParameterType typars.[0] "the argument is 'A"
                Expect.equal invoke.ReturnType typars.[1] "the return is 'B"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "Vesper.Core.dll must not reference FSharp.Core (refs: %A)" refs)
            }

            // `Vesper.Ref<'T>` ships in `Vesper.Core.dll` alongside `Fun\`2`
            // (records-plan §B7), so the captured-mutable promotion can resolve
            // the cell type through the normal external-reference path rather
            // than synthesising a local copy. The bytes are produced by
            // `vesperCoreDll`'s shared compile (prim-types-min.fs +
            // core-types.fs), so this reflection check round-trips that exact
            // artifact.
            test "Vesper.Core.dll contains Vesper.Ref`1" {
                let asm = AssemblyLoadContext.Default.LoadFromAssemblyPath vesperCoreDll.Value

                let refTy = asm.GetType("Vesper.Ref`1")
                Expect.isNotNull refTy "the DLL contains Vesper.Ref`1"
                Expect.isTrue refTy.IsClass "Ref`1 is a (reference) class"
                Expect.isTrue refTy.IsGenericTypeDefinition "Ref`1 is a generic type definition"

                let typars = refTy.GetGenericArguments()
                Expect.equal typars.Length 1 "Ref`1 has one type parameter"
                Expect.equal typars.[0].Name "T" "the type parameter is 'T"

                let contents = refTy.GetField "contents"
                Expect.isNotNull contents "Ref`1 has a `contents` field"
                Expect.equal contents.FieldType typars.[0] "the field's declared type is 'T"
                Expect.isFalse contents.IsInitOnly "the field is mutable"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "Vesper.Core.dll must not reference FSharp.Core (refs: %A)" refs)
            }

            // An abstract method declaring its
            // *own* generic parameters. `GenericParam` rows must be globally sorted
            // by `CodedIndex.TypeOrMethodDef` — the method's `'B` (a MethodDef owner)
            // sorts *before* the type's `'A` (a later TypeDef owner), so a naive
            // type-then-method emit produces an unsorted table SRM rejects on serialize.
            test "compiles a generic interface method to a generic MethodDef with interleaved GenericParam rows" {
                let src =
                    "namespace Vesper\n\ntype Mapper<'A> =\n    abstract member Map<'B> : arg: 'A -> 'B"

                let project = ProjectInfo.library "Vesper.Mapper"
                let artifact = compileSourceTo project src

                Expect.isEmpty artifact.FSharpCoreDependencies "a typar-only signature pins no FSharp.Core"

                let asm = loadAssembly (Codegen.toBytes artifact)

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

                let ps = map.GetParameters()
                Expect.equal ps.Length 1 "Map takes one argument"
                Expect.equal ps.[0].ParameterType typeArgs.[0] "the argument is the type's 'A"
                Expect.equal map.ReturnType methodArgs.[0] "the return is the method's 'B"
            }

            // The backend keys a primitive's emitted IL type off its
            // *representation string* — the value a `type x = (# "..." #)`
            // intrinsic binds (on `TastFile.IntrinsicReprTypes`) — not a hard-coded
            // Vesper name, so retargeting is a one-line `.fs` edit. Compiling the same
            // interface twice with different bindings, the param/return type follows it.
            test "an interface method's custom intrinsic primitive is emitted from its representation string" {
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

                    let asm = loadAssembly (Codegen.toBytes artifact)
                    let boxTy = asm.GetType("Vesper.IBox")
                    Expect.isNotNull boxTy "the DLL contains Vesper.IBox"
                    let unwrap = boxTy.GetMethod("Unwrap")
                    Expect.isNotNull unwrap "IBox has an Unwrap method"
                    unwrap

                let asInt = unwrapOf "I32" "System.Int32"
                Expect.equal (asInt.GetParameters().[0].ParameterType) typeof<int> "System.Int32 ⇒ int32 parameter"
                Expect.equal asInt.ReturnType typeof<int> "System.Int32 ⇒ int32 return"

                let asInt64 = unwrapOf "I64" "System.Int64"

                Expect.equal
                    (asInt64.GetParameters().[0].ParameterType)
                    typeof<int64>
                    "retargeted to System.Int64 ⇒ int64 parameter"

                Expect.equal asInt64.ReturnType typeof<int64> "retargeted to System.Int64 ⇒ int64 return"
            }

            // The same rekey on the executable path (`ClrProvider.encodeType`).
            // Before G7, `encodeType` had no arm for a custom `TyConst "myint"` and
            // would `failwith`. The closure is built but never invoked — the program
            // only proves the signature encoded and the assembly runs.
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

            // `assembleLibrary` reuses a `ClrProvider`'s `encodeType`, so an
            // abstract method may reference a *concrete* type — here a nested
            // function `('A -> 'B)` — that the old provider-free path would
            // `failwith` on. That function type encodes to `Vesper.Fun\`2`
            // (read from Vesper.Core), so the DLL references Vesper.Core and
            // pins **no** FSharp.Core construct.
            test "an interface method referencing a function type encodes to Vesper.Fun via the provider" {
                let src =
                    "namespace Vesper\n\ntype Applier<'A, 'B> =\n    abstract member Apply : f: ('A -> 'B) -> x: 'A -> 'B"

                let project = ProjectInfo.library "Vesper.Applier"
                let artifact = compileSourceTo project src

                Expect.isEmpty
                    artifact.FSharpCoreDependencies
                    "the function-typed parameter is Vesper.Fun now — no FSharp.Core construct"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)
                Expect.isFalse (refs |> Array.contains "FSharp.Core") "the DLL does not reference FSharp.Core (R1)"
                Expect.contains refs "Vesper.Core" "the DLL references Vesper.Core (via Vesper.Fun)"

                let applierTy = asm.GetType("Vesper.Applier`2")
                Expect.isNotNull applierTy "the DLL contains Vesper.Applier`2"
                Expect.isTrue applierTy.IsInterface "Applier`2 is an interface"

                let typeArgs = applierTy.GetGenericArguments()
                Expect.equal typeArgs.Length 2 "Applier`2 has two type parameters"

                let apply = applierTy.GetMethod("Apply")
                Expect.isNotNull apply "Applier`2 has an Apply method"

                let ps = apply.GetParameters()
                Expect.equal ps.Length 2 "Apply takes two arguments"

                let funcParam = ps.[0].ParameterType
                Expect.isTrue funcParam.IsGenericType "the first parameter is a generic type"

                Expect.equal
                    (funcParam.GetGenericTypeDefinition().FullName)
                    "Vesper.Fun`2"
                    "the first parameter is Vesper.Fun`2"

                let funcArgs = funcParam.GetGenericArguments()
                Expect.equal funcArgs.[0] typeArgs.[0] "Fun's domain is the type's 'A"
                Expect.equal funcArgs.[1] typeArgs.[1] "Fun's range is the type's 'B"
                Expect.equal ps.[1].ParameterType typeArgs.[0] "the second parameter is the type's 'A"
                Expect.equal apply.ReturnType typeArgs.[1] "the return is the type's 'B"
            }
        ]
