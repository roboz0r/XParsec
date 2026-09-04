module XParsec.FSharp.Codegen.Clr.Tests.SelfHostTests

open System
open System.IO
open System.Reflection
open System.Runtime.Loader
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// The self-hosting bootstrap is FSharp.Core-free. These anchors pin the emission surface:
// `Vesper.Core.dll`'s `Fun`2` / `Ref`1`, provider-encoded interface signatures, function
// values as `Vesper.Fun` closures, `Vesper.List.dll`, and the canonical sample's bundle.

let private fullSample =
    "let inline sum xs = List.fold (+) 0 xs\nlet nums = [1; 2; 3; 4; 5]\nprintfn \"%d\" (sum nums)"

[<Tests>]
let tests =
    testList
        "SelfHost"
        [
            // ---- Vesper.Core: the Fun`2 interface + Ref`1 cell --------
            test "compiles prim-types-min.clr.fs to a Vesper.Core.dll with the Fun`2 interface and no FSharp.Core" {
                let src = File.ReadAllText(vesperCoreSource "prim-types-min.clr.fs")
                let project = ProjectInfo.library "Vesper.Core"
                let artifact = compileSourceTo project src

                expectNoFSharpCore artifact "the Fun interface"

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

            // `Vesper.Ref<'T>` ships in `Vesper.Core.dll` alongside `Fun`2`, so captured-mutable
            // promotion resolves the cell type through the ordinary external-reference path.
            test "Vesper.Core.dll contains Vesper.Ref`1" {
                let asm = AssemblyLoadContext.Default.LoadFromAssemblyPath vesperCoreDll.Value

                let refTy = asm.GetType("Vesper.Ref`1")
                Expect.isNotNull refTy "the DLL contains Vesper.Ref`1"
                Expect.isTrue refTy.IsClass "Ref`1 is a (reference) class"
                Expect.isTrue refTy.IsGenericTypeDefinition "Ref`1 is a generic type definition"

                let typars = refTy.GetGenericArguments()
                Expect.equal typars.Length 1 "Ref`1 has one type parameter"
                Expect.equal typars.[0].Name "T" "the type parameter is 'T"

                let contents = refTy.GetProperty "contents"
                Expect.isNotNull contents "Ref`1 has a `contents` property"
                Expect.equal contents.PropertyType typars.[0] "the property's declared type is 'T"
                Expect.isTrue contents.CanWrite "a `mutable` record field carries its setter"

                let storage =
                    refTy.GetField("contents@", BindingFlags.NonPublic ||| BindingFlags.Instance)

                Expect.isNotNull storage "the property is backed by private storage"
                Expect.isFalse storage.IsInitOnly "the storage is written outside the ctor"

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "Vesper.Core.dll must not reference FSharp.Core (refs: %A)" refs)
            }

            // `GenericParam` rows must be globally sorted by `CodedIndex.TypeOrMethodDef`: the
            // method's `'B` (a MethodDef owner) sorts BEFORE the type's `'A` (a later TypeDef
            // owner), so a naive type-then-method emit order is a table SRM rejects.
            test "compiles a generic interface method to a generic MethodDef with interleaved GenericParam rows" {
                let src =
                    "namespace Vesper\n\ntype Mapper<'A> =\n    abstract member Map<'B> : arg: 'A -> 'B"

                let project = ProjectInfo.library "Vesper.Mapper"
                let artifact = compileSourceTo project src

                expectNoFSharpCore artifact "a typar-only signature"

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

            // The backend keys a primitive's emitted IL type off its representation string
            // (what `type x = (# "…" #)` binds), not a hard-coded Vesper name.
            test "an interface method's custom intrinsic primitive is emitted from its representation string" {
                let unwrapOf (asmSuffix: string) (repr: string) : MethodInfo =
                    let src =
                        sprintf
                            "namespace Vesper\n\ntype myint = (# \"%s\" #)\n\ntype IBox =\n    abstract member Unwrap : x: myint -> myint"
                            repr

                    // Distinct assembly names so two same-shaped PEs don't collide
                    // on identity when both are `Assembly.Load`ed in this process.
                    let project = ProjectInfo.library (sprintf "Vesper.G7Box.%s" asmSuffix)
                    let artifact = compileSourceTo project src

                    expectNoFSharpCore artifact "a primitive-only interface"

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

            // The same rekey on the executable path. The closure is built but never invoked,
            // so the program only proves the signature encoded and the assembly runs.
            test "a program's custom intrinsic resolves through encodeType on the executable path" {
                let src =
                    "type myint = (# \"System.Int32\" #)\nlet boxId : myint -> myint = fun x -> x\nprintfn \"ok\""

                let artifact = compileSource "G7ExeOverlay" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "ok"
                    "the custom-intrinsic-typed closure encoded via the repr rekey and the app ran"
            }

            // A concrete function type `('A -> 'B)` in an abstract method encodes to
            // `Vesper.Fun`2` read from Vesper.Core, so the DLL references Vesper.Core.
            test "an interface method referencing a function type encodes to Vesper.Fun via the provider" {
                let src =
                    "namespace Vesper\n\ntype Applier<'A, 'B> =\n    abstract member Apply : f: ('A -> 'B) -> x: 'A -> 'B"

                let project = ProjectInfo.library "Vesper.Applier"
                let artifact = compileSourceTo project src

                expectNoFSharpCore artifact "the function-typed parameter is Vesper.Fun"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)
                Expect.isFalse (refs |> Array.contains "FSharp.Core") "the DLL does not reference FSharp.Core"
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

            // ---- Function values as Vesper.Fun closures -----------------
            test "the Vesper.Core.dll the cutover references is on disk and named Vesper.Core" {
                // Forcing the lazy compiles prim-types-min.clr.fs to the on-disk DLL and loads
                // it for in-process resolution.
                let corePath = vesperCoreDll.Value
                Expect.isTrue (File.Exists corePath) "Vesper.Core.dll written to disk"

                let coreAsm = AssemblyName.GetAssemblyName corePath
                Expect.equal coreAsm.Name "Vesper.Core" "the library is named Vesper.Core"
            }

            test "a closure program references Vesper.Core (for Fun), not FSharp.Core, and runs" {
                // A lambda capturing a genuine local (`mk`'s parameter `n`) is synthesised as a
                // closure, whereas one capturing only a top-level VALUE lowers to a static method.
                let artifact =
                    compileSource "R1Closure" "let mk n = (fun x -> x + n)\nlet f = mk 1\nprintfn \"%d\" (f 41)"

                expectNoFSharpCore artifact "a plain closure"

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

            test "the synthesised closure derives from System.Object and implements Vesper.Fun`2" {
                let artifact =
                    compileSource "R1ClosureShape" "let mk n = (fun x -> x + n)\nlet f = mk 1\nprintfn \"%d\" (f 41)"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let implementsFun (t: Type) =
                    t.GetInterfaces()
                    |> Array.exists (fun i -> i.IsGenericType && i.GetGenericTypeDefinition().FullName = "Vesper.Fun`2")

                match asm.GetTypes() |> Array.tryFind implementsFun with
                | None -> failtest "no emitted type implements Vesper.Fun`2"
                | Some t ->
                    Expect.equal t.BaseType typeof<Object> "the closure derives from System.Object (no FSharpFunc base)"

                    Expect.isNotNull
                        (t.GetMethod("Invoke"))
                        "the closure has an Invoke method implementing the Fun slot"
            }

            test "an eta-reified curried operator value (`let add = (+)`) runs through nested Fun closures" {
                let artifact = compileSource "R1Eta" "let add = (+)\nprintfn \"%d\" (add 40 2)"

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Trim())
                    "42"
                    "outer Fun closure newobjs the inner; Invoke().Invoke() runs op_Addition"
            }

            // ---- Vesper.List: the cons-list package ---------------------
            test
                "Vesper.List.dll exports List`1 (Cons/Empty + IsEmpty/Head/Tail) and ListModule::fold (its own package)" {
                // Forcing the lazy compiles `src/Vesper.List/list.fs` into a standalone DLL.
                let listPath = vesperListDll.Value
                let listAsm = Assembly.LoadFrom listPath

                let listTy =
                    listAsm.GetTypes()
                    |> Array.tryFind (fun t -> t.FullName = "Vesper.Collections.List`1")

                match listTy with
                | None -> failtest "Vesper.List.dll has no Vesper.Collections.List`1"
                | Some t ->
                    Expect.isTrue t.IsGenericTypeDefinition "List`1 is a generic type definition"
                    Expect.isNotNull (t.GetMethod "Cons") "List`1 has a static Cons factory"
                    Expect.isNotNull (t.GetMethod "Empty") "List`1 has a static Empty factory (`[]` case)"
                    Expect.isNotNull (t.GetMethod "get_IsEmpty") "List`1 has an instance get_IsEmpty"
                    Expect.isNotNull (t.GetMethod "get_Head") "List`1 has an instance get_Head"
                    Expect.isNotNull (t.GetMethod "get_Tail") "List`1 has an instance get_Tail"

                // `module List` compiles to a `Vesper.Collections.ListModule` static
                // class holding the public `fold` (a 2-typar generic static method).
                let listModule =
                    listAsm.GetTypes()
                    |> Array.tryFind (fun t -> t.FullName = "Vesper.Collections.ListModule")

                match listModule with
                | None -> failtest "Vesper.List.dll has no Vesper.Collections.ListModule"
                | Some m ->
                    let fold = m.GetMethod "fold"
                    Expect.isNotNull fold "ListModule has a static fold"
                    Expect.isTrue fold.IsStatic "fold is static"
                    Expect.isTrue fold.IsGenericMethodDefinition "fold is a generic method definition"
                    Expect.equal (fold.GetGenericArguments().Length) 2 "fold has two generic parameters"

                let refs = listAsm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                // `Head`/`Tail` use `failwith` → BCL `System.Exception`, and `fold`'s folder is
                // a `Vesper.Fun`, so the DLL references Vesper.Core and no FSharp.Core.
                Expect.isFalse
                    (refs |> Array.contains "FSharp.Core")
                    (sprintf "Vesper.List.dll is FSharp.Core-free (refs: %A)" refs)

                Expect.isTrue
                    (refs |> Array.contains "Vesper.Core")
                    (sprintf "Vesper.List.dll references Vesper.Core (fold's folder is a Vesper.Fun) (refs: %A)" refs)
            }

            // `List<'T>` authors only `interface seq<'T>` / `interface enumerator<'T>`, and never
            // `IEnumerable`, `IEnumerator`, `object Current` or `Reset`. The CLR backend
            // synthesises those BCL co-slots, so a plain BCL consumer can iterate the type.
            test "a BCL consumer iterates Vesper.List through the synthesised IEnumerable co-slots" {
                let listAsm = Assembly.LoadFrom vesperListDll.Value
                let listTy = listAsm.GetType "Vesper.Collections.List`1"
                let intList = listTy.MakeGenericType typeof<int>

                let empty () =
                    intList.GetMethod("Empty").Invoke(null, [||])

                let cons (h: int) (t: obj) =
                    intList.GetMethod("Cons").Invoke(null, [| box h; t |])

                // [1; 2; 3], built through the union's own factories.
                let xs = cons 1 (cons 2 (cons 3 (empty ())))

                // `IEnumerable<int>`'s `GetEnumerator` is the AUTHORED capability member, bound
                // to the BCL slot implicitly by name + signature.
                let generic = xs :?> System.Collections.Generic.IEnumerable<int>
                Expect.sequenceEqual generic [ 1; 2; 3 ] "IEnumerable<int> yields the elements in order"

                // The non-generic interface is entirely synthesised: its `GetEnumerator`
                // forwards to the capability's, and `object Current` boxes the capability's `'T`.
                let nonGeneric = xs :?> System.Collections.IEnumerable
                let e = nonGeneric.GetEnumerator()

                let walked =
                    [
                        while e.MoveNext() do
                            yield e.Current
                    ]

                Expect.equal walked [ box 1; box 2; box 3 ] "the non-generic IEnumerator yields the boxed elements"

                // `Reset` has no capability member to forward to, because the pull protocol has
                // no rewind, so the synthesised slot throws. It must EXIST, or the type would
                // not load; it just must not pretend to work.
                Expect.throwsT<NotSupportedException> (fun () -> e.Reset()) "the synthesised Reset co-slot throws"
            }

            // ---- The canonical sample: FSharp.Core-free in-process + bundle ----
            // The list literal builds a `Vesper.Collections.List` and `List.fold` is emitted
            // inline over it, so the sample is BCL + `Vesper.Core` + `Vesper.List`.
            test "the canonical sample compiles, runs in-process, prints 15 with no FSharp.Core" {
                let artifact = compileSource "CanonicalSample" fullSample

                expectNoFSharpCore artifact "the canonical sample"

                let asm = loadAssembly (Codegen.toBytes artifact)
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)
                Expect.contains refs "Vesper.Core" "references Vesper.Core (Fun)"
                Expect.contains refs "Vesper.List" "references Vesper.List (the cons-list)"

                Expect.isFalse
                    (refs |> Array.exists (fun n -> n = "FSharp.Core"))
                    (sprintf "no FSharp.Core AssemblyRef (refs: %A)" refs)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Trim()) "15" "List.fold (+) 0 [1..5] = 15"
            }

            // The bundle ships only the Vesper.* libraries the emitted PE references, and the
            // FSharp.Core-free app still runs out-of-process.
            test
                "the canonical sample's on-disk bundle ships Vesper.Core + Vesper.List + Vesper.Printf, no FSharp.Core.dll" {
                let outDir = tmpDir "selfhost-bundle"
                let project = withCore (ProjectInfo.app "XParsecBundle" outDir)
                let artifact = compileSourceTo project fullSample

                expectNoFSharpCore artifact "the canonical sample"

                // Deterministic regardless of a prior run leaving the dll behind.
                let fsCoreDst = Path.Combine(outDir, "FSharp.Core.dll")

                if File.Exists fsCoreDst then
                    File.Delete fsCoreDst

                Codegen.materialiseApp artifact

                Expect.isTrue (File.Exists(Path.Combine(outDir, "Vesper.Core.dll"))) "Vesper.Core.dll (Fun) shipped"

                Expect.isTrue
                    (File.Exists(Path.Combine(outDir, "Vesper.List.dll")))
                    "Vesper.List.dll (the cons-list) shipped"

                Expect.isTrue
                    (File.Exists(Path.Combine(outDir, "Vesper.Printf.dll")))
                    "Vesper.Printf.dll (the happy-path formatter) shipped"

                Expect.isFalse (File.Exists fsCoreDst) "no FSharp.Core.dll in the bundle"

                let exitCode, output = runOnDisk (Path.Combine(outDir, "XParsecBundle.dll"))
                Expect.equal exitCode 0 (sprintf "dotnet exits 0 (output was: %s)" output)
                Expect.equal (output.Trim()) "15" "the FSharp.Core-free bundle runs"
            }

            // `Vesper.Printf.dll` references `Vesper.Core` (its `RuntimeFormatState` implements
            // the Core-owned `IFormatSink`) AND `Vesper.List` (the `%A` engine's `Doc` child
            // lists + frame stack), so a `%d`-only program's bundle still ships both.
            test "a happy-path bundle ships Vesper.Printf + its Vesper.Core / Vesper.List deps, no FSharp.Core" {
                let outDir = tmpDir "selfhost-happy-bundle"
                let project = withCore (ProjectInfo.app "XParsecHappy" outDir)
                let artifact = compileSourceTo project "printfn \"%d\" 42"

                for stale in [ "FSharp.Core.dll"; "Vesper.Core.dll"; "Vesper.List.dll" ] do
                    let p = Path.Combine(outDir, stale)

                    if File.Exists p then
                        File.Delete p

                Codegen.materialiseApp artifact

                Expect.isTrue (File.Exists(Path.Combine(outDir, "Vesper.Printf.dll"))) "Vesper.Printf.dll shipped"
                Expect.isFalse (File.Exists(Path.Combine(outDir, "FSharp.Core.dll"))) "no FSharp.Core.dll"

                Expect.isTrue
                    (File.Exists(Path.Combine(outDir, "Vesper.Core.dll")))
                    "Vesper.Core.dll shipped because Vesper.Printf references it"

                Expect.isTrue
                    (File.Exists(Path.Combine(outDir, "Vesper.List.dll")))
                    "Vesper.List.dll shipped because the self-hosted %A engine references the Vesper cons-list"
            }

            // A referenced package's identity is read off the referenced FILE, not the
            // compiler host's own copy of it.
            test "a referenced Vesper package drives the emitted reference identity" {
                let project = withCore (ProjectInfo.defaults "XParsecRefIdentity")
                let src = "let xs = [1; 2; 3]"
                // Resolve `int` from the real contract stack, because `MockBuiltins` carries
                // no primitive reprs.
                let provider = ClrSymbolProviders.buildContract defaultPackages
                let artifact = compileAgainst provider project src

                Expect.contains
                    artifact.ReferencedAssemblies
                    "Vesper.List"
                    "the cons-list representation references Vesper.List"

                let asm = loadAssembly (Codegen.toBytes artifact)

                let listRef =
                    asm.GetReferencedAssemblies() |> Array.find (fun a -> a.Name = "Vesper.List")

                let expected = AssemblyName.GetAssemblyName(vesperListDll.Value).Version

                Expect.equal
                    listRef.Version
                    expected
                    "the emitted Vesper.List ref version is read off the referenced file"
            }
        ]
