module XParsec.FSharp.Codegen.Clr.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.PackageHarness

// `for x in src do body` lowers to `let e = src.GetEnumerator()` / `try while e.MoveNext()
// do (let x = e.Current in body)` / `finally if e <> null then e.Dispose()`. Which types
// those four slots resolve through is what the tests below distinguish.

[<Tests>]
let forInTests =
    testList
        "ForIn"
        [
            test "for-in over an empty BCL List<int> runs and prints nothing" {
                // `List<int>` takes the duck-typed struct path (its pattern
                // `GetEnumerator()` wins over `IEnumerable<int>`, as in C#), so this is
                // the value-typed loop at zero iterations: struct `Dispose`, no body.
                let src =
                    String.concat
                        "\n"
                        [
                            "let xs = new System.Collections.Generic.List<int>()"
                            "for x in xs do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "ForInEmpty" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "done" "empty list yields no iterations"
            }

            test "for-in over a populated IEnumerable<int> prints the elements in order" {
                // `Enumerable.Range` is a static returning a populated `IEnumerable<int>`,
                // so the source needs no external instance call (`xs.Add`) to fill it.
                let src =
                    String.concat "\n" [ "for x in System.Linq.Enumerable.Range(1, 3) do"; "    printfn \"%d\" x" ]

                let artifact = compileSource "ForInPopulated" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "iterates the sequence in order"
            }

            // The same struct path with elements in it: `List<int>.Enumerator` lives in a
            // value local, dispatched by `ldloca` + `constrained.` callvirt, so no boxed
            // `IEnumerator` is allocated.
            test "for-in over a populated List<int> walks its non-boxing struct Enumerator" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let xs = new System.Collections.Generic.List<int>(System.Linq.Enumerable.Range(1, 3))"
                            "for x in xs do"
                            "    printfn \"%d\" x"
                        ]

                let artifact = compileSource "ForInStructEnum" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "iterates the struct enumerator in order"

                // The interface (boxing) path never emits `constrained.` (0xFE 0x16), so
                // its presence is what distinguishes the two walks.
                let il = peMethodIl bytes "Program" "Main"

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "Main IL contains a `constrained.` prefix (non-boxing struct enumerator)"
            }

            test "for-in over a duck-typed source (no IEnumerable<'T>) type-checks via the pattern GetEnumerator()" {
                // `BitArray` implements only the NON-generic `IEnumerable`, so the element
                // type comes from its `GetEnumerator(): IEnumerator`'s `Current`.
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (ba: System.Collections.BitArray) ="
                            "    for x in ba do"
                            "        ()"
                        ]

                let provider = ClrSymbolProviders.buildContract defaultPackages
                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors |> Seq.toList

                Expect.isEmpty errors (sprintf "duck-typed for-in should type-check; got %A" errors)
            }

            // `ResizeArray<'T>` abbreviates `System.Collections.Generic.List<'T>`. The
            // explicit `<int>` pins the element type at the construction node, which is
            // what picks the parameterless `newobj List`1<int>::.ctor()`.
            test "no-`new` ResizeArray<int>() constructs the BCL List<int> and iterates" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let xs = ResizeArray<int>()"
                            "for x in xs do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "ResizeArrayCtor" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "done" "empty ResizeArray yields no iterations"
            }

            // `Take<TSource>` owns its typar, so the call site instantiates it to a fresh
            // var (solved to `int` from the `Range` argument) and codegen mints a
            // `MethodSpec Take<int>`, picked against a sibling `(…, Range)` overload.
            test "generic Enumerable.Take<TSource> resolves, emits a MethodSpec, and runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "for x in System.Linq.Enumerable.Take(System.Linq.Enumerable.Range(1, 5), 3) do"
                            "    printfn \"%d\" x"
                        ]

                let artifact = compileSource "GenericTake" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "Take(Range(1,5), 3) yields the first three"
            }

            // `for x in this` inside a generic class's own member: the class typar `'T`
            // has to be instantiated with the use-site `int` to pin the loop's bound
            // variable, which the `<<<` in the body then needs solved.
            test "generic for-in over `this` with a bitwise-shift loop body runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type C<'T>(items: System.Collections.Generic.IEnumerable<'T>) ="
                            "    interface System.Collections.Generic.IEnumerable<'T> with"
                            "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<'T> = items.GetEnumerator()"
                            "    interface System.Collections.IEnumerable with"
                            "        member this.GetEnumerator() : System.Collections.IEnumerator = items.GetEnumerator() :> System.Collections.IEnumerator"
                            "    member this.ComputeHashCode() ="
                            "        let mutable res = 0"
                            "        for x in this do"
                            "            res <- (res <<< 1) + (hash x) + 631"
                            "        res"
                            "let c = C<int>(System.Linq.Enumerable.Range(1, 3))"
                            "printfn \"%d\" (c.ComputeHashCode())"
                        ]

                let artifact = compileSource "GenericForInShift" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                // `hash` of an int is the int. e=1: (0<<<1)+1+631=632;
                // e=2: (632<<<1)+2+631=1897; e=3: (1897<<<1)+3+631=4428.
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "4428"
                    "for-in folds the three elements with the shift body"
            }

            // Both source and enumerator are project-local, and neither implements any
            // enumeration interface: the loop resolves entirely through the user member
            // tables (`Counter.GetEnumerator`, then `Enum.MoveNext` / `Enum.Current`).
            test "for-in over a user duck-typed source (pattern GetEnumerator, no interface) type-checks" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Enum ="
                            "    val mutable Cur : int"
                            "    val Stop : int"
                            "    new(stop: int) = { Cur = 0; Stop = stop }"
                            "    member this.MoveNext() : bool ="
                            "        this.Cur <- this.Cur + 1"
                            "        this.Cur <= this.Stop"
                            "    member this.Current : int = this.Cur"
                            "type Counter(stop: int) ="
                            "    member _.GetEnumerator() : Enum = Enum(stop)"
                            "let f (c: Counter) ="
                            "    for x in c do"
                            "        printfn \"%d\" x"
                        ]

                let provider = ClrSymbolProviders.buildContract defaultPackages
                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors |> Seq.toList

                Expect.isEmpty errors (sprintf "user duck-typed for-in should type-check; got %A" errors)
            }

            // The same shape run end-to-end: all three handles `callvirt` the user
            // `TypeDef`'s own slots.
            test "for-in over a user duck-typed source walks its user enumerator and prints the elements" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Enum ="
                            "    val mutable Cur : int"
                            "    val Stop : int"
                            "    new(stop: int) = { Cur = 0; Stop = stop }"
                            "    member this.MoveNext() : bool ="
                            "        this.Cur <- this.Cur + 1"
                            "        this.Cur <= this.Stop"
                            "    member this.Current : int = this.Cur"
                            "type Counter(stop: int) ="
                            "    member _.GetEnumerator() : Enum = Enum(stop)"
                            "let c = Counter(3)"
                            "for x in c do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "UserDuckTyped" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3\ndone" "walks the user enumerator in order"
            }

            // `MoveNext` mutates `this.Cur`, so the walk must address the struct
            // enumerator for the mutation to persist across iterations; a by-value copy
            // never advances and the loop hangs, which is what running to `done` rules out.
            test "for-in over a user duck-typed struct enumerator walks it by address without boxing" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Enum ="
                            "    val mutable Cur : int"
                            "    val Stop : int"
                            "    new(stop: int) = { Cur = 0; Stop = stop }"
                            "    member this.MoveNext() : bool ="
                            "        this.Cur <- this.Cur + 1"
                            "        this.Cur <= this.Stop"
                            "    member this.Current : int = this.Cur"
                            "type Counter(stop: int) ="
                            "    member _.GetEnumerator() : Enum = Enum(stop)"
                            "let c = Counter(3)"
                            "for x in c do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "UserDuckTypedStruct" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3\ndone" "walks the struct enumerator in order"

                // `MoveNext` / `Current` are non-virtual, so they take a direct `call` on
                // the address. This enumerator is not `IDisposable`, so a `constrained.`
                // (0xFE 0x16) anywhere in `Main` could only be a member call.
                let il = peMethodIl bytes "Program" "Main"

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isFalse
                    hasConstrained
                    "struct enumerator members dispatch via direct `call`, not `constrained. callvirt`"
            }

            // A duck-typed enumerator that also implements `System.IDisposable` is
            // disposed in the `finally` (C# parity). `Dispose` prints, so the expected
            // output pins both that it fired and where: once, after the walk, before `done`.
            test "for-in over a user duck-typed disposable enumerator disposes it once after the walk" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Enum ="
                            "    val mutable Cur : int"
                            "    val Stop : int"
                            "    new(stop: int) = { Cur = 0; Stop = stop }"
                            "    member this.MoveNext() : bool ="
                            "        this.Cur <- this.Cur + 1"
                            "        this.Cur <= this.Stop"
                            "    member this.Current : int = this.Cur"
                            "    interface System.IDisposable with"
                            "        member _.Dispose() = printfn \"disposed\""
                            "type Counter(stop: int) ="
                            "    member _.GetEnumerator() : Enum = Enum(stop)"
                            "let c = Counter(3)"
                            "for x in c do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "UserDuckTypedDisposable" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n2\n3\ndisposed\ndone"
                    "walks the enumerator then disposes it once before 'done'"
            }

            // The four slots split across the local/external seam: `Wrap.GetEnumerator` is
            // a local member, but the `List<int>.Enumerator` it hands back is a BCL struct
            // whose `MoveNext` / `Current` / `Dispose` are external member refs.
            test "for-in over a user source whose GetEnumerator returns a BCL struct enumerator walks it" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type Wrap(xs: System.Collections.Generic.List<int>) ="
                            "    member _.GetEnumerator() = xs.GetEnumerator()"
                            "let xs = new System.Collections.Generic.List<int>(System.Linq.Enumerable.Range(1, 3))"
                            "for x in Wrap(xs) do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "UserSourceExternalEnum" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n2\n3\ndone"
                    "walks the external struct enumerator in order"
            }

            // Here the SOURCE is the struct, not just the enumerator: `GetEnumerator` is a
            // method call on a value, so `c` must be addressed (`ldloca`) too. Pushing it
            // by value and `callvirt`-ing is malformed IL on a value type.
            test "for-in over a value-type struct source addresses it for GetEnumerator and walks" {
                let src =
                    String.concat
                        "\n"
                        [
                            "[<Struct>]"
                            "type Enum ="
                            "    val mutable Cur : int"
                            "    val Stop : int"
                            "    new(stop: int) = { Cur = 0; Stop = stop }"
                            "    member this.MoveNext() : bool ="
                            "        this.Cur <- this.Cur + 1"
                            "        this.Cur <= this.Stop"
                            "    member this.Current : int = this.Cur"
                            "[<Struct>]"
                            "type Counter ="
                            "    val Stop : int"
                            "    new(stop: int) = { Stop = stop }"
                            "    member this.GetEnumerator() : Enum = Enum(this.Stop)"
                            "let c = Counter(3)"
                            "for x in c do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let artifact = compileSource "ValueTypeSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3\ndone" "walks the value-type source in order"
            }

            // A BARE cons-list, with no `:> seq` upcast: the source is an external UNION
            // whose `.fsi` declares `interface IEnumerable<'T>`. `runsPackages` builds the
            // real `Vesper.List` through this backend, so the callvirt has to land.
            test "a BARE cons-list `for x in [1;2;3]` iterates the real Vesper.List on CLR (runtime)" {
                let src = String.concat "\n" [ "for x in [1; 2; 3] do"; "    printfn \"%d\" x" ]

                runsPackages [] "1\n2\n3" src
            }

            // `'T[]` declares `interface seq<'T>`, whose platform repr is
            // `IEnumerable<T>` — an interface a CLR array implements without any Vesper code.
            // So this takes the boxing `Interface` walk: correct, though an index loop would
            // be faster. Built by the raw `newarr` intrinsic, an array LITERAL lowering
            // through a module this test's package set does not serve.
            test "`for x in arr` walks an array through the seq capability on CLR (runtime)" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let a : int[] = (# \"newarr !0\" type (int) 3 : int[] #)"
                            "a.[0] <- 1"
                            "a.[1] <- 2"
                            "a.[2] <- 3"
                            "for x in a do"
                            "    printfn \"%d\" x"
                        ]

                runsPackages [] "1\n2\n3" src
            }

            test "for-in over a user class implementing IEnumerable<int> resolves through its interface slots" {
                let src =
                    String.concat
                        "\n"
                        [
                            "type C(e: System.Collections.Generic.IEnumerator<int>) ="
                            "    interface System.Collections.Generic.IEnumerable<int> with"
                            "        member this.GetEnumerator() : System.Collections.Generic.IEnumerator<int> = e"
                            ""
                            "let f (c: C) ="
                            "    for x in c do"
                            "        printfn \"%d\" x"
                        ]

                let provider = ClrSymbolProviders.buildContract defaultPackages
                let lexed, file = parseFile src

                let tast =
                    Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors |> Seq.toList

                Expect.isEmpty errors (sprintf "user-interface for-in should type-check; got %A" errors)
            }

            // A RECORD source, reached only through `interface seq<'T>`, because a pattern
            // `GetEnumerator()` on a record would not resolve. Generic on purpose: the
            // record's typar is substituted with the use-site `int` to pin `x`.
            test "for-in over a generic record implementing the seq capability walks its elements" {
                let src =
                    String.concat
                        "\n"
                        [
                            "open Vesper.Collections"
                            "[<Struct>]"
                            "type BagEnumerator<'T> ="
                            "    val Items : 'T[]"
                            "    val mutable Idx : int"
                            "    new(items: 'T[]) = { Items = items; Idx = -1 }"
                            "    interface enumerator<'T> with"
                            "        member this.Current : 'T = this.Items.[this.Idx]"
                            "        member this.MoveNext() : bool ="
                            "            this.Idx <- this.Idx + 1"
                            "            this.Idx < this.Items.Length"
                            "    interface Vesper.disposable with"
                            "        member this.Dispose() : unit = ()"
                            "type Bag<'T> ="
                            "    { Items: 'T[] }"
                            "    interface seq<'T> with"
                            "        member this.GetEnumerator() = (new BagEnumerator<'T>(this.Items) :> enumerator<'T>)"
                            "let b : Bag<int> = { Items = [| 1; 2; 3 |] }"
                            "for x in b do"
                            "    printfn \"%d\" x"
                        ]

                let artifact = compileSource "RecordForIn" src

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n2\n3"
                    "`for x in b` walks the record's elements in order"
            }
        ]
