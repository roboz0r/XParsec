module XParsec.FSharp.Codegen.Clr.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// `for x in src do body` over an `IEnumerable<'T>` lowers to the standard
// enumerator loop:
//   let e = src.GetEnumerator() in
//   try while e.MoveNext() do (let x = e.Current in body)
//   finally if e <> null then e.Dispose()
// The four member slots are resolved through the *interface* declaring types
// (IEnumerable<'T> / IEnumerator<'T> / IEnumerator / IDisposable), so a
// `callvirt` dispatches to the source collection's implementation. The source
// here is a BCL `System.Collections.Generic.List<int>` (the metadata provider
// surfaces its `IEnumerable<int>` interface for the front-end element-type
// probe). Asserting on captured stdout proves the loop walks the elements in
// order and terminates (the empty case prints nothing).

[<Tests>]
let forInTests =
    testList
        "ForIn"
        [
            test "for-in over an empty BCL List<int> runs and prints nothing" {
                // `List<int>` takes the duck-typed struct path (C# precedence: its
                // pattern `GetEnumerator()` returning the value-type `List<int>.Enumerator`
                // wins over the boxing `IEnumerable<int>` interface), so this also
                // guards the value-receiver loop on the empty case (zero iterations +
                // struct `Dispose` in the finally).
                let src =
                    String.concat
                        "\n"
                        [
                            "let xs = new System.Collections.Generic.List<int>()"
                            "for x in xs do"
                            "    printfn \"%d\" x"
                            "printfn \"done\""
                        ]

                let _, artifact = compileSource "ForInEmpty" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "done" "empty list yields no iterations"
            }

            test "for-in over a populated IEnumerable<int> prints the elements in order" {
                // Source from `System.Linq.Enumerable.Range` — a static method returning
                // a non-empty `IEnumerable<int>` directly, avoiding external
                // instance-method calls (`xs.Add`).
                let src =
                    String.concat "\n" [ "for x in System.Linq.Enumerable.Range(1, 3) do"; "    printfn \"%d\" x" ]

                let _, artifact = compileSource "ForInPopulated" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "iterates the sequence in order"
            }

            // Duck-typed struct enumerator codegen. A concrete `List<int>` source
            // walks its non-boxing value-type `List<int>.Enumerator` (C# precedence
            // prefers the pattern `GetEnumerator()` over the `IEnumerable<int>`
            // interface). The enumerator lives in a value local, dispatched by
            // `ldloca` + `constrained. <Enumerator>` callvirt; no boxed `IEnumerator`
            // is allocated. The list is populated through the `List(IEnumerable<T>)`
            // ctor over `Linq.Range`.
            test "for-in over a populated List<int> walks its non-boxing struct Enumerator" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let xs = new System.Collections.Generic.List<int>(System.Linq.Enumerable.Range(1, 3))"
                            "for x in xs do"
                            "    printfn \"%d\" x"
                        ]

                let _, artifact = compileSource "ForInStructEnum" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "iterates the struct enumerator in order"

                // The value-type path emits the `constrained.` prefix (0xFE 0x16)
                // before each enumerator member callvirt — the interface (boxing)
                // path never does. Its presence proves the non-boxing struct walk.
                let il = peMethodIl bytes "Program" "Main"

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isTrue hasConstrained "Main IL contains a `constrained.` prefix (non-boxing struct enumerator)"
            }

            test "for-in over a duck-typed source (no IEnumerable<'T>) type-checks via the pattern GetEnumerator()" {
                // `System.Collections.BitArray` implements only the *non-generic*
                // `IEnumerable`. The duck-typed fallback resolves it through its
                // public `GetEnumerator(): IEnumerator`, whose `MoveNext(): bool` +
                // `Current` property drive the loop and pin the element type.
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (ba: System.Collections.BitArray) ="
                            "    for x in ba do"
                            "        ()"
                        ]

                let provider = ClrSymbolProviders.buildContract defaultManifests
                let lexed, file = parseFile src
                let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors |> Seq.toList

                Expect.isEmpty errors (sprintf "duck-typed for-in should type-check; got %A" errors)
            }

            // `ResizeArray<'T>` is the `Vesper.List` abbreviation
            // `= System.Collections.Generic.List<'T>`. The explicit `<int>` pins the
            // element type up front, so the construction node carries
            // `TyClass(List`1, [int])` — driving the parameterless-ctor overload pick
            // and emitting `newobj List`1<int>::.ctor()`. Iterating the (empty) result
            // proves it's a genuine BCL `List<int>`.
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

                let _, artifact = compileSource "ResizeArrayCtor" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "done" "empty ResizeArray yields no iterations"
            }

            // `System.Linq.Enumerable.Take<TSource>`: its method-owned `TSource`
            // rides as a baked `FTTypar(Method, 0)` in the member's `ExternalSignature`
            // template; the call site instantiates it to a fresh var (solved to `int`
            // from the `Range` arg), and codegen mints a `MethodSpec Take<int>`.
            // Iterating the (truncated) result proves the whole path — including
            // overload selection against the `(…, Range)` sibling overload — resolves
            // and runs.
            test "generic Enumerable.Take<TSource> resolves, emits a MethodSpec, and runs" {
                let src =
                    String.concat
                        "\n"
                        [
                            "for x in System.Linq.Enumerable.Take(System.Linq.Enumerable.Range(1, 5), 3) do"
                            "    printfn \"%d\" x"
                        ]

                let _, artifact = compileSource "GenericTake" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "Take(Range(1,5), 3) yields the first three"
            }

            // A *generic* user class implementing `IEnumerable<'T>` walked by
            // `for x in this` inside its own member. Exercises the generic
            // interface-enumerator probe (`tryLocalInterfaceEnumerator` instantiates
            // the class typar with the use-site arg) and the bitwise shift operator
            // surface (`x <<< 1`). Run at `int` to prove the whole path codegens and
            // the enumerator actually walks the elements.
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

                let _, artifact = compileSource "GenericForInShift" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                // 0,1,2,3 folded: res starts 0; combineHash res (hash e) =
                // (res<<<1)+hash(e)+631. hash of an int is the int itself.
                // e=1: (0<<<1)+1+631=632; e=2: (632<<<1)+2+631=1897;
                // e=3: (1897<<<1)+3+631=4428.
                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "4428"
                    "for-in folds the three elements with the shift body"
            }

            // A project-local source class exposing only a pattern `GetEnumerator()`
            // — no `IEnumerable<'T>` — whose enumerator `E` is itself a user class
            // with `MoveNext(): bool` and a `Current` property. The duck-typed probe
            // (`tryLocalDuckTypedEnumerator`) resolves the loop through the user
            // member tables.
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

                let provider = ClrSymbolProviders.buildContract defaultManifests
                let lexed, file = parseFile src
                let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors |> Seq.toList

                Expect.isEmpty errors (sprintf "user duck-typed for-in should type-check; got %A" errors)
            }

            // The same shape, run end-to-end: the three handles (`GetEnumerator` on
            // the source, `MoveNext` / `Current` on the user enumerator `E`) resolve
            // through `EmitResolve.resolveInstanceMember` and `callvirt` the user
            // `TypeDef`'s slots.
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

                let _, artifact = compileSource "UserDuckTyped" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3\ndone" "walks the user enumerator in order"
            }

            // The user enumerator `E` is a `[<Struct>]`, so the loop walks it by
            // address (`ldloca` + a direct `call`), never boxing it — exactly how the
            // F# compiler lowers `for x in struct-enumerator`. The members `MoveNext`
            // / `Current` are ordinary (non-virtual) instance methods on `E`, so the
            // call is a plain `call`, *not* `constrained. callvirt`: a
            // `constrained. callvirt` to a non-virtual struct `MethodDef`
            // mis-dispatches against an uninitialised receiver (the walk never
            // advances, infinite-loops). `MoveNext`'s mutation to `this.Cur` must
            // persist across iterations through the by-address receiver, so a wrong
            // (by-value-copy) walk would loop forever — the run-to-`done` assertion is
            // the guard.
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

                let _, artifact = compileSource "UserDuckTypedStruct" src
                let bytes = Codegen.toBytes artifact
                let exitCode, output = runEntryPoint bytes

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3\ndone" "walks the struct enumerator in order"

                // The struct walk addresses the enumerator (`ldloca`) and dispatches
                // its own non-virtual members with a direct `call` — not
                // `constrained. callvirt`. This enumerator isn't `IDisposable`, so the
                // only place a `constrained.` (0xFE 0x16) could appear is the
                // (now-eliminated) member-call path; its absence proves the
                // direct-`call` lowering.
                let il = peMethodIl bytes "Program" "Main"

                let hasConstrained =
                    il
                    |> Array.windowed 2
                    |> Array.exists (fun w -> w.[0] = 0xFEuy && w.[1] = 0x16uy)

                Expect.isFalse
                    hasConstrained
                    "struct enumerator members dispatch via direct `call`, not `constrained. callvirt`"
            }

            // A user duck-typed enumerator that also implements `System.IDisposable`
            // is disposed in a `finally` after the walk (C# parity). The front-end
            // probe sets `dispose` from the enumerator's interface impls; codegen
            // mints `System.IDisposable::Dispose` and emits the null-checked `finally`
            // callvirt. The side effect (a `Dispose` that prints) must fire exactly
            // once, after the elements and before `done`.
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

                let _, artifact = compileSource "UserDuckTypedDisposable" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n2\n3\ndisposed\ndone"
                    "walks the enumerator then disposes it once before 'done'"
            }

            // A project-local source `Wrap` exposes a pattern `GetEnumerator()` that
            // hands back an *external* (BCL) enumerator — `List<int>.Enumerator`, a
            // `[<Struct>]` that is also `IDisposable`. The loop resolves the local
            // `GetEnumerator` through `resolveInstanceMember` but mints the external
            // enumerator's `MoveNext` / `Current` / `Dispose` via `ExternalMemberRefOn`.
            // Because the enumerator is a struct it walks by address; because it's
            // `IDisposable` it disposes through `constrained. <Enumerator>` in the
            // `finally`.
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

                let _, artifact = compileSource "UserSourceExternalEnum" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n2\n3\ndone"
                    "walks the external struct enumerator in order"
            }

            // The for-in *source* is itself a
            // `[<Struct>]`. `GetEnumerator` is a method call on a value, so the source
            // must be addressed (`ldloca`) the same way the enumerator receiver is —
            // not pushed by value and `callvirt`-ed (malformed IL on a value type).
            // The seq module's `MapSeq`/`ArraySeq` are exactly this shape, so this is
            // the minimal isolation case that forces the fix. The struct `Counter`'s
            // own `GetEnumerator` is a non-virtual `MethodDef`, dispatched by a direct
            // `call` on the address; the struct `Enum` then walks by address as before.
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

                let _, artifact = compileSource "ValueTypeSource" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3\ndone" "walks the value-type source in order"
            }

            // §14.6 capstone (W1+W4): a BARE cons-list `[1;2;3]` — NO `:> seq` upcast —
            // iterates over the REAL `Vesper.List` DLL. The front-end admits it because
            // the `.fsi` union's `interface IEnumerable<'T>` now rides
            // `ExternalTypeShape.Union.interfaces` (`tryForInEnumerator`'s union arm);
            // codegen emits a `GetEnumerator` callvirt against `IEnumerable<int>`, which
            // dispatches to `List<'T>`'s native impl (its `ListEnumerator` cursor walk).
            // THE load-bearing gate: this RUNS the emitted IL (W1's shared admission
            // could type-check yet emit a callvirt against a List that lacks the
            // interface — a runtime fault). `runsPackages` builds Vesper.List through our
            // own backend and runs the driver in `packageAlc`.
            test "a BARE cons-list `for x in [1;2;3]` iterates the real Vesper.List on CLR (runtime)" {
                let src = String.concat "\n" [ "for x in [1; 2; 3] do"; "    printfn \"%d\" x" ]

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

                let provider = ClrSymbolProviders.buildContract defaultManifests
                let lexed, file = parseFile src
                let tast = Pipeline.analyseSem provider (Hashing.originSourceOfText src lexed) file

                let errors = tast.Diagnostics |> Diagnostic.errors |> Seq.toList

                Expect.isEmpty errors (sprintf "user-interface for-in should type-check; got %A" errors)
            }

            // A project-local RECORD source implementing the iteration capability
            // (`interface seq<'T>`). The front end resolves it through the same
            // `IInterfaceImplHost` walk the class and union hosts use
            // (`tryLocalInterfaceEnumeratorOn`); the backend needed nothing new, since a
            // record's synthesised `IEnumerable<'T>` co-slots are exactly the ones the
            // class `Interface` walk already `callvirt`s. Generic on purpose: the record's
            // typar has to be substituted with the use-site `int` to pin the loop binder.
            // Only the `Interface` surface is open to a record — a pattern `GetEnumerator()`
            // on a record would need a record member table in `resolveInstanceMember`.
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

                let tast, artifact = compileSource "RecordForIn" src
                Expect.isEmpty tast.Diagnostics (sprintf "no diagnostics: %A" tast.Diagnostics)

                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"

                Expect.equal
                    (output.Replace("\r", "").Trim())
                    "1\n2\n3"
                    "`for x in b` walks the record's elements in order"
            }
        ]
