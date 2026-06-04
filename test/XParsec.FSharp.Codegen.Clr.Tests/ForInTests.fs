module XParsec.FSharp.Codegen.Clr.Tests.ForInTests

open System
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// vesper-set-sprint-phase-4 Step 4.2 / B-6 backend tests. `for x in src do body`
// over an `IEnumerable<'T>` lowers to the standard enumerator loop:
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
                // Post §4.4 this `List<int>` source now takes the duck-typed struct
                // path (C# precedence: its pattern `GetEnumerator()` returning the
                // value-type `List<int>.Enumerator` wins over the boxing
                // `IEnumerable<int>` interface), so this also guards the new
                // value-receiver loop on the empty case (zero iterations + struct
                // `Dispose` in the finally).
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
                // Source it from `System.Linq.Enumerable.Range` — a static method
                // returning a non-empty `IEnumerable<int>` directly, so the test
                // exercises a populated walk without depending on external
                // *instance*-method calls (`xs.Add`, a separate resolution gap).
                let src =
                    String.concat "\n" [ "for x in System.Linq.Enumerable.Range(1, 3) do"; "    printfn \"%d\" x" ]

                let _, artifact = compileSource "ForInPopulated" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 "Main returns 0"
                Expect.equal (output.Replace("\r", "").Trim()) "1\n2\n3" "iterates the sequence in order"
            }

            // vesper-set-sprint-phase-4 Step 4.4 — duck-typed struct enumerator
            // codegen. A concrete `List<int>` source walks its non-boxing value-type
            // `List<int>.Enumerator` (C# precedence prefers the pattern
            // `GetEnumerator()` over the `IEnumerable<int>` interface). The
            // enumerator lives in a value local, dispatched by `ldloca` +
            // `constrained. <Enumerator>` callvirt; no boxed `IEnumerator` is
            // allocated. The list is populated through the `List(IEnumerable<T>)`
            // ctor over `Linq.Range`, sidestepping the external-instance `Add` gap.
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

            // vesper-set-sprint-phase-4 Step 4.4 — front end only. The duck-typed
            // (pattern-based `GetEnumerator()`) codegen is deferred (the project's
            // first value-type member-call IL), so this asserts the *analysis*
            // resolves the loop rather than running the program.
            test "for-in over a duck-typed source (no IEnumerable<'T>) type-checks via the pattern GetEnumerator()" {
                // `System.Collections.BitArray` implements only the *non-generic*
                // `IEnumerable`, so the §4.2 `IEnumerable<'T>` interface probe
                // misses it; the §4.4 duck-typed fallback resolves it through its
                // public `GetEnumerator(): IEnumerator`, whose `MoveNext(): bool` +
                // `Current` property drive the loop and pin the element type. Before
                // §4.4 this raised a "source is not a supported enumerable"
                // diagnostic.
                let src =
                    String.concat
                        "\n"
                        [
                            "let f (ba: System.Collections.BitArray) ="
                            "    for x in ba do"
                            "        ()"
                        ]

                let provider = SymbolProviders.buildContract defaultManifests
                let lexed, file = parseFile src
                let tast = Pipeline.analyseSem provider src lexed file

                let errors =
                    tast.Diagnostics
                    |> Seq.filter (fun d -> d.Severity = Severity.Error)
                    |> Seq.toList

                Expect.isEmpty errors (sprintf "duck-typed for-in should type-check; got %A" errors)
            }

            // Constructing an external *generic*
            // class through the no-`new` sugar `ResizeArray<int>()`. `ResizeArray<'T>`
            // is the `Vesper.List` abbreviation `= System.Collections.Generic.List<'T>`
            // (now that contract extraction resolves the qualified BCL head against
            // the metadata provider instead of collapsing onto the local cons-list
            // `List` union). The explicit `<int>` pins the element type up front, so
            // the construction node carries `TyClass(List`1, [int])` — driving the
            // parameterless-ctor overload pick and emitting `newobj List`1<int>::.ctor()`.
            // Iterating the (empty) result proves it's a genuine BCL `List<int>`, not
            // a free TyVar or the wrong nominal type.
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

            // A *generic external static method*
            // (`System.Linq.Enumerable.Take<TSource>(IEnumerable<TSource>, int)`,
            // the `truncate` blocker). Its method-owned `TSource` is no longer
            // dropped at extraction: it rides as a baked `FTTypar(Method, 0)` in the
            // member's `ExternalSignature` template, the call site instantiates it to
            // a fresh var (solved to `int` from the `Range` arg), and codegen mints a
            // `MethodSpec Take<int>`. Iterating the (truncated) result proves the
            // whole path — including overload selection against the `(…, Range)`
            // sibling overload — resolves and runs.
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
        ]
