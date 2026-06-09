module XParsec.FSharp.Codegen.Clr.Tests.TupleTests

// Tuple representation — every tuple, at every
// arity, is a `System.ValueTuple`n` (no `System.Tuple`). The three testLists
// below track the layers: `ValueTuple`n` family resolution
// (`ClrProvider`/`ClrEncoder`); encoding a bare `FTTuple` to a generic
// instantiation TypeSpec — both pure metadata-resolution checks. Then the
// first *behavioural* gate: emit + reflect a constructed tuple value.

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// A fresh provider over an empty metadata context — enough to mint TypeRef /
/// TypeSpec / MemberRef handles without emitting a full assembly. `defaults`
/// carries the `int`/`string`/… → IL-repr map the element encoding needs;
/// `nullProvider` because no external-symbol resolution is exercised here.
let private provider () =
    ClrProvider(MetadataContext(), IntrinsicRepr.defaults, Map.empty, ExternalSymbols.nullProvider, "TupleTest")

let private ftConst (name: string) = FTConst(name, EqArray.empty)

[<Tests>]
let tests =
    testList
        "Tuple representation — Step 1 (ValueTuple resolution)"
        [
            test "ValueTuple`2<int, string> resolves: non-nil TypeSpec, ctor, and two Item fields" {
                let p = provider ()
                let refs = p.ValueTupleRefs [ ftConst "int"; ftConst "string" ]
                Expect.isFalse refs.TypeSpec.IsNil "TypeSpec handle is nil"
                Expect.isFalse refs.Ctor.IsNil "ctor handle is nil"
                Expect.equal refs.ItemFields.Length 2 "expected two Item fields"

                for f in refs.ItemFields do
                    Expect.isFalse f.IsNil "an Item field handle is nil"
            }

            test "arity 7 resolves with seven Item fields" {
                let p = provider ()
                let refs = p.ValueTupleRefs(List.replicate 7 (ftConst "int"))
                Expect.equal refs.ItemFields.Length 7 "expected seven Item fields"
                Expect.isFalse refs.Ctor.IsNil "ctor handle is nil"
            }

            test "arity below 2 is rejected (the nullary unit case stays on its own path)" {
                let p = provider ()
                Expect.throws (fun () -> p.ValueTupleRefs [ ftConst "int" ] |> ignore) "arity 1 should throw"
            }

            test "arity above 7 is deferred (TRest nesting not yet emitted)" {
                let p = provider ()

                Expect.throws
                    (fun () -> p.ValueTupleRefs(List.replicate 8 (ftConst "int")) |> ignore)
                    "arity 8 should throw"
            }
        ]

/// Step 2 gate: a bare `FTTuple` encodes (it used to `failwithf`) to a
/// `ValueTuple`n` generic-instantiation TypeSpec via `encodeType`. Pure
/// metadata-resolution checks — that the encoder no longer rejects the
/// structural tuple and mints a non-nil spec for each in-range arity.
[<Tests>]
let encodeTests =
    let ftTuple (tys: FrozenType list) = FTTuple(EqArray.ofList tys)

    testList
        "Tuple representation — Step 2 (FTTuple encoding)"
        [
            test "FTTuple [int; string] encodes to a non-nil TypeSpec" {
                let p = provider ()
                let spec = p.TypeSpecOf(ftTuple [ ftConst "int"; ftConst "string" ])
                Expect.isFalse spec.IsNil "tuple TypeSpec handle is nil"
            }

            test "nested FTTuple encodes (each element recurses through encodeType)" {
                let p = provider ()
                let inner = ftTuple [ ftConst "int"; ftConst "int" ]
                let spec = p.TypeSpecOf(ftTuple [ ftConst "string"; inner ])
                Expect.isFalse spec.IsNil "nested tuple TypeSpec handle is nil"
            }

            test "arity 8 tuple is rejected at encode (TRest nesting deferred)" {
                let p = provider ()

                Expect.throws
                    (fun () -> p.TypeSpecOf(ftTuple (List.replicate 8 (ftConst "int"))) |> ignore)
                    "arity 8 tuple encode should throw"
            }
        ]

/// Step 3 gate: a standalone `TExprG.Tuple` is built into a value
/// (`newobj ValueTuple`n::.ctor`) rather than flattened as an argument list. A
/// top-level function returning a tuple is emitted as a static method; invoking
/// it by reflection yields a live `System.ValueTuple`n` whose `Item` fields hold
/// the constructed elements. This is the first *behavioural* tuple gate (Steps
/// 1–2 were pure metadata resolution).
[<Tests>]
let constructTests =
    /// The single `fn$…`-mangled static method an emitted bare program carries.
    let theStaticFn (bytes: byte[]) : MethodInfo =
        let asm = loadAssembly bytes

        asm.GetTypes()
        |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static))
        |> Array.find (fun m -> m.Name.StartsWith "fn$")

    testList
        "Tuple representation — Step 3 (construct a tuple value)"
        [
            test "a function returning (n, n+1) yields a ValueTuple`2 with the constructed fields" {
                let _, artifact = compileSource "TupleStep3" "let pair (n: int) = (n, n + 1)"
                let m = theStaticFn (Codegen.toBytes artifact)
                let result = m.Invoke(null, [| box 5 |])

                Expect.isNotNull result "tuple result is null"
                let ty = result.GetType()

                Expect.isTrue
                    (ty.FullName.StartsWith "System.ValueTuple`2")
                    (sprintf "expected a ValueTuple`2 but got %s" ty.FullName)

                let item1 = ty.GetField("Item1").GetValue result :?> int
                let item2 = ty.GetField("Item2").GetValue result :?> int
                Expect.equal item1 5 "Item1"
                Expect.equal item2 6 "Item2"
            }

            test "a heterogeneous (int, string) tuple constructs with both element types" {
                let _, artifact = compileSource "TupleStep3b" "let tag (n: int) = (n, \"x\")"
                let m = theStaticFn (Codegen.toBytes artifact)
                let result = m.Invoke(null, [| box 7 |])

                let ty = result.GetType()

                Expect.isTrue
                    (ty.FullName.StartsWith "System.ValueTuple`2")
                    (sprintf "expected a ValueTuple`2 but got %s" ty.FullName)

                Expect.equal (ty.GetField("Item1").GetValue result :?> int) 7 "Item1"
                Expect.equal (ty.GetField("Item2").GetValue result :?> string) "x" "Item2"
            }
        ]

/// Step 4 gate: tuple *destructuring* through the shared irrefutable `bindPattern`
/// (let / for-in) and the match compiler's new `TPatG.Tuple` arm. Each program is
/// a one-arg static function returning an int; reflecting the invoke result proves
/// the leaf bindings were pulled out of the `ValueTuple`n` `Item` fields. A
/// wildcard sub-pattern must bind nothing (and skip the field load), a nested
/// tuple must recurse.
[<Tests>]
let destructureTests =
    let invokeIntFn (source: string) (arg: int) : int =
        let _, artifact = compileSource "TupleStep4" source
        let asm = loadAssembly (Codegen.toBytes artifact)

        let m =
            asm.GetTypes()
            |> Array.collect (fun t ->
                t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
            )
            |> Array.find (fun m -> m.Name.StartsWith "fn$")

        m.Invoke(null, [| box arg |]) :?> int

    testList
        "Tuple representation — Step 4 (destructure a tuple)"
        [
            test "let a, b = (z, z + 1) binds both elements (a + b)" {
                let r = invokeIntFn "let f (z: int) = let a, b = (z, z + 1) in a + b" 5
                Expect.equal r 11 "5 + 6"
            }

            test "a nested let a, (b, c) recurses into the inner tuple" {
                let r =
                    invokeIntFn "let f (z: int) = let a, (b, c) = (z, (z + 1, z + 2)) in a + b + c" 1

                Expect.equal r 6 "1 + 2 + 3"
            }

            test "a wildcard tuple element binds nothing (let a, _ = …)" {
                let r = invokeIntFn "let f (z: int) = let a, _ = (z, z + 1) in a" 5
                Expect.equal r 5 "the second element is dropped"
            }

            test "match on a tuple binds the sub-patterns (n, m -> n + m)" {
                let r = invokeIntFn "let f (z: int) = match (z, z + 1) with (n, m) -> n + m" 5
                Expect.equal r 11 "5 + 6"
            }
        ]

/// Step 5 gate: a tuple lambda *parameter* (`fun (a, b) -> …`), the original
/// blocker. The lambda is emitted as a closure whose `Invoke` receives the
/// `ValueTuple`n` at `ldarg.1` and `bindPattern`s the element bindings out of it
/// before running the body. Reusing the Step-4 `invokeIntFn` harness (a one-arg
/// static fn returning int), each program builds such a closure and applies it.
[<Tests>]
let lambdaParamTests =
    let invokeIntFn (source: string) (arg: int) : int =
        let _, artifact = compileSource "TupleStep5" source
        let asm = loadAssembly (Codegen.toBytes artifact)

        let m =
            asm.GetTypes()
            |> Array.collect (fun t ->
                t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static)
            )
            |> Array.find (fun m -> m.Name.StartsWith "fn$")

        m.Invoke(null, [| box arg |]) :?> int

    testList
        "Tuple representation — Step 5 (tuple lambda parameter)"
        [
            test "a fun (a, b) -> a + b closure destructures its tuple param" {
                let r = invokeIntFn "let f (z: int) = let g = fun (a, b) -> a + b in g (z, z + 1)" 5

                Expect.equal r 11 "g(5, 6) = 11"
            }

            test "a tuple-param closure capturing an outer var keeps a/b as params, z as capture" {
                let r = invokeIntFn "let f (z: int) = let g = fun (a, b) -> a + b + z in g (1, 2)" 5

                Expect.equal r 8 "1 + 2 + 5 — z is captured, a/b are not"
            }

            test "a wildcard tuple param element binds nothing (fun (a, _) -> a)" {
                // The discarded element is annotated only to ground it (it is
                // touched by no operator); `a` grounds via `a + 0`. The point of
                // this case is the codegen wildcard arm (no `Item2` field load),
                // not inference.
                let r =
                    invokeIntFn "let f (z: int) = let g = fun (a, _: int) -> a + 0 in g (z, z + 1)" 5

                Expect.equal r 5 "the second element is dropped"
            }
        ]
