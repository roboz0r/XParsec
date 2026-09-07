module XParsec.FSharp.Codegen.Clr.Tests.TupleTests

// Every tuple, at every arity, is a `System.ValueTuple`n`; `System.Tuple` is never
// emitted.

open System.Reflection
open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection
open XParsec.FSharp.Codegen.Clr.Tests.ReflectionHarness

/// `int` → `System.Int32` and `string` → `System.String`: the element reprs the
/// tuple encodings below need, with no assembly emitted around them.
let private ownIntrinsics =
    System.Collections.Generic.Dictionary(
        dict
            [
                RuntimeNames.intKey, PlatformTypeId "System.Int32"
                RuntimeNames.stringKey, PlatformTypeId "System.String"
            ]
    )

let private provider () =
    let symbols = CodegenSymbols.ofProvider ExternalSymbolProviders.nullProvider
    ClrProvider(MetadataContext(), ownIntrinsics, Map.empty, symbols)

let private ftConst (name: string) =
    FTConst(RuntimeNames.primitiveKey name, Block.empty)

[<Tests>]
let tests =
    testList
        "Tuple representation: ValueTuple resolution"
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

            test "arity 8 resolves: 7 direct Item fields + a single-element Rest nesting" {
                let p = provider ()
                let refs = p.ValueTupleRefs(List.replicate 8 (ftConst "int"))
                Expect.equal refs.ItemFields.Length 7 "arity-8 stores 7 elements directly (slots 0–6)"
                Expect.isTrue refs.Rest.IsSome "arity-8 carries a TRest nesting"
                let rest = refs.Rest.Value
                Expect.isFalse rest.RestField.IsNil "Rest field handle is nil"
                Expect.equal rest.Nested.ItemFields.Length 1 "the nested Rest holds the 8th element"
                Expect.isTrue rest.Nested.Rest.IsNone "a 1-element Rest does not nest further"
            }

            test "arity 15 double-nests (Rest of Rest): 7 + 7 + 1" {
                let p = provider ()
                let refs = p.ValueTupleRefs(List.replicate 15 (ftConst "int"))
                Expect.equal refs.ItemFields.Length 7 "level 1 stores the first 7 directly"
                let lvl2 = refs.Rest.Value.Nested
                Expect.equal lvl2.ItemFields.Length 7 "level 2 (first Rest) stores the next 7"
                Expect.isTrue lvl2.Rest.IsSome "15 elements need a second Rest"
                let lvl3 = lvl2.Rest.Value.Nested
                Expect.equal lvl3.ItemFields.Length 1 "the 15th element lands in the double-nested Rest"
                Expect.isTrue lvl3.Rest.IsNone "no third level"
            }
        ]

/// A bare `FTTuple` encodes to a `ValueTuple`n` generic-instantiation TypeSpec.
[<Tests>]
let encodeTests =
    let ftTuple (tys: FrozenType list) = FTTuple(Block.ofList tys)

    testList
        "Tuple representation: FTTuple encoding"
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

            test "arity 8 tuple encodes to a non-nil TypeSpec (ValueTuple`8 with a nested TRest)" {
                let p = provider ()
                let spec = p.TypeSpecOf(ftTuple (List.replicate 8 (ftConst "int")))
                Expect.isFalse spec.IsNil "arity-8 tuple TypeSpec handle is nil"
            }

            test "arity 15 tuple encodes (double TRest nesting)" {
                let p = provider ()
                let spec = p.TypeSpecOf(ftTuple (List.replicate 15 (ftConst "int")))
                Expect.isFalse spec.IsNil "arity-15 tuple TypeSpec handle is nil"
            }
        ]

/// A standalone tuple expression is built into a value (`newobj ValueTuple`n::.ctor`)
/// rather than flattened into an argument list.
[<Tests>]
let constructTests =
    /// The single top-level static method an emitted bare program carries.
    let theStaticFn (bytes: byte[]) : MethodInfo = programClassMethods bytes |> Array.head

    testList
        "Tuple representation: construct a tuple value"
        [
            test "a function returning (n, n+1) yields a ValueTuple`2 with the constructed fields" {
                let artifact = compileSource "TupleStep3" "let pair (n: int) = (n, n + 1)"
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
                let artifact = compileSource "TupleStep3b" "let tag (n: int) = (n, \"x\")"
                let m = theStaticFn (Codegen.toBytes artifact)
                let result = m.Invoke(null, [| box 7 |])

                let ty = result.GetType()

                Expect.isTrue
                    (ty.FullName.StartsWith "System.ValueTuple`2")
                    (sprintf "expected a ValueTuple`2 but got %s" ty.FullName)

                Expect.equal (ty.GetField("Item1").GetValue result :?> int) 7 "Item1"
                Expect.equal (ty.GetField("Item2").GetValue result :?> string) "x" "Item2"
            }

            test "an 8-tuple constructs as ValueTuple`8 whose Rest holds the 8th element" {
                let artifact =
                    compileSource
                        "TupleStep3c"
                        "let octet (n: int) = (n, n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7)"

                let m = theStaticFn (Codegen.toBytes artifact)
                let result = m.Invoke(null, [| box 10 |])
                let ty = result.GetType()

                Expect.isTrue
                    (ty.FullName.StartsWith "System.ValueTuple`8")
                    (sprintf "expected a ValueTuple`8 but got %s" ty.FullName)

                Expect.equal (ty.GetField("Item1").GetValue result :?> int) 10 "Item1"
                Expect.equal (ty.GetField("Item7").GetValue result :?> int) 16 "Item7"
                let rest = ty.GetField("Rest").GetValue result
                Expect.equal (rest.GetType().GetField("Item1").GetValue rest :?> int) 17 "Rest.Item1 (8th element)"
            }
        ]

/// Tuple destructuring, through both the irrefutable `let` path and the match
/// compiler. Each program is `let f (z: int) = …` returning an int, so the invoke
/// result is the sum of whatever the pattern's bound variables took.
[<Tests>]
let destructureTests =
    let invokeIntFn = invokeIntFn "TupleStep4" "f"

    testList
        "Tuple representation: destructure a tuple"
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

            // There is no `Item8`: index 7 has to be read through the `Rest` field.
            test "let a..h = an 8-tuple binds all eight (index 7 chases Rest)" {
                let r =
                    invokeIntFn
                        "let f (z: int) = let a, b, c, d, e, g, h, i = (z, z+1, z+2, z+3, z+4, z+5, z+6, z+7) in a+b+c+d+e+g+h+i"
                        1

                Expect.equal r 36 "8*1 + (0+1+..+7) = 8 + 28"
            }

            // A 15-tuple is `ValueTuple`8<…, ValueTuple`8<…, ValueTuple`1<…>>>`, so the
            // 15th element takes two `Rest` hops then an `Item1`.
            test "let of a 15-tuple binds all fifteen (double Rest nesting)" {
                let r =
                    invokeIntFn
                        "let f (z: int) = let a,b,c,d,e,g,h,i,j,k,l,m,n,o,p = (z,z+1,z+2,z+3,z+4,z+5,z+6,z+7,z+8,z+9,z+10,z+11,z+12,z+13,z+14) in a+b+c+d+e+g+h+i+j+k+l+m+n+o+p"
                        1

                Expect.equal r 120 "15*1 + (0+1+..+14) = 15 + 105"
            }
        ]

/// `fun (a, b) -> …`: the closure's `Invoke` takes ONE argument, the `ValueTuple`n`,
/// and binds the elements out of it before running the body. Each program is
/// `let f (z: int) = …` returning an int.
[<Tests>]
let lambdaParamTests =
    let invokeIntFn = invokeIntFn "TupleStep5" "f"

    testList
        "Tuple representation: tuple lambda parameter"
        [
            test "a fun (a, b) -> a + b closure destructures its tuple param" {
                let r = invokeIntFn "let f (z: int) = let g = fun (a, b) -> a + b in g (z, z + 1)" 5

                Expect.equal r 11 "g(5, 6) = 11"
            }

            test "a tuple-param closure capturing an outer var keeps a/b as params, z as capture" {
                let r = invokeIntFn "let f (z: int) = let g = fun (a, b) -> a + b + z in g (1, 2)" 5

                Expect.equal r 8 "1 + 2 + 5 = 8, because z is captured while a/b stay params"
            }

            test "a wildcard tuple param element binds nothing (fun (a, _) -> a)" {
                // The `: int` and the `+ 0` only ground the two element types; the case
                // under test is the wildcard arm skipping the `Item2` load.
                let r =
                    invokeIntFn "let f (z: int) = let g = fun (a, _: int) -> a + 0 in g (z, z + 1)" 5

                Expect.equal r 5 "the second element is dropped"
            }

            test "a fun (a..h) -> closure destructures an 8-tuple param (Rest-chased)" {
                let r =
                    invokeIntFn
                        "let f (z: int) = let g = fun (a, b, c, d, e, x, y, w) -> a+b+c+d+e+x+y+w in g (z, z+1, z+2, z+3, z+4, z+5, z+6, z+7)"
                        1

                Expect.equal r 36 "g over an 8-tuple = 8 + 28"
            }

            test "the member a tuple is CLASSIFIED as is the member it is EMITTED as" {
                // The layout verdict keys off `typeKey`, the `TypeRef` off `memberName`. Drift
                // between them would classify a tuple as something else than it emits.
                for arity in 2..12 do
                    Expect.equal
                        (SymbolKeyOps.typeMetaName (ClrTuples.typeKey arity))
                        (ClrTuples.Namespace + "." + ClrTuples.memberName (ClrTuples.memberArity arity))
                        (sprintf "arity %d" arity)
            }

            test "every classified ValueTuple member resolves, as a value type" {
                // The layout verdict is a metadata lookup, so a member that did not resolve would
                // yield `Unsettled` and silently classify a tuple as a reference.
                let facts =
                    match MetadataSymbols.provider.Platform with
                    | ValueSome f -> f
                    | ValueNone -> failtest "the metadata provider IS the platform"

                for arity in 2..12 do
                    match facts.TupleType arity with
                    | ValueSome key -> Expect.equal (facts.IsValueType key) (ValueSome true) (sprintf "arity %d" arity)
                    | ValueNone -> failtestf "arity %d reached no tuple type" arity

                Expect.equal (facts.TupleType 1) ValueNone "a 1-tuple is not a tuple value"
            }
        ]
