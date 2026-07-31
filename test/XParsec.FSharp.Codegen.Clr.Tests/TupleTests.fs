module XParsec.FSharp.Codegen.Clr.Tests.TupleTests

// Tuple representation — every tuple, at every arity, is a `System.ValueTuple`n`
// (no `System.Tuple`). The testLists below cover: `ValueTuple`n` family
// resolution (`ClrProvider`/`ClrEncoder`); encoding a bare `FTTuple` to a generic
// instantiation TypeSpec (pure metadata); emit + reflect a constructed tuple
// value (behavioural); destructuring; and tuple lambda parameters.

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

/// A fresh provider over an empty metadata context — enough to mint TypeRef /
/// TypeSpec / MemberRef handles without emitting a full assembly. The own-file
/// intrinsic forward map carries the `int`/`string` → IL-repr the element encoding
/// needs (the two primitives these tests exercise); `nullProvider` because no
/// external-symbol resolution is involved.
let private ownIntrinsics =
    System.Collections.Generic.Dictionary(
        dict [ RuntimeNames.intKey, "System.Int32"; RuntimeNames.stringKey, "System.String" ]
    )

let private provider () =
    ClrProvider(MetadataContext(), ownIntrinsics, Map.empty, ExternalSymbolProviders.nullProvider)

let private ftConst (name: string) =
    FTConst(RuntimeNames.primitiveKey name, EqArray.empty)

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

            test "arity 8 resolves: 7 direct Item fields + a single-element Rest nesting" {
                let p = provider ()
                let refs = p.ValueTupleRefs(List.replicate 8 (ftConst "int"))
                Expect.equal refs.ItemFields.Length 7 "arity-8 stores 7 elements directly (slots 0–6)"
                Expect.isTrue refs.Rest.IsSome "arity-8 carries a TRest nesting"
                let rest = refs.Rest.Value
                Expect.isFalse rest.RestField.IsNil "Rest field handle is nil"
                Expect.equal rest.Nested.ItemFields.Length 1 "the nested tail holds the 8th element"
                Expect.isTrue rest.Nested.Rest.IsNone "a 1-element tail does not nest further"
            }

            test "arity 15 double-nests (Rest of Rest): 7 + 7 + 1" {
                let p = provider ()
                let refs = p.ValueTupleRefs(List.replicate 15 (ftConst "int"))
                Expect.equal refs.ItemFields.Length 7 "level 1 stores the first 7 directly"
                let lvl2 = refs.Rest.Value.Nested
                Expect.equal lvl2.ItemFields.Length 7 "level 2 (first Rest) stores the next 7"
                Expect.isTrue lvl2.Rest.IsSome "15 elements need a second Rest"
                let lvl3 = lvl2.Rest.Value.Nested
                Expect.equal lvl3.ItemFields.Length 1 "the 15th element lands in the double-nested tail"
                Expect.isTrue lvl3.Rest.IsNone "no third level"
            }
        ]

/// A bare `FTTuple` encodes to a `ValueTuple`n` generic-instantiation TypeSpec
/// via `encodeType`. Pure metadata-resolution checks — the encoder mints a
/// non-nil spec for each in-range arity.
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

/// A standalone `TExprG.Tuple` is built into a value (`newobj ValueTuple`n::.ctor`)
/// rather than flattened as an argument list. A top-level function returning a
/// tuple is emitted as a static method; invoking it by reflection yields a live
/// `System.ValueTuple`n` whose `Item` fields hold the constructed elements.
[<Tests>]
let constructTests =
    /// The single top-level static method an emitted bare program carries.
    let theStaticFn (bytes: byte[]) : MethodInfo =
        programHolderMethods bytes |> Array.head

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

            test "an 8-tuple constructs as ValueTuple`8 whose Rest holds the 8th element" {
                let _, artifact =
                    compileSource
                        "TupleStep3c"
                        "let octet (n: int) = (n, n + 1, n + 2, n + 3, n + 4, n + 5, n + 6, n + 7)"

                let m = theStaticFn (Codegen.toBytes artifact)
                let result = m.Invoke(null, [| box 10 |])
                let ty = result.GetType()

                Expect.isTrue
                    (ty.FullName.StartsWith "System.ValueTuple`8")
                    (sprintf "expected a ValueTuple`8 but got %s" ty.FullName)

                // Slots 0–6 are the direct `Item1..Item7`; the 8th value rides the
                // nested `Rest` (a `ValueTuple`1`), reachable as `Rest.Item1`.
                Expect.equal (ty.GetField("Item1").GetValue result :?> int) 10 "Item1"
                Expect.equal (ty.GetField("Item7").GetValue result :?> int) 16 "Item7"
                let rest = ty.GetField("Rest").GetValue result
                Expect.equal (rest.GetType().GetField("Item1").GetValue rest :?> int) 17 "Rest.Item1 (8th element)"
            }
        ]

/// Tuple *destructuring* through the shared irrefutable `bindPattern`
/// (let / for-in) and the match compiler's `TPatG.Tuple` arm. Each program is a
/// one-arg static function returning an int; reflecting the invoke result proves
/// the leaf bindings were pulled out of the `ValueTuple`n` `Item` fields. A
/// wildcard sub-pattern must bind nothing (skip the field load); a nested tuple
/// must recurse.
[<Tests>]
let destructureTests =
    let invokeIntFn (source: string) (arg: int) : int =
        let _, artifact = compileSource "TupleStep4" source
        let m = programHolderMethods (Codegen.toBytes artifact) |> Array.head

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

            // Round-trip through `TRest` nesting: construct an 8-tuple, then bind
            // all eight elements out of it — the destructure of index 7 must chase
            // the `Rest` field, not read a (nonexistent) `Item8`.
            test "let a..h = an 8-tuple binds all eight (index 7 chases Rest)" {
                let r =
                    invokeIntFn
                        "let f (z: int) = let a, b, c, d, e, g, h, i = (z, z+1, z+2, z+3, z+4, z+5, z+6, z+7) in a+b+c+d+e+g+h+i"
                        1

                Expect.equal r 36 "8*1 + (0+1+..+7) = 8 + 28"
            }

            // Double nesting (Rest-of-Rest): a 15-tuple is `ValueTuple`8<…,
            // ValueTuple`8<…, ValueTuple`1<…>>>`, so binding the 15th element walks
            // two `Rest` hops then a final `Item1`.
            test "let of a 15-tuple binds all fifteen (double Rest nesting)" {
                let r =
                    invokeIntFn
                        "let f (z: int) = let a,b,c,d,e,g,h,i,j,k,l,m,n,o,p = (z,z+1,z+2,z+3,z+4,z+5,z+6,z+7,z+8,z+9,z+10,z+11,z+12,z+13,z+14) in a+b+c+d+e+g+h+i+j+k+l+m+n+o+p"
                        1

                Expect.equal r 120 "15*1 + (0+1+..+14) = 15 + 105"
            }
        ]

/// A tuple lambda *parameter* (`fun (a, b) -> …`). The lambda is emitted as a
/// closure whose `Invoke` receives the `ValueTuple`n` at `ldarg.1` and
/// `bindPattern`s the element bindings out of it before running the body.
[<Tests>]
let lambdaParamTests =
    let invokeIntFn (source: string) (arg: int) : int =
        let _, artifact = compileSource "TupleStep5" source
        let m = programHolderMethods (Codegen.toBytes artifact) |> Array.head

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

            // An 8-tuple lambda parameter: the closure's `Invoke` receives the
            // `ValueTuple`8` and `bindPattern`s all eight elements, chasing `Rest`
            // for the 8th.
            test "a fun (a..h) -> closure destructures an 8-tuple param (Rest-chased)" {
                let r =
                    invokeIntFn
                        "let f (z: int) = let g = fun (a, b, c, d, e, x, y, w) -> a+b+c+d+e+x+y+w in g (z, z+1, z+2, z+3, z+4, z+5, z+6, z+7)"
                        1

                Expect.equal r 36 "g over an 8-tuple = 8 + 28"
            }
        ]
