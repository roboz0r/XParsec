module XParsec.FSharp.Codegen.Clr.Tests.GenericParamFlagsTests

open System.Reflection
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

// The flag bits of a `GenericParam` row, one owner kind per test: a module function, a
// type declaration, a member's own typar and an interface slot's. Equality, comparison
// and nullness have no CLI encoding and leave the row at `None`.

let private bytesOf (name: string) (lines: string list) : byte[] =
    Codegen.toBytes (compileSource name (String.concat "\n" lines))

let private valueType =
    GenericParameterAttributes.NotNullableValueTypeConstraint
    ||| GenericParameterAttributes.DefaultConstructorConstraint

let private referenceType = GenericParameterAttributes.ReferenceTypeConstraint

let private defaultCtor = GenericParameterAttributes.DefaultConstructorConstraint

[<Tests>]
let tests =
    testList
        "GenericParam flags"
        [
            test "`struct` on a module function is the value-type pair" {
                let bytes =
                    bytesOf "GpStruct" [ "let onlyStruct<'a when 'a: struct> (x: 'a) = x"; "let i = onlyStruct 42" ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "onlyStruct")
                    [ "a", valueType ]
                    "struct ⇒ NotNullableValueType ||| DefaultConstructor"
            }

            // The `MapSeq<...>(source, f)` call instantiates `MapSeq`'s constraints over `map`'s own
            // typars, so `map` quantifies exactly its five.
            test "a module function constrained through a constrained nominal quantifies only its own typars" {
                let bytes =
                    Codegen.toBytes (compileSource "GpStructSeqMap" (dataSource "StructSeqMultiMapChain"))

                let rows = methodGenericParamsOf bytes "Program" "map"
                Expect.equal (List.length rows) 5 "map has five GenericParam rows"

                Expect.all
                    rows
                    (fun (_, attrs) -> attrs = GenericParameterAttributes.None)
                    "coercion constraints set no flag bits"
            }

            // A generic module VALUE lowers to a zero-arg generic static method, and its
            // scheme reaches that method's rows like a function's.
            test "`not struct` on a generic module value is the reference-type bit" {
                let bytes =
                    bytesOf
                        "GpValueNotStruct"
                        [
                            "let nothing<'a when 'a: not struct> : 'a list = []"
                            "let s : string list = nothing"
                            "ignore s"
                        ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "nothing")
                    [ "a", referenceType ]
                    "not struct ⇒ ReferenceType"
            }

            test "`not struct` on a module function is the reference-type bit" {
                let bytes =
                    bytesOf "GpNotStruct" [ "let onlyRef<'a when 'a: not struct> (x: 'a) = x"; "let s = onlyRef \"x\"" ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "onlyRef")
                    [ "a", referenceType ]
                    "not struct ⇒ ReferenceType"
            }

            test "`new()` on a module function is the default-constructor bit" {
                let bytes =
                    bytesOf
                        "GpNew"
                        [
                            "type Counter() ="
                            "    member _.Count = 0"
                            "let construct<'a when 'a: (new: unit -> 'a)> (x: 'a) = x"
                            "let c = construct (Counter())"
                        ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "construct")
                    [ "a", defaultCtor ]
                    "new() ⇒ DefaultConstructor"
            }

            test "`not struct` and `new()` on one typar combine, and a second typar is untouched" {
                let bytes =
                    bytesOf
                        "GpCombined"
                        [
                            "type Counter() ="
                            "    member _.Count = 0"
                            "let both<'a, 'b when 'a: not struct and 'a: (new: unit -> 'a)> (x: 'a) (y: 'b) = y"
                            "let r = both (Counter()) 1"
                        ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "both")
                    [ "a", referenceType ||| defaultCtor; "b", GenericParameterAttributes.None ]
                    "flags are per typar"
            }

            test "`equality` and `comparison` have no flag bits" {
                let bytes =
                    bytesOf
                        "GpEquality"
                        [
                            "let eq<'a when 'a: equality> (x: 'a) (y: 'a) = x = y"
                            "let cmp<'a when 'a: comparison> (x: 'a) (y: 'a) = x < y"
                            "let a = eq 1 1"
                            "let b = cmp 1 2"
                        ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "eq")
                    [ "a", GenericParameterAttributes.None ]
                    "equality"

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "cmp")
                    [ "a", GenericParameterAttributes.None ]
                    "comparison"
            }

            test "a type declaration's typar carries its constraint" {
                let bytes =
                    bytesOf
                        "GpTypeDecl"
                        [
                            "type Box<'a when 'a: struct> = { Item: 'a }"
                            "type Ref<'a when 'a: not struct>(x: 'a) ="
                            "    member _.X = x"
                            "let b = { Item = 1 }"
                            "let r = Ref(\"x\")"
                        ]

                Expect.equal (typeGenericParamsOf bytes "Box`1") [ "a", valueType ] "record typar"
                Expect.equal (typeGenericParamsOf bytes "Ref`1") [ "a", referenceType ] "class typar"
            }

            // The CLR erases a measure-kinded typar: only the type-kinded ones get a row, and
            // the metadata name counts those alone, whether or not `'u` appears in a field.
            test "a measure-kinded typar has no GenericParam row" {
                let bytes =
                    bytesOf
                        "GpMeasure"
                        [
                            "type Pair<[<Measure>] 'u, 'a when 'a: struct> = { V: 'a; W: float<'u> }"
                            "let n = 1"
                            "ignore n"
                        ]

                Expect.equal (typeGenericParamsOf bytes "Pair`1") [ "a", valueType ] "one row, the type-kinded typar's"
            }

            test "a union's case classes carry the declaring typar's constraint" {
                let bytes =
                    bytesOf
                        "GpUnion"
                        [
                            "type Shape<'a when 'a: struct> ="
                            "    | Leaf of 'a"
                            "    | Empty"
                            "let s = Leaf 1"
                        ]

                Expect.equal (typeGenericParamsOf bytes "Shape`1") [ "a", valueType ] "union typar"

                for nested in emittedTypes bytes |> List.filter (fun t -> t.Name.StartsWith "Shape`1+") do
                    match typeGenericParamsOf bytes nested.Name with
                    | [] -> ()
                    | rows -> Expect.equal rows [ "a", valueType ] (sprintf "%s typar" nested.Name)
            }

            test "a member's own typar carries its constraint" {
                let bytes =
                    bytesOf
                        "GpMember"
                        [
                            "type C() ="
                            "    member _.Only<'a when 'a: struct>(x: 'a) = x"
                            "let c = C()"
                            "let v = c.Only 1"
                        ]

                Expect.equal (methodGenericParamsOf bytes "C" "Only") [ "a", valueType ] "member typar"
            }

            test "an interface slot's own typar carries its constraint" {
                let bytes =
                    bytesOf "GpInterface" [ "type I ="; "    abstract Only<'a when 'a: struct> : 'a -> 'a" ]

                Expect.equal (methodGenericParamsOf bytes "I" "Only") [ "a", valueType ] "interface slot typar"
            }
        ]
