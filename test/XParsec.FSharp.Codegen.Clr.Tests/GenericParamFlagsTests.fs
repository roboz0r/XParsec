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
                    [ "T0", valueType ]
                    "struct ⇒ NotNullableValueType ||| DefaultConstructor"
            }

            // The `MapSeq<...>(source, f)` call instantiates `MapSeq`'s bounds over `map`'s own
            // typars, so `map` quantifies exactly its five.
            test "a module function bounded through a constrained nominal quantifies only its own typars" {
                let bytes =
                    Codegen.toBytes (compileSource "GpStructSeqMap" (dataSource "StructSeqMultiMapChain"))

                let rows = methodGenericParamsOf bytes "Program" "map"
                Expect.equal (List.length rows) 5 "map has five GenericParam rows"

                Expect.all
                    rows
                    (fun (_, attrs) -> attrs = GenericParameterAttributes.None)
                    "coercion bounds set no flag bits"
            }

            test "`not struct` on a module function is the reference-type bit" {
                let bytes =
                    bytesOf "GpNotStruct" [ "let onlyRef<'a when 'a: not struct> (x: 'a) = x"; "let s = onlyRef \"x\"" ]

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "onlyRef")
                    [ "T0", referenceType ]
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
                    [ "T0", defaultCtor ]
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
                    [ "T0", referenceType ||| defaultCtor; "T1", GenericParameterAttributes.None ]
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
                    [ "T0", GenericParameterAttributes.None ]
                    "equality"

                Expect.equal
                    (methodGenericParamsOf bytes "Program" "cmp")
                    [ "T0", GenericParameterAttributes.None ]
                    "comparison"
            }

            test "a type declaration's typar carries its bound" {
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

            test "a union's case classes carry the declaring typar's bound" {
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

            test "a member's own typar carries its bound" {
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

            test "an interface slot's own typar carries its bound" {
                let bytes =
                    bytesOf "GpInterface" [ "type I ="; "    abstract Only<'a when 'a: struct> : 'a -> 'a" ]

                Expect.equal (methodGenericParamsOf bytes "I" "Only") [ "a", valueType ] "interface slot typar"
            }
        ]
