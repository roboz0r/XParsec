module XParsec.FSharp.Codegen.Clr.Tests.GenericParamConstraintTests

open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.MetadataStructure

// The `GenericParamConstraint` rows of a `GenericParam` row, read back through
// `MetadataReader`. As `fsc` writes them, a coercion is the only constraint kind that adds
// a row: a bare nominal target is its `TypeDef` / `TypeRef`, an instantiation a `TypeSpec`
// whose sibling typars are the owner's own slots.

let private bytesOf (name: string) (lines: string list) : byte[] =
    Codegen.toBytes (compileSource name (String.concat "\n" lines))

// A primitive's BCL interfaces are not modelled by the front end, so a BCL target is
// instantiated at a user class.
let private shapeDecls =
    [
        "type IShape ="
        "    abstract Area: float"
        "type Square(s: float) ="
        "    interface IShape with"
        "        member _.Area = s * s"
        "type Ranked(s: float) ="
        "    interface IShape with"
        "        member _.Area = s"
        "    interface System.IComparable with"
        "        member _.CompareTo(o: obj) = 0"
    ]

[<Tests>]
let tests =
    testList
        "GenericParamConstraint rows"
        [
            test "a coercion to a user interface on a module function is a TypeDef row" {
                let bytes =
                    bytesOf
                        "GpcUserInterface"
                        (shapeDecls
                         @ [
                             "let keep<'a when 'a :> IShape> (x: 'a) = x"
                             "let s = keep (Square 2.0)"
                             "ignore s"
                         ])

                Expect.equal (methodGenericParamConstraintsOf bytes "Program" "keep") [ "a", [ "IShape" ] ] "one row"
            }

            test "a coercion to a BCL interface on a module function is a TypeRef row" {
                let bytes =
                    bytesOf
                        "GpcBclInterface"
                        (shapeDecls
                         @ [
                             "let keep<'a when 'a :> System.IComparable> (x: 'a) = x"
                             "let r = keep (Ranked 1.0)"
                             "ignore r"
                         ])

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "Program" "keep")
                    [ "a", [ "System.IComparable" ] ]
                    "one row"
            }

            test "a coercion to a generic interface over the typar itself is a TypeSpec row on the method slot" {
                let bytes =
                    bytesOf
                        "GpcSelfInstantiation"
                        [
                            "let keep<'a when 'a :> System.IComparable<'a>> (x: 'a) = x"
                            "let i = keep 1"
                            "ignore i"
                        ]

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "Program" "keep")
                    [ "a", [ "System.IComparable`1<!!0>" ] ]
                    "one row over `!!0`"
            }

            test "two coercions on one typar are two rows in constraint order" {
                let bytes =
                    bytesOf
                        "GpcTwoRows"
                        (shapeDecls
                         @ [
                             "let keep<'a when 'a :> IShape and 'a :> System.IComparable> (x: 'a) = x"
                             "let r = keep (Ranked 2.0)"
                             "ignore r"
                         ])

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "Program" "keep")
                    [ "a", [ "IShape"; "System.IComparable" ] ]
                    "two rows"
            }

            test "a type declaration's typar carries its coercion row" {
                let bytes =
                    bytesOf
                        "GpcTypeDecl"
                        (shapeDecls
                         @ [
                             "type Box<'a when 'a :> IShape> = { Item: 'a }"
                             "type Holder<'a when 'a :> IShape>(x: 'a) ="
                             "    member _.X = x"
                             "let b = { Item = Square 1.0 }"
                             "let h = Holder(Square 2.0)"
                             "ignore b"
                             "ignore h"
                         ])

                Expect.equal (typeGenericParamConstraintsOf bytes "Box`1") [ "a", [ "IShape" ] ] "record typar"
                Expect.equal (typeGenericParamConstraintsOf bytes "Holder`1") [ "a", [ "IShape" ] ] "class typar"
            }

            test "a member's own typar constrained over the declaring typar spells the type slot" {
                let bytes =
                    bytesOf
                        "GpcMemberOverDeclaring"
                        (shapeDecls
                         @ [
                             "type Holder<'a when 'a :> IShape>(x: 'a) ="
                             "    member _.Pair<'b when 'b :> System.IComparable<'a>>(y: 'b) = y"
                             "let h = Holder(Square 2.0)"
                             "ignore h"
                         ])

                Expect.equal (typeGenericParamConstraintsOf bytes "Holder`1") [ "a", [ "IShape" ] ] "type typar"

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "Holder`1" "Pair")
                    [ "b", [ "System.IComparable`1<!0>" ] ]
                    "member typar over `!0`"
            }

            test "an interface slot's own typar carries its coercion row" {
                let bytes =
                    bytesOf
                        "GpcInterfaceSlot"
                        (shapeDecls
                         @ [ "type I ="; "    abstract Only<'a when 'a :> IShape> : 'a -> 'a" ])

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "I" "Only")
                    [ "a", [ "IShape" ] ]
                    "interface slot typar"
            }

            // `map`'s source typars `'TFunc, 'S, 'E, 'T, 'U` emit as `M0`…`M4`: each coercion
            // target instantiates a generic interface over sibling method slots.
            test "a module function constrained through constrained nominals carries one row per coercion" {
                let bytes =
                    Codegen.toBytes (compileSource "GpcStructSeqMap" (dataSource "StructSeqMultiMapChain"))

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "Program" "map")
                    [
                        "M0", [ "Vesper.Fun`2<!!3, !!4>" ]
                        "M1", [ "IStructSeq`2<!!3, !!2>" ]
                        "M2", [ "IStructEnumerator`1<!!3>" ]
                        "M3", []
                        "M4", []
                    ]
                    "one row per coercion"
            }

            // The front end rejects every instantiation of `'a :> obj`, so the probe is the
            // declaration alone.
            test "a coercion to `obj` adds no row" {
                let bytes = bytesOf "GpcObjCoercion" [ "let keep<'a when 'a :> obj> (x: 'a) = x" ]

                Expect.equal (methodGenericParamConstraintsOf bytes "Program" "keep") [ "a", [] ] "no row"
            }

            test "`struct`, `new()`, `enum<_>`, `null`, `equality` and `comparison` add no row" {
                let bytes =
                    bytesOf
                        "GpcNoRows"
                        [
                            "type Colour ="
                            "    | Red = 1"
                            "    | Green = 2"
                            "type Counter() ="
                            "    member _.Count = 0"
                            "let onlyStruct<'a when 'a: struct> (x: 'a) = x"
                            "let onlyNew<'a when 'a: (new: unit -> 'a)> (x: 'a) = x"
                            "let onlyEnum<'a when 'a: enum<int>> (x: 'a) = x"
                            "let onlyNull<'a when 'a: null> (x: 'a) = x"
                            "let eq<'a when 'a: equality> (x: 'a) (y: 'a) = x = y"
                            "let cmp<'a when 'a: comparison> (x: 'a) (y: 'a) = x < y"
                            "let i = onlyStruct 42"
                            "let c = onlyNew (Counter())"
                            "let g = onlyEnum Colour.Green"
                            "let f (s: string | null) = onlyNull s"
                            "let a = eq 1 1"
                            "let b = cmp 1 2"
                            "ignore i"
                            "ignore c"
                            "ignore g"
                            "ignore f"
                            "ignore a"
                            "ignore b"
                        ]

                Expect.equal (genericParamConstraintRowCount bytes) 0 "no GenericParamConstraint row"

                Expect.equal
                    (methodGenericParamConstraintsOf bytes "Program" "onlyEnum")
                    [ "a", [] ]
                    "enum<_> has no System.Enum row, as fsc writes it"
            }
        ]
