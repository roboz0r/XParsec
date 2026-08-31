module XParsec.FSharp.Codegen.Clr.Tests.UnionRegimeTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr

let private ofRef (caseCount: int) (anyCaseCarriesFields: bool) =
    UnionRegime.classify UnionValueKind.RefType caseCount anyCaseCarriesFields

let private ofStruct (caseCount: int) (anyCaseCarriesFields: bool) =
    UnionRegime.classify UnionValueKind.Struct caseCount anyCaseCarriesFields

[<Tests>]
let tests =
    testList
        "UnionRegime.classify"
        [
            test "one case is SingleCase whether or not it carries fields" {
                Expect.equal (ofRef 1 true) UnionRegime.SingleCase "one payload case"
                Expect.equal (ofRef 1 false) UnionRegime.SingleCase "one nullary case"
            }

            test "two or more cases, all nullary, are EnumLike" {
                Expect.equal (ofRef 2 false) UnionRegime.EnumLike "two"
                Expect.equal (ofRef 7 false) UnionRegime.EnumLike "seven"
            }

            test "a nullary case count past the type-test limit stays EnumLike" {
                Expect.equal (ofRef (UnionRegime.TypeTestCaseLimit + 1) false) UnionRegime.EnumLike "four nullary cases"
            }

            test "two or three cases with a payload are TypeTested" {
                Expect.equal (ofRef 2 true) UnionRegime.TypeTested "two cases"
                Expect.equal (ofRef 3 true) UnionRegime.TypeTested "three cases"
            }

            test "the case past the type-test limit flips a payload-carrying union to Tagged" {
                Expect.equal (ofRef UnionRegime.TypeTestCaseLimit true) UnionRegime.TypeTested "at the limit"
                Expect.equal (ofRef (UnionRegime.TypeTestCaseLimit + 1) true) UnionRegime.Tagged "one past it"
            }

            // A value type can neither inherit nor be abstract, so the case count buys it
            // nothing: `StructTagged` is the one regime a payload-carrying struct union
            // reaches, and it is unreachable for a reference union.
            test "a struct union with a payload case is StructTagged at any case count" {
                Expect.equal (ofStruct 2 true) UnionRegime.StructTagged "two cases"
                Expect.equal (ofStruct 3 true) UnionRegime.StructTagged "three cases"
                Expect.equal (ofStruct 4 true) UnionRegime.StructTagged "four cases"
            }

            test "a struct union classifies like a reference one where no type test is involved" {
                Expect.equal (ofStruct 1 true) UnionRegime.SingleCase "one case"
                Expect.equal (ofStruct 3 false) UnionRegime.EnumLike "all nullary"
            }
        ]

[<Tests>]
let hierarchy =
    testList
        "UnionRegime.isHierarchy"
        [
            test "the payload-carrying reference regimes nest a type per case" {
                Expect.isTrue (UnionRegime.isHierarchy UnionRegime.TypeTested) "TypeTested"
                Expect.isTrue (UnionRegime.isHierarchy UnionRegime.Tagged) "Tagged"
            }

            // The value kind is settled by the time a regime exists, so `classify` alone
            // decides this and a struct union in a hierarchy regime is unrepresentable.
            test "the remaining regimes are flat" {
                Expect.isFalse (UnionRegime.isHierarchy UnionRegime.SingleCase) "SingleCase"
                Expect.isFalse (UnionRegime.isHierarchy UnionRegime.EnumLike) "EnumLike"
                Expect.isFalse (UnionRegime.isHierarchy UnionRegime.StructTagged) "StructTagged"
            }

            test "a case's fields take their own type's names exactly where they have one" {
                Expect.isTrue (UnionCaseFields.ownType UnionRegime.TypeTested) "a hierarchy case owns its type"

                Expect.isTrue
                    (UnionCaseFields.ownType UnionRegime.SingleCase)
                    "the sole case of a single-case union owns the union itself"

                Expect.isFalse
                    (UnionCaseFields.ownType UnionRegime.StructTagged)
                    "a struct union holds every case's fields co-resident"
            }
        ]

[<Tests>]
let tagPredicates =
    testList
        "UnionRegime tag predicates"
        [
            test "SingleCase alone declares no _tag row" {
                Expect.isFalse (UnionRegime.hasTagRow UnionRegime.SingleCase) "SingleCase"

                for regime in
                    [
                        UnionRegime.EnumLike
                        UnionRegime.StructTagged
                        UnionRegime.TypeTested
                        UnionRegime.Tagged
                    ] do
                    Expect.isTrue (UnionRegime.hasTagRow regime) (sprintf "%A" regime)
            }

            // `TypeTested` is the one regime where the two predicates split: its `_tag`
            // row survives (until step 4 of the hierarchy plan) but no consumer loads it.
            test "TypeTested keeps its row while no consumer reads it" {
                Expect.isTrue (UnionRegime.hasTagRow UnionRegime.TypeTested) "the row"
                Expect.isFalse (UnionRegime.readsTag UnionRegime.TypeTested) "the read"
            }

            test "the flat multi-case regimes and Tagged read the tag" {
                Expect.isFalse (UnionRegime.readsTag UnionRegime.SingleCase) "SingleCase"

                for regime in [ UnionRegime.EnumLike; UnionRegime.StructTagged; UnionRegime.Tagged ] do
                    Expect.isTrue (UnionRegime.readsTag regime) (sprintf "%A" regime)
            }
        ]

[<Tests>]
let caseFieldNames =
    testList
        "UnionCaseFields.names"
        [
            // FSC's spelling, which is what would let the external path read an
            // FSC-emitted union: a declared name prefixed `_`, a lone positional field
            // `item`, and otherwise the field's 1-based position IN THE CASE.
            test "a case with a type of its own takes FSC's spelling" {
                Expect.equal
                    (UnionCaseFields.names UnionRegime.TypeTested "C" [ ValueNone ])
                    [ "item" ]
                    "a lone positional field"

                Expect.equal
                    (UnionCaseFields.names UnionRegime.TypeTested "C" [ ValueNone; ValueNone ])
                    [ "item1"; "item2" ]
                    "two positional fields"

                Expect.equal
                    (UnionCaseFields.names UnionRegime.TypeTested "C" [ ValueSome "radius" ])
                    [ "_radius" ]
                    "a declared name"

                Expect.equal
                    (UnionCaseFields.names UnionRegime.TypeTested "M1" [ ValueSome "tag"; ValueNone ])
                    [ "_tag"; "item2" ]
                    "the index counts every field, not the positional ones alone"
            }

            test "co-resident fields stay qualified by their case" {
                Expect.equal
                    (UnionCaseFields.names UnionRegime.StructTagged "Pair" [ ValueNone; ValueNone ])
                    [ "Pair_0"; "Pair_1" ]
                    "two cases' positional fields would otherwise collide"

                Expect.equal
                    (UnionCaseFields.names UnionRegime.StructTagged "Val" [ ValueSome "v" ])
                    [ "Val_0" ]
                    "a declared name is qualified too"
            }
        ]

[<Tests>]
let caseTest =
    testList
        "UnionCaseTest.ofRegime"
        [
            // Each thunk mints its handle in the caller's scope, so a regime must force
            // only the thunk it reads — a wrongly forced thunk here is the failure.
            let noMint what () : System.Reflection.Metadata.EntityHandle =
                failwithf "the regime forced the %s thunk" what

            test "the tag-reading regimes compare the minted tag ref against the case's tag" {
                let minted = System.Reflection.Metadata.EntityHandle()

                for regime in [ UnionRegime.EnumLike; UnionRegime.StructTagged; UnionRegime.Tagged ] do
                    Expect.equal
                        (UnionCaseTest.ofRegime regime 2 (fun () -> minted) (noMint "case-type"))
                        (UnionCaseTest.TagEquals(minted, 2))
                        (sprintf "%A reads the tag" regime)
            }

            test "TypeTested tests the minted case type and never mints a tag ref" {
                let minted = System.Reflection.Metadata.EntityHandle()

                Expect.equal
                    (UnionCaseTest.ofRegime UnionRegime.TypeTested 1 (noMint "tag-ref") (fun () -> minted))
                    (UnionCaseTest.IsInst minted)
                    "the case's runtime type discriminates"
            }

            test "SingleCase is irrefutable and mints nothing" {
                Expect.equal
                    (UnionCaseTest.ofRegime UnionRegime.SingleCase 0 (noMint "tag-ref") (noMint "case-type"))
                    UnionCaseTest.Irrefutable
                    "the sole case needs no test"
            }
        ]
