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

            test "a struct union with a payload case is Tagged at any case count" {
                Expect.equal (ofStruct 2 true) UnionRegime.Tagged "two cases"
                Expect.equal (ofStruct 3 true) UnionRegime.Tagged "three cases"
                Expect.equal (ofStruct 4 true) UnionRegime.Tagged "four cases"
            }

            test "a struct union classifies like a reference one where no type test is involved" {
                Expect.equal (ofStruct 1 true) UnionRegime.SingleCase "one case"
                Expect.equal (ofStruct 3 false) UnionRegime.EnumLike "all nullary"
            }
        ]
