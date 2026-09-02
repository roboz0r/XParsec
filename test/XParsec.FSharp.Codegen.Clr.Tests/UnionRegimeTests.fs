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
let tagPredicate =
    testList
        "UnionRegime.hasTag"
        [
            // The two regimes that settle a case without a discriminant: one shape, and
            // one nested type per case.
            test "SingleCase and TypeTested carry no _tag" {
                Expect.isFalse (UnionRegime.hasTag UnionRegime.SingleCase) "SingleCase"
                Expect.isFalse (UnionRegime.hasTag UnionRegime.TypeTested) "TypeTested"
            }

            test "the flat multi-case regimes and Tagged carry one" {
                for regime in [ UnionRegime.EnumLike; UnionRegime.StructTagged; UnionRegime.Tagged ] do
                    Expect.isTrue (UnionRegime.hasTag regime) (sprintf "%A" regime)
            }
        ]

[<Tests>]
let ctorShape =
    testList
        "UnionCtorShape.ofRegime"
        [
            // A struct union's factories each build the `Payload` and `newobj` the whole
            // value, so its `.ctor` takes `_tag` and `_payload`.
            test "a StructTagged union's ctor takes the tag and the payload" {
                Expect.equal (UnionCtorShape.ofRegime UnionRegime.StructTagged) UnionCtorShape.FlatTagged "StructTagged"
            }

            // A union declaring a `_tag` and no payload takes the tag as its sole ctor
            // parameter, written once: a `Tagged` case `.ctor` chains it with the case's
            // discriminant, an enum-like struct factory stamps it, and an enum-like
            // reference `.cctor` passes it per singleton.
            test "a tagged union without a payload takes the tag alone" {
                for regime in [ UnionRegime.EnumLike; UnionRegime.Tagged ] do
                    Expect.equal (UnionCtorShape.ofRegime regime) UnionCtorShape.TagOnly (sprintf "%A" regime)
            }

            // A single case IS every case whichever way the union is stored, so the one
            // `.ctor` takes the payload and nothing stores after construction. That is
            // what leaves every union field `initonly`.
            test "a single-case union's ctor is flat" {
                Expect.equal (UnionCtorShape.ofRegime UnionRegime.SingleCase) UnionCtorShape.Flat "SingleCase"
            }

            // The `TypeTested` base declares no field at all, so nothing is left to pass.
            test "a TypeTested base alone is nullary" {
                Expect.equal (UnionCtorShape.ofRegime UnionRegime.TypeTested) UnionCtorShape.Nullary "TypeTested"
            }
        ]

[<Tests>]
let factoryShape =
    testList
        "UnionFactoryShape.ofCase"
        [
            // A value type caches nothing, so arity never redirects a struct union's
            // factory: it always `newobj`s the union's own flat `.ctor`.
            test "a struct union's factory always constructs" {
                Expect.equal
                    (UnionFactoryShape.ofCase UnionValueKind.Struct UnionRegime.StructTagged 2)
                    UnionFactoryShape.StructTagged
                    "StructTagged"

                Expect.equal
                    (UnionFactoryShape.ofCase UnionValueKind.Struct UnionRegime.EnumLike 0)
                    UnionFactoryShape.StructTag
                    "a struct EnumLike case is nullary and still constructs, from the tag alone"

                Expect.equal
                    (UnionFactoryShape.ofCase UnionValueKind.Struct UnionRegime.SingleCase 0)
                    UnionFactoryShape.UnionCtor
                    "a nullary single-case struct forwards no parameter"
            }

            // The rule the singleton work rests on: one nullary case of a reference union
            // has one value, in every regime.
            test "every nullary case of a reference union is cached" {
                for regime in
                    [
                        UnionRegime.SingleCase
                        UnionRegime.EnumLike
                        UnionRegime.TypeTested
                        UnionRegime.Tagged
                    ] do
                    Expect.equal
                        (UnionFactoryShape.ofCase UnionValueKind.RefType regime 0)
                        UnionFactoryShape.Cached
                        (sprintf "%A" regime)
            }

            test "a reference case with a payload constructs where its payload lives" {
                for regime in [ UnionRegime.TypeTested; UnionRegime.Tagged ] do
                    Expect.equal
                        (UnionFactoryShape.ofCase UnionValueKind.RefType regime 1)
                        UnionFactoryShape.CaseCtor
                        (sprintf "%A puts the payload on the case type" regime)

                Expect.equal
                    (UnionFactoryShape.ofCase UnionValueKind.RefType UnionRegime.SingleCase 1)
                    UnionFactoryShape.UnionCtor
                    "a single case's payload is the union's own"
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
                Expect.equal (UnionCaseFields.names [ ValueNone ]) [ "item" ] "a lone positional field"

                Expect.equal
                    (UnionCaseFields.names [ ValueNone; ValueNone ])
                    [ "item1"; "item2" ]
                    "two positional fields"

                Expect.equal (UnionCaseFields.names [ ValueSome "radius" ]) [ "_radius" ] "a declared name"

                Expect.equal
                    (UnionCaseFields.names [ ValueSome "tag"; ValueNone ])
                    [ "_tag"; "item2" ]
                    "the index counts every field, not the positional ones alone"
            }

            // A regime whose fields have no `TypeDef` to themselves places them in shared
            // slots spelled `_ref<n>` / `_val<n>`, which `UnionPlacementsTests` covers.
            test "the regimes that name a case's fields are exactly the own-type ones" {
                let regimes =
                    [
                        UnionRegime.SingleCase
                        UnionRegime.EnumLike
                        UnionRegime.StructTagged
                        UnionRegime.TypeTested
                        UnionRegime.Tagged
                    ]

                Expect.equal
                    [ for r in regimes -> r, UnionCaseFields.ownType r ]
                    [
                        UnionRegime.SingleCase, true
                        UnionRegime.EnumLike, false
                        UnionRegime.StructTagged, false
                        UnionRegime.TypeTested, true
                        UnionRegime.Tagged, true
                    ]
                    "own-type regimes"
            }
        ]

[<Tests>]
let caseTest =
    testList
        "UnionCaseTest.ofRegime"
        [
            // Each thunk mints its handle in the caller's scope, so a regime must force
            // only the thunk it reads; a wrongly forced thunk is the failure this pins.
            let noMint what () : System.Reflection.Metadata.EntityHandle =
                failwithf "the regime forced the %s thunk" what

            test "the tag-reading regimes compare the minted accessor against the case's tag" {
                let minted = System.Reflection.Metadata.EntityHandle()

                for regime in [ UnionRegime.EnumLike; UnionRegime.StructTagged; UnionRegime.Tagged ] do
                    Expect.equal
                        (UnionCaseTest.ofRegime regime 2 (fun () -> minted) (noMint "case-type"))
                        (UnionCaseTest.TagEquals { Getter = minted; Tag = 2 })
                        (sprintf "%A reads the tag" regime)
            }

            test "TypeTested tests the minted case type and never mints an accessor ref" {
                let minted = System.Reflection.Metadata.EntityHandle()

                Expect.equal
                    (UnionCaseTest.ofRegime UnionRegime.TypeTested 1 (noMint "tag-getter") (fun () -> minted))
                    (UnionCaseTest.IsInst minted)
                    "the case's runtime type discriminates"
            }

            test "SingleCase is irrefutable and mints nothing" {
                Expect.equal
                    (UnionCaseTest.ofRegime UnionRegime.SingleCase 0 (noMint "tag-getter") (noMint "case-type"))
                    UnionCaseTest.Irrefutable
                    "the sole case needs no test"
            }
        ]
