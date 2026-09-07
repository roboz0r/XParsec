module XParsec.FSharp.SemanticAnalysis.Tests.TyparListTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

let private unconstrained (name: string) : TypeTypar =
    {
        Name = TyparName.Written name
        Constraints = ConstraintSet.empty
    }

[<Tests>]
let tests =
    testList
        "TyparList"
        [
            test "kinds round-trip through ofSeq in source order" {
                let written =
                    [
                        "'u", TyparKind.Measure
                        "'a", TyparKind.Type
                        "'v", TyparKind.Measure
                        "'b", TyparKind.Type
                    ]

                let typars: TyparList = TyparList.ofSeq written

                Expect.equal
                    (EqArray.toList (TyparList.kinds typars))
                    [ for (_, k) in written -> k ]
                    "kinds read back in source order"

                Expect.equal typars.Length written.Length "arity counts both kinds"
                Expect.equal (EqArray.toList typars.Names) [ for (n, _) in written -> n ] "names in source order"
            }

            test "Order indexes Types and Measures apart" {
                let typars: TyparList =
                    TyparList.ofSeq [ "'u", TyparKind.Measure; "'a", TyparKind.Type; "'v", TyparKind.Measure ]

                Expect.equal
                    (EqArray.toList typars.Order)
                    [ TyparSlot.Measure 0; TyparSlot.Type 0; TyparSlot.Measure 1 ]
                    "each slot is the next index of its own kind"

                Expect.equal (EqArray.toList typars.Types) [ unconstrained "'a" ] "the type-kinded parameters"

                Expect.equal
                    (EqArray.toList typars.Measures)
                    [
                        {
                            MeasureTypar.Name = TyparName.Written "'u"
                        }
                        {
                            MeasureTypar.Name = TyparName.Written "'v"
                        }
                    ]
                    "the measure-kinded parameters"
            }

            test "ofKinded constrains a type-kinded parameter by its source position" {
                let typars: TyparList =
                    TyparList.ofKinded
                        (fun i ->
                            if i = 2 then
                                ConstraintSet.ofKinds [ TyparConstraintKindG.Equality ]
                            else
                                ConstraintSet.empty
                        )
                        [ "'a", TyparKind.Type; "'u", TyparKind.Measure; "'b", TyparKind.Type ]

                Expect.equal
                    [ for t in typars.Types -> EqSet.toList t.Constraints.Kinds ]
                    [ []; [ TyparConstraintKindG.Equality ] ]
                    "the constraint lands on the second type-kinded parameter"

                Expect.isTrue typars.HasConstraints "a constrained list reports it"
            }

            test "positional names its parameters by index" {
                let typars: TyparList = TyparList.positional 2

                Expect.equal
                    (EqArray.toList typars.Types)
                    [
                        {
                            Name = TyparName.Positional 0
                            Constraints = ConstraintSet.empty
                        }
                        {
                            Name = TyparName.Positional 1
                            Constraints = ConstraintSet.empty
                        }
                    ]
                    "no name is fabricated"

                Expect.equal typars.Measures.Length 0 "positional parameters are type-kinded"
                Expect.equal (TyparList.positional 0) TyparList.empty "arity 0 is empty"
                Expect.isFalse typars.HasConstraints "positional parameters are unconstrained"
            }
        ]
