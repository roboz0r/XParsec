module XParsec.FSharp.SemanticAnalysis.Tests.TyparListTests

open Expecto
open XParsec.FSharp.SemanticAnalysis

[<Tests>]
let tests =
    testList
        "TyparList"
        [
            test "kinds round-trip through ofSeq in source order" {
                let written =
                    [
                        {
                            TTypeParam.Name = "'u"
                            Kind = TyparKind.Measure
                        }
                        {
                            TTypeParam.Name = "'a"
                            Kind = TyparKind.Type
                        }
                        {
                            TTypeParam.Name = "'v"
                            Kind = TyparKind.Measure
                        }
                        {
                            TTypeParam.Name = "'b"
                            Kind = TyparKind.Type
                        }
                    ]

                let typars = TyparList.ofSeq written

                Expect.equal
                    (EqArray.toList (TyparList.kinds typars))
                    [ for p in written -> p.Kind ]
                    "kinds read back in source order"

                Expect.equal typars.Length written.Length "arity counts both kinds"
            }

            test "Order indexes Types and Measures apart" {
                let typars =
                    TyparList.ofSeq
                        [
                            {
                                TTypeParam.Name = "'u"
                                Kind = TyparKind.Measure
                            }
                            {
                                TTypeParam.Name = "'a"
                                Kind = TyparKind.Type
                            }
                            {
                                TTypeParam.Name = "'v"
                                Kind = TyparKind.Measure
                            }
                        ]

                Expect.equal
                    (EqArray.toList typars.Order)
                    [ TyparSlot.Measure 0; TyparSlot.Type 0; TyparSlot.Measure 1 ]
                    "each slot is the next index of its own kind"

                Expect.equal
                    (EqArray.toList typars.Types)
                    [
                        {
                            TypeTypar.Name = TyparName.Written "'a"
                        }
                    ]
                    "the type-kinded parameters"

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

            test "positional names its parameters by index" {
                let typars = TyparList.positional 2

                Expect.equal
                    (EqArray.toList typars.Types)
                    [
                        {
                            TypeTypar.Name = TyparName.Positional 0
                        }
                        {
                            TypeTypar.Name = TyparName.Positional 1
                        }
                    ]
                    "no name is fabricated"

                Expect.equal typars.Measures.Length 0 "positional parameters are type-kinded"
                Expect.equal (TyparList.positional 0) TyparList.empty "arity 0 is empty"
            }
        ]
