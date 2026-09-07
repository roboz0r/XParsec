module XParsec.FSharp.SemanticAnalysis.Tests.TyparListTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis

let private unconstrained (name: string) : TypeTypar =
    {
        Name = TyparName.Written name
        Constraints = ConstraintSet.empty
    }

let private declared (name: string) (kind: TyparKind) (tyVar: int) : DeclaredTypar =
    {
        Name = name
        TyVar = LanguagePrimitives.Int32WithMeasure tyVar
        Kind = kind
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
                    (Block.toList (TyparList.kinds typars))
                    [ for (_, k) in written -> k ]
                    "kinds read back in source order"

                Expect.equal typars.Length written.Length "arity counts both kinds"
                Expect.equal (Block.toList typars.Names) [ for (n, _) in written -> n ] "names in source order"
            }

            test "Order indexes Types and Measures apart" {
                let typars: TyparList =
                    TyparList.ofSeq [ "'u", TyparKind.Measure; "'a", TyparKind.Type; "'v", TyparKind.Measure ]

                Expect.equal
                    (Block.toList typars.Order)
                    [
                        TyparSlot.Measure 0<measureSlot>
                        TyparSlot.Type 0<typeSlot>
                        TyparSlot.Measure 1<measureSlot>
                    ]
                    "each slot is the next index of its own kind"

                Expect.equal (Block.toList typars.Types) [ unconstrained "'a" ] "the type-kinded parameters"

                Expect.equal
                    (Block.toList typars.Measures)
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

            test "a measure typar before a type typar counts on its own axis" {
                let typars: TyparList =
                    TyparList.ofSeq [ "'u", TyparKind.Measure; "'a", TyparKind.Type; "'b", TyparKind.Type ]

                Expect.equal typars.Length 3 "the signature arity counts both kinds"
                Expect.equal typars.TypeArity 2<typeSlot> "the type-kinded count"
                Expect.equal typars.MeasureArity 1<measureSlot> "the measure-kinded count"

                Expect.equal (TyparList.typeSlotOf typars 0<sigSlot>) ValueNone "signature slot 0 is measure-kinded"
                Expect.equal (TyparList.measureSlotOf typars 0<sigSlot>) (ValueSome 0<measureSlot>) "its measure slot"
                Expect.equal (TyparList.typeSlotOf typars 1<sigSlot>) (ValueSome 0<typeSlot>) "'a is type slot 0"
                Expect.equal (TyparList.typeSlotOf typars 2<sigSlot>) (ValueSome 1<typeSlot>) "'b is type slot 1"
                Expect.equal (TyparList.typeSlotOf typars 3<sigSlot>) ValueNone "past the arity"

                Expect.equal
                    (TyparList.sigSlotOf typars (TyparSlot.Type 1<typeSlot>))
                    (ValueSome 2<sigSlot>)
                    "type slot 1 is written third"

                Expect.equal
                    (TyparList.sigSlotOf typars (TyparSlot.Measure 0<measureSlot>))
                    (ValueSome 0<sigSlot>)
                    "measure slot 0 is written first"
            }

            test "ofDeclared constrains a type-kinded parameter by its declaration" {
                let typeParams =
                    Block.ofList
                        [
                            declared "'a" TyparKind.Type 0
                            declared "'u" TyparKind.Measure 1
                            declared "'b" TyparKind.Type 2
                        ]

                let typars: TyparList =
                    typeParams
                    |> TyparList.ofDeclared (fun tp ->
                        if tp.Name = "'b" then
                            ConstraintSet.ofKinds [ TyparConstraintKindG.Equality ]
                        else
                            ConstraintSet.empty
                    )

                Expect.equal
                    [ for t in typars.Types -> EqSet.toList t.Constraints.Kinds ]
                    [ []; [ TyparConstraintKindG.Equality ] ]
                    "the constraint lands on the second type-kinded parameter"

                Expect.isTrue typars.HasConstraints "a constrained list reports it"
            }

            test "positional names its parameters by index" {
                let typars: TyparList = TyparList.positional 2<typeSlot>

                Expect.equal
                    (Block.toList typars.Types)
                    [
                        {
                            Name = TyparName.Positional 0<typeSlot>
                            Constraints = ConstraintSet.empty
                        }
                        {
                            Name = TyparName.Positional 1<typeSlot>
                            Constraints = ConstraintSet.empty
                        }
                    ]
                    "no name is fabricated"

                Expect.equal typars.MeasureArity 0<measureSlot> "positional parameters are type-kinded"
                Expect.equal (TyparList.positional 0<typeSlot>) TyparList.empty "arity 0 is empty"
                Expect.isFalse typars.HasConstraints "positional parameters are unconstrained"
            }
        ]
