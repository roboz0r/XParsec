module XParsec.FSharp.SemanticAnalysis.Tests.AttributeTargetTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private src (lines: string list) = String.concat "\n" lines


// `[<AttributeUsage>]` target enforcement (FS0842's wording, error severity): the used
// attribute declaration's mask — local registry or referenced contract — against the flags
// of the element it is written on. A declaration without a reachable AttributeUsage passes
// everywhere.

[<Tests>]
let targetTests =
    testList
        "AttributeUsage targets"
        [
            test "a Class-only local attribute passes on a class, errors on a method and on a let value" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Class)>]"
                                "type ClsOnlyAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<ClsOnly>]"
                                "type Good() ="
                                "    member this.X = 1"
                                ""
                                "type Bad() ="
                                "    [<ClsOnly>]"
                                "    member this.Y() = 1"
                                ""
                                "[<ClsOnly>]"
                                "let v = 1"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ onMethod; onLet ] ->
                    Expect.equal
                        onMethod
                        "This attribute cannot be applied to method, return value. Valid targets are: class"
                        "fsc's element list for a method"

                    Expect.equal
                        onLet
                        "This attribute cannot be applied to property, field, return value. Valid targets are: class"
                        "fsc's element list for a module value"
                | other -> failtestf "expected the method and let errors, got %A" other
            }

            test "[<Global>] on a type errors: the contract declares Property ||| Field" {
                let pools = freezeFor (src [ "[<Global>]"; "type G() ="; "    member this.X = 1" ])

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to class. Valid targets are: property, field"
                        "the Vesper contract's GlobalAttribute mask"
                | other -> failtestf "expected exactly the [<Global>] error, got %A" other
            }

            test "an attribute with no AttributeUsage passes on a type, a method and a let" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type FreeAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<Free>]"
                                "type T() ="
                                "    [<Free>]"
                                "    member this.Y() = 1"
                                ""
                                "[<Free>]"
                                "let v = 1"
                            ]
                    )

                Expect.isEmpty
                    (errorMessages (FrozenPools.blockingErrors pools))
                    "no AttributeUsage defaults to AttributeTargets.All"
            }

            // fsc checks a measure declaration as it checks an alias: any type-kind target
            // passes, and a non-type target errors with the alias's element list.
            test "a Struct-only attribute passes on a measure, a Method-only one errors" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Struct)>]"
                                "type StructOnlyAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<AttributeUsage(AttributeTargets.Method)>]"
                                "type MethodOnlyAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<Measure; StructOnly>]"
                                "type m"
                                ""
                                "[<Measure; MethodOnly>]"
                                "type s = m"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ onMeasure ] ->
                    Expect.equal
                        onMeasure
                        "This attribute cannot be applied to class, struct, enum, interface, delegate. Valid targets are: method"
                        "fsc's element list for a measure"
                | other -> failtestf "expected the Method-only error alone, got %A" other
            }

            test "a Class ||| Struct mask passes on a class and a struct record, errors on an interface" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Class ||| AttributeTargets.Struct)>]"
                                "type CSAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<CS>]"
                                "type C() ="
                                "    member this.X = 1"
                                ""
                                "[<CS>]"
                                "[<Struct>]"
                                "type S = { X: int }"
                                ""
                                "[<CS>]"
                                "type I ="
                                "    abstract M: unit -> int"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to interface. Valid targets are: class, struct"
                        "the folded ||| mask admits class and struct only"
                | other -> failtestf "expected exactly the interface error, got %A" other
            }

            test "an attribute declared later in a rec group states its mask at an earlier use" {
                let pools =
                    freezeFor (
                        src
                            [
                                "type [<Late>] First() ="
                                "    member this.X = 1"
                                ""
                                "and [<AttributeUsage(AttributeTargets.Method)>] LateAttribute() ="
                                "    member this.M() = 1"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to class. Valid targets are: method"
                        "the later declaration's mask reaches the earlier use"
                | other -> failtestf "expected exactly the class error, got %A" other
            }

            test "a parameter, a type parameter and an abstract member each take their own element" {
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Parameter)>]"
                                "type ParamOnlyAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "[<AttributeUsage(AttributeTargets.GenericParameter)>]"
                                "type TyparOnlyAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "let ok ([<ParamOnly>] x: int) = x"
                                "let bad ([<TyparOnly>] y: int) = y"
                                ""
                                "type Boxed<[<TyparOnly>] 'T> = { V: 'T }"
                                "type Boxed2<[<ParamOnly>] 'T> = { W: 'T }"
                                ""
                                "type IFace ="
                                "    [<ParamOnly>]"
                                "    abstract M: int -> int"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ onParam; onTypar; onAbstract ] ->
                    Expect.equal
                        onParam
                        "This attribute cannot be applied to parameter. Valid targets are: generic parameter"
                        "a parameter is a Parameter element"

                    Expect.equal
                        onTypar
                        "This attribute cannot be applied to generic parameter. Valid targets are: parameter"
                        "a declared typar is a GenericParameter element"

                    Expect.equal
                        onAbstract
                        "This attribute cannot be applied to method, return value. Valid targets are: parameter"
                        "a curried abstract signature is a Method element"
                | other -> failtestf "expected the parameter, typar and abstract errors, got %A" other
            }

            test "a contract-declared mask is enforced: [<Sealed>] on a method errors" {
                let pools =
                    freezeFor (src [ "type K() ="; "    [<Sealed>]"; "    member this.M() = 1" ])

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to method, return value. Valid targets are: class"
                        "SealedAttribute's contract mask is Class"
                | other -> failtestf "expected exactly the [<Sealed>] error, got %A" other
            }

            ptest "GAP: a derived attribute class does not take its base's AttributeUsage mask" {
                // `AttributeUsageAttribute` is `Inherited = true` in the BCL, so CLR
                // reflection reports the base's mask on the derived class and C# enforces it
                // there. `Inherited` is decoded nowhere here, and `Vesper`'s own
                // `AttributeUsageAttribute` declaration carries no `[<AttributeUsage>]` to
                // read it from, so a class declaring none defaults to `All`. fsc also
                // reports nothing for this source.
                let pools =
                    freezeFor (
                        src
                            [
                                "[<AttributeUsage(AttributeTargets.Class)>]"
                                "type BaseMarkAttribute() ="
                                "    member this.M() = 1"
                                ""
                                "type DerivedMarkAttribute() ="
                                "    inherit BaseMarkAttribute()"
                                ""
                                "type Bad() ="
                                "    [<DerivedMark>]"
                                "    member this.Y() = 1"
                            ]
                    )

                match errorMessages (FrozenPools.blockingErrors pools) with
                | [ msg ] ->
                    Expect.equal
                        msg
                        "This attribute cannot be applied to method, return value. Valid targets are: class"
                        "the inherited mask is enforced on the derived class"
                | other -> failtestf "expected the inherited-mask error, got %A" other
            }
        ]

let private position (usedOn: AttrTarget) : AttributePosition =
    {
        UsedOn = usedOn
        Attributes = ResolvedAttributes.None
        Checked = Block.empty
    }

let private siteAt (offset: int) =
    NodeKey.ofSource offset NodeKind.DeclAttributes

[<Tests>]
let positionTableTests =
    testList
        "AttributePositionTable"
        [
            test "Seal takes the positions in source order" {
                let table = AttributePositionTable()
                table.Declare(siteAt 20, position AttrTarget.Method)
                table.Declare(siteAt 5, position AttrTarget.Class)

                Expect.equal
                    [ for p in table.Seal().InSourceOrder -> p.UsedOn ]
                    [ AttrTarget.Class; AttrTarget.Method ]
                    "the earlier offset comes first"
            }

            test "a position declared after the seal fails" {
                let table = AttributePositionTable()
                table.Seal() |> ignore

                Expect.throws
                    (fun () -> table.Declare(siteAt 0, position AttrTarget.Class))
                    "a pass scheduled after the check cannot file a position it would miss"
            }

            test "one site declared twice fails" {
                let table = AttributePositionTable()
                table.Declare(siteAt 0, position AttrTarget.Class)

                Expect.throws
                    (fun () -> table.Declare(siteAt 0, position AttrTarget.Method))
                    "the second declaration would overwrite the first"
            }
        ]
