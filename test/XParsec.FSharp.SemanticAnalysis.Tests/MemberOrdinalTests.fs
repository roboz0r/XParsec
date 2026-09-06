module XParsec.FSharp.SemanticAnalysis.Tests.MemberOrdinalTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

let private ordinalOf (members: TypeMemberInfo[]) (name: string) : MemberOrdinal =
    (members |> Array.find (fun m -> m.Name = name)).Ordinal

[<Tests>]
let tests =
    testList
        "MemberOrdinal and LocalBindingId"
        [
            test "a class's ordinals follow source order: primary ctor first, then each declaration in turn" {
                let ctx =
                    analyse (
                        String.concat
                            "\n"
                            [
                                "type IRank ="
                                "    abstract Rank: unit -> int"
                                ""
                                "type C(x: int) ="
                                "    member this.A() = x"
                                "    new() = C(0)"
                                "    member this.B() = x + 1"
                                "    interface IRank with"
                                "        member this.Rank() = x"
                            ]
                    )

                let info = expectClass ctx "C"

                Expect.equal info.Body.PrimaryCtor (ValueSome(MemberOrdinal 0)) "primary ctor is member 0"
                Expect.equal (ordinalOf info.Body.Members "A") (MemberOrdinal 1) "A"

                Expect.equal
                    (info.Body.SecondaryCtors |> Array.map (fun c -> c.Ordinal))
                    [| MemberOrdinal 2 |]
                    "the secondary ctor sits where it is written"

                Expect.equal (ordinalOf info.Body.Members "B") (MemberOrdinal 3) "B"

                let impl = Array.exactlyOne info.Body.InterfaceImpls
                Expect.equal (ordinalOf impl.Members "Rank") (MemberOrdinal 4) "the interface impl member"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a class without a primary ctor starts its ordinals at the first secondary" {
                let ctx =
                    analyse "type C =\n    val N: int\n    new(n) = { N = n }\n    member this.Get() = this.N"

                let info = expectClass ctx "C"
                Expect.equal info.Body.PrimaryCtor ValueNone "no primary ctor"
                Expect.equal (Array.exactlyOne info.Body.SecondaryCtors).Ordinal (MemberOrdinal 0) "secondary ctor"
                Expect.equal (ordinalOf info.Body.Members "Get") (MemberOrdinal 1) "Get"
            }

            test "an accessor pair takes one ordinal per half" {
                let ctx =
                    analyse "type C() =\n    abstract P: int with get, set\n    abstract Q: int"

                let info = expectClass ctx "C"
                Expect.equal (ordinalOf info.Body.Members "P") (MemberOrdinal 1) "getter half"
                Expect.equal (ordinalOf info.Body.Members "set_P") (MemberOrdinal 2) "setter half"
                Expect.equal (ordinalOf info.Body.Members "Q") (MemberOrdinal 3) "the next member"
            }

            test "a union augmentation numbers its members from zero" {
                let ctx =
                    analyse
                        "type U =
    | A
    | B

    member this.IsA = match this with A -> true | B -> false"

                let info = expectUnion ctx "U"
                Expect.equal (ordinalOf info.Members "IsA") (MemberOrdinal 0) "IsA"
            }

            test "a rejected `new` or `val` in an augmentation takes no ordinal and adds no other diagnostic" {
                let ctx =
                    analyse (
                        String.concat
                            "
"
                            [
                                "type U ="
                                "    | A of int"
                                "    | B"
                                ""
                                "    with"
                                ""
                                "    new(x: int) = A x"
                                "    val mutable N: int"
                                "    member this.IsA = match this with A _ -> true | B -> false"
                            ]
                    )

                let info = expectUnion ctx "U"

                Expect.equal
                    (ordinalOf info.Members "IsA")
                    (MemberOrdinal 0)
                    "IsA follows the rejected declarations directly"

                Expect.equal
                    (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> Seq.sort |> List.ofSeq)
                    [
                        "Constructors cannot be defined for this type"
                        "Explicit fields cannot be defined for this type"
                    ]
                    "one diagnostic per rejected declaration"
            }

            test "a `val` or `new` inside an interface implementation is rejected" {
                let ctx =
                    analyse (
                        String.concat
                            "
"
                            [
                                "type IRank ="
                                "    abstract Rank: unit -> int"
                                ""
                                "type C() ="
                                "    interface IRank with"
                                "        val N: int"
                                "        new() = C()"
                                "        member this.Rank() = 1"
                            ]
                    )

                let info = expectClass ctx "C"
                let impl = Array.exactlyOne info.Body.InterfaceImpls

                Expect.equal
                    (ordinalOf impl.Members "Rank")
                    (MemberOrdinal 1)
                    "the impl member follows the primary ctor"

                Expect.equal
                    (ctx.Diagnostics |> Seq.map (fun d -> d.Message) |> Seq.sort |> List.ofSeq)
                    [
                        "A constructor is not permitted in an interface implementation"
                        "A field declaration is not permitted in an interface implementation"
                    ]
                    "one diagnostic per rejected declaration"
            }

            test "each generalised binding takes one dense LocalBindingId, kept across re-generalisation" {
                // `f` is pre-bound from its annotations before its body is typed, then
                // generalised again; `g` is body-local; a `mutable` never generalises.
                let ctx =
                    analyse
                        "let f (x: 'a) : 'a =
    let g y = y
    g x
let mutable v = 0"

                let entries = ctx.Bindings.Scheme.AsDictionary()
                Expect.equal entries.Count 2 "f and g generalise; v does not"

                Expect.equal
                    (entries.Values
                     |> Seq.map (fun e -> let (LocalBindingId i) = e.Id in i)
                     |> Seq.sort
                     |> List.ofSeq)
                    [ 0 .. entries.Count - 1 ]
                    "ids are dense"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "a `let rec` member keeps its LocalBindingId across the retraction that bars polymorphic recursion" {
                // `f`'s annotation-derived scheme is recorded, retracted while the group's
                // bodies type, then recorded again.
                let ctx =
                    analyse
                        "let rec f (x: 'a) : 'a = g x
and g (y: 'b) : 'b = y"

                let entries = ctx.Bindings.Scheme.AsDictionary()
                Expect.equal entries.Count 2 "f and g generalise"

                for e in entries.Values do
                    Expect.isTrue e.Scheme.IsSome "every entry holds a scheme once the group has typed"

                Expect.equal
                    (entries.Values
                     |> Seq.map (fun e -> let (LocalBindingId i) = e.Id in i)
                     |> Seq.sort
                     |> List.ofSeq)
                    [ 0; 1 ]
                    "ids are dense: the retraction minted nothing"

                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }
        ]
