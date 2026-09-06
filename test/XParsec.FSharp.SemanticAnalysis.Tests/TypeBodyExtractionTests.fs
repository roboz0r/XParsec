module XParsec.FSharp.SemanticAnalysis.Tests.TypeBodyExtractionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers
open XParsec.FSharp.SemanticAnalysis.Tests.UnificationTestHelpers

let private memberNamed (members: TypeMemberInfo[]) (name: string) : TypeMemberInfo =
    members |> Array.find (fun m -> m.Name = name)

[<Tests>]
let tests =
    testList
        "TypeBodyExtraction and LocalBindingId"
        [
            test "a class body registers its ctors, members, accessor halves and interface impl members" {
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
                                "    abstract P: int with get, set"
                                "    interface IRank with"
                                "        member this.Rank() = x"
                            ]
                    )

                let info = expectClass ctx "C"
                Expect.isTrue info.Body.HasPrimaryCtor "primary ctor"
                Expect.equal info.Body.SecondaryCtors.Length 1 "one secondary ctor"

                Expect.equal
                    (info.Body.Members |> Array.map (fun m -> m.Name) |> List.ofArray)
                    [ "A"; "P"; "set_P" ]
                    "members in source order, one per accessor half"

                let impl = Array.exactlyOne info.Body.InterfaceImpls
                Expect.equal (memberNamed impl.Members "Rank").Name "Rank" "the interface impl member"
                Expect.isEmpty ctx.Diagnostics "no diagnostics"
            }

            test "the `val`-field form has no primary ctor" {
                let ctx =
                    analyse "type C =\n    val N: int\n    new(n) = { N = n }\n    member this.Get() = this.N"

                let info = expectClass ctx "C"
                Expect.isFalse info.Body.HasPrimaryCtor "no primary ctor"
                Expect.equal info.Body.SecondaryCtors.Length 1 "one secondary ctor"
            }

            test "a rejected `new` or `val` in an augmentation adds no other diagnostic" {
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
                Expect.equal (memberNamed info.Members "IsA").Name "IsA" "IsA is registered"

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
                Expect.equal (memberNamed impl.Members "Rank").Name "Rank" "the impl member is registered"

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
