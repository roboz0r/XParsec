module XParsec.FSharp.SemanticAnalysis.Tests.ConformanceBodyTests

// Type BODY conformance: the halves agree on a type's name and family, and what they declare
// inside it is compared as the surface each publishes.

open Expecto

open XParsec.FSharp.SemanticAnalysis.Tests.ConformanceTests


let private conformV (sigDecl: string) (implDecl: string) : string list =
    conformAnalysed ("namespace V\n\n" + sigDecl) ("namespace V\n\n" + implDecl)

[<Tests>]
let bodyConformanceTests =
    testList
        "AnalysedBodyConformance"
        [
            test "a record field required by the signature and absent from the implementation" {
                let findings = conformV "type T = { X: int }" "type T = { W: int }"

                Expect.equal (List.length findings) 2 "the missing field and the unsignatured one"
                Expect.stringContains findings.[0] "'X' was required by the signature" "the FS0313 analogue"

                Expect.stringContains
                    findings.[1]
                    "'W' is present in the implementation"
                    "the converse is also an error"
            }

            test "a record field whose type differs across the pair" {
                let m = conformV "type T = { X: int }" "type T = { X: string }" |> theOne "finding"

                Expect.stringContains m "V.T" "names the identity"
                Expect.stringContains m "declared as 'X: int'" "the signature's field"
                Expect.stringContains m "defined as 'X: string'" "the implementation's field"
            }

            test "a generic record's field is compared over the declaring typars" {
                let m = conformV "type T<'a> = { Y: 'a }" "type T<'a> = { Z: 'a }" |> List.head

                Expect.stringContains
                    m
                    "'Y' was required by the signature"
                    "the generic pair is compared like the plain one"
            }

            test "the same record fields in a different order" {
                let m =
                    conformV "type T = { X: int; Y: int }" "type T = { Y: int; X: int }"
                    |> theOne "finding"

                Expect.stringContains m "different order" "the compiled constructor depends on the order"
            }

            test "a field mutable on one half alone" {
                let m =
                    conformV "type T = { mutable X: int }" "type T = { X: int }" |> theOne "finding"

                Expect.stringContains m "declared as 'mutable X: int'" "the signature's field"
                Expect.stringContains m "defined as 'X: int'" "the implementation's field"
            }

            test "a matching generic, self-referential record conforms" {
                let decl = "type T<'a> = { V: 'a; Next: T<'a> list; Flag: bool }"
                Expect.isEmpty (conformV decl decl) "field templates over the declaring typars compare equal"
            }

            test "a union case renamed across the pair" {
                let m = conformV "type U = | A | B" "type U = | A | C" |> theOne "finding"

                Expect.stringContains m "union case 1" "cases compare positionally"
                Expect.stringContains m "declared as 'B'" "the signature's case"
                Expect.stringContains m "defined as 'C'" "the implementation's case"
            }

            test "a union case whose payload differs" {
                let m =
                    conformV "type U = | A of int | B" "type U = | A of x: string | B"
                    |> theOne "finding"

                Expect.stringContains m "declared as 'A of int'" "positional payload rendered"
                Expect.stringContains m "defined as 'A of x: string'" "named payload rendered"
            }

            test "a union with a different number of cases" {
                let m = conformV "type U = | A | B" "type U = | A | B | C" |> theOne "finding"
                Expect.stringContains m "declares 2 union case(s)" "the signature's count"
                Expect.stringContains m "defines 3" "the implementation's count"
            }

            test "a matching union with named and generic payloads conforms" {
                let decl = "type U<'a> = | A of x: 'a * y: int | B of 'a | C"
                Expect.isEmpty (conformV decl decl) "case names, field names and field types agree"
            }

            test "an enum case whose value differs" {
                let m =
                    conformV "type E =\n    | A = 1\n    | B = 2" "type E =\n    | A = 1\n    | B = 3"
                    |> theOne "finding"

                Expect.stringContains m "'B' has value 2" "the signature's value"
                Expect.stringContains m "but 3" "the implementation's value"
            }

            test "an enum case on one half alone" {
                let findings =
                    conformV "type E =\n    | A = 1\n    | B = 2" "type E =\n    | A = 1\n    | C = 2"

                Expect.equal (List.length findings) 2 "one per unmatched case"
                Expect.stringContains findings.[0] "'B' was required by the signature" "missing in the implementation"
                Expect.stringContains findings.[1] "'C' is present in the implementation" "missing in the signature"
            }

            test "an abbreviation whose body differs" {
                let m = conformV "type alias = int" "type alias = string" |> theOne "finding"
                Expect.stringContains m "abbreviates 'int'" "the signature's body"
                Expect.stringContains m "but 'string'" "the implementation's body"
            }

            test "a member the signature declares that the implementation defines differently" {
                let m =
                    conformV
                        "type C =\n    new: unit -> C\n    member M: int -> int"
                        "type C() =\n    member _.M(x: string) = x"
                    |> theOne "finding"

                Expect.stringContains m "V.C" "names the type"
                Expect.stringContains m "member 'M'" "names the member"
                Expect.stringContains m "does not define" "the FS0193 analogue"
            }

            test "matching class members conform: curried, tupled, static, property and constructor" {
                Expect.isEmpty
                    (conformV
                        "type C =\n    new: unit -> C\n    member M: int -> int -> int\n    member N: int * string -> int\n    static member S: unit -> int\n    member P: int"
                        "type C() =\n    member _.M (a: int) (b: int) = a + b\n    member _.N(a: int, b: string) = a\n    static member S() = 1\n    member _.P = 1")
                    "each declared member is matched by one defined member"
            }

            test "an implementation-only member is hidden, not drift" {
                Expect.isEmpty
                    (conformV
                        "type C =\n    new: unit -> C\n    member P: int"
                        "type C() =\n    member _.P = 1\n    member _.Q = 2")
                    "F# hides a member the signature omits"
            }

            test "a generic member's typar order is compared" {
                let m =
                    conformV
                        "type C =\n    new: unit -> C\n    member M<'b, 'a> : 'a * 'b -> 'a"
                        "type C() =\n    member _.M<'a, 'b>(x: 'a, y: 'b) = x"
                    |> theOne "finding"

                Expect.stringContains m "member 'M'" "the swapped scheme matches no implementation member"
            }

            test "a matching generic member conforms" {
                Expect.isEmpty
                    (conformV
                        "type C =\n    new: unit -> C\n    member M<'a> : 'a -> 'a"
                        "type C() =\n    member _.M<'a>(x: 'a) = x")
                    "the schemes agree"
            }

            test "an interface slot whose signature differs" {
                let m =
                    conformV "type I =\n    abstract M: int -> int" "type I =\n    abstract M: string -> int"
                    |> theOne "finding"

                Expect.stringContains m "member 'M'" "the declared slot has no implementation"
            }

            test "a matching interface conforms" {
                let decl = "type I<'a> =\n    abstract M: 'a -> int\n    abstract P: int"
                Expect.isEmpty (conformV decl decl) "the slots agree"
            }

            test "a record's augmentation members are compared" {
                let m =
                    conformV
                        "type R =\n    { X: int }\n    member Twice: int"
                        "type R =\n    { X: int }\n    member r.Thrice = r.X * 3"
                    |> theOne "finding"

                Expect.stringContains m "member 'Twice'" "the augmentation member is owed"
            }

            test "a union case satisfies the static member its constructor compiles to" {
                Expect.isEmpty
                    (conformV
                        "type U<'a> =\n    | Nil\n    | Cell of head: 'a * tail: U<'a>\n    static member Nil: U<'a>\n    static member Cell: head: 'a * tail: U<'a> -> U<'a>"
                        "type U<'a> =\n    | Nil\n    | Cell of head: 'a * tail: U<'a>")
                    "`list.fsi`'s `static member Cons` is the `::` case, as in FSharp.Core"
            }

            test "a union member the cases do not satisfy is still owed" {
                let m =
                    conformV "type U =\n    | Nil\n    static member Make: int -> U" "type U =\n    | Nil"
                    |> theOne "finding"

                Expect.stringContains m "member 'Make'" "a case of another name satisfies nothing"
            }

            test "a struct record on one half alone" {
                let m =
                    conformV "[<Struct>]\ntype T = { X: int }" "type T = { X: int }"
                    |> theOne "finding"

                Expect.stringContains m "is 'struct' in the signature" "the signature commits to a value type"
            }

            test "sealed on one half alone" {
                let m =
                    conformV "type C =\n    new: unit -> C" "[<Sealed>]\ntype C() = class end"
                    |> theOne "finding"

                Expect.stringContains
                    m
                    "is not 'sealed' in the signature"
                    "the implementation seals what the signature does not"
            }

            test "an opaque signature type demands no body" {
                Expect.isEmpty
                    (conformV "type T" "type T = { X: int }\n\ntype U = | A")
                    "an opaque `type T` hides the representation"
            }
        ]
