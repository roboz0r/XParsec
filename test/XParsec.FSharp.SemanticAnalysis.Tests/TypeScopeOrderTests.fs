module XParsec.FSharp.SemanticAnalysis.Tests.TypeScopeOrderTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

let private errors (tast: TastFile) =
    [
        for d in tast.Diagnostics do
            if d.Severity = Severity.Error then
                yield d.Message
    ]

let private expectError (needle: string) (source: string) =
    let es = errors (analyse source)

    Expect.isTrue
        (es |> List.exists (fun m -> m.Contains needle))
        (sprintf "expected an error containing '%s'; diagnostics were %A" needle es)

let private expectClean (source: string) =
    let es = errors (analyse source)
    Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

// F# type scoping is strictly file-ordered: a type sees the types declared ABOVE it and
// nothing below, and `type X = … and Y = …` — one `ModuleElem.Type` group — is the one
// unit of mutual recursion. Registration walks the file top-down one group at a time, so
// the rule holds by construction; these pin the accept/reject verdicts against the
// reference compiler's (a forward reference is FS0039 "not defined", a group-local
// inheritance cycle FS0954, a group-local alias cycle FS0953).
[<Tests>]
let tests =
    testList
        "TypeScopeOrder"
        [
            // A reference to a type declared BELOW, in a later group, is a plain
            // unknown-type error — not a special "forward reference" one. Every position
            // that names a type is the same rule.
            for position, source in
                [
                    "record field", "type A = { x: B }\ntype B = { y: int }"
                    "union case field", "type A = | Wrap of B\ntype B = { y: int }"
                    "abbreviation RHS", "type A = B\ntype B = { y: int }"
                    "member signature", "type A() =\n    member this.M(v: B) = v\ntype B = { y: int }"
                    "ctor parameter", "type A(v: B) =\n    member this.V = v\ntype B = { y: int }"
                    "val field", "type A =\n    val x: B\ntype B = { y: int }"
                    "type argument", "type A = { x: Wrap<B> }\ntype Wrap<'a> = { w: 'a }\ntype B = { y: int }"
                ] -> test $"forward reference from a {position} is rejected" { expectError "'B' is not defined" source }

            // The `inherit` clause is the one reference resolved against the parent's
            // registered DETAIL rather than its identity, so it fills at group close — and
            // a parent below the group is never going to fill it.
            yield
                test "forward reference from an inherit clause is rejected" {
                    expectError
                        "unknown type 'Base'"
                        "type Derived() =\n    inherit Base()\ntype Base() =\n    member this.X = 1"
                }

            // `and` is the recursive group: every member's name and arity is claimed before
            // any member's detail registers, which is all a field / case / signature
            // reference to a sibling needs.
            for shape, source in
                [
                    "records", "type A = { x: B }\nand B = { y: A option }"
                    "record and union", "type A = { x: B }\nand B = | Leaf | Node of A"
                    "unions", "type A = | Wrap of B\nand B = | Back of A"
                    "classes", "type A() =\n    member this.M(b: B) = b\nand B() =\n    member this.M(a: A) = a"
                    "class inheriting a sibling declared below it",
                    "type Derived() =\n    inherit Base()\nand Base() =\n    member this.X = 1"
                    "abbreviation of a sibling declared below it", "type A = B\nand B = { y: int }"
                ] -> test $"and-joined mutual recursion of {shape} is accepted" { expectClean source }

            // THE case a naive cycle check would break. The cycle here runs through
            // REFERENCE-type record fields, and the indirection breaks it — F# compiles
            // this clean. Only inheritance edges and STRUCT-field edges make a real cycle.
            yield
                test "reference-type record mutual recursion is accepted" {
                    expectClean "type A = { x: B }\nand B = { y: A }"
                }

            // A cycle needs a back-edge, and file-order scoping means only a group can hold
            // one. So both cycle classes are group-local, and both are diagnosed.
            yield
                test "inheritance cycle within a group is diagnosed" {
                    expectError "cyclic inheritance" "type A() =\n    inherit B()\nand B() =\n    inherit A()"
                }

            yield
                test "abbreviation cycle within a group is diagnosed" {
                    expectError "is cyclic" "type A = B\nand B = A"
                }

            // A type declared below shadows nothing above it: above its declaration the
            // external `exn` is the only `exn` there is, so the reference resolves rather
            // than diagnosing. This is what keeps the file-order check from firing on every
            // external name that a unit happens to redeclare later.
            yield
                test "an external type of the same name still resolves above a local declaration" {
                    expectClean "type Holder = { e: exn }\ntype exn = { message: int }"
                }
        ]
