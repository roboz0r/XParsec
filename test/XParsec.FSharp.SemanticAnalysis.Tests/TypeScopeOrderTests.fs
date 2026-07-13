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

/// The RESOLVED type of one record field, off the elaborated TAST — so a scoping test can
/// pin what a name BOUND to, not merely that it was accepted.
let private recordFieldType (source: string) (typeName: string) (fieldName: string) : SemType =
    let tast = analyse source

    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Kind with
                    | TTypeKind.Record(fields = fs) ->
                        for f in fs do
                            if f.Name = fieldName then
                                yield f.Type
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ ty ] -> ty
    | other -> failtestf "expected exactly one field '%s' on '%s', got %A" fieldName typeName other

/// The RESOLVED type of one class member, off the elaborated TAST. A member's type is
/// INFERRED from its body (Unification owns it), so this is the other half of the shadowing
/// rule: the signature annotations in that body must bind what the classification made of
/// them at registration, not what the registry says once the whole file is in it.
let private classMemberParamType (source: string) (typeName: string) (memberName: string) : SemType =
    let tast = analyse source

    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Kind with
                    | TTypeKind.Class c ->
                        for m in c.Members do
                            if m.Name = memberName then
                                yield! [ for (_, ty) in m.Params -> ty ]
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ ty ] -> ty
    | other -> failtestf "expected exactly one parameter on '%s.%s', got %A" typeName memberName other

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
                    "records", "type A = { x: B }\nand B = { y: A[] }"
                    "record and union", "type A = { x: B }\nand B = | Leaf | Node of A"
                    "unions", "type A = | Wrap of B\nand B = | Back of A"
                    "classes", "type A() =\n    member this.M(b: B) = b\nand B() =\n    member this.M(a: A) = a"
                    "class inheriting a sibling declared below it",
                    "type Derived() =\n    inherit Base()\nand Base() =\n    member this.X = 1"
                    "abbreviation of a sibling declared below it", "type A = B\nand B = { y: int }"
                ] -> test $"and-joined mutual recursion of {shape} is accepted" { expectClean source }

            // THE case a naive cycle check would break, and the exact pair the struct test
            // below rejects — modulo `[<Struct>]`. The cycle here runs through REFERENCE-type
            // record fields, and the indirection breaks it, so F# compiles this clean. Only
            // inheritance edges and STRUCT-field edges make a real cycle.
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

            // A STRUCT stores its fields inline, so a struct field is an immediate
            // containment edge and a cycle through one has no finite layout. F# rejects this
            // with FS0954 — the same code an inheritance cycle gets, because both are the
            // same immediate-containment relation.
            yield
                test "struct-field cycle within a group is diagnosed" {
                    expectError
                        "immediate cyclic reference through a struct field"
                        "[<Struct>]\ntype A = { x: B }\nand [<Struct>] B = { y: A }"
                }

            // A type declared below shadows nothing above it: above its declaration the
            // external `exn` is the only `exn` there is, so the reference resolves rather
            // than diagnosing. This is what keeps the file-order rule from firing on every
            // external name that a unit happens to redeclare later.
            yield
                test "an external type of the same name still resolves above a local declaration" {
                    expectClean "type Holder = { e: exn }\ntype exn = { message: int }"
                }

            // …and it BINDS to the external type, not merely accepts. Diagnostics and
            // resolution are one mechanism here: the head was classified external where it
            // was written (nothing had claimed `exn` yet), so it stays external once the
            // local `exn` registers. Pinned against the SAME program without the local
            // declaration, so the assertion is "identical resolution", not a hardcoded key.
            yield
                test "a shadowing local declaration below a use does not capture it" {
                    let external = recordFieldType "type Holder = { e: exn }" "Holder" "e"

                    let shadowed =
                        recordFieldType "type Holder = { e: exn }\ntype exn = { message: int }" "Holder" "e"

                    Expect.equal shadowed external "Holder.e binds the external `exn`, not the local one declared below"
                }

            // The same rule through a member SIGNATURE, whose annotation is translated by
            // the body walk long after the whole file is registered. It still binds the
            // external type, because the head's classification — made where it was written,
            // with nothing yet claiming `exn` — is what the translation reads.
            yield
                test "a shadowing local declaration below a member signature does not capture it" {
                    let src = "type Holder() =\n    member this.M(e: exn) = e"

                    let external = classMemberParamType src "Holder" "M"

                    let shadowed =
                        classMemberParamType (src + "\ntype exn = { message: int }") "Holder" "M"

                    Expect.equal shadowed external "Holder.M's parameter binds the external `exn`"
                }
        ]
