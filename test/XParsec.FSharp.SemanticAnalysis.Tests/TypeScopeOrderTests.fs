module XParsec.FSharp.SemanticAnalysis.Tests.TypeScopeOrderTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

let private errors (tast: TastFile) =
    [
        for d in tast.Diagnostics do
            if Diagnostic.isError d then
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

/// A member's type is INFERRED from its body, so its signature annotations must bind what
/// they were classified as at registration, not what the registry says once the file is in.
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

/// A preamble bound variable carries no name of its own once it is a field, so a SHADOWING
/// test gives the shadowing `let` a different type and reads back which one a member bound.
let private classMemberReturnType (source: string) (typeName: string) (memberName: string) : SemType =
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
                                yield m.ReturnTy
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ ty ] -> ty
    | other -> failtestf "expected exactly one member '%s.%s', got %A" typeName memberName other

let private soleModuleLetType (source: string) : SemType =
    let tast = analyse source

    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Let(ty = ty) -> yield ty
                | _ -> ()
        ]

    match found with
    | [ ty ] -> ty
    | other -> failtestf "expected exactly one module-level let, got %A" other

// F# type scoping is strictly file-ordered: a type sees the types declared ABOVE it and
// nothing below, and `type X = … and Y = …` is the one unit of mutual recursion. These pin
// the verdicts against F#'s: FS0039 forward reference, FS0954 inheritance / FS0953 alias cycle.
[<Tests>]
let tests =
    testList
        "TypeScopeOrder"
        [
            // A reference to a type declared BELOW is a plain unknown-type error, not a
            // special "forward reference" one, and every position that writes a type is alike.
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

            // `inherit` is the one reference resolved against the parent's registered DETAIL
            // rather than its identity, so it fills at group close — never from below.
            yield
                test "forward reference from an inherit clause is rejected" {
                    expectError
                        "unknown type 'Base'"
                        "type Derived() =\n    inherit Base()\ntype Base() =\n    member this.X = 1"
                }

            // `and` is the recursive group: every name and arity in it is claimed before any
            // detail registers, which is all a field / case / signature reference needs.
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

            // The exact pair the struct test below rejects, modulo `[<Struct>]`: a cycle
            // through REFERENCE-type record fields is broken by the indirection, so F#
            // compiles it clean. Only inheritance and STRUCT-field edges are real cycles.
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

            // A STRUCT stores its fields inline, so a cycle through one has no finite layout.
            // F# gives it FS0954, the same code an inheritance cycle gets: both are the same
            // immediate-containment relation.
            yield
                test "struct-field cycle within a group is diagnosed" {
                    expectError
                        "immediate cyclic reference through a struct field"
                        "[<Struct>]\ntype A = { x: B }\nand [<Struct>] B = { y: A }"
                }

            // A type declared below shadows nothing above it: above its declaration the
            // external `exn` is the only `exn` there is, so the reference resolves.
            yield
                test "an external type of the same name still resolves above a local declaration" {
                    expectClean "type Container = { e: exn }\ntype exn = { message: int }"
                }

            // …and it BINDS to the external type, not merely accepts: `exn` was classified
            // external where it was written, nothing having claimed the name yet. Pinned
            // against the SAME program without the local declaration, not a hardcoded key.
            yield
                test "a shadowing local declaration below a use does not capture it" {
                    let external = recordFieldType "type Container = { e: exn }" "Container" "e"

                    let shadowed =
                        recordFieldType "type Container = { e: exn }\ntype exn = { message: int }" "Container" "e"

                    Expect.equal
                        shadowed
                        external
                        "Container.e binds the external `exn`, not the local one declared below"
                }

            // The same rule through a member SIGNATURE, whose annotation is translated long
            // after the whole file is registered: classification at the write site is what
            // the translation reads.
            yield
                test "a shadowing local declaration below a member signature does not capture it" {
                    let src = "type Container() =\n    member this.M(e: exn) = e"

                    let external = classMemberParamType src "Container" "M"

                    let shadowed =
                        classMemberParamType (src + "\ntype exn = { message: int }") "Container" "M"

                    Expect.equal shadowed external "Container.M's parameter binds the external `exn`"
                }

            // Types and module `let`s are ONE ordered sequence, not two passes: a `let` sees
            // the types above it and nothing below, and every annotation it writes — in its
            // signature or anywhere in its body — is that same rule.
            for position, source in
                [
                    "parameter annotation", "let f (a: A) = a\ntype A = { x: int }"
                    "return-type annotation", "let f a : A = a\ntype A = { x: int }"
                    "body let annotation", "let g () =\n    let x: A = { x = 1 }\n    x\ntype A = { x: int }"
                    "body coercion", "let g (o: obj) = o :?> A\ntype A = { x: int }"
                ] ->
                test $"a module let's {position} naming a type declared below is rejected" {
                    expectError "The type 'A' is not defined" source
                }

            // …and the sequence really is interleaved: each element referring only UPWARD is
            // legal, however many times the file alternates between the two kinds.
            yield
                test "types and module lets interleave in one top-down sequence" {
                    expectClean "type A = { x: int }\nlet f (a: A) = a.x\ntype B = { y: A }\nlet g (b: B) = f b.y"
                }

            // The shadowing rule at its third site (record field, member signature, module
            // let), pinned the same way: against the SAME program without the local `exn`.
            yield
                test "a shadowing local declaration below a module let does not capture it" {
                    let src = "let f (e: exn) = e"
                    let external = soleModuleLetType src

                    let shadowed = soleModuleLetType (src + "\ntype exn = { message: int }")

                    Expect.equal shadowed external "f's parameter binds the external `exn`, not the local one below"
                }

            // A type BODY is two-tier: the `let`/`do` preamble is strictly top-down, while
            // MEMBERS are one mutually-recursive group seeing every let. Probed: a preamble
            // `let` referencing a later `let` is FS0039; a member calling a later member compiles.
            yield
                test "a class static let referencing a later static let is rejected" {
                    expectError
                        "Unresolved identifier: b"
                        "type C() =\n    static let a = b\n    static let b = 1\n    static member A = a"
                }

            yield
                test "a class static let referencing an earlier static let is accepted" {
                    expectClean "type C() =\n    static let b = 1\n    static let a = b + 1\n    static member A = a"
                }

            yield
                test "a member calling a member declared below it is accepted" {
                    expectClean "type C() =\n    member this.P() = this.Q() + 1\n    member _.Q() = 2"
                }

            yield
                test "a member referencing an earlier let and a later member is accepted" {
                    expectClean
                        "type C() =\n    static let k = 10\n    member this.P() = k + this.Q()\n    member _.Q() = 2"
                }

            // Member names are not in the preamble's LEXICAL scope: a let may reach a member
            // only through the type (`C.Q()`) or the self-identifier (F#: bare `Q` is FS0039).
            yield
                test "a class let referencing a member by bare name is rejected" {
                    expectError
                        "Unresolved identifier: Q"
                        "type C() =\n    static let a = Q()\n    static member Q() = 2\n    static member A = a"
                }

            yield
                test "a class static let referencing a static member through the type name is accepted" {
                    expectClean
                        "type C() =\n    static let a = C.Q()\n    static member Q() = 2\n    static member A = a"
                }

            // The preamble and the primary `inherit` args are ordinary expressions, so an
            // operator in either must carry its compiled name through to elaboration.
            yield
                test "an operator in a primary inherit argument is accepted" {
                    expectClean
                        "type B(n: int) =\n    member _.N = n\ntype D() =\n    inherit B(1 + 2)\n    member this.M = 3"
                }

            // The instance preamble obeys the same two-tier rule: one top-down sequence, every
            // bound variable in scope for the mutually-recursive member group.
            yield
                test "a member referencing an instance let is accepted" {
                    expectClean "type C() =\n    let b = 1\n    let a = b + 1\n    member _.A = a"
                }

            // The `.cctor` has already run when the primary ctor does, so an instance entry
            // may read a `static let` above it.
            yield
                test "an instance let referencing a static let is accepted" {
                    expectClean "type C() =\n    static let k = 10\n    let a = k + 1\n    member _.A = a"
                }

            // …and not the other way round: the two sequences are separate ordered scopes, so
            // a `static let` never sees an instance bound variable. F# reports FS0039 too.
            yield
                test "a static let referencing an instance let is rejected" {
                    expectError
                        "Unresolved identifier: a"
                        "type C() =\n    let a = 1\n    static let k = a + 1\n    member _.K = k"
                }

            // An instance `let`/`do` runs in the PRIMARY ctor; the `val`-field form has none,
            // so F# rejects it (FS0963) rather than picking a secondary ctor.
            yield
                test "an instance let in a class with no primary constructor is rejected" {
                    expectError
                        "primary constructor"
                        "type C =\n    val x: int\n    let a = 1 + 1\n    new() = { x = 0 }"
                }

            yield
                test "an instance do in a class with no primary constructor is rejected" {
                    expectError
                        "primary constructor"
                        "type C =\n    val x: int\n    do ignore (1 + 1)\n    new() = { x = 0 }"
                }

            // That rejection is a property of the CLASS, and every preamble diagnostic anchors at
            // the type's decl key — so it must be reported once, not once per offending entry.
            yield
                test "a class with no primary constructor reports its instance preamble once" {
                    let es =
                        errors (
                            analyse
                                "type C =\n    val x: int\n    let a = 1 + 1\n    do ignore 2\n    let b = 3\n    new() = { x = 0 }"
                        )
                        |> List.filter (fun m -> m.Contains "primary constructor")

                    Expect.equal (List.length es) 1 (sprintf "one error for three offending entries, got %A" es)
                }

            // A struct's zero-arg default ctor is not ours to write, so a preamble field would
            // be left unset by `Unchecked.defaultof<S>`. F# rejects both shapes, FS0901 / FS0035.
            yield
                test "an instance let on a struct is rejected" {
                    expectError
                        "Structs cannot contain value definitions"
                        "[<Struct>]\ntype S(x: int) =\n    let y = x + 1\n    member _.Y = y"
                }

            yield
                test "an instance do on a struct is rejected" {
                    expectError
                        "Structs cannot contain `do` bindings"
                        "[<Struct>]\ntype S(x: int) =\n    do ignore (x + 1)\n    member _.X = x"
                }

            // …and only the INSTANCE sequence: a struct's static preamble runs in the `.cctor`,
            // which owes nothing to the default ctor. F# accepts it.
            yield
                test "a static let on a struct is accepted" {
                    expectClean "[<Struct>]\ntype S(x: int) =\n    static let k = 41\n    member _.X = x + k"
                }

            // A `static let mutable` bound variable IS the static field, so a write stores to
            // it: `TExpr.StaticFieldSet`, emitted `stsfld` / a class-object property assign.
            yield
                test "a write to a `static let mutable` is accepted" {
                    expectClean "type C() =\n    static let mutable n = 0\n    member _.Bump () = n <- n + 1"
                }

            yield
                test "a write to a non-mutable `static let` is rejected" {
                    expectError
                        "assignment to immutable binding"
                        "type C() =\n    static let n = 0\n    member _.Bump () = n <- n + 1"
                }

            // F# exposes the object to the preamble only through an explicit `as self`, and
            // even then a member call from a `let` throws at run time (initialisation
            // soundness). We reject rather than read a not-yet-initialised field.
            yield
                test "a preamble let calling a member through the `as` alias is rejected" {
                    expectError
                        "Unresolved qualified name: self"
                        "type C(n: int) as self =\n    let m = self.Double n\n    member _.Double x = x * 2\n    member _.M = m"
                }

            // Ctor params, `val` fields and preamble `let`s all mint a field carrying their
            // source name, so two of one name are one duplicate CLR Field row (static-ness is
            // a flag, not identity). F# uniquifies by position; until we do, reject. NOT invalid F#.
            for form, source in
                [
                    "an instance let shadowing a ctor param", "type C(n: int) =\n    let n = n + 1\n    member _.N = n"
                    "an instance let shadowing an earlier instance let",
                    "type C() =\n    let v = 1\n    let v = v + 1\n    member _.V = v"
                    "a static let shadowing an earlier static let",
                    "type C() =\n    static let v = 1\n    static let v = v + 1\n    static member V = v"
                    // Not shadowing at all — two DISTINCT storage locations in F#, one static and
                    // one instance — yet still a single Field row on the CLR, so still rejected.
                    "an instance let colliding with a static let",
                    "type C() =\n    static let v = 1\n    let v = v + 1\n    member _.V = v\n    static member SV = v"
                    "an instance let colliding with a val field",
                    "type C(n: int) =\n    [<DefaultValue>]\n    val mutable x: int\n    let x = n + 1\n    member _.X = x"
                    // A ctor param and a `val` of one name were always two fields of one name.
                    "a val field colliding with a ctor param",
                    "type C(x: int) =\n    [<DefaultValue>]\n    val mutable x: int\n    member this.X = this.x + x"
                ] -> test $"{form} is rejected" { expectError "Duplicate field name" source }

            // FS0905 is a REAL F# rule, not a limitation of ours: a member's name is its public
            // surface, so a class `let` may not share it. Both sides static is no different (probed).
            for form, source in
                [
                    "an instance let colliding with a member name",
                    "type C(n: int) =\n    let M = n + 1\n    member _.M = M * 10"
                    "a static let colliding with a static member name",
                    "type C() =\n    static let T = 1\n    static member T = T * 10"
                ] ->
                test $"{form} is rejected" {
                    expectError "A member and a local class binding both have the name" source
                }

            // …and a ctor param may share a member's name (F# accepts it: a param is not a local
            // class binding), so the FS0905 check must not over-reach into the ctor params.
            yield
                test "a ctor param sharing a member's name is accepted" {
                    let ty = classMemberReturnType "type C(n: int) =\n    member _.n = n > 0" "C" "n"
                    Expect.equal ty BuiltinTypes.tyBool "the member is typed from its own body, not the param"
                }

            // …and the non-shadowing neighbours still resolve, pinned by TYPE: the member reads
            // the bound variable it references, which an acceptance test could not tell from a
            // blanket rejection of anything that merely LOOKS like a preamble let.
            yield
                test "a distinctly-named instance let over a ctor param binds the let in a member" {
                    let ty =
                        classMemberReturnType "type C(n: int) =\n    let m = n + 1 > 0\n    member _.M = m" "C" "M"

                    Expect.equal ty BuiltinTypes.tyBool "the member sees the `bool` let, not the `int` ctor param"
                }

            yield
                test "a later instance let binds the earlier one in its initialiser" {
                    let ty =
                        classMemberReturnType
                            "type C() =\n    let v = 1\n    let w = v + 1 > 0\n    member _.W = w"
                            "C"
                            "W"

                    Expect.equal
                        ty
                        BuiltinTypes.tyBool
                        "the member sees the second let, whose initialiser read the first"
                }

            // The static/instance neighbours of the cross-family rejections above: a static
            // field and an instance field of one class are not interchangeable.
            yield
                test "distinctly-named static and instance lets each bind their own member" {
                    let source =
                        "type C() =\n    static let s = 1\n    let i = s > 0\n    member _.I = i\n    static member S = s"

                    Expect.equal (classMemberReturnType source "C" "I") BuiltinTypes.tyBool "`I` reads the `bool` let"
                    Expect.equal (classMemberReturnType source "C" "S") BuiltinTypes.tyInt "`S` reads the `int` static"
                }

            yield
                test "a distinctly-named val field alongside an instance let binds each in a member" {
                    let source =
                        "type C(n: int) =\n    [<DefaultValue>]\n    val mutable y: int\n    let m = n > 0\n    member _.M = m\n    member this.Y = this.y"

                    Expect.equal (classMemberReturnType source "C" "M") BuiltinTypes.tyBool "`M` reads the `bool` let"

                    Expect.equal
                        (classMemberReturnType source "C" "Y")
                        BuiltinTypes.tyInt
                        "`Y` reads the `int` val field"
                }

            // A preamble `let` may capture a `let mutable` — which is a FIELD, so the closure
            // and the member bodies read the same storage.
            yield
                test "an instance do body and a let-bound closure over a let mutable are accepted" {
                    expectClean
                        "type C() =\n    let mutable c = 0\n    let bump () = c <- c + 1\n    do bump ()\n    member _.C = c"
                }

            // `let rec` puts its bound variable in scope of its OWN initialiser: recursion,
            // not shadowing, so the shadowing rejection above must not swallow it.
            yield
                test "a recursive instance let is accepted" {
                    expectClean
                        "type C() =\n    let rec fact k = if k <= 1 then 1 else k * fact (k - 1)\n    member _.F = fact 5"
                }

            // A preamble bound variable is an instance field, and those already work
            // generically (a ctor param is one).
            yield
                test "instance lets in a generic class are accepted" {
                    expectClean
                        "type G<'T>(x: 'T) =\n    let count = 1 + 1\n    let stored = x\n    member _.Stored = stored\n    member _.Count = count"
                }
        ]
