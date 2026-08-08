module XParsec.FSharp.SemanticAnalysis.Tests.TypeScopeOrderTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText lexed) file

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

/// The RESOLVED return type of one class member, off the elaborated TAST. A preamble binder
/// carries no name of its own once it is a field, so this is how a SHADOWING test pins which
/// binder a name bound to: give the shadowing `let` a different type and read it back here.
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

/// The RESOLVED type of the unit's ONE module-level `let`, off the elaborated TAST — the
/// module-`let` counterpart of the two accessors above, so a scoping test can pin what a
/// `let`'s annotation BOUND to rather than merely that it was accepted.
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
            // resolution are one mechanism here: the name was classified external where it
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
            // external type, because the type name's classification — made where it was written,
            // with nothing yet claiming `exn` — is what the translation reads.
            yield
                test "a shadowing local declaration below a member signature does not capture it" {
                    let src = "type Holder() =\n    member this.M(e: exn) = e"

                    let external = classMemberParamType src "Holder" "M"

                    let shadowed =
                        classMemberParamType (src + "\ntype exn = { message: int }") "Holder" "M"

                    Expect.equal shadowed external "Holder.M's parameter binds the external `exn`"
                }

            // Types and module `let`s are ONE ordered sequence, not two passes: a `let` sees
            // the types declared above it and nothing below. Every annotation a `let` writes —
            // in its signature or anywhere in its body — is that same rule, so all of these
            // are the ordinary unknown-type error.
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

            // The shadowing rule, through a module `let` — the third site of the same
            // mechanism (record field, member signature, module let). The name was classified
            // where it was written, with nothing yet claiming `exn`, so it stamped external
            // and stays bound there once the local `exn` registers. Pinned against the SAME
            // program without the local declaration, so the assertion is "identical
            // resolution", not a hardcoded key.
            yield
                test "a shadowing local declaration below a module let does not capture it" {
                    let src = "let f (e: exn) = e"
                    let external = soleModuleLetType src

                    let shadowed = soleModuleLetType (src + "\ntype exn = { message: int }")

                    Expect.equal shadowed external "f's parameter binds the external `exn`, not the local one below"
                }

            // A type BODY has the module's two-tier shape: its `let`/`do` preamble is one
            // strictly top-down sequence (each binding sees only the ones ABOVE it), and its
            // MEMBERS are a mutually-recursive group that may reference each other in any
            // order and see every let. Confirmed against the reference compiler: a preamble
            // `let` naming a later `let` is FS0039 "not defined" (static and instance alike),
            // while a member calling a member declared below it compiles.
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

            // Member names are not in the preamble's LEXICAL scope — a let is evaluated
            // during construction, so it may only reach a member through the type (`C.Q()`)
            // or the self-identifier, never by bare name. F# agrees: bare `Q` is FS0039.
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

            // The preamble and the primary `inherit` args are scoped and inferred like any
            // other expression, so an operator in either must carry a compiled name through to
            // Elaborate — the same requirement a member body has.
            yield
                test "an operator in a primary inherit argument is accepted" {
                    expectClean
                        "type B(n: int) =\n    member _.N = n\ntype D() =\n    inherit B(1 + 2)\n    member this.M = 3"
                }

            // The instance preamble obeys the same two-tier rule: it is one top-down sequence
            // (a later `let` sees an earlier one), and every binder is in scope for the
            // mutually-recursive member group.
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
            // a `static let` never sees an instance binder. F# reports FS0039 too.
            yield
                test "a static let referencing an instance let is rejected" {
                    expectError
                        "Unresolved identifier: a"
                        "type C() =\n    let a = 1\n    static let k = a + 1\n    member _.K = k"
                }

            // An instance `let`/`do` runs in the PRIMARY ctor. The `val`-field form has none,
            // so there is nowhere for it to run — F# rejects it (FS0963) rather than picking a
            // secondary ctor.
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

            // A struct's zero-arg default ctor is not ours to write, so an instance binder's field
            // would be left unset by `Unchecked.defaultof<S>` — F# rejects both shapes (FS0901 /
            // FS0035), and so must we: `buildClassPrimaryCtor` would otherwise happily run the
            // preamble in the ctor we DO emit and leave the default-constructed value inconsistent.
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

            // A `static let mutable` binder IS the static field, so a write stores to it
            // (`TExpr.StaticFieldSet`, emitted `stsfld` / a class-object property assign).
            yield
                test "a write to a `static let mutable` is accepted" {
                    expectClean "type C() =\n    static let mutable n = 0\n    member _.Bump () = n <- n + 1"
                }

            // …but a write to a NON-mutable `static let` is still an immutable-binding error —
            // the mutability gate reads `IsMutable` faithfully off the binder, so only the
            // `mutable` form is writable.
            yield
                test "a write to a non-mutable `static let` is rejected" {
                    expectError
                        "assignment to immutable binding"
                        "type C() =\n    static let n = 0\n    member _.Bump () = n <- n + 1"
                }

            // The object is NOT nameable from the preamble: F# only exposes it through an
            // explicit `as self`, and even then a member call from a `let` throws at run time
            // (initialisation soundness). Absent that analysis, rejecting is the only
            // alternative to silently reading a not-yet-initialised field.
            yield
                test "a preamble let calling a member through the `as` alias is rejected" {
                    expectError
                        "Unresolved qualified name: self"
                        "type C(n: int) as self =\n    let m = self.Double n\n    member _.Double x = x * 2\n    member _.M = m"
                }

            // Ctor params, `val` fields, `static let`s and instance `let`s ALL mint a field
            // carrying their source name, so any two of them sharing a name mint two fields of one
            // name — and on the CLR that is one duplicate Field row, static-ness being in the
            // flags rather than the identity. F# accepts every one of these (it uniquifies the
            // backing-field names by source position); until that pass exists — local `let`s need
            // it just as much — reject rather than miscompile. A LIMITATION, not invalid F#.
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
                    // Pre-existing hazard, not one the preamble introduced: a ctor param and a
                    // `val` of one name were always two fields of one name.
                    "a val field colliding with a ctor param",
                    "type C(x: int) =\n    [<DefaultValue>]\n    val mutable x: int\n    member this.X = this.x + x"
                ] -> test $"{form} is rejected" { expectError "Duplicate field name" source }

            // FS0905 — unlike the collisions above this is a REAL F# rule, and one that binder
            // uniquification would not lift: a member's name is its public surface, so a class
            // `let` may not share it. Both sides being static makes no difference (probed).
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

            // …and the non-shadowing neighbours of those programs still resolve, pinned by TYPE
            // rather than by acceptance: the member reads the binder it names, not a same-shaped
            // one — a blanket rejection of anything that merely LOOKS like a preamble let would
            // pass an acceptance test.
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

            // The static/instance neighbours of the cross-family rejections above: distinctly
            // named, each member must read the binder it NAMES — a static field and an instance
            // field of one class are not interchangeable.
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

            // An operator in a preamble initialiser / `do` body must carry a compiled name
            // through to Elaborate (`InfixApp … missing DesugaredForm` is a hard crash), and a
            // preamble `let` may capture a `let mutable` — which is a FIELD, so the closure and
            // the member bodies read the same storage.
            yield
                test "an instance do body and a let-bound closure over a let mutable are accepted" {
                    expectClean
                        "type C() =\n    let mutable c = 0\n    let bump () = c <- c + 1\n    do bump ()\n    member _.C = c"
                }

            // `let rec` puts its binder in scope of its OWN initialiser: recursion, not shadowing,
            // so the shadowing rejection above must not swallow it.
            yield
                test "a recursive instance let is accepted" {
                    expectClean
                        "type C() =\n    let rec fact k = if k <= 1 then 1 else k * fact (k - 1)\n    member _.F = fact 5"
                }

            // Instance lets in a GENERIC class: a preamble binder is an instance field, and
            // those already work generically (a ctor param is one).
            yield
                test "instance lets in a generic class are accepted" {
                    expectClean
                        "type G<'T>(x: 'T) =\n    let count = 1 + 1\n    let stored = x\n    member _.Stored = stored\n    member _.Count = count"
                }
        ]
