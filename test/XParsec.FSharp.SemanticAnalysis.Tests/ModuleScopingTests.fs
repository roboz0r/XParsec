module XParsec.FSharp.SemanticAnalysis.Tests.ModuleScopingTests

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

let private expectClean (tast: TastFile) =
    Expect.isEmpty (errors tast) (sprintf "expected no errors; diagnostics were %A" (errors tast))

/// A bare name that reaches no declaration in scope must not resolve. Only the VERDICT is
/// pinned, not the wording: F# blames these with FS0039 ("The type 'T' is not defined").
let private expectRejected (source: string) =
    let es = errors (analyse source)
    Expect.isNonEmpty es "expected a diagnostic: the name names nothing in scope here"

/// The `SymbolKey` of the type `typeName` at `arity` declared in the module `moduleName` —
/// selected by the declaration's own HOLDER and its own ARITY, because the whole point here
/// is that two sibling `T`s (or two `T`s of different arity) are two types and a name cannot
/// tell them apart.
let private typeDeclKeyInArity (tast: TastFile) (moduleName: string) (typeName: string) (arity: int) : SymbolKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Key with
                    | SymbolKey.Type {
                                         Holder = TypeHolder.InModule m
                                         TyparArity = a
                                     } when m.Name = moduleName && a = arity -> yield td.Key
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ k ] -> k
    | other -> failtestf "expected one '%s'/%d held by module '%s', got %A" typeName arity moduleName other

/// The `SymbolKey` of the NON-GENERIC type `typeName` declared in the module `moduleName`.
let private typeDeclKeyIn (tast: TastFile) (moduleName: string) (typeName: string) : SymbolKey =
    typeDeclKeyInArity tast moduleName typeName 0

/// The `SymbolKey` of the non-generic `typeName` declared in the module `inner` NESTED in the
/// module `outer` — the whole chain, because a sibling module of the same short name as
/// `inner` may declare the same type name, and only the chain tells the two apart.
let private typeDeclKeyInNested (tast: TastFile) (outer: string) (inner: string) (typeName: string) : SymbolKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Key with
                    | SymbolKey.Type {
                                         Holder = TypeHolder.InModule {
                                                                          Name = i
                                                                          Holder = ModuleHolder.InModule o
                                                                      }
                                     } when i = inner && o.Name = outer -> yield td.Key
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ k ] -> k
    | other -> failtestf "expected one '%s' held by module '%s.%s', got %A" typeName outer inner other

/// The `SymbolKey` of the type `typeName` declared directly in a NAMESPACE.
let private typeDeclKeyInNamespace (tast: TastFile) (typeName: string) : SymbolKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Key with
                    | SymbolKey.Type { Holder = TypeHolder.InNamespace _ } -> yield td.Key
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ k ] -> k
    | other -> failtestf "expected one namespace-held '%s', got %A" typeName other

let private nominalKey (ty: SemType) : SymbolKey =
    match ty with
    | SemType.TyClass(k, _)
    | SemType.TyRecord(k, _)
    | SemType.TyUnion(k, _) -> SymbolKey.Type k
    | other -> failtestf "expected a nominal type, got %A" other

/// The ARGUMENT type of the unit's sole module-level `let` — for `let f (v: T) = v`, what
/// the written `T` bound to. Every program below writes exactly one `let`, so the use site
/// under test is the only one there is.
let private soleLetArg (tast: TastFile) : SemType =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Let(ty = ty) -> yield ty
                | _ -> ()
        ]

    match found with
    | [ SemType.TyFun(arg, _) ] -> arg
    | other -> failtestf "expected exactly one module-level let of function type, got %A" other

let private src (lines: string list) = String.concat "\n" lines

// A bare type name resolves INNERMOST-OUTWARD: the use's own module, the `open`s in that
// scope, then each enclosing module, then the enclosing namespace. A SIBLING module
// contributes nothing without an `open` — `module A`'s types are simply not in scope in
// `module B`, and F# says so (FS0039).
//
// Every one of these asserts the RESOLVED IDENTITY, not acceptance: a shadowing rule got
// backwards still compiles, and only the key it bound to says which type the name meant.
[<Tests>]
let tests =
    testList
        "ModuleScoping"
        [
            test "a sibling module's type is NOT in scope by its bare name" {
                expectRejected (
                    src
                        [
                            "namespace N"
                            ""
                            "module A ="
                            "    type T = { fromA: int }"
                            ""
                            "module B ="
                            "    let f (v: T) = v"
                        ]
                )
            }

            test "an `open` brings the sibling module's type into scope, and it binds to THAT type" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    open A"
                                "    let f (v: T) = v"
                            ]
                    )

                expectClean tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "T bound to A's T"
            }

            test "a namespace-level type is visible inside a module of that namespace, with no `open`" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "type T = { atNamespace: int }"
                                ""
                                "module B ="
                                "    let f (v: T) = v"
                            ]
                    )

                expectClean tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInNamespace tast "T")
                    "T bound to the namespace-held T — the namespace is an ancestor scope"
            }

            // Innermost wins. The control is the SAME program with the inner declaration
            // removed: it binds to the outer `T`, so the shadowed case is pinned against the
            // type it would otherwise have bound to, not merely against acceptance.
            test "a nested module's own type shadows the enclosing module's" {
                let shadowed =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module Outer ="
                                "    type T = { fromOuter: int }"
                                ""
                                "    module Inner ="
                                "        type T = { fromInner: int }"
                                "        let f (v: T) = v"
                            ]
                    )

                expectClean shadowed

                Expect.equal
                    (nominalKey (soleLetArg shadowed))
                    (typeDeclKeyIn shadowed "Inner" "T")
                    "the inner T shadows the outer one"

                let unshadowed =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module Outer ="
                                "    type T = { fromOuter: int }"
                                ""
                                "    module Inner ="
                                "        let f (v: T) = v"
                            ]
                    )

                expectClean unshadowed

                Expect.equal
                    (nominalKey (soleLetArg unshadowed))
                    (typeDeclKeyIn unshadowed "Outer" "T")
                    "without an inner T the same use binds to the enclosing module's"
            }

            // An `open` is added to the name environment where it is WRITTEN, and the last
            // thing added wins — so an `open` inside the inner module outranks the enclosing
            // module's declaration.
            test "an `open` beats an enclosing module's declaration" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module Outer ="
                                "    type T = { fromOuter: int }"
                                ""
                                "    module Inner ="
                                "        open A"
                                "        let f (v: T) = v"
                            ]
                    )

                expectClean tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "the `open` outranks Outer's T"
            }

            // Two `open`s both offering a `T`: the LAST one silently wins. Deliberate
            // shadowing is idiomatic F#, so there is no warning to emit — only a binding.
            test "the last of two `open`s wins" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    type T = { fromB: int }"
                                ""
                                "module C ="
                                "    open A"
                                "    open B"
                                "    let f (v: T) = v"
                            ]
                    )

                expectClean tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "B" "T") "the later `open B` wins"
            }

            // Within ONE scope a declaration and an `open` are ordered by nothing but the
            // text — F# adds each to the name environment where it is written, and the last
            // one added is what the name means. (Probed against `dotnet fsi`: with
            // `type T` then `open A`, `typeof<T>` is A's T.)
            test "in one scope, a declaration and an `open` are ordered by the text" {
                let openThenDecl =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    open A"
                                "    type T = { fromB: int }"
                                "    let f (v: T) = v"
                            ]
                    )

                expectClean openThenDecl

                Expect.equal
                    (nominalKey (soleLetArg openThenDecl))
                    (typeDeclKeyIn openThenDecl "B" "T")
                    "the declaration is written after the `open`, so it wins"

                let declThenOpen =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    type T = { fromB: int }"
                                "    open A"
                                "    let f (v: T) = v"
                            ]
                    )

                expectClean declThenOpen

                Expect.equal
                    (nominalKey (soleLetArg declThenOpen))
                    (typeDeclKeyIn declThenOpen "A" "T")
                    "the `open` is written after the declaration, so it wins"
            }
        ]

// A type is reached from outside the module holding it by NAMING that module: the qualifier
// is a path through the scopes this unit declares, resolved from the use — its own scopes
// (innermost first), the `open`s in force, or the root (a fully-qualified path). The type it
// selects there is as local as a bare one.
//
// Every program below declares a SECOND `T`, in a sibling module, so nothing can pass by
// conflating the two: only the key the name bound to says which `T` it meant.
[<Tests>]
let qualifiedTests =
    testList
        "ModuleScoping qualified"
        [
            test "a sibling module's type is reached by naming its module" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    type T = { fromB: int }"
                                "    let f (v: A.T) = v"
                            ]
                    )

                expectClean tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyIn tast "A" "T")
                    "A.T is A's T — not the B.T that a bare T would have named"
            }

            test "a fully-qualified path names the type from anywhere in the unit" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    type T = { fromB: int }"
                                "    let f (v: N.A.T) = v"
                            ]
                    )

                expectClean tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "N.A.T is A's T"
            }

            // The namespace is not an ancestor scope of the use, so the path is resolved from
            // the ROOT — the only route left, and the one F# leaves open from everywhere.
            test "a fully-qualified path names the type from ANOTHER namespace" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "module B ="
                                "    type T = { fromB: int }"
                                ""
                                "namespace M"
                                ""
                                "module C ="
                                "    let f (v: N.A.T) = v"
                            ]
                    )

                expectClean tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "N.A.T is A's T"
            }

            test "a nested module's type is reached through the path to it" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    module B ="
                                "        type T = { fromAB: int }"
                                ""
                                "module B ="
                                "    type T = { fromB: int }"
                                ""
                                "module C ="
                                "    let f (v: A.B.T) = v"
                            ]
                    )

                expectClean tast

                // The path is walked from its HEAD, so the `B` in `A.B` is the one A holds —
                // not the sibling module B, whose own `T` is a different type entirely.
                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInNested tast "A" "B" "T")
                    "A.B.T is the T of the B nested in A"
            }

            // Arity is part of the claim, so it is part of what a qualified name selects: `A`
            // holds BOTH a `T` and a `T<'a>`, and the written head picks one of them.
            test "a qualified generic head resolves at its written arity" {
                let source =
                    src
                        [
                            "namespace N"
                            ""
                            "module A ="
                            "    type T = { fromA: int }"
                            "    type T<'a> = { fromGenericA: 'a }"
                            ""
                            "module B ="
                            "    type T<'a> = { fromB: 'a }"
                            "    let f (v: A.T<int>) = v"
                        ]

                let tast = analyse source
                expectClean tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInArity tast "A" "T" 1)
                    "A.T<int> is A's generic T, not its non-generic one and not B's"

                match soleLetArg tast with
                | SemType.TyRecord(_, args) ->
                    Expect.equal (EqArray.toList args) [ BuiltinTypes.tyInt ] "the written type argument is applied"
                | other -> failtestf "expected a record, got %A" other

                // The same path at the OTHER arity names the other type — the arity is not
                // decoration on one name, it is part of which claim is held.
                let nonGeneric =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                "    type T<'a> = { fromGenericA: 'a }"
                                ""
                                "module B ="
                                "    type T<'a> = { fromB: 'a }"
                                "    let f (v: A.T) = v"
                            ]
                    )

                expectClean nonGeneric

                Expect.equal
                    (nominalKey (soleLetArg nonGeneric))
                    (typeDeclKeyInArity nonGeneric "A" "T" 0)
                    "A.T names A's non-generic T"
            }

            // An `open` qualifies a PARTIAL path: `open N` + `A.T` names `N.A.T`, exactly as
            // it brings `N`'s own types into scope bare. (Probed against `dotnet fsi`.)
            test "an `open` qualifies a partial path" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module A ="
                                "    type T = { fromA: int }"
                                ""
                                "namespace M"
                                ""
                                "module A ="
                                "    type T = { fromM: int }"
                                ""
                                "module C ="
                                "    open N"
                                "    let f (v: A.T) = v"
                            ]
                    )

                expectClean tast

                // `M` also holds an `A.T`, and `M` is an ancestor scope of the use — so the
                // `open N` must OUTRANK it (it is written deeper, and later), or the name
                // would mean M's.
                match soleLetArg tast with
                | SemType.TyRecord(k, _) -> Expect.equal k.Namespace.Dotted "N" "the `open N` qualifies A.T to N.A.T"
                | other -> failtestf "expected a record, got %A" other
            }

            // A qualifier that names a module of THIS unit wins over an external type of the
            // same dotted spelling: `Vesper.Collections.seq` is a real external type (the
            // contract's `seq` interface), and the local module chain shadows it. F#'s answer,
            // probed against `dotnet fsi`: the nearest scope that can name the qualifier wins,
            // and the enclosing namespace holds this one.
            test "a project-local qualified type beats an external type of the same spelling" {
                let tast =
                    analyse (
                        src
                            [
                                "namespace N"
                                ""
                                "module Vesper ="
                                "    module Collections ="
                                "        type seq<'a> = { fromLocal: 'a }"
                                ""
                                "module B ="
                                "    let f (v: Vesper.Collections.seq<int>) = v"
                            ]
                    )

                expectClean tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInArity tast "Collections" "seq" 1)
                    "the local Vesper.Collections.seq wins over the external one"
            }

            // A name a scope OF THIS UNIT does not hold is a DIAGNOSTIC: we know every type
            // our own scopes hold. A fresh type variable would unify with anything and
            // surface the mistake as unencodable output far from it.
            //
            // The same cannot be said under a qualifier we do not declare: what an external
            // name means is the provider's to answer, and the provider is a partial view.
            test "a name a module of this unit does not hold is not defined" {
                expectRejected (
                    src
                        [
                            "namespace N"
                            ""
                            "module A ="
                            "    type T = { fromA: int }"
                            ""
                            "module B ="
                            "    let f (v: A.Nope) = v"
                        ]
                )

                expectRejected (
                    src
                        [
                            "namespace N"
                            ""
                            "module A ="
                            "    module B ="
                            "        type T = { fromAB: int }"
                            ""
                            "module C ="
                            "    let f (v: A.B.Nope) = v"
                        ]
                )
            }

            // The head names A's `T`, at an arity A does not hold it at — so the ARITY is
            // blamed. A local claim is never abandoned for an external type of the same
            // spelling just because the arity is wrong.
            test "a qualified head at the wrong arity blames the arity" {
                let es =
                    errors (
                        analyse (
                            src
                                [
                                    "namespace N"
                                    ""
                                    "module A ="
                                    "    type T = { fromA: int }"
                                    ""
                                    "module B ="
                                    "    let f (v: A.T<int>) = v"
                                ]
                        )
                    )

                Expect.isNonEmpty es "expected an arity diagnostic"

                Expect.isTrue
                    (es |> List.exists (fun e -> e.Contains "A.T" && e.Contains "type argument"))
                    (sprintf "expected the arity of A.T to be blamed; got %A" es)
            }
        ]
