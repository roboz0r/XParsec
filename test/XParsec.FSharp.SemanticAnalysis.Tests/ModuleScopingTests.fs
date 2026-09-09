module XParsec.FSharp.SemanticAnalysis.Tests.ModuleScopingTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// A bare name that reaches no declaration in scope must not resolve. Only the VERDICT is
/// pinned, not the wording: F# reports these as FS0039 ("The type 'T' is not defined").
let private expectRejected (source: string) =
    let es = semErrors source
    Expect.isNonEmpty es "expected a diagnostic: the name does not resolve in scope here"

/// Selected by the declaration's own HOLDER and its own ARITY: two sibling `T`s, or two `T`s
/// of different arity, are two different types that a bare name cannot tell apart.
let private typeDeclKeyInArity (tast: TastFile) (moduleName: string) (typeName: string) (arity: int) : SymbolKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Key with
                    | SymbolKey.Type {
                                         Container = TypeContainer.InModule m
                                         TyparArity = a
                                     } when m.Name = moduleName && a = KeyArity.Compiled(TyparIndex.typeSlot arity) ->
                        yield td.Key
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ k ] -> k
    | other -> failtestf "expected one '%s'/%d held by module '%s', got %A" typeName arity moduleName other

/// The `SymbolKey` of the NON-GENERIC type `typeName` declared in the module `moduleName`.
let private typeDeclKeyIn (tast: TastFile) (moduleName: string) (typeName: string) : SymbolKey =
    typeDeclKeyInArity tast moduleName typeName 0

/// Matched on the whole `outer.inner` chain: a sibling module of the same short name as
/// `inner` may declare the same type name, and only the chain tells the two apart.
let private typeDeclKeyInNested (tast: TastFile) (outer: string) (inner: string) (typeName: string) : SymbolKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Key with
                    | SymbolKey.Type {
                                         Container = TypeContainer.InModule {
                                                                                Name = i
                                                                                Container = ModuleContainer.InModule o
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
                    | SymbolKey.Type {
                                         Container = TypeContainer.InNamespace _
                                     } -> yield td.Key
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

/// The ARGUMENT type of the file's sole module-level `let` — for `let f (v: T) = v`, what
/// the written `T` bound to. Every program below writes exactly one `let`.
let private soleLetArg (tast: TastFile) : SemType =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Let(binding = m) -> yield m.Ty
                | _ -> ()
        ]

    match found with
    | [ SemType.TyFun(arg, _) ] -> arg
    | other -> failtestf "expected exactly one module-level let of function type, got %A" other

let private src (lines: string list) = String.concat "\n" lines

// A bare type name resolves INNERMOST-OUTWARD: the use's own module, the `open`s in that
// scope, then each enclosing module, then the namespace. A SIBLING module contributes nothing
// without an `open` (F#: FS0039). Each test pins the RESOLVED IDENTITY, not acceptance.
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
                    analyseSem (
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

                expectCleanTast tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "T bound to A's T"
            }

            test "a namespace-level type is visible inside a module of that namespace, with no `open`" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInNamespace tast "T")
                    "T bound to the namespace-held T — the namespace is an ancestor scope"
            }

            // Innermost wins. The control is the SAME program with the inner declaration
            // removed, so the shadowed case is pinned against the type it would otherwise
            // have bound to.
            test "a nested module's own type shadows the enclosing module's" {
                let shadowed =
                    analyseSem (
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

                expectCleanTast shadowed

                Expect.equal
                    (nominalKey (soleLetArg shadowed))
                    (typeDeclKeyIn shadowed "Inner" "T")
                    "the inner T shadows the outer one"

                let unshadowed =
                    analyseSem (
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

                expectCleanTast unshadowed

                Expect.equal
                    (nominalKey (soleLetArg unshadowed))
                    (typeDeclKeyIn unshadowed "Outer" "T")
                    "without an inner T the same use binds to the enclosing module's"
            }

            // An `open` is added to the name environment where it is WRITTEN, and the last
            // thing added wins.
            test "an `open` beats an enclosing module's declaration" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "the `open` outranks Outer's T"
            }

            // Two `open`s both offering a `T`: the LAST one silently wins. Deliberate
            // shadowing is idiomatic F#, so there is no warning to emit — only a binding.
            test "the last of two `open`s wins" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "B" "T") "the later `open B` wins"
            }

            // Within ONE scope a declaration and an `open` are ordered by nothing but the
            // text: the last one written is what the name means. (Probed against `dotnet
            // fsi`: with `type T` then `open A`, `typeof<T>` is A's T.)
            test "in one scope, a declaration and an `open` are ordered by the text" {
                let openThenDecl =
                    analyseSem (
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

                expectCleanTast openThenDecl

                Expect.equal
                    (nominalKey (soleLetArg openThenDecl))
                    (typeDeclKeyIn openThenDecl "B" "T")
                    "the declaration is written after the `open`, so it wins"

                let declThenOpen =
                    analyseSem (
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

                expectCleanTast declThenOpen

                Expect.equal
                    (nominalKey (soleLetArg declThenOpen))
                    (typeDeclKeyIn declThenOpen "A" "T")
                    "the `open` is written after the declaration, so it wins"
            }
        ]

// A type outside the module holding it is reached by NAMING that module; the qualifier is
// resolved from the use — its own scopes innermost-first, the `open`s in force, or the root.
// Every program below declares a SECOND `T` in a sibling module, so nothing passes by luck.
[<Tests>]
let qualifiedTests =
    testList
        "ModuleScoping qualified"
        [
            test "a sibling module's type is reached by qualifying with its module" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyIn tast "A" "T")
                    "A.T is A's T — not the B.T that a bare T would have named"
            }

            test "a fully-qualified path resolves to the type from anywhere in the file" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "N.A.T is A's T"
            }

            // `N` is not an ancestor scope of the use, so the path is resolved from the ROOT.
            test "a fully-qualified path resolves to the type from ANOTHER namespace" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast
                Expect.equal (nominalKey (soleLetArg tast)) (typeDeclKeyIn tast "A" "T") "N.A.T is A's T"
            }

            test "a nested module's type is reached through the path to it" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast

                // The path is walked from its ANCHOR, so the `B` in `A.B` is the one A holds —
                // not the sibling module B, whose own `T` is a different type entirely.
                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInNested tast "A" "B" "T")
                    "A.B.T is the T of the B nested in A"
            }

            // Arity is part of the claim, so it is part of what a qualified name selects: `A`
            // holds BOTH a `T` and a `T<'a>`, and the written name picks one of them.
            test "a qualified generic name resolves at its written arity" {
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

                let tast = analyseSem source
                expectCleanTast tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInArity tast "A" "T" 1)
                    "A.T<int> is A's generic T, not its non-generic one and not B's"

                match soleLetArg tast with
                | SemType.TyRecord(_, args) ->
                    Expect.equal (Block.toList args) [ BuiltinTypes.tyInt ] "the written type argument is applied"
                | other -> failtestf "expected a record, got %A" other

                // The same path at the OTHER arity resolves to the other type.
                let nonGeneric =
                    analyseSem (
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

                expectCleanTast nonGeneric

                Expect.equal
                    (nominalKey (soleLetArg nonGeneric))
                    (typeDeclKeyInArity nonGeneric "A" "T" 0)
                    "A.T names A's non-generic T"
            }

            // An `open` qualifies a PARTIAL path: `open N` + `A.T` resolves to `N.A.T`, exactly as
            // it brings `N`'s own types into scope bare. (Probed against `dotnet fsi`.)
            test "an `open` qualifies a partial path" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast

                // `M` also holds an `A.T` and is an ancestor scope of the use, so the `open N`
                // — written deeper and later — must outrank it.
                match soleLetArg tast with
                | SemType.TyRecord(k, _) -> Expect.equal k.Namespace.Dotted "N" "the `open N` qualifies A.T to N.A.T"
                | other -> failtestf "expected a record, got %A" other
            }

            // `Vesper.Collections.seq` is a real external type (the contract's `seq`
            // interface), and the local module chain of the same spelling shadows it. Probed:
            // the nearest scope that can resolve the qualifier wins, here the enclosing namespace.
            test "a project-local qualified type beats an external type of the same spelling" {
                let tast =
                    analyseSem (
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

                expectCleanTast tast

                Expect.equal
                    (nominalKey (soleLetArg tast))
                    (typeDeclKeyInArity tast "Collections" "seq" 1)
                    "the local Vesper.Collections.seq wins over the external one"
            }

            // A name a scope OF THIS UNIT does not hold is a DIAGNOSTIC: we know every type
            // our own scopes hold, and a fresh type variable would unify with anything. Under
            // a qualifier we do NOT declare, the provider resolves it, and its view is partial.
            test "a name a module of this file does not hold is not defined" {
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

            // The name denotes A's `T` at an arity A does not hold it at, so the diagnostic
            // reports the ARITY — a local claim is not abandoned for an external type over a
            // bad arity.
            test "a qualified name at the wrong arity reports the arity" {
                let es =
                    semErrors (
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

                Expect.isNonEmpty es "expected an arity diagnostic"

                Expect.isTrue
                    (es |> List.exists (fun e -> e.Contains "A.T" && e.Contains "type argument"))
                    (sprintf "expected the arity of A.T to be reported; got %A" es)
            }
        ]
