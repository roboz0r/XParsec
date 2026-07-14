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

/// The `SymbolKey` of the type `typeName` declared in the module `moduleName` — selected by
/// the declaration's own HOLDER, because the whole point here is that two sibling `T`s are
/// two types and a name cannot tell them apart.
let private typeDeclKeyIn (tast: TastFile) (moduleName: string) (typeName: string) : SymbolKey =
    let found =
        [
            for d in tast.Decls do
                match d with
                | TDecl.Type td when td.Name = typeName ->
                    match td.Key with
                    | SymbolKey.Type { Holder = TypeHolder.InModule m } when m.Name = moduleName -> yield td.Key
                    | _ -> ()
                | _ -> ()
        ]

    match found with
    | [ k ] -> k
    | other -> failtestf "expected one '%s' held by module '%s', got %A" typeName moduleName other

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
    | SemType.TyUnion(k, _) -> k
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
