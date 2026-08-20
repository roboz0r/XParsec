module XParsec.FSharp.SemanticAnalysis.Tests.DuplicateTypeNameTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (LexedFile.ofText lexed) file

/// `analyse`, naming the unit being compiled. The assembly name is in no `SymbolKey`; it
/// identifies the UNIT, which is what the own-contract exemption below turns on.
let private analyseAs (assemblyName: string) (input: string) =
    let lexed, file = parseFile input

    Pipeline.analyseSemFor { Name = assemblyName; Target = "clr" } realProvider.Value (LexedFile.ofText lexed) file

let private errors (tast: TastFile) =
    [
        for d in tast.Diagnostics do
            if Diagnostic.isError d then
                yield d.Message
    ]

let private has (tast: TastFile) (s: string) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains s)

/// A duplicate is a plain user diagnostic, never the internal key-collision backstop: that
/// backstop witnesses a mint that dropped part of `(container, name, arity)`.
let private expectDuplicate (source: string) =
    let tast = analyse source
    Expect.isTrue (has tast "Duplicate type definition") "duplicate-type diagnostic emitted"
    Expect.isFalse (has tast "Internal error") "no internal SymbolKey-collision error"

let private expectNoDuplicate (source: string) =
    let tast = analyse source
    Expect.isFalse (has tast "Duplicate type definition") "no duplicate-type diagnostic"
    Expect.isFalse (has tast "Internal error") "no internal SymbolKey-collision error"

// One declaration claims one NAME at one ARITY in the MODULE that holds it, and a claim may
// be held by at most one type of any kind.
[<Tests>]
let tests =
    testList
        "DuplicateTypeName"
        [
            // Every ordered kind-pair: the second declaration in source order loses, and
            // which kinds are involved is irrelevant to the verdict.
            for first, second, source in
                [
                    "record", "record", "type T = { a: int }\ntype T = { b: int }"
                    "record", "union", "type T = { a: int }\ntype T = A | B"
                    "record", "enum", "type T = { a: int }\ntype T = | A = 1"
                    "record", "abbrev", "type T = { a: int }\ntype T = int"
                    "record", "class", "type T = { a: int }\ntype T() =\n    member this.X = 1"
                    "union", "record", "type T = A | B\ntype T = { a: int }"
                    "union", "enum", "type T = A | B\ntype T = | A = 1"
                    "union", "abbrev", "type T = A | B\ntype T = int"
                    "union", "class", "type T = A | B\ntype T() =\n    member this.X = 1"
                    "enum", "record", "type T = | A = 1\ntype T = { a: int }"
                    "enum", "union", "type T = | A = 1\ntype T = B | C"
                    "enum", "abbrev", "type T = | A = 1\ntype T = int"
                    "enum", "class", "type T = | A = 1\ntype T() =\n    member this.X = 1"
                    "abbrev", "record", "type T = int\ntype T = { a: int }"
                    "abbrev", "enum", "type T = int\ntype T = | A = 1"
                    "abbrev", "abbrev", "type T = int\ntype T = bool"
                    "abbrev", "class", "type T = int\ntype T() =\n    member this.X = 1"
                    "class", "record", "type T() =\n    member this.X = 1\ntype T = { a: int }"
                    "class", "enum", "type T() =\n    member this.X = 1\ntype T = | A = 1"
                    "class", "class", "type T() =\n    member this.X = 1\ntype T() =\n    member this.Y = 2"
                ] -> test $"{second} after {first} collides" { expectDuplicate source }

            // An intrinsic binding declares a NAME in the type namespace, so it collides with
            // every kind at its declared arity. Its repr string is not a name-table concern —
            // two types sharing a repr is an identity question, not a duplicate name.
            for kind, source in
                [
                    "record", "type widget = (# \"System.Int32\" #)\ntype widget = { a: int }"
                    "union", "type widget = (# \"System.Int32\" #)\ntype widget = A | B"
                    "enum", "type widget = (# \"System.Int32\" #)\ntype widget = | A = 1"
                    "class", "type widget = (# \"System.Int32\" #)\ntype widget() =\n    member this.X = 1"
                    "abbrev", "type widget = (# \"System.Int32\" #)\ntype widget = int"
                    "intrinsic", "type widget = (# \"System.Int32\" #)\ntype widget = (# \"System.Int64\" #)"
                ] -> test $"{kind} after intrinsic-repr alias collides" { expectDuplicate source }

            // Arity overloading is across the board, as in F#: `Foo` and `Foo`1` are distinct
            // claims and may be held by different kinds, abbreviations and intrinsics included.
            for kinds, source in
                [
                    "non-generic enum, generic record", "type E = | A = 1\ntype E<'a> = { X: 'a }"
                    "non-generic record, generic abbrev", "type Foo = { X: int }\ntype Foo<'a> = 'a"
                    "generic abbrev, non-generic record", "type Foo<'a> = 'a\ntype Foo = { X: int }"
                    "non-generic enum, generic abbrev", "type Foo = | A = 1\ntype Foo<'a> = 'a"
                    "non-generic class, generic union", "type C() = class end\ntype C<'a> = | A of 'a"
                    "non-generic record, generic intrinsic",
                    "type widget<'a> = (# \"System.Int32\" #)\ntype widget = { X: int }"
                    "generic abbrev, generic abbrev of another arity", "type Foo<'a> = 'a\ntype Foo<'a, 'b> = 'a * 'b"
                ] -> test $"{kinds} of the same name coexist" { expectNoDuplicate source }

            // The FIRST declaration to claim `(name, arity)` owns it; the duplicate registers
            // nothing, so its `with member Bar` does not leak onto the owner. `v.Bar()` then
            // fails ordinary lookup: a member-not-found diagnostic beside the duplicate one.
            yield
                test "detail of a rejected duplicate does not leak onto the claim's owner" {
                    let tast =
                        analyse
                            "type Foo = { a: int }\ntype Foo = { a: int } with\n    member this.Bar() = 1\nlet f (v: Foo) = v.Bar()"

                    let errors =
                        [
                            for d in tast.Diagnostics do
                                if Diagnostic.isError d then
                                    yield d.Message
                        ]

                    Expect.isTrue
                        (errors |> List.exists (fun m -> m.Contains "Duplicate type definition"))
                        "the second Foo is rejected"

                    Expect.isTrue
                        (errors
                         |> List.exists (fun m -> m.StartsWith "Type 'Foo' has no" && m.Contains "'Bar'"))
                        (sprintf "`Bar` is not on the surviving Foo, and says so; diagnostics were %A" errors)

                    Expect.isFalse (has tast "Internal error") "no internal SymbolKey-collision error"
                }

            yield
                test "a bare name resolves to the type CLAIMING it at arity 0, not a generic alias" {
                    // The `(Foo, 0)` claim decides, and the record holds it. The generic alias
                    // is a FUNCTION type, so were it picked instead, `v.X` could not type.
                    let tast =
                        analyse "type Foo = { X: int }\ntype Foo<'a> = 'a -> 'a\nlet f (v: Foo) = v.X"

                    Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) "bare `Foo` is the record, so `v.X` types"
                }

            // A `SymbolKey` carries no home assembly, licensed by one fact: within a
            // compilation a fully-qualified name names at most one type. The `List`1` below is
            // already Vesper.List's, so re-declaring it is an error, not a silent shadow.
            yield
                test "a type a referenced assembly already claims is an error citing both" {
                    let src =
                        "namespace Vesper.Collections\n\ntype List<'T> =\n    | Nil\n    | Cons of 'T * List<'T>"

                    let msgs = errors (analyseAs "MyApp" src)

                    let collision =
                        msgs
                        |> List.tryFind (fun m -> m.Contains "Vesper.Collections.List`1" && m.Contains "Vesper.List")

                    Expect.isSome
                        collision
                        (sprintf "the collision is diagnosed, naming type and assembly; got %A" msgs)
                }

            // A unit's OWN contract is not a "referenced assembly": compiling `Vesper.List`
            // against a stack mounting its own `.fsi` means declaring the types it publishes.
            // Same source and provider as above — only the unit's identity differs.
            yield
                test "the unit that OWNS the contract may declare the types it publishes" {
                    let src =
                        "namespace Vesper.Collections\n\ntype List<'T> =\n    | Nil\n    | Cons of 'T * List<'T>"

                    let msgs = errors (analyseAs "Vesper.List" src)

                    Expect.isFalse
                        (msgs
                         |> List.exists (fun m -> m.Contains "already exists in the referenced assembly"))
                        (sprintf "no collision against its own contract; got %A" msgs)
                }

            // The claim is `(container, name, arity)`, so the MODULE is part of it: sibling
            // modules each declaring `T` declare two types, not one name twice.
            yield
                test "sibling modules may each declare the same type name — different containers, different claims" {
                    let src =
                        "namespace N\n\nmodule A =\n    type T = { X: int }\n\nmodule B =\n    type T = { Y: int }"

                    let msgs = errors (analyseAs "MyApp" src)

                    Expect.isFalse
                        (msgs |> List.exists (fun m -> m.Contains "Duplicate type definition: T"))
                        (sprintf "`N.A.T` and `N.B.T` are two types; got %A" msgs)

                    Expect.isEmpty msgs (sprintf "no diagnostics at all; got %A" msgs)
                }

            yield
                test "a second declaration of the name in the SAME module is still a duplicate" {
                    expectDuplicate "namespace N\n\nmodule A =\n    type T = { X: int }\n    type T = { Y: int }"
                }
        ]
