module XParsec.FSharp.SemanticAnalysis.Tests.DuplicateTypeNameTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

/// `analyse`, naming the unit being compiled. The assembly name is not part of any
/// `SymbolKey`; it identifies the UNIT, which is what the "a unit may declare the types
/// its own contract publishes" exemption is a statement about.
let private analyseAs (assemblyName: string) (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSemFor assemblyName realProvider.Value input lexed file

let private errors (tast: TastFile) =
    [
        for d in tast.Diagnostics do
            if Diagnostic.isError d then
                yield d.Message
    ]

let private has (tast: TastFile) (s: string) =
    tast.Diagnostics |> Seq.exists (fun d -> d.Message.Contains s)

/// A duplicate is a plain user diagnostic, never the `stampLocalTypeKey` collision
/// backstop — that branch witnesses a MINT that dropped something the claim kept, and the
/// claim is `(holder, name, arity)`, which is exactly what the key is minted from.
let private expectDuplicate (source: string) =
    let tast = analyse source
    Expect.isTrue (has tast "Duplicate type definition") "duplicate-type diagnostic emitted"
    Expect.isFalse (has tast "Internal error") "no internal SymbolKey-collision error"

let private expectNoDuplicate (source: string) =
    let tast = analyse source
    Expect.isFalse (has tast "Duplicate type definition") "no duplicate-type diagnostic"
    Expect.isFalse (has tast "Internal error") "no internal SymbolKey-collision error"

// One declaration claims one NAME at one ARITY in the MODULE that holds it, and a claim may
// be held by at most one type of ANY kind (`TypeRegistry.TypeClaims`). Every pair below is
// the same predicate over the same table, so a kind added later cannot be wired into some
// guards and forgotten in others — the drift that once let an `enum` name be silently
// re-declared as an abbreviation or a class.
[<Tests>]
let tests =
    testList
        "DuplicateTypeName"
        [
            // Every ordered kind-pair. Source order is what matters now (the identity pass
            // walks the file in order); the KINDS involved are irrelevant to the verdict.
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

            // An intrinsic binding declares a NAME in the type namespace, so that name is a
            // name-table citizen like any other type's — it collides with every kind, at its
            // declared arity. (Its target-representation string is NOT a name-table concern:
            // two types sharing a repr is an identity/origin question, handled elsewhere.)
            for kind, source in
                [
                    "record", "type widget = (# \"System.Int32\" #)\ntype widget = { a: int }"
                    "union", "type widget = (# \"System.Int32\" #)\ntype widget = A | B"
                    "enum", "type widget = (# \"System.Int32\" #)\ntype widget = | A = 1"
                    "class", "type widget = (# \"System.Int32\" #)\ntype widget() =\n    member this.X = 1"
                    "abbrev", "type widget = (# \"System.Int32\" #)\ntype widget = int"
                    "intrinsic", "type widget = (# \"System.Int32\" #)\ntype widget = (# \"System.Int64\" #)"
                ] -> test $"{kind} after intrinsic-repr alias collides" { expectDuplicate source }

            // Arity overloading is ACROSS THE BOARD, as in F#: `Foo` and `Foo`1` are distinct
            // claims and may be held by different kinds. Abbreviations and intrinsic bindings
            // are ordinary arity-keyed citizens, not bare-name special cases.
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
            // nothing at all — it never reaches a registrar, so none of its detail (fields,
            // cases, `with member …` augmentation, `inherit`) leaks onto the type that does
            // own the name. A use site naming detail that existed only on the rejected
            // declaration therefore fails ORDINARY lookup, which is the correct answer: a
            // member-not-found diagnostic alongside the duplicate one, not a crash.
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
                    // The reason arity-keying the abbreviation table is the right fix. With a
                    // bare-keyed alias table, `Foo` at a use site resolved through
                    // `Abbreviation` — which the resolution cascade consulted BEFORE `Record` —
                    // and found the GENERIC alias applied to no arguments. Now the `(Foo, 0)`
                    // claim decides, and it belongs to the record. The alias here is a function
                    // type, so were it picked, `v.X` could not type.
                    let tast =
                        analyse "type Foo = { X: int }\ntype Foo<'a> = 'a -> 'a\nlet f (v: Foo) = v.X"

                    Expect.isEmpty (tast.Diagnostics |> Diagnostic.errors) "bare `Foo` is the record, so `v.X` types"
                }

            // THE PREMISE, ENFORCED — the CS0433 analogue. A `SymbolKey` carries no home
            // assembly, which is licensed by exactly one fact: within one compilation a
            // fully-qualified name names at most one type. So a declaration whose key a
            // REFERENCED assembly already answers for is an error, not a silent shadow —
            // without it, codegen would still be right (the local table is checked first)
            // but the unifier would unify two genuinely different types under one key.
            //
            // `Vesper.Collections.List<'T>` is Vesper.List's; `realProvider` references it.
            yield
                test "a type a referenced assembly already claims is an error naming both" {
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
            // against a stack that mounts `Vesper.List`'s own `.fsi` (which is what
            // `SymbolProviders.inlineBodies` does for every package's impl, and what the
            // self-host suites drive) means the unit declares the very types its contract
            // publishes — that is what compiling it MEANS. Same source, same provider, same
            // key: only the identity of the unit differs, and that is what decides.
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

            // The claim is `(holder, name, arity)`, so the MODULE is part of it: sibling
            // modules each declaring `T` declare two types, not one name twice. Only a second
            // `T` in the SAME module contests a claim.
            yield
                test "sibling modules may each declare the same type name — different holders, different claims" {
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
