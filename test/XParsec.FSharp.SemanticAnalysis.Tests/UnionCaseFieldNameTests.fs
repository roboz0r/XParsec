module XParsec.FSharp.SemanticAnalysis.Tests.UnionCaseFieldNameTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    lexed, Pipeline.analyseSemFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

/// One FS3176 report as `(name, clash, at)`, where `at` is the character offset of the
/// reported token in `source`.
let private clashes (source: string) =
    let lexed, tast = analyse source

    [
        for d in tast.Diagnostics do
            match d.Kind, d.Site with
            | Kind.UnionCaseFieldNameClash(name, clash), Site.At i -> yield name, clash, lexed.Tokens.[i].StartIndex
            | Kind.UnionCaseFieldNameClash _, site -> failtestf "FS3176 sited at %A rather than a token" site
            | _ -> ()
    ]

let private accepts (source: string) =
    let _, tast = analyse source

    Expect.isEmpty
        [
            for d in tast.Diagnostics do
                if Diagnostic.isError d then
                    yield d.Message
        ]
        "no error"

/// The character offset of the `n`th occurrence of `needle` in `source`, counted from 1.
let private nth (n: int) (needle: string) (source: string) : int =
    let mutable at = -1

    for _ in 1..n do
        at <- source.IndexOf(needle, at + 1)

    at

// fsc refuses two fields of one case claiming a single logical name under FS3176, in two
// sentences and at most once per case. The rows below are pinned against `dotnet fsi`.
[<Tests>]
let tests =
    testList
        "UnionCaseFieldName"
        [
            test "two declared names agreeing are a clash at the earlier one" {
                let source = "type T = | M of aa: int * bb: float * aa: string"

                Expect.equal
                    (clashes source)
                    [ "aa", UnionFieldNameClash.Declared, nth 1 "aa" source ]
                    "the repeated name, once, at its first occurrence"
            }

            // `Item2` is what the second field of a three-or-more-field case is called, so a
            // field DECLARED `Item2` beside an anonymous one at position 2 claims it twice.
            test "a declared name that is an anonymous field's position is a clash at the declared name" {
                let source = "type T = | M of Item2: int * float"

                Expect.equal
                    (clashes source)
                    [ "Item2", UnionFieldNameClash.AnonymousSpelling, nth 1 "Item2" source ]
                    "the declared field claims the anonymous one's spelling"

                let source = "type T = | M of int * Item1: float"

                Expect.equal
                    (clashes source)
                    [ "Item1", UnionFieldNameClash.AnonymousSpelling, nth 1 "Item1" source ]
                    "the anonymous field's spelling is claimed after it"
            }

            // fsc reports once per case: a repeated declared name first, at its first
            // occurrence, else the first declared name spelled like an anonymous field.
            test "a case reports at most once, a repeated declared name first" {
                let source = "type T = | M of a: int * a: int * a: int"

                Expect.equal
                    (clashes source)
                    [ "a", UnionFieldNameClash.Declared, nth 1 "a:" source ]
                    "three claims on one name report once"

                let source = "type T = | M of Item2: int * float * Item2: string"

                Expect.equal
                    (clashes source)
                    [ "Item2", UnionFieldNameClash.Declared, nth 1 "Item2" source ]
                    "the repeat wins over the anonymous spelling it also claims"

                let source = "type T = | M of int * Item1: float * Item1: string"

                Expect.equal
                    (clashes source)
                    [ "Item1", UnionFieldNameClash.Declared, nth 1 "Item1" source ]
                    "the repeat wins, at the earlier declared name"

                let source = "type T = | M of a: int * b: int * b: int * a: int"

                Expect.equal
                    (clashes source)
                    [ "a", UnionFieldNameClash.Declared, nth 1 "a:" source ]
                    "the first-declared repeated name reports"

                let source = "type T = | M of int * Item1: float * a: int * a: string"

                Expect.equal
                    (clashes source)
                    [ "a", UnionFieldNameClash.Declared, nth 1 "a:" source ]
                    "a repeat later in the case wins over an earlier anonymous spelling"
            }

            test "a clash is reported per case" {
                let source =
                    "type T =\n    | M of int * Item1: string\n    | N of q: int * q: float"

                Expect.equal
                    (clashes source)
                    [
                        "Item1", UnionFieldNameClash.AnonymousSpelling, nth 1 "Item1" source
                        "q", UnionFieldNameClash.Declared, nth 1 "q:" source
                    ]
                    "both cases report"
            }

            test "a `[<Struct>]` union's cases are checked the same way" {
                let source = "[<Struct>]\ntype T = | M of Item2: int * float"

                Expect.equal
                    (clashes source)
                    [ "Item2", UnionFieldNameClash.AnonymousSpelling, nth 1 "Item2" source ]
                    "the attribute is irrelevant to the naming rule"
            }

            // A positional name is only claimed where an anonymous field actually takes it,
            // and the position counts EVERY field, so most `Item<n>` spellings are free.
            test "a declared `Item<n>` no anonymous field takes is accepted" {
                // A lone anonymous field is `Item`, never `Item2`.
                accepts "type T = | M of Item2: int"
                // The anonymous field at position 2 is `Item2`.
                accepts "type T = | M of Item: int * float"
                // Every field is declared, so no positional spelling is in play.
                accepts "type T = | M of a: int * Item1: float"
                // The anonymous fields are at positions 1 and 2.
                accepts "type T = | M of int * int * Item3: float"
            }

            // fsc's FS3585 rejects a struct union's same-name fields at differing types. This
            // compiler accepts them: each case owns its own placements, so the two never share a slot.
            test "one name across two cases is accepted at differing types" {
                accepts "[<Struct>]\ntype T = | A of x: int | B of x: string"
                accepts "type T = | A of int | B of string"
            }

            // The union's `Get_<Case>_<i>` readers and its `GetPayload_<Case>` view accessors
            // are two families, so a case whose name ends in the positional spelling of
            // another compiles.
            test "a case named as another case's reader index is accepted" {
                accepts "[<Struct>]\ntype T = | X_0 of int | X of int * int"
            }
        ]
