module XParsec.FSharp.SemanticAnalysis.Tests.SameNameResolutionTests

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

let private expectClean (source: string) =
    let es = errors (analyse source)
    Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

let private expectErrorIn (es: string list) (needle: string) =
    Expect.isTrue
        (es |> List.exists (fun m -> m.Contains needle))
        (sprintf "expected an error containing '%s'; diagnostics were %A" needle es)

let private expectError (needle: string) (source: string) =
    expectErrorIn (errors (analyse source)) needle

/// `expectError`, and the diagnostic must be a user-facing one: an unresolved name that
/// reaches the freeze as a stray `TyVar` trips the internal backstop instead.
let private expectUserError (needle: string) (source: string) =
    let es = errors (analyse source)
    expectErrorIn es needle
    Expect.isFalse (es |> List.exists (fun m -> m.Contains "internal compiler error")) "no internal error"

// A type name is claimed at an ARITY: one module may hold `T` and `T<'a>` at once, and a bare
// name reaches an outer arity-0 claim past a nearer generic one. Within one arity the max-rank
// claim wins before its KIND is read, so a class shadows a same-arity record out of reach.
//
// Each case's F# verdict is quoted under its FS code, pinned with `dotnet fsi`.
[<Tests>]
let tests =
    testList
        "SameNameResolution"
        [
            testList
                "arity is part of the claim"
                [
                    test "one module declares `T` and `T<'a>` at once" {
                        expectClean
                            "\
type T = { X: int }
type T<'a> = { Y: 'a }
let a: T = { X = 1 }
let b: T<int> = { Y = 2 }
"
                    }

                    test "a bare name reaches an outer arity-0 type past a nearer generic one" {
                        expectClean
                            "\
type T = { X: int }

module N =
    type T<'a> = { Y: 'a }

    let f (t: T) = t.X
"
                    }

                    test "a bare name reaches an outer arity-0 type past a nearer generic CLASS" {
                        expectClean
                            "\
type T = { X: int }

module N =
    type T<'a>(x: 'a) =
        member _.Y = x

    let f (t: T) = t.X
"
                    }

                    test "a nearer arity-0 type wins over an outer generic one" {
                        expectClean
                            "\
type T<'a> = { Y: 'a }

module N =
    type T = { X: int }

    let f (t: T) = t.X
"
                    }

                    test "an arity-0 claim outranks a later `open`'s generic one" {
                        expectClean
                            "\
module A =
    type T = { X: int }

module B =
    type T<'a> = { Y: 'a }

module N =
    open A
    open B

    let f (t: T) = t.X
"
                    }
                ]

            testList
                "a bare generic name in TYPE position is an error"
                [
                    // FS0033: "The type 'T<_>' expects 1 type argument(s) but is given 0". The
                    // type argument is NOT inferred from the annotation's context. The
                    // annotation is currently accepted, reporting nothing.
                    ptest "GAP: a bare generic name in an annotation is an error" {
                        expectUserError
                            "expects 1 type argument"
                            "\
module A =
    type T<'a> = { Y: 'a }

module N =
    open A

    let f (t: T) = t.Y
"
                    }

                    // FS0033 again, against the best-ranked candidate: several arities in scope
                    // and none of them 0 is still a missing-argument error, not an ambiguity.
                    // The annotation is currently accepted, reporting nothing.
                    ptest "GAP: two generic arities and no arity-0 claim is an error" {
                        expectUserError
                            "expects 2 type argument"
                            "\
module A =
    type T<'a> = { Y: 'a }

module B =
    type T<'a, 'b> = { P: 'a; Q: 'b }

module N =
    open A
    open B

    let f (t: T) = t
"
                    }

                    // FS0033 at a type ARGUMENT: no type position exempts a bare generic name,
                    // `inherit`, `typeof<>` and `interface ... with` included.
                    ptest "GAP: a bare generic name as a type argument is an error" {
                        expectUserError
                            "expects 1 type argument"
                            "\
type C<'a>(x: 'a) =
    member _.X = x

let f (xs: C list) = List.length xs
"
                    }

                    // FS0033 in an `inherit` clause: the base's value arguments do NOT supply
                    // its type arguments.
                    ptest "GAP: a bare generic name in an `inherit` clause is an error" {
                        expectUserError
                            "expects 1 type argument"
                            "\
type B<'a>(x: 'a) =
    member _.X = x

type D(y: int) =
    inherit B(y)
"
                    }
                ]

            testList
                "a bare name in EXPRESSION position takes the candidates' agreed arity"
                [
                    test "one generic arity in scope constructs from the argument" {
                        expectClean
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.X = x

module N =
    open A

    let v = T(1)
"
                    }

                    // The arity-0 claim wins outright and the value argument is checked against
                    // its `unit` constructor, with no fallback to the generic claim. F# refuses
                    // it as FS0501, "The object constructor 'T' takes 0 argument(s) but is here
                    // given 1".
                    test "an arity-0 claim wins and is not backtracked when its ctor mismatches" {
                        expectError
                            "Type mismatch: int vs unit"
                            "\
type T() =
    member _.X = 0

type T<'a>(x: 'a) =
    member _.X = x

let v = T(1)
"
                    }

                    // FS1124: "Multiple types exist called 'T', taking different numbers of
                    // generic parameters. Provide a type instantiation to disambiguate." The
                    // name currently resolves to nothing silently, and the unresolved type
                    // reaches the freeze as a stray `TyVar`.
                    ptest "GAP: generic claims that disagree on arity report an ambiguity" {
                        expectUserError
                            "Multiple types"
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.X = x

module B =
    type T<'a, 'b>(x: 'a, y: 'b) =
        member _.X = x

module N =
    open A
    open B

    let v = T(1)
"
                    }

                    // A static access supplies the instantiation from the argument. Where it
                    // cannot be inferred F# warns (FS1125) and still resolves, so a bare
                    // generic name is never an error here.
                    test "a static member resolves off a bare generic name" {
                        expectClean
                            "type C<'a>(x: 'a) =
    member _.X = x
    static member Make(v: 'a) = C v

let v = (C.Make 5).X
"
                    }

                    test "a union case of a generic union matches from a bare name" {
                        expectClean
                            "type U<'a> =
    | A of 'a
    | B

let f (u: U<int>) =
    match u with
    | A n -> n
    | B -> 0
"
                    }
                ]

            testList
                "the max-rank claim wins before its KIND is read"
                [
                    test "a nearer class shadows an outer same-arity record" {
                        expectClean
                            "\
type T = { X: int }

module N =
    type T() =
        member _.Y = 42

    let f (t: T) = t.Y
"
                    }

                    // FS0039: "The type 'T' does not define the field, constructor or member
                    // 'X'". The shadowed record is out of reach, not merely outranked.
                    test "a shadowed record is unreachable by nesting" {
                        expectError
                            "has no instance member 'X'"
                            "\
type T = { X: int }

module N =
    type T() =
        member _.Y = 42

    let f (t: T) = t.X
"
                    }

                    // FS0039 again: the shadowing is by RANK, so the last `open` shadows just
                    // as a nearer declaration does.
                    test "a shadowed record is unreachable through a later `open`" {
                        expectError
                            "has no instance member 'X'"
                            "\
module A =
    type T = { X: int }

module B =
    type T() =
        member _.Y = 42

module N =
    open A
    open B

    let f (t: T) = t.X
"
                    }
                ]
        ]
