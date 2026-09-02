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

/// `expectErrorIn`, and the diagnostic must be a user-facing one: an unresolved name that
/// reaches the freeze as a stray `TyVar` trips the internal backstop instead.
let private expectUserErrorIn (es: string list) (needle: string) =
    expectErrorIn es needle
    Expect.isFalse (es |> List.exists (fun m -> m.Contains "internal compiler error")) "no internal error"

let private expectUserError (needle: string) (source: string) =
    expectUserErrorIn (errors (analyse source)) needle

/// `expectUserError`, and exactly one diagnostic matches.
let private expectOneUserError (needle: string) (source: string) =
    let es = errors (analyse source)
    expectUserErrorIn es needle

    Expect.equal
        (es |> List.filter (fun m -> m.Contains needle) |> List.length)
        1
        (sprintf "'%s' reported once; diagnostics were %A" needle es)

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
                    // type argument is NOT inferred from the annotation's context.
                    test "a bare generic name in an annotation is an error" {
                        expectOneUserError
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
                    test "two generic arities and no arity-0 claim is an error" {
                        expectOneUserError
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
                    test "a bare generic name as a type argument is an error" {
                        expectOneUserError
                            "expects 1 type argument"
                            "\
type C<'a>(x: 'a) =
    member _.X = x

let f (xs: C list) = List.length xs
"
                    }

                    // FS0033 in an `inherit` clause: the base's value arguments do NOT supply
                    // its type arguments.
                    test "a bare generic name in an `inherit` clause is an error" {
                        expectOneUserError
                            "expects 1 type argument"
                            "\
type B<'a>(x: 'a) =
    member _.X = x

type D(y: int) =
    inherit B(y)
"
                    }

                    // A WRITTEN arity that no claim on the name holds is the same FS0033, and
                    // both NameResolution and the type translation reach it.
                    test "a written arity no claim holds is reported once" {
                        expectOneUserError
                            "expects 2 type argument"
                            "\
type T<'a, 'b> = { P: 'a; Q: 'b }

let f (t: T<int>) = t
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

            // A WRITTEN arity in expression position is FS0033 exactly as in type position, but
            // NameResolution defers it to Unification, which reaches it only through a nominal
            // result or a scheme. A ctor and a static member give one; an enum-case or
            // union-case qualifier gives neither, so nothing is reported and the unpinned
            // TyVars reach the freeze as an internal error.
            testList
                "a WRITTEN arity in EXPRESSION position is checked against the claim"
                [
                    test "a ctor at a written arity no claim holds is an error" {
                        expectOneUserError
                            "expects 1 type argument"
                            "\
type C<'a>(x: 'a) =
    member _.X = x

let c = C<int, string>(1)
"
                    }

                    test "a static member at a written arity no claim holds is an error" {
                        expectOneUserError
                            "expects 1 type argument"
                            "\
type C<'a>() =
    static member M = 1

let x = C<int, string>.M
"
                    }

                    // FS0033: "The non-generic type 'E' does not expect any type arguments, but
                    // here is given 1 type argument(s)".
                    ptest "GAP: an enum qualifier at a written arity is unreported" {
                        expectOneUserError
                            "expects 0 type argument"
                            "\
type E =
    | A = 1

let x = E<int>.A
"
                    }

                    // FS0033: "The type 'U<_>' expects 1 type argument(s) but is given 2".
                    ptest "GAP: a union-case qualifier at a written arity is unreported" {
                        expectOneUserError
                            "expects 1 type argument"
                            "\
type U<'a> =
    | Case of 'a

let u = U<int, string>.Case 1
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
