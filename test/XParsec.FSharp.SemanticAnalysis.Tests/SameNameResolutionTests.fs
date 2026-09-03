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

                    // FS0033 again: several arities in scope and none of them 0 is still a
                    // missing-argument error, not an ambiguity. F# reports the best-RANKED
                    // candidate `B.T<_,_>`, "expects 2"; this compiler reports the NEAREST arity.
                    test "two generic arities and no arity-0 claim reports the nearest arity" {
                        expectOneUserError
                            "expects 1 type argument"
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

                    // A deliberate divergence: F# ranks by scope distance and reports the nearer
                    // `B.T<_>`, "expects 1 type argument(s) but is given 4", whereas
                    // `A.T<_,_,_>` is one arity out and is the likelier intent.
                    test "a wrong written arity reports the nearest arity, not the nearest scope" {
                        expectOneUserError
                            "expects 3 type argument"
                            "\
module A =
    type T<'a, 'b, 'c> = { P: 'a; Q: 'b; R: 'c }

module B =
    type T<'a> = { Y: 'a }

module N =
    open A
    open B

    let f (t: T<int, int, int, int>) = t
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

                    // An `inherit` clause is TYPE position, so an arity disagreement there is
                    // FS0033, never FS1124. F# reports the max-rank claim `B.T<_,_>`, "expects
                    // 2"; this compiler reports the nearest arity, `A.T<_>`.
                    test "an inherit clause under an arity disagreement reports FS0033, not an ambiguity" {
                        expectOneUserError
                            "expects 1 type argument"
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

    type D(y: int) =
        inherit T(y)
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
                    // generic parameters. Provide a type instantiation to disambiguate." F#
                    // reports it alone, so the name recovers to the claim of nearest arity and
                    // the binding still types.
                    test "generic claims that disagree on arity report an ambiguity" {
                        expectOneUserError
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

                    // Two RECORDS at different arities are the same FS1124: the candidate set
                    // is every claim the position admits.
                    test "record claims that disagree on arity report an ambiguity" {
                        expectOneUserError
                            "Multiple types"
                            "\
module A =
    type T<'a> = { X: 'a }

module B =
    type T<'a, 'b> = { Y: 'a; Z: 'b }

module N =
    open A
    open B

    let v = T
"
                    }

                    // The arity-0 record sits outside the candidate set, so it supplies no
                    // arity and the two classes still disagree. F# reports FS1124 here.
                    test "an arity-0 record leaves two disagreeing classes ambiguous" {
                        expectOneUserError
                            "Multiple types"
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.X = x

module B =
    type T<'a, 'b>(x: 'a, y: 'b) =
        member _.X = x

module C =
    type T = { Z: int }

module N =
    open A
    open B
    open C

    let v = T(1)
"
                    }

                    // The qualifier of a static access is the same bare name, so the same
                    // FS1124. F# reports it before the member is searched, so a member only
                    // one candidate declares does not settle the ambiguity.
                    test "a qualified static member under an arity disagreement reports the ambiguity" {
                        expectOneUserError
                            "Multiple types"
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.X = x
        static member M (v: 'a) = T<'a>(v)

module B =
    type T<'a, 'b>(x: 'a, y: 'b) =
        member _.X = x

module N =
    open A
    open B

    let v = T.M 1
"
                    }

                    test "a qualified access on records that disagree on arity reports the ambiguity" {
                        expectOneUserError
                            "Multiple types"
                            "\
module A =
    type T<'a> = { X: 'a }

module B =
    type T<'a, 'b> = { Y: 'a; Z: 'b }

module N =
    open A
    open B

    let v = T.X
"
                    }

                    // The candidate set is the CLASSES: a union at arity 0 supplies nothing, so
                    // the sole class settles the qualifier. F# accepts this.
                    test "a qualified static member past an arity-0 union takes the sole class" {
                        expectClean
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.X = x
        static member M (v: 'a) = T<'a>(v)

module C =
    type T =
        | Q
        | R

module N =
    open A
    open C

    let v = T.M 1
"
                    }

                    // PATTERN position searches every claim for the case and reports no
                    // ambiguity. F# accepts this.
                    test "a qualified case pattern under an arity disagreement matches" {
                        expectClean
                            "\
module A =
    type T<'a> =
        | A of 'a
        | B

module B =
    type T<'a, 'b> =
        | A of 'a
        | C of 'b

module N =
    open A
    open B

    let f x =
        match x with
        | T.A v -> v
        | _ -> 0
"
                    }

                    // A bare name in expression position denotes a CONSTRUCTOR, so the one
                    // class claiming it settles the arity and the same-named record of another
                    // arity is no ambiguity. F# accepts this.
                    test "a lone class claim settles the arity past a record of another arity" {
                        expectClean
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.X = x

module B =
    type T<'a, 'b> = { X: 'a; Y: 'b }

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

            // A WRITTEN arity in expression position is FS0033 exactly as in type position.
            // NameResolution defers it to Unification, where each qualifier form compares the
            // written count against the claim it resolves, so the report lands once.
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
                    test "an enum qualifier at a written arity is an error" {
                        expectOneUserError
                            "expects 0 type argument"
                            "\
type E =
    | A = 1

let x = E<int>.A
"
                    }

                    // FS0033: "The type 'U<_>' expects 1 type argument(s) but is given 2".
                    test "a union-case qualifier at a written arity is an error" {
                        expectOneUserError
                            "expects 1 type argument"
                            "\
type U<'a> =
    | Case of 'a

let u = U<int, string>.Case 1
"
                    }

                    test "a union-case qualifier at the claim's arity resolves" {
                        expectClean
                            "\
type U<'a> =
    | Case of 'a

let u = U<int>.Case 1
"
                    }

                    // `expectError` and not `expectOneUserError`: the folded `U.Nope 1` trips
                    // the freeze backstop too, the `NoCase` error type being a free `TyVar`
                    // the binding then carries.
                    test "a union-case miss through an instantiated qualifier is NoCase" {
                        expectError
                            "Union 'U' has no case 'Nope'"
                            "\
type U<'a> =
    | Case of 'a

let u = U<int>.Nope 1
"
                    }

                    // FS0033 alone, as F# reports it: an enum claims arity 0, and the written
                    // count is checked before the case name.
                    test "an enum-case miss at a written arity is the arity error alone" {
                        expectOneUserError
                            "expects 0 type argument"
                            "\
type E =
    | A = 1

let x = E<int>.Nope
"
                    }

                    // FS0039, reported once by NameResolution as the folded `C.Nope` is.
                    test "a class member miss through an instantiated qualifier is one NoMember" {
                        expectOneUserError
                            "has no value or member 'Nope'"
                            "\
type C<'a>(x: 'a) =
    member _.X = x

let v = C<int>.Nope
"
                    }

                    test "a union static member through an instantiated qualifier resolves ahead of the case search" {
                        expectClean
                            "\
type U<'a> =
    | Case of 'a

    static member Make(x: 'a) : U<'a> = Case x

let u = U<int>.Make 1
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

                    // An `inherit` clause's type ARGUMENT is type position too, so the nearer
                    // class is what `Base<T>` is instantiated at.
                    test "an inherit type argument takes the max-rank claim, not the first kind" {
                        expectClean
                            "\
type Base<'a>(x: 'a) =
    member _.V = x

type T = { X: int }

module N =
    type T() =
        member _.Y = 42

    type D(t: T) =
        inherit Base<T>(t)

    let d = D(T())
    let y = d.V.Y
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

            // The shadowing above is TYPE position's. A bare name in expression position
            // denotes a constructor, so the candidate set is the CLASSES reaching the site and
            // a claim of another kind neither shadows one nor settles the arity.
            testList
                "a constructor is picked among the CLASSES, whatever outranks them"
                [
                    // The same `T`, in the same scope, is the nearer record in type position and
                    // the outer class in expression position.
                    test "a ctor reaches an outer class past a nearer same-arity record" {
                        expectClean
                            "\
type T() =
    member _.Y = 42

module N =
    type T = { X: int }

    let asType (t: T) = t.X
    let asCtor = T().Y
"
                    }

                    // An arity-0 claim settles a bare name only from within the candidate set:
                    // a record at arity 0 leaves the sole class claim to supply the arity.
                    test "an arity-0 record does not settle a bare ctor name" {
                        expectClean
                            "\
module A =
    type T<'a>(x: 'a) =
        member _.Y = x

module C =
    type T = { Z: int }

module N =
    open A
    open C

    let v = T(1).Y
"
                    }
                ]
        ]
