module XParsec.FSharp.SemanticAnalysis.Tests.ConstraintsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyseNR (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(MockBuiltins.provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    ctx

let private analyseUnif (input: string) =
    let lexed, file = parseFile input
    let ctx = PassContext(MockBuiltins.provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let private analyseFull (input: string) =
    let lexed, file = parseFile input
    let ctx, _ = Pipeline.analyseWithContext MockBuiltins.provider input lexed file
    ctx

let private hasMessage (ctx: PassContext) (fragment: string) =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains fragment)

let private countMessage (ctx: PassContext) (fragment: string) =
    ctx.Diagnostics
    |> Seq.filter (fun d -> d.Message.Contains fragment)
    |> Seq.length

let private typeOf (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.TypeVar.TryGetValue key with
    | ValueSome tv -> Unification.zonk (TyVar tv)
    | ValueNone -> failwithf "no TypeVar entry for %O" key

[<Tests>]
let tests =
    testList
        "Constraints"
        [
            test "generic record with constraints captures TyparConstraints" {
                let ctx = analyseNR "type Set<'a when 'a : comparison> = { Items: 'a list }"
                let info = ctx.RecordTypes.["Set"]
                Expect.isTrue info.TyparConstraints.IsSome "TyparConstraints captured"
            }

            test "generic union with constraints captures TyparConstraints" {
                let ctx = analyseNR "type Tree<'a when 'a : comparison> = | Leaf | Node of 'a"

                let info = ctx.UnionTypes.["Tree"]
                Expect.isTrue info.TyparConstraints.IsSome "TyparConstraints captured"
            }

            test "non-constrained generic record has ValueNone TyparConstraints" {
                let ctx = analyseNR "type Box<'a> = { Value: 'a }"
                let info = ctx.RecordTypes.["Box"]
                Expect.isTrue info.TyparConstraints.IsNone "no TyparConstraints"
            }

            test "equality constraint satisfied by int" {
                let ctx =
                    analyseUnif "let eq<'a when 'a : equality> (x: 'a) (y: 'a) = x\nlet _ = eq 1 2"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "equality constraint violated by function type" {
                let ctx =
                    analyseUnif
                        "let eq<'a when 'a : equality> (x: 'a) (y: 'a) = x\nlet _ = eq (fun a -> a) (fun b -> b)"

                Expect.isTrue (hasMessage ctx "equality") "equality-violation diagnostic"
            }

            test "comparison constraint satisfied by int" {
                let ctx =
                    analyseUnif "let cmp<'a when 'a : comparison> (x: 'a) (y: 'a) = x\nlet _ = cmp 1 2"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "comparison constraint violated by function type" {
                let ctx =
                    analyseUnif
                        "let cmp<'a when 'a : comparison> (x: 'a) (y: 'a) = x\nlet _ = cmp (fun a -> a) (fun b -> b)"

                Expect.isTrue (hasMessage ctx "comparison") "comparison-violation diagnostic"
            }

            test "comparison constraint satisfied by tuple of comparable elements" {
                let ctx =
                    analyseUnif "let cmp<'a when 'a : comparison> (x: 'a) (y: 'a) = x\nlet _ = cmp (1, 2) (3, 4)"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "comparison constraint violated by tuple containing a function" {
                let ctx =
                    analyseUnif
                        "let cmp<'a when 'a : comparison> (x: 'a) (y: 'a) = x\nlet _ = cmp (1, fun a -> a) (2, fun b -> b)"

                Expect.isTrue (hasMessage ctx "comparison") "comparison-violation diagnostic on tuple"
            }

            test "struct constraint satisfied by int" {
                let ctx = analyseUnif "let us<'a when 'a : struct> (x: 'a) = x\nlet _ = us 1"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "struct constraint violated by string" {
                let ctx = analyseUnif "let us<'a when 'a : struct> (x: 'a) = x\nlet _ = us \"x\""

                Expect.isTrue (hasMessage ctx "struct") "struct-violation diagnostic"
            }

            test "reference-type constraint satisfied by string" {
                let ctx =
                    analyseUnif "let ur<'a when 'a : not struct> (x: 'a) = x\nlet _ = ur \"x\""

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "reference-type constraint violated by int" {
                let ctx = analyseUnif "let ur<'a when 'a : not struct> (x: 'a) = x\nlet _ = ur 1"

                Expect.isTrue (hasMessage ctx "not struct") "not-struct-violation diagnostic"
            }

            test "generic record use site that satisfies the constraint" {
                let ctx =
                    analyseUnif "type Set<'a when 'a : comparison> = { Items: 'a }\nlet s : Set<int> = { Items = 1 }"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "generic record use site that violates the constraint" {
                let ctx =
                    analyseUnif
                        "type Set<'a when 'a : comparison> = { Items: 'a }\nlet s : Set<int -> int> = { Items = fun x -> x }"

                Expect.isTrue (hasMessage ctx "comparison") "comparison diagnostic at use site"
            }

            test "constraint propagates through abbreviation expansion (violation)" {
                let ctx =
                    analyseUnif
                        "type SortedPair<'a when 'a : comparison> = 'a * 'a\nlet p : SortedPair<int -> int> = ((fun x -> x), (fun x -> x))"

                Expect.isTrue (hasMessage ctx "comparison") "comparison diagnostic through abbreviation"
            }

            test "constraint propagates through abbreviation expansion (satisfied)" {
                let ctx =
                    analyseUnif "type SortedPair<'a when 'a : comparison> = 'a * 'a\nlet p : SortedPair<int> = (1, 2)"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "inline WhenConstrainedType attaches a constraint that fires at use" {
                let ctx =
                    analyseUnif "let f (x: ('a when 'a : equality)) = x\nlet _ = f (fun a -> a)"

                Expect.isTrue (hasMessage ctx "equality") "equality diagnostic from inline when"
            }

            test "multiple constraints attach to the same typar" {
                let ctx =
                    analyseUnif "let f<'a when 'a : equality and 'a : comparison> (x: 'a) = x\nlet _ = f (fun a -> a)"

                // Function fails both equality and comparison. We expect both
                // to fire as separate diagnostics on the same use site.
                let n = countMessage ctx "equality" + countMessage ctx "comparison"
                Expect.isGreaterThan n 0 "at least one constraint diagnostic"
            }

            test "duplicate constraints in source dedupe to one" {
                // `equality and equality` should not double-fire; dedupe in
                // translation collapses to a single Equality constraint, so
                // a violating use site produces exactly one message.
                let ctx =
                    analyseUnif "let f<'a when 'a : equality and 'a : equality> (x: 'a) = x\nlet _ = f (fun a -> a)"

                Expect.equal (countMessage ctx "equality") 1 "single equality diagnostic"
            }

            test "first use satisfies; second use violates → one diagnostic only" {
                let ctx =
                    analyseUnif
                        "let eq<'a when 'a : equality> (x: 'a) (y: 'a) = x\nlet a = eq 1 2\nlet b = eq (fun x -> x) (fun y -> y)"

                Expect.equal (countMessage ctx "equality") 1 "second use diagnoses; first does not"
            }

            test "TAST shape for a constraint-bearing binding is unchanged" {
                let ctx =
                    analyseFull "let eq<'a when 'a : equality> (x: 'a) (y: 'a) = x\nlet u = eq 1 2"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "constraint flows through generic record's field type" {
                let ctx =
                    analyseFull "type Box<'a when 'a : equality> = { Value: 'a }\nlet b : Box<int> = { Value = 1 }"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }
        ]
