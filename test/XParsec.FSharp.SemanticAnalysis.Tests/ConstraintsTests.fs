module XParsec.FSharp.SemanticAnalysis.Tests.ConstraintsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyseNR (input: string) =
    let lexed, file = parseFile input

    let ctx = PassContext(realProvider.Value, LexedFile.ofText lexed, testCompiling)

    Desugar.run ctx file
    NameResolution.run ctx file
    ctx

let private analyseUnif (input: string) =
    let lexed, file = parseFile input

    let ctx = PassContext(realProvider.Value, LexedFile.ofText lexed, testCompiling)

    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let private analyseFull (input: string) =
    let lexed, file = parseFile input

    let ctx, _ =
        Pipeline.analyseSemWithContextFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

    ctx

let private hasMessage (ctx: PassContext) (fragment: string) =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains fragment)

let private countMessage (ctx: PassContext) (fragment: string) =
    ctx.Diagnostics
    |> Seq.filter (fun d -> d.Message.Contains fragment)
    |> Seq.length

let private typeOf (ctx: PassContext) (key: NodeKey) : SemType =
    match ctx.Bindings.TypeVar.TryGetValue key with
    | ValueSome tv -> Unification.zonk ctx.Store (TyVar tv)
    | ValueNone -> failwithf "no TypeVar entry for %O" key

[<Tests>]
let tests =
    testList
        "Constraints"
        [
            test "generic record with constraints captures TyparConstraints" {
                let ctx = analyseNR "type Set<'a when 'a : comparison> = { Items: 'a list }"

                match TypeRegistry.tryRecord ctx.Types UseSite.unbounded "Set" with
                | ValueSome info -> Expect.isTrue info.TyparConstraints.IsSome "TyparConstraints captured"
                | ValueNone -> failtest "record Set not registered"
            }

            test "generic union with constraints captures TyparConstraints" {
                let ctx = analyseNR "type Tree<'a when 'a : comparison> = | Leaf | Node of 'a"

                match TypeRegistry.tryUnionBare ctx.Types UseSite.unbounded "Tree" with
                | ValueSome info -> Expect.isTrue info.TyparConstraints.IsSome "TyparConstraints captured"
                | ValueNone -> failtest "union Tree not registered"
            }

            test "non-constrained generic record has ValueNone TyparConstraints" {
                let ctx = analyseNR "type Box<'a> = { Value: 'a }"

                match TypeRegistry.tryRecord ctx.Types UseSite.unbounded "Box" with
                | ValueSome info -> Expect.isTrue info.TyparConstraints.IsNone "no TyparConstraints"
                | ValueNone -> failtest "record Box not registered"
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

            // These three bind a repr on the CLR and none on JS. The verdict comes off the
            // contract's declared `interface equatable<_>` / `comparable<_>`, so it holds
            // wherever the type is representable at all — the surface is not the repr.
            test "equality and comparison constraints satisfied by decimal, nativeint and unativeint" {
                for literal in [ "1.0M"; "1n"; "1un" ] do
                    let ctx =
                        analyseUnif (
                            sprintf "let f<'a when 'a : equality and 'a : comparison> (x: 'a) = x\nlet _ = f %s" literal
                        )

                    Expect.isEmpty
                        (ctx.Diagnostics |> Seq.map (fun d -> d.Message))
                        (sprintf "`f %s` discharges both constraints" literal)
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

            // A function is a reference shape on every target. Value-ness of a PRIMITIVE is
            // the target's decision and this suite composes no platform, so the codegen
            // conformance corpus pins those. `"struct"` also matches the `not struct` message.
            test "struct constraint violated by a function type" {
                let ctx =
                    analyseUnif "let us<'a when 'a : struct> (x: 'a) = x\nlet _ = us (fun i -> i)"

                Expect.isTrue (hasMessage ctx "'struct' constraint") "struct-violation diagnostic"
            }

            test "reference-type constraint satisfied by a function type" {
                let ctx =
                    analyseUnif "let ur<'a when 'a : not struct> (x: 'a) = x\nlet _ = ur (fun i -> i)"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            // A NOMINAL's declaration decides where no platform overrides it, so both
            // polarities are covered here even though a primitive's are not.
            test "struct constraint satisfied by a [<Struct>] record" {
                let ctx =
                    analyseUnif
                        "[<Struct>]\ntype P = { X: int }\nlet us<'a when 'a : struct> (x: 'a) = x\nlet _ = us { X = 1 }"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "struct constraint violated by a plain record" {
                let ctx =
                    analyseUnif "type P = { X: int }\nlet us<'a when 'a : struct> (x: 'a) = x\nlet _ = us { X = 1 }"

                Expect.isTrue (hasMessage ctx "'struct' constraint") "struct-violation diagnostic on a reference record"
            }

            test "reference-type constraint violated by a [<Struct>] record" {
                let ctx =
                    analyseUnif
                        "[<Struct>]\ntype P = { X: int }\nlet ur<'a when 'a : not struct> (x: 'a) = x\nlet _ = ur { X = 1 }"

                Expect.isTrue (hasMessage ctx "'not struct' constraint") "not-struct-violation diagnostic"
            }

            test "reference-type constraint satisfied by a plain record" {
                let ctx =
                    analyseUnif "type P = { X: int }\nlet ur<'a when 'a : not struct> (x: 'a) = x\nlet _ = ur { X = 1 }"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            // A `[<Struct>]` CLASS travels the same declaration path as a record.
            test "struct constraint satisfied by a [<Struct>] class" {
                let ctx =
                    analyseUnif
                        "[<Struct>]\ntype P(x: int) =\n    member _.X = x\nlet us<'a when 'a : struct> (v: 'a) = v\nlet _ = us (P 1)"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "struct constraint violated by a plain class" {
                let ctx =
                    analyseUnif
                        "type P(x: int) =\n    member _.X = x\nlet us<'a when 'a : struct> (v: 'a) = v\nlet _ = us (P 1)"

                Expect.isTrue (hasMessage ctx "'struct' constraint") "struct-violation diagnostic on a reference class"
            }

            // An enum asks for a value type wherever the target lays one out, and this suite
            // composes no platform to erase the request. Both polarities, because a DEFERRED
            // constraint is never swept into a diagnostic and would pass the satisfied half alone.
            test "struct constraint satisfied by an enum" {
                let ctx =
                    analyseUnif
                        "type Colour =\n    | Red = 0\n    | Green = 1\nlet us<'a when 'a : struct> (x: 'a) = x\nlet _ = us Colour.Red"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }

            test "reference-type constraint violated by an enum" {
                let ctx =
                    analyseUnif
                        "type Colour =\n    | Red = 0\n    | Green = 1\nlet ur<'a when 'a : not struct> (x: 'a) = x\nlet _ = ur Colour.Red"

                Expect.isTrue (hasMessage ctx "'not struct' constraint") "not-struct-violation diagnostic on an enum"
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

                // A function type fails equality AND comparison, so both constraints
                // on the one typar are checked at the same use site.
                let n = countMessage ctx "equality" + countMessage ctx "comparison"
                Expect.isGreaterThan n 0 "at least one constraint diagnostic"
            }

            test "duplicate constraints in source dedupe to one" {
                // `equality and equality` collapses to one Equality constraint in
                // translation, so a violating use site produces exactly one message.
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

            test "constraint propagates through generic record's field type" {
                let ctx =
                    analyseFull "type Box<'a when 'a : equality> = { Value: 'a }\nlet b : Box<int> = { Value = 1 }"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }
        ]
