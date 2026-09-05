module XParsec.FSharp.SemanticAnalysis.Tests.ConstraintsTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyseNR (input: string) =
    let lexed, file = parseFile input

    let ctx = PassContext(realProvider.Value, LexedFile.ofText lexed, testCompiling)

    NameResolution.run ctx file
    ctx

/// NameResolution and Unification over the real contract stack plus `extras`, stand-in
/// referenced assemblies.
let private analyseUnifWith (extras: IExternalSymbolProvider list) (input: string) =
    let lexed, file = parseFile input

    let provider = ExternalSymbolProviders.composite (extras @ [ realProvider.Value ])
    let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

    NameResolution.run ctx file
    Unification.run ctx file
    ctx

let private analyseUnif (input: string) = analyseUnifWith [] input

let private analyseFull (input: string) =
    let lexed, file = parseFile input

    let ctx, _ =
        Pipeline.analyseSemWithContextFor testCompiling realProvider.Value (LexedFile.ofText lexed) file

    ctx

let private hasMessage (ctx: PassContext) (fragment: string) =
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains fragment)

/// A referenced assembly publishing the enum `qualifiedName` with cases `A = 1` and `B = 2`
/// at `kind`, its namespace implicitly open.
let private importedEnum (qualifiedName: string) (kind: IntKind) : IExternalSymbolProvider =
    let ns = qualifiedName.Substring(0, qualifiedName.LastIndexOf '.')

    let cases =
        [ "A", 1L; "B", 2L ]
        |> List.map (fun (n, v) ->
            {
                Name = n
                Value = ExternalEnumCaseValue.IntVal(kind, v)
            }
            : ExternalEnumCaseShape
        )
        |> EqArray.ofList

    providerOfSurface (fun b ->
        PublishedSurfaceBuilder.addType
            b
            (SymbolKeyOps.qualifiedTypeKeyOf qualifiedName 0)
            (ExternalTypeShape.Enum(cases, RuntimeNames.intKindKey kind, SymbolOrigin.Empty))

        b.ImplicitOpens <- [ SymbolKeyOps.assemblyAutoOpen ns ]
    )

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

            // Checking `Tree<'T> : equality` re-enters itself through the `Node` payload
            // with the same goal, and carries no in-progress set, so `checkConstraint` →
            // `verdictOutcome` → `reduceOutcome` → `checkConstraint` overflows the stack.
            ptest "gap: a self-recursive generic union under an equality constraint overflows the stack" {
                let ctx =
                    analyseFull
                        "type Tree<'T> =\n    | Leaf of 'T\n    | Node of Tree<'T> * Tree<'T>\nlet t = Node(Leaf 1, Leaf 2)\nlet e = (t = t)"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
            }
        ]

// `new`, `unmanaged`, `enum<_>` and `delegate<_,_>` on target-neutral shapes. A verdict that
// reads a layout the target decides (`string` under `new`, a tuple under `unmanaged`) is
// pinned by the conformance corpus instead.

let private newFn = "let mk<'a when 'a : (new : unit -> 'a)> (x: 'a) = x\n"
let private unmanagedFn = "let um<'a when 'a : unmanaged> (x: 'a) = x\n"
let private enumFn = "let en<'a when 'a : enum<int>> (x: 'a) = x\n"
let private delegateFn = "let dg<'a when 'a : delegate<int, int>> (x: 'a) = x\n"

let private satisfied (name: string) (source: string) =
    test name {
        let ctx = analyseUnif source
        Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
    }

let private violated (name: string) (fragment: string) (source: string) =
    test name {
        let ctx = analyseUnif source

        Expect.isTrue
            (hasMessage ctx fragment)
            (sprintf "expected a diagnostic containing %s, got %A" fragment [ for d in ctx.Diagnostics -> d.Message ])
    }

[<Tests>]
let kindTests =
    testList
        "Constraint kinds"
        [
            satisfied
                "new: a class with a parameterless primary ctor"
                (newFn + "type C() = member _.X = 1\nlet _ = mk (C())")
            satisfied
                "new: a class with a parameterless secondary ctor"
                (newFn
                 + "type C(x: int) =\n    new() = C(0)\n    member _.X = x\nlet _ = mk (C())")
            satisfied "new: a value type" (newFn + "let _ = mk 1")
            satisfied "new: an enum" (newFn + "type E = | A = 1 | B = 2\nlet _ = mk E.A")
            satisfied "new: obj" (newFn + "let _ = mk (obj())")
            violated
                "new: a class with only a parameterised ctor"
                "'new' constraint"
                (newFn + "type C(x: int) =\n    member _.X = x\nlet _ = mk (C(1))")
            violated "new: a reference record" "'new' constraint" (newFn + "type R = { X: int }\nlet _ = mk { X = 1 }")
            violated "new: a function" "'new' constraint" (newFn + "let _ = mk (fun (x: int) -> x)")
            violated
                "new: a result typar other than the constrained one is refused"
                "'new' constraints must take one argument"
                "let mk<'a, 'b when 'a : (new : unit -> 'b)> (x: 'a) = x"

            satisfied "unmanaged: a numeric primitive" (unmanagedFn + "let _ = um 1")
            satisfied "unmanaged: bool and char" (unmanagedFn + "let _ = um true\nlet _ = um 'c'")
            satisfied "unmanaged: decimal" (unmanagedFn + "let _ = um 1.0m")
            satisfied "unmanaged: an enum" (unmanagedFn + "type E = | A = 1 | B = 2\nlet _ = um E.A")
            satisfied
                "unmanaged: a struct record of scalars"
                (unmanagedFn
                 + "[<Struct>]\ntype P = { X: int; Y: int }\nlet _ = um { X = 1; Y = 2 }")
            satisfied
                "unmanaged: a struct union of scalars"
                (unmanagedFn
                 + "[<Struct>]\ntype U = | A of a: int | B of b: float\nlet _ = um (A 1)")
            violated "unmanaged: string" "'unmanaged' constraint" (unmanagedFn + "let _ = um \"s\"")
            violated "unmanaged: unit" "'unmanaged' constraint" (unmanagedFn + "let _ = um ()")
            violated
                "unmanaged: a reference record"
                "'unmanaged' constraint"
                (unmanagedFn + "type R = { X: int }\nlet _ = um { X = 1 }")
            violated
                "unmanaged: a struct record holding a reference"
                "'unmanaged' constraint"
                (unmanagedFn
                 + "[<Struct>]\ntype Q = { X: int; S: string }\nlet _ = um { X = 1; S = \"s\" }")
            violated
                "unmanaged: a generic struct record, whatever its argument"
                "'unmanaged' constraint"
                (unmanagedFn + "[<Struct>]\ntype G<'t> = { V: 't }\nlet _ = um { V = 1 }")
            // A struct class carries every primary-ctor parameter as a field, read or not,
            // beside its `val` fields; `fsc` refuses each of the shapes below.
            satisfied
                "unmanaged: a struct class of scalar ctor parameters"
                (unmanagedFn
                 + "[<Struct>]\ntype S(n: int, b: bool) =\n    member _.N = n\n    member _.B = b\nlet _ = um (S(1, true))")
            violated
                "unmanaged: a struct class holding a reference through a read ctor parameter"
                "'unmanaged' constraint"
                (unmanagedFn
                 + "[<Struct>]\ntype S(o: obj, n: int) =\n    member _.O = o\n    member _.N = n\nlet _ = um (S(box 1, 2))")
            violated
                "unmanaged: a struct class holding a reference through an unread ctor parameter"
                "'unmanaged' constraint"
                (unmanagedFn
                 + "[<Struct>]\ntype S(o: obj, n: int) =\n    member _.N = n\nlet _ = um (S(box 1, 2))")
            violated
                "unmanaged: a struct class holding a reference through a val field"
                "'unmanaged' constraint"
                (unmanagedFn
                 + "[<Struct>]\ntype S =\n    val O: obj\n    val N: int\n    new(o, n) = { O = o; N = n }\nlet _ = um (S(box 1, 2))")
            satisfied
                "equality: a struct class holding obj through a ctor parameter"
                ("let eq<'a when 'a : equality> (x: 'a) = x\n"
                 + "[<Struct>]\ntype S(o: obj, n: int) =\n    member _.O = o\n    member _.N = n\nlet _ = eq (S(box 1, 2))")
            violated
                "equality: a struct class holding a function through a ctor parameter"
                "equality"
                ("let eq<'a when 'a : equality> (x: 'a) = x\n"
                 + "[<Struct>]\ntype S(f: int -> int) =\n    member _.F = f\nlet _ = eq (S(id))")
            violated "unmanaged: an array" "'unmanaged' constraint" (unmanagedFn + "let _ = um [| 1 |]")
            violated "unmanaged: a function" "'unmanaged' constraint" (unmanagedFn + "let _ = um (fun (x: int) -> x)")

            satisfied "enum<int>: an int enum" (enumFn + "type E = | A = 1 | B = 2\nlet _ = en E.A")
            satisfied
                "enum<int>: a member's clause"
                "type H() =\n    member _.Pick<'b when 'b : enum<int>> (b: 'b) = b\ntype E = | A = 1 | B = 2\nlet _ = H().Pick E.A"
            violated
                "enum<int>: an int64 enum mismatches on the underlying type"
                "Type mismatch"
                (enumFn + "type L = | A = 1L | B = 2L\nlet _ = en L.A")
            satisfied
                "enum<string>: a string enum"
                ("let es<'a when 'a : enum<string>> (x: 'a) = x\n"
                 + "type S = | A = \"a\" | B = \"b\"\nlet _ = es S.A")
            violated "enum<int>: int itself" "'enum<int>' constraint" (enumFn + "let _ = en 1")
            violated "enum<int>: string" "'enum<int>' constraint" (enumFn + "let _ = en \"s\"")
            violated
                "enum<int>: a record"
                "'enum<int>' constraint"
                (enumFn + "type R = { X: int }\nlet _ = en { X = 1 }")
            test "enum<'u>: the underlying type is inferred from the enum" {
                let ctx =
                    analyseUnif
                        "let under<'a, 'u when 'a : enum<'u>> (x: 'a) (u: 'u) = u\ntype L = | A = 1L | B = 2L\nlet _ = under L.A 3L"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
                Expect.isFalse (hasMessage ctx "Type mismatch") "int64 is L's underlying type"
            }
            test "enum<'u>: a wrong underlying argument mismatches" {
                let ctx =
                    analyseUnif
                        "let under<'a, 'u when 'a : enum<'u>> (x: 'a) (u: 'u) = u\ntype L = | A = 1L | B = 2L\nlet _ = under L.A 3"

                Expect.isTrue (hasMessage ctx "Type mismatch") "int is not L's underlying type"
            }
            test "enum<'u>: an imported enum's underlying type is its published width" {
                let ctx =
                    analyseUnifWith
                        [ importedEnum "Ext.Wide" IntKind.Int64 ]
                        "let under<'a, 'u when 'a : enum<'u>> (x: 'a) (u: 'u) = u\nlet _ = under Wide.A 3L"

                Expect.isFalse (hasMessage ctx "does not support") "no constraint diagnostic"
                Expect.isFalse (hasMessage ctx "Type mismatch") "int64 is Wide's underlying type"
            }
            test "enum<'u>: an imported enum's published width rejects a narrower argument" {
                let ctx =
                    analyseUnifWith
                        [ importedEnum "Ext.Wide" IntKind.Int64 ]
                        "let under<'a, 'u when 'a : enum<'u>> (x: 'a) (u: 'u) = u\nlet _ = under Wide.A 3"

                Expect.isTrue (hasMessage ctx "Type mismatch") "int is not Wide's underlying type"
            }

            violated "delegate<int, int>: the clause is not yet supported" "'delegate' constraint" delegateFn
        ]
