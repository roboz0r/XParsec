module XParsec.FSharp.SemanticAnalysis.Tests.MeasureResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// The frozen type of the file's only `let`, which more than one `let` fails rather than picks
/// between.
let private soleFrozenLetType (frozen: FrozenPools) : FrozenType =
    match (TastUnpool.ofPools frozen).Decls with
    | EqList [ TDeclG.Let(ty = ty) ] -> ty
    | other -> failtestf "expected a single let, got %A" other

let private asm: CompilingAssembly =
    {
        Name = AssemblyName "MeasureAsm"
        Target = "none"
    }

let private analyseUnits (units: SourceUnit list) : AnalysedAssembly =
    AnalysedAssembly.analyse
        Pipeline.analyseFileFor
        realProvider.Value
        {
            Assembly = asm
            Units = List.map (AssemblyUnit.parse Set.empty) units
        }

/// The errors every unit of an assembly run surfaces, `.fsi` match findings included.
let private assemblyErrors (units: SourceUnit list) : string list =
    (analyseUnits units).Units
    |> List.collect UnitOutcome.surfaced
    |> List.map (fun d -> d.Diagnostic)
    |> errorMessages

/// The name and generic arity of the keyed type frozen for the single `let` in the last unit.
let private lastLetTypeClaim (units: SourceUnit list) : string * int =
    match List.last (analyseUnits units).Units with
    | UnitOutcome.Failed _ -> failtest "the last unit did not analyse"
    | UnitOutcome.Analysed u ->
        let lets =
            [
                for d in (TastUnpool.ofPools u.File.Frozen).Decls do
                    match d with
                    | TDeclG.Let(ty = ty) -> ty
                    | _ -> ()
            ]

        match lets with
        | [ FTKeyed(key, _) ] -> key.Name, key.TyparArity
        | [ other ] -> failtestf "expected the let to freeze to a keyed type, got %A" other
        | other -> failtestf "expected a single let, got %A" other

/// The referenced contracts' claims on `Vesper.<name>`, as (arity, typar kinds) ascending by arity.
let private vesperTypeClaims (name: string) : (int * TyparKind list) list =
    let scope = realProvider.Value.Scope

    match scope.TryContainer "Vesper" with
    | ValueNone -> failtest "the `Vesper` namespace is not published"
    | ValueSome container ->
        [
            for struct (key, shape) in (scope.TypesNamed(container, name)).Underlying ->
                key.TyparArity, EqArray.toList shape.TyparKinds
        ]

// A measured numeric type is not a special form. FSharp.Core claims each numeric primitive
// at arity 0 and again at arity 1 with a MEASURE-kinded parameter, so `float<m>` is the
// per-arity resolution of `float` at arity 1 followed by a KIND check of `m` against the
// parameter. The measure name resolves exactly as a type name does: scoped top-down, and
// distinct from a namesake in another module.
//
// Each case's F# verdict is quoted under its FS code, pinned with `dotnet fsi`. A `ptest`
// names what this compiler does today in its parenthetical.
[<Tests>]
let tests =
    testList
        "MeasureResolution"
        [
            testList
                "a type parameter carries its kind"
                [
                    test "`[<Measure>]` on a typar registers it measure-kinded, a plain typar type-kinded" {
                        let ctx, _ =
                            analyseNameRes realProvider.Value "type Pair<[<Measure>] 'u, 'a> = { V: 'a }"

                        let info = expectRecord ctx "Pair"

                        Expect.equal
                            [ for tp in info.TypeParams -> tp.Name, tp.Kind ]
                            [ "'u", TyparKind.Measure; "'a", TyparKind.Type ]
                            "the `[<Measure>]` attribute decides the kind"
                    }

                    test "a prefix typar (`'a Box`) is type-kinded" {
                        let ctx, _ = analyseNameRes realProvider.Value "type 'a Box = { V: 'a }"
                        let info = expectRecord ctx "Box"

                        Expect.equal
                            [ for tp in info.TypeParams -> tp.Name, tp.Kind ]
                            [ "'a", TyparKind.Type ]
                            "the prefix form has no attribute slot"
                    }

                    test "a declaration's kinds reach the frozen contract" {
                        let frozen = freezeFor "type Pair<[<Measure>] 'u, 'a> = { V: 'a }\n"

                        match (TastUnpool.ofPools frozen).Decls with
                        | EqList [ TDeclG.Type td ] ->
                            Expect.equal
                                [ for p in td.TypeParams -> p.Name, p.Kind ]
                                [ "'u", TyparKind.Measure; "'a", TyparKind.Type ]
                                "the frozen declaration carries the kinds"
                        | other -> failtestf "expected a single type declaration, got %A" other
                    }

                    // Both forms register no typar list, so their kinds are read off the `TypeName`.
                    test "an `extern` declaration's kinds reach its published shape" {
                        let r =
                            SignatureResolutionTests.resolveFsi
                                "M.fsi"
                                "namespace Vesper\n\ntype carrier<[<Measure>] 'u> = extern\n"

                        Expect.equal
                            (EqArray.toList (SignatureResolutionTests.shapeOf r "carrier`1").TyparKinds)
                            [ TyparKind.Measure ]
                            "an extern primitive carries its declared kinds"
                    }

                    test "an opaque declaration's kinds reach its published shape" {
                        let r =
                            SignatureResolutionTests.resolveFsi
                                "M.fsi"
                                "namespace App\n\nmodule M =\n    type Carrier<[<Measure>] 'u>\n"

                        Expect.equal
                            (EqArray.toList (SignatureResolutionTests.shapeOf r "Carrier`1").TyparKinds)
                            [ TyparKind.Measure ]
                            "an opaque type carries its declared kinds"
                    }
                ]

            testList
                "Vesper.Core claims each numeric carrier at two arities"
                [
                    // The arity-0 contract and `prim-types-*-measured` are different UNITS of
                    // Vesper.Core, so a name's arities must survive the composition of the
                    // units' published views.
                    for key in RuntimeNames.numericKeys do
                        test $"`{key.Name}` is claimed at arity 0 and at arity 1 over a measure" {
                            Expect.equal
                                (vesperTypeClaims key.Name)
                                [ 0, []; 1, [ TyparKind.Measure ] ]
                                "the measured claim sits beside the bare one"
                        }
                ]

            testList
                "the carrier resolves at arity 1 and the measure resolves as a type name"
                [
                    test "every measured carrier accepts a declared measure" {
                        expectClean
                            "\
[<Measure>] type m
let a: float<m> = 1.0<m>
let b: decimal<m> = 1.0M<m>
let c: int64<m> = 1L<m>
let d: float32<m> = 1.0f<m>
"
                    }

                    test "an abbreviation of a measured type is a measured type" {
                        expectClean
                            "\
[<Measure>] type m
type S = float<m>
let x: S = 1.0<m>
"
                    }

                    // FS0039: "The type 'm' is not defined".
                    ptest
                        "an undeclared measure in a type is undefined (`m` is never looked up today, so nothing is reported)" {
                        expectUserErrorReportedAlone
                            "The type 'm' is not defined"
                            "\
let x: float<m> = 1.0<m>
"
                    }

                    // FS0039 at the literal too: `1.0<m>` looks `m` up like any type name.
                    ptest
                        "an undeclared measure in a literal is undefined (`m` is never looked up today, so nothing is reported)" {
                        expectUserErrorReportedAlone
                            "The type 'm' is not defined"
                            "\
let x = 1.0<m>
"
                    }

                    // FS0039: scoping is top-down, as for any type.
                    ptest
                        "a measure declared after its use is undefined (no scoping is applied today, so nothing is reported)" {
                        expectUserErrorReportedAlone
                            "The type 'm' is not defined"
                            "\
let x: float<m> = 1.0<m>
[<Measure>] type m
"
                    }

                    // FS0001: "The unit of measure 'Q.m' does not match the unit of measure
                    // 'P.m'". Measures are identities, not spellings, so the two DECLARED
                    // PATHS are what the message must carry; a bare "Measure mismatch" would
                    // read the same whether or not the identities differ.
                    ptest
                        "two measures of one name in different modules are distinct (`MeasureTerm` keys on the spelling today, so the two compare equal)" {
                        let es =
                            semErrors
                                "\
module P =
    [<Measure>] type m

module Q =
    [<Measure>] type m

open P
let a = 1.0<m>
open Q
let b = 1.0<m>
let c = a + b
"

                        expectUserErrorIn es "Measure mismatch"
                        expectErrorIn es "P.m"
                        expectErrorIn es "Q.m"
                    }

                    ptest
                        "a measure abbreviation expands to its term (the declaration registers nothing today, so <m/s> and <v> mismatch)" {
                        expectClean
                            "\
[<Measure>] type m
[<Measure>] type s
[<Measure>] type v = m / s
let x: float<v> = 1.0<m/s>
"
                    }
                ]

            testList
                "the argument's KIND is checked against the parameter's"
                [
                    // FS0704: "Expected type, not unit-of-measure".
                    ptest
                        "a measure argument on a type-kinded parameter is an error (no measure claim today, so FS0039 stands in for FS0704)" {
                        expectUserErrorReportedAlone
                            "Expected type, not unit-of-measure"
                            "\
[<Measure>] type m
type Box<'a> = { V: 'a }
let x: Box<m> = { V = 1 }
"
                    }

                    // FS0704 again: a measure claim in TYPE position.
                    ptest
                        "a measure in type position is an error (no measure claim today, so FS0039 stands in for FS0704)" {
                        expectUserErrorReportedAlone
                            "Expected type, not unit-of-measure"
                            "\
[<Measure>] type m
let f (x: m) = x
"
                    }

                    // FS0705: "Expected unit-of-measure, not type".
                    ptest
                        "a type argument on a measure-kinded parameter is an error (no kind check today; the dimensionless message prints a record)" {
                        expectUserErrorReportedAlone
                            "Expected unit-of-measure, not type"
                            "\
let x: float<int> = 1.0
"
                    }

                    // FS0705: the name resolves to a record, so the kind check fails.
                    ptest
                        "a same-named record in the measure position is a type, not a measure (no kind check today; the dimensionless message prints a record)" {
                        expectUserErrorReportedAlone
                            "Expected unit-of-measure, not type"
                            "\
type m = { A: int }
let x: float<m> = 1.0
"
                    }

                    // FS0704: the local arity-1 claim outranks FSharp.Core's, and its
                    // parameter is a type.
                    ptest
                        "a local generic `float` outranks the measured primitive (the spelling gate ignores the local claim today, so nothing is reported)" {
                        expectUserErrorReportedAlone
                            "Expected type, not unit-of-measure"
                            "\
[<Measure>] type m
type float<'a> = { V: 'a }
let x: float<m> = 1.0<m>
"
                    }
                ]

            testList
                "a carrier claimed at arity 0 only reports the arity"
                [
                    // FS0033: "The non-generic type 'string' does not expect any type
                    // arguments, but here is given 1 type argument(s)".
                    ptest
                        "`string<m>` reports the arity (no external nearest-arity leg today, so FS0039 stands in for FS0033)" {
                        expectUserErrorReportedAlone
                            "expects 0 type argument"
                            "\
[<Measure>] type m
let x: string<m> = \"\"
"
                    }

                    // FS0033 alone: the abbreviation is claimed at arity 0 only, and the
                    // measure argument is not read past the arity check.
                    ptest
                        "an abbreviation of a carrier at arity 0 reports the arity alone (FS0039 and the dimensionless message stand beside it today)" {
                        expectUserErrorReportedAlone
                            "expects 0 type argument"
                            "\
[<Measure>] type m
type MyFloat = float
let x: MyFloat<m> = 1.0<m>
"
                    }
                ]

            testList
                "a measured root freezes to its carrier"
                [
                    // The `UnresolvedTyVars` backstop does not fire here: `addFreeRoots`
                    // follows the measured root's `Link` through to `float` while `zonk`
                    // stops at it, so the empty error list below passes today and the
                    // frozen type is the only thing that notices.
                    ptest "a measured let freezes to `float` (freezes to FTUnknown UnresolvedTypar today)" {
                        let frozen = freezeFor "[<Measure>] type m\nlet x = 1.0<m>\n"
                        let es = errorMessages (FrozenPools.blockingErrors frozen)
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        Expect.equal
                            (soleFrozenLetType frozen)
                            (FTConst(RuntimeNames.floatKey, EqArray.empty))
                            "the frozen type is the carrier"
                    }
                ]

            testList
                "a name claimed at two arities crosses the signature match"
                [
                    test "a `.fsi` declaring `T` and `T<'a>` matches its implementation" {
                        let signature =
                            "\
namespace Test.A

module M =
    type T = { X: int }
    type T<'a> = { Y: 'a }
    val a: T
    val b: T<int>
"

                        let implementation =
                            "\
namespace Test.A

module M =
    type T = { X: int }
    type T<'a> = { Y: 'a }
    let a: T = { X = 1 }
    let b: T<int> = { Y = 2 }
"

                        // `t.X` and `t.Y` are what separate the two claims: reading the
                        // arity-0 field off the arity-1 type is refused, so a run that
                        // collapsed the claims onto one would redden here.
                        let consumer =
                            "\
namespace Test.B

open Test.A.M

module N =
    let f (t: T) = t.X
    let g (t: T<int>) = t.Y
    let h: T = a
    let k: T<int> = b
"

                        let es =
                            assemblyErrors
                                [
                                    SourceUnit.paired
                                        (SourceFile.ofText "file1.fsi" signature)
                                        (SourceFile.ofText "file1.fs" implementation)
                                    SourceUnit.ofImplementation (SourceFile.ofText "file2.fs" consumer)
                                ]

                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)
                    }

                    // The dotted-name counterpart of `resolveType`'s per-arity rule, which is what
                    // lets `prim-types-int-measured` sit in the namespace of the carrier it
                    // re-claims. Both readings are error-free, so the frozen type separates them.
                    test "a dotted name reaches the arity another unit claims, not this file's" {
                        let producer =
                            "\
namespace Test.A

type Tag =
    | Item of int
"

                        let consumer =
                            "\
namespace Test.A

type Tag<'a> =
    | Item of 'a

module N =
    let x = Test.A.Tag.Item 1
"

                        let units =
                            [
                                SourceUnit.ofImplementation (SourceFile.ofText "file1.fs" producer)
                                SourceUnit.ofImplementation (SourceFile.ofText "file2.fs" consumer)
                            ]

                        let es = assemblyErrors units
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        Expect.equal (lastLetTypeClaim units) ("Tag", 0) "`Item` is the arity-0 `Tag`'s case"
                    }
                ]
        ]
