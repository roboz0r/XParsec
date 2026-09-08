module XParsec.FSharp.SemanticAnalysis.Tests.MeasureResolutionTests

open Vesper
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// The frozen type of the file's one `let`, beside any type declarations.
let private soleFrozenLetType (frozen: FrozenPools) : FrozenType =
    let lets =
        (TastUnpool.ofPools frozen).Decls
        |> Seq.choose (
            function
            | TDeclG.Let(binding = m) -> Some m.Ty
            | _ -> None
        )
        |> List.ofSeq

    match lets with
    | [ ty ] -> ty
    | other -> failtestf "expected a single let, got %A" other

let private asm: CompilingAssembly =
    {
        Name = AssemblyName "MeasureAsm"
        Target = "none"
    }

/// An implementation-only unit of an assembly run.
let private impl (fileName: string) (text: string) : SourceUnit =
    SourceUnit.ofImplementation (SourceFile.ofText fileName text)

let private analyseUnits (units: SourceUnit list) : AnalysedAssembly =
    analyseUnitsOf asm realProvider.Value units

/// The errors every unit of an assembly run surfaces, `.fsi` match findings included.
let private assemblyErrors (units: SourceUnit list) : string list =
    (analyseUnits units).Units
    |> List.collect UnitOutcome.surfaced
    |> List.map (fun d -> d.Diagnostic)
    |> errorMessages

/// A `.fsi`/`.fs` pair declaring three measures and `val speed: float<v>`.
let private measuredSignaturePair: SourceUnit =
    let signature =
        "\
namespace Test.A

[<Measure>] type m
[<Measure>] type s
[<Measure>] type v = m / s
module M =
    val speed: float<v>
"

    let implementation =
        "\
namespace Test.A

[<Measure>] type m
[<Measure>] type s
[<Measure>] type v = m / s
module M =
    let speed: float<v> = 1.0<m/s>
"

    SourceUnit.paired (SourceFile.ofText "file1.fsi" signature) (SourceFile.ofText "file1.fs" implementation)

/// The real `Vesper.Core` contract for the JS target, which declares no `decimal`.
let private jsCoreProvider: Lazy<IExternalSymbolProvider> =
    lazy
        ([ srcManifest "js" "Vesper.Core" ]
         |> PackageProviders.composeContract PackageProviders.noPlatformMetadata
         |> fun composed -> composed.Provider)

/// The error messages of `src` analysed for the JS target over `jsCoreProvider`.
let private jsErrors (src: string) : string list =
    let lexed, file = parseFile src

    let tast =
        Pipeline.analyseSemFor { Name = testAsm; Target = "js" } jsCoreProvider.Value (LexedFile.ofText lexed) file

    errorMessages tast.Diagnostics

/// The name and generic arity of the keyed type frozen for the single `let` in the last unit.
let private lastLetTypeClaim (units: SourceUnit list) : string * int =
    match List.last (analyseUnits units).Units with
    | UnitOutcome.Failed _ -> failtest "the last unit did not analyse"
    | UnitOutcome.Analysed u ->
        let lets =
            [
                for d in (TastUnpool.ofPools u.File.Frozen).Decls do
                    match d with
                    | TDeclG.Let(binding = m) -> m.Ty
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
            for struct (key, shape) in scope.TypesNamed(container, name) ->
                key.TyparArity, Block.toList (TyparList.kinds shape.Typars)
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
                                (List.zip
                                    (Block.toList td.TypeParams.Names)
                                    (Block.toList (TyparList.kinds td.TypeParams)))
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
                            (SignatureResolutionTests.shapeOf r "carrier`1").Typars
                            (TyparList.ofSeq [ "'u", TyparKind.Measure ])
                            "an extern primitive carries its declared kinds"
                    }

                    test "an opaque declaration's kinds reach its published shape" {
                        let r =
                            SignatureResolutionTests.resolveFsi
                                "M.fsi"
                                "namespace App\n\nmodule M =\n    type Carrier<[<Measure>] 'u>\n"

                        Expect.equal
                            (SignatureResolutionTests.shapeOf r "Carrier`1").Typars
                            (TyparList.ofSeq [ "'u", TyparKind.Measure ])
                            "an opaque type carries its declared kinds"
                    }
                ]

            testList
                "a measure typar is an atom of its declaration"
                [
                    test "`float<'u>` in a record field resolves" {
                        expectClean "type Pair<[<Measure>] 'u, 'a> = { V: 'a; W: float<'u> }\n"
                    }

                    test "`float<'u>` in a class field and a member signature resolves" {
                        expectClean
                            "\
type Vec<[<Measure>] 'u>(x: float<'u>) =
    member _.X: float<'u> = x
    member _.Scale (k: float) : float<'u> = x
"
                    }

                    test "`float<'u>` in a union case field resolves" {
                        expectClean "type Reading<[<Measure>] 'u> = | Sampled of float<'u> | Missing\n"
                    }

                    test "`float<'u>` on a module `let`'s own measure typar resolves" {
                        expectClean "let scale<[<Measure>] 'u> (x: float<'u>) = x\n"
                    }

                    test "`float<'u>` on an abstract slot's own measure typar resolves" {
                        expectClean
                            "\
type IScale =
    abstract Scale<[<Measure>] 'u> : float<'u> -> float<'u>
"
                    }

                    // The atom carries the declaring scope and the `Measures` slot, so the
                    // measure typar of one declaration is unequal to another's.
                    test "a record field's measure typar freezes to a `Typar` atom at its measure slot" {
                        let pools = freezeFor "type Pair<[<Measure>] 'u, 'a> = { V: 'a; W: float<'u> }\n"

                        let td =
                            match (TastUnpool.ofPools pools).Decls with
                            | EqList [ TDeclG.Type td ] -> td
                            | other -> failtestf "expected a single type declaration, got %A" other

                        let fieldTy =
                            match td.Kind with
                            | TTypeKindG.Record r -> r.Fields.[1].Type
                            | other -> failtestf "Pair is not a record: %A" other

                        match fieldTy with
                        | FTConst(_, EqList [ FTMeasure units ]) ->
                            Expect.equal
                                units.Exponents
                                [ MeasureAtom.Typar(TyparScope.Type td.TypeKey, 0<measureSlot>), Rational.One ]
                                "the declaration's own measure typar at measure slot 0"
                        | other -> failtestf "expected a measured carrier, got %A" other
                    }

                    // FS0702: "Expected unit-of-measure parameter, not type parameter."
                    test "a type-kinded typar in measure position is an error" {
                        expectUserErrorReportedAlone
                            "Expected unit-of-measure parameter, not type parameter"
                            "\
type Pair<[<Measure>] 'u, 'a> = { V: float<'a> }
"
                    }

                    // FS0703: "Expected type parameter, not unit-of-measure parameter."
                    test "a measure typar in type position is an error" {
                        expectUserErrorReportedAlone
                            "Expected type parameter, not unit-of-measure parameter"
                            "\
type Pair<[<Measure>] 'u> = { V: list<'u> }
"
                    }

                    // FS0665: a local binding declares no scope for its typars to resolve under.
                    test "an explicit measure typar on a local binding is an error" {
                        expectUserErrorReportedAlone
                            "Explicit type parameters may only be used on module or member bindings"
                            "\
let outer () =
    let g<[<Measure>] 'u> (x: float<'u>) = x
    g
"
                    }

                    test "an explicit typar on a class `let` is an error" {
                        expectUserErrorReportedAlone
                            "Explicit type parameters may only be used on module or member bindings"
                            "\
type C() =
    let g<'a> (x: 'a) = x
    member _.G = g 1
"
                    }

                    // A class `let` has no `TyparScope` case to recover under, so its measure
                    // typar stays a type entry and `float<'u>` reports FS0702 beside FS0665.
                    ptest "an explicit measure typar on a class `let` is FS0665 alone (reports FS0702 beside it)" {
                        expectUserErrorReportedAlone
                            "Explicit type parameters may only be used on module or member bindings"
                            "\
type C() =
    let g<[<Measure>] 'u> (x: float<'u>) = x
    member _.G = g 1.0
"
                    }

                    // fsc generalises an undeclared `'zz` as a measure typar, which needs
                    // measure variables in the store and Abelian-group unification.
                    test "an undeclared measure typar is unsupported (fsc generalises it)" {
                        expectUserErrorReportedAlone
                            "an implicit measure type parameter"
                            "\
let f (x: float<'zz>) = x
"
                    }

                    // The atom is rigid, and a call site has no measure variable to bind it to,
                    // so `scale 1.0<m>` reports "Measure mismatch: <m> vs <'m0>". Instantiating
                    // one needs Abelian-group unification, which is its own plan.
                    ptest "a measure typar is not instantiated at a call site (fsc accepts it)" {
                        expectClean
                            "\
[<Measure>] type m
let scale<[<Measure>] 'u> (x: float<'u>) = x
let y = scale 1.0<m>
"
                    }

                    // fsc infers the wildcard's measure from the use site.
                    test "a measure wildcard is unsupported (fsc infers it)" {
                        expectUserErrorReportedAlone
                            "a measure wildcard"
                            "\
let f (x: float<_>) = x
"
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

                    // FS0039: "The type 'm' is not defined". The annotation is the only
                    // occurrence, because a second one is a second undefined name and reports
                    // again; the literal has its own case below.
                    test "an undeclared measure in a type is undefined" {
                        expectUserErrorReportedAlone
                            "The type 'm' is not defined"
                            "\
let x: float<m> = 1.0
"
                    }

                    // FS0039 at the literal too: `1.0<m>` looks `m` up like any type name.
                    test "an undeclared measure in a literal is undefined" {
                        expectUserErrorReportedAlone
                            "The type 'm' is not defined"
                            "\
let x = 1.0<m>
"
                    }

                    // FS0039: scoping is top-down, as for any type.
                    test "a measure declared after its use is undefined" {
                        expectUserErrorReportedAlone
                            "The type 'm' is not defined"
                            "\
let x: float<m> = 1.0
[<Measure>] type m
"
                    }

                    // FS0001: "The unit of measure 'Q.m' does not match the unit of measure
                    // 'P.m'". Measures are identities, not spellings, so the two DECLARED
                    // PATHS are what the message must carry; a bare "Measure mismatch" would
                    // read the same whether or not the identities differ.
                    test "two measures of one name in different modules are distinct" {
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

                    test "a measure abbreviation expands to its term" {
                        expectClean
                            "\
[<Measure>] type m
[<Measure>] type s
[<Measure>] type v = m / s
let x: float<v> = 1.0<m/s>
"
                    }

                    // The claim reads `[<Measure>]` before the group's later siblings are
                    // claimed, so the reading must not record a verdict against them.
                    test "a measure abbreviation may precede its base measure in one group" {
                        expectClean
                            "\
[<Measure>] type v = m / s
and [<Measure>] m
and [<Measure>] s
let x: float<v> = 1.0<m/s>
"
                    }

                    // A dotted measure name resolves through its module, as a type name does.
                    test "a measure qualified by its module resolves" {
                        expectClean
                            "\
module M =
    [<Measure>] type m
let x: float<M.m> = 1.0<M.m>
"
                    }
                ]

            testList
                "the argument's KIND is checked against the parameter's"
                [
                    // FS0704: "Expected type, not unit-of-measure".
                    test "a measure argument on a type-kinded parameter is an error" {
                        expectUserErrorReportedAlone
                            "Expected type, not unit-of-measure"
                            "\
[<Measure>] type m
type Box<'a> = { V: 'a }
let x: Box<m> = { V = 1 }
"
                    }

                    // FS0703: "Expected type parameter, not unit-of-measure parameter".
                    test "a `when` clause on a measure-kinded parameter is an error" {
                        expectUserErrorReportedAlone
                            "Expected type parameter, not unit-of-measure parameter"
                            "\
type Pair<[<Measure>] 'u, 'a when 'u: equality> = { V: 'a }
"
                    }

                    // FS0704 again: a measure claim in TYPE position.
                    test "a measure in type position is an error" {
                        expectUserErrorReportedAlone
                            "Expected type, not unit-of-measure"
                            "\
[<Measure>] type m
let f (x: m) = x
"
                    }

                    // FS0705: "Expected unit-of-measure, not type".
                    test "a type argument on a measure-kinded parameter is an error" {
                        expectUserErrorReportedAlone
                            "Expected unit-of-measure, not type"
                            "\
let x: float<int> = 1.0
"
                    }

                    // FS0705: the name resolves to a record, so the kind check fails.
                    test "a same-named record in the measure position is a type, not a measure" {
                        expectUserErrorReportedAlone
                            "Expected unit-of-measure, not type"
                            "\
type m = { A: int }
let x: float<m> = 1.0
"
                    }

                    // The kinds are minted with the claim, so a reference resolves them before
                    // the declaration's own entry registers.
                    test "a forward reference within a `type … and …` group reads the claim's kinds" {
                        expectClean
                            "\
[<Measure>] type m
type A = { X: B<m> }
and B<[<Measure>] 'u> = { V: int }
"
                    }

                    // FS0704: the local arity-1 claim outranks FSharp.Core's, and its
                    // parameter is a type.
                    test "a local generic `float` outranks the measured primitive" {
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
                    test "`string<m>` reports the arity alone" {
                        expectUserErrorReportedAlone
                            "expects 0 type argument"
                            "\
[<Measure>] type m
let x: string<m> = \"\"
"
                    }

                    // FS0033 alone: the abbreviation is claimed at arity 0 only, and the
                    // measure argument is not read past the arity check.
                    test "an abbreviation of a carrier at arity 0 reports the arity alone" {
                        expectUserErrorReportedAlone
                            "expects 0 type argument"
                            "\
[<Measure>] type m
type MyFloat = float
let x: MyFloat<m> = 1.0<m>
"
                    }
                ]

            // A target-optional primitive the compiling target declares no contract for
            // resolves to its language-known key at the arities the language knows it at, and
            // `PlatformTypes` reports the mention. `decimal` is CLR-only, so the JS contract
            // is the real stack with the carrier absent.
            testList
                "a carrier the target lacks"
                [
                    test "`decimal<m>` on a target without `decimal` is unsupported alone" {
                        Expect.equal
                            (jsErrors "[<Measure>] type m\nlet f (x: decimal<m>) = x\n")
                            [ "decimal is not supported on the js target" ]
                            "the platform error, and the measured spelling is accepted"
                    }

                    // FS0033 against the nearest language-known arity, beside the platform error.
                    test "`decimal<m, s>` on a target without `decimal` reports the arity and the platform" {
                        Expect.equal
                            (List.sort (
                                jsErrors "[<Measure>] type m\n[<Measure>] type s\nlet f (x: decimal<m, s>) = x\n"
                            ))
                            [
                                "Type 'decimal' expects 1 type argument(s) but got 2"
                                "decimal is not supported on the js target"
                            ]
                            "both errors, nothing else"
                    }

                    // `string` is language-known at arity 0 alone, so `string<m>` is FS0033 on
                    // every target; on one that lacks `string` the platform error stands beside it.
                    test "`string<m>` on a target without `string` reports the arity and the platform" {
                        let m = SymbolKeyOps.typeKeyOfArity "Units" "m" 0

                        let provider =
                            ExternalSymbolProviders.stack
                                (ValueSome(SymbolHome.InAssembly(AssemblyName "Units")))
                                []
                                [ providerOfTypes [ m, ExternalTypeShape.Measure(MeasureTerm.atom m) ] ]

                        let lexed, file = parseFile "let f (x: string<Units.m>) = x\n"

                        let tast =
                            Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                        Expect.equal
                            (List.sort (errorMessages tast.Diagnostics))
                            [
                                "Type 'string' expects 0 type argument(s) but got 1"
                                "string is not supported on the clr target"
                            ]
                            "both errors, nothing else"
                    }
                ]

            testList
                "a measured root freezes to its carrier"
                [
                    // F#'s own representation: the carrier's arity-1 claim applied to the
                    // measure, which each backend erases to the abbreviation's expansion.
                    test "a measured let freezes to `float<m>`, the arity-1 claim over the measure" {
                        let frozen = freezeFor "[<Measure>] type m\nlet x = 1.0<m>\n"
                        let es = errorMessages (FrozenPools.blockingErrors frozen)
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        Expect.equal
                            (soleFrozenLetType frozen)
                            (FTConst(
                                FrozenTypeBridge.measuredClaimKey RuntimeNames.floatKey,
                                Block.singleton (FTMeasure(MeasureTerm.atom (SymbolKeyOps.typeKeyOf "" "m")))
                            ))
                            "the frozen type is the measured carrier"
                    }

                    // The provider's operator is the node's `IntrinsicKey` on the measured
                    // path too, so Elaborate finds the callee whose spliced body operates on
                    // the erased carrier.
                    test "measured arithmetic elaborates to a resolved operator call" {
                        let frozen = freezeFor "[<Measure>] type m\nlet x = 2.0<m> * 3.0<m>\n"
                        let es = errorMessages (FrozenPools.blockingErrors frozen)
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        let unresolved =
                            frozen.ExprPayloads
                            |> Array.exists (
                                function
                                | ExprPayload.Unresolved -> true
                                | _ -> false
                            )

                        Expect.isFalse unresolved "every node of the frozen tree is resolved"
                    }
                ]

            testList
                "a measure declared in another unit or assembly is published"
                [
                    // A `[<Measure>]` declaration is a claim like any other, so a later unit
                    // resolves it through the published surface: a base measure as its own
                    // atom, an abbreviation as its expanded term.
                    test "a measure declared in an earlier unit resolves in a later one" {
                        let producer =
                            "\
namespace Test.A

[<Measure>] type m
[<Measure>] type s
[<Measure>] type v = m / s
"

                        let consumer =
                            "\
namespace Test.B

open Test.A

module N =
    let a: float<m> = 1.0<m>
    let b: float<v> = 1.0<m/s>
    let c: float<Test.A.m> = a
"

                        let units = [ impl "file1.fs" producer; impl "file2.fs" consumer ]

                        let analysed = analyseUnits units

                        let es =
                            analysed.Units
                            |> List.collect UnitOutcome.surfaced
                            |> List.map (fun d -> d.Diagnostic)
                            |> errorMessages

                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        let published =
                            match analysed.Units with
                            | UnitOutcome.Analysed u :: _ -> u.Published
                            | _ -> failtest "the producer did not analyse"

                        let m = SymbolKeyOps.typeKeyOfArity "Test.A" "m" 0
                        let s = SymbolKeyOps.typeKeyOfArity "Test.A" "s" 0
                        let v = SymbolKeyOps.typeKeyOfArity "Test.A" "v" 0

                        Expect.equal
                            (published.TryLookupType m)
                            (ValueSome(ExternalTypeShape.Measure(MeasureTerm.atom m)))
                            "a base measure publishes as its own atom"

                        Expect.equal
                            (published.TryLookupType v)
                            (ValueSome(
                                ExternalTypeShape.Measure(MeasureTerm.div (MeasureTerm.atom m) (MeasureTerm.atom s))
                            ))
                            "a measure abbreviation publishes its expanded term"
                    }

                    test "a `.fsi` declaring measures publishes them and matches its implementation" {
                        let es = assemblyErrors [ measuredSignaturePair ]
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)
                    }

                    // The published type of `speed` is its frozen type, so the consumer thaws
                    // the measured claim back to `float<m/s>` and its annotation agrees.
                    test "a consumer annotates a published measured value with its measure" {
                        let consumer =
                            "\
namespace Test.B

open Test.A

module N =
    let x: float<m/s> = M.speed
"

                        let es = assemblyErrors [ measuredSignaturePair; impl "file2.fs" consumer ]
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)
                    }

                    // FS0704, as for a measure declared in the same file.
                    test "a measure from another unit in type position is an error" {
                        let es =
                            assemblyErrors
                                [
                                    impl "file1.fs" "namespace Test.A\n\n[<Measure>] type m\n"
                                    impl "file2.fs" "module Test.B\n\nlet f (x: Test.A.m) = x\n"
                                ]

                        expectUserErrorIn es "Expected type, not unit-of-measure"
                        Expect.equal es.Length 1 (sprintf "reported alone; diagnostics were %A" es)
                    }

                    // FS0705, as for a type declared in the same file.
                    test "a type from another unit in measure position is an error" {
                        let es =
                            assemblyErrors
                                [
                                    impl "file1.fs" "namespace Test.A\n\ntype R = { A: int }\n"
                                    impl "file2.fs" "module Test.B\n\nlet x: float<Test.A.R> = 1.0\n"
                                ]

                        expectUserErrorIn es "Expected unit-of-measure, not type"
                        Expect.equal es.Length 1 (sprintf "reported alone; diagnostics were %A" es)
                    }

                    test "two measures of one name in different units are distinct" {
                        let es =
                            assemblyErrors
                                [
                                    impl "file1.fs" "namespace Test.P\n\n[<Measure>] type m\n"
                                    impl "file2.fs" "namespace Test.Q\n\n[<Measure>] type m\n"
                                    impl
                                        "file3.fs"
                                        "module Test.R\n\nlet a = 1.0<Test.P.m>\nlet b = 1.0<Test.Q.m>\nlet c = a + b\n"
                                ]

                        expectUserErrorIn es "Measure mismatch"
                        expectErrorIn es "P.m"
                        expectErrorIn es "Q.m"
                    }

                    // The frozen blob is how a measure reaches a referencing assembly.
                    test "a measure declaration round-trips through the frozen codec" {
                        let frozen =
                            freezeFor
                                "namespace Test.A\n\n[<Measure>] type m\n[<Measure>] type s\n[<Measure>] type v = m / s\n"

                        let es = errorMessages (FrozenPools.blockingErrors frozen)
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        let decoded = FrozenCodec.thaw (FrozenCodec.flatten frozen)
                        Expect.equal (TastUnpool.ofPools decoded) (TastUnpool.ofPools frozen) "the tree round-trips"

                        let m = SymbolKeyOps.typeKeyOfArity "Test.A" "m" 0
                        let s = SymbolKeyOps.typeKeyOfArity "Test.A" "s" 0

                        let terms =
                            [
                                for d in (TastUnpool.ofPools decoded).Decls do
                                    match d with
                                    | TDeclG.Type {
                                                      Name = name
                                                      Kind = TTypeKindG.Measure term
                                                  } -> name, term
                                    | _ -> ()
                            ]

                        Expect.equal
                            terms
                            [
                                "m", MeasureTerm.atom m
                                "s", MeasureTerm.atom s
                                "v", MeasureTerm.div (MeasureTerm.atom m) (MeasureTerm.atom s)
                            ]
                            "each measure decl decodes to its term"
                    }

                    // The stack's ambient list SHADOWS the inner providers' implicit opens, so
                    // the contract's own prelude is passed through: `float` at arity 1 is a
                    // `Vesper` claim, and a `Vesper` left closed leaves it unresolved.
                    test "a measure published by a referenced assembly resolves" {
                        let m = SymbolKeyOps.typeKeyOfArity "Units" "m" 0

                        let provider =
                            ExternalSymbolProviders.stack
                                (ValueSome(SymbolHome.InAssembly(AssemblyName "Units")))
                                (ExternalSymbolProviders.collectImplicitOpens [ realProvider.Value ])
                                [
                                    providerOfTypes [ m, ExternalTypeShape.Measure(MeasureTerm.atom m) ]
                                    realProvider.Value
                                ]

                        let lexed, file = parseFile "let x: float<Units.m> = 1.0<Units.m>\n"

                        let tast =
                            Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                        expectCleanTast tast
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
                                    impl "file2.fs" consumer
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

                        let units = [ impl "file1.fs" producer; impl "file2.fs" consumer ]

                        let es = assemblyErrors units
                        Expect.isEmpty es (sprintf "expected no errors; diagnostics were %A" es)

                        Expect.equal (lastLetTypeClaim units) ("Tag", 0) "`Item` is the arity-0 `Tag`'s case"
                    }
                ]
        ]
