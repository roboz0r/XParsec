module XParsec.FSharp.SemanticAnalysis.Tests.LongIdentResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// How a dotted name resolves, in each syntactic position, across and within files: through a
// module path, a type, or bare after an `open`, in F#'s order. The `ptest`s pin the one gap
// left: a later `open` shadowing an earlier declaration mid-file.

let private asm: CompilingAssembly =
    {
        Name = AssemblyName "LongIdentAsm"
        Target = "none"
    }

let private impl (id: string) (text: string) : SourceUnit =
    SourceUnit.ofImplementation (SourceFile.ofText id text)

/// The analysed files, or a test failure naming the first unit that did not parse.
let private analyse (units: SourceUnit list) : FrozenFile list =
    let analysed =
        AnalysedAssembly.analyse
            Pipeline.analyseFor
            realProvider.Value
            {
                Assembly = asm
                Units = List.map (AssemblyUnit.parse Set.empty) units
            }

    analysed.Units
    |> List.map (
        function
        | UnitOutcome.Analysed u -> u.File
        | UnitOutcome.Failed(leading, rest) ->
            failtestf
                "unit failed to parse: %A"
                [ for e in leading :: rest -> e.Id.Name, FileFault.diagnostics e.Fault ]
    )

let private errorsOf (f: FrozenFile) : Diagnostic list =
    f.Frozen.Residue.Diagnostics |> List.filter Diagnostic.isError

/// Two files, the second resolving against the first's published view; every file free of
/// error-severity diagnostics.
let private resolves (name: string) (file1: string) (file2: string) =
    test name {
        let all = analyse [ impl "file1.fs" file1; impl "file2.fs" file2 ]

        for f in all do
            Expect.isEmpty (errorsOf f) (sprintf "%A: %A" f.Retained.Path (errorsOf f))
    }

/// The second file reports at least one error satisfying `expected`, and NOTHING crashes: a
/// name that resolves is typed, so a deliberate mismatch surfaces as a diagnostic.
let private reports (name: string) (expected: Diagnostic -> bool) (file1: string) (file2: string) =
    test name {
        let all = analyse [ impl "file1.fs" file1; impl "file2.fs" file2 ]
        let errors = errorsOf all.[1]
        Expect.isTrue (errors |> List.exists expected) (sprintf "expected diagnostic absent; got %A" errors)
    }

let private typeMismatch (d: Diagnostic) : bool = d.Message.Contains "Type mismatch"

let private undefinedDiscriminator (d: Diagnostic) : bool =
    match d.Kind with
    | Kind.UndefinedPatternDiscriminator _ -> true
    | _ -> false

// --- file 1: a module holding a value, a function and a union ---------------------------

let private moduleLib =
    "\
namespace Test.A

module M =
    let f (x: int) : int = x + 1

    let v : int = 5

    type Color =
        | Red
        | Green of int
"

let private namespaceLib =
    "\
namespace Test.A

type Color =
    | Red
    | Green of int

module M =
    let g (c: Color) : int = 0
"

let private rqaLib =
    "\
namespace Test.A

module M =
    [<RequireQualifiedAccess>]
    type Color =
        | Red
        | Green of int
"

[<Tests>]
let tests =
    testList
        "LongIdentResolution"
        [
            testList
                "cross-file, resolving today"
                [
                    resolves
                        "module fn, qualified"
                        moduleLib
                        "\
namespace Test.B

module N =
    let a () : int = Test.A.M.f 3
"
                    resolves
                        "module value, qualified"
                        moduleLib
                        "\
namespace Test.B

module N =
    let a () : int = Test.A.M.v
"
                    resolves
                        "module value, opened"
                        moduleLib
                        "\
namespace Test.B

open Test.A.M

module N =
    let a () = v + 1
"
                    resolves
                        "module value, opened, from an anonymous top-level module"
                        moduleLib
                        "\
open Test.A.M

let a = v + 1
"
                    resolves
                        "module-held case, bare after open, construct + match"
                        moduleLib
                        "\
namespace Test.B

open Test.A.M

module N =
    let a () : Color = Green 3

    let b (c: Color) : int =
        match c with
        | Red -> 0
        | Green n -> n
"
                    resolves
                        "module-held case, qualified by its TYPE, construct + match"
                        moduleLib
                        "\
namespace Test.B

open Test.A.M

module N =
    let a () : Color = Color.Green 3

    let b (c: Color) : int =
        match c with
        | Color.Red -> 0
        | Color.Green n -> n
"
                    resolves
                        "namespace-level case, bare after open, construct + match"
                        namespaceLib
                        "\
namespace Test.B

open Test.A

module N =
    let a () : Color = Green 3

    let b (c: Color) : int =
        match c with
        | Red -> 0
        | Green n -> n
"
                    reports
                        "module value is TYPED, not silently accepted: a mismatch is reported"
                        typeMismatch
                        moduleLib
                        "\
namespace Test.B

module N =
    let a () : string = Test.A.M.v
"
                    reports
                        "bare case is TYPED: a mismatch is reported"
                        typeMismatch
                        moduleLib
                        "\
namespace Test.B

open Test.A.M

module N =
    let a () : int = Red
"
                ]

            testList
                "a case qualified by its MODULE"
                [
                    // Expression position: `Test.A.M` is a module path and `Red` a case in the
                    // module's contents, typed as the union, so the mismatch is reported.
                    test "module-qualified nullary case in expression position is typed" {
                        let all =
                            analyse
                                [
                                    impl "file1.fs" moduleLib
                                    impl
                                        "file2.fs"
                                        "\
namespace Test.B

module N =
    let a () : int = Test.A.M.Red
"
                                ]

                        Expect.isTrue (errorsOf all.[1] |> List.exists typeMismatch) (sprintf "%A" (errorsOf all.[1]))
                    }
                    test "module-qualified payload case in expression position is typed" {
                        let all =
                            analyse
                                [
                                    impl "file1.fs" moduleLib
                                    impl
                                        "file2.fs"
                                        "\
namespace Test.B

module N =
    let a () : int = Test.A.M.Green 3
"
                                ]

                        Expect.isTrue (errorsOf all.[1] |> List.exists typeMismatch) (sprintf "%A" (errorsOf all.[1]))
                    }
                    // Pattern position, cross-file, the root namespace as the first segment.
                    test "module-qualified case pattern, cross-file, three segments" {
                        let all =
                            analyse
                                [
                                    impl "file1.fs" moduleLib
                                    impl
                                        "file2.fs"
                                        "\
namespace Test.B

module N =
    let a (c: Test.A.M.Color) : int =
        match c with
        | Test.A.M.Red -> 0
        | Test.A.M.Green n -> n
"
                                ]

                        Expect.isEmpty (errorsOf all.[1]) "resolves as the case"
                    }
                    test "module-qualified case pattern after opening the namespace" {
                        let all =
                            analyse
                                [
                                    impl "file1.fs" moduleLib
                                    impl
                                        "file2.fs"
                                        "\
open Test.A

let a (c: M.Color) =
    match c with
    | M.Red -> 0
    | M.Green n -> n
"
                                ]

                        Expect.isEmpty (errorsOf all.[1]) "resolves as the case"
                    }
                    // Pattern position, SINGLE file: the local half resolves the same query.
                    test "module-qualified case pattern within one file" {
                        let all =
                            analyse
                                [
                                    impl
                                        "file1.fs"
                                        "\
namespace Test.A

module M =
    type Color =
        | Red
        | Green of int

module N =
    let a (c: M.Color) : int =
        match c with
        | M.Red -> 0
        | M.Green n -> n
"
                                ]

                        Expect.isEmpty (errorsOf all.[0]) "resolves as the case"
                    }
                    // FCS resolves the case through the module path and THEN reports FS0035
                    // (`CheckExpressions.fs:2063`); `dotnet fsi` agrees. Here the resolver
                    // carries the flag and `Kind.RequireQualifiedAccessCase` is the report.
                    test "module-qualified case of a RequireQualifiedAccess union resolves, then is reported" {
                        let all =
                            analyse
                                [
                                    impl "file1.fs" rqaLib
                                    impl
                                        "file2.fs"
                                        "\
namespace Test.B

module N =
    let a () : Test.A.M.Color = Test.A.M.Red
"
                                ]

                        let requiresQualification (d: Diagnostic) =
                            match d.Kind with
                            | Kind.RequireQualifiedAccessCase("Color", "Red") -> true
                            | _ -> false

                        Expect.isTrue
                            (errorsOf all.[1] |> List.exists requiresQualification)
                            (sprintf "FS0035 naming Color.Red; got %A" (errorsOf all.[1]))
                    }
                ]

            testList
                "a diagnosed pattern reports"
                [
                    // `Elaborate.run` degrades totally on a diagnosed error: the decls and
                    // every bound-variable table go together, so the report reaches the caller.
                    test "undefined pattern discriminator is REPORTED, not a crash" {
                        let all =
                            analyse
                                [
                                    impl
                                        "file1.fs"
                                        "\
namespace Test.A

module N =
    let a (c: int) : int =
        match c with
        | Nope.Zip -> 0
        | _ -> 1
"
                                ]

                        Expect.isTrue
                            (errorsOf all.[0] |> List.exists undefinedDiscriminator)
                            (sprintf "%A" (errorsOf all.[0]))
                    }
                ]

            // Closing this gap ranks BOTH halves by `BindingRank`, the local registry and the
            // provider stack alike: a declaration and an `open` each take effect from their own
            // rank rather than for the whole file.
            testList
                "GAP: `open` order within a file"
                [
                    // `dotnet fsi`: a later `open` shadows an EARLIER declaration of the same
                    // name, and a later `open` shadows an earlier one. Pinned by TYPE: each
                    // module's `f` returns a different type, so the annotation says which one
                    // resolved.
                    ptest "GAP a later `open` shadows an earlier `open`" {
                        let all =
                            analyse
                                [
                                    impl
                                        "file1.fs"
                                        "\
namespace Test

module Lib =

    module A =
        let f () : int = 1

    module B =
        let f () : string = \"2\"
"
                                    impl
                                        "file2.fs"
                                        "\
open Test.Lib

open A
let x : int = f ()
open B
let y : string = f ()
"
                                ]

                        Expect.isEmpty (errorsOf all.[1]) "x resolves A.f, y resolves B.f"
                    }
                    ptest "GAP a later `open` shadows an earlier local declaration" {
                        let all =
                            analyse
                                [
                                    impl
                                        "file1.fs"
                                        "\
namespace Test

module Lib =

    module C =
        let g () : string = \"20\"
"
                                    impl
                                        "file2.fs"
                                        "\
open Test.Lib

let g () : int = 10
open C
let z : string = g ()
"
                                ]

                        Expect.isEmpty (errorsOf all.[1]) "z resolves C.g, the later open"
                    }
                ]
        ]
