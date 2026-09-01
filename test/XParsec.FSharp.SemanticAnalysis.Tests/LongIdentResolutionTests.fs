module XParsec.FSharp.SemanticAnalysis.Tests.LongIdentResolutionTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// How a dotted name resolves, in each syntactic position, across and within files: through a
// module path, a type, or bare after an `open`, in F#'s order. The `ptest`s of the
// "`open` precedence" list pin `docs/open-overhaul-plan.md`, and each names the step of that
// plan which un-pends it.

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
            Pipeline.analyseFileFor
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

/// Pending twin of `resolves`: `dotnet fsi` accepts this source and the analysis does not yet.
let private presolves (name: string) (file1: string) (file2: string) =
    ptest name {
        let all = analyse [ impl "file1.fs" file1; impl "file2.fs" file2 ]

        for f in all do
            Expect.isEmpty (errorsOf f) (sprintf "%A: %A" f.Retained.Path (errorsOf f))
    }

/// Pending twin of `reports`.
let private preports (name: string) (expected: Diagnostic -> bool) (file1: string) (file2: string) =
    ptest name {
        let all = analyse [ impl "file1.fs" file1; impl "file2.fs" file2 ]
        let errors = errorsOf all.[1]
        Expect.isTrue (errors |> List.exists expected) (sprintf "expected diagnostic absent; got %A" errors)
    }

let private typeMismatch (d: Diagnostic) : bool = d.Message.Contains "Type mismatch"

let private unresolvedIdentifier (d: Diagnostic) : bool =
    d.Message.Contains "Unresolved identifier"

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

// --- file 1 for the `open`-precedence suite ---------------------------------------------
//
// Sibling modules declare the same name at different types, so which one resolved is legible
// from the type of the binding that used it.

let private openOrderLib =
    "\
namespace Test

module Lib =

    module A =
        let f () : int = 1

    module B =
        let f () : string = \"2\"

    module C =
        let g () : string = \"20\"

    module U1 =
        type E = Zip of int

        type T = { Q: int }

    module U2 =
        type E = Zip of string

        type T = { Q: string }
"

let private autoOpenLib =
    "\
namespace Test.Auto

[<AutoOpen>]
module Auto =
    let h () : string = \"auto\"

module D =
    let h () : int = 7
"

// --- file 1: `module Foo` beside `type Foo`, so the module compiles to `FooModule` -------

/// The suffixed module publishes a TYPE only.
let private suffixedTypeOnlyLib =
    "\
namespace Test.Suffix

type Foo =
    | A
    | B

module Foo =
    type Inner = | Zip
"

/// The same module, publishing a value as well.
let private suffixedWithValueLib =
    "\
namespace Test.Suffix

type Foo =
    | A
    | B

module Foo =
    type Inner = | Zip
    let seed = 1
"

/// A suffixed module holding a nested module.
let private suffixedNestedLib =
    "\
namespace Test.Suffix

type Foo =
    | A
    | B

module Foo =
    module Bar =
        type Inner = | Zip
        let seed = 1
"

/// No name collision, so the compiled and source names agree: the control.
let private unsuffixedLib =
    "\
namespace Test.Suffix

module Foo =
    type Inner = | Zip
    let seed = 1
"

/// A value whose `[<CompiledName>]` makes its emitted method differ from the name its source
/// writes.
let private compiledNameValueLib =
    "\
namespace Test.Suffix

module Bag =
    [<CompiledName(\"Empty\")>]
    let empty : int = 0
"

// --- what a consumer that never sees the declaration reads ------------------------------

/// The surface `file1` publishes, taken through the blob so the assertions cover the freeze.
let private publishedBy (file1: string) : PublishedSurface =
    let f = (analyse [ impl "file1.fs" file1 ]).[0]
    Expect.isEmpty (errorsOf f) (sprintf "%A" (errorsOf f))
    FrozenSignature.toSurface f.Retained (FrozenCodec.thaw (FrozenCodec.flatten f.Frozen))

let private compiledNameText (CompiledName n) : string = n

/// Every published module keyed against the class name it emits as, ordinal-ordered by
/// `PublishedSurface`. The key half is asserted too: the channel IS the association, and a
/// compiled name landing on the wrong module is what the key flip risks.
let private publishedModuleNames (file1: string) : (string * string) list =
    [
        for e in (publishedBy file1).CompiledModuleNames -> SymbolKeyOps.moduleFullName e.Key, compiledNameText e.Value
    ]

/// Every published value keyed against the method name it emits as.
let private publishedValueNames (file1: string) : (string * string) list =
    [
        for e in (publishedBy file1).Symbols do
            match e.Value.CompiledName with
            | ValueSome cn -> SymbolKeyOps.qualifiedName (SymbolKey.Binding e.Key), compiledNameText cn
            | ValueNone -> ()
    ]

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

            // A module whose COMPILED name differs from the name its source writes
            // (`module Foo` beside `type Foo` compiles to `FooModule`). `dotnet fsi` accepts
            // every case here; a `ptest` is one this analysis does not yet, and
            // `docs/semantic-names-in-keys-plan.md` un-pends them. The boundary is exactly
            // whether the module publishes a VALUE: a published value carries the source path
            // of its declaring module, and nothing else contributes one.
            testList
                "a module whose compiled name differs"
                [
                    testList
                        "holds today"
                        [
                            resolves
                                "CONTROL: an unsuffixed module is reached by `open`"
                                unsuffixedLib
                                "namespace Consumer

open Test.Suffix.Foo

module M =
    let f (v: Inner) = v
"
                            resolves
                                "a suffixed module publishing a value is reached by `open`"
                                suffixedWithValueLib
                                "namespace Consumer

open Test.Suffix.Foo

module M =
    let f (v: Inner) = v
"
                            resolves
                                "a suffixed module publishing a value supplies a bare name"
                                suffixedWithValueLib
                                "namespace Consumer

open Test.Suffix.Foo

module M =
    let n : int = seed
"
                            resolves
                                "a suffixed module publishing a value is reached by a qualified path"
                                suffixedWithValueLib
                                "namespace Consumer

module M =
    let n : int = Test.Suffix.Foo.seed
"
                            resolves
                                "a module nested under a suffixed one is reached by `open`"
                                suffixedNestedLib
                                "namespace Consumer

open Test.Suffix.Foo.Bar

module M =
    let n : int = seed
"
                            // Within the file, the source path is what `LocalContainers` is
                            // keyed by, so the suffix never surfaces.
                            resolves
                                "SAME FILE: a suffixed module publishing only a type is reached by a qualified path"
                                "namespace Test.Suffix

type Foo =
    | A
    | B

module Foo =
    type Inner = | Zip

module M =
    let f (v: Foo.Inner) = v
"
                                "namespace Consumer

module Q =
    let z = 1
"
                            resolves
                                "SAME FILE: a suffixed module publishing only a type is reached by `open`"
                                "namespace Test.Suffix

type Foo =
    | A
    | B

module Foo =
    type Inner = | Zip

module M =
    open Foo
    let f (v: Inner) = v
"
                                "namespace Consumer

module Q =
    let z = 1
"
                        ]

                    testList
                        "pending"
                        [
                            // The published surface indexes a module under its COMPILED path;
                            // the source path arrives only with a published value, which a
                            // module holding one type does not have.
                            presolves
                                "a suffixed module publishing only a type is reached by `open`"
                                suffixedTypeOnlyLib
                                "namespace Consumer

open Test.Suffix.Foo

module M =
    let f (v: Inner) = v
"
                            presolves
                                "a suffixed module publishing only a type is reached by a qualified path"
                                suffixedTypeOnlyLib
                                "namespace Consumer

module M =
    let f (v: Test.Suffix.Foo.Inner) = v
"
                            // The descent composes a COMPILED prefix with a SOURCE segment,
                            // spelling `Test.Suffix.FooModule.Bar`, which no index holds.
                            presolves
                                "a module nested under a suffixed one is reached by a qualified path"
                                suffixedNestedLib
                                "namespace Consumer

module M =
    let n : int = Test.Suffix.Foo.Bar.seed
"
                        ]

                    // The compiled name as a published fact of its own, independent of the
                    // key: what `Layout`, `ClrEnv` and `ClrRecipes` read once the keys hold
                    // semantic names. Absence means the declaration compiles under the name
                    // its source writes.
                    testList
                        "publishes its compiled name"
                        [
                            test "a module publishing only a type publishes it" {
                                Expect.equal
                                    (publishedModuleNames suffixedTypeOnlyLib)
                                    [ "Test.Suffix.FooModule", "FooModule" ]
                                    "the suffix survives the freeze without a published value"
                            }

                            test "a module publishing a value publishes it" {
                                Expect.equal
                                    (publishedModuleNames suffixedWithValueLib)
                                    [ "Test.Suffix.FooModule", "FooModule" ]
                                    "the suffix survives the freeze"
                            }

                            test "only the suffixed module of a nested pair publishes one" {
                                Expect.equal
                                    (publishedModuleNames suffixedNestedLib)
                                    [ "Test.Suffix.FooModule", "FooModule" ]
                                    "`Foo` carries the suffix and `Bar` compiles under its source name"
                            }

                            test "CONTROL: an unsuffixed module publishes none" {
                                Expect.isEmpty
                                    (publishedModuleNames unsuffixedLib)
                                    "no declaration here compiles under another name"
                            }

                            test "`[<CompiledName>]` on a value publishes it" {
                                Expect.equal
                                    (publishedValueNames compiledNameValueLib)
                                    [ "Test.Suffix.Bag.Empty", "Empty" ]
                                    "the attribute's name survives the freeze"
                            }

                            test "CONTROL: a value with no `[<CompiledName>]` publishes none" {
                                Expect.isEmpty
                                    (publishedValueNames unsuffixedLib)
                                    "`seed` compiles under the name its source writes"
                            }
                        ]
                ]

            // `open` precedence, as `dotnet fsi` has it. Every case here was probed against the
            // real compiler; the negative controls all produced FS0001, so a positive case
            // cannot pass by the name going unresolved. A `ptest` is a case F# accepts and the
            // analysis does not yet, and the step of `docs/open-overhaul-plan.md` that un-pends
            // it names it.
            testList
                "`open` precedence"
                [
                    testList
                        "holds today"
                        [
                            resolves
                                "an absolute `open` reaches a bare name"
                                openOrderLib
                                "\
open Test.Lib.A
let x : int = f ()
"
                            reports
                                "an absolute `open` resolves to that module's `f`"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib.A
let x : string = f ()
"
                            resolves
                                "a later `let` shadows an earlier `open`"
                                openOrderLib
                                "\
open Test.Lib.C
let g () : int = 10
let z : int = g ()
"
                            reports
                                "the shadowing `let` supplies the type"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib.C
let g () : int = 10
let z : string = g ()
"
                            resolves
                                "an inner declaration beats an outer `open`"
                                openOrderLib
                                "\
open Test.Lib.A

module M =
    let f () : string = \"inner\"
    let y : string = f ()
"
                            reports
                                "an `open` ends with the module holding it"
                                unresolvedIdentifier
                                openOrderLib
                                "\
open Test.Lib

module Inner =
    open C
    let z : string = g ()

let after = g ()
"
                            // Whether a `rec` scope hoists an `open` never arises: F# refuses
                            // the source, FS3200, and so does this compiler.
                            reports
                                "FS3200: in a `rec` group, `open` comes first in each module"
                                (fun d -> d.Message.Contains "'open' declarations must come first")
                                openOrderLib
                                "\
module rec R =
    let a : int = 1
    open Test.Lib
    let b : string = C.g ()
"
                        ]

                    // An `open` written relative to an enclosing one, on the bare-name route.
                    testList
                        "a relative `open` on the bare-name route"
                        [
                            resolves
                                "`open Test.Lib` then `open A` reaches a bare `f`"
                                openOrderLib
                                "\
open Test.Lib
open A
let x : int = f ()
"
                            reports
                                "a relative `open` resolves to that module's `f`"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib
open A
let x : string = f ()
"
                        ]

                    // One ranked stack: a declaration and an `open` each take effect from their
                    // own `BindingRank`, so within a scope the later of the two wins, for values,
                    // union cases and types alike.
                    testList
                        "one ranked stack"
                        [
                            resolves
                                "a later `open` shadows an earlier `open`"
                                openOrderLib
                                "\
open Test.Lib

open A
let x : int = f ()
open B
let y : string = f ()
"
                            reports
                                "the later `open` supplies `f`"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib
open A
open B
let y : int = f ()
"
                            resolves
                                "a later `open` shadows an earlier local declaration"
                                openOrderLib
                                "\
open Test.Lib

let g () : int = 10
open C
let z : string = g ()
"
                            reports
                                "the later `open` supplies `g`"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib
let g () : int = 10
open C
let z : int = g ()
"
                            // No annotation, and the two `Zip`s carry different payloads: F#'s
                            // type-directed disambiguation can reach a SHADOWED case through an
                            // expected type, so an annotated case test proves nothing.
                            resolves
                                "a later `open` shadows an earlier `open` for a union case"
                                openOrderLib
                                "\
open Test.Lib.U1
let c1 = Zip 1
open Test.Lib.U2
let c2 = Zip \"s\"
"
                            reports
                                "the later `open` supplies `Zip`"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib.U1
open Test.Lib.U2
let c = Zip 1
"
                            resolves
                                "a later `open` shadows an earlier local type's case"
                                openOrderLib
                                "\
type E = Zip of bool
open Test.Lib.U2
let c = Zip \"s\"
"
                            reports
                                "the later `open` supplies `Zip` over the local type"
                                typeMismatch
                                openOrderLib
                                "\
type E = Zip of bool
open Test.Lib.U2
let c = Zip true
"
                            resolves
                                "a later `open` shadows an earlier `open` for a record type"
                                openOrderLib
                                "\
open Test.Lib.U1
let t1 = { Q = 1 }
open Test.Lib.U2
let t2 = { Q = \"s\" }
"
                            reports
                                "the later `open` supplies the record type"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib.U1
open Test.Lib.U2
let t = { Q = 1 }
"
                        ]

                    // `Depth` outranks `Offset`. The two axes only disagree under `rec`, where a
                    // declaration is visible from the top of its scope: here the outer `g` reaches
                    // `Inner`, and the deeper `open C` still wins.
                    testList
                        "depth outranks offset"
                        [
                            resolves
                                "a deeper `open` beats a `rec`-hoisted outer declaration"
                                openOrderLib
                                "\
module rec Outer =
    module Inner =
        open Test.Lib.C
        let z : string = g ()

    let g () : int = 10
"
                            reports
                                "the deeper `open` supplies `g`"
                                typeMismatch
                                openOrderLib
                                "\
module rec Outer =
    module Inner =
        open Test.Lib.C
        let z : int = g ()

    let g () : int = 10
"
                        ]

                    // Under `rec`, a scope's own declarations beat the scope's own `open`s
                    // whatever the offsets: FS3200 pins the `open`s ahead of every
                    // declaration, so the declarations enter after the scope's whole prelude
                    // (`BindingRank.afterPrelude`). A deeper `open` still wins on depth.
                    testList
                        "a `rec` scope's own declarations beat its own `open`s"
                        [
                            resolves
                                "a `rec`-hoisted `let` beats the same scope's `open`"
                                openOrderLib
                                "\
module rec R =
    open Test.Lib.C
    let g () : int = 10
    let z : int = g ()
"
                            reports
                                "the `rec`-hoisted `let` supplies `g`"
                                typeMismatch
                                openOrderLib
                                "\
module rec R =
    open Test.Lib.C
    let g () : int = 10
    let z : string = g ()
"
                            resolves
                                "a non-`rec` module of a `rec` scope keeps its own `let` too"
                                openOrderLib
                                "\
module rec Outer =
    module Inner =
        open Test.Lib.C
        let g () : int = 10
        let z : int = g ()
"
                            resolves
                                "a `rec` scope's own type beats the same scope's `open`"
                                openOrderLib
                                "\
module C2 =
    type T = { Q: string }

module rec R =
    open C2
    type T = { Q: int }
    let z : T = { Q = 3 }
"
                            reports
                                "the `rec` scope's own type supplies the fields"
                                typeMismatch
                                openOrderLib
                                "\
module C2 =
    type T = { Q: string }

module rec R =
    open C2
    type T = { Q: int }
    let z : T = { Q = \"s\" }
"
                        ]

                    // A non-`rec` group's bindings scope below the group: inside its own RHS
                    // the name still means whatever was in scope above, probed in `dotnet
                    // fsi` (`let f (x: int) : string = f (x + 1)` under an `open` supplying
                    // `f` typechecks and calls the opened `f`).
                    testList
                        "a group's bindings scope below the group"
                        [
                            resolves
                                "a non-`rec` `let`'s RHS reads the opened value of its own name"
                                openOrderLib
                                "\
open Test.Lib.A
let f (n: string) : int = f ()
let ok : int = f \"x\"
"
                            reports
                                "the opened value supplies the RHS `f`"
                                typeMismatch
                                openOrderLib
                                "\
open Test.Lib.A
let f (n: string) : string = f ()
"
                            reports
                                "a non-`rec` `let` with no outer claim does not see itself"
                                unresolvedIdentifier
                                openOrderLib
                                "\
let selfless x = selfless x
"
                        ]

                    // An `[<AutoOpen>]` module is NOT a floor: it takes the rank of the `open`
                    // that brought its enclosing scope into view, so it shadows anything written
                    // above that `open`. Only a module in scope with no `open` written for it —
                    // an assembly auto-open, or the file's own `namespace` header — is a floor.
                    testList
                        "implicit opens rank at their activating `open`"
                        [
                            resolves
                                "an explicit `open` after the activating one wins"
                                autoOpenLib
                                "\
open Test.Auto
let a : string = h ()
open Test.Auto.D
let b : int = h ()
"
                            resolves
                                "an `[<AutoOpen>]` activated later shadows an earlier explicit `open`"
                                autoOpenLib
                                "\
open Test.Auto.D
let a : int = h ()
open Test.Auto
let b : string = h ()
"
                            reports
                                "the later-activated `[<AutoOpen>]` supplies `h`"
                                typeMismatch
                                autoOpenLib
                                "\
open Test.Auto.D
open Test.Auto
let b : int = h ()
"
                            resolves
                                "an `[<AutoOpen>]` activated later shadows an earlier local declaration"
                                autoOpenLib
                                "\
let h () : int = 5
open Test.Auto
let b : string = h ()
"
                            resolves
                                "an `[<AutoOpen>]` reached through the file's own namespace is a floor"
                                autoOpenLib
                                "\
namespace Test.Auto

module Consumer =
    let a : string = h ()
    open D
    let b : int = h ()
"
                        ]
                ]
        ]
