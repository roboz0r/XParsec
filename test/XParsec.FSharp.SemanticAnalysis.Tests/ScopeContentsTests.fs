module XParsec.FSharp.SemanticAnalysis.Tests.ScopeContentsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The entity-scoped query both halves of name resolution answer: a published surface through
// `IExternalSymbolProvider.Scope`, the file's own declarations through `LocalScope`. Each
// answer is filed under the container that declares it, so a same-named case in two modules
// is two answers rather than one bare-name winner.

let private asm: CompilingAssembly =
    {
        Name = AssemblyName "ScopeAsm"
        Target = "none"
    }

/// The provider view each unit pushed for the units after it, in order.
let private publishedViewsOfUnits (units: SourceUnit list) : IExternalSymbolProvider list =
    let analysed =
        AnalysedAssembly.analyse
            Pipeline.analyseFor
            realProvider.Value
            {
                Assembly = asm
                Units = [ for u in units -> AssemblyUnit.parse Set.empty u ]
            }

    analysed.Units
    |> List.map (
        function
        | UnitOutcome.Analysed u -> u.File.View
        | UnitOutcome.Failed(leading, rest) ->
            failtestf
                "unit failed to parse: %A"
                [ for e in leading :: rest -> e.Id.Name, FileFault.diagnostics e.Fault ]
    )

/// `publishedViewsOfUnits` over implementation-only files.
let private publishedViews (files: (string * string) list) : IExternalSymbolProvider list =
    publishedViewsOfUnits
        [
            for (id, text) in files -> SourceUnit.ofImplementation (SourceFile.ofText id text)
        ]

let private lib =
    "\
namespace Test.A

module M =
    let v : int = 5

    type Color =
        | Red
        | Green of int

    type Box<'T> = { Item: 'T }

module N =
    type Light =
        | Red
        | Off
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

/// A `ModuleSuffix` module: `Test.A.Bag` in source, `Test.A.BagModule` compiled.
let private bagImplementation =
    "\
namespace Test.A

type Bag<'T> = { Items: 'T list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bag =
    type Tag = { Label: string }

    let size : int = 0

    let ``a.size`` : int = 0

    [<CompiledName(\"Count\")>]
    let count : int = 0
"

let private bagSignature =
    "\
namespace Test.A

type Bag<'T> = { Items: 'T list }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Bag =
    val size : int

    val ``a.size`` : int

    [<CompiledName(\"Count\")>]
    val count : int
"

let private containerOrFail (scope: IScopeContents) (path: string) : ModuleContainer =
    match scope.TryContainer path with
    | ValueSome c -> c
    | ValueNone -> failtestf "no container at %s" path

let private moduleName (c: ModuleContainer) : string =
    match c with
    | ModuleContainer.InModule m -> m.Name
    | ModuleContainer.InNamespace ns -> "namespace " + ns.Dotted

let private expectBagSpellings (view: IExternalSymbolProvider) : unit =
    let bySource = containerOrFail view.Scope "Test.A.Bag"
    let byCompiled = containerOrFail view.Scope "Test.A.BagModule"
    Expect.equal bySource byCompiled "one module, two spellings"
    Expect.isTrue (view.Scope.TryValue(bySource, "size")).IsSome "its value answers under either"

    match view.Scope.TryValue(bySource, "count"), view.Scope.TryValue(bySource, "Count") with
    | ValueSome bySourceName, ValueSome byCompiledName ->
        Expect.equal bySourceName.Key.Name "Count" "the source short name reaches the compiled binding"
        Expect.equal byCompiledName.Key bySourceName.Key "one binding under both short names"
    | other -> failtestf "a [<CompiledName>] value answers under either short name: %A" other

    // A short name is read off the binding key, never split back out of a rendered one:
    // `` `a.size` `` renders `Test.A.BagModule.a.size`, whose last dotted segment is `size`.
    match view.Scope.TryValue(bySource, "size"), view.Scope.TryValue(bySource, "a.size") with
    | ValueSome plain, ValueSome quoted ->
        Expect.equal plain.Key.Name "size" "a quoted name holding a dot claims no sibling's slot"
        Expect.equal quoted.Key.Name "a.size" "and answers under the whole name it binds"
    | other -> failtestf "both bindings answer under their own short names: %A" other

[<Tests>]
let tests =
    testList
        "ScopeContents"
        [
            testList
                "published surface"
                [
                    test "a module path and each namespace prefix are containers; an unknown path is not" {
                        let scope = (publishedViews [ "lib.fs", lib ]).[0].Scope

                        Expect.equal (moduleName (containerOrFail scope "Test.A.M")) "M" "the module"
                        Expect.equal (moduleName (containerOrFail scope "Test.A")) "namespace Test.A" "its namespace"

                        Expect.equal
                            (moduleName (containerOrFail scope "Test"))
                            "namespace Test"
                            "the namespace's prefix"

                        Expect.isTrue (scope.TryContainer "Test.A.Nope").IsNone "no such module"
                        Expect.isTrue (scope.TryContainer "Other").IsNone "no such namespace"
                    }

                    test "a value answers in its declaring module only" {
                        let scope = (publishedViews [ "lib.fs", lib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"
                        let ns = containerOrFail scope "Test.A"

                        match scope.TryValue(m, "v") with
                        | ValueSome sym -> Expect.equal sym.Key.Name "v" "the binding"
                        | ValueNone -> failtest "v is declared in M"

                        Expect.isTrue (scope.TryValue(m, "w")).IsNone "no such value"
                        Expect.isTrue (scope.TryValue(ns, "v")).IsNone "v is not declared directly in the namespace"
                    }

                    test "a same-named case in two modules is two answers, each carrying its union" {
                        let scope = (publishedViews [ "lib.fs", lib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"
                        let n = containerOrFail scope "Test.A.N"

                        match scope.UnionCasesNamed(m, "Red"), scope.UnionCasesNamed(n, "Red") with
                        | EqOne inM, EqOne inN ->
                            Expect.equal inM.UnionKey.Name "Color" "M's Red is Color's"
                            Expect.equal inN.UnionKey.Name "Light" "N's Red is Light's"
                            Expect.isFalse inM.IsRequireQualifiedAccess "Color is not RQA"
                        | other -> failtestf "both modules declare a Red: %A" other

                        Expect.isEmpty (scope.UnionCasesNamed(m, "Off")) "Off is N's, not M's"
                    }

                    test "an RQA union's case answers with the flag set" {
                        let scope = (publishedViews [ "lib.fs", rqaLib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"

                        match scope.UnionCasesNamed(m, "Red") with
                        | EqOne uc -> Expect.isTrue uc.IsRequireQualifiedAccess "the flag rides the answer"
                        | other -> failtestf "Red resolves through its module; the report is the caller's: %A" other
                    }

                    test "types answer by name within a module, carrying their arity" {
                        let scope = (publishedViews [ "lib.fs", lib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"

                        match scope.TypesNamed(m, "Box") with
                        | EqList [ struct (key, _) ] ->
                            Expect.equal key.Name "Box" "the type"
                            Expect.equal key.TyparArity 1 "generic in one"
                        | other -> failtestf "one Box: %A" other

                        Expect.equal (scope.TypesNamed(m, "Light")).Length 0 "Light is N's"
                    }

                    test "a signature publishes a ModuleSuffix module under its source path and its compiled name" {
                        let unit =
                            SourceUnit.paired
                                (SourceFile.ofText "lib.fsi" bagSignature)
                                (SourceFile.ofText "lib.fs" bagImplementation)

                        expectBagSpellings (publishedViewsOfUnits [ unit ]).[0]
                    }

                    test "an implementation with no signature publishes the source spelling too" {
                        let view = (publishedViews [ "lib.fs", bagImplementation ]).[0]
                        expectBagSpellings view

                        match ScopeContents.tryValueAt view.Scope "Test.A.Bag.count" with
                        | ValueSome sym -> Expect.equal sym.Key.Name "Count" "the alias carries the compiled key"
                        | ValueNone -> failtest "a [<CompiledName>] binding publishes its source spelling"

                        match view.Scope.TypesNamed(containerOrFail view.Scope "Test.A.Bag", "Tag") with
                        | EqOne(struct (key, _)) ->
                            Expect.equal
                                (SymbolKeyOps.typeMetaName key)
                                "Test.A.BagModule+Tag"
                                "the source path reaches the compiled module"
                        | other -> failtestf "a type the module holds resolves under the module's source path: %A" other
                    }

                    test "a composed stack answers from every file, nearest first" {
                        let views =
                            publishedViews
                                [
                                    "lib.fs", lib
                                    "more.fs",
                                    "\
namespace Test.B

module P =
    let p : int = 1
"
                                ]

                        let scope = (ExternalSymbolProviders.composite (List.rev views)).Scope
                        let m = containerOrFail scope "Test.A.M"
                        let p = containerOrFail scope "Test.B.P"

                        Expect.isTrue (scope.TryValue(m, "v")).IsSome "file 1's value through the stack"
                        Expect.isTrue (scope.TryValue(p, "p")).IsSome "file 2's value through the stack"
                        Expect.isTrue (scope.TryValue(p, "v")).IsNone "no cross-module leak"
                    }
                ]

            testList
                "the file's own declarations"
                [
                    test "the local half answers the same questions over the registry" {
                        let lexed, file = parseFile lib

                        let ctx, _ =
                            Pipeline.analyseSemWithContextFor
                                testCompiling
                                realProvider.Value
                                (LexedFile.ofText lexed)
                                file

                        let m =
                            match LocalScope.tryContainer ctx "Test.A.M" with
                            | ValueSome c -> c
                            | ValueNone -> failtest "M is a local container"

                        let n =
                            match LocalScope.tryContainer ctx "Test.A.N" with
                            | ValueSome c -> c
                            | ValueNone -> failtest "N is a local container"

                        Expect.isTrue (LocalScope.tryContainer ctx "Test.A.Nope").IsNone "no such module"

                        Expect.isTrue (LocalScope.tryValue ctx UseSite.unbounded m "v").IsSome "M declares v"
                        Expect.isTrue (LocalScope.tryValue ctx UseSite.unbounded n "v").IsNone "N does not"

                        match
                            LocalScope.tryUnionCase ctx UseSite.unbounded m "Red",
                            LocalScope.tryUnionCase ctx UseSite.unbounded n "Red"
                        with
                        | ValueSome inM, ValueSome inN ->
                            Expect.equal inM.UnionName "Color" "M's Red"
                            Expect.equal inN.UnionName "Light" "N's Red"
                        | other -> failtestf "both modules declare a Red: %A" other

                        match LocalScope.typesNamed ctx UseSite.unbounded m "Box" with
                        | [ claim ] -> Expect.equal claim.TyparArity 1 "Box<'T>"
                        | other -> failtestf "one Box: %A" other

                        Expect.isEmpty (LocalScope.typesNamed ctx UseSite.unbounded m "Light") "Light is N's"
                    }
                ]
        ]
