module XParsec.FSharp.SemanticAnalysis.Tests.ScopeContentsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.AssemblyFiles
open XParsec.FSharp.SemanticAnalysis.AssemblyAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// The entity-scoped query implemented by both halves of name resolution: a published surface
// through `IExternalSymbolProvider.Scope`, the file's own declarations through `LocalScope`.
// Each entry is filed under the container that declares it, so a same-named case in two
// modules yields two entries rather than one bare-name winner.

let private asm: CompilingAssembly =
    {
        Name = AssemblyName "ScopeAsm"
        Target = "none"
    }

/// The provider view each unit pushed for the units after it, in order.
let private publishedViewsOfUnits (units: SourceUnit list) : IExternalSymbolProvider list =
    let analysed =
        AnalysedAssembly.analyse
            Pipeline.analyseFileFor
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

/// One name at three arities in a single module, declared WIDEST first. The published tables
/// are ordered by ORDINAL metadata name, under which `` P`10 `` precedes `` P`2 ``.
let private arityLib =
    "\
namespace Test.Ar

module M =
    type P<'A, 'B, 'C, 'D, 'E, 'F, 'G, 'H, 'I, 'J> = { Ten: int }
    type P<'A, 'B> = { Two: int }
    type P = { N: int }
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

    Expect.isTrue
        (view.Scope.TryContainer "Test.A.BagModule").IsNone
        "the compiled class name is no spelling a source writes"

    let valueNamed (name: string) =
        view.Scope.TryValue(SymbolKeyOps.bindingKeyOf bySource name)

    Expect.isTrue (valueNamed "size").IsSome "its value resolves under the module"

    match valueNamed "count" with
    | ValueSome sym ->
        Expect.equal sym.Key.Name "count" "a [<CompiledName>] value is keyed by the name its source writes"

        Expect.equal sym.CompiledName (ValueSome(CompiledName "Count")) "and states the method it emits as separately"
    | ValueNone -> failtest "a [<CompiledName>] value resolves under its source short name"

    Expect.isTrue (valueNamed "Count").IsNone "the compiled method name is no spelling a source writes"

    // A short name is read off the binding key, never split back out of a rendered one:
    // `` `a.size` `` renders `Test.A.BagModule.a.size`, whose last dotted segment is `size`.
    match valueNamed "size", valueNamed "a.size" with
    | ValueSome plain, ValueSome quoted ->
        Expect.equal plain.Key.Name "size" "a quoted name holding a dot claims no sibling's slot"
        Expect.equal quoted.Key.Name "a.size" "and resolves under the whole name it binds"
    | other -> failtestf "both bindings resolve under their own short names: %A" other

    match bySource with
    | ModuleContainer.InModule m ->
        Expect.equal
            (view.Scope.ModuleClassNameOf m)
            (ModuleClassName.Compiled(CompiledName "BagModule"))
            "the suffixed module states the class it emits as"
    | ModuleContainer.InNamespace _ -> failtest "Test.A.Bag is a module"

    Expect.equal
        (view.Scope.ModuleClassNameOf(SymbolKeyOps.moduleInNamespace "Test.A" "Absent"))
        ModuleClassName.Undeclared
        "a module this surface never published states nothing, rather than defaulting to its source name"

// --- Composition ------------------------------------------------------------------------

let private caseOf (unionKey: TypeKey) (name: string) : ExternalUnionCase =
    {
        UnionKey = unionKey
        Case = ExternalCaseShape.create (name, EqArray.empty)
        IsRequireQualifiedAccess = false
    }

/// A scope declaring `cases` in every container.
let private scopeOfCases (cases: ExternalUnionCase list) : IScopeContents =
    { new IScopeContents with
        member _.TryContainer _ = ValueNone

        member _.TryValue _ = ValueNone

        member _.UnionCasesNamed(_, name) =
            EqArray.ofList
                [
                    for c in cases do
                        if c.Case.Name = name then
                            c
                ]

        member _.TypesNamed(_, _) = EqArray.empty
        member _.ModuleClassNameOf _ = ModuleClassName.Undeclared
    }

let private root = ModuleContainer.InNamespace NamespaceKey.Global

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

                    test "a value resolves in its declaring module only" {
                        let scope = (publishedViews [ "lib.fs", lib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"
                        let ns = containerOrFail scope "Test.A"

                        match scope.TryValue(SymbolKeyOps.bindingKeyOf m "v") with
                        | ValueSome sym -> Expect.equal sym.Key.Name "v" "the binding"
                        | ValueNone -> failtest "v is declared in M"

                        Expect.isTrue (scope.TryValue(SymbolKeyOps.bindingKeyOf m "w")).IsNone "no such value"

                        Expect.isTrue
                            (scope.TryValue(SymbolKeyOps.bindingKeyOf ns "v")).IsNone
                            "v is not declared directly in the namespace"
                    }

                    test "a same-named case in two modules resolves twice, each carrying its union" {
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

                    test "an RQA union's case resolves with the flag set" {
                        let scope = (publishedViews [ "lib.fs", rqaLib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"

                        match scope.UnionCasesNamed(m, "Red") with
                        | EqOne uc ->
                            Expect.isTrue uc.IsRequireQualifiedAccess "the flag is carried on the resolved case"
                        | other -> failtestf "Red resolves through its module; the report is the caller's: %A" other
                    }

                    test "types resolve by name within a module, carrying their arity" {
                        let scope = (publishedViews [ "lib.fs", lib ]).[0].Scope
                        let m = containerOrFail scope "Test.A.M"

                        match scope.TypesNamed(m, "Box") with
                        | EqList [ struct (key, _) ] ->
                            Expect.equal key.Name "Box" "the type"
                            Expect.equal key.TyparArity 1 "generic in one"
                        | other -> failtestf "one Box: %A" other

                        Expect.equal (scope.TypesNamed(m, "Light")).Length 0 "Light is N's"
                    }

                    test "a name declared at several arities resolves narrowest first" {
                        let scope = (publishedViews [ "lib.fs", arityLib ]).[0].Scope
                        let m = containerOrFail scope "Test.Ar.M"

                        let arities =
                            [
                                for struct (key, _) in (scope.TypesNamed(m, "P")).Underlying -> key.TyparArity
                            ]

                        Expect.equal arities [ 0; 2; 10 ] "every declared arity, ascending"
                    }

                    test "a signature publishes a ModuleSuffix module under the path its source writes" {
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
                        | ValueSome sym -> Expect.equal sym.Key.Name "count" "the whole source path reaches the binding"
                        | ValueNone -> failtest "a [<CompiledName>] binding publishes its source spelling"

                        match view.Scope.TypesNamed(containerOrFail view.Scope "Test.A.Bag", "Tag") with
                        | EqOne(struct (key, _)) ->
                            Expect.equal
                                (SymbolKeyOps.typeMetaName key)
                                "Test.A.Bag+Tag"
                                "the type is held by the module, not by the namespace"
                        | other -> failtestf "a type the module holds resolves under the module's source path: %A" other
                    }

                    test "an unsuffixed module states that it emits under the name its source writes" {
                        let scope = (publishedViews [ "lib.fs", arityLib ]).[0].Scope

                        match containerOrFail scope "Test.Ar.M" with
                        | ModuleContainer.InModule m ->
                            Expect.equal
                                (scope.ModuleClassNameOf m)
                                ModuleClassName.SourceName
                                "a published module carrying no suffix is a declaration, not an absence"
                        | ModuleContainer.InNamespace _ -> failtest "Test.Ar.M is a module"
                    }

                    test "a composed stack resolves from every file, nearest first" {
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

                        Expect.isTrue
                            (scope.TryValue(SymbolKeyOps.bindingKeyOf m "v")).IsSome
                            "file 1's value through the stack"

                        Expect.isTrue
                            (scope.TryValue(SymbolKeyOps.bindingKeyOf p "p")).IsSome
                            "file 2's value through the stack"

                        Expect.isTrue (scope.TryValue(SymbolKeyOps.bindingKeyOf p "v")).IsNone "no cross-module leak"
                    }
                ]

            testList
                "the file's own declarations"
                [
                    test "the local half resolves the same lookups over the registry" {
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
            testList
                "composition"
                [
                    test "two sources publishing the SAME case resolve to one; two unions resolve to two" {
                        let color = SymbolKeyOps.typeKeyOfArity "Test" "Color" 0
                        let light = SymbolKeyOps.typeKeyOfArity "Test" "Light" 0

                        let shared = caseOf color "Red"

                        // The `.fsi` and `.fs` halves of one package both publish its cases.
                        let composed =
                            ScopeContents.composite
                                [ scopeOfCases [ shared ]; scopeOfCases [ shared; caseOf light "Red" ] ]

                        match EqArray.toArray (composed.UnionCasesNamed(root, "Red")) with
                        | [| a; b |] ->
                            Expect.equal a.UnionKey color "the shared case, once"
                            Expect.equal b.UnionKey light "the second union's own claim"
                        | other -> failtestf "one entry per case identity: %A" other
                    }

                    test "an implicit open listed twice names its container once" {
                        // A file's own `namespace Vesper` header under an assembly-level
                        // `[<assembly: AutoOpen("Vesper")>]`.
                        let vesper = SymbolKeyOps.namespaceKey "Vesper"

                        match
                            ImplicitOpen.containers
                                [ ImplicitOpen.AssemblyAutoOpen vesper; ImplicitOpen.CurrentFileScope vesper ]
                        with
                        | [ r; v ] ->
                            Expect.equal r root "the root namespace leads"
                            Expect.equal (moduleName v) "namespace Vesper" "the container, once"
                        | other -> failtestf "the root plus one container: %A" other
                    }
                ]
        ]
