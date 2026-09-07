module XParsec.FSharp.Codegen.Js.Tests.JsNativeSymbolsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Js

[<Tests>]
let tests =
    testList
        "JsNativeSymbols scope"
        [
            // `Error` is a GLOBAL: its container is the root namespace, which has no dotted
            // path, so the test builds it directly rather than through `TryContainer`.
            test "the stub table's scope resolves the Error stub in the root namespace" {
                let scope = JsNativeSymbols.provider.Scope
                let root = ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey "")

                match EqArray.toList (scope.TypesNamed(root, "Error")) with
                | [ struct (key, shape) ] ->
                    Expect.equal key (SymbolKeyOps.typeKeyOf "" "Error") "registered identity"
                    Expect.equal shape.TyparArity key.TyparArity "key and shape agree on arity"

                    match shape with
                    | ExternalTypeShape.Class c -> Expect.isFalse c.IsInterface "Error is a class"
                    | other -> failtestf "expected the Error class stub, got %A" other
                | other -> failtestf "expected exactly the Error stub, got %A" other

                Expect.isTrue (scope.TryContainer "Error").IsNone "a stub type is not a container"
                Expect.isTrue (scope.TryContainer "System").IsNone "no namespace is declared"
            }

            test "the stub table's scope publishes no values and no union cases" {
                let scope = JsNativeSymbols.provider.Scope
                let root = ModuleContainer.InNamespace(SymbolKeyOps.namespaceKey "")

                Expect.isTrue
                    (scope.TryValue(SymbolKeyOps.bindingKeyOf root "Error")).IsNone
                    "the table holds types alone"

                Expect.equal (scope.UnionCasesNamed(root, "Error")).Length 0 "no union case"
            }
        ]
