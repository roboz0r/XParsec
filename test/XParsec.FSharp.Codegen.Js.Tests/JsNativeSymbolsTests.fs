module XParsec.FSharp.Codegen.Js.Tests.JsNativeSymbolsTests

open Vesper
open Expecto
open XParsec.FSharp.Lexer
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

                match Block.toList (scope.TypesNamed(root, "Error")) with
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

            // The JS backend emits no attributes, so the gate admits every representable
            // constant, including the values ECMA-335 II.23.3 refuses.
            test "the platform facts encode every constant" {
                let facts =
                    match JsNativeSymbols.provider.Platform with
                    | ValueSome facts -> facts
                    | ValueNone -> failtest "the JS stub table IS the platform metadata"

                let at (key: TypeKey) (v: TConstValue) =
                    let ty = FTConst(key, Block.empty)
                    facts.ConstEncoding(ty, TConstExpr.Literal(v, ty, Anchor.nowhere))

                Expect.equal
                    (at RuntimeNames.decimalKey (TConstValue.Decimal 1.5M))
                    ConstEncoding.Encodable
                    "a decimal argument encodes"

                Expect.equal
                    (at RuntimeNames.nativeintKey (TConstValue.Integral(IntValue.NativeInt 1L)))
                    ConstEncoding.Encodable
                    "a nativeint argument encodes"
            }
        ]
