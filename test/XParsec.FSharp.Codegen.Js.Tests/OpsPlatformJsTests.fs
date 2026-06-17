module XParsec.FSharp.Codegen.Js.Tests.OpsPlatformJsTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

/// All `ILIntrinsic` opCode (template) strings reachable in an inline body.
let private ilOpCodes (body: InlineBody) : string list =
    let acc = ResizeArray<string>()

    let rec walkExpr (e: TExpr) =
        match e with
        | TExpr.ILIntrinsic(opCode, _, args, _, _) ->
            acc.Add opCode

            for a in args do
                walkExpr a
        | TExpr.StaticOptimization(clauses, dflt, _, _) ->
            for c in clauses do
                walkExpr c.Body

            walkExpr dflt
        | TExpr.Lambda(_, b, _, _) -> walkExpr b
        | _ -> ()

    match body.Decl with
    | TDecl.Let(_, v, _, _) -> walkExpr v
    | _ -> ()

    List.ofSeq acc

[<Tests>]
let tests =
    testList
        "OpsPlatformJs"
        [
            test "target selection swaps in the JS bodies (Math.imul present for js, absent for clr)" {
                let js =
                    SymbolProviders.contractInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                let clr = SymbolProviders.contractInlineBodiesFor None [ vesperCoreManifest ]

                let jsMul = ilOpCodes js.["op_Multiply"]
                let clrMul = ilOpCodes clr.["op_Multiply"]

                Expect.contains jsMul "Math.imul($0, $1)" "js `*` int32 clause is the Math.imul template"
                Expect.contains clrMul "mul" "clr `*` base is the CIL `mul` mnemonic"

                Expect.isFalse
                    (clrMul |> List.exists (fun s -> s.Contains "Math.imul"))
                    "the CLR collection of the same manifest does NOT pick up the JS template"
            }

            test "arithmetic operator bodies are collected as cross-package inlines (js target)" {
                let js =
                    SymbolProviders.contractInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                for name in
                    [
                        "op_Addition"
                        "op_Subtraction"
                        "op_Multiply"
                        "op_Division"
                        "op_Modulus"
                        "op_UnaryNegation"
                    ] do
                    Expect.isTrue (Map.containsKey name js) (sprintf "%s sourced from ops-platform.js.fs" name)
            }

            test "the int32 / int64 / float clauses freeze with their JS templates intact" {
                let js =
                    SymbolProviders.contractInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                let add = ilOpCodes js.["op_Addition"]

                Expect.contains add "$0 + $1" "float base template"
                Expect.contains add "($0 + $1) | 0" "int32 `| 0` truncation clause"
                Expect.contains add "BigInt.asIntN(64, $0 + $1)" "int64 BigInt-wrap clause"

                // Unary negation is a static-opt with base + int + int64 clauses.
                let neg = ilOpCodes js.["op_UnaryNegation"]
                Expect.contains neg "-$0" "unary-neg float base"
                Expect.contains neg "(-$0) | 0" "unary-neg int32 clause"

                // The `%` in the modulus template is NOT a printf placeholder — must survive freeze verbatim.
                let modOps = ilOpCodes js.["op_Modulus"]
                Expect.contains modOps "$0 % $1" "modulus `%` carried through verbatim"
                Expect.contains modOps "($0 % $1) | 0" "modulus int32 clause keeps the bare `%`"
            }

            test "equality operators freeze with `===` primitive clauses + structural-call base" {
                let js =
                    SymbolProviders.contractInlineBodiesFor (Some Target.Js) [ vesperCoreManifest ]

                // The aggregate base is a CALL to `structuralEquals`, not an IL template;
                // `ilOpCodes` sees the `===` clauses but no `equals(` opcode.
                let eq = ilOpCodes js.["op_Equality"]
                Expect.contains eq "$0 === $1" "primitive `===` clause"

                Expect.isFalse
                    (eq |> List.exists (fun s -> s.Contains "equals"))
                    "the structural base is an external call, not a bare-name IL template"

                let neq = ilOpCodes js.["op_Inequality"]
                Expect.contains neq "$0 !== $1" "primitive `!==` clause"
                Expect.contains neq "!$0" "the base negates the `structuralEquals` call via a `!$0` template"

                Expect.isFalse
                    (neq |> List.exists (fun s -> s.Contains "equals"))
                    "the negated base wraps a call, not a bare-name IL template"
            }

            test "JS numeric reprs: canon identities stay distinct while both platform-project to `number`" {
                // `int` and `float` must keep distinct canon faces; a shared repr would conflate %d/%f
                // and integer division.
                let js = SymbolProviders.buildContractFor (Some Target.Js) [ vesperCoreManifest ]

                let facesOf name =
                    match js.TryLookupType name with
                    | ValueSome(ExternalTypeShape.Intrinsic(canon = canon; platform = Some platform)) -> canon, platform
                    | other -> failtestf "expected %s as an Intrinsic shape with a JS repr, got %A" name other

                let intCanon, intPlat = facesOf "Vesper.int"
                let floatCanon, floatPlat = facesOf "Vesper.float"

                // Identity axis — the canon faces ARE the `.fsi` names, platform-
                // INVARIANT (a JS build never sees a BCL name) and distinct, so the
                // unifier never conflates `int` with `float`.
                Expect.equal intCanon "int" "int canon identity is the `.fsi` name"
                Expect.equal floatCanon "float" "float canon identity is the `.fsi` name"
                Expect.notEqual intCanon floatCanon "int and float MUST keep distinct canon identities"

                Expect.equal intPlat "number" "int platform face repoints to JS `number`"
                Expect.equal floatPlat "number" "float platform face repoints to JS `number`"
                Expect.notEqual intCanon intPlat "the two faces genuinely diverge on JS (identity ≠ runtime repr)"
            }

            test "CLR target: canon is the `.fsi` name, platform is the BCL repr" {
                let clr = SymbolProviders.buildContractFor None [ vesperCoreManifest ]

                match clr.TryLookupType "Vesper.int" with
                | ValueSome(ExternalTypeShape.Intrinsic(canon = canon; platform = Some platform)) ->
                    Expect.equal canon "int" "int canon on CLR is the `.fsi` name"
                    Expect.equal platform "System.Int32" "int platform face on CLR is the BCL repr"
                    Expect.notEqual canon platform "the two faces diverge on CLR too (identity ≠ runtime repr)"
                | other -> failtestf "expected Vesper.int as an Intrinsic shape, got %A" other
            }

            test "JS target: unit -> undefined, int64/uint64 -> bigint (canon = `.fsi` name)" {
                // `number` loses precision past 53 bits, so int64/uint64 must use `bigint`.
                let js = SymbolProviders.buildContractFor (Some Target.Js) [ vesperCoreManifest ]

                let facesOf name =
                    match js.TryLookupType name with
                    | ValueSome(ExternalTypeShape.Intrinsic(canon = canon; platform = Some platform)) -> canon, platform
                    | other -> failtestf "expected %s as an Intrinsic shape with a JS repr, got %A" name other

                Expect.equal (facesOf "Vesper.unit") ("unit", "undefined") "unit -> undefined on JS"
                Expect.equal (facesOf "Vesper.int64") ("int64", "bigint") "int64 -> bigint on JS"
                Expect.equal (facesOf "Vesper.uint64") ("uint64", "bigint") "uint64 -> bigint on JS"
            }
        ]
