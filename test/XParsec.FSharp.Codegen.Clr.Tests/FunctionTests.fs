module XParsec.FSharp.Codegen.Clr.Tests.FunctionTests

open System.Reflection
open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: function definition + application forms — a single
// argument, currying (multi-arg), partial application, recursion, one static
// method calling another, `inline`, and a higher-order function taking a lambda.
// The table is the broad behavioral net; the anchors beneath it pin the emission
// strategy (top-level fn → static method, capturing fn → closure), the
// `inline`-expansion NodeKey freshening, and the nested-module holder shape that
// the former `Slice3`/`Rung2` milestone files proved.

/// Every emitted `fn$…` static method (the mangled top-level-function form).
let private staticFnMethods (bytes: byte[]) : MethodInfo[] =
    let asm = loadAssembly bytes

    asm.GetTypes()
    |> Array.collect (fun t -> t.GetMethods(BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static))
    |> Array.filter (fun m -> m.Name.StartsWith "fn$")

/// A public static method `name` on the named holder type — for nested-module
/// functions, which carry their *source* name (no `fn$` mangling).
let private moduleStaticMethod (bytes: byte[]) (holder: string) (name: string) : MethodInfo =
    let asm = loadAssembly bytes

    match asm.GetTypes() |> Array.tryFind (fun t -> t.FullName = holder) with
    | None -> failtestf "no `%s` holder type emitted for the nested module" holder
    | Some t ->
        match t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static) with
        | null -> failtestf "`%s` is not a public static method on the `%s` holder" name holder
        | m -> m

[<Tests>]
let tests =
    testList
        "Functions"
        [
            for src, expected in
                [
                    // one argument
                    "let twice x = x + x\nprintfn \"%d\" (twice 21)", "42"
                    // currying: a two-argument function fully applied
                    "let add a b = a + b\nprintfn \"%d\" (add 3 4)", "7"
                    // NOT covered yet: a 3+-argument generic function (`a - b - c`
                    // leaves the params as typars and codegen can't infer the
                    // static-method instantiation for arg 3), and partial
                    // application of a user multi-arg function (`let inc = add 1`
                    // hits "cannot encode SemType: TyVar"). Add rows when they land.
                    // recursion
                    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\nprintfn \"%d\" (fact 5)", "120"
                    // one static method calling another
                    "let inc x = x + 1\nlet add3 x = inc (inc (inc x))\nprintfn \"%d\" (add3 10)", "13"
                    // inline
                    "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)", "42"
                    // higher-order: a function parameter applied to an argument
                    "let apply f x = f x\nprintfn \"%d\" (apply (fun n -> n + 1) 41)", "42"
                ] -> test src { runs expected src }

            // ---- inline expansion (former Slice3 anchors) --------------------
            yield
                test "an inline binding keeps its template and expands its use site pre-freeze" {
                    // the `let inline succ` template (decl 0)
                    // is retained verbatim, but the use site `succ 41` is now
                    // expanded *pre-freeze* by `InlineExpansion` — the call beta-
                    // reduces to a `Let` binding the argument 41 over `succ`'s
                    // `x + 1` body (the `op_Addition` head left for codegen's
                    // `BuiltinOps`). (Previously this stayed an `App(Var, 41)` call
                    // head for codegen to expand.)
                    let tast = analyse "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)"
                    Expect.isEmpty tast.Diagnostics "no diagnostics"

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple _,
                                         TExpr.Lambda _,
                                         true,
                                         TyFun(TyConst("int", _), TyConst("int", _)))
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(_,
                                           TExpr.Let(TPat.NamedSimple _,
                                                     TExpr.Const(TConstValue.Int 41, _),
                                                     TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _),
                                                                         TExpr.Var _,
                                                                         _),
                                                               TExpr.Const(TConstValue.Int 1, _),
                                                               _),
                                                     _)) ] -> ()
                        | other -> failtestf "unexpected segments: %A" other
                    | other -> failtestf "unexpected inline TAST: %A" other
                }

            // Without NodeKey freshening the inner and outer expansions share the
            // parameter's NodeKey — and thus its local slot — and the program
            // computes 41 instead of 42.
            yield
                test "nested inline expansion prints 42 (proves NodeKey freshening)" {
                    runs "42" "let inline succ x = x + 1\nprintfn \"%d\" (succ (succ 40))"
                }

            // ---- emission strategy: static method vs closure (former Rung2) ---
            yield
                test "a top-level function is emitted as a static method with one real Param row (G8)" {
                    let _, artifact =
                        compileSource "FnStatic" "let twice x = x + x\nprintfn \"%d\" (twice 21)"

                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "twice 21 = 42 via a direct static call"

                    // G8: a nil `ParamList` would throw `BadImageFormatException` on
                    // `GetParameters`, so the reflection round-trip guards it.
                    match staticFnMethods bytes with
                    | [| m |] ->
                        Expect.isTrue m.IsStatic "emitted as a static method"
                        Expect.equal (m.GetParameters().Length) 1 "one real Param row (G8)"
                    | other -> failtestf "expected one static fn, got %A" (other |> Array.map (fun m -> m.Name))
                }

            yield
                test "a recursive function recurses via a direct static call (one static method, no closure)" {
                    let src =
                        "let rec sumTo n =\n    match n with\n    | 0 -> 0\n    | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                    let _, artifact = compileSource "FnRec" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "15" "sumTo 5 = 5+4+3+2+1+0 via a self-recursive static call"
                    Expect.equal (staticFnMethods bytes).Length 1 "sumTo is the one static method (no closure)"
                }

            yield
                test "one static method calls another by a direct call (both are static methods)" {
                    let src =
                        "let inc x = x + 1\nlet add3 x = inc (inc (inc x))\nprintfn \"%d\" (add3 10)"

                    let _, artifact = compileSource "FnCross" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "13" "add3 10 = inc(inc(inc 10)) = 13"
                    Expect.equal (staticFnMethods bytes).Length 2 "both inc and add3 are static methods"
                }

            yield
                test "a capturing function stays a closure, not a static method (prints 15)" {
                    let src = "let n = 10\nlet addN x = x + n\nprintfn \"%d\" (addN 5)"
                    let _, artifact = compileSource "FnCapture" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "15" "addN 5 = 15 via a capturing closure"
                    Expect.isEmpty (staticFnMethods bytes) "addN captures n, so it stays a closure (no static fn)"

                    let asm = loadAssembly bytes

                    let hasClosure =
                        asm.GetTypes() |> Array.exists (fun t -> t.Name.StartsWith "<closure>")

                    Expect.isTrue hasClosure "a closure type was emitted for the capturing addN"
                }

            // A function inside a `module M = …` compiles to a static method on an
            // `M` holder type, carrying its *source* name (no `fn$`).
            yield
                test "a function inside a nested module runs as a static method on its holder (prints 42)" {
                    let src =
                        "let start = 0\nmodule M =\n    let twice x = x + x\nprintfn \"%d\" (twice 21)"

                    let _, artifact = compileSource "FnNestedMod" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "twice 21 = 42 — the nested-module function ran"

                    let twice = moduleStaticMethod bytes "M" "twice"
                    Expect.isTrue twice.IsStatic "twice is a static method on the M holder"
                    Expect.isEmpty (staticFnMethods bytes) "carries its source name `twice`, not an anonymous `fn$`"
                }

            yield
                test "a recursive function inside a nested module recurses (prints 15)" {
                    let src =
                        "module M =\n    let rec sumTo n =\n        match n with\n        | 0 -> 0\n        | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                    let _, artifact = compileSource "FnNestedRec" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"

                    Expect.equal
                        (output.Trim())
                        "15"
                        "sumTo 5 = 15 via a self-recursive static call from a nested module"

                    let sumTo = moduleStaticMethod bytes "M" "sumTo"
                    Expect.isTrue sumTo.IsStatic "sumTo is a static method on the M holder"
                    Expect.isEmpty (staticFnMethods bytes) "carries its source name `sumTo`, not an anonymous `fn$`"
                }
        ]
