module XParsec.FSharp.Codegen.Clr.Tests.FunctionTests

open System.Reflection
open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers
open XParsec.FSharp.Codegen.Clr.Tests.PeInspection

// Function definition + application forms, one row per form; the anchors beneath pin the
// emission strategy (top-level fn → static method, capturing fn → closure).

/// Nested-module functions land on their module's class rather than on `Program`.
let private moduleStaticMethod (bytes: byte[]) (moduleClass: string) (name: string) : MethodInfo =
    let asm = loadAssembly bytes

    match asm.GetTypes() |> Array.tryFind (fun t -> t.FullName = moduleClass) with
    | None -> failtestf "no `%s` class emitted for the nested module" moduleClass
    | Some t ->
        match t.GetMethod(name, BindingFlags.Public ||| BindingFlags.Static) with
        | null -> failtestf "`%s` is not a public static method on the `%s` class" name moduleClass
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
                    // recursion
                    "let rec fact n = if n <= 1 then 1 else n * fact (n - 1)\nprintfn \"%d\" (fact 5)", "120"
                    // one static method calling another
                    "let inc x = x + 1\nlet add3 x = inc (inc (inc x))\nprintfn \"%d\" (add3 10)", "13"
                    // inline
                    "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)", "42"
                    // higher-order: a function parameter applied to an argument
                    "let apply f x = f x\nprintfn \"%d\" (apply (fun n -> n + 1) 41)", "42"
                ] -> test src { runs expected src }

            // ---- inline expansion --------------------
            yield
                test "an inline binding keeps its template and outlines its use site pre-freeze" {
                    // The `let inline succ` template (decl 0) survives as an ordinary function;
                    // the use site `succ 41` resolves pre-freeze into an EDGE carrying the one
                    // argument. Its body is `_`: `(+)` inline-expands to `ILIntrinsic "add"`.
                    let tast = analyse "let inline succ x = x + 1\nprintfn \"%d\" (succ 41)"
                    Expect.isEmpty tast.Diagnostics "no diagnostics"

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple _, TExpr.Lambda _, true, TyFun(TyConst(k1, _), TyConst(k2, _)))
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ] when
                        SymbolKeyOps.typeSimpleName k1 = DisplayName "int"
                        && SymbolKeyOps.typeSimpleName k2 = DisplayName "int"
                        ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(_,
                                           TExpr.InlineCall(
                                               args = EqList [ TExpr.Const(TConstValue.Integral(IntKind.Int32, 41L),
                                                                           _,
                                                                           _) ])) ] -> ()
                        | other -> failtestf "unexpected segments: %A" other
                    | other -> failtestf "unexpected inline TAST: %A" other
                }

            // Without NodeKey freshening the inner and outer expansions share the
            // parameter's local slot and the program computes 41 instead of 42.
            yield
                test "nested inline expansion prints 42 (proves NodeKey freshening)" {
                    runs "42" "let inline succ x = x + 1\nprintfn \"%d\" (succ (succ 40))"
                }

            // ---- emission strategy: static method vs closure ---
            yield
                test "a top-level function is emitted as a static method with one real Param row" {
                    let artifact =
                        compileSource "FnStatic" "let twice x = x + x\nprintfn \"%d\" (twice 21)"

                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "twice 21 = 42 via a direct static call"

                    // A nil `ParamList` throws `BadImageFormatException` on `GetParameters`,
                    // so the reflection round-trip guards it.
                    match programClassMethods bytes with
                    | [| m |] ->
                        Expect.isTrue m.IsStatic "emitted as a static method"
                        Expect.equal (m.GetParameters().Length) 1 "one real Param row"
                    | other -> failtestf "expected one static fn, got %A" (other |> Array.map (fun m -> m.Name))
                }

            yield
                test "a recursive function recurses via a direct static call (one static method, no closure)" {
                    let src =
                        "let rec sumTo n =\n    match n with\n    | 0 -> 0\n    | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                    let artifact = compileSource "FnRec" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "15" "sumTo 5 = 5+4+3+2+1+0 via a self-recursive static call"
                    Expect.equal (programClassMethods bytes).Length 1 "sumTo is the one static method (no closure)"
                }

            yield
                test "one static method calls another by a direct call (both are static methods)" {
                    let src =
                        "let inc x = x + 1\nlet add3 x = inc (inc (inc x))\nprintfn \"%d\" (add3 10)"

                    let artifact = compileSource "FnCross" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "13" "add3 10 = inc(inc(inc 10)) = 13"
                    Expect.equal (programClassMethods bytes).Length 2 "both inc and add3 are static methods"
                }

            // A top-level *value* is a static field, so `let n = 10; let addN x = x + n`
            // captures nothing and lowers to a static method. Capturing a real local
            // (`mk`'s parameter `n`) is what forces closure synthesis.
            yield
                test "a lambda capturing a function parameter is a closure (curried mk; prints 15)" {
                    let src = "let mk n = (fun x -> x + n)\nlet addN = mk 10\nprintfn \"%d\" (addN 5)"
                    let artifact = compileSource "FnCapture" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "15" "addN 5 = (mk 10) 5 = 15 via a capturing closure"

                    let asm = loadAssembly bytes

                    let hasClosure =
                        asm.GetTypes() |> Array.exists (fun t -> t.Name.StartsWith "<closure>")

                    Expect.isTrue hasClosure "a closure type was emitted for mk's inner lambda (it captures n)"
                }

            yield
                test "a function inside a nested module runs as a static method on its module class (prints 42)" {
                    let src =
                        "let start = 0\nmodule M =\n    let twice x = x + x\nprintfn \"%d\" (twice 21)"

                    let artifact = compileSource "FnNestedMod" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "twice 21 = 42, so the nested-module function ran"

                    let twice = moduleStaticMethod bytes "M" "twice"
                    Expect.isTrue twice.IsStatic "twice is a static method on the M module class"
                    Expect.isEmpty (programClassMethods bytes) "lands on the M module class, not the Program class"
                }

            yield
                test "a recursive function inside a nested module recurses (prints 15)" {
                    let src =
                        "module M =\n    let rec sumTo n =\n        match n with\n        | 0 -> 0\n        | _ -> n + sumTo (n - 1)\nprintfn \"%d\" (sumTo 5)"

                    let artifact = compileSource "FnNestedRec" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"

                    Expect.equal
                        (output.Trim())
                        "15"
                        "sumTo 5 = 15 via a self-recursive static call from a nested module"

                    let sumTo = moduleStaticMethod bytes "M" "sumTo"
                    Expect.isTrue sumTo.IsStatic "sumTo is a static method on the M module class"
                    Expect.isEmpty (programClassMethods bytes) "lands on the M module class, not the Program class"
                }

            // ---- escape bridge -------------------------------------------------
            // A module function used higher-order keeps its flat static method (the contract
            // an `.fsi` advertises), and the escape ADDS a wrapper closure `fun a -> addOne a`.
            yield
                test "an exported module function used higher-order keeps its flat static method (escape gap)" {
                    let src =
                        String.concat
                            "\n"
                            [
                                "module M ="
                                "    let addOne x = x + 1"
                                "let apply g x = g x"
                                "printfn \"%d\" (apply addOne 41)"
                            ]

                    let artifact = compileSource "FnEscapeExport" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "apply addOne 41 = 42 via the wrapper closure"

                    let addOne = moduleStaticMethod bytes "M" "addOne"
                    Expect.isTrue addOne.IsStatic "addOne is still a static method on the M module class"
                    Expect.equal (addOne.GetParameters().Length) 1 "one flat param (the contract the .fsi advertises)"

                    // The value-use is carried by a wrapper closure that `call`s it.
                    let asm = loadAssembly bytes

                    let hasClosure =
                        asm.GetTypes() |> Array.exists (fun t -> t.Name.StartsWith "<closure>")

                    Expect.isTrue hasClosure "a wrapper closure was emitted for the eta-expanded value-use"
                }

            // The Program-class analogue: a top-level function used as a value also keeps its
            // flat static method, because there is no exported-vs-Program-class split.
            yield
                test "a Program-class module function used higher-order keeps its flat static method" {
                    let src =
                        String.concat
                            "\n"
                            [
                                "let addOne x = x + 1"
                                "let apply g x = g x"
                                "printfn \"%d\" (apply addOne 41)"
                            ]

                    let artifact = compileSource "FnEscapeUnnamedModule" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "apply addOne 41 = 42 via the wrapper closure"

                    let addOne =
                        programClassMethods bytes
                        |> Array.tryFind (fun m -> m.GetParameters().Length = 1)

                    Expect.isSome addOne "addOne emitted as a 1-param static method, not demoted to a closure"

                    let asm = loadAssembly bytes

                    let hasClosure =
                        asm.GetTypes() |> Array.exists (fun t -> t.Name.StartsWith "<closure>")

                    Expect.isTrue hasClosure "a wrapper closure carries the eta-expanded value-use"
                }

            // ---- compiled-form: tuple flattening + void everywhere -------------
            // A tupled source group flattens to N flat CLR params, and a `unit` return is
            // genuine `void` for module functions and static members alike.

            yield
                test "a tupled-param module function flattens to N flat params (f(int, int), not f(ValueTuple))" {
                    // `let f (x, y)` and `let f x y` share the source type `int -> int -> int`
                    // after currying erasure, yet the tupled form must emit a FLAT 2-param
                    // method, because one `ValueTuple` param disagrees with an `fsc`-built DLL's ABI.
                    let src =
                        String.concat
                            "\n"
                            [
                                "module M ="
                                "    let addPair (x, y) = x + y"
                                "printfn \"%d\" (M.addPair (3, 4))"
                            ]

                    let artifact = compileSource "FnTupled" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"

                    Expect.equal
                        (output.Trim())
                        "7"
                        "addPair (3, 4) = 7, so the tupled arg flattened to two pushed values"

                    let m = moduleStaticMethod bytes "M" "addPair"
                    let ps = m.GetParameters()
                    Expect.equal ps.Length 2 "the tuple group flattened to TWO flat params, not one ValueTuple"
                    Expect.equal ps.[0].ParameterType typeof<int> "param 0 is a bare int"
                    Expect.equal ps.[1].ParameterType typeof<int> "param 1 is a bare int"
                }

            yield
                test "a tuple VALUE passed to a tupled-param function spills and pushes each element (prints 7)" {
                    // `f t` where `t : int * int`: the method expects two flat params, so the
                    // call site spills the tuple to a local and pushes each `ValueTuple` `Item`.
                    let src =
                        String.concat
                            "\n"
                            [
                                "module M ="
                                "    let addPair (x, y) = x + y"
                                "let t = (3, 4)"
                                "printfn \"%d\" (M.addPair t)"
                            ]

                    runs "7" src
                }

            yield
                test
                    "nested tuple param flattens exactly one level (f((a,b), c): first param a ValueTuple, second an int)" {
                    let src =
                        String.concat
                            "\n"
                            [
                                "module M ="
                                "    let f ((a, b), c) = a + b + c"
                                "printfn \"%d\" (M.f ((1, 2), 3))"
                            ]

                    let artifact = compileSource "FnNestedTuple" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "6" "f ((1,2), 3) = 6"

                    let m = moduleStaticMethod bytes "M" "f"
                    let ps = m.GetParameters()
                    Expect.equal ps.Length 2 "flattened ONE level: the outer pair → two params"

                    Expect.isTrue
                        ps.[0].ParameterType.IsGenericType
                        "param 0 stays a ValueTuple (the nested pair, not unpacked)"

                    Expect.stringStarts ps.[0].ParameterType.Name "ValueTuple" "param 0 is a System.ValueTuple"
                    Expect.equal ps.[1].ParameterType typeof<int> "param 1 is a bare int"
                }

            yield
                test "a unit-returning static member emits genuine CLR void (the NominalEmit static asymmetry is gone)" {
                    let src =
                        String.concat
                            "\n"
                            [
                                "type C() ="
                                "    static member Act (x: int) : unit = ()"
                                "    member _.M = 0"
                            ]

                    let artifact = compileSource "StaticVoid" src
                    let bytes = Codegen.toBytes artifact
                    let asm = loadAssembly bytes
                    let c = asm.GetType "C"
                    let act = c.GetMethod("Act", BindingFlags.Public ||| BindingFlags.Static)
                    Expect.isNotNull act "Act emitted as a static method"
                    Expect.equal act.ReturnType typeof<System.Void> "a unit-returning static member is CLR void"
                }

            yield
                test "a unit-returning module function self-called in statement position reifies unit (prints 42)" {
                    // `doNothing` is emitted `void`, so calling it in a `Sequential` middle
                    // position must reify a `unit` to keep the body stack-balanced.
                    let src =
                        String.concat
                            "\n"
                            [
                                "module M ="
                                "    let doNothing (x: int) : unit = ()"
                                "    let useIt x ="
                                "        doNothing x"
                                "        x + 1"
                                "printfn \"%d\" (M.useIt 41)"
                            ]

                    let artifact = compileSource "ModFnVoid" src
                    let bytes = Codegen.toBytes artifact
                    let exitCode, output = runEntryPoint bytes
                    Expect.equal exitCode 0 "Main returns 0"
                    Expect.equal (output.Trim()) "42" "useIt 41 = 42, so the void self-call reified unit"

                    let doNothing = moduleStaticMethod bytes "M" "doNothing"
                    Expect.equal doNothing.ReturnType typeof<System.Void> "doNothing is CLR void"
                }
        ]
