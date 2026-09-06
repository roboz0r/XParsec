module XParsec.FSharp.Codegen.Clr.Tests.ClosureTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// The compile path adds `Vesper.Core.dll` to `References`, so the captured-`Ref` and
// `Vesper.Fun` references resolve at load without an explicit force here.

[<Tests>]
let tests =
    testList
        "Closures"
        [
            for src, expected in
                [
                    // a non-capturing lambda bound to a value, then Invoked
                    "let f = fun x -> x + 1\nprintfn \"%d\" (f 41)", "42"
                    // capture a value from the enclosing scope
                    "let n = 10\nlet g = fun x -> x + n\nprintfn \"%d\" (g 41)", "51"
                    // an inline lambda applied directly in argument position
                    "printfn \"%d\" ((fun x -> x + 1) 5)", "6"
                    // a returned (generic) closure capturing its argument (`data/MkConst.fs`)
                    dataSource "MkConst"
                    + "\nlet always10 = mkConst 10\nprintfn \"%d\" (always10 ())",
                    "10"
                ] -> test src { runs expected src }

            // Closure discovery walks class member bodies, not just top-level decls.
            yield
                test "a non-capturing closure inside a mono class member body" {
                    runs
                        "42"
                        (String.concat
                            "\n"
                            [
                                "type C() ="
                                "    member this.M x ="
                                "        let f = fun y -> y + 1"
                                "        f x"
                                "let c = C()"
                                "printfn \"%d\" (c.M 41)"
                            ])
                }

            // The same, but the inner lambda captures a ctor-param backing field (`n`),
            // a ground (non-typar) capture, so a monomorphic closure.
            yield
                test "a closure inside a mono class member body capturing a ctor param" {
                    runs
                        "15"
                        (String.concat
                            "\n"
                            [
                                "type C(n: int) ="
                                "    member this.Apply x ="
                                "        let f = fun y -> y + n"
                                "        f x"
                                "let c = C(10)"
                                "printfn \"%d\" (c.Apply 5)"
                            ])
                }

            // The closure captures `v: 'T` from the class typar (declaring axis) and
            // re-projects it onto its own class typar `!0`.
            yield
                test "a closure inside a generic class member body capturing a class-typar value" {
                    runs
                        "8"
                        (String.concat
                            "\n"
                            [
                                "type Box<'T>(v: 'T) ="
                                "    member this.Mapped (f: 'T -> int) ="
                                "        let g = fun () -> f v"
                                "        g ()"
                                "let b = Box(7)"
                                "printfn \"%d\" (b.Mapped (fun x -> x + 1))"
                            ])
                }

            // Mixed-axis capture: `'T` on the declaring axis and `'U` on the method axis,
            // where `'U` lands at the closure's `!(d + j)`.
            yield
                test "a closure inside a generic class member body capturing class + member typar values" {
                    runs
                        "12"
                        (String.concat
                            "\n"
                            [
                                "type Holder<'T>(v: 'T) ="
                                "    member this.Combine (u: 'U) (f: 'T -> 'U -> int) ="
                                "        let g = fun () -> f v u"
                                "        g ()"
                                "let h = Holder(3)"
                                "printfn \"%d\" (h.Combine 4 (fun a b -> a * b))"
                            ])
                }

            // `Fold`'s unannotated params infer to fresh typars and the member is never
            // called here, so they must generalise into method typars. Otherwise they leak
            // as free TyVars into the closure's capture field, surfacing at emission.
            yield
                test "a generic class member with body-inferred (uncalled) method typars (Set.Fold shape)" {
                    runs
                        "ok"
                        (String.concat
                            "\n"
                            [
                                "type Tree<'T> = { Value: 'T }"
                                "module TreeOps ="
                                "    let fold f x (t: Tree<'T>) = f x t.Value"
                                "type Coll<'T>(t: Tree<'T>) ="
                                "    member s.Fold f z = TreeOps.fold (fun x z -> f z x) z t"
                                "let c = Coll({ Value = 3 })"
                                "printfn \"%s\" \"ok\""
                            ])
                }

            // `go`'s recursive self-reference must count as bound (it lowers to the closure's
            // `this`), not free, because a free `go` makes `outer` look non-static, drops it from the
            // static-method-eligible set, and the call from `C.M` fails "no binding for variable".
            yield
                test "a module fn with a recursive nested helper capturing outer params stays a static method" {
                    runs
                        "9"
                        (String.concat
                            "\n"
                            [
                                "module M ="
                                "    let outer a b ="
                                "        let rec go n acc = if n = 0 then acc else go (n - 1) (acc + a + b)"
                                "        go 3 0"
                                "type C() ="
                                "    member this.M () = M.outer 1 2"
                                "let c = C()"
                                "printfn \"%d\" (c.M ())"
                            ])
                }

            // `(+)` used as a VALUE is eta-reified and the operator's contract body spliced
            // into the `App` the eta minted, so the binding holds a two-lambda closure over
            // that body, not a bare `External op_Addition` reference.
            yield
                test "`let add = (+)` analyses clean to an eta-reified closure over the operator body" {
                    let tast = analyse "let add = (+)\nprintfn \"%d\" (add 40 2)"
                    Expect.isEmpty tast.Diagnostics "no diagnostics, because (+) resolves as a value"

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(kAdd, _, _, _),
                                         TExpr.Lambda(_, TExpr.Lambda(_, _, _, _), _, _),
                                         false,
                                         _,
                                         _)
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _, _), _) ] ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(_,
                                           TExpr.App(TExpr.App(TExpr.Var(kUse, _, _),
                                                               TExpr.Const(TConstValue.Integral(IntKind.Int32, 40L),
                                                                           _,
                                                                           _),
                                                               _,
                                                               _),
                                                     TExpr.Const(TConstValue.Integral(IntKind.Int32, 2L), _, _),
                                                     _,
                                                     _)) ] ->
                            Expect.equal kUse kAdd "the call site references the (+) binding"
                        | other -> failtestf "unexpected segments: %A" other
                    | other -> failtestf "unexpected (+)-as-value TAST: %A" other
                }

            // `(+)` here resolves to the class's `static member (+)`, not a built-in, so the
            // eta-reified body must `call` that member rather than collapse to inline IL.
            yield
                test "a mono own-class static-operator member passed as a value" {
                    runs
                        "6"
                        (String.concat
                            "\n"
                            [
                                "type V(n: int) ="
                                "    member x.N = n"
                                "    static member (+) (a: V, b: V) = V(a.N + b.N)"
                                "let xs = [ V 1; V 2; V 3 ]"
                                "let total = List.fold (+) (V 0) xs"
                                "printfn \"%d\" total.N"
                            ])
                }

            // The value elaborates to a plain keyed `External("op_Addition")`; eta-reification
            // makes `fun a b -> (+) a b`, and the spliced body's SRTP trait call resolves
            // against the nominal `V<int>` support type to `V<_>.op_Addition(a, b)`.
            yield
                test "a generic own-class static-operator value dispatches to its own op_Addition" {
                    // The default contract stack carries the SRTP `(+)`, so the
                    // operator unifies with `V<int>` rather than forcing `int`.
                    let src =
                        String.concat
                            "\n"
                            [
                                "type V<'T>(n: int) ="
                                "    member x.N = n"
                                "    static member (+) (a: V<'T>, b: V<'T>) = V<'T>(a.N + b.N)"
                                "let add : V<int> -> V<int> -> V<int> = (+)"
                            ]

                    let provider =
                        XParsec.FSharp.Codegen.Clr.ClrSymbolProviders.buildContract defaultPackages

                    let lexed, file = parseFile src

                    let tast =
                        Pipeline.analyseSemFor testCompiling provider (LexedFile.ofText lexed) file

                    if not (List.isEmpty tast.Diagnostics) then
                        failtestf "diagnostics: %A" (tast.Diagnostics |> List.map (fun d -> d.Message))

                    let addBinding =
                        tast.Decls
                        |> EqArray.toList
                        |> List.tryPick (fun d ->
                            match d with
                            | TDecl.Let(TPat.NamedSimple _, value, _, _, _) -> Some value
                            | _ -> None
                        )

                    // Through the edge: the eta reification mints an `InlineCall` pointing to the
                    // resolved `(+)` entry, whose abstraction IS the two-lambda closure.
                    match addBinding |> Option.map (throughEdge tast) with
                    | Some(TExpr.Lambda(_, TExpr.Lambda(_, body, _, _), _, _)) ->
                        let ownOpCallArities = ResizeArray<int>()

                        let it =
                            { TastWalk.identityIter with
                                VisitExpr =
                                    fun _ e ->
                                        match e with
                                        | TExpr.StaticMethodCall(SymbolKey.Member { Name = "op_Addition" },
                                                                 _,
                                                                 args,
                                                                 _,
                                                                 _) -> ownOpCallArities.Add args.Length
                                        | _ -> ()

                                        true
                            }

                        // Ordinary beta-reduction `Let`s sit between the lambdas and the
                        // call, so search through the edges rather than match the body.
                        iterThroughEdges it tast body

                        Expect.equal
                            (List.ofSeq ownOpCallArities)
                            [ 2 ]
                            (sprintf "the eta'd body calls V's own op_Addition on both params, got %A" body)
                    | other -> failtestf "expected a curried Lambda eta-expansion, got %A" other
                }

            // The parameter is bound before the closure is created, so the later write to
            // `m` is invisible to it.
            yield
                test "a mutable local passed to an inline function returning a closure is captured by value" {
                    runs
                        "1"
                        ("let inline delay (x: int) = fun () -> x\n"
                         + "let test () =\n    let mutable m = 1\n    let g = delay m\n    m <- 2\n    g ()\n"
                         + "printfn \"%d\" (test ())")
                }
        ]
