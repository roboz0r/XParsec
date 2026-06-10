module XParsec.FSharp.Codegen.Clr.Tests.ClosureTests

open Expecto
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Layer 1 behavioral corpus: closures. Slice5Tests + CapturedMutableTests hold
// the deep anchors (closure-type emission, the generic-closure `GenericParam`
// rows, the captured-`Vesper.Ref` cell). This is the broad net over the
// observable runtime behaviours: a non-capturing lambda value, a value capture,
// an inline lambda in argument position, a returned (generic) closure, and a
// captured mutable surviving across invocations.
//
// `runs` already forces `Vesper.Core.dll` into the default load context (via
// `withCore`, which the compile path injects), so the captured-`Ref` and
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
                    // a returned (generic) closure capturing its argument
                    "let mkConst x =\n    let f = fun () -> x\n    f\nlet always10 = mkConst 10\nprintfn \"%d\" (always10 ())",
                    "10"
                ] -> test src { runs expected src }

            // a captured `let mutable` cell shared across three invocations of
            // the escaping closure (promotion to Vesper.Ref)
            yield
                test "captured mutable counter: three invocations share the cell" {
                    runsLines
                        [ "1"; "2"; "3" ]
                        (String.concat
                            "\n"
                            [
                                "let mkCounter () ="
                                "    let mutable n = 0"
                                "    fun () ->"
                                "        n <- n + 1"
                                "        n"
                                "let c = mkCounter ()"
                                "printfn \"%d\" (c ())"
                                "printfn \"%d\" (c ())"
                                "printfn \"%d\" (c ())"
                            ])
                }

            // A closure *inside a class member body* (vesper-set-phase-9 wall): a
            // mono class, a member whose body builds and applies a non-capturing
            // lambda. `discoverClosures` must now walk member bodies, not just
            // top-level decls.
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

            // The same, but the inner lambda captures a ctor-param backing field
            // (`n`) — a ground (non-typar) capture, so a monomorphic closure.
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

            // A closure inside a *generic* class member body, capturing a value
            // typed by the class typar `'T` (declaring axis) and a function over
            // it. The closure re-projects `'T` (`FTTypar(Declaring,0)`) onto its
            // own class typar `!0` (vesper-set Phase 9, the `Set.Fold` shape).
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

            // A closure inside a generic class member body that captures *both* a
            // class-typar value (`'T`, declaring axis) and an implicit member-typar
            // value (`'U`, method axis) — the mixed-axis projection (`'U` lands at
            // the closure's `!(d + j)`). This is the `Set.map` shape.
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

            // The `Set.Fold` shape: a generic class member with *unannotated*
            // params whose types are inferred (via a generic module fn call) to a
            // fresh state typar. Because the member is never *called* in this
            // assembly (a library API), those typars must be generalised into the
            // member's own method typars — if generalisation fails they leak as
            // `?ungrounded-operator` into the closure's capture field. The member
            // is emitted regardless of being called, so the leak surfaces at
            // emission.
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

            // `(+)` resolves as a *value* — an `External op_Addition` the call site
            // references — before eta-reification gives it nested `Vesper.Fun`
            // closures (the runtime shape lives in `SelfHostTests`). Former Slice5 M3.
            yield
                test "`let add = (+)` analyses clean to an External op_Addition value" {
                    let tast = analyse "let add = (+)\nprintfn \"%d\" (add 40 2)"
                    Expect.isEmpty tast.Diagnostics "no diagnostics — (+) resolves as a value"

                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple(kAdd, _), TExpr.External("op_Addition", _, _), false, _)
                               TDecl.Expression(TExpr.Format(FormatSink.ToStdOut true, segs, _), _) ] ->
                        match EqArray.toList segs with
                        | [ FormatSeg.Hole(_,
                                           TExpr.App(TExpr.App(TExpr.Var(kUse, _), TExpr.Const(TConstValue.Int 40, _), _),
                                                     TExpr.Const(TConstValue.Int 2, _),
                                                     _)) ] ->
                            Expect.equal kUse kAdd "the call site references the (+) binding"
                        | other -> failtestf "unexpected segments: %A" other
                    | other -> failtestf "unexpected (+)-as-value TAST: %A" other
                }
        ]
