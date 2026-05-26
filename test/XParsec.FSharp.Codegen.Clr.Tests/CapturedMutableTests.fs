module XParsec.FSharp.Codegen.Clr.Tests.CapturedMutableTests

open System.Runtime.Loader
open Expecto
open XParsec.FSharp.Codegen.Clr
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Clr.Tests.TestHelpers

// Captured-mutable promotion (records-plan §B7): `let mutable` captured by an
// escaping closure is rewritten to a `Vesper.Ref<'T>` cell so the closure and
// outer frame share the same heap-allocated reference. The cell type lives in
// `Vesper.Core.dll`; the codegen resolves it through the cross-package record
// path; the consumer PE declares no copy. The promotion fires only for
// bindings whose Regions verdict is `HeapShared` — uncaptured `let mutable`
// (CallerStack) stays a stack-local.
//
// These tests cover the promotion's TAST-level guarantees: the consumer's
// `Decls` carries no `Ref` type, the binding-site rewrite, and the `n <- v`
// ↦ FieldSet + `n` ↦ FieldGet lowering. Generic-closure synthesis
// (closure-plan.md) is the remaining gap for runtime cases that surface an
// un-pinned `TyVar` in a closure's capture-field signature.

[<Tests>]
let tests =
    testList
        "CapturedMutable"
        [
            test "TAST: a captured `let mutable` does NOT prepend a Ref decl (cell lives in Vesper.Core.dll)" {
                // Pre-F2 the pass prepended a synthetic `type Ref<'T> = { mutable
                // contents: 'T }` so the codegen's user-record path could find it.
                // F2 retired that stopgap: the cell type ships in
                // `Vesper.Core.dll` and the codegen resolves it through
                // `externalRecordRef`. The consumer PE's `Decls` carries no
                // `Ref` declaration regardless of whether the promotion fires.
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkCounter z ="
                            "    let mutable n = z"
                            "    fun y ->"
                            "        n <- n + 1"
                            "        n"
                        ]

                let tast = analyse src

                let hasLocalRefDecl =
                    tast.Decls
                    |> List.exists (fun d ->
                        match d with
                        | TDecl.Type td when td.Name = "Ref" || td.Name = "Vesper.Ref" -> true
                        | _ -> false
                    )

                Expect.isFalse hasLocalRefDecl "no Ref<'T> decl synthesised locally — the cell lives in Vesper.Core.dll"
            }

            test "TAST: a non-captured `let mutable` is not promoted (no rewrite either)" {
                // The cell never crosses a closure boundary, so Regions reports
                // CallerStack and the promotion pass skips it. No rewrite, and
                // (as before F2) no Ref decl synthesised.
                let src =
                    String.concat "\n" [ "let useLocal z ="; "    let mutable n = z"; "    n <- 7"; "    n" ]

                let tast = analyse src

                let hasRefDecl =
                    tast.Decls
                    |> List.exists (fun d ->
                        match d with
                        | TDecl.Type td when td.Name = "Ref" || td.Name = "Vesper.Ref" -> true
                        | _ -> false
                    )

                Expect.isFalse hasRefDecl "no Ref<'T> decl synthesised — the cell stayed local"
            }

            test "TAST: a captured cell's use/write/init sites all lower through Vesper.Ref<_>" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let mkCounter z ="
                            "    let mutable n = z"
                            "    fun y ->"
                            "        n <- n + 1"
                            "        n"
                        ]

                let tast = analyse src

                let rec scanExpr (predicate: TExpr -> bool) (e: TExpr) : bool =
                    if predicate e then
                        true
                    else
                        match e with
                        | TExpr.Lambda(_, b, _) -> scanExpr predicate b
                        | TExpr.Let(_, v, b, _) -> scanExpr predicate v || scanExpr predicate b
                        | TExpr.App(f, a, _) -> scanExpr predicate f || scanExpr predicate a
                        | TExpr.Sequential(items, _) -> items |> List.exists (scanExpr predicate)
                        | TExpr.IfThenElse(c, t, e, _) ->
                            scanExpr predicate c || scanExpr predicate t || scanExpr predicate e
                        | TExpr.FieldGet(r, _, _) -> scanExpr predicate r
                        | TExpr.FieldSet(r, _, v, _) -> scanExpr predicate r || scanExpr predicate v
                        | TExpr.RecordCons(fields, _) -> fields |> List.exists (fun (_, v) -> scanExpr predicate v)
                        | TExpr.Match(sc, arms, _) ->
                            scanExpr predicate sc
                            || arms |> List.exists (fun a -> scanExpr predicate a.Body)
                        | _ -> false

                let isContentsFieldSet =
                    function
                    | TExpr.FieldSet(_, "contents", _, _) -> true
                    | _ -> false

                let isContentsFieldGet =
                    function
                    | TExpr.FieldGet(_, "contents", _) -> true
                    | _ -> false

                let isRecordConsOfRef =
                    function
                    | TExpr.RecordCons(_, TyRecord("Vesper.Ref", _)) -> true
                    | _ -> false

                let scan p =
                    tast.Decls
                    |> List.exists (fun d ->
                        match d with
                        | TDecl.Let(_, v, _, _) -> scanExpr p v
                        | TDecl.Expression(e, _) -> scanExpr p e
                        | _ -> false
                    )

                Expect.isTrue (scan isContentsFieldSet) "`n <- n + 1` lowered to a FieldSet on `contents`"
                Expect.isTrue (scan isContentsFieldGet) "every bare `n` read lowered to a FieldGet on `contents`"

                Expect.isTrue (scan isRecordConsOfRef) "`let mutable n = z` lowered to a Vesper.Ref<_> RecordCons"
            }

            // Records-handoff Phase 2 follow-up F2 acceptance: when the
            // captured-mutable pass fires, the consumer PE *references*
            // `Vesper.Ref\`1` from `Vesper.Core.dll` (mints a `TypeRef`) and
            // does *not* declare its own copy. The runtime end-to-end tests
            // (`mkCounter ()` counter, two-closures-share-the-cell) are pinned
            // by the pre-existing closure-emit gaps documented at the top of
            // this file (F3); the IL-level reflection check below is the F2
            // acceptance the handoff calls for.
            test "Codegen: consumer PE referencing Vesper.Ref<_> does NOT declare its own copy" {
                // The simplest cell-promoting program whose codegen path doesn't
                // hit the deferred closure-emit gaps: a single `let mutable n`
                // captured by a one-shot inner function that's then *applied
                // immediately and discarded*. Both reads/writes lower to
                // `Vesper.Ref<int>::contents`; the closure's invocation completes
                // before the outer function returns, so escape analysis flags it
                // — but the closure value itself is consumed synchronously.
                let src =
                    String.concat
                        "\n"
                        [
                            "let useCounter (z: int) : int ="
                            "    let mutable n = z"
                            "    let bump (v: int) ="
                            "        n <- n + v"
                            "    bump 7"
                            "    n"
                        ]

                // Force the shared Vesper.Core.dll into the Default ALC up front
                // so the consumer's `Vesper.Ref\`1` AssemblyRef resolves at load.
                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "VesperRefConsumer" src
                let bytes = Codegen.toBytes artifact
                let asm = loadAssembly bytes

                // Acceptance: no local `Vesper.Ref\`1` TypeDef.
                let localRef = asm.GetType "Vesper.Ref`1"

                Expect.isNull
                    localRef
                    "the consumer PE must NOT declare a local Vesper.Ref`1 — the cell lives in Vesper.Core.dll"

                // The reference table must include `Vesper.Core` (the
                // `externalRecordRef` path mints a TypeRef whose AssemblyRef
                // resolves to it).
                let refs = asm.GetReferencedAssemblies() |> Array.map (fun a -> a.Name)

                Expect.contains
                    refs
                    "Vesper.Core"
                    (sprintf "consumer PE must reference Vesper.Core for Vesper.Ref`1 (refs: %A)" refs)
            }

            // Records-handoff Phase 2 §1 (runtime): the captured-mutable cell
            // survives across multiple invocations of the escaping closure. The
            // `unit -> int` Invoke + `mkCounter ()` static-method call both
            // require F3's unit-parameter closure peel (peelLambda /
            // discoverClosures); the cell itself is the F2 `Vesper.Ref\`1` from
            // `Vesper.Core.dll`.
            test "mkCounter () counter: three invocations see the shared Vesper.Ref<int> cell" {
                let src =
                    String.concat
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
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkCounter" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)

                Expect.equal
                    (output.Trim().Replace("\r\n", "\n"))
                    "1\n2\n3"
                    "the three invocations read 1, 2, 3 from the shared Ref<int> cell"
            }

            // Records-handoff Phase 2 §1 test #3 (runtime): two closures captured
            // by the same `let mutable` cell observe each other's writes — the
            // F2 `Vesper.Ref\`1` is shared. Both closures take a unit parameter,
            // so F3.1 is required for both to compile. Wrapped in an outer
            // function (`useTwoClosures`) so RefCellPromotion fires for the
            // local `let mutable n` (module-level mutables don't promote — they
            // live in a static field, a different mechanism beyond F3).
            test "two closures share a single Vesper.Ref<int> cell" {
                let src =
                    String.concat
                        "\n"
                        [
                            "let useTwoClosures () ="
                            "    let mutable n = 0"
                            "    let inc () = n <- n + 1"
                            "    let read () = n"
                            "    inc ()"
                            "    inc ()"
                            "    inc ()"
                            "    read ()"
                            "printfn \"%d\" (useTwoClosures ())"
                        ]

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "TwoClosuresShareCell" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)
                Expect.equal (output.Trim()) "3" "three increments, observed through the read closure, sum to 3"
            }

            // Pending (closure-plan.md): a higher-order function that *returns*
            // a closure with an un-pinned typar in its capture-field signature
            // surfaces `ClrProvider: cannot encode SemType: TyVar`. The fix
            // needs generic closure synthesis — the inner closure type becomes
            // generic over its enclosing static method's typars, instantiated
            // per call site. The genuine generic-closure case here: the inner
            // closure captures a value of an *unconstrained* typar (`'a`). No
            // operators apply to `x` inside the inner closure, so F#'s
            // static-member-default mechanism never kicks in; `mkConst` stays
            // `'a -> unit -> 'a` and the static method's typar must flow
            // through into the closure's capture-field signature.
            //
            // (`let mkAdder x = fun y -> y + x` is NOT a generic-closure case
            // in F# — the `+` operator's `default ^T1: int` clause in
            // `Vesper.Core/ops-platform.fsi` forces `int` in the absence of
            // other type direction. `mkConst` cannot be defaulted; it is the
            // genuine trigger.)
            ptest "Higher-order returning a closure with an un-pinned typar (closure-plan.md)" {
                let src =
                    "let mkConst x = fun () -> x\nlet always10 = mkConst 10\nprintfn \"%d\" (always10 ())"

                vesperCoreDll.Value |> ignore

                let _, artifact = compileSource "MkConst" src
                let exitCode, output = runEntryPoint (Codegen.toBytes artifact)

                Expect.equal exitCode 0 (sprintf "Main returns 0 (output: %s)" output)

                Expect.equal
                    (output.Trim())
                    "10"
                    "the inner closure carries its capture's type through the static-method typar"
            }
        ]
