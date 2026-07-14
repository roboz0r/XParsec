module XParsec.FSharp.SemanticAnalysis.Tests.InlineTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Covers the `Inline` marker on `TDecl.Let` and the codegen-facing
// `Inline.inlineExpand` body-substitution helper.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value input lexed file

let private firstDecl (input: string) : TDecl =
    let tast = analyse input

    if tast.Decls.IsEmpty then
        failwithf "no decls for %s" input
    else
        tast.Decls.[0]

let private declType (tast: TastFile) : SemType =
    match tast.Decls with
    | EqList [ TDecl.Let(_, _, _, ty) ] -> ty
    | _ -> failwithf "expected single TDecl.Let, got %A" tast.Decls

/// The template as a CONSUMER receives it: published by `Freeze` into the unit's inline
/// vocabulary (typars named on the self-describing `FTTypar` axis) and THAWED back into
/// fresh `TyVar` cells.
///
/// This — not `tast.Decls` — is the shape `Inline.inlineExpand` runs on for any GENERIC
/// template. The post-`freezeTypars` SemType decl in `tast.Decls` deliberately carries
/// `TyTypar`, not roots: an inline binding is never emitted, so its typars are quantified
/// unconditionally, which is what lets freeze name them at all. (A SAME-unit splice sees
/// the roots because `Passes.InlineExpansion` runs BEFORE that cut.)
///
/// The `namespace` + nested `module` wrapper is not incidental: only a binding with a
/// declaring MODULE has a holder chain, hence an exportable identity, hence a vocabulary
/// entry. A top-level binding lives in the anonymous Program holder and is published
/// nowhere — it is spliceable only within its own unit.
let private thawedTemplate (letInline: string) : TDecl =
    let input = "namespace Ns\n\nmodule M =\n    " + letInline + "\n"
    let lexed, file = parseFile input
    let frozen = Pipeline.analyse realProvider.Value input lexed file

    match frozen.InlineBodies |> EqArray.toList with
    | [ v ] -> Inline.thawBody v.Body.Decl
    | other -> failwithf "expected exactly one published inline body for %s, got %d" letInline (List.length other)

[<Tests>]
let tests =
    testList
        "Inline"
        [
            test "`let inline` sets isInline on the TDecl.Let" {
                match firstDecl "let inline succ x = x + 1" with
                | TDecl.Let(_, _, true, _) -> ()
                | other -> failtestf "expected inline TDecl.Let, got %A" other
            }

            test "a plain `let` leaves isInline clear" {
                match firstDecl "let succ x = x + 1" with
                | TDecl.Let(_, _, false, _) -> ()
                | other -> failtestf "expected non-inline TDecl.Let, got %A" other
            }

            test "prettyDecl renders the inline keyword" {
                let tast = analyse "let inline succ x = x + 1"

                Expect.equal
                    (TastShape.prettyDecl tast.Decls.[0])
                    "let inline v0 = fun v1 -> (v1 + 1)"
                    "TAST shape includes inline"
            }

            test "inline binding still types and freezes its body verbatim" {
                let tast = analyse "let inline succ x = x + 1"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                Expect.equal (declType tast) (TyFun(BuiltinTypes.tyInt, BuiltinTypes.tyInt)) "succ : int -> int"
            }

            test "monomorphic inline binding has no quantified typars" {
                match firstDecl "let inline succ x = x + 1" with
                | TDecl.Let(_, _, _, declTy) -> Expect.isEmpty (Inline.quantifiedTypars declTy) "no typars"
                | other -> failtestf "unexpected %A" other
            }

            test "a thawed polymorphic template exposes one quantified typar" {
                match thawedTemplate "let inline id x = x" with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal
                        (Inline.quantifiedTypars declTy).Length
                        1
                        "id's single typar round-trips freeze → publish → thaw as one fresh root"
                | other -> failtestf "unexpected %A" other
            }

            test "expanding a monomorphic binding returns an equal body — and still WALKS it" {
                let decl = firstDecl "let inline succ x = x + 1"

                let body =
                    match decl with
                    | TDecl.Let(_, v, _, _) -> v
                    | other -> failtestf "unexpected %A" other

                let expanded, unresolved = Inline.inlineExpand decl [||]

                // Structurally the same body — there is no typar to substitute. It is
                // NOT the same object, and must not be: the substituting walk is also
                // what resolves `StaticOptimization` and `TraitCall` nodes, and neither
                // backend can emit those. Short-circuiting an empty substitution to
                // return the body by reference (the shape this once asserted) let both
                // node kinds ride an unwalked body straight through to codegen.
                Expect.equal expanded body "body structurally unchanged"
                Expect.isEmpty unresolved "a monomorphic `+` on int resolves"
            }

            test "expanding a thawed polymorphic template substitutes the typar through the body" {
                let decl = thawedTemplate "let inline id x = x"
                let expanded, _ = Inline.inlineExpand decl [| BuiltinTypes.tyInt |]

                // `id`'s body is `fun x -> x`; instantiating 'a := int makes
                // every position concrete int.
                match expanded with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst(k1, _), _),
                               TExpr.Var(_, TyConst(k2, _), _),
                               TyFun(TyConst(k3, _), TyConst(k4, _)),
                               _) when
                    [ k1; k2; k3; k4 ]
                    |> List.forall (fun k -> SymbolKeyOps.simpleName k = DisplayName "int")
                    ->
                    ()
                | other -> failtestf "expected fully-int `fun x -> x`, got %A" other
            }

            test "inlineExpand does not mutate the original decl" {
                let decl = thawedTemplate "let inline id x = x"
                // Expand once at int…
                Inline.inlineExpand decl [| BuiltinTypes.tyInt |] |> ignore

                // …the decl's own type must still carry a free typar so a
                // second call-site can instantiate it independently.
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal (Inline.quantifiedTypars declTy).Length 1 "typar still free after expansion"

                    let again, _ = Inline.inlineExpand decl [| BuiltinTypes.tyBool |]

                    match again with
                    | TExpr.Lambda(TPat.NamedSimple(_, TyConst(k, _), _), _, _, _) when
                        SymbolKeyOps.simpleName k = DisplayName "bool"
                        ->
                        ()
                    | other -> failtestf "second expansion at bool failed: %A" other
                | other -> failtestf "unexpected %A" other
            }

            test "inlineExpand on a TDecl.Expression raises" {
                // A top-level expression has no binding to expand.
                let decl =
                    TDecl.Expression(TExpr.Const(TConstValue.Unit, BuiltinTypes.tyUnit, dummyTok), BuiltinTypes.tyUnit)

                Expect.throws (fun () -> Inline.inlineExpand decl [||] |> ignore) "expects a TDecl.Let"
            }

            test "`let inline succ x = x + 1 in succ 41` keeps the inline template and expands its use site" {
                // At module level the parser lifts `let inline succ … in body`
                // into a top-level inline binding followed by the body as its
                // own expression — so the marker lands on a TDecl.Let. The
                // template (decl 0) is retained verbatim, but the use site `succ
                // 41` is expanded *pre-freeze* by `InlineExpansion`: the call
                // beta-reduces to a `Let` binding the argument, with `succ`'s
                // `x + 1` body inlined. The `op_Addition` head survives here
                // because `realProvider` is a CONTRACT-only stack (`.fsi`
                // signatures, no `.fs` inline bodies), so there is no `(+)` body
                // to splice; a codegen provider serves one and it splices.
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                match tast.Decls with
                | EqList [ TDecl.Let(TPat.NamedSimple _, TExpr.Lambda _, true, TyFun(TyConst(k1, _), TyConst(k2, _)))
                           TDecl.Expression(TExpr.Let(TPat.NamedSimple _,
                                                      TExpr.Const(TConstValue.Integral(IntWidth.Int32, 41L), _, _),
                                                      TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _),
                                                                          TExpr.Var _,
                                                                          _,
                                                                          _),
                                                                TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L),
                                                                            _,
                                                                            _),
                                                                _,
                                                                _),
                                                      _,
                                                      _),
                                            _) ] when
                    SymbolKeyOps.simpleName k1 = DisplayName "int"
                    && SymbolKeyOps.simpleName k2 = DisplayName "int"
                    ->
                    ()
                | _ -> failtestf "unexpected shape: %A" tast.Decls
            }

            test "expanding the §C inline succ at its use site yields its int-typed body" {
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                let succDecl = tast.Decls.[0]

                // succ is monomorphic (int -> int) — expansion is a no-op
                // substitution returning the retained `fun x -> x + 1` body.
                match fst (Inline.inlineExpand succDecl [||]) with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst(k1, _), _),
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _),
                                                   TExpr.Var(_, TyConst(k2, _), _),
                                                   _,
                                                   _),
                                         TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _),
                                         _,
                                         _),
                               TyFun(TyConst(k3, _), TyConst(k4, _)),
                               _) when
                    [ k1; k2; k3; k4 ]
                    |> List.forall (fun k -> SymbolKeyOps.simpleName k = DisplayName "int")
                    ->
                    ()
                | other -> failtestf "expected `fun x -> x + 1` body, got %A" other
            }

            // Mirrors the minter `InlineExpansion` owns: a monotone counter, shared across
            // calls, so two expansions never mint the same binder key.
            let sharedMinter () =
                let mutable n = 0

                fun () ->
                    let k = NodeKey.ofSyntheticCounter n NodeKind.SynthPreFreezeInline
                    n <- n + 1
                    k

            // Pull (binder key, body-Var key) out of the frozen `succ` body
            // shape `fun x -> x + 1`.
            let succBinderAndVar (e: TExpr) =
                match e with
                | TExpr.Lambda(TPat.NamedSimple(kb, _, _),
                               TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _), TExpr.Var(kv, _, _), _, _),
                                         TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _),
                                         _,
                                         _),
                               _,
                               _) -> kb, kv
                | other -> failtestf "unexpected succ body: %A" other

            test "freshen renames binders and rewires their references" {
                let body =
                    match firstDecl "let inline succ x = x + 1" with
                    | TDecl.Let(_, v, _, _) -> v
                    | other -> failtestf "unexpected %A" other

                let kb0, kv0 = succBinderAndVar body
                Expect.equal kv0 kb0 "original body Var references the original binder"

                let mint = sharedMinter ()
                let f1 = Inline.freshen mint body
                let f2 = Inline.freshen mint body

                let kb1, kv1 = succBinderAndVar f1
                let kb2, kv2 = succBinderAndVar f2

                // (a) every binder key differs from the original and between results.
                Expect.notEqual kb1 kb0 "first expansion's binder is fresh"
                Expect.notEqual kb2 kb0 "second expansion's binder is fresh"
                Expect.notEqual kb1 kb2 "the two expansions don't share a binder"
                // (b) the internal Var is rewired to the new binder.
                Expect.equal kv1 kb1 "first expansion's Var follows its fresh binder"
                Expect.equal kv2 kb2 "second expansion's Var follows its fresh binder"
            }

            test "freshen leaves a free Var untouched" {
                // `let bound = <free> in bound`: `free`'s key is never bound
                // inside the body, so it must pass through; `bound` is rebound
                // and its reference rewired.
                let tyInt = BuiltinTypes.tyInt
                let freeKey = NodeKey.ofSource 999 NodeKind.ExprIdent
                let boundKey = NodeKey.ofSource 1 NodeKind.PatIdent

                let body =
                    TExpr.Let(
                        TPat.NamedSimple(boundKey, tyInt, dummyTok),
                        TExpr.Var(freeKey, tyInt, dummyTok),
                        TExpr.Var(boundKey, tyInt, dummyTok),
                        tyInt,
                        dummyTok
                    )

                match Inline.freshen (sharedMinter ()) body with
                | TExpr.Let(TPat.NamedSimple(kb, _, _), TExpr.Var(kFree, _, _), TExpr.Var(kRef, _, _), _, _) ->
                    Expect.equal kFree freeKey "free Var passes through unchanged"
                    Expect.notEqual kb boundKey "the bound name is freshened"
                    Expect.equal kRef kb "the bound reference follows the fresh binder"
                | other -> failtestf "unexpected freshened shape: %A" other
            }

            // inline-first soundness, beta-reduction half:
            // a lambda argument bound to an inline parameter and FULLY APPLIED in
            // the body is inlined away — its closure never exists. A stored /
            // partially-applied lambda parameter survives as a real closure.

            // The parameter annotations keep these inlines monomorphic so the
            // expanded use grounds fully (a polymorphic inline use leaves the
            // template's typars free, an orthogonal front-end limitation). The
            // lambda-elimination logic under test is independent of polymorphism.

            test "a fully-applied inline lambda parameter is eliminated (no surviving closure)" {
                // `apply` saturates `f` (one arg, arity 1), so the lambda is
                // spliced in and beta-reduced — the expanded use has no `fun`.
                let tast =
                    analyse "let inline apply (f: int -> int) (x: int) = f x in apply (fun y -> y + 1) 41"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "no surviving closure: %s" body)
                Expect.stringContains body "+ 1" "the inlined lambda body survives"
            }

            test "a doubly-applied inline lambda parameter is eliminated at both sites" {
                // `twice f x = f (f x)`: both uses are saturated, so both copies
                // of the lambda are spliced (each freshened) and no closure remains.
                let tast =
                    analyse "let inline twice (f: int -> int) (x: int) = f (f x) in twice (fun y -> y + 1) 10"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "both closures eliminated: %s" body)
            }

            test "a published body's reference to a NON-inline module sibling is an External carrying its key" {
                // The published body is spliced at a CONSUMER, where none of this unit's
                // binders exist. A module-level sibling — inline template or ordinary
                // compiled value, it makes no difference — must therefore leave the unit
                // as `External` + `SymbolKey`, never as a `Var` naming a binder only this
                // unit's tree has.
                let input =
                    "namespace Ns\n\nmodule M =\n    let k = 3\n    let inline addK x = x + k\n"

                let lexed, file = parseFile input
                let sem = Pipeline.analyseSem realProvider.Value input lexed file
                let frozen = Pipeline.analyse realProvider.Value input lexed file

                // `k` is the module's first decl; its published identity is the one its
                // `ModuleMemberInfo` mints — the same one the rewrite must have baked in.
                let kKey =
                    match sem.Decls.[0] with
                    | TDecl.Let(TPat.NamedSimple(k, _, _), _, _, _) -> k
                    | other -> failtestf "expected `let k` first, got %A" other

                let expected =
                    match Map.tryFind kKey sem.ModuleMembers with
                    | Some info -> info.Key
                    | None -> failtest "`k` has no ModuleMemberInfo"

                Expect.isEmpty frozen.Diagnostics "no diagnostics"

                let body =
                    match frozen.InlineBodies |> EqArray.toList with
                    | [ v ] -> Inline.thawBody v.Body.Decl
                    | other -> failtestf "expected exactly one published body, got %d" (List.length other)

                let refs = ResizeArray<string * SymbolKey>()
                let vars = ResizeArray<NodeKey>()

                let collect =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.External(name, ValueSome key, _, _) -> refs.Add(name, key)
                                | TExpr.Var(k, _, _) -> vars.Add k
                                | _ -> ()

                                true
                    }

                match body with
                | TDecl.Let(_, v, _, _) -> TastWalk.iterExpr collect v
                | other -> failtestf "unexpected published decl %A" other

                Expect.contains refs ("k", expected) "the sibling reference is an External carrying `k`'s SymbolKey"

                // The only `Var` left is the template's own parameter, which the splice
                // rebinds.
                Expect.isFalse (vars.Contains kKey) "no residual Var naming `k`'s binder"
            }

            test "an inline template referencing a TOP-LEVEL binding is diagnosed, not published" {
                // A top-level (implicit-`Program`-module) binding records a `TopLevelNames`
                // entry but no `ModuleMemberInfo`, hence no `SymbolKey` — there is nothing
                // for the sibling rewrite to bake in, so the free `Var` survives. That is
                // exactly what the publish-time free-`Var` check exists to catch.
                let input = "let k = 3\n\nmodule M =\n    let inline addK x = x + k\n"
                let lexed, file = parseFile input
                let _, frozen = Pipeline.analyseWithContext realProvider.Value input lexed file

                Expect.isEmpty (EqArray.toList frozen.InlineBodies) "the un-splice-able template is not published"

                let errors =
                    frozen.Diagnostics
                    |> List.filter (fun d -> d.Severity = Severity.Error && d.Message.Contains "cannot be published")

                Expect.isNonEmpty errors "the free `Var` is reported"
            }

            test "a stored inline lambda parameter survives as a closure" {
                // `pick f = f` returns its parameter rather than applying it, so
                // the lambda cannot be inlined away — it stays a real closure (the
                // 3A-3 fallback; the byref-capture reject of such a survivor is the
                // deferred half).
                let tast = analyse "let inline pick (f: int -> int) = f in pick (fun y -> y + 1)"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.stringContains body "fun" (sprintf "closure survives: %s" body)
            }
        ]
