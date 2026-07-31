module XParsec.FSharp.SemanticAnalysis.Tests.InlineTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// Covers the `Inline` marker on `TDecl.Let` and the codegen-facing
// `Inline.inlineExpand` body-substitution helper.

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText input lexed) file

/// `Inline.inlineExpand`'s trait-call resolution point needs a `PassContext` to mint a
/// dispatched operator's total key. These direct-expansion tests are all primitive-`int`
/// (no nominal operator dispatch reaches the minter), so an empty context suffices — no
/// local type or provider member is consulted.
let private ctx0: PassContext =
    let lexed, _ = parseFile "module M"
    PassContext(realProvider.Value, Hashing.originSourceOfText "module M" lexed)

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

/// The template as a CONSUMER receives it: published by `Freeze` into the file's inline
/// vocabulary (typars named on the self-describing `FTTypar` axis) and THAWED back into
/// fresh `TyVar` cells.
///
/// This — not `tast.Decls` — is the shape `Inline.inlineExpand` runs on for any GENERIC
/// template. The post-`freezeTypars` SemType decl in `tast.Decls` carries `TyTypar`, not
/// roots — that cut is what lets freeze NAME the typars — so a decl read out of
/// `tast.Decls` cannot stand in for the published template. (A SAME-file splice does see
/// the roots, because `Passes.InlineExpansion` runs BEFORE that cut.)
///
/// The `namespace` + nested `module` wrapper is not incidental: only a binding with a
/// declaring MODULE has a holder chain, hence an exportable identity, hence a vocabulary
/// entry. A top-level binding lives in the anonymous Program holder and is published
/// nowhere — it is spliceable only within its own file.
// Returns the thaw `TypeStore` alongside the decl: the thawed typars are fresh roots in
// THAT store, so a test reading them back (`Inline.quantifiedTypars`) must use the same one.
let private thawedTemplate (letInline: string) : TypeStore * TDecl =
    let input = "namespace Ns\n\nmodule M =\n    " + letInline + "\n"
    let lexed, file = parseFile input
    let source = Hashing.originSourceOfText input lexed
    // The vocabulary is a pool root array; `declTree` unpools a template to the DU form the
    // cross-file wire (and `InlineThaw`) speaks — the very path a provider serves it through.
    let pools = Pipeline.analyse realProvider.Value source file

    let pool = TastPoolBuilder.openOver pools

    match List.ofArray pools.InlineTemplates with
    // Thaw into `ctx0.Store` — the SAME arena `Inline.inlineExpand ctx0` and
    // `Inline.quantifiedTypars` read the thawed roots' dense ids against.
    | [ v ] -> ctx0.Store, thawPublished ctx0.Store source (TastPoolBuilder.declTree pool v.Decl)
    | other -> failwithf "expected exactly one published inline body for %s, got %d" letInline (List.length other)

// ── the anchor domain a wire body carries ──────────────────────────────────────────────
//
// An unpooled body keeps the PRODUCER's token indices. Which file they index is not in them, so a
// consumer names the producer file and reads them there (`InlineThaw.bodyAtOrigin`) — the one
// reading there is. It is sound only while that file still holds the text the indices were taken
// against: every index stays in range across an edit, so nothing downstream could notice the
// difference.

let private producerSrc =
    "namespace Ns\n\nmodule M =\n    let inline sq x = x * x\n"

/// The same producer, edited. Same binding, same shape, DIFFERENT tokens — so a body anchored
/// against the original still dereferences cleanly here and lands on the wrong ones.
let private editedProducerSrc =
    "namespace Ns\n\nmodule M =\n    let inline twice x = x + x\n    let inline sq x = x * x\n"

/// A producer file retained the way a real collection retains one: hashed off the very text
/// that was parsed, which is what makes its hash the one an entry's `OriginFile` is checked
/// against.
let private retainedSource (input: string) : OriginSource =
    let lexed, _ = parseFile input

    Hashing.originSource
        {
            BucketName = "Producer"
            Relative = "sq.fs"
        }
        input
        lexed

/// Every position a decl carries, in `TastConvert`'s own traversal order — the total walk of
/// the position axis, so a body and its thaw are directly comparable node for node.
let private positions (d: TDeclG<'ty, 'tok, 'id>) : 'tok list =
    let acc = ResizeArray<'tok>()

    TastConvert.decl
        id
        (fun t ->
            acc.Add t
            t
        )
        d
    |> ignore

    List.ofSeq acc

let private tokenIndices (toks: SyntaxToken list) : int list =
    toks
    |> List.map (fun t ->
        match t.Index with
        | TokenIndex.Regular i -> int i
        | TokenIndex.Virtual -> -1
    )

/// The abstraction of the entry `spec` names. EVERY inline call is outlined — a template of the
/// file being compiled included — so what a use site expands TO is read off the table rather
/// than out of the declaration the call sits in.
let private entryValue (tast: TastFile) (spec: SpecializationId) : TExpr =
    let (SpecializationId i) = spec
    snd (TSpecializationG.binding spec tast.Specializations.[i])

/// What a `do` declaration's inline call expanded to, with the entry's own abstraction peeled
/// off. Peeled because the edge's arguments are positional against those leading lambdas: they
/// are the reduction's surviving PARAMETERS, not a closure anything allocates.
let private expandedCore (tast: TastFile) (d: TDecl) : string =
    let rec peel (n: int) (e: TExpr) : TExpr =
        match n, e with
        | 0, _ -> e
        | n, TExpr.Lambda(_, body, _, _) -> peel (n - 1) body
        | _, other -> failwithf "the entry abstracts fewer parameters than its edge carries: %A" other

    match d with
    | TDecl.Expression(TExpr.InlineCall(spec = spec; args = args), _) ->
        TastShape.prettyExpr (peel args.Length (entryValue tast spec))
    | other -> failwithf "expected a `do` of one inline call; got %A" other

/// The producer's sole published template, unpooled to the wire form a provider serves.
let private publishedTemplate () : Wire.TDecl =
    let lexed, file = parseFile producerSrc

    let pools =
        Pipeline.analyse realProvider.Value (Hashing.originSourceOfText producerSrc lexed) file

    let pool = TastPoolBuilder.openOver pools

    match List.ofArray pools.InlineTemplates with
    | [ v ] -> TastPoolBuilder.declTree pool v.Decl
    | other -> failwithf "expected exactly one published template, got %d" (List.length other)

[<Tests>]
let tests =
    testList
        "Inline"
        [
            test "a wire body thawed at its origin keeps every token it was written at" {
                let body = publishedTemplate ()
                let source = retainedSource producerSrc
                let sources = OriginSources.ofSeq [ source ]

                let atOrigin =
                    InlineThaw.bodyAtOrigin (TypeStore()) sources source.File body
                    |> positions
                    |> tokenIndices

                let written = positions body |> List.map Anchor.toStored

                Expect.isNonEmpty written "the fixture body actually carries positions"

                Expect.isGreaterThan
                    (written |> List.filter (fun i -> i >= 0) |> List.distinct |> List.length)
                    1
                    "…and more than one of them, or a collapse onto a single token would be indistinguishable from preserving them"

                Expect.equal atOrigin written "every node resolves to the exact token index it carries"
            }

            test "a producer edited since the body was anchored FAULTS rather than re-attributing it" {
                let body = publishedTemplate ()
                // What the entry recorded, against what the same path now holds. Both parse, both
                // hold the binding, and every recorded index is still in range against the edited
                // file — so the hash is the only thing that can tell them apart.
                let anchoredAgainst = (retainedSource producerSrc).File
                let onDisk = retainedSource editedProducerSrc

                Expect.equal
                    anchoredAgainst.Path
                    onDisk.File.Path
                    "the fixture is the SAME file — a differing path would fault for the wrong reason"

                Expect.notEqual
                    anchoredAgainst.Content
                    onDisk.File.Content
                    "…at different contents, which is the whole of the difference"

                // The MESSAGE is asserted, not merely that something threw: every other way this
                // could throw (a missing file, an out-of-range index) is a different bug, and a
                // bare `throws` would call the guard proven by any of them.
                Expect.throwsC
                    (fun () ->
                        InlineThaw.bodyAtOrigin (TypeStore()) (OriginSources.ofSeq [ onDisk ]) anchoredAgainst body
                        |> ignore
                    )
                    (fun e ->
                        Expect.stringContains
                            e.Message
                            "has changed since the tree anchored in it was built"
                            "reading anchors against a changed producer is a hard failure, and says so"
                    )
            }

            test "a producer that was never retained FAULTS rather than yielding positionless nodes" {
                let body = publishedTemplate ()
                let anchoredAgainst = (retainedSource producerSrc).File

                Expect.throwsC
                    (fun () ->
                        InlineThaw.bodyAtOrigin (TypeStore()) OriginSources.empty anchoredAgainst body
                        |> ignore
                    )
                    (fun e ->
                        Expect.stringContains
                            e.Message
                            "no retained source for"
                            "a body whose origin file is not in hand has no readable positions at all"
                    )
            }

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
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.isEmpty (Inline.quantifiedTypars (TypeStore()) declTy) "no typars"
                | other -> failtestf "unexpected %A" other
            }

            test "a thawed polymorphic template exposes one quantified typar" {
                let store, decl = thawedTemplate "let inline id x = x"

                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal
                        (Inline.quantifiedTypars store declTy).Length
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

                let expanded, unresolved = Inline.inlineExpand ctx0 decl [||]

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
                let _, decl = thawedTemplate "let inline id x = x"
                let expanded, _ = Inline.inlineExpand ctx0 decl [| BuiltinTypes.tyInt |]

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
                let store, decl = thawedTemplate "let inline id x = x"
                // Expand once at int…
                Inline.inlineExpand ctx0 decl [| BuiltinTypes.tyInt |] |> ignore

                // …the decl's own type must still carry a free typar so a
                // second call-site can instantiate it independently.
                match decl with
                | TDecl.Let(_, _, _, declTy) ->
                    Expect.equal (Inline.quantifiedTypars store declTy).Length 1 "typar still free after expansion"

                    let again, _ = Inline.inlineExpand ctx0 decl [| BuiltinTypes.tyBool |]

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

                Expect.throws (fun () -> Inline.inlineExpand ctx0 decl [||] |> ignore) "expects a TDecl.Let"
            }

            test "`let inline succ x = x + 1 in succ 41` keeps the inline template and outlines its use site" {
                // At module level the parser lifts `let inline succ … in body`
                // into a top-level inline binding followed by the body as its
                // own expression — so the marker lands on a TDecl.Let. The
                // template (decl 0) is retained verbatim, and the use site `succ
                // 41` is resolved *pre-freeze* by `InlineExpansion` into an EDGE
                // naming the entry `succ`'s resolved body went into. The
                // `op_Addition` head survives inside that entry because
                // `realProvider` is a CONTRACT-only stack (`.fsi` signatures, no
                // `.fs` inline bodies), so there is no `(+)` body to resolve; a
                // codegen provider serves one and it is outlined in turn.
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let edge =
                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple _, TExpr.Lambda _, true, TyFun(TyConst(k1, _), TyConst(k2, _)))
                               TDecl.Expression(e, _) ] when
                        SymbolKeyOps.simpleName k1 = DisplayName "int"
                        && SymbolKeyOps.simpleName k2 = DisplayName "int"
                        ->
                        e
                    | _ -> failtestf "unexpected shape: %A" tast.Decls

                match edge with
                | TExpr.InlineCall(
                    spec = spec; args = EqList [ TExpr.Const(TConstValue.Integral(IntWidth.Int32, 41L), _, _) ]) ->
                    // The argument rides the EDGE and is bound by the emit-time expansion, so
                    // the entry is `succ`'s body under the parameter it abstracts.
                    match entryValue tast spec with
                    | TExpr.Lambda(TPat.NamedSimple _,
                                   TExpr.App(TExpr.App(TExpr.External("op_Addition", _, _, _), TExpr.Var _, _, _),
                                             TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _),
                                             _,
                                             _),
                                   _,
                                   _) -> ()
                    | other -> failtestf "the entry is not `fun x -> x + 1`: %A" other
                | other -> failtestf "the use site is not an edge carrying its one argument: %A" other
            }

            test "expanding the §C inline succ at its use site yields its int-typed body" {
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                let succDecl = tast.Decls.[0]

                // succ is monomorphic (int -> int) — expansion is a no-op
                // substitution returning the retained `fun x -> x + 1` body.
                match fst (Inline.inlineExpand ctx0 succDecl [||]) with
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
                // substituted at its use and beta-reduced — the reduction has no `fun` beyond
                // the parameter its entry abstracts.
                let tast =
                    analyse "let inline apply (f: int -> int) (x: int) = f x in apply (fun y -> y + 1) 41"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = expandedCore tast tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "no surviving closure: %s" body)
                Expect.stringContains body "+ 1" "the inlined lambda body survives"
            }

            test "a doubly-applied inline lambda parameter is eliminated at both sites" {
                // `twice f x = f (f x)`: both uses are saturated, so both copies
                // of the lambda are substituted (each freshened) and no closure remains.
                let tast =
                    analyse "let inline twice (f: int -> int) (x: int) = f (f x) in twice (fun y -> y + 1) 10"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = expandedCore tast tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "both closures eliminated: %s" body)
                Expect.stringContains body "+ 1" "…and both copies of the lambda's body are there to show for it"
            }

            test "a published body's reference to a NON-inline module sibling is an External carrying its key" {
                // The published body is expanded at a CONSUMER, where none of this file's
                // binders exist. A module-level sibling — inline template or ordinary
                // compiled value, it makes no difference — must therefore leave the file
                // as `External` + `SymbolKey`, never as a `Var` naming a binder only this
                // file's tree has.
                let input =
                    "namespace Ns\n\nmodule M =\n    let k = 3\n    let inline addK x = x + k\n"

                let lexed, file = parseFile input
                let source = Hashing.originSourceOfText input lexed
                let sem = Pipeline.analyseSem realProvider.Value source file
                let pools = Pipeline.analyse realProvider.Value source file
                let pool = TastPoolBuilder.openOver pools

                // `k` is the module's first decl; its published identity is the one its
                // `ModuleBindingInfo` mints — the same one the rewrite must have baked in.
                let kBinder =
                    match sem.Decls.[0] with
                    | TDecl.Let(head, _, _, _) ->
                        match BinderKey.ofPat head with
                        | ValueSome b -> b
                        | ValueNone -> failtest "expected `let k` to introduce a binder"
                    | other -> failtestf "expected `let k` first, got %A" other

                let kKey = BinderKey.identity kBinder

                let expected =
                    match Map.tryFind kBinder sem.ModuleMembers with
                    | Some info -> info.Key
                    | None -> failtest "`k` has no ModuleBindingInfo"

                Expect.isEmpty pools.Residue.Diagnostics "no diagnostics"

                let body =
                    match List.ofArray pools.InlineTemplates with
                    | [ v ] -> thawPublished (TypeStore()) source (TastPoolBuilder.declTree pool v.Decl)
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

                // The only `Var` left is the template's own parameter, which the expansion
                // rebinds.
                Expect.isFalse (vars.Contains kKey) "no residual Var naming `k`'s binder"
            }

            test "an inline template referencing a TOP-LEVEL binding IS published" {
                // A top-level binding declares no module, but it is held by the file's
                // namespace and so has a `SymbolKey` like any other module-level binding.
                // The sibling rewrite bakes that key in, so the template publishes — this is
                // the case the publish-time free-`Var` check used to refuse for want of an
                // identity to name.
                let input = "let k = 3\n\nmodule M =\n    let inline addK x = x + k\n"
                let lexed, file = parseFile input
                let source = Hashing.originSourceOfText input lexed
                let sem = Pipeline.analyseSem realProvider.Value source file
                let _, pools = Pipeline.analyseWithContext realProvider.Value source file
                let frozen = TastUnpool.ofPools pools

                Expect.isEmpty frozen.Diagnostics "no diagnostics — nothing is refused"

                let kKey =
                    match sem.Decls.[0] with
                    | TDecl.Let(head, _, _, _) ->
                        match BinderKey.ofPat head with
                        | ValueSome b ->
                            match Map.tryFind b sem.ModuleMembers with
                            | Some info -> info.Key
                            | None -> failtest "the top-level `k` has no ModuleBindingInfo"
                        | ValueNone -> failtest "expected `let k` to introduce a binder"
                    | other -> failtestf "expected `let k` first, got %A" other

                // Held by the file's namespace — the global one here — so it qualifies to
                // the bare name a consumer resolves it by.
                Expect.equal
                    kKey
                    (SymbolKeyOps.valueKey (ModuleHolder.InNamespace NamespaceKey.Global) "k")
                    "a top-level binding is keyed in its file's namespace"

                let refs = ResizeArray<string * SymbolKey>()

                let collect =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.External(name, ValueSome key, _, _) -> refs.Add(name, key)
                                | _ -> ()

                                true
                    }

                let pool = TastPoolBuilder.openOver pools

                let body =
                    match List.ofArray pools.InlineTemplates with
                    | [ v ] -> thawPublished (TypeStore()) source (TastPoolBuilder.declTree pool v.Decl)
                    | other -> failtestf "expected exactly one published body, got %d" (List.length other)

                match body with
                | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr collect value
                | other -> failtestf "unexpected published decl %A" other

                Expect.contains refs ("k", kKey) "the top-level sibling reference carries its SymbolKey"
            }

            test "an inline template referencing a DESTRUCTURING module binding is diagnosed, not published" {
                // The residue the publish-time free-`Var` check still catches: `let (a, b) =
                // …` binds two names at once, so it has no single `ModuleBindingInfo` and
                // nothing for the sibling rewrite to bake in.
                let input = "module M =\n    let (a, b) = (1, 2)\n    let inline addA x = x + a\n"
                let lexed, file = parseFile input

                let _, pools =
                    Pipeline.analyseWithContext realProvider.Value (Hashing.originSourceOfText input lexed) file

                let frozen = TastUnpool.ofPools pools

                Expect.isEmpty (EqArray.toList frozen.InlineBodies) "the un-splice-able template is not published"

                let errors =
                    frozen.Diagnostics
                    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "cannot be published")

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
