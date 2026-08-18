module XParsec.FSharp.SemanticAnalysis.Tests.InlineTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

let private analyse (input: string) =
    let lexed, file = parseFile input
    Pipeline.analyseSem realProvider.Value (Hashing.originSourceOfText lexed) file

/// Resolving a trait call needs a `PassContext` to mint the dispatched operator's key.
/// These expansions are all primitive-`int`, so no nominal dispatch reaches the minter and
/// an empty context is never read.
let private ctx0: PassContext =
    let lexed, _ = parseFile "module M"
    PassContext(realProvider.Value, Hashing.originSourceOfText lexed, CompilingAssembly.none)

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

/// The template as a CONSUMER receives it: published into the file's inline vocabulary, then
/// thawed into fresh `TyVar` roots of the RETURNED store — the only store they read back
/// against. A decl from `tast.Decls` carries named `TyTypar`s instead and cannot stand in.
let private thawedTemplate (letInline: string) : TypeStore * TDecl =
    let input = "namespace Ns\n\nmodule M =\n    " + letInline + "\n"
    let lexed, file = parseFile input
    let source = Hashing.originSourceOfText lexed
    // The vocabulary is a pool root array; unpooling it gives the DU form the wire speaks.
    let pools = Pipeline.analyse realProvider.Value source file

    let pool = TastPoolBuilder.openOver pools

    match List.ofArray pools.InlineTemplates with
    | [ v ] -> ctx0.Store, thawPublished ctx0.Store source (TastPoolBuilder.declTree pool v.Decl)
    | other -> failwithf "expected exactly one published inline body for %s, got %d" letInline (List.length other)

// A wire body keeps the PRODUCER's token indices and does not say which file they index, so a
// consumer identifies that file and reads them there. An edit leaves every index still in range, so
// the file's hash is the only thing that can tell a stale anchor from a live one.

let private producerSrc =
    "namespace Ns\n\nmodule M =\n    let inline sq x = x * x\n"

/// The same producer, edited. Same binding, same shape, DIFFERENT tokens — so a body anchored
/// against the original still dereferences cleanly here and lands on the wrong ones.
let private editedProducerSrc =
    "namespace Ns\n\nmodule M =\n    let inline twice x = x + x\n    let inline sq x = x * x\n"

/// A producer file retained the way a real collection retains one: hashed off the very text
/// that was parsed, so its hash is the one an entry's `OriginFile` is checked against.
let private retainedSource (input: string) : OriginSource =
    let lexed, _ = parseFile input

    Hashing.originSource
        {
            BucketName = "Producer"
            Relative = AssemblyFileId.ofRelative "sq.fs"
        }
        lexed

/// Every position a decl carries, in one fixed traversal order, so a body and its thaw
/// compare node for node.
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

/// The abstraction the entry `spec` identifies. An inline call is outlined, so what a use site
/// expands TO is read off the specialization table, not out of the decl the call sits in.
let private entryValue (tast: TastFile) (spec: SpecializationId) : TExpr =
    let (SpecializationId i) = spec
    snd (TSpecializationG.binding spec tast.Specializations.[i])

/// What a `do` declaration's inline call expanded to, with the entry's leading lambdas peeled:
/// the edge's arguments are positional against them, not a closure anything allocates.
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
        Pipeline.analyse realProvider.Value (Hashing.originSourceOfText lexed) file

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
                // What the entry recorded, against what the same path now holds.
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

                // The MESSAGE is asserted: a missing file or an out-of-range index throws here
                // too, and a bare `throws` would call the guard proven by either.
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
                    "let inline v0 = fun v1 -> spec#0(v1, 1)"
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

                // Structurally the same body — there is no typar to substitute — but not the
                // same object: the substituting walk is also what resolves `StaticOptimization`
                // and `TraitCall`, neither of which a backend can emit.
                Expect.equal expanded body "body structurally unchanged"
                Expect.isEmpty unresolved "a monomorphic `+` on int resolves"
            }

            test "expanding a thawed polymorphic template substitutes the typar through the body" {
                let _, decl = thawedTemplate "let inline id x = x"
                let expanded, _ = Inline.inlineExpand ctx0 decl [| BuiltinTypes.tyInt |]

                // `id`'s body is `fun x -> x`; at 'a := int every position is concrete int.
                match expanded with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst(k1, _), _),
                               TExpr.Var(_, TyConst(k2, _), _),
                               TyFun(TyConst(k3, _), TyConst(k4, _)),
                               _) when
                    [ k1; k2; k3; k4 ]
                    |> List.forall (fun k -> SymbolKeyOps.typeSimpleName k = DisplayName "int")
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
                        SymbolKeyOps.typeSimpleName k = DisplayName "bool"
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
                // At module level the parser lifts `let inline succ … in body` into a top-level
                // inline binding plus the body as its own decl, whose use site is outlined into
                // an edge. The served `+` is itself an edge inside `succ`'s entry.
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                Expect.isEmpty tast.Diagnostics "no diagnostics"

                let edge =
                    match tast.Decls with
                    | EqList [ TDecl.Let(TPat.NamedSimple _, TExpr.Lambda _, true, TyFun(TyConst(k1, _), TyConst(k2, _)))
                               TDecl.Expression(e, _) ] when
                        SymbolKeyOps.typeSimpleName k1 = DisplayName "int"
                        && SymbolKeyOps.typeSimpleName k2 = DisplayName "int"
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
                                   TExpr.InlineCall(
                                       args = EqList [ TExpr.Var _
                                                       TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _) ]),
                                   _,
                                   _) -> ()
                    | other -> failtestf "the entry is not `fun x -> x + 1` (with `+` as an edge): %A" other
                | other -> failtestf "the use site is not an edge carrying its one argument: %A" other
            }

            test "expanding the §C inline succ at its use site yields its int-typed body" {
                let tast = analyse "let inline succ x = x + 1 in succ 41"
                let succDecl = tast.Decls.[0]

                // succ is monomorphic (int -> int) — expansion is a no-op
                // substitution returning the retained `fun x -> x + 1` body.
                match fst (Inline.inlineExpand ctx0 succDecl [||]) with
                | TExpr.Lambda(TPat.NamedSimple(_, TyConst(k1, _), _),
                               TExpr.InlineCall(
                                   args = EqList [ TExpr.Var(_, TyConst(k2, _), _)
                                                   TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _) ]),
                               TyFun(TyConst(k3, _), TyConst(k4, _)),
                               _) when
                    [ k1; k2; k3; k4 ]
                    |> List.forall (fun k -> SymbolKeyOps.typeSimpleName k = DisplayName "int")
                    ->
                    ()
                | other -> failtestf "expected `fun x -> x + 1` body (with `+` as an edge), got %A" other
            }

            // A monotone counter shared across calls, so two expansions never mint the same
            // bound variable key.
            let sharedMinter () =
                let mutable n = 0

                fun () ->
                    let k = NodeKey.ofSyntheticCounter n NodeKind.SynthPreFreezeInline
                    n <- n + 1
                    k

            // Pull (bound variable key, body-Var key) out of the frozen `succ` body
            // shape `fun x -> x + 1`.
            let succBoundVarAndVar (e: TExpr) =
                match e with
                | TExpr.Lambda(TPat.NamedSimple(kb, _, _),
                               TExpr.InlineCall(
                                   args = EqList [ TExpr.Var(kv, _, _)
                                                   TExpr.Const(TConstValue.Integral(IntWidth.Int32, 1L), _, _) ]),
                               _,
                               _) -> kb, kv
                | other -> failtestf "unexpected succ body: %A" other

            test "freshen renames bound variables and rewires their references" {
                let body =
                    match firstDecl "let inline succ x = x + 1" with
                    | TDecl.Let(_, v, _, _) -> v
                    | other -> failtestf "unexpected %A" other

                let kb0, kv0 = succBoundVarAndVar body
                Expect.equal kv0 kb0 "original body Var references the original bound variable"

                let mint = sharedMinter ()
                let f1 = Inline.freshen mint body
                let f2 = Inline.freshen mint body

                let kb1, kv1 = succBoundVarAndVar f1
                let kb2, kv2 = succBoundVarAndVar f2

                // (a) every bound variable key differs from the original and between results.
                Expect.notEqual kb1 kb0 "first expansion's bound variable is fresh"
                Expect.notEqual kb2 kb0 "second expansion's bound variable is fresh"
                Expect.notEqual kb1 kb2 "the two expansions don't share a bound variable"
                // (b) the internal Var is rewired to the new bound variable.
                Expect.equal kv1 kb1 "first expansion's Var follows its fresh bound variable"
                Expect.equal kv2 kb2 "second expansion's Var follows its fresh bound variable"
            }

            test "freshen leaves a free Var untouched" {
                // `let bound = <free> in bound`: `free`'s key is never bound inside the body, so
                // it must pass through; `bound` is rebound and its reference rewired.
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
                    Expect.equal kRef kb "the bound reference follows the fresh bound variable"
                | other -> failtestf "unexpected freshened shape: %A" other
            }

            // A lambda argument bound to an inline parameter and FULLY APPLIED in the body is
            // inlined away; a stored or partially-applied one survives as a real closure.

            // The parameter annotations keep these inlines monomorphic so the expanded use
            // grounds fully — a polymorphic use would leave the template's typars free.

            test "a fully-applied inline lambda parameter is eliminated (no surviving closure)" {
                // `apply` saturates `f`, so the lambda is substituted at its use and
                // beta-reduced — no `fun` survives beyond the parameter the entry abstracts.
                let tast =
                    analyse "let inline apply (f: int -> int) (x: int) = f x in apply (fun y -> y + 1) 41"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = expandedCore tast tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "no surviving closure: %s" body)
                Expect.stringContains body ", 1)" "the inlined lambda body survives (the served `+` edge applied to 1)"
            }

            test "a doubly-applied inline lambda parameter is eliminated at both sites" {
                // `twice f x = f (f x)`: both uses are saturated, so both copies
                // of the lambda are substituted (each freshened) and no closure remains.
                let tast =
                    analyse "let inline twice (f: int -> int) (x: int) = f (f x) in twice (fun y -> y + 1) 10"

                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = expandedCore tast tast.Decls.[1]
                Expect.isFalse (body.Contains "fun") (sprintf "both closures eliminated: %s" body)
                Expect.stringContains body ", 1)" "…and both copies of the lambda's body are there to show for it"
            }

            test "a published body's reference to a NON-inline module sibling is an External carrying its key" {
                // The published body is expanded at a CONSUMER, where none of this file's bound
                // variables exist, so a module-level sibling must leave the file as `External` +
                // `SymbolKey`, never as a `Var` referencing a bound variable only this tree has.
                let input =
                    "namespace Ns\n\nmodule M =\n    let k = 3\n    let inline addK x = x + k\n"

                let lexed, file = parseFile input
                let source = Hashing.originSourceOfText lexed
                let sem = Pipeline.analyseSem realProvider.Value source file
                let pools = Pipeline.analyse realProvider.Value source file
                let pool = TastPoolBuilder.openOver pools

                // `k` is the module's first decl; its published identity is the one its
                // `ModuleBindingInfo` mints — the same one the rewrite must have baked in.
                let kBoundVar =
                    match sem.Decls.[0] with
                    | TDecl.Let(pattern, _, _, _) ->
                        match BoundVarKey.ofPat pattern with
                        | ValueSome b -> b
                        | ValueNone -> failtest "expected `let k` to introduce a bound variable"
                    | other -> failtestf "expected `let k` first, got %A" other

                let kKey = BoundVarKey.identity kBoundVar

                let expected =
                    match Map.tryFind kBoundVar sem.ModuleMembers with
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
                Expect.isFalse (vars.Contains kKey) "no residual Var naming `k`'s bound variable"
            }

            test "an inline template referencing a TOP-LEVEL binding IS published" {
                // A top-level binding declares no module, but is held by the file's namespace and
                // so has a `SymbolKey` like any module-level binding — an identity the sibling
                // rewrite can bake in, so the template publishes.
                let input = "let k = 3\n\nmodule M =\n    let inline addK x = x + k\n"
                let lexed, file = parseFile input
                let source = Hashing.originSourceOfText lexed
                let sem = Pipeline.analyseSem realProvider.Value source file
                let _, pools = Pipeline.analyseWithContext realProvider.Value source file
                let frozen = TastUnpool.ofPools pools

                Expect.isEmpty frozen.Diagnostics "no diagnostics — nothing is refused"

                let kKey =
                    match sem.Decls.[0] with
                    | TDecl.Let(pattern, _, _, _) ->
                        match BoundVarKey.ofPat pattern with
                        | ValueSome b ->
                            match Map.tryFind b sem.ModuleMembers with
                            | Some info -> info.Key
                            | None -> failtest "the top-level `k` has no ModuleBindingInfo"
                        | ValueNone -> failtest "expected `let k` to introduce a bound variable"
                    | other -> failtestf "expected `let k` first, got %A" other

                // Held by the file's namespace — the global one here — so it qualifies to
                // the bare name a consumer resolves it by.
                Expect.equal
                    kKey
                    (SymbolKeyOps.valueKey (ModuleContainer.InNamespace NamespaceKey.Global) "k")
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
                // `let (a, b) = …` binds two names at once, so it has no single
                // `ModuleBindingInfo` and nothing for the sibling rewrite to bake in.
                let input = "module M =\n    let (a, b) = (1, 2)\n    let inline addA x = x + a\n"
                let lexed, file = parseFile input

                let _, pools =
                    Pipeline.analyseWithContext realProvider.Value (Hashing.originSourceOfText lexed) file

                let frozen = TastUnpool.ofPools pools

                Expect.isEmpty (EqArray.toList frozen.InlineBodies) "the un-splice-able template is not published"

                let errors =
                    frozen.Diagnostics
                    |> List.filter (fun d -> Diagnostic.isError d && d.Message.Contains "cannot be published")

                Expect.isNonEmpty errors "the free `Var` is reported"
            }

            test "a stored inline lambda parameter survives as a closure" {
                // `pick f = f` returns its parameter rather than applying it, so the lambda
                // cannot be inlined away — it stays a real closure.
                let tast = analyse "let inline pick (f: int -> int) = f in pick (fun y -> y + 1)"
                Expect.isEmpty tast.Diagnostics "no diagnostics"
                let body = TastShape.prettyDecl tast.Decls.[1]
                Expect.stringContains body "fun" (sprintf "closure survives: %s" body)
            }
        ]
