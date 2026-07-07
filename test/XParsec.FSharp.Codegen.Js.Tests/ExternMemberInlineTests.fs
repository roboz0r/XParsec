module XParsec.FSharp.Codegen.Js.Tests.ExternMemberInlineTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js

// W9 Stage 1b — CAPTURE + STORE of a concrete `(# … #)`-bodied member on an
// intrinsic/`extern` type as a MEMBER-KEYED inline body. Pins the load-bearing
// KEY-AGREEMENT seam: the body is stored under the FINALIZED member key
// `TryLookupMember` resolves, which is the key a use-site `TExpr.ExternalMember.Key`
// will carry.
//
// Stage 1b-elab RESOLVED the former blocker: the impl spelling
// `type widget = (# "object" #) with member …` (a `TypeDefn.Abbrev` carrying
// extensions) now ELABORATES to a real `TDecl.Type(Class)` whose members carry
// `this`-first `(# … #)` bodies. The `harvest on real elaboration` test below
// front-ends that impl string and runs the SAME harvest arm `collectInlineBodies`
// uses, proving the harvest fires on genuine elaboration output.
//
// The key-agreement test still hand-builds the harvest input member and sources the
// FINALIZED key from a REAL loaded `.fsi` — wiring the FULL manifest path (a package
// carrying BOTH the widget `.fsi` contract AND the widget `.fs` under
// `inline-bodies-js`, so `TryLookupInlineBody(TryLookupMember(...))` closes against
// real elaboration end-to-end) is deferred as disproportionate for this stage.

let private dummyTok: SyntaxToken =
    SyntaxToken.virtualToken (PositionedToken.Create(Token.EOF, 0))

/// A provider carrying the `widget` `.fsi` contract (a concrete member
/// `Poke: int -> int` on an `extern` intrinsic) — the dual-faced Class + member
/// surface Edit 1 registers. Returns the provider and the resolved shape key.
let private widgetContract () : IExternalSymbolProvider * string =
    let ctx = VesperLib.ExtractCtx.empty ()
    // The BASE repr marks `widget` intrinsic; the platform repr is its `.fs` face.
    ctx.IntrinsicBaseReprs.["widget"] <- "object"
    ctx.IntrinsicReprs.["widget"] <- "object"

    let input =
        "namespace Widgets\n\ntype widget = extern with\n    member Poke : int -> int\n"

    let lexed =
        match Lexing.lexString input with
        | Result.Ok l -> l
        | Result.Error e -> failwithf "lex failed: %A" e

    let ast =
        let reader = Reader.ofLexed lexed input Set.empty

        match FSharpAst.parseSignature reader with
        | Result.Ok a -> a
        | Result.Error e -> failwithf "parse failed: %A" e

    let parsed: VesperLibManifest.ParsedFile =
        {
            File =
                {
                    BucketName = "Widgets"
                    Relative = "widget.fsi"
                    Absolute = "widget.fsi"
                }
            Input = input
            Lexed = lexed
            Ast = ast
        }

    VesperLib.extractSymbols ctx parsed
    VesperLib.finalizeDeferred ctx

    let key =
        match ctx.TypeShapes.Keys |> Seq.tryFind (fun k -> k.EndsWith "widget") with
        | Some k -> k
        | None -> failtestf "widget registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

    VesperLib.ExtractCtx.toProvider ctx, key

/// A hand-built `widget.Poke` member: `member _.Poke (x: int) : int = (# "$0 + 1" x : int #)`.
/// This is what elaboration of the impl `.fs` would surface once abbrev-with-members
/// lands — the harvest input the store keys through the provider.
let private pokeMember () : TTypeMember =
    let xKey = NodeKey.ofSynthetic 2 NodeKind.SynthLambdaBody
    let thisKey = NodeKey.ofSynthetic 1 NodeKind.SynthLambdaBody

    let body =
        TExpr.ILIntrinsic(
            "$0 + 1",
            ValueSome BuiltinTypes.tyInt,
            EqArray.ofList [ TExpr.Var(xKey, BuiltinTypes.tyInt, dummyTok) ],
            BuiltinTypes.tyInt,
            dummyTok
        )

    {
        Name = "Poke"
        IsStatic = false
        Kind = TMemberKind.Method
        IsOverride = false
        ThisKey = ValueSome thisKey
        BaseKey = ValueNone
        ThisTy = TyConst(BuiltinTypes.intrinsicKey "widget", EqArray.empty)
        Params = EqArray.ofList [ (xKey, BuiltinTypes.tyInt) ]
        Body = body
        ReturnTy = BuiltinTypes.tyInt
        MethodTypeParams = GeneralizedTypars.empty
    }

/// Serve `byKey` inline bodies over `inner`, delegating everything else — the
/// `SymbolProviders.withInlineBodies` serve path (private in production), replicated
/// here to exercise `TryLookupInlineBody` over a hand-stored member body.
let private serving
    (inner: IExternalSymbolProvider)
    (byKey: System.Collections.Generic.Dictionary<SymbolKey, InlineBody>)
    : IExternalSymbolProvider =
    { new IExternalSymbolProvider with
        member _.TryLookup name = inner.TryLookup name
        member _.TryLookupType name = inner.TryLookupType name
        member _.TryLookupMember(t, m) = inner.TryLookupMember(t, m)
        member _.TryLookupMembers(t, m) = inner.TryLookupMembers(t, m)
        member _.TryLookupIndexSignature t = inner.TryLookupIndexSignature t
        member _.TryLookupUnionCase c = inner.TryLookupUnionCase c
        member _.AmbientOpenPrefixes = inner.AmbientOpenPrefixes

        member _.TryLookupInlineBody key =
            match byKey.TryGetValue key with
            | true, v -> ValueSome v
            | _ -> ValueNone

        member _.TryLookupInlineBodyByName name = inner.TryLookupInlineBodyByName name
        member _.IntrinsicReverseCanon = inner.IntrinsicReverseCanon
        member _.IntrinsicForwardRepr = inner.IntrinsicForwardRepr
    }

// ─── Stage 1c: end-to-end SPLICE proof over the loadable `widget` fixture ────
//
// The fixture package (`fixtures/widget/`) carries BOTH the `.fsi` contract AND the
// `.js.fs` harvest source under `inline-bodies-js`, so the JS-native provider closes
// `TryLookupInlineBody(TryLookupMember("widget","Poke").Key)` against real elaboration.
// Stacked AHEAD of `jsManifests` (which carry Vesper.Core, so `int` resolves).

/// `fixtures/widget/manifest.toml`.
let private widgetManifest: string =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "fixtures", "widget", "manifest.toml")

/// The JS-native provider stack with the `widget` fixture layered ahead of the standard
/// JS manifests — so widget's Class + `Poke` member AND the harvested member inline body
/// (keyed under the finalized member key) are all present.
let private widgetProvider: Lazy<IExternalSymbolProvider> =
    lazy JsNativeSymbols.buildJsNativeContractFor (Some Target.Js) (widgetManifest :: TestHelpers.jsManifests)

/// Emit a consumer snippet through the widget-inclusive provider. No runtime module is
/// injected: `widget`'s member is fully spliced, so the emitted `usePoke` imports nothing.
let private emitWidget (input: string) : string =
    TestHelpers.emitWith widgetProvider.Value Map.empty false input

[<Tests>]
let tests =
    testList
        "ExternMemberInline"
        [
            // THE Stage 1c end-to-end assertion: a consumer call `w.Poke 41` on the
            // loadable fixture SPLICES its member body (`41 + 1`) — no `.Poke(` method
            // call survives, and `widget`'s harvest-only `Class` decl never reaches emit.
            test "`w.Poke 41` splices to `41 + 1` end-to-end (no `.Poke`, no `class widget`)" {
                let js = emitWidget "open Widgets\nlet usePoke (w: widget) : int = w.Poke 41\n"

                // The spliced body: the `$0 + 1` template with `$0` ← the arg `41`
                // (the operand parenthesises to `(41)`).
                Expect.stringContains js "(41) + 1" (sprintf "expected the spliced `(41) + 1` body, got:\n%s" js)

                // No method call survived — the member was spliced, not called.
                Expect.isFalse (js.Contains ".Poke") (sprintf "a `.Poke` method call leaked into emit:\n%s" js)

                // The harvest-only `Class` decl (widget's `.js.fs` `TDecl.Type(Class)`) must
                // NEVER reach emit — it lives only in the inline-bodies file. Pins the
                // 1b-elab flag that a harvest-only Class decl is not emitted.
                Expect.isFalse (js.Contains "class widget") (sprintf "widget's Class decl leaked into emit:\n%s" js)
            }

            test "harvestMemberBody mints a `this`-first curried inline TDecl.Let" {
                match SymbolProviders.harvestMemberBody "widget" (pokeMember ()) with
                | Some mb ->
                    Expect.equal mb.MemberName "Poke" "member name preserved"
                    Expect.equal mb.TypeName "widget" "declaring type name preserved"

                    match mb.Body.Decl with
                    | TDecl.Let(_, TExpr.Lambda(TPat.NamedSimple(_, thisTy, _), inner, _, _), true, declTy) ->
                        // Outermost lambda binds `this : widget`.
                        Expect.equal
                            thisTy
                            (TyConst(BuiltinTypes.intrinsicKey "widget", EqArray.empty))
                            "outer param is `this : widget`"

                        // Inner lambda binds the value param; its body is the IL intrinsic.
                        match inner with
                        | TExpr.Lambda(TPat.NamedSimple(_, TyConst(k1, _), _),
                                       TExpr.ILIntrinsic _,
                                       TyFun(TyConst(k2, _), TyConst(k3, _)),
                                       _) when
                            SymbolKeyOps.simpleName k1 = "int"
                            && SymbolKeyOps.simpleName k2 = "int"
                            && SymbolKeyOps.simpleName k3 = "int"
                            ->
                            ()
                        | other -> failtestf "expected inner `fun x -> (# … #)`, got %A" other

                        // `declTy` is the full curried arrow `widget -> int -> int`.
                        match declTy with
                        | TyFun(TyConst(k1, _), TyFun(TyConst(k2, _), TyConst(k3, _))) when
                            SymbolKeyOps.simpleName k1 = "widget"
                            && SymbolKeyOps.simpleName k2 = "int"
                            && SymbolKeyOps.simpleName k3 = "int"
                            ->
                            ()
                        | other -> failtestf "declTy is not `widget -> int -> int`: %A" other

                        // ParamAttrs aligned to curried position: leading `this` + value param.
                        Expect.equal mb.Body.ParamAttrs.Length 2 "two curried ParamAttrs (this + x)"
                    | other -> failtestf "expected a `this`-first curried inline lambda, got %A" other
                | None -> failtest "harvestMemberBody returned None for an inline-IL member"
            }

            test "a STATIC member harvests with no leading `this` param" {
                let staticPoke =
                    { pokeMember () with
                        IsStatic = true
                        ThisKey = ValueNone
                    }

                match SymbolProviders.harvestMemberBody "widget" staticPoke with
                | Some mb ->
                    match mb.Body.Decl with
                    | TDecl.Let(_,
                                TExpr.Lambda(TPat.NamedSimple(_, TyConst(k0, _), _), TExpr.ILIntrinsic _, _, _),
                                true,
                                declTy) when SymbolKeyOps.simpleName k0 = "int" ->
                        match declTy with
                        | TyFun(TyConst(k1, _), TyConst(k2, _)) when
                            SymbolKeyOps.simpleName k1 = "int" && SymbolKeyOps.simpleName k2 = "int"
                            ->
                            ()
                        | other -> failtestf "static declTy is not `int -> int`: %A" other

                        Expect.equal mb.Body.ParamAttrs.Length 1 "one curried ParamAttr (x only, no this)"
                    | other -> failtestf "expected `fun x -> (# … #)` with no `this`, got %A" other
                | None -> failtest "harvestMemberBody returned None for a static inline-IL member"
            }

            // THE load-bearing assertion: the member body stores under the FINALIZED
            // member key `TryLookupMember` resolves and is retrievable via
            // `TryLookupInlineBody(mem.Key)` — the key AGREES.
            test "member-keyed inline body stores and serves under the finalized member key" {
                let provider, key = widgetContract ()

                // Edit 1: the concrete member surface survived capture and is resolvable.
                let mem =
                    match provider.TryLookupMember(key, "Poke") with
                    | ValueSome m -> m
                    | ValueNone -> failtest "TryLookupMember(widget, Poke) missing — Edit 1 capture failed"

                // Edit 2: harvest the `this`-first inline body.
                let mb =
                    match SymbolProviders.harvestMemberBody key (pokeMember ()) with
                    | Some mb -> mb
                    | None -> failtest "harvestMemberBody returned None"

                // Edit 3: store under the FINALIZED member key (NEVER a hand-rolled MemberKey).
                let byKey =
                    System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                byKey.[mem.Key] <- mb.Body

                let served = serving provider byKey

                match served.TryLookupInlineBody mem.Key with
                | ValueSome _ -> ()
                | ValueNone -> failtest "TryLookupInlineBody(mem.Key) missed — key DISAGREEMENT"
            }

            // THE end-to-end assertion: front-end the impl `.fs` spelling and run the
            // harvest arm `collectInlineBodies` uses over the REAL elaborated
            // `TDecl.Type(Class)`. Proves Stage 1b-elab wired the abbrev-with-members
            // host so the (formerly dormant) member-inline harvest fires on genuine
            // elaboration output — not a hand-built member.
            test "harvest fires on real elaboration of `type widget = (# … #) with member …`" {
                let input =
                    "module Widgets\n\n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       member _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
                     \x20   end\n"

                let lexed, file = TestHelpers.parseFile input
                let tast = Pipeline.analyseSem TestHelpers.jsProvider.Value input lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no analysis errors: %A" (errors |> List.map (fun d -> d.Message)))

                // Replicate `SymbolProviders.collectInlineBodies`'s `TDecl.Type` arm
                // exactly: for each `Class`-kind decl, harvest each member.
                let harvested =
                    [
                        for d in tast.Decls do
                            match d with
                            | TDecl.Type tdecl ->
                                match tdecl.Kind with
                                | TTypeKind.Class clsG ->
                                    for m in clsG.Members do
                                        match SymbolProviders.harvestMemberBody tdecl.Name m with
                                        | Some mb -> yield mb
                                        | None -> ()
                                | _ -> ()
                            | _ -> ()
                    ]

                match harvested |> List.tryFind (fun mb -> mb.MemberName = "Poke") with
                | Some mb ->
                    Expect.equal mb.TypeName "widget" "harvested under the abbrev's name"

                    match mb.Body.Decl with
                    | TDecl.Let(_, TExpr.Lambda(TPat.NamedSimple(_, TyConst(key, _), _), _, _, _), true, _) when
                        SymbolKeyOps.simpleName key = "widget"
                        ->
                        ()
                    | other -> failtestf "expected a `this : widget`-first inline lambda, got %A" other
                | None -> failtestf "harvest produced no Poke body; harvested: %A" harvested
            }
        ]
