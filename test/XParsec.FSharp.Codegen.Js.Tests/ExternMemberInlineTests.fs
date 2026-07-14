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
// `inline-bodies-js`, so the harvested body reaches the use site through
// `TryLookupMember(...).InlineBody` end-to-end) is deferred as disproportionate for this
// stage.

let private dummyTok: SyntaxToken =
    SyntaxToken.virtualToken (PositionedToken.Create(Token.EOF, 0))

/// A provider carrying a `widget` `.fsi` contract whose `extern` intrinsic declares
/// `members` — the member-bearing `Class` a concrete (non-interface) member surface
/// registers. Returns the provider and the resolved shape key.
let private widgetContractOf (members: string) : IExternalSymbolProvider * string =
    let ctx = VesperLib.ExtractCtx.empty ()
    // The BASE repr marks `widget` intrinsic; the platform repr is its `.fs` face.
    ctx.IntrinsicBaseReprs.["widget"] <- "object"
    ctx.IntrinsicReprs.["widget"] <- "object"

    let input = "namespace Widgets\n\ntype widget = extern with\n" + members

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

/// The single-member contract: `member Poke : int -> int`.
let private widgetContract () : IExternalSymbolProvider * string =
    widgetContractOf "    member Poke : int -> int\n"

let private ftInt: FrozenType = toFrozen BuiltinTypes.tyInt

let private ftString: FrozenType = toFrozen BuiltinTypes.tyString

let private ftWidget: FrozenType =
    FTConst(RuntimeNames.opaqueKey "widget", EqArray.empty)

/// A hand-built FROZEN `widget.Poke` member over one value parameter:
/// `member _.Poke (x: 'paramTy) : int = (# template x : int #)`. Frozen because that is
/// what the harvest reads — a published inline body never carries a live inference cell.
let private pokeMemberOf (template: string) (paramTy: FrozenType) : Frozen.TTypeMember =
    let xKey = NodeKey.ofSynthetic 2 NodeKind.SynthLambdaBody
    let thisKey = NodeKey.ofSynthetic 1 NodeKind.SynthLambdaBody

    let body =
        TExprG.ILIntrinsic(
            template,
            ValueSome paramTy,
            EqArray.ofList [ TExprG.Var(xKey, paramTy, dummyTok) ],
            ftInt,
            dummyTok
        )

    {
        Name = "Poke"
        IsStatic = false
        Kind = TMemberKind.Method
        IsOverride = false
        ThisKey = ValueSome thisKey
        BaseKey = ValueNone
        ThisTy = ftWidget
        Params = EqArray.ofList [ (xKey, paramTy) ]
        Body = body
        ReturnTy = ftInt
        MethodTypeParams = GeneralizedTypars.empty
    }

/// `member _.Poke (x: int) : int = (# "$0 + 1" x : int #)`.
let private pokeMember () : Frozen.TTypeMember = pokeMemberOf "$0 + 1" ftInt

// ─── Stage 1c: end-to-end SPLICE proof over the loadable `widget` fixture ────
//
// The fixture package (`fixtures/widget/`) carries BOTH the `.fsi` contract AND the
// `.js.fs` harvest source under `inline-bodies-js`, so the JS-native provider closes
// `TryLookupMember("widget","Poke").InlineBody` against real elaboration.
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
                match SymbolProviders.harvestMemberBody (pokeMember ()) with
                | Some body ->
                    match body.Decl with
                    | TDeclG.Let(_, TExprG.Lambda(TPatG.NamedSimple(_, thisTy, _), inner, _, _), true, declTy) ->
                        // Outermost lambda binds `this : widget`.
                        Expect.equal thisTy ftWidget "outer param is `this : widget`"

                        // Inner lambda binds the value param; its body is the IL intrinsic.
                        match inner with
                        | TExprG.Lambda(TPatG.NamedSimple(_, FTConst(k1, _), _),
                                        TExprG.ILIntrinsic _,
                                        FTFun(FTConst(k2, _), FTConst(k3, _)),
                                        _) when
                            SymbolKeyOps.simpleName k1 = DisplayName "int"
                            && SymbolKeyOps.simpleName k2 = DisplayName "int"
                            && SymbolKeyOps.simpleName k3 = DisplayName "int"
                            ->
                            ()
                        | other -> failtestf "expected inner `fun x -> (# … #)`, got %A" other

                        // `declTy` is the full curried arrow `widget -> int -> int`.
                        match declTy with
                        | FTFun(FTConst(k1, _), FTFun(FTConst(k2, _), FTConst(k3, _))) when
                            SymbolKeyOps.simpleName k1 = DisplayName "widget"
                            && SymbolKeyOps.simpleName k2 = DisplayName "int"
                            && SymbolKeyOps.simpleName k3 = DisplayName "int"
                            ->
                            ()
                        | other -> failtestf "declTy is not `widget -> int -> int`: %A" other

                        // ParamAttrs aligned to curried position: leading `this` + value param.
                        Expect.equal body.ParamAttrs.Length 2 "two curried ParamAttrs (this + x)"
                    | other -> failtestf "expected a `this`-first curried inline lambda, got %A" other
                | None -> failtest "harvestMemberBody returned None for an inline-IL member"
            }

            test "a STATIC member harvests with no leading `this` param" {
                let staticPoke =
                    { pokeMember () with
                        IsStatic = true
                        ThisKey = ValueNone
                    }

                match SymbolProviders.harvestMemberBody staticPoke with
                | Some body ->
                    match body.Decl with
                    | TDeclG.Let(_,
                                 TExprG.Lambda(TPatG.NamedSimple(_, FTConst(k0, _), _), TExprG.ILIntrinsic _, _, _),
                                 true,
                                 declTy) when SymbolKeyOps.simpleName k0 = DisplayName "int" ->
                        match declTy with
                        | FTFun(FTConst(k1, _), FTConst(k2, _)) when
                            SymbolKeyOps.simpleName k1 = DisplayName "int"
                            && SymbolKeyOps.simpleName k2 = DisplayName "int"
                            ->
                            ()
                        | other -> failtestf "static declTy is not `int -> int`: %A" other

                        Expect.equal body.ParamAttrs.Length 1 "one curried ParamAttr (x only, no this)"
                    | other -> failtestf "expected `fun x -> (# … #)` with no `this`, got %A" other
                | None -> failtest "harvestMemberBody returned None for a static inline-IL member"
            }

            // THE load-bearing assertion: the member body stores under the FINALIZED
            // member key `TryLookupMember` resolves, and the fold serves it back ON that
            // very member entry — the key AGREES.
            test "member-keyed inline body stores and serves under the finalized member key" {
                let provider, key = widgetContract ()

                // The concrete member surface survived capture and is resolvable.
                let mem =
                    match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "Poke") with
                    | ValueSome m -> m
                    | ValueNone -> failtest "TryLookupMember(widget, Poke) missing — member capture failed"

                let body =
                    match SymbolProviders.harvestMemberBody (pokeMember ()) with
                    | Some b -> b
                    | None -> failtest "harvestMemberBody returned None"

                // Store under the FINALIZED member key (NEVER a hand-rolled MemberKey).
                let byKey =
                    System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                byKey.[SymbolKey.Member mem.Key] <- body

                let served =
                    provider
                    |> ExternalSymbolProviders.withInlineBodies (fun k ->
                        match byKey.TryGetValue k with
                        | true, v -> ValueSome v
                        | _ -> ValueNone
                    )

                match served.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "Poke") with
                | ValueSome m ->
                    Expect.isTrue m.InlineBody.IsSome "the member entry carries its inline body — the key AGREES"
                | ValueNone -> failtest "TryLookupMember(widget, Poke) missed through the inline-body fold"
            }

            // The OVERLOAD hazard, pinned: two `Poke` overloads carry DIFFERENT bodies, and
            // a splice site holding one overload's `MemberKey` must get THAT overload's
            // body. Only the by-key channel can answer it — `TryLookupMember`'s
            // best-by-arity collapse serves one entry for the whole name, so a name lookup
            // would splice the `int` body into a `string` call.
            test "an OVERLOADED member's body is selected by the use site's exact MemberKey" {
                let provider, key =
                    widgetContractOf "    member Poke : int -> int\n    member Poke : string -> int\n"

                let declKey = SymbolKeyOps.qualifiedTypeKey key 0

                let overloads = provider.TryLookupMembers(declKey, "Poke")
                Expect.equal overloads.Length 2 "both `Poke` overloads are published"

                // The finalized keys DISAGREE (the `argSig` axis is what separates them);
                // never hand-rolled here — the store minted them.
                let keyOf (paramTy: string) =
                    match
                        overloads
                        |> Array.tryFind (fun m -> m.Key.ArgSig |> EqArray.toList |> List.exists (fun s -> s = paramTy))
                    with
                    | Some m -> SymbolKey.Member m.Key
                    | None ->
                        failtestf
                            "no `Poke` overload over `%s`; argSigs: %A"
                            paramTy
                            (overloads |> Array.map (fun m -> m.Key))

                let intKey = keyOf "int"
                let stringKey = keyOf "string"
                Expect.notEqual intKey stringKey "the two overloads intern under distinct keys"

                // Each overload's OWN body, stored under its OWN key.
                let bodyOf (template: string) (paramTy: FrozenType) =
                    match SymbolProviders.harvestMemberBody (pokeMemberOf template paramTy) with
                    | Some b -> b
                    | None -> failtest "harvestMemberBody returned None"

                let byKey =
                    System.Collections.Generic.Dictionary<SymbolKey, InlineBody>(HashIdentity.Structural)

                byKey.[intKey] <- bodyOf "$0 + 1" ftInt
                byKey.[stringKey] <- bodyOf "$0.length" ftString

                let served =
                    provider
                    |> ExternalSymbolProviders.withInlineBodies (fun k ->
                        match byKey.TryGetValue k with
                        | true, v -> ValueSome v
                        | _ -> ValueNone
                    )

                // The IL template a body splices — the observable that tells the two apart.
                let templateOf (body: InlineBody) : string =
                    match body.Decl with
                    | TDeclG.Let(_,
                                 TExprG.Lambda(_, TExprG.Lambda(_, TExprG.ILIntrinsic(t, _, _, _, _), _, _), _, _),
                                 _,
                                 _) -> t
                    | other -> failtestf "not a `this`-first single-param inline body: %A" other

                let splice (k: SymbolKey) =
                    match ExternalSymbolProviders.tryInlineBody served k with
                    | ValueSome b -> templateOf b
                    | ValueNone -> failtestf "no inline body served for %A" k

                Expect.equal (splice intKey) "$0 + 1" "the `int` overload splices ITS body"
                Expect.equal (splice stringKey) "$0.length" "the `string` overload splices ITS body"

                // And the name channel genuinely CANNOT serve this: it collapses the pair
                // to one entry, so one of the two keys would splice the other's body.
                match served.TryLookupMember(declKey, "Poke") with
                | ValueSome collapsed ->
                    let collapsedKey = SymbolKey.Member collapsed.Key

                    Expect.isTrue
                        (collapsedKey = intKey || collapsedKey = stringKey)
                        "the collapse picks ONE overload for the whole name"
                | ValueNone -> failtest "TryLookupMember(widget, Poke) missed"
            }

            // THE end-to-end assertion: front-end the impl `.fs` spelling and run the
            // harvest arm `collectInlineBodies` uses over the REAL elaborated + FROZEN
            // `TDecl.Type(Class)`.
            test "harvest fires on real elaboration of `type widget = (# … #) with member …`" {
                let input =
                    "module Widgets\n\n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       member _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
                     \x20   end\n"

                let lexed, file = TestHelpers.parseFile input
                let tast = Pipeline.analyse TestHelpers.jsProvider.Value input lexed file

                let errors = tast.Diagnostics |> List.filter (fun d -> d.Severity = Severity.Error)
                Expect.isEmpty errors (sprintf "no analysis errors: %A" (errors |> List.map (fun d -> d.Message)))

                // Replicate `SymbolProviders.collectInlineBodies`'s `TDecl.Type` arm
                // exactly: for each member-bearing decl, harvest each member.
                let harvested =
                    [
                        for d in tast.Decls do
                            match d with
                            | TDeclG.Type tdecl ->
                                for m in TTypeKindG.members tdecl.Kind do
                                    match SymbolProviders.harvestMemberBody m with
                                    | Some body -> yield m.Name, body
                                    | None -> ()
                            | _ -> ()
                    ]

                match harvested |> List.tryFind (fun (name, _) -> name = "Poke") with
                | Some(_, body) ->
                    match body.Decl with
                    | TDeclG.Let(_, TExprG.Lambda(TPatG.NamedSimple(_, FTConst(key, _), _), _, _, _), true, _) when
                        SymbolKeyOps.simpleName key = DisplayName "widget"
                        ->
                        ()
                    | other -> failtestf "expected a `this : widget`-first inline lambda, got %A" other
                | None -> failtestf "harvest produced no Poke body; harvested: %A" (harvested |> List.map fst)
            }
        ]
