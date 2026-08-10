module XParsec.FSharp.Codegen.Js.Tests.ExternMemberInlineTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Lexer.Lexing
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js

// A concrete `(# … #)`-bodied member on an intrinsic/`extern` type is captured and stored as
// a MEMBER-KEYED inline body. The store key is the finalized member key a lookup resolves,
// which is the same key a use site carries.

/// The hand-built member bodies below belong to no file, so their nodes sit at no source
/// position.
let private dummyTok: Anchor = Anchor.nowhere

/// The origin a lift of such a body names. Every node anchors `Anchor.nowhere`, which
/// resolves without consulting a retained source, so the empty text below is never read.
let private nowhereSource: OriginSource =
    {
        File = OriginFile.nowhere
        Lexed =
            match Lexing.lexString "" with
            | Result.Ok l -> l
            | Result.Error e -> failwithf "lex failed: %A" e
    }

/// A provider carrying a `widget` `.fsi` contract whose `extern` intrinsic declares
/// `members`. Returns the provider and the resolved shape key.
let private widgetContractOf (members: string) : IExternalSymbolProvider * string =
    let ctx = VesperLib.ExtractCtx.empty Target.Js
    // The marker makes `widget` intrinsic; the repr is its platform name.
    ctx.IntrinsicMarkers.Add "widget" |> ignore
    ctx.IntrinsicReprs.["widget"] <- "object"

    let input = "namespace Widgets\n\ntype widget = extern with\n" + members

    let lexed =
        match Lexing.lexString input with
        | Result.Ok l -> l
        | Result.Error e -> failwithf "lex failed: %A" e

    let ast =
        let reader = Reader.ofLexed lexed Set.empty

        match FSharpAst.parseSignature reader with
        | Result.Ok a -> a
        | Result.Error e -> failwithf "parse failed: %A" e

    let parsed: VesperLibManifest.ParsedFile =
        {
            File =
                {
                    Path =
                        {
                            BucketName = "Widgets"
                            Relative = "widget.fsi"
                        }
                    Absolute = "widget.fsi"
                }
            Lexed = lexed
            Ast = ast
        }

    VesperLib.extractSymbols ctx parsed

    let key =
        match ctx.TypeShapes.Keys |> Seq.tryFind (fun k -> k.EndsWith "widget") with
        | Some k -> k
        | None -> failtestf "widget registered no shape. Shapes: %A" (Seq.toList ctx.TypeShapes.Keys)

    VesperLib.ExtractCtx.toProvider ctx, key

/// The single-member contract: `member inline Poke : int -> int`.
let private widgetContract () : IExternalSymbolProvider * string =
    widgetContractOf "    member inline Poke : int -> int\n"

let private ftInt: FrozenType = toFrozen BuiltinTypes.tyInt

let private ftString: FrozenType = toFrozen BuiltinTypes.tyString

let private ftWidget: FrozenType =
    FTConst(RuntimeNames.opaqueKey "widget", EqArray.empty)

/// A hand-built FROZEN `widget.Poke` over one value parameter, its body minted by `mkBody`
/// from that parameter's bound variable: `member inline _.Poke (x: 'paramTy) : int = <body>`.
/// Frozen because a published inline body never carries a live inference cell.
let private pokeMemberWith (paramTy: FrozenType) (mkBody: BoundVarId -> Pooled.TExpr) : TastAccessor.TypeMember =
    // The hand-built body is a node of no file, so it gets a pool of its own; the lifting
    // mints its wrapping lambdas straight into it.
    let pool = TastPoolBuilder.openEmpty ()
    let xId = TastPoolBuilder.mintBoundVar pool

    // The parameter's definition site, taken off the `NamedSimple` pattern a source member
    // would carry.
    let xBoundVar =
        match BoundVarKey.ofPat (TPatG.NamedSimple(xId, paramTy, dummyTok)) with
        | ValueSome b -> b
        | ValueNone -> failwith "a `NamedSimple` pattern introduces a bound variable"

    let thisBoundVar = BoundVarKey.ofInterned (TastPoolBuilder.mintBoundVar pool)

    let body = mkBody xId |> TastPoolBuilder.appendExprTree pool

    {
        Name = "Poke"
        IsStatic = false
        Accessibility = Accessibility.Public
        IsInline = true
        Kind = TMemberKind.Method
        IsOverride = false
        ThisKey = ValueSome thisBoundVar
        BaseKey = ValueNone
        ThisTy = ftWidget
        Params = EqArray.ofList [ (xBoundVar, paramTy) ]
        Body = { Pool = pool; Id = body }
        ReturnTy = ftInt
        MethodTypeParams = EqArray.empty
    }

/// `member inline _.Poke (x: 'paramTy) : int = (# template x : int #)`.
let private pokeMemberOf (template: string) (paramTy: FrozenType) : TastAccessor.TypeMember =
    pokeMemberWith
        paramTy
        (fun xId ->
            TExprG.ILIntrinsic(
                template,
                ValueSome paramTy,
                EqArray.ofList [ TExprG.Var(xId, paramTy, dummyTok) ],
                ftInt,
                dummyTok
            )
        )

/// `member inline _.Poke (x: int) : int = (# "$0 + 1" x : int #)`.
let private pokeMember () : TastAccessor.TypeMember = pokeMemberOf "$0 + 1" ftInt

// End-to-end SPLICE over the loadable `widget` fixture. `fixtures/widget/` carries BOTH the
// `.fsi` contract AND its `.js.fs` bodies, so a lookup of `widget.Poke` closes on an inline
// body from real elaboration; the `(# "object" #)` binding also makes the hosts intrinsic.

/// `fixtures/widget/manifest.toml`.
let private widgetManifest: string =
    System.IO.Path.Combine(__SOURCE_DIRECTORY__, "fixtures", "widget", "manifest.toml")

/// The JS-native contract with the `widget` fixture layered ahead. The WHOLE contract, not
/// just its provider: a spliced body's positions read only against this set's retained
/// producer files, and `widget.js.fs` is in this retention and in no other.
let private widgetFixtureContract: Lazy<SymbolProviders.Contract> =
    lazy JsNativeSymbols.jsNativeContractFor Target.Js (widgetManifest :: TestHelpers.jsManifests)

/// Emit a consumer snippet through the widget-inclusive contract. No runtime module is
/// injected: `widget`'s member is fully spliced, so the emitted code imports nothing.
let private emitWidget (input: string) : string =
    TestHelpers.emitWith widgetFixtureContract.Value Map.empty false input

[<Tests>]
let tests =
    testList
        "ExternMemberInline"
        [
            test "`w.Poke 41` splices to `41 + 1` end-to-end (no `.Poke`, no `class widget`)" {
                let js = emitWidget "open Widgets\nlet usePoke (w: widget) : int = w.Poke 41\n"

                // The spliced body: the `$0 + 1` template with `$0` ← the arg `41`
                // (the operand parenthesises to `(41)`).
                Expect.stringContains js "(41) + 1" (sprintf "expected the spliced `(41) + 1` body, got:\n%s" js)

                Expect.isFalse (js.Contains ".Poke") (sprintf "a `.Poke` method call leaked into emit:\n%s" js)

                // widget's `.js.fs` `Class` decl is a splice source, not an emitted type.
                Expect.isFalse (js.Contains "class widget") (sprintf "widget's Class decl leaked into emit:\n%s" js)
            }

            // The NON-IL body. `gadget.Bump`'s body is `w.Poke x`, a keyed reference to a
            // member of a foreign type: it crosses the provider seam, re-resolves in the
            // consumer, and the splice it produces is itself spliced.
            test "`gadget.Bump w` splices its NON-IL body, and the `Poke` it yields splices too" {
                let js = emitWidget "open Widgets\nlet useBump (w: widget) : int = gadget.Bump w\n"

                Expect.stringContains js "(41) + 1" (sprintf "expected the twice-spliced `(41) + 1`, got:\n%s" js)
                Expect.isFalse (js.Contains ".Bump") (sprintf "a `.Bump` method call leaked into emit:\n%s" js)
                Expect.isFalse (js.Contains ".Poke") (sprintf "a `.Poke` method call leaked into emit:\n%s" js)
            }

            // TWO parameters: the arity at which the call site's ONE tupled argument stops
            // coinciding with the curried parameter it peels against. Left tupled, the whole
            // tuple lands in `a` and the emit is a partial application where an `int` is due.
            test "`w.Poke2(3, 4)` splices to `3 + 4`: the tupled argument opens to both parameters" {
                let js = emitWidget "open Widgets\nlet useP2 (w: widget) : int = w.Poke2(3, 4)\n"

                Expect.stringContains js "(3) + (4)" (sprintf "expected the spliced `(3) + (4)` body, got:\n%s" js)
                Expect.isFalse (js.Contains ".Poke2") (sprintf "a `.Poke2` method call leaked into emit:\n%s" js)

                // The consumer's own `(w) =>` is the ONLY arrow the snippet may emit; an
                // unsupplied curried parameter would survive as a second one.
                Expect.equal (js.Split("=>").Length - 1) 1 (sprintf "a curried remnant survived the splice:\n%s" js)
            }

            // A tuple VALUE selects the same 2-parameter member a literal does, but the splice
            // needs one EXPRESSION per parameter. It normalises to `let (a, b) = t in
            // w.Poke2(a, b)`, so the tuple is destructured once and the same body splices.
            test "`w.Poke2 t` at a tuple VALUE destructures and splices the same body" {
                let js =
                    emitWidget "open Widgets\nlet useP2v (w: widget) : int =\n\x20   let t = (3, 4)\n\x20   w.Poke2 t\n"

                // The elements reach the template's `$0`/`$1` through the destructuring's
                // own bound variables, so the spliced body is an addition of the two of them.
                Expect.isTrue
                    (System.Text.RegularExpressions.Regex.IsMatch(js, @"\(\w+\) \+ \(\w+\)"))
                    (sprintf "expected the spliced `$0 + $1` body over the destructured elements, got:\n%s" js)

                Expect.isFalse (js.Contains ".Poke2") (sprintf "a `.Poke2` method call leaked into emit:\n%s" js)

                // `t` is built ONCE: the destructuring binds it, the elements read off it.
                Expect.equal
                    (js.Split("[3, 4]").Length - 1)
                    1
                    (sprintf "the tuple must be built once, not per element:\n%s" js)
            }

            // The same arity, STATIC: no object argument occupies curried position 0, so the
            // untupled arguments land at a different offset. Its body is itself a
            // two-parameter instance call, so one use site untuples twice.
            test "`gadget.Bump2(w, 41)` untuples a STATIC two-parameter member, twice over" {
                let js =
                    emitWidget "open Widgets\nlet useB2 (w: widget) : int = gadget.Bump2(w, 41)\n"

                Expect.stringContains js "(41) + (7)" (sprintf "expected the twice-spliced `(41) + (7)`, got:\n%s" js)
                Expect.isFalse (js.Contains ".Bump2") (sprintf "a `.Bump2` method call leaked into emit:\n%s" js)
                Expect.isFalse (js.Contains ".Poke2") (sprintf "a `.Poke2` method call leaked into emit:\n%s" js)

                Expect.equal (js.Split("=>").Length - 1) 1 (sprintf "a curried remnant survived the splice:\n%s" js)
            }

            test "liftMemberBody mints a `this`-first curried inline TDecl.Let" {
                match SymbolProviders.liftMemberBody nowhereSource (pokeMember ()) with
                | Some body ->
                    match body.Decl with
                    | TDeclG.Let(_, TExprG.Lambda(TPatG.NamedSimple(_, thisTy, _), inner, _, _), true, declTy) ->
                        Expect.equal thisTy ftWidget "outer param is `this : widget`"

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

                        match declTy with
                        | FTFun(FTConst(k1, _), FTFun(FTConst(k2, _), FTConst(k3, _))) when
                            SymbolKeyOps.simpleName k1 = DisplayName "widget"
                            && SymbolKeyOps.simpleName k2 = DisplayName "int"
                            && SymbolKeyOps.simpleName k3 = DisplayName "int"
                            ->
                            ()
                        | other -> failtestf "declTy is not `widget -> int -> int`: %A" other

                        Expect.equal body.ParamAttrs.Length 2 "two curried ParamAttrs (this + x)"
                    | other -> failtestf "expected a `this`-first curried inline lambda, got %A" other
                | None -> failtest "liftMemberBody returned None for an inline-IL member"
            }

            test "a STATIC member lifts with no leading `this` param" {
                let staticPoke =
                    { pokeMember () with
                        IsStatic = true
                        ThisKey = ValueNone
                    }

                match SymbolProviders.liftMemberBody nowhereSource staticPoke with
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
                | None -> failtest "liftMemberBody returned None for a static inline-IL member"
            }

            // `inline` on the DECLARATION is the whole test: body shape is not consulted, an
            // ordinary expression publishing exactly as an inline-IL template does. That is
            // what lets a primitive whose operator body is a BCL call be spliced at all.
            test "a non-IL body publishes when the member is declared inline" {
                let identity = pokeMemberWith ftInt (fun xId -> TExprG.Var(xId, ftInt, dummyTok))

                match SymbolProviders.liftMemberBody nowhereSource identity with
                | Some body ->
                    match body.Decl with
                    | TDeclG.Let(_, TExprG.Lambda(_, TExprG.Lambda(_, TExprG.Var _, _, _), _, _), true, _) -> ()
                    | other -> failtestf "expected a `this`-first curried lambda over the `Var` body, got %A" other
                | None -> failtest "liftMemberBody returned None for a non-IL `member inline`"
            }

            // The converse: a member the author did NOT mark `inline` is a real callable,
            // whatever it is bodied with, and an inline-IL body is the sharpest form of that.
            test "a NON-inline member publishes nothing, even with an inline-IL body" {
                let notInline = { pokeMember () with IsInline = false }

                Expect.isNone
                    (SymbolProviders.liftMemberBody nowhereSource notInline)
                    "a member without `inline` is a real callable, not a splice template"
            }

            test "member-keyed inline body stores and serves under the finalized member key" {
                let provider, key = widgetContract ()

                let mem =
                    match provider.TryLookupMember(SymbolKeyOps.qualifiedTypeKey key 0, "Poke") with
                    | ValueSome m -> m
                    | ValueNone -> failtest "TryLookupMember(widget, Poke) missing — member capture failed"

                let body =
                    match SymbolProviders.liftMemberBody nowhereSource (pokeMember ()) with
                    | Some b -> b
                    | None -> failtest "liftMemberBody returned None"

                // Store under the FINALIZED member key, never a hand-rolled one.
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

            // Two `Poke` overloads carry DIFFERENT bodies, and a splice site holding one
            // overload's key must get THAT body. Only the by-key channel can: a name lookup
            // collapses the pair to one entry, splicing the `int` body into a `string` call.
            test "an OVERLOADED member's body is selected by the use site's exact MemberKey" {
                let provider, key =
                    widgetContractOf "    member inline Poke : int -> int\n    member inline Poke : string -> int\n"

                let declKey = SymbolKeyOps.qualifiedTypeKey key 0

                let overloads = provider.TryLookupMembers(declKey, "Poke")
                Expect.equal overloads.Length 2 "both `Poke` overloads are published"

                // The finalized keys differ on the `argSig` axis, and the store minted them.
                let keyOf (paramTy: FrozenType) =
                    match overloads |> EqArray.tryFind (fun m -> m.Key.ArgSig |> EqArray.contains paramTy) with
                    | ValueSome m -> SymbolKey.Member m.Key
                    | ValueNone ->
                        failtestf
                            "no `Poke` overload over %A; argSigs: %A"
                            paramTy
                            (overloads |> EqArray.map (fun m -> m.Key))

                let intKey = keyOf ftInt
                let stringKey = keyOf ftString
                Expect.notEqual intKey stringKey "the two overloads intern under distinct keys"

                // Each overload's OWN body, stored under its OWN key.
                let bodyOf (template: string) (paramTy: FrozenType) =
                    match SymbolProviders.liftMemberBody nowhereSource (pokeMemberOf template paramTy) with
                    | Some b -> b
                    | None -> failtest "liftMemberBody returned None"

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

                // The IL template a body splices: the observable that tells the two apart.
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

                match served.TryLookupMember(declKey, "Poke") with
                | ValueSome collapsed ->
                    let collapsedKey = SymbolKey.Member collapsed.Key

                    Expect.isTrue
                        (collapsedKey = intKey || collapsedKey = stringKey)
                        "the collapse picks ONE overload for the whole name"
                | ValueNone -> failtest "TryLookupMember(widget, Poke) missed"
            }

            // Two same-name lifted signatures differing only by parameter TYPE must mint two
            // DISTINCT keys, or the second lifted body overwrites the first. Hand-built,
            // because two `(# … #)`-bodied same-name overloads are not declarable in one file.
            test "collectInlineBodies mints distinct keys for two distinct lifted overload signatures" {
                let declKey = SymbolKeyOps.qualifiedTypeKeyOf "widget" 0

                // The per-member key mint the inline-body collector performs: decl, name,
                // kind, structural argSig and method-typar arity.
                let mintKey (m: TastAccessor.TypeMember) : SymbolKey =
                    let kind =
                        match m.Kind with
                        | TMemberKind.Method -> MemberKind.Method
                        | TMemberKind.Property -> MemberKind.Property

                    SymbolKeyOps.memberKey declKey m.Name (m.Params |> EqArray.map snd) m.MethodTypeParams.Length kind

                let kInt = mintKey (pokeMemberOf "$0 + 1" ftInt)
                let kStr = mintKey (pokeMemberOf "$0.length" ftString)

                Expect.notEqual kInt kStr "distinct param types mint distinct member keys"

                // Neither overwrites the other in a by-key store.
                let byKey =
                    System.Collections.Generic.Dictionary<SymbolKey, string>(HashIdentity.Structural)

                byKey.[kInt] <- "int-body"
                byKey.[kStr] <- "string-body"
                Expect.equal byKey.Count 2 "both lifted bodies are retained under distinct keys"
                Expect.equal byKey.[kInt] "int-body" "the int overload keeps its own body"
                Expect.equal byKey.[kStr] "string-body" "the string overload keeps its own body"
            }

            test "lifting fires on real elaboration of `type widget = (# … #) with member …`" {
                let input =
                    "module Widgets\n\n\
                     type widget =\n\
                     \x20   (# \"object\" #)\n\
                     \x20   with\n\
                     \x20       member inline _.Poke (x: int) : int = (# \"$0 + 1\" x : int #)\n\
                     \x20   end\n"

                let lexed, file = TestHelpers.parseFile input
                let source = Hashing.originSourceOfText lexed
                let tast = Pipeline.analyse TestHelpers.jsProvider.Value source file
                let errors = tast.Residue.Diagnostics |> Diagnostic.errors

                Expect.isEmpty errors (sprintf "no analysis errors: %A" (errors |> List.map (fun d -> d.Message)))

                // Replicate the collector's `DeclShape.Type` arm: for each member-bearing
                // decl, lift each member.
                let pool = TastPoolBuilder.openOver tast

                let lifted =
                    [
                        for d in TastAccessor.roots pool do
                            match TastAccessor.declKind d with
                            | DeclShape.Type ->
                                let tdecl = TastAccessor.declType d

                                for m in TTypeKindG.members tdecl.Kind do
                                    match SymbolProviders.liftMemberBody source m with
                                    | Some body -> yield m.Name, body
                                    | None -> ()
                            | _ -> ()
                    ]

                match lifted |> List.tryFind (fun (name, _) -> name = "Poke") with
                | Some(_, body) ->
                    match body.Decl with
                    | TDeclG.Let(_, TExprG.Lambda(TPatG.NamedSimple(_, FTConst(key, _), _), _, _, _), true, _) when
                        SymbolKeyOps.simpleName key = DisplayName "widget"
                        ->
                        ()
                    | other -> failtestf "expected a `this : widget`-first inline lambda, got %A" other
                | None -> failtestf "lifting produced no Poke body; lifted: %A" (lifted |> List.map fst)
            }
        ]
