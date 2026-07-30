module XParsec.FSharp.Codegen.Js.Tests.SpecializationTableTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The RESOLVED-SPECIALIZATION table `Passes.InlineExpansion` builds: what it interns, what it
// shares, and — the property the whole deferral exists for — where an entry's nodes are
// anchored.
//
// Here rather than in the SemanticAnalysis suite because a served body only carries an anchor
// domain when the provider RETAINED the producer file, and the only stack that does is the
// codegen contract (`SymbolProviders.inlineBodies`, which parses the `inline-bodies` files and
// keeps them). The front-end-only providers the SA suite composes publish no bodies at all.

/// Run the front end up to (and including) inline expansion, and hand back the pass's own
/// product — the flattened decls AND the table its edges named.
///
/// The prefix mirrors `Pipeline.analyseSemWithContextForCore` up to `Elaborate.run`, stopping
/// where the table would otherwise be flattened away and discarded.
let private expandedFor (input: string) : InlineExpansion.Expanded =
    let lexed, file = parseFile input
    let ctx = PassContext(jsProvider.Value, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    Validation.run ctx file

    match ctx.Diagnostics |> List.ofSeq |> Diagnostic.errors with
    | [] -> InlineExpansion.run ctx (Elaborate.elaborate ctx file)
    | errors -> failtestf "analysis errors: %A" (errors |> List.map (fun d -> d.Message))

/// The entries resolved from the template `name` names — `op_Addition`, `op_Multiply`, … —
/// picked out of a table that also holds every other inline the source happened to reach.
let private entriesFor (name: string) (table: TSpecialization[]) : TSpecialization list =
    [
        for e in table do
            if SymbolKeyOps.intrinsicName e.Key.Template = name then
                yield e
    ]

/// Every position a specialization entry's declaration carries, in `TastConvert`'s own
/// traversal order — the total walk of the position axis, so nothing an entry holds is
/// exempt from the anchoring assertions below.
let private positions (e: TSpecialization) : SyntaxToken list =
    let acc = ResizeArray<SyntaxToken>()

    TastConvert.decl
        id
        (fun t ->
            acc.Add t
            t
        )
        e.Decl
    |> ignore

    List.ofSeq acc

/// Every position an EXPRESSION subtree carries, on the same total walk of the position axis
/// `positions` takes over a whole declaration.
let private exprPositions (e: TExpr) : SyntaxToken list =
    let acc = ResizeArray<SyntaxToken>()

    TastConvert.expr
        id
        (fun t ->
            acc.Add t
            t
        )
        e
    |> ignore

    List.ofSeq acc

/// An entry's abstraction. An entry is ALWAYS a `TDecl.Let` of lambdas, so every assertion
/// about its body reads it through here.
let private entryValue (e: TSpecialization) : TExpr =
    match e.Decl with
    | TDecl.Let(_, value, _, _) -> value
    | other -> failtestf "an entry is a `TDecl.Let` of lambdas; got %A" other

/// The entry's own arity: the leading lambdas an `InlineCall`'s arguments are positional
/// against. Nothing stores it, which is the point — a parameter the reduction fused is simply
/// not one of these.
let rec private abstractedParams (value: TExpr) : int =
    match value with
    | TExpr.Lambda(_, body, _, _) -> 1 + abstractedParams body
    | _ -> 0

/// Every subtree a `CallerExpr` marks as written one frame OUT from the entry it sits in.
let private callerMarked (value: TExpr) : TExpr list =
    let acc = ResizeArray<TExpr>()

    TastWalk.iterExpr
        { TastWalk.identityIter with
            VisitExpr =
                fun _ e ->
                    match e with
                    | TExpr.CallerExpr(body, _, _) -> acc.Add body
                    | _ -> ()

                    true
        }
        value

    List.ofSeq acc

/// The positions of the entry's OWN expression nodes — everything a caller mark does not
/// cover. Stops AT a mark rather than skipping the node, so a nested mark inside a marked
/// subtree stays where it belongs.
let private unmarkedPositions (value: TExpr) : SyntaxToken list =
    let acc = ResizeArray<SyntaxToken>()

    TastWalk.iterExpr
        { TastWalk.identityIter with
            VisitExpr =
                fun _ e ->
                    match e with
                    | TExpr.CallerExpr _ -> false
                    | _ ->
                        acc.Add(TastWalk.exprTok e)
                        true
        }
        value

    List.ofSeq acc

let private tokenIndices (toks: SyntaxToken list) : int list =
    toks
    |> List.choose (fun t ->
        match t.Index with
        | TokenIndex.Regular i -> Some(int i)
        | TokenIndex.Virtual -> None
    )

[<Tests>]
let tests =
    testList
        "SpecializationTable"
        [
            test "two call sites at the SAME grounding share one entry" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 30 + 40\n"

                match entriesFor "op_Addition" expanded.Specializations with
                | [ _ ] -> ()
                | other ->
                    failtestf
                        "two `int + int` sites ground `(+)` identically and must name ONE entry; got %d"
                        (List.length other)
            }

            test "two call sites at DIFFERENT groundings get their own entries" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 1.5 + 2.5\n"

                // `(+)` resolves its static-opt clause against the operand type, so `int` and
                // `float` are two different bodies — sharing them would emit one primitive's
                // template for the other.
                match entriesFor "op_Addition" expanded.Specializations with
                | [ x; y ] ->
                    Expect.notEqual
                        x.Key.TypeArgs
                        y.Key.TypeArgs
                        "the two entries are distinguished by their type arguments, not merely counted"
                | other ->
                    failtestf
                        "`int + int` and `float + float` are two groundings and must name TWO entries; got %d"
                        (List.length other)
            }

            test "an entry's nodes keep the PRODUCER's anchors, not the call site's" {
                // ONE token in the consuming file, so a body collapsed onto the call site would
                // resolve to index 0 everywhere and could not possibly index past this file.
                let input = "let a = 1 + 2\n"
                let expanded = expandedFor input
                let lexed, _ = parseFile input

                let entry =
                    match entriesFor "op_Addition" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(+)` entry, got %d" (List.length other)

                let origins = JsNativeSymbols.jsNativeInlineOriginsFor (Some Target.Js) jsManifests

                let indices = positions entry |> tokenIndices
                Expect.isNonEmpty indices "the entry actually carries positions"

                Expect.isGreaterThan
                    (indices |> List.distinct |> List.length)
                    1
                    "…more than one of them, or a collapse onto a single token would be indistinguishable from keeping them"

                // The load-bearing assertion: every one of those integers reads back, against
                // the PRODUCER file the entry names, as exactly the token the node carries.
                // Nothing weaker would do — an index is in range against the consuming file
                // too, and would simply name an unrelated token of it.
                for tok in positions entry do
                    match tok.Index with
                    | TokenIndex.Virtual -> ()
                    | TokenIndex.Regular i ->
                        Expect.equal
                            (OriginSources.tokenAt origins entry.Origin (ForeignAnchor.ofAnchor (Anchor.ofToken tok)))
                            tok
                            "an entry node resolves against the file its `OriginFile` names"

                Expect.isGreaterThan
                    (List.max indices)
                    (int lexed.Tokens.Length)
                    "…and past the end of the CONSUMING file, so these cannot be its indices"
            }

            test "a nullary intrinsic value reference is an ordinary entry, shared across sites" {
                // `undefined` is a zero-operand `(# … #)` alias: no parameters to fuse, so its
                // entry is closed by construction and both references name it.
                let expanded =
                    expandedFor "let a: undefined = undefined\nlet b: undefined = undefined\n"

                match entriesFor "undefined" expanded.Specializations with
                | [ e ] ->
                    match e.Decl with
                    | TDecl.Let(_, TExpr.ILIntrinsic _, _, _) -> ()
                    | other -> failtestf "a nullary intrinsic entry's body is the intrinsic itself; got %A" other
                | other -> failtestf "two `undefined` references must name ONE entry; got %d" (List.length other)
            }

            test "a FUSED parameter is not a parameter of the entry — arity is what survived" {
                // `(&&)` is `let inline (&&) a [<CallAtMostOnce>] b = if a then b else false`:
                // `b` is substituted at its single use rather than bound, so it vanishes into
                // the body and the entry abstracts ONE parameter, not two. Nothing records an
                // arity — the lambda spine and the edge's argument count are the same fact.
                let expanded = expandedFor "let a = true && false\n"

                let entry =
                    match entriesFor "op_BooleanAnd" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(&&)` entry, got %d" (List.length other)

                Expect.equal (abstractedParams (entryValue entry)) 1 "`(&&)` takes two operands and outlines one"
            }

            test "a fused argument is MARKED as caller material" {
                let expanded = expandedFor "let a = true && false\n"

                let entry =
                    match entriesFor "op_BooleanAnd" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(&&)` entry, got %d" (List.length other)

                // Exactly one, and unconditionally: `false` is as trivial an argument as an
                // expression gets, and it is marked anyway — an invariant that skipped the
                // trivial cases could not be checked at all.
                match callerMarked (entryValue entry) with
                | [ _ ] -> ()
                | other ->
                    failtestf
                        "`(&&)`'s `[<CallAtMostOnce>]` operand is the one fused parameter, so the entry marks exactly one subtree; got %d"
                        (List.length other)
            }

            test "a marked subtree keeps CONSUMER anchors while the body around it keeps PRODUCER anchors" {
                // The entire point of the node: `a && b` outlines as `if a then ⟨b⟩ else false`,
                // where the `if`/`then`/`else` were written in `ops-platform.fs` and `b` was
                // written HERE — one entry, two anchor domains, told apart by the mark.
                let input = "let a = true && false\n"
                let expanded = expandedFor input
                let lexed, _ = parseFile input

                let entry =
                    match entriesFor "op_BooleanAnd" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(&&)` entry, got %d" (List.length other)

                let value = entryValue entry
                let origins = JsNativeSymbols.jsNativeInlineOriginsFor (Some Target.Js) jsManifests

                let markedIndices = callerMarked value |> List.collect exprPositions |> tokenIndices

                Expect.isNonEmpty markedIndices "the marked subtree actually carries positions"

                for tok in callerMarked value |> List.collect exprPositions do
                    match tok.Index with
                    | TokenIndex.Virtual -> ()
                    | TokenIndex.Regular i ->
                        Expect.equal
                            (SyntaxToken.syntaxToken lexed.Tokens.[i] (int i))
                            tok
                            "a marked node resolves against the CONSUMING file to the token it carries"

                let ownIndices = unmarkedPositions value |> tokenIndices
                Expect.isNonEmpty ownIndices "the entry's own body actually carries positions"

                for tok in unmarkedPositions value do
                    match tok.Index with
                    | TokenIndex.Virtual -> ()
                    | TokenIndex.Regular _ ->
                        Expect.equal
                            (OriginSources.tokenAt origins entry.Origin (ForeignAnchor.ofAnchor (Anchor.ofToken tok)))
                            tok
                            "an unmarked node resolves against the file the entry's `OriginFile` names"

                // The discriminating half: the two sets cannot be the same index space, because
                // the producer's run past the end of a consuming file this short. Without it,
                // both resolutions above could be reading one file twice.
                Expect.isLessThan
                    (List.max markedIndices)
                    (int lexed.Tokens.Length)
                    "every marked index is inside the consuming file"

                Expect.isGreaterThan
                    (List.min ownIndices)
                    (int lexed.Tokens.Length)
                    "…and every unmarked one is past its end, so they are not its indices"
            }

            test "a SHAREABLE entry marks nothing — which is what makes the mark well-defined" {
                // Two sites at one grounding name ONE entry (asserted above), so "the caller" of
                // anything inside it names no single file. `Peeled.isClosed` is what rules the
                // fusions out; this is that condition observed from the outside.
                let expanded = expandedFor "let a = 1 + 2\nlet b = 30 + 40\n"

                let entry =
                    match entriesFor "op_Addition" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(+)` entry, got %d" (List.length other)

                Expect.isEmpty
                    (callerMarked (entryValue entry))
                    "a closed reduction fuses nothing, so a shared entry is one anchor domain throughout"
            }

            test "the flattener unwraps every mark — no marker survives the pass" {
                let expanded = expandedFor "let a = true && false\n"

                Expect.isNonEmpty
                    (expanded.Specializations
                     |> Array.toList
                     |> List.collect (entryValue >> callerMarked))
                    "the fixture actually produces a marked entry, or what follows is vacuous"

                let mutable marks = 0

                let it =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.CallerExpr _ -> marks <- marks + 1
                                | _ -> ()

                                true
                    }

                for (d, _) in expanded.Decls do
                    match d with
                    | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr it value
                    | TDecl.Expression(e, _) -> TastWalk.iterExpr it e
                    | TDecl.Type _ -> ()

                Expect.equal
                    marks
                    0
                    "flattening collapses the frame stack, so a marker that outlived it would claim a distinction the tree no longer draws"
            }

            test "the table is what the decls were flattened FROM — no edge survives the pass" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 1.5 * 2.5\n"

                Expect.isNonEmpty
                    (List.ofArray expanded.Specializations)
                    "the fixture actually reaches cross-package inline bodies"

                let mutable edges = 0

                let it =
                    { TastWalk.identityIter with
                        VisitExpr =
                            fun _ e ->
                                match e with
                                | TExpr.InlineCall _ -> edges <- edges + 1
                                | _ -> ()

                                true
                    }

                for (d, _) in expanded.Decls do
                    match d with
                    | TDecl.Let(_, value, _, _) -> TastWalk.iterExpr it value
                    | TDecl.Expression(e, _) -> TastWalk.iterExpr it e
                    | TDecl.Type _ -> ()

                Expect.equal edges 0 "every edge was spliced back, so the pass's output is what it always was"
            }
        ]
