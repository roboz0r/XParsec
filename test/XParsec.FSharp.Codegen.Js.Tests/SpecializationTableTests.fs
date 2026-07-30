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
/// product — the flattened decls AND the table its edges named — paired with the diagnostics
/// THE PASS reported, kept apart from whatever the passes before it found so that a verdict
/// about an expansion cannot be mistaken for one about the source that reached it.
///
/// The prefix mirrors `Pipeline.analyseSemWithContextForCore` up to `Elaborate.run`, stopping
/// where the table would otherwise be flattened away and discarded.
let private expandedWith
    (provider: IExternalSymbolProvider)
    (input: string)
    : InlineExpansion.Expanded * Diagnostic list =
    let lexed, file = parseFile input
    let ctx = PassContext(provider, input, lexed)
    Desugar.run ctx file
    NameResolution.run ctx file
    Unification.run ctx file
    Validation.run ctx file

    match ctx.Diagnostics |> List.ofSeq |> Diagnostic.errors with
    | [] ->
        let before = ctx.Diagnostics.Count
        let expanded = InlineExpansion.run ctx (Elaborate.elaborate ctx file)
        expanded, [ for i in before .. ctx.Diagnostics.Count - 1 -> ctx.Diagnostics.[i] ]
    | errors -> failtestf "analysis errors before expansion: %A" (errors |> List.map (fun d -> d.Message))

let private expandedWithDiagnostics (input: string) : InlineExpansion.Expanded * Diagnostic list =
    expandedWith jsProvider.Value input

let private expandedFor (input: string) : InlineExpansion.Expanded =
    match expandedWithDiagnostics input with
    | expanded, [] -> expanded
    | _, ds -> failtestf "the expansion reported: %A" (ds |> List.map (fun d -> d.Message))

/// The recursive-inline verdicts among `ds`, as the binding each closes on and the way round.
let private cyclicInlines (ds: Diagnostic list) : (string * string list) list =
    [
        for d in ds do
            match d.Kind with
            | Kind.CyclicInline(binding, via) -> yield binding, via
            | _ -> ()
    ]

/// A synthetic PRODUCER package, written under `tmp/` and resolved against like any other.
///
/// Authored here rather than added to a real manifest because the bodies these tests need are
/// exactly the ones a working library cannot hold: an inline binding that calls itself has no
/// expansion, so a package carrying one breaks every consumer that touches it. A package is
/// nonetheless the only route to a body with a retained `OriginFile` — and without one there is
/// no entry, no table, and nothing for the acyclicity check to find a cycle on.
///
/// `selfLoop`'s parameter SURVIVES its reduction, so its entry is shareable and interned;
/// `fusedLoop`'s `[<CallAtMostOnce>]` operand is fused, so its entry is deliberately NOT interned
/// and only the slot reservation stands between it and an unbounded expansion.
let private recursiveProducer: Lazy<IExternalSymbolProvider> =
    lazy
        let dir = tmpDir "Cycle.Probe"

        let write (name: string) (text: string) =
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, name), text)

        // No `depends-on`: it resolves package names against sibling directories of the package
        // itself, and this one lives beside no library. The manifest list supplies Vesper.Core.
        write
            "manifest.toml"
            """[core]
name = "Cycle.Probe"
namespace = "CycleProbe"
description = "Inline bodies that call themselves, for the acyclicity check."
files = ["probe.fsi"]
impl = []
inline-bodies = ["probe.fs"]
inline-bodies-js = ["probe.fs"]
"""

        write
            "probe.fsi"
            """namespace CycleProbe

[<AutoOpen>]
module Probe =
    val inline selfLoop: x: int -> int
    val inline fusedLoop: a: int -> b: int -> int
"""

        write
            "probe.fs"
            """namespace CycleProbe

open Vesper

[<AutoOpen>]
module Probe =
    let rec inline selfLoop (x: int) : int = selfLoop x
    let rec inline fusedLoop (a: int) ([<CallAtMostOnce>] b: int) : int = fusedLoop a b
"""

        JsNativeSymbols.buildJsNativeContractFor
            (Some Target.Js)
            (jsManifests @ [ System.IO.Path.Combine(dir, "manifest.toml") ])

/// A synthetic producer whose recursion closes on a MEMBER, which is the one head whose
/// RECEIVER is not a spine argument: the reduction PREPENDS it, so the entry's parameters are
/// peeled from `this :: args` while the application it was reached through carries only `args`.
/// A back edge taking the application's spine would therefore name the entry with one argument
/// too few — a miscompile no non-recursive member call can expose, because every other edge is
/// minted from the survivors of the very peel it belongs to.
///
/// `'T[]`'s `get_Item` is OVERRIDDEN rather than a fresh type declared: a member inline body is
/// harvested only off a `(# … #)`-rooted member (`SymbolProviders.harvestMemberBody`), and such
/// a body cannot name its own type's member — within its declaring unit that call is not
/// external at all. Reaching the member through `bounce`, whose own unit sees the array type as
/// a foreign one, is what makes the reference keyed. Bodies are keyed and a later manifest wins,
/// so appending this package replaces the real body for `[]`.get_Item.
let private recursiveMemberProducer: Lazy<IExternalSymbolProvider> =
    lazy
        let dir = tmpDir "Cycle.Member"

        let write (name: string) (text: string) =
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, name), text)

        write
            "manifest.toml"
            """[core]
name = "Cycle.Member"
namespace = "CycleMember"
description = "A member inline body that reaches itself, for the back edge's arity."
files = ["bounce.fsi"]
impl = []
inline-bodies = ["bounce.fs", "array-cycle.js.fs"]
inline-bodies-js = ["bounce.fs", "array-cycle.js.fs"]
"""

        write
            "bounce.fsi"
            """namespace CycleMember

[<AutoOpen>]
module Bounce =
    val inline bounce: a: 'a[] -> i: int -> 'a
"""

        // The array type is FOREIGN here, so `a.[i]` is a keyed member reference — the same
        // `MemberKey` the body below is harvested under.
        write
            "bounce.fs"
            """namespace CycleMember

[<AutoOpen>]
module Bounce =
    let inline bounce (a: 'a[]) (i: int) : 'a = a.[i]
"""

        write
            "array-cycle.js.fs"
            """namespace global

#nowarn "42"

open CycleMember

type 'T ``[]`` =
    (# "!0[]" #)

    with

        member this.get_Item(index: int) : 'T = (# "ldelem.any !0" type ('T) this (bounce this index) : 'T #)

    end
"""

        JsNativeSymbols.buildJsNativeContractFor
            (Some Target.Js)
            (jsManifests @ [ System.IO.Path.Combine(dir, "manifest.toml") ])

/// Every `InlineCall` edge in `e`, as the slot it names and the number of arguments it carries.
let private edgeArities (e: TExpr) : (SpecializationId * int) list =
    e
    |> TastWalk.chooseExpr (fun n ->
        match n with
        | TExpr.InlineCall(spec, args, _, _) -> ValueSome(spec, args.Length)
        | _ -> ValueNone
    )

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
    value
    |> TastWalk.chooseExpr (fun e ->
        match e with
        | TExpr.CallerExpr(body, _, _) -> ValueSome body
        | _ -> ValueNone
    )

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
            test "a self-referential `let inline` is a verdict, not an exhausted stack" {
                // A local template SPLICES, so there is no entry and no table: the guard that
                // stops this is the in-flight frame, and what it produces has to be a diagnostic
                // about the user's source — the expansion the program asks for does not exist.
                let _, ds = expandedWithDiagnostics "let rec inline f x = f x\nlet a = f 1\n"

                Expect.equal
                    (cyclicInlines ds)
                    [ "f", [] ]
                    "`f` reaches itself directly, so the chain is the binding and nothing between"
            }

            test "a MUTUALLY recursive pair is caught, and the verdict names the way round" {
                let _, ds =
                    expandedWithDiagnostics "let rec inline f x = g x\nand inline g x = f x\nlet a = f 1\n"

                // Each binding is separately unexpandable and each is reported where it is
                // written, so the two rotations are two verdicts and not one repeated.
                Expect.containsAll
                    (cyclicInlines ds)
                    [ "f", [ "g" ]; "g", [ "f" ] ]
                    "a → b → a is a cycle even though neither binding names itself"
            }

            test "a recursive SERVED body terminates into a cyclic table, which is rejected" {
                // The outlined half. `selfLoop`'s reduction is CLOSED, so its entry is shareable
                // and interned; what stops the expansion is the reserved slot, and what the call
                // that reaches it becomes is a back edge — leaving a finite, inspectable table.
                let expanded, ds = expandedWith recursiveProducer.Value "let a = selfLoop 1\n"

                Expect.equal
                    (cyclicInlines ds)
                    [ "selfLoop", [] ]
                    "the verdict comes off the TABLE, and names the template the entry resolved"

                match InlineSpecTable.findCycle expanded.Specializations with
                | ValueSome cycle ->
                    Expect.equal
                        (List.length cycle)
                        1
                        "a body that reaches itself at one grounding is a ONE-entry cycle: a self-edge"
                | ValueNone -> failtest "the table the verdict was read off must actually be cyclic"

                // Unflattened, and necessarily so: flattening is what would not terminate.
                Expect.isNonEmpty
                    (expanded.Decls
                     |> List.collect (fun (d, _) ->
                         match d with
                         | TDecl.Let(_, value, _, _) -> InlineSpecTable.edges value
                         | _ -> []
                     ))
                    "the declarations keep their edges, since nothing may walk a cyclic table"
            }

            test "a recursive served body whose reduction FUSES is rejected too" {
                // The case the INTERNING does not reach: `fusedLoop`'s `[<CallAtMostOnce>]`
                // operand is fused, so the entry is deliberately not shareable and no lookup will
                // ever return it. Only the slot reservation — which covers every outlined entry,
                // not just the shareable ones — keeps this finite.
                let expanded, ds = expandedWith recursiveProducer.Value "let a = fusedLoop 1 2\n"

                Expect.equal (cyclicInlines ds) [ "fusedLoop", [] ] "the fused path reaches the same verdict"

                Expect.isTrue
                    (ValueOption.isSome (InlineSpecTable.findCycle expanded.Specializations))
                    "…because its expansion terminated into a table rather than into the stack"

                Expect.isNonEmpty
                    (expanded.Specializations
                     |> Array.toList
                     |> List.filter (entryValue >> InlineSpecTable.containsCallerExpr))
                    "the entry really did fuse call-site material, or this exercises the shareable path again"
            }

            test "a recursion that closes on a MEMBER answers the call without losing its receiver" {
                // The one head whose receiver is not a spine argument. `a.[1]` expands
                // `get_Item`, whose reduction peels `this :: [index]`; its body reaches the same
                // member through `bounce`, and THAT call is answered rather than expanded.
                //
                // A harvested member body is served with its producer file
                // (`SymbolProviders.collectInlineBodies` anchors both halves of what it drains),
                // so the member reduction is OUTLINED: it holds a table slot, and the call that
                // reaches it while it is in flight becomes a back edge rather than an
                // un-expandable call left as written.
                let expanded, ds =
                    expandedWith recursiveMemberProducer.Value "let a = [| 1; 2; 3 |]\nlet x = a.[1]\n"

                Expect.equal
                    (cyclicInlines ds)
                    [ "get_Item", [ "bounce" ] ]
                    "the recursion closes on the MEMBER, reached through the value template that indexes it"

                let table = expanded.Specializations

                let allEdges =
                    [
                        for e in table do
                            yield! edgeArities (entryValue e)
                        for (d, _) in expanded.Decls do
                            match d with
                            | TDecl.Let(_, value, _, _) -> yield! edgeArities value
                            | TDecl.Expression(x, _) -> yield! edgeArities x
                            | TDecl.Type _ -> ()
                    ]

                Expect.isNonEmpty allEdges "the run produced edges at all, or what follows is vacuous"

                // THE invariant, over every edge the run produced, whichever site minted it: an
                // `InlineCall`'s arguments are positional against the entry's abstracted
                // parameters, so a disagreement is a call of the wrong arity — silent here, and
                // first observable in a backend.
                for (SpecializationId i, argCount) in allEdges do
                    Expect.equal
                        argCount
                        (abstractedParams (entryValue table.[i]))
                        "an edge carries exactly the arguments the entry it names abstracts"

                // …and that invariant is only a claim about the member path if the member path
                // produced an entry at all. The one slot per template, by name.
                let slotOf (name: string) : int =
                    match
                        [
                            for i in 0 .. table.Length - 1 do
                                if SymbolKeyOps.intrinsicName table.[i].Key.Template = name then
                                    yield i
                        ]
                    with
                    | [ i ] -> i
                    | other -> failtestf "expected exactly one `%s` entry; got %d" name (List.length other)

                let memberSlot = slotOf "get_Item"
                let bounceSlot = slotOf "bounce"

                Expect.equal
                    table.[memberSlot].Origin.Path.Relative
                    "array-cycle.js.fs"
                    "the member entry is anchored in the file the member was WRITTEN in, not the consuming one"

                // THE case: the back edge is minted inside `bounce`'s entry, where the source
                // spells `a.[i]` — one explicit argument. It carries TWO, because the peel it was
                // minted from prepended the receiver. An edge taking the application's own spine
                // would name this two-parameter entry with one argument, and nothing before the
                // backend would notice.
                match
                    edgeArities (entryValue table.[bounceSlot])
                    |> List.filter (fun (SpecializationId i, _) -> i = memberSlot)
                with
                | [ (_, argCount) ] ->
                    Expect.equal
                        argCount
                        2
                        "the back edge carries `this` ahead of the index — the receiver the application never held in its spine"
                | other ->
                    failtestf "`bounce`'s body closes the loop with exactly one back edge; got %d" (List.length other)
            }

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

                let origins = jsContract.Value.Origins

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
                let origins = jsContract.Value.Origins

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

            test "the edges SURVIVE the pass — placement is the backends' to do" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 1.5 * 2.5\n"

                Expect.isNonEmpty
                    (List.ofArray expanded.Specializations)
                    "the fixture actually reaches cross-package inline bodies"

                let edges =
                    [
                        for (d, _) in expanded.Decls do
                            match d with
                            | TDecl.Let(_, value, _, _) -> yield! InlineSpecTable.edges value
                            | TDecl.Expression(e, _) -> yield! InlineSpecTable.edges e
                            | TDecl.Type _ -> ()
                    ]

                // One per outlined call site, and each names a slot of the table that came
                // back with them: a decl carrying an edge into nothing is what publishing the
                // two apart would produce.
                Expect.equal (List.length edges) 2 "both operator call sites left an edge"

                for SpecializationId i in edges do
                    Expect.isLessThan i expanded.Specializations.Length "the edge names a slot the table has"
            }

            test "a fused entry named by TWO edges is what the closure assertion convicts" {
                // The condition that LICENSES the mark: a `CallerExpr` pops one frame, and "the
                // frame out" names one file only while the entry has one caller. `(&&)`'s entry
                // fuses, so a second edge to it would make its own mark undefined.
                let expanded = expandedFor "let a = true && false\n"

                let slot =
                    match
                        expanded.Specializations
                        |> Array.tryFindIndex (fun e -> SymbolKeyOps.intrinsicName e.Key.Template = "op_BooleanAnd")
                    with
                    | Some i -> i
                    | None -> failtest "the fixture must reach `(&&)`'s fused entry"

                let spec = SpecializationId slot
                let entry = expanded.Specializations.[slot]

                let edge =
                    TExpr.InlineCall(spec, EqArray.empty, TastWalk.exprTy (entryValue entry), SyntaxToken.nowhere)

                Expect.isEmpty
                    (InlineSpecTable.miscountedFusedEntries [ edge ] expanded.Specializations)
                    "one edge to a fused entry is exactly what the invariant asks for"

                Expect.equal
                    (InlineSpecTable.miscountedFusedEntries [ edge; edge ] expanded.Specializations)
                    [ spec, 2 ]
                    "…and a second one convicts it, naming the entry and the count"
            }

            test "a fused external in call-head position anchors where the ENTRY wrote it" {
                // `(|>) arg func = func arg` binds `func` to the bare external `not`, which the
                // classification substitutes into the body — so the `App` the walker then meets
                // has a head written HERE inside an application written in `ops-std.fs`. The
                // rewrite consumes the head and its mark with it, so the edge that replaces the
                // application can only be right if it takes the APPLICATION's position: there is
                // no marker left to say the head's would have been the caller's.
                let input = "let b = true\nlet a = b |> not\n"
                let expanded = expandedFor input
                let lexed, _ = parseFile input
                let origins = jsContract.Value.Origins

                let entry =
                    match entriesFor "op_PipeRight" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(|>)` entry, got %d" (List.length other)

                Expect.isEmpty
                    (callerMarked (entryValue entry))
                    "the rewrite consumed the head, so no mark is left to carry the position"

                // The entry-references-entry leg of the DAG, which no other fixture reaches: `(|>)`
                // outlines an application whose head has an entry of its own.
                Expect.isNonEmpty (InlineSpecTable.edges (entryValue entry)) "the entry's body names another entry"

                let ownToks = unmarkedPositions (entryValue entry)

                for tok in ownToks do
                    match tok.Index with
                    | TokenIndex.Virtual -> ()
                    | TokenIndex.Regular _ ->
                        Expect.equal
                            (OriginSources.tokenAt origins entry.Origin (ForeignAnchor.ofAnchor (Anchor.ofToken tok)))
                            tok
                            "every unmarked node of the entry — the edge included — reads against its `OriginFile`"

                Expect.isGreaterThan
                    (List.min (tokenIndices ownToks))
                    (int lexed.Tokens.Length)
                    "…and past the end of the CONSUMING file, so none of them is the caller's head token"
            }
        ]
