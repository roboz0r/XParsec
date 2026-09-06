module XParsec.FSharp.Codegen.Js.Tests.SpecializationTableTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.Codegen.Common
open XParsec.FSharp.Codegen.Js
open XParsec.FSharp.Codegen.Js.Tests.TestHelpers

// The RESOLVED-SPECIALIZATION table inline expansion builds: what it interns, what it shares,
// and where an entry's nodes are anchored. In this suite rather than SemanticAnalysis because
// only the codegen contract retains declaring files, so only it can serve an anchored body.

/// The front end run up to (and including) inline expansion.
type private Analysed =
    {
        /// The context the passes ran against, so a test may run a LATER pass over the same
        /// state and ask what the table costs it.
        Ctx: PassContext
        Expanded: InlineExpansion.Expanded
        /// What THE EXPANSION reported, kept apart from what the passes before it found.
        Diagnostics: Diagnostic list
    }

/// Run the front end up to and including inline expansion, and hand back the pass's own
/// product: the flattened decls AND the table its edges reference. The production pipeline runs
/// the same prefix, then flattens the table away and discards it.
let private expandedWith (provider: IExternalSymbolProvider) (input: string) : Analysed =
    let lexed, file = parseFile input

    let ctx = PassContext(provider, LexedFile.ofText lexed, testCompiling)

    NameResolution.run ctx file
    Unification.run ctx file
    Validation.run ctx file

    match ctx.Diagnostics |> List.ofSeq |> Diagnostic.errors with
    | [] ->
        let before = ctx.Diagnostics.Count
        let expanded = InlineExpansion.run ctx (Elaborate.elaborate ctx file)

        {
            Ctx = ctx
            Expanded = expanded
            Diagnostics = [ for i in before .. ctx.Diagnostics.Count - 1 -> ctx.Diagnostics.[i] ]
        }
    | errors -> failtestf "analysis errors before expansion: %A" (errors |> List.map (fun d -> d.Message))

let private expandedWithDiagnostics (input: string) : InlineExpansion.Expanded * Diagnostic list =
    let analysed = expandedWith jsProvider.Value input
    analysed.Expanded, analysed.Diagnostics

let private expandedFor (input: string) : InlineExpansion.Expanded =
    match expandedWithDiagnostics input with
    | expanded, [] -> expanded
    | _, ds -> failtestf "the expansion reported: %A" (ds |> List.map (fun d -> d.Message))

/// How many of `input`'s bindings the escape analysis calls `HeapShared`, with the
/// specialization table either handed to the pass or withheld from it. A fresh analysis each
/// way: the pass writes its verdicts onto the context it was given.
let private heapSharedCount (input: string) (walkTable: bool) : int =
    let analysed = expandedWith jsProvider.Value input
    let decls = EqArray.ofList [ for (d, _) in analysed.Expanded.Decls -> d ]

    let table =
        if walkTable then
            EqArray.ofArray analysed.Expanded.Specializations
        else
            EqArray.empty

    Regions.run analysed.Ctx decls table |> ignore

    analysed.Ctx.Bindings.Escape.AsDictionary()
    |> Seq.filter (fun kv -> kv.Value = EscapeState.HeapShared)
    |> Seq.length

/// The file the harness analyses `input` under, off the same mint the harness uses.
let private compilingOrigin (input: string) : AssemblyFilePath =
    let lexed, _ = parseFile input
    (LexedFile.ofText lexed).Path

/// The recursive-inline verdicts among `ds`, as the binding each closes on and the way round.
let private cyclicInlines (ds: Diagnostic list) : (string * string list) list =
    [
        for d in ds do
            match d.Kind with
            | Kind.CyclicInline(binding, via) -> yield binding, via
            | _ -> ()
    ]

/// A synthetic DECLARING package written under `tmp/`. A package is the only route to a body
/// with a retained `AssemblyFilePath`, and no working library can hold these bodies: an inline
/// binding that calls itself breaks every consumer that touches it.
let private recursivePackage: Lazy<IExternalSymbolProvider> =
    lazy
        let dir = tmpDir "Cycle.Probe"

        let write (name: string) (text: string) =
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, name), text)

        // `int` is Vesper.Core's, and a contract resolves only what its own dependencies
        // declare, so this fixture under `tmp/` spells the way back to `src/`.
        write
            "manifest.js.toml"
            """[core]
name = "Cycle.Probe"
description = "Inline bodies that call themselves, for the acyclicity check."
depends-on = ["../../src/Vesper.Core"]
files = ["probe.fsi", "probe.fs"]
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

        JsNativeSymbols.buildJsNativeContract (jsPackages @ [ dir ])

/// A declaring package whose recursion closes on a MEMBER. `'T[]`'s `get_Item` is OVERRIDDEN rather than
/// a fresh type declared (a later manifest's body wins), because a member inline body cannot
/// reference its own type's member; reaching it through `bounce` is what makes the reference keyed.
let private recursiveMemberPackage: Lazy<IExternalSymbolProvider> =
    lazy
        let dir = tmpDir "Cycle.Member"

        let write (name: string) (text: string) =
            System.IO.File.WriteAllText(System.IO.Path.Combine(dir, name), text)

        write
            "manifest.js.toml"
            """[core]
name = "Cycle.Member"
description = "A member inline body that reaches itself, for the back edge's arity."
depends-on = ["../../src/Vesper.Core"]
files = ["bounce.fsi", "bounce.fs", "array-cycle.js.fs"]
"""

        write
            "bounce.fsi"
            """namespace CycleMember

[<AutoOpen>]
module Bounce =
    val inline bounce: a: 'a[] -> i: int -> 'a
"""

        // The array type is FOREIGN here, so `a.[i]` is a keyed member reference, under the
        // same `MemberKey` the body below is lifted with.
        write
            "bounce.fs"
            """namespace CycleMember

[<AutoOpen>]
module Bounce =
    let inline bounce (a: 'a[]) (i: int) : 'a = a.[i]
"""

        write
            "array-cycle.js.fs"
            """namespace Vesper

#nowarn "42"

open CycleMember

type 'T ``[]`` =
    (# "!0[]" #)

    with

        member inline this.get_Item(index: int) : 'T = (# "ldelem.any !0" type ('T) this (bounce this index) : 'T #)

    end
"""

        JsNativeSymbols.buildJsNativeContract (jsPackages @ [ dir ])

/// Every `InlineCall` edge in `e`, as the slot it points to and the number of arguments it carries.
let private edgeArities (e: TExpr) : (SpecializationId * int) list =
    e
    |> TastWalk.chooseExpr (fun n ->
        match n with
        | TExpr.InlineCall(spec = spec; args = args) -> ValueSome(spec, args.Length)
        | _ -> ValueNone
    )

/// The entries resolved from the template `name` identifies (`op_Addition`, `op_Multiply`, …),
/// picked out of a table that also holds every other inline the source happened to reach.
let private entriesFor (name: string) (table: TSpecialization[]) : TSpecialization list =
    [
        for e in table do
            if SymbolKeyOps.intrinsicName e.Key.Template = name then
                yield e
    ]

/// `entriesFor`, split by WHERE the template is declared. An arithmetic use site resolves to TWO
/// entries under one compiled name: the operator's `let inline` module BINDING, and the static
/// MEMBER witness it dispatches to, named after the operator. Only the key's case tells them apart.
let private operatorEntriesFor (name: string) (table: TSpecialization[]) : TSpecialization list =
    entriesFor name table
    |> List.filter (fun e ->
        match e.Key.Template with
        | SymbolKey.Binding _ -> true
        | _ -> false
    )

/// The primitive-side half of `operatorEntriesFor`: `int`'s own `static member (+)`.
let private witnessEntriesFor (name: string) (table: TSpecialization[]) : TSpecialization list =
    entriesFor name table
    |> List.filter (fun e ->
        match e.Key.Template with
        | SymbolKey.Member _ -> true
        | _ -> false
    )

/// Every position a specialization entry carries, in the converter's own traversal order: a
/// total walk, so no position escapes the anchoring assertions below.
let private positions (e: TSpecialization) : SyntaxToken list =
    let acc = ResizeArray<SyntaxToken>()

    let record (t: SyntaxToken) =
        acc.Add t
        t

    TastConvert.pat id record e.Pat |> ignore
    TastConvert.expr id record e.Value |> ignore

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

let private entryValue (e: TSpecialization) : TExpr = e.Value

/// The entry's own arity: the leading lambdas an `InlineCall`'s arguments are positional
/// against. Nothing stores it, and a parameter the reduction fused is simply not one of these.
let rec private abstractedParams (value: TExpr) : int =
    match value with
    | TExpr.Lambda(_, body, _, _) -> 1 + abstractedParams body
    | _ -> 0

/// Every subtree a `CallerExpr` marks as written in a file other than its entry's.
let private callerMarked (value: TExpr) : TExpr list =
    value
    |> TastWalk.chooseExpr (fun e ->
        match e with
        | TExpr.CallerExpr(body = body) -> ValueSome body
        | _ -> ValueNone
    )

/// The positions of the entry's OWN expression nodes, meaning everything a caller mark does
/// not cover. Stops AT a mark rather than skipping the node, so a nested mark inside a marked
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
                // A local template is outlined like any other, so what stops this is the slot its
                // own expansion reserved: the call reaching it while in flight becomes a back
                // edge, leaving a finite table for the acyclicity check to convict.
                let expanded, ds = expandedWithDiagnostics "let rec inline f x = f x\nlet a = f 1\n"

                Expect.equal
                    (cyclicInlines ds)
                    [ "f", [] ]
                    "`f` reaches itself directly, so the chain is the binding and nothing between"

                Expect.isTrue
                    (ValueOption.isSome (InlineSpecTable.findCycle expanded.Specializations))
                    "…and the verdict came off a table that really is cyclic"
            }

            test "a MUTUALLY recursive pair is caught, and the verdict reports the way round" {
                let _, ds =
                    expandedWithDiagnostics "let rec inline f x = g x\nand inline g x = f x\nlet a = f 1\n"

                // ONE verdict, not one per rotation: `g → f → g` is a single loop, reported in call
                // order. It closes on `g` because `g` is the first entry minted: an inline binding
                // is walked as the function it also emits, so `f`'s body reaches `g` first.
                Expect.equal
                    (cyclicInlines ds)
                    [ "g", [ "f" ] ]
                    "a → b → a is a cycle even though neither binding references itself"
            }

            test "a recursive SERVED body terminates into a cyclic table, which is rejected" {
                // `selfLoop`'s reduction is CLOSED, so its entry is shareable and interned. What
                // stops the expansion is the reserved slot: the call that reaches it becomes a
                // back edge, leaving a finite, inspectable table.
                let {
                        Expanded = expanded
                        Diagnostics = ds
                    } =
                    expandedWith recursivePackage.Value "let a = selfLoop 1\n"

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
                         | TDecl.Let(_, value, _, _, _) -> InlineSpecTable.edges value
                         | _ -> []
                     ))
                    "the declarations keep their edges, since nothing may walk a cyclic table"
            }

            test "a recursive served body whose reduction FUSES is rejected too" {
                // The case INTERNING does not reach: `fusedLoop`'s `[<CallAtMostOnce>]` operand is
                // fused, so the entry is not shareable and no lookup returns it. Only the slot
                // reservation, which covers every outlined entry, keeps this finite.
                let {
                        Expanded = expanded
                        Diagnostics = ds
                    } =
                    expandedWith recursivePackage.Value "let a = fusedLoop 1 2\n"

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

            test "a recursion that closes on a MEMBER resolves the call without losing its object argument" {
                // A member's object argument is not an applied argument: `a.[1]` expands
                // `get_Item`, whose reduction peels `this :: [index]`, and its body reaches the
                // same member through `bounce`, where that call becomes a table edge rather than
                // an expansion.
                let {
                        Expanded = expanded
                        Diagnostics = ds
                    } =
                    expandedWith recursiveMemberPackage.Value "let a = [| 1; 2; 3 |]\nlet x = a.[1]\n"

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
                            | TDecl.Let(_, value, _, _, _) -> yield! edgeArities value
                            | TDecl.Expression(x, _) -> yield! edgeArities x
                            | TDecl.Type _ -> ()
                    ]

                Expect.isNonEmpty allEdges "the run produced edges at all, or what follows is vacuous"

                // Over every edge the run produced, whichever site minted it: an `InlineCall`'s
                // arguments are positional against the entry's abstracted parameters, so a
                // disagreement is a wrong-arity call, silent here and first seen in a backend.
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
                    (AssemblyFileId.toStored table.[memberSlot].Path.Relative)
                    "array-cycle.js.fs"
                    "the member entry is anchored in the file the member was WRITTEN in, not the consuming one"

                // The back edge is minted inside `bounce`'s entry, where the source spells
                // `a.[i]`: one explicit argument. It carries TWO, because the peel that minted it
                // prepended the object argument, and nothing before the backend would notice.
                match
                    edgeArities (entryValue table.[bounceSlot])
                    |> List.filter (fun (SpecializationId i, _) -> i = memberSlot)
                with
                | [ (_, argCount) ] ->
                    Expect.equal
                        argCount
                        2
                        "the back edge carries `this` ahead of the index — the object argument the application never held as an argument"
                | other ->
                    failtestf "`bounce`'s body closes the loop with exactly one back edge; got %d" (List.length other)
            }

            test "two call sites at the SAME grounding share one entry" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 30 + 40\n"

                // Both halves of the chain share: the operator, and the `int` witness it
                // dispatches to. Sharing only one of the two would still duplicate a body.
                match operatorEntriesFor "op_Addition" expanded.Specializations with
                | [ _ ] -> ()
                | other ->
                    failtestf
                        "two `int + int` sites ground `(+)` identically and must name ONE entry; got %d"
                        (List.length other)

                match witnessEntriesFor "op_Addition" expanded.Specializations with
                | [ _ ] -> ()
                | other -> failtestf "…and ONE `int` witness below it; got %d" (List.length other)
            }

            test "two call sites at DIFFERENT groundings get their own entries" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 1.5 + 2.5\n"

                // `(+)` dispatches its trait call to the operand type's own `static member (+)`,
                // so `int` and `float` reach two different witnesses; sharing the operator entry
                // would emit one primitive's template for the other.
                match operatorEntriesFor "op_Addition" expanded.Specializations with
                | [ x; y ] ->
                    Expect.notEqual
                        x.Key.TypeArgs
                        y.Key.TypeArgs
                        "the two entries are distinguished by their type arguments, not merely counted"
                | other ->
                    failtestf
                        "`int + int` and `float + float` are two groundings and must name TWO entries; got %d"
                        (List.length other)

                // The witnesses are distinct TEMPLATES, not one template at two groundings:
                // `int` and `float` each declare their own member.
                match witnessEntriesFor "op_Addition" expanded.Specializations with
                | [ x; y ] ->
                    Expect.notEqual x.Key.Template y.Key.Template "the two witnesses are declared on different types"
                | other -> failtestf "…and two witnesses, one per width; got %d" (List.length other)
            }

            test "an entry's nodes keep the DECLARING file's anchors, not the call site's" {
                // A very short consuming file, so a body collapsed onto the call site could not
                // possibly carry an index past its end.
                let input = "let a = 1 + 2\n"
                let expanded = expandedFor input
                let lexed, _ = parseFile input

                // The WITNESS, not the operator: `int`'s own `(+)` is where the template text
                // lives, so it is the entry with declaring-file tokens to read back.
                let entry =
                    match witnessEntriesFor "op_Addition" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `int` `(+)` witness entry, got %d" (List.length other)

                let sources = jsContract.Value.Retained

                let indices = positions entry |> tokenIndices
                Expect.isNonEmpty indices "the entry actually carries positions"

                Expect.isGreaterThan
                    (indices |> List.distinct |> List.length)
                    1
                    "…more than one of them, or a collapse onto a single token would be indistinguishable from keeping them"

                // Every one of those integers reads back, against the DECLARING file the entry
                // points to, as exactly the token the node carries. Nothing weaker would do: an index
                // is in range against the consuming file too, pointing at an unrelated token of it.
                for tok in positions entry do
                    match tok.Index with
                    | TokenIndex.Virtual -> ()
                    | TokenIndex.Regular i ->
                        Expect.equal
                            (LexedFiles.tokenAt sources entry.Path (Anchor.ofToken tok))
                            tok
                            "an entry node resolves against the file its `AssemblyFilePath` names"

                Expect.isGreaterThan
                    (List.max indices)
                    (int lexed.Tokens.Length)
                    "…and past the end of the CONSUMING file, so these cannot be its indices"
            }

            test "a nullary intrinsic value reference is an ordinary entry, shared across sites" {
                // `undefined` is a zero-operand `(# … #)` alias: no parameters to fuse, so its
                // entry is closed by construction and both references resolve to it.
                let expanded =
                    expandedFor "let a: undefined = undefined\nlet b: undefined = undefined\n"

                match entriesFor "undefined" expanded.Specializations with
                | [ e ] ->
                    match e.Value with
                    | TExpr.ILIntrinsic _ -> ()
                    | other -> failtestf "a nullary intrinsic entry's body is the intrinsic itself; got %A" other
                | other -> failtestf "two `undefined` references must name ONE entry; got %d" (List.length other)
            }

            test "a FUSED parameter is not a parameter of the entry, so arity is what survived" {
                // `(&&)` is `let inline (&&) a [<CallAtMostOnce>] b = if a then b else false`:
                // `b` is substituted at its single use rather than bound, so the entry abstracts
                // ONE parameter. Nothing records an arity; the lambda chain IS the arity.
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
                // expression gets and is marked anyway, since an invariant that skipped the
                // trivial cases could not be checked at all.
                match callerMarked (entryValue entry) with
                | [ _ ] -> ()
                | other ->
                    failtestf
                        "`(&&)`'s `[<CallAtMostOnce>]` operand is the one fused parameter, so the entry marks exactly one subtree; got %d"
                        (List.length other)
            }

            test "a marked subtree keeps CONSUMER anchors while the body around it keeps DECLARING-file anchors" {
                // The entire point of the node: `a && b` outlines as `if a then ⟨b⟩ else false`,
                // where the `if`/`then`/`else` were written in `ops-std.fs` and `b` was written
                // HERE. One entry, two anchor domains, told apart by the mark.
                let input = "let a = true && false\n"
                let expanded = expandedFor input
                let lexed, _ = parseFile input

                let entry =
                    match entriesFor "op_BooleanAnd" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(&&)` entry, got %d" (List.length other)

                let value = entryValue entry
                let sources = jsContract.Value.Retained

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
                            (LexedFiles.tokenAt sources entry.Path (Anchor.ofToken tok))
                            tok
                            "an unmarked node resolves against the file the entry's `AssemblyFilePath` names"

                // The discriminating half: the two sets cannot be one index space, because the
                // declaring file's indices run past the end of a consuming file this short. Without it,
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

            test "a SHAREABLE entry marks nothing, which is what makes sharing sound" {
                // Two sites at one grounding share ONE entry, so material fused from either would
                // be evaluated at both. The reduction's closedness condition, seen from outside.
                let expanded = expandedFor "let a = 1 + 2\nlet b = 30 + 40\n"

                let entry =
                    match operatorEntriesFor "op_Addition" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(+)` entry, got %d" (List.length other)

                Expect.isEmpty
                    (callerMarked (entryValue entry))
                    "a closed reduction fuses nothing, so a shared entry holds no site's operand"
            }

            test "the flattener unwraps every mark, so no marker survives the pass" {
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
                    | TDecl.Let(_, value, _, _, _) -> TastWalk.iterExpr it value
                    | TDecl.Expression(e, _) -> TastWalk.iterExpr it e
                    | TDecl.Type _ -> ()

                Expect.equal
                    marks
                    0
                    "flattening collapses the frame stack, so a marker that outlived it would claim a distinction the tree no longer draws"
            }

            test "the edges SURVIVE the pass, since placement is the backends' to do" {
                let expanded = expandedFor "let a = 1 + 2\nlet b = 1.5 * 2.5\n"

                Expect.isNonEmpty
                    (List.ofArray expanded.Specializations)
                    "the fixture actually reaches cross-package inline bodies"

                let edges =
                    [
                        for (d, _) in expanded.Decls do
                            match d with
                            | TDecl.Let(_, value, _, _, _) -> yield! InlineSpecTable.edges value
                            | TDecl.Expression(e, _) -> yield! InlineSpecTable.edges e
                            | TDecl.Type _ -> ()
                    ]

                // One per outlined call site, and each points to a slot of the table that came
                // back with them: a decl carrying an edge into nothing is what publishing the
                // two apart would produce.
                Expect.equal (List.length edges) 2 "both operator call sites left an edge"

                for SpecializationId i in edges do
                    Expect.isLessThan i expanded.Specializations.Length "the edge names a slot the table has"
            }

            // The two halves of an outlined call sit in DIFFERENT files, and each node says
            // which. Without that, an anchor's meaning is a property of the descent that
            // reached it rather than of the node.
            test "an edge and its mark point to the CALLING file; the entry to the declaring file" {
                let src = "let a = true && false\n"
                let expanded = expandedFor src
                let compiling = compilingOrigin src

                let entry =
                    match
                        expanded.Specializations
                        |> Array.filter (fun e -> SymbolKeyOps.intrinsicName e.Key.Template = "op_BooleanAnd")
                    with
                    | [| e |] -> e
                    | other -> failtestf "expected exactly one `(&&)` entry, got %d" other.Length

                Expect.notEqual
                    entry.Path
                    compiling
                    "the fixture must reach a body from ANOTHER file, else every origin agrees and nothing is being tested"

                let markOrigins =
                    entryValue entry
                    |> TastWalk.chooseExpr (fun e ->
                        match e with
                        | TExpr.CallerExpr(path = o) -> ValueSome o
                        | _ -> ValueNone
                    )

                Expect.isNonEmpty markOrigins "`(&&)` fuses its right operand, so its entry carries marks"

                for o in markOrigins do
                    Expect.equal o compiling "a fused operand was written HERE, not in the file the entry came from"

                let edgeOrigins =
                    [
                        for (d, _) in expanded.Decls do
                            match d with
                            | TDecl.Let(_, value, _, _, _) ->
                                yield!
                                    value
                                    |> TastWalk.chooseExpr (fun e ->
                                        match e with
                                        | TExpr.InlineCall(path = o) -> ValueSome o
                                        | _ -> ValueNone
                                    )
                            | _ -> ()
                    ]

                Expect.isNonEmpty edgeOrigins "the call site left an edge"

                for o in edgeOrigins do
                    Expect.equal o compiling "the call site is this file's material, whatever file the body came from"
            }

            test "a fused entry referenced by TWO edges is what the closure assertion convicts" {
                // Fused material belongs to the one site that wrote it. `(&&)`'s entry fuses its
                // right operand, so a second edge to it would run that call with this one's.
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

                // Only the slot is read, so the edge's own domain is free; the entry's is the
                // one file this fixture can reference without reaching back into the compile.
                let edge =
                    TExpr.InlineCall(
                        spec,
                        EqArray.empty,
                        entry.Path,
                        TastWalk.exprTy (entryValue entry),
                        SyntaxToken.nowhere
                    )

                Expect.isEmpty
                    (InlineSpecTable.miscountedFusedEntries [ edge ] expanded.Specializations)
                    "one edge to a fused entry is exactly what the invariant asks for"

                Expect.equal
                    (InlineSpecTable.miscountedFusedEntries [ edge; edge ] expanded.Specializations)
                    [ spec, 2 ]
                    "…and a second one convicts it, naming the entry and the count"
            }

            test "a fused external in applied-function position anchors where the ENTRY wrote it" {
                // `(|>) arg func = func arg` substitutes `func` with the bare external `not`, so the
                // `App` the walker meets has an applied function written HERE inside an application
                // written in `ops-std.fs`. Its mark is consumed with it, leaving only the `App`'s position.
                let input = "let b = true\nlet a = b |> not\n"
                let expanded = expandedFor input
                let lexed, _ = parseFile input
                let sources = jsContract.Value.Retained

                let entry =
                    match entriesFor "op_PipeRight" expanded.Specializations with
                    | [ e ] -> e
                    | other -> failtestf "expected exactly one `(|>)` entry, got %d" (List.length other)

                Expect.isEmpty
                    (callerMarked (entryValue entry))
                    "the rewrite consumed the applied function, so no mark is left to carry the position"

                // The entry-references-entry leg of the DAG, which no other fixture reaches: `(|>)`
                // outlines an application whose applied function has an entry of its own.
                Expect.isNonEmpty (InlineSpecTable.edges (entryValue entry)) "the entry's body names another entry"

                let ownToks = unmarkedPositions (entryValue entry)

                for tok in ownToks do
                    match tok.Index with
                    | TokenIndex.Virtual -> ()
                    | TokenIndex.Regular _ ->
                        Expect.equal
                            (LexedFiles.tokenAt sources entry.Path (Anchor.ofToken tok))
                            tok
                            "every unmarked node of the entry — the edge included — reads against its `AssemblyFilePath`"

                Expect.isGreaterThan
                    (List.min (tokenIndices ownToks))
                    (int lexed.Tokens.Length)
                    "…and past the end of the CONSUMING file, so none of them is the caller's function token"
            }

            test "a capture WRITTEN inside an entry is seen only because the table is walked" {
                // `(&&)`'s second operand is fused, so a lambda written in it lands in the ENTRY
                // and nowhere in the decls. `acc` is then a mutable captured by a closure the
                // decls do not contain, so escape analysis given only the decls never sees it.
                let src =
                    "let f () =\n    let mutable acc = 0\n    true && (fun () -> acc > 0) ()\n"

                Expect.isGreaterThan
                    (heapSharedCount src true)
                    (heapSharedCount src false)
                    "the capture is inside the entry, so withholding the table is what loses it"
            }
        ]
