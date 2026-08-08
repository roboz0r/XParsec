module XParsec.FSharp.SemanticAnalysis.Tests.ExternalUnionCaseStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

// NameResolution — the one resolve-once layer — recognises external union cases in
// BOTH pattern and expression position, applying the opens / RQA / qualifier
// discipline, and stamps the resolved `ExternalUnionCase` under the ctor's
// `NodeKey` (`Resolution.ExternalUnionCaseStamp`). Unification's `InferPat` /
// `InferIdentExpr` and Elaborate's `translatePat` / `tryCtorRef` READ that stamp instead
// of handing raw spelling back to `TryLookupUnionCase(string)`. A MISSED stamp where a
// consumer reads is a phantom bound variable / mis-lowering, so these tests assert the stamp is
// present at representative pattern sites — including or-pattern alternatives, which the
// bound-variable-collection walk skips (they bind nothing) and so the stamping walk must reach
// independently.

/// A provider that knows two non-RQA unions: `Tests.Hue` (case `Blue`) whose
/// namespace `Tests` is AUTO-OPENED via `AmbientOpenPrefixes` — as the real prelude
/// auto-opens the package namespace so `Some`/`None` are bare-visible — and
/// `Other.Shade` (case `Green`) whose namespace `Other` is NOT ambient, so a bare
/// `Green` resolves only under an explicit `open Other`. `TryLookupUnionCase` is the
/// reverse case index NameResolution resolves through; F# gates a bare hit on the
/// declaring namespace being open (no global reverse case index).
let private provider: IExternalSymbolProvider =
    ExternalSymbolProviders.ofNamedLeaf
        { ExternalSymbolProviders.NamedLeaf.empty with
            TryLookupUnionCase =
                fun caseName ->
                    let mk union name =
                        ValueSome
                            {
                                UnionName = union
                                TyparArity = 0
                                Origin = SymbolOrigin.Empty
                                Case = ExternalCaseShape.create (name, [||])
                                IsRequireQualifiedAccess = false
                            }

                    match caseName with
                    | "Blue" -> mk "Tests.Hue" "Blue"
                    | "Green" -> mk "Other.Shade" "Green"
                    | _ -> ValueNone
            AmbientOpenPrefixes = [ "Tests" ]
        }

let private analyse (input: string) = analyseNameRes provider input

/// Every sub-pattern node of `p`, `p` first — the SAME `CstWalk.iterPat`
/// recursion the stamping walk uses, so a stamp missed on any position the
/// walker reaches surfaces here by construction.
let private patNodes (p: Pat<SyntaxToken>) : Pat<SyntaxToken> list =
    let acc = ResizeArray<Pat<SyntaxToken>>()

    CstWalk.iterPat
        {
            VisitPat =
                fun _ pat ->
                    acc.Add pat
                    true
        }
        p

    List.ofSeq acc

/// Every pattern node reachable in `file` — module-let patterns/args plus the patterns
/// entering scope in lambda / for-in / match-arm bodies.
let private allPats (file: ImplementationFile<SyntaxToken>) : Pat<SyntaxToken> list =
    let acc = ResizeArray<Pat<SyntaxToken>>()
    let add (p: Pat<SyntaxToken>) = acc.AddRange(patNodes p)

    let walker =
        { CstWalk.identityExprWalker with
            EnterFun =
                fun () pats ->
                    (for p in pats do
                        add p)
            EnterBindingRhs =
                fun () _ _ b ->
                    (for p in b.argumentPats do
                        add p)
            EnterLetBody =
                fun () bindings ->
                    (for b in bindings do
                        add b.pattern)
            EnterForIn = fun () p -> add p
            EnterMatchArm = fun () p -> add p
        }

    for m in CstWalk.implFileElems file do
        match m with
        | ModuleElem.FunctionOrValue(ModuleFunctionOrValueDefn.Let(bindings = bindings)) ->
            for b in bindings do
                add b.pattern

                for p in b.argumentPats do
                    add p

                CstWalk.iterExpr walker () b.expr
        | ModuleElem.Expression e -> CstWalk.iterExpr walker () e
        | _ -> ()

    List.ofSeq acc

/// The ctor pattern nodes whose case name (bare ident, or last segment of a
/// qualified `Pat.Named`) equals `caseName`.
let private caseCtors (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) (caseName: string) =
    allPats file
    |> List.filter (fun p ->
        match p with
        | Pat.NamedSimple t -> ctx.NameOf t = caseName
        | Pat.Named(longIdent = li) when li.Idents.Length >= 1 ->
            ctx.NameOf li.Idents.[li.Idents.Length - 1] = caseName
        | _ -> false
    )

/// Assert every `caseName` ctor in `input` carries a pattern-position stamp,
/// and that exactly `expected` ctors were found (so a missed traversal position
/// can't pass by finding zero).
let private assertPatStamped (input: string) (caseName: string) (expected: int) =
    let ctx, file = analyse input
    let ctors = caseCtors ctx file caseName
    Expect.equal ctors.Length expected (sprintf "ctor count for '%s' in: %s" caseName input)

    for h in ctors do
        Expect.isTrue
            (ctx.Resolution.ExternalUnionCaseStamp.ContainsKey(CstKeys.ofPat h))
            (sprintf "external case '%s' stamped at its ctor pattern in: %s" caseName input)

/// Assert every `caseName` ctor in `input` is NOT stamped (its declaring namespace
/// is not open, so NameResolution treats the name as a bound variable, not an external
/// case — the opens false-accept this gate closes).
let private assertPatNotStamped (input: string) (caseName: string) (expected: int) =
    let ctx, file = analyse input
    let ctors = caseCtors ctx file caseName
    Expect.equal ctors.Length expected (sprintf "ctor count for '%s' in: %s" caseName input)

    for h in ctors do
        Expect.isFalse
            (ctx.Resolution.ExternalUnionCaseStamp.ContainsKey(CstKeys.ofPat h))
            (sprintf "bare case '%s' in a non-opened namespace is NOT stamped in: %s" caseName input)

/// Does `input` raise an "Unresolved" diagnostic? (bare external case in
/// expression position whose namespace is not open falls to unresolved-identifier).
let private hasUnresolved (input: string) : bool =
    let ctx, _ = analyse input
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

[<Tests>]
let tests =
    testList
        "ExternalUnionCaseStamp"
        [
            test "bare case pattern is stamped" {
                assertPatStamped "let f (o: obj) = match o with | Blue -> 1 | _ -> 0" "Blue" 1
            }

            test "qualified external case pattern is stamped" {
                assertPatStamped "let f (o: obj) = match o with | Hue.Blue -> 1 | _ -> 0" "Blue" 1
            }

            test "case nested inside a tuple pattern is stamped" {
                assertPatStamped "let f (o: obj) = match o with | (Blue, _) -> 1 | _ -> 0" "Blue" 1
            }

            // The load-bearing gap: or-pattern alternatives bind nothing, so the
            // bound-variable-collection walk never visits them; the stamping walk must reach
            // BOTH alternatives independently.
            test "both alternatives of an or-pattern are stamped" {
                assertPatStamped "let f (o: obj) = match o with | Blue | Blue -> 1 | _ -> 0" "Blue" 2
            }

            // A bare RQA-free case in a `let` binding pattern still stamps (the bound variable walk
            // treats the head as a nullary ctor).
            test "case in a let binding pattern is stamped" {
                assertPatStamped "let f (o: obj) = let Blue = o in 1" "Blue" 1
            }

            // The opens gate: `Other.Shade`'s namespace `Other` is not auto-opened,
            // so a BARE `Green` must not stamp — F# has no global reverse case index,
            // a bare case needs its declaring namespace opened.
            test "bare case whose namespace is not opened is not stamped (pattern)" {
                assertPatNotStamped "let f (o: obj) = match o with | Green -> 1 | _ -> 0" "Green" 1
            }

            // Positive: the SAME case stamps once its namespace is explicitly opened.
            test "bare case is stamped once its namespace is opened (pattern)" {
                assertPatStamped "open Other\nlet f (o: obj) = match o with | Green -> 1 | _ -> 0" "Green" 1
            }

            // A qualified reference resolves without the namespace opened (F# resolves
            // `Union.Case` without consulting the per-scope unqualified tables).
            test "qualified case whose namespace is not opened is still stamped" {
                assertPatStamped "let f (o: obj) = match o with | Shade.Green -> 1 | _ -> 0" "Green" 1
            }

            // Expression position: a bare case in a non-opened namespace is a plain
            // unresolved identifier (nothing stamps it, no external ctor recognised).
            test "bare case whose namespace is not opened is unresolved (expression)" {
                Expect.isTrue (hasUnresolved "let x = Green") "bare Green is unresolved with Other not opened"
            }

            test "bare case resolves in expression position once its namespace is opened" {
                Expect.isFalse (hasUnresolved "open Other\nlet x = Green") "bare Green resolves under open Other"
            }
        ]
