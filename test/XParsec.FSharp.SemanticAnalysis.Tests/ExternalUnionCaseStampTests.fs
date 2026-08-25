module XParsec.FSharp.SemanticAnalysis.Tests.ExternalUnionCaseStampTests

open Expecto
open XParsec.FSharp.Lexer
open XParsec.FSharp.Parser
open XParsec.FSharp.SemanticAnalysis
open XParsec.FSharp.SemanticAnalysis.Passes
open XParsec.FSharp.SemanticAnalysis.Tests.TestHelpers

/// Three non-RQA unions: `Tests.Hue` (case `Blue`) sits in the ambient namespace `Tests`,
/// as the real prelude auto-opens the package namespace so `Some`/`None` are bare-visible;
/// `Other.Shade` and `Rival.Tint` (both case `Green`) do not, so bare `Green` needs an
/// explicit `open Other`, and needs exactly one of the two opens to be unambiguous.
let private provider: IExternalSymbolProvider =
    providerOfSurface (fun b ->
        publishUnion
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Tests.Hue" 0)
            [ ExternalCaseShape.create ("Blue", EqArray.empty) ]

        publishUnion
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Other.Shade" 0)
            [ ExternalCaseShape.create ("Green", EqArray.empty) ]

        publishUnion
            b
            (SymbolKeyOps.qualifiedTypeKeyOf "Rival.Tint" 0)
            [ ExternalCaseShape.create ("Green", EqArray.empty) ]

        b.AmbientOpenPrefixes <- [ "Tests" ]
    )

let private analyse (input: string) = analyseNameRes provider input

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

/// Every pattern node in `file`: module-let patterns and args, lambda, for-in, match arms.
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

    for m in CstModuleTree.implFileElems file do
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

/// Ctor patterns whose case name — bare, or the last segment of a qualified one — is `caseName`.
let private caseCtors (ctx: PassContext) (file: ImplementationFile<SyntaxToken>) (caseName: string) =
    allPats file
    |> List.filter (fun p ->
        match p with
        | Pat.NamedSimple t -> ctx.NameOf t = caseName
        | Pat.Named(longIdent = li) when li.Idents.Length >= 1 ->
            ctx.NameOf li.Idents.[li.Idents.Length - 1] = caseName
        | _ -> false
    )

/// Assert every `caseName` ctor in `input` carries a pattern-position stamp. `expected`
/// pins the ctor count, so a missed traversal position cannot pass by finding zero.
let private assertPatStamped (input: string) (caseName: string) (expected: int) =
    let ctx, file = analyse input
    let ctors = caseCtors ctx file caseName
    Expect.equal ctors.Length expected (sprintf "ctor count for '%s' in: %s" caseName input)

    for h in ctors do
        Expect.isTrue
            ((ResolvedStamps.tryExternalUnionCase ctx.Resolution.Resolved (CstKeys.ofPat h)).IsSome)
            (sprintf "external case '%s' stamped at its ctor pattern in: %s" caseName input)

/// Assert every `caseName` ctor in `input` is NOT stamped.
let private assertPatNotStamped (input: string) (caseName: string) (expected: int) =
    let ctx, file = analyse input
    let ctors = caseCtors ctx file caseName
    Expect.equal ctors.Length expected (sprintf "ctor count for '%s' in: %s" caseName input)

    for h in ctors do
        Expect.isFalse
            ((ResolvedStamps.tryExternalUnionCase ctx.Resolution.Resolved (CstKeys.ofPat h)).IsSome)
            (sprintf "bare case '%s' in a non-opened namespace is NOT stamped in: %s" caseName input)

let private hasUnresolved (input: string) : bool =
    let ctx, _ = analyse input
    ctx.Diagnostics |> Seq.exists (fun d -> d.Message.Contains "Unresolved")

let private hasAmbiguousCtor (input: string) : bool =
    let ctx, _ = analyse input

    ctx.Diagnostics
    |> Seq.exists (fun d -> d.Message.Contains "Ambiguous constructor 'Green'")

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

            // A nullary case binds nothing, so no bound-variable collection stamps it;
            // the stamping walk must reach both alternatives itself.
            test "both alternatives of an or-pattern are stamped" {
                assertPatStamped "let f (o: obj) = match o with | Blue | Blue -> 1 | _ -> 0" "Blue" 2
            }

            test "case in a let binding pattern is stamped" {
                assertPatStamped "let f (o: obj) = let Blue = o in 1" "Blue" 1
            }

            // There is no global reverse case index: a bare case needs its declaring
            // namespace opened, and `Other` is not.
            test "bare case whose namespace is not opened is not stamped (pattern)" {
                assertPatNotStamped "let f (o: obj) = match o with | Green -> 1 | _ -> 0" "Green" 1
            }

            test "bare case is stamped once its namespace is opened (pattern)" {
                assertPatStamped "open Other\nlet f (o: obj) = match o with | Green -> 1 | _ -> 0" "Green" 1
            }

            // `dotnet fsi` rejects the qualified form too (FS0039 on `Shade`): the union's
            // own name has to be reachable before its case is.
            test "qualified case whose namespace is not opened is not stamped" {
                assertPatNotStamped "let f (o: obj) = match o with | Shade.Green -> 1 | _ -> 0" "Green" 1
            }

            test "qualified case is stamped once its namespace is opened" {
                assertPatStamped "open Other\nlet f (o: obj) = match o with | Shade.Green -> 1 | _ -> 0" "Green" 1
            }

            // In expression position an unstamped bare case is a plain unresolved identifier.
            test "bare case whose namespace is not opened is unresolved (expression)" {
                Expect.isTrue (hasUnresolved "let x = Green") "bare Green is unresolved with Other not opened"
            }

            test "bare case resolves in expression position once its namespace is opened" {
                Expect.isFalse (hasUnresolved "open Other\nlet x = Green") "bare Green resolves under open Other"
            }

            // Two referenced unions claim the bare name, so the reference is reported rather
            // than settled by index order. F# itself shadows instead, resolving to the last
            // `open`.
            test "a bare case both opened unions declare is ambiguous (expression)" {
                Expect.isTrue
                    (hasAmbiguousCtor "open Other\nopen Rival\nlet x = Green")
                    "Green is claimed by Other.Shade and Rival.Tint"
            }

            test "a bare case both opened unions declare is ambiguous (pattern)" {
                Expect.isTrue
                    (hasAmbiguousCtor "open Other\nopen Rival\nlet f (o: obj) = match o with | Green -> 1 | _ -> 0")
                    "Green is claimed by Other.Shade and Rival.Tint"
            }

            // The qualifier picks a union directly, so the second claim is irrelevant.
            test "a qualifier resolves the ambiguity" {
                let input =
                    "open Other\nopen Rival\nlet f (o: obj) = match o with | Tint.Green -> 1 | _ -> 0"

                Expect.isFalse (hasAmbiguousCtor input) "Tint.Green names its union"
                assertPatStamped input "Green" 1
            }
        ]
